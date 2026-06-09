"""Sentiment scoring.  Mirrors R: sentiment_score(), sentiment_match().

Pipeline (no TensorFlow, no xgboost at serve): embed text -> small JSON scoring head
(pure numpy, see _scoring.py) -> score in [-1, 1] (negative .. positive). The head is
the same artifact the R package ships; the forward pass is verified bit-for-bit against
R score_json_head.
"""
from __future__ import annotations

import json
from importlib.resources import files
from typing import Mapping, Sequence

import numpy as np

from . import _scoring, _hf_classifier
from ._models import resolve
from ._profiles import get_default_model
from .embedding import embed_text

# placeholder embedded in place of missing inputs; its score is overwritten with NaN, so
# the actual text is irrelevant (we only need something the embedder won't choke on).
_PLACEHOLDER = "na"

_default_poles_cache: "dict[str, list[str]] | None" = None


def _default_poles() -> dict[str, list[str]]:
    """The bundled, balanced default poles for :func:`sentiment_match`.

    A curated 40-positive / 40-negative set, byte-identical to the R package's
    ``inst/default_poles.json`` so both packages explain a score the same way. Loaded
    once and cached; a fresh copy is returned each call so callers can mutate it safely.
    """
    global _default_poles_cache
    if _default_poles_cache is None:
        res = files("sentimentai").joinpath("default_poles.json")
        _default_poles_cache = json.loads(res.read_text(encoding="utf-8"))
    return {k: list(v) for k, v in _default_poles_cache.items()}


def _missing_mask(texts: list, include_empty: bool) -> list[bool]:
    """Which elements are missing — None or NaN (and, if ``include_empty``, ``""``).

    Mirrors R, where NA inputs are mapped to NA outputs (and sentiment_match also treats
    the empty string ``""`` as missing). Without this, ``"query: " + None`` would raise on
    the e5 backend and ``"query: "`` (empty text) would score as a real, meaningless value.
    """
    mask = []
    for t in texts:
        if t is None:
            mask.append(True)
        elif isinstance(t, float) and np.isnan(t):
            mask.append(True)
        elif include_empty and isinstance(t, str) and t == "":
            mask.append(True)
        else:
            mask.append(False)
    return mask


def sentiment_score(
    x: "str | Sequence[str]",
    model: "str | None" = None,
    scoring: str = "mlp",
    batch_size: int = 100,
    scoring_version: str = "2.0",
    **kwargs,
) -> "np.ndarray | None":
    """Return sentiment scores rescaled to ``[-1, 1]`` (negative .. positive).

    R equivalent: ``sentiment_score(x, model, scoring, scoring_version, ...)``. Embeds
    ``x`` then applies the matching scoring head for that embedding space. ``scoring`` is
    ``"mlp"`` (default) or ``"logistic"`` — both ship in the wheel and need neither
    xgboost nor TensorFlow. Missing inputs (``None``/``NaN``) score as ``NaN``, matching R;
    ``x is None`` returns ``None`` (as R returns ``NULL``).
    """
    if x is None:
        return None
    texts = [x] if isinstance(x, str) else list(x)
    miss = _missing_mask(texts, include_empty=False)
    safe = [_PLACEHOLDER if m else t for t, m in zip(texts, miss)]

    model = model or get_default_model()
    if resolve(model).kind == "hf-classifier":   # opt-in end-to-end transformer (RoBERTa)
        p = _hf_classifier.classify_probs(safe, resolve(model).hf_id, batch_size)
        scores = p[:, 2] - p[:, 0]
    else:
        emb = embed_text(safe, model=model, batch_size=batch_size, **kwargs)
        scores = _scoring.score(emb, model=model, scoring=scoring, version=scoring_version)
    if any(miss):
        scores = scores.astype(float)
        scores[np.asarray(miss)] = np.nan
    return scores


def sentiment(
    x: "str | Sequence[str]",
    model: "str | None" = None,
    scoring: str = "mlp",
    batch_size: int = 100,
    scoring_version: str = "2.0",
    **kwargs,
) -> list[dict]:
    """Tidy three-class sentiment: the whole picture the head computes, not just a scalar.

    R equivalent: ``sentiment(x, model, scoring, ...)``. Each row carries the same fields
    as the R data.frame — ``text``, ``sentiment`` (= ``prob_pos - prob_neg``, in
    ``[-1, 1]``), ``prob_neg``, ``prob_neu``, ``prob_pos`` (the head's calibrated class
    probabilities -- temperature-scaled, ECE ~ 0.01-0.02 on the held-out test -- summing to
    1), ``class`` (the most probable label), and
    ``confidence`` (that class's probability). Use it when you need the neutral mass, a
    label, or a confidence to triage rows. Only the JSON heads (``"mlp"`` / ``"logistic"``)
    expose probabilities. Missing inputs (``None``/``NaN``/``""``) yield ``NaN``/``None`` rows.
    """
    if x is None:
        return []
    texts = [x] if isinstance(x, str) else list(x)
    miss = _missing_mask(texts, include_empty=True)
    safe = [_PLACEHOLDER if m else t for t, m in zip(texts, miss)]

    model = model or get_default_model()
    backend = resolve(model)
    if backend.kind == "hf-classifier":          # opt-in end-to-end transformer (RoBERTa)
        p = _hf_classifier.classify_probs(safe, backend.hf_id, batch_size)   # (n, 3)
        hate_h = mixed_h = style_h = None         # flags need the e5 embedding space
        p_hate = p_mixed = style_p = None
    else:
        emb = embed_text(safe, model=model, batch_size=batch_size, **kwargs)
        p = _scoring.probs(emb, model=model, scoring=scoring, version=scoring_version)   # (n, 3)
        # Post-processing flags from the SAME embedding — only when aux heads ship for this model.
        # hate_speech: P(hate) >= recall>=0.90 threshold; mixed: True when neutral yet matches the
        # mixed-vs-neutral classifier; style: top writing style.
        emb_arr = np.asarray(emb, dtype=np.float64)
        hate_h  = _scoring.load_aux("hate", model)
        mixed_h = _scoring.load_aux("mixed", model)
        style_h = _scoring.load_aux("style", model)
        p_hate  = _scoring.logistic_binary_prob(emb_arr, hate_h)  if hate_h  else None
        p_mixed = _scoring.logistic_binary_prob(emb_arr, mixed_h) if mixed_h else None
        style_p = _scoring.multilabel_probs(emb_arr, style_h)     if style_h else None
    classes = ("negative", "neutral", "positive")

    out: list[dict] = []
    for r, text in enumerate(texts):
        if miss[r]:
            row = {"text": text, "sentiment": np.nan,
                   "prob_neg": np.nan, "prob_neu": np.nan, "prob_pos": np.nan,
                   "class": None, "confidence": np.nan}
            if hate_h is not None:  row["hate_speech"], row["p_hate"] = None, np.nan
            if mixed_h is not None: row["mixed"] = None
            if style_h is not None: row["style"] = None
            out.append(row)
            continue
        pn, pu, pp = float(p[r, 0]), float(p[r, 1]), float(p[r, 2])
        j = int(np.argmax(p[r]))
        row = {
            "text": text,
            "sentiment": pp - pn,
            "prob_neg": pn, "prob_neu": pu, "prob_pos": pp,
            "class": classes[j],
            "confidence": float(p[r, j]),
        }
        if hate_h is not None:
            ph = float(p_hate[r])
            row["hate_speech"] = bool(ph >= hate_h["recommended_threshold"])
            row["p_hate"] = ph
        if mixed_h is not None:
            row["mixed"] = bool(classes[j] == "neutral"
                                and float(p_mixed[r]) >= mixed_h["recommended_threshold"])
        if style_h is not None:
            row["style"] = style_h["classes"][int(np.argmax(style_p[r]))]
        out.append(row)
    return out


def sentiment_match(
    x: "str | Sequence[str]",
    phrases: "Mapping[str, Sequence[str]] | None" = None,
    model: "str | None" = None,
    scoring: str = "mlp",
    batch_size: int = 100,
    scoring_version: str = "2.0",
    **kwargs,
) -> list[dict]:
    """Calibrated sentiment plus a nearest-phrase explanation of *why*.

    R equivalent: ``sentiment_match(x, phrases, model, scoring, scoring_version, ...)``;
    the returned rows carry the same columns — ``text, sentiment, phrase, class, similarity``.

    The ``sentiment`` value is the **calibrated head score** — identical to
    :func:`sentiment_score` and independent of the poles. The poles only supply the
    *explanation*: each input is matched to its single nearest example phrase, and
    ``class`` (the pole that phrase belongs to) plus ``similarity`` (cosine to it) describe
    that match. Pass domain ``phrases`` (e.g. quality vs price poles) to relabel why a
    score landed where it did without moving the score. ``phrases`` defaults to the bundled
    balanced pole set (:func:`_default_poles`), shared with the R package.

    Missing inputs (``None``/``NaN``/``""``) yield ``NaN`` sentiment/similarity and ``None``
    phrase/class, with the original text preserved — matching R.
    """
    if x is None:
        return []
    poles = {k: list(v) for k, v in (phrases or _default_poles()).items()}
    if len(poles) < 2:
        raise ValueError("sentiment_match needs at least two poles (e.g. positive/negative).")
    texts = [x] if isinstance(x, str) else list(x)
    miss = _missing_mask(texts, include_empty=True)
    safe = [_PLACEHOLDER if m else t for t, m in zip(texts, miss)]

    model = model or get_default_model()
    if resolve(model).kind == "hf-classifier":
        raise ValueError(
            f"sentiment_match needs an embedding model for the pole comparison; {model!r} is an "
            f"end-to-end classifier. Use an e5/openai model (e.g. model='e5-base').")
    x_emb = embed_text(safe, model=model, batch_size=batch_size, **kwargs)          # (n, dim)
    sentiments = _scoring.score(x_emb, model=model, scoring=scoring,
                                version=scoring_version)                            # (n,) head

    flat_phrases = [(pole, p) for pole, ps in poles.items() for p in ps]
    p_emb = embed_text([p for _, p in flat_phrases], model=model,
                       batch_size=batch_size, **kwargs)
    sims = x_emb @ p_emb.T                                                          # (n, n_phrases) cosine
    np.clip(sims, -1.0, 1.0, out=sims)                                              # guard float overshoot past +-1

    out: list[dict] = []
    for r, text in enumerate(texts):
        if miss[r]:
            out.append({"text": text, "sentiment": np.nan,
                        "phrase": None, "class": None, "similarity": np.nan})
            continue
        j = int(np.argmax(sims[r]))                                                # nearest phrase
        out.append({
            "text": text,
            "sentiment": float(sentiments[r]),
            "phrase": flat_phrases[j][1],
            "class": flat_phrases[j][0],
            "similarity": float(sims[r, j]),
        })
    return out


def sentiment_provenance(
    model: "str | None" = None,
    scoring: str = "mlp",
    scoring_version: str = "2.0",
) -> dict:
    """Provenance for a model + scoring head -- the Python twin of R ``sentiment_provenance()``.

    Returns a dict describing exactly what produces a score: the encoder backend and its
    ``dim`` / ``prefix`` / pinned ``revision``, plus the scoring ``head_type`` and
    ``temperature``. Useful for auditing and reproducibility.
    """
    model = model or get_default_model()
    b = resolve(model)
    out = {
        "model": model, "backend": b.kind, "dim": b.dim, "prefix": b.prefix,
        "revision": b.revision, "scoring": scoring, "scoring_version": scoring_version,
        "head_type": None, "temperature": None,
    }
    try:
        h = _scoring.load_packaged_head(model, scoring, scoring_version)
        out["head_type"] = h.get("type")
        out["temperature"] = h.get("T")
    except Exception:
        pass
    return out
