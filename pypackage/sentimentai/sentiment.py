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

from . import _scoring
from ._models import DEFAULT_MODEL
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
    model: str = DEFAULT_MODEL,
    scoring: str = "mlp",
    batch_size: int = 100,
    scoring_version: str = "1.0",
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

    emb = embed_text(safe, model=model, batch_size=batch_size, **kwargs)
    scores = _scoring.score(emb, model=model, scoring=scoring, version=scoring_version)
    if any(miss):
        scores = scores.astype(float)
        scores[np.asarray(miss)] = np.nan
    return scores


def sentiment_match(
    x: "str | Sequence[str]",
    phrases: "Mapping[str, Sequence[str]] | None" = None,
    model: str = DEFAULT_MODEL,
    scoring: str = "mlp",
    batch_size: int = 100,
    scoring_version: str = "1.0",
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

    x_emb = embed_text(safe, model=model, batch_size=batch_size, **kwargs)          # (n, dim)
    sentiments = _scoring.score(x_emb, model=model, scoring=scoring,
                                version=scoring_version)                            # (n,) head

    flat_phrases = [(pole, p) for pole, ps in poles.items() for p in ps]
    p_emb = embed_text([p for _, p in flat_phrases], model=model,
                       batch_size=batch_size, **kwargs)
    sims = x_emb @ p_emb.T                                                          # (n, n_phrases) cosine

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
