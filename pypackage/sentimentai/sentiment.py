"""Sentiment scoring.  Mirrors R: sentiment_score(), sentiment_match().

Pipeline (no TensorFlow, no xgboost at serve): embed text -> small JSON scoring head
(pure numpy, see _scoring.py) -> score in [-1, 1] (negative .. positive). The head is
the same artifact the R package ships; the forward pass is verified bit-for-bit against
R score_json_head.
"""
from __future__ import annotations

from typing import Mapping, Sequence

import numpy as np

from . import _scoring
from ._models import DEFAULT_MODEL
from .embedding import embed_text

# default sentiment poles for sentiment_match when the caller gives none
_DEFAULT_POLES: dict[str, list[str]] = {
    "positive": ["good", "great", "excellent", "wonderful", "I love this"],
    "negative": ["bad", "terrible", "awful", "horrible", "I hate this"],
}


def sentiment_score(
    x: "str | Sequence[str]",
    model: str = DEFAULT_MODEL,
    scoring: str = "mlp",
    batch_size: int = 100,
    version: str = "1.0",
    **kwargs,
) -> np.ndarray:
    """Return sentiment scores rescaled to ``[-1, 1]`` (negative .. positive).

    R equivalent: ``sentiment_score(x, model, scoring, ...)``. Embeds ``x`` then applies
    the matching scoring head for that embedding space. ``scoring`` is ``"mlp"`` (default)
    or ``"logistic"`` — both ship in the wheel and need neither xgboost nor TensorFlow.
    """
    emb = embed_text(x, model=model, batch_size=batch_size, **kwargs)
    return _scoring.score(emb, model=model, scoring=scoring, version=version)


def sentiment_match(
    x: "str | Sequence[str]",
    phrases: "Mapping[str, Sequence[str]] | None" = None,
    model: str = DEFAULT_MODEL,
    batch_size: int = 100,
    **kwargs,
) -> list[dict]:
    """Sentiment by affinity to tunable phrase poles, with a nearest-phrase explanation.

    R equivalent: ``sentiment_match(x, phrases, model, ...)``. ``phrases`` is a mapping of
    pole name -> example phrases (defaults to generic positive/negative). Embeddings are
    L2-normalised, so cosine similarity is a dot product. For each input we return its
    affinity ``mean_sim(positive_pole) - mean_sim(negative_pole)`` and the single nearest
    example phrase across all poles — letting the caller define what the poles *mean* for
    their domain (e.g. high vs low quality), exactly like the R function.
    """
    poles = {k: list(v) for k, v in (phrases or _DEFAULT_POLES).items()}
    if len(poles) < 2:
        raise ValueError("sentiment_match needs at least two poles (e.g. positive/negative).")
    texts = [x] if isinstance(x, str) else list(x)

    x_emb = embed_text(texts, model=model, batch_size=batch_size, **kwargs)        # (n, dim)
    flat_phrases = [(pole, p) for pole, ps in poles.items() for p in ps]
    p_emb = embed_text([p for _, p in flat_phrases], model=model, batch_size=batch_size, **kwargs)
    sims = x_emb @ p_emb.T                                                          # (n, n_phrases) cosine

    pole_names = list(poles.keys())
    pos_name, neg_name = pole_names[0], pole_names[-1]
    cols: dict[str, np.ndarray] = {
        name: np.array([i for i, (pole, _) in enumerate(flat_phrases) if pole == name])
        for name in pole_names
    }

    out: list[dict] = []
    for r, text in enumerate(texts):
        affinity = float(sims[r, cols[pos_name]].mean() - sims[r, cols[neg_name]].mean())
        j = int(np.argmax(sims[r]))
        out.append({
            "text": text,
            "sentiment": affinity,
            "phrase": flat_phrases[j][1],
            "pole": flat_phrases[j][0],
        })
    return out
