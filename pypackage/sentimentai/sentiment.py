"""Sentiment scoring.  Mirrors R: sentiment_score(), sentiment_match()."""
from __future__ import annotations
from typing import Sequence, Mapping
from ._models import DEFAULT_MODEL

_STUB = ("scaffold only — implement to 1:1 parity with R v2. "
         "See pypackage/README.md (parity table).")


def sentiment_score(
    x: "str | Sequence[str]",
    model: str = DEFAULT_MODEL,
    scoring: str = "xgb",
    batch_size: int = 100,
    **kwargs,
):
    """Return sentiment scores rescaled to [-1, 1] (negative .. positive).

    R equivalent: ``sentiment_score(x, model, scoring, ...)``.
    Pipeline: embed `x` (embedding.embed_text) -> apply the matching `.xgb`
    scorer for that embedding space -> rescale probabilities to [-1, 1].

    NOT YET IMPLEMENTED.
    """
    raise NotImplementedError(f"sentiment_score: {_STUB}")


def sentiment_match(
    x: "str | Sequence[str]",
    phrases: "Mapping[str, Sequence[str]] | None" = None,
    model: str = DEFAULT_MODEL,
    batch_size: int = 100,
    **kwargs,
):
    """Sentiment score plus nearest-phrase explanation (tunable poles).

    R equivalent: ``sentiment_match(x, phrases, model, ...)``. `phrases` lets the
    caller define what positive/negative mean per domain (e.g. high vs low quality).

    NOT YET IMPLEMENTED.
    """
    raise NotImplementedError(f"sentiment_match: {_STUB}")
