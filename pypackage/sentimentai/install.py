"""Model-artifact provisioning (download on first use).

Mirrors R: install_sentiment.ai() / install_scoring_model() / init_sentiment.ai().
Unlike R, there is NO reticulate/conda/virtualenv dance — sentence-transformers and
xgboost are direct Python deps. This just fetches the embedder (HuggingFace) and the
matching `.xgb` scorer (GitHub Releases) and caches them locally.
"""
from __future__ import annotations
from ._models import DEFAULT_MODEL

_STUB = ("scaffold only — implement to 1:1 parity with R v2. "
         "See pypackage/README.md (parity table).")


def ensure_model(model: str = DEFAULT_MODEL, **kwargs) -> None:
    """Ensure the embedder + scorer for `model` are downloaded and cached.

    NOT YET IMPLEMENTED.
    """
    raise NotImplementedError(f"ensure_model: {_STUB}")
