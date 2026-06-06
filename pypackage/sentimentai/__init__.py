"""sentiment.ai (Python) — TensorFlow-free sentiment from sentence embeddings.

Python sibling of the R package `sentiment.ai`. Same trained scorer artifacts (the
small JSON heads ship in the wheel) and the same public API. The scoring forward pass
is verified bit-for-bit against R `score_json_head`, so the two bindings agree by
construction. e5 runs on-device with no TensorFlow and no xgboost at serve.
"""
from ._version import __version__
from ._models import BACKENDS, DEFAULT_MODEL, Backend, resolve
from .embedding import embed_text
from .sentiment import sentiment_score, sentiment_match, sentiment
from .install import ensure_model

__all__ = [
    "embed_text",
    "sentiment_score",
    "sentiment",
    "sentiment_match",
    "ensure_model",
    "BACKENDS",
    "DEFAULT_MODEL",
    "Backend",
    "resolve",
    "__version__",
]
