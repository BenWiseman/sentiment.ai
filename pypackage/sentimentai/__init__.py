"""sentiment.ai (Python) — TensorFlow-free sentiment from sentence embeddings.

Python sibling of the R package `sentiment.ai`. Same trained scorer artifacts,
same public API. This is a SCAFFOLD: the registry and signatures are in place;
function bodies are populated once the R v2 package is locked, for 1:1 parity.
"""
from ._version import __version__
from ._models import BACKENDS, DEFAULT_MODEL, Backend, resolve
from .embedding import embed_text
from .sentiment import sentiment_score, sentiment_match
from .install import ensure_model

__all__ = [
    "embed_text",
    "sentiment_score",
    "sentiment_match",
    "ensure_model",
    "BACKENDS",
    "DEFAULT_MODEL",
    "Backend",
    "resolve",
    "__version__",
]
