"""Text -> embedding matrix.  Mirrors R: embed_text()."""
from __future__ import annotations
from typing import Sequence
from ._models import DEFAULT_MODEL

_STUB = ("scaffold only — implement to 1:1 parity with R v2. "
         "See pypackage/README.md (parity table).")


def embed_text(
    text: "str | Sequence[str]",
    model: str = DEFAULT_MODEL,
    batch_size: int = 64,
    **kwargs,
):
    """Embed `text` into a (n, dim) float matrix.

    R equivalent: ``embed_text(text, model, batch_size, ...)``.
    Backend is chosen from the model registry: sentence-transformers (on-device,
    default), OpenAI (API), or legacy USE (TensorFlow, opt-in).

    NOT YET IMPLEMENTED.
    """
    raise NotImplementedError(f"embed_text: {_STUB}")
