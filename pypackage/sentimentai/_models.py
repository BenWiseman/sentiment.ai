"""Model registry — the embedding backends and their matching trained scorers.

Each backend pairs an embedder with an `.xgb` scoring model trained on that
embedder's vector space (downloaded on first use; see install.py).

macro-F1 values below are from the v2 subsample bake-off (2026-06-03) and are
PROVISIONAL — they get replaced by full-data figures, and the multilingual
backend is pending a cross-lingual validation. Do not treat as final.
"""
from __future__ import annotations
from dataclasses import dataclass


@dataclass(frozen=True)
class Backend:
    name: str
    hf_id: str | None      # HuggingFace id (on-device); None for API backends
    dim: int
    kind: str              # "sentence-transformers" | "openai" | "tfhub-legacy"
    needs_tf: bool
    f1: float | None       # provisional subsample macro-F1
    note: str


BACKENDS: dict[str, Backend] = {
    "bge-small": Backend(
        "bge-small", "BAAI/bge-small-en-v1.5", 384,
        "sentence-transformers", False, 0.824,
        "DEFAULT — tiny, ~4x faster, on-device, no TensorFlow"),
    "bge-base": Backend(
        "bge-base", "BAAI/bge-base-en-v1.5", 768,
        "sentence-transformers", False, 0.836,
        "best on-device accuracy"),
    "openai": Backend(
        "openai", None, 1536,
        "openai", False, 0.861,
        "text-embedding-3-small — best overall, paid API, text leaves device"),
    "multilingual": Backend(
        "multilingual", "sentence-transformers/paraphrase-multilingual-mpnet-base-v2", 768,
        "sentence-transformers", False, None,
        "50+ languages, no TensorFlow (cross-lingual validation pending)"),
    # --- legacy: opt-in, requires TensorFlow. Kept only to reproduce pre-v2 results. ---
    "use": Backend(
        "use", "universal-sentence-encoder/4", 512,
        "tfhub-legacy", True, None, "legacy USE (TensorFlow)"),
    "use-large": Backend(
        "use-large", "universal-sentence-encoder-large/5", 512,
        "tfhub-legacy", True, None, "legacy USE-large (TensorFlow)"),
}

DEFAULT_MODEL = "bge-small"


def resolve(model: str) -> Backend:
    """Look up a backend by name; raise a helpful error listing the options."""
    if model not in BACKENDS:
        raise KeyError(f"unknown model {model!r}; choose from {sorted(BACKENDS)}")
    return BACKENDS[model]
