"""Model registry — the embedding backends and their matching trained scorers.

Each backend pairs an embedder with a small JSON scoring head (mlp or logistic)
trained on that embedder's vector space and shipped inside the wheel
(sentimentai/scoring/). The embedder downloads on first use; the head does not.
No xgboost, no TensorFlow at score time.

Lineup LOCKED 2026-06-03: multilingual-by-default (e5), OpenAI as the optional paid
tier, legacy USE models opt-in only (require TensorFlow). macro-F1 values are from the
v2 subsample bake-off and are PROVISIONAL pending the full-data run.
"""
from __future__ import annotations
from dataclasses import dataclass


@dataclass(frozen=True)
class Backend:
    name: str
    hf_id: str | None      # HuggingFace id (on-device); None for API backends
    dim: int
    kind: str              # "sentence-transformers" | "openai" | "tfhub-legacy"
    needs_tf: bool         # True => legacy, opt-in (install_sentiment.ai(legacy=TRUE))
    prefix: str            # text prefix the embedder expects (e5 wants "query: ")
    f1: float | None       # provisional subsample macro-F1
    note: str


BACKENDS: dict[str, Backend] = {
    "e5-small": Backend(
        "e5-small", "intfloat/multilingual-e5-small", 384,
        "sentence-transformers", False, "query: ", 0.813,
        "DEFAULT — tiny, fast, ~100 languages, no TensorFlow"),
    "e5-base": Backend(
        "e5-base", "intfloat/multilingual-e5-base", 768,
        "sentence-transformers", False, "query: ", 0.860,
        "best on-device — ties OpenAI, ~100 languages, no TensorFlow"),
    "openai": Backend(
        "openai", None, 1536,
        "openai", False, "", 0.861,
        "text-embedding-3-small — best overall, paid API, text leaves device"),
    # --- legacy: opt-in, requires TensorFlow. install_sentiment.ai(legacy=TRUE). ---
    # Each has a strictly-better TF-free migration target (see note).
    "en.large": Backend(
        "en.large", "universal-sentence-encoder-large/5", 512,
        "tfhub-legacy", True, "", None, "legacy USE-en-large (TF) -> migrate to e5-base"),
    "en": Backend(
        "en", "universal-sentence-encoder/4", 512,
        "tfhub-legacy", True, "", None, "legacy USE-en (TF) -> migrate to e5-small"),
    "multi.large": Backend(
        "multi.large", "universal-sentence-encoder-multilingual-large/3", 512,
        "tfhub-legacy", True, "", None, "legacy USE-multi-large (TF) -> migrate to e5-base"),
    "multi": Backend(
        "multi", "universal-sentence-encoder-multilingual/3", 512,
        "tfhub-legacy", True, "", None, "legacy USE-multi (TF) -> migrate to e5-small"),
}

DEFAULT_MODEL = "e5-small"


def resolve(model: str) -> Backend:
    """Look up a backend by name; raise a helpful error listing the options."""
    if model not in BACKENDS:
        raise KeyError(f"unknown model {model!r}; choose from {sorted(BACKENDS)}")
    return BACKENDS[model]
