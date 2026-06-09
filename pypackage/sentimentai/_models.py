"""Model registry — the embedding backends and their matching trained scorers.

Each backend pairs an embedder with a small JSON scoring head (mlp or logistic)
trained on that embedder's vector space and shipped inside the wheel
(sentimentai/scoring/). The embedder downloads on first use; the head does not.
No xgboost, no TensorFlow at score time.

Lineup LOCKED 2026-06-03: multilingual-by-default (e5), OpenAI as the optional paid
tier, legacy USE models opt-in only (require TensorFlow). Benchmark numbers live in the
package docs (not here), so the registry cannot drift from them. e5 backends pin an
immutable HuggingFace commit (revision) so the downloaded weights are auditable.
"""
from __future__ import annotations
from dataclasses import dataclass


@dataclass(frozen=True)
class Backend:
    name: str
    hf_id: str | None      # HuggingFace id (on-device); None for API backends
    dim: int
    kind: str              # "sentence-transformers" | "openai" | "hf-classifier" | "tfhub-legacy"
    needs_tf: bool         # True => legacy, opt-in (install_sentiment.ai(legacy=TRUE))
    prefix: str            # text prefix the embedder expects (e5 wants "query: ")
    revision: str | None   # pinned HF commit SHA (on-device); None for API/legacy
    note: str


BACKENDS: dict[str, Backend] = {
    "e5-small": Backend(
        "e5-small", "intfloat/multilingual-e5-small", 384,
        "sentence-transformers", False, "",
        "614241f622f53c4eeff9890bdc4f31cfecc418b3",
        "DEFAULT — tiny, fast, ~100 languages, no TensorFlow"),
    "e5-base": Backend(
        "e5-base", "intfloat/multilingual-e5-base", 768,
        "sentence-transformers", False, "",
        "d128750597153bb5987e10b1c3493a34e5a4502a",
        "best on-device — ties OpenAI, ~100 languages, no TensorFlow"),
    "openai": Backend(
        "openai", None, 1536,
        "openai", False, "", None,
        "text-embedding-3-small — best overall, paid API, text leaves device"),
    # --- opt-in end-to-end transformer classifiers ("if you can't beat 'em, join 'em").
    # Bypass the embed->head pipeline; ~500MB-1GB download; no hate/mixed/style flags. ---
    "twitter-roberta": Backend(
        "twitter-roberta", "cardiffnlp/twitter-roberta-base-sentiment-latest", 3,
        "hf-classifier", False, "", None,
        "English RoBERTa sentiment — opt-in max-accuracy backend (~500MB, English-only)"),
    "xlm-roberta": Backend(
        "xlm-roberta", "cardiffnlp/twitter-xlm-roberta-base-sentiment", 3,
        "hf-classifier", False, "", None,
        "Multilingual XLM-R sentiment — opt-in heavy backend (~1GB, ~8 languages)"),
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

# accept the raw OpenAI id as well as the "openai" shorthand, so the same handle
# works in both the R and Python packages. The OpenAI backend here is
# text-embedding-3-small.
_ALIASES: dict[str, str] = {
    "text-embedding-3-small": "openai",
}


def resolve(model: str) -> Backend:
    """Look up a backend by name; raise a helpful error listing the options."""
    model = _ALIASES.get(model, model)
    if model not in BACKENDS:
        raise KeyError(f"unknown model {model!r}; choose from {sorted(BACKENDS)}")
    return BACKENDS[model]
