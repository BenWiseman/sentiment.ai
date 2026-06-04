"""Model-artifact provisioning.

Mirrors R: install_sentiment.ai() / init_sentiment.ai() — but with NO reticulate /
conda / virtualenv dance, because the engine is already Python. The small JSON scoring
heads ship *inside* the wheel (sentimentai/scoring/), so the only thing to fetch on first
use is the on-device embedder (HuggingFace, cached by sentence-transformers). No xgboost,
no TensorFlow, no GitHub-Releases download for the default e5 backends.
"""
from __future__ import annotations

from ._models import DEFAULT_MODEL, resolve
from ._scoring import resolve_head_path


def ensure_model(model: str = DEFAULT_MODEL, scoring: str = "mlp", version: str = "1.0") -> dict:
    """Ensure the embedder + scoring head for ``model`` are available, downloading the
    embedder on first use. Returns a small status dict. Raises for legacy (TF) backends.
    """
    backend = resolve(model)
    if backend.kind == "tfhub-legacy":
        raise NotImplementedError(
            f"model {model!r} is a legacy TensorFlow embedder, not supported in the "
            f"TensorFlow-free Python package; use the R package or a TF-free e5 model.")

    head = resolve_head_path(model, scoring, version)
    if not head.is_file():
        raise FileNotFoundError(
            f"no {scoring!r} scoring head packaged for model {model!r} (version {version}).")

    embedder = None
    if backend.kind == "sentence-transformers":
        from .embedding import _load_st          # triggers HuggingFace download + cache
        _load_st(backend.hf_id)
        embedder = backend.hf_id
    elif backend.kind == "openai":
        embedder = "openai-api"                  # nothing to download; key checked at call time

    return {"model": model, "embedder": embedder, "scoring": scoring,
            "head": str(head), "needs_tf": backend.needs_tf}
