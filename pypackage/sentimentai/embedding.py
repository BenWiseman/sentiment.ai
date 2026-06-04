"""Text -> embedding matrix.  Mirrors R: embed_text().

Backends come from the registry (_models.py): sentence-transformers (on-device,
default, TensorFlow-free), OpenAI (paid API), or legacy USE (TensorFlow — NOT
supported in the Python sibling; use the R package for those). e5 models require a
``"query: "`` prefix and are used with L2-normalised embeddings, matching the space the
scoring heads were trained in (R inst/get_embedder.py does the same).
"""
from __future__ import annotations

import os
from functools import lru_cache
from typing import Sequence

import numpy as np

from ._models import DEFAULT_MODEL, resolve


def _as_list(text: "str | Sequence[str]") -> list[str]:
    return [text] if isinstance(text, str) else list(text)


@lru_cache(maxsize=4)
def _load_st(hf_id: str):
    """Load (and cache) a sentence-transformers embedder, never importing TensorFlow."""
    os.environ.setdefault("USE_TF", "0")          # transformers must not pull TF
    os.environ.setdefault("TRANSFORMERS_NO_ADVISORY_WARNINGS", "1")
    try:
        from sentence_transformers import SentenceTransformer
    except ImportError as exc:                    # pragma: no cover - env-dependent
        raise ImportError(
            "the default on-device backend needs `sentence-transformers` "
            "(pip install --pre sentimentai-py).") from exc
    return SentenceTransformer(hf_id)


def _embed_openai(texts: list[str], api_key: "str | None" = None,
                  openai_model: str = "text-embedding-3-small", **_) -> np.ndarray:
    api_key = api_key or os.environ.get("OPENAI_API_KEY")
    if not api_key:
        raise RuntimeError(
            "the openai backend needs an API key (set OPENAI_API_KEY or pass api_key=). "
            "Text leaves the device for this backend.")
    import requests
    resp = requests.post(
        "https://api.openai.com/v1/embeddings",
        headers={"Authorization": f"Bearer {api_key}"},
        json={"model": openai_model, "input": texts},
        timeout=60,
    )
    resp.raise_for_status()
    data = sorted(resp.json()["data"], key=lambda d: d["index"])
    return np.asarray([d["embedding"] for d in data], dtype=np.float64)


def embed_text(
    text: "str | Sequence[str]",
    model: str = DEFAULT_MODEL,
    batch_size: int = 64,
    **kwargs,
) -> np.ndarray:
    """Embed ``text`` into an ``(n, dim)`` float64 matrix.

    R equivalent: ``embed_text(text, model, batch_size, ...)``. The backend is chosen
    from the registry; e5 (default) runs on-device with no TensorFlow.
    """
    backend = resolve(model)
    texts = _as_list(text)

    if backend.kind == "sentence-transformers":
        st = _load_st(backend.hf_id)
        prefixed = [backend.prefix + t for t in texts]          # e5 expects "query: "
        emb = st.encode(
            prefixed, batch_size=batch_size, normalize_embeddings=True,
            convert_to_numpy=True, show_progress_bar=False,
        )
        return np.asarray(emb, dtype=np.float64)

    if backend.kind == "openai":
        return _embed_openai(texts, **kwargs)

    if backend.kind == "tfhub-legacy":
        target = backend.note.split("->")[-1].strip() if "->" in backend.note else "e5-base"
        raise NotImplementedError(
            f"model {model!r} is a legacy TensorFlow (Universal Sentence Encoder) embedder. "
            f"The Python package is TensorFlow-free — use the R package's legacy mode, "
            f"or switch to the TF-free migration target ({target}).")

    raise ValueError(f"unknown backend kind {backend.kind!r}")
