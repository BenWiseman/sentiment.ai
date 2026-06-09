"""End-to-end HuggingFace sequence classifiers — the optional "max accuracy" backend.

If you can't beat 'em, join 'em: RoBERTa-twitter leads English sentiment accuracy, so
we expose it as an opt-in backend (model="twitter-roberta" / "xlm-roberta"). It bypasses
the embed->JSON-head pipeline — the transformer outputs neg/neu/pos directly. Heavier
(downloads a full ~500MB transformer on first use) and twitter-roberta is English-only;
the default e5 path stays tiny, multilingual, on-device, and TF-free.

transformers + torch are already present (sentence-transformers depends on them), so this
adds no new dependency — only a model download. Post-processing flags (hate/mixed/style)
are NOT produced on this path (they need the e5 embedding space); use an e5 model for those.
"""
from __future__ import annotations

import os
from functools import lru_cache
from typing import Sequence

import numpy as np


@lru_cache(maxsize=2)
def _load(model_id: str):
    os.environ.setdefault("USE_TF", "0")
    os.environ.setdefault("TOKENIZERS_PARALLELISM", "false")
    try:
        import torch
        from transformers import AutoModelForSequenceClassification, AutoTokenizer
    except ImportError as exc:  # pragma: no cover - deps ship with sentence-transformers
        raise ImportError(
            "the HF-classifier backend needs `transformers` and `torch` "
            "(they install with sentence-transformers).") from exc
    tok = AutoTokenizer.from_pretrained(model_id)
    mdl = AutoModelForSequenceClassification.from_pretrained(model_id).eval()
    dev = "mps" if torch.backends.mps.is_available() else (
        "cuda" if torch.cuda.is_available() else "cpu")
    mdl = mdl.to(dev)
    # map each output index to a column in our fixed [neg, neu, pos] order via id2label
    col = {}
    for idx, lab in mdl.config.id2label.items():
        lab = str(lab).lower()
        col[int(idx)] = 0 if "neg" in lab else (2 if "pos" in lab else 1)
    return tok, mdl, dev, col


def classify_probs(texts: "Sequence[str]", model_id: str, batch_size: int = 64) -> np.ndarray:
    """Return the (n, 3) [neg, neu, pos] probability matrix from an HF classifier."""
    import torch
    tok, mdl, dev, col = _load(model_id)
    out = np.zeros((len(texts), 3), dtype=np.float64)
    for i in range(0, len(texts), batch_size):
        chunk = [str(t)[:1000] for t in texts[i:i + batch_size]]
        enc = tok(chunk, return_tensors="pt", padding=True, truncation=True, max_length=128).to(dev)
        with torch.no_grad():
            probs = torch.softmax(mdl(**enc).logits, dim=-1).cpu().numpy()
        for j in range(probs.shape[0]):
            for idx in range(probs.shape[1]):
                out[i + j, col[idx]] += probs[j, idx]
    return out
