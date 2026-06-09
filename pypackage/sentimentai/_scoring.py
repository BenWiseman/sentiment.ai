"""Pure-numpy scoring head — the Python twin of R `score_json_head` (R/sentiment.R).

The trained head ships as a small JSON ({type, dim, T, layers|coef, classes}) inside
the wheel, exactly like the R package ships it in inst/scoring/. The forward pass is
matmul + ReLU + temperature + softmax -> P(pos) - P(neg) in [-1, 1]. No xgboost, no
torch, no TensorFlow at serve time. Weight convention is torch [out, in] (the heads are
exported from torch/sklearn that way), matching the R reader.
"""
from __future__ import annotations

import json
from importlib.resources import files
from typing import Any, Mapping

import numpy as np

# class column order in every head: [-1, 0, 1] == [negative, neutral, positive]
_NEG, _POS = 0, 2


def load_head(path: str) -> dict[str, Any]:
    """Read a head JSON from a filesystem path."""
    with open(path, "r", encoding="utf-8") as fh:
        return json.load(fh)


def resolve_head_path(model: str, scoring: str = "mlp", version: str = "1.0"):
    """Locate a head vendored as package data: sentimentai/scoring/<scoring>/<version>/<model>.json."""
    return files("sentimentai").joinpath("scoring", scoring, version, f"{model}.json")


def load_packaged_head(model: str, scoring: str = "mlp", version: str = "1.0") -> dict[str, Any]:
    """Read a head shipped inside the wheel."""
    res = resolve_head_path(model, scoring, version)
    if not res.is_file():
        raise ValueError(
            f"no {scoring!r} scoring head is bundled for model {model!r} (version {version}). "
            f"The 'logistic' head ships for e5-small / e5-base only; use scoring='mlp' "
            f"(the default), which ships for all e5 and openai models."
        )
    return json.loads(res.read_text(encoding="utf-8"))


def probs_with_head(embeddings: np.ndarray, head: Mapping[str, Any]) -> np.ndarray:
    """Forward pass identical to R score_json_probs: the full [neg, neutral, pos]
    probability matrix, shape ``(n, 3)``, each row summing to 1.

    Mirrors R exactly:
      - mlp:      loop layers, z = z @ W.T + b, ReLU on all but the last layer
      - logistic: z = x @ coef.T + intercept
      - then z / T (temperature) and a numerically-stable softmax
    float64 throughout to match R's double precision for bit-level parity.
    """
    X = np.asarray(embeddings, dtype=np.float64)
    if X.ndim == 1:
        X = X[None, :]

    if head.get("type") == "mlp":
        z = X
        layers = head["layers"]
        last = len(layers) - 1
        for i, layer in enumerate(layers):
            W = np.asarray(layer["W"], dtype=np.float64)   # [out, in]
            b = np.asarray(layer["b"], dtype=np.float64)
            z = z @ W.T + b
            if i < last:
                z = np.maximum(z, 0.0)                      # ReLU on hidden layers only
    else:                                                  # multinomial logistic
        W = np.asarray(head["coef"], dtype=np.float64)     # [classes, in]
        b = np.asarray(head["intercept"], dtype=np.float64)
        z = X @ W.T + b

    T = head.get("T")
    if T is not None:
        z = z / float(T)                                   # temperature scaling

    z = z - z.max(axis=1, keepdims=True)                   # numerically-stable softmax
    p = np.exp(z)
    return p / p.sum(axis=1, keepdims=True)                # columns: [neg, neutral, pos]


def score_with_head(embeddings: np.ndarray, head: Mapping[str, Any]) -> np.ndarray:
    """Collapse the head to P(pos) - P(neg) per row, in [-1, 1] (R score_json_head)."""
    p = probs_with_head(embeddings, head)
    return p[:, _POS] - p[:, _NEG]


def score(
    embeddings: np.ndarray,
    model: str,
    scoring: str = "mlp",
    version: str = "2.0",
) -> np.ndarray:
    """Convenience: load the packaged head for (model, scoring) and score `embeddings`."""
    return score_with_head(embeddings, load_packaged_head(model, scoring, version))


def probs(
    embeddings: np.ndarray,
    model: str,
    scoring: str = "mlp",
    version: str = "2.0",
) -> np.ndarray:
    """Convenience: load the packaged head and return the (n, 3) [neg, neu, pos] matrix."""
    return probs_with_head(embeddings, load_packaged_head(model, scoring, version))


# ── post-processing AUX heads (hate / mixed / style) ──────────────────────────
# Small per-encoder flag heads, applied to the SAME embedding the mainline head
# scored. logistic_binary -> P(positive_class); mlp_multilabel -> per-style sigmoids.

def resolve_aux_path(kind: str, model: str):
    """Locate an aux head: sentimentai/scoring/auxiliary/<kind>_<model>.json (kind: hate|mixed|style).

    Directory is 'auxiliary' (not 'aux') because 'aux' is a reserved device name on Windows.
    """
    return files("sentimentai").joinpath("scoring", "auxiliary", f"{kind}_{model}.json")


def load_aux(kind: str, model: str) -> "dict[str, Any] | None":
    """Read an aux head if it ships for this model, else None (the flag is silently omitted)."""
    res = resolve_aux_path(kind, model)
    if not res.is_file():
        return None
    return json.loads(res.read_text(encoding="utf-8"))


def _sigmoid(z: np.ndarray) -> np.ndarray:
    return 1.0 / (1.0 + np.exp(-z))


def logistic_binary_prob(embeddings: np.ndarray, head: Mapping[str, Any]) -> np.ndarray:
    """P(positive_class) for a logistic_binary aux head: sigmoid(X @ coef + intercept). Shape (n,)."""
    X = np.asarray(embeddings, dtype=np.float64)
    if X.ndim == 1:
        X = X[None, :]
    coef = np.asarray(head["coef"], dtype=np.float64)
    b = float(head["intercept"])
    return _sigmoid(X @ coef + b)


def multilabel_probs(embeddings: np.ndarray, head: Mapping[str, Any]) -> np.ndarray:
    """Per-class sigmoid probabilities for an mlp_multilabel aux head (style). Shape (n, n_classes).

    Same forward as an mlp head but with a sigmoid (not softmax) output — each class is an
    independent probability, so a text can be e.g. both 'formal' and 'analytical'.
    """
    X = np.asarray(embeddings, dtype=np.float64)
    if X.ndim == 1:
        X = X[None, :]
    z = X
    layers = head["layers"]
    last = len(layers) - 1
    for i, layer in enumerate(layers):
        W = np.asarray(layer["W"], dtype=np.float64)
        b = np.asarray(layer["b"], dtype=np.float64)
        z = z @ W.T + b
        if i < last:
            z = np.maximum(z, 0.0)
    return _sigmoid(z)
