"""Model-artifact provisioning + interactive profile setup.

Mirrors R: install_sentiment.ai() / init_sentiment.ai() — but with NO reticulate /
conda / virtualenv dance, because the engine is already Python. The small JSON scoring
heads ship *inside* the wheel (sentimentai/scoring/), so the only thing to fetch on first
use is the on-device embedder (or, for the opt-in transformer backends, the classifier).
"""
from __future__ import annotations

from ._models import resolve
from ._profiles import DEFAULT_PROFILE, PROFILES, get_default_model, use_profile
from ._scoring import resolve_head_path


def ensure_model(model: "str | None" = None, scoring: str = "mlp", version: str = "2.0") -> dict:
    """Ensure everything for ``model`` is available, downloading on first use. ``model``
    defaults to the configured profile. e5/openai use embed->JSON-head; the opt-in
    transformer backends (twitter-roberta / xlm-roberta) run end-to-end (no head).
    """
    model = model or get_default_model()
    backend = resolve(model)
    if backend.kind == "tfhub-legacy":
        raise NotImplementedError(
            f"model {model!r} is a legacy TensorFlow embedder, not supported in the "
            f"TensorFlow-free Python package; use the R package or a TF-free e5 model.")

    if backend.kind == "hf-classifier":
        from ._hf_classifier import _load
        _load(backend.hf_id)                      # downloads + caches the transformer
        return {"model": model, "embedder": backend.hf_id, "scoring": "end-to-end",
                "head": None, "needs_tf": False}

    head = resolve_head_path(model, scoring, version)
    if not head.is_file():
        raise FileNotFoundError(
            f"no {scoring!r} scoring head packaged for model {model!r} (version {version}).")
    embedder = None
    if backend.kind == "sentence-transformers":
        from .embedding import _load_st           # triggers HuggingFace download + cache
        _load_st(backend.hf_id)
        embedder = backend.hf_id
    elif backend.kind == "openai":
        embedder = "openai-api"                   # nothing to download; key checked at call time
    return {"model": model, "embedder": embedder, "scoring": scoring,
            "head": str(head), "needs_tf": backend.needs_tf}


def setup(profile: "str | None" = None, download: bool = True) -> str:
    """Pick a profile, make it the default, and (by default) provision it.

    Profiles — plain intent, not model names:
      lightest          tiny on-device, multilingual, + hate/mixed/style flags
      multilingual      best on-device multilingual WITH flags
      max-english       best English accuracy (sentiment only, ~500MB)
      max-multilingual  best multilingual accuracy (sentiment only, ~1GB)

    Pass ``profile=`` to skip the prompt; returns the chosen profile name.
    """
    keys = list(PROFILES)
    if profile is None:
        print("Choose a sentiment.ai profile:\n")
        for i, k in enumerate(keys, 1):
            pr = PROFILES[k]
            print(f"  {i}. {k:22} — {pr['summary']}")
            print(f"     {pr['desc']}\n")
        try:
            raw = input(f"Profile [1-{len(keys)}] (default 1 = lightest): ").strip()
        except EOFError:
            raw = ""
        if raw in PROFILES:
            profile = raw
        else:
            try:
                profile = keys[int(raw) - 1] if raw else DEFAULT_PROFILE
            except (ValueError, IndexError):
                profile = DEFAULT_PROFILE

    model = use_profile(profile)
    print(f"\n✓ default profile = {profile!r}  →  model {model!r}")
    print(f"  {PROFILES[profile]['desc']}")
    if download:
        print("  provisioning …")
        info = ensure_model(model)
        print(f"  ready: {info['embedder']}")
    return profile
