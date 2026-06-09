"""Intent-based profiles + the configurable default model.

Instead of making callers learn the model zoo, a profile maps a plain intent to a
backend and becomes the default for sentiment_score()/sentiment()/sentiment_match().
The choice persists to a small JSON config (XDG config dir), so it sticks across
sessions; an env var (SENTIMENTAI_MODEL) overrides it for one-off runs.

Key nuance: the hate/mixed/style flags need the e5 embedding space, so "multilingual
WITH flags" is e5-base, NOT the (more accurate but flag-less) XLM-R transformer.
"""
from __future__ import annotations

import json
import os
from pathlib import Path

from ._models import DEFAULT_MODEL, resolve

PROFILES: dict[str, dict] = {
    "lightest": {
        "model": "e5-small",
        "summary": "Lightest on-device",
        "desc": "Tiny + instant, ~100 languages, fully offline. Includes hate/mixed/style flags. ~2MB head.",
    },
    "multilingual": {
        "model": "e5-base",
        "summary": "Best on-device multilingual, with flags",
        "desc": "Stronger multilingual sentiment, fully offline. Includes hate/mixed/style flags. ~7MB head.",
    },
    "max-english": {
        "model": "twitter-roberta",
        "summary": "Best English accuracy (sentiment only)",
        "desc": "Highest English accuracy (RoBERTa). Sentiment only — no flags. ~500MB download, English-only.",
    },
    "max-multilingual": {
        "model": "xlm-roberta",
        "summary": "Best multilingual accuracy (sentiment only)",
        "desc": "Highest multilingual accuracy (XLM-R). Sentiment only — no flags. ~1GB download.",
    },
}
DEFAULT_PROFILE = "multilingual"


def _config_path() -> Path:
    base = os.environ.get("XDG_CONFIG_HOME") or os.path.expanduser("~/.config")
    return Path(base) / "sentimentai" / "config.json"


def _read_config() -> dict:
    try:
        return json.loads(_config_path().read_text(encoding="utf-8"))
    except Exception:
        return {}


def get_default_model() -> str:
    """Resolve the default model: env override > persisted config > lightest profile."""
    env = os.environ.get("SENTIMENTAI_MODEL")
    if env:
        return env
    cfg = _read_config()
    return cfg.get("default_model", PROFILES.get(DEFAULT_PROFILE, {}).get("model", DEFAULT_MODEL))


def set_default_model(model: str) -> None:
    """Validate and persist `model` as the default for this user."""
    resolve(model)  # raises KeyError on an unknown handle
    p = _config_path()
    p.parent.mkdir(parents=True, exist_ok=True)
    cfg = _read_config()
    cfg["default_model"] = model
    p.write_text(json.dumps(cfg, indent=2), encoding="utf-8")


def use_profile(name: str) -> str:
    """Set the default model from a named profile; returns the model handle."""
    if name not in PROFILES:
        raise KeyError(f"unknown profile {name!r}; choose from {sorted(PROFILES)}")
    model = PROFILES[name]["model"]
    set_default_model(model)
    return model
