"""Intent-based profiles + configurable default model (hermetic — no model loads)."""
import importlib

import pytest

import sentimentai
import sentimentai._profiles as P

_sent = importlib.import_module("sentimentai.sentiment")

_EXPECTED = {
    "lightest": "e5-small",
    "multilingual": "e5-base",
    "max-english": "twitter-roberta",
    "max-multilingual": "xlm-roberta",
}


def test_profiles_map_to_real_models():
    assert set(P.PROFILES) == set(_EXPECTED)
    for name, model in _EXPECTED.items():
        assert P.PROFILES[name]["model"] == model
        sentimentai.resolve(model)            # each handle resolves (no raise)


@pytest.fixture
def isolated_config(tmp_path, monkeypatch):
    monkeypatch.setattr(P, "_config_path", lambda: tmp_path / "config.json")
    monkeypatch.delenv("SENTIMENTAI_MODEL", raising=False)


def test_use_profile_persists_default(isolated_config):
    assert P.get_default_model() == "e5-small"          # lightest fallback
    assert P.use_profile("multilingual") == "e5-base"
    assert P.get_default_model() == "e5-base"            # persisted + read back
    P.use_profile("max-english")
    assert P.get_default_model() == "twitter-roberta"


def test_unknown_profile_raises(isolated_config):
    with pytest.raises(KeyError):
        P.use_profile("nope")


def test_env_overrides_config(isolated_config, monkeypatch):
    P.use_profile("multilingual")                         # config says e5-base
    monkeypatch.setenv("SENTIMENTAI_MODEL", "openai")     # env wins
    assert P.get_default_model() == "openai"


def test_sentiment_uses_configured_default(monkeypatch):
    """sentiment(model=None) routes through the configured default."""
    captured = {}
    def fake_classify(texts, model_id, batch_size=64):
        captured["model_id"] = model_id
        import numpy as np
        return np.tile([0.1, 0.2, 0.7], (len(texts), 1))
    monkeypatch.setattr(_sent._hf_classifier, "classify_probs", fake_classify)
    monkeypatch.setattr(_sent, "get_default_model", lambda: "twitter-roberta")

    rows = sentimentai.sentiment(["hi"])                  # no model arg
    assert captured["model_id"] == "cardiffnlp/twitter-roberta-base-sentiment-latest"
    assert rows[0]["class"] == "positive"
