"""Smoke tests for the scaffold. The registry/imports are real and must pass;
functional tests are skipped until the engine is implemented to R v2 parity."""
import pytest
import sentimentai


def test_imports_and_registry():
    assert sentimentai.DEFAULT_MODEL == "bge-small"
    assert "bge-base" in sentimentai.BACKENDS
    assert "openai" in sentimentai.BACKENDS
    # default is on-device and TF-free
    assert sentimentai.BACKENDS["bge-small"].needs_tf is False


def test_public_api_present():
    for fn in ("embed_text", "sentiment_score", "sentiment_match", "ensure_model"):
        assert callable(getattr(sentimentai, fn))


@pytest.mark.skip(reason="scaffold — implement with R v2 parity")
def test_sentiment_score_roundtrip():
    s = sentimentai.sentiment_score(["I love this", "this is terrible"])
    assert s[0] > 0 > s[1]
