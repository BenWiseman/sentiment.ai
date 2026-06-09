"""Smoke tests for the scaffold. The registry/imports are real and must pass;
functional tests are skipped until the engine is implemented to R v2 parity."""
import pytest
import sentimentai


def test_imports_and_registry():
    assert sentimentai.DEFAULT_MODEL == "e5-base"
    assert "e5-small" in sentimentai.BACKENDS
    assert "openai" in sentimentai.BACKENDS
    # default is on-device, multilingual, and TF-free
    assert sentimentai.BACKENDS["e5-base"].needs_tf is False
    # legacy USE variants are opt-in (require TF)
    assert sentimentai.BACKENDS["multi.large"].needs_tf is True


def test_public_api_present():
    for fn in ("embed_text", "sentiment_score", "sentiment_match", "ensure_model"):
        assert callable(getattr(sentimentai, fn))


def _have_embedder():
    try:
        import sentence_transformers  # noqa: F401
        return True
    except Exception:
        return False


@pytest.mark.skipif(not _have_embedder(), reason="needs sentence-transformers (on-device embedder)")
def test_sentiment_score_roundtrip():
    # real end-to-end: e5 embed -> numpy head -> [-1, 1]
    s = sentimentai.sentiment_score(["I love this, it is wonderful", "this is awful, I hate it"])
    assert s[0] > 0 > s[1]
