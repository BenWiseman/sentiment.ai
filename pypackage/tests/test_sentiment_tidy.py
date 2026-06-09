"""sentiment(): the tidy 3-class output (Python twin of R sentiment()).

Hermetic — a deterministic fake embedder feeds the REAL shipped head, so the values are
meaningless but the distribution is valid. We lock the shape, the within-row invariants,
and that the derived scalar still agrees with sentiment_score().
"""
import hashlib
import importlib

import numpy as np
import pytest

import sentimentai
# the submodule (sentimentai.sentiment is shadowed by the exported sentiment() function)
_sent = importlib.import_module("sentimentai.sentiment")

_DIM = {"e5-small": 384, "e5-base": 768}


def _fake_embed(x, model="e5-small", batch_size=100, **kwargs):
    texts = [x] if isinstance(x, str) else list(x)
    dim = _DIM[model]
    out = np.empty((len(texts), dim), dtype=np.float64)
    for i, t in enumerate(texts):
        seed = int(hashlib.sha1(str(t).encode("utf-8")).hexdigest()[:8], 16)
        v = np.random.RandomState(seed).standard_normal(dim)
        out[i] = v / np.linalg.norm(v)
    return out


@pytest.fixture
def fake_embed(monkeypatch):
    monkeypatch.setattr(_sent, "embed_text", _fake_embed)
    monkeypatch.setattr(_sent, "get_default_model", lambda: "e5-small")  # deterministic default


_COLS_CORE = {"text", "sentiment", "prob_neg", "prob_neu", "prob_pos", "class", "confidence"}
# v2 post-processing flags: present when aux heads ship for the model (e5-small default does)
_COLS_AUX = {"mixed", "hate_speech", "p_hate", "style"}
_COLS = _COLS_CORE | _COLS_AUX
_CLASSES = ("negative", "neutral", "positive")
_STYLES = {"analytical", "descriptive", "formal", "informal", "inquisitive"}


def test_tidy_shape_and_invariants(fake_embed):
    rows = sentimentai.sentiment(["great", "fine", "awful"])
    assert len(rows) == 3
    for row in rows:
        assert set(row) == _COLS
        probs = [row["prob_neg"], row["prob_neu"], row["prob_pos"]]
        assert all(0.0 <= p <= 1.0 for p in probs)
        assert sum(probs) == pytest.approx(1.0, abs=1e-9)
        assert row["sentiment"] == pytest.approx(row["prob_pos"] - row["prob_neg"])
        assert row["confidence"] == pytest.approx(max(probs))
        assert _CLASSES.index(row["class"]) == probs.index(max(probs))   # class == argmax
        # v2 flag invariants
        assert isinstance(row["hate_speech"], bool) and 0.0 <= row["p_hate"] <= 1.0
        assert isinstance(row["mixed"], bool)
        assert row["style"] in _STYLES


def test_scalar_matches_sentiment_score(fake_embed):
    x = ["good", "bad", "ok"]
    got = np.array([r["sentiment"] for r in sentimentai.sentiment(x)])
    np.testing.assert_allclose(got, sentimentai.sentiment_score(x), atol=1e-12)


def test_missing_rows_blanked(fake_embed):
    rows = sentimentai.sentiment(["good", None, ""])
    assert rows[0]["class"] in _CLASSES
    for i in (1, 2):
        assert rows[i]["class"] is None
        assert np.isnan(rows[i]["sentiment"]) and np.isnan(rows[i]["prob_pos"])
    assert rows[1]["text"] is None and rows[2]["text"] == ""


def test_hf_classifier_backend(monkeypatch):
    """The opt-in transformer backend (RoBERTa) returns core columns and NO e5-only flags."""
    def fake_classify(texts, model_id, batch_size=64):
        return np.tile([0.1, 0.2, 0.7], (len(texts), 1))   # deterministic 'positive'
    monkeypatch.setattr(_sent._hf_classifier, "classify_probs", fake_classify)

    rows = sentimentai.sentiment(["a", "b"], model="twitter-roberta")
    assert len(rows) == 2
    for row in rows:
        assert set(row) == _COLS_CORE          # flags omitted on the transformer path
        assert row["class"] == "positive"
        assert row["sentiment"] == pytest.approx(0.6)
    scores = sentimentai.sentiment_score(["a"], model="twitter-roberta")
    assert scores[0] == pytest.approx(0.6)
