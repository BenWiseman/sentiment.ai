"""sentiment_match: contract + first-class behaviour, mirroring the R tests.

Hermetic — a deterministic per-string fake embedder stands in for the e5 encoder
(same trick as the R suite's local_fake_embedder), so these run anywhere with no
sentence-transformers download. The scoring head is the REAL shipped artifact.

What we lock down:
  * the bundled default poles are the balanced 40/40 set shared with R;
  * rows carry R's exact columns: text, sentiment, phrase, class, similarity;
  * `sentiment` is the calibrated head score (== sentiment_score) and is
    INDEPENDENT of the poles — swapping poles never moves it;
  * the poles drive only the nearest-phrase explanation (phrase/class/similarity).
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
    """Deterministic unit vector per string (same string -> same embedding), so an
    input equal to a pole phrase matches it at cosine 1.0."""
    texts = [x] if isinstance(x, str) else list(x)
    dim = _DIM[model]
    out = np.empty((len(texts), dim), dtype=np.float64)
    for i, t in enumerate(texts):
        seed = int(hashlib.sha1(t.encode("utf-8")).hexdigest()[:8], 16)
        v = np.random.RandomState(seed).standard_normal(dim)
        out[i] = v / np.linalg.norm(v)
    return out


@pytest.fixture
def fake_embed(monkeypatch):
    monkeypatch.setattr(_sent, "embed_text", _fake_embed)


def test_default_poles_are_the_shared_balanced_set():
    poles = _sent._default_poles()
    assert set(poles) == {"positive", "negative"}
    assert len(poles["positive"]) == 40
    assert len(poles["negative"]) == 40
    # no phrase straddles both poles
    assert not (set(poles["positive"]) & set(poles["negative"]))


def test_match_contract_and_head_driven(fake_embed):
    texts = ["the product is great", "i regret it", "totally fine"]
    res = sentimentai.sentiment_match(texts)

    assert len(res) == len(texts)
    for r in res:
        assert set(r) == {"text", "sentiment", "phrase", "class", "similarity"}
        assert r["class"] in ("positive", "negative")
        assert -1.0 <= r["sentiment"] <= 1.0
        assert -1.0 <= r["similarity"] <= 1.0

    # the sentiment column IS the calibrated head score, not a pole affinity
    want = sentimentai.sentiment_score(texts)
    got = np.array([r["sentiment"] for r in res])
    np.testing.assert_allclose(got, want, atol=1e-12)


def test_nearest_phrase_explains_the_match(fake_embed):
    res = sentimentai.sentiment_match(["excellent", "waste of money"])
    assert res[0]["phrase"] == "excellent" and res[0]["class"] == "positive"
    assert res[0]["similarity"] == pytest.approx(1.0, abs=1e-9)
    assert res[1]["phrase"] == "waste of money" and res[1]["class"] == "negative"
    assert res[1]["similarity"] == pytest.approx(1.0, abs=1e-9)


def test_custom_poles_relabel_without_moving_the_score(fake_embed):
    text = ["fast and cheap"]
    res = sentimentai.sentiment_match(
        text, phrases={"quality": ["well made", "reliable"],
                       "price": ["cheap", "affordable"]})
    assert res[0]["class"] in ("quality", "price")
    # same head score whatever poles we pass -> sentiment_match is first-class, not a
    # second scoring path
    assert res[0]["sentiment"] == pytest.approx(
        float(sentimentai.sentiment_score(text)[0]), abs=1e-12)


def test_guardrails(fake_embed):
    with pytest.raises(ValueError):
        sentimentai.sentiment_match(["x"], phrases={"only": ["a"]})
    assert sentimentai.sentiment_match(None) == []


def test_missing_inputs_map_to_nan_like_r(fake_embed):
    res = sentimentai.sentiment_match(["good", None, "", float("nan"), "bad"])
    assert len(res) == 5
    assert res[0]["text"] == "good" and np.isfinite(res[0]["sentiment"])
    assert res[4]["text"] == "bad" and np.isfinite(res[4]["sentiment"])
    for i in (1, 2, 3):                       # None, "", NaN -> blanked, like R
        assert res[i]["phrase"] is None and res[i]["class"] is None
        assert np.isnan(res[i]["sentiment"]) and np.isnan(res[i]["similarity"])
    # original text is preserved verbatim
    assert res[1]["text"] is None
    assert res[2]["text"] == ""
    assert np.isnan(res[3]["text"])


def test_sentiment_score_missing_and_none(fake_embed):
    s = sentimentai.sentiment_score(["good", None, float("nan"), "bad"])
    assert np.isfinite(s[0]) and np.isfinite(s[3])
    assert np.isnan(s[1]) and np.isnan(s[2])
    # whole-None input returns None, mirroring R's sentiment_score(NULL) -> NULL
    assert sentimentai.sentiment_score(None) is None


def test_default_poles_file_matches_committed_md5():
    # the SAME constant the R suite asserts on inst/default_poles.json -- if either
    # package edits its copy without the other, one of the two tests fails.
    import hashlib
    from importlib.resources import files
    raw = files("sentimentai").joinpath("default_poles.json").read_bytes()
    assert hashlib.md5(raw).hexdigest() == "8c26694cccf2adcf32c1c9602c33af52"
