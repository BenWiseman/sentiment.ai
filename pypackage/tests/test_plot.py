"""plot_sentiment(): the interactive sentiment map.

Hermetic — synthetic embeddings clustered around 3 topic centroids + precomputed
sentiment rows, so no model download and no network. Locks the figure type, the returned
data shape, deterministic c-TF-IDF labels, the empty-text drop, and the classifier guard.
"""
import numpy as np
import pytest

import sentimentai

pytest.importorskip("plotly")
pytest.importorskip("sklearn")
pytest.importorskip("pandas")


def _synthetic(seed=0):
    rng = np.random.RandomState(seed)
    topics = {
        "delays": ["flight delayed three hours", "stuck on the tarmac", "missed my connection",
                   "late departure again", "hours of waiting", "boarding was so late"],
        "staff":  ["the crew were lovely", "friendly cabin staff", "rude gate agent",
                   "helpful attendant", "staff ignored us", "warm welcome aboard"],
        "food":   ["the meal was tasty", "cold bland dinner", "great coffee onboard",
                   "no vegetarian option", "delicious snack", "soggy sandwich"],
    }
    cent = rng.standard_normal((3, 384))
    texts, embs, rows = [], [], []
    for ti, ex in enumerate(topics.values()):
        for t in ex:
            v = cent[ti] + 0.3 * rng.standard_normal(384)
            v /= np.linalg.norm(v)
            embs.append(v)
            texts.append(t)
            s = float(np.clip(rng.standard_normal() * 0.5, -1, 1))
            cls = "positive" if s > 0.15 else ("negative" if s < -0.15 else "neutral")
            rows.append({"sentiment": s, "class": cls, "hate_speech": False,
                         "mixed": False, "style": "informal"})
    return texts, np.array(embs), rows


def test_returns_figure_and_data():
    import plotly.graph_objects as go
    texts, embs, rows = _synthetic()
    fig, df = sentimentai.plot_sentiment(texts, embeddings=embs, rows=rows,
                                         reducer="pca", labels="tfidf", return_data=True)
    assert isinstance(fig, go.Figure)
    assert len(df) == len(texts)
    assert {"x", "y", "text", "sentiment", "class", "cluster", "cluster_label"} <= set(df.columns)
    assert df.cluster.nunique() >= 2                      # 3 separable topics
    assert all(isinstance(s, str) and s for s in df.cluster_label)
    assert len(fig.layout.annotations) == df.cluster.nunique()   # one label per cluster


def test_color_by_class_builds():
    texts, embs, rows = _synthetic()
    fig = sentimentai.plot_sentiment(texts, embeddings=embs, rows=rows,
                                     reducer="pca", color_by="class", labels="none")
    assert len(fig.data) >= 1


def test_labels_are_deterministic():
    texts, embs, rows = _synthetic()
    _, d1 = sentimentai.plot_sentiment(texts, embeddings=embs, rows=rows,
                                       reducer="pca", return_data=True)
    _, d2 = sentimentai.plot_sentiment(texts, embeddings=embs, rows=rows,
                                       reducer="pca", return_data=True)
    assert list(d1.cluster_label) == list(d2.cluster_label)   # c-TF-IDF is deterministic


def test_drops_empty_texts():
    texts, embs, rows = _synthetic()
    texts2 = list(texts)
    texts2[0] = ""                                        # one empty input
    with pytest.warns(UserWarning):
        fig, df = sentimentai.plot_sentiment(texts2, embeddings=embs, rows=rows,
                                             reducer="pca", return_data=True)
    assert len(df) == len(texts) - 1


def test_classifier_model_raises():
    texts, _, _ = _synthetic()
    with pytest.raises(ValueError):
        sentimentai.plot_sentiment(texts, model="twitter-roberta")   # no embedding space
