"""Locks the Python scoring engine to R `score_json_head`.

The fixture holds real e5-small embeddings and the GOLDEN scores produced by the R
package's score_json_head on the same embeddings + the same shipped head. The numpy
engine must reproduce them. (When this was first measured the max |python - R| across
60 encoder x scoring x row cases was 4.4e-16 — machine epsilon.) Needs neither R nor
sentence-transformers, so it runs anywhere.
"""
import json
from pathlib import Path

import numpy as np

from sentimentai import _scoring

FIXTURE = Path(__file__).parent / "fixtures" / "parity_e5-small_mlp.json"


def _load():
    return json.loads(FIXTURE.read_text())


def test_packaged_head_matches_r_golden():
    fx = _load()
    X = np.asarray(fx["embeddings"], dtype=np.float64)
    got = _scoring.score(X, model=fx["model"], scoring=fx["scoring"], version=fx["version"])
    want = np.asarray(fx["expected"], dtype=np.float64)
    assert got.shape == want.shape
    np.testing.assert_allclose(got, want, atol=1e-10, rtol=0,
                               err_msg="numpy engine diverged from R score_json_head golden scores")


def test_scores_are_in_range_and_signed():
    fx = _load()
    X = np.asarray(fx["embeddings"], dtype=np.float64)
    s = _scoring.score(X, model=fx["model"], scoring=fx["scoring"])
    assert np.all(s >= -1.0) and np.all(s <= 1.0)
    # negative-labelled rows are negatively oriented AS A CLASS (placeholder head ~0.84 F1,
    # so a single noisy/ambiguous item may flip — class mean is the robust check).
    labels = np.asarray(fx["labels"])
    assert s[labels == -1].mean() < 0.0


def test_load_logistic_head_too():
    # both shipped head types load and score without xgboost/torch/TF
    X = np.asarray(_load()["embeddings"], dtype=np.float64)
    s = _scoring.score(X, model="e5-small", scoring="logistic")
    assert s.shape[0] == X.shape[0]
