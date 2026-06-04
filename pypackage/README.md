# sentiment.ai — Python

TensorFlow-free sentiment analysis from sentence embeddings. The Python sibling of
the R package [`sentiment.ai`](../rpackage), sharing the **same trained scorer
artifacts** and the **same public API**.

> **Status: working pre-release (0.1.0a1).** The engine is implemented and verified
> end-to-end (e5 embed → numpy scoring head → score in `[-1, 1]`); the forward pass
> matches the R package's `score_json_head` to machine epsilon (max diff `4.4e-16`).
> The shipped e5 heads are **placeholders** pending full-data weights — treat scores as
> indicative until the full-data weights land.

## Install

```bash
pip install --pre sentimentai-py            # alpha pre-release
pip install --pre "sentimentai-py[openai]"  # + optional OpenAI backend
```

`--pre` is required while this is an alpha. The **import** name is `sentimentai`.

## Quick start

```python
import sentimentai as sa

sa.sentiment_score(["I love this", "this is terrible"])
# array([ 0.9, -0.9])   # about 1 = positive, about -1 = negative

# define what positive / negative mean for your domain (tunable poles)
sa.sentiment_match(["great value", "broke instantly"],
                   phrases={"positive": ["high quality"], "negative": ["low quality"]})

# or just get embeddings
sa.embed_text(["dogs", "cats"], model="e5-small")   # (2, 384)
```

The first call downloads the e5 model from HuggingFace (cached afterwards); the small
scoring heads ship inside the wheel.

Why a Python package: the v2 engine is already Python (sentence-transformers + a tiny
numpy scoring head). The R package reaches it through `reticulate`; Python calls it
directly — strictly less machinery, no bridge, **no TensorFlow, and no xgboost at serve**.

## Models (provisional bake-off macro-F1, 2026-06-03)

| `model=` | macro-F1 | dim | notes |
|---|---|---|---|
| `e5-small` *(default)* | 0.813 | 384 | tiny, fast, ~100 languages, no TF |
| `e5-base` | 0.860 | 768 | best on-device, ~100 languages, no TF |
| `openai` | 0.861 | 1536 | best overall, paid API |
| `en` / `en.large` / `multi` / `multi.large` | legacy | 512 | opt-in, **requires TensorFlow** |

Numbers are subsample figures and get replaced by full-data results.

## R ↔ Python parity map

| R (`sentiment.ai`) | Python (`sentimentai`) | status |
|---|---|---|
| `embed_text()` | `embed_text()` | done (e5 / openai; legacy TF raises) |
| `sentiment_score()` | `sentiment_score()` | done (mlp / logistic heads) |
| `sentiment_match()` | `sentiment_match()` | done (tunable phrase poles) |
| `score_json_head()` | `_scoring.score()` | done — verified bit-identical to R |
| `install_sentiment.ai()` / `init_sentiment.ai()` | `ensure_model()` | done (no reticulate dance) |
| `default_models`, `model="en.large"` | `BACKENDS`, `model="e5-small"` | done (registry) |

## Layout

```
pypackage/
├── pyproject.toml
├── sentimentai/
│   ├── __init__.py     public API re-exports
│   ├── _models.py      backend registry (done)
│   ├── embedding.py    embed_text()        (e5 / openai)
│   ├── sentiment.py    sentiment_score / sentiment_match
│   ├── _scoring.py     numpy scoring head  (verified vs R)
│   ├── install.py      ensure_model()
│   └── scoring/        JSON heads shipped in the wheel
└── tests/               parity (vs R golden) + smoke/registry tests
```

The small JSON scoring heads **ship inside the wheel** (`sentimentai/scoring/`); only the
on-device embedder downloads from HuggingFace on first use. No `.xgb`, no TensorFlow.
