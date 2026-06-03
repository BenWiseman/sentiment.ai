# sentiment.ai — Python

TensorFlow-free sentiment analysis from sentence embeddings. The Python sibling of
the R package [`sentiment.ai`](../rpackage), sharing the **same trained scorer
artifacts** and the **same public API**.

> **Status: scaffold.** The package structure, model registry, and API signatures
> are in place; the function bodies are stubs (`NotImplementedError`). They get
> populated once the R v2 package is locked, so the two stay 1:1.

Why a Python package: the v2 engine is already Python (sentence-transformers +
xgboost). The R package reaches it through `reticulate`; Python calls it directly —
strictly less machinery, no bridge, and xgboost runs multithreaded here.

## Models (provisional bake-off macro-F1, 2026-06-03)

| `model=` | macro-F1 | dim | notes |
|---|---|---|---|
| `e5-small` *(default)* | 0.813 | 384 | tiny, fast, ~100 languages, no TF |
| `e5-base` | 0.860 | 768 | best on-device — ties OpenAI, ~100 languages, no TF |
| `openai` | 0.861 | 1536 | best overall, paid API |
| `en` / `en.large` / `multi` / `multi.large` | legacy | 512 | opt-in, **requires TensorFlow** |

Numbers are subsample figures and get replaced by full-data results.

## Intended API (mirrors R)

```python
import sentimentai as sa
sa.sentiment_score(["I love this", "this is terrible"])   # -> [~+1, ~-1]
sa.sentiment_match(texts, phrases={"positive": [...], "negative": [...]})
sa.embed_text(texts, model="e5-small")
```

## R ↔ Python parity map

| R (`sentiment.ai`) | Python (`sentimentai`) | status |
|---|---|---|
| `embed_text()` | `embed_text()` | stub |
| `sentiment_score()` | `sentiment_score()` | stub |
| `sentiment_match()` | `sentiment_match()` | stub |
| `install_sentiment.ai()` / `init_sentiment.ai()` | `ensure_model()` | stub (no reticulate dance) |
| `default_models`, `model="en.large"` | `BACKENDS`, `model="e5-small"` | done (registry) |

## Layout

```
pypackage/
├── pyproject.toml
├── sentimentai/
│   ├── __init__.py     public API re-exports
│   ├── _models.py      backend registry (done)
│   ├── embedding.py    embed_text()        (stub)
│   ├── sentiment.py    sentiment_score/match (stub)
│   └── install.py      ensure_model()      (stub)
└── tests/test_smoke.py registry/import tests pass; functional tests skipped
```

Model weights are **not** shipped in the wheel — the embedder (HuggingFace) and the
matching `.xgb` scorer (GitHub Releases) download on first use, same pattern as R.
