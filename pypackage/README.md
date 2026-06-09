# sentiment.ai — Python

TensorFlow-free sentiment analysis from sentence embeddings. The Python sibling of
the R package [`sentiment.ai`](../rpackage), sharing the **same trained scorer
artifacts** and the **same public API**.

> **Status: working pre-release (0.2.0a1).** Engine verified end-to-end (e5 embed →
> numpy scoring head → score in `[-1, 1]`), bit-for-bit against the R package's
> `score_json_head` (max diff `4.4e-16`). **v2** adds **hate / mixed / style flags**,
> **intent-based profiles** (`setup()` / `use_profile()`), **opt-in transformer backends**
> (RoBERTa / XLM-R — *if you can't beat 'em, join 'em*), and an interactive
> **`plot_sentiment()`** map. Shipped e5 heads: e5-small **0.86** / e5-base **0.90**
> real-only macro-F1.

![A sentiment map: every comment embedded, projected to 2-D, coloured by sentiment, with auto-labelled clusters.](https://raw.githubusercontent.com/BenWiseman/sentiment.ai/main/docs/sentiment-map.png)

## Install

```bash
pip install --pre sentimentai-py            # alpha pre-release
pip install --pre "sentimentai-py[openai]"  # + optional OpenAI embedding backend
pip install --pre "sentimentai-py[plot]"    # + interactive plot_sentiment() map
```

`--pre` is required while this is an alpha. The **import** name is `sentimentai`.

## Quick start

```python
import sentimentai as sa

sa.sentiment_score(["I love this", "this is terrible"])
# array([ 1.  , -0.99])   # about 1 = positive, about -1 = negative

# the whole picture, not just the scalar: one dict per row with text, sentiment,
# prob_neg / prob_neu / prob_pos, class, and confidence. Use it when the neutral mass
# matters or to triage low-confidence rows.
sa.sentiment(["I love this", "The package arrived on Tuesday afternoon.", "this is terrible"])

# same calibrated score, plus a nearest-phrase explanation (text/sentiment/phrase/
# class/similarity). Pass tunable poles to define what they mean for your domain,
# or omit `phrases` for the bundled balanced 40/40 defaults — the score is identical
# to sentiment_score() either way; poles only shape the explanation.
sa.sentiment_match(["great value", "broke instantly"],
                   phrases={"positive": ["high quality", "great value", "works well"],
                            "negative": ["low quality", "broke quickly", "poor build"]})
# the `sentiment` score is always the reliable signal; give each pole >=3 phrases so the
# nearest-phrase explanation is stable (single-phrase poles can mislabel the phrase)

# or just get embeddings
sa.embed_text(["dogs", "cats"], model="e5-small")   # (2, 384)

# --- v2 ---------------------------------------------------------------------
# sentiment() also returns hate_speech / p_hate / mixed / style flags for e5 models,
# computed from the SAME embedding (no second model, no extra download).
sa.sentiment(["go back to your country, you filth, nobody wants you"])[0]   # ... 'hate_speech': True, 'p_hate': 0.79 ...

# pick a backend by INTENT (persists across sessions) instead of memorising model names:
sa.use_profile("multilingual")    # e5-base, with flags
sa.setup()                         # interactive one-time picker

# if you can't beat 'em, join 'em — opt-in fine-tuned transformer, same API:
sa.sentiment_score(["the gate agent was incredible"], model="twitter-roberta")  # English
sa.sentiment_score(["le service était incroyable"], model="xlm-roberta")        # multilingual

# map a whole corpus: embed -> 2-D -> colour by sentiment, hover text, auto cluster labels
fig = sa.plot_sentiment(reviews)             # needs sentimentai-py[plot]
fig.write_html("map.html")                    # or fig.show() in a notebook
```

The first call downloads the e5 model from HuggingFace (cached afterwards); the small
scoring heads ship inside the wheel.

Why a Python package: the v2 engine is already Python (sentence-transformers + a tiny
numpy scoring head). The R package reaches it through `reticulate`; Python calls it
directly — strictly less machinery, no bridge, **no TensorFlow, and no xgboost at serve**.

## Models (real-only macro-F1, n = 1,247)

| `model=` | macro-F1 | dim | notes |
|---|---|---|---|
| `e5-small` *(default)* | 0.86 | 384 | tiny, fast, ~100 languages, no TF, **+ flags** |
| `e5-base` | 0.90 | 768 | best on-device, ~100 languages, no TF, **+ flags** |
| `openai` | 0.89 | 1536 | paid API (needs a key), **+ flags** |
| `twitter-roberta` | — | — | opt-in end-to-end English RoBERTa; **leads in-domain** (~500 MB) |
| `xlm-roberta` | — | — | opt-in end-to-end multilingual XLM-R; leads multilingual (~1 GB) |
| `en` / `en.large` / `multi` / `multi.large` | legacy | 512 | opt-in, **requires TensorFlow** |

Honest picture: on *in-domain* tweet benchmarks the fine-tuned transformers win (RoBERTa
0.72/0.76 SemEval/airline vs e5-base 0.67/0.65; XLM-R 0.70 multilingual vs 0.57) — so we
**ship them as opt-in backends** rather than overclaim. On-device `e5-base` matches the
paid OpenAI embedding. Full method + the synthetic-vs-real breakdown are in the R
package's `NEWS.md` / `planning/benchmark-v2.md`.

## R ↔ Python parity map

| R (`sentiment.ai`) | Python (`sentimentai`) | status |
|---|---|---|
| `embed_text()` | `embed_text()` | done (e5 / openai; legacy TF raises) |
| `sentiment_score()` | `sentiment_score()` | done (mlp / logistic heads) |
| `sentiment()` | `sentiment()` | done (3-class tidy output) |
| `sentiment_match()` | `sentiment_match()` | done (tunable phrase poles) |
| `score_json_head()` | `_scoring.score()` | done — verified bit-identical to R |
| `sentiment()` flags (hate/mixed/style) | `sentiment()` flag keys | done — aux heads, same thresholds |
| `use_profile()` / `setup_sentiment.ai()` | `use_profile()` / `setup()` | done — profiles + persisted default |
| `plot_sentiment()` | `plot_sentiment()` | done — plotly map, c-TF-IDF / OpenAI labels |
| `model="twitter-roberta"` / `"xlm-roberta"` | same handles | done — opt-in transformer backends |
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
│   ├── sentiment.py    sentiment_score / sentiment / sentiment_match
│   ├── _scoring.py     numpy scoring head  (verified vs R)
│   ├── _profiles.py    profiles + persisted default (use_profile)
│   ├── plot.py         plot_sentiment()    (optional: sentimentai-py[plot])
│   ├── install.py      ensure_model() / setup()
│   └── scoring/        JSON heads shipped in the wheel
└── tests/               parity (vs R golden) + smoke/registry/plot tests
```

> **Pre-release.** `--pre` is required only while this is an alpha; once `1.0` ships,
> plain `pip install sentimentai-py` will fetch it. The import name stays `sentimentai`.

The small JSON scoring heads **ship inside the wheel** (`sentimentai/scoring/`); only the
on-device embedder downloads from HuggingFace on first use. No `.xgb`, no TensorFlow.
