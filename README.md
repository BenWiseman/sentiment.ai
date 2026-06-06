![](https://github.com/BenWiseman/sentiment.ai/blob/main/repository-open-graph-template.png)



[![CRAN version](https://img.shields.io/cran/v/sentiment.ai?style=flat-square)](https://cran.r-project.org/package=sentiment.ai)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/last-month/sentiment.ai?color=brightgreen)](https://cran.r-project.org/package=sentiment.ai)
[![GitHub commits](https://img.shields.io/github/last-commit/BenWiseman/sentiment.ai)](https://GitHub.com/BenWiseman/sentiment.ai/commit/)
[![Repo Size](https://img.shields.io/github/repo-size/BenWiseman/sentiment.ai)]()
[![Languages](https://img.shields.io/github/languages/count/BenWiseman/sentiment.ai)]()
[![Top Language](https://img.shields.io/github/languages/top/BenWiseman/sentiment.ai)]()
[![License](https://img.shields.io/cran/l/sentiment.ai?style=flat-square)](https://cran.r-project.org/package=sentiment.ai)

# sentiment.ai

Sentiment analysis from sentence embeddings — multilingual, on-device, and
**TensorFlow-free** by default.

**See the project page:**
[https://benwiseman.github.io/sentiment.ai/](https://benwiseman.github.io/sentiment.ai/)

## Contributors
![GitHub Contributors Image](https://contrib.rocks/image?repo=BenWiseman/sentiment.ai)

> **v2.** The default backend is now the on-device multilingual model
> `multilingual-e5-small` (no TensorFlow, no API key). The legacy Universal Sentence
> Encoder models still work but are opt-in (`install_sentiment.ai(legacy = TRUE)`). A
> Python sibling, [`sentimentai-py`](https://pypi.org/project/sentimentai-py/), shares
> the same scoring heads.

# Quick start

```r
install.packages("sentiment.ai")        # from CRAN
library(sentiment.ai)

# one-time setup — TensorFlow-free, runs on your machine
install_sentiment.ai()   # interactive wizard walks you through it

# model loads on first use — no explicit init() needed
sentiment_score(c("I love this!", "this is terrible"))
#> [1]  0.9  -0.9   (about 1 = positive, about -1 = negative)

# tidy output with class probabilities and calibrated confidence
sentiment(c("I love this!", "it's fine", "this is terrible"))
#>               text sentiment    class confidence
#> 1     I love this!      0.95 positive       0.96
#> 2        it's fine      0.10  neutral       0.74
#> 3 this is terrible     -0.93 negative       0.95
```

No TensorFlow, no API key, and the model runs on your machine.

# Overview

`sentiment.ai` turns text into a sentiment score by (1) embedding it with a
sentence-embedding model and (2) running a small, bundled scoring head over that
embedding. Compared with lexicon/dictionary methods this:

1. **Is more robust** — tolerates spelling mistakes and mixed case, and the default
   model handles **~100 languages**.
2. **Doesn't need a rigid lexicon** — text becomes an embedding vector, so you get
   sensible scores for words and phrasings no dictionary lists.
3. **Lets you choose the context** — with `sentiment_match()` you define what
   *positive* and *negative* mean for your domain.
4. **Is auditable** — scoring is deterministic, and `sentiment_provenance()` reports
   exactly which model and scoring head produced a score.

# Models

| `model`        | dim  | notes                                                            |
|:---------------|:----:|:-----------------------------------------------------------------|
| `e5-small`     | 384  | **default** — tiny, fast, ~100 languages, on-device, no TensorFlow |
| `e5-base`      | 768  | best on-device quality, ~100 languages, no TensorFlow            |
| `openai`       | 1536 | `text-embedding-3-small` — paid API, text leaves your machine    |
| `en` / `en.large` / `multi` / `multi.large` | 512 | legacy Universal Sentence Encoder — **opt-in, requires TensorFlow** |

Scoring head: `"mlp"` (default) or `"logistic"` — small pure-R JSON heads, **no
xgboost and no TensorFlow at score time**.

# Benchmark

On a public test mix (Amazon / IMDB / tweets / financial-news reviews + GPT synthetic;
**no proprietary data**), scored with one identical XGBoost recipe per embedder, on the
**real-only** slice (n = 1,247): `e5-base` reaches macro-F1 **0.899** and **94%**
directional accuracy on real positive/negative reviews — tied with paid OpenAI
(0.886 / 94%); the default `e5-small` is **0.854 / 89%**, level with the old TensorFlow
USE default it replaces. So the TensorFlow-free default *matches* the old one on
accuracy while dropping all of TensorFlow. (Real *neutral* text is scarce in the
benchmark, so pos/neg accuracy is the most reliable read; see `NEWS.md` for the full
table.)

# Why not just use tidytext / vader / sentimentr?

Lexicon-based tools are fast and easy to inspect, but they have a hard ceiling: they
score **words in a fixed dictionary**, so out-of-vocabulary terms, misspellings, and
new phrasings get missed; negation and context are handled only by hand-written rules;
and most convenient lexicons are **English-only**. `sentiment.ai` avoids those limits
because it never looks at individual words — it maps the whole sentence to an embedding
vector and then classifies that vector:

- **Handles ~100 languages** from a single model (no separate multilingual lexicons).
- **Scores phrases a dictionary has never seen** — slang, domain jargon, product names.
- **Explicitly models neutral** as a third class (not just "not positive, not negative").
- **Calibrated confidence** — the `confidence` value from `sentiment()` matches
  empirical accuracy (ECE ≈ 0.015), so you can triage on it.
- **Tunable context** — `sentiment_match()` lets you define what *positive* and
  *negative* mean for your domain instead of accepting a generic polarity scale.

The tradeoff is setup: lexicon tools install with no Python dependency; `sentiment.ai`
needs a one-time `install_sentiment.ai()` to download a ~120 MB on-device model. After
that there is no API key, no internet connection, and scoring is deterministic across
machines.

# Sentiment analysis

`sentiment_score()` returns one score per input in `[-1, 1]`. `sentiment_match()` returns
that **same calibrated score** plus a nearest-phrase explanation against **tunable poles**
(`phrase` / `class` / `similarity`) — the poles only shape the explanation, never the score.
Omit `phrases` to use the bundled, balanced 40/40 default poles:

```r
sentiment_match(c("great service", "lost my bag"),
                phrases = list(positive = c("on time", "friendly"),
                               negative = c("delayed", "rude")))
```

`sentiment_provenance("e5-small")` prints the exact model, license, source, prefix, and
scoring head behind a score.

# Python sibling

The same engine is on PyPI as
[`sentimentai-py`](https://pypi.org/project/sentimentai-py/) — same scoring heads, a
forward pass verified bit-for-bit against this package:

```bash
pip install --pre sentimentai-py
```

# Contribute a scoring head

Scoring heads turn an embedding into a sentiment score; you can train and contribute
your own under `scoring/<type>/<version>/<model>.json` (e.g.
`scoring/mlp/1.0/e5-small.json`). A head is a small JSON describing the forward pass,
read by a pure-R scorer — no xgboost or TensorFlow at score time.

---

Originally created by the Korn Ferry Institute AITMI team.
