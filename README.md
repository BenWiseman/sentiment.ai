![](https://github.com/BenWiseman/sentiment.ai/blob/main/repository-open-graph-template.png)



[![CRAN version](https://img.shields.io/cran/v/sentiment.ai?style=flat-square)](https://cran.r-project.org/package=sentiment.ai)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/last-month/sentiment.ai?color=brightgreen)](https://cran.r-project.org/package=sentiment.ai)
[![GitHub commits](https://img.shields.io/github/last-commit/BenWiseman/sentiment.ai)](https://GitHub.com/BenWiseman/sentiment.ai/commit/)
[![Repo Size](https://img.shields.io/github/repo-size/BenWiseman/sentiment.ai)]()
[![Languages](https://img.shields.io/github/languages/count/BenWiseman/sentiment.ai)]()
[![Top Language](https://img.shields.io/github/languages/top/BenWiseman/sentiment.ai)]()
[![License](https://img.shields.io/cran/l/sentiment.ai?style=flat-square)](https://cran.r-project.org/package=sentiment.ai)

# sentiment.ai

**The most complete on-device sentiment toolkit in the R/Python ecosystem** — tiny by
default, with hate-speech / mixed / style flags, intent-based profiles, an interactive
sentiment map, and opt-in transformer backends for when you want maximum accuracy.

On-device `e5-base` **matches the paid OpenAI embedding** (`text-embedding-3-small`) on
our benchmarks — that quality, free, across **~100 languages**, with no API key and no
data leaving your machine. When you need the last few points of *in-domain* accuracy,
opt into the bundled fine-tuned transformers (`max-english` / `max-multilingual`) behind
the same one-line API. We publish **honest benchmarks — including where the big
transformers win** — on the project page.

**See the project page:**
[https://benwiseman.github.io/sentiment.ai/](https://benwiseman.github.io/sentiment.ai/)

![A sentiment map: every comment embedded, projected to 2-D, coloured by sentiment, with auto-labelled clusters.](docs/sentiment-map.png)

## Contributors
![GitHub Contributors Image](https://contrib.rocks/image?repo=BenWiseman/sentiment.ai)

> **v2.** TensorFlow-free by default. Calibrated confidence and a tidy 3-class output.
> **Hate-speech / mixed / style flags** from the same embedding. **Intent-based profiles**
> (`use_profile()`, plus an interactive `setup()` in Python) so you never learn the model
> zoo. **Opt-in transformer
> backends** (RoBERTa / XLM-R) for max accuracy — *if you can't beat 'em, join 'em*.
> An interactive **`plot_sentiment()`** map. Diagnostic signals for when *not* to trust a
> score, and agreement statistics for methods sections. A Python sibling
> ([`sentimentai-py`](https://pypi.org/project/sentimentai-py/)) shares the same scoring
> heads, verified bit-for-bit.

# Quick start

```r
install.packages("sentiment.ai")        # from CRAN
library(sentiment.ai)

# one-time setup — TensorFlow-free, runs on your machine
install_sentiment.ai()   # interactive wizard walks you through it

# model loads on first use — no explicit init() needed
sentiment_score(c("I love this!", "this is terrible"))
#> [1]  1.00 -0.99   (about 1 = positive, about -1 = negative)

# tidy output with class probabilities and calibrated confidence
sentiment(c("I love this!", "The package arrived on Tuesday afternoon.", "this is terrible"))
#>                                        text sentiment    class confidence
#> 1                              I love this!      1.00 positive       1.00
#> 2 The package arrived on Tuesday afternoon.      0.13  neutral       0.86
#> 3                          this is terrible     -0.99 negative       0.99
```

No TensorFlow, no API key, and the model runs on your machine.

# Pick a profile (or just go with the default)

You don't have to learn the model zoo. A **profile** maps a plain intent to a backend and
makes it the default; the choice persists across sessions.

```r
sentiment_profiles()          # see the options
use_profile("multilingual")   # best on-device multilingual, WITH flags  (e5-base)
use_profile("max-english")    # best English accuracy, sentiment only     (RoBERTa)
```

| profile             | model             | what you get                                              |
|:--------------------|:------------------|:----------------------------------------------------------|
| `lightest` *(default)* | `e5-small`     | tiny, instant, ~100 languages, on-device, **+ flags**     |
| `multilingual`      | `e5-base`         | stronger on-device multilingual, **+ flags**              |
| `max-english`       | `twitter-roberta` | best English accuracy (opt-in ~500 MB transformer)        |
| `max-multilingual`  | `xlm-roberta`     | best multilingual accuracy (opt-in ~1 GB transformer)     |

# Safety & style flags

For the on-device e5 models, `sentiment()` adds three post-processing flags computed from
the **same embedding** — no second model, no extra download:

```r
sentiment("go back to your country, you filth, nobody wants you")
#>  ... class = negative ... hate_speech = TRUE  p_hate = 0.79
```

- **`hate_speech`** / **`p_hate`** — hate-speech detector (AUROC ≈ 0.95–0.97; tuned to
  ~0.90 recall with a very low false-positive rate on normal text).
- **`mixed`** — flags neutral-looking rows that carry competing positive *and* negative signal.
- **`style`** — top writing style (analytical / descriptive / formal / informal / inquisitive).

These ship for `e5-small` / `e5-base` / `openai`. (Honest limits: the `mixed` head is
trained on explicit mixed text and can miss terse contrastive phrasing; slur-free
dehumanising hate can fall just under threshold — lower it if you need higher recall.)

# If you can't beat 'em, join 'em — transformer backends

Fine-tuned transformers still lead **in-domain** accuracy, so rather than overclaim, we
ship them as **opt-in backends** behind the same API:

```r
sentiment_score("the gate agent was incredible", model = "twitter-roberta")  # English
sentiment_score("le service était incroyable",   model = "xlm-roberta")       # multilingual
```

They download a full transformer on first use and produce **sentiment only** (no flags —
those need the e5 embedding space). The default stays tiny, on-device, and TF-free.

# Map your whole corpus — `plot_sentiment()`

One picture: every comment embedded, projected to 2-D, coloured by sentiment, with the
full text on hover and **human-readable cluster labels** (deterministic c-TF-IDF, or pass
`labels = "openai"` to spend a fraction of a cent on tidier topics).

```r
p <- plot_sentiment(reviews$text)          # interactive plotly widget
htmlwidgets::saveWidget(p, "map.html")
```

(Plotting uses `plotly`; `uwot` / `Rtsne` give nicer projections if installed, otherwise
it falls back to PCA. In Python: `pip install "sentimentai-py[plot]"`.)

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
| `twitter-roberta` | —  | opt-in end-to-end English RoBERTa — max in-domain accuracy (~500 MB) |
| `xlm-roberta`  |  —   | opt-in end-to-end multilingual XLM-R — max accuracy (~1 GB)       |
| `en` / `en.large` / `multi` / `multi.large` | 512 | legacy Universal Sentence Encoder — **opt-in, requires TensorFlow** |

Scoring head: `"mlp"` (default) or `"logistic"` — small pure-R JSON heads, **no
xgboost and no TensorFlow at score time**.

# Benchmark

On a public test mix (Amazon / IMDB / tweets / financial-news reviews + GPT synthetic;
**no proprietary data**), on the **real-only** slice (n = 1,247): `e5-base` reaches
macro-F1 **0.90** and **94%** directional accuracy on real positive/negative reviews;
`oai_3_small` is close behind (0.903 / 94%); the default `e5-small` is **0.860 / 90%**,
comfortably above the old TensorFlow USE default it replaces. (Real *neutral* text is
scarce in the benchmark, so pos/neg accuracy is the most reliable read; see `NEWS.md`
for the full table.)

**Where the big transformers win — and why we ship them.** On *in-domain* tweet
benchmarks the fine-tuned transformers lead: cardiffnlp Twitter-RoBERTa scores macro-F1
0.72 / 0.76 on SemEval-2017 / airline vs `e5-base` 0.67 / 0.65, and XLM-R leads
multilingual (0.70 vs 0.57). A frozen-embedding + tiny-MLP head doesn't out-accuracy a
125M end-to-end fine-tuned model, and we don't claim it does — instead we ship those
transformers as the opt-in `max-english` / `max-multilingual` backends. Our heads win on
**footprint, speed, privacy, multilingual coverage without a heavy download, and the
hate / mixed / style flags**; on-device `e5-base` also **matches the paid OpenAI
embedding**. Pick the trade-off you need with one `model=` / `use_profile()` call.

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
