![](https://github.com/BenWiseman/sentiment.ai/blob/main/repository-open-graph-template.png)



[![CRAN version](https://img.shields.io/cran/v/sentiment.ai?style=flat-square)](https://cran.r-project.org/package=sentiment.ai)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/last-month/sentiment.ai?color=brightgreen)](https://cran.r-project.org/package=sentiment.ai)
[![GitHub commits](https://img.shields.io/github/last-commit/BenWiseman/sentiment.ai)](https://GitHub.com/BenWiseman/sentiment.ai/commit/)
[![Repo Size](https://img.shields.io/github/repo-size/BenWiseman/sentiment.ai)]()
[![Languages](https://img.shields.io/github/languages/count/BenWiseman/sentiment.ai)]()
[![Top Language](https://img.shields.io/github/languages/top/BenWiseman/sentiment.ai)]()
[![License](https://img.shields.io/cran/l/sentiment.ai?style=flat-square)](https://cran.r-project.org/package=sentiment.ai)

# sentiment.ai

**The most complete on-device sentiment toolkit in the R/Python ecosystem.** It ships
hate-speech / mixed / style flags, intent-based profiles, an interactive sentiment map, and
opt-in transformer backends for when you want maximum accuracy.

The default model, on-device `e5-base`, **matches the paid OpenAI embedding**
(`text-embedding-3-small`) on our benchmarks: that quality, free, across **~100 languages**,
with no API key and no data leaving your machine. When your text is tweets or other short
social posts and you want the last few points of accuracy, opt into the bundled fine-tuned
transformers (`max-english`, `max-multilingual`) behind the same one-line API. We publish
honest benchmarks, including where they win, on the project page.

**See the project page:**
[https://benwiseman.github.io/sentiment.ai/](https://benwiseman.github.io/sentiment.ai/)

## Contributors
![GitHub Contributors Image](https://contrib.rocks/image?repo=BenWiseman/sentiment.ai)

> **v2.** TensorFlow-free by default. Calibrated confidence and a tidy 3-class output.
> **Hate-speech / mixed / style flags** from the same embedding. **Intent-based profiles**
> (`use_profile()`, plus an interactive `setup()` in Python) so you never learn the model
> zoo. **Opt-in transformer backends** (RoBERTa, XLM-R) for maximum in-domain accuracy.
> An interactive **`plot_sentiment()`** map. Diagnostic signals for when not to trust a
> score, and agreement statistics for methods sections. A Python sibling
> ([`sentimentai-py`](https://pypi.org/project/sentimentai-py/)) shares the same scoring
> heads, verified bit-for-bit.

# Quick start

```r
install.packages("sentiment.ai")        # from CRAN
library(sentiment.ai)

# one-time setup, TensorFlow-free, runs on your machine
install_sentiment.ai()   # interactive wizard walks you through it

# model loads on first use, no explicit init() needed
sentiment_score(c("I love this!", "this is terrible"))
#> [1]  1.00 -1.00   (about 1 = positive, about -1 = negative)

# tidy output with class probabilities and calibrated confidence
sentiment(c("I love this!", "The package arrived on Tuesday afternoon.", "this is terrible"))
#>                                        text sentiment    class confidence
#> 1                              I love this!      1.00 positive       1.00
#> 2 The package arrived on Tuesday afternoon.      0.00  neutral       0.99
#> 3                          this is terrible     -1.00 negative       1.00
```

No TensorFlow, no API key, and the model runs on your machine.

# Pick a profile (or just use the default)

You do not have to learn the model zoo. A **profile** maps a plain intent to a backend and
makes it the default; the choice persists across sessions.

```r
sentiment_profiles()          # see the options
use_profile("lightest")       # smaller and faster on-device model (e5-small)
use_profile("max-english")    # strongest on tweets, sentiment only (RoBERTa)
```

| profile             | model             | what you get                                              |
|:--------------------|:------------------|:----------------------------------------------------------|
| `multilingual` *(default)* | `e5-base`  | best on-device quality, ~100 languages, **+ flags**       |
| `lightest`          | `e5-small`        | smaller and faster, ~100 languages, on-device, **+ flags** |
| `max-english`       | `twitter-roberta` | strongest on tweets / short social, English (opt-in ~500 MB) |
| `max-multilingual`  | `xlm-roberta`     | strongest on tweets / short social, multilingual (opt-in ~1 GB) |

# Safety & style flags

For the on-device e5 models, `sentiment()` adds three post-processing flags computed from
the **same embedding**: no second model, no extra download.

```r
sentiment("go back to your country, you filth, nobody wants you")
#>  ... class = negative ... hate_speech = TRUE  p_hate = 0.85
```

- **`hate_speech`** / **`p_hate`**: a hate-speech detector (AUROC 0.95 to 0.97; tuned to
  about 0.90 recall with a very low false-positive rate on normal text).
- **`mixed`**: flags neutral-looking rows that carry competing positive and negative signal.
- **`style`**: top writing style (analytical, descriptive, formal, informal, inquisitive).

These ship for `e5-small`, `e5-base`, and `openai`. (Honest limits: the `mixed` head is
trained on explicit mixed text and can miss terse contrastive phrasing; slur-free
dehumanising hate can fall just under threshold, so lower it if you need higher recall.)

# Map your whole corpus: `plot_sentiment()`

One picture: every comment embedded, projected to 2-D, coloured by sentiment, with the full
text on hover and **human-readable cluster labels** (deterministic c-TF-IDF, or pass
`labels = "openai"` to spend a fraction of a cent on tidier topics).

![A sentiment map of customer reviews: each point is one comment, positioned by a 2-D projection of its embedding, coloured from red (negative) to green (positive), grouped into auto-labelled clusters.](docs/sentiment-map.png)

```r
p <- plot_sentiment(reviews$text)          # interactive plotly widget
htmlwidgets::saveWidget(p, "map.html")
```

Plotting uses `plotly`; `uwot` or `Rtsne` give nicer projections if installed, otherwise it
falls back to PCA. In Python: `pip install "sentimentai-py[plot]"`.

# Transformer backends (opt-in)

Fine-tuned transformers lead in-domain accuracy, so rather than overclaim we ship them as
opt-in backends behind the same API:

```r
sentiment_score("the gate agent was incredible", model = "twitter-roberta")  # English
sentiment_score("le service etait incroyable",   model = "xlm-roberta")       # multilingual
```

They are a different tool, with real costs. They **produce sentiment only**: the
hate/mixed/style flags, `sentiment_match()`, `plot_sentiment()`, and `cosine_match()` all
need the e5 embedding space, and the transformers do not give you one. They also download a
full model (500 MB to 1 GB) on first use, and on CPU they are far slower than the light
`e5-small` (about the same speed as `e5-base`):

| backend | CPU throughput (texts/sec, higher is better) |
|:--------|:--------------------------------------------:|
| `e5-small` | ~850 |
| `e5-base` (default) | ~300 |
| `twitter-roberta` (opt-in) | ~310 |

Reach for them when tweet-style accuracy is the priority and a label is all you need.

# Benchmarks

Most people scoring sentiment are scoring reviews, tickets, and survey text, not tweets, so
we lead with general business text. All benchmarks run locally on public data, no
proprietary data.

![sentiment.ai benchmarks: macro-F1 on general business text (employee reviews) for every backend, plus a panel showing the fine-tuned transformer's lead appears only on tweets.](docs/benchmark.png)

**General business text** (employee reviews, macro-F1, n = 10,085):

| model | macro-F1 |
|:------|:--------:|
| `twitter-roberta` (opt-in transformer) | 0.909 |
| `openai` (paid embedding) | 0.896 |
| **`e5-base` (default, on-device)** | **0.888** |
| distilBERT-SST2 | 0.879 |
| `e5-small` (on-device) | 0.836 |
| VADER | 0.681 |
| TextBlob | 0.626 |

On real business text the on-device `e5-base` default lands within about two points of both
the paid OpenAI embedding and a 125M fine-tuned transformer, clears distilBERT, and sits 20
to 30 points above the lexicon tools. On a separate held-out set of general review text
(n = 19,547) the on-device heads reach macro-F1 0.93 (`e5-base`) and 0.94 (`e5-small`).

**Where the transformer pulls ahead: tweets.** On Twitter benchmarks the fine-tuned
`twitter-roberta` opens a real gap, because tweets are its training data:

| model | SemEval-2017 tweets | Airline tweets |
|:------|:-------------------:|:--------------:|
| `twitter-roberta` (opt-in) | **0.724** | **0.761** |
| `e5-base` (default) | 0.672 | 0.651 |
| `e5-small` | 0.587 | 0.581 |
| VADER | 0.529 | 0.457 |

If your text really is tweets, opt into the `max-english` backend. For everything else the
gap is small, and `e5-base` is the only option here that also covers ~100 languages, carries
the hate / mixed / style flags, gives you `sentiment_match()` and `plot_sentiment()`, keeps
your data on the machine, and stays free.

# Why not just use tidytext / vader / sentimentr?

Lexicon tools are fast and easy to inspect, but they have a hard ceiling: they score **words
in a fixed dictionary**, so out-of-vocabulary terms, misspellings, and new phrasings get
missed; negation and context are handled only by hand-written rules; and most convenient
lexicons are **English only**. `sentiment.ai` avoids those limits because it never looks at
individual words. It maps the whole sentence to an embedding vector and classifies that
vector:

- **Handles ~100 languages** from a single model (no separate multilingual lexicons).
- **Scores phrases a dictionary has never seen**: slang, domain jargon, product names.
- **Explicitly models neutral** as a third class, not just "not positive, not negative".
- **Calibrated confidence**: the `confidence` value from `sentiment()` tracks empirical
  accuracy (ECE about 0.015), so you can triage on it.
- **Tunable context**: `sentiment_match()` lets you define what *positive* and *negative*
  mean for your domain instead of accepting a generic polarity scale.

The trade-off is setup. Lexicon tools install with no Python dependency; `sentiment.ai`
needs a one-time `install_sentiment.ai()` to download an on-device model (about 280 MB for
the default `e5-base`, about 120 MB for `e5-small`). After that there is no API key, no
internet connection, and scoring is deterministic across machines.

# Sentiment analysis

`sentiment_score()` returns one score per input in `[-1, 1]`. `sentiment_match()` returns
that **same calibrated score** plus a nearest-phrase explanation against **tunable poles**
(`phrase`, `class`, `similarity`). The poles only shape the explanation, never the score.
Omit `phrases` to use the bundled, balanced 40/40 default poles:

```r
sentiment_match(c("The cabin crew were friendly and helpful", "My bag was lost and nobody helped"),
                phrases = list(positive = c("friendly", "on time", "helpful"),
                               negative = c("rude", "delayed", "lost luggage")))
#>                                       text sentiment       phrase    class similarity
#> 1 The cabin crew were friendly and helpful      0.30     friendly positive       0.84
#> 2        My bag was lost and nobody helped     -0.91 lost luggage negative       0.88
```

`sentiment_provenance("e5-base")` reports the exact model, license, source, prefix, and
scoring head behind a score.

# Python sibling

The same engine is on PyPI as
[`sentimentai-py`](https://pypi.org/project/sentimentai-py/): the same scoring heads, with a
forward pass verified bit-for-bit against this package.

```bash
pip install --pre sentimentai-py
```

# Contribute a scoring head

Scoring heads turn an embedding into a sentiment score; you can train and contribute your
own under `scoring/<type>/<version>/<model>.json` (for example
`scoring/mlp/2.0/e5-base.json`). A head is a small JSON describing the forward pass, read by
a pure-R scorer, with no xgboost or TensorFlow at score time.

---

Originally created by the Korn Ferry Institute AITMI team. Now maintained by Linnet Labs.
