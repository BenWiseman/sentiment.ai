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
with no API key and no data leaving your machine. When you need the last few points of
in-domain accuracy, opt into the bundled fine-tuned transformers (`max-english`,
`max-multilingual`) behind the same one-line API. We publish honest benchmarks, including
where the big transformers win, on the project page.

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
use_profile("max-english")    # best English accuracy, sentiment only (RoBERTa)
```

| profile             | model             | what you get                                              |
|:--------------------|:------------------|:----------------------------------------------------------|
| `multilingual` *(default)* | `e5-base`  | best on-device quality, ~100 languages, **+ flags**       |
| `lightest`          | `e5-small`        | smaller and faster, ~100 languages, on-device, **+ flags** |
| `max-english`       | `twitter-roberta` | best English accuracy (opt-in ~500 MB transformer)        |
| `max-multilingual`  | `xlm-roberta`     | best multilingual accuracy (opt-in ~1 GB transformer)     |

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
full model (500 MB to 1 GB) on first use. On CPU they run at roughly `e5-base` speed and
about 2.7x slower than the lighter `e5-small` (see the speed panel below). Reach for them
when in-domain accuracy is the priority and a label is all you need.

# Benchmarks

![sentiment.ai benchmarks: accuracy across SemEval and airline tweets, and CPU throughput, comparing e5-small, e5-base, the transformer backends, and lexicon baselines.](docs/benchmark.png)

3-class macro-F1 (negative / neutral / positive). All benchmarks run locally on public
data, no proprietary data:

| model | SemEval-2017 | Airline tweets | Multilingual (8 lang) |
|:------|:------------:|:--------------:|:---------------------:|
| `twitter-roberta` (opt-in) | **0.724** | **0.761** | English only |
| `xlm-roberta` (opt-in) | n/a | n/a | **0.701** |
| **`e5-base` (default)** | 0.672 | 0.651 | 0.574 |
| `openai` (paid) | 0.622 | 0.634 | n/a |
| `e5-small` | 0.587 | 0.581 | 0.466 |
| VADER | 0.529 | 0.457 | collapses off English |
| distilBERT-SST2 | 0.383 | 0.509 | English only |

**The honest read.** A frozen embedding plus a tiny scoring head does not out-accuracy a
125M-parameter fine-tuned transformer, and we do not claim it does: that is exactly why the
transformers ship as the opt-in `max-english` / `max-multilingual` backends. What the e5
heads win on is everything else: on-device `e5-base` **matches the paid OpenAI embedding**,
beats distilBERT on reviews, crushes the lexicon tools, covers ~100 languages with no heavy
download, runs far faster on CPU, keeps your data on the machine, and carries the
hate/mixed/style flags. Pick the trade-off you need with one `model =` or `use_profile()`
call.

(On a separate broad mixed-domain held-out set of Amazon / IMDB / financial-news / tweet
reviews, n = 1,247, `e5-base` reaches macro-F1 0.90 and `e5-small` 0.86, where a
Twitter-tuned model is out of its domain. See `NEWS.md` for the full tables.)

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
