# sentiment.ai

Sentiment analysis from sentence embeddings — multilingual, on-device, and
**TensorFlow-free** by default.

**Project page:**
[https://benwiseman.github.io/sentiment.ai/](https://benwiseman.github.io/sentiment.ai/)

> **v2 note.** The default backend is now the on-device multilingual model
> `multilingual-e5-small` (no TensorFlow, no API key). The legacy Universal
> Sentence Encoder models still work but are opt-in (`install_sentiment.ai(legacy = TRUE)`).
> A Python sibling, [`sentimentai-py`](https://pypi.org/project/sentimentai-py/),
> shares the same scoring heads.

# Quick start

```r
install.packages("sentiment.ai")        # from CRAN
library(sentiment.ai)

# one-time setup of the (TensorFlow-free) Python backend
install_sentiment.ai()

# load the default model (multilingual e5-small), then score some text
init_sentiment.ai(model = "e5-small")
sentiment_score(c("I love this!", "this is terrible"))
#> [1]  0.9  -0.9   (about 1 = positive, about -1 = negative)
```

That's it — no TensorFlow, no API key, and the model runs on your machine.

# Overview

`sentiment.ai` turns text into a sentiment score by (1) embedding the text with a
sentence-embedding model and (2) running a small, bundled scoring head over that
embedding. Compared with traditional lexicon/dictionary methods this:

1. **Is more robust** — it tolerates spelling mistakes and mixed case, and the
   default model handles **~100 languages**.

2. **Doesn't need a rigid lexicon** — text is reduced to an embedding vector, so you
   get sensible scores for words and phrasings that aren't in any dictionary.

3. **Lets you choose the context** — with `sentiment_match()` you define what
   *positive* and *negative* mean for your domain (e.g. `"high quality"` vs
   `"low quality"` for product reviews).

4. **Is auditable** — scoring is deterministic, and `sentiment_provenance()` reports
   exactly which model, scoring head, and settings produced a score.

# Models

Pick a model with `init_sentiment.ai(model = ...)` (and `sentiment_score(model = ...)`):

| `model`        | dim  | notes                                                            |
|:---------------|:----:|:-----------------------------------------------------------------|
| `e5-small`     | 384  | **default** — tiny, fast, ~100 languages, on-device, no TensorFlow |
| `e5-base`      | 768  | best on-device quality, ~100 languages, no TensorFlow            |
| `openai`       | 1536 | `text-embedding-3-small` — paid API, text leaves your machine    |
| `en` / `en.large` / `multi` / `multi.large` | 512 | legacy Universal Sentence Encoder — **opt-in, requires TensorFlow** (`install_sentiment.ai(legacy = TRUE)`) |

The scoring head is chosen with `scoring`: `"mlp"` (default) or `"logistic"`. Both are
small, pure-R JSON heads bundled with the package — **no xgboost and no TensorFlow at
score time**.

# Installation & setup

After installing the package from CRAN, set up the Python backend once:

```r
install_sentiment.ai()           # TensorFlow-free default (sentence-transformers + e5)
```

This creates a Python virtual/conda environment (default name `"r-sentiment-ai"`) and
installs the on-device embedder. Useful arguments:

* `envname` — name of the Python environment.
* `method` — `"auto"` (default), `"virtualenv"`, or `"conda"`.
* `legacy` — `FALSE` (default) installs the TensorFlow-free stack. Set `TRUE` to also
  install TensorFlow / TF-Hub for the legacy Universal Sentence Encoder models.
* `gpu` — only relevant when `legacy = TRUE` (the default e5 backend uses PyTorch,
  which picks its own device).

```r
# opt into the legacy Universal Sentence Encoder models (installs TensorFlow):
# install_sentiment.ai(legacy = TRUE)
```

Because this bridges R and Python via `reticulate`, see **Troubleshooting** below if
the environment is hard to activate.

# Initialize

`init_sentiment.ai()` loads the embedding model into memory so repeated scoring is
fast. It is optional — `sentiment_score()` will call it for you — but pre-initializing
avoids re-loading the model on every call.

```r
init_sentiment.ai()                       # default model: e5-small
init_sentiment.ai(model = "e5-base")      # larger on-device model
# init_sentiment.ai(model = "en.large")   # legacy USE (needs legacy install)
```

# Sentiment analysis

## `sentiment_score()`

The main function. Returns one score per input, rescaled to `[-1, 1]` (about `1` =
positive, about `-1` = negative).

```r
my_comments <- c("Will you marry me?", "Oh, you're breaking up with me...")
sentiment_score(my_comments)            # default: e5-small + mlp head
```

Key arguments: `x` (text or a pre-computed embedding matrix), `model` (default
`"e5-small"`), `scoring` (`"mlp"` default, or `"logistic"`), and `batch_size`.

## `sentiment_match()`

Same score, plus a nearest-phrase explanation against **tunable poles** — you define
what positive and negative mean for your domain:

```r
my_categories <- list(positive = c("excited", "loving", "content", "happy"),
                      negative = c("lame", "lonely", "sad", "angry"))

result <- sentiment_match(my_comments, phrases = my_categories)
print(result)
#>                                 text  sentiment phrase    pole
#> 1                Will you marry me?       0.54  loving positive
#> 2 Oh, you're breaking up with me...      -0.66     sad negative
```

You can pass any set of poles (not just positive/negative) to do arbitrary category
matching. Cosine similarity is relative, so longer text tends to match any single
phrase less strongly.

## `sentiment_provenance()`

See exactly what produced a score — the encoder, its license/source/revision, the
embedding prefix, and the scoring head — plus a train/serve prefix-skew check:

```r
sentiment_provenance("e5-small")
#> sentiment.ai provenance
#>   model    : e5-small (st, dim 384)
#>   prefix   : "query: "
#>   license  : MIT
#>   source   : https://huggingface.co/intfloat/multilingual-e5-small
#>   scoring  : mlp 1.0 (mlp, T=...)
```

# Embeddings & matrix helpers

If you've called `init_sentiment.ai()`, you can embed text yourself with
`embed_text()` (e.g. to cluster comments), and compare embeddings with `cosine()` /
`cosine_match()`:

```r
target_mx <- embed_text(c("dogs", "cat", "IT", "computer"))
ref_mx    <- embed_text(c("animals", "technology"))

cosine_match(target_mx, ref_mx)[rank == 1]   # top match per row
#>      target  reference similarity rank
#> 1:     dogs    animals       0.80    1
#> 2:      cat    animals       0.61    1
#> 3:       IT technology       0.44    1
#> 4: computer technology       0.67    1
```

# Python sibling

The same engine is available in Python as
[`sentimentai-py`](https://pypi.org/project/sentimentai-py/) — it ships the **same**
scoring heads and its forward pass is verified bit-for-bit against the R one:

```bash
pip install --pre sentimentai-py
```
```python
import sentimentai as sa
sa.sentiment_score(["I love this", "this is terrible"])
```

# Worked example

A spread of tricky inputs, scored with `sentiment_score()`:

```r
text <- c(
  "What a great car. It stopped working after a week.",
  "the resturant is my favorite!",
  "this restront is my FAVRIT innit!",
  "the resturant was my absolute favorite until they gave me food poisoning",
  "This fantastic app freezes all the time!",
  "I learned so much on my trip to Hiroshima museum last year!",
  "What happened to the people of Hiroshima in 1945"
)
sentiment_score(text)
```

> **Benchmark.** On a public test mix (Amazon / IMDB / tweets / financial-news reviews
> plus GPT synthetic; **no proprietary data**), scored with one identical XGBoost recipe
> per embedder, on the **real-only** slice (n = 1,247): `e5-base` reaches macro-F1
> **0.899** and **94%** directional accuracy on real positive/negative reviews — tied
> with paid OpenAI (0.886 / 94%); the default `e5-small` is **0.854 / 89%**, level with
> the old TensorFlow USE default it replaces. So the TensorFlow-free default *matches*
> the old one on accuracy while dropping all of TensorFlow. The bundled `mlp` heads **are**
> those full-data heads — e5-small **0.860 / 90%**, e5-base **0.919 / 94%** on the real-only
> slice, at the embedder ceiling. Real *neutral* text is scarce in the benchmark, so pos/neg
> accuracy is the most reliable read; see `NEWS.md` for the full table.

# Contribute a scoring head

Scoring heads turn an embedding into a sentiment score; you can train and contribute
your own. Heads live under:

```
scoring/<type>/<version>/<model>.json
```

For example, `scoring/mlp/1.0/e5-small.json` is the default used by
`sentiment_score(model = "e5-small", scoring = "mlp")`. The small `mlp`/`logistic`
JSON heads ship inside the package; larger artifacts download on first use. A head is
a small JSON describing the forward pass (`{type, dim, T, layers | coef, classes}`),
read by a pure-R scorer (no xgboost, no TensorFlow at score time). Use a descriptive
version (e.g. `mlp/jimbo_imdb1/e5-small.json`) for community heads and reserve numeric
versions (`1.0`, `2.0`, …) for official defaults. Open an issue/PR if your head is a
general-purpose improvement on the defaults.

# Troubleshooting

Because this bridges R and Python via `reticulate`, environment activation is the most
common snag:

* **`reticulate` won't switch environments** (often in RStudio, projects, or
  containers) — this usually means `RETICULATE_PYTHON` is set. Restart R, or set the
  interpreter under *Tools > Global Options > Python*, or install into the base
  `r-reticulate` environment.
* **Missing Python module errors** — either `install_sentiment.ai()` didn't complete,
  or reticulate isn't using the `r-sentiment-ai` environment (see above).
* **TLS/SSL errors during `pip` install** — see
  [this guide](https://stackoverflow.com/questions/45954528/pip-is-configured-with-locations-that-require-tls-ssl-however-the-ssl-module-in).
* **Legacy TensorFlow models** — `en`/`multi`/etc. require `install_sentiment.ai(legacy = TRUE)`
  and a compatible TensorFlow setup; the default e5 path avoids all of this.

# Roadmap

* Pinned model revisions for fully reproducible downloads.
* More scoring heads, including community-contributed ones.
* Re-run multilingual benchmarks on the v2 default.

---

Originally created by the Korn Ferry Institute AITMI team.
