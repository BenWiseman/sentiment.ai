# sentiment.ai 1.0.0

`sentiment.ai` 1.0 makes the package fast, modern, and install-clean by default. The big
change: **TensorFlow is no longer required.** The default sentiment pipeline now runs on a
no-TensorFlow, on-device embedder (**e5-small**), and the legacy TensorFlow Universal
Sentence Encoder path is preserved as an explicit opt-in. In development testing the new
default **matches — and slightly improves on — the old TensorFlow USE default it replaces**
on a clean install (0.832 → 0.842 macro-F1), and for users who want top accuracy, an
on-device **e5-base** option **matches paid OpenAI embeddings on a free, on-device,
multilingual, zero-TensorFlow model** (0.890 vs 0.897; paired-bootstrap 95% CI on ΔF1
includes 0). If you only ever called `sentiment_score()` / `sentiment_match()`
with the defaults, you get a cleaner install and a stronger default — and your existing USE
scripts still work.

> The macro-F1 figures below are from the **full-data** evaluation (full train pool,
> `num_parallel_tree = 24`, no early stopping) on the held-out test split, with one identical
> XGBoost scoring recipe per embedder so the comparison isolates **embedding quality**.
> Macro-F1 is the mean of the three per-class F1s (neg / neutral / pos).

---

## Why TensorFlow had to go (frank, not a tantrum)

The original package was built on the TensorFlow Hub Universal Sentence Encoder. It worked,
and for 2021 it was a good choice. But over three years the TensorFlow backend became, by a
wide margin, the single biggest source of user pain — and most of that pain had nothing to do
with sentiment analysis:

- **Pinned-version fragility.** A working install depended on a narrow, ageing set of pins
  (`tensorflow` 2.4.x, `tensorflow_hub` 0.12.0, `tensorflow-text` 2.4.3, `sentencepiece`).
  Drift in any one of them — or in the surrounding Python/conda environment — broke the
  install, and the failure messages pointed at TensorFlow internals, not at anything a
  sentiment-analysis user could reasonably debug.
- **The Apple-Silicon `tensorflow-text` gap.** There was no clean prebuilt `tensorflow-text`
  wheel for Apple-Silicon Macs for a long stretch, which is exactly the hardware much of the
  R/research audience moved to. We carried special-case code paths to paper over it; it was
  never robust.
- **Install hell as the first experience.** For too many users, the *first* thing the package
  did was fail to install. A sentiment tool whose hardest problem is "get TensorFlow to load"
  has its costs in the wrong place.

Honest accounting: **the TensorFlow backend cost users more than it returned.** The accuracy
it bought (see the table) is now matched or beaten by on-device models that need no TensorFlow
at all, and TF Hub itself is being deprecated upstream — so keeping it as the *default* would
mean carrying escalating maintenance cost for a backend the ecosystem is walking away from.
This is not a swipe at TensorFlow; it's a statement about the right default for *this* package.

## What's new

- **No-TensorFlow default.** The default embedder is now an on-device sentence-transformers
  model (PyTorch, no TensorFlow, no `tensorflow-text`), so the default install is
  Apple-Silicon-clean and free of the old version pins. The transformers backend is pinned to
  PyTorch (`USE_TF=0`, `USE_TORCH=1`) so TensorFlow is never imported.
- **Modern, stronger on-device models** that explicitly model the **neutral** class, returning
  scores as `-1, 0, 1`.
- **OpenAI embeddings** as a first-class paid/API backend (`text-embedding-3-small`).
- **Legacy Universal Sentence Encoder preserved** as an explicit opt-in, so existing USE-based
  scripts keep working unchanged.

## Benchmark — what replaced TensorFlow, and why

Each candidate embedder was scored with one identical XGBoost recipe (`multi:softprob`, three
classes — neg / neutral / pos) on the same held-out test set, so the ranking isolates
**embedding quality**. The data is a public mix — Amazon / IMDB / tweets / financial-news
reviews plus GPT-generated synthetic examples; **no proprietary data**. Because the synthetic
split is neutral-heavy, we report macro-F1 on both the full test (n = 3,255) and the
**real-only** slice (n = 1,247), plus **directional accuracy on the 1,187 real, clearly
positive/negative examples** — the most reliable real-world read. Full-data XGBoost
(`num_parallel_tree = 24`, no early stopping).

| Model (in `sentiment.ai`)      | Embedder                          | No TF | macro-F1 (full) | macro-F1 (real-only) | real pos/neg acc |
|--------------------------------|-----------------------------------|:-----:|:---------------:|:--------------------:|:----------------:|
| **openai**                     | `text-embedding-3-small` (API)    |  yes  |     0.897       |        0.886         |    **94.3%**     |
| **e5-base**                    | `intfloat/multilingual-e5-base`   |  yes  |     0.890       |      **0.899**       |    **94.1%**     |
| **e5-small** (default)         | `intfloat/multilingual-e5-small`  |  yes  |     0.842       |        0.854         |      89.3%       |
| _use-large (old TF default)_   | _TF-Hub Universal Sentence Encoder_ | _no_ |    _0.832_      |       _0.850_        |     _88.9%_      |

Read-out (honest):

- **On real text, the best on-device model ties paid OpenAI.** e5-base reaches macro-F1 0.899
  vs OpenAI's 0.886 on the real-only slice, and both get ~94% of real positive/negative reviews'
  sign right (94.1% vs 94.3%). They are tied within noise on a single split — neither is a clear
  win. This is the **e5-base** tier — one flag from the default, not the default itself.
- **The new default matches the old TensorFlow default — the value is dropping TensorFlow, not
  raw accuracy.** On real text e5-small ties the old USE-large default (0.854 vs 0.850; 89.3% vs
  88.9%). It delivers that same accuracy with **no TensorFlow, on-device, multilingual, and at a
  fraction of the size** — the TensorFlow tax bought no accuracy. (On the synthetic-inclusive
  test e5-small edges USE-large 0.842 → 0.832, but on real text they are level.)
- **The bundled scoring heads are full-data.** The small `mlp` heads we ship get **90.1% /
  93.8%** real pos/neg accuracy (macro-F1 **0.860 / 0.919**) for e5-small / e5-base — at the
  full-data XGBoost ceiling above, and they need neither xgboost nor TensorFlow to run.
- **Caveat:** real *neutral* examples are scarce (n = 60 in the test set), so the 3-class
  macro-F1 leans on synthetic neutral; the directional accuracy on 1,187 real positive/negative
  examples is the more reliable real-world figure.

> The e5 models require a `query: ` prefix on each input; `sentiment.ai` applies it internally
> for the e5 backends so you never pass it yourself. The e5 backbone is **multilingual** (the
> upstream model card lists ~100 languages), so the multilingual reach of the old USE backend is
> **carried forward** in capability. Note our reported macro-F1 is measured on an
> **English-derived** test set: multilingual sentiment quality is inherited from the e5 backbone
> and is **not independently benchmarked here**.

## The new model lineup

| Model name (in `sentiment.ai`) | Embedder                              | Use it for                                                              | Install |
|--------------------------------|---------------------------------------|-------------------------------------------------------------------------|---------|
| **e5-small** (default)         | `intfloat/multilingual-e5-small`      | fast, lightweight, on-device, multilingual backbone                     | default (no TF) |
| **e5-base**                    | `intfloat/multilingual-e5-base`       | best on-device accuracy (matches OpenAI on this English benchmark, within noise) | default (no TF) |
| **openai**                     | `text-embedding-3-small`              | top accuracy via paid API (needs an API key; text leaves your machine)  | API, no local model |
| legacy USE (`en`, `en.large`, `multi`, `multi.large`) | TF Hub Universal Sentence Encoder | backward compatibility for existing USE scripts | opt-in (`legacy = TRUE`, needs TensorFlow) |

## Legacy TensorFlow: opt-in, still real

The TensorFlow USE backend is **not removed** — it is demoted from the default path to an
explicit opt-in, installed only when you ask for it:

```r
install_sentiment.ai(legacy = TRUE)   # installs the TensorFlow USE backend
```

- Existing scripts that name a USE model (`"en"`, `"en.large"`, `"multi"`, `"multi.large"`)
  continue to resolve to the legacy backend, so **your numbers don't silently change** if you
  pin to those models.
- **Each legacy USE model has a no-TensorFlow replacement at or above the old USE default's
  accuracy.** On the legacy model we benchmarked (USE-large, 0.832), every shipped no-TF option
  meets or beats it, so for new work there is a TF-free option that does not cost you accuracy:

  | Legacy USE model | TF-free replacement | Macro-F1 |
  |------------------|---------------------|----------|
  | `en` / `en.large` (English USE) | `e5-base` (on-device) or `openai` (API) | USE-large 0.832 → **0.890 / 0.897** |
  | `multi` / `multi.large` (multilingual USE) | `e5-base` / `e5-small` (multilingual backbone) | **0.890 / 0.842** (vs USE-large 0.832; `multi` / `multi.large` not separately benchmarked) |

  *Only USE-large was run in the benchmark, as the strongest legacy default; the smaller `en` /
  `multi` USE variants were not separately measured, so the replacement is stated as "at or above
  the old USE default," not a model-for-model "better."*
- **TF Hub itself is deprecating upstream.** The tfhub.dev model URLs are deprecated; the legacy
  USE weights are now vendored so the opt-in path keeps working. Keeping USE as a default would
  mean owning the maintenance cost of a backend the wider ecosystem is retiring. As an opt-in
  compatibility layer it stays genuinely supported and tested, without taxing every install.

## Upgrade notes

- **The default model changed**, which can change scores for scripts that relied on the default
  embedder. This is why 1.0 is a **major** version bump. To reproduce pre-1.0 results exactly,
  install the legacy backend (`install_sentiment.ai(legacy = TRUE)`) and name a USE model
  explicitly (e.g. `model = "en.large"`).
- The default install no longer pulls in TensorFlow, `tensorflow-text`, or the old version pins.
- `sentiment_match()` (context-tunable positive/negative definitions) is unchanged and works
  across all backends.

## Attribution

`sentiment.ai` remains **MIT-licensed** and was created by the **Korn Ferry Institute** AITMI
team; that authorship and funding history is retained in full. Maintainer/lead author:
**Ben Wiseman**. Co-authors and contributors per `DESCRIPTION`.


# sentiment.ai 0.2.0

* New, more powerful default models that explicitly categorize neutral statements 
returning scores as -1, 0, 1. Set version argument to 1 to use original models. 

* Support for openAI/Ada embeddings

* python modules can now be set to "latest" for compatibility with other python interpreters

* Seemingly fixed tensorflow-text bug on apple silicone 


# sentiment.ai 0.1.1

added silent argument in initsentiment.ai

using method = "auto" in init_sentiment.ai() now uses py_discover_config

Patch for case when no conda binary is present

updated default python to 3.8.10 for virtualenv and conda compatibility

* Fixed typos. Note that `get_default_embedding` had a typo in the initial launch and was called `get_defualt_embedding`. Make sure to update your code if you used this function.


# sentiment.ai 0.1.0

INITIAL RELEASE 
see [github page](https://benwiseman.github.io/sentiment.ai/) for details 

[Korn Ferry Institute](https://www.kornferry.com/institute)'s AITMI team made `sentiment.ai` for researchers and tinkerers who want a straight-forward way to
use powerful, open source deep learning models to improve their sentiment analyses. Our approach is relatively simple and aims to improve on traditional lexicon-based sentiment analysis. We decided to open-source our simplified interface to turn Universal Sentence Encoder embedding vectors into sentiment scores. 

We've wrapped a lot of the underlying hassle up to make the process as simple as possible. In addition to just being cool, this approach solves several problems with traditional sentiment analysis, namely: 

1) **More robust**, can handle spelling mitsakes and mixed case, and can be applied to **dieciséis (16) languages**! 

2) **Doesn't need a ridged lexicon**, rather it matches to an embedding vector (reduces language to a vector of numbers that capture the information, kind of like a PCA). This means you can get scores for words that are not in the lexicon but are similar to existing words! 

3) **Choose the context** for what negative and positive mean using the `sentiment_match()` function. For example, you could set `positive` to mean `"high quality"` and negative to mean `"low quality"` when looking at product reviews.

4) **Power** Because it draws from language embedding models trained on billions of texts, news articles, and wikipedia entries, it is able to detect things such as *"I learned so much on my trip to Hiroshima museum last year!"* is associated with something positive and that *"What happeded to the people of Hiroshima in 1945"* is associated with something negative.

5) **The power is yours** We've designed `sentiment.ai` such that the community can contribute sentiment models via [github](https://github.com/BenWiseman/sentiment.ai/tree/main/models). This way, it's easier for the community to work together to make sentiment analysis more reliable! 
Currently only xgboost and glms (trained on the 512-D embeddings generated with tensorflow) are supported, however in a future update we will add functionality to allow arbitrary sentiment scoring models. 
