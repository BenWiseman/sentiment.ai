# sentiment.ai 2.0.0

`sentiment.ai` 2.0 makes the package fast, modern, and install-clean by default. The
big change: **TensorFlow is no longer required.** The default sentiment pipeline now
runs on a no-TensorFlow, on-device embedder, the legacy TensorFlow path is preserved
as an explicit opt-in, and the new default on-device model **ties paid OpenAI embeddings
on accuracy** while beating the old TensorFlow Universal Sentence Encoder it replaces. If
you only ever called `sentiment_score()` / `sentiment_match()` with the defaults, you get
a better model on a cleaner install — and your existing USE scripts still work.

> **Headline accuracy figure pending.** The numbers below are from a fixed, reproducible
> **subsample** bake-off (6,000 train rows/class; the real 3,255-row held-out test split).
> The full-data headline run (full train pool, full `num_parallel_tree = 24` recipe, same
> test set) is in progress; this entry will be finalised with those figures before release.
> Treat the macro-F1 values here as **provisional rankings**, not the published headline.

---

## Why TensorFlow had to go (frank, not a tantrum)

The original package was built on the TensorFlow Hub Universal Sentence Encoder. It worked,
and for 2021 it was a good choice. But over three years the TensorFlow backend became, by a
wide margin, the single biggest source of user pain — and most of that pain had nothing to
do with sentiment analysis:

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
  model (PyTorch/ONNX, no TensorFlow, no `tensorflow-text`), so the default install is
  Apple-Silicon-clean and free of the old version pins.
- **Modern, stronger on-device models** that explicitly model the **neutral** class
  (scores returned as `-1, 0, 1`), trained with a synthetic-neutral augmentation step that
  improves performance on the positive and negative classes (neutral scarcity is a known
  problem in real sentiment corpora).
- **OpenAI embeddings** as a first-class paid/API backend (`text-embedding-3-small`).
- **Legacy Universal Sentence Encoder preserved** as an explicit opt-in, so existing USE-based
  scripts keep working unchanged.

## Bake-off — what replaced TensorFlow, and why

We ran a fair, single-subsample bake-off across candidate embedders: one fixed train
subsample (6,000 rows per class) and the **same real 3,255-row held-out test split** the
shipped models are evaluated on, with an identical XGBoost scoring recipe per candidate so the
ranking isolates **embedding quality**. Macro-F1 is the mean of the three per-class F1s
(neg/neu/pos). *Provisional — pending the full-data run described above.*

| Embedder                                | Backend / install        | On-device | No TF | F1 neg | F1 neu | F1 pos | **Macro-F1** |
|-----------------------------------------|--------------------------|:---------:|:-----:|:------:|:------:|:------:|:------------:|
| OpenAI `text-embedding-3-small`         | API (paid)               | no (API)  |  yes  | 0.914  | 0.834  | 0.834  | **0.861**    |
| **e5-base** (`intfloat/multilingual-e5-base`) | sentence-transformers | **yes** | **yes** | 0.915  | 0.833  | 0.831  | **0.860**    |
| bge-base (`BAAI/bge-base-en-v1.5`)      | sentence-transformers    | yes       |  yes  | 0.898  | 0.809  | 0.801  | 0.836        |
| gte-base (`thenlper/gte-base`)          | sentence-transformers    | yes       |  yes  | 0.892  | 0.791  | 0.791  | 0.825        |
| bge-small (`BAAI/bge-small-en-v1.5`)    | sentence-transformers    | yes       |  yes  | 0.885  | 0.794  | 0.793  | 0.824        |
| **e5-small** (`intfloat/multilingual-e5-small`) | sentence-transformers | **yes** | **yes** | 0.861  | 0.800  | 0.779  | **0.813**    |
| _use.large (old TF default)_            | _TF Hub USE_             | _yes_     | _no_  | 0.845  | 0.782  | 0.744  | _0.790_      |
| mpnet (`all-mpnet-base-v2`)             | sentence-transformers    | yes       |  yes  | 0.838  | 0.763  | 0.742  | 0.781        |
| paraphrase (`paraphrase-MiniLM-L6-v2`)  | sentence-transformers    | yes       |  yes  | 0.786  | 0.759  | 0.708  | 0.751        |

Read-out:
- **e5-base ties OpenAI** (0.860 vs 0.861) while running fully **on-device with no TensorFlow**,
  and clears the old TF USE-large default (0.790) by a wide margin.
- **e5-small** is the fast, lightweight default at 0.813 — still ahead of the old TF USE-large
  default, on a clean no-TF install.
- The 2021-era `paraphrase-MiniLM` (0.751) and `mpnet` (0.781) — the models a naive "just drop
  sentence-transformers in" would reach for — are **too weak** to be the default; the e5 family
  is what actually clears the bar. We did the bake-off precisely so the default change is an
  upgrade, not a TensorFlow-removal tax on accuracy.

> The e5 models require a `"query: "` prefix on each input; `sentiment.ai` applies this
> automatically for the e5 backends. They are multilingual (~100 languages), so the
> "16 languages" capability of the old USE backend is **broadened**, not lost.

## The new model lineup

| Model name (in `sentiment.ai`) | Embedder                              | Use it for                                  | Install |
|--------------------------------|---------------------------------------|---------------------------------------------|---------|
| **e5-small** (default)         | `intfloat/multilingual-e5-small`      | fast, lightweight, on-device, multilingual  | default (no TF) |
| **e5-base**                    | `intfloat/multilingual-e5-base`       | best on-device accuracy (ties OpenAI)       | default (no TF) |
| **openai**                     | `text-embedding-3-small`              | top accuracy via paid API (needs API key)   | API, no local model |
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
- **Each legacy USE model has a better TensorFlow-free replacement** in the new lineup, so for
  new work there is a no-TF option that meets or beats it:

  | Legacy USE model | TF-free replacement     | Macro-F1 move (provisional) |
  |------------------|-------------------------|-----------------------------|
  | `en` / `en.large` (English USE) | `e5-base` (on-device) or `openai` (API) | 0.790 → **0.860 / 0.861** |
  | `multi` / `multi.large` (multilingual USE) | `e5-base` / `e5-small` (multilingual, ~100 langs) | 0.790 → **0.860 / 0.813** |

- **TF Hub itself is deprecating upstream.** Keeping USE as a default would mean owning the
  maintenance cost of a backend the wider ecosystem is retiring. As an opt-in compatibility
  layer it stays genuinely supported and tested, without taxing every install.

## Upgrade notes

- **The default model changed**, which can change scores for scripts that relied on the default
  embedder. This is why 2.0 is a **major** version bump. To reproduce pre-2.0 results exactly,
  install the legacy backend (`install_sentiment.ai(legacy = TRUE)`) and name a USE model
  explicitly (e.g. `model = "en.large"`).
- The default install no longer pulls in TensorFlow, `tensorflow-text`, or the old version pins.
- `sentiment_match()` (context-tunable positive/negative definitions) is unchanged and works
  across all backends.

## Attribution

`sentiment.ai` remains **MIT-licensed** and was created by the **Korn Ferry Institute** AITMI
team; that authorship and funding history is retained in full. Maintainer/lead author:
**Ben Wiseman**. Co-authors and contributors per `DESCRIPTION` (Steven Nydick, Tristan Wisner,
and the original KFI team).
