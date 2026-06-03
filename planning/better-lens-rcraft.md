# What would make `sentiment.ai` the OBVIOUS default sentiment package in R

> **Lens:** R-package craft + ecosystem fit. Not the v2 model migration (assume e5/no-TF is done).
> The question is: as a *tool*, what makes an R user reach for `sentiment.ai` over `vader`,
> `sentimentr`, `syuzhet`, `tidytext`+lexicons, or `text`?
> **Scope discipline:** every claim about the current package is read from
> `rpackage/sentiment.ai/R/` (cited inline). Every claim about a competitor is from that package's
> public CRAN docs/sources — no fabricated benchmarks. Written 2026-06-03. Author: Ben Wiseman.
> **Constraint:** this doc proposes design; it does **not** edit `R/`.

---

## The core craft problem, stated once

The single biggest *tool* weakness is the **return contract**, and it is visible in the source:

- `sentiment_score()` (`R/sentiment.R:61-117`) returns a **bare, named numeric vector** in `[-1,1]`.
  It computes a 3-class probability internally (`find_sentiment_probs`, `R/sentiment.R:247-285`)
  and then **throws away everything except the positive axis**: `scores <- (probs - .5) * 2`
  (`:111`). The neutral probability the model produced is discarded; there is no class, no
  confidence, no neutral.
- `sentiment_match()` (`R/sentiment.R:158-238`) returns a **`data.table`** with
  `text, sentiment, phrase, class, similarity`.

So the two headline functions of the same package return **two different shapes** (atomic vector
vs `data.table`), and the richer internal signal (neutral prob, class) is reachable only through
the *matching* function, not the *scoring* function. That is the thing to fix first, and most of
what follows hangs off it.

This matters because **the competitor set has already converged on a tidy data-frame contract.**
`sentimentr::sentiment_by()` returns a `data.frame` with `element_id, word_count, sd, ave_sentiment`;
`tidytext::get_sentiments()` is built to `inner_join()` a tibble of tokens; `text::textClassify()`/
the `text` package returns tibbles. A package whose primary output is a *headless numeric vector*
is the odd one out in 2026 tidyverse-shaped R, and it forces the user to `cbind()` it back onto
their data by hand and **re-derive the class with a hand-picked threshold the package already knew.**

---

## Where `sentiment.ai` wins / loses vs each competitor (grounded)

| Package | What it is | Where it genuinely beats `sentiment.ai` today | Where `sentiment.ai` wins (or should) |
|---|---|---|---|
| **`vader`** (Hutto & Gilbert rule-based, VADER port) | Lexicon + grammatical heuristics (caps, punctuation, negation, degree). Zero install, deterministic, fast, returns `compound`+pos/neu/neg proportions. | **No Python, no model download, instant.** Returns neu/pos/neg *proportions* out of the box — the exact 3-class signal `sentiment.ai` hides. Social-media-tuned (emoji, slang, ALL CAPS). | Semantics over surface features: handles paraphrase, spelling, non-lexicon words, 16+ langs. Beats VADER on anything that isn't tweet-shaped. |
| **`sentimentr`** (Rinker) | Valence-shifter-aware lexicon scoring, sentence-level, **polarity_dt** swappable. | **Tidy `data.frame` output**, sentence aggregation, `highlight()` HTML, `extract_sentiment_terms()` for *which words drove it*. Pure R, fast, no install. Huge install base. | Embedding semantics + custom anchors (`sentiment_match`). But sentimentr's **term attribution is a feature `sentiment.ai` simply lacks** — see below. |
| **`syuzhet`** (Jockers) | Narrative arc + NRC emotion lexicon. | **8 emotion categories** (NRC), narrative-arc smoothing, literary audience. Trivial install. | Accuracy on polarity; but `sentiment.ai` has **no emotion-category output at all**, so for emotion work syuzhet wins outright. |
| **`tidytext`** + bing/afinn/nrc | The default teaching path; `unnest_tokens()` → `inner_join(get_sentiments())`. | **It IS the tidyverse workflow.** Every R sentiment tutorial starts here. Token-level, fully transparent, composes with `dplyr`. | Document-level semantics without losing nuance to bag-of-words; no lexicon-coverage gaps. But `sentiment.ai` **does not plug into this workflow at all** — that is a gap, not a feature. |
| **`text`** (Kjell, Schwartz, Sikström) | The serious modern competitor: HF transformers via reticulate, embeddings + tidymodels integration, `textClassify()`, peer-reviewed (Kjell et al. 2023, *Psychological Methods*). | **Already does what v2 is becoming** (no-TF HF backbone, tibbles, tidymodels). Academic credibility, `textTrain*()` pipeline, broad NLP scope. | `sentiment.ai` is *narrower and therefore simpler*: one job (sentiment), one call, pre-trained scorer, custom anchors. `text` is a toolkit; `sentiment.ai` should win on **"score sentiment in one line and trust it."** |

**Honest read:** `sentiment.ai`'s defensible wedge is **(a) document-level semantic accuracy with a
pre-trained scorer** (the bake-off shows it's competitive with paid APIs) and **(b) custom anchor
phrases** (`sentiment_match`) — nobody else in the lexicon set lets you *redefine* positive/negative
by example. Its losses are almost all **craft/ergonomics, not capability**: headless output, no
term attribution, no tidy/quanteda glue, install friction, `cat()`-based errors. Those are fixable
without touching the model. That is the whole opportunity.

---

# TOP 3 — the changes that flip the default

These are ordered by *leverage per unit of work* and chosen so each directly closes a gap against a
named competitor. They are the three I'd ship first.

---

## #1 — Make the output a tidy tibble that carries the 3-class signal the model already computes

**The fix:** `sentiment_score()` should, by default, return a **tibble** (or `data.table`; pick one
shape and use it everywhere) with the columns the model *already has*:

```
text  prob_negative  prob_neutral  prob_positive  sentiment  class  confidence
```

- `sentiment` = the current `[-1,1]` score (keep it; it's the back-compat axis).
- `class` = `argmax` of the three probabilities — a real neg/neu/pos label.
- `confidence` = the max class probability (or margin between top-2). This is the single most
  requested thing a sentiment user wants and **the package is currently deleting it at
  `R/sentiment.R:111`.**
- `prob_*` = the per-class probabilities `find_sentiment_probs()` produces.

This is not a model change — it is **stop discarding `probs`**. The 3-class object is already
computed; `sentiment_score` collapses it to one number. Surface it.

**Why this is #1:**
- It directly answers the strongest thing `vader` has over you: vader returns neu/pos/neg
  *proportions* out of the box; you compute the equivalent and throw it away.
- It unifies the two return shapes (`sentiment_score` vector vs `sentiment_match` data.table) into
  one contract, which is the root inconsistency above.
- It makes `class` honest. Right now a user who wants neg/neu/pos has to **pick their own threshold**
  on `[-1,1]` — but the model has a principled neutral class internally. Letting the user guess a
  cutoff the model already knew is the opposite of the "trustworthy, computed-not-guessed" brand.

**Back-compat without a breaking change** (this is the craft detail that makes it shippable):
add an `output` arg — `output = c("tibble", "vector")` — *or* a thin companion. Keep
`sentiment_score(x)` returning the vector if you must not break the existing CRAN API, but make the
**rich tibble the documented, example-first path**, e.g. a new `sentiment(x)` /
`sentiment_table(x)` as the headline verb and demote the bare-vector function to "fast path." Either
way: **never make a confidence-aware user reconstruct what you deleted.**

**Concrete column contract (so it's testable):** one row per input element, `NA` rows preserved
positionally (the code already tracks `na_index`, `R/sentiment.R:84`), `class` an ordered factor
`c("negative","neutral","positive")`, `confidence` in `[0,1]`. Snapshot-test it.

**Tidy glue, same change:** ship `broom::tidy()` / `glance()` methods for the result object so
`tidy(sentiment(reviews$text))` Just Works, and document the one-liner that everyone actually wants:

```r
reviews |>
  mutate(sentiment(text)) |>     # binds prob_*, class, confidence as columns
  count(class)
```

That single example, on the README above the fold, is what makes a `dplyr` user choose you over
`tidytext`'s three-line `unnest_tokens |> inner_join |> summarise` dance.

---

## #2 — Token / phrase **attribution**: "which words drove this score?"

**The fix:** add an attribution output — the embedding analogue of
`sentimentr::extract_sentiment_terms()` / `syuzhet`'s word-level visibility. You already have the
machinery: `sentiment_match()` does cosine matching against anchor phrases
(`cosine_match`, `R/matrix_helpers.R:69`). Turn that lens **inward on the input**:

- **Cheap, ships now (no model internals):** segment each input into sentences/clauses (or sliding
  n-gram windows), embed each segment, score each segment with the existing scorer, and return a
  per-segment breakdown with the most-positive and most-negative span flagged. This is "which part of
  this review is the angry part" — and `sentiment.ai` is *uniquely* able to do it semantically rather
  than by lexicon hit, because it scores spans that contain **no lexicon words at all** (the
  README's own Hiroshima/Nagasaki examples are exactly this).
- **Anchor attribution (already 90% built):** for `sentiment_match`, you already return the closest
  anchor phrase + similarity. Promote that to a first-class **explanation column** and document it as
  *the* explainability story: "this comment scored −0.66, and its nearest anchor was *'sad'* at
  cosine 0.15." Return the top-_k_ anchors, not just rank 1, behind a `k =` arg.

**Why this is #2:** explainability is the entire reason a cautious analyst keeps using lexicon
packages despite worse accuracy — they can *see why*. `sentimentr` wins audits because
`extract_sentiment_terms()` shows the driving words and `highlight()` renders them. Today
`sentiment.ai` is a black box that emits a number, which is **brand-corrosive for a package whose
pitch is "computed, auditable, trustworthy — not AI hype."** Span-level attribution is the feature
that lets you say "auditable" truthfully: the score decomposes into spans the user can read, and
the matched anchor is a literal, inspectable phrase. It converts the embedding model from "trust the
black box" to "here's the receipt," which is the differentiator the brand is built on.

**Honesty guardrail (do this right or don't ship it):** segment scores are *local* approximations,
not a true Shapley decomposition of the document embedding — they will not sum to the document
score. Say so, in the docs and the column docs. Call the column `span_sentiment`, not
`contribution`, and state plainly: "per-span scores are independent re-scores of each span, provided
for inspection; they do not additively reconstruct the document score." A fabricated "this word
contributed +0.3" claim would be exactly the kind of overclaim the rest of the v2 honesty pass is
killing. Inspectable spans = honest; invented additive attributions = not.

---

## #3 — Kill the install/first-run cliff and make errors `cli`-grade

**The fix:** the thing that makes people *bounce before they ever see the accuracy* is the
reticulate/Python wall. v2 dropping TF helps, but the craft work around it is separable and decisive:

1. **A single, honest readiness API.** `sentiment_status()` / `sentiment_check()` that reports, in
   one call: backend resolved? env present? weights downloaded (which model, what revision, how big,
   cached where)? It returns a structured object **and** prints a `cli`-styled status block. Right now
   readiness is implicit and failures surface as raw reticulate/Python tracebacks. A user should be
   able to run one function and know exactly what's missing and the exact next command to fix it.

2. **Replace `cat()`-based errors with `rlang::abort()` + `cli`.** The current error helper is
   `create_error_text()` (`R/create_error_text.R`), which just `cat()`s padded text — these are not
   real conditions (can't be `tryCatch`'d on class, no `rlang::last_error()`, no actionable bullets).
   And `embed_text()` currently `warning()`s "sentiment.env$embed not found!" then *tries to recover
   by silently initialising* (`R/embedding.R:187-198`) — a warning-then-guess path that is precisely
   the "quietly wrong" failure mode the model-lineup review (SEV-1) flags. Convert to:
   `cli::cli_abort()` with a clear message, a `i` bullet naming the cause, and a `>` bullet with the
   literal fix command (`install_sentiment.ai(...)`). Classed conditions (`sentiment_ai_not_installed`,
   `sentiment_ai_model_missing`) so downstream code can handle them. This is pure craft and it's the
   difference between "weird Python error, gave up" and "oh, I just run that one line."

3. **First-run UX as a designed step, not a hang.** When weights aren't present, *ask once*, disclose
   size up front, show a progress bar (you already use `utils::txtProgressBar`/`pbapply`), cache to
   `tools::R_user_dir("sentiment.ai","cache")`, and **`skip_on_cran` / `skip_if_offline` everywhere
   weights are needed** so examples, tests, and `R CMD check` pass cleanly on a fresh machine. A
   1GB silent download is the same install-hell the v2 migration exists to kill — design it out.

**Why this is #3:** `vader`, `sentimentr`, `syuzhet`, and `tidytext` all win one brutal comparison —
`install.packages(); library(); score()` works in 10 seconds with zero Python. You will never match
*zero* install (you have a model), but you can make first-run **legible**: consented, progress-barred,
cached, with errors that tell the user the exact fix. The gap that loses you users is rarely accuracy
(the bake-off says you're competitive with paid APIs) — it's the **30 seconds where a beginner hits a
reticulate traceback and types `install.packages("sentimentr")` instead.** Close that and the accuracy
advantage actually gets to matter.

---

## Runner-up moves (documented so they aren't lost; below the top-3 cut line)

- **quanteda / tidytext native methods.** A `sentiment()` method that accepts a `quanteda::corpus`
  or a tidytext token tibble and respects `doc_id`, so you slot into the two dominant text-analysis
  pipelines instead of asking users to leave them. Low effort, high "of course I'd use it."
- **`sentiment_match()` is a general zero-shot classifier — say so.** The README already half-admits
  it ("pass arbitrary themes"). Promoting it to a documented `classify_text(x, labels = list(...))`
  verb gives you a feature *none* of the lexicon competitors have (custom semantic categories), and
  it's already implemented. This is arguably a #1-tier idea reframed as positioning, not code.
- **Batching/throughput honesty + benchmark.** v2 has multithreaded xgb; document realistic
  throughput (texts/sec on CPU vs the API path) and expose `batch_size`/`n_threads` clearly. Don't
  claim a speed number you haven't measured — but *do* measure and publish texts/sec vs `sentimentr`
  on a fixed corpus, because "as accurate as paid APIs, on-device, at N texts/sec" is the whole pitch.
- **Reproducibility surface.** Return/print the model id + pinned revision + scorer version alongside
  scores (or as an attribute), so a published analysis can state exactly which weights produced it.
  This is a *craft* expression of the "trustworthy/auditable" brand and costs almost nothing.
- **Stable seedless determinism note.** Inference is deterministic given fixed weights; state it.
  Researchers comparing to `syuzhet`/`sentimentr` care that re-running gives identical scores.

---

## The one-paragraph "why it's the obvious default" you're building toward

After #1–#3, the pitch is: *"`sentiment.ai` scores sentiment at document level with embedding
semantics that beat every lexicon package and rival paid APIs — and it hands you a tidy tibble with
per-class probabilities, a real neg/neu/pos label, a confidence, and the spans that drove the score,
so you can audit every number. One line in a `dplyr` pipe, custom positive/negative anchors when you
need them, and errors that tell you exactly what to run."* That is a sentence no competitor can say in
full: `vader`/`sentimentr` can't claim the accuracy or the semantics, `tidytext` can't claim the
nuance, `text` can't claim the one-line simplicity, and none of them can claim custom anchors. The
work to earn that sentence is **mostly craft, not modelling** — which is why it's worth doing now.
