# A better sentiment-scoring tool — the practitioner lens

> Scope: not the v2/TF migration (assume done). The question is narrower and harder:
> given the v2 engine (no-TF multilingual `e5` default, OpenAI backend, legacy USE
> opt-in, tunable xgboost/glm scorer, `sentiment_match()` anchor mode), **what would
> make `sentiment.ai` a better _tool_** for the people who actually run it?
>
> Method: walk the real jobs one analyst at a time, name what a single `-1..1` number
> fails to give them, and rank the gaps by how badly they block adoption. Everything
> below is grounded in the current source and in real, citable methods/datasets/packages.
> No fabricated benchmarks.

---

## What the tool returns today (read from source, so we don't argue with a strawman)

`sentiment_score()` (`R/sentiment.R:61-117`) returns a **bare numeric vector** in
`[-1, 1]`. The scorer underneath is genuinely richer than that: `find_sentiment_probs()`
(`R/sentiment.R:247-285`) runs a 3-class-capable xgboost (or a glm) over the embedding
and produces a **positive probability**, which is then flattened by
`scores <- (probs - .5) * 2` (`R/sentiment.R:111`) and the rest is discarded. So the
information practitioners keep asking for — *how confident is this? is this a strong
negative or a borderline one? is it actually neutral or is it mixed?* — **is computed
and then thrown away one line before the return.** That is the single most important
fact in this document.

`sentiment_match()` (`R/sentiment.R:158-238`) already proves the package _can_ return
structure: it hands back a `data.table` of `text, sentiment, phrase, class, similarity`.
The richer-output path is not a new architecture — it's an existing one that
`sentiment_score()` doesn't use.

Multilingual is already half-solved: the v2 default is `intfloat/multilingual-e5-*`
(`R/constants.R:6-7`), which covers ~100 languages. So "multilingual" below is mostly a
*surfacing/UX* gap, not a modelling one.

Two structural facts shape every recommendation:

1. **Linnet's brand is trustworthiness, not raw accuracy** — the bake-off says accuracy
   is already competitive, so the marginal point of macro-F1 is not where adoption is
   won or lost. **Defensibility, auditability, and honest uncertainty are.** A better
   tool here is one a practitioner can *defend in a room* — to a works council, a
   regulator, a journal reviewer, a skeptical VP — not one that wins a leaderboard.
2. **The output is the product.** The model is good enough. What's missing is the
   *shape, honesty, and provenance of the number coming out.*

---

## The jobs, and where `-1..1` fails each one

### Job 1 — Survey / engagement open-text verbatims (HR, the Linnet home turf)
**User:** People-analytics analyst pooling thousands of "What's one thing we could
improve?" responses into an engagement readout for leadership.

**Where one number fails:**
- A verbatim is routinely **mixed**: *"Love my team, hate the new RTO policy."* A single
  `-0.1` averages two strong, opposite signals into a meaningless middle and **hides the
  one thing the executive needed to see** (RTO is on fire). This is the killer gap for
  this job: the analyst needs **aspect/target-level sentiment**, not a document mean.
- They need to **roll up to a driver** ("pay", "manager", "workload") and report
  *% negative per driver*, not an average. Averaging a bimodal distribution is
  statistically indefensible and any decent analyst knows it.
- **Neutral is real and load-bearing.** "I have no comment" / "N/A" / a factual
  statement must not be miscoded as mild positive/negative — it inflates whichever way
  the scorer leans and quietly biases the topline. The 3-class scorer already models
  neutral; the API just doesn't expose `neutral` as a first-class outcome.
- **Suppression for safety.** HR aggregates have an n≥k floor (Linnet's own convention).
  The tool should make it trivial to compute group means *and* refuse to emit a cell
  below threshold — today that's the analyst's manual afterthought.

**What they actually need:** aspect/driver-level sentiment + an explicit `neutral` class +
distribution-aware rollups (% pos/neu/neg), not a mean.

---

### Job 2 — Exit / stay interviews (lower volume, very high stakes per row)
**User:** HRBP or I/O analyst reading 40 exit transcripts to find why people leave.

**Where one number fails:**
- Volume is low, **stakes per row are high**, and a misclassification is visible and
  embarrassing. The analyst will not trust a black-box `-0.4`. They need
  **why**: which span drove it, which anchor phrase it matched. `sentiment_match()`
  gives the matched `phrase`/`class` — but it's a separate function with a separate
  mental model, not the default explanation attached to a score.
- Long transcripts get embedded as one blob and **diluted into mush** — a paragraph of
  neutral logistics drowns the one scathing sentence. They need **sentence/segment-level
  scoring with the offsets**, so they can jump to the span.
- **Confidence triage.** With 40 rows they'll read the borderline ones by hand — *if the
  tool tells them which rows are borderline.* A score with no confidence gives them no
  way to prioritise human review.

**What they actually need:** segment-level scores with character offsets + per-row
confidence to triage human review + the matched explanation inline.

---

### Job 3 — Social media / reviews at volume (marketing, brand, CX)
**User:** Brand analyst scoring 500k tweets/reviews, often on a recurring schedule.

**Where one number fails:**
- **Throughput and cost predictability.** Today `sentiment_score()` re-embeds and the
  batching is coarse (`batch_size` default 100, `R/sentiment.R:65`); the OpenAI path
  re-requests every call. At 500k rows the practitioner needs **embedding caching/dedup**
  (reviews repeat verbatim), **resumable batch jobs**, and a **cost/throughput estimate
  _before_ committing** — especially on the paid OpenAI backend, where a surprise bill
  kills the pilot.
- **Domain drift / sarcasm.** A scorer trained on one register will silently degrade on
  another (gaming slang, finance jargon, emoji-heavy). The number *looks* fine — it just
  quietly gets worse. The practitioner has no warning. This is the second killer gap:
  **silent out-of-domain failure.**
- **Recurring runs need stability.** If this feeds a weekly dashboard, a model/threshold
  change that shifts every score breaks trend continuity. They need **pinned, versioned,
  reproducible scores.**

**What they actually need:** caching + resumable batch + a pre-flight cost/throughput
estimate + an out-of-domain / drift warning + pinned score versions.

---

### Job 4 — Finance / clinical notes (regulated, on-prem, high consequence)
**User:** Risk/research analyst on earnings-call transcripts, broker notes, or clinical
free-text.

**Where one number fails:**
- **Privacy is the gate, full stop.** PHI/MNPI cannot leave the building. The v2 on-device
  `e5` default is exactly right here — but the tool must **state and prove** that the
  default path makes **zero network calls**, and make the OpenAI backend an obvious,
  audited opt-out, not an ambient possibility. "Did this send my clinical notes to an
  API?" must have a one-line, verifiable answer.
- **Domain mismatch is severe.** General sentiment is wrong for finance ("liability",
  "aggressive growth") and clinical ("positive" = bad). These users have *real labelled
  lexicons and corpora* — **Loughran-McDonald** finance dictionary, **Financial
  PhraseBank** (Malo et al. 2014), **MIMIC**-derived clinical text. The tool's value is
  letting them **fine-tune/calibrate the scorer to their domain**, which the architecture
  already supports (xgboost over embeddings + `sentiment_match()` custom anchors) but
  doesn't *package* as a workflow.
- **Auditability is mandatory, not nice-to-have.** They must reproduce a score months
  later and show the chain: text → model id → score version → scorer hash → score. This
  is precisely Linnet's brand promise; here it's a hard compliance requirement.

**What they actually need:** provable on-prem/no-network guarantee + a domain-calibration
workflow + a reproducible, hash-pinned audit trail per score.

---

## The cross-cutting gaps, ranked by how hard they block adoption

| Rank | Gap | Blocks which jobs | Why it's a blocker, not a nicety |
|---|---|---|---|
| **1** | **Structured output: probabilities + explicit `neutral` + confidence**, not a bare scalar | all four | The information already exists in the scorer and is discarded (`R/sentiment.R:111`). Without it, no analyst can triage, defend, or distinguish "strong negative" from "borderline" or "neutral." Cheapest, highest-leverage fix in the package. |
| **2** | **Mixed / aspect-level sentiment** | survey, exit, reviews | A document mean on bimodal text is statistically wrong and *hides the actionable signal*. This is the difference between a toy and a tool for verbatims — Linnet's core market. |
| **3** | **Domain-fit / drift warning + a calibration workflow** | reviews, finance, clinical | The number looks fine while silently degrading out-of-domain. Honest tools warn; this *is* the trustworthiness brand made concrete. |
| **4** | **Reproducible, versioned, citable scores (audit trail)** | finance, clinical, survey | Pinned model+scorer versions and a per-score provenance record. Compliance-blocking in regulated jobs; brand-defining everywhere. |
| **5** | **Throughput: caching/dedup, resumable batch, pre-flight cost/time estimate** | reviews, large survey | The wall every pilot hits at scale; a surprise OpenAI bill or a non-resumable 500k job ends adoption quietly. |
| 6 | On-prem / no-network *proof* (not just capability) | finance, clinical | v2 mostly delivers the capability; the gap is making the guarantee explicit and verifiable. |
| 7 | Exportability (tidy `data.table`/`tibble`, IDs preserved, `broom`-ish shape) | all | Scores must flow into `dplyr`/`gt`/Quarto without reshaping. `sentiment_match()` already returns a table; `sentiment_score()` should too. |

**Why this ranking.** Gaps 1 and 2 are about *the shape of the answer* and block the
everyday verbatim job that is Linnet's home market — and #1 is nearly free because the
scorer already computes the probabilities. Gaps 3 and 4 are where the **trustworthiness
brand stops being a slogan and becomes a feature**: a tool that *warns when it's out of
its depth* and *can be reproduced and cited* is the differentiator the bake-off can't buy.
Gap 5 is the operational wall that ends pilots regardless of accuracy.

---

## Concrete moves (grounded; no new claims about accuracy)

- **Return structure by default, scalar by request.** Add `output = c("score","prob","data.frame")`
  to `sentiment_score()`. The richer path returns `text, score, neg, neu, pos, label,
  confidence` (e.g. `confidence = max(prob) - second(prob)`, or entropy of the 3-class
  vector). The probabilities already exist in `find_sentiment_probs()` — stop discarding
  them at `R/sentiment.R:111`. Keep the bare-vector default for one release for back-compat,
  then flip. This single change covers gaps #1 and most of #7.
- **Aspect/target mode built on the existing primitives.** `sentiment_match()` already does
  anchor matching; extend it (or add `sentiment_aspect()`) to score sentiment *per supplied
  aspect anchor* and report per-aspect pos/neu/neg. For verbatims, ship a thin
  sentence-splitter (e.g. `tokenizers::tokenize_sentences`) so a mixed comment yields
  per-sentence rows with offsets — this is the segment-level need across survey/exit/reviews.
- **A drift/OOD signal that costs almost nothing.** The embeddings are already in hand.
  Report each row's **max cosine similarity to the scorer's training anchors / class
  centroids** (the package *already* computes cosine matches — `R/matrix_helpers.R`).
  Low max-similarity = "this text is unlike anything the scorer was calibrated on —
  treat the label with caution." That is a defensible, computed, brand-perfect warning,
  and it reuses code that already exists.
- **Calibration as a first-class workflow, with real corpora.** Document and helper-ify
  "bring your own labels → refit the xgboost head on your embeddings." Ship reproducible
  recipes (not bundled scores) for **SST / SST-5** (Socher et al. 2013), **Financial
  PhraseBank** (Malo et al. 2014) + **Loughran-McDonald** for finance, and **Amazon/Yelp
  polarity** (Zhang et al. 2015) for reviews. Validate with `caret`/`yardstick`
  (macro-F1, Cohen's κ) — packages the audience already trusts.
- **Provenance object.** Attach `attr(scores, "provenance")` (or a column) carrying
  `model_id`, `score_version`, scorer file hash, `sentiment.ai` version, timestamp, and
  `language`. Make scores reproducible and *citable* — which is the whole brand.
- **Throughput primitives.** Hash-dedup identical texts before embedding (reviews repeat);
  cache embeddings to disk keyed by `(text_hash, model_id)`; make batch jobs resumable; and
  add a `dry_run`/`estimate` that reports rows, est. tokens, and est. cost/time **before**
  the OpenAI path spends a cent.

---

## TOP 3 (the ones that most block adoption)

1. **Stop throwing away the probabilities — return structured output with an explicit
   `neutral` class and a per-row confidence.** The 3-class scorer already computes this and
   `sentiment_score()` discards it one line before returning (`R/sentiment.R:111`). Without
   it, no practitioner can triage, defend, or tell a strong negative from a coin-flip — and
   it's the cheapest fix in the package.

2. **Detect mixed / aspect-level sentiment instead of averaging it away.** Verbatims, exit
   interviews, and reviews are routinely bimodal; a document mean is statistically
   indefensible and *hides the actionable driver*. Build it on the anchor/cosine primitives
   that already exist (`sentiment_match()`, `cosine_match()`) plus sentence splitting. This
   is what separates a toy from a verbatim tool in Linnet's core market.

3. **Make scores honest about their limits and reproducible: an out-of-domain/drift warning
   plus versioned, hash-pinned, citable provenance.** The OOD signal is almost free (max
   cosine similarity to training anchors — code already present), and it converts Linnet's
   "trustworthy, computed-not-hyped" brand from a tagline into the actual differentiator that
   a competitive macro-F1 score cannot give a practitioner who has to defend the number.
