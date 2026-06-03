# sentiment.ai v2 — Benchmark Page Spec

**Page:** `benwiseman.github.io/sentiment.ai` → the public "bake-off results" page (docs site / pkgdown article or standalone `docs/benchmark.html`).
**Purpose:** the *pretty visuals* of the v2 embedder bake-off. One scrollable narrative: **"You can now drop TensorFlow and lose nothing."**
**Audience:** R / Python users, IO-psych & HR-analytics researchers, students on laptops with no GPU. Marketing-adjacent but **evidence-first** — every claim has a chart and a caveat.
**Status of this doc:** spec only (charts, data sources, narrative, framing, IP boundaries). Not the page itself.
**Author / attribution:** Ben Wiseman. Package is MIT, Korn Ferry Institute–attributed — keep KFI credit in the page footer.

---

## 0. Ground truth (do not fabricate — these are the only numbers the page may show)

All numbers below are **real**, pulled from the bake-off harness at
`/Users/benwiseman/sentiment.ai_training/sentiment.ai_training/bakeoff/` (per-model `sub_f1_*.csv`)
and the training repo's per-class files (`models/f1_scores_*.csv`). Macro-F1 = unweighted mean of the three class F1s.

**Provenance of the split** (`bakeoff/make_subsample.R`): ONE fixed subsample reused by every candidate so the ranking is fair.
- Test = the real **3,255-row** held-out split (reproduces `create_dmatrix`'s RNG: `set.seed(42)` → balanced 43,437/class → `createDataPartition(p=0.975)`).
- Train = **18,000** rows (6,000/class, seed 2024) subsampled from the remaining train pool.
- Scorer = XGBoost, same recipe for every embedder; only the embed step changes. Dev-Mac XGBoost is single-threaded (no OpenMP) — note this if any timing is shown.

### Subsample leaderboard (PROVISIONAL — 3,255 test / 18,000 train)

| model (page label) | HF / API id | dim | F1(−1) | F1(0) | F1(+1) | **macro-F1** | on-device | no-TF | multilingual |
|---|---|---:|---:|---:|---:|---:|:--:|:--:|:--:|
| **OpenAI 3-small** | `text-embedding-3-small` (API) | 1536 | 0.914 | 0.834 | 0.834 | **0.861** | no (API) | yes | — |
| **e5-base** | `intfloat/multilingual-e5-base` | 768 | 0.915 | 0.833 | 0.831 | **0.860** | yes | yes | ~100 langs |
| bge-base | `BAAI/bge-base-en-v1.5` | 768 | 0.898 | 0.809 | 0.801 | **0.836** | yes | yes | en |
| gte-base | `thenlper/gte-base` | 768 | 0.892 | 0.791 | 0.791 | **0.825** | yes | yes | en |
| bge-small | `BAAI/bge-small-en-v1.5` | 384 | 0.885 | 0.794 | 0.793 | **0.824** | yes | yes | en |
| **e5-small** (default) | `intfloat/multilingual-e5-small` | 384 | 0.861 | 0.800 | 0.779 | **0.813** | yes | yes | ~100 langs |
| mml-mpnet | `…multilingual-mpnet…` | 768 | 0.850 | 0.799 | 0.757 | **0.802** | yes | yes | multi |
| **USE-large (legacy)** | tfhub Universal Sentence Encoder | 512 | 0.845 | 0.782 | 0.744 | **0.790** | yes | **❌ TF** | multi |
| mpnet | `sentence-transformers/all-mpnet-base-v2` | 768 | 0.838 | 0.763 | 0.742 | **0.781** | yes | yes | en |
| paraphrase (old no-TF) | `paraphrase-MiniLM-L6-v2` | 384 | 0.786 | 0.759 | 0.708 | **0.751** | yes | yes | multi |

**Two headline facts the page is built around:**
1. **No-TF beats TF.** The best on-device no-TF embedder (**e5-base, 0.860**) beats the old TF default (**USE-large, 0.790**) by **+0.070 macro-F1** — and even the *small* default (**e5-small, 0.813**) beats it by **+0.023**. The TF tax bought you nothing.
2. **e5-base ties OpenAI.** On-device **e5-base 0.860** vs API **OpenAI 0.861**. The gap is **0.001** — i.e. noise on a 3,255-row subsample. You do not need to send text to an API to get top-tier accuracy.

> **e5 models need a `query: ` prefix** at embed time (documented quirk of the E5 family; the package handles it internally). The page may mention this once as a footnote — it is NOT an accuracy caveat, it is a usage detail.

### Synthetic-neutral curve (separate, orthogonal finding)

Source: `test_methods/multiclass_neutral_percentage_results.csv` + `neutral_mix_experiment.Rmd` (title: *"Benefit of simulated neutral data"*, Wiseman, 2024). XGBoost on USE embeddings, training-set neutral proportion swept 1%→99%, fixed validation set.

| % neutral in train | F1(+1) positive | F1(−1) negative | F1(0) neutral |
|---:|---:|---:|---:|
| 1% | 0.586 | 0.785 | — |
| 25% | 0.758 | 0.827 | 0.770 |
| **33%** | **0.766** (peak) | 0.826 | 0.806 |
| 50% | 0.738 | 0.814 | 0.816 |
| 99% | 0.014 | 0.040 | 0.575 |

**The finding:** neutral data is scarce in real sentiment corpora; adding (GPT-4o-synthesized) neutral rows up to ~⅓ of training **raises** the F1 of the **positive and negative** classes — not just the neutral class. Positive F1 climbs from 0.586 → 0.766 as neutral share goes 1% → 33%, then both extremes collapse. There is a sweet spot, and ignoring neutral hurts the classes you *do* care about.

---

## 1. Narrative arc (the scroll, top to bottom)

The page is a **single argument in five beats**, each beat = one section = one chart. Order matters; do not reshuffle.

1. **The hook — "Drop TensorFlow. Keep the accuracy."**
   One sentence + the **no-TF-beats-TF bar** (Chart 1). The whole pitch in one viewport: the model we're escaping (USE/TF) is *below* every modern no-TF option. Install pain was never buying accuracy.

2. **"You don't even need an API."**
   The **e5-base ties OpenAI** comparison (Chart 2). Top on-device model lands within 0.001 of the paid API. Privacy + cost + offline, at no accuracy cost.

3. **"Bigger isn't the point — pick your tier."**
   The **macro-F1 vs model-size scatter** (Chart 3). Lets the reader choose: e5-small (default, 384-D, ships small) vs e5-base (best on-device, 768-D) vs OpenAI (API). Shows diminishing returns and where the defaults sit on the frontier.

4. **"We fixed the positive class."**
   The **per-class F1** chart (Chart 4) + the **synthetic-neutral curve** (Chart 5). The hardest class in 3-class sentiment is positive (it confuses with neutral); show how the modern embedders + the neutral-augmentation finding lift it.

5. **"And it works in ~100 languages."**
   The **multilingual coverage** panel (Chart 6). The default (e5-small) and best on-device (e5-base) are multilingual-E5 — the legacy USE multilingual model now has a stronger, TF-free replacement.

Close with: **honest-framing callout** (provisional/subsample, full-data run in progress, OpenAI nominally +0.001) + **reproducibility / how-to-run** block + **citation/credits**.

---

## 2. Chart specifications

> Conventions for every chart: deterministic, computed-not-AI; macro-F1 = mean of 3 class F1s; colour-encode **no-TF (teal/green) vs TF-legacy (grey/amber)**; the two package defaults (e5-small, e5-base) and OpenAI always **labelled and highlighted**; every chart carries a one-line caption with N and "provisional/subsample". Suggested stack: static (ggplot2 PNG/SVG baked from the CSVs) for reproducibility, optionally re-skinned interactive (Plotly/Vega-Lite/ECharts) — but the **numbers come from the CSVs, never hand-typed**.

### Chart 1 — "No-TF beats TF" (the hero bar)
- **Type:** horizontal bar, sorted descending by macro-F1.
- **X:** macro-F1 (start axis ~0.70, not 0, to make the spread readable — but annotate that the axis is truncated, honestly).
- **Y:** model label.
- **Encoding:** USE-large bar in **legacy/grey** with a "requires TensorFlow" tag; all bars above it in **no-TF teal**; draw a **dashed reference line at USE-large = 0.790** ("old TF default") so the eye sees everything modern sitting above it.
- **Highlight:** e5-small (default) and e5-base (best on-device) get a marker/label; OpenAI labelled "API".
- **Caption:** "Macro-F1, 3-class. Subsample: 3,255 test / 18,000 train. Provisional — full-data run in progress. USE-large is the only TensorFlow model shown; every model above it runs with no TensorFlow."
- **Data:** the macro-F1 column of the leaderboard table (§0).

### Chart 2 — "e5-base ties OpenAI" (the dead-heat)
- **Type:** two-bar (or dumbbell) head-to-head: **e5-base 0.860** vs **OpenAI 3-small 0.861**, with a callout of the **0.001 gap**.
- **Annotation:** "on-device, no API" under e5-base; "paid API" under OpenAI. A small bracket labelling the gap as "≈ noise on 3,255 rows."
- **Optional:** faint error-context note ("a 0.001 difference on a 3,255-row subsample is not a meaningful lead; the full-data run will confirm").
- **Honesty rule:** OpenAI is **nominally ahead** — say so. Frame as "ties / statistical dead heat," never "e5-base wins." Do not invert the ranking.
- **Data:** two rows of the leaderboard.

### Chart 3 — "Accuracy vs model size" (the frontier scatter)
- **Type:** scatter. **X = embedding dimension** (proxy for size/cost: 384 / 512 / 768 / 1536) on a log or ordinal axis; **Y = macro-F1**. Point label = model.
- **Encoding:** shape/colour for backend kind (sentence-transformers on-device vs API vs TF-legacy). Optionally size points by a cost/footprint proxy *only if a real number exists* — otherwise keep size constant (do **not** invent latency/$ figures; see §4).
- **Annotations:** circle the two defaults; draw the **Pareto frontier** (e5-small at 384-D, e5-base at 768-D, OpenAI at 1536-D are the frontier; USE/mpnet/paraphrase are dominated). Caption notes "dimension is a rough size proxy, not a strict cost axis."
- **Caption:** "Higher and to the left is better (more accuracy, smaller vectors). e5-small (384-D) gives most of the accuracy at the smallest footprint; e5-base (768-D) reaches the API tier on-device."
- **Data:** macro-F1 + dim columns from the leaderboard (dims are verified from the embedding files: e5-small/bge-small/paraphrase = 384, USE = 512, bge/gte/mpnet/e5-base = 768, OpenAI = 1536).

### Chart 4 — "Per-class F1, with the positive-class fix"
- **Type:** grouped bars (3 bars/model: −1 / 0 / +1) **or** a slim per-class small-multiple. Keep to a curated set so it's readable: **USE-large (legacy) · e5-small (default) · e5-base · OpenAI** (4 models × 3 classes).
- **Story it must show:** the **positive class (+1) is the weakest** for every model (it confuses with neutral) — and it's exactly where the jump from legacy→modern is largest. USE-large +1 = **0.744** → e5-base +1 = **0.831** (+0.087); e5-small +1 = 0.779. Negative is easiest everywhere (~0.85–0.92).
- **Encoding:** consistent class colours across models; annotate the +1 lift from USE-large to e5-base.
- **Caption:** "Positive is the hard class in 3-way sentiment (it sits between neutral and positive). Modern no-TF embedders lift it most."
- **Data:** the F1(−1)/F1(0)/F1(+1) columns of the leaderboard.

### Chart 5 — "Why neutral data helps the other classes" (synthetic-neutral curve)
- **Type:** line chart. **X = % neutral in training set** (1 → 99); **three lines = F1 positive / negative / neutral**.
- **Story:** positive-class F1 rises **0.586 → 0.766** as neutral share goes 1%→33%, peaks near **⅓ neutral**, then all classes collapse toward the 99% extreme. Mark the **sweet spot (~33%)** with a vertical guide; shade the "too little neutral" and "too much neutral" zones.
- **Framing (critical, this is a research claim):** label it **"finding — provisional"**; this is a single-condition experiment (one embedding space = USE, XGBoost, one seed, sampled data). State plainly: *"adding synthetic neutral data up to ~⅓ of training raises F1 on the positive and negative classes — not just neutral. Hardening (seeds, CIs, vs class-weights/SMOTE baselines) is in progress before this is a published result."* Do **not** present it as settled or general.
- **Caption:** "Source: neutral-mix experiment (Wiseman 2024). Synthetic neutral rows from GPT-4o. Single-condition; treat as a directional finding, not a benchmark."
- **Data:** `test_methods/multiclass_neutral_percentage_results.csv`.

### Chart 6 — "Multilingual coverage"
- **Type:** small panel — a **world/region badge or simple icon row**, NOT a fabricated per-language accuracy chart (we have **no per-language F1 numbers**; do not invent them).
- **Content:** state factually that the default (e5-small) and best on-device (e5-base) are **multilingual-E5**, trained for **~100 languages**; the legacy multilingual USE model now has a TF-free, stronger replacement. English-only models (bge/gte/mpnet) flagged as such in the leaderboard's "multilingual" column.
- **Honesty rule:** the bake-off F1 numbers are on an **English-dominant** corpus. The multilingual claim is about the **embedder's training coverage** (cited: the multilingual-E5 model card / paper), **not** a measured multilingual sentiment benchmark. Say this explicitly: *"Multilingual coverage refers to the embedding model; our reported F1 is measured on an English-dominant test set. Cross-lingual sentiment evaluation is future work."*
- **Data:** model cards for `intfloat/multilingual-e5-{small,base}` (language coverage); the "multilingual" column of the leaderboard.

---

## 3. Honest-framing callout (must appear on the page, near the leaderboard)

A boxed, non-skippable note — credibility is the brand:

- **Provisional & subsampled.** Numbers are from a **3,255-test / 18,000-train subsample** with one fixed split, XGBoost scorer. A **full-data run is in progress**; rankings near the top (OpenAI vs e5-base, the 0.860/0.861 pair) are within noise and may swap. Page will be updated with full-data numbers + the date.
- **OpenAI is nominally ahead (+0.001).** State it. e5-base **ties**, it does not beat OpenAI on these numbers.
- **No significance tests yet.** Single seed, point estimates, no CIs. Treat differences < ~0.01 as ties.
- **English-dominant test set.** F1 is measured on a largely-English corpus; multilingual = embedder coverage, not a measured multilingual benchmark.
- **Contamination caveat.** Public corpora (sentiment140/IMDb/Amazon, etc.) may have been seen during embedder pretraining → possible optimism for all neural models equally; a recent held-out set is future work.
- **Date-stamp** the page and link the harness so claims are checkable.

(These mirror the publication plan's "what I would not claim" — the public page is explicitly **not** presented as peer-reviewable evidence; it's an honest engineering bake-off.)

---

## 4. IP-safe boundaries (what the page may and may NOT show)

**May show (display-ready facts):**
- Macro-F1 and per-class F1 per model (the leaderboard).
- Model **names/ids and dims** (these are public HF/OpenAI identifiers).
- The fixed-split methodology in prose + a link to the harness (`make_subsample.R`, `embed.py`/`sub_embed.py`, `train.R`) — reproducibility is a feature.
- The corpora *names* and counts (public datasets).
- The neutral-mix curve as a **provisional finding**.

**Must NOT show / keep server-side or out of scope:**
- **No trained XGBoost model weights, no DMatrix binaries, no embedding matrices** on the page (these are large and are the trained IP; link conceptually, do not host the `.xgb`/`.bin`/`sub_emb_*.csv` files as page assets).
- **No invented numbers.** No fabricated latency, $/1k-docs, throughput, or per-language F1 unless a **real measured value** exists in the repo. (Timing is single-threaded dev-Mac and not representative — omit or caveat heavily; do not publish a misleading speed claim.)
- **No competitor / copier name-checks.** Let publish dates and provenance speak (sentiment.ai predates the imitators). No "X copied us."
- **Never frame sentiment.ai's own output as "AI."** It's a deterministic embedding → XGBoost scorer. "AI" appears only when describing the *embedders'* lineage or competitors. The package is **computed, auditable**, not "AI-generated."
- **No synthetic-neutral training data samples** that could leak the augmentation prompts/recipe beyond what the paper will disclose.

**Attribution that MUST appear:** MIT licence; "Funded by / attributed to the Korn Ferry Institute"; author Ben Wiseman; co-authors (Nydick, Wisner) as on the package. Keep KFI authorship history intact.

---

## 5. Build notes (non-binding implementation hints)

- **Single source of truth:** generate all charts from the CSVs via one script (e.g. `docs/make_benchmark_figs.R`) so the page can't drift from the data. Re-run when the full-data numbers land.
- **Defaults always legible:** e5-small (default) and e5-base (best on-device) must be visually distinct in every chart; OpenAI always tagged "API".
- **Truncated axes annotated** wherever used (Chart 1) — don't mislead with a zoomed bar.
- **Mobile:** the hero bar (Chart 1) and the e5-vs-OpenAI dead-heat (Chart 2) must read on a phone; the rest can stack.
- **Update hook:** leave a visible "Last updated / full-data status" line tied to the harness run.

---

## 6. Chart list (the deliverable)

1. **Chart 1 — No-TF-beats-TF bar.** Horizontal macro-F1 bar, all models, USE-large (TF) greyed below a dashed 0.790 reference line; every no-TF model above it; defaults highlighted. *Headline: the TF tax bought no accuracy.*
2. **Chart 2 — e5-base ties OpenAI.** Two-bar / dumbbell head-to-head, e5-base 0.860 vs OpenAI 0.861, 0.001 gap called out as "≈ noise"; on-device vs API tags. *Headline: top accuracy without an API.*
3. **Chart 3 — Macro-F1 vs model-size scatter.** X = embedding dim (384/512/768/1536), Y = macro-F1, Pareto frontier drawn, two defaults circled. *Headline: pick your tier; small e5 gets most of it.*
4. **Chart 4 — Per-class F1 (incl. positive-class fix).** Grouped bars, −1/0/+1, curated 4 models (USE-large, e5-small, e5-base, OpenAI); annotate +1 lift 0.744→0.831. *Headline: the hard positive class, fixed.*
5. **Chart 5 — Synthetic-neutral curve.** Line chart, F1 of pos/neg/neutral vs % neutral in training (1→99), ~33% sweet spot guide; labelled "provisional finding." *Headline: neutral data lifts the other classes.*
6. **Chart 6 — Multilingual coverage panel.** Factual badge/icon panel: e5-small & e5-base = multilingual-E5, ~100 languages; English-only models flagged; explicit caveat that F1 is English-dominant. *Headline: works beyond English (embedder coverage).*

Plus two non-chart blocks: **Honest-framing callout** (§3) and **Reproducibility + citation/credits** (KFI/MIT/author).
