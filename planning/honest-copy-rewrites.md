# sentiment.ai v2 — Honest-copy rewrites

> Applies the adversarial reviews' one-line honesty rewrites
> (`review-lineup.md` SEV-2/4/6, `review-methodology.md` S1–S7, S3 §"One-line honesty rewrites")
> across every customer-facing copy surface: **NEWS**, **DESCRIPTION**, the **docs benchmark
> page**, and the **README/docs benchmark section**. Final copy blocks below are drop-in
> replacements for the prior drafts.
>
> Five corrections, applied consistently everywhere:
> 1. **Decouple "ties OpenAI" from "the default."** The model that ties OpenAI is **e5-base**.
>    The default is **e5-small** (0.813 provisional), which is 4.7 macro-F1 points behind it.
>    Never let "ties OpenAI" sit next to "the default."
> 2. **Frame the OpenAI/e5 result as a statistical tie, not a measured equality.**
>    "Statistically indistinguishable on this English benchmark (paired bootstrap, 95% CI on
>    ΔF1 includes 0)." OpenAI is nominally +0.001 ahead — say so; never "e5-base wins."
> 3. **Label absolute F1 as upper bounds.** Partially-public corpora (sentiment140/IMDb/Amazon
>    may be in encoder pretraining) and the balanced neutral class is ~93% synthetic, so these
>    are upper bounds on real-world generalisation, not generalisation estimates.
> 4. **Scope the multilingual claim.** "Multilingual backbone (e5); not independently
>    benchmarked here — the test set is English-derived."
> 5. **Soften the legacy "better replacement" table** to "a no-TF replacement **at or above**
>    the old USE default's accuracy," not a model-for-model "better" (only USE-large was
>    benchmarked; `en`/`multi` USE-small/multilingual were not).
>
> Status discipline retained from the drafts: every number here is **provisional subsample**
> (3,255 test / 18,000 train, `num_parallel_tree = 1`); the full-data `num_parallel_tree = 24`
> run is still in flight (and the oai/use_lg full-subset extraction must be re-run — it wrote
> 0 rows). Do not promote any figure to a published headline until that run is clean and the
> paired bootstrap from `review-methodology.md` S3 has been computed.

---

## ★ Corrected default / headline line (the canonical sentence)

> **sentiment.ai 2.0 drops TensorFlow as the default.** The default embedder, **e5-small**, is
> a fast, lightweight, multilingual-backbone, on-device model that runs with no TensorFlow and
> clears the old TensorFlow USE-large default it replaces (0.790 → **0.813** macro-F1,
> provisional). For top accuracy, the **e5-base** option is — on this English benchmark —
> **statistically indistinguishable from paid OpenAI embeddings** (paired bootstrap; 95% CI on
> ΔF1 includes 0), entirely on-device. Absolute F1 figures are **upper bounds** (partially-public
> corpora; the balanced neutral class is ~93% synthetic) and remain **provisional** pending the
> full-data run.

Use this sentence (or the tighter variant below) anywhere a single headline is needed.

**Tight variant (one line, for DESCRIPTION / social / page title):**

> No TensorFlow by default: the **e5-small** default beats the old USE default on a clean
> install, and the on-device **e5-base** option is statistically indistinguishable from paid
> OpenAI on an English benchmark (provisional; absolute F1 are upper bounds).

**Two facts, never collapsed into one:**
- *The default* (e5-small, 0.813) **beats the old TF default** (USE-large, 0.790). ✅
- *The top on-device option* (e5-base, 0.860) **ties OpenAI** (0.861) — within noise. ✅
- ❌ Never: "the new default ties OpenAI." (The default is e5-small; it does not.)

---

## Surface 1 — NEWS (`planning/NEWS-v2-draft.md` → `NEWS.md`)

### 1a. Opening paragraph — REPLACE lines 3–9 ("ties paid OpenAI" conflation)

> `sentiment.ai` 2.0 makes the package fast, modern, and install-clean by default. The big
> change: **TensorFlow is no longer required.** The default sentiment pipeline now runs on a
> no-TensorFlow, on-device embedder (**e5-small**), and the legacy TensorFlow Universal
> Sentence Encoder path is preserved as an explicit opt-in. The new default **beats the old
> TensorFlow USE default it replaces** on a clean install, and for users who want top accuracy,
> an on-device **e5-base** option is — on this English benchmark — **statistically
> indistinguishable from paid OpenAI embeddings** (paired bootstrap; 95% CI on ΔF1 includes 0),
> with no API and no TensorFlow. If you only ever called `sentiment_score()` /
> `sentiment_match()` with the defaults, you get a cleaner install and a stronger default — and
> your existing USE scripts still work.

### 1b. Provisional banner — REPLACE the blockquote at lines 11–16 (add upper-bound + tie language)

> > **Accuracy figures are provisional and are upper bounds.** The numbers below come from a
> > fixed, reproducible **subsample** bake-off (6,000 train rows/class; the real 3,255-row
> > held-out test split). The full-data headline run (full train pool, `num_parallel_tree = 24`,
> > same test set) is in progress and this entry will be finalised with those figures before
> > release. Treat macro-F1 here as a **provisional ranking**, not a published headline. Treat
> > absolute F1 as an **upper bound on real-world accuracy**, not an estimate of it: several
> > source corpora are public (sentiment140 / IMDb / Amazon) and may sit in the embedders'
> > pretraining, and the balanced neutral class is ~93% GPT-synthesised text. We report these
> > numbers to **compare embedders under one identical pipeline**, not to predict accuracy on
> > your data.

### 1c. Bake-off read-out — REPLACE lines 78–86 (decouple tie from default; upper-bound caveat)

> Read-out (provisional subsample; absolute F1 are upper bounds):
> - **e5-base is statistically indistinguishable from OpenAI on this English benchmark**
>   (0.860 vs 0.861; paired-bootstrap 95% CI on ΔF1 includes 0 — pending the full-data confirm),
>   while running fully **on-device with no TensorFlow**. OpenAI is nominally +0.001 ahead; this
>   is a dead heat, not an e5-base win.
> - **e5-small is the default** at 0.813 — chosen for being fast, lightweight, and on a
>   multilingual backbone, and it **clears the old TF USE-large default (0.790 → 0.813)** on a
>   clean no-TF install. It is **not** the ties-OpenAI model: that is e5-base, one flag away.
>   (For English-only users, the smaller-footprint bge-small is competitive at the 384-D tier —
>   e5-small's edge is the multilingual backbone, not a leaderboard win.)
> - The 2021-era `paraphrase-MiniLM` (0.751) and `mpnet` (0.781) — the models a naive "just drop
>   sentence-transformers in" would reach for — are **too weak** to be the default; the e5 family
>   is what clears the bar. We ran the bake-off precisely so the default change is an upgrade,
>   not a TensorFlow-removal tax on accuracy.

### 1d. Prefix + multilingual note — REPLACE the blockquote at lines 88–90 (scope the claim)

> > The e5 models require a `"query: "` prefix on each input; `sentiment.ai` applies it
> > internally for the e5 backends so you never pass it yourself. The e5 backbone is
> > **multilingual** (the upstream model card lists ~100 languages), so the multilingual reach
> > of the old USE backend is **carried forward** in capability. Note our reported F1 is measured
> > on an **English-derived** test set: multilingual sentiment quality is inherited from the e5
> > backbone and is **not independently benchmarked here**.

### 1e. Lineup table — REPLACE the e5-base "Use it for" cell (lines 96–97)

In *The new model lineup* table, change the e5-base row's "Use it for" cell:

| Model name (in `sentiment.ai`) | Embedder | Use it for | Install |
|---|---|---|---|
| **e5-small** (default) | `intfloat/multilingual-e5-small` | fast, lightweight, on-device, multilingual backbone | default (no TF) |
| **e5-base** | `intfloat/multilingual-e5-base` | best on-device accuracy (ties OpenAI on this English benchmark, within noise) | default (no TF) |

### 1f. Legacy replacement table — REPLACE lines 113–119 (soften "better" → "at or above")

> - **Each legacy USE model has a no-TensorFlow replacement at or above the old USE default's
>   accuracy.** On the one legacy model we benchmarked (USE-large, 0.790), every shipped no-TF
>   option meets or beats it; for new work there is a TF-free option that does not cost you
>   accuracy:
>
>   | Legacy USE model | TF-free replacement | Macro-F1 (provisional, upper bound) |
>   |---|---|---|
>   | `en` / `en.large` (English USE) | `e5-base` (on-device) or `openai` (API) | USE-large benchmarked at 0.790 → **0.860 / 0.861** |
>   | `multi` / `multi.large` (multilingual USE) | `e5-base` / `e5-small` (multilingual backbone) | **0.860 / 0.813** (vs USE-large 0.790; `multi`/`multi.large` not separately benchmarked) |
>
>   *Only USE-large was run in the bake-off. We benchmarked it as the strongest legacy default;
>   the smaller `en`/`multi` USE variants were not separately measured, so the replacement is
>   stated as "at or above the old USE default," not a model-for-model "better."*

---

## Surface 2 — DESCRIPTION (`rpackage/sentiment.ai/DESCRIPTION`, `Description:` field)

> **Replaces the `Description:` field.** Keeps CRAN's prose-only constraint (no markdown,
> wrapped, continuation lines indented). Decouples default from tie; scopes the multilingual
> claim; caveats the benchmark.

```
Description: Sentiment analysis via deep-learning embeddings and gradient-boosted scoring,
  with the setup hassle handled so the process is as simple as possible. As of 2.0 the
  default pipeline runs on a no-TensorFlow, on-device embedder; the legacy TensorFlow
  Universal Sentence Encoder backend is preserved as an opt-in. The default model (e5-small)
  uses a multilingual backbone and beats the old TensorFlow default it replaces on an internal
  benchmark; a higher-accuracy on-device option (e5-base) is statistically indistinguishable
  from paid OpenAI embeddings on an English benchmark (see
  <https://benwiseman.github.io/sentiment.ai/#Benchmarks>; figures are provisional and are
  upper bounds, measured on partially-public corpora). The package also returns embedding
  vectors for use in other analyses. GPU acceleration is supported on Windows and Linux.
```

*(Separately, per `review-lineup.md` SEV/CRAN notes: drop the unused `openai` Import, guard the
TF/text2vec/tfhub Suggests, and move runtime weight downloads out of the package dir. Those are
code/metadata fixes owned by the main thread — flagged here only so the copy and the manifest
don't contradict each other.)*

---

## Surface 3 — Docs benchmark page (`docs-benchmark-page-spec.md` → public page)

### 3a. Page sub-hed / "two headline facts" — REPLACE §0 lines 37–41

> **Two headline facts the page is built around:**
> 1. **No-TF beats TF.** The best on-device no-TF embedder (**e5-base, 0.860**) beats the old TF
>    default (**USE-large, 0.790**) by **+0.070 macro-F1**, and even the **default** (**e5-small,
>    0.813**) beats it by **+0.023**. The TensorFlow tax bought no accuracy.
> 2. **The top on-device model ties OpenAI.** On-device **e5-base 0.860** vs API **OpenAI
>    0.861** — a **0.001** gap, i.e. statistically indistinguishable on this English benchmark
>    (paired bootstrap, 95% CI on ΔF1 includes 0; pending the full-data confirm). You don't need
>    to send text to an API for top-tier accuracy. **Note:** this is **e5-base**, not the
>    default — the default is e5-small (0.813). OpenAI is nominally +0.001 ahead; this is a tie,
>    not an e5-base win.
>
> > All absolute F1 on this page are **upper bounds**, not generalisation estimates: several
> > source corpora are public (sentiment140 / IMDb / Amazon) and may be in the embedders'
> > pretraining, and the balanced neutral class is ~93% GPT-synthesised. Read the leaderboard as
> > a **relative embedder comparison under one fixed pipeline**.

### 3b. Beat 2 of the narrative arc — REPLACE §1 beat 2 (lines 66–68)

> 2. **"You don't even need an API (for top accuracy)."**
>    The **e5-base ties OpenAI** comparison (Chart 2). The top on-device model lands within
>    0.001 of the paid API — statistically indistinguishable on this English benchmark.
>    Privacy + cost + offline, at no measured accuracy cost. (This is the e5-base tier, one flag
>    above the e5-small default.)

### 3c. Chart 2 honesty rule — REPLACE §2 Chart 2 annotation/honesty lines (95–101)

> - **Annotation:** "on-device, no API" under e5-base; "paid API" under OpenAI. Bracket the gap
>   as "≈ noise — statistically indistinguishable (paired bootstrap, 95% CI on ΔF1 includes 0)."
> - **Honesty rule:** OpenAI is **nominally +0.001 ahead** — say so. Frame as "statistical tie /
>   dead heat on this English benchmark," never "e5-base wins," and never imply this is the
>   default (it is e5-base; the default is e5-small). Do not invert the ranking.

### 3d. Chart 6 multilingual caveat — REPLACE §2 Chart 6 honesty rule (line 126)

> - **Honesty rule:** the F1 numbers are on an **English-derived** corpus. The multilingual
>   claim is about the **embedder's training backbone** (cited: the multilingual-e5 model card /
>   paper), **not** a measured multilingual sentiment benchmark. State it plainly:
>   *"Multilingual support is inherited from the e5 backbone; it is not independently benchmarked
>   here. Our reported F1 is measured on an English-derived test set. Cross-lingual sentiment
>   evaluation is future work."*

### 3e. Honest-framing callout — REPLACE §3 bullets (133–140) with the consolidated block

> **Honest framing (boxed, non-skippable — credibility is the brand):**
> - **Provisional & subsampled.** Numbers are from a **3,255-test / 18,000-train** subsample,
>   one fixed split, XGBoost scorer, `num_parallel_tree = 1`. A **full-data run
>   (`num_parallel_tree = 24`)** is in progress; the top pair (OpenAI vs e5-base, 0.860/0.861) is
>   within noise and may swap. This page is updated with full-data numbers + a date.
> - **Absolute F1 are upper bounds.** Source corpora are partially public (sentiment140 / IMDb /
>   Amazon may be in encoder pretraining) and the balanced neutral class is ~93% synthetic. Read
>   the leaderboard as a **relative** embedder comparison, not a real-world accuracy estimate.
> - **The default is e5-small, not the ties-OpenAI model.** e5-base (0.860) ties OpenAI; the
>   default e5-small (0.813) clears the old USE default but is a separate, smaller tier.
> - **It's a statistical tie, not a win.** OpenAI is nominally +0.001 ahead. No CIs are published
>   yet beyond the planned paired bootstrap; treat differences < ~0.01 as ties.
> - **English-derived test set.** Multilingual = embedder-backbone coverage, not a measured
>   multilingual benchmark.
> - **Date-stamped**, with the harness linked so every claim is checkable.

---

## Surface 4 — README / docs benchmark section (`README.md`, Benchmarks anchor)

> The shipped `README.md` Benchmarks section is still v1 (USE/TensorFlow framing). When the v2
> Benchmarks block is written, lead with the corrected default/headline line and these
> caveated bullets. Drop-in block:

> ## Benchmarks
>
> As of 2.0, `sentiment.ai` runs with **no TensorFlow by default**.
>
> - The **default** embedder, **e5-small**, is fast, lightweight, and on a multilingual
>   backbone, and **beats the old TensorFlow USE default it replaces** (0.790 → **0.813**
>   macro-F1, provisional) on a clean install.
> - For top accuracy, the on-device **e5-base** option is — on this English benchmark —
>   **statistically indistinguishable from paid OpenAI embeddings** (0.860 vs 0.861; paired
>   bootstrap, 95% CI on ΔF1 includes 0). OpenAI is nominally +0.001 ahead; this is a tie, not
>   an e5-base win, and it is the **e5-base** tier — not the default.
> - The e5 backbone is **multilingual** (~100 languages per the upstream model card); our
>   benchmark is **English-derived**, so multilingual sentiment quality is inherited, **not
>   independently measured here**.
>
> All absolute F1 figures are **provisional** (subsample run; full-data run in progress) and are
> **upper bounds** on real-world accuracy: several source corpora are public (sentiment140 /
> IMDb / Amazon) and may sit in the embedders' pretraining, and the balanced neutral class is
> ~93% synthetic. Full bake-off, charts, and caveats:
> <https://benwiseman.github.io/sentiment.ai/#Benchmarks>.

---

## Cross-surface checklist (apply before publishing any of the above)

- [ ] No surface puts "ties OpenAI" next to "the default." (Default = e5-small; tie = e5-base.)
- [ ] Every "ties OpenAI" reads "statistically indistinguishable on this English benchmark
      (paired bootstrap, 95% CI on ΔF1 includes 0)"; OpenAI noted as nominally +0.001 ahead;
      never "e5-base wins."
- [ ] Every absolute F1 is labelled **provisional** AND **upper bound** (partially-public
      corpora; ~93% synthetic neutral).
- [ ] Every multilingual mention is scoped: "multilingual backbone; not independently
      benchmarked here (English-derived test set)."
- [ ] Legacy replacement copy says "at or above the old USE default," not model-for-model
      "better" (only USE-large was benchmarked).
- [ ] The paired-bootstrap CI is **computed and the interval includes 0** before the "tie"
      phrasing ships (review-methodology.md S3); until then keep "provisional" prominent.
- [ ] Source claims (these copy blocks) only; no R/ source edits — that tree is owned by the
      main thread.
