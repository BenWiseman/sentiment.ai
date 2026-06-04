# sentiment.ai v2 — Ship review (skeptical staff engineer / tech lead)

> **Role:** ship-lead doing a release gate, not a roadmap author. I read the code on
> branch `v2` and *ran the test suite and the failing call path* before writing this.
> **Date:** 2026-06-03. **Writes:** `planning/` only — I did not touch source.
> **One-line verdict:** the architecture is right and ~80% landed, but the new default
> **cannot complete a single `sentiment_score("good")` call today** — I reproduced the
> error — so this is **not shippable yet**. It is close: the remaining work is wiring and
> validation, not new ML. **Would I depend on it in production today? No.** With the three
> blockers below closed and the score-parity test un-skipped, **yes for batch/offline use.**

This builds on and (where the tree has moved) *corrects* the prior adversarial reviews
(`review-migration.md`, `review-lineup.md`, `review-methodology.md`). Those were written
against an earlier tree; several of their S0 claims are now partly fixed (the ST loader
exists, e5 scorers are packaged, dim-handling and a real testthat suite landed). I verified
the *current* state and re-cut to what actually still gates the release.

---

## Ground truth I established by running it (not by reading the roadmap)

| Check | Result | Evidence |
|---|---|---|
| Test suite, no-Python tier | **49 pass / 5 fail** | `testthat::test_dir(...)` on `load_all` |
| `sentiment_score("good")` end-to-end | **ERRORS** before scoring | reproduced below |
| e5 scorers packaged? | **Yes** — `inst/scoring/xgb/1.0/{e5-small,e5-base}.xgb` | `find inst/scoring` |
| Init dispatch wired to ST loader? | **No** — `init` still calls `load_hub_embedding` (TF) on the default path | `init_and_install.R:507,511` |
| e5 `"query: "` prefix injected anywhere in R? | **No** | grep `R/` = 0 hits; 2 red tests |
| Golden score-parity test active? | **No — all skipped** (no committed fixtures) | `find fixtures` = README only |
| NAMESPACE clean of TF? | **No** — `import(tensorflow)`, `import(tfhub)` still present | `NAMESPACE`; red test G1 |

The reproduced failure (this is the actual blocker, not a theoretical one):

```r
sentiment.ai:::install_scoring_model("e5-small", "xgb", "1.0")
#> Error: 'arg' should be one of "en.large", "en", "multi.large", "multi"
```

`sentiment_score()` defaults `model = "e5-small"` and calls `install_scoring_model(model, ...)`
on its **second line**, which `match.arg(model)`-rejects the new default. The package's own
default call dies before it embeds anything. The migration review flagged this; **it is still
in the tree.**

---

## What's genuinely good (don't relitigate it)

This is not a rewrite. The bones are correct and several hard things are already done well:

- **Three-layer separation is clean** (swappable `sentiment.env$embed` pointer → pure-R xgb/glm
  scorer → install machinery). The TF rot is confined to layer-1's legacy backend exactly as the
  roadmap claims.
- **The 3-class → `[-1,1]` collapse is already correct and version-aware.** `find_sentiment_score`
  (`sentiment.R:274-282`) detects the v2 `multi:softprob` 3-vector and computes `P(pos) - P(neg)`,
  with a legacy binary fallback. That's the right shape and it's robust.
- **`get_embedder.py` (the active one) is well-built.** `load_st_embedder` forces `USE_TF=0/USE_TORCH=1`
  at import (kills the transformers-imports-TF crash), normalises input to 2-D, L2-normalises. The
  legacy TF imports are correctly *lazy, inside the function*. This is the no-TF default done right —
  **it's just not called from R yet.**
- **The dim-coupling fix landed.** `model_dims` registry + the matrix-passthrough width check in
  `sentiment_score` (`sentiment.R:99-104`) kill the old hard-coded 512. (One stray `512` lives in a
  roxygen doc line — caught by a tripwire test, trivial.)
- **The test suite is a real asset.** `helper-skips.R`'s `local_fake_embedder` (records the strings
  the embedder sees, returns deterministic content-hashed vectors, no Python) is exactly the right
  tool — it makes the prefix contract and routing testable with zero TF/Python. The CI gates are
  well-chosen (TF-in-NAMESPACE tripwire, no-512 tripwire, prefix assertion). Several are **red on
  purpose (TDD)** — that's a feature, not a defect, *as long as they're green before tag*.
- **`model_meta.R` provenance registry** (license / source / pinned revision per model) is ahead of
  most CRAN packages and is the structural backbone for the "auditable" brand. Good instinct.

---

## TOP 3 BLOCKERS (I would not tag v2 with any of these open)

### BLOCKER 1 — The default path doesn't run end-to-end. Wire init dispatch + kill the `match.arg`.
**Severity: release-fatal. This is the whole ballgame.**

Two independent breaks on the *default, no-arg* path, both reproduced/confirmed in the live tree:

1. **Init never dispatches to the ST loader.** `init_sentiment.ai()` branches only OpenAI vs
   `load_hub_embedding` (the TF-Hub loader) at `init_and_install.R:496-513`. For `model="e5-small"`
   it takes the **else** branch and hands the HuggingFace id to the *TensorFlow* loader → import/load
   failure. `model_class()` and `load_st_embedder` both exist and are **never called from R**. The
   v2 default has no live load path; the "smoke test" worked only by calling the Python function
   directly, bypassing `init`.
2. **`install_scoring_model()` `match.arg(model)` rejects `e5-small`** (`init_and_install.R:288`) —
   reproduced above. The comment on the line above literally says "Remove match.arg not used" but the
   call is still there.

**Fix (small, mechanical):** in `init`, dispatch on `model_class(model)` → `{st, openai, legacy}`;
the `st` branch calls `load_st_embedder(hf_id, prefix = model_prefix[[model]] %||% "")`. Replace
`match.arg(model)` with a membership check against
`c(names(default_models), names(openai_models), names(legacy_models))` (or drop it — the comment
intended to). **Exit gate:** a no-arg `sentiment_score("good")` returns a number with **zero
TensorFlow importable in the env.** That one assertion is the release gate; everything else is
quality.

### BLOCKER 2 — The e5 prefix is a *train/inference consistency* problem, not a "wire it in" task. Resolve it before trusting any score.
**Severity: silent-accuracy / trust-fatal. This is the subtle one the wiring hides.**

Three facts that only become dangerous *together*:
- The e5 model card requires a `"query: "` prefix; dropping it degrades embeddings (Wang et al. 2024).
- `review-methodology.md` S2 found `grep -c "query:"` over **all bake-off logs = 0** — i.e. the
  "e5-base ties OpenAI (0.860 vs 0.861)" headline may have been measured on the **un-prefixed**
  (degraded) embedding. **The shipped `e5-*.xgb` scorers were almost certainly trained on whatever
  the bake-off produced — unprefixed.**
- The failing `test-e5-prefix.R` demands the R inference path *inject* `"query: "`.

So the trap: **if you wire the prefix into inference but the scorer was trained unprefixed, you now
have a train/inference distribution mismatch — and scores silently get worse, with a green test.**
The prefix being "correct per the model card" is irrelevant; what matters is that **train and
inference embeddings come from the identical recipe.** Right now nobody has established which recipe
the shipped scorer used.

**Fix (must happen in this order):**
1. Determine how the packaged `e5-small.xgb` / `e5-base.xgb` were trained — prefixed or not. (Check
   the bake-off/training repo embed step; the methodology review says the flag was absent → likely
   unprefixed.)
2. **Make a deliberate decision and commit to ONE recipe** end to end. The right answer per the model
   card is `"query: "` on *all* sentiment inputs (symmetric documents — do **not** mix `query:`/
   `passage:`). If the scorer was trained unprefixed, **retrain it prefixed** (cheap — embeddings
   re-extract, xgb retrain is minutes) so train and inference match the model-card-correct recipe.
3. **Re-confirm the parity claim on the prefixed embeddings.** The "ties OpenAI" line is a brand
   claim ("matches OpenAI") and the package's whole credibility rests on it being *true on the recipe
   that actually ships*. If prefixed e5-base still ties OpenAI → keep the headline. If it moves →
   change the headline to whatever is true. (This is squarely the "no overclaiming" brand rule.)
4. Then wire the prefix into the R path *idempotently* (the red tests already encode skip-if-present),
   and **un-skip the golden snapshot test** (Blocker 3) so the recipe is pinned forever.

This is the single item most likely to make a *shipped* number wrong while every test passes. Treat
it as the real blocker, above the CRAN hygiene.

### BLOCKER 3 — Nothing pins the scores. The golden parity tests are all skipped (no fixtures).
**Severity: high — without this you cannot detect Blocker 2 regressing, ever.**

`test-snapshot-scorer.R` and the prefix-regression test are well-designed (committed embeddings →
known `[-1,1]` scores, plus a class-direction check that survives re-snapshot). But `fixtures/`
contains **only README.md**, so every one of them `skip_if_no_fixture()`s out. The suite is green-ish
on routing but **proves nothing about the actual numbers.** A package whose brand is "reproducible,
auditable measurement" that ships with **zero pinned-score tests** is contradicting itself.

**Fix:** commit the fixture embeddings (`emb_e5-small.rds`, `emb_e5-base.rds`, a small labelled
`corpus.rds`) generated *with the final prefixed recipe from Blocker 2*, and the golden score
snapshots. This is ~1 hour and it (a) makes Blocker 2's decision permanent and (b) gives you the
"re-run in 2028, identical number" guarantee the brand sells. **This is the test that makes the
brand true.** Do it last in the sequence (it depends on Blocker 2's recipe) but do not tag without it.

---

## The 80/20 path to a shippable, trustworthy v2

**The 20% of work that gets 80% of the value — in dependency order. ~2-4 focused days.**

**Phase A — Make it run and be honest (the actual release-gate set):**
1. **Blocker 1** — init `model_class` dispatch + drop `match.arg`. *Gate: no-arg score returns a
   number, zero TF.* (hours)
2. **Blocker 2** — settle the prefix recipe, retrain the scorer if it was trained unprefixed,
   re-confirm or restate the OpenAI-parity headline. (half day)
3. **Blocker 3** — commit golden fixtures + un-skip the snapshot/prefix-regression tests on the final
   recipe. (hours)
4. **Strip TF from NAMESPACE** (delete the `@import tensorflow`/`@import tfhub` tags in
   `init_and_install.R`, re-`document()`) — turns CI gate G1 green and is a hard `R CMD check` ERROR
   otherwise. (minutes)
5. **One source of truth for the default.** `DEFAULT_MODEL` is defined and **never used**;
   `init_sentiment.ai`'s signature still defaults to `c("en.large", ...)` and its examples still say
   `en.large`. Point `sentiment_score`, `sentiment_match`, `init`, and the roxygen examples at
   `DEFAULT_MODEL`. (hours)

**Phase B — The CRAN/legacy gate (required to *submit*, not to *function*):**
6. **Move runtime downloads out of the package dir** → `tools::R_user_dir("sentiment.ai","cache")`.
   `install_scoring_model`/`install_default_embeddings` currently `download.file()` into
   `system.file(package=...)` — a CRAN-forbidden write to the library that also *fails on a normal
   read-only install today*. (half day; this is also a real runtime bug, not just policy.)
7. **Remove `:::` into reticulate internals** (`reticulate:::py_install_method_detect`,
   `init_and_install.R:141`, and `local_from_reticulate.R`) — hard `--as-cran` failure. (Mostly
   already vendored in `local_from_reticulate.R`; just stop calling the `:::` one.)
8. **Drop the dead `Imports: openai`** — the R path is hand-rolled `httr`; nothing imports `openai`.
   Reconcile NAMESPACE vs DESCRIPTION (Suggests has TF, NAMESPACE imports it — pick one). (minutes)
9. **Implement `legacy = FALSE` on `install_sentiment.ai`** and have `init` raise a *directed* error
   ("`en.large` is a legacy TensorFlow model — run `install_sentiment.ai(legacy = TRUE)`") instead of
   a raw Python `ImportError`. The whole plan assumes this flag; it doesn't exist. Strip the
   unconditional TF pins from the default module list while you're there. (half day)

**Phase C — Ship the README honesty, defer the rest:**
10. **Reconcile the metrics story.** `models/xgb/1.0/metrics.json` reports `en.large = 0.92` while the
    bake-off honestly reports use-large ≈ 0.79–0.83 macro-F1. Those are *different metrics* (the 0.92
    looks like accuracy or an old number) presented as if comparable. Before any benchmark page ships,
    state the metric, the test set, and the split for **every** number, or you re-introduce exactly the
    overclaim the brand forbids. (hours)
11. **NEWS/DESCRIPTION sweep** for the truthful default story + semver. This is a `0.2.0 → 1.0.0`
    event (embedding-space change → everyone's numbers move). Warn-first on the default flip. (hours)

**Everything in `better-sentiment-roadmap.md` (tidy output, calibration, abstention, agreement,
fairness) is post-v2.** It's excellent and on-brand, but **none of it should block this release** —
and critically, **you cannot calibrate or report agreement on a pipeline whose base scores aren't
even reproducible yet** (Blocker 3). Land the honest, runnable, pinned core first; that *is* the
precondition for the entire differentiator programme. The one Move-1 idea I'd pull *forward* into v2
if there's slack: return the 3-class probs as an attribute on the existing vector (non-breaking) so
the discarded signal is at least *recoverable* from day one — but it's optional, not a gate.

---

## Challenges to the existing plan (where I disagree or would push)

1. **Challenge the default model choice itself.** `e5-small` (0.813 macro-F1) is the **weakest no-TF
   option in your own lineup table.** `bge-small` (0.824) is the *same 384-dim and same no-TF/on-device
   profile* and beats it; `e5-base` (0.860) ties OpenAI. Shipping the weakest model as the flagship
   default, for a brand whose entire pitch is "honest, rigorous measurement," is a positioning own-goal
   waiting for a reviewer to notice. **Options, pick deliberately:** (a) make `e5-base` the default and
   `e5-small` the "fast/small" opt-in — the accuracy story becomes "ties OpenAI out of the box"; or
   (b) keep `e5-small` default for size/speed but say so *explicitly* in the model card ("default
   favours footprint; `e5-base` for max accuracy"). What you can't do is ship the weakest option
   *silently*. (e5's multilingual-100-languages story is a legitimate reason to prefer e5 over
   bge/gte if multilingual is the wedge — if so, *say that's why*, since the English macro-F1 is lower.)

2. **The "smoke-tested, produces correct scores, zero TF" claim is true but misleading as evidence of
   readiness.** It exercised the Python function directly; it did **not** exercise `init` →
   `embed_text` → `sentiment_score`, which is the path that errors (Blocker 1). A green smoke test on a
   bypassed path is how the `match.arg` bug survived. The real readiness signal is the no-arg public
   call + the un-skipped golden test — gate on *those*, not the smoke test.

3. **Python parity scaffold: right contract, don't let it imply parity exists.** `PARITY.md` is genuinely
   good (the `[MATCH]/[SHAPE]/[MAY-DIFFER]` tiering is the correct frame). But `pypackage/` is pure
   `NotImplementedError`. That's fine — *as long as nothing public claims the Python package works.* The
   risk is shipping/announcing "R and Python" when Python is a stub. Ship R v2 alone; the parity spec is
   the contract to fill in *after*, and the golden fixtures from Blocker 3 are exactly the cross-language
   `[MATCH]` test vectors — reuse them, don't invent a second set.

4. **Legacy reproducibility (review-migration S3) is real but I'd *down-rank* it from a v2 blocker to a
   documented caveat.** tfhub.dev → Kaggle decay is a genuine external time bomb, but vendoring the USE
   SavedModels is a meaningful chunk of work for the *opt-in legacy* path that, by definition, fewer
   users hit. **80/20 call:** ship v2 with legacy as honestly-labelled "best-effort, frozen-2021 stack,
   may stop installing" + a directed error, and *defer* the vendoring/mirror to a fast-follow. Don't let
   perfecting the path you're trying to deprecate delay the path you're trying to ship. (This is a
   deliberate disagreement with that review's "do these first" ranking of S3 third — it's important but
   it's not on the v2 critical path.)

---

## Would I depend on this in production today?

**No, today** — the default call errors, no score is pinned, and the headline accuracy claim is
un-validated on the recipe that ships (Blocker 2). Any of those three alone is disqualifying for a
measurement tool.

**Yes, for batch/offline analysis, once Phase A is done** (Blockers 1-3 + NAMESPACE + single default
source). At that point you have a runnable, no-TF, multilingual, *reproducible* scorer with a pinned
golden test — which is more than most R sentiment packages can say, and the provenance registry makes
it more auditable than the paid APIs it matches.

**Not yet for a live latency-sensitive service or a regulated audit**, even after Phase A, until: the
CRAN hygiene (Phase B) lands, calibration ships (the `(p-0.5)*2` output is *uncalibrated* — fine for
ranking, not for "X% positive" claims, per the roadmap), and the OpenMP collision between R-xgboost
and python-torch in one process is actually resolved rather than worked around (it's on your remaining
list — verify it under load, it's the kind of thing that's fine in a smoke test and deadlocks in a
batch job).

**Net:** strong architecture, honest planning docs, ~2-4 days of wiring-and-validation from a release
I'd trust for its intended job. The gap is execution-completion and proof, not design. Close Blockers
1-3, un-skip the golden test, and tag it `1.0.0`.
