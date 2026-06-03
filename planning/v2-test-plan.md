# sentiment.ai v2 — Test Plan

> Companion to `v2-roadmap.md`. Scope: the test suite that locks the v2 contract —
> a no-TF default (`e5-small`), a real backend registry, a working legacy USE
> compat-gate, and a clean CRAN `R CMD check` with TensorFlow demoted to `Suggests`.
> Written 2026-06-03 after an audit of the current tree.

## Why this plan exists (the gap it covers)

The package is **half-migrated**. `R/constants.R` already holds the full v2 registry —
`default_models` (`e5-small`→`intfloat/multilingual-e5-small`, `e5-base`→`intfloat/multilingual-e5-base`),
`openai_models`, `legacy_models`, `model_dims`, `model_prefix` (`"query: "` for the e5 family),
`DEFAULT_MODEL <- "e5-small"`, and `model_class()`. **The consumers do not yet honour it.**
Verified against the current source:

- `R/sentiment.R` hard-codes **512** in two places: the embedding-passthrough guard
  `is.matrix(x) && ncol(x)==512` (`sentiment_score`, ~line 98) and the dead seed
  `probs <- numeric(512)` in `find_sentiment_probs` (~line 261).
- `R/init_and_install.R::init_sentiment.ai()` defaults `model` to USE names and branches only
  **openai vs `load_hub_embedding` (TF)** — there is **no sentence-transformers (`st`) branch**
  and **no `query: ` prefix injection**. `embed_text()` (`R/embedding.R`, ~line 214) falls
  through to `hub_embed()` (TF) for everything non-OpenAI.
- `R/init_and_install.R` carries `@import tensorflow` / `@import tfhub` roxygen tags and hard
  TF pins in the default `modules` list, while `DESCRIPTION` has **already** moved `tensorflow`,
  `tfhub`, `text2vec` to `Suggests`. An unconditional import of a Suggested package is a
  **guaranteed `R CMD check` ERROR** — this is the single highest-priority gate below.
- `R/onload.R` still advertises `paraphrase` / `use` / `oai_3_small` and states *"use will still
  be the default"* — directly contradicting `DEFAULT_MODEL <- "e5-small"`.
- There is **no `tests/` directory** — testthat must be bootstrapped from zero.
- No legacy gate exists: asking for `multi.large` today does not produce a helpful "install with
  `legacy = TRUE`" error; it falls into the TF path and crashes on a missing import.

**Consequence for the suite.** Most v2 tests are written against the v2 *contract* and will
**fail against the current code** until Roadmap Phases 1–4 land. That is intended: they are the
executable definition of "done". Each section below tags whether a test is expected RED now or
GREEN now, so a future session can run the suite as a migration checklist.

## Test taxonomy and how it stays offline

Four tiers, gated so CRAN and a TF-free CI both stay green:

| Tier | Network / Python / TF | Runs on CRAN? | Runs in CI default job? | Mechanism |
|---|---|---|---|---|
| **A. Pure-R unit** | none | yes | yes | plain `testthat` |
| **B. Snapshot/regression (scorer)** | none (fixture embeddings) | yes | yes | `testthat::expect_snapshot_value` over committed `.rds` |
| **C. Backend integration** | Python + model download | **no** (`skip_on_cran`) | yes (nightly/matrix) | `skip_if_not(reticulate::py_module_available(...))` |
| **D. Paid / network** (OpenAI, GitHub model fetch) | network + key | **no** | only if secret present | `skip_if(Sys.getenv("OPENAI_API_KEY")=="")` |

Golden rule: **Tiers A and B never touch Python, the network, or TensorFlow.** They are the only
tiers CRAN runs, and they must pass with **zero** Python/TF installed. Everything that needs an
embedder is Tier C/D and must `skip_*` cleanly (a skip is a pass; a hard error on a CRAN/macOS-noTF
box is a rejection).

### Bootstrapping testthat (does not exist yet)
- Add `testthat (>= 3.0.0)` to `Suggests`; enable edition 3 (`Config/testthat/edition: 3`).
- Create `tests/testthat.R` and `tests/testthat/`. Add `tests/testthat/fixtures/` for committed
  deterministic embedding matrices and golden scores (see Tier B).
- Add `tests/testthat/helper-skips.R` with shared helpers:
  `skip_if_no_python()`, `skip_if_no_st()` (sentence-transformers), `skip_if_no_tf()`,
  `skip_if_no_openai_key()`, and `local_fake_embedder(dim)` (installs a deterministic stub into
  `sentiment.env$embed` so backend-routing logic is testable with **no** real model).

---

## Tier A — Pure-R unit tests (no embedder)

### A1. Registry integrity (`test-constants.R`) — GREEN now
The registry in `constants.R` is the contract; assert it can't silently drift.
- `default_models` contains exactly `e5-small`, `e5-base`; values are the `intfloat/multilingual-e5-*` ids.
- `DEFAULT_MODEL == "e5-small"` **and** `DEFAULT_MODEL %in% names(default_models)` (catches a default
  that points at an unregistered model).
- `names(model_dims)` is a **superset** of every name across `default_models`, `openai_models`,
  `legacy_models` — i.e. **every** selectable model has a declared dimension. This is the structural
  test that prevents the old hard-coded-512 bug from re-appearing for any new backend.
- `model_dims` values: `e5-small=384`, `e5-base=768`, `text-embedding-3-small=1536`,
  `text-embedding-3-large=3072`, `ada-002=1536`, all USE `=512`. (Dims grounded in the published
  model cards: multilingual-e5-small 384-d, -base 768-d; OpenAI text-embedding-3-small 1536-d,
  -3-large 3072-d, ada-002 1536-d; USE family 512-d.)
- `model_prefix` is `"query: "` for **both** e5 entries and effectively `""`/absent for all others.
  The e5 prefix requirement is per the intfloat/e5 model card and Wang et al. 2024,
  *"Multilingual E5 Text Embeddings: A Technical Report"* (arXiv:2402.05672) — asymmetric `query:`/
  `passage:` prefixes are part of the model's training recipe, not a packaging choice. Cite this in
  the test comment so nobody "cleans up" the prefix later.

### A2. `model_class()` routing (`test-model-class.R`) — GREEN now
Table-driven; one expectation per row, by **both** handle and underlying id:
- `e5-small`, `e5-base`, and their HF ids → `"st"`.
- `text-embedding-3-small`/`-large`/`ada-002` (and ids) → `"openai"`.
- `en`, `en.large`, `multi`, `multi.large` (and TF-Hub ids) → `"legacy"`.
- `"definitely-not-a-model"`, `NA_character_`, `character(0)` → `"unknown"` (or a defined error for
  the empty case — pick one and pin it).

### A3. `choose_model()` resolution (`test-choose-model.R`) — GREEN now
- `st` handles resolve to HF ids; `openai` handles to OpenAI names; legacy handles to the full
  `https://tfhub.dev/google/...` URL.
- Unknown name → returns input **with a warning** (`expect_warning(...,"cowboy")`); it must not error
  (the "you're on your own" passthrough is deliberate).
- `choose_model(NA)` / length-0 → the explicit `stop("model is of length 0...")`.

### A4. Score transform & NA handling (`test-score-transform.R`) — partial, needs stub
The `(probs - .5) * 2` rescale and NA round-trip in `sentiment_score()` are pure R. Using
`local_fake_embedder()` + a stub scoring model:
- probs `0 → -1`, `0.5 → 0`, `1 → +1` (boundaries exact).
- `NA` inputs come back `NA` at the **same indices**, and a vector of all-`NA` returns all-`NA`
  without calling the embedder.
- `is.null(x)` → `NULL` (early return), no embedder init.

---

## Tier B — Snapshot / regression tests per backend (the core ask)

**Intent:** *deterministic embeddings → known scores.* The embedder is the only nondeterministic,
heavy, network-bound part. We **cut it out** of these tests entirely: feed the scorer fixed embedding
matrices read from committed fixtures, snapshot the resulting scores. This makes the **scoring half**
(the part that's pure R and the part that actually changes between versions) fully reproducible on
CRAN with no Python.

### B1. Fixture generation (one-time, committed) — `data-raw/make_test_fixtures.R`
For each backend, generate **once** offline and commit:
- A small, fixed corpus of ~20 sentences spanning clear-positive / clear-negative / genuinely-neutral
  (the neutral rows matter — see B4).
- Its real embedding matrix at the backend's **native dimension** (384 / 768 / 1536), as
  `fixtures/emb_<backend>.rds`. Generated with a pinned model revision and `set.seed`-free
  (embeddings are deterministic given the model + the `query: ` prefix; record the HF revision hash
  in a sidecar `fixtures/PROVENANCE.md`).
- The golden score vector `fixtures/scores_<backend>_xgb.rds` produced by the **current** scorer.

These fixtures are the regression anchor. They are NOT re-derived in the test run.

### B2. Per-backend scorer snapshot (`test-snapshot-<backend>.R`) — RED until Phase 3
One file per scoring space (`e5-small`, `e5-base`, `oai_3_small`; add bake-off winner when chosen):
- Load `fixtures/emb_<backend>.rds` → `find_sentiment_probs(scoring="xgb", model=<backend>, ...)`.
- `expect_snapshot_value(scores, style = "json2", tolerance = 1e-4)`. The tolerance absorbs
  platform-level floating-point drift (notably the dev Mac's **single-threaded, no-OpenMP** xgboost vs
  a multi-threaded Linux CI build — thread count must not change determinism, but guard the float
  jitter regardless). If a snapshot changes, that is a **scoring-model change** and must be a
  deliberate, NEWS-documented event — exactly the "changing the default silently changes users'
  numbers" risk the roadmap flags.
- **Class-direction assertions** (model-agnostic, survive a re-snapshot): clear-positive rows score
  `> 0`, clear-negative `< 0`, with a margin (e.g. `> 0.25` / `< -0.25`). These catch a sign flip or
  a wrong model-file mapping even when the exact snapshot is regenerated.
- A **dimension guard**: `ncol(emb_<backend>) == model_dims[[<backend>]]`. If someone commits a 768-d
  fixture for a 384-d model the scorer would mispredict silently; this fails loudly first.

### B3. xgb vs glm agreement (`test-scoring-parity.R`) — RED until both models packaged
Where both a `.xgb` and a `.csv` glm exist for a space, assert they agree in **sign** on the
clear-polarity fixtures (not in exact value — glm is the compatibility fallback, not a clone).
Pins the contract that `scoring="glm"` is a usable degraded path, not silently broken.

### B4. Synthetic-neutral regression guard (`test-neutral-fixture.R`) — RED until packaged
The roadmap's headline finding is that adding GPT-4o synthetic **neutral** training rows raises F1 on
the **positive/negative** classes (neutral scarcity is a known, real problem in 3-class sentiment;
keep this framed as the documented finding, not a vague claim). This test does **not** retrain — it
guards the *shipped* model's behaviour on the committed neutral fixtures:
- Genuinely-neutral fixture rows score within a `|score| < neutral_band` band (band chosen from the
  training-repo eval, recorded in `PROVENANCE.md` — do **not** invent a threshold).
- This is a behavioural anchor so a future scorer swap that quietly regresses the neutral class is
  caught. (The *quantitative* F1 finding lives in the benchmark vignette / paper with real numbers,
  not here.)

---

## Tier C — Backend integration tests (Python; `skip_on_cran` + `skip_if_not`)

These actually load an embedder. Each begins with the matching `skip_*` so a CRAN/macOS-noTF box
skips rather than errors.

### C1. e5 default path, end-to-end (`test-backend-e5.R`) — RED until init `st` branch lands
`skip_if_no_st()`. Then `init_sentiment.ai(model = "e5-small")` and:
- `sentiment.env$embed` is set and is the **sentence-transformers** path (not `hub_embed`/TF). Assert
  no TF: `expect_false(reticulate::py_module_available("tensorflow"))` is too strong if TF happens to
  be present, so instead assert the **chosen backend kind** via an introspectable
  `sentiment.env$backend` field (add one in Phase 2's registry) equals `"st"`.
- `embed_text(c("query: ...","..."))` returns a matrix with `ncol == 384` for `e5-small`,
  `768` for `e5-base` — the **live** dimension test that replaces the hard-coded 512.
- `nrow == length(text)`, `rownames == text`.

### C2. The e5 `query: ` prefix (`test-e5-prefix.R`) — RED until prefix injection lands
This is a correctness test, not cosmetics: e5 is trained with asymmetric prefixes and **drops
measurable accuracy if you skip them** (arXiv:2402.05672). The package must inject `model_prefix[model]`
**internally**, exactly once, so users never type `"query: "` themselves.
- With `local_fake_embedder()` capturing its input: call `embed_text("good service", model="e5-small")`
  and assert the string the embedder actually received is `"query: good service"`.
- **Idempotence / no double-prefix:** input already starting with `"query: "` must not become
  `"query: query: ..."`. (Decide the contract — strip-then-add or skip-if-present — and pin it.)
- **Non-e5 models get no prefix:** same capture with `model="text-embedding-3-small"` receives the raw
  string (its `model_prefix` is empty).
- Batch path: every element in a multi-row batch is prefixed (regression against prefixing only the
  first element).

### C3. Dimension handling across backends (`test-dim-handling.R`) — RED until 512 removed
The bug being killed: `sentiment_score()` only accepts a pre-computed embedding matrix when
`ncol(x)==512`, and `find_sentiment_probs` seeds `numeric(512)`. Replace with registry-driven dims.
- **Passthrough** of a pre-embedded matrix must work at the backend's native width: a 384-col matrix
  with `model="e5-small"`, a 1536-col matrix with `model="text-embedding-3-small"` — both should
  score without re-embedding. A `512`-col matrix should **only** pass through for a 512-d (legacy)
  model, not universally.
- **Mismatch is a clear error**, not a silent mispredict: passing a 768-col matrix with
  `model="e5-small"` (expects 384) must raise an informative error naming both dims.
- Grep-guard (cheap Tier-A static test): `expect_false(any(grepl("512", readLines("R/sentiment.R"))))`
  after migration — a literal tripwire so the magic number can't sneak back. (Keep as a `skip`-able
  dev-only test if reading source files in `R CMD check` is awkward.)

### C4. OpenAI path (Tier D, `test-backend-openai.R`) — needs key
`skip_if_no_openai_key()`. `init_sentiment.ai(model="text-embedding-3-small", api_key=...)`:
- `sentiment.env$openai == TRUE`; embed returns `ncol == 1536`.
- Rate-limit branch in `openai_embed()` is exercised with a tiny `request_limit`/`token_limit` to hit
  the `Sys.sleep`/reset path (mock the clock or set limits=1 and a 2-row input; do not actually sleep
  60s in CI — inject the sleep fn). Keep this one cheap (2–3 short strings) to bound spend.

---

## Tier C — The legacy USE gate (the explicit ask)

### L1. Helpful error when legacy requested without legacy install (`test-legacy-gate.R`)
**Contract:** `sentiment_score(x, model = "multi.large")` (or `init_sentiment.ai(model="multi.large")`)
on a machine **without** the legacy TF backend must return a **clear, actionable error** — *not* a
reticulate `ImportError` stack on `tensorflow_text`, and *not* a silent fall-through.

The gate is the seam to build (Phase 1/5) and to test:
- `skip_if(requireNamespace("tensorflow", quietly=TRUE) && reticulate::py_module_available("tensorflow_hub"))`
  — i.e. **only run this test when TF is genuinely absent**, which is the situation we're protecting.
- `expect_error(init_sentiment.ai(model="multi.large"), regexp = "legacy")` and assert the message
  names the fix: must mention `install_sentiment.ai(legacy = TRUE)` **and** the TF-free replacement
  (`multi.large → e5-base`, per the `constants.R` comment). Test the message *content*, because the
  whole point is that it's helpful.
- Same for `en`, `en.large`, `multi` (table-driven) — each names its replacement (`en → e5-small`;
  `en.large`/`multi`/`multi.large → e5-base`).
- It must fail **at the R level before any Python import** — assert via a mocked
  `reticulate::source_python`/`py_module_available` that the gate short-circuits, so the failure can't
  depend on Python being reachable at all.

### L2. Legacy path *works* when legacy IS installed (`test-legacy-roundtrip.R`)
`skip_if_no_tf()`. The roadmap is explicit: "keep the legacy USE path genuinely working ... or
'compatibility layer' is a lie." So when TF + tfhub + the legacy env are present:
- `init_sentiment.ai(model="en.large", legacy=...)` loads the USE-512 model and `embed_text` returns
  `ncol == 512`.
- A legacy snapshot (Tier B style) on `xgb_model_use(_lg)` still reproduces the historical scores —
  this is the **backward-compat** guarantee for existing users' scripts.

### L3. Legacy not installed by default (`test-no-tf-default.R`) — the CI headline
On the **default** install (no `legacy=TRUE`):
- `requireNamespace("tensorflow")` is allowed to be FALSE and the package still **loads, attaches, and
  runs the e5 default path**. This is the GH-Actions matrix job the roadmap demands: *"default (no-TF)
  path must pass with zero TF installed."*
- Loading the package emits **no** unconditional TF warning (see CRAN-G4) and the `.onLoad` message,
  if any, reflects `e5-small` as default — **not** the current `onload.R` text that says
  "use will still be the default" / advertises `paraphrase`/`oai_3_small`. That message is stale and
  this test pins the correction.

---

## CRAN `R CMD check` gotchas (the explicit ask)

These are the checks that will bounce the submission. Each gets a guard test or a static assertion.

### G1. Suggested packages must be `requireNamespace`-guarded — **highest priority, RED now**
`DESCRIPTION` already moved `tensorflow`, `tfhub`, `text2vec` to `Suggests`, **but**
`R/init_and_install.R` still has roxygen `@import tensorflow` / `@import tfhub`, which generate
`import(tensorflow)` / `import(tfhub)` in `NAMESPACE` — an **unconditional import of a Suggested
package**. That is a hard `R CMD check` ERROR ("namespace dependency not required") and also breaks
load on any box without TF.
- **Fix:** delete those `@import` tags; load TF lazily only inside the legacy branch via
  `if (!requireNamespace("tensorflow", quietly = TRUE)) stop(<legacy-gate message>)` then
  `tfhub::load_savedmodel(...)` / `reticulate` behind the guard.
- **Test (static, Tier A):** parse `NAMESPACE` and assert `!any(grepl("^import\\((tensorflow|tfhub|text2vec)\\)", readLines("NAMESPACE")))`.
- **Test (behavioural):** the L3 no-TF job *is* this guard's integration proof.
- Audit every other Suggests use the same way: `rstudioapi` (already called via
  `rstudioapi::hasFun(...)` — good pattern, keep it), `rappdirs`, `microbenchmark`, `prettydoc`,
  `magrittr`, `knitr`/`rmarkdown` (vignette engine — guard the vignette, see G5).

### G2. Model-download-on-first-use must never run during check/examples/tests
`install_scoring_model()` and `install_default_embeddings()` hit GitHub via `download.file`.
CRAN forbids network access and writing outside `tempdir()` during check.
- No example, test (Tier A/B), or vignette code path may trigger a download. Tier B uses **committed
  fixtures**, never a fetch.
- The download target is `system.file("scoring", ...)` inside the **installed package dir** — writing
  there during check/tests is a second violation (non-writable / not tempdir). For tests that must
  exercise the downloader, redirect to `tempdir()` via the `repo_url`/path seam and **mock**
  `utils::download.file` (e.g. `testthat::local_mocked_bindings(download.file = ...)`) so nothing
  leaves the machine. Assert the *intended URL* is constructed correctly (`.../scoring/xgb/1.0/e5-small.xgb?raw=true`)
  rather than performing the GET.
- Assert the `tryCatch` failure path returns gracefully (status `0`, informative message) when the
  mocked download errors — a flaky network must degrade, not crash.

### G3. `\dontrun{}` pitfalls in examples
Current examples in `sentiment_score`, `sentiment_match`, `install_sentiment.ai`, `init_sentiment.ai`
wrap everything in `\dontrun{}`. CRAN increasingly dislikes blanket `\dontrun` (it reads as "this
never runs / can't be checked").
- Prefer `\donttest{}` for code that is correct but slow/needs a model (CRAN runs `\donttest` under
  `--run-donttest`, so it must still not need network/keys — gate inside with
  `if (requireNamespace(...))`), and reserve `\dontrun{}` strictly for genuinely
  un-runnable-on-CRAN snippets (needs API key, needs TF env).
- At least one **runnable** (un-wrapped) example per exported fn that can run with no embedder —
  e.g. `model_class()`, `choose_model()`, `as_py_list()`, and a `sentiment_score()` call on a
  pre-computed fixture embedding matrix (no Python). Runnable examples are checked and document the
  happy path.
- **Test/lint:** a dev-only check that no exported `.Rd` has *only* `\dontrun` with zero runnable
  lines for the pure-R helpers. (`load_openai_embedding`'s current example calls the live API with a
  placeholder key and is **not** wrapped — wrap it in `\dontrun{}`; it will error under check as-is.)

### G4. `.onLoad` / `.onAttach` hygiene
- `.onLoad` currently calls `warning(...)` (the Apple-Silicon TF note) and `message(...)`. CRAN
  policy: **no `warning()`/`cat()` in `.onLoad`**; startup chatter belongs in `.onAttach` via
  `packageStartupMessage()` only, and must be suppressible. Move both, convert `warning`→
  `packageStartupMessage`, and make the TF note conditional on the legacy backend being requested
  (not fired on every load for every Apple-Silicon user who only wants e5).
- **Test:** `expect_silent(suppressPackageStartupMessages(library(sentiment.ai)))` and assert no
  `warning` is signalled on load with TF absent.

### G5. Vignette must build without Python/TF/network
The vignette engine (`knitr`) and `prettydoc` are Suggests; the vignette currently assumes a model.
- Set heavy chunks `eval = FALSE` or guard with `requireNamespace`/`py_module_available`, so
  `R CMD check --as-cran` builds it on a bare box. The **benchmark vignette** (roadmap Phase 7) that
  reports USE vs e5 vs 3-small vs lexicon must ship **pre-computed** results (committed) — never
  recompute embeddings at build time.
- Lead it with the synthetic-neutral F1 finding using the **real** numbers from the training repo's
  `f1_scores_*.csv`; do not fabricate benchmark figures in the vignette.

### G6. Misc CRAN tripwires to assert/avoid
- `RoxygenNote: 7.1.2` is stale vs current roxygen2 — bump and re-document so `man/` matches; a
  `man/` that's out of sync with roxygen blocks triggers a NOTE.
- `match.arg(scoring_version)` in `sentiment_score()` is called with a single default `"1.0"` and a
  formal default of `"1.0"` — fine, but when a second version is added the formal must list both or
  `match.arg` errors; add a test that `scoring_version="1.0"` resolves and an unknown version errors
  cleanly.
- Examples/tests must not leave files in the user's library or home dir; everything writable goes to
  `tempdir()` (ties to G2).
- No non-ASCII gremlins in code/`DESCRIPTION` (the existing "16 languages / dieciséis" text lives in
  NEWS/README, which is fine; keep accented chars out of R source). Add an encoding sanity check.
- `Authors@R` / KFI funding (`fnd`) attribution must remain intact (MIT + Korn Ferry Institute) —
  add a trivial test that `packageDescription` still lists the KFI funder and Ben Wiseman as `cre`,
  so a future refactor can't quietly strip credits.

---

## CI matrix (Roadmap Phase 7)

GitHub Actions, two dimensions:
1. **Default / no-TF job (required, blocking):** Linux + macOS, R release, **no TensorFlow installed**.
   Runs Tiers A + B + the no-TF integration tests (C1–C3 with sentence-transformers present, L1, L3).
   This is the job that proves the headline claim. Add an Apple-Silicon (`macos-14`, arm64) leg —
   it's the original install-nightmare platform and the whole point of v2.
2. **Legacy job (allowed-to-fail / nightly):** installs the TF compat env; runs L2 + legacy snapshots
   to prove the compatibility layer is real (roadmap risk: "or it's a lie").
3. **Paid job (manual / secret-gated):** Tier D OpenAI tests, only when `OPENAI_API_KEY` secret is set.
4. Standard `R-CMD-check` action with `--as-cran` on the default job; `_R_CHECK_DEPENDS_ONLY_=true`
   so Suggests are genuinely absent and G1 can't pass by accident.

## Definition of done
- `tests/` exists; Tiers A + B fully GREEN on a no-Python, no-TF box.
- `R CMD check --as-cran` clean (0 ERROR, 0 WARNING; NOTEs explained) on the default job.
- L1 legacy-gate message verified to name both the `legacy = TRUE` install and the e5 replacement.
- No literal `512` survives in `R/` outside `constants.R`'s `model_dims`.
- e5 `query: ` prefix injected internally, once, and proven by capture; non-e5 unaffected.
- Every snapshot change is a conscious, NEWS-logged scoring-model change — never an accident.
