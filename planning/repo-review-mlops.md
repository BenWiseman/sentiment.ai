# Senior MLOps / production-engineering review — sentiment.ai v2

**Reviewer role:** production MLOps / distribution engineer. **Date:** 2026-06-03.
**Branch:** `v2`. **Scope of mandate:** model distribution (the ~20 MB xgb scorers vs the
CRAN 5 MB tarball limit; download-on-first-use), reproducibility + revision pinning, the
OpenMP R-xgboost / python-torch collision, install reliability + reticulate interpreter
discovery, the Python-train / R-load cross-language seam, caching (`R_user_dir`), and CI.

**Method.** Read the code on disk, not the roadmap's description of it:
`R/{init_and_install,sentiment,embedding,constants,choose_model,model_meta,local_from_reticulate,onload}.R`,
`inst/get_embedder.py`, `inst/scoring/`, `tests/testthat/*`, `DESCRIPTION`, `NAMESPACE`,
`.Rbuildignore`, `pypackage/{PARITY.md,sentimentai/sentiment.py}`, and the existing planning
set (`review-migration.md`, `cran-checklist.md`, `use-vendoring-plan.md`, `v2-test-plan.md`,
`review-methodology.md`, `NEWS-v2-draft.md`). Verified git-tracking state and grepped the
source for thread/OMP guards.

**What this review does NOT re-litigate.** `review-migration.md` (the broken default path,
the missing legacy flag, tfhub.dev decay) and `cran-checklist.md` (the 14 `--as-cran`
blockers) are already excellent and correct. I build on them where MLOps and those reviews
intersect, and I do **not** repeat their findings. My job is the *production distribution and
runtime-environment* layer those reviews touch but don't own: how the bytes get to the user's
disk, how two native runtimes share one process, how the build excludes the big artifacts, how
the Python and R packages stay byte-identical, and what CI has to prove.

---

## Headline state (MLOps lens)

The architecture is **right** — frozen embedder + light downloadable scorer + a manifest
(`available_models.csv`) — and the team clearly knows the shape of the problem
(`inst/scoring/readme.txt` already documents "these are ~20 MB, must not ship in the tarball,
move to download-on-first-use"). But three production realities are currently either
**unimplemented** or **actively wired wrong**, and any one of them turns into a support fire or
a silent wrong-number on a user's machine:

1. **The download-on-first-use cache writes into the read-only package library** (the same A1-A5
   the CRAN review flags), AND there is **no `.Rbuildignore` guard** stopping the 58 MB of dev
   `.xgb` files now sitting in `inst/scoring/xgb/1.0/` from shipping in the tarball. Distribution
   is the single biggest unshipped piece, and it's two independent failure modes, not one.
2. **The OpenMP R-xgboost / python-torch collision has zero mitigation in the code.** No env
   guard, no thread pinning, nothing. The planning docs only discuss the *build-time*
   no-OpenMP xgboost on the dev Mac (`review-methodology.md:182`) — a different, milder problem.
   The *runtime* dual-OpenMP-runtime-in-one-process collision (the thing that aborts the R
   session) is real, is on the new default path (R loads torch via reticulate **and** calls
   R-xgboost in the same process), and is unaddressed.
3. **The cross-language seam (`PARITY.md`) is a beautifully specified contract with no
   enforcement and no artifact-integrity story.** Python is `NotImplementedError`; there is no
   checksum on the downloaded `.xgb`, no shared manifest of truth between R and Python, and the
   "same bytes ⇒ same scores" promise is currently unverifiable.

Everything below is concrete and grounded in a file:line. The TOP 3 are at the end.

---

## 1. Model distribution: the 5 MB CRAN limit vs the 20 MB scorers

### 1.1 The build does not yet exclude the big artifacts — a maintainer footgun

`inst/scoring/xgb/1.0/` on disk right now holds **three ~20 MB boosters** (`e5-small.xgb`
19.9 MB, `e5-base.xgb` 19.9 MB, `multi.large.xgb` 20.4 MB = **~58 MB**), plus
`inst/default_embeddings/` is **15 MB**. The current `.Rbuildignore` excludes `^.*.csv`,
`data-raw`, `inst/tfhub_modules/*`, etc. — but **nothing excludes `inst/scoring/*.xgb` or
`inst/default_embeddings/*.json`**. They are untracked in git (good — not committed), but
`.gitignore` and `.Rbuildignore` are *different gates*: `R CMD build` tars the working
directory, not the git index. **The moment a maintainer runs `R CMD build` with these dev
files present, a ~73 MB tarball is produced and CRAN auto-rejects it** (limit 5 MB).

This is a latent landmine because it "works on my machine" right up until the release build,
and the failure (a 73 MB tarball) doesn't point at the cause. The team *intends*
download-on-first-use (the readme says so), but intent isn't enforced by the build config.

**Fix (small, do now):**
- Add to `.Rbuildignore`:
  ```
  ^inst/scoring/.*\.xgb$
  ^inst/scoring/.*\.csv$        # glm weights, same story
  ^inst/default_embeddings/.*\.json$
  ```
  Keep `inst/scoring/available_models.csv` and `inst/scoring/readme.txt` (the manifest +
  doc are tiny and *must* ship). The exclusion regex must be narrow enough not to eat them —
  note `available_models.csv` would be caught by a blanket `\.csv$`, so exclude the **glm
  weight CSVs by path** (`^inst/scoring/(xgb|glm)/.*\.(xgb|csv)$`) not by extension.
- Add a build-size **assertion to CI** (see §6): `R CMD build` then
  `du -k` the tarball and fail if `> 4900` KB. This is the only thing that makes "don't ship
  the weights" a *guarantee* instead of a hope. Without it, §1.1 will recur every release.

### 1.2 Download-on-first-use writes to the wrong place (the A1-A5 cluster) — the MLOps framing

`cran-checklist.md` correctly flags that `install_scoring_model()`
(`init_and_install.R:305-308`), `install_default_embeddings()` (`:368-370`), and the legacy
tfhub cache (`:479`) all `download.file()` into `system.file(package=...)`. I'm not re-listing
those as CRAN items; I'm naming the **production consequence**: on any real multi-user install
(an RStudio Server, a Docker image where the lib is baked read-only at build, a `renv`-managed
project library, an HPC module) **the library is not writable at runtime**, so the *first
score call fails with a permission error* — not on CRAN's farm, on a *paying user's box*. The
download-into-library pattern is a support-ticket generator independent of CRAN.

The fix is the one `cran-checklist.md` and `use-vendoring-plan.md` already specify
(`tools::R_user_dir("sentiment.ai", "cache")`), but from the MLOps seat I'd add three
hardening requirements the planning docs don't yet pin:

- **One cache-root helper, used by every reader and writer.** Right now the read path
  (`find_sentiment_score()` at `sentiment.R:256`, `score_dir <- system.file("scoring", ...)`)
  and the write path (`install_scoring_model()`) compute the location independently. They
  **must** share a single internal `.sentiment_cache_dir()` or they will drift and the reader
  will look where the writer didn't write. This is a correctness requirement, not cosmetics.
- **`Sys.setenv("HF_HOME"/"SENTENCE_TRANSFORMERS_HOME"/"TRANSFORMERS_CACHE")`** to the same
  `R_user_dir` cache before the first `SentenceTransformer(hf_id)` call. The e5 weights
  (~470 MB for `e5-base`) are downloaded by **huggingface_hub inside Python**, which by default
  caches to `~/.cache/huggingface` — a *fourth* cache location nobody is managing, invisible to
  the R user, not cleaned by uninstall, and unshared with the Python sibling package. Pin it
  explicitly so the whole package has *one* documented cache tree.
- **A `clear_sentiment_cache()` / `sentiment_cache_dir()` pair, exported.** Users on shared or
  quota'd disks need to find and purge ~500 MB of model weights. A package that silently
  scatters half a GB across two-to-four cache locations with no "where is it / how do I delete
  it" surface is an ops liability. This is ~15 lines and removes a whole class of tickets.

### 1.3 Download integrity: there is no checksum on the scorer — a silent-wrong-number risk

`install_scoring_model()` does `download.file(target_url, obj_path, mode="wb")` inside a
`tryCatch` and that's it. **No SHA-256, no size check, no content-type check.** Failure modes
this allows, all of which produce a *file that exists but is wrong*:

- GitHub serves an HTML error page (rate-limit, 404 after a path change, LFS pointer) with a
  200 status → you get a 200-byte HTML file named `e5-small.xgb`. `xgb.load()` then throws a
  cryptic binary-parse error, or worse, on a partial download, loads a truncated booster and
  **scores silently wrong**.
- A proxy / corporate MITM mangles the bytes.
- A half-finished download (connection drop) leaves a truncated file that *passes
  `file.exists()`* so the next run never re-downloads it (the function returns early at
  `init_and_install.R:314` on `file.exists(obj_path)`).

For a package whose entire brand is **"auditable, reproducible measurement,"** shipping a
download path that can hand you a corrupt scorer and score on it is the sharpest contradiction
in the repo. `use-vendoring-plan.md` already establishes a SHA-256 manifest discipline for the
*legacy USE* assets — **apply the exact same discipline to the primary e5 scorers**, which are
the ones every default user downloads.

**Fix:**
- Put a `sha256` column in `available_models.csv` (it already has `model,version,
  embedding_model,dim,default` — add `sha256,bytes`). It's the manifest; it should carry the
  integrity data.
- After download: verify `tools::sha256sum`-equivalent (`digest::digest(file, algo="sha256")`
  or `openssl::sha256`) against the manifest; on mismatch, **delete the file and error**, do
  not score. Download to a `.part` temp then atomic-rename on success so a dropped connection
  never leaves a "valid-looking" partial.
- This also closes a reproducibility hole: the manifest hash is what lets a 2028 re-run *prove*
  it loaded the identical scorer — the D1 "re-run, identical number" claim in
  `better-sentiment-roadmap.md` is only true if the bytes are pinned and checked.

### 1.4 Where the scorers actually live — pick one host and version it

Today `install_scoring_model()` pulls from
`https://github.com/BenWiseman/sentiment.ai/raw/main/models/<scoring>/<version>/<model>.<ext>`
(`init_and_install.R:282`). Pulling from `main` is a **moving target**: a future commit that
retrains a scorer changes the bytes under everyone still on package v2.0.0 with no version
bump — a silent score change, the exact thing the migration review (S2) says is a semver-major
event. The scorer URL must be pinned to an **immutable** ref:

- Serve scorers from a **GitHub Release tag** (e.g. `scorers-v1`), not `raw/main`. Release
  assets are immutable; `raw/main` is not. (This mirrors exactly what `use-vendoring-plan.md`
  prescribes for the legacy USE models — same logic, the primary scorers deserve it more.)
- The `scoring_version` in the path (`1.0`) should map to a release tag, so "scorer 1.0" is a
  frozen artifact forever and "scorer 1.1" is a new release + a NEWS entry. That makes the
  `scoring_version` argument *mean* something reproducible instead of being decorative.

---

## 2. The OpenMP R-xgboost / python-torch collision (the runtime landmine)

**This is the finding the planning set has not yet engaged with, and it is on the default path.**

### 2.1 What it is, precisely

The new default pipeline does, *in a single OS process*:
1. `reticulate::source_python(...)` → imports `sentence_transformers` → imports **PyTorch**,
   which ships and dynamically loads its own OpenMP runtime (`libgomp` on Linux, and on macOS
   the torch/MKL stack often pulls Intel's `libiomp5`).
2. `xgboost::xgb.load()` + `predict()` (`sentiment.R:271-272`) → R's **xgboost** also links an
   OpenMP runtime (`libgomp` via the system toolchain, or on macOS a Homebrew `libomp`).

When two different OpenMP runtimes (or two copies of the same one) get loaded into one process,
the documented outcomes are: a hard abort with
`OMP: Error #15: Initializing libiomp5.dylib, but found libomp.dylib already initialized`,
**silent wrong results**, deadlocks on fork, or a crash that takes down the whole R session
(and a user's unsaved work). On macOS specifically — *the exact platform v2 exists to rescue*
— this is a frequent torch+native-lib failure. The brief calls this out; the code does nothing
about it. Grep confirms: **no `OMP_*`, `KMP_*`, `nthread`, or torch thread guard anywhere in
`R/` or `inst/`.**

### 2.2 Why it's worse here than usual

- It's on the **happy path**, not an edge case: every default `sentiment_score("text")` call
  loads torch (embed) then xgboost (score) in the same R process.
- It's **platform- and install-order-dependent**, so it'll pass on the dev Mac and CI and then
  abort on a user's differently-built box — the worst kind of bug to debug remotely.
- The dev Mac's xgboost is built **without OpenMP** (`review-methodology.md:182`), which means
  *the maintainer's own machine cannot reproduce the collision* — it only bites users whose
  xgboost **does** have OpenMP (i.e. most Linux CRAN-binary users). This is a true "works for
  me, aborts for them" trap.

### 2.3 Fixes, in order of robustness

- **Cheapest, do immediately:** set the standard interop-safety env vars **before** Python /
  torch initialize, in `.onLoad` or at the top of `init_sentiment.ai()`:
  ```r
  if (Sys.getenv("KMP_DUPLICATE_LIB_OK") == "")
    Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")   # downgrade the abort to a warning
  ```
  `KMP_DUPLICATE_LIB_OK=TRUE` is the well-known mitigation for the macOS `libiomp/libomp`
  duplicate-init abort. It is a **blunt instrument** (it permits the duplicate load, which *can*
  in principle produce wrong numbers), so it is a floor, not the final answer — but it converts
  "session aborts" into "session runs," which on the install-nightmare platform is the
  difference between adoption and a one-star bug report. Document the caveat.
- **Constrain the thread fight:** force both runtimes to a sane, non-conflicting thread count
  so they don't both try to grab every core and thrash. Set `OMP_NUM_THREADS` /
  `torch.set_num_threads(1)` for the embed step (sentence-transformers on CPU is fine
  single-threaded for typical batch sizes) and pass `nthread` explicitly to xgboost `predict`
  rather than letting it default to "all cores." Predicting a 3-class softprob on a 20 MB
  booster is microseconds; it does not need OpenMP at all — **build/ship the xgboost predict
  path single-threaded on purpose** and the whole collision surface shrinks.
- **Most robust (architectural):** the embed step and the score step **do not need to share a
  process.** The torch-heavy embed runs in the reticulate Python interpreter; the xgboost score
  is pure R on a matrix. If the collision proves intractable on some platform, run the scorer in
  a **separate, forked R process** (`callr::r()`) that never imported torch — clean OpenMP
  state, byte-identical scores, ~50 ms fork overhead amortised across a batch. This is the
  bulletproof fallback and worth a spike before release on a real Linux box with OpenMP-xgboost.
- **CI must actually exercise this:** the no-TF default job (§6) has to run a real
  `sentence_transformers` embed **then** an R-xgboost score **in one session on Linux with a
  stock OpenMP xgboost** — i.e. *not* the dev Mac's no-OpenMP build — or the collision will
  never show up in CI and will ship. This is a specific, named gap in `v2-test-plan.md`'s CI
  matrix: it specifies "no TF" but not "OpenMP-enabled xgboost present," which is the condition
  that triggers the bug.

---

## 3. Reproducibility + revision pinning

The team has done real, credible work here already: `model_meta.R` is exactly the right
structure (per-model license / source URL / **pinned HF revision SHA**), and it honestly flags
the gap — every e5 row is `revision = "main"` with a `# TODO: pin to commit SHA at release`.
That's the correct posture. The MLOps-specific reinforcement:

- **`revision = "main"` is a reproducibility hole that's *trivially* closable and must be closed
  before release, and asserted in CI.** intfloat could re-upload `multilingual-e5-small` (they
  have, historically, re-pushed quantization/format changes), and every unpinned user silently
  gets different embeddings → different scores → an unreproducible published number. The loader
  **must forward `revision=` to `SentenceTransformer`** — note `get_embedder.py:54` currently
  calls `SentenceTransformer(hf_id)` with **no `revision` argument at all**, so even though
  `model_meta.R` carries the SHA, *nothing passes it to the loader*. The plumbing is half-built:
  the metadata exists, the consumer doesn't read it. Wire `revision = model_revision[model]`
  through `init_sentiment.ai()` → `load_st_embedder(hf_id, prefix, revision)` →
  `SentenceTransformer(hf_id, revision=revision)`. Add a test that the revision is non-`"main"`
  at release (a CI gate: fail if any shipped default model's revision is `"main"`/`NA`).
- **The provenance stamp must record what was *actually* loaded, not what was *requested*.**
  `better-sentiment-roadmap.md` Move 3 wants a returnable `{package_version, embedder,
  embedder_revision, scoring, scoring_version, prefix}` stamp. From the MLOps seat: have the
  loader read back the **resolved** revision from huggingface_hub's snapshot (the actual commit
  it pulled) and the **sha256 of the scorer file it loaded** (§1.3), and put *those* in the
  stamp. A stamp that echoes the requested `"main"` is theatre; a stamp that records "loaded
  e5-small @ commit `abc123`, scorer sha256 `def456`" is auditable. This is the single most
  brand-aligned MLOps feature in the repo and it's ~30 lines on top of work already done.
- **Determinism note for the record:** sentence-transformers `.encode` on CPU is deterministic;
  xgboost `predict` is deterministic; the BLAS/threading nondeterminism is ~1e-6 (PARITY.md §9
  has this right). So bit-exact reproducibility *is* achievable if (and only if) the model
  revision and scorer hash are pinned. The whole reproducibility claim rests on §1.3 + §3, both
  of which are 90% specified and 0% wired.

---

## 4. Install reliability + the reticulate interpreter-discovery problem

This is historically the package's worst user experience, and v2 has already done the single
most important thing: **the vendored `local_from_reticulate.R` removes the `reticulate:::`
internal call** (well-documented, Apache-2.0 attributed, public API where possible). That's
genuinely good engineering. Remaining MLOps concerns:

- **The one live `reticulate:::` call is still in `install_sentiment.ai()`**
  (`init_and_install.R:141`, `reticulate:::py_install_method_detect(...)`), even though the
  vendored replacement exists and is wired in elsewhere (`:652`/`:662`). `cran-checklist.md`
  B2 has this; I flag it here only because it's the *interpreter-discovery* path specifically —
  swap it for the local `py_install_method_detect()`. One-line fix, already has a tested
  replacement.
- **`RETICULATE_PYTHON` pinning is the real discovery hazard and the package fights it instead
  of owning it.** The 60-line roxygen note (`init_and_install.R:42-72`) documents — eloquently
  — that reticulate will silently use the wrong interpreter if `RETICULATE_PYTHON` is set by a
  project / `.Renviron` / a prior `py_config()`. Documenting a footgun is not fixing it. The
  modern, robust pattern is to **stop fighting reticulate's global env discovery and use an
  explicit, package-owned environment**: `reticulate::use_virtualenv("r-sentiment-ai",
  required = TRUE)` (or `reticulate::virtualenv_python()` to get the exact binary) pinned at
  the *start* of `init_sentiment.ai()`, before any other Python touch, with a clear error if it
  isn't the active one. Even better, adopt reticulate's **`uv`-based ephemeral env / `py_require()`**
  flow (the 2024+ reticulate direction) so the package *declares* its Python deps and reticulate
  provisions an isolated, reproducible interpreter — no conda/virtualenv dance, no
  `RETICULATE_PYTHON` collision, which is precisely the install-hell v2 is trying to end. This is
  the highest-leverage install-reliability move available and it dovetails with the "frictionless
  install IS the product" thesis in `v2-roadmap.md`.
- **First-run UX is silent for a long time.** The first `SentenceTransformer("intfloat/...")`
  downloads ~120 MB (small) to ~470 MB (base) with **no R-side progress bar** (the download
  happens inside Python/hf_hub). A user calling `sentiment_score("good")` for the first time
  sees R "hang" for a minute. Emit a one-time consented `message()` before the first model
  pull ("downloading the e5-small model (~120 MB) to <cache>; this happens once") so the wait
  is legible, not a perceived freeze. Pairs with the `sentiment_status()` readiness call
  `better-sentiment-roadmap.md` table-stakes #7 already wants.
- **`test_parallel_support()` spins up a full cluster on every `init`** (`init_and_install.R:668`,
  `makeCluster(detectCores()-1)` + `parLapply`) purely to *detect* core count, then tears it
  down. On a locked-down / containerised box where forking a cluster is restricted, this can
  hang or warn on a path that has nothing to do with the user's actual call. `parallel::detectCores()`
  alone answers the question without the cluster round-trip; reserve the actual-cluster probe
  for the OpenAI-parallel path that needs it. Minor, but it's startup latency + a failure
  surface on exactly the constrained environments where install reliability matters most.

---

## 5. The cross-language Python-train / R-load seam

`PARITY.md` is an unusually good contract — it correctly identifies that parity holds *because*
both languages load the **same HF embedder id** and the **same `.xgb`/`.csv` artifact** rather
than reimplementing, and it nails the subtle traps (the e5 `"query: "` prefix is load-bearing
for scores; the hardcoded-512 bug; NA/empty-string asymmetry; intercept-first glm convention).
From the MLOps seat, the *contract is sound but unenforced and under-instrumented*:

- **There is no single source of truth for the registry across languages.** R defines models in
  `constants.R` (`default_models`, `model_dims`, `model_prefix`); Python will define them in
  `_models.py` (`BACKENDS`); the scorer manifest is `available_models.csv`; `model_meta.R` holds
  licenses/revisions. **Four places, hand-synced.** They *will* drift (PARITY.md §2 already lists
  reconciliation gaps — missing OpenAI aliases, unknown-name behaviour differing). The fix is to
  make **`available_models.csv` (plus a small `models.json`) the one machine-readable registry
  both languages and the docs read**, and generate/validate the R and Python constant tables
  *from* it in CI. A registry that lives in code twice is a registry that's wrong half the time.
- **The "same bytes ⇒ same scores" promise needs the §1.3 checksum to be *true*, and a golden
  test to be *proven*.** PARITY.md §10 item 9 ("cross-language golden test") is exactly right and
  is the regression gate that *defines* parity — but it's listed as a to-do. Until it exists, the
  1:1 claim is aspirational. Concretely: commit a tiny fixture corpus + its expected scores per
  `(model, scoring)`; run it in **both** R and Python CI; assert `|score_R - score_Py| < 1e-4`.
  This is the contract made executable. It also catches the prefix bug, the dim-feature-order
  bug, and an accidental scorer re-train all at once.
- **Python ships `NotImplementedError` today** (`sentiment.py:25,42`). That's honest scaffolding,
  fine — but the MLOps risk is **publishing the PyPI package before the golden test is green**.
  A `pip install sentimentai` that raises `NotImplementedError` on `sentiment_score` is a brand
  wound for a package selling reliability. Gate the *first PyPI publish* on the golden parity
  test, not on "the stubs are filled in."
- **One real seam bug to flag (not in the reviews I read):** the **embedder is shared but the
  embedder's *download cache* is not.** R's reticulate Python and the standalone Python package
  use *different* Python interpreters with *different* default HF caches, so an R user and a
  Python user on the same machine download the same ~470 MB e5-base **twice**. Pointing both at a
  shared `HF_HOME` (§1.2) under a documented location fixes it and is a genuinely nice "it just
  shares the cache" property for someone using both packages in one project (the Linnet
  `linnetlabs` integration in `v2-roadmap.md` Phase 8 will do exactly that).

---

## 6. CI — currently absent, and the matrix must encode the failure conditions

**There is no `.github/` directory and no CI workflow in the repo at all.** `v2-test-plan.md`
specifies a good matrix on paper; none of it exists yet. The 24-file `tests/testthat/` suite
(genuinely solid — deterministic no-Python fake embedder in `helper-skips.R`, snapshot scorer,
legacy gate, no-512-literal, namespace-no-TF-import) currently runs **only on whoever types
`devtools::test()`**. For a package whose headline claim is "installs clean with no TF," *CI is
the only thing that can prove the claim* — and it's the missing piece.

The matrix `v2-test-plan.md` describes is right; the MLOps additions that turn it from
"tests pass" into "the production failure modes can't ship":

1. **Tarball-size gate (new, blocking).** `R CMD build` → assert tarball ≤ 4900 KB. This is the
   *only* enforcement that §1.1 can't regress. Without it, the 73 MB tarball ships the day
   someone forgets `.Rbuildignore`.
2. **OpenMP-collision job (new, blocking on Linux).** A job on stock Linux with a **CRAN-binary
   xgboost (OpenMP enabled)** that does a real e5 embed → R-xgboost score in one session.
   This is the only environment that reproduces §2; the dev Mac (no-OpenMP xgboost) structurally
   cannot. Without this leg, the collision is invisible to CI and ships to Linux users.
3. **No-TF default job with `_R_CHECK_DEPENDS_ONLY_=true`** (as the plan says) — so Suggests are
   genuinely absent and "no TF needed" is proven, not accidentally satisfied by a TF that
   happens to be on the runner.
4. **Apple-Silicon (`macos-14`, arm64) leg** — the original install-nightmare platform; v2's
   entire reason to exist. Must run the default path end-to-end.
5. **Revision-pinned gate (new):** fail if any shipped default model's `model_revision` is
   `"main"` or `NA` (closes §3).
6. **Cross-language golden test (new, once Python is implemented):** §5.
7. **`R-CMD-check --as-cran`** on the default job (the 14 blockers in `cran-checklist.md` are the
   acceptance criteria for this job going green).

Keep the legacy-TF job **nightly / allowed-to-fail** (the plan has this right) — it proves the
compat layer is real without blocking the mainline on the fragile 2021 stack.

---

## 7. Smaller production notes (grounded, lower priority)

- **`openai_embed()` is O(n) serial with a `cbind` in a loop** (`embedding.R:257-283`) — quadratic
  memory churn (each `cbind` reallocates the growing matrix) and one HTTP round-trip per row.
  For a 10k-row HR-verbatim job that's 10k sequential API calls and a reallocating matrix. The
  OpenAI embeddings API takes a **batched `input` array**; send `batch_size` rows per request and
  preallocate. The parallel variant (`openai_embed_parallel`) exists but is **never called** — the
  `parallel > 2` branch at `:206` calls the *serial* function in both arms (a latent bug
  `cran-checklist.md` F2 also spotted). Fix the batching first; it's a 10-100x throughput win and
  a cost reduction on the paid path.
- **`stray print(url)`** in the Azure branch (`embedding.R:399`) leaks the request URL to console
  on every Azure embed — remove (also a CRAN nit).
- **`scoring_version` is `match.arg`'d against a single value** (`sentiment.R:71`) — the moment a
  v1.1 scorer ships, the formal default list must include it or `match.arg` errors. Make it a
  membership check against the manifest's version column, same pattern as the `model` fix.
- **Cache eviction / versioning:** when `scoring_version` bumps, old scorers linger in the cache
  forever. Not urgent, but a `which="cache"` tree that only grows is an eventual disk complaint;
  key the cache path by version (already implied) and offer the `clear_sentiment_cache()` from §1.2.

---

## TOP 3 (do these first)

1. **Make distribution real and safe end-to-end: exclude the big artifacts from the build, move
   the cache to `R_user_dir`, and checksum every download.** Three tightly-coupled fixes that
   together are the difference between "an architecture for download-on-first-use" and a package
   that actually ships and scores correctly: (a) add `^inst/scoring/(xgb|glm)/.*\.(xgb|csv)$` +
   `^inst/default_embeddings/.*\.json$` to `.Rbuildignore` **and** a CI tarball-size gate (≤5 MB),
   or the 73 MB dev tree ships and CRAN bounces it; (b) route every scorer/embedding read+write
   through one `.sentiment_cache_dir()` helper on `tools::R_user_dir("sentiment.ai","cache")`
   (fixes the read-only-library failure on Server/Docker/HPC, not just CRAN); (c) add `sha256`/
   `bytes` to `available_models.csv`, verify after download (atomic `.part` rename, delete-and-
   error on mismatch), and serve scorers from an **immutable GitHub Release tag**, not `raw/main`.
   Without (c) a corrupt/partial/HTML-error download scores silently wrong — the single sharpest
   violation of the "auditable measurement" brand.

2. **Defuse the runtime OpenMP collision before it ships — it's on the default path and the dev
   Mac structurally can't reproduce it.** The new happy path loads python-torch (embed) and
   R-xgboost (score) in one process; nothing in the code guards the dual-OpenMP-runtime collision
   that aborts the R session on stock-OpenMP-xgboost boxes (i.e. most Linux users). Floor: set
   `KMP_DUPLICATE_LIB_OK=TRUE` + pin threads (`torch.set_num_threads(1)`, explicit single-thread
   xgboost `predict`) before Python initialises. Robust fallback: run the pure-R scorer in a
   forked `callr` process with clean OpenMP state. And add a **Linux CI job with OpenMP-enabled
   xgboost that embeds-then-scores in one session** — the only environment that reproduces the
   bug; the maintainer's no-OpenMP-xgboost Mac will pass while users abort.

3. **Wire the reproducibility/parity plumbing that's specified-but-not-connected: pass the pinned
   revision to the loader, stamp what actually loaded, and gate releases on a cross-language golden
   test.** `model_meta.R` carries per-model SHA revisions but `get_embedder.py:54` calls
   `SentenceTransformer(hf_id)` with **no `revision=`** — the metadata exists and nothing consumes
   it; thread `revision` through and fail CI if any default model is still `"main"`/`NA` at release.
   Build the provenance stamp from the **resolved** HF commit + the **scorer's verified sha256**
   (not the requested values), so "re-run in 2028, identical number" is provable, not theatre. And
   make `available_models.csv`/`models.json` the **single registry** both R (`constants.R`) and
   Python (`_models.py`) are validated against in CI, with a committed `|score_R - score_Py| < 1e-4`
   golden test gating the first PyPI publish — the contract in `PARITY.md` made executable.

---

## Cross-references

- Builds on (does not repeat): `review-migration.md` (S0-S7 broken default path / missing legacy
  flag / tfhub decay), `cran-checklist.md` (14 `--as-cran` blockers — A1-A5 cache, B1/B2 `:::`,
  C1-C4 Suggests guards, F1 unused `openai` Import), `use-vendoring-plan.md` (SHA-256 manifest +
  GitHub-Release-as-primary pattern, applied here to the *primary* e5 scorers not just legacy USE),
  `v2-test-plan.md` (CI matrix — extended here with the tarball-size, OpenMP-collision, and
  revision-pin gates it's missing), `review-methodology.md:182` (the *build-time* no-OpenMP xgboost
  — distinct from §2's *runtime* collision), `better-sentiment-roadmap.md` (Move 3 provenance stamp
  — grounded here in resolved-commit + verified-hash), `PARITY.md` (the cross-language contract this
  review asks to make enforceable).
- Code touchpoints: `init_and_install.R:141,282,305-308,368-370,479,668`, `sentiment.R:71,256,271`,
  `get_embedder.py:54`, `model_meta.R` (revision table), `constants.R` (registry), `.Rbuildignore`,
  `available_models.csv`, absent `.github/`.
