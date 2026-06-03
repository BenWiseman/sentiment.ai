# sentiment.ai v2 — CRAN `R CMD check --as-cran` Compliance Checklist

Scope reviewed: `R/init_and_install.R`, `R/local_from_reticulate.R`, `R/sentiment.R`,
`R/embedding.R`, `R/onload.R`, `R/constants.R`, `R/choose_model.R`, `NAMESPACE`,
`DESCRIPTION`, `inst/`, `.Rbuildignore`.

Status of repo as inspected (2026-06-03): **no `tests/` directory exists**, `NAMESPACE`
still has `import(tensorflow)` + `import(tfhub)`, `DESCRIPTION` still has `openai` in
`Imports`, and `tensorflow`/`tfhub`/`rstudioapi`/`rappdirs` are in `Suggests`. This
mismatch (hard `import()` of a Suggests package) is itself a hard ERROR (see B1).

Severity legend:
- **BLOCKER** — `R CMD check --as-cran` produces an ERROR, or a WARNING/NOTE that CRAN
  reviewers reject on submission (runtime writes to the library, `:::`, Suggests used
  unconditionally, examples that download weights). Submission will bounce.
- **SHOULD** — NOTE-level or policy item a human reviewer commonly flags; fix before submit.
- **NICE** — cleanup, not a gate.

---

## A. Runtime downloads / writes into the installed package directory

CRAN policy: *"Packages should not write in the user's home filespace ... nor anywhere
else on the file system apart from the R session's temporary directory ... Limited
exceptions may be allowed ... [use] `tools::R_user_dir()`."* Writing into
`system.file(package=)` (the installed library) fails on every CRAN/admin machine
because the library is read-only, and even when writable it mutates the installed
package — an automatic rejection.

### A1. BLOCKER — `install_scoring_model()` downloads into the package dir
- **File/line:** `R/init_and_install.R:305-322` (`pkg_path <- system.file(package = pkg_name)`,
  `dl_path <- file.path(pkg_path, "scoring", scoring, scoring_version)`, then
  `dir.create(dl_path, ...)` + `utils::download.file(..., destfile = obj_path)` at
  `:319` / `:328`).
- **What:** writes downloaded xgb/glm scoring files under the installed library tree.
- **Why CRAN rejects:** runtime download into a read-only install location; also a network
  access during what can be triggered from examples/tests. Library is not writable on the
  check farm.
- **Fix:** compute the cache root with
  `tools::R_user_dir("sentiment.ai", which = "cache")`, build
  `file.path(cache_root, "scoring", scoring, scoring_version)`, `dir.create(..., recursive = TRUE)`
  there. Add a single internal helper (e.g. `.sentiment_cache_dir()`) and use it everywhere.
  `download.file` must be wrapped so a failure is a soft, non-erroring condition (it already
  is via `tryCatch`, keep that) and must never run during `R CMD check`.

### A2. BLOCKER — `find_sentiment_probs()` reads scoring weights from the package dir
- **File/line:** `R/sentiment.R:256-258`
  (`score_dir <- file.path(system.file("scoring", package = utils::packageName()), scoring, scoring_version)`),
  consumed at `:262`, `:268`, `:278`.
- **What:** the reader of the weights points at `system.file("scoring", ...)`, i.e. the same
  in-library path A1 writes to. Must move in lockstep with A1 or scoring silently can't find
  its files.
- **Why CRAN rejects:** companion to A1 — the read side must resolve the new
  `R_user_dir` cache, not the install dir.
- **Fix:** resolve `score_dir` from the same `.sentiment_cache_dir()` helper as A1. Guard the
  case where the file is absent and call `install_scoring_model()` (download-on-demand) before
  reading, returning an informative error if still missing.

### A3. BLOCKER — `install_default_embeddings()` downloads into the package dir
- **File/line:** `R/init_and_install.R:368-390`
  (`pkg_path <- system.file(package = "sentiment.ai")`,
  `dl_path <- file.path(pkg_path, "default_embeddings")`, `dir.create` `:381`,
  `utils::download.file(..., destfile = obj_path)` `:390`).
- **What:** downloads the precalculated default-embeddings JSON into the installed library.
- **Why CRAN rejects:** same as A1 — runtime write into read-only install location.
- **Fix:** same `R_user_dir("sentiment.ai", "cache")` migration. Note `inst/default_embeddings/0.1.0.json`
  ships in the package (read-only, fine via `system.file`); the *downloaded* newer versions
  must go to the cache. Have the reader check cache first, then fall back to the bundled
  `system.file("default_embeddings", "0.1.0.json", ...)`.

### A4. BLOCKER — `get_default_embedding()` reads embeddings from the package dir
- **File/line:** `R/embedding.R:38-39`
  (`pkg_path <- system.file(package = "sentiment.ai")`,
  `emb_file <- file.path(pkg_path, "default_embeddings", paste0(version, ".json"))`).
- **What:** read side of A3; looks in the install dir for a file A3 tries to write there.
- **Why CRAN rejects:** companion to A3.
- **Fix:** resolve from the `R_user_dir` cache (downloaded versions) with a `system.file`
  fallback to the bundled JSON.

### A5. BLOCKER — `init_sentiment.ai()` writes the TF-Hub module cache into the package dir
- **File/line:** `R/init_and_install.R:479-485`
  (`cache_dir <- file.path(pkg_path, "tfhub_modules")` where
  `pkg_path <- system.file(package = pkg_name)` at `:442`, then `dir.create(path = cache_dir, ...)`).
- **What:** sets the TF-Hub model download/cache directory to a subfolder of the installed
  library. (`.Rbuildignore` even has `^inst/tfhub_modules/*`, confirming this was being written
  in-tree.)
- **Why CRAN rejects:** runtime write into the install location; downloads multi-hundred-MB
  models on first use.
- **Fix:** point `cache_dir` at `tools::R_user_dir("sentiment.ai", "cache")` /`tfhub_modules`.
  This whole branch is legacy/opt-in (TF only) — keep it behind the `requireNamespace("tfhub")`
  guard in C2 so it never executes on the check farm.

---

## B. `:::` and non-public-API usage

CRAN policy explicitly forbids `:::` to access another package's internals (and
`R CMD check --as-cran` emits *"Unexported objects imported by ':::' calls"* /
*"There are ::: calls to the package's namespace ..."*). Reaching into `reticulate:::`
is the textbook rejection.

### B1. BLOCKER — `import(tensorflow)` / `import(tfhub)` of Suggests packages
- **File/line:** `NAMESPACE:13-14` (`import(tensorflow)`, `import(tfhub)`); generated from
  `R/init_and_install.R:95-96` (`#' @import tensorflow`, `#' @import tfhub`).
- **What:** full `import()` of two packages that `DESCRIPTION` lists under **Suggests**
  (`tensorflow (>= 2.2.0)`, `tfhub (>= 0.8.0)` at `DESCRIPTION:63,65`).
- **Why CRAN rejects:** a package may only `import`/`importFrom` packages in `Depends`/`Imports`.
  Importing a *Suggests* package is an ERROR (`'tensorflow' ... required but not available`)
  on any machine without TF, i.e. the entire check farm. This is the single most certain
  bounce.
- **Fix:** delete the `#' @import tensorflow` and `#' @import tfhub` roxygen tags, re-run
  `roxygen2::roxygenise()` so `import(tensorflow)`/`import(tfhub)` leave `NAMESPACE`. Access
  every TF/tfhub symbol via `tfhub::` / `tensorflow::` *inside* a `requireNamespace()` guard
  (see C2). Legacy USE is opt-in, so TF must stay in Suggests, not move to Imports.

### B2. BLOCKER — `reticulate:::py_install_method_detect()` in `install_sentiment.ai()`
- **File/line:** `R/init_and_install.R:141`
  (`method <- reticulate:::py_install_method_detect(envname = envname, ...)`).
- **What:** calls an unexported reticulate internal via `:::`.
- **Why CRAN rejects:** `--as-cran` flags `:::` to another namespace; reviewers reject on sight.
- **Fix:** the package already vendored this logic — `R/local_from_reticulate.R` defines a local
  `install_method_detect()` (`:210`) and a wrapper `py_install_method_detect()`
  (`R/init_and_install.R:652`). Replace the `reticulate:::` call at `:141` with the local
  `py_install_method_detect(envname = envname, ...)` (or `install_method_detect(...)` directly).
  This is the one remaining live `:::`; the file header at `R/local_from_reticulate.R:2` claims
  the `:::` was removed but `:141` was missed.

### B3. SHOULD — vendored reticulate internals are a maintenance/version-skew hazard
- **File/line:** all of `R/local_from_reticulate.R` (e.g. `miniconda_*` `:24-101`,
  `conda_python` `:121`, `virtualenv_default_python` `:185`, `install_method_detect` `:210`).
- **What:** ~220 lines copied from reticulate internals to dodge `:::`. Not a `--as-cran`
  ERROR by itself, but it re-implements private reticulate behaviour that can drift.
- **Why flagged:** reviewers dislike copied internals; they break silently when reticulate
  changes. Lower priority than B1/B2 but note it.
- **Fix:** prefer public reticulate API where one exists — `reticulate::virtualenv_exists()`,
  `reticulate::conda_binary()`, `reticulate::conda_list()`, `reticulate::py_discover_config()`
  are public and already used. The only genuinely-missing public function is the
  method-detect heuristic; keep the minimal vendored `install_method_detect()` and drop the
  rest where a public call suffices. Attribute the copied code (reticulate is Apache-2.0) in a
  comment if retained.

### B4. SHOULD — local `sprintf()` shadow is dead/confusing
- **File/line:** `R/local_from_reticulate.R:13-21` (re-defines `sprintf` in the package namespace).
- **What:** shadows `base::sprintf`; only used by the vendored `conda_python`. Harmless but
  surprising; a reviewer may query it.
- **Fix:** remove and call `base::sprintf()` directly, or rename to `.retic_sprintf` to avoid
  masking a base generic in the package namespace.

---

## C. `requireNamespace()` guards for Suggests (tensorflow / tfhub / rstudioapi / rappdirs)

CRAN policy: *"Packages using a package in Suggests ... must do so conditionally"* — i.e.
behind `requireNamespace("pkg", quietly = TRUE)`. Any unconditional use of a Suggests
package is a check failure on machines lacking it.

### C1. BLOCKER — `rstudioapi` (Suggests) used unconditionally
- **File/line:** `R/init_and_install.R:236-237`
  (`if(restart_session && rstudioapi::hasFun("restartSession")){ rstudioapi::restartSession() }`).
- **What:** `rstudioapi::` called without a `requireNamespace` guard. `rstudioapi` is in
  **Suggests** (`DESCRIPTION:62`).
- **Why CRAN rejects:** Suggests package used unconditionally → error where `rstudioapi` is
  absent (headless check farm).
- **Fix:**
  `if (restart_session && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::hasFun("restartSession")) rstudioapi::restartSession()`.
  Also gate on `interactive()` (see D2) — never restart a session non-interactively.

### C2. BLOCKER — `tfhub` / `tensorflow` (Suggests) used unconditionally
- **File/line:** the legacy hub-embedding path. `load_hub_embedding(model, cache_dir)` is called
  at `R/init_and_install.R:507` and `:511`; the loader and `hub_embed()`
  (`R/embedding.R:118-155`, called from `embed_text` `:216`) depend on tfhub/tensorflow being
  loaded (the now-removed `import(tfhub)`/`import(tensorflow)` made these symbols available).
  `Sys.setenv("TF_FORCE_GPU_ALLOW_GROWTH"...)` at `:445` is also TF-specific.
- **What:** once B1 strips `import(tfhub)`/`import(tensorflow)`, every legacy call site must
  guard and namespace-qualify, or it errors at run/check time.
- **Why CRAN rejects:** Suggests used unconditionally; without the guard the legacy path errors
  on any TF-less machine instead of degrading.
- **Fix:** wrap the entire legacy branch in
  `if (!requireNamespace("tfhub", quietly = TRUE) || !requireNamespace("tensorflow", quietly = TRUE)) stop("Legacy USE models require the optional 'tensorflow' and 'tfhub' packages; install them or use a default (e5) / openai model.", call. = FALSE)`
  and qualify all symbols as `tfhub::` / `tensorflow::`. e5 (default) and openai paths must
  have zero TF/tfhub touch so the package is fully usable with neither installed.

### C3. SHOULD — `rappdirs` (Suggests) used unconditionally in vendored code
- **File/line:** `R/local_from_reticulate.R:52`
  (`root <- rappdirs::user_data_dir("r-reticulate")`, inside `miniconda_meta_path()`).
- **What:** `rappdirs::` with no guard; `rappdirs` is in **Suggests** (`DESCRIPTION:61`).
- **Why flagged:** Suggests used unconditionally. Reached only via the miniconda-install path,
  but `--as-cran` does not care that it's rarely hit.
- **Fix:** either guard with `requireNamespace("rappdirs", quietly = TRUE)`, or — cleaner —
  replace with `tools::R_user_dir("reticulate", "data")` (base R, ≥ 4.0; you already
  `Depends: R (>= 4.0.0)` at `DESCRIPTION:42`) and drop the rappdirs dependency entirely.

### C4. SHOULD — `parallel` / `utils` functions used unqualified (not in NAMESPACE)
- **File/line:** `R/init_and_install.R:670` `makeCluster(...)`, `:678` `parLapply(...)`,
  `:682` `stopCluster(...)`; `R/embedding.R:299` `makeCluster`, `:304` `clusterExport`,
  `:332` `stopCluster`; `R/embedding.R:255` `txtProgressBar`, `:282` `setTxtProgressBar`
  (the `init_and_install`/`embedding` copies at `:133`,`:150` are correctly `utils::`-qualified).
  Also bare `library(parallel)` at `R/embedding.R:1`.
- **What:** `makeCluster`/`parLapply`/`stopCluster`/`clusterExport` (from `parallel`) and
  `txtProgressBar`/`setTxtProgressBar` (from `utils`) are called bare. `parallel` and `utils`
  are in `Imports` but `NAMESPACE` only `importFrom`s a handful of symbols — these are not
  imported, so they resolve only by accident (search path / `library(parallel)` side effect).
- **Why flagged:** `--as-cran` emits *"no visible global function definition for 'makeCluster'"*
  etc. NOTEs; the `library(parallel)` call inside a package is also flagged (packages must not
  call `library()`).
- **Fix:** qualify as `parallel::makeCluster`, `parallel::parLapply`, `parallel::stopCluster`,
  `parallel::clusterExport`, `utils::txtProgressBar`, `utils::setTxtProgressBar`, OR add
  `@importFrom` tags. Delete `library(parallel)` at `R/embedding.R:1`.

---

## D. Interactive prompts / `readline` / session restart gating

CRAN policy: examples, tests and vignettes run non-interactively; any code path that can be
triggered there must not block on input or restart the session. `R CMD check` runs with
`interactive() == FALSE`.

### D1. BLOCKER — `readline()` prompt in `check_virtualenv_py()`
- **File/line:** `R/init_and_install.R:706`
  (`inst_promt <- readline(paste0("Python ", version, " is missing. Install now? [Y/n]:\n"))`),
  reachable from `install_sentiment.ai()` via the virtualenv branch (`:201`).
- **What:** blocks on `readline` for a y/n install confirmation.
- **Why CRAN rejects:** a non-interactive `readline` returns `""` immediately and/or hangs
  under `--as-cran`; prompting at all from a documented installer path is rejected.
- **Fix:** guard with `if (interactive()) { ... readline ... } else { stop("Python ", version,
  " is not installed; install it with reticulate::install_python() and retry.", call. = FALSE) }`.

### D2. BLOCKER — `rstudioapi::restartSession()` can fire non-interactively
- **File/line:** `R/init_and_install.R:236-237`.
- **What:** restarts the R session at the end of `install_sentiment.ai()`. Default
  `restart_session = TRUE` (`:111`).
- **Why CRAN rejects:** restarting the session during a check is fatal; also unconditional
  Suggests use (see C1).
- **Fix:** gate on `interactive() && requireNamespace("rstudioapi", quietly = TRUE) &&
  rstudioapi::hasFun("restartSession")`. Consider defaulting `restart_session = interactive()`.

### D3. SHOULD — `miniconda_install_prompt()` readline (vendored)
- **File/line:** `R/local_from_reticulate.R:85` and `:99`
  (`readline("Would you like to install Miniconda? [Y/n]: ")`, `readline("Please answer yes or no: ")`).
- **What:** interactive miniconda prompt. Already guarded by `if (!interactive()) return(FALSE)`
  at `:77`, so it is safe under check — hence SHOULD not BLOCKER.
- **Why flagged:** keep the `interactive()` guard; verify the only caller
  (`install_method_detect` `:215`) is itself never hit from examples/tests (it isn't, given
  all examples are `\dontrun`). No code change strictly required; documented for completeness.

### D4. SHOULD — stray `print(url)` debug call
- **File/line:** `R/embedding.R:399` (`print(url)` in the Azure branch of `load_openai_embedding`).
- **What:** leftover debug print to console.
- **Why flagged:** CRAN dislikes packages writing to the console outside `message`/`warning`;
  `print()` for debugging is a review nit. Not an ERROR.
- **Fix:** remove it (or convert to `if (!silent) message(url)`).

---

## E. Examples / tests that need weights, network, Python, or an API key

CRAN policy: examples run by default must not require internet, a Python env, downloaded
weights, or credentials. Tests must `skip_on_cran()` / `skip_if_not_installed()` for the same.

### E1. SHOULD — confirm every weight/Python/network example is `\dontrun{}`
- **File/line:** `R/init_and_install.R:74-84` (setup example — already `\dontrun`);
  `R/sentiment.R:28-56` (`sentiment_score`, `\dontrun`), `:133-153` (`sentiment_match`, `\dontrun`);
  `R/embedding.R:234-238` (`openai_embed`, `\dontrun`).
- **What:** these are correctly wrapped. The two that are **NOT** wrapped are E2.
- **Why flagged:** mostly already compliant; listed so the fix in E2 isn't missed.
- **Fix:** none for these; verify after roxygen regen.

### E2. BLOCKER — `load_openai_embedding()` example runs live (not `\dontrun`)
- **File/line:** `R/embedding.R:355-358` — the roxygen example block is bare:
  ```
  #' embed_text <- load_openai_embedding("text-embedding-ada-002", "your_api_key_here")
  #' result <- embed_text("some text")
  ```
  (no `\dontrun{}`). Note this function is not currently exported, but if it is exported/
  documented the example is run; even as-is `R CMD check` runs documented examples.
- **What:** an example that constructs an OpenAI embedder with a fake key and then calls it,
  hitting the live OpenAI API over the network.
- **Why CRAN rejects:** example performs a network call with a bogus credential → error/hang
  on the check farm; requires internet + secret.
- **Fix:** wrap both lines in `\dontrun{ ... }` (matching the `openai_embed` example at
  `:234-238`). If the function stays internal, drop `@examples` entirely / mark `@keywords internal`
  and `@noRd`.

### E3. BLOCKER — no test infrastructure with `skip_on_cran()` (and download/Python in any added test)
- **File/line:** repository has **no `tests/` directory** (confirmed: `tests/` absent;
  `Suggests: knitr, rmarkdown` only). When tests are added (v2 needs them), any test that calls
  `init_sentiment.ai`, `install_*`, `embed_text`, or `sentiment_*` will download weights / spin
  up Python / hit the network.
- **What:** future/parallel test work must not run these on CRAN.
- **Why CRAN rejects:** tests that download or require Python error on the check farm.
- **Fix:** every such test starts with `testthat::skip_on_cran()` plus
  `skip_if_not_installed("reticulate")` / `skip_if_offline()` / `skip_if(Sys.getenv("OPENAI_API_KEY") == "")`
  as appropriate. (Tracked as a blocker because v2 ships tests; flag now so the test author
  bakes the skips in. This item is owned by the test track — see `planning/v2-test-plan.md`.)

### E4. SHOULD — vignette must not build against Python/weights on CRAN
- **File/line:** `DESCRIPTION:40` `VignetteBuilder: knitr`, `DESCRIPTION:66-82` Collate /
  `vignettes/` present.
- **What:** if the vignette evaluates embedding chunks it needs Python + weights at build time.
- **Why flagged:** CRAN builds vignettes; a live-embedding chunk fails.
- **Fix:** set those chunks `eval = FALSE` (or precompute / `purl`), and gate on
  `requireNamespace("reticulate")` + a smoke check.

---

## F. DESCRIPTION metadata

### F1. BLOCKER — unused `Imports: openai`
- **File/line:** `DESCRIPTION:47` (`openai,` under `Imports`).
- **What:** the R `openai` package is listed in `Imports` but **never used** in R code — the
  package talks to OpenAI directly via `httr` (`R/embedding.R:368-411`: `httr::add_headers`,
  `httr::POST`, `httr::status_code`, `httr::content`). The string `openai` elsewhere is the
  *Python* module name in the install `modules` list (`R/init_and_install.R:108,126`) and HTTP
  URLs — not the R package. No `openai::` call exists anywhere in `R/`.
- **Why CRAN rejects:** `--as-cran` NOTE *"Namespace in Imports field not imported from: 'openai'"*
  / *"All declared Imports should be used."* CRAN treats unused Imports as a submission blocker.
- **Fix:** remove `openai,` from `Imports` in `DESCRIPTION:47`. (`httr` and `jsonlite` are
  genuinely used and stay.)

### F2. SHOULD — `Imports` vs actual usage audit (xgboost, pbapply, parallel, jsonlite)
- **File/line:** `DESCRIPTION:43-54`.
- **What:** verify each is used and reachable:
  - `xgboost` — used (`R/sentiment.R:278` `xgboost::xgb.load`). OK.
  - `pbapply` — used only in `openai_embed_parallel()` (`R/embedding.R:313-314`), which is
    **not called** anywhere (the live path uses `openai_embed`, `:208/:211`). If
    `openai_embed_parallel` stays dead, `pbapply` is an unused Import → same NOTE as F1.
  - `jsonlite` — used (`R/embedding.R:14` `jsonlite::fromJSON`; vendored
    `R/local_from_reticulate.R:61,71`). OK.
  - `parallel` — used but unqualified (see C4); keep in Imports, fix qualification.
- **Why flagged:** unused Imports NOTE if `pbapply` path stays dead.
- **Fix:** either wire `openai_embed_parallel` into `embed_text` (the `parallel > 2` branch at
  `R/embedding.R:206` currently calls the *serial* `openai_embed` in both arms — a latent bug),
  or remove `pbapply` from Imports and delete the dead parallel function.

### F3. SHOULD — `RoxygenNote: 7.1.2` is stale
- **File/line:** `DESCRIPTION:39`.
- **What:** old roxygen2. Re-document with current roxygen2 (needed anyway to regen NAMESPACE
  for B1/C4) and let it bump this field.
- **Fix:** `roxygen2::roxygenise()` with a current roxygen2; commit the regenerated `NAMESPACE`
  and `man/`.

### F4. NICE — `Description` field references a live anchor URL
- **File/line:** `DESCRIPTION:29-32` (`<https://benwiseman.github.io/sentiment.ai/#Benchmarks>`).
- **What:** CRAN's URL checker pings this; `#Benchmarks` anchor must resolve (200) at submission.
- **Fix:** ensure the benchmarks page/anchor is live before submitting (ties into the
  benchmark-page work in `planning/docs-benchmark-page-spec.md`).

---

## G. Other `--as-cran` items surfaced while reading

### G1. SHOULD — `.onLoad` emits a `message()`/`warning()` on load
- **File/line:** `R/onload.R:4-28` (`warning(...)` Apple-Silicon block) and `:32-37`
  (`message(...)` soft-deprecation).
- **What:** `.onLoad` should be silent; startup chatter belongs in `.onAttach` via
  `packageStartupMessage()`. A `warning()` fired from `.onLoad` is specifically discouraged and
  can trip `--as-cran`.
- **Why flagged:** CRAN: *"Packages should not ... produce output on load."* `.onLoad`
  `message`/`warning` is a NOTE/reviewer flag.
- **Fix:** move user-facing text to `.onAttach(libname, pkgname)` using
  `packageStartupMessage()`; keep `.onLoad` for setup only (or empty).

### G2. NICE — `.onLoad` deprecation text references non-existent model handles
- **File/line:** `R/onload.R:33-36` mentions `'paraphrase'`, `'oai_3_small'`, `'use'` — none of
  which are in `constants.R` (`default_models` = `e5-small`/`e5-base`; `openai_models` =
  `text-embedding-3-small/large`, `ada-002`; `legacy_models` = `en`/`en.large`/`multi`/`multi.large`).
  Also `'API ket'` typo (`:35`).
- **What:** not a CRAN gate, but the startup message advertises model names the package can't
  resolve — `choose_model()` (`R/choose_model.R`) would warn "cowboy mode" on them.
- **Fix:** update the message to the real v2 handles (`e5-small`, `e5-base`,
  `text-embedding-3-small`) when moving it to `.onAttach` (G1).

### G3. SHOULD — `inst/scoring/` and `inst/default_embeddings/` ship only `readme.txt` + one JSON
- **File/line:** `inst/scoring/readme.txt` (no weights bundled), `inst/default_embeddings/0.1.0.json`.
- **What:** confirms the package relies on runtime download for scoring weights (A1/A2). Fine for
  size, but means `find_sentiment_probs()` (A2) **will** fail closed if download is blocked.
- **Fix:** ensure A1/A2's `R_user_dir` cache + download-on-demand path returns a clear error
  (not a cryptic file-not-found) when offline, and that examples/tests never hit it (covered by
  E1-E3).

---

## Blocker-level item count

**14 BLOCKER items** (deduplicated, as listed):
A1, A2, A3, A4, A5, B1, B2, C1, C2, D1, D2, E2, E3, F1.

### BLOCKER_COUNT = 14
(A1, A2, A3, A4, A5, B1, B2, C1, C2, D1, D2, E2, E3, F1)

SHOULD: B3, B4, C3, C4, D3, D4, E1, E4, F2, F3, G1, G3 (12).
NICE: F4, G2 (2).
