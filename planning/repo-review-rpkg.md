# `sentiment.ai` — senior R-package / CRAN-maintainer review (v2 branch)

> Reviewer lens: R-package engineering + CRAN maintainership. Scope: `rpackage/sentiment.ai/`.
> Read in full before writing: `R/init_and_install.R`, `R/embedding.R`, `R/sentiment.R`,
> `R/constants.R`, `R/choose_model.R`, `R/model_meta.R`, `R/local_from_reticulate.R`,
> `R/object-sentiment_env.R`, `R/onload.R`, `R/matrix_helpers.R`, `R/create_error_text.R`,
> `NAMESPACE`, `DESCRIPTION`, `inst/get_embedder.py`, and the `tests/testthat/` suite.
> Every claim is cited to a file:line. Planning-only — this doc does NOT edit `R/`.
> Written 2026-06-03. Author: Ben Wiseman.

## What this review deliberately does NOT repeat

Two existing docs are strong and I build on them rather than restate:

- **`planning/cran-checklist.md`** — the 14 CRAN blockers (in-library writes → `R_user_dir`,
  `reticulate:::`, Suggests guards, `readline`/`restartSession`, unused `Imports: openai`,
  `import(tensorflow)`/`import(tfhub)` in NAMESPACE). I treat that list as authoritative and
  reference it; I do not re-derive it. **One correction to it below** (it predates the new
  `tests/` dir and one of its premises is now stale).
- **`planning/better-lens-rcraft.md` / `better-sentiment-roadmap.md`** — the return-contract
  finding (`sentiment_score` discards the 3-class signal at `R/sentiment.R` and returns a bare
  vector while `sentiment_match` returns a `data.table`). I agree completely and extend it with
  the *implementation mechanics* (§B3) — but I will not re-argue the case for it; it's made.

This review's distinct contribution is the **wiring/architecture layer the other docs skip**:
the v2 init-dispatch that is genuinely *not finished*, the `sentiment.env` state object, the
embedding orientation contract, and a set of latent bugs not yet catalogued anywhere.

---

## Headline: the test suite is the v2 spec, and the source has not caught up to it

The single most important structural fact about this branch: **the `tests/testthat/` suite is
written TDD-first and is RED against contracts the source does not yet implement.** The tests
encode the intended v2 design; `R/` lags it. Three concrete, verifiable gaps:

1. **There is no `st` (sentence-transformers / e5) branch in `init_sentiment.ai()`.**
   `init_sentiment.ai()` (`R/init_and_install.R:496-513`) has exactly two arms: OpenAI
   (`api_key` + openai model → `load_openai_embedding`, `:499`) and *everything else* →
   `load_hub_embedding(model, cache_dir)` (`:507`,`:511`) — the **TensorFlow** path. The e5
   default therefore falls through to the TF-Hub loader, which is the one thing v2 exists to
   avoid. `inst/get_embedder.py` already ships `load_st_embedder(hf_id, prefix)` (`:31`), but
   **nothing in R calls it.** The "wire the init dispatch" item in the brief is not a cleanup —
   it's the load-bearing remaining work, and `test-backend-e5.R:8` (`init_sentiment.ai(model =
   "e5-small")` then `embed_text(...)`) cannot pass until it lands.

2. **`sentiment.env$backend` is asserted by tests but never set.**
   `test-backend-e5.R:19` asserts `sentiment.env$backend == "st"`. The env object
   (`R/object-sentiment_env.R:9-12`) only ever sets `$openai <- FALSE` and `$parallel <- 0`.
   No code path ever assigns `$backend`. This field is the registry the whole dispatch should
   turn on (see §A).

3. **The legacy gate asserted by `test-legacy-gate.R` does not exist.**
   The test demands that `init_sentiment.ai(model = "multi.large")` *without TF* raise an R-level
   error naming both `legacy = TRUE` and the replacement model, **before any Python import**
   (`test-legacy-gate.R:35-46`). Today there is no such gate: `choose_model("multi.large")`
   (`R/choose_model.R:24`) happily returns the tfhub URL, `init` proceeds to `source_python`
   (`R/init_and_install.R:457`) and `load_hub_embedding`, and the failure surfaces as a raw
   Python `ImportError` on `tensorflow_text` — exactly the "quietly wrong → cryptic crash"
   the gate is meant to replace. Note `install_sentiment.ai()` also has **no `legacy =` argument
   at all** (`:98-112`), so the message the gate tells users to run (`install_sentiment.ai(legacy
   = TRUE)`) currently does nothing.

**Correction to `cran-checklist.md`:** it states (line 7) "no `tests/` directory exists." That
is now false on this branch — there are 21 test files plus `helper-skips.R`. Its blocker **E3**
("no test infrastructure with `skip_on_cran()`") is therefore **resolved**: `helper-skips.R`
provides `skip_if_no_st()`, `skip_if_no_tf()`, `skip_on_cran()` usage, and a no-Python
`local_fake_embedder()`. Downgrade E3 from BLOCKER to done; the rest of that checklist stands.

The good news: the test design is genuinely excellent (a no-Python fake embedder that records
the strings it's handed so the `"query: "` prefix injection can be asserted, `local_mocked_bindings`
to stub `find_sentiment_score`, tiered skips). The implementation just has to be built to match it.

---

# TOP 3 — the highest-leverage engineering fixes

Ordered by leverage. #1 and #2 are the v2 finish line; #3 is the durable design fix that prevents
the next class of bug. These are *complementary to*, not overlapping with, the return-contract fix
in `better-lens-rcraft.md` (which I rank as the #1 **product** change; these are the #1–3 **build**
changes that have to land for v2 to be coherent and CRAN-shippable).

---

## #1 — Finish the backend dispatch: route on `model_class()`, set `$backend`, gate legacy

This is the actual remaining v2 work and it has a clean shape. The package *already has* the
router primitive — `model_class(model)` in `R/constants.R:38-44` returns `"st" | "openai" |
"legacy" | "unknown"`. Nothing calls it. Make it the single dispatch point.

**The concrete design (replaces the ad-hoc `if (openai) … else hub` ladder):**

```r
# init_sentiment.ai(), after choose_model() resolves the id:
backend <- model_class(model_handle)      # "st" | "openai" | "legacy" | "unknown"
env <- sentiment.env
env$backend <- backend                    # <-- the field test-backend-e5.R:19 needs
env$model   <- model_handle               # for provenance + the prefix contract (§C)

switch(backend,
  st = {
    # the missing arm. Source get_embedder.py, call load_st_embedder.
    reticulate::source_python(system.file("get_embedder.py", package = pkg_name))
    prefix    <- model_prefix[[model_handle]] %||% ""     # constants.R:32
    env$embed <- load_st_embedder(choose_model(model_handle), prefix)
    env$openai <- FALSE
  },
  openai = {
    if (is.null(api_key)) cli::cli_abort("OpenAI model {.val {model_handle}} needs an api_key.")
    env$embed <- load_openai_embedding(model_handle, api_key, api_base, ...)
    env$openai <- TRUE
  },
  legacy = {
    if (!requireNamespace("tensorflow", quietly = TRUE) ||
        !requireNamespace("tfhub", quietly = TRUE) ||
        !reticulate::py_module_available("tensorflow")) {
      cli::cli_abort(c(
        "Legacy USE model {.val {model_handle}} needs the optional TensorFlow backend.",
        "i" = "Enable it with {.run install_sentiment.ai(legacy = TRUE)}.",
        "i" = "Or use the TensorFlow-free replacement {.val {legacy_repl[[model_handle]]}}."
      ))
    }
    reticulate::source_python(system.file("get_embedder.py", package = pkg_name))
    env$embed  <- load_hub_embedding(choose_model(model_handle), cache_dir)
    env$openai <- FALSE
  },
  unknown = cli::cli_abort("Unknown model {.val {model_handle}}.")
)
```

Why this specific shape:

- **The legacy gate fires at the R level before `source_python`**, which is *exactly* what
  `test-legacy-gate.R:35-46` proves (it mocks `reticulate::source_python` to fail if reached).
  That test cannot pass with the current "fall into the TF path and let Python `ImportError`"
  flow. The gate also needs a `legacy_repl` map — it already lives in the test
  (`test-legacy-gate.R:12-13`); promote it into `constants.R` so source and test share one truth.
- **`env$backend` is set in one place**, so the registry the dispatch turns on can't drift from
  what actually loaded. This is the field tests assert and the field a provenance stamp (§C)
  reads.
- **`get_embedder.py` is sourced inside the branch that needs it** — the `st` arm and the
  `legacy` arm both source it, but the `openai` arm (pure-R `httr`) and the legacy *gate failure*
  never touch Python. Today `source_python` runs unconditionally at `:457` for every non-OpenAI
  model, which is why the legacy gate's "no Python import" guarantee is currently impossible.

Add the missing `legacy` argument to `install_sentiment.ai()` (`R/init_and_install.R:98`) and have
it gate the TF modules: the default module list **still installs TensorFlow 2.4.1 unconditionally**
(`:105-107`) — for a package whose headline is "no TensorFlow by default," the *installer* still
makes TF mandatory. The default install should pull only `sentence_transformers` (+ `torch`,
`numpy`); TF/tfhub/tensorflow-text move behind `legacy = TRUE`. This also shrinks the install
surface that triggers the OpenMP collision the brief mentions.

**Effort:** ~1 day for the dispatch + gate; the Python side and the tests are already written.
**Unblocks:** `test-backend-e5.R`, `test-legacy-gate.R`, and the entire "smoke-tested but not
wired" status the brief describes.

---

## #2 — Fix the embedding orientation contract + the dead OpenAI-parallel branch (latent bugs)

`embed_text()` (`R/embedding.R:175-223`) is where the backends converge, and it has a real
orientation hazard plus a dead branch — neither is in any existing review doc.

**(a) Double-transpose / orientation mismatch across backends.** The contract is "rows = texts,
cols = dims, rownames = the text" — set at `R/embedding.R:219-220` (`t(as.matrix(text_embed))`
then `rownames<-`). But the three producers disagree on what they hand in:

- `load_st_embedder` (`inst/get_embedder.py:70-74`) returns `model.encode(...)` which is **already
  `(n_rows, dim)`** — rows = texts. Passing that through `t()` at `:219` would transpose it to
  `(dim, n_rows)` — **wrong**. The `st` arm must NOT go through the `hub_embed`/`t()` path.
- `hub_embed` (`R/embedding.R:118-155`) builds a `data.table` of `t(as.matrix(this_embed))`
  (`:141`) i.e. cols = texts, so the outer `t()` at `:219` un-transposes it back. This only works
  because two transposes cancel — fragile, and it's why `text_embed` is a `data.table` here but a
  matrix elsewhere.
- `openai_embed` (`R/embedding.R:252-288`) `cbind`s column-vectors → `(dim, n)`, so the outer
  `t()` gives `(n, dim)`. OK, but by a *different* convention than the other two.

This is exactly the bug class `test-dim-handling.R` is guarding (`expect_identical(rownames(emb),
c("good","bad","fine"))`, `nrow == 3`). **Recommendation:** make each backend return the canonical
`(n_rows, dim)` matrix *itself*, delete the outer `t()` at `:219`, and have `embed_text` only set
rownames. One orientation, asserted once, no cancelling transposes. The fake embedder in
`helper-skips.R:104` already returns `t(emb)` to "mimic that orientation" (rows = texts) — so the
test harness has already chosen the convention; the real backends must match it.

**(b) The OpenAI-parallel branch is dead and the live path is accidentally serial.**
`embed_text` `:206-212`:

```r
if (sentiment.env$parallel > 2) {
  text_embed <- openai_embed(text, request_limit, token_limit)   # serial
} else {
  text_embed <- openai_embed(text, request_limit, token_limit)   # identical
}
```

**Both arms call the serial `openai_embed`.** `openai_embed_parallel()` (`:293-338`) — the only
consumer of `pbapply` (`R/embedding.R:313-314`) — is **never called**. So: (i) OpenAI embedding of
N texts is N sequential HTTP round-trips with a per-call progress bar — painfully slow and a
silent cost/latency trap; (ii) `pbapply` is an unused `Import` → the CRAN NOTE
`cran-checklist.md` F2 flags. **Either** wire the `> 2` arm to `openai_embed_parallel` (and make
the rate-limit counters actually shared across workers — right now they're `clusterExport`ed once
and never read back, so the limiter is a no-op in the parallel version, `:304-310`), **or** delete
`openai_embed_parallel` and drop `pbapply`. Given the rate-limit correctness problem, I'd delete
the parallel version for now and batch the serial one instead (OpenAI's embeddings endpoint takes
an *array* `input` in one request — `load_openai_embedding` already sends `input = text`,
`:374` — so the real win is one request per *batch*, not one per *text*; the current
`openai_embed` loop sends one text per request, `:252`,`:272`, which is the actual performance bug).

**(c) `embed_text`'s recovery path is the "quietly wrong" anti-pattern.** When `sentiment.env$embed`
is NULL it `warning()`s then *silently calls `init_sentiment.ai()` with guessed args*
(`R/embedding.R:187-198`). `cran-checklist.md` and `review-lineup.md` both flag the principle;
the *mechanism* to fix is: this is the place that should `cli::cli_abort()` with a classed
condition (`sentiment_ai_not_initialised`) and the literal next command, not recover by guessing.
A user who never ran `init` should get a directed error, not a model loaded under defaults they
didn't choose (which then silently uses `e5-small` even if they wanted `e5-base`).

**Effort:** ~half a day. **Unblocks:** clean `test-dim-handling.R` across all widths, removes
the `pbapply` NOTE, and kills two latent correctness bugs.

---

## #3 — Promote `sentiment.env` from an exported mutable global to a real state object

`sentiment.env` (`R/object-sentiment_env.R:9`, `@export`) is a bare `new.env()` exported into the
user's namespace and mutated in place from half a dozen functions. This is the package's central
piece of state and it's the weakest-designed object in the codebase. Problems, all grounded:

- **It's exported and user-writable.** `export(sentiment.env)` (`NAMESPACE:9`) means a user can do
  `sentiment.env$embed <- function(x) "lol"` and silently corrupt scoring. Exported only because
  internal code reaches it via `sentiment.ai::sentiment.env$...` (e.g. `R/embedding.R:187,205,242`,
  `R/init_and_install.R:489`). The `helper-skips.R:83-85` `.sentiment_env()` accessor already shows
  the right pattern: `get("sentiment.env", envir = asNamespace("sentiment.ai"))` — reach it via the
  namespace, and it does **not** need to be exported. Un-exporting it is a clean win (and removes a
  documented-object obligation).
- **Its state is a flat boolean (`$openai`) where the design wants a class (`$backend`).** Routing
  on `if (sentiment.env$openai)` (`R/embedding.R:205`) is a two-valued proxy for a four-valued fact
  (`st`/`openai`/`legacy`/`unknown`). The moment the `st` branch lands (#1), `$openai == FALSE` no
  longer means "TF hub" — it means "st *or* legacy." Every `if ($openai) … else hub` site
  (`R/embedding.R:205-217`) is then wrong for e5. Replace the boolean with `$backend` and
  `switch()` on it everywhere; keep `$openai` only as a deprecated alias if needed.
- **No accessor, no print method, no provenance.** This object is the natural home for the
  reproducibility stamp the roadmap wants (`{package_version, model, backend, model_revision,
  prefix, scoring, scoring_version}`). Give it a constructor (`.new_sentiment_state()`), populate
  `$backend`/`$model`/`$prefix` in `init` (#1), and add `sentiment_status()` — a single readiness
  call that prints, via `cli`, "backend: st · model: e5-small · revision: <sha> · scorer: xgb 1.0 ·
  weights: cached at …". That function is the install-legibility fix from `better-lens-rcraft.md`
  #3 **and** the provenance surface from the roadmap, and it falls out of this object naturally.

**The prefix-as-property contract belongs here too.** `model_prefix` (`R/constants.R:32`) is the
e5 `"query: "` string. `review-lineup.md` (SEV-1, per the brief) wants embedder-prefix ==
scorer-prefix asserted at score time. With `$prefix` stored on the state object at init, the
assertion is one line in `find_sentiment_score`: the scorer for `e5-small` was trained on
`"query: "`-prefixed embeddings, so if `sentiment.env$prefix` doesn't match the scorer's expected
prefix, abort. Today the prefix is applied *inside* `load_st_embedder` (`get_embedder.py:65-68`)
and never surfaced to R, so R can't check it and a provenance stamp would be guessing.

**Effort:** ~1 day, mostly mechanical (un-export, add accessor + `switch` sites + `print`/status).
**Why it's #3 not lower:** every other v2 fix writes to or reads from this object; designing it
properly once stops the dispatch logic from re-fragmenting the next time a backend is added
(the roadmap's "multi-head platform" vision means a *4th* backend is coming).

---

# Secondary findings (concrete, not in the cut, grouped)

**API design / idiom**

- **`sentiment_score` and `sentiment_match` documented params are stale to the point of wrong.**
  `@param x` says "512-D numeric embedding" (`R/sentiment.R:6-8`) — false for every v2 model
  (384/768/1536). `@param model` documents only the legacy USE handles (`:9-10`); the default is
  `e5-small`. `init_sentiment.ai`'s `@param model` default list is `c("en.large", …, "ada-002")`
  (`R/init_and_install.R:427`) — none of which is the actual default `DEFAULT_MODEL <- "e5-small"`.
  The `@details` table (`:28-36`) still lists `tensorflow 2.4.1` / `tensorflow-text` as required.
  All of this re-documents the package as a TF tool. Full roxygen rewrite needed before CRAN.
- **`sentiment_match`'s `model` default differs from `sentiment_score`'s.** `sentiment_match` uses
  `model = names(default_models)` (`R/sentiment.R:163`, a length-2 vector → `[1]` taken at `:175`),
  while `sentiment_score` uses `model = DEFAULT_MODEL` (`:62`). Same intent, two spellings — unify
  on `DEFAULT_MODEL`.
- **`match.arg(scoring_version)` on a single-valued arg is a latent trap.** `scoring_version = "1.0"`
  then `match.arg(scoring_version)` (`R/sentiment.R:64,71`) — `match.arg` against a length-1 default
  "works" only because the formal equals the choice; the moment a second version exists this needs a
  real `choices=` vector. Minor, but it reads as a misuse.
- **`@export as_py_list`** (`R/embedding.R:425-426`) — a one-line reticulate helper is in the public
  API. It's an internal shim; `@keywords internal` / unexport.

**Error handling**

- **`create_error_text()` (`R/create_error_text.R`) `cat()`s padded text — it is not a condition.**
  Cannot be `tryCatch`'d on class, doesn't reach `rlang::last_error()`, prints even when the caller
  wanted silence. Every use site (`R/init_and_install.R:642`, `R/embedding.R:189`) should become
  `cli::cli_abort(c(msg, "i" = cause, ">" = fix), class = "sentiment_ai_*")`. This is the single
  biggest "felt" quality gap for a user hitting a problem. Add `cli` to `Imports` (it's tiny and
  already a transitive dep via most of the stack).
- **`print(url)` debug leftover** in the Azure branch (`R/embedding.R:399`) and `print(...)`
  calls in `get_embedder.py:118,125,127` — remove before submit (also `cran-checklist.md` D4).

**Python / reticulate integration**

- **`get_embedder.py` sets `USE_TF=0` / `USE_TORCH=1` at module import (`:24-25`) — good**, that's
  the right place to stop `transformers` importing the TF backend. But it's only effective if
  `get_embedder.py` is sourced *before* `transformers` is first imported in the session. With the
  current unconditional `source_python` at init (`R/init_and_install.R:457`) that holds; once #1
  moves sourcing into branches, ensure the `st` branch sources it *before* any other code can
  import `sentence_transformers`. Worth a comment in the dispatch.
- **The OpenMP collision (R-xgboost vs python-torch) the brief mentions** has a known mitigation
  worth recording: set `Sys.setenv(KMP_DUPLICATE_LIB_OK = "TRUE")` is the *unsafe* workaround;
  the *correct* one is to ensure only one OpenMP runtime loads — load `torch` (via the embedder)
  and `xgboost` such that they share `libomp`, or set `OMP_NUM_THREADS=1` for the xgb predict
  (scoring is cheap; the embed is the hot loop). Don't ship `KMP_DUPLICATE_LIB_OK`; it can mask
  real corruption. Document the chosen fix in the dispatch code, not just the smoke test.
- **`virtualenv_default_python()` (`R/local_from_reticulate.R:141`) drops reticulate's
  `required_python_version` check** (noted in the comment at `:144`). That's fine for the vendored
  copy, but it means the installer can pick a Python that the e5 wheel doesn't support. Pin a
  minimum in `install_sentiment.ai` (`python_version` default is still `"3.8.10"` at `:101` —
  torch/sentence-transformers want ≥3.9; the Apple-silicon branch already bumps to 3.9.6, `:120`).

**DESCRIPTION / NAMESPACE / build**

- **`Description` overclaims** (`DESCRIPTION:29-32`): "out-performing traditional lexicon-based
  sentiment analysis." The brand line is "matches OpenAI, beats the old default" — and the
  methodology review caveats the unqualified F1 headline. CRAN aside, this contradicts the honesty
  posture; reword to a cited, hedged claim and ensure the `#Benchmarks` anchor resolves (it's also
  `cran-checklist.md` F4).
- **`@import tensorflow` / `@import tfhub`** still in `init_and_install.R:95-96` → `NAMESPACE:13-14`
  (the `cran-checklist.md` B1 blocker). Confirmed present. The roxygen tags drive it; delete them
  and re-document. This is the hardest CRAN bounce and it's still live.
- **Collate order risk:** `constants.R` is loaded *11th* (`DESCRIPTION:78`), after
  `init_and_install.R`/`embedding.R`/`sentiment.R` which use `DEFAULT_MODEL`, `model_dims`,
  `default_models`. Package-level objects are fine at *call* time (load completes first), but the
  `model = DEFAULT_MODEL` *default argument* in `sentiment_score` (`R/sentiment.R:62`) and
  `model = names(default_models)` in `sentiment_match` (`:163`) are evaluated lazily so it works —
  fragile to rely on. Move `constants.R` and `model_meta.R` to the *top* of Collate so the registry
  exists before anything references it. Cheap, removes a footgun.
- **`library(parallel)` at `R/embedding.R:1`** — a `library()` call inside a package; CRAN flags it,
  and it's the reason the bare `makeCluster`/`parLapply` resolve (`cran-checklist.md` C4). Remove
  and qualify.

**Tests (the suite is good; two gaps)**

- The suite encodes the contract beautifully but **most of it is RED until #1 lands** — that's by
  design (TDD), but it means CI must distinguish "RED because unimplemented" from "regressed."
  Tag the not-yet-implemented contract tests with `skip("pending st dispatch — issue #N")` or an
  `xfail`-style marker so a green CI run is meaningful *now*, and flip them on as #1 ships.
- No test asserts the **`sentiment_match` return shape** (the `data.table` with `text, sentiment,
  phrase, class, similarity`, `R/sentiment.R:240`). Given the return-contract unification is the
  top product change, snapshot that shape now so the refactor is safe.

---

# The one-paragraph state-of-the-package

The v2 architecture is *sound and mostly designed* — `model_class()`, `model_dims`, `model_prefix`,
`model_meta`, the no-TF `get_embedder.py`, and a genuinely first-rate test harness are all in place.
What's missing is the **wiring**: the `st` branch that makes e5 the real default, the `$backend`
registry the tests assert, and the legacy gate that turns a Python `ImportError` into a directed R
error — all three of which are *already specified by the test suite* and just need to be built to
match it (#1). Layer on the orientation/parallel bug fixes (#2) and a properly-designed state object
that carries provenance and the prefix contract (#3), and the package is both CRAN-shippable (after
`cran-checklist.md`'s blockers) and architecturally ready for the multi-head platform the roadmap
envisions. The work is unusually low-risk because the spec is already written down in `tests/` — the
job is to make `R/` honest to it.
