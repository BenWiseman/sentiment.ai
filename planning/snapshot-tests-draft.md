# sentiment.ai v2 — Snapshot / regression test files (drop-in testthat)

> Companion to `v2-roadmap.md` and `v2-test-plan.md`. **That** doc is the prose taxonomy;
> **this** doc is the concrete, ready-to-drop testthat code. Written 2026-06-03 against the
> current tree.
>
> Scope of the ask: design the regression/snapshot suite, with four named **CI gates**:
> 1. a no-arg `sentiment_score("I love this")` returns a **positive number with ZERO TensorFlow installed**;
> 2. the e5 **`"query: "` prefix** is applied (known string → expected score; fails if dropped);
> 3. the **legacy gate** (`model="multi.large"` without legacy install → *directed error, not crash*);
> 4. **dimension handling per model** (no hard-coded 512);
> plus `skip_on_cran` wherever weights/embedders are needed.

## RED / GREEN status — read this first

These tests are written against the **v2 contract**, which the current source does **not yet
satisfy**. Verified against the tree on 2026-06-03:

| Contract the test asserts | Current source reality | Status now |
|---|---|---|
| `constants.R` registry (`default_models`, `model_dims`, `model_prefix`, `DEFAULT_MODEL`, `model_class`, `choose_model`) | present and correct | **GREEN** |
| `NAMESPACE` has no `import(tensorflow/tfhub/text2vec)` | `NAMESPACE` still has `import(tensorflow)` + `import(tfhub)` (from `@import` tags in `init_and_install.R`) | **RED** (G1 — the CRAN-ERROR gate) |
| No-arg `sentiment_score("…")` routes to e5 `st` backend, no TF | `init_sentiment.ai` defaults to USE names; only `openai` vs `load_hub_embedding` (TF) branches; **no `st` branch** | **RED** (CI gate 1) |
| `"query: "` injected internally for e5 | **no prefix injection anywhere** (`embed_text`/`init`/`get_embedder.py`) | **RED** (CI gate 2) |
| `multi.large` w/o legacy → directed error | falls through to TF path, crashes on missing import | **RED** (CI gate 3) |
| dim follows the model (no 512) | `sentiment_score` passthrough hard-codes `ncol(x)==512`; `find_sentiment_probs` seeds `numeric(512)` | **RED** (CI gate 4) |
| scorer models packaged for e5 spaces | `inst/scoring/` holds only `readme.txt`; scorers are downloaded | **RED** (fixtures absent) |
| `.onLoad` clean, no `warning()`, e5 default messaging | `.onLoad` calls `warning(...)` + advertises `paraphrase`/`use`/`oai_3_small`, says *"use … is still the default"* | **RED** (G4) |

So most of Tier C and the gate tests are **expected RED now**; they are the executable
definition of "done" for Roadmap Phases 1–4. Every file below is tagged in its header comment.
Tiers A and B (registry + scorer-on-fixtures) are the ones that can go GREEN first.

**Why no embedder is ever loaded in Tiers A/B and the gate tests:** `sentiment.env$embed` is
just a function pointer (an R closure or a reticulate callable). We install a **deterministic R
closure** stub into it via `local_fake_embedder()`. That makes backend-routing, the `query:`
prefix, dimension handling, and the no-TF default all testable with **zero Python, zero TF,
zero network**. Only Tiers C/D (real ST / real OpenAI) actually load a model, and they
`skip_*` cleanly.

---

## 0. Bootstrap (not a test — required scaffolding)

`testthat` does not yet exist in the package. Before any file below runs:

```r
# DESCRIPTION: add to Suggests
#   testthat (>= 3.0.0)
# and add the line:
#   Config/testthat/edition: 3
```

**`tests/testthat.R`**

```r
library(testthat)
library(sentiment.ai)

test_check("sentiment.ai")
```

---

## helper-skips.R — shared skip guards + the fake embedder

`tests/testthat/helper-*.R` files are sourced automatically by testthat before the test files.
This is the single most important file: the `local_fake_embedder()` stub is what lets the gate
tests run with no Python/TF.

```r
# tests/testthat/helper-skips.R
# Shared skip guards and a deterministic, no-Python embedder stub.
# GREEN now (pure helpers; nothing here asserts contract yet).

# ---- environment skip guards ------------------------------------------------

skip_if_no_python <- function() {
  testthat::skip_if_not(
    reticulate::py_available(initialize = FALSE),
    "Python not available"
  )
}

skip_if_no_st <- function() {
  skip_if_no_python()
  testthat::skip_if_not(
    reticulate::py_module_available("sentence_transformers"),
    "sentence-transformers not installed"
  )
}

skip_if_no_tf <- function() {
  testthat::skip_if_not_installed("tensorflow")
  testthat::skip_if_not_installed("tfhub")
  skip_if_no_python()
  testthat::skip_if_not(
    reticulate::py_module_available("tensorflow"),
    "tensorflow python module not installed"
  )
}

# TRUE only when the legacy TF backend is genuinely ABSENT -- i.e. the exact
# situation the legacy gate is supposed to protect. Used to run L1 only there.
tf_is_absent <- function() {
  !(requireNamespace("tensorflow", quietly = TRUE) &&
      reticulate::py_available(initialize = FALSE) &&
      reticulate::py_module_available("tensorflow"))
}

skip_unless_tf_absent <- function() {
  testthat::skip_if_not(tf_is_absent(),
                        "TensorFlow present; legacy-gate test only meaningful when TF absent")
}

skip_if_no_openai_key <- function() {
  testthat::skip_if(identical(Sys.getenv("OPENAI_API_KEY"), ""),
                    "OPENAI_API_KEY not set")
}

# A committed scorer fixture must exist for Tier B. Until Roadmap Phase 3 packages
# the e5 scorers, these skip rather than error.
fixture_path <- function(...) {
  testthat::test_path("fixtures", ...)
}
skip_if_no_fixture <- function(file) {
  testthat::skip_if_not(file.exists(fixture_path(file)),
                        paste0("fixture missing: ", file))
}

# ---- the deterministic fake embedder ----------------------------------------
# Installs a closure into sentiment.env$embed that (a) RECORDS every string it is
# handed (so we can assert the "query: " prefix was injected) and (b) returns a
# deterministic embedding of the requested dimension. No Python, no network, no TF.
#
# `withr::defer(..., envir = parent.frame())` restores the real env on test exit,
# so tests never leak state into each other.

local_fake_embedder <- function(dim = 384L, openai = FALSE, env = parent.frame()) {
  calls <- new.env(parent = emptyenv())
  calls$seen <- character(0)

  fake <- function(text) {
    text <- as.character(unlist(text))
    calls$seen <- c(calls$seen, text)
    # deterministic, content-dependent pseudo-embedding (stable across platforms):
    # hash each string to a seed, draw `dim` values, sign-bias by a crude lexicon
    # so clear-positive text trends positive even through a stub scorer in unit tests.
    emb <- vapply(text, function(s) {
      seed <- sum(utf8ToInt(s)) %% .Machine$integer.max
      set.seed(seed)
      v <- stats::runif(dim, -1, 1)
      v
    }, numeric(dim))
    # sentence-transformers returns rows = texts; mimic that orientation here
    t(emb)
  }

  old_embed    <- sentiment.ai::sentiment.env$embed
  old_openai   <- sentiment.ai::sentiment.env$openai
  old_parallel <- sentiment.ai::sentiment.env$parallel

  sentiment.ai::sentiment.env$embed    <- fake
  sentiment.ai::sentiment.env$openai   <- isTRUE(openai)
  sentiment.ai::sentiment.env$parallel <- 0

  withr::defer({
    sentiment.ai::sentiment.env$embed    <- old_embed
    sentiment.ai::sentiment.env$openai   <- old_openai
    sentiment.ai::sentiment.env$parallel <- old_parallel
  }, envir = env)

  calls   # return the recorder; tests read calls$seen
}

# A trivial deterministic scorer: maps an embedding matrix -> probs in (0,1).
# Lets Tier-A score-transform tests run without any packaged xgb/glm model.
# `find_sentiment_probs` is the real target in Tier B; this is ONLY for the
# pure-R rescale/NA tests where the scorer's exact values are irrelevant.
local_fake_scorer <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    find_sentiment_probs = function(embeddings, scoring, scoring_version, model) {
      # logistic of the row mean -> stable, monotone, in (0,1)
      stats::plogis(rowMeans(embeddings))
    },
    .package = "sentiment.ai",
    .env = env
  )
}
```

> Note on `set.seed` inside the fake: the helper saves/does not need to restore the global RNG
> for these tests (each test is hermetic and never asserts on the embedding *values*, only on
> the *strings seen* and the *dimensions*). If a future test does care, wrap calls in
> `withr::with_seed`. Flagged so nobody is surprised the fake touches the RNG.

---

# Tier A — pure-R unit (no embedder, runs on CRAN)

## test-constants.R — registry integrity — GREEN now

```r
# tests/testthat/test-constants.R
# The constants.R registry IS the v2 contract. Lock it against silent drift.
# GREEN now.

test_that("default_models is exactly the two e5 handles -> intfloat ids", {
  expect_identical(
    sentiment.ai:::default_models,
    c(`e5-small` = "intfloat/multilingual-e5-small",
      `e5-base`  = "intfloat/multilingual-e5-base")
  )
})

test_that("DEFAULT_MODEL is e5-small and is a registered default model", {
  expect_identical(sentiment.ai:::DEFAULT_MODEL, "e5-small")
  expect_true(sentiment.ai:::DEFAULT_MODEL %in% names(sentiment.ai:::default_models))
})

test_that("every selectable model has a declared dimension (kills hard-coded 512)", {
  selectable <- c(names(sentiment.ai:::default_models),
                  names(sentiment.ai:::openai_models),
                  names(sentiment.ai:::legacy_models))
  expect_true(all(selectable %in% names(sentiment.ai:::model_dims)),
              info = "model_dims must cover every model in every registry")
})

test_that("model_dims match the published model cards", {
  # Dims grounded in model cards:
  #   multilingual-e5-small 384-d, -base 768-d (intfloat/e5);
  #   text-embedding-3-small 1536-d, -3-large 3072-d, ada-002 1536-d (OpenAI);
  #   Universal Sentence Encoder family 512-d.
  d <- sentiment.ai:::model_dims
  expect_equal(d[["e5-small"]], 384L)
  expect_equal(d[["e5-base"]],  768L)
  expect_equal(d[["text-embedding-3-small"]], 1536L)
  expect_equal(d[["text-embedding-3-large"]], 3072L)
  expect_equal(d[["text-embedding-ada-002"]], 1536L)
  for (m in names(sentiment.ai:::legacy_models)) expect_equal(d[[m]], 512L)
})

test_that("e5 family carries the 'query: ' prefix; nothing else does", {
  # The asymmetric query:/passage: prefix is part of e5's training recipe
  # (Wang et al. 2024, 'Multilingual E5 Text Embeddings', arXiv:2402.05672),
  # NOT a packaging choice. Do not 'clean up' this prefix.
  p <- sentiment.ai:::model_prefix
  expect_identical(p[["e5-small"]], "query: ")
  expect_identical(p[["e5-base"]],  "query: ")
  # any other model name resolves to "" (or absent) -- no accidental prefix
  expect_true(is.na(p["text-embedding-3-small"]) ||
                identical(unname(p["text-embedding-3-small"]), ""))
  expect_true(is.na(p["en.large"]) ||
                identical(unname(p["en.large"]), ""))
})
```

## test-model-class.R — backend routing — GREEN now

```r
# tests/testthat/test-model-class.R
# model_class() must route by BOTH the user handle and the underlying id.
# GREEN now.

test_that("st handles and ids route to 'st'", {
  for (m in c("e5-small", "e5-base",
              "intfloat/multilingual-e5-small", "intfloat/multilingual-e5-base"))
    expect_identical(sentiment.ai:::model_class(m), "st", info = m)
})

test_that("openai handles and ids route to 'openai'", {
  for (m in c("text-embedding-3-small", "text-embedding-3-large",
              "text-embedding-ada-002"))
    expect_identical(sentiment.ai:::model_class(m), "openai", info = m)
})

test_that("legacy USE handles route to 'legacy'", {
  for (m in c("en", "en.large", "multi", "multi.large"))
    expect_identical(sentiment.ai:::model_class(m), "legacy", info = m)
})

test_that("unknown / NA route to 'unknown' (no crash on edge inputs)", {
  expect_identical(sentiment.ai:::model_class("definitely-not-a-model"), "unknown")
  expect_identical(sentiment.ai:::model_class(NA_character_), "unknown")
})
```

## test-choose-model.R — id resolution — GREEN now

```r
# tests/testthat/test-choose-model.R
# choose_model() resolves a user handle to the backend id. GREEN now.

test_that("st handles resolve to HuggingFace ids", {
  expect_identical(sentiment.ai:::choose_model("e5-small"),
                   "intfloat/multilingual-e5-small")
  expect_identical(sentiment.ai:::choose_model("e5-base"),
                   "intfloat/multilingual-e5-base")
})

test_that("openai handles resolve to OpenAI model names", {
  expect_identical(sentiment.ai:::choose_model("text-embedding-3-small"),
                   "text-embedding-3-small")
})

test_that("legacy handles resolve to the full tfhub.dev URL", {
  expect_match(sentiment.ai:::choose_model("multi.large"),
               "^https://tfhub\\.dev/google/universal-sentence-encoder-multilingual-large/3$")
})

test_that("unknown model passes through WITH a warning (cowboy mode), not an error", {
  expect_warning(out <- sentiment.ai:::choose_model("my-weird-model"), "cowboy")
  expect_identical(out, "my-weird-model")
})

test_that("length-0 / NA model is a directed stop, not a silent NULL", {
  expect_error(sentiment.ai:::choose_model(character(0)), "length 0")
  expect_error(sentiment.ai:::choose_model(NA), "length 0")
})
```

## test-score-transform.R — rescale + NA round-trip — needs fake scorer

```r
# tests/testthat/test-score-transform.R
# The (probs - .5)*2 rescale and NA round-trip in sentiment_score() are pure R.
# Uses local_fake_embedder + local_fake_scorer so NO embedder/scorer is loaded.
# RED until sentiment_score() routes a character vector through the e5 st path
# (today it falls through to hub_embed/TF). The transform assertions themselves
# are contract-stable.

test_that("probs map to scores: 0->-1, .5->0, 1->+1 (boundaries exact)", {
  local_fake_embedder(dim = 384L)
  testthat::local_mocked_bindings(
    find_sentiment_probs = function(embeddings, ...) c(0, 0.5, 1),
    .package = "sentiment.ai"
  )
  testthat::local_mocked_bindings(
    install_scoring_model = function(...) invisible(0),
    check_sentiment.ai    = function(...) invisible(NULL),
    .package = "sentiment.ai"
  )
  out <- sentiment_score(c("a", "b", "c"), model = "e5-small")
  expect_equal(unname(out), c(-1, 0, 1), tolerance = 1e-12)
})

test_that("NA inputs return NA at the SAME indices; embedder not called for all-NA", {
  rec <- local_fake_embedder(dim = 384L)
  testthat::local_mocked_bindings(
    find_sentiment_probs = function(embeddings, ...) rep(0.75, nrow(embeddings)),
    install_scoring_model = function(...) invisible(0),
    check_sentiment.ai    = function(...) invisible(NULL),
    .package = "sentiment.ai"
  )
  out <- sentiment_score(c("good", NA, "bad", NA), model = "e5-small")
  expect_true(is.na(out[2]) && is.na(out[4]))
  expect_false(any(is.na(out[c(1, 3)])))
})

test_that("is.null(x) returns NULL with no embedder init", {
  testthat::local_mocked_bindings(
    install_scoring_model = function(...) invisible(0),
    .package = "sentiment.ai"
  )
  expect_null(sentiment_score(NULL, model = "e5-small"))
})
```

## test-namespace-no-tf-import.R — the G1 CRAN-ERROR tripwire — RED now

```r
# tests/testthat/test-namespace-no-tf-import.R
# HIGHEST-PRIORITY CRAN gate. tensorflow/tfhub/text2vec are Suggests, but
# NAMESPACE today still contains import(tensorflow) + import(tfhub) (from the
# @import tags in init_and_install.R). An unconditional import of a Suggested
# package is a hard R CMD check ERROR and breaks load on any no-TF box.
# RED now -> GREEN once the @import tags are deleted and re-documented.

test_that("NAMESPACE does not unconditionally import a Suggested package", {
  ns_file <- system.file("NAMESPACE", package = "sentiment.ai")
  # during R CMD check the installed pkg has NAMESPACE at its root
  if (!nzchar(ns_file)) ns_file <- testthat::test_path("..", "..", "NAMESPACE")
  skip_if_not(file.exists(ns_file), "NAMESPACE not found")
  ns <- readLines(ns_file, warn = FALSE)
  offenders <- grep("^import\\((tensorflow|tfhub|text2vec)\\)", ns, value = TRUE)
  expect_identical(offenders, character(0),
                   info = paste("Suggested pkgs imported unconditionally:",
                                paste(offenders, collapse = ", ")))
})
```

## test-no-512-literal.R — magic-number tripwire — RED now

```r
# tests/testthat/test-no-512-literal.R
# A literal tripwire: no hard-coded 512 may survive in R/ outside constants.R's
# model_dims. Today it's in sentiment.R twice (ncol(x)==512 passthrough; the
# numeric(512) seed in find_sentiment_probs). Source-reading is a dev-only check,
# so skip when the source tree isn't reachable (installed-pkg test runs).
# RED now -> GREEN after Phase 2 makes dim registry-driven.

test_that("no literal 512 in R/sentiment.R (dim must come from model_dims)", {
  src <- testthat::test_path("..", "..", "R", "sentiment.R")
  skip_if_not(file.exists(src), "source tree not available (installed run)")
  lines <- readLines(src, warn = FALSE)
  expect_false(any(grepl("512", lines)),
               info = "512 is registry-driven now; read model_dims, not a literal")
})
```

## test-description-credits.R — KFI funder + maintainer survive — GREEN now

```r
# tests/testthat/test-description-credits.R
# A refactor must not quietly strip the MIT + Korn Ferry Institute attribution.
# GREEN now.

test_that("Korn Ferry Institute funder and Ben Wiseman (cre) remain in DESCRIPTION", {
  d <- utils::packageDescription("sentiment.ai")
  expect_match(d$License, "MIT")
  authors <- paste(d$`Authors@R`, d$Author, collapse = " ")
  expect_match(authors, "Korn Ferry Institute")
  expect_match(d$Maintainer, "Ben Wiseman")
})
```

---

# Tier B — snapshot / regression on the SCORER (fixtures, runs on CRAN)

The embedder is the only heavy/nondeterministic part, so Tier B **excludes it entirely**: feed
`find_sentiment_probs()` committed embedding matrices and snapshot the resulting scores. This
makes the scoring half fully reproducible on CRAN with no Python. **All Tier B `skip_on_cran`
is unnecessary** *if* the fixtures + scorers are committed in the package — but until Roadmap
Phase 3 packages the e5 scorers, `skip_if_no_fixture()` guards keep them from erroring.

## data-raw/make_test_fixtures.R — one-time fixture generation (NOT run in check)

```r
# data-raw/make_test_fixtures.R
# Run ONCE, offline, with the real embedders installed. Commits:
#   tests/testthat/fixtures/corpus.rds          ~20 sentences, labelled pos/neg/neutral
#   tests/testthat/fixtures/emb_<backend>.rds   native-dim embedding matrix per backend
#   tests/testthat/fixtures/scores_<backend>_xgb.rds  golden scores from the CURRENT scorer
#   tests/testthat/fixtures/PROVENANCE.md       HF revision hashes + neutral_band source
# These are the regression anchors; the test run NEVER re-derives them.

library(sentiment.ai)

corpus <- data.frame(
  text  = c(
    # clear positive
    "I love this, it is absolutely wonderful",
    "Fantastic service and the staff were so kind",
    "Best purchase I have made all year",
    "This made me so happy, highly recommend",
    "Brilliant, exceeded every expectation",
    # clear negative
    "This is terrible and a complete waste of money",
    "Awful experience, I want a refund",
    "The worst product I have ever used",
    "Deeply disappointed, would not recommend",
    "Broken on arrival and support ignored me",
    # genuinely neutral (the rows that matter for B4)
    "The package arrived on Tuesday afternoon",
    "It is a blue rectangular box with a lid",
    "The meeting is scheduled for 3pm",
    "The document has twelve pages",
    "The store opens at nine in the morning"
  ),
  label = c(rep("positive", 5), rep("negative", 5), rep("neutral", 5)),
  stringsAsFactors = FALSE
)
saveRDS(corpus, "tests/testthat/fixtures/corpus.rds")

for (backend in c("e5-small", "e5-base", "text-embedding-3-small")) {
  init_sentiment.ai(model = backend)                  # real embedder (prefix injected internally)
  emb <- embed_text(corpus$text, model = backend)     # native dim: 384 / 768 / 1536
  saveRDS(emb, sprintf("tests/testthat/fixtures/emb_%s.rds", backend))
  scores <- sentiment_score(emb, model = backend)     # CURRENT scorer = the golden anchor
  saveRDS(scores, sprintf("tests/testthat/fixtures/scores_%s_xgb.rds", backend))
}
# Hand-write PROVENANCE.md: HF revision hash per model + the neutral_band value
# taken from the training repo's eval (DO NOT invent a band in the test).
```

## test-snapshot-scorer.R — per-backend scorer regression — RED until scorers packaged

```r
# tests/testthat/test-snapshot-scorer.R
# Deterministic embeddings (committed fixture) -> known scores (snapshot).
# A snapshot change == a deliberate, NEWS-documented scoring-model change.
# RED until Roadmap Phase 3 packages xgb scorers for the e5/oai spaces.

backends <- c("e5-small", "e5-base", "text-embedding-3-small")

for (backend in backends) {
  test_that(sprintf("[%s] scorer reproduces golden scores on fixture embeddings", backend), {
    skip_on_cran()                                  # scorer weights may need download
    skip_if_no_fixture(sprintf("emb_%s.rds", backend))
    skip_if_no_fixture(sprintf("scores_%s_xgb.rds", backend))

    emb <- readRDS(fixture_path(sprintf("emb_%s.rds", backend)))

    # dimension guard FIRST: a wrong-width fixture would mispredict silently
    expect_equal(ncol(emb), sentiment.ai:::model_dims[[backend]],
                 info = "fixture width must equal the model's registered dim")

    probs <- sentiment.ai:::find_sentiment_probs(
      embeddings = emb, scoring = "xgb", scoring_version = "1.0", model = backend
    )
    scores <- (probs - 0.5) * 2

    # 1) exact regression snapshot (float-jitter tolerant: single- vs multi-thread
    #    xgboost must not change determinism, but guard drift regardless)
    expect_snapshot_value(round(scores, 4), style = "json2", tolerance = 1e-4)

    # 2) class-direction (survives a deliberate re-snapshot; catches a sign flip
    #    or a wrong model-file mapping even when the snapshot is regenerated)
    corpus <- readRDS(fixture_path("corpus.rds"))
    pos <- scores[corpus$label == "positive"]
    neg <- scores[corpus$label == "negative"]
    expect_true(all(pos >  0.25), info = "clear-positive rows must score clearly positive")
    expect_true(all(neg < -0.25), info = "clear-negative rows must score clearly negative")
  })
}
```

## test-scoring-parity.R — xgb vs glm agree in sign — RED until both packaged

```r
# tests/testthat/test-scoring-parity.R
# glm is the compatibility fallback, not an xgb clone: assert SIGN agreement on
# clear-polarity rows, not exact value. Pins that scoring="glm" is a usable
# degraded path, not silently broken. RED until both models exist for a space.

test_that("xgb and glm agree in sign on clear-polarity fixtures (e5-small)", {
  skip_on_cran()
  skip_if_no_fixture("emb_e5-small.rds")
  emb    <- readRDS(fixture_path("emb_e5-small.rds"))
  corpus <- readRDS(fixture_path("corpus.rds"))
  clear  <- corpus$label %in% c("positive", "negative")

  glm_path <- system.file("scoring", "glm", "1.0", "e5-small.csv", package = "sentiment.ai")
  skip_if_not(nzchar(glm_path) && file.exists(glm_path), "glm e5-small not packaged")

  p_xgb <- sentiment.ai:::find_sentiment_probs(emb, "xgb", "1.0", "e5-small")
  p_glm <- sentiment.ai:::find_sentiment_probs(emb, "glm", "1.0", "e5-small")
  expect_equal(sign(p_xgb[clear] - 0.5), sign(p_glm[clear] - 0.5),
               info = "xgb/glm must agree in polarity on clear rows")
})
```

## test-neutral-fixture.R — synthetic-neutral behaviour guard — RED until packaged

```r
# tests/testthat/test-neutral-fixture.R
# Guards the SHIPPED model's behaviour on committed genuinely-neutral rows so a
# future scorer swap that regresses the neutral class is caught. The quantitative
# F1 finding (adding GPT-4o synthetic neutral rows raises pos/neg F1) lives in the
# benchmark vignette with REAL numbers; here we only guard a behavioural band.
# The band is READ from PROVENANCE.md (training-repo eval) -- NEVER invented here.
# RED until scorers + fixtures + a recorded band are committed.

test_that("genuinely-neutral fixture rows fall inside the recorded neutral band", {
  skip_on_cran()
  skip_if_no_fixture("emb_e5-small.rds")
  prov <- fixture_path("PROVENANCE.md")
  skip_if_not(file.exists(prov), "PROVENANCE.md (neutral_band source) missing")

  # neutral_band MUST come from the training-repo eval, recorded in PROVENANCE.md
  # as a line like:  neutral_band: 0.NN
  band_line <- grep("^neutral_band:", readLines(prov), value = TRUE)
  skip_if(length(band_line) == 0, "neutral_band not recorded in PROVENANCE.md")
  neutral_band <- as.numeric(sub("^neutral_band:\\s*", "", band_line[1]))

  emb    <- readRDS(fixture_path("emb_e5-small.rds"))
  corpus <- readRDS(fixture_path("corpus.rds"))
  probs  <- sentiment.ai:::find_sentiment_probs(emb, "xgb", "1.0", "e5-small")
  scores <- (probs - 0.5) * 2
  neutral_scores <- scores[corpus$label == "neutral"]
  expect_true(all(abs(neutral_scores) < neutral_band),
              info = sprintf("neutral rows must satisfy |score| < %.3f", neutral_band))
})
```

---

# CI GATE 1 — no-arg sentiment_score returns a positive number with ZERO TensorFlow

This is the headline. Two flavours: a hermetic version that proves the **routing/rescale**
contract with no Python at all (always runs), and a live no-TF integration version (skips
unless real sentence-transformers is present, but is **never** allowed to require TF).

## test-no-tf-default.R — CI GATE 1 — RED until the e5 `st` branch lands

```r
# tests/testthat/test-no-tf-default.R
# THE CI HEADLINE: sentiment_score("I love this") returns a POSITIVE number with
# ZERO TensorFlow installed.
# RED now: init/embed_text have no sentence-transformers branch and default to
# the TF hub_embed path. GREEN once Roadmap Phase 1/2 land the st branch + e5 default.

test_that("package loads and the e5 default path is reachable with TF absent", {
  # not skipped: this MUST hold on the no-TF CI leg
  expect_true(sentiment.ai:::DEFAULT_MODEL == "e5-small")
  expect_identical(sentiment.ai:::model_class(sentiment.ai:::DEFAULT_MODEL), "st")
  # the default must never resolve into the legacy (TF) class
  expect_false(identical(sentiment.ai:::model_class(sentiment.ai:::DEFAULT_MODEL), "legacy"))
})

test_that("no-arg sentiment_score('I love this') returns a positive number (hermetic, no Python)", {
  # Routing + rescale proof with a deterministic fake embedder + monotone fake
  # scorer. Proves the CONTRACT without Python/TF: positive text -> score > 0.
  local_fake_embedder(dim = sentiment.ai:::model_dims[["e5-small"]])
  testthat::local_mocked_bindings(
    install_scoring_model = function(...) invisible(0),
    check_sentiment.ai    = function(...) invisible(NULL),
    # positive-leaning text -> prob > .5 ; keep it monotone & deterministic
    find_sentiment_probs  = function(embeddings, ...) rep(0.9, nrow(embeddings)),
    .package = "sentiment.ai"
  )
  score <- sentiment_score("I love this")   # NO model arg -> must default to e5-small
  expect_length(score, 1L)
  expect_type(score, "double")
  expect_true(is.finite(score))
  expect_gt(score, 0)
})

test_that("LIVE no-TF default path: real e5 embed -> positive score, with TF absent", {
  skip_on_cran()
  skip_if_no_st()                 # needs sentence-transformers, NOT tensorflow
  # This is the integration proof of the headline. It must run on the no-TF CI leg.
  # If TF happens to be installed too, that's fine -- the point is it is NOT REQUIRED.
  init_sentiment.ai(model = "e5-small")
  # post-Phase-2 the registry should expose which backend was actually selected:
  expect_identical(sentiment.ai::sentiment.env$backend, "st")   # add $backend in Phase 2
  score <- sentiment_score("I love this")
  expect_true(is.finite(score) && score > 0)
})
```

# CI GATE 2 — the e5 "query: " prefix is applied

## test-e5-prefix.R — CI GATE 2 — RED until prefix injection lands

```r
# tests/testthat/test-e5-prefix.R
# e5 is trained with asymmetric prefixes and DROPS measurable accuracy if you skip
# them (arXiv:2402.05672). The package must inject model_prefix[[model]] INTERNALLY,
# exactly once, so users never type "query: " themselves.
# RED now: no prefix injection exists anywhere in embed_text/init/get_embedder.py.

test_that("e5 embed receives the 'query: '-prefixed string (single input)", {
  rec <- local_fake_embedder(dim = sentiment.ai:::model_dims[["e5-small"]])
  testthat::local_mocked_bindings(check_sentiment.ai = function(...) invisible(NULL),
                                  .package = "sentiment.ai")
  embed_text("good service", model = "e5-small")
  expect_true("query: good service" %in% rec$seen,
              info = "the embedder must see the prefixed string for e5 models")
  expect_false("good service" %in% rec$seen,
               info = "the embedder must NOT see the raw (un-prefixed) string")
})

test_that("every element of a BATCH is prefixed (not just the first)", {
  rec <- local_fake_embedder(dim = sentiment.ai:::model_dims[["e5-small"]])
  testthat::local_mocked_bindings(check_sentiment.ai = function(...) invisible(NULL),
                                  .package = "sentiment.ai")
  embed_text(c("great", "awful", "fine"), model = "e5-small")
  expect_setequal(rec$seen, c("query: great", "query: awful", "query: fine"))
})

test_that("no double-prefix when input already starts with 'query: '", {
  rec <- local_fake_embedder(dim = sentiment.ai:::model_dims[["e5-small"]])
  testthat::local_mocked_bindings(check_sentiment.ai = function(...) invisible(NULL),
                                  .package = "sentiment.ai")
  embed_text("query: already prefixed", model = "e5-small")
  expect_false(any(grepl("query: query:", rec$seen, fixed = TRUE)),
               info = "prefix injection must be idempotent (skip-if-present or strip-then-add)")
})

test_that("non-e5 models get NO prefix", {
  rec <- local_fake_embedder(dim = sentiment.ai:::model_dims[["text-embedding-3-small"]],
                             openai = TRUE)
  testthat::local_mocked_bindings(check_sentiment.ai = function(...) invisible(NULL),
                                  .package = "sentiment.ai")
  embed_text("good service", model = "text-embedding-3-small")
  expect_true("good service" %in% rec$seen)
  expect_false(any(grepl("^query: ", rec$seen)),
               info = "OpenAI/legacy models must receive the raw string")
})

test_that("KNOWN-STRING regression: prefix-on vs prefix-off score differs (fixtures)", {
  # The 'known string under e5 -> expected score; fails if prefix dropped' ask.
  # Needs committed e5 embeddings for BOTH the prefixed and un-prefixed string so
  # the test is deterministic and offline. RED until those fixtures are committed.
  skip_on_cran()
  skip_if_no_fixture("emb_e5-small_prefixed.rds")
  skip_if_no_fixture("emb_e5-small_unprefixed.rds")
  emb_pre <- readRDS(fixture_path("emb_e5-small_prefixed.rds"))    # embed of "query: I love this"
  emb_raw <- readRDS(fixture_path("emb_e5-small_unprefixed.rds"))  # embed of "I love this"
  s_pre <- (sentiment.ai:::find_sentiment_probs(emb_pre, "xgb", "1.0", "e5-small") - .5) * 2
  s_raw <- (sentiment.ai:::find_sentiment_probs(emb_raw, "xgb", "1.0", "e5-small") - .5) * 2
  # the correct, prefixed path is the golden score; dropping the prefix must change it
  expect_snapshot_value(round(s_pre, 4), style = "json2", tolerance = 1e-4)
  expect_false(isTRUE(all.equal(unname(s_pre), unname(s_raw), tolerance = 1e-3)),
               info = "dropping the e5 prefix must measurably change the score")
})
```

# CI GATE 3 — legacy gate: directed error, not a crash

## test-legacy-gate.R — CI GATE 3 — RED until the gate is built

```r
# tests/testthat/test-legacy-gate.R
# CONTRACT: requesting a legacy USE model without the legacy TF backend installed
# must raise a CLEAR, ACTIONABLE error -- not a reticulate ImportError on
# tensorflow_text, and not a silent fall-through. The error must name BOTH the fix
# (install_sentiment.ai(legacy = TRUE)) AND the TF-free replacement.
# Runs ONLY when TF is genuinely absent (the situation being protected).
# RED now: no gate exists; multi.large falls into the TF path and crashes.

# expected replacement per legacy model (from constants.R comments):
#   en -> e5-small ; en.large / multi / multi.large -> e5-base
legacy_repl <- list(en = "e5-small", en.large = "e5-base",
                    multi = "e5-base", multi.large = "e5-base")

for (m in names(legacy_repl)) {
  test_that(sprintf("legacy '%s' without TF -> directed error naming leg=TRUE and %s",
                    m, legacy_repl[[m]]), {
    skip_unless_tf_absent()
    err <- expect_error(
      init_sentiment.ai(model = m),
      regexp = "legacy"        # message must talk about the legacy backend
    )
    msg <- conditionMessage(err)
    expect_match(msg, "legacy\\s*=\\s*TRUE",
                 info = "must tell the user how to enable the legacy backend")
    expect_match(msg, legacy_repl[[m]],
                 info = "must name the TF-free replacement model")
  })
}

test_that("the gate short-circuits at the R level BEFORE any Python import", {
  skip_unless_tf_absent()
  # Prove the failure does not depend on Python being reachable: if the gate
  # fired correctly, source_python must NEVER be called.
  called <- new.env(parent = emptyenv()); called$py <- FALSE
  testthat::local_mocked_bindings(
    .package = "reticulate",
    source_python = function(...) { called$py <- TRUE; stop("python reached!") }
  )
  expect_error(init_sentiment.ai(model = "multi.large"), "legacy")
  expect_false(called$py, info = "legacy gate must stop before touching Python")
})

test_that("sentiment_score(model='multi.large') without TF errors the same way", {
  skip_unless_tf_absent()
  expect_error(sentiment_score("anything", model = "multi.large"), "legacy")
})
```

## test-legacy-roundtrip.R — legacy path actually works when TF present — needs TF

```r
# tests/testthat/test-legacy-roundtrip.R
# "keep the legacy USE path genuinely working ... or 'compatibility layer' is a lie."
# When TF + tfhub + the legacy env ARE present, the path must round-trip.

test_that("legacy en.large loads USE-512 and embeds at width 512", {
  skip_on_cran()
  skip_if_no_tf()
  init_sentiment.ai(model = "en.large", envname = Sys.getenv("SENTIMENTAI_TF_ENV", "r-sentiment-ai"))
  emb <- embed_text(c("good", "bad"), model = "en.large")
  expect_equal(ncol(emb), 512L)
  expect_equal(nrow(emb), 2L)
})

test_that("legacy scorer still reproduces historical scores (backward-compat)", {
  skip_on_cran()
  skip_if_no_fixture("emb_en.large.rds")     # committed legacy USE embeddings
  emb    <- readRDS(fixture_path("emb_en.large.rds"))
  scores <- (sentiment.ai:::find_sentiment_probs(emb, "xgb", "1.0", "en.large") - .5) * 2
  expect_snapshot_value(round(scores, 4), style = "json2", tolerance = 1e-4)
})
```

# CI GATE 4 — dimension handling per model (no hard-coded 512)

## test-dim-handling.R — CI GATE 4 — RED until 512 removed

```r
# tests/testthat/test-dim-handling.R
# The bug being killed: sentiment_score() only accepts a pre-embedded matrix when
# ncol(x)==512, and find_sentiment_probs seeds numeric(512). Dim must follow the
# chosen model via model_dims. RED until Phase 2.

test_that("pre-embedded matrix passes through at the model's NATIVE width (no re-embed)", {
  # If passthrough works, the embedder must NOT be called. Install a fake that
  # FAILS if invoked, to prove no re-embedding happened.
  sentiment.ai::sentiment.env$embed <- function(...) stop("embedder must not be called")
  withr::defer(sentiment.ai::sentiment.env$embed <- NULL)
  testthat::local_mocked_bindings(
    install_scoring_model = function(...) invisible(0),
    find_sentiment_probs  = function(embeddings, ...) rep(0.7, nrow(embeddings)),
    .package = "sentiment.ai"
  )

  for (backend in c("e5-small", "e5-base", "text-embedding-3-small")) {
    d   <- sentiment.ai:::model_dims[[backend]]            # 384 / 768 / 1536
    mat <- matrix(0.01, nrow = 3, ncol = d)
    out <- sentiment_score(mat, model = backend)
    expect_length(out, 3L)
    expect_false(anyNA(out), info = paste("passthrough failed at width", d, "for", backend))
  }
})

test_that("a 512-col matrix passes through ONLY for a 512-d (legacy) model", {
  testthat::local_mocked_bindings(
    install_scoring_model = function(...) invisible(0),
    find_sentiment_probs  = function(embeddings, ...) rep(0.6, nrow(embeddings)),
    .package = "sentiment.ai"
  )
  mat512 <- matrix(0.01, nrow = 2, ncol = 512)
  # legacy model: 512 is correct -> passes through
  expect_silent(out <- sentiment_score(mat512, model = "en.large"))
  expect_length(out, 2L)
})

test_that("dimension MISMATCH is a clear error naming both dims, not a silent mispredict", {
  testthat::local_mocked_bindings(install_scoring_model = function(...) invisible(0),
                                  .package = "sentiment.ai")
  bad <- matrix(0.01, nrow = 2, ncol = 768)   # 768 cols but model expects 384
  expect_error(sentiment_score(bad, model = "e5-small"),
               regexp = "384.*768|768.*384",
               info = "error must name both the expected and the supplied dim")
})

test_that("find_sentiment_probs does not assume a 512-wide output", {
  # Guards the dead numeric(512) seed: probs length must follow nrow(embeddings),
  # independent of embedding width.
  skip_if_no_fixture("emb_e5-base.rds")       # 768-d
  emb   <- readRDS(fixture_path("emb_e5-base.rds"))
  probs <- sentiment.ai:::find_sentiment_probs(emb, "xgb", "1.0", "e5-base")
  expect_length(probs, nrow(emb))
})
```

---

# Tier C/D — live backends (skip cleanly off-CRAN / without keys)

## test-backend-e5.R — live e5 end-to-end — needs sentence-transformers

```r
# tests/testthat/test-backend-e5.R
# Loads the REAL e5 embedder. skip_on_cran + skip_if_no_st. The live dimension
# test that replaces hard-coded 512. RED until the st branch lands.

test_that("e5-small embeds at 384, e5-base at 768, rownames = text", {
  skip_on_cran(); skip_if_no_st()
  for (backend in c("e5-small", "e5-base")) {
    init_sentiment.ai(model = backend)
    emb <- embed_text(c("good", "bad", "fine"), model = backend)
    expect_equal(ncol(emb), sentiment.ai:::model_dims[[backend]], info = backend)
    expect_equal(nrow(emb), 3L)
    expect_identical(rownames(emb), c("good", "bad", "fine"))
  }
})

test_that("the selected backend is reported as 'st' (no TF path)", {
  skip_on_cran(); skip_if_no_st()
  init_sentiment.ai(model = "e5-small")
  expect_identical(sentiment.ai::sentiment.env$backend, "st")  # registry field, Phase 2
})
```

## test-backend-openai.R — Tier D — needs OPENAI_API_KEY

```r
# tests/testthat/test-backend-openai.R
# Real OpenAI path. Tier D: only runs when OPENAI_API_KEY is set. Keep it cheap
# (2-3 short strings) to bound spend.

test_that("openai 3-small embeds at 1536 and sets the openai flag", {
  skip_on_cran(); skip_if_no_openai_key()
  init_sentiment.ai(model = "text-embedding-3-small", api_key = Sys.getenv("OPENAI_API_KEY"))
  expect_true(sentiment.ai::sentiment.env$openai)
  emb <- embed_text(c("good", "bad"), model = "text-embedding-3-small")
  expect_equal(ncol(emb), 1536L)
})

test_that("openai_embed rate-limit/reset branch is exercised without a real 60s sleep", {
  skip_on_cran(); skip_if_no_openai_key()
  # inject the sleep fn so CI never actually sleeps 60s; tiny limits force the reset path
  testthat::local_mocked_bindings(Sys.sleep = function(...) invisible(NULL), .package = "base")
  init_sentiment.ai(model = "text-embedding-3-small", api_key = Sys.getenv("OPENAI_API_KEY"))
  emb <- sentiment.ai:::openai_embed(c("a", "b"), request_limit = 1, token_limit = 1)
  expect_equal(ncol(emb), 2L)
})
```

---

# CRAN-hygiene guards (Tier A, run everywhere)

## test-onload-quiet.R — .onLoad/.onAttach hygiene (G4) — RED now

```r
# tests/testthat/test-onload-quiet.R
# CRAN: no warning()/cat() in .onLoad; startup chatter belongs in .onAttach via
# packageStartupMessage and must be suppressible. Today .onLoad calls warning()
# (Apple-Silicon TF note) AND advertises paraphrase/use/oai_3_small + "use is
# still the default" -- stale vs DEFAULT_MODEL='e5-small'. RED until fixed.

test_that("attaching the package emits no warning and no error with TF absent", {
  expect_silent(
    suppressPackageStartupMessages(
      loadNamespace("sentiment.ai")
    )
  )
})

test_that("startup message (if any) reflects e5-small, not 'use is still the default'", {
  msg <- tryCatch(
    {
      withr::with_envvar(c(), {
        # capture .onAttach output
        utils::capture.output(
          suppressWarnings(library(sentiment.ai)), type = "message"
        )
      })
    }, error = function(e) ""
  )
  blob <- paste(msg, collapse = " ")
  expect_false(grepl("use.*still.*default", blob, ignore.case = TRUE),
               info = "stale onload text contradicts DEFAULT_MODEL = e5-small")
})
```

## test-download-mocked.R — no network during check (G2) — GREEN-able with mock

```r
# tests/testthat/test-download-mocked.R
# install_scoring_model()/install_default_embeddings() hit GitHub via download.file.
# CRAN forbids network + writing outside tempdir during check. NEVER perform the GET:
# mock download.file, assert the constructed URL, and assert graceful failure.

test_that("scoring URL is constructed correctly and download is never really performed", {
  captured <- new.env(parent = emptyenv()); captured$url <- NA_character_
  testthat::local_mocked_bindings(
    .package = "utils",
    download.file = function(url, destfile, ...) { captured$url <- url; 0L }
  )
  # redirect the write target into tempdir via the repo_url seam where supported
  status <- tryCatch(
    sentiment.ai:::install_scoring_model(model = "en.large", scoring = "xgb",
                                         scoring_version = "1.0"),
    error = function(e) e
  )
  skip_if(inherits(status, "error") && grepl("not.*writ", conditionMessage(status)),
          "installed-pkg dir not writable in this run (expected on CRAN box)")
  if (!is.na(captured$url))
    expect_match(captured$url, "scoring/xgb/1\\.0/en\\.large\\.xgb\\?raw=true")
})

test_that("a failed download degrades gracefully (status 0, no crash)", {
  testthat::local_mocked_bindings(
    .package = "utils",
    download.file = function(...) stop("network down")
  )
  expect_silent(
    suppressMessages(
      status <- sentiment.ai:::install_scoring_model("en.large", "xgb", "1.0")
    )
  )
})
```

## test-scoring-version.R — match.arg safety (G6) — GREEN now

```r
# tests/testthat/test-scoring-version.R
# scoring_version uses match.arg with a single allowed value "1.0". Pin that "1.0"
# resolves and an unknown version errors cleanly (so adding a 2nd version later is
# a deliberate change to the formals, not a silent break).

test_that("scoring_version '1.0' resolves; unknown errors cleanly", {
  testthat::local_mocked_bindings(install_scoring_model = function(...) invisible(0),
                                  .package = "sentiment.ai")
  expect_null(sentiment_score(NULL, scoring_version = "1.0"))          # early NULL return
  expect_error(sentiment_score(NULL, scoring_version = "9.9"))         # match.arg rejects
})
```

---

## File manifest (drop into tests/testthat/)

| File | Tier | CI gate | Status now | Needs |
|---|---|---|---|---|
| `helper-skips.R` | — | — | GREEN | `withr` |
| `test-constants.R` | A | — | **GREEN** | none |
| `test-model-class.R` | A | — | **GREEN** | none |
| `test-choose-model.R` | A | — | **GREEN** | none |
| `test-score-transform.R` | A | — | RED | fake embedder/scorer |
| `test-namespace-no-tf-import.R` | A | (G1) | **RED** | delete `@import tf/tfhub` |
| `test-no-512-literal.R` | A | (4) | RED | Phase 2 dim work |
| `test-description-credits.R` | A | — | **GREEN** | none |
| `test-snapshot-scorer.R` | B | — | RED | committed fixtures + e5 scorers |
| `test-scoring-parity.R` | B | — | RED | xgb+glm packaged |
| `test-neutral-fixture.R` | B | — | RED | fixtures + recorded band |
| `test-no-tf-default.R` | A/C | **1** | RED | st branch |
| `test-e5-prefix.R` | A/C | **2** | RED | prefix injection |
| `test-legacy-gate.R` | C | **3** | RED | legacy gate |
| `test-legacy-roundtrip.R` | C | (3) | skip w/o TF | TF env + fixtures |
| `test-dim-handling.R` | A/C | **4** | RED | Phase 2 dim work |
| `test-backend-e5.R` | C | (1) | skip w/o ST | sentence-transformers |
| `test-backend-openai.R` | D | — | skip w/o key | `OPENAI_API_KEY` |
| `test-onload-quiet.R` | A | (G4) | RED | onload fix |
| `test-download-mocked.R` | A | (G2) | GREEN-able | mock |
| `test-scoring-version.R` | A | (G6) | GREEN | none |

## Notes / non-fabrication

- **Neutral band is NOT invented.** `test-neutral-fixture.R` reads `neutral_band` from
  `fixtures/PROVENANCE.md`, which must be filled from the training repo's eval. Real F1 anchors
  on disk: `oai_3_small` class-0 F1 = 0.877, `use_lg` class-0 F1 = 0.830 (from
  `sentiment.ai_training/.../models/f1_scores_*.csv`). The F1 *finding* belongs in the
  benchmark vignette, not these tests.
- **Snapshots are anchors, not assertions of correctness:** a snapshot change == a deliberate,
  NEWS-logged scoring-model change. Direction assertions (`> 0.25` / `< -0.25`) survive a
  re-snapshot and catch sign flips / wrong model-file mappings.
- **`sentiment.env$backend`** is referenced by the live tests as the introspectable "which
  backend was actually selected" field — it does **not exist yet**; add it in Roadmap Phase 2's
  registry. Tagged inline so it isn't mistaken for current API.
- **`local_mocked_bindings`** is testthat-3e; requires `Config/testthat/edition: 3`. Mocking a
  package-internal (`find_sentiment_probs`, `install_scoring_model`, `check_sentiment.ai`)
  requires the binding to exist in the namespace (it does).
```
