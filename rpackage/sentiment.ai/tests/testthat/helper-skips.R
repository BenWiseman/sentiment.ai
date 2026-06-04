# tests/testthat/helper-skips.R
# Shared skip guards and a deterministic, no-Python embedder stub.
# GREEN now (pure helpers; nothing here asserts contract yet).
#
# helper-*.R files are sourced automatically by testthat before the test files.
# This is the single most important file: the local_fake_embedder() stub is what
# lets the gate tests run with no Python/TF.

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
# situation the legacy gate is supposed to protect. Used to run the gate only
# in that situation.
tf_is_absent <- function() {
  !(requireNamespace("tensorflow", quietly = TRUE) &&
      reticulate::py_available(initialize = FALSE) &&
      reticulate::py_module_available("tensorflow"))
}

skip_unless_tf_absent <- function() {
  testthat::skip_if_not(
    tf_is_absent(),
    "TensorFlow present; legacy-gate test only meaningful when TF absent"
  )
}

skip_if_no_openai_key <- function() {
  testthat::skip_if(
    identical(Sys.getenv("OPENAI_API_KEY"), ""),
    "OPENAI_API_KEY not set"
  )
}

# A committed scorer / embedding fixture must exist for Tier B. Until the e5
# scorers + fixtures are packaged, these skip rather than error.
fixture_path <- function(...) {
  testthat::test_path("fixtures", ...)
}
skip_if_no_fixture <- function(file) {
  testthat::skip_if_not(
    file.exists(fixture_path(file)),
    paste0("fixture missing: ", file)
  )
}

# ---- the deterministic fake embedder ----------------------------------------
# Installs a closure into sentiment.env$embed that (a) RECORDS every string it is
# handed (so we can assert the "query: " prefix was injected) and (b) returns a
# deterministic embedding of the requested dimension. No Python, no network, no TF.
#
# withr::defer(..., envir = parent.frame()) restores the real env on test exit,
# so tests never leak state into each other.

# Resolve the (exported) sentiment.env environment portably. Under an installed
# package `sentiment.ai::sentiment.env` works; under pkgload::load_all() the
# package isn't attached, so reach into the namespace instead. sentiment.env is an
# environment (reference semantics), so the returned object can be mutated in place.
.sentiment_env <- function() {
  get("sentiment.env", envir = asNamespace("sentiment.ai"))
}

local_fake_embedder <- function(dim = 384L, openai = FALSE, env = parent.frame()) {
  senv  <- .sentiment_env()
  calls <- new.env(parent = emptyenv())
  calls$seen <- character(0)

  fake <- function(text) {
    text <- as.character(unlist(text))
    calls$seen <- c(calls$seen, text)
    # deterministic, content-dependent pseudo-embedding (stable across platforms):
    # hash each string to a seed and draw `dim` values.
    emb <- vapply(text, function(s) {
      seed <- sum(utf8ToInt(s)) %% .Machine$integer.max
      set.seed(seed)
      stats::runif(dim, -1, 1)
    }, numeric(dim))
    # sentence-transformers returns rows = texts; mimic that orientation here
    t(emb)
  }

  old_embed    <- senv$embed
  old_openai   <- senv$openai
  old_parallel <- senv$parallel

  senv$embed    <- fake
  senv$openai   <- isTRUE(openai)
  senv$parallel <- 0

  withr::defer({
    senv$embed    <- old_embed
    senv$openai   <- old_openai
    senv$parallel <- old_parallel
  }, envir = env)

  calls   # return the recorder; tests read calls$seen
}

# A trivial deterministic scorer: maps an embedding matrix -> scores in (-1, 1).
# Lets Tier-A tests run without any packaged xgb/glm model. find_sentiment_score
# is the REAL internal (it already returns [-1, 1]); this is ONLY for the pure-R
# routing/NA tests where the scorer's exact values are irrelevant.
local_fake_scorer <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    find_sentiment_score = function(embeddings, scoring, scoring_version, model) {
      # tanh of the row mean -> stable, monotone, in (-1, 1)
      tanh(rowMeans(embeddings))
    },
    .package = "sentiment.ai",
    .env = env
  )
}

# Restore the package sentiment.env after a test that really initialises a backend.
# init_sentiment.ai() sets $embed/$openai/$st with no cleanup of its own, so a test
# that calls it (e.g. the OpenAI Tier-D tests, when a key is present) would otherwise
# leak a live embedder into later tests -- which makes check_sentiment.ai() skip init
# and the legacy gate never fire. Call this BEFORE init in such tests.
reset_sentiment_env <- function(env = parent.frame()) {
  senv <- .sentiment_env()
  old  <- list(embed = senv$embed, openai = senv$openai,
               st = senv$st, prefix = senv$prefix)
  withr::defer({
    senv$embed  <- old$embed
    senv$openai <- old$openai
    senv$st     <- old$st
    senv$prefix <- old$prefix
  }, envir = env)
}

# Note on set.seed inside the fake: the helper does not restore the global RNG;
# each test is hermetic and asserts only on the strings seen and the dimensions,
# never on the embedding values. If a future test cares, wrap in withr::with_seed.
