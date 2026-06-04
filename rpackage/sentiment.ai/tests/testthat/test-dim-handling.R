# tests/testthat/test-dim-handling.R
# CI GATE 4 -- dimension handling per model (no hard-coded 512).
# The bug class being guarded: a pre-embedded matrix must be validated/accepted at
# the chosen model's NATIVE width (model_dims[[model]]: 384 / 768 / 1536 / 512),
# never against a single hard-coded 512. find_sentiment_score's output length must
# follow nrow(embeddings), independent of embedding width.

test_that("pre-embedded matrix passes through at the model's NATIVE width (no re-embed)", {
  # If passthrough works, the embedder must NOT be called. Install a fake that
  # FAILS if invoked, to prove no re-embedding happened.
  senv <- .sentiment_env()
  old_embed <- senv$embed
  senv$embed <- function(...) stop("embedder must not be called")
  withr::defer(senv$embed <- old_embed)
  testthat::local_mocked_bindings(
    install_scoring_model = function(...) invisible(0),
    find_sentiment_score  = function(embeddings, ...) rep(0.5, nrow(embeddings)),
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
    find_sentiment_score  = function(embeddings, ...) rep(0.4, nrow(embeddings)),
    .package = "sentiment.ai"
  )
  mat512 <- matrix(0.01, nrow = 2, ncol = 512)
  # legacy model: 512 is correct -> passes through
  expect_silent(out <- sentiment_score(mat512, model = "en.large"))
  expect_length(out, 2L)
})

test_that("dimension MISMATCH is a clear error naming both dims, not a silent mispredict", {
  testthat::local_mocked_bindings(
    install_scoring_model = function(...) invisible(0),
    .package = "sentiment.ai"
  )
  bad <- matrix(0.01, nrow = 2, ncol = 768)   # 768 cols but model expects 384
  expect_error(sentiment_score(bad, model = "e5-small"),
               regexp = "384.*768|768.*384",
               info = "error must name both the expected and the supplied dim")
})

test_that("find_sentiment_score does not assume a 512-wide output", {
  # Guards the dead numeric(512) seed: output length must follow nrow(embeddings),
  # independent of embedding width.
  skip_if_no_fixture("emb_e5-base.rds")       # 768-d
  emb    <- readRDS(fixture_path("emb_e5-base.rds"))
  scores <- sentiment.ai:::find_sentiment_score(emb, "xgb", "1.0", "e5-base")
  expect_length(scores, nrow(emb))
})
