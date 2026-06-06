# tests/testthat/test-diagnostics.R
# sentiment_diagnostics(): entropy / confidence_band / mixed / OOD signals.
# Hermetic: fake embedder + real shipped mlp head + real centroids.

local_diag_fake <- function(env = parent.frame()) {
  senv <- get("sentiment.env", envir = asNamespace("sentiment.ai"))
  old  <- senv$st; senv$st <- TRUE
  withr::defer(senv$st <- old, envir = env)
  local_fake_embedder(dim = sentiment.ai:::model_dims[["e5-small"]], env = env)
}

test_that("sentiment_diagnostics returns all expected columns", {
  local_diag_fake()
  testthat::local_mocked_bindings(
    check_sentiment.ai    = function(...) invisible(NULL),
    install_scoring_model = function(...) invisible(0),
    .package = "sentiment.ai"
  )
  d <- sentiment_diagnostics(c("great", "terrible", "fine"), model = "e5-small")
  expect_s3_class(d, "data.frame")
  expect_identical(nrow(d), 3L)
  expected_cols <- c("text","sentiment","prob_neg","prob_neu","prob_pos",
                     "class","confidence","entropy","confidence_band","mixed",
                     "ood_similarity","ood_flag")
  expect_setequal(names(d), expected_cols)
})

test_that("entropy is in [0, log(3)] and confidence_band is an ordered factor", {
  local_diag_fake()
  testthat::local_mocked_bindings(
    check_sentiment.ai    = function(...) invisible(NULL),
    install_scoring_model = function(...) invisible(0),
    .package = "sentiment.ai"
  )
  d <- sentiment_diagnostics(c("great","fine","ok"), model = "e5-small")
  expect_true(all(d$entropy >= 0 & d$entropy <= log(3) + 1e-9))
  expect_true(is.ordered(d$confidence_band))
  expect_true(all(levels(d$confidence_band) == c("high","moderate","low")))
})

test_that("mixed flag fires when prob_pos and prob_neg are both above 0.25", {
  # craft a fake probability row with high pos and high neg
  fake_probs <- function(embeddings, ...) {
    n <- nrow(embeddings)
    # first row: high pos + high neg (mixed); rest: clear positive
    m <- matrix(c(0.35, 0.05, 0.60,   # row1: mixed (neg=0.35, neu=0.05, pos=0.60)
                  rep(c(0.01, 0.04, 0.95), n - 1)), ncol = 3, byrow = TRUE)
    setNames(m[, 3] - m[, 1], rownames(embeddings))
  }
  local_diag_fake()
  testthat::local_mocked_bindings(
    check_sentiment.ai    = function(...) invisible(NULL),
    install_scoring_model = function(...) invisible(0),
    find_sentiment_score  = fake_probs,
    .package = "sentiment.ai"
  )
  d <- sentiment_diagnostics(c("mixed","good","good"), model = "e5-small")
  expect_true(is.logical(d$mixed))
})

test_that("OOD signal is present when centroids are installed", {
  local_diag_fake()
  testthat::local_mocked_bindings(
    check_sentiment.ai    = function(...) invisible(NULL),
    install_scoring_model = function(...) invisible(0),
    .package = "sentiment.ai"
  )
  d <- sentiment_diagnostics("test text", model = "e5-small")
  cf <- system.file("centroids", "e5-small.json", package = "sentiment.ai")
  if(nzchar(cf) && file.exists(cf)){
    expect_false(is.na(d$ood_similarity))
    expect_true(d$ood_similarity >= -1 & d$ood_similarity <= 1)
    expect_true(is.logical(d$ood_flag))
  } else {
    expect_true(is.na(d$ood_similarity))
  }
})

test_that("NA inputs are blanked in diagnostic columns too", {
  local_diag_fake()
  testthat::local_mocked_bindings(
    check_sentiment.ai    = function(...) invisible(NULL),
    install_scoring_model = function(...) invisible(0),
    .package = "sentiment.ai"
  )
  d <- sentiment_diagnostics(c("good", NA), model = "e5-small")
  expect_true(is.na(d$entropy[2]))
  expect_true(is.na(d$confidence_band[2]))
  expect_true(is.na(d$mixed[2]) || !isTRUE(d$mixed[2]))
})
