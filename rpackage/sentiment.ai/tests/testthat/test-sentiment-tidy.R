# tests/testthat/test-sentiment-tidy.R
# sentiment(): the 3-class tidy output. Fake embedder + the REAL shipped JSON head
# (random embeddings -> a valid but meaningless probability distribution), so we lock
# the SHAPE and the within-row invariants, not specific values. Pure R, no Python/TF.

local_fake <- function(env = parent.frame()) {
  senv <- get("sentiment.env", envir = asNamespace("sentiment.ai"))
  old <- senv$st; senv$st <- TRUE
  withr::defer(senv$st <- old, envir = env)
  local_fake_embedder(dim = sentiment.ai:::model_dims[["e5-small"]], env = env)
}

test_that("sentiment() returns the tidy 3-class frame with consistent columns", {
  local_fake()
  testthat::local_mocked_bindings(
    check_sentiment.ai    = function(...) invisible(NULL),
    install_scoring_model = function(...) invisible(0),
    .package = "sentiment.ai"
  )
  res <- sentiment(c("great", "fine", "awful"), model = "e5-small")
  expect_s3_class(res, "data.frame")
  expect_identical(
    names(res),
    c("text", "sentiment", "prob_neg", "prob_neu", "prob_pos", "class", "confidence"))
  expect_identical(nrow(res), 3L)

  # the three probabilities form a distribution
  probs <- as.matrix(res[, c("prob_neg", "prob_neu", "prob_pos")])
  expect_true(all(probs >= 0 & probs <= 1))
  expect_equal(unname(rowSums(probs)), rep(1, 3), tolerance = 1e-8)

  # derived columns are internally consistent
  expect_equal(res$sentiment, res$prob_pos - res$prob_neg)
  expect_equal(res$confidence, apply(probs, 1, max))
  expect_true(is.ordered(res$class))
  expect_true(all(as.character(res$class) %in% c("negative", "neutral", "positive")))
})

test_that("sentiment() scalar matches sentiment_score(), and NA rows are blanked", {
  local_fake()
  testthat::local_mocked_bindings(
    check_sentiment.ai    = function(...) invisible(NULL),
    install_scoring_model = function(...) invisible(0),
    .package = "sentiment.ai"
  )
  x    <- c("good", NA, "bad")
  rich <- sentiment(x, model = "e5-small")
  flat <- sentiment_score(x, model = "e5-small")
  # same embeddings + same head -> the scalar agrees with the legacy entry point
  expect_equal(rich$sentiment, as.numeric(flat))
  # the NA row is blanked across all numeric columns, text preserved, order kept
  expect_true(is.na(rich$sentiment[2]) && is.na(rich$class[2]) && is.na(rich$prob_pos[2]))
  expect_identical(rich$text, c("good", NA, "bad"))
})

test_that("sentiment() rejects legacy scalar scorers (probabilities are JSON-head only)", {
  local_fake()
  testthat::local_mocked_bindings(
    check_sentiment.ai    = function(...) invisible(NULL),
    install_scoring_model = function(...) invisible(0),
    .package = "sentiment.ai"
  )
  expect_error(sentiment("x", model = "e5-small", scoring = "xgb"))   # not an mlp/logistic head
})
