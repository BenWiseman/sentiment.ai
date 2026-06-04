# tests/testthat/test-score-transform.R
# sentiment_score() returns the scorer's [-1, 1] output verbatim and threads NA
# back at the original indices -- both pure R. Uses local_fake_embedder + a mocked
# find_sentiment_score (the real internal; it already returns [-1, 1]) so NO
# embedder/scorer is loaded.
#
# Contract note: the real internal is find_sentiment_score(), which returns scores
# already mapped to [-1, 1] (the softprob -> [-1,1] mapping is the scorer's job).
# These tests therefore mock that function with final scores, not probabilities.

test_that("scorer output in [-1, 1] is returned verbatim (boundaries exact)", {
  local_fake_embedder(dim = 384L)
  testthat::local_mocked_bindings(
    find_sentiment_score  = function(embeddings, ...) c(-1, 0, 1),
    install_scoring_model = function(...) invisible(0),
    check_sentiment.ai    = function(...) invisible(NULL),
    .package = "sentiment.ai"
  )
  out <- sentiment_score(c("a", "b", "c"), model = "e5-small")
  expect_equal(unname(out), c(-1, 0, 1), tolerance = 1e-12)
})

test_that("NA inputs return NA at the SAME indices; clear rows are not NA", {
  local_fake_embedder(dim = 384L)
  testthat::local_mocked_bindings(
    find_sentiment_score  = function(embeddings, ...) rep(0.5, nrow(embeddings)),
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
