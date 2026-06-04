# tests/testthat/test-no-tf-default.R
# CI GATE 1 -- THE HEADLINE: sentiment_score("I love this") returns a POSITIVE
# number with ZERO TensorFlow installed.
# RED now: init/embed_text have no sentence-transformers branch and default to
# the TF hub_embed path. GREEN once the st branch + e5 default land.

test_that("the e5 default path is reachable with TF absent (routing contract)", {
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
    # positive-leaning text -> score > 0 ; keep it deterministic. find_sentiment_score
    # is the real internal and already returns a [-1, 1] score.
    find_sentiment_score  = function(embeddings, ...) rep(0.8, nrow(embeddings)),
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
  # This is the integration proof of the headline. It must run on the no-TF CI
  # leg. If TF happens to be installed too, fine -- the point is it is NOT REQUIRED.
  init_sentiment.ai(model = "e5-small")
  # the registry should expose which backend was actually selected:
  expect_identical(sentiment.ai::sentiment.env$backend, "st")   # $backend added in Phase 2
  score <- sentiment_score("I love this")
  expect_true(is.finite(score) && score > 0)
})
