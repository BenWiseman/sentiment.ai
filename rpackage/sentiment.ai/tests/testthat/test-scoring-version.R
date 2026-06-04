# tests/testthat/test-scoring-version.R
# match.arg safety (G6): scoring_version uses match.arg with a single allowed
# value "1.0". Pin that "1.0" resolves and an unknown version errors cleanly (so
# adding a 2nd version later is a deliberate change to the formals, not a silent
# break). GREEN now.

test_that("scoring_version '1.0' resolves; unknown errors cleanly", {
  testthat::local_mocked_bindings(
    install_scoring_model = function(...) invisible(0),
    .package = "sentiment.ai"
  )
  expect_null(sentiment_score(NULL, scoring_version = "1.0"))   # early NULL return
  expect_error(sentiment_score(NULL, scoring_version = "9.9"))  # match.arg rejects
})
