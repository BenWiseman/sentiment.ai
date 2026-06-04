# tests/testthat/test-scoring-parity.R
# Tier B: glm is the compatibility fallback, not an xgb clone. Assert SIGN
# agreement on clear-polarity rows, not exact value. Pins that scoring="glm" is a
# usable degraded path, not silently broken. RED until both models exist for a space.

test_that("xgb and glm agree in sign on clear-polarity fixtures (e5-small)", {
  skip_on_cran()
  skip_if_no_fixture("emb_e5-small.rds")
  emb    <- readRDS(fixture_path("emb_e5-small.rds"))
  corpus <- readRDS(fixture_path("corpus.rds"))
  clear  <- corpus$label %in% c("positive", "negative")

  glm_path <- system.file("scoring", "glm", "1.0", "e5-small.csv",
                          package = "sentiment.ai")
  skip_if_not(nzchar(glm_path) && file.exists(glm_path), "glm e5-small not packaged")

  # find_sentiment_score returns [-1, 1]; polarity is just its sign.
  s_xgb <- sentiment.ai:::find_sentiment_score(emb, "xgb", "1.0", "e5-small")
  s_glm <- sentiment.ai:::find_sentiment_score(emb, "glm", "1.0", "e5-small")
  expect_equal(sign(s_xgb[clear]), sign(s_glm[clear]),
               info = "xgb/glm must agree in polarity on clear rows")
})
