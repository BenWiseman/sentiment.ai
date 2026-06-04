# tests/testthat/test-legacy-roundtrip.R
# "keep the legacy USE path genuinely working ... or 'compatibility layer' is a
# lie." When TF + tfhub + the legacy env ARE present, the path must round-trip.
# Skips cleanly off-CRAN / without TF.

test_that("legacy en.large loads USE-512 and embeds at width 512", {
  skip_on_cran()
  skip_if_no_tf()
  init_sentiment.ai(model = "en.large",
                    envname = Sys.getenv("SENTIMENTAI_TF_ENV", "r-sentiment-ai"))
  emb <- embed_text(c("good", "bad"), model = "en.large")
  expect_equal(ncol(emb), 512L)
  expect_equal(nrow(emb), 2L)
})

test_that("legacy scorer still reproduces historical scores (backward-compat)", {
  skip_on_cran()
  skip_if_no_fixture("emb_en.large.rds")     # committed legacy USE embeddings
  emb    <- readRDS(fixture_path("emb_en.large.rds"))
  scores <- sentiment.ai:::find_sentiment_score(emb, "xgb", "1.0", "en.large")
  expect_snapshot_value(round(scores, 4), style = "json2", tolerance = 1e-4)
})
