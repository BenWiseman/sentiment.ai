# tests/testthat/test-snapshot-scorer.R
# Tier B: deterministic fixture embeddings -> exact golden mlp scores.
# A change here means a deliberate, NEWS-documented head upgrade.
# Uses the SHIPPED mlp head (bundled in-package; no download, no xgboost needed).
# Fixtures: tests/testthat/fixtures/emb_{model}.rds  (real e5 embeddings, 60 rows,
# 30 positive + 30 negative from the held-out real-text test split).

for (bk in c("e5-small", "e5-base")) {
  local({
    model <- bk
    test_that(sprintf("[%s] shipped mlp scorer reproduces golden scores", model), {
      # pure in-memory (RDS fixture + in-package JSON head) -- no Python, no network,
      # no download. skip_on_cran() removed: these are CRAN-safe.
      skip_if_no_fixture(sprintf("emb_%s.rds", model))
      skip_if_no_fixture(sprintf("scores_mlp_%s.rds", model))
      skip_if_no_fixture("corpus.rds")

      emb    <- readRDS(fixture_path(sprintf("emb_%s.rds", model)))
      golden <- readRDS(fixture_path(sprintf("scores_mlp_%s.rds", model)))
      corpus <- readRDS(fixture_path("corpus.rds"))

      # dimension guard: a wrong-width fixture would mispredict silently
      expect_equal(ncol(emb), sentiment.ai:::model_dims[[model]],
                   info = "fixture width must equal the model's registered dim")

      scores <- sentiment.ai:::find_sentiment_score(
        embeddings = emb, scoring = "mlp", scoring_version = "1.0", model = model)

      # 1. regression: scores must match the committed golden values within float noise.
      #    A failure here == deliberate head upgrade -> update golden + add NEWS entry.
      expect_equal(scores, golden, tolerance = 1e-4,
                   info = paste("mlp head output differs from committed golden scores.",
                                "If this is intentional (head upgrade), regenerate",
                                "fixtures and add a NEWS entry."))

      # 2. direction + confidence: catches sign flips and head degradation after
      #    a golden refresh, independent of the exact scores.
      pos <- scores[corpus$label == "positive"]
      neg <- scores[corpus$label == "negative"]
      expect_true(mean(pos) >  0.3, info = "positive rows must score positive on average")
      expect_true(mean(neg) < -0.3, info = "negative rows must score negative on average")
      # mid-band guard: confident, not just correct in sign
      expect_true(mean(abs(pos)) > 0.5, info = "positive rows should be clearly positive, not near-zero")
      expect_true(mean(abs(neg)) > 0.5, info = "negative rows should be clearly negative, not near-zero")
    })
  })
}
