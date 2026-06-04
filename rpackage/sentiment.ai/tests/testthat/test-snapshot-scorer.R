# tests/testthat/test-snapshot-scorer.R
# Tier B: deterministic embeddings (committed fixture) -> known scores (snapshot).
# A snapshot change == a deliberate, NEWS-documented scoring-model change.
# RED until the e5/oai xgb scorers + fixtures are packaged.

backends <- c("e5-small", "e5-base", "text-embedding-3-small")

for (backend in backends) {
  local({
    bk <- backend
    test_that(sprintf("[%s] scorer reproduces golden scores on fixture embeddings", bk), {
      skip_on_cran()                                  # scorer weights may need download
      skip_if_no_fixture(sprintf("emb_%s.rds", bk))
      skip_if_no_fixture(sprintf("scores_%s_xgb.rds", bk))

      emb <- readRDS(fixture_path(sprintf("emb_%s.rds", bk)))

      # dimension guard FIRST: a wrong-width fixture would mispredict silently
      expect_equal(ncol(emb), sentiment.ai:::model_dims[[bk]],
                   info = "fixture width must equal the model's registered dim")

      # find_sentiment_score returns scores already mapped to [-1, 1].
      scores <- sentiment.ai:::find_sentiment_score(
        embeddings = emb, scoring = "xgb", scoring_version = "1.0", model = bk
      )

      # 1) exact regression snapshot (float-jitter tolerant: single- vs multi-thread
      #    xgboost must not change determinism, but guard drift regardless)
      expect_snapshot_value(round(scores, 4), style = "json2", tolerance = 1e-4)

      # 2) class-direction (survives a deliberate re-snapshot; catches a sign flip
      #    or a wrong model-file mapping even when the snapshot is regenerated)
      corpus <- readRDS(fixture_path("corpus.rds"))
      pos <- scores[corpus$label == "positive"]
      neg <- scores[corpus$label == "negative"]
      expect_true(all(pos >  0.25), info = "clear-positive rows must score clearly positive")
      expect_true(all(neg < -0.25), info = "clear-negative rows must score clearly negative")
    })
  })
}
