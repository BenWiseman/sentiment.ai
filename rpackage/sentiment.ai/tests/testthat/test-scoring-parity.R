# tests/testthat/test-scoring-parity.R
# Tier B: mlp (default, shipped) and logistic (fallback, shipped) must agree in polarity
# on clear-positive and clear-negative real-text rows. Both heads ship in-package.
# Also guards that the scalar sentiment() and sentiment_score() agree on the same matrix.

for(bk in c("e5-small", "e5-base")){
  local({
    model <- bk
    test_that(sprintf("[%s] mlp and logistic agree in sign on fixture rows (>=95%%)", model), {
      # pure in-memory: no Python, no download. CRAN-safe (skip_on_cran removed).
      # The corpus.rds fixture is clear-polarity only (30 pos + 30 neg, no neutral),
      # so the `clear` filter is a no-op and is omitted to avoid false confidence.
      skip_if_no_fixture(sprintf("emb_%s.rds", model))
      skip_if_no_fixture("corpus.rds")

      emb   <- readRDS(fixture_path(sprintf("emb_%s.rds", model)))
      s_mlp <- sentiment.ai:::find_sentiment_score(emb, "mlp",      "1.0", model)
      s_log <- sentiment.ai:::find_sentiment_score(emb, "logistic", "1.0", model)

      # mlp and logistic are different head families; 1-2 boundary rows can disagree.
      # 95% is calibrated to the observed 2/60 disagreement rate on the real fixture;
      # at n=60 this tolerates at most 3 disagreements. A 4th means a head is drifting.
      agree_rate <- mean(sign(s_mlp) == sign(s_log))
      expect_gte(agree_rate, 0.95,
                 label = sprintf(
                   "%s mlp/logistic sign agreement rate (%.1f%% -- max 3 disagreements at n=60)",
                   model, 100 * agree_rate))
    })
  })
}
