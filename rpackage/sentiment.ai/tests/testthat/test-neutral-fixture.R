# tests/testthat/test-neutral-fixture.R
# Tier B: guards the SHIPPED model's behaviour on committed genuinely-neutral rows
# so a future scorer swap that regresses the neutral class is caught. The
# quantitative F1 finding lives in the benchmark vignette with REAL numbers; here
# we only guard a behavioural band. The band is READ from PROVENANCE.md
# (training-repo eval) -- NEVER invented here.
# RED until scorers + fixtures + a recorded band are committed.

test_that("genuinely-neutral fixture rows fall inside the recorded neutral band", {
  skip_on_cran()
  skip_if_no_fixture("emb_e5-small.rds")
  prov <- fixture_path("PROVENANCE.md")
  skip_if_not(file.exists(prov), "PROVENANCE.md (neutral_band source) missing")

  # neutral_band MUST come from the training-repo eval, recorded in PROVENANCE.md
  # as a line like:  neutral_band: 0.NN
  band_line <- grep("^neutral_band:", readLines(prov), value = TRUE)
  skip_if(length(band_line) == 0, "neutral_band not recorded in PROVENANCE.md")
  neutral_band <- as.numeric(sub("^neutral_band:\\s*", "", band_line[1]))

  emb    <- readRDS(fixture_path("emb_e5-small.rds"))
  corpus <- readRDS(fixture_path("corpus.rds"))
  # find_sentiment_score returns scores already in [-1, 1].
  scores <- sentiment.ai:::find_sentiment_score(emb, "xgb", "1.0", "e5-small")
  neutral_scores <- scores[corpus$label == "neutral"]
  expect_true(all(abs(neutral_scores) < neutral_band),
              info = sprintf("neutral rows must satisfy |score| < %.3f", neutral_band))
})
