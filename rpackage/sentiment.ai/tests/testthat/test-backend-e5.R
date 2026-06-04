# tests/testthat/test-backend-e5.R
# Tier C: loads the REAL e5 embedder. skip_on_cran + skip_if_no_st. The live
# dimension test that replaces hard-coded 512. RED until the st branch lands.

test_that("e5-small embeds at 384, e5-base at 768, rownames = text", {
  skip_on_cran(); skip_if_no_st()
  for (backend in c("e5-small", "e5-base")) {
    init_sentiment.ai(model = backend)
    emb <- embed_text(c("good", "bad", "fine"), model = backend)
    expect_equal(ncol(emb), sentiment.ai:::model_dims[[backend]], info = backend)
    expect_equal(nrow(emb), 3L)
    expect_identical(rownames(emb), c("good", "bad", "fine"))
  }
})

test_that("the selected backend is reported as 'st' (no TF path)", {
  skip_on_cran(); skip_if_no_st()
  init_sentiment.ai(model = "e5-small")
  expect_identical(sentiment.ai::sentiment.env$backend, "st")  # registry field, Phase 2
})
