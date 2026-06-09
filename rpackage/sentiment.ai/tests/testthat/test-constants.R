# tests/testthat/test-constants.R
# The constants.R registry IS the v2 contract. Lock it against silent drift.
# GREEN now.

test_that("default_models is exactly the two e5 handles -> intfloat ids", {
  expect_identical(
    sentiment.ai:::default_models,
    c(`e5-small` = "intfloat/multilingual-e5-small",
      `e5-base`  = "intfloat/multilingual-e5-base")
  )
})

test_that("DEFAULT_MODEL is e5-base and is a registered default model", {
  expect_identical(sentiment.ai:::DEFAULT_MODEL, "e5-base")
  expect_true(sentiment.ai:::DEFAULT_MODEL %in% names(sentiment.ai:::default_models))
})

test_that("every selectable model has a declared dimension (kills hard-coded 512)", {
  selectable <- c(names(sentiment.ai:::default_models),
                  names(sentiment.ai:::openai_models),
                  names(sentiment.ai:::legacy_models))
  expect_true(all(selectable %in% names(sentiment.ai:::model_dims)),
              info = "model_dims must cover every model in every registry")
})

test_that("model_dims match the published model cards", {
  # Dims grounded in model cards:
  #   multilingual-e5-small 384-d, -base 768-d (intfloat/e5);
  #   text-embedding-3-small 1536-d, -3-large 3072-d, ada-002 1536-d (OpenAI);
  #   Universal Sentence Encoder family 512-d.
  d <- sentiment.ai:::model_dims
  expect_equal(d[["e5-small"]], 384L)
  expect_equal(d[["e5-base"]],  768L)
  expect_equal(d[["text-embedding-3-small"]], 1536L)
  expect_equal(d[["text-embedding-3-large"]], 3072L)
  expect_equal(d[["text-embedding-ada-002"]], 1536L)
  for (m in names(sentiment.ai:::legacy_models)) expect_equal(d[[m]], 512L)
})

test_that("v2 e5 heads are served prefix-free (trained without 'query: ')", {
  # The shipped v2/v3 scoring heads were trained on UN-prefixed e5 embeddings
  # (bakeoff/expand_real_sources.py encodes the raw text), so serving must also be
  # prefix-free: e5's asymmetric "query: "/"passage: " prefix is a *retrieval* recipe
  # (Wang et al. 2024, arXiv:2402.05672), and adding it at serve time here would be a
  # train/serve mismatch that hurts accuracy. Hence model_prefix is empty for e5.
  p <- sentiment.ai:::model_prefix
  expect_identical(unname(p[["e5-small"]]), "")
  expect_identical(unname(p[["e5-base"]]),  "")
  # no other model carries a prefix either
  expect_true(is.na(p["text-embedding-3-small"]) ||
                identical(unname(p["text-embedding-3-small"]), ""))
  expect_true(is.na(p["en.large"]) ||
                identical(unname(p["en.large"]), ""))
})
