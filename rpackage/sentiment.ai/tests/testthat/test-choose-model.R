# tests/testthat/test-choose-model.R
# choose_model() resolves a user handle to the backend id. GREEN now.

test_that("st handles resolve to HuggingFace ids", {
  expect_identical(sentiment.ai:::choose_model("e5-small"),
                   "intfloat/multilingual-e5-small")
  expect_identical(sentiment.ai:::choose_model("e5-base"),
                   "intfloat/multilingual-e5-base")
})

test_that("openai handles resolve to OpenAI model names", {
  expect_identical(sentiment.ai:::choose_model("text-embedding-3-small"),
                   "text-embedding-3-small")
})

test_that("legacy handles resolve to the full tfhub.dev URL", {
  expect_match(sentiment.ai:::choose_model("multi.large"),
               "^https://tfhub\\.dev/google/universal-sentence-encoder-multilingual-large/3$")
})

test_that("unknown model passes through WITH a warning (cowboy mode), not an error", {
  expect_warning(out <- sentiment.ai:::choose_model("my-weird-model"), "cowboy")
  expect_identical(out, "my-weird-model")
})

test_that("length-0 / NA model is a directed stop, not a silent NULL", {
  expect_error(sentiment.ai:::choose_model(character(0)), "length 0")
  expect_error(sentiment.ai:::choose_model(NA), "length 0")
})
