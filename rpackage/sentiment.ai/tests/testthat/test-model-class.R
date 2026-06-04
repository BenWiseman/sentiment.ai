# tests/testthat/test-model-class.R
# model_class() must route by BOTH the user handle and the underlying id.
# GREEN now.

test_that("st handles and ids route to 'st'", {
  for (m in c("e5-small", "e5-base",
              "intfloat/multilingual-e5-small", "intfloat/multilingual-e5-base"))
    expect_identical(sentiment.ai:::model_class(m), "st", info = m)
})

test_that("openai handles and ids route to 'openai'", {
  for (m in c("text-embedding-3-small", "text-embedding-3-large",
              "text-embedding-ada-002"))
    expect_identical(sentiment.ai:::model_class(m), "openai", info = m)
})

test_that("legacy USE handles route to 'legacy'", {
  for (m in c("en", "en.large", "multi", "multi.large"))
    expect_identical(sentiment.ai:::model_class(m), "legacy", info = m)
})

test_that("unknown / NA route to 'unknown' (no crash on edge inputs)", {
  expect_identical(sentiment.ai:::model_class("definitely-not-a-model"), "unknown")
  expect_identical(sentiment.ai:::model_class(NA_character_), "unknown")
})
