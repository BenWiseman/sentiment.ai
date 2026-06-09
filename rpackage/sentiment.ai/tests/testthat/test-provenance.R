# tests/testthat/test-provenance.R
# sentiment_provenance() reports auditable model + head metadata and guards against
# train/serve prefix skew (the e5 "query: " prefix). Pure R, no Python/TF.

test_that("provenance reports the e5-small registry facts + shipped head metadata", {
  p <- sentiment_provenance("e5-small", scoring = "mlp")
  expect_s3_class(p, "sentiment_provenance")
  expect_identical(p$model, "e5-small")
  expect_identical(p$backend, "st")
  expect_identical(p$dim, sentiment.ai:::model_dims[["e5-small"]])
  expect_identical(p$prefix, "")        # v2 heads are served prefix-free (see test-e5-prefix)
  expect_identical(p$license, "MIT")
  expect_match(p$source, "huggingface.co/intfloat/multilingual-e5-small")
  # the placeholder mlp head ships in the package -> head metadata is populated
  expect_identical(p$head_type, "mlp")
  expect_true(is.finite(p$temperature))
  # with no init, serve prefix defaults to the registry prefix -> consistent
  expect_true(p$prefix_ok)
})

test_that("provenance marks OpenAI as API-governed with no prefix", {
  p <- sentiment_provenance("text-embedding-3-small")
  expect_identical(p$backend, "openai")
  expect_identical(p$prefix, "")
  expect_match(p$license, "OpenAI API")
})

test_that("provenance flags train/serve prefix skew", {
  senv <- get("sentiment.env", envir = asNamespace("sentiment.ai"))
  old_embed <- senv$embed; old_prefix <- senv$prefix
  withr::defer({ senv$embed <- old_embed; senv$prefix <- old_prefix })
  senv$embed  <- function(x) x   # pretend a backend is initialised...
  senv$prefix <- "query: "       # ...but a "query: " prefix was wrongly ADDED at serve
  p <- sentiment_provenance("e5-small")   # registry prefix is "" for the v2 heads
  expect_false(p$prefix_ok)               # serve "query: " != registry "" -> skew flagged
})
