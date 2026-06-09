# tests/testthat/test-profiles.R
# Intent-based profiles + the persisted default model. Pure R, no Python/TF.

test_that("sentiment_profiles() lists the four presets -> real, resolvable models", {
  p <- sentiment_profiles()
  expect_s3_class(p, "data.frame")
  expect_setequal(p$profile,
                  c("lightest", "multilingual", "max-english", "max-multilingual"))
  expected <- c(lightest = "e5-small", multilingual = "e5-base",
                `max-english` = "twitter-roberta", `max-multilingual` = "xlm-roberta")
  got <- setNames(p$model, p$profile)
  expect_identical(got[names(expected)], expected)
  for (m in p$model) expect_true(nzchar(suppressWarnings(sentiment.ai:::choose_model(m))))
})

test_that("use_profile() sets + persists the default model (isolated config dir)", {
  withr::local_envvar(R_USER_CONFIG_DIR = withr::local_tempdir(),
                      SENTIMENTAI_MODEL = "")
  ns  <- asNamespace("sentiment.ai")
  old <- ns$DEFAULT_MODEL
  withr::defer({
    unlockBinding("DEFAULT_MODEL", ns); assign("DEFAULT_MODEL", old, ns)
    lockBinding("DEFAULT_MODEL", ns)
  })

  expect_identical(use_profile("multilingual"), "e5-base")
  expect_identical(sentiment.ai:::DEFAULT_MODEL, "e5-base")
  # persisted and re-readable from the JSON config
  expect_identical(sentiment.ai:::.read_user_config()$default_model, "e5-base")
  # switching to a transformer profile is accepted (registry knows the handle)
  expect_identical(use_profile("max-english"), "twitter-roberta")
  expect_identical(sentiment.ai:::DEFAULT_MODEL, "twitter-roberta")
})

test_that("use_profile() rejects an unknown profile", {
  expect_error(use_profile("nope"))
})
