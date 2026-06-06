# tests/testthat/test-agreement.R
# sentiment_agreement(): agreement statistics between model scores and human labels.
# Fully offline: no embedder, no Python. Tests the statistical machinery directly.

test_that("sentiment_agreement works with numeric labels", {
  # perfect agreement case
  scores <- c( 0.9,  0.8, -0.8, -0.9,  0.1)
  labels <- c(   1,    1,   -1,   -1,    0)
  ag <- sentiment_agreement(scores, labels)
  expect_s3_class(ag, "sentiment_agreement")
  expect_equal(ag$n, 5L)
  expect_equal(ag$percent_agreement, 1.0)
  expect_gte(ag$spearman_r, 0.9)
  expect_s3_class(ag$confusion_matrix, "table")
})

test_that("sentiment_agreement works with string labels", {
  scores <- c( 0.9, -0.7,  0.05, -0.85,  0.75)
  labels <- c("positive","negative","neutral","negative","positive")
  ag <- sentiment_agreement(scores, labels)
  expect_s3_class(ag, "sentiment_agreement")
  expect_equal(ag$n, 5L)
  expect_gte(ag$spearman_r, 0.5)
})

test_that("accepts a sentiment() data.frame", {
  df <- data.frame(text = c("x","y"), sentiment = c(0.8, -0.7),
                   prob_neg = c(0.05, 0.85), prob_neu = c(0.05, 0.10),
                   prob_pos = c(0.90, 0.05), class = c("positive","negative"),
                   confidence = c(0.90, 0.85), stringsAsFactors = FALSE)
  ag <- sentiment_agreement(df, c("positive","negative"))
  expect_equal(ag$percent_agreement, 1.0)
})

test_that("NA pairs are silently dropped", {
  scores <- c( 0.9, NA,  -0.8)
  labels <- c(   1,  1,    -1)
  ag <- sentiment_agreement(scores, labels)
  expect_equal(ag$n, 2L)
})

test_that("irr stats are present when irr is installed", {
  scores <- c( 0.9,  0.7, -0.8, -0.9,  0.05,  0.05, -0.05,  0.8, -0.7,  0.0)
  labels <- c(   1,    1,   -1,   -1,    0,     0,     0,    1,   -1,    0)
  ag <- sentiment_agreement(scores, labels)
  if(requireNamespace("irr", quietly = TRUE)){
    expect_false(is.na(ag$weighted_kappa))
    expect_false(is.na(ag$krippendorff_alpha))
    expect_true(is.list(ag$icc))
    # kappa in (-1, 1)
    expect_gte(ag$weighted_kappa, -1); expect_lte(ag$weighted_kappa, 1)
  } else {
    expect_true(is.na(ag$weighted_kappa))  # graceful degradation
  }
})

test_that("confusion matrix has correct shape and labelling", {
  scores <- c(0.9, -0.8, 0.0)
  labels <- c(  1,   -1,   0)
  ag <- sentiment_agreement(scores, labels)
  cm <- ag$confusion_matrix
  expect_equal(dim(cm), c(3L, 3L))
  expect_equal(dimnames(cm)$true, c("negative","neutral","positive"))
  expect_equal(dimnames(cm)$predicted, c("negative","neutral","positive"))
})

test_that("print.sentiment_agreement runs without error", {
  scores <- c(0.9, -0.7, 0.1, 0.8, -0.9)
  labels <- c(  1,   -1,   0,   1,   -1)
  ag <- sentiment_agreement(scores, labels)
  expect_invisible(print(ag))
})
