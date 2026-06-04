# tests/testthat/test-namespace-no-tf-import.R
# HIGHEST-PRIORITY CRAN gate (G1). tensorflow/tfhub/text2vec are Suggests, but
# NAMESPACE today still contains import(tensorflow) + import(tfhub) (from the
# @import tags in init_and_install.R). An unconditional import of a Suggested
# package is a hard R CMD check ERROR and breaks load on any no-TF box.
# RED now -> GREEN once the @import tags are deleted and re-documented.

test_that("NAMESPACE does not unconditionally import a Suggested package", {
  ns_file <- system.file("NAMESPACE", package = "sentiment.ai")
  # during R CMD check the installed pkg has NAMESPACE at its root
  if (!nzchar(ns_file)) ns_file <- testthat::test_path("..", "..", "NAMESPACE")
  skip_if_not(file.exists(ns_file), "NAMESPACE not found")
  ns <- readLines(ns_file, warn = FALSE)
  offenders <- grep("^import\\((tensorflow|tfhub|text2vec)\\)", ns, value = TRUE)
  expect_identical(offenders, character(0),
                   info = paste("Suggested pkgs imported unconditionally:",
                                paste(offenders, collapse = ", ")))
})
