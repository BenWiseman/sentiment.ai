# tests/testthat/test-no-512-literal.R
# A literal tripwire (CI gate 4, source-level): no hard-coded 512 may survive in
# R/sentiment.R outside constants.R's model_dims. The dim-handling logic is now
# registry-driven (model_dims[[model]]); this guards against a 512 creeping back,
# including in roxygen doc examples (a "512-D numeric" doc line is itself a leak,
# since the accepted width now follows the chosen model).
# Source-reading is a dev-only check, so skip when the source tree isn't reachable
# (installed-pkg test runs).

test_that("no literal 512 in R/sentiment.R (dim must come from model_dims)", {
  src <- testthat::test_path("..", "..", "R", "sentiment.R")
  skip_if_not(file.exists(src), "source tree not available (installed run)")
  lines <- readLines(src, warn = FALSE)
  hits  <- grep("512", lines, value = TRUE)
  expect_identical(
    hits, character(0),
    info = paste0("512 is registry-driven now; read model_dims, not a literal. ",
                  "Offending line(s): ", paste(trimws(hits), collapse = " | "))
  )
})
