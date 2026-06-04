# tests/testthat/test-description-credits.R
# A refactor must not quietly strip the MIT + Korn Ferry Institute attribution.
# GREEN now.

test_that("Korn Ferry Institute funder and Ben Wiseman (cre) remain in DESCRIPTION", {
  d <- utils::packageDescription("sentiment.ai")
  expect_match(d$License, "MIT")
  authors <- paste(d$`Authors@R`, d$Author, collapse = " ")
  expect_match(authors, "Korn Ferry Institute")
  expect_match(d$Maintainer, "Ben Wiseman")
})
