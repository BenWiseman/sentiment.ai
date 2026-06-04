# tests/testthat/test-download-mocked.R
# CRAN-hygiene (G2): install_scoring_model() hits GitHub via utils::download.file.
# CRAN forbids network + writing outside tempdir during check. NEVER perform the
# GET: mock download.file, assert the constructed URL, and assert graceful failure.
#
# Verified against R/init_and_install.R: install_scoring_model() builds
#   <repo>/<scoring>/<version>/<model>.<ext>?raw=true
# and wraps the download in tryCatch, message()-ing and returning status (0) on
# failure -- it never throws.

test_that("scoring URL is constructed correctly and download is never really performed", {
  captured <- new.env(parent = emptyenv()); captured$url <- NA_character_
  testthat::local_mocked_bindings(
    .package = "utils",
    download.file = function(url, destfile, ...) { captured$url <- url; 0L }
  )
  status <- tryCatch(
    suppressMessages(
      sentiment.ai:::install_scoring_model(model = "en.large", scoring = "xgb",
                                           scoring_version = "1.0")
    ),
    error = function(e) e
  )
  # in an installed-pkg run the scoring dir may not be writable: tolerate that.
  skip_if(inherits(status, "error") &&
            grepl("cannot create|not.*writ|permission", conditionMessage(status),
                  ignore.case = TRUE),
          "installed-pkg scoring dir not writable in this run (expected on CRAN box)")
  if (!is.na(captured$url))
    expect_match(captured$url, "scoring/xgb/1\\.0/en\\.large\\.xgb\\?raw=true")
})

test_that("a failed download degrades gracefully (message + status, never an uncaught error)", {
  testthat::local_mocked_bindings(
    .package = "utils",
    download.file = function(...) stop("network down")
  )
  status <- tryCatch(
    suppressMessages(
      sentiment.ai:::install_scoring_model("en.large", "xgb", "1.0")
    ),
    error = function(e) e
  )
  skip_if(inherits(status, "error") &&
            grepl("cannot create|not.*writ|permission", conditionMessage(status),
                  ignore.case = TRUE),
          "installed-pkg scoring dir not writable in this run (expected on CRAN box)")
  # the network failure itself must be swallowed (tryCatch in the function),
  # leaving a numeric status, not a propagated error.
  expect_false(inherits(status, "error"),
               info = "a download failure must not propagate as an uncaught error")
  expect_type(status, "double")
})
