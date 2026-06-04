# tests/testthat/test-onload-quiet.R
# CRAN-hygiene (G4): no warning()/cat() in .onLoad; startup chatter belongs in
# .onAttach via packageStartupMessage and must be suppressible. Today .onLoad
# calls warning() (Apple-Silicon TF note) AND advertises paraphrase/use/oai_3_small
# + "use is still the default" -- stale vs DEFAULT_MODEL='e5-small'. RED until fixed.

test_that("attaching the package emits no warning and no error with TF absent", {
  expect_silent(
    suppressPackageStartupMessages(
      loadNamespace("sentiment.ai")
    )
  )
})

test_that("startup message (if any) reflects e5-small, not 'use is still the default'", {
  msg <- tryCatch(
    {
      withr::with_envvar(c(), {
        # capture .onAttach output
        utils::capture.output(
          suppressWarnings(library(sentiment.ai)), type = "message"
        )
      })
    }, error = function(e) ""
  )
  blob <- paste(msg, collapse = " ")
  expect_false(grepl("use.*still.*default", blob, ignore.case = TRUE),
               info = "stale onload text contradicts DEFAULT_MODEL = e5-small")
})
