# tests/testthat/test-init-activate.R
# Regression for the init crash found by the clean-install test: init_sentiment.ai()
# on its default method = "auto" called .activate_env(), which (a) invoked
# py_install_method_detect() with NO envname -> "invalid non-character version
# specification" and (b) "recovered" via assign(method, pos = -2) -- an invalid pos that
# crashed the error handler. Guard both so they cannot come back.

test_that("py_install_method_detect requires an envname", {
  # without an envname the detector errors (so .activate_env MUST pass one). It also
  # emits diagnostic messages/warnings on the way down, which we silence here.
  expect_error(
    suppressWarnings(suppressMessages(sentiment.ai:::py_install_method_detect())))
})

test_that(".activate_env passes envname and has no invalid assign(pos = -2) failover", {
  src <- paste(deparse(sentiment.ai:::.activate_env), collapse = "\n")
  # the detector must be called WITH an envname
  expect_match(src, "py_install_method_detect\\(\\s*envname")
  # the old failover used assign(..., pos = -2), which is invalid for assign() and
  # crashed the handler; it must be gone (we use `method <<- ...` now)
  expect_false(grepl("pos\\s*=\\s*-2", src),
               label = "assign(pos = -2) must not reappear in .activate_env")
})
