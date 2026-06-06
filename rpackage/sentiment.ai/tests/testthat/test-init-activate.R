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

test_that(".backend_ready reflects whether the named env exists", {
  testthat::local_mocked_bindings(
    .package = "reticulate",
    virtualenv_list = function(...) c("r-sentiment-ai", "other"),
    conda_list      = function(...) data.frame(name = "base", stringsAsFactors = FALSE))
  expect_true(sentiment.ai:::.backend_ready("r-sentiment-ai"))  # virtualenv
  expect_true(sentiment.ai:::.backend_ready("base"))            # conda
  expect_false(sentiment.ai:::.backend_ready("nope"))
})

test_that(".torch_index defaults to the smaller CPU build off macOS", {
  testthat::local_mocked_bindings(is.os_mac = function() FALSE, .package = "roperators")
  expect_match(sentiment.ai:::.torch_index(gpu = FALSE), "whl/cpu")  # default = CPU torch
  expect_null(sentiment.ai:::.torch_index(gpu = TRUE))               # CUDA = default index
  testthat::local_mocked_bindings(is.os_mac = function() TRUE, .package = "roperators")
  expect_null(sentiment.ai:::.torch_index(gpu = FALSE))              # mac is always CPU/MPS
})

test_that("setup_sentiment.ai is a script-safe no-op (FALSE) when non-interactive", {
  # tests run non-interactively -> the walkthrough must not prompt or install
  expect_false(suppressMessages(setup_sentiment.ai()))
})

test_that("check_sentiment.ai errors (never auto-installs) when backend missing + non-interactive", {
  senv <- get("sentiment.env", envir = asNamespace("sentiment.ai"))
  old <- senv$embed; senv$embed <- NULL; withr::defer(senv$embed <- old)
  testthat::local_mocked_bindings(.backend_ready = function(...) FALSE,
                                  .package = "sentiment.ai")
  expect_error(suppressMessages(sentiment.ai:::check_sentiment.ai()),
               "install_sentiment.ai")
})

test_that("check_sentiment.ai inits (no setup prompt) when the backend is ready", {
  senv <- get("sentiment.env", envir = asNamespace("sentiment.ai"))
  old <- senv$embed; senv$embed <- NULL; withr::defer(senv$embed <- old)
  flag <- new.env(); flag$init <- FALSE
  testthat::local_mocked_bindings(
    .backend_ready    = function(...) TRUE,
    init_sentiment.ai = function(...) { flag$init <- TRUE; invisible(NULL) },
    .package = "sentiment.ai")
  suppressMessages(sentiment.ai:::check_sentiment.ai())
  expect_true(flag$init)
})

test_that(".autoinit_envname parses the sentiment.ai.autoinit opt-in", {
  # off -> NULL (no eager init on attach)
  expect_null(sentiment.ai:::.autoinit_envname(""))
  expect_null(sentiment.ai:::.autoinit_envname(FALSE))
  expect_null(sentiment.ai:::.autoinit_envname(NULL))
  # truthy -> the default env
  expect_equal(sentiment.ai:::.autoinit_envname(TRUE), "r-sentiment-ai")
  expect_equal(sentiment.ai:::.autoinit_envname("true"), "r-sentiment-ai")
  expect_equal(sentiment.ai:::.autoinit_envname("YES"), "r-sentiment-ai")
  # a custom env name passes through
  expect_equal(sentiment.ai:::.autoinit_envname("my-env"), "my-env")
})
