# tests/testthat/test-legacy-gate.R
# CI GATE 3 -- legacy gate: directed error, not a crash.
# CONTRACT: requesting a legacy USE model without the legacy TF backend installed
# must raise a CLEAR, ACTIONABLE error -- not a reticulate ImportError on
# tensorflow_text, and not a silent fall-through. The error must name BOTH the fix
# (install_sentiment.ai(legacy = TRUE)) AND the TF-free replacement model.
# Runs ONLY when TF is genuinely absent (the situation being protected).
# RED now: no gate exists; multi.large falls into the TF path and crashes.

# expected replacement per legacy model (from constants.R comments):
#   en -> e5-small ; en.large / multi / multi.large -> e5-base
legacy_repl <- list(en = "e5-small", en.large = "e5-base",
                    multi = "e5-base", multi.large = "e5-base")

for (m in names(legacy_repl)) {
  local({
    model_m <- m
    repl_m  <- legacy_repl[[m]]
    test_that(sprintf("legacy '%s' without TF -> directed error naming leg=TRUE and %s",
                      model_m, repl_m), {
      skip_unless_tf_absent()
      err <- expect_error(
        suppressWarnings(init_sentiment.ai(model = model_m)),  # USE end-of-life warning
        regexp = "legacy"        # message must talk about the legacy backend
      )
      msg <- conditionMessage(err)
      expect_match(msg, "legacy\\s*=\\s*TRUE",
                   info = "must tell the user how to enable the legacy backend")
      expect_match(msg, repl_m,
                   info = "must name the TF-free replacement model")
    })
  })
}

test_that("the gate short-circuits at the R level BEFORE any Python import", {
  skip_unless_tf_absent()
  # Prove the failure does not depend on Python being reachable: if the gate
  # fired correctly, source_python must NEVER be called.
  called <- new.env(parent = emptyenv()); called$py <- FALSE
  testthat::local_mocked_bindings(
    .package = "reticulate",
    source_python = function(...) { called$py <- TRUE; stop("python reached!") }
  )
  expect_error(suppressWarnings(init_sentiment.ai(model = "multi.large")), "legacy")
  expect_false(called$py, info = "legacy gate must stop before touching Python")
})

test_that("sentiment_score(model='multi.large') without TF errors the same way", {
  skip_unless_tf_absent()
  expect_error(suppressWarnings(sentiment_score("anything", model = "multi.large")),
               "legacy")
})

test_that("requesting a legacy USE model warns that USE is end-of-life", {
  skip_unless_tf_absent()
  senv <- get("sentiment.env", envir = asNamespace("sentiment.ai"))
  old  <- senv$warned_use_eol; senv$warned_use_eol <- NULL
  withr::defer(senv$warned_use_eol <- old)
  # the warning fires for ANY legacy request, before the gate errors
  expect_warning(
    tryCatch(init_sentiment.ai(model = "multi.large"), error = function(e) NULL),
    "Universal Sentence Encoder"
  )
})
