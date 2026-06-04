# tests/testthat/test-sentiment-match.R
# sentiment_match: live-embed the pole phrases (default OR custom), match each input to
# its nearest phrase, attach a sentiment score. v2 ships a curated, balanced 40/40 default
# pole set (inst/default_poles.json) and embeds it live with e5, caching the result -- no
# download. Pure R, no Python/TF (fake embedder + mocked scorer).

local_st_fake <- function(env = parent.frame()) {
  senv <- get("sentiment.env", envir = asNamespace("sentiment.ai"))
  old <- senv$st; senv$st <- TRUE
  withr::defer(senv$st <- old, envir = env)
  local_fake_embedder(dim = sentiment.ai:::model_dims[["e5-small"]], env = env)
}

test_that("embed_topics live-embeds custom + default poles, never downloads", {
  local_st_fake()
  # custom poles
  cust <- sentiment.ai:::embed_topics(phrases = list(good = "great", bad = "awful"),
                                      model = "e5-small")
  expect_equal(nrow(cust$embeddings), 2L)
  expect_setequal(cust$lookup$class, c("good", "bad"))
  # the curated 40/40 default poles, live-embedded (and cached) -- no download
  def <- sentiment.ai:::embed_topics(phrases = NULL, model = "e5-small")
  expect_equal(nrow(def$embeddings), 80L)
  expect_setequal(unique(def$lookup$class), c("positive", "negative"))
})

test_that("the default poles are cached in the env after first use", {
  local_st_fake()
  senv <- get("sentiment.env", envir = asNamespace("sentiment.ai"))
  if (exists("default_emb_e5-small", envir = senv, inherits = FALSE))
    rm("default_emb_e5-small", envir = senv)
  sentiment.ai:::embed_topics(phrases = NULL, model = "e5-small")
  expect_true(exists("default_emb_e5-small", envir = senv, inherits = FALSE))
})

test_that("sentiment_match returns a scored, phrase-matched table (custom poles)", {
  local_st_fake()
  testthat::local_mocked_bindings(
    check_sentiment.ai    = function(...) invisible(NULL),
    install_scoring_model = function(...) invisible(0),
    find_sentiment_score  = function(embeddings, ...) tanh(rowMeans(embeddings)),
    .package = "sentiment.ai"
  )
  res <- sentiment_match(c("good service", "awful experience"),
                         phrases = list(positive = c("great", "lovely"),
                                        negative = c("terrible", "bad")),
                         model = "e5-small")
  expect_s3_class(res, "data.frame")
  expect_setequal(names(res), c("text", "sentiment", "phrase", "class", "similarity"))
  expect_identical(nrow(res), 2L)
  expect_true(all(res$class %in% c("positive", "negative")))
  expect_true(all(is.finite(res$sentiment)))
})

test_that("the `sentiment` column is the head score -- independent of the poles", {
  local_st_fake()
  testthat::local_mocked_bindings(
    check_sentiment.ai    = function(...) invisible(NULL),
    install_scoring_model = function(...) invisible(0),
    find_sentiment_score  = function(embeddings, ...) tanh(rowMeans(embeddings)),
    .package = "sentiment.ai"
  )
  x  <- c("good service", "awful experience", "totally fine")
  s1 <- sentiment_match(x, phrases = list(positive = c("great", "lovely"),
                                          negative = c("terrible", "bad")),
                        model = "e5-small")
  s2 <- sentiment_match(x, phrases = list(up   = c("excellent", "wonderful"),
                                          down = c("horrible", "poor")),
                        model = "e5-small")
  # swapping the poles changes only the explanation, never the calibrated score
  expect_equal(s1$sentiment, s2$sentiment)
  expect_false(isTRUE(all.equal(s1$class, s2$class)))   # explanation DID change
  # and that score is exactly what sentiment_score() returns on the same text
  expect_equal(s1$sentiment, as.numeric(sentiment_score(x, model = "e5-small")))
})

test_that("sentiment_match maps NA and empty-string inputs to NA, preserving text", {
  local_st_fake()
  testthat::local_mocked_bindings(
    check_sentiment.ai    = function(...) invisible(NULL),
    install_scoring_model = function(...) invisible(0),
    find_sentiment_score  = function(embeddings, ...) tanh(rowMeans(embeddings)),
    .package = "sentiment.ai"
  )
  res <- sentiment_match(c("good", NA, "", "bad"),
                         phrases = list(positive = "great", negative = "terrible"),
                         model = "e5-small")
  expect_identical(nrow(res), 4L)
  bad <- c(2L, 3L)                                       # the NA and "" rows
  expect_true(all(is.na(res$sentiment[bad])))
  expect_true(all(is.na(res$similarity[bad])))
  expect_true(all(is.na(res$phrase[bad])))
  expect_true(all(is.na(res$class[bad])))
  expect_true(all(is.finite(res$sentiment[c(1L, 4L)]))) # real rows still scored
  expect_identical(res$text, c("good", NA, "", "bad"))  # order + original text kept
})

test_that("the bundled default poles are the balanced 40/40 shared set", {
  poles <- sentiment.ai:::.default_poles()
  expect_setequal(names(poles), c("positive", "negative"))
  expect_length(poles$positive, 40L)
  expect_length(poles$negative, 40L)
  expect_length(intersect(poles$positive, poles$negative), 0L)
  # byte-identical to the Python package's copy -- guard against the two drifting apart
  # (the Python suite asserts the SAME md5 on its own default_poles.json).
  expect_equal(
    unname(tools::md5sum(system.file("default_poles.json", package = "sentiment.ai"))),
    "8c26694cccf2adcf32c1c9602c33af52")
})
