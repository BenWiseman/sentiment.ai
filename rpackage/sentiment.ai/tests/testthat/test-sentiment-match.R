# tests/testthat/test-sentiment-match.R
# sentiment_match: live-embed the pole phrases (default OR custom), match each input to
# its nearest phrase, attach a sentiment score. v2 must NOT download the legacy
# default-embeddings file -- it embeds the default poles live with e5 and caches them.
# Pure R, no Python/TF (fake embedder + mocked scorer).

local_st_fake <- function(env = parent.frame()) {
  senv <- get("sentiment.env", envir = asNamespace("sentiment.ai"))
  old <- senv$st; senv$st <- TRUE
  withr::defer(senv$st <- old, envir = env)
  local_fake_embedder(dim = sentiment.ai:::model_dims[["e5-small"]], env = env)
}

test_that("embed_topics live-embeds custom + default poles, never downloads", {
  local_st_fake()
  testthat::local_mocked_bindings(
    install_default_embeddings = function(...) stop("v2 must not download default embeddings"),
    .package = "sentiment.ai"
  )
  # custom poles
  cust <- sentiment.ai:::embed_topics(phrases = list(good = "great", bad = "awful"),
                                      model = "e5-small")
  expect_equal(nrow(cust$embeddings), 2L)
  expect_setequal(cust$lookup$class, c("good", "bad"))
  # the ~900-term default dictionary, live-embedded (and cached) -- no download
  def <- sentiment.ai:::embed_topics(phrases = NULL, model = "e5-small")
  expect_gt(nrow(def$embeddings), 100L)
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
