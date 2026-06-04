# tests/testthat/test-backend-openai.R
# Tier D: real OpenAI path. Only runs when OPENAI_API_KEY is set. Keep it cheap
# (2-3 short strings) to bound spend.

test_that("openai 3-small embeds at 1536 and sets the openai flag", {
  skip_on_cran(); skip_if_no_openai_key()
  reset_sentiment_env()   # init_sentiment.ai sets $embed/$openai; restore after this test
  init_sentiment.ai(model = "text-embedding-3-small",
                    api_key = Sys.getenv("OPENAI_API_KEY"))
  expect_true(sentiment.ai::sentiment.env$openai)
  emb <- embed_text(c("good", "bad"), model = "text-embedding-3-small")
  expect_equal(ncol(emb), 1536L)
})

test_that("openai_embed rate-limit/reset branch is exercised without a real 60s sleep", {
  skip_on_cran(); skip_if_no_openai_key()
  # inject the sleep fn so CI never actually sleeps 60s; tiny limits force the reset path
  testthat::local_mocked_bindings(Sys.sleep = function(...) invisible(NULL),
                                  .package = "base")
  reset_sentiment_env()   # restore $embed/$openai after this test
  init_sentiment.ai(model = "text-embedding-3-small",
                    api_key = Sys.getenv("OPENAI_API_KEY"))
  emb <- sentiment.ai:::openai_embed(c("a", "b"), request_limit = 1, token_limit = 1)
  expect_equal(ncol(emb), 2L)
})
