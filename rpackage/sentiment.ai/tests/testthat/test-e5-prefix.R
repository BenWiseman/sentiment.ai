# tests/testthat/test-e5-prefix.R
# CI GATE 2 -- the e5 "query: " prefix is applied.
# e5 is trained with asymmetric prefixes and DROPS measurable accuracy if you skip
# them (Wang et al. 2024, "Multilingual E5 Text Embeddings", arXiv:2402.05672).
# The package must inject model_prefix[[model]] INTERNALLY, exactly once, so users
# never type "query: " themselves.
# RED now: no prefix injection exists anywhere in embed_text/init/get_embedder.py.

test_that("e5 embed receives the 'query: '-prefixed string (single input)", {
  rec <- local_fake_embedder(dim = sentiment.ai:::model_dims[["e5-small"]])
  testthat::local_mocked_bindings(
    check_sentiment.ai = function(...) invisible(NULL),
    .package = "sentiment.ai"
  )
  embed_text("good service", model = "e5-small")
  expect_true("query: good service" %in% rec$seen,
              info = "the embedder must see the prefixed string for e5 models")
  expect_false("good service" %in% rec$seen,
               info = "the embedder must NOT see the raw (un-prefixed) string")
})

test_that("every element of a BATCH is prefixed (not just the first)", {
  rec <- local_fake_embedder(dim = sentiment.ai:::model_dims[["e5-small"]])
  testthat::local_mocked_bindings(
    check_sentiment.ai = function(...) invisible(NULL),
    .package = "sentiment.ai"
  )
  embed_text(c("great", "awful", "fine"), model = "e5-small")
  expect_setequal(rec$seen, c("query: great", "query: awful", "query: fine"))
})

test_that("no double-prefix when input already starts with 'query: '", {
  rec <- local_fake_embedder(dim = sentiment.ai:::model_dims[["e5-small"]])
  testthat::local_mocked_bindings(
    check_sentiment.ai = function(...) invisible(NULL),
    .package = "sentiment.ai"
  )
  embed_text("query: already prefixed", model = "e5-small")
  expect_false(any(grepl("query: query:", rec$seen, fixed = TRUE)),
               info = "prefix injection must be idempotent (skip-if-present or strip-then-add)")
})

test_that("non-e5 models get NO prefix", {
  rec <- local_fake_embedder(dim = sentiment.ai:::model_dims[["text-embedding-3-small"]],
                             openai = TRUE)
  testthat::local_mocked_bindings(
    check_sentiment.ai = function(...) invisible(NULL),
    .package = "sentiment.ai"
  )
  embed_text("good service", model = "text-embedding-3-small")
  expect_true("good service" %in% rec$seen)
  expect_false(any(grepl("^query: ", rec$seen)),
               info = "OpenAI/legacy models must receive the raw string")
})

test_that("KNOWN-STRING regression: prefix-on vs prefix-off score differs (fixtures)", {
  # The 'known string under e5 -> expected score; fails if prefix dropped' ask.
  # Needs committed e5 embeddings for BOTH the prefixed and un-prefixed string so
  # the test is deterministic and offline. RED until those fixtures are committed.
  skip_on_cran()
  skip_if_no_fixture("emb_e5-small_prefixed.rds")
  skip_if_no_fixture("emb_e5-small_unprefixed.rds")
  emb_pre <- readRDS(fixture_path("emb_e5-small_prefixed.rds"))    # embed of "query: I love this"
  emb_raw <- readRDS(fixture_path("emb_e5-small_unprefixed.rds"))  # embed of "I love this"
  # find_sentiment_score already returns a [-1, 1] score (no rescale needed)
  s_pre <- sentiment.ai:::find_sentiment_score(emb_pre, "xgb", "1.0", "e5-small")
  s_raw <- sentiment.ai:::find_sentiment_score(emb_raw, "xgb", "1.0", "e5-small")
  # the correct, prefixed path is the golden score; dropping the prefix must change it
  expect_snapshot_value(round(s_pre, 4), style = "json2", tolerance = 1e-4)
  expect_false(isTRUE(all.equal(unname(s_pre), unname(s_raw), tolerance = 1e-3)),
               info = "dropping the e5 prefix must measurably change the score")
})
