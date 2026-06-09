# tests/testthat/test-e5-prefix.R
# CI GATE 2 -- v2 e5 scoring is served PREFIX-FREE.
# e5's asymmetric "query: "/"passage: " prefix is a *retrieval* recipe (Wang et al. 2024,
# arXiv:2402.05672). The shipped v2/v3 heads were trained on UN-prefixed e5 embeddings
# (bakeoff/expand_real_sources.py encodes raw text), so embed_text() must pass the raw
# string straight through -- adding "query: " at serve time would be a train/serve
# mismatch that hurts accuracy. These tests pin that the embedder sees the raw text.

test_that("e5 embed receives the RAW string (v2 heads are prefix-free)", {
  rec <- local_fake_embedder(dim = sentiment.ai:::model_dims[["e5-small"]])
  testthat::local_mocked_bindings(
    check_sentiment.ai = function(...) invisible(NULL),
    .package = "sentiment.ai"
  )
  embed_text("good service", model = "e5-small")
  expect_true("good service" %in% rec$seen,
              info = "the embedder must see the raw string (v2 heads trained prefix-free)")
  expect_false("query: good service" %in% rec$seen,
               info = "no 'query: ' retrieval prefix is injected for the v2 heads")
})

test_that("every element of a batch is passed raw (no prefix added)", {
  rec <- local_fake_embedder(dim = sentiment.ai:::model_dims[["e5-small"]])
  testthat::local_mocked_bindings(
    check_sentiment.ai = function(...) invisible(NULL),
    .package = "sentiment.ai"
  )
  embed_text(c("great", "awful", "fine"), model = "e5-small")
  expect_setequal(rec$seen, c("great", "awful", "fine"))
})

test_that("input is never mangled with a 'query: ' prefix", {
  rec <- local_fake_embedder(dim = sentiment.ai:::model_dims[["e5-small"]])
  testthat::local_mocked_bindings(
    check_sentiment.ai = function(...) invisible(NULL),
    .package = "sentiment.ai"
  )
  embed_text("query: already prefixed", model = "e5-small")
  # the literal text the user passed is embedded verbatim, and no prefix is bolted on
  expect_true("query: already prefixed" %in% rec$seen)
  expect_false(any(grepl("query: query:", rec$seen, fixed = TRUE)))
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
