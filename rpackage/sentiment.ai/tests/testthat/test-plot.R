# tests/testthat/test-plot.R
# plot_sentiment(): the interactive sentiment map. Hermetic -- precomputed embeddings
# clustered around 3 topic centroids, scored through the real shipped heads via the
# sentiment() matrix path; PCA reducer so no uwot/Rtsne and no Python/network are needed.

make_plot_data <- function(d = 384L, seed = 0L){
  set.seed(seed)
  topics <- list(
    delays = c("flight delayed three hours", "stuck on the tarmac", "missed my connection",
               "late departure again", "hours of waiting", "boarding was so late"),
    staff  = c("the crew were lovely", "friendly cabin staff", "rude gate agent",
               "helpful attendant", "staff ignored us", "warm welcome aboard"),
    food   = c("the meal was tasty", "cold bland dinner", "great coffee onboard",
               "no vegetarian option", "delicious snack", "soggy sandwich"))
  cent <- matrix(stats::rnorm(3 * d), 3, d)
  texts <- character(0); emb <- NULL
  for (ti in seq_along(topics)) for (t in topics[[ti]]) {
    v <- cent[ti, ] + 0.3 * stats::rnorm(d); v <- v / sqrt(sum(v^2))
    emb <- rbind(emb, v); texts <- c(texts, t)
  }
  list(texts = texts, emb = emb)
}

test_that("plot_sentiment() returns a plotly widget + tidy data frame", {
  testthat::skip_if_not_installed("plotly")
  dat <- make_plot_data()
  res <- plot_sentiment(dat$texts, model = "e5-small", embeddings = dat$emb,
                        reducer = "pca", labels = "tfidf", return_data = TRUE)
  expect_true(inherits(res$plot, "plotly") || inherits(res$plot, "htmlwidget"))
  expect_s3_class(res$data, "data.frame")
  expect_identical(nrow(res$data), length(dat$texts))
  expect_true(all(c("x", "y", "text", "sentiment", "class", "cluster", "cluster_label") %in%
                  names(res$data)))
  expect_gte(length(unique(res$data$cluster)), 2)         # 3 separable topics
  expect_true(all(nzchar(res$data$cluster_label)))
})

test_that("plot_sentiment() c-TF-IDF labels are deterministic", {
  testthat::skip_if_not_installed("plotly")
  dat <- make_plot_data()
  f <- function() plot_sentiment(dat$texts, model = "e5-small", embeddings = dat$emb,
                                 reducer = "pca", return_data = TRUE)$data$cluster_label
  expect_identical(f(), f())
})

test_that("plot_sentiment() rejects an end-to-end classifier (needs embedding space)", {
  testthat::skip_if_not_installed("plotly")
  expect_error(plot_sentiment(c("a", "b", "c"), model = "twitter-roberta"))
})

test_that("plot_sentiment() needs at least two non-empty texts", {
  testthat::skip_if_not_installed("plotly")
  dat <- make_plot_data()
  expect_error(plot_sentiment("only one", model = "e5-small", embeddings = dat$emb[1, , drop = FALSE]))
})
