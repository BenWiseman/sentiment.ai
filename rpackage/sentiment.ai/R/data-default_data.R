#' A large example sentiment-matching dictionary
#'
#' A broad dictionary of positive and negative example phrases (468 positive,
#' 470 negative). This is **not** the out-of-the-box default for
#' [sentiment_match()] -- that uses a curated, balanced 40/40 pole set
#' (`inst/default_poles.json`). Pass this object as `phrases` when you want a
#' wider, finer-grained set of poles, or use it as a template to build your own.
#' The main point is that there needs to be a corresponding negative for each
#' positive.
#'
#' @examples
#' # Use the large dictionary instead of the built-in 40/40 default poles:
#' # sentiment_match(x, phrases = default)
#'
#' # Or roll your own -- each positive needs a corresponding negative:
#' pos <- c("good apples", "fresh", "delicious")
#' neg <- c("bad apples", "not fresh", "not delicious")
#'
#' @docType data
#'
#' @usage data(default)
#'
#' @format A named `list` with two character vectors, `positive` and `negative`.
#'
"default"

