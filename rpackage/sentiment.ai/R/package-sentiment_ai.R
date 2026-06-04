#' @description
#' sentiment.ai turns text into sentiment scores using a small, on-device
#' multilingual embedding model (default \code{multilingual-e5-small}: no
#' TensorFlow, no API key) plus a small bundled scoring head. You can also opt into
#' a larger on-device model (\code{e5-base}), a paid API (\code{openai}), or the
#' legacy Universal Sentence Encoder models (which require TensorFlow).
#'
#' Main benefits:
#' \itemize{
#'   \item{Tolerates spelling mistakes}
#'   \item{Not dependent on exactly matching a fixed dictionary}
#'   \item{Requires less pre-processing}
#'   \item{More flexible than dictionary-based methods}
#'   \item{Multilingual and runs on-device with no TensorFlow}
#' }
#'
#' Scores run from about \code{1} (positive) to about \code{-1} (negative). See
#' \code{\link{sentiment_score}} to get started, and
#' \code{\link{sentiment_provenance}} to see exactly which model and scoring head
#' produced a score.
"_PACKAGE"
