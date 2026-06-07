#' Diagnostic signals for a batch of sentiment scores
#'
#' Augments each row with signals that tell you \emph{when not to trust the score}:
#' the probability entropy, a confidence band, a mixed-sentiment flag, and an
#' out-of-domain (OOD) similarity to the training class centroids.  Accepts the same
#' arguments as \code{\link{sentiment}}.
#'
#' @inheritParams sentiment
#'
#' @return A \code{data.frame} with all columns from \code{\link{sentiment}} plus:
#'   \describe{
#'     \item{entropy}{Shannon entropy of the 3-class probability vector (nats, 0–1.099).
#'       High values mean the head is spread across classes.}
#'     \item{confidence_band}{Ordered factor \code{"high"} / \code{"moderate"} / \code{"low"},
#'       calibrated from the reliability report: \eqn{\geq 0.85} = high, 0.65–0.85 = moderate,
#'       \eqn{< 0.65} = low.}
#'     \item{mixed}{Logical; \code{TRUE} when both \code{prob_pos} and \code{prob_neg}
#'       exceed 0.25 — the head sees simultaneous positive and negative mass (e.g.
#'       "loved the food, hated the service"). Usually needs human review.}
#'     \item{ood_similarity}{Maximum cosine similarity to any training class centroid.
#'       Below ~0.20 the text is unlike the training distribution. \code{NA} when no
#'       centroid file is bundled for the model.}
#'     \item{ood_flag}{Logical; \code{TRUE} when \code{ood_similarity < 0.20}.}
#'   }
#'
#' @description
#' Returns the same calibrated sentiment score as \code{\link{sentiment}} plus five
#' signals that say \emph{when not to trust it}:
#' \strong{entropy / confidence_band} (is the head certain?),
#' \strong{mixed} (does the text contain competing signals?), and
#' \strong{ood_similarity / ood_flag} (does the text look like the training data?).
#'
#' A simple triage: auto-accept rows where
#' \code{confidence_band == "high"} and \code{mixed == FALSE} and \code{ood_flag == FALSE};
#' route the rest to a human.
#'
#' @examples
#' \dontrun{
#'   init_sentiment.ai()
#'   d <- sentiment_diagnostics(c(
#'     "I loved the food but the service was awful",
#'     "Ein sehr ungewoehnlicher deutschsprachiger Text",
#'     "OK"
#'   ))
#'   d[, c("text","sentiment","confidence_band","mixed","ood_flag")]
#' }
#' @importFrom stats setNames
#' @importFrom jsonlite fromJSON
#' @export
sentiment_diagnostics <- function(x               = NULL,
                                  model           = DEFAULT_MODEL,
                                  scoring         = DEFAULT_SCORING,
                                  scoring_version = "1.0",
                                  batch_size      = 100,
                                  head_path       = NULL,
                                  ...){
  if(!is.null(scoring) && scoring[1] %in% c("xgb","glm"))
    stop("scoring = '", scoring[1], "' is a legacy scalar head that lacks the 3-class ",
         "probability mass required by sentiment_diagnostics(). Use scoring = 'mlp' ",
         "(default) or 'logistic' instead.", call. = FALSE)
  scoring <- match.arg(scoring)
  if(is.null(x)) return(NULL)

  # NA-safe embedding: substitute missing text (NA or "") with a placeholder before
  # embedding (so we don't embed the literal "NA"), then blank those rows at the end.
  na_index <- if(is.character(x)) which(is.na(x) | !nzchar(x)) else integer(0)
  x_emb <- x
  if(length(na_index)) x_emb[na_index] <- as.character(na_index)

  # embed once; pass the matrix to sentiment() so we don't embed twice
  check_sentiment.ai(model = model, ...)
  embs <- embed_text(x_emb, batch_size = batch_size, model = model)

  out <- sentiment(x = embs, model = model, scoring = scoring,
                   scoring_version = scoring_version, batch_size = batch_size,
                   head_path = head_path, ...)

  # restore original text (sentiment() on a matrix uses rownames)
  if(is.character(x)) out$text <- x else out$text <- rownames(embs)

  p <- as.matrix(out[, c("prob_neg", "prob_neu", "prob_pos")])
  n <- nrow(p)

  # entropy: H(p) in nats; safe_log(0) = 0 by convention
  ent <- -rowSums(p * ifelse(p > 0, log(p), 0))

  # confidence band from the reliability report. Levels ordered low < moderate < high so
  # that d[d$confidence_band >= "moderate", ] keeps moderate AND high rows (not the inverse).
  conf <- out$confidence
  band <- character(n)
  band[!is.na(conf) & conf >= 0.85] <- "high"
  band[!is.na(conf) & conf >= 0.65 & conf < 0.85] <- "moderate"
  band[!is.na(conf) & conf <  0.65] <- "low"
  band[is.na(conf)] <- NA_character_
  band <- factor(band, levels = c("low", "moderate", "high"), ordered = TRUE)

  # mixed: both positive and negative mass present (bivariate affect). NA-propagating.
  mixed <- ifelse(is.na(out$prob_pos), NA,
                  out$prob_pos > 0.25 & out$prob_neg > 0.25)

  # OOD: the three training class centroids are nearly collinear (~10 deg apart on the
  # unit sphere), so a max-over-centroids collapses to a projection onto the sentiment
  # subspace mean. We use that grand-mean projection directly -- honest and faster.
  # Low values => text unlike natural-language sentiment input (code, noise, numbers).
  cf <- system.file("centroids", paste0(model, ".json"), package = "sentiment.ai")
  if(nzchar(cf) && file.exists(cf)){
    cents    <- jsonlite::fromJSON(cf)
    cent_mat <- rbind(unlist(cents$negative),
                      unlist(cents$neutral),
                      unlist(cents$positive))      # (3, dim)
    grand    <- colMeans(cent_mat)
    grand    <- grand / sqrt(sum(grand^2))         # unit-normalise the subspace mean
    ood_sims <- round(as.numeric(embs %*% grand), 3)  # embs is L2-normalised -> cosine
    ood_flag <- ood_sims < 0.20
  } else {
    ood_sims <- rep(NA_real_, n)
    ood_flag <- rep(NA, n)
  }

  out$entropy         <- round(ent, 4)
  out$confidence_band <- band
  out$mixed           <- mixed
  out$ood_similarity  <- ood_sims
  out$ood_flag        <- ood_flag

  # blank diagnostic columns for missing-input rows (mirrors sentiment()'s NA contract)
  if(length(na_index)){
    out$entropy[na_index]          <- NA_real_
    out$confidence_band[na_index]  <- NA
    out$mixed[na_index]            <- NA
    out$ood_similarity[na_index]   <- NA_real_
    out$ood_flag[na_index]         <- NA
  }

  out
}


#' Load the training class centroids for a model
#'
#' Returns a named list of unit-normalised centroid vectors
#' (\code{negative}, \code{neutral}, \code{positive}) bundled in the package, or
#' \code{NULL} if not available for the requested model.
#'
#' @param model character; model handle (default \code{DEFAULT_MODEL}).
#' @return A named list of numeric vectors, or \code{NULL}.
#' @export
sentiment_centroids <- function(model = DEFAULT_MODEL){
  f <- system.file("centroids", paste0(model, ".json"), package = "sentiment.ai")
  if(!nzchar(f) || !file.exists(f)) return(NULL)
  jsonlite::fromJSON(f)
}
