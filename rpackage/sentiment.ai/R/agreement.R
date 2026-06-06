#' Agreement statistics between model scores and human labels
#'
#' Compares the model's continuous scores (or predicted class labels) against a set
#' of human-provided reference labels. Returns a named list of agreement statistics
#' framed against the kind of \emph{human-to-human} ceiling you would quote in a
#' methods section: Krippendorff's \eqn{\alpha}, quadratic-weighted \eqn{\kappa},
#' intraclass correlation, percent agreement, and Spearman correlation.
#'
#' @param scores Numeric vector of model scores in \code{[-1, 1]}, as returned by
#'   \code{\link{sentiment_score}}.  Alternatively pass the full tidy data frame
#'   from \code{\link{sentiment}} and the \code{sentiment} column is extracted.
#' @param labels Reference labels. Either:
#'   \itemize{
#'     \item a numeric vector of values in \code{c(-1, 0, 1)} (negative, neutral, positive),
#'     \item a character or factor vector with levels in
#'           \code{c("negative", "neutral", "positive")} (case-insensitive).
#'   }
#'   Must be the same length as \code{scores}.
#' @param positive_threshold Numeric; scores above this are predicted "positive"
#'   (default 1/3).
#' @param negative_threshold Numeric; scores below this are predicted "negative"
#'   (default -1/3). Scores between the two thresholds are predicted "neutral".
#'
#' @return A named list of class \code{"sentiment_agreement"}:
#'   \describe{
#'     \item{n}{Number of non-NA pairs used.}
#'     \item{spearman_r}{Spearman rank correlation between model score and numeric label.}
#'     \item{percent_agreement}{Proportion of rows where predicted class equals the reference
#'       label (0–1).}
#'     \item{weighted_kappa}{Quadratic-weighted Cohen's \eqn{\kappa} on the ordinal
#'       3-class labels (Suggests \code{irr}).}
#'     \item{krippendorff_alpha}{Krippendorff's \eqn{\alpha} (ordinal), treating the
#'       reference and the model as two independent raters (Suggests \code{irr}).}
#'     \item{icc}{Two-way mixed ICC(2,1) with 95\% CI for the numeric scores vs labels
#'       (Suggests \code{irr}).}
#'     \item{confusion_matrix}{3×3 table (true vs predicted, rows = true, cols = predicted).}
#'     \item{thresholds}{The positive/negative thresholds used for class assignment.}
#'   }
#'   Print the return value for a formatted summary.
#'
#' @description
#' Use this to audit whether the model's scores match human annotations well enough
#' for your use case, and to quote a defensible number in a methods section.  For context,
#' on multi-annotator corpora like GoEmotions and SemEval-2017-4, \emph{human-human}
#' weighted-\eqn{\kappa} on 3-class sentiment typically falls in the range 0.50–0.65.
#' A value of 0.70+ is strong; 0.40–0.70 is moderate; below 0.40 warrants caution.
#'
#' @examples
#' \dontrun{
#'   init_sentiment.ai()
#'   # compare model scores to manually annotated labels
#'   human_labels <- c("positive","negative","neutral","positive","negative")
#'   model_scores <- sentiment_score(c(
#'     "I love it", "Terrible", "It exists", "Amazing", "Awful"))
#'   ag <- sentiment_agreement(model_scores, human_labels)
#'   print(ag)
#' }
#' @export
sentiment_agreement <- function(scores,
                                labels,
                                positive_threshold =  1/3,
                                negative_threshold = -1/3){

  # accept a sentiment() data.frame: extract the 'sentiment' column
  if(is.data.frame(scores)){
    if(!"sentiment" %in% names(scores))
      stop("scores data.frame must contain a 'sentiment' column; found: ",
           paste(names(scores), collapse = ", "), call. = FALSE)
    scores <- scores$sentiment
  }
  scores <- as.numeric(scores)

  # normalise labels to -1 / 0 / 1 (numeric stays; strings map via .map_sentiment_label)
  if(is.character(labels) || is.factor(labels)){
    labels <- .map_sentiment_label(tolower(trimws(as.character(labels))))
  }
  labels <- as.numeric(labels)

  stopifnot(length(scores) == length(labels))

  # guard against out-of-range numeric labels (accepted values: -1, 0, 1)
  bad_labels <- labels[!is.na(labels) & !labels %in% c(-1, 0, 1)]
  if(length(bad_labels))
    stop("labels must be in c(-1, 0, 1); found: ",
         paste(sort(unique(bad_labels)), collapse = ", "), call. = FALSE)

  # drop NA pairs
  ok     <- !is.na(scores) & !is.na(labels)
  s      <- scores[ok]
  l      <- labels[ok]
  n      <- sum(ok)
  if(n < 2L) stop("need at least 2 non-NA pairs for agreement statistics", call. = FALSE)

  # predicted class from thresholds
  pred_num <- ifelse(s >  positive_threshold,  1L,
              ifelse(s < negative_threshold, -1L, 0L))

  # ---- stats that need no extra package --------------------------------------

  # Spearman correlation (score vs numeric label)
  sp_r <- suppressWarnings(stats::cor(s, l, method = "spearman"))

  # % agreement (class match)
  pct_agree <- mean(pred_num == l)

  # confusion matrix (true label rows, predicted label columns)
  lvls <- c(-1L, 0L, 1L)
  l_fac    <- factor(l,        levels = lvls, labels = c("negative","neutral","positive"))
  pred_fac <- factor(pred_num, levels = lvls, labels = c("negative","neutral","positive"))
  conf     <- table(true = l_fac, predicted = pred_fac)

  result <- list(
    n                    = n,
    spearman_r           = round(sp_r, 4),
    percent_agreement    = round(pct_agree, 4),
    weighted_kappa       = NA_real_,
    krippendorff_alpha   = NA_real_,
    icc                  = list(value = NA_real_, lower = NA_real_, upper = NA_real_),
    confusion_matrix     = conf,
    thresholds           = c(negative = negative_threshold,
                             positive = positive_threshold)
  )

  # ---- stats from irr (Suggests) ---------------------------------------------
  if(requireNamespace("irr", quietly = TRUE)){
    # quadratic-weighted kappa on ordinal 3-class (rater1=labels, rater2=model).
    # Pin both columns to 3 fixed levels so the weight matrix is always 3x3,
    # even when some classes are absent from this sample.
    rat_mat <- cbind(
      factor(match(l,        lvls), levels = 1:3),
      factor(match(pred_num, lvls), levels = 1:3)
    )
    k2 <- tryCatch(
      irr::kappa2(rat_mat, weight = "squared"),
      error = function(e){
        warning("weighted_kappa computation failed: ", conditionMessage(e),
                call. = FALSE)
        NULL
      })
    if(!is.null(k2)) result$weighted_kappa <- round(k2$value, 4)

    # Krippendorff alpha (ordinal): 2-rater matrix, each row = one rater
    kr_mat <- rbind(l, pred_num)
    kr <- tryCatch(
      irr::kripp.alpha(kr_mat, method = "ordinal"),
      error = function(e){
        warning("krippendorff_alpha computation failed: ", conditionMessage(e),
                call. = FALSE)
        NULL
      })
    if(!is.null(kr)) result$krippendorff_alpha <- round(kr$value, 4)

    # ICC(2,1) two-way mixed: continuous scores and numeric labels as two raters
    icc_df <- data.frame(label = l, score = s)
    ic <- tryCatch(
      irr::icc(icc_df, model = "twoway", type = "agreement", unit = "single"),
      error = function(e){
        warning("ICC computation failed: ", conditionMessage(e), call. = FALSE)
        NULL
      })
    if(!is.null(ic)){
      result$icc <- list(value = round(ic$value, 4),
                         lower = round(ic$lbound, 4),
                         upper = round(ic$ubound, 4))
    }
  }

  class(result) <- "sentiment_agreement"
  result
}


# internal: map label strings to -1/0/1
.map_sentiment_label <- function(x){
  out <- rep(NA_real_, length(x))
  out[x %in% c("positive","pos","1")]  <-  1
  out[x %in% c("neutral","neu","0")]   <-  0
  out[x %in% c("negative","neg","-1")] <- -1
  out
}


#' @export
print.sentiment_agreement <- function(x, ...){
  cat("sentiment.ai agreement statistics  (n =", x$n, ")\n")
  cat("-----------------------------------------------\n")
  cat("  Spearman r (score vs label)  :", x$spearman_r, "\n")
  cat("  Percent agreement (3-class)  :", .scales_pct(x$percent_agreement), "\n")
  if(!is.na(x$weighted_kappa)){
    cat("  Weighted kappa (quad, 3-cls) :", x$weighted_kappa, "\n")
    cat("\nHuman-human ceiling (indicative): weighted kappa ~0.50-0.65 on ",
        "GoEmotions / SemEval-2017-4.\n", sep = "")
  }
  if(!is.na(x$krippendorff_alpha))
    cat("  Krippendorff alpha (ordinal) :", x$krippendorff_alpha, "\n")
  if(!is.na(x$icc$value))
    cat("  ICC(2,1)                     :", x$icc$value,
        sprintf("  95%% CI [%.4f, %.4f]\n", x$icc$lower, x$icc$upper))
  cat("\nConfusion matrix (rows=true, cols=predicted):\n")
  print(x$confusion_matrix)
  invisible(x)
}

.scales_pct <- function(x) paste0(round(100 * x, 1), "%")
