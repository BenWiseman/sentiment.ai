#' Cosine Similarity
#'
#' @param x A numeric vector or matrix
#' @param y A numeric vector or matrix of the same dimensions as x
#'
#' @examples
#' \dontrun{
#' n <- 5
#' d <- 384  # embedding width (e.g. e5-small)
#' y <- matrix(rnorm(n * d), ncol = d)
#' x <- matrix(rnorm(n * d), ncol = d)
#'
#' all.equal(cosine(x, y),
#'           text2vec::sim2(x, y))
#' }
#'
#' @name matrix_similarity
NULL

#' @rdname matrix_similarity
#' @export
cosine <- function(x, y = NULL){

  # faster than text2vec::sim2
  tcrossprod(x = rescale(x),
             y = rescale(y))
}


# not exporting for now!
# @rdname matrix_similarity
# @export
rescale <- function(x){

  if(is.null(x)){
    return(NULL)
  } else if(!is.matrix(x)){
    x  <- rbind(x)
  }

  if(nrow(x) == 1){
    sx <- sum(x^2)
  } else{
    sx <- rowSums(x^2)
  }

  x / sqrt(sx)
}

#' cosine_match()
#'
#' @param target numeric matrix of j values where each row is one observation.
#'   Use row names as ID.
#' @param reference numeric matrix of j values where each row is one
#'   observation. Use row names as ID.
#' @param keep_target_order logical; include column indicating original row
#'   order of target matrix.
#' @param approx logical; when \code{TRUE} and the \code{RANN} package is
#'   available, uses an approximate k-d tree nearest-neighbour search instead
#'   of the full pairwise cosine matrix. Much faster when \code{reference} is
#'   large (thousands of rows). Only returns the rank-1 match per target row
#'   (versus all pairs for the exact path). Falls back to exact with a message
#'   if \code{RANN} is not installed. Default \code{FALSE}.
#'
#' @return data.table containing ranked (1 = top) pairwise similarities between
#'   target and reference. When \code{approx = TRUE} only rank-1 rows are
#'   returned.
#'
#' @importFrom data.table
#'             data.table
#'             setnames
#' @rdname matrix_similarity
#' @export
cosine_match <- function(target, reference,
                         keep_target_order = FALSE,
                         approx            = FALSE){

  # fix global variable declaration for using data.table (to pass CRAN checks)
  id__temp__ <- rn <- value <- target_order <- similarity <- ..columns <- NULL

  # columns has issues, so removing to prevent those issues!
  rm(..columns)

  # --- approximate nearest-neighbour path (opt-in, requires RANN) -----------
  if(approx){
    if(!requireNamespace("RANN", quietly = TRUE)){
      message("cosine_match(): approx=TRUE requires RANN; falling back to exact.")
      approx <- FALSE
    }
  }

  if(approx){
    # For unit-normalised vectors: L2^2 = 2 - 2*cos => cos = 1 - L2^2/2.
    # The e5 embeddings are already L2-normalised; rescale() is a safe no-op.
    tgt_norm <- rescale(target)
    ref_norm <- rescale(reference)

    # RANN::nn2: data = reference pool, query = target items
    nn <- RANN::nn2(data  = ref_norm,
                    query = tgt_norm,
                    k     = 1L)

    cos_sim <- as.numeric(1 - nn$nn.dists[, 1L]^2 / 2)
    cos_sim <- round(pmax(-1, pmin(1, cos_sim)), 6)   # clamp float noise

    tgt_names <- if(!is.null(rownames(target)))
                   rownames(target) else as.character(seq_len(nrow(target)))
    ref_names <- if(!is.null(rownames(reference)))
                   rownames(reference) else as.character(seq_len(nrow(reference)))

    result <- data.table::data.table(
      target     = tgt_names,
      reference  = ref_names[nn$nn.idx[, 1L]],
      similarity = cos_sim,
      rank       = 1L
    )
    if(keep_target_order) result[, target_order := seq_len(.N)]

    columns <- c("target", "reference", "similarity", "rank")
    if(keep_target_order) columns <- c(columns, "target_order")
    return(result[, ..columns])
  }
  # --- end approximate path --------------------------------------------------

  # TODO: explore hashing the reference table
  # ie reduce to local search only

  # this could be quite large, so overwrite variable to free memory

  # Create data.table of cosine similarities, like so
  # | rn   |   a  |   b  |   c  |
  # | ---- | ---- | ---- | ---- |
  # |  a   | 1.00 | 0.50 | 0.25 |
  # |  B   | 0.40 | 0.90 | 0.15 |
  # |  ... |  ... |  ... |  ... |


  # data.table keeping rownames from matrices (will be column called rn)
  sim_dt <- data.table(cosine(target, reference),
                       keep.rownames = TRUE)

  # if no rownames, improvise some!
  if(is.null(sim_dt$rn)) sim_dt[, rn := 1:.N]

  setnames(sim_dt, old = "rn", new = "target", skip_absent = TRUE)

  # Make long, then rank match by text input rowname (rn)
  # melt like so
  # | rn   |  word | value |
  # | ---- | ----- | ----- |
  # |  a   |   a   | 1.00  |
  # |  a   |   b   | 0.50  |
  # |  a   |   c   | 0.25  |
  # |  B   |   a   | 0.40  |
  # |  B   |   b   | 0.90  |
  # |  B   |   c   | 0.15  |
  # | ...  |  ...  |  ...  |

  sim_dt[, target_order := 1:.N]
  sim_dt <- data.table::melt(sim_dt,
                             id.vars=(c("target", "target_order")),
                             variable.name = "reference",
                             value.name = "similarity")

  # rank matches per target (rnn) with frank() and by reference for speed.
  # ties.method = "first" keeps a single deterministic rank-1 on exact-tie cosines
  # (the default "average" gives tied rows rank 1.5, so the rank==1 filter drops both
  # and the row falls through to NA) -- and matches Python's argmax (first-wins).
  sim_dt[, rank := data.table::frank(-similarity, ties.method = "first"),
         by = .(target, target_order)]

  # return prettier table
  columns <- c("target", "reference", "similarity", "rank")
  if(keep_target_order) columns <- c(columns, "target_order")

  return(sim_dt[, ..columns])
}
