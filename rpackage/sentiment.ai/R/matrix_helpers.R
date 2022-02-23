#' Cosine Similarity
#'
#' @param x A numeric vector or matrix
#' @param y A numeric vector or matrix of the same dimensions as x
#'
#' @examples
#' \dontrun{
#' n <- 5
#' y <- matrix(rnorm(n * 512), ncol = 512)
#' x <- matrix(rnorm(n * 512), ncol = 512)
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

#' ToDo add aproximate NN hash matching for speed on large data
hash_match <- function(){
   # RANN already has c++ ANN lookup
   # eg  test_nn <- nn2(centers, matrix(c(test, rnorm(512)), byrow = TRUE, nrow = 2), k = 1)
}


#' cosine_match()
#'
#' @param target numeric matrix of j values where each row is one observation. Use row names as ID
#' @param reference  numeric matrix of j values where each row is one observation. Use row names as ID
#' @param keep_target_order logical include column indicating original row order of target matrix
#'
#' @return data.table containing ranked (1 = top) pairwise similarities between target and reference
#'
#' @importFrom data.table
#'             data.table
#'             setnames
#' @rdname matrix_similarity
#' @export
cosine_match <- function(target, reference, keep_target_order=FALSE){

  # fix global variable declaration for using data.table (to pass CRAN checks)
  id__temp__ <- rn <- value <- target_order <- similarity <- ..columns <- NULL

  # columns has issues, so removing to prevent those issues!
  rm(..columns)

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

  # rank matches per target (rnn) with frank() and by reference for speed
  sim_dt[, rank := data.table::frank(-similarity), by = .(target, target_order)]

  # return prettier table
  columns <- c("target", "reference", "similarity", "rank")
  if(keep_target_order) columns <- c(columns, "target_order")

  return(sim_dt[, ..columns])
}
