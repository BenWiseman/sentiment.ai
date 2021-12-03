#' Cosine Similarity
#'
#' @param x A numeric vector or matrix
#' @param y A numeric vector or matrix of the same dimensions as x
#'
#' @examples
#' n <- 1000
#' y <- matrix(rnorm(n * 512), ncol = 512)
#' x <- matrix(rnorm(n * 512), ncol = 512)
#'
#' microbenchmark::microbenchmark(
#'   cosine(x, y),
#'   text2vec::sim2(x, y),
#'   times = 10
#' )
#'
#' all.equal(cosine(x, y),
#'           text2vec::sim2(x, y))
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

#' @rdname matrix_similarity
#' @export
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

#' @importFrom data.table
#'             data.table
#'             setnames
cosine_match <- function(target, reference){

    # TODO: explore hashing the reference table
    # ie reduce to local search only

    # this could be quite large, so overwrite variable to free memory

    # Create data.table of cosine similarities, like so
    # | rn   |   a  |   b  |   c  |
    # | ---- | ---- | ---- | ---- |
    # |  a   | 1.00 | 0.50 | 0.25 |
    # |  B   | 0.40 | 0.90 | 0.15 |
    # |  ... |  ... |  ... |  ... |

    sim_dt <- data.table(cosine(target, reference),
                         keep.rownames = TRUE)

    # this only works if rn is a column of the data, right? uper specific!
    setnames(sim_dt,
             old = "rn.V1",
             new = "rn",
             skip_absent = TRUE)

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

    sim_dt <- data.table::melt(sim_dt,
                               id.vars = "rn",
                               variable.name = "word")

    # rank matches per target (rnn) with frank() and by reference for speed
    sim_dt[ ,
           rank := data.table::frank(-value, ties.method = "first"),
           by    = .(rn)]

    return(sim_dt)
}
