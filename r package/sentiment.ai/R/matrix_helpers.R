
#' Cosine Similarity
#' @export
#' @rdname matrix_similarity
cosine <- function(x, y) return(crossprod(x, y)/sqrt(crossprod(x) * crossprod(y)))

#' Cosine Similarity
#' @export
#' @rdname matrix_similarity
cosine_lookup  <- function(i, y) apply(y, 1, function(j) cosine(i, j))

.cosine_match <- function(target, reference){

    # this could be quite large, so overwrite variable to free memory

    # Create data.table of cosine similarities, like so
    # | rn   |   a  |   b  |   c  |
    # | ---- | ---- | ---- | ---- |
    # |  a   | 1.00 | 0.50 | 0.25 |
    # |  B   | 0.40 | 0.90 | 0.15 |
    # |  ... |  ... |  ... |  ... |

    sim_dt <- data.table(t(apply(target, 1, cosine_lookup, y=reference)),
                        keep.rownames = TRUE)

    # Make long, then rank matchec by text input (rn)
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

    sim_dt <- data.table::melt(sim_dt, id.vars=("rn"), variable.name = "word")
    # rank matches per target (rnn) with frank() and by reference for speed
    sim_dt[, rank := data.table::frank(-value), by = .(rn)]

    return(sim_dt)
}
