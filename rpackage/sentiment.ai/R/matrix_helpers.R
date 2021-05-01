
#' Cosine Similarity
#' @export
#' @rdname matrix_similarity
cosine <- function(x, y) {
    return(crossprod(x, y)/sqrt(crossprod(x) * crossprod(y)))
}

#' Cosine Similarity Lookup
.cosine_lookup  <- function(i, y) {
    apply(y, 1, function(j) cosine(i, j))

}

#' Do Each relevant match
.match_each <- function(target, reference){
    # Slower, was hoping underlying cpp would help
    t(apply(target, 1, .cosine_lookup, y=reference))
}

#' Do all matches at once (slower)
.match_all <- function(target, reference){
    # Slower, was hoping underlying cpp would help
    # Can also try text2vec::sim2
    mx_all  <- cbind(t(target), t(reference))
    cos_all <- coop:::cosine.matrix(mx_all)
    cos_all[rownames(target), rownames(reference)]
}

#' ToDo add aproximate NN hash matching for speed on large data
.hash_match <- function(){
   # RANN already has c++ ANN lookup
   # eg  test_nn <- nn2(centers, matrix(c(test, rnorm(512)), byrow = TRUE, nrow = 2), k = 1)
}


# There is room to optimise this. Can lag a bit when target is large
# microbenchmark::microbenchmark(.match_all(target, reference),
#                               .match_each(target, reference))

.cosine_match <- function(target, reference){

    # TODO: explore hashing the reference table
    # ie reduce to local search only

    # this could be quite large, so overwrite variable to free memory

    # Create data.table of cosine similarities, like so
    # | rn   |   a  |   b  |   c  |
    # | ---- | ---- | ---- | ---- |
    # |  a   | 1.00 | 0.50 | 0.25 |
    # |  B   | 0.40 | 0.90 | 0.15 |
    # |  ... |  ... |  ... |  ... |

    sim_dt <- data.table(text2vec::sim2(target, reference), keep.rownames = TRUE)
    setnames(sim_dt, old = "rn.V1", new ="rn", skip_absent = TRUE)

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

    sim_dt <- data.table::melt(sim_dt, id.vars=("rn"), variable.name = "word")
    # rank matches per target (rnn) with frank() and by reference for speed
    sim_dt[, rank := data.table::frank(-value, ties.method = "first"), by = .(rn)]

    return(sim_dt)
}
