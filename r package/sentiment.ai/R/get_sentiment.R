#' main func

#' @param text EITHER a plain text vector or column name if data supplied
#' @param data OPTIONAL dataframe or data.table with text
#' @param idcol OPTIONAL IF data supplied, use idcol to do shit
#' @param vector.out Logical, return simple score vector OR dataframe with more details
#' @param lexicon data.frame or data.table of words: sentiment (default is XXX)
#' @param envname specify virtual environment for Reticulate
#' @param model embedding from tensorflow-hub
sentiment_plus <- function(text = NULL,
                           data = NULL,
                           idcol = NULL,
                           lexicon = NULL,
                           envname = "r-sentiment-ai",
                           model   = "https://tfhub.dev/google/universal-sentence-encoder-multilingual-large/3"){

    # DEBUUG
    text = c("The holocaust",
             "Bad taste in music",
             "The resturant smelled bad",
             "The aardvark ate a scone",
             "The resturant served human flesh",
             "Good taste in music",
             "And they lived happily ever after",
             "not good",
             "not bad",
             "Thanos won",
             "David Bowie and Bob Ross",
             "Good",
             "Evil",
             "Hitler",
             "Yeah nah not bad I reckon!",
             "My favorite show got cancelled after two seasons!",
             "Fox News",
             "War in iraq",
             "Fall of the Berlin Wall",
             "Dogs",
             "Terrorism"
             )

    # Step 1 - Activate environment
    activate_env(envname)

    # Step 2 - Create embeder object
    message("Preparing Model")
    reticulate::source_python("Python/get_embedder.py")
    embed = load_language_model(model)

    # Step 3 - Make lookup table of reference embeddings
    reference_table <- make_lookup_table(lexicon)

    # Step 4 - Generate reference embeddings
    message("Applying Model")
    reference_embeddings <- as.matrix(embed(reference_table$word))
    row.names(reference_embeddings) <- reference_table$word

    # Step 5 - parse text
    # TODO
    if(!is.null(data)){

    }

    # Step 6 - text embeddings
    text_embeddings <- as.matrix(embed(text))
    row.names(text_embeddings) <- text


    # Vector similarity
    message("Comparing Text with Lexicon")
    cosine <- function(x, y) return(crossprod(x, y)/sqrt(crossprod(x) * crossprod(y)))
    cosine_lookup  <- function(i) apply(reference_embeddings, 1, function(j) cosine(i, j))

    # this could be quite large, so overwrite variable to free memory
    sims <- data.table(t(apply(text_embeddings, 1, cosine_lookup)),
                       keep.rownames = TRUE)

    # Make long, then rank matchec by text input (rn)
    sims <- data.table::melt(sims, id.vars=("rn"), variable.name = "word")
    sims[, rank := rank(-value), by = .(rn)]

    # shouldn't be ties...
    sims <- sims[rank == 1, .(text = rn, word, value)]

    # join to sentiment table
    sims <- reference_table[sims, on="word", mult="first"]
    # Now invert value for negative!
    sims[!grepl("positive", sentiment), value := -value]

    # RETURN
    # TODO make sure return order matcher input order!!!
    return(sims[, .(text, sentiment, matched=word, value)])

}


#' Simpler sentiment - give score ranging from good to evil
#' @param text EITHER a plain text vector or column name if data supplied
#' @param data OPTIONAL dataframe or data.table with text
#' @param envname specify virtual environment for Reticulate
#' @param model embedding from tensorflow-hub
sentiment <- function(text = NULL,
                      data = NULL,
                      envname = "r-sentiment-ai",
                      model   = "https://tfhub.dev/google/universal-sentence-encoder-multilingual-large/3"){

    # DEBUUG
    text = c("The holocaust",
             "Bad taste in music",
             "The resturant smelled bad",
             "The aardvark ate a scone",
             "The resturant served human flesh",
             "Good taste in music",
             "And they lived happily ever after",
             "not good",
             "not bad",
             "Thanos won",
             "David Bowie and Bob Ross",
             "Good",
             "Evil",
             "Hitler",
             "Yeah nah not bad I reckon!",
             "My favorite show got cancelled after two seasons!",
             "Fox News",
             "War in iraq",
             "Fall of the Berlin Wall",
             "Dogs",
             "Terrorism"
    )

    # Step 1 - Activate environment
    activate_env(envname)

    # Step 2 - Create embeder object
    message("Preparing Model")
    reticulate::source_python("Python/get_embedder.py")
    embed = load_language_model(model)

    # Step 3 - Make lookup table of reference embeddings
    reference_table <- data.table(word = c("Good", "Evil"),
                                  sentiment = c("positive", "negative"),
                                  key = "word")

    # Step 4 - Generate reference embeddings
    message("Applying Model")
    reference_embeddings <- as.matrix(embed(reference_table$word))
    row.names(reference_embeddings) <- reference_table$word

    # Step 5 - parse text
    # TODO
    if(!is.null(data)){

    }

    # Step 6 - text embeddings
    text_embeddings <- as.matrix(embed(text))
    row.names(text_embeddings) <- text


    # Vector similarity
    message("Comparing Text with Lexicon")
    cosine <- function(x, y) return(crossprod(x, y)/sqrt(crossprod(x) * crossprod(y)))
    cosine_lookup  <- function(i) apply(reference_embeddings, 1, function(j) cosine(i, j))

    # this could be quite large, so overwrite variable to free memory
    sims <- data.table(t(apply(text_embeddings, 1, cosine_lookup)),
                       keep.rownames = TRUE)

    # Make long, then rank matchec by text input (rn)
    sims <- data.table::melt(sims, id.vars=("rn"), variable.name = "word")
    sims[, rank := rank(-value), by = .(rn)]

    # shouldn't be ties...
    sims <- sims[rank == 1, .(text = rn, word, value)]

    # join to sentiment table
    sims <- reference_table[sims, on="word", mult="first"]
    # Now invert value for negative!
    sims[!grepl("positive", sentiment), value := -value]

    # RETURN
    # TODO make sure return order matcher input order!!!
    return(sims$value)

}
