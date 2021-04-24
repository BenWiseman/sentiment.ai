#' main func

#' @param text EITHER a plain text vector or column name if data supplied
#' @param data OPTIONAL dataframe or data.table with text
#' @param idcol OPTIONAL IF data supplied, use idcol to do shit
#' @param lexicon data.frame or data.table of words: sentiment (default is XXX)
#' @param envname specify virtual environment for Reticulate
#' @param model embedding from tensorflow-hub
sentiment_plus <- function(text = NULL,
                           data = NULL,
                           idcol = NULL,
                           lexicon = NULL,
                           envname = "r-sentiment-ai",
                           model   = "multilingual"){

    # Step 1 - Activate environment
    activate_env(envname)

    # Step 2 - Create embeder object
    if(!exists("sentiment.ai.embed")){
        message("Preparing Model")
        reticulate::source_python("Python/get_embedder.py")
        sentiment.ai.embed <- load_language_model(model)
    } else{
        message("sentiment.ai.embed found in environment.")

    }


    # Step 3 - Make lookup table of reference embeddings
    reference_table <- make_lookup_table(lexicon)

    # Step 4 - Generate reference embeddings
    message("Applying Model")
    reference_embeddings <- as.matrix(sentiment.ai.embed(reference_table$word))
    row.names(reference_embeddings) <- reference_table$word

    # Step 5 - parse text
    # TODO
    if(!is.null(data)){

    }

    # Step 6 - text embeddings
    text_embeddings <- as.matrix(sentiment.ai.embed(text))
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


#' Simpler sentiment - give score ranging from good to bad
#' This returns a single vector, and will run a little faster
#' @param text EITHER a plain text vector or column name if data supplied
#' @param positive Custom positive word or term to compare against. e.g. "happy", "high quality"
#' @param negative Custom Negative word or term to compare against. e.g. "unhappy", "low quality"
#' @param envname specify virtual environment for Reticulate
#' @param model embedding from tensorflow-hub
sentiment_easy <- function(text = NULL,
                           positive = "good",
                           negative = "bad",
                           envname  = "r-sentiment-ai",
                           model    = "multilingual"){
    # Step 1 - Activate environment
    activate_env(envname)

    # Step 2 - Create embeder object
    message("Preparing Model")
    reticulate::source_python("Python/get_embedder.py")
    embed = load_language_model(model)

    # Step 3 - Make lookup table of reference embeddings
    reference_table <- data.table(word = c(positive, negative),
                                  sentiment = c("positive", "negative"),
                                  key = "word")

    # Step 4 - Generate reference embeddings
    message("Applying Model")
    reference_embeddings <- as.matrix(embed(reference_table$word))
    row.names(reference_embeddings) <- reference_table$word


    # Step 5 - text embeddings
    text_embeddings <- as.matrix(embed(text))
    row.names(text_embeddings) <- text


    # Step 6 - Vector similarity
    message("Comparing Text with Lexicon")
    # funcs to compare vector similarity
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
    # Force order to be same as input!
    setkeyv(sims, "text")
    return(sims[text, value])

}
