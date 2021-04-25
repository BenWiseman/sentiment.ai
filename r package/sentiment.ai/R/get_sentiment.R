#' main func

#' @param text EITHER a plain text vector or column name if data supplied
#' @param data OPTIONAL dataframe or data.table with text
#' @param idcol OPTIONAL IF data supplied, use idcol to do shit
#' @param lexicon data.frame or data.table of words: sentiment (default is XXX)
#' @param envname specify virtual environment for Reticulate
#' @param model embedding from tensorflow-hub
#' @export
#' @rdname sentiment_plus
sentiment_plus <- function(text = NULL,
                           data = NULL,
                           idcol = NULL,
                           lexicon = NULL,
                           model   = "multi",
                           envname = "r-sentiment-ai"
                           ){

    # Step 1 - Activate environment
    activate_env(envname)

    # Step 2 - Create embeder object
    if(!exists("sentiment.ai.embed")){
        message("Preparing Model")
        reticulate::py_run_file(system.file("Python/get_embedder.py", package = "sentiment.ai"))
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
#' @export
#' @rdname sentiment_easy
sentiment_easy <- function(text = NULL,
                           positive = default$positive,
                           negative = default$negative,
                           model    = "en",
                           envname  = "r-sentiment-ai"
                           ){

    if(is.null(text)) return(NULL)
    if(any(is.na(text))){
        warning("NA in text detected. Be sure to filter these from your results!")
        text[is.na(text)] <- "NA"
    }


    # Step 1&2 - Activate env and Create embeder object
    if(!exists("sentiment.ai.embed")){
        message("Preparing Model (this may take a while)\n Considder running sentiment.ai.init()")
        sentiment.ai.init(model = model, envname = envname)
    } else{
        message("sentiment.ai.embed found in environment.\n To change model call sentiment.ai.init again")

    }

    # Step 3 - Make lookup table of reference embeddings
    # in case user feeds in vector, rep positive and negative labels
    reference_table <- data.table(word = c(positive, negative),
                                  sentiment = c(rep("positive", length(positive)),
                                                rep("negative", length(negative))),
                                  key = "word")

    # Step 4 - Generate reference embeddings
    reference_embeddings <- as.matrix(sentiment.ai.embed(reference_table$word))
    row.names(reference_embeddings) <- reference_table$word

    # Step 5 - text embeddings
    # BANDAID: tensorflow or reticulate is being a shit when text is length 1
    bandaid <- function(x) if(length(x)==1) reticulate::r_to_py(list(x)) else x
    text_embeddings <- as.matrix(sentiment.ai.embed(bandaid(text)))
    row.names(text_embeddings) <- text

    # Step 6 - Vector similarity
    match <- .cosine_match(target    = text_embeddings,
                           reference = reference_embeddings)

    # filter to top match
    match <- match[rank == 1, .(text = rn, word, value)]

    # join to sentiment table (add word:sentiment)
    match <- reference_table[match, on="word", mult="first"]

    # Now invert value for negative! (positive for positive, negative for negative)
    # Do slightly fuzzy match in case user screws up
    match[!grepl("positive", sentiment, ignore.case = TRUE), value := -value]

    # RETURN
    # Force order to be same as input!
    # Set key as text column and then return indexed by original text
    setkeyv(match, "text")
    return(match[text, value])

}

