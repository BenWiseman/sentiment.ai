#' main func
# Abstracted out -
#

.textSentiment <- function(text, batch_size){

    # Step 5 in sentiment_() - text embeddings
    # This is a bit messy, banaids and memorty work-arounds
    # BANDAID: tensorflow or reticulate is being a shit when text is length 1
    bandaid <- function(x) if(length(x)==1) reticulate::r_to_py(list(x)) else x

    # Need to account for batch size, by default it tries to do it all at once!
    # 1) run in batches of n size to account for RAM usage
    batches <- unique(1:(length(text)-1) %/% batch_size)

    # 2 ) Set up progress indication!
    cat("Model Running...")
    pb  <- txtProgressBar(min=0, max=max(batches)+1, char = "|", style = 3)

    # Initial batch
    text_embeddings <- as.matrix(sentiment.ai.embed(bandaid(text[1:min(length(text), batch_size)])))
    setTxtProgressBar(pb, batches[1]+1)

    # use data.table ':=' on transverse matrix to add by reference
    text_embeddings <- data.table(t(text_embeddings))

    # now go along the rest and add each batch to text_embeddings
    for(i in seq_along(batches[-1])){
        # Set bounds
        from <- min(length(text), 1+(i*batch_size))
        to   <- min(length(text), (i+1)*batch_size)

        # Make vectors into a list/data.frame (so can be added to data.table in :=)
        temp_embeddings <- data.frame(t(as.matrix(sentiment.ai.embed(bandaid(text[from:to])))))

        # Add to container by reference
        text_embeddings[, `:=`(paste0("V", from:to), lapply(temp_embeddings, function(x) x))]

        # update progress
        setTxtProgressBar(pb, i+1)
    }
    # Cool now set names and should be good to go!
    # Turn back into matrix
    text_embeddings <- t(as.matrix(text_embeddings))
}

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
                           batch_size = 100,
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

    # step 5 - Texe embeddings (messy, wrapped in func)
    text_embeddings <- .text_sentiment(text, batch_size)
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

