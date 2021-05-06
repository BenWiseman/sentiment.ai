#' main func
# Abstracted out -
# BANDAID: tensorflow or reticulate is being a shit when text is length 1
.bandaid <- function(x) if(length(x)==1) reticulate::r_to_py(list(x)) else x

.text_embed <- function(text, batch_size){

    # Step 5 in sentiment_() - text embeddings
    # This is a bit messy, banaids and memorty work-arounds
    # Need to account for batch size, by default it tries to do it all at once!
    # 1) run in batches of n size to account for RAM usage
    batches <- unique(1:(length(text)-1) %/% batch_size)

    # 2 ) Set up progress indication!
    talk <- length(text>batch_size)
    if(talk) cat("Model Running...")
    pb  <- txtProgressBar(min=0, max=max(batches)+1, char = "|", style = 3)

    # Initial batch
    text_embeddings <- as.matrix(sentiment.ai.embed(.bandaid(text[1:min(length(text), batch_size)])))
    if(talk) setTxtProgressBar(pb, batches[1]+1)

    # use data.table ':=' on transverse matrix to add by reference
    text_embeddings <- data.table(t(text_embeddings))

    # now go along the rest and add each batch to text_embeddings
    for(i in seq_along(batches[-1])){
        # Set bounds
        from <- min(length(text), 1+(i*batch_size))
        to   <- min(length(text), (i+1)*batch_size)

        # Make vectors into a list/data.frame (so can be added to data.table in :=)
        temp_embeddings <- data.frame(t(as.matrix(sentiment.ai.embed(.bandaid(text[from:to])))))

        # Add to container by reference
        text_embeddings[, `:=`(paste0("V", from:to), lapply(temp_embeddings, function(x) x))]

        # update progress
        if(talk) setTxtProgressBar(pb, i+1)
    }
    # Cool now set names and should be good to go!
    # Turn back into matrix
    text_embeddings <- t(as.matrix(text_embeddings))
    row.names(text_embeddings) <- text
    return(text_embeddings)
}

#' Needs to return a list of embeddings AND the category/sentiment of each row
.reference_embed <- function(positive=NULL, negative=NULL, model){

    # Retrieve defaults
    if(is.null(positive)) pos = rownames(default_embeddings[[model]]$positive) else pos = positive
    if(is.null(negative)) neg = rownames(default_embeddings[[model]]$negative) else neg = negative

    #if pre calculated embeddings exist, just load them!
    if(is.null(positive) && model %in%  c("en.large", "multi.large", "en", "multi")){
        pos_embedding <- default_embeddings[[model]]$positive
        # already mx with rownames!
    } else{
        pos_embedding <- as.matrix(sentiment.ai.embed(.bandaid(pos)))
        rownames(pos_embedding) <- pos
    }

    if(is.null(negative) && model %in%  c("en.large", "multi.large", "en", "multi")){
        neg_embedding <- default_embeddings[[model]]$negative
        # already mx with rownames!
    } else{
        neg_embedding <- as.matrix(sentiment.ai.embed(.bandaid(neg)))
        rownames(neg_embedding) <- neg
    }

    # Step 4 - return combined pos and neg embeddings
    embeddings <- rbind(pos_embedding, neg_embedding)

    lookup <- data.table(word = c(rownames(embeddings)),
                         sentiment = c(rep("positive", nrow(pos_embedding)),
                                       rep("negative", nrow(neg_embedding))),
                         key = "word")
    return(list(embeddings = embeddings,
                lookup     = lookup))
}

#' Apply model to embeddings to get sentiment score
.sentiment_probs <- function(embeddings, scoring, scoring_version, model){

    # Step 1: where is the scoring object located
    score_dir <- file.path(system.file("scoring", package = "sentiment.ai"), scoring, scoring_version)

    probs <- numeric(512)
    # Step 2: load & apply
    if(scoring == "glm"){
        # glms bve big when serialized (R also saves all the data ~ 500mb), so just saving the weights
        weights   <- readRDS(file.path(score_dir, paste0(model[1],  ".rds")))
        # extract glm coefs (i.e intercept and betas!)
        intercept <- weights[1]
        coefs     <- weights[2:length(weights)]
        # apply logistic regression (multiply each row with all coefs)
        prods <- t(apply(embeddings, 1, function(row) row * coefs))
        sums  <- rowSums(prods) + intercept
        odds  <- exp(sums)

        probs <- odds/(1+odds)

    } else if(scoring=="xgb"){
        require(xgboost)
        xgb_model <- xgb.load(file.path(score_dir, paste0(model[1],  ".xgb")))
        probs     <- predict(xgb_model, newdata = embeddings)
    }

    names(probs) = rownames(embeddings)
    return(probs)
}



#' Simpler Sentiment - give score ranging from -1 (negative) to +1 (positive)
#' This returns a single vector, and will run a little faster than sentiment_match or sentiment_model
#' Defaults to English-specific, change model to 'multi' for support of 16 languages from Universal Sentence Encoder Multilingual.
#' @param x a plain text vector or column name if data supplied
#' @param model embedding from tensorflow-hub. Shortcuts are en (english-large) and multi (multi-lingual-large)
#' @param scoring model used to score embedding. Can choose between a glm or xbg.
#' @param batch_size Compute embeddings in n batches. The higher the faster BUT be careful not to exhaust your system memory!
#' @param envname specify virtual environment for Reticulate
#' @param scoring_version Placeholder - future versions may include updates/improvements
#' @export
#' @rdname sentiment_easy
sentiment_easy <- function(x = NULL,
                           model    = c("en.large", "multi.large", "en", "multi"),
                           scoring  = c("xgb", "glm"),
                           scoring_version = "1.0",
                           batch_size = 100,
                           envname  = "r-sentiment-ai"
                           ){
    # Setup stuff
    model   <- model[1] # not arg match to allow manual force override from power user
    scoring <- match.arg(scoring)
    scoring_version <- match.arg(scoring_version)

    # Can't handle NAs, so replace with numbers and delete their scores at end
    if(is.null(x)) return(NULL)
    na_index <- is.na(x)
    x[which(na_index)] <- chr(which(na_index))

    # Activate env and Create embeder object (if needed)
    if(!exists("sentiment.ai.embed")){
        message("Preparing Model (this may take a while)\n Considder running sentiment.ai.init()")
        sentiment.ai.init(model = model, envname = envname)
    } else {
        message("sentiment.ai.embed found in environment.\n To change model call sentiment.ai.init again")}


    # $tep 3 - Text embeddings (needs batch option == messy, wrapped in func)
    text_embeddings <- .text_embed(x, batch_size)

    # Step 4 - Apply scoring model
    probs  <- .sentiment_probs(text_embeddings, scoring, scoring_version, model)
    scores <- (probs-0.5)*2
    scores
    # Restore NAs before return
    scores[which(na_index)] <- NA
    # Done!
    return(scores)

}

#' Sentiment Matching - give score AND explanation
#' This returns a single vector, and will run a little faster
#' @return data.table with sentiment score, topic match, and match score
#' @param x EITHER a plain text vector or column name if data supplied
#' @param positive Custom positive word or term to compare against. e.g. "happy", "high quality"
#' @param negative Custom Negative word or term to compare against. e.g. "unhappy", "low quality"
#' @param envname specify virtual environment for Reticulate
#' @param model embedding from tensorflow-hub
#' @export
#' @rdname sentiment_easy
sentiment_match <- function(x = NULL,
                            model    = c("en.large", "multi.large", "en", "multi"),
                            positive = default$positive,
                            negative = default$negative,
                            scoring  = c("xgb", "glm"),
                            scoring_version = "1.0",
                            batch_size = 100,
                            envname  = "r-sentiment-ai"
){
    # Setup stuff
    model <- model[1] # not arg match to allow manual force override from power user

    # Can't handle NAs, so replace with numbers and delete their scores at end
    if(is.null(x)) return(NULL)
    na_index <- is.na(x)
    x[which(na_index)] <- chr(which(na_index))

    # Activate env and Create embeder object (if needed)
    if(!exists("sentiment.ai.embed")){
        message("Preparing Model (this may take a while)\n Considder running sentiment.ai.init()")
        sentiment.ai.init(model = model, envname = envname)
    } else {
        message("sentiment.ai.embed found in environment.\n To change model call sentiment.ai.init again")}


    # Step 3 - Make lookup table of reference embeddings
    # in case user feeds in vector, rep positive and negative labels
    reference <- .reference_embed(positive, negative, model)

    # step 4 - Text embeddings (needs batch option == messy, wrapped in func)
    text_embeddings <- .text_embed(x, batch_size)

    # Step 5 - Vector similarity
    match <- .cosine_match(target    = text_embeddings,
                           reference = reference$embeddings)

    # filter to top match
    match <- match[rank == 1, .(text = rn, word, value)]
    # join to sentiment table (add word:sentiment)
    match <- reference$lookup[match, on="word", mult="first"]
    # Now invert value for negative! (positive for positive, negative for negative)
    # Do slightly fuzzy match in case user screws up custom positive/negative labels
    match[!grepl("positive", sentiment, ignore.case = TRUE), value := -value]

    # RETURN
    # Force order to be same as input!
    # Set key as text column and then return indexed by original text
    setkeyv(match, "text")
    out <- match[x, value]
    # Restore NAs before return
    out[which(na_index)] <- NA
    # Done!
    return(out)

}


#' @param x EITHER a plain text vector or column name if data supplied
#' @param data OPTIONAL dataframe or data.table with text
#' @param idcol OPTIONAL IF data supplied, use idcol to do shit
#' @param lexicon data.frame or data.table of words: sentiment (default is XXX)
#' @param envname specify virtual environment for Reticulate
#' @param model embedding from tensorflow-hub
#' @export
#' @rdname sentiment_match
sentiment_model <- function(x = NULL,
                            data = NULL,
                            idcol = NULL,
                            lexicon = NULL,
                            model   = "multi",
                            envname = "r-sentiment-ai"){

    # PLACEHOLDER
    # if x is numeric matrix, don't need to embed text, just model right away
    # fit custom model on pos, neg
    # give scores on custom model

}
