#' Simple Sentiment Scores
#'
#' Uses simple preditive models on embeddings to provide probability of positive
#' score (rescaled to -1:1 for consistency with other packages)
#'
#' @param x A plain text vector or column name if data is supplied
#' @param model An embedding name from tensorflow-hub, some of which are
#'        "en" (english large or not) and "multi" (multi-lingual large or not).
#' @param scoring Model to use for scoring the embedding matrix (currently
#'        either "xgb" or "glm").
#' @param batch_size Size of batches to use. Larger numbers will be faster than
#'        smaller numbers, but do not exhaust your system memory!
#' @inheritParams install_sentiment.ai
#'
#' @description
#' This uses a simple model (xgboost or glm) to return a simple predictive score,
#' where numbers closer to 1 are more positive and numbers closer to -1 are more
#' negative. This can be used to determine whether the sentiment is positive
#' or negative.
#'
#' @examples
#'
#' @importFrom roperators
#'             chr
#' @export
sentiment_score <- function(x          = NULL,
                            model      = names(default_models),
                            scoring    = c("xgb", "glm"),
                            scoring_version = "1.0",
                            batch_size = 100,
                            ...){

  # setup everything (don't use arg.match for model ...)
  model           <- model[1]
  scoring         <- match.arg(scoring)
  scoring_version <- match.arg(scoring_version)

  # note: what do we do for xgb/glm with custom models??

  # if x is missing, return NOTHING
  if(is.null(x)){
    return(NULL)
  }

  # replace missing values with index numbers (can't handle missing)
  na_index    <- which(is.na(x))
  x[na_index] <- chr(na_index)

  # activate environment
  check_sentiment.ai(model = model, ...)

  # calculate text embeddings
  text_embed  <- embed_text(x, batch_size)

  # find sentiment probabilities
  probs  <- find_sentiment_probs(embeddings = text_embed,
                                 scoring    = scoring,
                                 scoring_version = scoring_version,
                                 model      = model)
  scores <- (probs - .5) * 2

  # add back nas
  scores[na_index] <- NA

  return(scores)
}

#' Sentiment Matching
#'
#' @inheritParams sentiment_score
#' @param positive Custom positive words/terms to compare against
#'        (e.g., "happy", "high quality", ...)
#' @param negative Custom negative words/terms to compare against
#'        (e.g., "unhappy", "low quality", ...)
#'
#' @description
#' Provides score and explanation, returns a single vector, and runs relatively
#' fast.
#'
#' @importFrom data.table
#'             setkeyv
#' @export
sentiment_match <- function(x        = NULL,
                            model    = names(default_models),
                            positive = default$positive,
                            negative = default$negative,
                            batch_size = 100,
                            ...){

  # setup everything (don't use arg.match for model ...)
  model           <- model[1]

  # if x is missing, return NOTHING
  if(is.null(x)){
    return(NULL)
  }

  # replace missing values with index numbers (can't handle missing)
  na_index    <- which(is.na(x))
  x[na_index] <- chr(na_index)

  # activate environment
  check_sentiment.ai(model = model, ...)

    # 2 ) Set up progress indication!
    talk <- length(text) > batch_size
    if(talk) cat("Model Running...")
    if(talk) pb  <- txtProgressBar(min=0, max=max(batches)+1, char = "|", style = 3)

  # calculate text embeddings
  text_embed  <- embed_text(x, batch_size)

  # make lookup table of reference embeddings
  reference   <- embed_pos_neg(positive, negative, model)

  # determine similarity between the embeddings
  similarity  <- cosine_match(target    = text_embed,
                              reference = reference$embeddings)

  # filter to top match, join to sentiment table (and add word:sentiment)
  similarity  <- similarity[rank = 1,
                            .(text = rn, word, value)]
  similarity  <- reference$lookup[similarity, on = "word", mult = "first"]

  # invert value for negative (with slightly fuzzy match in case user types
  # positive/negative labels wrong)
  similarity[!grepl(x       = sentiment,
                    pattern = "positive",
                    ignore.case = TRUE),
             value := -value]

  # force order to be the same as input
  setkeyv(similarity, "text")
  scores      <- similarity[x, value]

  # add back nas
  scores[na_index] <- NA

  return(scores)
}

#' Model-Based Sentiment
#'
#' @inheritParams sentiment_score
#' @param data A data.frame or data.table with text
#' @param idcol If data is supplied, use this column to pick something?
#' @param lexicon A data.frame or data.table of words: sentiment (default is XXX)
#'
#' @description
#'
#' @export
sentiment_model <- function(x       = NULL,
                            data    = NULL,
                            idcol   = NULL,
                            lexicon = NULL,
                            model   = names(default_models),
                            ...){

  # PLACEHOLDER
  # if x is numeric matrix, don't need to embed text, just model right away
  # fit custom model on pos, neg
  # give scores on custom model

}

# Y. HELPER FUNCTIONS ==========================================================

#' Create Pos/Neg Embeddings
embed_pos_neg <- function(positive = NULL,
                          negative = NULL,
                          model    = "en.large"){

  # all models
  cur_models  <- names(default_models)
  first_embed <- default_embeddings[[cur_models[[1]]]]

  # - if pre-calculated embeddings exist, load them
  # - otherwise, calculate embeddings
  if(is.null(positive) && model %in% cur_models){
    pos_embed  <- default_embeddings[[model]]$positive
  } else{
    if(is.null(positive)){
      positive <- rownames(first_embed$positive)
    }
    pos_embed  <- embed_text(positive)
  }

  # - if pre-calculated embeddings exist, load them
  # - otherwise, calculate embeddings
  if(is.null(negative) && model %in% cur_models){
    neg_embed  <- default_embeddings[[model]]$negative
  } else{
    if(is.null(negative)){
      negative <- rownames(first_embed$negative)
    }
    neg_embed  <- embed_text(negative)
  }

  # return and combine pos/neg embeddings
  this_embed <- rbind(pos_embed,
                      neg_embed)
  lookup     <- data.table(word      = rownames(this_embed),
                           sentiment = c(rep("positive", nrow(pos_embed)),
                                         rep("negative", nrow(neg_embed))),
                           key       = "word")

  list(embeddings = this_embed,
       lookup     = lookup)
}

#' Create Text Embeddings
#'
#' @importFrom data.table data.table
embed_text <- function(text, batch_size = NULL){

  # INTERNAL FUNCTION NEEDED FOR sentiment_() #

  if(!exists("sentiment.ai_embed")){
    warning("Embedding model: sentiment.ai_embed not found!")
    cat(
      "
      Initiating an instance now with model = ", model,
      "
      If you have not ran install_sentiment.ai() yet this will probably cause an error!
      "
      )
    init_sentiment.ai(model = model)
  }

  if(is.null(batch_size)){
    batch_size <- length(text)
  }

  # divide data into batch sizes
  text_size <- length(text)
  batches   <- rep(1:ceiling(text_size / batch_size),
                   each       = batch_size,
                   length.out = text_size)
  batches   <- split(x = seq_along(text),
                     f = batches)

  # if we have more than one batch, we should talk
  n_batches <- length(batches)
  do_talk   <- n_batches > 1

  # add progress bar if we should chat
  if(do_talk){
    message("Model Running ...")
    pb <- utils::txtProgressBar(min   = 0,
                                max   = n_batches,
                                char  = "|",
                                style = 3)
  }

  # initialize final data.table thingy
  text_embed <- NULL

  # note: all of the transposing doesn't seem to be memory efficient ... is there
  #       a better way of doing some of this?

  for(this_batch in seq_len(n_batches)){

    # determine the current indices
    this_inds  <- batches[[this_batch]]

    # make vectors into a list/data.frame (so can be added to data.table in :=)
    this_embed <- sentiment.ai_embed(as_py_list(text[this_inds]))
    this_embed <- data.table(t(as.matrix(this_embed)))

    if(is.null(text_embed)){
      text_embed <- this_embed
    } else{
      text_embed[ , paste0("V", this_inds) := this_embed]
    }

    if(do_talk){
      utils::setTxtProgressBar(pb, this_batch)
    }
  }

  # set names and should be good to go (after turn into matrix)
  text_embed <- t(as.matrix(text_embed))
  rownames(text_embed) <- text

  return(text_embed)
}

#' Apply Model for Sentiment Score
find_sentiment_probs <- function(embeddings,
                                 scoring,
                                 scoring_version,
                                 model){

  # NOTE: scoring/scoring_version/model are limited and will BREAK if not
  #       specified correctly

  # find the scoring object (ONLY WORKS FOR CERTAIN scoring/scoring_version)
  score_dir <- file.path(system.file("scoring", package = packageName()),
                         scoring,
                         scoring_version)

  # create a silly vector of 0s ??
  probs      <- numeric(512)
  model_path <- file.path(score_dir, model[1])

  if(scoring == "glm"){

    # pulling out the weights (too large if serialized!)
    weights    <- readRDS(paste0(model_path, ".rds"))

    # add intercept to embeddings and calculating probs
    embeddings <- cbind(1, embeddings)
    probs      <- c(1 / (1 + exp(-embeddings %*% weights)))
  } else if(scoring == "xgb"){
    xgb_model  <- xgboost::xgb.load(paste0(model_path, ".xgb"))
    probs      <- predict(object  = xgb_model,
                          newdata = embeddings)
  }

  setNames(object = probs,
           nm     = rownames(embeddings))
}

# Z. UTILITY FUNCTIONS =========================================================

# conversion doesn't work with list is of length 1
as_py_list <- function(x){

  # do we need to convert this to python? reticulate::r_to_py
  if(length(x) <= 1){
    as.list(x)
  } else{
    x
  }
}
