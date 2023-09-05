#' Simple Sentiment Scores
#'
#' Uses simple preditive models on embeddings to provide probability of positive
#' score (rescaled to -1:1 for consistency with other packages).
#'
#' @param x A plain text vector or column name if data is supplied.
#'        If you know what you're doing, you can also pass in a 512-D numeric
#'        embedding.
#' @param model An embedding name from tensorflow-hub, some of which are
#'        "en" (english large or not) and "multi" (multi-lingual large or not).
#' @param scoring Model to use for scoring the embedding matrix (currently
#'        either "xgb" or "glm").
#' @param scoring_version The scoring version to use, currently only 1.0, but
#'        other versions might be supported in the future.
#' @param batch_size Size of batches to use. Larger numbers will be faster than
#'        smaller numbers, but do not exhaust your system memory!
#'
#' @return numeric vector of length(x) containing a re-scaled sentiment probabilities.
#'
#' @inheritParams install_sentiment.ai
#'
#' @description
#' This uses a simple model (xgboost or glm) to return a simple predictive score,
#' where numbers closer to 1 are more positive and numbers closer to -1 are more
#' negative. This can be used to determine whether the sentiment is positive
#' or negative.
#'
#' @examples
#' \dontrun{
#' envname <- "r-sentiment-ai"
#'
#' # make sure to install sentiment ai (install_sentiment.ai)
#' # install_sentiment.ai(envname = envname,
#' #                      method  = "conda")
#'
#' # running the model
#' mod_xgb <- sentiment_score(x       = airline_tweets$text,
#'                            model   = "en.large",
#'                            scoring = "xgb",
#'                            envname = envname)
#' mod_glm <- sentiment_score(x       = airline_tweets$text,
#'                            model   = "en.large",
#'                            scoring = "glm",
#'                            envname = envname)
#'
#' # checking performance
#' pos_neg <- factor(airline_tweets$airline_sentiment,
#'                   levels = c("negative", "neutral", "positive"))
#' pos_neg <- (as.numeric(pos_neg) - 1) / 2
#' cosine(mod_xgb, pos_neg)
#' cosine(mod_glm, pos_neg)
#'
#' # you could also calculate accuracy/kappa
#'
#'
#' }
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

  # check and install scoring model
  install_scoring_model(model, scoring, scoring_version, ...)

  # note: what do we do for xgb/glm with custom models??

  # if x is missing, return NOTHING
  if(is.null(x)){
    return(NULL)
  }

  # replace missing values with index numbers (can't handle missing)
  na_index    <- which(is.na(x))

  # calculate text embeddings if x is text
  # else if x looks like numeric embedding matrix, pass it through as-is
  if(is.character(x)){
    # replace NA in x with token text (will revert to NA after applying model)
    x[na_index] <- as.character(na_index)

    # activate environment
    check_sentiment.ai(model = model, ...)

    #  apply embedding
    text_embed  <- embed_text(x, batch_size, model)

  } else if(is.matrix(x) && ncol(x)==512){
    # x looks like embedding already - no need to init the embedder
    # ! assumes user knows what they're doing!
    x[na_index] <- 0 # not passing NA to python!
    text_embed  <- x
  }


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
#' @param phrases A named list of examples phrases with each element of the list
#'        being words/terms that are indications of the name of that element
#'        (such as positive words/terms under the name "positive" and negative
#'        words/terms under the name "negative", all within the same list).
#'
#' @return data.table containing text, sentiment, phrase, class, and similarity.
#'
#' @description
#' Provides score and explanation, returns a single vector, and runs relatively
#' fast.
#'
#' @examples
#' \dontrun{
#' envname   <- "r-sentiment-ai"
#'
#' # make sure to install sentiment ai (install_sentiment.ai)
#' # install_sentiment.ai(envname = envname,
#' #                      method  = "conda")
#'
#' # running the model
#' mod_match <- sentiment_match(x       = airline_tweets$text,
#'                              model   = "en.large",
#'                              envname = envname)
#'
#' # checking performance
#' pos_neg <- factor(airline_tweets$airline_sentiment,
#'                   levels = c("negative", "neutral", "positive"))
#' pos_neg <- (as.numeric(pos_neg) - 1) / 2
#' cosine(mod_match$sentiment, pos_neg)
#'
#' # you could also calculate accuracy/kappa
#' }
#'
#' @importFrom data.table
#'             setkeyv
#' @export
sentiment_match <- function(x        = NULL,
                            phrases  = NULL,
                            model    = names(default_models),
                            batch_size = 100,
                            ...){

  # fix global variable declaration for using data.table (to pass CRAN checks)
  similarity <- text <- phrase <- id__temp__ <- rn <-
    word <- value <- sentiment <- target_order <- target <- NULL

  # what to do with:
  # text, batches

  # setup everything (don't use arg.match for model ...)
  model           <- model[1]

  # if x is missing, return NOTHING
  if(is.null(x)){
    return(NULL)
  }

  # replace missing values with index numbers (can't handle missing)
  na_index  <- as.character(which(is.na(x)))
  x_index   <- as.character(which(x == ""))
  bad_index <- c(na_index, x_index)
  x_orig    <- x[as.numeric(bad_index)] # use this later
  x[which(is.na(x))] <- na_index
  x[which(x =="")]   <- x_index

  # temp_id for duplicated
  temp_id   <- seq_along(x)

  # activate environment
  check_sentiment.ai(model = model, ...)

  # calculate text embeddings
  text_embed   <- embed_text(x, batch_size, model)

  # make lookup table of reference embeddings
  reference    <- embed_topics(phrases, model)

  # determine match_table between the embeddings
  match_table  <- cosine_match(target    = text_embed,
                               reference = reference$embeddings,
                               keep_target_order=TRUE)

  match_table  <- match_table[rank == 1,
                              .(temp_id = target_order,
                                text    = target,
                                phrase  = reference,
                                similarity)]

  match_table  <- reference$lookup[match_table,
                                   on   = c("phrase"),
                                   mult = "first"]

  # force order to be the same as input
  setkeyv(match_table, c("text", "temp_id"))
  key_dt      <- data.table(text    = x,
                            temp_id = temp_id,
                            key     = c("text", "temp_id"))

  match_table <- match_table[key_dt,]
  setkeyv(match_table, c("temp_id"))

  # filter to top match, join to sentiment table (and add word:sentiment)
  sentiments <- sentiment_score(text_embed)
  match_table[, sentiment := sentiments]

  # add back nas
  setkeyv(match_table, c("text"))
  match_table[bad_index, "sentiment"]  <- NA
  match_table[bad_index, "similarity"] <- NA
  match_table[bad_index, "phrase"]     <- NA
  match_table[bad_index, "class"]      <- NA
  match_table[bad_index, "text"]       <- x_orig

  setorderv(match_table, "temp_id")

  return(match_table[, .(text, sentiment, phrase, class, similarity)])
}


# Y. HELPER FUNCTIONS ==========================================================

# Apply Model for Sentiment Score
#' @importFrom stats
#'             predict
#'             setNames
find_sentiment_probs <- function(embeddings,
                                 scoring,
                                 scoring_version,
                                 model){

  # NOTE: scoring/scoring_version/model are limited and will BREAK if not
  #       specified correctly

  # find the scoring object (ONLY WORKS FOR CERTAIN scoring/scoring_version)
  score_dir <- file.path(system.file("scoring", package = utils::packageName()),
                         scoring,
                         scoring_version)

  # create a silly vector of 0s ??
  probs      <- numeric(512)
  model_path <- file.path(score_dir, model[1])

  if(scoring == "glm"){

    # pulling out the weights (too large if serialized!)
    # Simple CSV of param name & value to named number vec for multiplication
    csv     <- utils::read.csv(paste0(model_path, ".csv"))
    names   <- csv[,1]
    weights <- csv[,2]
    names(weights) <- names


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

