#' Simple Sentiment Scores
#'
#' Uses simple preditive models on embeddings to provide probability of positive
#' score (rescaled to -1:1 for consistency with other packages)
#'
#' @param x A plain text vector or column name if data is supplied.
#'        If you know what you're doing, you can also pass in a 512-D numeric embedding
#' @param model An embedding name from tensorflow-hub, some of which are
#'        "en" (english large or not) and "multi" (multi-lingual large or not).
#' @param scoring Model to use for scoring the embedding matrix (currently
#'        either "xgb" or "glm").
#' @param scoring_version The scoring version to use, currently only 1.0, but
#'        other versions might be supported in the future.
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
#'        being words/terms that are indications of the name of that element (
#'        such as positive words/terms under the name "positive" and negative
#'        words/terms under the name "negative", all within the same list)
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
#' cosine(mod_match, pos_neg)
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

  # indicate that the model is running?!
  message("Model Running...")

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

#' read embedding file
#'take json path, return single embedding object for specific model
read_embedding <- function(file, model = "en.large", version = NULL){

  # json matrices can't have rownames so need to:
  # 1) Get the correct embeddings for the given model - saved in json.rownames
  # 2) restore the dim names from the json's rowname and columnnames fields
  # 3) return eme: a list of two embedding matrices with terms as rownames

  x <- fromJSON(readLines(file))

  emb <- x[[model]]
  rownames(emb$positive) <- x$rownames$positive
  colnames(emb$positive) <- x$colnames$positive

  rownames(emb$negative) <- x$rownames$negative
  colnames(emb$negative) <- x$colnames$negative

  return(emb)
}

#' get default embedding
#' If it exists, return the object
#' If not, try downloading it
#' If download works, return object
#' Else return NULL (to be handles in embed_topics())
get_defualt_embedding <- function(model){

  emb <- NULL

  # to find the defaults for this version of sentiment.ai
  version  <- packageDescription("sentiment.ai", fields = "Version")
  pkg_path <- system.file(package = "sentiment.ai")
  emb_file <- file.path(pkg_path, "default_embeddings", paste0(version, ".json"))

  emb_exists <- file.exists(emb_file)

  # not there? try downloading it
  if(!emb_exists) emb_exists <- install_default_embeddings() #1 if successful, 0 if fail

  # double check - has downloaded and is where it *should* be!
  if(emb_exists & file.exists(emb_file)) emb <- read_embedding(emb_file, model=model)

  # now return
  return(emb)

}


# Create Pos/Neg Embeddings
embed_topics <- function(phrases = NULL,
                         model   = c("en.large", "multi.large", "en", "multi")){

  # fix global variable declaration for using data.table (to pass CRAN checks)
  phrase <- NULL

  # make vector of repeating phrase labels per entry
  class_to_vec <- function(phrases){
    # make sure phrases has names
    nms   <- names(phrases)

    if(length(nms) == 0) nms <- seq_along(phrases)

    rep(nms, times = lengths(phrases))
  }

  # DEFAULT - now needs to check
  # not supplied phrases & names match default models
  # and default pos/neg embedding has been installed. if not, make phrases pos/neg and embed_text on them
  if(is.null(phrases) && model[1] %in% names(default_models)) {

    default_embeddings <- get_defualt_embedding(model[1])

    # if NULL then use default phrases
    phrases  <- list(positive = sentiment.ai::default$positive,
                     negative = sentiment.ai::default$negative)

    # if defaults didn't exist, embed them
    if(is.null(default_embeddings)){
      default_embeddings <- lapply(X     = phrases,
                                   FUN   = embed_text,
                                   model = model[1])
    }

    # return and combine pos/neg embeddings
    # rbind - must be matrix!
    mx_embed <- do.call(rbind, default_embeddings)

  } else{
    # NOT DEFAULT, do custom
    if(!length(phrases)) return(NULL)

    custom_embeddings <- lapply(X     = phrases,
                                FUN   = embed_text,
                                model = model[1])

    mx_embed <- do.call(rbind, custom_embeddings)
  }


  lookup  <- data.table(phrase = unname(unlist(phrases)),
                        class  = class_to_vec(phrases),
                        key    = "phrase")

  # de-dupe
  lookup  <- lookup[!duplicated(phrase), ]

  # RETURN
  return(list(embeddings = mx_embed, lookup = lookup))
}

#' Create Text Embedding Matrix
#'
#' @description turns charactewr vector into length(text)x512 embedding matrix.
#' For power users. Requires init_sentiment.ai() to have been called!
#'
#' @param text character vector to be embedded. Notye that longer comments take longer
#' @param batch_size integer - how many to embed at once. Higher numbers are faster but use more memory.
#' @param model character - the embedding model to use (same as sentiment_score())
#' @importFrom data.table data.table
#'
#' @export
embed_text <- function(text,
                       batch_size = NULL,
                       model      = NULL){

  # INTERNAL FUNCTION NEEDED FOR sentiment_() #

  if(is.null(sentiment.ai::sentiment.env$embed)){
    warning("Embedding model: sentiment.env$embed not found!")
    create_error_text(paste0("Initiating an instance now with model = ", model),
                      "",
                      "If you have not run install_sentiment.ai() yet, this will probably cause an error!")
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
    this_embed <- sentiment.ai::sentiment.env$embed(as_py_list(text[this_inds]))
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

# Z. UTILITY FUNCTIONS =========================================================

#' as py list
#' because R to Python conversion doesn't work with list is of length 1
#' @param x character vector that is to be passed into tensorflowtext via reticulate
#' @export
as_py_list <- function(x){

  # do we need to convert this to python? reticulate::r_to_py
  if(length(x) <= 1){
    as.list(x)
  } else{
    x
  }
}
