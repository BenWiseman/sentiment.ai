#' Simple Sentiment Scores
#'
#' Uses simple preditive models on embeddings to provide probability of positive
#' score (rescaled to -1:1 for consistency with other packages).
#'
#' @param x A plain text vector or column name if data is supplied.
#'        If you know what you're doing, you can also pass in a pre-computed
#'        numeric embedding matrix.
#' @param model character; embedding model handle (default \code{"e5-small"}). See
#'        \code{\link{init_sentiment.ai}} for the options. This is NOT a
#'        tensorflow-hub name.
#' @param scoring scoring head to use: \code{"mlp"} (default) or \code{"logistic"}
#'        -- small pure-R JSON heads bundled in the package (no xgboost, no
#'        TensorFlow at score time). \code{"xgb"}/\code{"glm"} are still accepted for
#'        legacy heads but are not the default.
#' @param scoring_version Scoring head version: \code{"2.0"} (default, improved
#'        calibration) or \code{"1.0"} (legacy). Both ship inside the package.
#' @param batch_size Size of batches to use. Larger numbers will be faster than
#'        smaller numbers, but do not exhaust your system memory!
#' @param head_path optional path to a custom JSON scoring head, bypassing the bundled
#'        heads. For advanced use (e.g. scoring with a head you trained yourself).
#'
#' @return numeric vector, one score per element of \code{x}, rescaled to
#'         \code{[-1, 1]} (about 1 = positive, about -1 = negative).
#'
#' @inheritParams install_sentiment.ai
#'
#' @description
#' Uses a small bundled scoring head (a multilayer perceptron by default) over text
#' embeddings to return a score where numbers closer to 1 are more positive and
#' numbers closer to -1 are more negative.
#'
#' @examples
#' \dontrun{
#' # one-time setup (TensorFlow-free), then load the default e5-small model
#' # install_sentiment.ai(envname = "r-sentiment-ai")
#' init_sentiment.ai(model = "e5-small")
#'
#' sentiment_score(c("I love this!", "this is terrible"))
#'
#' # score a column of text and compare to known labels
#' scores  <- sentiment_score(airline_tweets$text)            # e5-small + mlp
#' pos_neg <- factor(airline_tweets$airline_sentiment,
#'                   levels = c("negative", "neutral", "positive"))
#' pos_neg <- (as.numeric(pos_neg) - 1) / 2
#' cosine(scores, pos_neg)
#' }
#'
#' @importFrom roperators
#'             chr
#' @export
sentiment_score <- function(x          = NULL,
                            model      = DEFAULT_MODEL,
                            scoring    = DEFAULT_SCORING,
                            scoring_version = c("2.0", "1.0"),
                            batch_size = 100,
                            head_path  = NULL,
                            ...){

  # setup everything (don't use arg.match for model ...)
  model           <- model[1]
  scoring         <- match.arg(scoring, c("mlp", "logistic", "xgb", "glm"))
  scoring_version <- match.arg(scoring_version)

  # if x is missing, return NOTHING
  if(is.null(x)){
    return(NULL)
  }

  # replace missing values with index numbers (can't handle missing)
  na_index    <- which(is.na(x))

  # opt-in end-to-end transformer classifier (RoBERTa / XLM-R): no embedding and no
  # JSON head -- the transformer returns neg/neu/pos directly, collapsed here to the
  # same [-1, 1] score. A precomputed embedding matrix is meaningless for this backend.
  if(model_class(model) == "classifier"){
    if(!is.character(x))
      stop("model '", model, "' is an end-to-end classifier; pass text, not a ",
           "precomputed embedding matrix.", call. = FALSE)
    x[na_index] <- as.character(na_index)
    p      <- classifier_probs(x, model, ...)
    scores <- unname(p[, "prob_pos"] - p[, "prob_neg"])
    scores[na_index] <- NA
    return(scores)
  }

  # calculate text embeddings if x is text
  # else if x looks like numeric embedding matrix, pass it through as-is
  if(is.character(x)){
    # replace NA in x with token text (will revert to NA after applying model)
    x[na_index] <- as.character(na_index)

    # activate environment
    check_sentiment.ai(model = model, ...)

    #  apply embedding
    text_embed  <- embed_text(x, batch_size, model)

  } else if(is.matrix(x)){
    # x is a precomputed embedding matrix; validate its width against the model
    exp_dim <- model_dims[[model]]
    if(!is.null(exp_dim) && ncol(x) != exp_dim){
      stop(sprintf("embedding matrix has %d columns but model '%s' expects %d",
                   ncol(x), model, exp_dim), call. = FALSE)
    }
    x[na_index] <- 0 # not passing NA to python!
    text_embed  <- x
  }


  # fetch the scoring head only after the backend is validated and the text is embedded
  # -- shipped heads resolve locally; un-shipped models would hit the network, so this
  # must come AFTER the legacy gate fires in check_sentiment.ai().
  install_scoring_model(model, scoring, scoring_version, ...)

  # score the embeddings -> [-1, 1] (3-class softprob collapses to P(pos)-P(neg))
  scores <- find_sentiment_score(embeddings = text_embed,
                                 scoring    = scoring,
                                 scoring_version = scoring_version,
                                 model      = model,
                                 head_path  = head_path)

  # add back nas
  scores[na_index] <- NA

  return(scores)
}

#' Tidy sentiment with the full three-class signal
#'
#' Like [sentiment_score()], but instead of collapsing to one number it returns the whole
#' picture the scoring head computes: the per-row probability of each class plus a derived
#' scalar. Reach for this when you need the neutral mass, a class label, or a confidence to
#' triage rows for human review.
#'
#' @inheritParams sentiment_score
#'
#' @return A \code{data.frame} with one row per input and columns: \code{text};
#'   \code{sentiment} (= \code{prob_pos - prob_neg}, in \code{[-1, 1]}); \code{prob_neg},
#'   \code{prob_neu}, \code{prob_pos} (the head's calibrated class probabilities --
#'   temperature-scaled, ECE ~ 0.01-0.02 on the held-out test -- summing to 1);
#'   \code{class} (an ordered factor negative < neutral < positive -- the
#'   most probable class); and \code{confidence} (the probability of that class). Missing
#'   inputs yield \code{NA} rows.
#'
#' @description
#' Returns the head's three-class probabilities (negative / neutral / positive) plus a
#' class label and a confidence. Only the bundled JSON heads (\code{"mlp"}, the default,
#' or \code{"logistic"}) expose probabilities; legacy scorers stay scalar-only via
#' [sentiment_score()].
#'
#' @examples
#' \dontrun{
#'   init_sentiment.ai(model = "e5-small")
#'   sentiment(c("I love this!", "it's fine", "this is terrible"))
#' }
#' @export
sentiment <- function(x               = NULL,
                      model           = DEFAULT_MODEL,
                      scoring         = DEFAULT_SCORING,
                      scoring_version = c("2.0", "1.0"),
                      batch_size      = 100,
                      head_path       = NULL,
                      ...){
  model   <- model[1]
  if(!is.null(scoring) && scoring[1] %in% c("xgb","glm"))
    stop("scoring = '", scoring[1], "' is a legacy scalar head that lacks the 3-class ",
         "probability mass required by sentiment(). Use scoring = 'mlp' (default) or ",
         "'logistic' instead.", call. = FALSE)
  scoring <- match.arg(scoring, c("mlp", "logistic", "xgb", "glm"))
  scoring_version <- match.arg(scoring_version)

  if(is.null(x)) return(NULL)

  na_index <- which(is.na(x))

  # opt-in end-to-end transformer classifier: the 3-class probabilities come straight
  # from the transformer (no embedding, no JSON head, and no e5 hate/mixed/style flags,
  # which need the embedding space).
  if(model_class(model) == "classifier"){
    if(!is.character(x))
      stop("model '", model, "' is an end-to-end classifier; pass text, not a ",
           "precomputed embedding matrix.", call. = FALSE)
    text_col    <- x
    x[na_index] <- as.character(na_index)
    p    <- classifier_probs(x, model, ...)
    lvls <- c("negative", "neutral", "positive")
    cls  <- factor(lvls[max.col(p, ties.method = "first")], levels = lvls,
                   ordered = TRUE)
    out  <- data.frame(
      text       = text_col,
      sentiment  = unname(p[, "prob_pos"] - p[, "prob_neg"]),
      prob_neg   = unname(p[, "prob_neg"]),
      prob_neu   = unname(p[, "prob_neu"]),
      prob_pos   = unname(p[, "prob_pos"]),
      class      = cls,
      confidence = unname(apply(p, 1, max)),
      stringsAsFactors = FALSE
    )
    if(length(na_index)){
      out[na_index,
          c("sentiment", "prob_neg", "prob_neu", "prob_pos", "confidence")] <- NA
      out$class[na_index] <- NA
    }
    return(out)
  }

  if(is.character(x)){
    text_col    <- x                              # keeps original NAs (copy-on-modify)
    x[na_index] <- as.character(na_index)         # placeholder -> reverts to NA below
    check_sentiment.ai(model = model, ...)
    embeddings  <- embed_text(x, batch_size, model)
  } else if(is.matrix(x)){
    exp_dim <- model_dims[[model]]
    if(!is.null(exp_dim) && ncol(x) != exp_dim){
      stop(sprintf("embedding matrix has %d columns but model '%s' expects %d",
                   ncol(x), model, exp_dim), call. = FALSE)
    }
    text_col    <- rownames(x)
    x[na_index] <- 0
    embeddings  <- x
  } else {
    stop("x must be a character vector or a precomputed numeric embedding matrix.",
         call. = FALSE)
  }

  install_scoring_model(model, scoring, scoring_version, ...)
  p <- find_sentiment_probs(embeddings, scoring, scoring_version, model,
                            head_path = head_path)

  lvls <- c("negative", "neutral", "positive")
  cls  <- factor(lvls[max.col(p, ties.method = "first")], levels = lvls, ordered = TRUE)
  out  <- data.frame(
    text       = if(is.null(text_col)) seq_len(nrow(p)) else text_col,
    sentiment  = unname(p[, "prob_pos"] - p[, "prob_neg"]),
    prob_neg   = unname(p[, "prob_neg"]),
    prob_neu   = unname(p[, "prob_neu"]),
    prob_pos   = unname(p[, "prob_pos"]),
    class      = cls,
    confidence = unname(apply(p, 1, max)),
    stringsAsFactors = FALSE
  )

  # Post-processing flags from the SAME embedding -- only when aux heads ship for this
  # model (e5/openai do; legacy USE does not, so the flags are silently omitted). Mirrors
  # the Python package: hate_speech = P(hate) >= recall-0.90 threshold; mixed = neutral yet
  # matching the mixed-vs-neutral classifier; style = top writing style. Embedding-only:
  # an opt-in end-to-end transformer (no embedding) would skip this block.
  hate_h  <- load_aux_head("hate",  model)
  mixed_h <- load_aux_head("mixed", model)
  style_h <- load_aux_head("style", model)
  if(!is.null(hate_h)){
    p_hate            <- logistic_binary_prob(embeddings, hate_h)
    out$hate_speech   <- p_hate >= hate_h$recommended_threshold
    out$p_hate        <- unname(p_hate)
  }
  if(!is.null(mixed_h)){
    p_mixed           <- logistic_binary_prob(embeddings, mixed_h)
    out$mixed         <- as.character(cls) == "neutral" &
                         p_mixed >= mixed_h$recommended_threshold
  }
  if(!is.null(style_h)){
    style_p           <- multilabel_probs(embeddings, style_h)
    out$style         <- style_h$classes[max.col(style_p, ties.method = "first")]
  }

  # blank out missing rows (text already carries the original NA)
  if(length(na_index)){
    out[na_index, c("sentiment", "prob_neg", "prob_neu", "prob_pos", "confidence")] <- NA
    out$class[na_index] <- NA
    if(!is.null(hate_h)){ out$hate_speech[na_index] <- NA; out$p_hate[na_index] <- NA }
    if(!is.null(mixed_h)) out$mixed[na_index] <- NA
    if(!is.null(style_h)) out$style[na_index] <- NA
  }

  out
}

#' Sentiment Matching
#'
#' @inheritParams sentiment_score
#' @param phrases A named list of examples phrases with each element of the list
#'        being words/terms that are indications of the name of that element
#'        (such as positive words/terms under the name "positive" and negative
#'        words/terms under the name "negative", all within the same list).
#'
#' @return data.table with one row per input: \code{text}, \code{sentiment},
#'         \code{phrase}, \code{class}, and \code{similarity}.
#'
#' @description
#' Returns the same calibrated \code{sentiment} as \code{\link{sentiment_score}}, plus a
#' nearest-phrase explanation against tunable poles: for each input it reports the closest
#' example \code{phrase}, that phrase's pole (\code{class}), and the cosine
#' \code{similarity}. The poles only shape the explanation -- the \code{sentiment} value
#' does not depend on them.
#'
#' @examples
#' \dontrun{
#' # one-time setup (TensorFlow-free), then load the default e5-small model
#' # install_sentiment.ai(envname = "r-sentiment-ai")
#' init_sentiment.ai(model = "e5-small")
#'
#' # default poles
#' mod_match <- sentiment_match(airline_tweets$text)
#'
#' # or define what positive / negative mean for your domain (tunable poles)
#' mod_match <- sentiment_match(airline_tweets$text,
#'                              phrases = list(positive = c("great service", "on time"),
#'                                             negative = c("lost my bag", "rude staff")))
#'
#' pos_neg <- factor(airline_tweets$airline_sentiment,
#'                   levels = c("negative", "neutral", "positive"))
#' pos_neg <- (as.numeric(pos_neg) - 1) / 2
#' cosine(mod_match$sentiment, pos_neg)
#' }
#'
#' @importFrom data.table
#'             setkeyv
#' @export
sentiment_match <- function(x        = NULL,
                            phrases  = NULL,
                            model    = names(default_models),
                            scoring         = DEFAULT_SCORING,
                            scoring_version = c("2.0", "1.0"),
                            batch_size      = 100,
                            ...){

  # fix global variable declaration for using data.table (to pass CRAN checks)
  similarity <- text <- phrase <- id__temp__ <- rn <-
    word <- value <- sentiment <- target_order <- target <- NULL

  # what to do with:
  # text, batches

  # setup everything (don't use arg.match for model ...)
  model           <- model[1]
  scoring         <- match.arg(scoring, c("mlp", "logistic", "xgb", "glm"))
  scoring_version <- match.arg(scoring_version)

  # if x is missing, return NOTHING
  if(is.null(x)){
    return(NULL)
  }

  # the nearest-phrase explanation needs an embedding space, so the opt-in end-to-end
  # classifiers (which never embed) are unsupported here -- use an e5/openai model.
  if(model_class(model) == "classifier"){
    stop("sentiment_match() needs an embedding model for the pole comparison; '", model,
         "' is an end-to-end classifier. Use an e5/openai model (e.g. model = 'e5-base').",
         call. = FALSE)
  }

  # need at least two poles to have an axis (matches the Python package's contract)
  if(!is.null(phrases) && length(phrases) < 2L){
    stop("sentiment_match() needs at least two poles (e.g. positive/negative); got ",
         length(phrases), ".", call. = FALSE)
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

  # filter to top match, join to sentiment table (and add word:sentiment).
  # the sentiment column IS the head score -- pass model/scoring/version through so the
  # head matches the embedding space (a non-default model embeds at a different width
  # than the default head would expect).
  sentiments <- sentiment_score(text_embed, model = model,
                                scoring = scoring, scoring_version = scoring_version)
  match_table[, sentiment := sentiments]

  # add back NA/blank rows BY POSITION (temp_id), never by a text key: the placeholder
  # token for a missing row is its row index as a string (e.g. "2"), which would
  # otherwise collide with a genuine input of the same value and blank the wrong row.
  setorderv(match_table, "temp_id")
  bad_pos <- as.integer(bad_index)
  if(length(bad_pos)){
    match_table[bad_pos, c("sentiment", "similarity", "phrase", "class") := NA]
    match_table[bad_pos, text := x_orig]
  }

  return(match_table[, .(text, sentiment, phrase, class, similarity)])
}


# Y. HELPER FUNCTIONS ==========================================================

# (internal) End-to-end transformer-classifier probabilities: an (n, 3) matrix with
# columns [prob_neg, prob_neu, prob_pos]. Ensures the requested classifier is the active
# backend (lazy first-use setup via check_sentiment.ai, plus an explicit re-init when
# switching models mid-session), then calls the reticulate pipeline sourced from
# inst/get_embedder.py (load_hf_classifier).
classifier_probs <- function(text, model, ...){
  if(model_class(model) != "classifier")
    stop("classifier_probs() called for non-classifier model '", model, "'.",
         call. = FALSE)

  check_sentiment.ai(model = model, ...)          # first-use wizard / lazy init

  env <- sentiment.ai::sentiment.env
  if(is.null(env$classify) || !identical(env$classifier_model, model)){
    init_sentiment.ai(model = model, ...)          # load / switch the classifier
    env <- sentiment.ai::sentiment.env
  }

  p <- as.matrix(env$classify(as_py_list(text)))
  colnames(p) <- c("prob_neg", "prob_neu", "prob_pos")
  rownames(p) <- text
  p
}

# Apply Model for Sentiment Score
#' @importFrom stats
#'             predict
#'             setNames
find_sentiment_score <- function(embeddings,
                                 scoring,
                                 scoring_version,
                                 model,
                                 head_path = NULL){

  # custom head: bypass system.file resolution entirely
  if(!is.null(head_path)){
    if(!file.exists(head_path))
      stop("head_path '", head_path, "' does not exist.", call. = FALSE)
    return(score_json_head(embeddings, head_path))
  }

  # locate the scoring object: inst/scoring/<scoring>/<version>/<model>.{xgb,csv}
  score_dir  <- file.path(system.file("scoring", package = utils::packageName()),
                          scoring,
                          scoring_version)
  model_path <- file.path(score_dir, model[1])

  # v2 default: a pure-R JSON head (MLP or multinomial logistic) that ships inside
  # the package and returns the [-1, 1] score directly via score_json_head().
  if(scoring %in% c("mlp", "logistic")){
    return(score_json_head(embeddings, paste0(model_path, ".json")))
  }

  n          <- nrow(embeddings)

  if(scoring == "glm"){
    # legacy logistic GLM: CSV of (param, weight); yields P(positive)
    csv     <- utils::read.csv(paste0(model_path, ".csv"))
    weights <- csv[, 2]
    names(weights) <- csv[, 1]
    p_pos   <- c(1 / (1 + exp(-cbind(1, embeddings) %*% weights)))
    score   <- (p_pos - 0.5) * 2

  } else {  # xgb (legacy, opt-in: xgboost is Suggested, not a hard dependency)
    if(!requireNamespace("xgboost", quietly = TRUE)){
      stop("scoring = \"xgb\" is a legacy option and needs the 'xgboost' package. ",
           "Install it with install.packages(\"xgboost\"), or use the default ",
           "scoring = \"mlp\", which ships in the package and needs no extra dependency.",
           call. = FALSE)
    }
    xgb_model <- xgboost::xgb.load(paste0(model_path, ".xgb"))
    preds     <- predict(object = xgb_model, newdata = embeddings)

    if(length(preds) == n * 3L){
      # v2 three-class multi:softprob -> columns [neg, neutral, pos].
      # Collapse to a single [-1, 1] score = P(pos) - P(neg); neutral pulls to 0.
      probs <- matrix(preds, ncol = 3, byrow = TRUE)
      score <- probs[, 3] - probs[, 1]
    } else {
      # legacy binary classifier: preds = P(positive)
      score <- (preds - 0.5) * 2
    }
  }

  setNames(object = score, nm = rownames(embeddings))
}

# Pure-R forward pass for a JSON scoring head (MLP or multinomial logistic), returning
# the full [neg, neutral, pos] probability matrix. No xgboost, no Python; the head ships
# inside the package (a few KB-MB of weights + a temperature). Classes are ordered
# [neg, neutral, pos]; weight matrices follow the torch convention [out, in].
#' @importFrom jsonlite fromJSON
score_json_probs <- function(embeddings, path){
  # clear error when a (model, scoring, version) combination ships no head -- e.g.
  # scoring = "logistic" for openai: only "mlp" ships for openai; "logistic" ships for
  # e5-small / e5-base. Without this the failure is a cryptic jsonlite read error.
  if(!file.exists(path)){
    stop("no scoring head bundled at '", basename(path), "'. The 'logistic' head ships ",
         "for e5-small / e5-base only; use scoring = 'mlp' (the default), which ships ",
         "for all e5 and openai models.", call. = FALSE)
  }
  h <- jsonlite::fromJSON(path, simplifyVector = TRUE,
                          simplifyDataFrame = FALSE, simplifyMatrix = TRUE)

  # serve-time guard: the head's input width must match the embedding space, so a
  # head/model mismatch fails loudly here instead of as a cryptic matmul error.
  in_dim <- if(identical(h$type, "mlp")) ncol(h$layers[[1]]$W) else ncol(h$coef)
  if(!is.null(in_dim) && !is.na(in_dim) && ncol(embeddings) != in_dim){
    stop(sprintf(paste0("scoring head expects %d-dim embeddings but got %d -- the head ",
                        "and the embedding model do not match (head: %s)."),
                 in_dim, ncol(embeddings), basename(path)), call. = FALSE)
  }

  if(identical(h$type, "mlp")){
    z  <- embeddings
    nL <- length(h$layers)
    for(i in seq_len(nL)){
      z <- sweep(z %*% t(h$layers[[i]]$W), 2, h$layers[[i]]$b, `+`)
      if(i < nL) z[z < 0] <- 0          # ReLU on hidden layers only
    }
  } else {                              # multinomial logistic
    z <- sweep(embeddings %*% t(h$coef), 2, h$intercept, `+`)
  }

  if(!is.null(h$T)) z <- z / h$T         # temperature scaling
  z <- z - apply(z, 1, max)              # numerically stable softmax
  p <- exp(z); p <- p / rowSums(p)       # columns: [neg, neutral, pos]
  colnames(p) <- c("prob_neg", "prob_neu", "prob_pos")
  rownames(p) <- rownames(embeddings)
  p
}

# Collapse the 3-class head to the [-1, 1] scalar score = P(pos) - P(neg).
score_json_head <- function(embeddings, path){
  p <- score_json_probs(embeddings, path)
  setNames(object = p[, "prob_pos"] - p[, "prob_neg"], nm = rownames(p))
}

# 3-class probabilities for the JSON heads (mlp / logistic) -- the basis for sentiment().
# Legacy scalar scorers (xgb / glm) do not expose a calibrated neutral mass, so the
# probability surface is JSON-head only.
find_sentiment_probs <- function(embeddings, scoring, scoring_version, model,
                                 head_path = NULL){
  if(!is.null(head_path)){
    if(!file.exists(head_path))
      stop("head_path '", head_path, "' does not exist.", call. = FALSE)
    return(score_json_probs(embeddings, head_path))
  }
  if(!scoring %in% c("mlp", "logistic")){
    stop("the 3-class probability output (sentiment()) needs a JSON scoring head ",
         "('mlp' or 'logistic'); got '", scoring, "'.", call. = FALSE)
  }
  score_dir <- file.path(system.file("scoring", package = utils::packageName()),
                         scoring, scoring_version)
  score_json_probs(embeddings, paste0(file.path(score_dir, model[1]), ".json"))
}


# AUX HEADS (hate / mixed / style post-processing flags) =======================
# Small per-encoder flag heads applied to the SAME embedding the mainline head
# scored, mirroring the Python package (sentimentai/_scoring.py). They ship in
# inst/scoring/auxiliary/<kind>_<model>.json and are the basis for the flag columns in
# sentiment(). logistic_binary -> P(positive_class); mlp_multilabel -> per-style
# sigmoids. Missing for a model (e.g. legacy USE) => the flag is silently omitted.

# Locate an aux head; returns "" (system.file's not-found sentinel) if absent.
resolve_aux_head <- function(kind, model){
  system.file("scoring", "auxiliary", paste0(kind, "_", model[1], ".json"),
              package = utils::packageName())
}

# Read an aux head if it ships for this model, else NULL.
#' @importFrom jsonlite fromJSON
load_aux_head <- function(kind, model){
  path <- resolve_aux_head(kind, model)
  if(!nzchar(path) || !file.exists(path)) return(NULL)
  jsonlite::fromJSON(path, simplifyVector = TRUE,
                     simplifyDataFrame = FALSE, simplifyMatrix = TRUE)
}

aux_sigmoid <- function(z) 1 / (1 + exp(-z))

# P(positive_class) for a logistic_binary aux head: sigmoid(X %*% coef + intercept).
# Returns a length-nrow(embeddings) numeric vector. coef is a flat dim-vector, matching
# the Python head (X @ coef + b), so a head/embedding-width mismatch fails loudly here.
logistic_binary_prob <- function(embeddings, head){
  coef <- as.numeric(head$coef)
  if(ncol(embeddings) != length(coef)){
    stop(sprintf(paste0("aux head expects %d-dim embeddings but got %d -- the head and ",
                        "the embedding model do not match."),
                 length(coef), ncol(embeddings)), call. = FALSE)
  }
  as.numeric(aux_sigmoid(as.numeric(embeddings %*% coef) + head$intercept))
}

# Per-class sigmoid probabilities for an mlp_multilabel aux head (style). Same forward
# pass as the mlp mainline (z = z %*% t(W) + b, ReLU on hidden layers) but a SIGMOID
# (not softmax) output, so each class is independent. Returns an (n, n_classes) matrix.
multilabel_probs <- function(embeddings, head){
  z  <- embeddings
  nL <- length(head$layers)
  for(i in seq_len(nL)){
    z <- sweep(z %*% t(head$layers[[i]]$W), 2, head$layers[[i]]$b, `+`)
    if(i < nL) z[z < 0] <- 0            # ReLU on hidden layers only
  }
  p <- aux_sigmoid(z)
  if(!is.null(head$classes)) colnames(p) <- head$classes
  p
}

