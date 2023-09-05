#' Sentiment AI Embedding Environment
#'
#' This is the embedding envirnoment for the sentiment.ai model. On package load,
#' this object should be NULL. When the model is initialized, this should be
#' updated based on the required embedding model. The embedding function will
#' be stored in the "f" slot of this environment.
#'
#' @export
sentiment.env <- new.env()
# make flags for non-tf hub embedding models
sentiment.env$openai <- FALSE
sentiment.env$parallel <- 0
