#' Sentiment AI Embedding Environment
#'
#' This is the embedding envirnoment for the sentiment.ai model. On package load,
#' this object should be NULL. When the model is initialized, this should be
#' updated based on the required embedding model. The embedding function will
#' be stored in the "f" slot of this environment.
#'
#' @export
sentiment.env <- new.env()
# backend flags: openai = API; st = sentence-transformers (v2 default); else legacy TF-Hub
sentiment.env$openai <- FALSE
sentiment.env$st <- FALSE
# embedding prefix applied R-side in embed_text (e5 wants "query: "); set at init.
sentiment.env$prefix <- ""
# which backend init selected: "st" | "openai" | "legacy" (NA until init).
sentiment.env$backend <- NA_character_
sentiment.env$parallel <- 0
