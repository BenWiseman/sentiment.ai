#' Provenance for a sentiment.ai model and scoring head
#'
#' Reports exactly what produced (or would produce) a score: the encoder and its
#' license, source URL, pinned revision, and embedding prefix, plus the scoring head
#' (type and temperature). It also guards against train/serve prefix skew -- the e5
#' models are trained with a \code{"query: "} prefix and silently lose accuracy if it
#' is dropped (Wang et al. 2024), so \code{prefix_ok} flags any mismatch between the
#' prefix applied at serve time and the model's registered prefix.
#'
#' @param model character; a model handle (default \code{DEFAULT_MODEL}).
#' @param scoring character; scoring-head type ("mlp", "logistic", ...). Default "mlp".
#' @param scoring_version character; head version. Default "1.0".
#' @return A list of class \code{"sentiment_provenance"}: model, backend, dim, prefix,
#'   revision, license, source, scoring, scoring_version, head_type, temperature,
#'   serve_prefix, prefix_ok.
#' @examples
#' \dontrun{
#'   sentiment_provenance("e5-small")
#' }
#' @export
sentiment_provenance <- function(model           = DEFAULT_MODEL,
                                 scoring         = "mlp",
                                 scoring_version = "1.0"){
  model  <- model[1]
  cls    <- model_class(model)
  prefix <- { p <- model_prefix[model]; if(is.na(p)) "" else unname(p) }

  # head metadata (type + temperature) from the shipped JSON head, if present
  head_type <- NA_character_
  head_T    <- NA_real_
  hp <- system.file(file.path("scoring", scoring, scoring_version,
                              paste0(model, ".json")),
                    package = "sentiment.ai")
  if(nzchar(hp) && file.exists(hp)){
    h <- jsonlite::fromJSON(hp, simplifyVector = TRUE,
                            simplifyDataFrame = FALSE, simplifyMatrix = TRUE)
    if(!is.null(h$type)) head_type <- h$type
    if(!is.null(h$T))    head_T    <- h$T
  }

  # train/serve prefix-skew guard: the prefix applied at serve time
  # (sentiment.env$prefix, set by init for the active model) must equal the registered
  # prefix the scorer was trained with. Before init there is no serve prefix, so it
  # defaults to the registry value (consistent by construction).
  serve_prefix <- if(!is.null(sentiment.env$embed) && !is.null(sentiment.env$prefix))
                    sentiment.env$prefix else prefix
  prefix_ok <- identical(unname(serve_prefix), unname(prefix))

  structure(
    list(model           = model,
         backend         = cls,
         dim             = model_dims[[model]],
         prefix          = prefix,
         revision        = unname(model_revision[model]),
         license         = unname(model_license[model]),
         source          = unname(model_source_url[model]),
         scoring         = scoring,
         scoring_version = scoring_version,
         head_type       = head_type,
         temperature     = head_T,
         serve_prefix    = unname(serve_prefix),
         prefix_ok       = isTRUE(prefix_ok)),
    class = "sentiment_provenance")
}

#' @export
print.sentiment_provenance <- function(x, ...){
  cat("sentiment.ai provenance\n")
  cat("  model    :", x$model, paste0("(", x$backend, ", dim ", x$dim, ")"), "\n")
  cat("  prefix   :", sprintf('"%s"', x$prefix),
      if(!isTRUE(x$prefix_ok)) "  [!! serve/scorer prefix MISMATCH]" else "", "\n")
  cat("  revision :", x$revision, "\n")
  cat("  license  :", x$license, "\n")
  cat("  source   :", x$source, "\n")
  cat("  scoring  :", x$scoring, x$scoring_version,
      if(!is.na(x$head_type))
        paste0("(", x$head_type, ", T=", round(x$temperature, 3), ")") else "",
      "\n")
  invisible(x)
}
