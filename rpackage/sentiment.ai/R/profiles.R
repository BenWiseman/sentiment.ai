# Intent-based profiles + a persisted default model -- the R twin of the Python
# package's sentimentai/_profiles.py. A profile maps a plain intent to a backend and
# becomes the default for sentiment_score() / sentiment() / sentiment_match(), so callers
# do not have to learn the model zoo. The choice persists to a small JSON under
# tools::R_user_dir("sentiment.ai", "config"), so it sticks across sessions; the
# SENTIMENTAI_MODEL environment variable or options(sentiment.ai.model=) override it.
#
# Key nuance (identical to Python): the hate/mixed/style flags need the e5 embedding
# space, so "multilingual WITH flags" is e5-base, NOT the (more accurate but flag-less)
# XLM-R transformer.

# profile name -> list(model, summary, desc)
.sentiment_profiles <- list(
  lightest = list(
    model   = "e5-small",
    summary = "Lightest on-device",
    desc    = paste("Tiny + instant, ~100 languages, fully offline.",
                    "Includes hate/mixed/style flags. ~2MB head.")),
  multilingual = list(
    model   = "e5-base",
    summary = "Best on-device multilingual, with flags",
    desc    = paste("Stronger multilingual sentiment, fully offline.",
                    "Includes hate/mixed/style flags. ~7MB head.")),
  `max-english` = list(
    model   = "twitter-roberta",
    summary = "Best English accuracy (sentiment only)",
    desc    = paste("Highest English accuracy (RoBERTa). Sentiment only -- no flags.",
                    "~500MB download, English-only.")),
  `max-multilingual` = list(
    model   = "xlm-roberta",
    summary = "Best multilingual accuracy (sentiment only)",
    desc    = paste("Highest multilingual accuracy (XLM-R). Sentiment only -- no flags.",
                    "~1GB download."))
)

DEFAULT_PROFILE <- "multilingual"

#' Available sentiment.ai profiles
#'
#' @description Intent-based presets that choose the embedding / classifier backend for
#' you, so you do not have to learn the model handles. Make one the default with
#' [use_profile()].
#'
#' @return A \code{data.frame} with columns \code{profile}, \code{model},
#'   \code{summary}, \code{description}.
#' @examples
#' sentiment_profiles()
#' @export
sentiment_profiles <- function(){
  do.call(rbind, lapply(names(.sentiment_profiles), function(nm){
    p <- .sentiment_profiles[[nm]]
    data.frame(profile = nm, model = p$model, summary = p$summary,
               description = p$desc, stringsAsFactors = FALSE)
  }))
}

#' Set the default backend from an intent-based profile
#'
#' @description Pick a profile and make its model the default for [sentiment_score()],
#' [sentiment()] and [sentiment_match()]. Profiles:
#' \describe{
#'   \item{\code{"lightest"}}{e5-small -- tiny, on-device, multilingual, with the
#'     hate/mixed/style flags. The package default.}
#'   \item{\code{"multilingual"}}{e5-base -- stronger on-device multilingual, with flags.}
#'   \item{\code{"max-english"}}{twitter-roberta -- best English accuracy, sentiment only
#'     (opt-in ~500MB transformer, no flags).}
#'   \item{\code{"max-multilingual"}}{xlm-roberta -- best multilingual accuracy, sentiment
#'     only (opt-in ~1GB transformer, no flags).}
#' }
#' The choice persists across sessions (a small JSON under
#' \code{tools::R_user_dir("sentiment.ai", "config")}). The \code{SENTIMENTAI_MODEL}
#' environment variable or \code{options(sentiment.ai.model=)} override it for one-off runs.
#'
#' @param name Profile name; see [sentiment_profiles()].
#' @return (invisibly) the resolved model handle.
#' @examples
#' \dontrun{
#'   use_profile("multilingual")   # e5-base, with flags
#'   use_profile("max-english")    # opt-in RoBERTa, sentiment only
#' }
#' @export
use_profile <- function(name){
  name <- as.character(name)[1]
  if(!name %in% names(.sentiment_profiles))
    stop("unknown profile '", name, "'; choose from: ",
         paste(names(.sentiment_profiles), collapse = ", "), call. = FALSE)
  model <- .sentiment_profiles[[name]]$model
  .set_default_model(model)                    # session: namespace binding + option
  .write_user_config(default_model = model)    # persist across sessions
  message("sentiment.ai: profile '", name, "' -> default model '", model, "'.")
  invisible(model)
}

# --- persisted user config (tools::R_user_dir) ------------------------------
.sentimentai_config_path <- function(){
  file.path(tools::R_user_dir("sentiment.ai", which = "config"), "config.json")
}

#' @importFrom jsonlite fromJSON
.read_user_config <- function(){
  p <- .sentimentai_config_path()
  if(!file.exists(p)) return(list())
  tryCatch(as.list(jsonlite::fromJSON(p)), error = function(e) list())
}

#' @importFrom jsonlite write_json
.write_user_config <- function(...){
  p   <- .sentimentai_config_path()
  dir.create(dirname(p), showWarnings = FALSE, recursive = TRUE)
  cfg <- utils::modifyList(.read_user_config(), list(...))
  jsonlite::write_json(cfg, p, auto_unbox = TRUE, pretty = TRUE)
  invisible(cfg)
}
