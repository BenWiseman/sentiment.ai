# Resolve the `sentiment.ai.autoinit` opt-in to an env name (or NULL = no eager init).
# TRUE / "true" / "1" / "yes" -> the default env; a non-empty string -> that env name.
.autoinit_envname <- function(opt) {
  if (isTRUE(opt) || tolower(as.character(opt)[1]) %in% c("true", "1", "yes")) {
    "r-sentiment-ai"
  } else if (is.character(opt) && length(opt) && nzchar(opt[1])) {
    opt[1]
  } else {
    NULL
  }
}

.onLoad <- function(libname, pkgname){
  ns <- asNamespace(pkgname)

  set_const <- function(const, val){
    if(is.null(val) || !nzchar(val)) return(invisible())
    unlockBinding(const, ns)
    assign(const, as.character(val), envir = ns)
    lockBinding(const, ns)
  }

  # Scoring head default: options(sentiment.ai.scoring=) (e.g. in .Rprofile).
  set_const("DEFAULT_SCORING", getOption("sentiment.ai.scoring", NULL))

  # Default model precedence (mirrors the Python package): the SENTIMENTAI_MODEL
  # environment variable (one-off override) > options(sentiment.ai.model=) (.Rprofile)
  # > the profile persisted by use_profile() under tools::R_user_dir(). Falls back to
  # the package default (e5-small). Resolved before any function default is evaluated.
  m <- Sys.getenv("SENTIMENTAI_MODEL")
  if(!nzchar(m)) m <- as.character(getOption("sentiment.ai.model", ""))
  if(!nzchar(m)){
    cfg <- tryCatch(.read_user_config(), error = function(e) list())
    if(!is.null(cfg$default_model) && nzchar(cfg$default_model)) m <- cfg$default_model
  }
  set_const("DEFAULT_MODEL", m)
}

.onAttach <- function(libname, pkgname) {

  # Opt-in EAGER init: if the user asked for it (options(sentiment.ai.autoinit=) or the
  # SENTIMENTAI_AUTOINIT env var) AND the backend env already exists, load the model now
  # so library(sentiment.ai) comes up "ready". OFF by default and interactive-only, so a
  # plain attach -- and R CMD check -- stay fast. Either way the model also loads lazily
  # on the first sentiment_score() / sentiment() / sentiment_match() call.
  envname <- .autoinit_envname(getOption("sentiment.ai.autoinit",
                                         default = Sys.getenv("SENTIMENTAI_AUTOINIT")))

  if (!is.null(envname) && interactive()) {
    ready <- tryCatch(
      envname %in% as.character(reticulate::virtualenv_list()) ||
        envname %in% tryCatch(as.character(reticulate::conda_list()$name),
                              error = function(e) character(0)),
      error = function(e) FALSE)
    if (ready) {
      packageStartupMessage("sentiment.ai: auto-initialising '", envname,
                            "' (sentiment.ai.autoinit)...")
      ok <- tryCatch({ init_sentiment.ai(envname = envname, silent = TRUE); TRUE },
                     error = function(e) FALSE)
      if (ok) return(invisible())
      packageStartupMessage("  could not auto-init; the model will load on first use.")
      return(invisible())
    }
  }

  packageStartupMessage(
    "sentiment.ai: default model 'e5-small' (on-device, multilingual, no TensorFlow).\n",
    "  The model loads automatically on first use; run install_sentiment.ai() if you\n",
    "  haven't set up the backend (legacy USE: install_sentiment.ai(legacy = TRUE)).\n",
    "  Tip: options(sentiment.ai.autoinit = TRUE) loads it when you attach the package."
  )
}
