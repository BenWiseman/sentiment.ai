#' setup
#' @description Install and Setup sentiment.ai package
#'
#' @inheritParams reticulate::py_install
#' @param method Installation method. By default, "auto" automatically finds a
#'        method that will work in the local environment. Change the default to
#'        force a specific installation method ("virtualenv" or "conda").
#' @param gpu logical; \code{FALSE} (default) installs the smaller CPU-only build of
#'        PyTorch (on Linux/Windows the default PyPI build is the large CUDA one).
#'        \code{TRUE} installs the CUDA build (and enables GPU for the legacy TensorFlow
#'        stack). Ignored on macOS, which is always CPU/MPS (no CUDA wheels). The e5
#'        backend selects its device automatically at run time.
#' @param modules Named list of Python modules to install. Defaults to \code{NULL},
#'        which selects the correct set for \code{legacy}. Only change this if you
#'        know what you are doing.
#' @param fresh_install Whether to create the Python environment prior to
#'        installing the modules or to install everything in an existing
#'        environment (if one exists). Only change this argument if you know what
#'        you are doing! If the environment does not already exist, will create
#'        the environment first.
#' @param restart_session Whether to restart the R session after finishing
#'        installation. Only works on Rstudio.
#' @param fast Use \code{uv} (if installed) to create the virtualenv and install the
#'        dependencies in one fast step, instead of pip. \code{NA} (default) uses \code{uv}
#'        automatically when it is on the \code{PATH}; \code{TRUE} forces it; \code{FALSE}
#'        always uses the standard pip installer. If the fast path does not complete it
#'        falls back to the standard installer, so it is always safe to leave on.
#' @param model character; embedding model handle for \code{init_sentiment.ai}:
#'        \code{"e5-small"} (default, 384-d, multilingual, on-device, no TensorFlow),
#'        \code{"e5-base"} (768-d), \code{"openai"} (text-embedding-3-small, paid API),
#'        or a legacy Universal Sentence Encoder model (\code{"en"}, \code{"en.large"},
#'        \code{"multi"}, \code{"multi.large"}) which requires
#'        \code{install_sentiment.ai(legacy = TRUE)}.
#' @details
#' \code{install_sentiment.ai()} sets up the Python environment for the backend. The
#' default (v2) stack is TensorFlow-free:
#'
#' | Module                | Default |
#' | :-------------------- | :-----: |
#' | python                | 3.10    |
#' | numpy                 | latest  |
#' | sentencepiece         | latest  |
#' | sentence-transformers | latest  |
#' | openai                | latest  |
#'
#' With \code{legacy = TRUE} it additionally installs \code{tensorflow} and
#' \code{tensorflow-hub} (plus \code{tensorflow-text} off Apple Silicon) for the
#' Universal Sentence Encoder models. Installation uses \code{reticulate::py_install}
#' with \code{pip = TRUE}.
#'
#' @note
#' Setting environments with \code{reticulate} is notoriously difficult. If the
#' RETICULATE_PYTHON environment is set, then reticulate will not let you change
#' the Python binary used (or the Python environment) using \code{use_condaenv}
#' **or** \code{use_virtualenv}. This environment can be accidentally set in
#' the following ways:
#'
#' 1. If RETICULATE_PYTHON is in your .Renviron file or bash/zsh rc files. This is
#'    the most obvious place that this environment will be set.
#' 2. Using Project Options or Global Options under "Python>Python Interpreter".
#'    If this is set, then reticulate will almost always use this version of Python
#'    and will not let you change.
#' 3. If you have already loaded reticulate and have run `py_config`. Once a Python
#'    version/environment is instantiated, you will not be able to change it and
#'    will have to restart R.
#' 4. If you are in **any** project, at all! Currently (as of `reticulate` version
#'    1.22), every project automatically sets the RETICULATE_PYTHON environment
#'    variable, either through the Global or Project Options or by using heuristics.
#'    If you are in an RStudio project, you **must** update Global/Project Options
#'    with the specific version/environment of Python that you want to use, or
#'    you will not be able to change it!
#'
#' Manually setting the environment variable to NULL (using
#' `Sys.unsetenv("RETICULATE_PYTHON")`, updating your Project/Global options going
#' Tools>Project Options or Tools>Global Options and then select Python in the
#' left menu bar and click the "Select" button to manually set the Python
#' interpreter, and/or restarting your R session **might** fix the problem.
#'
#' We know this is a pain, and we would like to fix this for you, but we are
#' dependent on the RStudio/reticulate team to update how they determine the
#' allowable Python versions/environments.
#'
#' @examples
#' \dontrun{
#' # TensorFlow-free default: on-device multilingual e5
#' install_sentiment.ai(envname = "r-sentiment-ai")
#' init_sentiment.ai(model = "e5-small", envname = "r-sentiment-ai")
#' sentiment_score("I love this!")
#'
#' # opt-in legacy Universal Sentence Encoder (installs TensorFlow)
#' # install_sentiment.ai(legacy = TRUE)
#' # init_sentiment.ai(model = "en.large")
#'
#' # if you run into an issue, follow the instructions/see the note and retry!
#' }


# 1. INSTALL ===================================================================


# install environment and dependencies\

#' @rdname setup
#' @return NULL this function simply installs the required python dependencies and default scoring models and pre-calculated embedding vectors.
#' @importFrom roperators "%ni%"
#' @param legacy logical; if TRUE, also install the opt-in TensorFlow / TF-Hub stack
#'        for the legacy Universal Sentence Encoder models. Default FALSE -- v2 is
#'        TensorFlow-free (sentence-transformers + e5).
#' @export
install_sentiment.ai <- function(envname = "r-sentiment-ai",
                                 method  = c("auto", "virtualenv", "conda"),
                                 gpu     = FALSE,
                                 legacy  = FALSE,
                                 python_version = NULL,
                                 modules = NULL,
                                 fresh_install   = TRUE,
                                 restart_session = TRUE,
                                 fast    = NA,
                                 pin_versions    = NA,
                                 ...){

  method <- match.arg(method)
  legacy <- isTRUE(legacy)
  gpu    <- isTRUE(gpu)

  # if environment is missing, set it to r-sentiment-ai
  if(length(envname) == 0) envname <- "r-sentiment-ai"

  # v2 default backend is sentence-transformers -- NO TensorFlow. Only legacy = TRUE
  # adds the opt-in TF-Hub / Universal Sentence Encoder stack.
  if(is.null(python_version)) python_version <- "3.10"
  if(is.null(modules)){
    modules <- list(certifi                 = "latest",
                    numpy                   = "latest",
                    sentencepiece           = "latest",   # e5's tokenizer needs it
                    `sentence-transformers` = "latest",
                    openai                  = "latest")
    if(legacy){
      message("Installing the legacy TensorFlow / Universal Sentence Encoder backend. ",
              "Note that USE is end-of-life -- Google has retired it and TF Hub is ",
              "winding down -- so it is unsupported and kept only for backward ",
              "compatibility. The modern default ('e5-small'/'e5-base') needs none of this.")
      modules <- c(modules, list(tensorflow       = "latest",
                                 `tensorflow-hub` = "latest"))
      # tensorflow-text is unavailable on Apple Silicon; add it elsewhere only.
      if(!(roperators::is.os_arm() && roperators::is.os_mac()))
        modules[["tensorflow-text"]] <- "latest"
    }
  }

  # if method is default, figure out method using the vendored detector (no `:::`
  # into reticulate's private namespace -- see R/local_from_reticulate.R)
  if(method == "auto"){
    method <- install_method_detect(envname = envname)
  }

  # GPU choice: the default (gpu = FALSE) installs the much smaller CPU-only torch; gpu =
  # TRUE installs the CUDA build (and the legacy TF-GPU stack). macOS is always CPU/MPS (no
  # CUDA wheels) so gpu is forced FALSE there. The e5 backend picks its device at run time.
  if(gpu && roperators::is.os_mac()){
    warning("gpu not available for OSX; setting gpu flag to FALSE", call. = FALSE)
    gpu <- FALSE
  } else if(gpu && legacy && "tensorflow" %in% names(modules)){
    message("gpu flag is TRUE; installation needs CUDA configured")
    names(modules)[names(modules) == "tensorflow"] <- "tensorflow-gpu"
  }

  # When pin_versions = TRUE, substitute the verified-working baseline versions
  # for any "latest" entries (torch is handled separately; legacy TF stack keeps "latest").
  if(isTRUE(pin_versions) && is.null(attr(modules, "user_supplied"))){
    pinned <- .read_pinned_requirements()
    if(length(pinned)){
      pinned_map <- setNames(
        sub("^([^=]+)==(.+)$", "\\2", pinned),
        sub("^([^=]+)==.+$",   "\\1", pinned)
      )
      for(nm in names(modules)){
        if(modules[[nm]] == "latest" && nm %in% names(pinned_map))
          modules[[nm]] <- pinned_map[[nm]]
      }
    }
  }

  # make sure that all modules have length 1
  stopifnot(all(lengths(modules) == 1))


  # Handle NULL or empty versions
  modules_vers <- sapply(names(modules), function(name) {
    version <- modules[[name]]
    if (is.null(version) || version == "" || version =="latest") {
      return(name)
    } else {
      return(paste0(name, "==", version))
    }
  })


  # 3. install everything ------------------------------------------------------

  # FAST PATH (auto when uv is on the PATH; force with fast = TRUE, skip with fast = FALSE):
  # let uv create the named virtualenv AND install the (torch-sized) stack in one shot --
  # far quicker than pip. It is a normal named virtualenv, activated the usual way (NOT a
  # reticulate ephemeral env). We then VERIFY the key package landed; any failure (or a
  # missing package) falls straight through to the standard installer below, so the fast
  # path can never leave you worse off than the proven one.
  # default to the smaller CPU-only torch (Linux/Windows); gpu = TRUE keeps the CUDA build
  torch_index <- .torch_index(gpu)

  installed_via_uv <- FALSE
  # the uv one-shot installs the whole stack together, so use it only when no torch-specific
  # index is needed; CPU torch on Linux/Windows is staged in the standard path below instead
  use_uv <- fresh_install && method == "virtualenv" && is.null(torch_index) &&
            (isTRUE(fast) || (is.na(fast) && .uv_available()))
  if(use_uv){
    installed_via_uv <- tryCatch({
      message("Setting up the '", envname, "' backend with uv (fast)...")
      reticulate::virtualenv_create(envname = envname, version = python_version,
                                    packages = modules_vers, module = "uv")
      pkgs <- tryCatch(tolower(reticulate::py_list_packages(envname)$package),
                       error = function(e) character(0))
      if(!"sentence-transformers" %in% pkgs)
        stop("uv created the environment but the expected packages are missing")
      TRUE
    }, error = function(e){
      message("Fast (uv) setup did not complete (", conditionMessage(e),
              ") -- falling back to the standard installer."); FALSE })
  }

  if(!installed_via_uv){
    # if fresh install, create environment first
    if(fresh_install){
      switch(
        EXPR       = method,
        virtualenv = {
          check_virtualenv_py(envname = envname, version = python_version, ...)
          if(envname %ni% as.character(reticulate::virtualenv_list()) ){
            # if had to install py first
            reticulate::virtualenv_create(envname = envname, version = python_version, ...)
          }
          },
        conda      = reticulate::conda_create(envname = envname,python_version = python_version,...)
      )
    } else{
      message("Because 'fresh_install = FALSE', not creating environment before installing.\n",
              "Only do this if you know what you are doing, as you might have conflicting\n",
              "installations and/or reticulate might not be able to find the correct environment.")
    }

    # install the platform-appropriate torch first (the smaller CPU build by default) so
    # the rest of the stack does not pull the large default (CUDA) build. Fallback-safe:
    # if this step fails, the normal install below pulls the default torch instead.
    # conda envs with pip=TRUE also accept --index-url, so the same CPU-torch
    # pre-install works for both virtualenv and conda (both use pip under the hood here)
    if(!is.null(torch_index) && method %in% c("virtualenv", "conda")){
      message("Installing CPU-only torch (smaller download; pass gpu = TRUE for CUDA)...")
      tryCatch(
        reticulate::py_install(packages = "torch", envname = envname, method = method,
                               pip = TRUE, pip_options = c("--index-url", torch_index), ...),
        error = function(e) message("  CPU-torch step skipped (", conditionMessage(e),
                                    "); installing the default torch instead."))
    }

    reticulate::py_install(
      packages       = modules_vers,
      envname        = envname,
      method         = method,
      python_version = python_version,
      pip            = TRUE,
      ...
    )
  }

  # --- post-install verification and auto-fallback ----------------------------
  # pin_versions = NA (default): verify key packages landed; if not, retry with
  # the bundled pinned baseline so the user doesn't land in dependency hell.
  # pin_versions = TRUE:  already installed from pinned — just verify.
  # pin_versions = FALSE: skip check entirely (escape hatch for power users).
  if(!isFALSE(pin_versions)){
    st_ok <- tryCatch({
      pkgs <- tolower(reticulate::py_list_packages(envname)$package)
      "sentence-transformers" %in% pkgs
    }, error = function(e) FALSE)

    if(!st_ok){
      if(is.na(pin_versions)){
        message(
          "The 'latest' install did not place sentence-transformers correctly.\n",
          "Retrying automatically with the verified-working pinned baseline\n",
          "(install_sentiment.ai(pin_versions = TRUE) to always use this)...")
        pinned <- .read_pinned_requirements()
        if(length(pinned)){
          tryCatch(
            reticulate::py_install(packages = pinned, envname = envname,
                                   method = method, pip = TRUE, ...),
            error = function(e)
              warning("Pinned fallback also failed: ", conditionMessage(e),
                      "\nTry: install_sentiment.ai(pin_versions = TRUE)",
                      call. = FALSE))
        } else {
          warning("inst/python/requirements.txt not found in installed package; ",
                  "cannot auto-fall back. Run: install_sentiment.ai(pin_versions=TRUE)",
                  call. = FALSE)
        }
      } else {
        # pin_versions = TRUE but still failed — unusual; warn loudly
        warning("Pinned baseline install did not verify. Check your environment: ",
                envname, call. = FALSE)
      }
    }
  }
  # --------------------------------------------------------------------------

  message("Successfully created ", method, " environment: ", envname)

  # The default v2 scoring heads (mlp / logistic JSON) ship inside the package, so a
  # default install downloads no scorer at all. The legacy xgb/glm scorers are opt-in:
  # call install_scoring_model(scoring = "xgb") on demand (xgb also needs the suggested
  # 'xgboost' package).

  # restart session if needed
  if(restart_session && requireNamespace("rstudioapi", quietly = TRUE) &&
     rstudioapi::hasFun("restartSession")){
    rstudioapi::restartSession()
  } else{
    message("Please restart your R session")
  }

  invisible(NULL)
}


#' Install a Scoring Model
#'
#' @param model The embedding model, one of c("en.large", "en", "multi.large",
#'        "multi").
#' @param scoring The scoring model, currently one of:
#'   - "xgb" does default xgboost
#'   - "glm" does generalized linear model (if you can't run xgboost)
#' @param scoring_version Version of scoring model (will add more over time)
#' @param ... Additional options to the function, including:
#'   - repo_url: OPTIONAL custom github repo blob url for external scoring models.
#'     The default repo_url is "https://github.com/BenWiseman/sentiment.ai/blob/main/scoring"
#'
#' @return
#' 0 if model did not need to be downloaded.
#' 1 if model needed to be downloaded.
#'
#' @details
#' This downloads the scoring models from a set repository in order to keep the
#' main package within CRAN size limits.
#'
#' In the future, this will also make it possible for the community to add new
#' and improved models!
#'
#' @importFrom roperators "%ni%"
install_scoring_model <- function(model   =  DEFAULT_MODEL,
                                  scoring = c("mlp", "logistic", "xgb", "glm"),
                                  scoring_version = "1.0",
                                  ...){

  # for return status
  status <- 0

  # passthrough optional repo_url
  opts <- list(...)

  if(is.null(opts$repo_url)) {
    repo_url <- "https://github.com/BenWiseman/sentiment.ai/raw/main/scoring"
  } else{
    repo_url <- opts$repo_url
  }

  # accept any model handle (no match.arg straitjacket -- e5 names, etc.)
  model   <- model[1]
  scoring <- scoring[1]
  scoring_version <- scoring_version[1]

  # legacy scorers are opt-in and no longer fetched by default
  if(scoring %in% c("xgb", "glm")){
    message("Note: scoring = '", scoring, "' is a legacy scorer, kept for backward ",
            "compatibility and not installed by default. The v2 default (mlp) ships ",
            "in the package -- no download, and no xgboost, required.")
  }

  # file extension per scoring type: glm=csv, mlp/logistic=json (ship in package), else <scoring>
  file_ext   <- switch(scoring, glm = "csv", mlp = "json", logistic = "json", scoring)
  file_name  <- paste0(model, ".", file_ext)

  # base url - repo containing model objects
  target_url <- paste(repo_url, scoring, scoring_version, file_name, sep = "/")

  # Add query param to end
  target_url <- paste0(target_url, "?raw=true")


  # get download location: <pkg_dir>/scoring/xgb/1.0/en.large
  # determining package name and base path
  pkg_name <- utils::packageName()
  pkg_path <- system.file(package = pkg_name)
  dl_path  <- file.path(pkg_path, "scoring", scoring, scoring_version)
  obj_path <- file.path(dl_path, file_name)

  # should dl_path go to a library or the package dir? if it's in the package dir
  # they will always have to reinstall all of the scoring models if upgraded

  # if model exists, return NULL - nothing to do
  if(file.exists(obj_path)) return(0)

  # model doesn't exist, download it into dl_path
  if(!dir.exists(dl_path)) {
    # directory doesn't exist, make directory & download
    dir.create(dl_path,
               showWarnings = FALSE,
               recursive = TRUE)
  }

  message("Downloading ", model, ": ", scoring, " ", scoring_version,  " from github")

  tryCatch({
    # you can always use wb to download bites
    utils::download.file(url      = target_url,
                         destfile = obj_path,
                         mode     = "wb")
    status <- 1

  }, error=function(e){

    message("Attempt to pull scoring models failed.\n",
            "Unfortunately these are necessary and can't be bundled on CRAN!\n",
            "The error was:\n", as.character(e))

  })

  return(status)
}

# 2. INITIALIZE ================================================================


#' Initialize sentiment.ai Environment
#' @rdname setup
#'
#' @description Loads the embedding model into the session so repeated scoring is fast.
#' You normally do not need to call it explicitly: the scoring functions
#' (\code{\link{sentiment_score}}, \code{\link{sentiment}}, \code{\link{sentiment_match}})
#' load the model automatically on first use. Call it yourself to control \emph{when} the
#' (few-second) load happens, or to switch models. To load eagerly when the package is
#' attached, set \code{options(sentiment.ai.autoinit = TRUE)} (or the
#' \code{SENTIMENTAI_AUTOINIT} environment variable) before \code{library(sentiment.ai)} --
#' it then initialises on attach if the backend is already installed.
#'
#' @param silent logical flag indicating whether to suppress console logging. Note:
#'        this does not affect Python or native (C++) output.
#' @param api_key character string specifying the API key for OpenAI, if using the
#'        \code{"openai"} model.
#' @param api_base character string specifying the base URL for the OpenAI API.
#' @param api_version character string specifying the OpenAI API version.
#' @param api_type optional character; set to "azure" for an Azure OpenAI endpoint.
#' @param api_engine optional character; Azure deployment/engine name (Azure only).
#'
#' @return Python function for text embedding. This is stored in the package environment and does not need to be explicitly used.
#'
#' @importFrom reticulate py_run_string source_python
#' @importFrom utils packageName
#'
#' @export
init_sentiment.ai <- function(model       = DEFAULT_MODEL,
                              envname     = "r-sentiment-ai",
                              method      =  c("auto", "virtualenv", "conda"),
                              silent      = FALSE,
                              api_key     = NULL,
                              api_base    = "https://api.openai.com",
                              api_version = "v1",
                              api_type    = NULL,
                              api_engine  = "text-davinci-002"){


  method = match.arg(method)

  # determining package name and base path
  pkg_name <- utils::packageName()
  pkg_path <- system.file(package = pkg_name)

  # Guard the OpenMP collision: R's xgboost and the Python torch backend (reticulate)
  # each link their own libomp into one process and abort without this. (The legacy
  # TF GPU-growth flag now lives in the legacy branch where it's actually relevant.)
  if(Sys.getenv("KMP_DUPLICATE_LIB_OK") == "") Sys.setenv("KMP_DUPLICATE_LIB_OK" = "TRUE")

  # resolve the user-facing model name -> backend class + id. This is pure R and MUST
  # happen BEFORE any Python is touched, so the gates below can short-circuit cleanly
  # (a legacy model without TF, or OpenAI which needs no Python at all).
  model_name <- model[1]
  cls        <- model_class(model_name)
  model_id   <- choose_model(model_name)

  env <- sentiment.ai::sentiment.env
  env$openai  <- FALSE
  env$st      <- FALSE
  env$prefix  <- ""
  env$backend <- cls   # "st" | "openai" | "legacy" -- which backend was selected

  # USE is end-of-life: Google retired it and TF Hub is winding down. Warn once for ANY
  # legacy request, before the gate/load below, so the notice always precedes the
  # error-or-load.
  if(cls == "legacy" && !isTRUE(env$warned_use_eol)){
    warning(
      "The legacy Universal Sentence Encoder (USE) backend is no longer actively ",
      "maintained or supported. Google has retired the USE models and TF Hub itself ",
      "is winding down, so this path is frozen in place and kept only for backward ",
      "compatibility. The modern default ('e5-small' / 'e5-base') is on-device, ",
      "multilingual, and TensorFlow-free. (Another one for the Google graveyard -- ",
      "we would rather keep your scores reproducible than keep your dependencies ",
      "guessing.)",
      call. = FALSE)
    env$warned_use_eol <- TRUE
  }

  # ---- gates that must fire BEFORE activating Python -------------------------
  # OpenAI is an HTTP API: it needs neither reticulate nor a Python env.
  if(cls == "openai"){
    if(is.null(api_key))
      stop("model '", model_name, "' is an OpenAI model and needs an api_key.", call. = FALSE)
    env$embed    <- load_openai_embedding(model_id, api_key, api_base, api_version, api_type, api_engine)
    env$openai   <- TRUE
    env$parallel <- test_parallel_support()
    return(invisible(NULL))
  }

  # Legacy USE models require the opt-in TensorFlow backend. If it is absent, fail with
  # a directed, actionable error -- BEFORE activating Python or importing anything --
  # naming both the fix and the TF-free replacement model.
  if(cls == "legacy" &&
     (!requireNamespace("tensorflow", quietly = TRUE) ||
      !requireNamespace("tfhub", quietly = TRUE))){
    repl <- if(model_name == "en") "e5-small" else "e5-base"
    stop("'", model_name, "' is a legacy TensorFlow model. ",
         "Run install_sentiment.ai(legacy = TRUE) to enable it, or use the modern ",
         "default '", repl, "' (no TensorFlow required).", call. = FALSE)
  }

  # ---- legacy (TF) path ------------------------------------------------------
  # R packages are present (else the gate above fired). Bring up the TF backend and
  # convert ANY failure (missing python tensorflow module, env activation, etc.) into
  # the SAME directed error instead of a raw crash -- so a half-installed legacy setup
  # still gets actionable guidance.
  if(cls == "legacy"){
    ok <- tryCatch({
      .activate_env(envname, silent = silent, r_envir = -2, method = method)
      reticulate::source_python(system.file("get_embedder.py", package = pkg_name))
      Sys.setenv("TF_FORCE_GPU_ALLOW_GROWTH" = "true")
      cache_dir <- file.path(pkg_path, "tfhub_modules")
      dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
      env$embed <- if(silent) suppressMessages(load_hub_embedder(model_id, cache_dir))
                   else load_hub_embedder(model_id, cache_dir)
      TRUE
    }, error = function(e) FALSE)
    if(!isTRUE(ok)){
      repl <- if(model_name == "en") "e5-small" else "e5-base"
      stop("'", model_name, "' is a legacy TensorFlow model. ",
           "Run install_sentiment.ai(legacy = TRUE) to enable it, or use the modern ",
           "default '", repl, "' (no TensorFlow required).", call. = FALSE)
    }
    env$parallel <- test_parallel_support()
    return(invisible(NULL))
  }

  # ---- default path: sentence-transformers (e5) ------------------------------
  .activate_env(envname, silent = silent, r_envir = -2, method = method)
  if(!silent) message("Preparing Model")
  reticulate::source_python(system.file("get_embedder.py", package = pkg_name))

  env$parallel <- test_parallel_support()
  if(env$parallel >= 2) message(env$parallel, " CPU cores available for parallel processing")
  else message("No parallel processing support found on CPU")

  # The "query: " prefix is applied R-side in embed_text (consistent + testable), so
  # the embedder is loaded as a PLAIN encoder.
  env$prefix <- { p <- model_prefix[model_name]; if(is.na(p)) "" else unname(p) }
  # pin to the immutable commit SHA so the downloaded weights are auditable
  rev <- model_revision[model_name]
  rev <- if(length(rev) == 0L || is.na(rev)) NULL else unname(rev)
  env$revision <- rev
  if(!silent) message("Loading sentence-transformers model: ", model_id,
                      if(is.null(rev)) "" else paste0(" @ ", substr(rev, 1, 12)))
  env$embed <- load_st_embedder(model_id, "", revision = rev)
  env$st    <- TRUE


  invisible(NULL)
}

#' @rdname setup
#' @return NULL this function checks if `init_sentiment.ai()` has been called
#'         successfully, if not, it is called.
#' @export
check_sentiment.ai <- function(...){

  if(is.null(sentiment.ai::sentiment.env$embed)){

    # First use this session. If the backend isn't set up yet, walk an INTERACTIVE user
    # through the one-time setup; in a non-interactive session (scripts, R CMD check, CI)
    # never auto-install -- just give the clear instruction.
    dots    <- list(...)
    envname <- if(!is.null(dots$envname)) dots$envname else "r-sentiment-ai"
    if(!.backend_ready(envname)){
      did_setup <- if(interactive()) setup_sentiment.ai() else FALSE
      if(!isTRUE(did_setup)){
        stop("the sentiment.ai Python backend is not set up. Run install_sentiment.ai() ",
             "once (TensorFlow-free) to set it up, then try again.", call. = FALSE)
      }
    }

    message("sentiment.ai: loading the model on first use (a few seconds)...")
    init_sentiment.ai(...)
  }

  return(invisible(NULL))
}

# Is the named Python backend already set up (a virtualenv or conda env of that name)?
.backend_ready <- function(envname = "r-sentiment-ai"){
  tryCatch(
    envname %in% as.character(reticulate::virtualenv_list()) ||
      envname %in% tryCatch(as.character(reticulate::conda_list()$name),
                            error = function(e) character(0)),
    error = function(e) FALSE)
}

# Is uv (the fast Python installer) on the PATH?
.uv_available <- function() nzchar(Sys.which("uv"))

# Read inst/python/requirements.txt and return a character vector of
# "package==version" strings (comments and blank lines stripped).
.read_pinned_requirements <- function(){
  req <- system.file("python", "requirements.txt", package = "sentiment.ai")
  if(!nzchar(req) || !file.exists(req)) return(character(0))
  lines <- readLines(req, warn = FALSE)
  trimws(lines[!startsWith(lines, "#") & nzchar(trimws(lines))])
}

# pip index for torch: NULL means "use the default" (CUDA on Linux/Windows, CPU/MPS on mac).
# The default here is the small CPU-only build on Linux/Windows; gpu = TRUE keeps CUDA.
# macOS has no CUDA wheels, so torch is CPU/MPS there regardless -> no special index.
.torch_index <- function(gpu = FALSE){
  if(roperators::is.os_mac() || isTRUE(gpu)) return(NULL)
  "https://download.pytorch.org/whl/cpu"
}

#' Interactive first-run setup for the Python backend
#'
#' @description A friendly walkthrough for the one-time, TensorFlow-free Python setup. It
#' runs automatically the first time you score text in an \emph{interactive} session if the
#' backend is not ready, and you can also call it yourself. In a non-interactive session it
#' does nothing (call \code{\link{install_sentiment.ai}} directly in scripts). When
#' \code{uv} is available it is used for a much faster install.
#'
#' @return Invisibly \code{TRUE} if the backend was set up, otherwise \code{FALSE}.
#' @examples
#' \dontrun{
#'   setup_sentiment.ai()
#' }
#' @export
setup_sentiment.ai <- function(){
  if(!interactive()){
    message("sentiment.ai: run install_sentiment.ai() once to set up the backend ",
            "(non-interactive session -- not auto-installing).")
    return(invisible(FALSE))
  }

  fast <- .uv_available()
  choice <- utils::menu(
    choices = c(
      paste0("Quick setup  (recommended; CPU-only, smaller",
             if(fast) " -- fast, via uv" else "", ")"),
      "GPU / CUDA setup  (larger download; needs an NVIDIA GPU)",
      "Use conda instead of a virtualenv",
      "Also include the legacy TensorFlow USE models",
      "Just show me the command to run myself",
      "Not now"),
    title = paste0("sentiment.ai needs a one-time Python setup (TensorFlow-free; ",
                   "downloads a few hundred MB).\nHow would you like to proceed?"))

  done <- switch(as.character(choice),
    "1" = { install_sentiment.ai(restart_session = FALSE); TRUE },
    "2" = { install_sentiment.ai(gpu = TRUE, restart_session = FALSE); TRUE },
    "3" = { install_sentiment.ai(method = "conda", restart_session = FALSE); TRUE },
    "4" = { install_sentiment.ai(legacy = TRUE, restart_session = FALSE); TRUE },
    "5" = { message("Run this once (TensorFlow-free):\n  install_sentiment.ai()\n",
                    "  # add gpu = TRUE (CUDA), method = \"conda\", or legacy = TRUE as needed"); FALSE },
    FALSE)

  # If we actually installed the backend, offer a model choice
  if(isTRUE(done)){
    model_choice <- utils::menu(
      choices = c(
        "e5-small  (default — fast, ~384-d, ~100 languages; good for most tasks)",
        "e5-base   (best on-device quality — 768-d, ~2x slower, ~4x more RAM)"),
      title = paste0(
        "Which embedding model would you like to use by default?\n",
        "  Both are multilingual and run on-device with no TensorFlow or API key.\n",
        "  You can always change this later with init_sentiment.ai(model = ...)."))
    chosen_model <- switch(as.character(model_choice),
      "1" = "e5-small",
      "2" = "e5-base",
      NULL)
    if(!is.null(chosen_model)){
      .set_default_model(chosen_model)
      message("sentiment.ai: default model set to '", chosen_model, "' for this session.\n",
              "  To make this permanent, add to your .Rprofile:\n",
              "    options(sentiment.ai.model = \"", chosen_model, "\")")
    }
  }

  invisible(isTRUE(done))
}

# Update DEFAULT_MODEL for the current session and persist as an option.
# Uses unlockBinding so functions that use DEFAULT_MODEL as a default argument
# pick up the new value immediately.
.set_default_model <- function(model){
  model <- match.arg(model[1],
                     choices = c(names(default_models),
                                 names(openai_models),
                                 names(legacy_models)))
  ns <- asNamespace("sentiment.ai")
  unlockBinding("DEFAULT_MODEL", ns)
  assign("DEFAULT_MODEL", model, envir = ns)
  lockBinding("DEFAULT_MODEL", ns)
  options(sentiment.ai.model = model)
  invisible(model)
}

# 3. HELPER FUNCTIONS ==========================================================

# Activate sentiment.ai Environment
.activate_env <- function(envname = "r-sentiment-ai",
                          silent  = FALSE,
                          r_envir = -1,
                          method  = c("auto", "virtualenv", "conda")){

  method = match.arg(method)
  # pull the environment lists
  venv_envs  <- character(0)
  conda_envs <- character(0)

  # If problem finding envs for specified method or auto tell user!
  # if the specified method is broken, show error text else only if silent is FALSE
  tryCatch(venv_envs  <- reticulate::virtualenv_list(), error = function(e) if(method != "auto") message(e) else if(!silent) message(e))
  tryCatch(conda_envs <- reticulate::conda_list()$name, error = function(e) if(method != "auto") message(e) else if(!silent) message(e))

  # use py_install_method_detect but give note that default behavior is different!

  # exception handle: if detection fails, recover the method from where envname is found
  # (conda takes priority if present, else virtualenv). `<<-` updates the `method` in the
  # enclosing .activate_env frame -- the old assign(pos = -2) was invalid and crashed here.
  auto_failover <- function(e){
    message(e)
    if(!silent) message("Attempting failover which will prioritise conda if available.")
    if(envname %in% conda_envs)     method <<- "conda"
    else if(envname %in% venv_envs) method <<- "virtualenv"
  }

  # find method
  if(method == "auto"){
    # Find preferred install method (must pass envname -- without it the detector hits a
    # version check on a missing value and errors)
    tryCatch(method <- suppressWarnings(py_install_method_detect(envname = envname)),
             error = function(e) auto_failover(e))

    # sanity check
    if(method == "auto") stop("py_install_method_detect failed and environment could not be found!")
    # let user know if they're venv or conda
    if(!silent) message("Attempting to activate ", method, " environment...")
   }

  all_envs   <- c(venv_envs, conda_envs)

  # make sure the environment name is in the list
  stopifnot(is.character(envname),
            length(envname) == 1)

  if(envname %ni% all_envs){
    stop("the '", envname, "' Python backend is not set up yet. Run ",
         "install_sentiment.ai() once (TensorFlow-free) to set it up, then try again.",
         call. = FALSE)
  }

  # 0.1.1 patch to allow user specified environment

  switch(method,
    "conda"      = env_expr <- expression(reticulate::use_condaenv(envname, required = TRUE)),
    "virtualenv" = env_expr <- expression(reticulate::use_virtualenv(envname, required = TRUE))
  )


  # we need to check whether the environment is active, right??
  eval(expr  = env_expr,
       envir = r_envir)

  # pull out the python environment
  py_ver_def <- Sys.getenv("RETICULATE_PYTHON")

  # py config depends on OS - pick which one isn't NULL
  py_path    <- c(reticulate::py_discover_config()$exec_prefix,
                  reticulate::py_discover_config()$pythonhome)

  py_env_set <- vapply(py_path, normalizePath, character(1))

  # determine if environment is set correctly (if previous code returns silently)
  py_env_ok  <- any(endsWith(py_env_set, envname))

  # double check if system environment is set
  tryCatch(
    expr = {
      if(!py_env_ok){
        stop("")
      }
    },
    error = function(cond){
      if(nchar(py_ver_def) > 0){
        # doesn't work if in projects unless the RETICULATE_PYTHON is set correctly
        text <- c("The RETICULATE_PYTHON environment variable is set, which can be due to",
                  "being in a project (regardless of whether the global/project Python options",
                  "are set), having the global/project Python options set, or having",
                  "RETICULATE_PYTHON in your .Renviron file or bash/zsh rc files.")
      } else{
        text <- c("RETICULATE_PYTHON environment variable is not set, so that shouldn't be the problem.",
                  "Double check reticulate/python are installed correctly, that the corrdct dependencies are installed, and that the environment is loading.",
                  "In testing, this also happened when using Microsoft R Open and reticulate 1.6.")
      }

      head_text <- "Internal error when checking that the environment is active."
      tail_text <- c("We appreciate that getting conda environments correct with reticulate is a pain!",
                     "If you have difficulties with environments in RStudio, go to tools>Global Options>Python.",
                     "From there you can force RStudio to use the proper environment if reticulate isn't working.")

      create_error_text(head_text, text, "", tail_text, "")
      stop("env ", envname, " is not active. try restarting R and/or changing default environment.",
           call. = FALSE)
    }
  )

  return(TRUE)
}

# determine the python environment
py_install_method_detect <- function(envname,
                                     conda = "auto",
                                     ...){

  if(length(conda) == 0){
    conda <- "auto"
  }

  # is just reticulate:::py_install_method_detect
  # all in local_from_reticulate.R as internal funcs
  install_method_detect(envname = envname,conda   = conda)
}



# check if parallel support is working
test_parallel_support <- function() {
  cl <- tryCatch({
    parallel::makeCluster(parallel::detectCores() - 1)
  }, error = function(e) NULL)

  if (is.null(cl)) {
    return(FALSE)
  }

  test_result <- tryCatch({
    parallel::parLapply(cl, 1:2, function(x) x + 1)
    parallel::detectCores()
  }, error = function(e) 0)

  parallel::stopCluster(cl)
  return(test_result)
}



# internal func for virtualenv - may need user to install specific python version
# create env if possible else install py if needed./
# create env in same method as install py fails for some reason - so moved out of this func
# install python if needed for virtualenv
check_virtualenv_py <- function(envname, version, ...){

  tryCatch(
    reticulate::virtualenv_create(
      envname = envname,
      version = version,
      ...
    ),
    error = function(e){

      message(e)
      # check if pyenv_python error happens
      if(grepl('Try installing it with install_python', e)){

        inst_promt <- readline(paste0("Python ", version, " is missing. Install now? [Y/n]:\n"))

        if(grepl("^Y", inst_promt, ignore.case = TRUE)){
          # install python and try again
          reticulate::install_python(version = version)

        }
      } else{
        stop(e)
      }

    }
  )

}

