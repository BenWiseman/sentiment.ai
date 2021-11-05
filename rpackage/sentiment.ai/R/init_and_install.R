#' Install and Setup sentiment.ai Algorithm
#'
#' @inheritParams reticulate::py_install
#' @param method Installation method. By default, "auto" automatically finds a
#'        method that will work in the local environment. Change the default to
#'        force a specific installation method. Note that the "virtualenv"
#'        method may not available on Windows due to a tensorflow issue. Note
#'        also that since this command runs without privilege the "system"
#'        method is available only on Windows.
#' @param gpu Whether GPU should be enabled when installing TensorFlow
#' @param modules A list of modules needed for installing tensorflow. See
#'        details for more information. Only change this argument if you know
#'        what you are doing!
#' @param restart_session Whether to restart the R session after finishing
#'        installation. Only works on Rstudio.
#' @param model path to tensorflow hub embedding model. default is both universal
#'        sentence encoder  en (default) and multi
#'
#' @details
#' Sets up environment specific for sentiment.ai. The packages that it currently
#' needs are as follows:
#'
#' | Module          | Version |
#' | :-------------- | :-----: |
#' | python          | 3.7.10  |
#' | numpy           | 1.19.5  |
#' | tensorflow      | 2.4.1   |
#' | tensorflow_hub  | 0.12.0  |
#' | tensorflow-text | 2.4.3   |
#' | sentencepiece   | 0.1.95  |
#'
#' Please do not change these if you do not know what you are doing.
#'
#' Note that it installs with like \code{tensorflow::install_tensorflow} and
#' \code{pip = TRUE}
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
#'    variable, either through the Global or Project Options of by using heuristics.
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
#' install_sentiment.ai(envname = "r-sentiment-ai",
#'                      method  = "conda",
#'                      python_version = "3.7.10")
#' init_sentiment.ai(model   = "en.large",
#'                   envname = "r-sentiment-ai")
#' check_sentiment.ai()
#'
#' # if you run into an issue, follow the instructions/see the note and retry!
#' }

# 1. INSTALL ===================================================================

#' @export
#' @rdname setup
install_sentiment.ai <- function(envname = "r-sentiment-ai",
                                 method  = c("auto", "virtualenv", "conda"),
                                 gpu     = FALSE,
                                 python_version = "3.7.10",
                                 modules = list(numpy             = "1.19.5",
                                                sentencepiece     = "0.1.95",
                                                tensorflow        = "2.4.1",
                                                tensorflow_hub    = "0.12.0",
                                                `tensorflow-text` = "2.4.3"),
                                 restart_session = TRUE,
                                 ...){

  method <- match.arg(method)

  # 1. parse tensorflow version name -----------------------------------------

  tf_module <- "tensorflow"

  # make sure tensorflow is in the modules list
  if(tf_module %ni% names(modules)){
    stop("tensorflow version must be specified in 'modules'",
         call. = FALSE)
  }

  # make sure gpu is TRUE or FALSE
  gpu <- isTRUE(gpu)

  if(gpu && Sys.info()["sysname"] %in% "Darwin"){
    warning("gpu not available for OSX; setting gpu flag to FALSE",
            call. = FALSE)
    gpu <- FALSE
  } else if(gpu){
    message("gpu flag is TRUE; installation needs CUDA configured",
            call. = FALSE)
    names(modules)[names(modules) %in% tf_module] <- paste0(
      tf_module, "-", "gpu"
    )
  }

  # 2. parse other module names and versions -----------------------------------

  # make sure that all modules have length 1
  stopifnot(all(lengths(modules) == 1))

  # paste name with the version (should we check to make sure this is done OK?)
  modules_vers <- paste0(names(modules), "==", modules)

  # 3. install everything ------------------------------------------------------
  reticulate::py_install(
    packages       = modules_vers,
    envname        = envname,
    method         = method,
    python_version = python_version,
    pip            = TRUE,
    ...
  )

  message("Successfully created ", method, " environment: ", envname)

  # restart session if needed
  if(restart_session && rstudioapi::hasFun("restartSession")){
    rstudioapi::restartSession()
  }

  invisible(NULL)
}

# 2. INITIALIZE ================================================================

#' @export
#' @rdname setup
init_sentiment.ai <- function(model   = c("en.large", "multi.large", "en", "multi"),
                              envname = "r-sentiment-ai"){

  # determining package name and base path
  pkg_name <- packageName()
  pkg_path <- system.file(package = pkg_name)

  # can reduce GPU out oof memory issues
  Sys.setenv("TF_FORCE_GPU_ALLOW_GROWTH" = TRUE)

  # activating environment (what if the environment won't activate??)
  .activate_env(envname, silent = FALSE, r_envir = -2)

  # 2. preparing model ---------------------------------------------------------
  message("Preparing Model")
  reticulate:::source_python(
    system.file("get_embedder.py", package = pkg_name)
  )

  # pull out model link
  model <- choose_model(model)

  # parse cache folder
  # (needed or it will save to temp, which will through a fit after restart due to
  # "OSError: SavedModel file does not exist at: path/to/temp/dir")

  # pulling out the directory/name/version
  model_dir  <- gsub(x       = model,
                     pattern = ".*\\/(.*\\/)(.*$)",
                     replace = "\\1\\2")
  model_dir  <- strsplit(x     = model_dir,
                         split = "/")[[1]]

  model_name <- model_dir[1]
  model_ver  <- model_dir[2]

  # for setting generic cache folder (hopefuly works!)
  cache_dir  <- file.path(pkg_path, "tfhub_modules")

  # make sure the directory has been created
  # (for manual DL, need to create each level of name, version???)
  dir.create(path = cache_dir,
             showWarnings = FALSE,
             recursive    = TRUE)

  # create sentiment.ai_embed object and make it global IN the package
  sentiment.ai_embed <<- load_language_model(model, cache_dir)
}

#' @export
#' @rdname setup
check_sentiment.ai <- function(...){

  if(!exists("sentiment.ai_embed") || is.null(sentiment.ai_embed)){
    message("Preparing model (this may take a while).\n",
            "Consider running init_sentiment.ai().")
    init_sentiment.ai(...)
  } else{
    message("sentiment.ai_embed found in environment.\n",
            "To change model, call init_sentiment.ai().")
  }

  return(NULL)
}

# 3. HELPER FUNCTIONS ==========================================================


#' Activate sentiment.ai Environment
.activate_env <- function(envname = "r-sentiment-ai",
                          silent  = FALSE,
                          r_envir = -1){

  #TODO: add method argument

  # pull the environment lists
  venv_envs  <- reticulate::virtualenv_list()
  conda_envs <- reticulate::conda_list()$name
  all_envs   <- c(venv_envs, conda_envs)

  # make sure the environment name is in the list
  stopifnot(is.character(envname),
            length(envname) == 1)

  if(envname %ni% all_envs){
    stop(envname, " environment not available. ",
         "run sentiment.ai_install() on envname '", envname, "' first.",
         call. = FALSE)
  }

  # activate the environment
  if(envname %in% conda_envs){
    env_expr <- expression(reticulate::use_condaenv(envname, required = TRUE))
  } else{
    env_expr <- expression(reticulate::use_virtualenv(envname, required = TRUE))
  }

  # we need to check whether the environment is active, right??
  eval(expr  = env_expr,
       envir = r_envir)

  # pull out the python environment
  py_ver_def <- Sys.getenv("RETICULATE_PYTHON")
  py_env_set <- reticulate::py_discover_config()$exec_prefix

  # determine if environment is set correctly (if previous code returns silently)
  py_env_ok  <- endsWith(py_env_set, envname)

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
        # doesn't work with MS open (should we check this?)
        text <- c("This happens when using Microsoft R Open and reticulate 1.6/tensorflow 2.2.",
                  "You can upgrade those dependencies or check that the environment is loading.")
      }

      head_text <- "Internal error when checking that the environment is active."
      tail_text <- c("We appreciate that getting conda environments correct with reticulate is a pain!",
                     "If you have difficulties with environments in RStudio, go to tools>Global Options>Python.",
                     "From there you can force RStudio to use the proper environment if reticulate isn't working.")

      create_error_text(head_text, text, "", tail_text)
      stop("env ", envname, " is not active. try restarting R and/or changing default environment.",
           call. = FALSE)
    }
  )

  return(TRUE)
}
