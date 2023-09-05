#' setup
#' @description Install and Setup sentiment.ai package
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
#' @param fresh_install Whether to create the Python environment prior to
#'        installing the modules or to install everything in an existing
#'        environment (if one exists). Only change this argument if you know what
#'        you are doing! If the environment does not already exist, will create
#'        the environment first.
#' @param restart_session Whether to restart the R session after finishing
#'        installation. Only works on Rstudio.
#' @param model path to tensorflow hub embedding model. default is both universal
#'        sentence encoder en (default) and multi.
#' @details
#' Sets up environment specific for sentiment.ai. The packages that it currently
#' needs are as follows:
#'
#' | Module          | Version |
#' | :-------------- | :-----: |
#' | python          | 3.8.10  |
#' | numpy           | 1.19.5  |
#' | tensorflow      | 2.4.1   |
#' | tensorflow_hub  | 0.12.0  |
#' | tensorflow-text | 2.4.3   |
#' | sentencepiece   | 0.1.95  |
#'
#' Please do not change these unless you know what you are doing.
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
#' install_sentiment.ai(envname = "r-sentiment-ai",
#'                      method  = "conda",
#'                      python_version = "3.8.10")
#' init_sentiment.ai(model   = "en.large",
#'                   envname = "r-sentiment-ai")
#' check_sentiment.ai()
#'
#' # if you run into an issue, follow the instructions/see the note and retry!
#' }


# 1. INSTALL ===================================================================


# install environment and dependencies\

#' @rdname setup
#' @return NULL this function simply installs the required python dependencies and default scoring models and pre-calculated embedding vectors.
#' @importFrom roperators "%ni%"
#' @import tensorflow
#' @import tfhub
#' @export
install_sentiment.ai <- function(envname = "r-sentiment-ai",
                                 method  = c("auto", "virtualenv", "conda"),
                                 gpu     = FALSE,
                                 python_version = "3.8.10",
                                 modules = list(numpy             = "1.19.5",
                                                sentencepiece     = "0.1.95",
                                                tensorflow        = "2.4.1",
                                                tensorflow_hub    = "0.12.0",
                                                `tensorflow-text` = "2.4.3",
                                                openai            = "latest"),
                                 fresh_install   = TRUE,
                                 restart_session = TRUE,
                                 ...){

  # STILL HAVE INSTALL ISSUES ... SHOULD DO SOMETHING ABOUT THIS ???
  # Apple silicone warning
  if(roperators::is.os_arm() && roperators::is.os_mac()){
    warning("Apple Silicone detected. Unfortunately Tensorflow-text is not available for apple silicone yet.\n
            As a work-around, you can use the openai to do the embeddings. \n
            To do this, skip installing,and in the call to init_sentiment.ai set the environment to be r-reticulate (bypass tensorflow install) and the model to 'text-embedding-ada-002'. ")
  }



  method <- match.arg(method)

  # if environment is missing, set it to r-sentiment-ai
  if(length(envname) == 0){
    envname <- "r-sentiment-ai"
  }

  # if method is default, figure out method using reticulate
  if(method == "auto"){
    method   <- py_install_method_detect(
      envname = envname,
      ...
    )
  }

  # 1. parse tensorflow version name -----------------------------------------

  tf_module <- "tensorflow"

  # make sure tensorflow is in the modules list
  if(tf_module %ni% names(modules)){
    stop("tensorflow version must be specified in 'modules'",
         call. = FALSE)
  }

  # make sure gpu is TRUE or FALSE
  gpu <- isTRUE(gpu)

  if(gpu && roperators::is.os_mac()){
    warning("gpu not available for OSX; setting gpu flag to FALSE",
            call. = FALSE)
    gpu <- FALSE
  } else if(gpu){
    message("gpu flag is TRUE; installation needs CUDA configured")
    names(modules)[names(modules) %in% tf_module] <- paste0(
      tf_module, "-", "gpu"
    )
  }

  # 2. parse other module names and versions -----------------------------------

  # make sure that all modules have length 1
  stopifnot(all(lengths(modules) == 1))

  # TODO: find a permanent solution for apple silicone
  # Remove tensorflow-text for Apple Silicon
  if (roperators::is.os_arm() && roperators::is.os_mac()) {
    modules[["tensorflow-text"]] <- NULL
  }


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

  reticulate::py_install(
    packages       = modules_vers,
    envname        = envname,
    method         = method,
    python_version = python_version,
    pip            = TRUE,
    ...
  )

  message("Successfully created ", method, " environment: ", envname)

  # Since they already have internet here, install.
  message("Installing default scoring model from Github")
  install_scoring_model(scoring = "xgb")
  install_scoring_model(scoring = "glm")

  # also pull precalculated embeddings
  message("Instaling pre-calculated embeddings from Github")
  install_default_embeddings()

  # restart session if needed
  if(restart_session && rstudioapi::hasFun("restartSession")){
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
#'     The default repo_url is "https://github.com/BenWiseman/sentiment.ai/blob/main/models"
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
install_scoring_model <- function(model   =  c("en.large", "en", "multi.large", "multi"),
                                  scoring = c("xgb", "glm"),
                                  scoring_version = "1.0",
                                  ...){

  # for return status
  status <- 0

  # passthrough optional repo_url
  opts <- list(...)

  if(is.null(opts$repo_url)) {
    repo_url <- "https://github.com/BenWiseman/sentiment.ai/raw/main/models"
  } else{
    repo_url <- opts$repo_url
  }

  # Remove match.arg not used - give flexibility.
  model   <- match.arg(model)
  scoring <- scoring[1]
  scoring_version <- scoring_version[1]

  # glm models will be plain text for max compatibility
  file_ext   <- if(scoring == "glm") "csv" else scoring
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

#' Function to grab the default embeddings for `sentiment_match()`
#' Necessary to keep package size under 5Mb.
#' Will check if they're there, if so return TRUE.
#' If they are not there, try download and return TRUE.
#' Otherwise, return FALSE (and generate them - will take a few seconds!).
install_default_embeddings <- function(){
  # for return status
  status <- 0

  # base folder
  repo_url <- "https://github.com/BenWiseman/sentiment.ai/raw/main/default_embeddings"

  # to get right version
  version   <- "0.1.0"# utils::packageDescription("sentiment.ai", fields = "Version")
  file_name <- paste0(version, ".json") # update version manually when default embeddings change!

  # base url - repo containing model objects
  target_url <- paste(repo_url, file_name, sep = "/")

  # Add query param to end
  target_url <- paste0(target_url, "?raw=true")

  # get download location: <pkg_dir>/data/en.large_def_emb.rds
  # determining package name and base path
  pkg_path <- system.file(package = "sentiment.ai")
  dl_path  <- file.path(pkg_path, "default_embeddings")
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

  message("Downloading precalculated default embeddings v.", version, " from github")

  tryCatch({
    # you can always use wb to download bites
    utils::download.file(url      = target_url,
                         destfile = obj_path,
                         mode     = "wb")
    status <- 1

  }, error=function(e){

    message("Attempt to pull pre-calculated embeddings failed.\n",
            "This should only be a problem for speed as they can still be calculated on the fly!\n")

  })

  return(status)

}
# 2. INITIALIZE ================================================================


#' Initialize sentiment.ai Environment
#' @rdname setup
#'
#' @description Initializes the sentiment.ai environment by setting up the Python environment and loading the specified model.
#' This function must be called before using any other functions in the package that require a model.
#'
#' @param model character vector specifying the model to use. Options include "en.large", "multi.large", "en", "multi", and "text-embedding-ada-002".
#' @param envname character string specifying the name of the Python environment to use.
#' @param method character string specifying the method to use for Python environment management. Options are "auto", "virtualenv", and "conda".
#' @param silent logical flag indicating whether to suppress console logging. Note: This does not affect TensorFlow, GPU, Python, or C++ output.
#' @param api_key character string specifying the API key for OpenAI, if using an OpenAI model.
#' @param api_base character string specifying the base URL for the OpenAI API.
#'
#' @return Python function for text embedding. This is stored in the package environment and does not need to be explicitly used.
#'
#' @importFrom reticulate py_run_string source_python
#' @importFrom utils packageName system.file
#'
#' @export
init_sentiment.ai <- function(model       = c("en.large", "multi.large", "en", "multi", "text-embedding-ada-002"),
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

  # can reduce GPU out of memory issues
  Sys.setenv("TF_FORCE_GPU_ALLOW_GROWTH" = "true")

  # activating environment (what if the environment won't activate??)
  .activate_env(envname, silent = silent, r_envir = -2, method=method)

  # 2. preparing model ---------------------------------------------------------

  # make sure load_language_model is NULL to suppress NOTE
  load_language_model <- NULL

  # load things (including load_language_model)
  if(!silent) message("Preparing Model")
  reticulate::source_python(
    system.file("get_embedder.py", package = pkg_name)
  )

  # pull out model link
  model <- choose_model(model)

  # parse cache folder
  # (needed or it will save to temp, which will through a fit after restart due to
  # "OSError: SavedModel file does not exist at: path/to/temp/dir")

  # pulling out the directory/name/version
  model_dir  <- gsub(x           = model,
                     pattern     = ".*\\/(.*\\/)(.*$)",
                     replacement = "\\1\\2")
  model_dir  <- strsplit(x     = model_dir,
                         split = "/")[[1]]

  model_name <- model_dir[1]
  model_ver  <- model_dir[2]

  # for setting generic cache folder (hopefuly works!)
  cache_dir  <- file.path(pkg_path, "tfhub_modules")

  # make sure the directory has been created
  # (for manual DL, need to create each level of name, version???)
  dir.create(path         = cache_dir,
             showWarnings = FALSE,
             recursive    = TRUE)


  # create sentiment.env object and make it global IN the package
  env <- sentiment.ai::sentiment.env
  env$parallel <- test_parallel_support()

  if(env$parallel >= 2) message(env$parallel, " CPU cores available for parallel processing")
  else message("No parallel processing support found on CPU")

  # if using openai
  if (!is.null(api_key) && model %in% c(names(openai_models), openai_models)) {
    # If API key is provided, use OpenAI
    #env$embed <- py_run_string("load_openai_embedding('text-embedding-ada-002', your_api_key, your_api_base)")
    env$embed  <- load_openai_embedding(model, api_key, api_base, api_version, api_type, api_engine)
    env$openai <- TRUE # create flag

  } else {
    # Default to  TF Hub
    # allow silent/less log text!
    if(!silent){
      # do it with all the details
      env$embed <- load_hub_embedding(model, cache_dir)
    } else{
      # just give reduced message that it'll take a while
      message("Loading language model...")
      env$embed <- suppressMessages(load_hub_embedding(model, cache_dir))
    }
  }




  env$embed
}

#' @rdname setup
#' @return NULL this function checks if `init_sentiment.ai()` has been called
#'         successfully, if not, it is called.
#' @export
check_sentiment.ai <- function(...){

  if(is.null(sentiment.ai::sentiment.env$embed)){
    message("Preparing model (this may take a while).\n",
            "Consider running init_sentiment.ai().")
    init_sentiment.ai(...)
  } else{
    # commented out for now - this may get annoying for users to see every time
    # ...especially if they *apply instead of passing in a vector for some reason!
    # message("sentiment.env$embed found in environment.\n",
    #         "To change model, call init_sentiment.ai().")
  }

  return(NULL)
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

  # exception handle
  auto_failover <- function(e){
    message(e)
    if(!silent) message("Attempting failover which will prioritise conda if available.")
    # change env method based on where envname is found
    if(method == "auto" && envname %in% venv_envs) assign(method, value = "virtualenv", pos = -2, inherits = TRUE)
    # priority to conda (not better, just most popular)
    if(method == "auto" && envname %in% conda_envs) assign(method, value = "conda", pos = -2, inherits = TRUE)
  }

  # find method
  if(method == "auto"){
    # Find preferred install method
    tryCatch(method <- suppressWarnings(py_install_method_detect()),
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
    stop(envname, " environment not available. ",
         "run sentiment.ai_install() on envname '", envname, "' first.",
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
    makeCluster(parallel::detectCores() - 1)
  }, error = function(e) NULL)

  if (is.null(cl)) {
    return(FALSE)
  }

  test_result <- tryCatch({
    parLapply(cl, 1:2, function(x) x + 1)
    parallel::detectCores()
  }, error = function(e) 0)

  stopCluster(cl)
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

