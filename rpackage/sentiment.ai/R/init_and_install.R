

#' Activate sentiment.ai environment in reticulate
.activate_env <- function(envname = "r-sentiment-ai", silent=FALSE, r_envir = -1){
    #TODO: add method argument
    # I think virtualenv may be easier to make within a package
    venv_list  <- reticulate::virtualenv_list()
    conda_list <- reticulate::conda_list()$name

    # if r-sentiment-ai virtual environment doesn't exist, give warning!
    if(envname %ni% c(venv_list, conda_list)){
        warning("sentiment.ai environment has not been set up yet.\n
                Run: sentiment.ai.install() to configure the underlying python stuff\n")
    } else{
        # Activate environment
        if(envname %in% conda_list) {
            eval(reticulate::use_condaenv(envname, required = TRUE), envir = r_envir)
            if(!silent) message("Activated condaenv: ", envname)
        } else {
            eval(reticulate::use_virtualenv(envname, required = TRUE), envir = r_envir)
            if(!silent) message("Activated virtualenv:", envname)
        }
    }
}

#' Sets up environment specific for sentiment.as
#' Currently needs:
#'
#' | Module          | Version |
#' | :-------------- | :------:|
#' |python           | 3.7.10  |
#' |numpy            | 1.19.5  |
#' |tensorflow       | 2.4.1   |
#' |tensorflow_hub   | 0.12.0  |
#' |tensorflow-text  | 2.4.3   |
#' |sentencepiece    | 0.1.95  |
#'
#' Installs with like tensorflow::install_tensorflow with pip=TRUE
#'
#' @param method Installation method. By default, "auto" automatically finds a
#'   method that will work in the local environment. Change the default to force
#'   a specific installation method. Note that the "virtualenv" method may not
#'   available on Windows (issue in TensorFlow). Note also
#'   that since this command runs without privilege the "system" method is
#'   available only on Windows.
#'
#' @param gpu logical, should GPU enabled TensorFlow be installed?
#'
#' @export
#' @rdname setup
install_sentiment.ai <- function(envname = "r-sentiment-ai",
                                 method  = c("auto", "virtualenv", "conda"),
                                 gpu     = FALSE,
                                 conda   = "auto",
                                 python_version = "3.7.10",
                                 modules = list(numpy             = "1.19.5",
                                                sentencepiece     = "0.1.95",
                                                tensorflow        = "2.4.1",
                                                tensorflow_hub    = "0.12.0",
                                                `tensorflow-text` = "2.4.3"),
                                 restart_session = TRUE,
                                 ...){

    method <- match.arg(method)
    # # Step 1: create environment shouldn't be necessary...
    # switch (method,
    #     "virtualenv" = reticulate::virtualenv_create(envname),
    #     "conda"      = reticulate::conda_create(envname)
    # )

    # Step 2: parse tensorflow version name (done to allow gpu on/off)
    #   First append GPU flag
    tensorflow_ver <- paste(c("tensorflow", "gpu"[gpu]), collapse="-")
    #   Then append version
    tensorflow_ver <- paste0(tensorflow_ver, "==", modules$tensorflow)

    # Step 3: parse other modulenames & versions
    modules_ver    <- paste0(names(modules), "==", modules)
    # remove tensorflow from list
    modules_ver    <- modules_ver[names(modules) %ni% "tensorflow"]

    # Step 4: install into packages environment
    reticulate::py_install(
        packages = c(tensorflow_ver, modules_ver),
        envname  = envname,
        method   = method,
        conda    = conda,
        python_version = python_version,
        pip      = TRUE,
        ...
    )

    message("Successfully created ", method, " environment: ", envname)

    #Finally restart session
    if (restart_session && rstudioapi::hasFun("restartSession"))
        rstudioapi::restartSession()

    invisible(NULL)
}

#' Creates model object, speed up sessions and such
#' @param model path to tensorflow hub embedding model.default are universal sentence encoder  en (default) and multi
#' @export
#' @rdname setup
sentiment.ai.init <- function(model = c("en", "multi"),
                              envname = "r-sentiment-ai"){

    model = model[1]

    .activate_env(envname, silent = FALSE, r_envir = -2)
    message("Preparing Model")
    reticulate:::source_python(system.file("get_embedder.py", package = "sentiment.ai"))

    default_models <- c(en    = "https://tfhub.dev/google/universal-sentence-encoder-large/5",
                        multi = "https://tfhub.dev/google/universal-sentence-encoder-multilingual-large/3")

    # extract defaults
    if(tolower(model) %in% c(names(default_models), default_models) ) {
        model = default_models[model]
    } else{
        warning("
                model argument not in c('en', 'multi')
                Overriding the defaults is allowed, but may or may not work!
                You're on your own from here on, cowboy! Godspeed!
                ")
    }

    #parse cache folder
    # Need to do this ele it'll save to temp which will throw a fit after you restart.
    # Causes error OSError: SavedModel file does not exist at:  path/to/temp/dir
    # b'jesus I hate this kind of fuckery! - hub_load should have a cache_dir argument or a failover!

    # base path for package
    pkg_path   <- system.file(package = "sentiment.ai")

    # If manually saving each
    model_dir  <- gsub("^https://[a-z]+\\.[a-z]+/[a-z]+/", "", model)
    model_name <- gsub("/.*$", "", model_dir)
    model_ver  <- gsub("^[a-z -]+/", "", model_dir)

    # for setting generic cache folder (hopefuly works!)
    cache_dir  <- file.path(pkg_path, "tfhub_modules")

    # If model hasn't been downloaded already
    if(!dir.exists(cache_dir)){
        dir.create(cache_dir) # for manual DL, need to create each level of name, version
    }

    # Make global (saves a lot of time downstream)
    sentiment.ai.embed <<- load_language_model(model, cache_dir)

}
