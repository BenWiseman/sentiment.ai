# NOTE:
# This is all from reticulate - it's to avoid a ::: which is making issues for CRAN checks!'

# from rlang
`%||%` <-function (x, y)
  {
  if (is.null(x))
    y
  else x
  }

# reticulate - aesthetic
sprintf <- function(fmt, ...) {

  dots <- eval(substitute(alist(...)))
  if (length(dots) == 0)
    return(fmt)

  base::sprintf(fmt, ...)

}


miniconda_installable <- function(){
  meta <- miniconda_meta_read()
  !identical(meta$DisableInstallationPrompt, TRUE)
}

miniconda_enabled <- function(){
  enabled <- Sys.getenv("RETICULATE_MINICONDA_ENABLED",
                        unset = "TRUE")
  if (tolower(enabled) %in% c("false", "0"))
    return(FALSE)
  miniconda_installable()
}

miniconda_conda <- function(path = reticulate::miniconda_path())
{
  exe <- if (roperators::is.os_win())
    "condabin/conda.bat"
  else "bin/conda"
  file.path(path, exe)
}

miniconda_exists <- function (path = reticulate::miniconda_path()){
  conda <- miniconda_conda(path)
  file.exists(conda)
}

miniconda_meta_path <- function()
{
  root <- rappdirs::user_data_dir("r-reticulate")
  file.path(root, "miniconda.json")
}

miniconda_meta_read <- function()
{
  path <- miniconda_meta_path()
  if (!file.exists(path))
    return(list())
  json <- tryCatch(jsonlite::read_json(path), error = warning)
  if (is.list(json))
    return(json)
  list()
}

miniconda_meta_write <- function(data)
{
  path <- miniconda_meta_path()
  dir.create(dirname(path), recursive = TRUE)
  json <- jsonlite::toJSON(data, auto_unbox = TRUE, pretty = TRUE)
  writeLines(json, con = path)
}

miniconda_install_prompt <- function(){
  # insteractive was rlang::is_interactive - seems kinda pointless
  if (!interactive())
    return(FALSE)
  text <- paste("No non-system installation of Python could be found.",
                "Would you like to download and install Miniconda?",
                "Miniconda is an open source environment management system for Python.",
                "See https://docs.conda.io/en/latest/miniconda.html for more details.",
                "", sep = "\n")
  message(text)
  response <- readline("Would you like to install Miniconda? [Y/n]: ")
  repeat {
    ch <- tolower(substring(response, 1, 1))
    if (ch == "y" || ch == "") {
      reticulate::install_miniconda()
      return(TRUE)
    }
    if (ch == "n") {
      meta <- miniconda_meta_read()
      meta$DisableInstallationPrompt <- TRUE
      miniconda_meta_write(meta)
      message("Installation aborted.")
      return(FALSE)
    }
    response <- readline("Please answer yes or no: ")
  }
}


python_environment_resolve <- function (envname = NULL, resolve = identity)
{
  envname <- envname %||% Sys.getenv("RETICULATE_PYTHON_ENV",
                                     unset = "r-reticulate")
  if (grepl("[/\\]", envname)) {
    envname <- normalizePath(envname, winslash = "/",
                             mustWork = FALSE)
    return(envname)
  }
  resolve(envname)
}

condaenv_resolve <- function (envname = NULL)
{
  python_environment_resolve(envname = envname, resolve = identity)
}

conda_python <- function (envname = NULL, conda = "auto", all = FALSE)
{
  envname <- condaenv_resolve(envname)
  if (grepl("[/\\\\]", envname)) {
    suffix <- if (roperators::is.os_win())
      "python.exe"
    else "bin/python"
    path <- file.path(envname, suffix)
    if (file.exists(path))
      return(path)
    fmt <- "no conda environment exists at path '%s'"
    stop(sprintf(fmt, envname))
  }
  conda_envs <- reticulate::conda_list(conda = conda)
  env <- subset(conda_envs, conda_envs$name == envname)
  if (nrow(env) == 0)
    stop("conda environment '", envname, "' not found")
  python <- if (all)
    env$python
  else env$python[[1L]]
  path.expand(python)
}


condaenv_exists <- function (envname = NULL, conda = "auto")
  {
  condabin <- tryCatch(reticulate::conda_binary(conda = conda), error = identity)
  if (inherits(condabin, "error"))
    return(FALSE)
  python <- tryCatch(conda_python(envname, conda = conda),
                     error = identity)
  if (inherits(python, "error"))
    return(FALSE)
  file.exists(python)
}

python_version <- function (python)
{
  code <- "import platform; print(platform.python_version())"
  args <- c("-E", "-c", shQuote(code))
  output <- system2(python, args, stdout = TRUE, stderr = FALSE)
  sanitized <- gsub("[^0-9.-]", "", output)
  numeric_version(sanitized)
}

python_has_module <- function(python, module)
{
  code <- paste("import", module)
  args <- c("-E", "-c", shQuote(code))
  status <- system2(python, args, stdout = FALSE, stderr = FALSE)
  status == 0L
}

python_has_modules <- function(python, modules)
{
  file <- tempfile("reticulate-python-", fileext = ".py")
  code <- paste("import", modules)
  writeLines(code, con = file)
  on.exit(unlink(file), add = TRUE)
  status <- system2(python, shQuote(file), stdout = FALSE,
                    stderr = FALSE)
  status == 0L
}

virtualenv_default_python <- function (python = NULL)
{
  if (!is.null(python))
    return(path.expand(python))
  # removing .globals$required_python_version
  pythons <- c(Sys.getenv("RETICULATE_PYTHON"), Sys.which("python3"), Sys.which("python"))
  for (python in pythons) {
    if (!file.exists(python))
      next
    version <- tryCatch(suppressWarnings(python_version(python)),
                        error = identity)
    if (inherits(version, "error"))
      next
    py2_modules <- c("pip", "virtualenv")
    py3_modules <- c("pip", "venv")
    modules <- ifelse(version < 3, py2_modules, py3_modules)
    if (!python_has_modules(python, modules))
      next
    return(normalizePath(python, winslash = "/"))
  }
  config <- reticulate::py_discover_config()
  normalizePath(config$python, winslash = "/")
}


install_method_detect <- function (envname, conda = "auto")
{
  if (reticulate::virtualenv_exists(envname))
    return("virtualenv")
  if (miniconda_enabled() && miniconda_installable() && !miniconda_exists())
    miniconda_install_prompt()
  if (condaenv_exists(envname, conda = conda))
    return("conda")
  python <- virtualenv_default_python()
  if (python_has_module(python, "virtualenv") || python_has_module(python,
                                                                   "venv"))
    return("virtualenv")
  conda <- tryCatch(reticulate::conda_binary(conda = conda), error = identity)
  if (!inherits(conda, "error"))
    return("conda")
  "virtualenv"
}
