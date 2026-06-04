# Internal helpers vendored from the 'reticulate' package.
#
# WHY THIS FILE EXISTS
# --------------------
# `reticulate` does not export its Python-install-method heuristic
# (`reticulate:::py_install_method_detect()` and the private functions it calls).
# Reaching into it with `:::` is a hard CRAN rejection
# ("Unexported objects imported by ':::' calls"), so the minimal subset of that
# heuristic is reproduced here. Everything that reticulate *does* export is called
# via the public `reticulate::` API instead of being copied (see below) — only the
# genuinely-unexported logic is vendored.
#
# Public reticulate API used in place of former vendored copies:
#   reticulate::conda_python()      (was a local conda_python; it also resolves
#                                    the envname internally, so the former local
#                                    condaenv_resolve/python_environment_resolve
#                                    helpers were dropped as dead code)
#   reticulate::condaenv_exists()   (was a local condaenv_exists)
#   reticulate::conda_binary(), reticulate::conda_list(),
#   reticulate::virtualenv_exists(), reticulate::py_discover_config(),
#   reticulate::miniconda_path(), reticulate::install_miniconda()
#
# Still vendored (no public equivalent in reticulate):
#   install_method_detect(), virtualenv_default_python(), python_version(),
#   python_has_module(), python_has_modules(), and the miniconda_* meta/prompt
#   helpers reachable only from the install-method heuristic.
#
# Attribution: the vendored functions below are adapted from 'reticulate'
# (https://github.com/rstudio/reticulate), licensed under Apache License 2.0.
# Copyright (c) Posit Software, PBC. Behaviour is preserved deliberately so this
# matches reticulate's own install-method resolution.

miniconda_installable <- function() {
  meta <- miniconda_meta_read()
  !identical(meta$DisableInstallationPrompt, TRUE)
}

miniconda_enabled <- function() {
  enabled <- Sys.getenv("RETICULATE_MINICONDA_ENABLED", unset = "TRUE")
  if (tolower(enabled) %in% c("false", "0"))
    return(FALSE)
  miniconda_installable()
}

miniconda_conda <- function(path = reticulate::miniconda_path()) {
  exe <- if (roperators::is.os_win())
    "condabin/conda.bat"
  else "bin/conda"
  file.path(path, exe)
}

miniconda_exists <- function(path = reticulate::miniconda_path()) {
  conda <- miniconda_conda(path)
  file.exists(conda)
}

miniconda_meta_path <- function() {
  # reticulate stores this meta file under rappdirs::user_data_dir("r-reticulate").
  # Keep reading the *same* location so the user's "do not prompt me" choice stays
  # in sync with reticulate. rappdirs is a Suggests dependency, so it is used
  # conditionally (CRAN policy); fall back to base R's tools::R_user_dir() when
  # it is unavailable.
  root <- if (requireNamespace("rappdirs", quietly = TRUE)) {
    rappdirs::user_data_dir("r-reticulate")
  } else {
    tools::R_user_dir("r-reticulate", which = "data")
  }
  file.path(root, "miniconda.json")
}

miniconda_meta_read <- function() {
  path <- miniconda_meta_path()
  if (!file.exists(path))
    return(list())
  json <- tryCatch(jsonlite::read_json(path), error = warning)
  if (is.list(json))
    return(json)
  list()
}

miniconda_meta_write <- function(data) {
  path <- miniconda_meta_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  json <- jsonlite::toJSON(data, auto_unbox = TRUE, pretty = TRUE)
  writeLines(json, con = path)
}

miniconda_install_prompt <- function() {
  # `interactive()` guard is essential: never prompt under R CMD check / non-interactive.
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


python_version <- function(python) {
  code <- "import platform; print(platform.python_version())"
  args <- c("-E", "-c", shQuote(code))
  output <- system2(python, args, stdout = TRUE, stderr = FALSE)
  sanitized <- gsub("[^0-9.-]", "", output)
  numeric_version(sanitized)
}

python_has_module <- function(python, module) {
  code <- paste("import", module)
  args <- c("-E", "-c", shQuote(code))
  status <- system2(python, args, stdout = FALSE, stderr = FALSE)
  status == 0L
}

python_has_modules <- function(python, modules) {
  file <- tempfile("reticulate-python-", fileext = ".py")
  code <- paste("import", modules)
  writeLines(code, con = file)
  on.exit(unlink(file), add = TRUE)
  status <- system2(python, shQuote(file), stdout = FALSE, stderr = FALSE)
  status == 0L
}

virtualenv_default_python <- function(python = NULL) {
  if (!is.null(python))
    return(path.expand(python))
  # removing .globals$required_python_version (reticulate internal state)
  pythons <- c(Sys.getenv("RETICULATE_PYTHON"), Sys.which("python3"), Sys.which("python"))
  for (python in pythons) {
    if (!file.exists(python))
      next
    version <- tryCatch(suppressWarnings(python_version(python)), error = identity)
    if (inherits(version, "error"))
      next
    py2_modules <- c("pip", "virtualenv")
    py3_modules <- c("pip", "venv")
    modules <- if (version < 3) py2_modules else py3_modules
    if (!python_has_modules(python, modules))
      next
    return(normalizePath(python, winslash = "/"))
  }
  config <- reticulate::py_discover_config()
  normalizePath(config$python, winslash = "/")
}


# The one heuristic with no public reticulate equivalent. Equivalent to the
# (unexported) reticulate:::py_install_method_detect(); kept vendored so we never
# call into reticulate's namespace with `:::`.
install_method_detect <- function(envname, conda = "auto") {
  if (reticulate::virtualenv_exists(envname))
    return("virtualenv")
  if (miniconda_enabled() && miniconda_installable() && !miniconda_exists())
    miniconda_install_prompt()
  if (reticulate::condaenv_exists(envname, conda = conda))
    return("conda")
  python <- virtualenv_default_python()
  if (python_has_module(python, "virtualenv") || python_has_module(python, "venv"))
    return("virtualenv")
  conda <- tryCatch(reticulate::conda_binary(conda = conda), error = identity)
  if (!inherits(conda, "error"))
    return("conda")
  "virtualenv"
}
