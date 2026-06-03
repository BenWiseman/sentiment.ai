utils::globalVariables(
  names = c(
    ".",
    # package-level registry objects defined in constants.R
    "DEFAULT_MODEL", "default_models", "openai_models", "legacy_models",
    "model_dims", "model_prefix"
  )
)

objects <- new.env()
