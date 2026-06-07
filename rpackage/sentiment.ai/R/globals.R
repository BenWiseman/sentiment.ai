utils::globalVariables(
  names = c(
    ".",
    # package-level registry objects defined in constants.R
    "DEFAULT_MODEL", "DEFAULT_SCORING",
    "default_models", "openai_models", "legacy_models",
    "model_dims", "model_prefix",
    # embedder factories sourced from inst/get_embedder.py via reticulate::source_python
    "load_st_embedder", "load_hub_embedder", "load_hub_embedding"
  )
)

objects <- new.env()
