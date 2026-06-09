# Resolve a user-facing model name to its backend id.
#
# Returns the HuggingFace id for default (sentence-transformers) models, the
# OpenAI model name for API models, or the full TF-Hub URL for legacy USE models.
# Unknown names are passed through with a warning ("cowboy mode").
# (internal; not exported)
choose_model <- function(model){

  model <- model[1]

  if(length(model) <= 0 || is.na(model)){
    stop("model is of length 0 and needs to be length 1", call. = FALSE)
  }

  # default (v2) sentence-transformers models -> HuggingFace id
  if(model %in% names(default_models)) return(unname(default_models[model]))
  if(model %in% default_models)        return(model)

  # OpenAI API models -> model name
  if(model %in% names(openai_models))  return(unname(openai_models[model]))
  if(model %in% openai_models)         return(model)

  # opt-in end-to-end transformer classifiers (RoBERTa / XLM-R) -> HuggingFace id
  if(model %in% names(classifier_models)) return(unname(classifier_models[model]))
  if(model %in% classifier_models)        return(model)

  # legacy USE models -> full TF-Hub URL (opt-in, requires TensorFlow)
  if(model %in% names(legacy_models)){
    return(paste0("https://tfhub.dev/google/", unname(legacy_models[model])))
  }
  if(model %in% legacy_models){
    return(paste0("https://tfhub.dev/google/", model))
  }

  # unknown -> pass through, but warn
  warning(model, " is not one of the built-in models: ",
          paste(c(names(default_models), names(openai_models),
                  names(classifier_models), names(legacy_models)),
                collapse = ", "),
          ". Passing it through as-is. You're on your own from here, cowboy! Godspeed!",
          call. = FALSE)

  model
}
