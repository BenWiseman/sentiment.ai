# 1. EMBEDDINGS/MODELS =========================================================

# Default (v2) embedding models: multilingual, on-device, NO TensorFlow.
# Resolved through sentence-transformers (via reticulate). Names are the
# user-facing handles; values are the HuggingFace model ids.
default_models <- c(`e5-small` = "intfloat/multilingual-e5-small",
                    `e5-base`  = "intfloat/multilingual-e5-base")

# OpenAI embedding models (API backend).
openai_models <- c(
  openai                   = "text-embedding-3-small",   # shorthand alias (-> 3-small)
  `text-embedding-3-small` = "text-embedding-3-small",
  `text-embedding-3-large` = "text-embedding-3-large",
  `text-embedding-ada-002` = "text-embedding-ada-002"
)

# Opt-in end-to-end transformer sentiment classifiers ("if you can't beat 'em, join
# 'em"). These BYPASS the embed -> JSON-head pipeline: the transformer outputs
# neg/neu/pos directly via reticulate (transformers + torch, already pulled in by
# sentence-transformers). Heavier (a full ~500MB-1GB model downloads on first use) and
# they produce NO hate/mixed/style flags -- those need the e5 embedding space, so use an
# e5 model for flags. twitter-roberta is English-only; xlm-roberta is multilingual.
classifier_models <- c(
  `twitter-roberta` = "cardiffnlp/twitter-roberta-base-sentiment-latest",
  `xlm-roberta`     = "cardiffnlp/twitter-xlm-roberta-base-sentiment"
)

# Legacy Universal Sentence Encoder models -- OPT-IN, require TensorFlow.
# Enable with install_sentiment.ai(legacy = TRUE). Each has a better, TF-free
# replacement: en -> e5-small; en.large / multi / multi.large -> e5-base.
legacy_models <- c(en.large    = "universal-sentence-encoder-large/5",
                   en          = "universal-sentence-encoder/4",
                   multi.large = "universal-sentence-encoder-multilingual-large/3",
                   multi       = "universal-sentence-encoder-multilingual/3")

# Embedding width per model (kills the old hard-coded 512 assumption).
model_dims <- c(`e5-small` = 384L, `e5-base` = 768L,
                openai = 1536L,
                `text-embedding-3-small` = 1536L,
                `text-embedding-3-large` = 3072L,
                `text-embedding-ada-002` = 1536L,
                en.large = 512L, en = 512L, multi.large = 512L, multi = 512L)

# Text prefix a model expects. e5 uses no prefix for classification tasks.
model_prefix <- c(`e5-small` = "", `e5-base` = "")

# Default model when none is supplied.
DEFAULT_MODEL <- "e5-base"

# Default scoring head when none is supplied.
# Overridable via options(sentiment.ai.scoring = "xgb") -- picked up in .onLoad.
DEFAULT_SCORING <- "mlp"

# Helper: which backend class does a (user-facing) model name belong to?
model_class <- function(model) {
  model <- model[1]
  if (model %in% names(default_models) || model %in% default_models) return("st")
  if (model %in% names(openai_models) || model %in% openai_models)   return("openai")
  if (model %in% names(classifier_models) ||
      model %in% classifier_models)                                  return("classifier")
  if (model %in% names(legacy_models) || model %in% legacy_models)   return("legacy")
  "unknown"
}
