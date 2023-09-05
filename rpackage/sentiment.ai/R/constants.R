# 1. EMBEDDINGS/MODELS =========================================================
default_models     <- c(en.large    = "universal-sentence-encoder-large/5",
                        en          = "universal-sentence-encoder/4",
                        multi.large = "universal-sentence-encoder-multilingual-large/3",
                        multi       = "universal-sentence-encoder-multilingual/3")

openai_models <- c(
  `text-embedding-ada-002` = "text-embedding-ada-002",
  `*-davinci-*-001`        = "*-davinci-*-001",
  `*-curie-*-001`          = "*-curie-*-001",
  `*-babbage-*-001`        = "*-babbage-*-001",
  `*-ada-*-001`            = "*-ada-*-001"
)
