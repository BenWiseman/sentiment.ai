.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "sentiment.ai: default model is 'e5-small' (on-device, multilingual, no TensorFlow).\n",
    "  Run install_sentiment.ai() to set up; the model + scorer download on first use.\n",
    "  Legacy TensorFlow USE models (en / en.large / multi / multi.large) require\n",
    "  install_sentiment.ai(legacy = TRUE)."
  )
}
