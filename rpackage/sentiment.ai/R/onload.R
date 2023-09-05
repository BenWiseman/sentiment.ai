.onLoad <- function(libname, pkgname) {

  if (roperators::is.os_arm() && roperators::is.os_mac()) {
    warning("Tensorflow isn't fully compatible with Apple Silicone, specifically tensorflow-text.
            The following configuration worked on my m2 macbook pro with python 3.10.11 installed and set to default interpreter in rstudio/reticulate.

            # install latest versions into r-reticulate
            # run
            reticulate::py_discover_config()
            # if unsure what python version is available
            # remember to restart the R session afterwards if you don't want to use the r-reticulate enviromnment

            # tensorflow-text will automatically be skipped
            install_sentiment.ai(envname = 'r-sentimentai-dev',
                                 method  =  'virtualenv',
                                 gpu     =  FALSE,
                                 python_version = '3.10.11',
                                 modules = list(numpy             = 'latest',
                                                sentencepiece     = 'latest',
                                                tensorflow        = 'latest',
                                                tensorflow_hub    = 'latest',
                                                `tensorflow-text` = 'latest',
                                                openai            = 'latest'),
                                 fresh_install   = TRUE,
                                 restart_session = TRUE)

            ")
  }



}
