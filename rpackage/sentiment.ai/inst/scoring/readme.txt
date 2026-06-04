Scoring

This directory contains default embedding -> sentiment models.

Dir structure is: model_type/version

For example, if user passes scoring = 'glm' & version = 1.0 take scoring model from glm/1.0

------------------------------------------------------------------------
v2 scorers (xgboost)
------------------------------------------------------------------------

Scoring models are read from:

    system.file("scoring", "xgb", "1.0", "<model>.xgb", package = "sentiment.ai")

where <model> is the user-facing model name (see available_models.csv).

Each .xgb is an xgboost multi:softprob booster with 3 classes:
    index 0 = negative, 1 = neutral, 2 = positive.
The softprob -> [-1, 1] mapping is applied in R, not here.

available_models.csv is the manifest. Columns:
    model            user-facing model name (e.g. e5-small)
    version          scorer version (matches the xgb/<version>/ dir)
    embedding_model  HuggingFace id the embeddings (and scorer) were trained on
    dim              embedding dimension fed to the scorer
    default          TRUE for the package default model

Currently staged:
    e5-small.xgb   intfloat/multilingual-e5-small   dim 384   (default)
    e5-base.xgb    intfloat/multilingual-e5-base    dim 768

------------------------------------------------------------------------
CRAN note
------------------------------------------------------------------------

These scorers are ~20 MB each, which exceeds the 5 MB CRAN tarball limit.
They are staged here for DEV / TESTING ONLY. For the CRAN release the .xgb
files must move to download-on-first-use (fetched and cached on the user's
machine the first time a model is scored), and must NOT ship inside the
package tarball. available_models.csv stays in the package as the manifest
of which scorers are available to download.
