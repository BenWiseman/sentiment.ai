Scoring

This directory contains default embedding -> sentiment models.

Dir structure is: model_type/version

For example, if user passes scoring = 'glm' & version = 1.0 take scoring model from glm/1.0

NOTE FOR COMMUNITY MODELS -----

If you'd like to contribute a new/better/contexxt specific model, use the same folder structure
If it's not a glm or XGB, let us know so we can make it work in find_sentiment_probs()!

Currently we support xgboost and glm, and will add support for others as needed.

We only ask that numeric version (e.g xgb/2.0/...) names be left for 'official'/default models.
For community models, the version can be a descriptor (so long as it's a valid file name and URL!)
e.g: xgb/jimbojones_imdb1/en.large.xgb
This way we can keep it easy for less engaged people who want a package that "just works"
and we can accomodate power users that want custom models

* will add support to pass through custom github urls if you don't feel like sharing

NOTE FOR GLM MODELS -----

saving a GLM object from R uses a LOT of space. Hence we save just the paramaters in a csv
simply put, just pull the coefficients, and write.csv

should look like this:
"","x"
"(Intercept)",0.922440256482062
"V1",-6.42591883182618
"V2",-3.6621793890871

the column names don't matter, only the position.
e.g> write.csv(model$coefficients, "models/glm/2.0")

-----
