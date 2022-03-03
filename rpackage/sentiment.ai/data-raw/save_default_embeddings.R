       #####################
# Save Default Data #
#                   #
# Ben Wiseman       #
# 2021-04-27        #
#####################

# required package
require(sentiment.ai)
require(data.table)
require(magrittr)
require(RANN)

# make sure package is loaded to get at function
devtools::load_all()

# make sure we're in the right directory
cur_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Step 0 LOAD ==================================================================
default_xl    <- fread(file.path(cur_dir, "default_xlarge.csv"))


# Step 1 Make Embedding Hashes =================================================

# setup your environment if you haven't

# install_sentiment.ai()

# Step 2 Make lookup table of reference embeddings =============================

# Can't have NA or repeated values!
positive  <- unique(na.omit(default$positive))
negative  <- unique(na.omit(default$negative))


# Step 3 Generate reference embeddings =========================================

# english positive and negative
init_sentiment.ai(model = "en.large")

en_large_pos_embeddings <- as.matrix(sentiment.env$embed(positive))
rownames(en_large_pos_embeddings) <- positive
en_large_neg_embeddings <- as.matrix(sentiment.env$embed(negative))
rownames(en_large_neg_embeddings) <- negative

# english positive and negative
init_sentiment.ai(model = "en")

en_pos_embeddings <- as.matrix(sentiment.env$embed(positive))
rownames(en_pos_embeddings) <- positive
en_neg_embeddings <- as.matrix(sentiment.env$embed(negative))
rownames(en_neg_embeddings) <- negative


# Step 4 Repeat with Multi Embeddings ==========================================

# assumes previous things work
init_sentiment.ai(model = "multi.large")

multi_large_pos_embeddings <- as.matrix(sentiment.env$embed(positive))
rownames(multi_large_pos_embeddings) <- positive
multi_large_neg_embeddings <- as.matrix(sentiment.env$embed(negative))
rownames(multi_large_neg_embeddings) <- negative


init_sentiment.ai(model = "multi")

multi_pos_embeddings <- as.matrix(sentiment.env$embed(positive))
rownames(multi_pos_embeddings) <- positive
multi_neg_embeddings <- as.matrix(sentiment.env$embed(negative))
rownames(multi_neg_embeddings) <- negative
# Step 5 Combine Everything ====================================================


# For users that want stock-standard, pre-embed the reference dicts
en.large    = list(positive = en_large_pos_embeddings,
                   negative = en_large_neg_embeddings)

multi.large = list(positive = multi_large_pos_embeddings,
                   negative = multi_large_neg_embeddings)

en    = list(positive = en_pos_embeddings,
             negative = en_neg_embeddings)

multi = list(positive = multi_pos_embeddings,
             negative = multi_neg_embeddings)


# Step N SAVE ==================================================================

dev_info <- devtools::package_info() |> data.table()
pkg_info <- dev_info[package == "sentiment.ai" & source == "load_all()", ]


# output json will not preserve rownames, so will need to re-create!
# colnames shouldn't ever matter of differ between pos/neg or embeddings
out <- list(rownames    = list(positive = default$positive, negative = default$negative),
            colnames    = list(positive = colnames(en$positive), negative = colnames(en$negative)),
            en.large    = en.large,
            multi.large = multi.large,
            en          = en,
            multi       = multi,
            version     = pkg_info$loadedversion)


file_name = paste0(pkg_info$loadedversion, ".json")
file = file.path("../../default_embeddings", file_name)


require(jsonlite)

out |> toJSON(pretty = TRUE) |> writeLines(file)


# read_embedding <- function(file, model = "en.large", version = NULL){
#   # take json path, return single embedding object
#
#   x <- fromJSON(readLines(file))
#
#   emb <- x[[model]]
#   rownames(emb$positive) <- x$rownames$positive
#   colnames(emb$positive) <- x$colnames$positive
#
#   rownames(emb$negative) <- x$rownames$negative
#   colnames(emb$negative) <- x$colnames$negative
#
#   return(emb)
# }

#test = read_embedding(file)
