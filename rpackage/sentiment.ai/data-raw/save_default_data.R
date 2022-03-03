#####################
# Save Default Data #
#                   #
# Ben Wiseman       #
# 2021-04-27        #
#####################

# required packages
require(data.table)
require(magrittr)
require(RANN)

# make sure package is loaded to get at function
devtools::load_all()

# make sure we're in the right directory
cur_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Step 0 LOAD ==================================================================
# default       <- fread(file.path(cur_dir, "default_xlarge.csv"))
# default_small <- fread(file.path(cur_dir, "default_short.csv"))

# Step 1 Make Embedding Hashes =================================================

# setup your environment if you haven't

# install_sentiment.ai()
sentiment.ai.init(model = "en.large")

# add sentiment.ai_embed to global environment
sentiment.ai_embed <- sentiment.ai::sentiment.env$embed

# strategy: make mix of random and orthogonal hash keys

#> mean(text_embeddings)
# mu_guess <- -0.0002625558
#
# #> sd(text_embeddings)
# sd_guess <- 0.0441934
#
# plus     <- rep(sd_guess, 128)
# minus    <- -plus


# half by words/names,. half random
# set.seed(46290)
# hash_embeddings <- matrix(data = rnorm(512 * 32, mean = mu_guess, sd = sd_guess/32),
#                           ncol = 512) %>%
#                    set_rownames(paste0("rand", seq_len(nrow(.))))
#
# # randomize a little
# test_embed      <- as.matrix(sentiment.ai_embed(list("Calculator")))
#
# # simulate range
# range <- numeric(10000)
# for(i in seq_along(range)){
#   data_nn  <- matrix(data  = rnorm(512, sd = sd_guess),
#                      nrow  = 1,
#                      byrow = TRUE)
#   test_nn  <- nn2(data  = hash_embeddings,
#                   query = data_nn,
#                   k     = 1)
#   range[i] <- test_nn$nn.idx[1,1]
# }
#
# # make sure range looks OK
# hist(range, breaks=30)


# Step 2 Make lookup table of reference embeddings =============================

# Can't have NA or repeated values!
positive  <- unique(na.omit(default$positive))
negative  <- unique(na.omit(default$negative))


# Step 3 Generate reference embeddings =========================================

# english positive and negative
# en_pos_embeddings <- as.matrix(sentiment.ai_embed(positive))
# rownames(en_pos_embeddings) <- positive
# en_neg_embeddings <- as.matrix(sentiment.ai_embed(negative))
# rownames(en_neg_embeddings) <- negative

# Step 4 Repeat with Multi Embeddings ==========================================

# assumes previous things work
# sentiment.ai.init(model = "multi.large")
#
# multi_pos_embeddings <- as.matrix(sentiment.ai_embed(positive))
# rownames(multi_pos_embeddings) <- positive
# multi_neg_embeddings <- as.matrix(sentiment.ai_embed(negative))
# rownames(multi_neg_embeddings) <- negative

# Step 5 Combine Everything ====================================================

# For users that want stock-standard, pre-embed the reference dicts
# default_embeddings <- list(en.large    = list(positive = en_pos_embeddings,
#                                               negative = en_neg_embeddings),
#                            multi.large = list(positive = multi_pos_embeddings,
#                                               negative = multi_neg_embeddings))

# Need the unique na omitted vectors!!
default <- list(positive = positive,
                negative = negative)

# Step N SAVE ==================================================================

usethis::use_data(default, overwrite = TRUE)
