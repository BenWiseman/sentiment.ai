if(FALSE){
require(data.table)

# step 0 LOAD ==================================================================
default <- fread("default_xlarge.csv")
#default <- fread("default_large.csv")
default_small <- fread("default_short.csv")

# Step 1 make embedding hashes =================================================
sentiment.ai.init(model=("en"))
# Strategy: make mix of random and orthogonal hash keys


#> mean(text_embeddings)
mu_guess <- -0.0002625558
#> sd(text_embeddings)
sd_guess <- 0.0441934

plus  = rep(sd_guess, 128)
minus = rep(-sd_guess, 128)


# Half by words/names,. half random
set.seed(46290)
hash_embeddings <- matrix(rnorm(512*32, mean = mu_guess, sd = sd_guess/32), ncol = 512)

rownames(hash_embeddings) <- paste0("rand", 1:32)

# randomize a little
test_embed <- as.matrix(sentiment.ai.embed(list("Calculator")))


# simulate range
range <- numeric(10000)
for(i in 1:10000){
    test_nn    <- nn2(hash_embeddings, matrix(rnorm(512, sd=sd_guess), byrow = TRUE, nrow = 1), k = 1)
    range[i] <- test_nn$nn.idx[1,1]
}
hist(range, breaks=30)




# Step 3 - Make lookup table of reference embeddings ===========================
# in case user feeds in vector, rep positive and negative labels
positive <- unique(na.omit(default$positive))
negative <- unique(na.omit(default$negative))
pos_table <- data.table(word = positive,
                        sentiment = rep("positive", length(positive)),
                        key = "word")
neg_table <- data.table(word = negative,
                        sentiment = rep("positive", length(negative)),
                        key = "word")


# Step 4 - Generate reference embeddings
en_pos_embeddings <- as.matrix(sentiment.ai.embed(pos_table$word))
row.names(en_pos_embeddings) <- pos_table$word

en_neg_embeddings <- as.matrix(sentiment.ai.embed(neg_table$word))
row.names(en_neg_embeddings) <- neg_table$word


sentiment.ai.init(model="multi")
# Step 4 - Generate reference embeddings
multi_pos_embeddings <- as.matrix(sentiment.ai.embed(pos_table$word))
row.names(multi_pos_embeddings) <- pos_table$word

multi_neg_embeddings <- as.matrix(sentiment.ai.embed(neg_table$word))
row.names(multi_neg_embeddings) <- neg_table$word

# For users that want stock-standard, pre-embed the reference dicts
default_embeddings <- list(en = list(positive = en_pos_embeddings,
                                     negative = en_neg_embeddings),
                           multi = list(positive = multi_pos_embeddings,
                                        negative = multi_neg_embeddings))


# Step n SAVE ==================================================================
save(default_embeddings, file = "data/default_embeddings.RData")
save(default_xl, file = "data/default.RData")

}
