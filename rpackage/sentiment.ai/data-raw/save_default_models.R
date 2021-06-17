# STEP 1: ASSEMBLE DATA  -------------------------------------------------------


# make sure package is loaded to get at function
devtools::load_all()


require(data.table) # Fast data manipulation
require(keras)      # Interface to Tensorflow
require(caret)      # ML suite
require(magrittr)   # Function Pipes
require(tm)         # Text mining functions
require(pbapply)    # Parallel processing
require(parallel)   # Parallel back-end
require(SentimentAnalysis)
require(sentimentr)
`%+%` <- roperators::`%+%`
# make orthogonal (double negatives) to test
nouns <- htmltab::htmltab("https://www.talkenglish.com/vocabulary/top-1500-nouns.aspx", 4,
                          header = 0)


# 0) Glassdoor test data -------------------------------------------------------


dt <- fread("../../docs/test/siop_2020_txt_long.csv")


dt <- dt[text != "" & type %in% c("pro", "con"), ]
# remove some formatting junk
dt[, text  := gsub("^.*</b><br/>", "" ,text)]  # Remove pros/cons html tag
dt[, text  := gsub("<.*?>", "", text)] # remove HTML junt
dt[, text  := gsub("brbr$", "" ,text)]
dt[, text  := gsub("[\\\r\\\n]", " " , text)]
dt[, text := tolower(text)]
dt[, nword := vapply(gregexpr("[[:alpha:]]+", dt$text),
                     function(x) sum(x > 0), numeric(1))]
# remove totally useless
dt <- dt[nword > 3, ]

# Create binary column - 1 means text is pro, 0 is con
dt[, target := ifelse(type == "pro", 1, 0)]

# Partition training and test data balanced by type (pro/con) &  use 70% in training
set.seed(3364900) # fixes random number generation - for reproducability (within R, not CUDA!)
train_idx <- caret::createDataPartition(dt$type, p = .7, list = FALSE)

dt_test <- dt[-train_idx,]
dt_train <- dt[train_idx,]

if(FALSE){

  # Now split by balanced test/train indexes
  text <- dt[-train_idx,text] %>% as.matrix() %>% unname()
  y    <- dt[-train_idx, target] %>% as.matrix() %>% unname()

  train_text <- dt[train_idx,text] %>% as.matrix() %>% unname()
  train_y    <- dt[train_idx, target] %>% as.matrix() %>% unname()

  text_vec <- text[,1]
  glass_mx <- matrix(0, ncol=512, nrow=nrow(text))
  rownames(glass_mx) <- paste("row_", 1:length(text))

  dt_test[, idx := 1:.N]
  dt_test[, one := 1]
  dt_test[, batch  := ceiling(cumsum(one)/50)]
  batches <- unique(dt_test$batch)

  # For testing!
  # could be done in batches. Gods forgive me
  pb = txtProgressBar(0, length(batches), style = 3, char = "|")
  for(b in batches){
    batch_dt <- dt_test[batch == b,]
    glass_mx[min(batch_dt$idx):max(batch_dt$idx),] <- as.matrix(sentiment.ai.embed(batch_dt$text))
    setTxtProgressBar(pb, b)
  }

  saveRDS(glass_mx, "../../scratch/glassdoor_validation_embedding.rds")

}



dt <- fread("../../docs/test/siop_2020_txt_long.csv")


dt <- dt[text != "" & type %in% c("pro", "con"), ]
# remove some formatting junk
dt[, text  := gsub("^.*</b><br/>", "" ,text)]  # Remove pros/cons html tag
dt[, text  := gsub("<.*?>", "", text)] # remove HTML junt
dt[, text  := gsub("brbr$", "" ,text)]
dt[, text  := gsub("[\\\r\\\n]", " " , text)]
dt[, text := tolower(text)]
dt[, nword := vapply(gregexpr("[[:alpha:]]+", dt$text),
                     function(x) sum(x > 0), numeric(1))]
# remove totally useless
dt <- dt[nword > 3, ]

# Create binary column - 1 means text is pro, 0 is con
dt[, target := ifelse(type == "pro", 1, 0)]

# Partition training and test data balanced by type (pro/con) &  use 70% in training
set.seed(3364900) # fixes random number generation - for reproducability (within R, not CUDA!)
train_idx <- caret::createDataPartition(dt$type, p = .7, list = FALSE)

dt_test <- dt[-train_idx,]
dt_train <- dt[train_idx,]

# MISC data --------------------------------------------------------------------

# employee reviews
# (leave validation for benchmarking -- this is just a LM!)
reviews1 <- fread("../../scratch/employee_review_archive/train_set.csv")
reviews2 <- fread("../../scratch/employee_review_archive/test_set.csv")
employee_dt <- rbind(reviews1[label %in% c(0,7,8),
                              .(word = feedback_clean, sentiment = int(label > 2))],
                     reviews2[label %in% c(0,7,8),
                              .(word = feedback_clean, sentiment = int(label > 2))]
)
# oversample this small but useful data
#employee_dt <- rbind(employee_dt)


imdb  <- fread("../../scratch/IMDB_Test.csv/Test.csv", col.names = c("word", "sentiment"))

# this is big, just the test alone is huge!
amazon <- fread("../../scratch/amazon-reviews/amazon-reviews.csv")
amazon <- amazon[date > as.Date("2013-01-01") & rating %in% c(1,5)]
# imbalanced!
amazon_neg <- amazon[rating==1 & nchar(review) %><% c(30, 300),
                     j = .(word = review, sentiment = int(rating==5))]
amazon_pos <- amazon[rating==5 & nchar(review) %><% c(30, 300) & date > as.Date("2016-06-13"),
                     j = .(word = review, sentiment = int(rating==5))]
amazon <- rbind(amazon_neg, amazon_pos)


# sentiment140 (tweets)
tweets <- fread("../../scratch/training.1600000.processed.noemoticon.csv/training.1600000.processed.noemoticon.csv")
tweets <- tweets[!duplicated(V3) & V1 %in% c(0,4), .(word = V6, sentiment = int(V1==4))][!grepl("@|http:", word),]
tweets <- tweets[nchar(word) > 130]
# data looks unreliabl! filter for spelling ands stuff
tw_sent <- sentimentr::sentiment_by(get_sentences(tweets$word), 1:nrow(tweets))
tweets <- tweets[, r_sentiment := tw_sent$ave_sentiment]
# where both labels agree (try to remove obviously mislabeled)
tweets <- tweets[(r_sentiment <0 & sentiment == 0) | (r_sentiment>0 & sentiment==1),]
tweets[, z_sentiment := scale(r_sentiment)]
tweets <- tweets[abs(z_sentiment)>1]
tweets[, idx := 1:.N]
# fix imbalance
tweets_neg <- tweets[sentiment == 0]
tweets_pos <- tweets[sample(tweets[sentiment==1, idx], nrow(tweets_neg)),]
tweets <- rbind(tweets_neg, tweets_pos)[, .(word, sentiment)]

# Financial news

financial <- fread("../../scratch/fin_news_archive/FinancialPhraseBank/Sentences_AllAgree.txt", sep="@", header = FALSE)
financial <- financial[V2 %in% c("positive", "negative")][, .(word = V1, sentiment = int(V2 =="positive"))]



# 1) MAKE PRO/CON DATASET ------------------------------------------------------

# will load other datasets later
profanity <- c(lexicon::profanity_banned, lexicon::profanity_zac_anger)
racism    <- lexicon::profanity_racist
not_racist_like_that <- c("banana", "bounty bar", "coconut","coconuts",
                          "beaney", "brownies", "eight ball", "fruitcake",
                          "oreo", "oreos", "que", "sooty", "spade")

racism_profanity    <- unique(c(profanity, racism))
racism_profanity    <- racism_profanity[racism_profanity %ni% not_racist_like_that]
hash_huliu          <- lexicon::hash_sentiment_huliu
hash_jockers_rinker <- lexicon::hash_sentiment_jockers_rinker
hash_sentiword      <- lexicon::hash_sentiment_sentiword[abs(y) >= 0.75]
lexicons            <- rbind(hash_huliu, hash_jockers_rinker, hash_sentiword)
lexicons            <- lexicons[!duplicated(x)]


pkg_positives       <- default$positive
pkg_negatives       <- default$negative

positives <- c(lexicons[y>0, x],  pkg_positives)
negatives <- c(lexicons[y<0, x],  pkg_negatives, racism_profanity)

positives <- na.omit(positives)
negatives <- na.omit(negatives)

sentiment_dt <- data.table(word = c(positives, negatives),
                           sentiment = c(rep(1, length(positives)), rep(0, length(negatives))))
#sentiment_dt <- sentiment_dt[!duplicated(word),]

# append dt_train!
set.seed(46920)
train_sub_idx <- caret::createDataPartition(dt_train$target[dt_train$nword > 15], p=0.6, list = FALSE)
sample_glass  <- data.table(word      = dt_train$text[dt_train$nword > 15][train_sub_idx],
                            sentiment = dt_train$target[dt_train$nword > 15][train_sub_idx])


# bind
sentiment_dt <- rbind(sentiment_dt, sample_glass, employee_dt, imdb, amazon, tweets, financial)

saveRDS(sentiment_dt, "../../scratch/sentiment_train.rds")

# make_row_ids
embed_file <- "../../scratch/training_embeddings.rds"

models <- c("en", "en.large", "multi", "multi.large")

if(!file.exists(embed_file)){

  sentiment_dt[, rowid := 1:.N]

  # Need to do one per default model

  sentiment_dt[, idx := 1:.N]
  sentiment_dt[, one := 1]
  sentiment_dt[, batch  := ceiling(cumsum(one)/15)]
  batches <- unique(sentiment_dt$batch)

  mx_list <- list()

  for(m in models){

    sentiment.ai.init(model=m)

    result_mx <- matrix(0, ncol=512, nrow=nrow(sentiment_dt))
    rownames(result_mx) <- sentiment_dt$word


    # could be done in batches. Gods forgive me
    pb = txtProgressBar(0, length(batches), style = 3, char = "|")

    for(b in batches){
      batch_dt <- sentiment_dt[batch == b,]
      result_mx[min(batch_dt$idx):max(batch_dt$idx),] <- as.matrix(sentiment.ai.embed(batch_dt$word))
      setTxtProgressBar(pb, b)
    }

    mx_list[[m]] <- result_mx
  }

  saveRDS(mx_list[[m]], paste0("../../scratch/training_embeddings", "_", m, ".rds"))
} else{
  mx_list <- readRDS(embed_file)
}
saveRDS(mx_list, embed_file)
# TEST -------------------------------------------------------------------------


# Now make a model from each embedding file. Sentiment_dt has the target walues

sentiment_dt <- readRDS("../../scratch/sentiment_train.rds")

# TODO make validation for each model
#glass_mx     <- readRDS("../../scratch/glassdoor_validation_embedding.rds")

glm_accuracy <- list()
for(m in models){
  sentiment_mx <- mx_list[[m]]

  mod_df <- data.frame(sentiment = sentiment_dt$sentiment)
  mod_df <- cbind(mod_df, as.data.frame(sentiment_mx))
  mod    <- glm(sentiment ~ ., data=mod_df, family="binomial")
  pred   <- predict(mod, type="response")
  acc    <- sum(round(pred) == sentiment_dt$sentiment)/length(pred)

  glm_accuracy[[m]] <- acc
  message("Model: ", m, "Accuracy: ", acc)

  saveRDS(mod$coefficients, "inst/scoring/glm/1.0/" %+% m %+% ".rds")

}


json_acc <- jsonlite::toJSON(glm_accuracy, pretty = TRUE, auto_unbox = TRUE, digits = 2)
writeLines(json_acc,  "inst/scoring/glm/1.0/metrics.json")
# Try lm!


# 91%! without glassdoor
# 92 with a little glassdoor
# 90 with IMDB (is okay, should be more robust now!)
# 91 with amazon and sentiword lexicon
# 91 with tweets
# 91 with oversampling hand-made examples (double negatives, etc)


# XGB --------------------------------------------------------------------------

# Try xgboost?
require(xgboost)

xgb_accuracy <- list()
for(m in models){
  sentiment_mx <- mx_list[[m]]

  mod_df <- data.frame(sentiment = sentiment_dt$sentiment)
  mod_df <- cbind(mod_df, as.data.frame(sentiment_mx))

  train_idx <- createDataPartition(mod_df$sentiment, p=0.9, list=FALSE)

  dtrain <- xgb.DMatrix(data = sentiment_mx[train_idx,],  label=mod_df$sentiment[train_idx])
  dtest  <- xgb.DMatrix(data = sentiment_mx[-train_idx,], label=mod_df$sentiment[-train_idx])
  watchlist <- list(train=dtrain, test=dtest)
  #early_stopping_rounds

  # not much better than lm!
  xgb <- xgb.train(data = dtrain,
                   objective = "binary:logistic",
                   watchlist = watchlist,
                   early_stopping_rounds = 5,
                   num_parallel_tree = 24,
                   colsample_bytree  = .25,
                   subsample         = .25,
                   round             = 1,
                   max.depth = 6, eta = 0.15, lambda = 0.03,
                   nthread = 20, nrounds = 512)

  pred <- predict(xgb, dtest)
  acc  <- sum(round(pred) == mod_df$sentiment[-train_idx])/length(pred)
  # save some space - don't need to lug around data
  xgb$raw <- NULL


  xgb_accuracy[[m]] <- acc
  message("Model: ", m, "Accuracy: ", acc)

  xgb.save(xgb,  "inst/scoring/xgb/1.0/" %+% m %+% ".xgb")

}


json_acc <- jsonlite::toJSON(xgb_accuracy, pretty = TRUE, auto_unbox = TRUE, digits = 2)
writeLines(json_acc,  "inst/scoring/xgb/1.0/metrics.json")
