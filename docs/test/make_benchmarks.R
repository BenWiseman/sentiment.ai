# TODO GPU hogging occurs!
# https://stackoverflow.com/questions/34199233/how-to-prevent-tensorflow-from-allocating-the-totality-of-a-gpu-memory
Sys.setenv("CUDA_VISIBLE_DEVICES" = -1)
Sys.setenv("TF_FORCE_GPU_ALLOW_GROWTH" = TRUE)
gpus = tf$config$experimental$list_physical_devices('GPU')
tf$config$experimental$set_memory_growth(gpus[[1]], TRUE)


require(sentiment.ai)
require(data.table) # Fast data manipulation
require(keras)      # Interface to Tensorflow
require(caret)      # ML suite
require(magrittr)   # Function Pipes
require(tm)         # Text mining functions
require(pbapply)    # Parallel processing
require(parallel)   # Parallel back-end
require(SentimentAnalysis)


sentiment.ai.init(model="en")
dt <- fread("../../docs/test/siop_2020_txt_long.csv")


dt <- dt[text != "" & type %in% c("pro", "con"), ]
# remove some formatting junk
dt[, text  := gsub("^.*</b><br/>", "" ,text)]  # Remove pros/cons html tag
dt[, text  := gsub("<.*?>", "", text)] # remove HTML junt
dt[, text  := gsub("brbr$", "" ,text)]
dt[, text  := gsub("[\\\r\\\n]", " " , text)]

# Going to leave stopwords in - they shouldn't hurt an LSTM!
#dt[, text2 := tm::removePunctuation(text)]
# dt[, text2 := tm::removeWords(text2, tm::stopwords("en"))]
dt[, text := tolower(text)]
dt[, nword := vapply(gregexpr("[[:alpha:]]+", dt$text),
                     function(x) sum(x > 0), numeric(1))]

# reduct to where at least 3 words are said
dt <- data.table::copy(dt[nword > 3, ])

# Create binary column - 1 means text is pro, 0 is con
dt[, target := ifelse(type == "pro", 1, 0)]

set.seed(3364900) # fixes random number generation - for reproducability (within R, not CUDA!)

# Partition training and test data balanced by type (pro/con) &  use 70% in training
train_idx <- caret::createDataPartition(dt$type, p = .7, list = FALSE)


# Now split by balanced test/train indexes
text <- dt[-train_idx,text] %>% as.matrix() %>% unname()
y    <- dt[-train_idx, target] %>% as.matrix() %>% unname()

ai_sentiment <- sentiment_easy(text[,1])
ai_sentiment$y <- y


saveRDS(ai_sentiment, "../../docs/test/sentiment_ai_scores.rds")

ai_sentiment <- readRDS("../../docs/test/sentiment_ai_scores.rds")
require(caret)
ai_sentiment$y <- as.factor(ai_sentiment$y)
ai_sentiment$yhat <- factor(ai_sentiment$sentiment,
                            levels = c("negative", "positive"),
                            labels = c('0', '1'))
confusionMatrix(ai_sentiment$yhat,
                ai_sentiment$y)
