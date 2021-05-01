# TODO GPU hogging occurs!
# https://stackoverflow.com/questions/34199233/how-to-prevent-tensorflow-from-allocating-the-totality-of-a-gpu-memory
Sys.setenv("CUDA_VISIBLE_DEVICES" = -1)
Sys.setenv("TF_FORCE_GPU_ALLOW_GROWTH" = TRUE)
#gpus = tf$config$experimental$list_physical_devices('GPU')
#tf$config$experimental$set_memory_growth(gpus[[1]], TRUE)

require(microbenchmark)
require(sentiment.ai)
sentiment.ai.init(model="en")

require(data.table) # Fast data manipulation
require(keras)      # Interface to Tensorflow
require(caret)      # ML suite
require(magrittr)   # Function Pipes
require(tm)         # Text mining functions
require(pbapply)    # Parallel processing
require(parallel)   # Parallel back-end
require(SentimentAnalysis)
require(sentimentr)



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

text_vec <- text[,1]
ai_sentiment <- sentiment_easy(text_vec, batch_size = 300)

our_scores <- data.frame(sentiment = ai_sentiment,
                         y         = as.factor(y))
our_scores <- na.omit(our_scores)
our_scores$y_hat <- as.factor(ifelse(our_scores$sentiment > 0, "1", "0"))

summary(our_scores$y)
summary(our_scores$y_hat)

our_scores       <- droplevels(na.omit(our_scores))

our_scores$y %<>% factor(levels = c("0", "1"))
our_scores$y_hat %<>% factor(levels = c("0", "1"))


confusionMatrix(data = our_scores$y_hat, reference = our_scores$y, positive = "1")


saveRDS(our_scores, "../../docs/test/sentiment_ai_scores.rds")

our_scores <- readRDS("../../docs/test/sentiment_ai_scores.rds")



sentimentr_sentiment <- sentiment_by(get_sentences(text_vec), 1:length(text))
sentimentr_sentiment$y <- text_vec
saveRDS(sentimentr_sentiment, "../../docs/test/sentimentr_scores.rds")

dict_sentiment <- analyzeSentiment(text_vec)
dict_sentiment$y <- text_vec
saveRDS(dict_sentiment, "../../docs/test/dict_scores.rds")
