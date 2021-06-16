# TODO GPU hogging occurs!
# https://stackoverflow.com/questions/34199233/how-to-prevent-tensorflow-from-allocating-the-totality-of-a-gpu-memory
#Sys.setenv("CUDA_VISIBLE_DEVICES" = -1)
Sys.setenv("TF_FORCE_GPU_ALLOW_GROWTH" = TRUE)
#gpus = tf$config$experimental$list_physical_devices('GPU')
#tf$config$experimental$set_memory_growth(gpus[[1]], TRUE)

require(microbenchmark)
require(sentiment.ai)
sentiment.ai.init(model="en.large")

require(data.table) # Fast data manipulation
require(keras)      # Interface to Tensorflow
require(caret)      # ML suite
require(magrittr)   # Function Pipes
require(tm)         # Text mining functions
require(pbapply)    # Parallel processing
require(parallel)   # Parallel back-end
require(SentimentAnalysis)
require(sentimentr)


# GLASSDOOR BANCHMARKS =========================================================

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
sentimentr_sentiment$y <- our_scores$y
saveRDS(sentimentr_sentiment, "../../docs/test/sentimentr_scores.rds")

dict_sentiment <- analyzeSentiment(text_vec)
dict_sentiment$y <- our_scores$y
saveRDS(dict_sentiment, "../../docs/test/dict_scores.rds")


#  AIRLINES ====================================================================

tweets <- fread("../../docs/test/Airline_Tweets.csv",
                select = c("text", "airline_sentiment"))
tweets <- tweets[airline_sentiment %in% c("positive", "negative")]
tweets[, sentiment := factor(airline_sentiment, levels = c("negative", "positive"), labels = c("0","1"))]

ai_sentiment <- sentiment_easy(tweets$text, batch_size = 500)

our_scores <- data.frame(sentiment = ai_sentiment,
                         y         = tweets$sentiment)
our_scores <- na.omit(our_scores)
our_scores$y_hat <- as.factor(ifelse(our_scores$sentiment > 0, "1", "0"))

caret::confusionMatrix(data = our_scores$y_hat, reference = our_scores$y, positive = "1")

saveRDS(our_scores, "../../docs/test/sentimentai_tweet_scores.rds")

sentimentr_sentiment <- sentiment_by(get_sentences(tweets$text), 1:nrow(tweets))
sentimentr_sentiment$y <- tweets$sentiment
saveRDS(sentimentr_sentiment, "../../docs/test/sentimentr_tweet_scores.rds")

dict_sentiment <- analyzeSentiment(tweets$text)
dict_sentiment$y <- tweets$sentiment
saveRDS(dict_sentiment, "../../docs/test/dict_tweet_scores.rds")


# AZURE for airlines

# REMEMBER THAT THIS CAN COST MONEY! CURRENTLY HAVE 200 IN FREE AZURE CREDITS FOR A TRIAL
# AFTER 30 DAYS, THIS WILL COST ~20 DOLLARS TO RUN.

require(httr)
require(jsonlite)

azure_file_name <-  "../../docs/test/azure_tweet_scores.rds"

# Stored in .Rprofile -- you'll need one of your own.
api_key      <- "89e64c63dac54ef59958faca2f3813b5"
api_endpoint <- "https://throwawaytrial.cognitiveservices.azure.com/"
api_url_lan  <- "https://westus.api.cognitive.microsoft.com/text/analytics/v3.0/languages"
api_url_sen  <- "https://westus.api.cognitive.microsoft.com/text/analytics/v2.0/sentiment"
# text to pass into API - training data(for apples to apples validation)
text <- tweets
text$language <- "en"
text$id <- as.character(1:nrow(text))
setkey(text, id)


if(file.exists(azure_file_name)){
  azure_scores <- readRDS(azure_file_name)
} else{
  # As simple df
  text_data <- list(documents = text[, .(id, language, text)])
  object.size(jsonlite::toJSON(text_data)) %>% format("MB")
  # 2MB - may payload is 1mb -- REDUCE

  # Need to make multiple small requests
  # max 1000 records...
  text_data_list <- list()
  idx_max        <- length(text$text)
  set_max        <- 999
  sets_needed    <- ceiling(idx_max/set_max)

  # make as many sub dataframes as needed (13)
  idx_current    <- 1
  for(i in 1:sets_needed){
    this_limit <- min((i*set_max), idx_max)
    text_data_list[[i]] <- list(documents = text[idx_current:this_limit, .(id, language, text)])
    idx_current <- this_limit+1
  }

  azure_function <- function(x){
    out  <- httr::POST(url    = api_url_sen,
                       config = httr::add_headers(`Ocp-Apim-Subscription-Key`=api_key),
                       body   = jsonlite::toJSON(
                         auto_unbox = TRUE,
                         dataframe = "rows",
                         x)
                       ) %>%
      httr::content( as="text")%>%
      jsonlite::fromJSON() %>%
      extract2("documents") %>%
      data.table() %>%
      .[, id := as.integer(id)]
    return(out)
  }

  # Be 100% sure you want to run this -- single line of code could cost $$$
  if(FALSE){
    responses    <- lapply(text_data_list, azure_function)
  }

  azure_scores <- rbindlist(responses)
  azure_scores[, id := as.character(id)]
  azure_scores <- azure_scores[text, on="id", nomatch=0L]
  setnames(azure_scores, "sentiment", "y")
  azure_scores[, yhat := factor(round(scales::rescale(score, to=0:1)), levels = c("0","1"))]
  #Save to avoid making repeat API calls ($$)
  saveRDS(azure_scores, azure_file_name)

}
