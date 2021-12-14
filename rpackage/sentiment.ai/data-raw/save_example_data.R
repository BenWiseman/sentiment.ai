#####################
# Save Example Data #
#                   #
# Steven Nydick     #
# 2021-06-23        #
#####################

require(data.table)

# make sure we're in the right directory
cur_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Step 0 LOAD ==================================================================

airline_tweets <- data.table::fread(file.path(cur_dir, "airline_tweets.csv"))
airline_tweets <- airline_tweets[ , .(tweet_id, airline_sentiment, airline, text)]

# updating text to convert to UTF-8?
Encoding(airline_tweets$text) <- "latin1"
airline_tweets[ , text := iconv(text, "latin1", "UTF-8")]

# Step N SAVE ==================================================================

usethis::use_data(airline_tweets,
                  overwrite = TRUE)
