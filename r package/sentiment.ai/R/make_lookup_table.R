# TODO: make functional
make_lookup_table <- function(lexicon){

    # Could take input like
    # lexicon::hash_sentiment_senticnet
    # OR
    # tidytext::sentiments

    pos_words <- c("Good", "Bob Ross")
    neg_words <- c("Evil", "Ben Shapiro")
    sentiment <- c(rep("positive", length(pos_words)),
                   rep("negative", length(neg_words)))

    data.table(word = c(pos_words, neg_words),
               sentiment = sentiment)
}
