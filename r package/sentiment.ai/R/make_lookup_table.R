# TODO: make functional
make_lookup_table <- function(lexicon){

    # Could take input like
    # lexicon::hash_sentiment_senticnet
    # OR
    # tidytext::sentiments


    dt <- fread("default_short.csv")
    reference_table <- rbind(data.table(word = dt$positive, sentiment = "positive"),
                             data.table(word = dt$negative, sentiment = "negative"))

}

