# planning/run_benchmarks.R
# Run once. Produces planning/benchmark_results.rds
# Requires: sentimentr, tidytext, syuzhet, sentiment.ai (loaded)

suppressMessages({
  devtools::load_all("rpackage/sentiment.ai", quiet = TRUE)  # dev version
  library(sentimentr)
  library(tidytext)
  library(syuzhet)
  library(dplyr)
  library(data.table)
})

data(airline_tweets)
set.seed(42)
texts <- sample(airline_tweets$text, 1000, replace = FALSE)
true_labels <- airline_tweets$airline_sentiment[
  match(texts, airline_tweets$text)]

cat("=== Loading sentiment.ai models ===\n")
init_sentiment.ai(model = "e5-small")

# ---- SPEED BENCHMARKS -------------------------------------------------------

time_it <- function(expr, n_reps = 3) {
  times <- numeric(n_reps)
  for(i in seq_len(n_reps)){
    gc()
    times[i] <- system.time(eval(expr))["elapsed"]
  }
  median(times)
}

cat("Benchmarking e5-small...\n")
t_e5small <- time_it(quote(sentiment_score(texts)))

cat("Benchmarking e5-base...\n")
init_sentiment.ai(model = "e5-base")
t_e5base <- time_it(quote(sentiment_score(texts, model = "e5-base")))
init_sentiment.ai(model = "e5-small")  # back to default

cat("Benchmarking sentimentr...\n")
t_sentimentr <- time_it(quote(sentimentr::sentiment_by(texts)))

cat("Benchmarking tidytext (bing)...\n")
tidy_bing_fn <- function(x){
  dt <- data.frame(text = x, id = seq_along(x), stringsAsFactors = FALSE)
  words <- tidytext::unnest_tokens(dt, word, text)
  words %>%
    dplyr::inner_join(tidytext::get_sentiments("bing"), by = "word") %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(sentiment = sum(sentiment == "positive") -
                       sum(sentiment == "negative"), .groups = "drop")
}
# pre-load the lexicon so the timer doesn't include download
invisible(tidytext::get_sentiments("bing"))
t_tidytext <- time_it(quote(tidy_bing_fn(texts)))

cat("Benchmarking syuzhet (afinn)...\n")
t_syuzhet <- time_it(quote(syuzhet::get_sentiment(texts, method = "afinn")))

# OpenAI (optional — needs OPENAI_API_KEY)
t_openai <- NA_real_
if(nzchar(Sys.getenv("OPENAI_API_KEY"))){
  cat("Benchmarking OpenAI API (text-embedding-3-small)...\n")
  tryCatch({
    init_sentiment.ai(model = "openai")
    t_openai <- time_it(quote(sentiment_score(texts, model = "openai")), n_reps = 1)
    init_sentiment.ai(model = "e5-small")
  }, error = function(e) cat("  OpenAI benchmark skipped:", conditionMessage(e), "\n"))
}

speed_results <- data.frame(
  method      = c("sentiment.ai e5-small", "sentiment.ai e5-base",
                  "OpenAI API", "sentimentr", "tidytext (bing)", "syuzhet (AFINN)"),
  pkg         = c("sentiment.ai", "sentiment.ai", "sentiment.ai",
                  "sentimentr", "tidytext", "syuzhet"),
  language    = "R",
  n_texts     = 1000,
  median_sec  = c(t_e5small, t_e5base, t_openai, t_sentimentr, t_tidytext, t_syuzhet),
  stringsAsFactors = FALSE
)
speed_results$texts_per_sec <- round(speed_results$n_texts / speed_results$median_sec, 0)
cat("\n=== Speed results ===\n")
print(speed_results[, c("method", "median_sec", "texts_per_sec")])


# ---- ACCURACY BENCHMARKS ----------------------------------------------------
# Convert predictions to 3-class labels for fair comparison

to_3class <- function(scores, pos_thr = 1/3, neg_thr = -1/3){
  ifelse(scores > pos_thr, "positive", ifelse(scores < neg_thr, "negative", "neutral"))
}

macro_f1 <- function(pred, true){
  classes <- c("negative", "neutral", "positive")
  f1s <- sapply(classes, function(cls){
    tp <- sum(pred == cls & true == cls)
    fp <- sum(pred == cls & true != cls)
    fn <- sum(pred != cls & true == cls)
    if(tp + fp == 0 || tp + fn == 0) return(NA_real_)
    p <- tp / (tp + fp); r <- tp / (tp + fn)
    if(p + r == 0) return(0); 2*p*r/(p+r)
  })
  mean(f1s, na.rm = TRUE)
}

pct_agree <- function(pred, true) mean(pred == true, na.rm = TRUE)

cat("\n=== Accuracy benchmarks ===\n")

# e5-small
scores_e5small <- sentiment_score(texts)
pred_e5small   <- to_3class(scores_e5small)
acc_e5small    <- data.frame(
  method    = "sentiment.ai e5-small",
  macro_f1  = round(macro_f1(pred_e5small, true_labels), 3),
  pct_agree = round(pct_agree(pred_e5small, true_labels), 3)
)

# e5-base
init_sentiment.ai(model = "e5-base")
scores_e5base <- sentiment_score(texts, model = "e5-base")
pred_e5base   <- to_3class(scores_e5base)
acc_e5base    <- data.frame(
  method    = "sentiment.ai e5-base",
  macro_f1  = round(macro_f1(pred_e5base, true_labels), 3),
  pct_agree = round(pct_agree(pred_e5base, true_labels), 3)
)
init_sentiment.ai(model = "e5-small")

# sentimentr (returns per-sentence average; map to 3-class)
sr <- sentimentr::sentiment_by(texts)
pred_sr <- to_3class(sr$ave_sentiment)
acc_sentimentr <- data.frame(
  method    = "sentimentr",
  macro_f1  = round(macro_f1(pred_sr, true_labels), 3),
  pct_agree = round(pct_agree(pred_sr, true_labels), 3)
)

# syuzhet AFINN
af <- syuzhet::get_sentiment(texts, method = "afinn")
af_scaled <- af / max(abs(af), na.rm = TRUE)  # rescale to [-1,1]
pred_af <- to_3class(af_scaled)
acc_afinn <- data.frame(
  method    = "syuzhet (AFINN)",
  macro_f1  = round(macro_f1(pred_af, true_labels), 3),
  pct_agree = round(pct_agree(pred_af, true_labels), 3)
)

# tidytext bing (sentence-level: count pos - count neg words)
bing_df <- tidy_bing_fn(texts)
all_ids <- data.frame(id = seq_along(texts))
bing_full <- dplyr::left_join(all_ids, bing_df, by = "id")
bing_scores <- ifelse(is.na(bing_full$sentiment), 0, sign(bing_full$sentiment))
pred_bing <- to_3class(bing_scores * 0.5)  # force into [-1,1]-like range
acc_tidytext <- data.frame(
  method    = "tidytext (bing)",
  macro_f1  = round(macro_f1(pred_bing, true_labels), 3),
  pct_agree = round(pct_agree(pred_bing, true_labels), 3)
)

acc_results <- rbind(acc_e5small, acc_e5base, acc_sentimentr, acc_afinn, acc_tidytext)
cat("\n=== Accuracy results ===\n")
print(acc_results)

# Save all results
results <- list(
  speed    = speed_results,
  accuracy = acc_results,
  run_date = Sys.time(),
  platform = Sys.info()[["sysname"]],
  r_version = R.version$version.string
)
saveRDS(results, "planning/benchmark_results.rds")
cat("\nSaved to planning/benchmark_results.rds\n")
