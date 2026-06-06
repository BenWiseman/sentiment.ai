# data-raw/make_test_fixtures.R
# Run ONCE from the repo root when you need to regenerate the test fixtures.
# Requires the (separate) sentiment.ai_training repository on the same machine.
#
# Commits:
#   tests/testthat/fixtures/corpus.rds            60 real-text rows (30 pos, 30 neg)
#   tests/testthat/fixtures/emb_e5-small.rds      (60, 384) float64 embedding matrix
#   tests/testthat/fixtures/emb_e5-base.rds       (60, 768) float64 embedding matrix
#   tests/testthat/fixtures/scores_mlp_e5-small.rds  golden scores from the shipped mlp head
#   tests/testthat/fixtures/scores_mlp_e5-base.rds
#   tests/testthat/fixtures/PROVENANCE.md         source + revision record
#
# After regeneration:
#   1. Confirm the HF revision hashes in PROVENANCE.md are current.
#   2. Run `NOT_CRAN=true Rscript -e 'devtools::test()'` to verify the new fixtures pass.
#   3. If golden scores changed, add a NEWS entry (= deliberate head upgrade).
#
# NOT part of R CMD check. Do not call from tests.

# --- configuration -----------------------------------------------------------
TRAINING_BASE <- file.path(path.expand("~"), "sentiment.ai_training",
                            "sentiment.ai_training")
FIXTURE_DIR   <- "tests/testthat/fixtures"
N_PER_CLASS   <- 30L   # rows per clear-polarity class

# --- setup -------------------------------------------------------------------
stopifnot(dir.exists(TRAINING_BASE))
suppressMessages(devtools::load_all("."))
dir.create(FIXTURE_DIR, showWarnings = FALSE, recursive = TRUE)

# --- corpus: 30 pos + 30 neg from the real test split -----------------------
message("building corpus...")
idx  <- read.csv(file.path(TRAINING_BASE, "bakeoff", "full_idx.csv"))
meta <- read.csv(file.path(TRAINING_BASE, "training_data", "all_data.csv"))
te   <- merge(idx[idx$split != "train", c("index","sentiment_score")],
              meta[!meta$is_synthetic,], by = "index")
clear_pos <- te[te$sentiment_score ==  1L, ]
clear_neg <- te[te$sentiment_score == -1L, ]
corpus_df <- rbind(
  head(clear_pos, N_PER_CLASS),
  head(clear_neg, N_PER_CLASS)
)
corpus_out <- data.frame(
  text  = corpus_df$text,
  label = ifelse(corpus_df$sentiment_score == 1L, "positive", "negative"),
  score = corpus_df$sentiment_score,
  stringsAsFactors = FALSE
)
saveRDS(corpus_out, file.path(FIXTURE_DIR, "corpus.rds"))
message("corpus: ", nrow(corpus_out), " rows")

# --- embeddings: extract from the pre-computed training CSVs ----------------
# The training repo caches e5 embeddings per row; extract the corpus rows.
row_ids <- corpus_df$index

extract_rows <- function(csv_path, ids) {
  out <- list()
  con <- file(csv_path, "r")
  header <- strsplit(readLines(con, n = 1L), ",")[[1]]
  feat_cols <- startsWith(header, "V")
  while(TRUE) {
    chunk <- read.csv(textConnection(
      c(paste(header, collapse = ","), readLines(con, n = 20000L))),
      stringsAsFactors = FALSE)
    if(nrow(chunk) == 0L) break
    sub <- chunk[chunk$index %in% ids, ]
    if(nrow(sub)) out[[length(out) + 1L]] <- sub
  }
  close(con)
  do.call(rbind, out)
}

for(pair in list(
    list(tag = "me5_small_full", model = "e5-small", dim = 384L),
    list(tag = "me5_base_full",  model = "e5-base",  dim = 768L))) {

  message("extracting embeddings for ", pair$model, "...")
  csv <- file.path(TRAINING_BASE, "bakeoff",
                   paste0("sub_emb_", pair$tag, ".csv"))
  stopifnot(file.exists(csv))

  emb_df <- extract_rows(csv, row_ids)
  # match corpus row order
  emb_df  <- merge(data.frame(index = row_ids), emb_df, by = "index", sort = FALSE)
  feat    <- grep("^V", names(emb_df), value = TRUE)
  mat     <- as.matrix(emb_df[, feat])
  rownames(mat) <- corpus_out$text
  stopifnot(nrow(mat) == nrow(corpus_out), ncol(mat) == pair$dim)

  fname_emb   <- file.path(FIXTURE_DIR, paste0("emb_", pair$model, ".rds"))
  fname_score <- file.path(FIXTURE_DIR, paste0("scores_mlp_", pair$model, ".rds"))
  saveRDS(mat, fname_emb)
  scores <- find_sentiment_score(mat, "mlp", "1.0", pair$model)
  saveRDS(round(scores, 6L), fname_score)
  message("  saved ", basename(fname_emb), " and ", basename(fname_score))
}

# --- PROVENANCE.md -----------------------------------------------------------
writeLines(c(
  "# Fixture provenance",
  "",
  paste("Generated:", as.character(Sys.Date()), "by data-raw/make_test_fixtures.R"),
  "",
  "## Corpus",
  paste0("60 real-text rows (", N_PER_CLASS, " positive + ", N_PER_CLASS, " negative,"),
  "no neutral rows — corpus is clear-polarity by design). Source: held-out test",
  "split of training_data/all_data.csv, is_synthetic == FALSE.",
  "",
  "## HuggingFace revision hashes (from rpackage/sentiment.ai/R/model_meta.R)",
  paste0("intfloat/multilingual-e5-small: ",
         sentiment.ai:::model_revision["e5-small"]),
  paste0("intfloat/multilingual-e5-base:  ",
         sentiment.ai:::model_revision["e5-base"]),
  "",
  "## Scoring heads",
  "Shipped mlp JSON heads (inst/scoring/mlp/1.0/). Golden scores rounded to 6 dp.",
  "",
  "## After regeneration",
  "- If golden scores changed: add a NEWS entry.",
  "- Update HF hashes above if the model was upgraded.",
  "- Run: NOT_CRAN=true Rscript -e 'devtools::test()' to verify."
), file.path(FIXTURE_DIR, "PROVENANCE.md"))

message("done -- all fixtures written to ", FIXTURE_DIR)
