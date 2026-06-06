# Fixture provenance

## Corpus (corpus.rds)

60 real-text rows (30 positive, 30 negative — no neutral rows; the fixture is
explicitly clear-polarity by design):
- Source: held-out test split of `training_data/all_data.csv` from the
  `sentiment.ai_training` repository (separate from this repo).
- Rows selected: `split != "train"`, `is_synthetic == FALSE`,
  `sentiment_score %in% c(-1, 1)`, head(30) of each class.
- Column mapping: `text`, `label` ("positive"/"negative"), `score` (-1/1).

## Embeddings (emb_e5-small.rds, emb_e5-base.rds)

384-column / 768-column float64 matrices (60 rows each), extracted from the
cached bakeoff embedding CSVs (`bakeoff/sub_emb_me5_{small,base}_full.csv`).
These embeddings were produced by `sentence-transformers` using the model at the
revision pinned in `model_meta.R`:

- e5-small: `intfloat/multilingual-e5-small` @ `614241f622f53c4eeff9890bdc4f31cfecc418b3`
- e5-base:  `intfloat/multilingual-e5-base`  @ `d128750597153bb5987e10b1c3493a34e5a4502a`

Prefix: `"query: "` prepended to each text before encoding (e5 convention).

## Golden scores (scores_mlp_e5-{small,base}.rds)

Produced by `find_sentiment_score(embs, "mlp", "1.0", model)` on the embedding
matrices above, using the shipped JSON head at the time of fixture generation
(`inst/scoring/mlp/1.0/`). Rounded to 6 decimal places.

Generated: 2026-06-06 from `rpackage/sentiment.ai` at commit v2 branch.

## Regeneration

To regenerate, run:
  python3 /path/to/sentiment.ai_training/bakeoff/extract_fixtures.py
  Rscript rpackage/sentiment.ai/data-raw/make_fixtures.R

**After regeneration:**
1. Update HF revision hashes above if the model was upgraded.
2. Add a NEWS entry if the golden scores changed (= deliberate head upgrade).
3. Run `R CMD check` to confirm the snapshot tests pass.
