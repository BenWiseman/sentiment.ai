# Test fixtures

Regression anchors for the Tier-B scorer snapshot tests. Generated **offline,
once**, by `data-raw/make_test_fixtures.R` with the real embedders + the packaged
xgb scorers installed. The test run never re-derives them.

Expected contents (committed by the fixture generator):

| file | what |
|---|---|
| `corpus.rds` | ~15 labelled sentences (pos/neg/neutral) |
| `emb_e5-small.rds` | 384-d e5-small embeddings of `corpus` |
| `emb_e5-base.rds` | 768-d e5-base embeddings of `corpus` |
| `emb_text-embedding-3-small.rds` | 1536-d OpenAI embeddings of `corpus` |
| `scores_<backend>_xgb.rds` | golden scores from the current xgb scorer |
| `emb_e5-small_prefixed.rds` | embed of `"query: I love this"` |
| `emb_e5-small_unprefixed.rds` | embed of `"I love this"` |
| `PROVENANCE.md` | HF revision hashes + `neutral_band` (read from training eval) |

Until these are committed, the Tier-B / known-string tests `skip` (via
`skip_if_no_fixture()`) rather than error. The `neutral_band` value is **read from
`PROVENANCE.md`**, which must be filled from the training-repo eval — it is never
invented in a test.
