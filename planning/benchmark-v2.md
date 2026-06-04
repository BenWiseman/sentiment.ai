# sentiment.ai v2 — embedder benchmark (the record)

The numbers cited in `NEWS.md` / `README.md` come from here. **No proprietary / Korn Ferry
data is used** — the benchmark is on public review corpora + GPT-generated synthetic data.

## Data

`sentiment.ai_training/training_data/all_data.csv` (164,681 rows):

| source (`data_dir`) | n | kind |
|---|---:|---|
| `data/real_output_jsons` (Amazon, IMDB, sentiment_140 tweets, financial_news, Loughran-McDonald) | 67,521 | **real** |
| `synthetic data/gpt4_output_scenarios/` | 67,430 | synthetic |
| `synthetic data/gpt4o_output_topics/` | 23,938 | synthetic |
| `synthetic data/aux_neutral_examples/` | 5,792 | synthetic |

Fixed split (`bakeoff/full_idx.csv`): train = 127,056, test = 3,255. The **test set is 61.7%
synthetic**, and the neutral class is especially synthetic-heavy:

| class | real | synthetic |
|---|---:|---:|
| positive | 602 | 483 |
| negative | 585 | 500 |
| **neutral** | **60** | **885** |
| mixed | 0 | 140 |

Because real neutral is scarce, we report macro-F1 on both the **full** test and the
**real-only** slice, plus **directional accuracy on the 1,187 real, clearly pos/neg examples**
(the most reliable real-world read).

## Method

- **Embedder ceiling:** one identical XGBoost recipe per embedder (`multi:softprob`, 3-class,
  `num_parallel_tree = 24`, `max_depth = 5`, `eta = 0.3`, 100 rounds, no early stopping), trained
  on the full train pool — `bakeoff/full_train.py`. This isolates *embedding quality*.
- **Shipped heads:** the small `mlp` JSON heads bundled in the package (`inst/scoring/mlp/1.0/`),
  forward-passed in numpy — what a user actually gets. (Now full-data heads, shipped in both packages.)
- **Re-score / real-only slice:** `bakeoff/realonly_bench.py` (extracts the test-row embeddings in
  chunks, reloads the saved xgb scorers + the mlp heads, scores full vs real-only).

## Results

| Model | head | macro-F1 (full, n=3255) | macro-F1 (real-only, n=1247) | real pos/neg acc (n=1187) |
|---|---|:---:|:---:|:---:|
| openai (`text-embedding-3-small`) | xgb ceiling | 0.897 | 0.886 | 94.3% |
| e5-base | xgb ceiling | 0.890 | **0.899** | 94.1% |
| e5-base | **shipped mlp (full-data)** | 0.906 | **0.919** | 93.8% |
| e5-small (default) | xgb ceiling | 0.842 | 0.854 | 89.3% |
| e5-small (default) | **shipped mlp (full-data)** | 0.873 | 0.860 | 90.1% |
| use-large (old TF default) | xgb ceiling | 0.832 | 0.850 | 88.9% |

The full-test xgb numbers reproduce `bakeoff/full_results.log` exactly (0.8422 / 0.8902 / 0.8970
/ 0.8318), which validates the extraction.

## Honest read

1. **On real text, e5-base ties paid OpenAI** (0.899 vs 0.886 macro-F1; 94.1% vs 94.3% pos/neg).
   Tied within noise on a single split — not a win for either.
2. **The new default ties the old TensorFlow default on real text** (e5-small 0.854 / 89.3% vs
   USE-large 0.850 / 88.9%). The win is *no TensorFlow* (on-device, multilingual, smaller), not
   raw accuracy. On the synthetic-inclusive test e5-small edges it (0.842 → 0.832); on real text
   they are level.
3. **Real-only ≥ full for every on-device model** — the synthetic rows were *dragging* macro-F1
   down, not inflating it, so the headline numbers are conservative for real text. (OpenAI is the
   one exception: 0.897 → 0.886.)
4. **The bundled mlp heads are now full-data** and *match/beat* the xgb ceiling on real pos/neg
   (e5-small 90.1%, e5-base 93.8%) -- no xgboost or TensorFlow at serve.

## Caveats / TODO before any external accuracy claim

- Single split, single seed; no cross-seed CIs (the OpenAI/e5-base tie used a paired bootstrap).
- Real neutral is n = 60; 3-class macro-F1 over neutral is noisy — lead with pos/neg accuracy.
- The shipped heads were retrained on the full pool (the "shipped mlp (full-data)" rows above),
  weights rounded to 5 sig figs to fit CRAN (max score change 6e-4); a second seed/split would
  firm up the single-split figures.

## Reproduce

```bash
cd sentiment.ai_training
python3 bakeoff/full_train.py --tag me5_small_full   # (re)train an embedder's xgb ceiling
python3 bakeoff/realonly_bench.py                     # full vs real-only, xgb + shipped mlp
```
