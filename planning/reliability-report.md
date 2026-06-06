# sentiment.ai v2 — reliability / calibration report (the record)

Backs the word **"calibrated"** in `sentiment()` / `NEWS.md` / the READMEs. The shipped mlp
heads are temperature-scaled during training; this measures how well that worked. **No
proprietary / Korn Ferry data** — same public + GPT-synthetic test split as the embedder
benchmark ([benchmark-v2.md](benchmark-v2.md)).

## What "calibrated" means here

A probability is *calibrated* if, among the rows it calls positive with 0.8 confidence, about
80% really are positive. We measure the gap between confidence and accuracy:

- **ECE** (Expected Calibration Error, Guo et al. 2017): rows binned by top-class confidence
  (15 bins), the size-weighted mean of `|accuracy − confidence|`. 0 = perfect; **< 0.02 is
  well-calibrated**, deep nets are commonly 0.05–0.15 before calibration.
- **per-class ECE**: the same, one-vs-rest, for each of negative / neutral / positive.
- **Brier score** (Brier 1950): mean squared error of the 3-class probability vector vs the
  one-hot truth (range 0–2; lower is better) — a *proper* scoring rule combining calibration
  and sharpness.

## Method

The **shipped** head (`inst/scoring/mlp/1.0/<model>.json` — exactly what a user gets, weights
rounded to 5 sig figs for CRAN) is forward-passed in numpy over the **held-out test split's**
cached e5 embeddings, and compared to the true labels. Reported on the **full** test
(n = 3,255, 61.7% synthetic) and the **real-only** slice (n = 1,247). Script:
`sentiment.ai_training/bakeoff/reliability.py`.

## Results

| Model | head | slice | n | acc | **ECE** | Brier | per-class ECE (neg/neu/pos) |
|---|---|---|---:|---:|---:|---:|---|
| e5-small | shipped mlp | full | 3255 | 0.874 | **0.0119** | 0.180 | 0.010 / 0.015 / 0.016 |
| e5-small | shipped mlp | real-only | 1247 | 0.887 | **0.0152** | 0.160 | 0.017 / 0.017 / 0.026 |
| e5-base | shipped mlp | full | 3255 | 0.906 | **0.0168** | 0.131 | 0.009 / 0.012 / 0.014 |
| e5-base | shipped mlp | real-only | 1247 | 0.930 | **0.0166** | 0.102 | 0.019 / 0.008 / 0.016 |

### Reliability diagram — e5-small, real-only (confidence bin → empirical accuracy)

```
 0.40-0.47  n=   7  conf=0.440  acc=0.429   (confidence tracks accuracy down the diagonal)
 0.53-0.60  n=  45  conf=0.565  acc=0.533
 0.67-0.73  n=  60  conf=0.702  acc=0.733
 0.73-0.80  n=  82  conf=0.770  acc=0.780
 0.80-0.87  n= 116  conf=0.836  acc=0.810
 0.87-0.93  n= 165  conf=0.904  acc=0.915
 0.93-1.00  n= 686  conf=0.978  acc=0.988
```

## Honest read

1. **The shipped probabilities are well-calibrated** — ECE ≈ 0.012–0.017 (top-label) and
   ≤ 0.026 per class, on both the full and the real-only test. The temperature scaling fit at
   train time did its job; no post-hoc recalibration is needed for v2.
2. **Calibration holds on real text**, not just the synthetic-heavy full test (real-only ECE
   0.015 / 0.017), so the claim is not an artifact of the GPT rows.
3. **e5-base is both more accurate and slightly lower-Brier** than e5-small; both are
   comparably well-calibrated.

## Caveats / scope of the claim

- **Single split, single seed.** No cross-seed confidence intervals on ECE yet.
- **In-sample temperature risk.** If the temperature was fit on rows overlapping this test
  split, the ECE is mildly optimistic; the real-only slice is the more conservative read and is
  still ≤ 0.017.
- **Neutral is thin in real data** (real neutral n ≈ 60), so per-class neutral ECE on real text
  is noisier than the others.
- Calibration is measured **in-domain** (reviews / short verbatims). It is not guaranteed to
  transfer to very different text (legal, code, heavy sarcasm). A drift/OOD signal is on the
  roadmap.

## Reproduce

```bash
cd sentiment.ai_training            # the (separate) training repo
python3 bakeoff/reliability.py      # ECE / Brier / reliability table, full + real-only
```
