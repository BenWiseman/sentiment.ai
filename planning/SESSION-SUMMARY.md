# sentiment.ai v2 — autonomous session summary (handoff)

Worked solo while you were at the office. Goal was a **clean, working, committed v2** —
not the gold-plated everything. Here's exactly where it stands.

## ✅ Done, verified, committed (branch `v2`)

| commit | what |
|---|---|
| `cd8dd6e` | OpenMP guard (`KMP_DUPLICATE_LIB_OK`) + `.Rbuildignore` the 60 MB dev xgb scorers |
| `48c1529` | **backend wiring**: 3-way dispatch + non-tree head + ST routing |
| `613e12f` / `8e7b4a6` | foundation (TF→Suggests, e5 registry, resolver) + reviews/roadmaps |

**The wiring (all verified by `pkgload::load_all` — package loads clean):**
- `init_sentiment.ai`: `model_class()` → **st / openai / legacy** dispatch. ST loads
  `load_st_embedder(hf_id, prefix)`; legacy is gated behind a *directed* error
  ("run `install_sentiment.ai(legacy = TRUE)` or use `e5-base`") instead of a raw
  reticulate ImportError; default arg → `DEFAULT_MODEL`.
- `embed_text`: ST branch (returns `(n, dim)` directly, no transpose) + `env$st` flag.
- `find_sentiment_score`: 3-class `softprob → P(pos) − P(neg)`; **`score_json_head`** —
  a pure-R MLP/logistic forward pass (matrix mults + ReLU + temperature + softmax) so
  the head ships as a few-KB JSON with **no xgboost, no Python in the scoring path**.
  Default `scoring = "mlp"`. The 512-dim hardcode is gone (`model_dims`).
- `install_scoring_model`: dropped the `match.arg` that rejected `e5-small`; `.json`
  extension for the head types.
- NAMESPACE: `import(tensorflow)/import(tfhub)` removed. `globals.R`, OpenMP guard.

**Proven working end-to-end** (real text → e5-base embed, zero TF → wired
`find_sentiment_score` → `score_json_head` logistic head):
```
+0.992  I love this, it is wonderful
-0.999  this is awful, I hate it
-0.205  the meeting is scheduled for 3pm   (near-neutral)
+0.907  pretty good would recommend
-1.000  honestly the worst experience ever
```
And the forward-pass math is independently unit-verified on a synthetic head.

## ⚠️ The heads in the box are PLACEHOLDERS — swap them

`inst/scoring/logistic/1.0/{e5-small,e5-base}.json` are **subsample-trained** logistic
heads (e5-small 0.810, e5-base 0.856) — they exist to *prove the wiring works*, not to
ship. **Replace with your other session's tuned, full-data, temperature-scaled heads**
(e5-small mlp 0.871, e5-base mlp 0.908; logistic ties on the big encoders):
- drop `mlp_e5-small.json`, `mlp_e5-base.json` → `inst/scoring/mlp/1.0/`
- drop the tuned logistic JSONs → `inst/scoring/logistic/1.0/` (overwrite the placeholders)
- the JSON format `score_json_head` expects: `{type, dim, T, layers:[{W,b}…] | coef+intercept, classes:[-1,0,1]}` (torch `[out,in]` weight convention).

Once the **mlp** heads are in, a no-arg `sentiment_score("good")` uses the intended
default. Until then, `scoring = "logistic"` runs on the placeholders.

## 🧱 Env wall I hit (flagged, did not force)
- **This Mac swap-thrashes on torch training + 1 GB CSV loads** — every full-data head
  job stalled at 0% CPU. That's why the shipped weights come from your other session,
  not regenerated here. (The lightweight subsample/sklearn path ran fine.)
- The **public-API** `sentiment_score()` path needs the reticulate `r-sentiment-ai` env
  to contain `sentence-transformers`. The *scoring* path is proven; the env-managed
  `init → embed` step needs your reticulate setup (or `RETICULATE_PYTHON` → a python
  with sentence-transformers). `install_sentiment.ai()` still hard-pins TF in its default
  module list — the `legacy = FALSE/TRUE` split is **not yet wired** (deliberately left).

## 📋 Deliberately left for you (the sink-line we agreed)
- `install_sentiment.ai(legacy=)` — strip TF from the default install, gate TF behind `TRUE`.
- `sentiment_provenance()` + the `serve_prefix == scorer_prefix` assertion (senior panel's #1).
- Remove the `@import tensorflow`/`@import tfhub` roxygen in `init_and_install.R` so the
  NAMESPACE stays TF-free after a re-`roxygenize` (I edited NAMESPACE directly for now).
- Docs/roxygen v1-copy sweep ("16 languages / tfhub / 512-D"); `R CMD check --as-cran`.
- pubs, Python package, mixed-head — **not started** (the gold-plating zone).

## The honest result line (survives scrutiny)
e5 matches paid OpenAI on a free, on-device, multilingual, **zero-TensorFlow** model; the
non-tree head **matches xgb accuracy on real reviews** (the F1 "win" was synthetic-fit)
while shipping **~10× smaller, better-calibrated, in the tarball**. "Matches", never "beats".
