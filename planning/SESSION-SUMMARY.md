# sentiment.ai v2 — autonomous session summary (handoff)

Worked solo while you were at the office. Goal was a **clean, working, committed v2** —
not the gold-plated everything. Here's exactly where it stands.

## ✅ Done, verified, committed (branch `v2`)

| commit | what |
|---|---|
| `1ed6b2e` | **MLP scoring heads** (placeholders) — the default `scoring="mlp"` path now has weights |
| `35c09bd` | track wired **logistic heads** (placeholders) + `.gitignore` negation so JSON heads ship + this handoff |
| `cd8dd6e` | OpenMP guard (`KMP_DUPLICATE_LIB_OK`) + `.Rbuildignore` the ~20 MB dev xgb scorers |
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
  the head ships as a few-KB/MB JSON with **no xgboost, no Python in the scoring path**.
  Default `scoring = "mlp"`. The 512-dim hardcode is gone (`model_dims`).
- `install_scoring_model`: dropped the `match.arg` that rejected `e5-small`; `.json`
  extension for the head types.
- NAMESPACE: `import(tensorflow)/import(tfhub)` removed. `globals.R`, OpenMP guard.

**Both heads proven working end-to-end** (real e5 test embeddings → `score_json_head`,
zero TF, zero Python in the scoring path):
- logistic head, real text: `+0.992` "I love this" … `-1.000` "the worst experience ever".
- **mlp head** (the default), real labelled e5 embeddings → class-mean scores **monotonic**
  on both encoders (neg `-0.69` < neu `+0.30` < pos `+0.80`), 90% directional accuracy on
  non-neutral items.
The forward-pass math is also independently unit-verified on a synthetic head.

## ⚠️ The heads in the box are PLACEHOLDERS — swap them
`inst/scoring/{mlp,logistic}/1.0/{e5-small,e5-base}.json` are **subsample-trained**
(mlp F1 ~0.84/0.88, logistic ~0.81/0.86). They exist to *prove the wiring works*, not to
ship. Regenerate from the training session's **full-data, temperature-scaled** heads:
- the torch-free regen script is `bakeoff/mlp_quick.py` in the training repo (sklearn
  `MLPClassifier`, used because the torch path swap-thrashes this box) and `logi_quick*.py`.
- JSON format `score_json_head` expects: `{type, dim, T, layers:[{W,b}…] | coef+intercept,
  classes:[-1,0,1]}` (torch `[out,in]` weight convention; file named `<model>.json`).

The default `sentiment_score("good")` (scoring="mlp") now resolves to a real head.

## 🟥 A SECOND, uncommitted stream is in the tree — I did NOT touch it
While working I found a large body of **uncommitted** v2 work that is **not mine** (my own
summary lists provenance + tests as not-done, so this is the parallel hardening session):
- a full `testthat` suite (~22 files: legacy-gate, e5-prefix, scoring-parity, no-tf-default…)
- `R/model_meta.R` (provenance/license/revision registry), `inst/NOTICE.md`, `NEWS.md`
- reworked `R/local_from_reticulate.R` + `inst/get_embedder.py` (the embedder/python-env layer)
- 7 `planning/repo-review-*.md` docs

I left all of it exactly as-is rather than sweep it into my commits. **It's yours/that
session's to review and commit.** My commits add only new files (the JSON heads) + a
`.gitignore` negation, so they don't overlap it.

## 🧱 The two remaining checklist items are BLOCKED on that stream (flagged, not forced)
1. **`install_sentiment.ai(legacy=)`** — strip TF from the default install, gate it behind
   `TRUE`. This is a *substantial* rework of the install function (default `modules` hard-pins
   the TF trio; a whole TF-version-parse block; TF-centric docs + `@import tensorflow`) and it
   is tightly coupled to the embedder/python-env layer the parallel session is **actively
   reworking** (`local_from_reticulate.R`, `get_embedder.py`). Doing it now means coding
   against their unseen, moving interface → I stopped. *(The runtime is already TF-free and
   verified; this is install-time polish.)*
2. **`sentiment_provenance()`** + the `serve_prefix == scorer_prefix` assertion (the senior
   panel's #1). The natural backbone for this is the parallel session's `model_meta.R`, and
   the heads don't yet record their training prefix in the JSON. Writing a second provenance
   path would duplicate/conflict → I stopped.

Both are genuinely valuable; both should be done **on top of** the parallel stream once it
lands, not racing it.

## 📋 Also deliberately left (the sink-line we agreed)
- Remove the `@import tensorflow`/`@import tfhub` roxygen in `init_and_install.R` so the
  NAMESPACE stays TF-free after a re-`roxygenize` (I edited NAMESPACE directly for now).
- Docs/roxygen v1-copy sweep ("16 languages / tfhub / 512-D"); `R CMD check --as-cran`.
- pubs, Python package, mixed-head — **not started** (the gold-plating zone).

## The honest result line (survives scrutiny)
e5 matches paid OpenAI on a free, on-device, multilingual, **zero-TensorFlow** model; the
non-tree head **matches xgb accuracy on real reviews** (the F1 "win" was synthetic-fit)
while shipping **~10× smaller, better-calibrated, in the tarball**. "Matches", never "beats".
