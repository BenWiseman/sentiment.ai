# sentiment.ai v2 — Roadmap

> *Goal: kill the TensorFlow nightmare as the **default** path, demote it to an opt-in
> compatibility layer, and ship a fast, on-device, no-TF default — without breaking a
> single existing install.*

Status of this doc: strategy/working notes, written 2026-06-03 after an audit of the
current tree. Safe to hand to a future session as the starting point. Filename uses a
hyphen (`v2-roadmap.md`) instead of a space for shell sanity — rename if you want.

---

## TL;DR

The package is already architected for this. Three clean layers:

1. **Embedding backend** — a *swappable function pointer* (`sentiment.env$embed`).
2. **Scoring** — *pure R* (xgboost / glm on an embedding matrix). No rot here.
3. **Install/init machinery** — `init_and_install.R`. This is where the TF pinning hell lives.

The rot is confined to **layer 1's TF backend** and **the install machinery that exists to
feed it**. Layers 2 and 3-for-non-TF are fine. And the expensive missing piece — *scoring
models for non-TF embedding spaces* — **already exists** in `../sentiment.ai_training`.

So v2 is mostly **integration + dependency surgery + packaging**, not new ML.

---

## What's already true (verified, not assumed)

**Embedding backends wired or half-wired:**
- `tfhub` Universal Sentence Encoder, 512-D — `hub_embed()` in `R/embedding.R`. **The rot**
  (`tensorflow` 2.4.1 / `tensorflow_hub` 0.12.0 / `tensorflow-text` 2.4.3 / `sentencepiece`,
  plus Apple-Silicon special-casing in `onload.R` and `init_and_install.R`).
- **OpenAI** (ada-002 / 3-small / 3-large), 1536-D — `openai_embed()` + `load_openai_embedding()`.
  Committed and working (last commit: `1ab5c31 openai embedding working`).
- **sentence-transformers paraphrase** (MiniLM ≈384-D, mpnet/distilroberta 768-D) — present in
  `get_embedder_v2.py` (**uncommitted**), reticulate-based but **no TensorFlow**. `.onLoad`
  already soft-promotes `"paraphrase"` (on-device) and `"oai_3_small"` (API).

**Scoring is portable.** `find_sentiment_probs()` in `R/sentiment.R` is pure R: xgb
(`xgb.load` + `predict`) or glm (CSV weights + logistic). Reads from
`inst/scoring/{scoring}/{version}/{model}`. Nothing TF here.

**The trained models for the new spaces already exist** in `../sentiment.ai_training/.../models/`:
- `xgb_model_use.xgb`, `xgb_model_use_lg.xgb`, **`xgb_model_paraphrase.xgb`**, **`xgb_model_oai_3_small.xgb`**
- 3-class (`-1, 0, 1`) — explicitly models **neutral**, balanced to 43,437/class.
- Trained on real public corpora (sentiment140, IMDb, Amazon, financial news, TweetSemEval,
  COVID surveys, reviewshake) **plus GPT-4o synthetic neutral data** to fix neutral scarcity.
- The `neutral_mix_experiment.Rmd` writeup has a real, publishable finding: *adding synthetic
  neutral data paradoxically raises F1 on the positive/negative classes.* (Credibility asset —
  see "Strategic" below.)
- Reproducible pipeline: `raw_data_to_jsons.R` → `jsons_to_csvs.R` → `train_models.R`, with
  pre-built DMatrix binaries per space in `dmatrix_files/`.

**The coupling problem to fix.** The package hardcodes USE's dimensionality:
`sentiment_score()` passes through only `is.matrix(x) && ncol(x)==512`, and
`find_sentiment_probs()` seeds `numeric(512)`. Each embedding space has a *different* width
(512 / 768 / 1536), and each already has its *own* scoring model. v2 must make dimension and
scoring-model selection **follow the chosen backend** instead of assuming 512.

---

## Reality check: paraphrase is too weak to be the default

Measured macro-F1 (mean of the 3 class F1s) from `../sentiment.ai_training/models/f1_scores_*.csv`:

| backend | F1(−1) | F1(0) | F1(+1) | **macro-F1** | on-device? | no-TF? |
|---|---|---|---|---|---|---|
| oai_3_small | 0.933 | 0.877 | 0.870 | **0.893** | no (API) | yes |
| use_lg | 0.868 | 0.830 | 0.798 | **0.832** | yes | ❌ TF |
| use | 0.846 | 0.821 | 0.783 | **0.817** | yes | ❌ TF |
| paraphrase | 0.816 | 0.812 | 0.751 | **0.793** | yes | ✅ |

**The bind this exposes (the actual hard problem of v2):**
- Best overall = `oai_3_small` (0.893) but it's **API-only** (cost, key, not free/offline).
- Best on-device = `use_lg` (0.832) but it's **welded to TF** — the thing we're escaping.
- The *only* no-TF **and** on-device option = `paraphrase` (0.793), the **weakest** of the four,
  and it collapses on the positive class.

So "escape TF" and "strong free on-device default" are in tension, and paraphrase does **not**
resolve it. It cannot be the flagship default.

**Why this is fixable, not fatal:** "paraphrase" = `paraphrase-MiniLM-L6-v2`, a small *2021*
distilled model — weak by today's standards, and it runs on PyTorch/ONNX, **not TF**. The fix is
to retrain the scorer on a *modern* no-TF on-device embedder. See the bake-off (Phase 3.5).

---

## The core move: TF becomes a compatibility layer

Today: TF is a hard `Imports` dependency and the default path. Every install pays the TF tax.

v2: TF is an **opt-in `Suggests`**, loaded lazily **only** when the user explicitly asks for a
`use`/`en.large`/`multi*` model. Existing scripts that name a USE model still work — they just
trigger a one-time "installing the legacy USE backend…" path. The no-TF *default* model is an
**open, empirical question** pending the bake-off — it is NOT paraphrase-MiniLM.

```
best overall:    text → OpenAI 3-small (API)            → xgb_model_oai_3_small → score   [0.893]
best on-device:  text → <winner of Phase 3.5 bake-off>  → retrained xgb         → score   [TBD]
legacy/compat:   text → tfhub USE-512 (lazy TF)         → xgb_model_use(_lg)    → score   [0.83]
fast/weak:       text → paraphrase-MiniLM (no TF)       → xgb_model_paraphrase  → score   [0.793]
```

---

## Phased plan

### Phase 0 — Land the in-flight work (don't lose it)
The tree has **uncommitted v2 work**: modified `DESCRIPTION`, `NEWS.md`, `init_and_install.R`,
`onload.R`, `inst/get_embedder.py`, requirements files, and new `get_embedder_v2.py`.
- Commit it on a `v2` branch with a clear message before touching anything else.
- Reconcile `get_embedder.py` (committed) vs `get_embedder_v2.py` (new) into one embedder entry
  point. Decide the canonical name; delete the dead one.

### Phase 1 — Dependency surgery
- Move `tensorflow`, `tfhub` from `Imports` → `Suggests` in `DESCRIPTION`.
- Keep `reticulate` (still needed for sentence-transformers + OpenAI-via-python; the OpenAI
  path can also be pure-`httr`, see Phase 6).
- Add `sentence-transformers` to the install module lists; strip the hard TF version pins from
  the default install (`init_and_install.R` lines ~104–127).
- Replace the unconditional TF warning in `onload.R` with backend-aware messaging.

### Phase 2 — Formalize the backend registry
- Turn the implicit `sentiment.env$embed` pointer into an explicit backend table:
  `{ name, kind (st|openai|tfhub), dim, default_scoring_model, init_fn }`.
- Kill hardcoded `512`: `sentiment_score()` passthrough check and `find_sentiment_probs()`
  read `dim` and the matching scoring model from the registry.
- `constants.R`: add `paraphrase`/`oai_3_small` (and friends) alongside `default_models`.

### Phase 3 — Package the new scoring models
- Copy `xgb_model_paraphrase.xgb` + `xgb_model_oai_3_small.xgb` from the training repo into
  `inst/scoring/xgb/{version}/…` (or wire `install_scoring_model()` to fetch them, as USE does).
- Update `models/available_models.csv` (currently only `xgb,1.0,en.large,TRUE`).
- Decide bundle-vs-download per model size and CRAN's 5 MB limit.

### Phase 3.5 — Embedding bake-off (the deciding experiment)
The default on-device model is unknown until measured. paraphrase-MiniLM (0.793) is too weak;
USE-large (0.832) needs TF. Goal: find a **no-TF, on-device** embedder that meets or beats the
**0.832 use_lg bar** (ideally closes on oai's 0.893).
- Candidates, roughly newest/strongest first: `all-mpnet-base-v2` (already wired), then 2024-era
  `bge-small/base-en-v1.5`, `gte-small/base`, `e5-base-v2`, `nomic-embed-text-v1.5`. All run on
  PyTorch/ONNX, **not TF**.
- Method: reuse the pipeline (`raw_data_to_jsons.R` → embeddings → `train_models.R`). Only the
  embed step is new per candidate; xgb retrain is cheap. Report macro + per-class F1 vs the
  use_lg / oai baselines, same test split.
- Watch model size + dim (mpnet 768-D, bge/gte/e5 base ≈768-D) for the CRAN/packaging budget.
- **Decision output:** the winner becomes the on-device default; if *nothing* clears ~0.83,
  fall back to "USE (TF compat) stays the default, paraphrase/mpnet offered as the no-TF option
  with an honest accuracy caveat" — don't ship a weak default just to be rid of TF.

### Phase 4 — Flip the default (carefully)
- New default backend = **the Phase 3.5 winner**, not paraphrase. **Any default change alters
  scores for scripts that rely on the default** → major-version bump (`1.0.0`), loud `NEWS.md`,
  keep `model="use"` honored.
- Consider a one-release transition where the old default still resolves but warns.
- If the bake-off fails to beat use_lg, the "default" stays USE via the compat layer and the
  headline becomes *"no-TF option added,"* not *"TF-free by default"* — set the NEWS framing to
  match what's actually true.

### Phase 5 — Install UX
- Single `install_sentiment.ai(backend = "paraphrase" | "openai" | "use")`:
  - `paraphrase` → minimal CPU env (numpy + sentence-transformers + sentencepiece), no TF,
    no tensorflow-text, Apple-Silicon-clean.
  - `openai` → no local model at all (pure API; ideally no python — see Phase 6).
  - `use` → the legacy TF env, lazily, with the old pins quarantined here.

### Phase 6 — No-Python north star (v2.x / v3 research spike)
The user hates Python too, not just TF. Two ways to shed it:
- **OpenAI path → pure `httr`** (no reticulate). `load_openai_embedding()` already does HTTP;
  finish that and the API path needs zero Python.
- **On-device path → ONNX.** Export the paraphrase model to ONNX and run via `onnxruntime`
  (or a Rust/`candle` backend compiled into the package). This is the real "no-Python install
  that just works for students" endgame. Spike it; don't block v2 on it.
- Pure-R fallback (`text2vec` GloVe) is weaker but a zero-dependency safety net worth a stub.

### Phase 7 — Tests, CI, benchmarks
- Snapshot tests per backend (deterministic embeddings → known scores).
- Port the training-repo benchmarks into a package vignette: USE vs paraphrase vs 3-small vs
  lexicon baselines, on the real corpora. Lead with the neutral-data F1 finding.
- GH Actions matrix: default (no-TF) path must pass with **zero** TF installed.

### Phase 8 — Linnet integration
- Once the no-TF default is stable, this is the embedding + sentiment **engine** under the
  planned `linnetlabs` text-network package (see linnet memory `project_linnetlabs_text_package`):
  embeddings feed the semantic-network/coherence layer; sentiment overlays the graph.

---

## Open decisions (flag for the user)

1. **What is the on-device default** — *settled by the Phase 3.5 bake-off, not by guess.*
   paraphrase-MiniLM (0.793) is out as default. The question is whether a modern no-TF embedder
   (mpnet / bge / gte / e5) clears the use_lg 0.832 bar. If yes → it's the default. If no → USE
   stays default via the TF compat layer and "no-TF" becomes an *option*, not the default.
2. **Bundle vs download** the new scoring models (CRAN 5 MB).
3. **Default-flip timing** — flip in one major bump, or a warn-first transition release?
4. **Attribution / branding.** Package is MIT and **funded by / attributed to the Korn Ferry
   Institute** (DESCRIPTION authors). You're lead author + maintainer, so continuing development
   and having Linnet depend on it is clean — but keep the KFI authorship history intact; don't
   "rebrand as Linnet's." (Note the irony worth savouring privately, not publicly: a KFI-funded
   OSS package becoming the engine under the Korn-Ferry-killer surface.)

---

## Strategic notes (why this is worth doing)

- **Provenance is the moat.** sentiment.ai pioneered transformer-embedding sentiment in R and
  predates the imitators. v2 reclaims that lead by being the *better, still-original* tool —
  let publish dates talk; never namecheck the copier publicly (reads insecure).
- **The synthetic-neutral finding is a paper + a credibility line**, not just a training trick.
  It's exactly the "grounded, not invented" rigor the brand sells.
- **Frictionless install IS the product.** The whole student-adoption play dies if v2 re-imports
  the TF/Python fragility. Phase 6 (no-Python) is the strategic core, not a nice-to-have.

## Risks / watch-outs
- Changing the default silently changes users' numbers — version + NEWS discipline is mandatory.
- sentence-transformers still drags in Python; it's a big step down from TF in fragility but not
  zero. ONNX is the real fix.
- Keep the legacy USE path genuinely working (real test, real install), or "compatibility layer"
  is a lie that breaks trust with existing users.
