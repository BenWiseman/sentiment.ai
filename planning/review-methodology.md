# Bake-off methodology — adversarial validity review

Author of review: red-team pass for sentiment.ai v2.
Date: 2026-06-03.
Scope: the Phase-3.5 embedder bake-off in
`../sentiment.ai_training/sentiment.ai_training/bakeoff/`.
Status of the bake-off itself: **provisional** (subsample ranking done; full-data run in
flight). All numbers below were read from the harness scripts, the `sub_f1_*.csv` files, and
the run logs — not assumed.

> Bottom line: the bake-off is a *reasonable internal ranking* of embedders under one fixed,
> identical pipeline, and it is fair *as a ranking* because every candidate sees byte-identical
> splits and xgb config. But it does **not** currently support the publishable claims being
> drafted around it — specifically "e5-base ties OpenAI", "e5-base > bge-base", and any
> absolute macro-F1 headline — because there are no confidence intervals, a single seed, a test
> set whose neutral class is ~93% GPT-generated text, an e5 prefix that appears **not to have
> been applied**, and a config that differs between the ranking and the headline runs. Fixes
> below are concrete and mostly cheap.

---

## What the harness actually does (verified)

- **Split.** `make_subsample.R` / `make_full_idx.R` / `train.R` all reproduce
  `create_dmatrix()` verbatim: `set.seed(42)` → balance to `min(43437, .N)` per class →
  `createDataPartition(p = 0.975)`. The 3,255-row test set is **the same rows for every
  candidate**. Good: the test set is held fixed across embedders.
- **Ranking trainer** (`sub_train.R`): `set.seed(7)`, `num_parallel_tree = 1`, 6,000/class
  train subsample (18,000 train / 3,255 test), `nrounds = 100`,
  `early_stopping_rounds = 3` on the **test** set, metric = per-class F1, headline = unweighted
  mean of the three class F1s ("macro-F1").
- **Headline trainer** (`full_train.py`): `seed = 7`, `num_parallel_tree = 24`, `nthread = 0`
  (multithreaded, dodges the dev-Mac single-thread/no-OpenMP xgboost), full ~127k train, same
  3,255 test.
- **Embedders**: `sub_embed.py` (sentence-transformers, `USE_TF=0`/`USE_TORCH=1`, MPS),
  `embed.py` (full corpus), `sub_subset.py` (reuse old USE/OAI/paraphrase embeddings by index).
- **Provisional ranking** (from `sub_f1_*.csv`, mean of 3 class F1s):
  oai_3_small 0.8610 · me5_base 0.8599 · bge_base 0.8362 · gte_base 0.8246 ·
  bge_small 0.8243 · me5_small 0.8132 · use_lg 0.7905 · mml_mpnet 0.8020 · mpnet 0.7811 ·
  paraphrase 0.7510. (These are the numbers the LOCKED-lineup claims rest on.)

---

## Findings, ranked by severity

### S1 — CRITICAL: the test set's neutral class is ~93% GPT-generated text. The headline measures "recognise GPT-4o's neutral style", not neutral sentiment.

**Evidence.** `all_data.csv` has 164,714 rows; **97,160 (59%) are `is_synthetic == TRUE`**.
By `data_dir`: `real_output_jsons` 67,521; `gpt4_output_scenarios` 67,430;
`gpt4o_output_topics` 23,938; `aux_neutral_examples` 5,792. Of the **43,437 neutral rows,
40,576 (93.4%) are synthetic** and only 2,861 are real. The balanced sampler takes
`min(43437, .N)` neutral = **all** of them, so the neutral test rows are ~93% GPT-written by
construction.

**Why this is the top threat.**
- The drafted "synthetic-neutral raises F1 on pos/neg" finding and the headline macro-F1 are
  partly circular: synthetic neutral text has detectable stylistic regularities (templated
  topic framing, register, length), so a strong encoder + xgb can hit the neutral class by
  recognising *the generator*, not the *construct*. F1(0) is then an upper bound that will not
  transfer to natural neutral text (the exact class that is scarce and that the package most
  needs to get right).
- It also confounds the cross-model ranking: any embedder whose pretraining distribution is
  closer to GPT-4o's output distribution (most modern web-scraped encoders, including e5/bge/gte
  trained on post-2021 web text that already contains LLM output) gets a tailwind that USE
  (2018-era) and paraphrase (2021) do not. This is a plausible partial explanation for why the
  modern encoders cluster so far above USE — it is *not isolated* from genuine embedding quality.
- This is a **construct-validity + contamination** problem, not just a balance problem.

**Fixes (do before any publication).**
1. Build a **real-only neutral evaluation slice**: hold out the 2,861 real neutral rows (plus
   any real neutral you can source — financial-news "neutral", TweetSemEval neutral) as a
   *separate* test stratum and report F1(0) on it. If F1(0) collapses there, the synthetic-neutral
   finding is a style artefact and must be reframed.
2. Report the headline **two ways**: (a) current balanced set, (b) a "natural" set with neutral
   drawn only from real corpora. The gap between (a) and (b) is itself the publishable result.
3. For the "synthetic neutral helps pos/neg" claim specifically: the *test* set's pos/neg are
   majority-real, so that claim is more defensible — but state explicitly that **train**-side
   synthetic neutral is what helps, and **test** neutral must be real to measure it honestly.
   Run the ablation (train with/without synthetic neutral) and evaluate **only on real test
   rows** for all three classes.

---

### S2 — CRITICAL: the e5 `query:` prefix appears NOT to have been applied. Every e5 number may be on the wrong (degraded) embedding, which makes the "e5-base ties OpenAI" claim un-trustworthy in *either* direction.

**Evidence.** `sub_embed.py` only prepends the prefix when `--prefix "query: "` is passed, and
it does **not** echo the prefix in its header line. The `embed_modern2.log` / `full_embed.log`
headers for the e5 runs read `... | intfloat/multilingual-e5-base | tag=me5_base | dev=mps:0 |
maxseq=512` with **no prefix shown**, and `grep -c "query:"` over all bake-off logs returns
**0**. The brief assumes e5 was run *with* `query: `; the artefacts suggest it was run *without*.

**Why it matters.** The intfloat e5 family (Wang et al., 2022/2024, *"Text Embeddings by Weakly-
Supervised Contrastive Pre-training"* / *"Multilingual E5"*) is trained with mandatory
`query:` / `passage:` instruction prefixes; the model card states embeddings are degraded if the
prefix is omitted. So:
- If e5 ran **without** the prefix, 0.860 is an **under**-estimate — e5-base could be *beating*
  OpenAI once corrected, which strengthens the on-device story but means the published number is
  wrong.
- Either way, an asymmetric-prefix bake-off (e5 needs a prefix, bge/gte/OpenAI do not) is only
  fair if each model is run **in its documented best-practice configuration**. Right now we don't
  know which configuration e5 was in.

**Fixes.**
1. Re-run both e5 models **with `--prefix "query: "`** and (separately) without, and record both
   macro-F1s. Publish the prefixed number as the e5 result; report the delta as a footnote.
   (For sentiment, all inputs are symmetric documents, so `query: ` on both sides is the correct,
   model-card-sanctioned choice; do **not** mix `query:`/`passage:`.)
2. Echo the actual prefix in the embedder log header (one-line change to `sub_embed.py`/`embed.py`)
   so this can never again be ambiguous from the logs.
3. Apply the same "documented best config" rule to bge/gte: BAAI's bge-*-en-v1.5 retrieval card
   recommends a query instruction for *retrieval*, but for symmetric similarity/classification the
   card says no instruction — so bge/gte with no prefix is correct. Document this decision per
   model so the asymmetry is principled, not accidental.

---

### S3 — HIGH: no confidence intervals, single seed, single split → the headline ties and orderings are not shown to be real.

**Evidence.** `set.seed(7)` once; one `createDataPartition` split; F1 reported to 4 decimals with
no variance. The decisive gaps are tiny: **oai 0.8610 vs me5_base 0.8599 = 0.0011**;
**me5_base 0.8599 vs bge_base 0.8362 = 0.0237**; **bge_base vs gte_base 0.8246 = 0.0116**;
**bge_small 0.8243 vs me5_small 0.8132 = 0.0111**.

**Why it matters.** On 3,255 test items, the per-class F1 standard error is on the order of
~0.01–0.015 (a single class F1 is built from ~1,085 positives; a rough binomial SE on the
underlying precision/recall at p≈0.85 is sqrt(.85·.15/1085) ≈ 0.011). The macro-F1 SE is
similar in magnitude. So:
- **"e5-base ties OpenAI" is meaningful only as "indistinguishable", and that is exactly the
  right framing** — 0.0011 is far inside noise. But "ties" should be stated as *"within noise;
  no significant difference (overlapping CIs)"*, not as a measured equality.
- **"e5-base > bge-base" (0.024)** is *probably* real but is **not demonstrated** at one seed.
  It is ~1.5–2 SE; you need CIs or a paired test to claim it.
- **bge-small vs me5-small (0.011)** and **bge-base vs gte-base (0.012)** are **within noise** —
  do not assert an ordering between these without a test.

**Fixes (cheap, high value).**
1. **Paired bootstrap on the fixed test set**: resample the 3,255 test items with replacement
   (B = 2,000), recompute each model's macro-F1 on the resample, report mean and 2.5/97.5
   percentile CI per model, and the **paired** ΔF1 distribution for each contrast (e5-base −
   OpenAI, e5-base − bge-base, …). This reuses the already-saved per-row predictions — no
   re-embedding, no re-training. This is the single highest ROI fix.
2. **Multi-seed**: re-run `sub_train.R` for seeds {7, 17, 23, 42, 101} (xgb subsample = 0.5 makes
   it stochastic) and report mean ± sd of macro-F1 per embedder. Five seeds × ten embedders is
   minutes of compute and kills the single-seed objection.
3. Report ties honestly: in NEWS/docs say *"e5-base is statistically indistinguishable from
   OpenAI 3-small on this benchmark (paired bootstrap, 95% CI on ΔF1 includes 0)."*

---

### S4 — HIGH: test contamination of LLM-pretrained encoders by public sentiment corpora (sentiment140 / IMDb / Amazon).

**Evidence.** `data/real_output_jsons` (67,521 rows) is the real corpus; the roadmap names the
sources as sentiment140, IMDb, Amazon reviews, financial news, TweetSemEval, COVID surveys,
reviewshake. The first real row in `all_data.csv` is a product-review sentence; sampled real
neutral/negative rows are clearly IMDb-style review text ("how has this piece of crap stayed on
tv this long?").

**Why it matters.** IMDb, sentiment140, and Amazon reviews are in the pretraining corpora of
essentially every modern encoder (Common Crawl / web text). The *embeddings* of these encoders
may already encode label-correlated structure memorised at pretraining time — a known issue for
benchmarking on canonical sentiment sets (cf. the MTEB classification caveats; and the general
data-contamination literature, e.g. Sainz et al. 2023 *"NLP Evaluation in Trouble"*, and the
"train-on-test" leakage discussion in the LLM-eval literature). This *inflates absolute F1* for
modern encoders relative to USE, and could subtly reorder the modern encoders by how much each
saw these corpora. It does not invalidate an internal ranking, but it **does** invalidate any
claim that these numbers estimate real-world generalisation to *unseen* text.

**Fixes.**
1. Add a **held-out, post-cutoff or proprietary test slice** that is not in any public crawl —
   e.g. recent in-house labelled text, or a freshly collected set — and report headline F1 there
   too. The gap vs the public-corpus number quantifies contamination.
2. Document, per source corpus, its public-crawl status, and caveat the absolute numbers as
   *"benchmarked on partially-public corpora; treat as upper bounds on generalisation."*
3. Do **not** publish absolute macro-F1 as a real-world accuracy estimate; publish it as a
   *relative* embedder comparison, with the contamination caveat stated.

---

### S5 — MEDIUM: ranking config (`num_parallel_tree = 1`) ≠ headline config (`= 24`); the ordering used to pick the default is not the config the default ships with.

**Evidence.** `sub_train.R` uses `num_parallel_tree = 1` "because xgboost here is single-threaded
(no OpenMP), so 24 trees/round is ~24× too slow"; `full_train.py` and the verbatim `train.R`
recipe use `num_parallel_tree = 24`. The *ranking* that selects the locked lineup was produced at
1; the *headline* numbers (and the shipped scorers) use 24.

**Why it matters.** `num_parallel_tree = 24` turns each boosting round into a 24-tree random
forest (boosted-RF). This changes the bias/variance profile and can **re-order** close
embedders: a 0.011–0.024 gap measured at np_tree=1 is not guaranteed to hold at np_tree=24,
especially for the small-dim encoders (e5-small 384-D, bge-small 384-D) where forest averaging
helps differently than for 768/1536-D. The whole point of the bake-off — picking the default —
was decided under a config the product will not use.

**Fixes.**
1. Re-rank the **top 4–5** candidates at the **production config (np_tree = 24)** on the same
   subsample via `full_train.py`-style multithreaded xgb (it already exists). You do not need all
   ten; just confirm the ordering near the decision boundary (e5-base / e5-small / bge-base /
   bge-small) is stable. Cheap, and it directly de-risks the default choice.
2. State in the writeup that ranking was done at np_tree=1 for speed and **confirmed** at
   np_tree=24 for the shortlist (once you've run it).

---

### S6 — MEDIUM: `early_stopping_rounds = 3` selects `best_iteration` on the **test** set → mild test peeking; inflates every absolute F1.

**Evidence.** Both `sub_train.R` and `full_train.py` pass the *test* DMatrix as the
early-stopping watchlist and stop on its mlogloss. `best_iter` ranges 53–83 across models.

**Why it matters.** The reported F1 is at the iteration that minimised loss **on the test set**,
i.e. the test set is used for model selection. This is a small optimistic bias and it is *not
uniform* across embedders (different best_iter each), so it can perturb the ranking slightly. It
is the reason no absolute number here should be quoted as an unbiased generalisation estimate.

**Fixes.**
1. Carve a **validation split** out of train for early stopping; keep the 3,255 test set strictly
   for final scoring. (Re-use the existing 97.5/2.5 split machinery to make a 3-way split.)
2. Or fix `nrounds` to a constant for all candidates (no early stopping) so at least the bias is
   identical across models. Either removes the peeking; the validation split is preferable.

---

### S7 — MEDIUM: multilingual claim ("~100 langs") is entirely untested by this bake-off; the test set is English-derived.

**Evidence.** Real corpora (IMDb/Amazon/sentiment140/financial/TweetSemEval) are English;
synthetic data was GPT-generated from English prompts (`gpt4o_output_topics`,
`gpt4_output_scenarios`). No language column; no non-English stratum. Yet e5-small/e5-base are
locked partly **because** they are multilingual ("~100 langs, NO TF").

**Why it matters.** The benchmark gives **zero** evidence for multilingual sentiment quality.
Shipping e5 as "multilingual" on the strength of an English F1 is an over-claim. Conversely, on a
*purely English* test, a strong English-only encoder (bge/gte-en) is arguably the fairer
default, and e5's multilinguality is a *latent* benefit you haven't measured — so the English
ranking may understate e5's real product value and overstate bge/gte's.

**Fixes.**
1. Either (a) **scope the claim**: "default chosen on an English benchmark; multilingual support
   is inherited from the e5 backbone and not independently benchmarked here," or (b) add a small
   non-English sentiment eval (e.g. a multilingual sentiment set) before claiming multilingual
   parity.
2. If multilinguality is a *product* requirement, it should be a *selection criterion stated up
   front*, not a post-hoc tiebreak — otherwise the English-only bge/gte numbers look unfairly
   competitive.

---

### S8 — LOW/MEDIUM: 130310 vs 130311 off-by-one — one labelled row silently dropped from the full-data subset.

**Evidence.** `make_full_idx.R` reports `test=3255 train=127056 total=130311`. `full_subset.log`:
`wrote bakeoff/sub_emb_oai_3_small_full.csv: 130310 rows (wanted 130311)` and same for use_lg.
One row in the wanted index has no matching embedding row and is dropped.

**Why it matters.** On its own, 1/130,311 is numerically negligible. But (a) it is a **silent**
mismatch the script merely prints, not a hard error, so a *larger* future mismatch would also pass
silently; (b) it signals an index-keying fragility (`str(int(float(i)))` coercion in
`sub_subset.py`, float↔int index coercion in `full_train.py`) that is exactly where a real
alignment bug — embedding row i paired with label of row j — would hide. An off-by-one in *which*
rows align (vs how many) would corrupt F1 without changing row counts.

**Fixes.**
1. Make the subset/merge **assert** `n_written == n_wanted` and **fail** (non-zero exit) on
   mismatch; print the missing index.
2. Add a one-line **alignment check** after every merge (already present in `train.R` via the
   `all(dt_sampled$index == rownames)` stop — replicate that guard in `full_train.py` and
   `sub_train.py`): assert the label vector and the embedding matrix agree on `index` row-for-row,
   not just on length.
3. Track down the one missing index in `all_data.csv` (likely a NA/empty-text row dropped by
   `fillna("")` vs a genuinely absent index) and document it.

---

### S9 — LOW: minor train/test duplicate-text leakage + a GPT refusal string with conflicting labels in the data.

**Evidence.** Among texts >20 chars, **125 texts are exact duplicates (130 extra rows, ~0.08%)**;
**6 have conflicting labels** across copies, and one duplicated synthetic row is the GPT refusal
string *"sorry, but i can't assist with that."* appearing with **both -1 and +1** labels. Some
real IMDb sentences appear 3×.

**Why it matters.** The leakage magnitude (~0.08%) is too small to move the ranking — not a
headline threat. But (a) exact duplicates that straddle the train/test split give a few free
correct predictions to every model equally (ranking-neutral, F1-inflating by a hair); (b) the
**refusal string is a synthetic-data hygiene failure** — GPT-4o refusals were ingested as
"neutral/sentiment" rows and mislabelled, which directly pollutes the very synthetic-neutral pool
that S1 is about. Conflicting labels on identical text are pure label noise.

**Fixes.**
1. Dedupe `all_data.csv` on normalised text **before** sampling; on label conflict, drop or
   majority-vote. Cheap, removes the cross-split duplicate leakage entirely.
2. Add a **refusal/boilerplate filter** to the synthetic-neutral pipeline (regex for "can't
   assist", "as an AI", "I cannot", empty/`{`/`\` artefacts — note 33 rows failed to parse and
   show up as `\`, `}`, `,`). This is required before publishing the synthetic-neutral finding.
3. Re-count synthetic-neutral *after* filtering; the "synthetic neutral helps" effect size must
   be re-measured on the cleaned pool.

---

### S10 — LOW: no orchestration script → reproducibility gap; provisional-vs-full mixing.

**Evidence.** No `Makefile`/`run.sh`/driver in `bakeoff/`; the per-model commands (model id, tag,
device, **and crucially the `--prefix`**) were issued by hand and survive only as log headers.
The brief mixes the provisional subsample ranking with an in-progress full-data run.

**Why it matters.** The exact command per model is the experiment. With no script, the prefix
question in S2 is unanswerable from version control, and the full run cannot be reproduced
byte-for-byte. Mixing provisional subsample F1 with eventual full-data F1 in the same headline
table (as the brief's lineup does) compares numbers produced under different train sizes.

**Fixes.**
1. Commit a single `run_bakeoff.sh` (or Make target) that encodes, per model: HF id, tag,
   device, batch, **prefix**, max_seq — the literal commands. This also fixes S2 permanently.
2. Keep **two separate tables**: "subsample ranking (18k train)" and "full-data headline (127k
   train)". Never quote a subsample number and a full-data number side by side as if comparable.

---

## What is actually fine (so the review is balanced)

- **The split is genuinely fixed and identical across candidates** (same `set.seed(42)`, same
  3,255 test rows) — the ranking is fair *as a ranking*, which is its stated purpose.
- **The xgb recipe is held constant across embedders** within each phase — embedding quality is
  reasonably isolated for the *relative* comparison.
- **USE/OAI/paraphrase reuse via `sub_subset.py`** is legitimate (those embeddings can't be
  re-made; USE's TF backend is the thing being killed) and is index-keyed, not re-embedded.
- **Macro-F1 (unweighted mean of 3 class F1s)** is the correct headline metric for a
  balanced-by-construction 3-class problem; do not switch to accuracy.

## Priority order for fixes (effort vs payoff)

1. **S3 paired bootstrap** — hours, reuses saved predictions, kills the "is the tie real" and
   "is e5>bge real" objections at once. Do first.
2. **S2 re-run e5 with `query:`** — one flag, re-embed two models; decides whether the headline
   e5 number is even correct.
3. **S1 real-only neutral eval** — the publishable finding (synthetic neutral) is not safe until
   this exists.
4. **S5 re-rank shortlist at np_tree=24** — confirms the default was chosen under shipping config.
5. **S6 validation-split early stopping**, **S8 alignment asserts**, **S9 dedupe+refusal filter**,
   **S10 driver script**, **S4 contamination caveat / held-out slice**, **S7 multilingual scoping**.

## One-line honesty rewrites for NEWS/docs (until the above are done)

- Not: "e5-base ties OpenAI (0.860 vs 0.861)." → "On this English benchmark, e5-base is
  statistically indistinguishable from OpenAI 3-small (paired bootstrap 95% CI on ΔF1 includes 0);
  absolute F1 is an upper bound (partially-public corpora, balanced neutral is ~93% synthetic)."
- Not: "e5-base > bge-base." → "e5-base ranks above bge-base by ~0.024; pending CI confirmation."
- Not: "multilingual, ~100 langs" as a benchmarked claim → "multilingual backbone (e5); the
  benchmark is English-only, multilingual sentiment quality is not independently measured here."
