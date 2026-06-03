# Real-neutral evaluation plan (addresses review S1 + S7)

Author: planning pass for sentiment.ai v2.
Date: 2026-06-03.
Status: design spec. Writes only to `planning/`; does not touch `R/` source or the
bake-off harness — it specifies new harness scripts/data the main thread can add.

> **Why this exists.** The methodology review's **S1 (CRITICAL)** showed the balanced
> test set's neutral class is **40,576 / 43,437 = 93.4% GPT-synthetic**, only **2,861
> real** neutral rows. So the published **F1(0)** measures "recognise GPT-4o's neutral
> register", not neutral *sentiment*. The "synthetic-neutral helps pos/neg" finding is
> not safe to publish until F1(0) is measured on **natural** neutral text. This plan
> builds that real-only neutral evaluation. It **also** lands the **S7 (multilingual)**
> fix for free: SemEval-2017 Task 4 ships in five languages, so the same protocol
> produces the first non-English sentiment evidence the lineup currently lacks.

---

## The two external datasets (+ the in-domain reals) and why

### 1. Financial PhraseBank — Malo et al. 2014 — `financial_phrasebank`

- **HF id:** `financial_phrasebank` (also mirrored as `takala/financial_phrasebank`).
- **Configs (pick by annotator agreement):** `sentences_50agree` (4,846 rows),
  `sentences_66agree`, `sentences_75agree`, `sentences_allagree` (2,264 rows, unanimous).
  **Use `sentences_75agree` as the headline real-neutral slice** (high label confidence,
  still ~3.4k rows); report `sentences_allagree` as a sensitivity check.
- **Label field:** `label` ∈ `{0,1,2}` with the dataset's own mapping
  **`0 = negative, 1 = neutral, 2 = positive`** (verify against `dataset.features["label"]`
  at load time — do **not** hard-code without the assert; see "label mapping" below).
- **Why this dataset.** It is the canonical *real* finance-news neutral corpus. Financial
  statements are overwhelmingly **factual/neutral** by nature ("the company will open a
  plant in Q3"), so it is the densest source of *natural* neutral sentences in NLP — the
  exact construct the 93%-synthetic neutral class fails to measure. It is human-annotated
  by 5–8 finance-literate annotators, English, single-sentence (matches our sentence-level
  scoring), and is **already named as an in-domain source** in the roadmap ("financial
  news"), so it is distribution-adjacent, not an alien domain.
- **Contamination caveat (ties to review S4).** PhraseBank is on HF and in web crawls, so a
  modern encoder may have seen it. That is *acceptable here* because this slice's job is to
  test **natural-neutral transfer relative to the synthetic-neutral number**, and the
  contamination hits every candidate embedder roughly equally. Report it as an upper bound,
  same caveat language as S4. (The truly-unseen requirement of S4 is a *separate* slice,
  out of scope for this neutral-specific plan.)

### 2. SemEval-2017 Task 4 — Rosenthal, Farra & Nakov 2017 — `sem_eval_2017`-family

- **HF id (primary):** `sem_eval_2017` — Task 4 subtask A (message-level 3-class:
  positive / neutral / negative), the standard "tweet sentiment" benchmark.
- **Multilingual coverage (the S7 payload):** SemEval-2017 Task 4A is released in **English
  + Arabic**; the broader multilingual sentiment line is filled by **SemEval-2017 Task 4
  Arabic** plus per-language community mirrors. To get clean non-English neutral without
  relying on a flaky single mirror, **pair it with the `cardiffnlp/tweet_sentiment_multilingual`
  HF dataset** (8 languages: English, Arabic, French, German, Hindi, Italian, Portuguese,
  Spanish; 3-class pos/neu/neg; ~1,500 test rows/language; built on the SemEval tweet line).
  This is the concrete, loadable HF artifact that delivers real neutral in 8 languages.
  - **Primary loadable id for the multilingual claim:** `cardiffnlp/tweet_sentiment_multilingual`,
    config `all` (or per-language: `english`, `arabic`, `french`, `german`, `hindi`,
    `italian`, `portuguese`, `spanish`), split `test`.
  - **English-SemEval anchor:** `cardiffnlp/tweet_eval`, config `sentiment` (the cleaned,
    de-duplicated SemEval-2017 sentiment subset, `test` = 12,284 rows) — use this as the
    English real-neutral tweet slice; it is the most reliable HF packaging of SemEval-2017
    sentiment and avoids the `sem_eval_2017` loader-script fragility.
- **Label field:** in `tweet_eval`/`tweet_sentiment_multilingual` the `label` is
  **`0 = negative, 1 = neutral, 2 = positive`** (assert against `features["label"]`).
- **Why this dataset.** (a) It supplies **real social-media neutral** — a *different* natural
  neutral register from finance (informal, noisy, short), so finance + tweets bracket the
  neutral construct rather than over-fitting one domain. (b) It is the **multilingual** real
  neutral source, which is the only way to put evidence under the locked e5 "multilingual"
  selling point that the English bake-off cannot support (S7). (c) Tweets match a core
  sentiment.ai use case (the existing `airline_tweets` data, TweetSemEval already in train).

### 3. In-domain reals — the 2,861 held-out real neutral rows (already on disk)

- **Source:** `training_data/all_data.csv`, rows with `sentiment_score == 0 & is_synthetic == FALSE`
  (the review counted exactly **2,861**). Add the real pos/neg test rows already in the fixed
  3,255-row test split so all three class F1s are computable on in-domain natural text.
- **Why.** This is the *distribution-matched* real-neutral control: same corpora, same
  preprocessing, no domain shift. It is the bridge between "our benchmark" and the two external
  sets — if F1(0) holds on in-domain reals but drops on PhraseBank/tweets, that is **domain
  shift**; if it drops on *all* real sets vs the synthetic number, that is the **synthetic-style
  artefact** S1 predicts. The three slices together let you attribute the gap.

**Return summary (the two external datasets + why):**

| dataset | HF id / config | real neutral register | role |
|---|---|---|---|
| Financial PhraseBank | `financial_phrasebank` / `sentences_75agree` (hl), `sentences_allagree` (sens.) | formal finance-news, factual → dense natural neutral | headline real-neutral slice; in-domain-adjacent |
| SemEval-2017 Task 4 (tweets) | English: `cardiffnlp/tweet_eval` / `sentiment` · multilingual: `cardiffnlp/tweet_sentiment_multilingual` / 8 langs | informal social-media; **8 languages** | 2nd-register neutral + the S7 multilingual evidence |

---

## Label mapping (do not hard-code blind)

Both external datasets and our internal data must land on the harness convention used by
`rigor.py` / `full_train.py`: internal `sentiment_score ∈ {-1, 0, 1}` → model class via
`YMAP = {-1: 0, 0: 1, 1: 2}` (so model-class **1 is neutral**, the class we report as F1(0)).

| source | native label | → internal `sentiment_score` | → model class (YMAP) |
|---|---|---|---|
| Financial PhraseBank | 0 negative | −1 | 0 |
|  | 1 neutral | 0 | **1 (F1(0))** |
|  | 2 positive | +1 | 2 |
| tweet_eval / tweet_sentiment_multilingual | 0 negative | −1 | 0 |
|  | 1 neutral | 0 | **1 (F1(0))** |
|  | 2 positive | +1 | 2 |
| in-domain reals (`all_data.csv`) | already `-1/0/1` | (identity) | YMAP |

**Mandatory guard (prevents a silent sign/permutation flip — the S8 class of bug):**
```python
feat = ds.features["label"]            # HF ClassLabel
names = [feat.int2str(i).lower() for i in range(feat.num_classes)]
assert names == ["negative", "neutral", "positive"], f"unexpected label order: {names}"
```
PhraseBank's `ClassLabel` names are `negative/neutral/positive` in that index order; if a
future mirror reorders them, the assert fails loudly instead of corrupting F1. **Never** map
by position without this check.

---

## Held-out eval protocol

**Core principle: these external sets are EVALUATION-ONLY. The trained scorer never sees a
single PhraseBank or SemEval row in training.** The scorer is trained exactly as shipped
(on `all_data.csv`, the existing `train` split, `num_parallel_tree=24`, the `rigor.py` recipe).
We only swap what we *score*.

### Step 0 — Freeze the scorer (no retraining for the external sets)
Use the production scorer from the shipping config: train xgb once per embedder on the
existing `train` split with `rigor.py`'s params (`max_depth=5, eta=0.3, subsample=0.5,
alpha=1e-5, num_parallel_tree=24, nthread=0, seed=7, num_boost_round=100`, **no early
stopping** — keeps the test/external sets out of model selection, the S6 fix). Persist
`bst` per embedder so every external slice is scored by the identical model.

### Step 1 — Embed each eval slice with the SAME embedder + prefix as the scorer was trained with
Reuse `bakeoff/sub_embed.py` unchanged. For the e5 default this is the load-bearing line —
**the `query: ` prefix MUST be applied here and in training identically** (review S2). Example
commands (new texts CSVs, `index,text` schema like `sub_texts.csv`):

```bash
# Financial PhraseBank, 75-agree, e5-small DEFAULT (note --prefix "query: ")
python3 bakeoff/sub_embed.py --model intfloat/multilingual-e5-small \
  --tag eval_fpb75_me5_small --texts bakeoff/eval_fpb75_texts.csv --prefix "query: "

# SemEval English (tweet_eval sentiment test), e5-small
python3 bakeoff/sub_embed.py --model intfloat/multilingual-e5-small \
  --tag eval_semeval_en_me5_small --texts bakeoff/eval_semeval_en_texts.csv --prefix "query: "

# SemEval multilingual, per language, e5-small (S7) — loop over fr/de/es/it/pt/ar/hi
python3 bakeoff/sub_embed.py --model intfloat/multilingual-e5-small \
  --tag eval_semeval_fr_me5_small --texts bakeoff/eval_semeval_fr_texts.csv --prefix "query: "

# In-domain real neutral + real pos/neg test rows
python3 bakeoff/sub_embed.py --model intfloat/multilingual-e5-small \
  --tag eval_indomain_real_me5_small --texts bakeoff/eval_indomain_real_texts.csv --prefix "query: "
```
For the e5-base "ties OpenAI" decoupling, repeat with `--model intfloat/multilingual-e5-base`.
For the OpenAI reference column, embed the same slices via the existing OpenAI path (no prefix).

### Step 2 — Score and report F1(0) + macro, per slice
A new eval script (`bakeoff/eval_real_neutral.py`, sibling to `rigor.py`) that:
1. loads the frozen `bst` for the embedder,
2. loads `bakeoff/sub_emb_eval_<slice>_<embtag>.csv` + the slice's label vector,
3. predicts `bst.predict(DMatrix(X)).argmax(1)`,
4. reports **per-class F1 with `labels=[0,1,2]`** (so `f1[1]` is the headline **F1(0)**),
   **macro-F1** (`average="macro"`), and **n per class**.

```python
from sklearn.metrics import f1_score, confusion_matrix
f1 = f1_score(y, pred, average=None, labels=[0,1,2])   # f1[1] == F1(neutral) == F1(0)
print(f"{slice}: F1(0)={f1[1]:.3f}  macro={f1.mean():.4f}  "
      f"neg={f1[0]:.3f} pos={f1[2]:.3f}  n={np.bincount(y, minlength=3)}")
print(confusion_matrix(y, pred, labels=[0,1,2]))       # neutral row shows neu->pos/neg leak
```
The **confusion matrix is non-optional** for the neutral story: it shows *where* misclassified
neutral goes (neutral→positive leak is the classic synthetic-style failure), which a scalar F1
hides.

### Step 3 — The headline comparison (this is the publishable result)
For each embedder, report **F1(0) three ways**, side by side:

| slice | neutral source | what it isolates |
|---|---|---|
| (a) balanced bake-off test | ~93% synthetic | the *current published* F1(0) — the inflated number |
| (b) in-domain reals (2,861) | real, same corpora | natural neutral, no domain shift |
| (c) Financial PhraseBank 75 | real, finance | natural neutral, mild domain shift |
| (d) SemEval English tweets | real, social | natural neutral, register shift |

**The gap (a) − (b)** is the synthetic-style inflation. **(b) vs (c)/(d)** is domain
transfer. Per S1's fix #2: *the gap between (a) and the real slices is itself the result.*
Publish it as: "F1(0) is X on the balanced (93%-synthetic) set but Y on real neutral
(PhraseBank/tweets); the synthetic-neutral metric is an upper bound."

### Step 4 — Confidence intervals (carry S3's method onto the external sets)
Reuse `rigor.py`'s paired-bootstrap block (B=2000, `rng = default_rng(7)`) **per external
slice**: resample the slice's rows with replacement, recompute F1(0) and macro per embedder,
report 2.5/97.5 percentile CIs, and the paired ΔF1(0) for e5-small−OpenAI and e5-base−OpenAI.
Mandatory because PhraseBank-75 (~3.4k) and per-language tweet slices (~1.5k) are **small** —
a scalar F1(0) there without a CI is exactly the single-point overclaim S3 warns against.

### Step 5 — The synthetic-neutral ablation, now honest (closes S1 fix #3)
`rigor.py` already runs the WITH/WITHOUT-synthetic-neutral training ablation but its caveat
notes the arms differ in neutral *count*, not just synthetic-ness — so it cannot cleanly
attribute the effect. **This plan resolves that:** run the ablation, then **evaluate both
arms on the external real-neutral slices (b)/(c)/(d)**, where neutral is 100% real and
count-fixed across arms. If "train with synthetic neutral" still improves **real** pos/neg F1
*and* real neutral F1, the synthetic-neutral finding is defensible; if it only helps the
synthetic test set, it is reframed as a style artefact (S1's required reframe).

---

## How this serves the multilingual claim (review S7)

The bake-off is English-only, so shipping e5 as "multilingual" is currently an over-claim.
This plan converts the SemEval slice into the first real multilingual sentiment evidence:

1. **Per-language real-neutral eval.** Run Steps 1–4 on `cardiffnlp/tweet_sentiment_multilingual`
   for each of its 8 languages (en, ar, fr, de, hi, it, pt, es), embedding with the **same
   e5-small/e5-base + `query: ` prefix** (e5 is genuinely multilingual; the prefix is
   language-agnostic and mandatory in all languages per the model card). Report
   **F1(0) and macro-F1 per language** — a single table.
2. **The fair-comparison angle (S7's deeper point).** Also embed the non-English slices with
   an **English-only encoder** (e.g. `bge-base-en-v1.5`) as a contrast column. e5 should hold
   up across languages while the English-only encoder degrades on non-English — *that* is the
   measured justification for the e5 multilingual default, replacing the post-hoc tiebreak the
   review flagged. If e5 does **not** beat the English encoder on non-English neutral, the
   multilingual claim must be scoped down, not asserted.
3. **Honest scoping language until the table exists** (from the review's rewrites): *"Default
   chosen on an English benchmark; multilingual support is inherited from the e5 backbone. We
   independently evaluate real-neutral F1 in {N} languages on SemEval-2017-derived tweet data
   (`cardiffnlp/tweet_sentiment_multilingual`); see the per-language table."* Once the table is
   produced, replace with the measured per-language numbers.

This makes one eval pipeline do double duty: it kills the S1 synthetic-neutral contamination
*and* supplies the S7 multilingual evidence, because the real neutral we need is the same
multilingual tweet data either way.

---

## Deliverables for the main thread to add (planning-only spec — not implemented here)

- `bakeoff/build_eval_slices.py` — loads the three HF datasets via `datasets.load_dataset`,
  runs the **label-name assert**, maps to internal `-1/0/1`, writes one `index,text` +
  `index,sentiment_score,lang` CSV pair per slice (PhraseBank-75, PhraseBank-allagree,
  tweet_eval-en, tweet_sentiment_multilingual ×8, in-domain-reals).
- Embed each slice with `bakeoff/sub_embed.py` (e5-small + `query: `; e5-base; OpenAI; one
  English-only contrast encoder) — commands above; this also closes S10 if the literal
  commands are committed in a `run_real_neutral_eval.sh`.
- `bakeoff/eval_real_neutral.py` — frozen-scorer scoring, per-slice F1(0)/macro/confusion +
  paired-bootstrap CIs (Steps 2–4), reusing `rigor.py`'s bootstrap block.
- Reporting tables: (i) F1(0) three/four ways per embedder, (ii) per-language multilingual
  F1(0)/macro for e5 vs an English-only encoder.

## What this plan deliberately does NOT do
- It does **not** add PhraseBank/SemEval to **training** — they are held-out eval only, so no
  in-domain leakage and the "natural-neutral transfer" reading stays valid.
- It does **not** re-open the embedder ranking — it inherits the locked lineup and only
  re-measures the neutral class honestly.
- It does **not** claim a truly-unseen/post-cutoff slice (S4) — PhraseBank/tweets are public;
  numbers are reported as upper bounds with the S4 caveat.
