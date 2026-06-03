# Better sentiment.ai — Lens: modern NLP / state of the art

> Scope: NOT the v2 TF→no-TF migration (assume done). Question: where does an
> embed + light-scorer (xgboost/glm on a frozen sentence embedding → score in
> [-1,1]) sentiment tool sit in 2026 vs the modern stack, and what is genuinely
> worth adopting **without breaking the design or the brand**.
>
> Brand constraint (load-bearing, not decoration): Linnet sells *trustworthy,
> computed-not-generated, auditable* measurement. The differentiator is
> honesty/rigour, not chasing the last F1 point. Every recommendation below is
> filtered through "does this make the tool more *defensible and inspectable*?"
> not just "more accurate." Where a method would trade auditability for a
> fractional accuracy gain, I say so and recommend against it.
>
> Written 2026-06-03. All datasets/models/packages named are real and findable;
> no benchmark numbers are invented here — the only numbers quoted are the ones
> already measured in this project's own `v2-roadmap.md` / training repo, clearly
> attributed as such.

---

## 0. The honest map: where embed+xgb wins and where it loses

The design is three frozen stages: `text → frozen sentence embedding → light
supervised head (xgb/glm) → [-1,1]`. This is a real, named architecture — it is
"linear/shallow probe on frozen sentence representations" (cf. the SentEval
probing tradition, Conneau & Kiela 2018, *SentEval: An Evaluation Toolkit for
Universal Sentence Representations*, LREC). Knowing that is useful because the
literature already tells us exactly what this class of model is good and bad at.

**Where embed + light scorer genuinely WINS (and the competition can't easily
match):**

- **No fine-tuning, no GPU to *use* it.** A fine-tuned transformer head
  (cardiffnlp/twitter-roberta-base-sentiment-latest, BERTweet, DeBERTa-v3 SST)
  needs the full transformer forward pass at inference and is painful to retrain
  on a user's own domain without a training loop + GPU. Here the head is a 1ms
  xgboost predict over a 768-d vector. **Retraining on a customer's own labels
  is a `xgboost::xgb.train()` call, not an ML pipeline.** That is the moat.
- **Tunability / domain adaptation by ordinary R users.** This is the thing the
  transformer-head world is *bad* at for non-ML users. A team with 2,000 labelled
  support tickets can refit the head in seconds. cardiffnlp can't be domain-adapted
  by an R analyst without HuggingFace `Trainer`, CUDA, and a weekend.
- **Anchor-based, label-free scoring** (`sentiment_match()`): define your own
  positive/negative anchor phrases, score by cosine to anchors. This is a *real*
  technique — it is exactly zero-shot classification by label-embedding similarity
  (the "SimCSE / label-as-anchor" idea; cf. the `zero-shot via NLI` and
  `embedding-anchor` families). It generalises to *any* axis the user can name
  (formality, urgency, churn-risk), not just sentiment. Transformer-head models
  are welded to their training labels.
- **Speed & on-device.** CPU, batchable, no per-call API cost, deterministic.
  LLM-as-judge is ~100–1000× slower and costs money per row.
- **Auditability.** The head is a logistic regression (glm) or a gradient-boosted
  tree you can `xgb.importance()` / SHAP. You can show *which embedding dimensions*
  drove a score. An LLM judge is a black box that can't be re-derived. This is the
  single biggest brand-fit advantage and it is under-exploited today (see §4, §7).

**Where embed + light scorer genuinely LOSES (be honest in the docs):**

- **Ceiling on hard, compositional cases.** A frozen sentence embedding throws
  away token-level structure. Negation scope ("not bad", "I can't say I didn't
  enjoy it"), contrastive discourse ("the food was great but the service ruined
  it"), and sarcasm are *exactly* the cases where a fine-tuned token-level
  transformer (which keeps per-token attention) wins. The literature is clear that
  sentence-embedding probes underperform fine-tuned heads on SST-2-hard / negation
  subsets. **Do not claim parity on these; claim parity on aggregate.**
- **Single-axis collapse.** One score in [-1,1] cannot represent *mixed* sentiment
  ("great product, terrible support"). The true label there is "both," and a scalar
  forces an average that is wrong for both aspects. This is the biggest *expressive*
  gap vs the modern stack (ABSA, multi-label emotion). See §2.
- **No aspect/target.** "The screen is gorgeous but the battery is dreadful" — embed+head
  gives one number; the customer wants per-aspect. ABSA owns this and the gap is real.
- **Domain drift in the frozen space.** The embedding is frozen, so if the customer's
  domain is far from the embedder's pretraining (clinical notes, legal, low-resource
  language), the *representation* is weak and no amount of head-tuning fully fixes it.
  Fine-tuning the encoder would, but that's off-design.

The strategic read: **don't try to win the hard-compositional fight with the same
scalar head.** Win by being the *honest, tunable, auditable, multi-signal* tool —
add the axes the scalar can't express (mixed-sentiment flag, intensity, calibrated
confidence, emotion) using the *same frozen-embedding + light-head* recipe, and be
candid in the docs about the negation/sarcasm ceiling. That is both more useful AND
more on-brand than chasing cardiffnlp's number.

---

## 1. WORTH ADOPTING — compatible with embed + light-scorer

These all reuse the existing recipe (frozen embedding → cheap head, or cheap
post-hoc transform of the score). None require fine-tuning the encoder. Ordered
by value × brand-fit.

### 1.1 Calibration of the score — HIGH value, perfect brand-fit, cheap
**Problem today:** `sentiment_score()` returns `(prob - .5)*2`, i.e. a raw,
*uncalibrated* model output linearly rescaled to [-1,1]. xgboost probabilities are
not calibrated, and a linear rescale of a 3-class problem to a scalar is doubly
suspect. A user who reads `-0.9` as "90% confident negative" is being misled, and
"honest measurement" is the brand. **This is the highest-leverage, lowest-effort,
most on-brand fix on this list.**

**What to adopt (all real, all in R):**
- **Calibrate the probabilities** with Platt scaling (logistic) or isotonic
  regression (`stats::isoreg`, or `CORElearn`/`betacal`) fit on a held-out set, so
  `0.8` means "≈80% of items scored 0.8 are actually positive." Standard refs:
  Platt 1999; Zadrozny & Elkan 2002 (*Transforming classifier scores into accurate
  multiclass probability estimates*, KDD); Niculescu-Mizil & Caruana 2005 (*Predicting
  Good Probabilities with Supervised Learning*, ICML — the canonical "boosted trees
  are poorly calibrated, fix with Platt/isotonic" paper, directly applicable to the
  xgb head).
- **Report a calibration curve / reliability diagram** in the benchmark vignette
  (already a planned docs page — fold it in). Expected Calibration Error (ECE,
  Guo et al. 2017, *On Calibration of Modern Neural Networks*, ICML) is the one
  number to publish. Being the R sentiment package that *publishes its calibration*
  is a credibility line nobody else has.
- **Return calibrated confidence alongside the score.** A `sentiment_score(...,
  return = "full")` that yields `score`, `confidence` (calibrated max-class prob),
  and the 3 class probs. This is strictly more honest than a bare scalar.

Why it's on-design: the calibrator is a tiny fitted transform applied *after* the
existing head. No encoder change. Pure R. Auditable. **Adopt first.**

### 1.2 Make the 3-class structure first-class (expose neutral & abstention) — HIGH value
The training models are already 3-class (-1/0/1) and there's a real, publishable
finding that synthetic-neutral data *raises* pos/neg F1 (per `v2-roadmap.md`). But
the public API collapses to one scalar. Two cheap wins:
- **Return the neutral probability**, don't hide it. A 0.0 score is ambiguous today
  ("genuinely neutral" vs "model is unsure / mixed"). The 3-class probs disambiguate.
- **Abstention / "uncertain" band.** Offer `sentiment_score(..., abstain = TRUE)`
  that returns `NA`/"uncertain" when no class clears a calibrated threshold. This is
  *selective prediction* (Geifman & El-Yaniv 2017, *Selective Classification for Deep
  Neural Networks*, NeurIPS; El-Yaniv & Wiener 2010). For a trust-first brand,
  "the tool tells you when it doesn't know" is a feature competitors won't ship.

### 1.3 Mixed-sentiment detection — HIGH value, directly fixes the scalar's worst gap
The single biggest expressive failure of one scalar is mixed sentiment. You can
*detect* it cheaply without ABSA machinery:
- The 3-class head already gives you the signal: high P(pos) **and** high P(neg)
  simultaneously ⇒ flag `mixed`. (Low-entropy → confident single class; high pos∧neg
  mass → genuinely mixed, distinct from "neutral" which is mass on the 0 class.)
- Optionally a dedicated light head trained to predict a `mixed` label using existing
  multi-label corpora that mark it (see §2 GoEmotions / SemEval multi-label).
- This is grounded: mixed/ambivalent sentiment is a recognised construct (SemEval-2007
  Task 14 "Affective Text"; the "mixed" class in several review corpora). Shipping a
  `mixed` flag is *more honest* than averaging to a misleading midpoint, and it's the
  thing reviewers of a single-scalar tool will (rightly) attack — pre-empt it.

### 1.4 Intensity / continuous valence (VAD) — MEDIUM-HIGH value, real datasets
Sentiment polarity ≠ intensity ("ok" vs "absolutely incredible" are both positive).
A second light head on the *same* embeddings can regress a continuous intensity, and
there are **real, gold datasets** to train it honestly:
- **NRC-VAD Lexicon** (Mohammad 2018, *Obtaining Reliable Human Ratings of Valence,
  Arousal, and Dominance for 20,000 English Words*, ACL) — word-level valence/arousal/
  dominance, human-rated. Use for a valence-intensity regression target.
- **SemEval-2018 Task 1 "Affect in Tweets"** (Mohammad et al.) — has a *sentiment
  intensity regression* subtask (V-reg) with gold continuous scores. This is the
  honest benchmark for an intensity head; report Pearson/Spearman against it.
- **EmoBank** (Buechel & Hahn 2017) — VAD-annotated sentences.
On-design: same frozen embeddings, a second xgb/glm regression head. Gives users
valence + arousal, which is more than a polarity scalar and grounded in cited data.

### 1.5 Emotion (categorical) — MEDIUM value, real dataset, clean reuse
Beyond pos/neg, categorical emotion (joy/anger/fear/sadness/…) is a common ask and a
trivial extension of the recipe — *one more light head on the same embeddings*:
- **GoEmotions** (Demszky et al. 2020, Google, *GoEmotions: A Dataset of Fine-Grained
  Emotions*, ACL) — 58k Reddit comments, 27 emotions + neutral, **multi-label**. Gold
  standard, permissively licensed, large.
- **SemEval-2018 Task 1 E-c** — multi-label emotion in tweets.
Train a multi-label light head (one-vs-rest xgb, or `glmnet` multinomial). Multi-label
(not single-label) is the honest choice — text carries several emotions at once. This
also feeds the `mixed` detector (§1.3) and is a clean, marketable `emotion_score()`
sibling function. Note the honesty caveat in docs: emotion from frozen embeddings is
weaker than from a fine-tuned head — publish the GoEmotions macro-F1 and don't oversell.

### 1.6 Better anchor-based scoring (`sentiment_match` 2.0) — MEDIUM-HIGH, this is the unique asset
`sentiment_match()` is the most *differentiated* thing in the package — user-defined
axes by anchor phrases. Modernise the method (all grounded):
- **Use multiple anchors per pole + centroid, and report margin.** Current cosine-to-
  nearest-phrase is brittle. Mean-pool several anchors per class (a "label centroid"),
  score by *difference* of cosine-to-pos-centroid minus cosine-to-neg-centroid. This is
  the standard label-embedding zero-shot formulation and is more stable.
- **Calibrate the cosine margin**, because raw cosine is not comparable across texts
  (the README already warns longer text → lower similarity — that's an *uncalibrated
  similarity* admission). Length/normalisation correction + a fitted monotone transform
  fixes it honestly.
- **Adopt prompt/anchor *templating*** from zero-shot NLI classification (Yin et al.
  2019, *Benchmarking Zero-shot Text Classification*, EMNLP — the hypothesis-template
  trick): "This text is about {label}" style anchors embed better than bare words.
- Position `sentiment_match()` explicitly as **general zero-shot text scoring on any
  user-named axis**, not just sentiment. That's a category-of-one feature in R.

### 1.7 Robust aggregation & document-length handling — MEDIUM value, fixes a real bug-class
Long documents are a known weak spot for sentence encoders (truncation, dilution). On-design fix:
- Sentence-split long inputs (`tokenizers::tokenize_sentences`), embed + score each,
  aggregate (max-pol, mean, or "most extreme" + variance). Report **within-document
  sentiment variance** as a *mixedness* signal (ties to §1.3). This is honest and cheap
  and addresses the README's own caveat about long-text similarity decay.

---

## 2. ASPECT-BASED SENTIMENT (ABSA) — the big real gap; adopt a *light* slice, not the whole field

ABSA is where the scalar most clearly loses, and it's a legitimate, well-defined field
(SemEval-2014 Task 4 Laptops/Restaurants is the canonical benchmark; Pontiki et al.).
Full ABSA = aspect-term extraction + aspect-category detection + aspect polarity +
opinion-term + the ACOS/quad tuple. The SOTA there is generative (T5/InstructABSA) or
span-tagging transformers — **off-design and scope-creep to chase in full.**

The honest, on-design slice worth adopting:
- **Targeted/aspect *polarity* given a user-supplied aspect term** — i.e. "score the
  sentiment of THIS text *toward* `battery`." You can do this with the *existing*
  recipe: embed the sentence conditioned on the aspect (aspect-aware template, or
  embed the aspect-bearing clause), score with the head. This is the "ATSC" (aspect-
  term sentiment classification) sub-task — the part that fits a light head.
- **Aspect *discovery* is out of scope** for the head; if wanted, lean on the anchor
  mechanism (§1.6): user names aspects as anchors, the tool routes clauses to aspects
  by similarity, then scores polarity. Honest framing: "aspect-routed sentiment," not
  "full ABSA."
- **Be explicit in docs that full ABSA (term extraction, quad tuples) is not the
  package's job.** Pointing users to the right tool is more trustworthy than a weak
  imitation. Scope discipline IS the brand.

Verdict: adopt **aspect-conditioned polarity** (small, reuses the head, big perceived
value); decline **full aspect extraction / quad** (transformer-tagging territory).

---

## 3. WHAT TO *NOT* ADOPT — scope-creep / off-brand

- **LLM-as-judge as the scorer.** Tempting for accuracy on hard cases, but it
  *violates the core brand promise*: non-deterministic, non-auditable, costs money per
  row, can't be re-derived, drifts with model updates. The whole pitch is
  "computed, not generated, auditable." An LLM judge is the opposite. The literature
  also shows LLM judges have known biases (position/verbosity/self-preference —
  Zheng et al. 2023, *Judging LLM-as-a-Judge*, NeurIPS). **Hard no for the scoring
  path.** *Narrow, honest exception:* LLM/teacher models are fine *offline* to
  generate training labels (the project already does this for synthetic neutral
  data) — that's distillation into an auditable head, not an LLM in the loop. Keep
  it strictly offline and disclosed.
- **Fine-tuning the encoder.** That's the cardiffnlp model. It abandons the "no-fine-
  tune, tunable-by-the-user, on-device" advantage and turns the package into a thin
  HuggingFace wrapper. The frozen-embedding + light-head design *is* the product;
  don't dissolve it.
- **Full generative ABSA (ACOS quads, InstructABSA).** Real field, wrong tool. §2.
- **Stance detection as a headline feature.** Stance (favor/against/none toward a
  *target/claim*, e.g. SemEval-2016 Task 6) is a *different task* from sentiment and a
  rabbit hole (target-conditioned, often needs world knowledge). The anchor mechanism
  can approximate target-stance, but don't market it as solved — note it as an
  advanced `sentiment_match()` use, nothing more.
- **Chasing the cardiffnlp/SST leaderboard number.** Re-stating the brand: a 1–2 point
  macro-F1 gain bought with a black-box transformer is a *bad* trade for this package.
  Win on calibration, tunability, auditability, multi-signal — not the scalar F1 race.

---

## 4. Honest positioning vs the modern stack (for docs / benchmark page)

A truthful one-paragraph framing, grounded, no fabricated numbers:

> sentiment.ai scores sentiment with a frozen multilingual sentence embedder and a
> small, *inspectable* gradient-boosted head. On aggregate polarity it is competitive
> with fine-tuned transformer heads and paid APIs (see benchmark page), while being
> CPU-fast, deterministic, retrainable on your own data in seconds, and able to score
> *any* axis you can describe with anchor phrases. It is **not** a token-level
> compositional model: like all sentence-embedding methods it is weaker on negation
> scope, contrastive ("great X but terrible Y") and sarcastic text — for those, a
> fine-tuned model such as cardiffnlp/twitter-roberta-base-sentiment-latest will edge
> it out, and we say so. What we add instead: calibrated confidence, an explicit
> neutral and *mixed* signal, an abstention option, and a fully auditable scorer.

Naming the competitor *as a respected reference for where they win* (not a put-down)
reads as confidence and is exactly the honest-brand move. (Aligns with the "let the
reader name the competitor / don't namecheck as a put-down" instinct — here it's a
*credit*, which is fine and disarming.)

The benchmark page should report, per backend, on **named public test sets** you can
cite (SST-2 / SST-5, SemEval-2017 Task 4 Twitter, IMDb, Amazon polarity, the airline
tweets already in the package, Financial PhraseBank for domain breadth) and include a
**calibration curve** — being the R sentiment package that publishes calibration is
the credibility kill-shot.

---

## 5. Evaluation rigor (the brand's actual battlefield)

Trust is won on *how you evaluate*, not the number. Adopt:
- **Macro-F1 AND per-class F1** (already done in training repo — surface it publicly).
  Macro, not accuracy, because sentiment sets are class-imbalanced.
- **Calibration (ECE + reliability diagram).** §1.1. Nobody in R sentiment publishes this.
- **Negation / hard-subset slice.** Report performance on a negation/contrast subset
  *separately* and honestly — this is where you lose, and disclosing it builds more
  trust than hiding it. (Use the contrast set idea: Gardner et al. 2020, *Evaluating
  Models' Local Decision Boundaries via Contrast Sets*, EMNLP.)
- **Cross-domain generalisation table.** Train-on-A / test-on-B grid across the corpora.
  Single-domain F1 is the standard overclaim; a cross-domain grid is the honest version.
- **Confidence intervals / significance** on the bake-off deltas (the migration roadmap
  already plans the bake-off — make the comparisons statistically honest, e.g.
  bootstrap CIs on macro-F1, not bare point estimates).

---

## 6. Multilingual (the v2 default is a multilingual e5) — adopt honest per-language reporting
Since the on-device default is a no-TF multilingual embedder (e5 family per the brief),
the honest move is **per-language evaluation**, not a single global number that hides
that English dominates. Grounded resources:
- **XNLI / multilingual SST translations**, and for genuinely multilingual sentiment:
  **SemEval-2017 Task 4 subtask** (Arabic + English), and language-specific review sets.
- Report which languages are *validated* vs *embedding-supported-but-untested*. "It
  embeds 100 languages" ≠ "it's accurate in 100 languages" — saying so is the brand.
The README already lists "language-specific benchmarks for multilingual model" as a
TODO; this makes it a *trust* feature, not just a checkbox.

---

## 7. The one structural upgrade that compounds with everything: a clean multi-head output object
Almost every recommendation above (calibration, neutral, mixed, intensity, emotion,
aspect, abstention) is *another cheap head or transform on the same frozen embedding*.
So the highest-ROI engineering move is to make the API return a **structured,
extensible result**, not a bare numeric vector:

```
sentiment(x) ->  data.table/tibble:
  score        # calibrated [-1,1]      (default, back-compatible scalar still available)
  confidence   # calibrated max-class prob
  p_neg p_neu p_pos
  mixed        # logical / mixedness score
  label        # neg/neu/pos/mixed/uncertain  (with abstention)
  [optional]   intensity, emotions[...], aspects[...]
```

This (a) makes the tool *honest by construction* (you can't hide neutral/mixed/
uncertainty behind a scalar), (b) lets every future head slot in without an API break,
and (c) is the "audit trail" the brand promises — each field traces to a fitted,
inspectable model. Keep the bare-scalar `sentiment_score()` for back-compat; add a
richer `sentiment()` as the recommended entry point.

---

## 8. Concrete adoption order (cheap→valuable first)
1. **Calibrate** the score + return calibrated confidence (§1.1). Publish a reliability
   diagram. Cheapest, most on-brand, biggest trust gain.
2. **Expose 3-class probs + neutral + abstention** (§1.2) and the **mixed flag** (§1.3) —
   nearly free given the head is already 3-class; fixes the scalar's worst failure.
3. **Structured output object** (§7) so 1–2 ship cleanly and everything else slots in.
4. **Intensity head** on NRC-VAD / SemEval-2018 (§1.4) and **emotion head** on GoEmotions
   (§1.5) — same recipe, real data, marketable siblings.
5. **`sentiment_match()` 2.0**: centroids, calibrated margin, NLI-style templates,
   reposition as general zero-shot axis scoring (§1.6).
6. **Aspect-conditioned polarity** (§2) — the honest slice of ABSA.
7. Throughout: **evaluation rigor** (§5) and **per-language honesty** (§6) on the
   already-planned benchmark page.

Never: LLM-as-judge in the scoring path, encoder fine-tuning, full generative ABSA,
stance-as-headline, leaderboard-chasing (§3).

---

## TOP 3 (return value)

1. **Calibrate the score and ship calibrated confidence + a reliability diagram**
   (Platt/isotonic on the held-out set; ECE per Guo 2017; Niculescu-Mizil & Caruana
   2005 directly applies to the xgb head). Cheapest, most on-brand, biggest trust win
   — today `(prob-.5)*2` is uncalibrated and a "honest measurement" brand can't ship
   that. Being the only R sentiment package that *publishes its calibration* is the
   credibility kill-shot.

2. **Make the already-3-class model honest in the API: expose neutral + a `mixed`
   flag + an abstention/"uncertain" option, behind a structured multi-head output
   object.** This directly fixes the single worst expressive failure of one scalar
   (mixed sentiment — "great product, terrible support"), is nearly free because the
   head is already 3-class, grounds "the tool tells you when it doesn't know"
   (selective prediction, Geifman & El-Yaniv 2017), and creates the extensible result
   object that every later head (intensity, emotion, aspect) slots into without an API
   break.

3. **Modernise `sentiment_match()` into a general, calibrated zero-shot axis scorer**
   (multi-anchor centroids, calibrated cosine margin, NLI-style hypothesis templates
   per Yin et al. 2019) — this is the package's category-of-one feature: user-defined
   scoring on *any* axis, label-free, on the same frozen-embedding recipe, which
   transformer-head models structurally cannot offer.
