# A better sentiment-scoring package — synthesised, prioritised roadmap

> Written 2026-06-03. Planning-only (writes to `planning/`, never touches `R/`).
> Synthesis of five lens docs: `better-lens-brand.md`, `better-lens-nlp.md`,
> `better-lens-practitioner.md`, `better-lens-psychometric.md`, `better-lens-rcraft.md`.
> Question answered: *assuming the v2 (no-TF, on-device multilingual e5) migration is done,
> what makes `sentiment.ai` a better TOOL — and which moves compound with the Linnet
> "honest, computed-not-AI, auditable" brand?*

---

## The one thing all five lenses agree on (so it's not a finding, it's a fact)

`sentiment_score()` computes a 3-class probability vector in `find_sentiment_probs()`
(`R/sentiment.R:247-285`) and then **deletes everything except the positive axis** at
`scores <- (probs - .5) * 2` (`R/sentiment.R:111`). Every lens independently arrived here:

- **brand:** the score is `scorer_v(embed_m(text))` and the provenance should be returnable.
- **nlp:** `(prob-.5)*2` is an *uncalibrated* probability rescaled — a trust violation.
- **practitioner:** "the information already exists in the scorer and is discarded… the single
  most important fact in this document."
- **psychometric:** the three zeros (neutral / mixed / uncertain) are reported identically.
- **rcraft:** the two headline functions return two different *shapes*; the rich one is unreachable.

So the spine of this roadmap is: **stop discarding the signal, calibrate it, return it in one
honest shape, and make it reproducible.** Everything else is a head or a transform on top.

---

## TABLE-STAKES — must-have to be a credible 2026 R sentiment package

These are the things whose *absence* is the reason a careful analyst bounces. None require an
encoder change; most are surfacing or one-D fits.

1. **Structured, tidy output that carries the 3-class signal.** A default return of one row per
   input with `text, sentiment[-1,1], prob_neg, prob_neu, prob_pos, class, confidence`. Unifies
   the `sentiment_score` vector vs `sentiment_match` data.table split. This is the floor: `vader`
   already returns neu/pos/neg proportions; the competitor set (sentimentr, tidytext, text) has
   converged on tidy frames. (rcraft #1, practitioner #1, nlp §7, psychometric #3.)
2. **Calibrated probabilities + a published reliability diagram + ECE.** Platt / isotonic
   (`stats::isoreg`, no new dep) / temperature scaling on a held-out set; report ECE (Guo et al.
   2017), Brier with its calibration/refinement split (Murphy 1973). A package that ships
   `(prob-.5)*2` and calls itself "honest measurement" is internally contradictory. (nlp #1,
   psychometric #1, brand.)
3. **An explicit `neutral` class and a confidence/uncertainty signal per row.** Neutral is real
   and load-bearing in HR verbatims; confidence is needed to triage human review. Nearly free —
   the head is already 3-class. (All five.)
4. **Reproducibility / provenance surface.** Return `{package_version, embedder,
   embedder_revision, scoring, scoring_version, prefix}` as an attribute or accessor, plus a
   stated determinism note. Pin scores by version so a weekly dashboard / a published paper / a
   regulated audit can reproduce the number. (brand #1, practitioner #4, rcraft runner-up.)
5. **A model card / honest "Limitations" surface in-package.** Trained-on corpora named; F1 as
   *upper bounds*; neutral is the weak class; multilingual is *inherited from e5, not
   independently benchmarked*; weaker on negation/sarcasm/contrast. Shipping your own limitations
   is itself the credibility move. (brand #4, nlp §4, practitioner.)
6. **Honest evaluation reporting: macro-F1 + per-class, not cosine-to-label, with CIs.** The
   current eval (`cosine(score, label)` on imbalanced `airline_tweets`) is not a recognised
   agreement metric. Macro-F1, weighted-κ, on *named public test sets* (SST/SST-5, SemEval-2017
   Task 4, IMDb, Financial PhraseBank), bootstrap CIs on deltas. (nlp §5, psychometric #2/#8.)
7. **Install / first-run legibility and real error conditions.** `sentiment_status()` readiness
   call; replace `cat()`-based `create_error_text()` with `cli::cli_abort()` + classed conditions
   (`sentiment_ai_not_installed`, `sentiment_ai_model_missing`); consented, progress-barred,
   cached first-run; `skip_on_cran`/`skip_if_offline`. Fix the `embed_text()`
   warning-then-silently-recover path (`R/embedding.R:187-198`). (rcraft #3.)

---

## DIFFERENTIATOR — what makes it the *trustworthy/rigorous* choice (the moat)

These are the moves a closed paid API or a lexicon package **structurally cannot copy**. This is
where to spend the brand's credibility, because each one compounds with Linnet's "auditable,
computed-not-AI" posture.

D1. **Auditability as a first-class, returnable property (the headline).** The provenance stamp
    (table-stakes #4) is the floor; the differentiator is *guaranteeing it true* via the
    prefix-as-scorer-property contract (assert embedder-prefix == scorer-prefix at score time,
    `review-lineup.md` SEV-1) so the stamp can't lie, plus pinned open weights + open scorer =
    "re-run in 2028, get the identical number." A closed API can never be reproducible or
    inspectable by definition. *This is the single sharpest line the package owns.* (brand.)

D2. **`sentiment_match()` as a calibrated, general zero-shot axis scorer + the explanation
    channel.** It is the one feature no lexicon package and no transformer-head model has:
    score *any* user-named axis (urgency, churn-risk, formality) by anchor phrases, label-free,
    and return the *nearest labelled anchor + cosine* as a built-in, deterministic, inspectable
    explanation ("scored −0.66; nearest anchor 'sad' at cos 0.15"). Modernise it: multi-anchor
    centroids, calibrated cosine margin (length-corrected), NLI-style hypothesis templates (Yin
    et al. 2019). Promote it to co-headline with scoring. (nlp #1.6, brand #2, rcraft runner-up,
    practitioner.)

D3. **"The tool tells you when it doesn't know": abstention + the three zeros separated.**
    A first-class `uncertain` output (selective prediction; El-Yaniv & Wiener 2010), tuned on a
    risk–coverage curve the package ships, plus distinguishing genuinely-neutral (high p_neu, low
    entropy) from mixed/ambivalent (high p_pos AND p_neg — bivariate affect, PANAS tradition;
    expose `pos_intensity`/`neg_intensity`, not just their difference) from model-uncertain (high
    entropy / ensemble spread). Conformal prediction sets (Angelopoulos & Bates 2023) are the
    most rigorous version and the most on-brand because the coverage guarantee is *provable*.
    (psychometric #3, nlp #1.2/#1.3, practitioner #2.)

D4. **Mixed / out-of-domain honesty — the warnings competitors won't ship.**
    (a) A `mixed` flag from the existing 3-class head (high p_pos ∧ high p_neg) + within-document
    sentiment variance from sentence-split scoring — fixes the scalar's worst failure ("loved the
    food, hated the service") which is exactly Linnet's HR-verbatim home turf.
    (b) An **out-of-domain / drift warning** at near-zero cost: each row's max cosine similarity
    to training class centroids (code already in `R/matrix_helpers.R`); low similarity = "this
    text is unlike anything the scorer was calibrated on — treat with caution." A tool that warns
    when it's out of its depth *is* the trustworthiness brand made into a feature. (practitioner
    #2/#3, nlp #1.3/#1.7.)

D5. **Agreement-with-humans + a fairness/validity argument as shipped artefacts.**
    `sentiment_agreement()` (Krippendorff α via `irr`, quadratic-weighted κ, ICC, % agreement +
    confusion matrix, Spearman — not cosine), framed against the *human–human ceiling* using
    multi-annotator corpora (GoEmotions, SemEval-2017-4, SST graded scores). A `sentiment_fairness()`
    probe on the Equity Evaluation Corpus (Kiritchenko & Mohammad 2018) / TwitterAAE — run it,
    publish whatever comes out including the bad parts. A `vignette("validity")` structured as a
    Messick/Kane validity argument (convergent vs `vader`/`sentimentr`/`syuzhet`, criterion vs
    star ratings, discriminant vs text length). The authorship (IO-psych / psychometrician) can
    write this authentically — it is the credibility signal no competitor offers. (psychometric
    #2/#5/#6, nlp §5.)

> **Why these five are the moat:** each is either *impossible for a closed API* (D1, D3 conformal,
> the offline guarantee), *structurally absent from lexicon/transformer-head tools* (D2 anchors),
> or *a rigor artefact nobody else ships* (D4 drift warning, D5 fairness/agreement/validity). They
> all reuse the existing frozen-embedding + light-head recipe — no encoder change — so the moat is
> cheap to build and expensive to copy.

---

## SCOPE-CREEP — tempting, skip or defer (and why)

| Idea | Verdict | Why |
|---|---|---|
| **LLM-as-judge in the scoring path** | **Hard no** | Non-deterministic, non-auditable, costs per row, drifts with model updates, known biases (Zheng et al. 2023). It is the *exact inverse* of the brand. Narrow allowed exception: LLM as an *offline* teacher to generate training labels (already done for synthetic neutral) — distillation into an auditable head, disclosed. |
| **Fine-tuning the encoder** (become cardiffnlp) | **No** | Abandons the no-fine-tune, user-tunable, on-device wedge and turns the package into a thin HuggingFace wrapper. The frozen-embedding + light-head design *is* the product. |
| **Full generative ABSA (ACOS quads, InstructABSA, aspect-term extraction)** | **Defer/decline** | Real field, wrong tool — span-tagging / generative transformer territory. Adopt only the honest slice: **aspect-conditioned polarity** given a user-supplied aspect (reuses the head + anchors). Point users elsewhere for full extraction; scope discipline is the brand. |
| **Chasing the SST/cardiffnlp leaderboard F1** | **No** | Accuracy is already at parity (e5-base 0.860 vs OpenAI 0.861 per `review-lineup.md`) and the methodology review forbids the unqualified headline anyway (neutral ~93% synthetic). A 1-2pt gain bought with a black box is a *bad* trade. |
| **A single invented composite ("sentiment intensity index", 0-100 mashup)** | **Hard no** | Coining a proprietary construct is the precise slop the Linnet brand is defined against. Measure one thing on a defensible basis; report a *panel* of cited reads, never one black-box number. |
| **Emotion / personality / "tone" expansion without a cited scheme** | **Conditional** | Emotion is *fine if grounded*: a multi-label head on **GoEmotions** (Demszky et al. 2020), publish macro-F1, don't oversell (frozen-embedding emotion is weaker than a fine-tuned head). Personality-from-text or ungrounded "tone" = no. |
| **Stance detection as a headline** | **Defer** | Different task from sentiment (target/claim-conditioned, needs world knowledge). The anchor mechanism can *approximate* it; note as an advanced `sentiment_match()` use, don't market as solved. |
| **VAD intensity head** | **Defer (not creep, just later)** | Legitimate and grounded (NRC-VAD, SemEval-2018 Task 1 V-reg, EmoBank) but it's a *second product axis*; ship after the core honesty surface lands. |
| **Namechecking paid APIs / imitators as a put-down** | **No** | Reads insecure (Linnet rule). Let reproducibility + publish dates talk. Crediting cardiffnlp as *where it legitimately wins* (negation/sarcasm) is fine and disarming — that's a credit, not a put-down. |

---

## Resolving the disagreements between lenses

The lenses mostly stack rather than conflict, but three tensions need a call:

1. **Aspect-level sentiment: practitioner says "killer gap," nlp/brand say "scope discipline."**
   Resolution: they're describing different sizes of the same thing. The practitioner's actual
   need is *driver/aspect-level rollups for verbatims* — which is satisfied by **aspect-conditioned
   polarity on user-supplied anchors + sentence-splitting** (the light slice nlp endorses), NOT
   full ABSA term-extraction (which nlp/brand correctly call creep). Ship the light slice; it
   closes the practitioner's gap without the scope blow-up. **No real conflict.**

2. **Conformal prediction (psychometric) vs simple calibrated-margin abstention (nlp).**
   Conformal is more rigorous and more on-brand (provable coverage), but it's heavier and adds a
   dependency. Resolution: **ship calibrated-margin / entropy abstention first** (table-stakes
   path, base R), offer **conformal as the rigorous opt-in** later. Don't let the perfect
   (conformal) block the cheap honest win (margin). The psychometric lens slightly over-indexes on
   conformal as a near-term must; it's a second-wave differentiator, not table-stakes.

3. **How much weight on fairness/validity now (psychometric) vs ship-the-output-first (rcraft/
   practitioner).** The psychometric lens is the most ambitious — agreement stats, fairness audit,
   reliability/perturbation suite, validity vignette, SE-aware aggregation. All real and valuable,
   but it's a *research programme*, not a v2.1. Resolution: **the output-shape + calibration +
   provenance trio ships first** (it's the precondition for *everything* the psychometric lens
   wants — you can't report agreement on a scalar you've thrown the probabilities away from), then
   agreement + fairness as the flagship differentiator artefacts. Sequenced, not dropped.

### Where a lens over-reached (call-outs)

- **psychometric** over-reached on *breadth-as-near-term*: 8 numbered items + an appendix is a
  multi-quarter agenda presented at one altitude. The calibration/agreement/abstention core (its
  own top-3) is gold and on-brand; the perturbation suite (#4), SE-aware aggregation (#7), and the
  full fairness battery (#5) are second-wave. Keep the top-3, defer the rest. Also: conformal as
  default is heavier than the brand needs on day one (see disagreement #2).
- **nlp** over-reached slightly on the **intensity (VAD) and emotion heads** as near-term — they're
  sound and grounded, but they're *new product axes* that dilute focus before the core honesty
  surface (output + calibration + provenance) exists. Correctly self-flags ABSA/LLM/fine-tune as
  no-go; that part is well-disciplined. Defer the extra heads.
- **practitioner** over-reached on **throughput primitives** (caching/dedup, resumable batch,
  pre-flight cost estimate) as a top-tier blocker. Real, and the OpenAI-bill-surprise risk is true,
  but most adoption is won on *output shape and honesty* (its own #1/#2), and throughput is
  classic engineering that doesn't compound with the rigor brand. Medium priority, not headline.
- **brand** is the most disciplined and is the correct organising spine (auditability over
  accuracy); its only risk is staying at the positioning altitude — it needs the other lenses'
  concrete functions (calibration, structured output, agreement) to become a *tool* and not a
  tagline. The synthesis below grounds it.
- **rcraft** is the most shippable and correctly identifies the return contract as the root; its
  only blind spot is treating the tidy-output fix as mostly ergonomic when it's *also* the
  precondition for the entire psychometric/honesty programme. Reframed here as load-bearing, not
  cosmetic.

---

## THE 3-5 HIGHEST-LEVERAGE MOVES

Ordered by leverage × brand-compounding. Effort tags: **quick** (days), **medium** (1-2 wk),
**deep** (multi-week / research). Each favours moves that turn the rigor brand from tagline into a
property of the API.

### Move 1 — Ship the structured tidy output that stops discarding the 3-class signal. **[quick]**
Default return of one tidy row per input: `text, sentiment[-1,1], prob_neg, prob_neu, prob_pos,
class, confidence`, `NA` rows preserved positionally, `class` an ordered factor. Add
`output = c("tibble","vector")` (or a new `sentiment()` verb) for back-compat; make the rich tibble
the example-first path.
**Why highest:** it's the move *every lens* names, it's nearly free (the probs already exist —
delete one collapsing line), and it is the **precondition for every differentiator** — you cannot
calibrate, abstain, detect-mixed, report agreement, or explain on a scalar you've thrown away. It
simultaneously closes the `vader` 3-class gap and the tidyverse-fit gap. Maximum compounding per
unit of work in the entire roadmap.

### Move 2 — Calibrate the probabilities and publish the reliability diagram + ECE. **[quick→medium]**
`calibrate_sentiment(method = c("isotonic","platt","temperature"))` (isotonic = base `stats::isoreg`,
zero new dep), calibrated probs flow into Move 1's output, and `reliability_report()` ships ECE
(Guo 2017), Brier + decomposition (Murphy 1973), reliability diagram with Wilson intervals.
**Why:** `(prob-.5)*2` is an uncalibrated output dressed as a probability — the *one number an
"honest measurement" brand cannot ship*. Calibration is the cheapest rigorous-thing-done-right that
exists, and "the only R sentiment package that publishes its calibration" is a credibility line no
competitor has. Compounds directly with the brand and unlocks honest confidence, abstention bands,
and any "X% positive" copy.

### Move 3 — Make auditability a returnable property: provenance stamp + prefix-contract guarantee. **[quick]**
A `sentiment_provenance()` accessor / `attr(scores,"provenance")` returning `{package_version,
embedder, embedder_revision, scoring, scoring_version, prefix}` — a one-call stamp to paste into a
methods section — *plus* the prefix-as-scorer-property assertion (embedder-prefix == scorer-prefix
at score time, `review-lineup.md` SEV-1) so the stamp is *guaranteed true*, not aspirational.
**Why:** this is the single sharpest differentiator the architecture gives for free and that a
closed API *structurally cannot match* — "re-run in 2028, identical number." It is the Linnet
"auditable, computed-not-AI" brand expressed in the smallest possible unit (a single reproducible
number), and it's the shared idiom the companion `textgraph` package inherits so a sentiment read
and a coherence read in the same study are provably on the same vectors. Tiny surface, maximal
brand compounding.

### Move 4 — The "knows its limits" trio: mixed flag + out-of-domain warning + abstention. **[medium]**
From the now-exposed 3-class head and existing cosine code: a `mixed` flag (high p_pos ∧ p_neg) +
within-document variance from sentence-split scoring; an **OOD/drift warning** (max cosine to
training centroids — code already in `R/matrix_helpers.R`); and a first-class `uncertain` output
tuned on a shipped risk–coverage curve (El-Yaniv & Wiener 2010). Separate the three zeros
(neutral / mixed / model-uncertain).
**Why:** this is "the tool tells you when it doesn't know and when it's out of its depth" — three
warnings *no competitor ships*, each almost free given Moves 1-2, each fixing a real failure
(bimodal verbatims, silent domain drift, false confidence) that is exactly Linnet's HR home turf.
It converts trustworthiness from slogan to feature. (Defer conformal sets to the rigorous opt-in.)

### Move 5 — `sentiment_agreement()` + a fairness probe, against the human ceiling. **[medium→deep]**
Replace `cosine(score,label)` as the headline metric with `sentiment_agreement()` (Krippendorff α,
quadratic-weighted κ, ICC, % agreement + confusion matrix, Spearman; `Suggests: irr, psych`),
framed against *human–human* agreement on multi-annotator corpora (GoEmotions, SemEval-2017-4, SST).
Add `sentiment_fairness()` on the Equity Evaluation Corpus / TwitterAAE — run it, publish the result
including the bad parts.
**Why:** it converts "competitive with paid APIs" from a cosine number into the vocabulary a
reviewer / regulator / sceptical buyer already trusts, and the human-ceiling reframing ("κ=0.55
where humans reach 0.60 is excellent") is a genuinely differentiating honest move. The authorship
can write the validity argument authentically. This is the deepest move and the one most likely to
become a publishable asset — sequence it after the core honesty surface (Moves 1-3) exists, because
it *needs* the exposed, calibrated probabilities to compute on.

---

## Sequencing in one line

**Moves 1→2→3 (all quick, the honesty core) ship together as the v2.1 "auditable output" release —
they unblock everything. Move 4 (knows-its-limits) is the next release and the first true
differentiator users feel. Move 5 (agreement + fairness) is the flagship rigor artefact and the
publication pipeline. Throughout: model card, honest macro-F1 benchmark page, and cli-grade
install/errors as the table-stakes hygiene that ships alongside.** Defer: VAD/emotion heads,
conformal, throughput primitives, aspect-conditioned polarity (the honest ABSA slice). Never:
LLM-in-scoring, encoder fine-tune, full ABSA, invented composite index, leaderboard-chasing.
