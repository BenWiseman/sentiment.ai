# Better `sentiment.ai`: the measurement-validity / psychometrician lens

*Scope: not the v2/TF-drop migration (assume done). The question is what would make this a better **tool** — specifically, what turns a sentiment score from a vibe into a **credible instrument**. Lens: treat each score as a measurement with a standard error, a calibration, an inter-rater agreement story, and documented validity/fairness evidence.*

*Grounding note: everything below references real methods, real R packages, and real public datasets. No fabricated benchmarks or citations. Where I name a number, it's a definition (e.g. ECE) or a dataset property, never a claimed result for this package.*

---

## The core problem with the current tool, stated psychometrically

`sentiment_score()` returns a single bare `numeric` in `[-1, 1]`:

```r
probs  <- find_sentiment_probs(...)   # xgb: predict(); glm: 1/(1+exp(-Xw))
scores <- (probs - .5) * 2            # affine rescale of P(positive)
```

Three things are wrong with this *as a measurement*, independent of accuracy:

1. **The score is `2·P̂(pos) − 1`, but `P̂` is an uncalibrated xgboost/logistic output.** A raw gradient-boosted probability is not a probability — it's a monotone score that *looks* like one. So `0.4` does not mean "40% likely positive," and two texts both scoring `0.4` are not interchangeable. The brand promise is *trustworthy, computed-not-hyped measurement*; an uncalibrated probability silently breaks that promise. This is the single highest-leverage fix.

2. **A 3-class construct (neg/neu/pos) is being collapsed onto one bipolar axis, which destroys the most important distinction in the field: genuinely-neutral vs. mixed/ambivalent vs. model-uncertain.** A score near `0` can mean (a) the text is affectively flat ("the meeting is at 3pm"), (b) the text is *both* strongly positive and strongly negative ("loved the food, hated the service"), or (c) the model has no idea. These are three different measurement states and the tool reports them identically. The default anchor list (`data/default.rda`) is literally `positive`/`negative` only — 468/470 phrases, **no neutral anchor** — so even `sentiment_match()` cannot represent neutrality as a thing, only as the absence of strong polarity.

3. **There is no reliability, no uncertainty, and no agreement-with-humans reported anywhere in the return value or the docs.** The only bundled benchmark is `airline_tweets` (n=14,640, class-imbalanced: 9,178 neg / 3,099 neu / 2,363 pos), and the README/examples evaluate with `cosine()` of the score against a recoded label. Cosine similarity to a label vector is not a recognised agreement statistic for ordinal human ratings — it ignores chance agreement entirely. A psychometrician cannot tell from any current output whether this instrument agrees with humans better than chance, how reliable it is under paraphrase, or where its standard error is large.

The rest of this doc is concrete functions, outputs, and checks to fix those three, organised so the **top 3** lead.

---

# TOP 3 (do these first)

## 1. Make the probability a *real* probability: `calibrate_sentiment()` + a calibration report

**What.** Add an explicit calibration layer between the scorer and the rescale, and ship a reliability diagram + ECE as a first-class diagnostic. Calibration is cheap (it's a 1-D fit on held-out data), it's exactly the kind of "we did the boring rigorous thing" that fits the Linnet brand, and it's the precondition for *every* downstream claim (uncertainty bands, abstain thresholds, "65% positive" copy).

**Methods (all real, all standard).**
- **Platt scaling** (Platt 1999): fit `P_cal = sigmoid(a·s + b)` on a held-out set. Trivial, but assumes a sigmoidal miscalibration shape.
- **Isotonic regression** (Zadrozny & Elkan 2002): non-parametric monotone fit. In R this is just `stats::isoreg()` — no new dependency. Better when miscalibration isn't sigmoidal; needs more held-out data and can overfit, so report both and let the user pick.
- For the 3-class scorer, calibrate per-class probabilities and renormalise; or use **temperature scaling** (Guo et al. 2017, "On Calibration of Modern Neural Networks") on the logits — a single scalar `T`, the most defensible default for multiclass because it can't reorder classes and barely touches accuracy.

**Measure calibration, don't assert it.**
- **Reliability diagram**: bin predicted prob into ~10–15 bins, plot mean predicted vs. empirical frequency, with the diagonal = perfect. Wilson intervals on each bin.
- **Expected Calibration Error (ECE)** and **Maximum Calibration Error (MCE)** (Naeini et al. 2015; Guo et al. 2017): `ECE = Σ_b (n_b/N)·|acc_b − conf_b|`. One number, reportable, comparable across releases.
- **Brier score** (`mean((p − y)^2)`) and its **calibration/refinement decomposition** (Murphy 1973) — separates "are the probabilities honest" from "are they sharp."
- Sanity reference: the `betacal` (beta calibration, Kull et al. 2017) and `CalibratR` packages exist on CRAN and are worth citing/benchmarking against, even if you implement your own to avoid the dependency.

**Concrete API.**
```r
# fit on a labelled held-out set (ships with one; user can supply their own)
cal <- calibrate_sentiment(scores, labels, method = c("isotonic","platt","temperature"))

# apply
sentiment_score(x, calibration = cal)        # returns calibrated probs/score
# or attach calibration to the default model so out-of-the-box scores are calibrated

# inspect — this is the trust artefact
reliability_report(cal)        # ECE, MCE, Brier + decomposition, reliability diagram
plot(cal)                      # the diagram itself
```

**Output change.** `sentiment_score()` gains an attribute or richer return with `p_neg, p_neu, p_pos` (calibrated) alongside the legacy bipolar score, so nothing breaks but the honest signal is available.

**Why this is #1.** It's the cheapest thing with the largest credibility payoff, it's literally `isoreg()` + a plotting function for the simple version, and it makes the brand claim ("trustworthy measurement") *testable* instead of asserted. Every other item below depends on having an honest probability.

---

## 2. Report agreement-with-humans the way the rating-reliability literature requires — and ship `sentiment_agreement()`

**What.** Replace `cosine(score, label)` as the headline evaluation with the agreement statistics actually used for human-coded content, and bundle them as a function so users can run them on *their* labelled data, not just yours. This is the difference between "our score correlates with a recoded column" and "our instrument agrees with human raters at κ = X, which is in the 'substantial' band."

**The right statistics (all real, all have R implementations).**
- **Krippendorff's α** (Krippendorff 2004; Hayes & Krippendorff 2007): the correct general-purpose inter-coder reliability coefficient — handles ordinal data, >2 coders, and missing data, and is the standard in content analysis. R: `irr::kripp.alpha()` (`irr` is on CRAN), or `icr`. Treat the model as "one more coder" alongside the humans and report α with and without it.
- **Cohen's / weighted κ** (Cohen 1968) for model-vs-consensus-human on the 3 classes; **quadratic-weighted κ** because neg/neu/pos is ordinal and a neg↔pos error should cost more than a neg↔neu error. R: `irr::kappa2(..., weight = "squared")` or `psych::cohen.kappa()`.
- **ICC** (Shrout & Fleish 1979) for the *continuous* `[-1,1]` score against averaged human ratings — pick the right form (ICC(2,1) for absolute agreement, single rater). R: `psych::ICC()` or `irr::icc()`.
- **% agreement and the full confusion matrix**, because κ alone is famously misleading under class imbalance (the κ paradox) — and `airline_tweets` is heavily imbalanced (63% negative). Always show raw agreement next to κ.
- **Spearman ρ** (not cosine) for monotone association with ordinal labels.

**Concrete API.**
```r
sentiment_agreement(pred, human, type = "ordinal")
# returns a named list / printable object:
#   krippendorff_alpha, weighted_kappa, ICC, pct_agreement,
#   spearman, confusion_matrix, n, ci  (bootstrap CIs on each)
```

**A second, deeper point — measure the human ceiling.** Report **inter-human agreement on the same items** as the upper bound the model is chasing. A model at κ=0.55 on a task where humans only reach κ=0.60 is *excellent*; the same κ where humans reach 0.90 is poor. This reframes "accuracy" honestly: the instrument's job is to approach the human reliability ceiling, not 100%. Datasets that make this possible because they ship **multiple annotations per item** (so you can compute human–human α directly, no fabrication):
- **GoEmotions** (Demszky et al. 2020, Google) — 58k Reddit comments, multiple raters, fine-grained emotions mappable to pos/neg/neutral; rater disagreement is published.
- **SemEval-2017 Task 4** (Rosenthal et al.) — Twitter 3-class sentiment, the canonical shared task; lets you place κ against a known field benchmark.
- **MELD / EmoryNLP** and **SST** (Socher et al. 2013, Stanford Sentiment Treebank) — SST has graded 1–25 human sentiment scores per phrase, ideal for **ICC on the continuous score**, not just class accuracy.

**Why this is #2.** It converts the package's central claim ("competitive with paid APIs") from a cosine number into the vocabulary a researcher, reviewer, or sceptical buyer already trusts — and the human-ceiling framing is a genuinely differentiating, honest move competitors don't make.

---

## 3. Add uncertainty + a principled **abstain / ambiguous band**, and separate *neutral* from *mixed* from *uncertain*

**What.** Stop forcing a single point on `[-1,1]`. Give every score (a) an uncertainty estimate, (b) the option to return `"uncertain"` instead of a number when the instrument shouldn't commit, and (c) a representation that distinguishes the three zero-ish states. This is the read that the construct (3-class sentiment) demands and the current bipolar collapse hides.

**Uncertainty — concrete, no retraining the embedder needed.**
- **Calibrated margin uncertainty:** once probabilities are honest (item 1), `1 − max(p_neg,p_neu,p_pos)` and the **entropy** of the (p_neg,p_neu,p_pos) distribution are usable confidence signals. Low margin → abstain.
- **Bootstrap / ensemble over scorers:** you already maintain `xgb` and `glm` scorers, plus multiple embedders (`e5-small`, `e5-base`, OpenAI). Disagreement across them is a cheap, model-agnostic uncertainty estimate (a mini-ensemble). Report the spread.
- **Conformal prediction** (Vovk; Angelusi & Bates 2023 tutorial; R packages `conformalClassification`, `crepes`/`probably` ecosystem) for a *distribution-free* guarantee: return a **prediction set** (e.g. `{neutral, positive}`) with a user-set coverage like 90%. A singleton set = confident; a 2–3 class set = genuinely ambiguous. This is the most rigorous, most on-brand version because the guarantee is provable and auditable, not asserted.

**The abstain band — make "I'm not sure" a first-class output.**
```r
sentiment_score(x, return = "class", abstain = TRUE, conf = 0.90)
# -> factor with levels: negative, neutral, positive, uncertain
```
Tune the abstain threshold on held-out data to hit a target precision (a **risk–coverage / selective-prediction curve**, El-Yaniv & Wiener 2010): plot accuracy-on-answered vs. fraction-answered, let the user choose where to sit. Ship that curve as a diagnostic. This is exactly the honesty move the brand wants: *the instrument knows when to shut up.*

**The three zeros — neutral ≠ mixed ≠ uncertain.** Concretely:
- **Genuinely neutral**: high `p_neu`, low entropy. Affectively flat. (Requires adding neutral anchors to `default.rda` and/or a real 3-class head — see below.)
- **Mixed / ambivalent**: `p_neg` *and* `p_pos` both non-trivial — high *positivity* and high *negativity* simultaneously. This is **bivariate affect** (Cacioppo & Berntson 1994; the PANAS tradition treats positive and negative affect as *separate dimensions*, not poles of one axis). The fix is to expose two scores, `pos_intensity` and `neg_intensity`, not just their difference. `sentiment_match()` is *already* structurally close to this — it returns similarity to positive and negative anchors — so surfacing both rather than only the winning class is a small change with real construct payoff.
- **Model-uncertain**: high entropy / large ensemble spread / multi-class conformal set. Reported via the uncertainty channel above.

**Concrete API.**
```r
sentiment_score(x, output = "full")
# data.table: text, score (legacy [-1,1]),
#             p_neg, p_neu, p_pos,         # calibrated
#             pos_intensity, neg_intensity, # bivariate affect
#             ambivalence = min(p_pos_int, p_neg_int),  # mixed-ness
#             entropy, conf, prediction_set, class (w/ "uncertain")
```

**Why this is #3.** It's the read the construct actually has, it directly powers the "honest, knows-its-limits" brand, and most of the machinery (two anchor sets, two scorers, multiple embedders) is *already in the package* — this is largely surfacing information the pipeline already computes but throws away in `(probs - .5) * 2`.

---

# The rest (high value, second wave)

## 4. Test–retest & perturbation reliability: `sentiment_reliability()`

A measurement that flips under trivial rewording is not reliable, regardless of accuracy. Add a reliability suite:
- **Paraphrase invariance**: score original vs. back-translation or paraphrase; report the score delta distribution and a **test–retest correlation / ICC**. (Back-translation needs no labels — purely an internal consistency check.)
- **Perturbation robustness** in the **CheckList** tradition (Ribeiro et al. 2020, *"Beyond Accuracy: Behavioral Testing of NLP Models"*): invariance tests (typos, casing, added neutral clauses, name swaps should *not* move sentiment) and directional tests (adding "I love that..." *should* move it positive). This is a behavioural-test harness, and it doubles as your regression suite across model versions.
- **Negation & contrast stress sets**: "not bad," "not good," "good, but...". Negation handling is the classic failure mode and a documented behavioural check is a credibility asset.

```r
sentiment_reliability(x, perturbations = c("paraphrase","typo","negation","name_swap"))
# -> per-perturbation: mean |Δscore|, test_retest_icc, % sign flips, flagged items
```

## 5. Fairness / bias audit on documented disparities: `sentiment_fairness()`

This is where sentiment models have *real, published* failure modes — citeable, not speculative:
- **Racial / dialect bias (AAVE):** Sap et al. (2019), *"The Risk of Racial Bias in Hate Speech Detection"*, and Blodgett et al. (2016/2020) document that NLP models systematically misread African-American English; tweets in AAVE are disproportionately flagged negative/toxic. Ship a check using Blodgett's **TwitterAAE** dialect data and report score distributions by inferred dialect.
- **Identity-term bias:** Dixon et al. (2018), *"Measuring and Mitigating Unintended Bias in Text Classification"* — sentences differing only in an identity term ("a gay person," "a straight person") get different sentiment. Kiritchenko & Mohammad (2018), the **Equity Evaluation Corpus (EEC)**, is purpose-built for exactly this (8,640 sentences varying race- and gender-associated names/words) and is public. This is the single best off-the-shelf fairness probe to bundle.
- **Non-native / L2 English:** score the same content from L2 corpora and check for systematic penalisation.

```r
sentiment_fairness(model, probe = "EEC")   # or "TwitterAAE", or user-supplied groups
# -> per-group mean score, score-gap effect sizes (Cohen's d), 
#    equalised-odds-style gaps, items where the only change is an identity term
```
Reporting this *at all* — even when results are imperfect — is a strong, honest differentiator. The dataset and citations are real; do not invent numbers, run the probe and publish whatever comes out, including the bad parts.

## 6. Construct & criterion validity evidence (the missing validity argument)

Right now there is no documented validity argument — only "out-performs lexicons." Add a `vignette("validity")` that assembles standard evidence types:
- **Convergent validity:** correlate the score with established lexicon/tool outputs on shared text — **VADER** (Hutto & Gilbert 2014, valence-aware, the right baseline for social text; R via `vader` package), **AFINN** (Nielsen 2011), **NRC / syuzhet** (`syuzhet`, `sentimentr` by Rinker — `sentimentr` notably handles valence shifters/negation and is the strongest R incumbent to benchmark against), and **LIWC** if available. High-but-not-perfect convergence is the expected, honest result.
- **Criterion / predictive validity:** does the score predict an *external outcome* — e.g. star ratings on **Amazon/Yelp reviews**, helpfulness votes, or **IMDB** (Maas et al. 2011) polarity? Predicting a real-world criterion is the strongest validity evidence and these datasets are public and labelled.
- **Discriminant validity:** the score should correlate *less* with things it shouldn't (text length, formality, topic). Showing low correlation with text length is a credible discriminant check.
- **Known-groups validity:** scores should separate groups known to differ (e.g. 1-star vs 5-star reviews) with a large, reportable effect size.

Frame the vignette as a **validity argument** (Messick 1995; Kane 2013) — content, response process, internal structure, relations to other variables, consequences. That structure *is* the IO-psychometrics credibility signal, and it's the lens this package's authors (KF Institute, ORCID'd psychometrician co-author) can write authentically.

## 7. Standard errors and aggregation that respects them

People use sentiment on *batches* ("mean sentiment of these 500 reviews"). A mean of point estimates with no SE is a measurement error. Provide:
- Per-score uncertainty (from item 3) propagated into **batch-level confidence intervals** (delta method or bootstrap).
- A warning/guard when aggregating across the abstain band (don't average over items the model declined to score).
- Optional **measurement-error-aware aggregation**: if downstream users regress on sentiment, document that the predictor has known error and point to errors-in-variables handling — even a doc note here is more rigorous than the field norm.

## 8. Honest, neutral-aware default benchmark page (ties to existing `docs-benchmark-page-spec.md` / `real-neutral-eval-plan.md`)

The current default eval (`cosine` on `airline_tweets`) under-reports neutral and uses a non-standard metric. The benchmark page should, per release, publish: per-class precision/recall/F1 **and macro-F1** (so the dominant negative class can't carry the score), quadratic-weighted κ and Krippendorff α vs. labels, ECE + reliability diagram, the risk–coverage curve, and the fairness gaps — across `e5-small`, `e5-base`, and the OpenAI backends, on **multiple** public datasets (airline tweets *and* SemEval-2017-4 *and* SST/GoEmotions for the human-ceiling story). Same numbers, every release, regression-tracked. This makes "competitive with paid APIs" a *reproducible artefact* instead of a claim. (Two planning docs already gesture at this — `real-neutral-eval-plan.md`, `docs-benchmark-page-spec.md` — so this lens is "do it with the psychometric statistics above," not "invent it.")

---

# Smallest credible first PR (if you want one slice)

`reliability_report()` + `sentiment_agreement()` on the bundled `airline_tweets`, plus `calibrate_sentiment(method="isotonic")` via `stats::isoreg()`. Zero new hard dependencies (isoreg is base; κ/α/ICC can be `Suggests: irr, psych`), it produces a reliability diagram + ECE + weighted-κ table that can go straight onto the benchmark page, and it makes the brand's central word — *trustworthy* — something a reviewer can check rather than take on faith.

---

## Appendix: real anchors for every claim above (no fabricated results, definitions/datasets/packages only)

- **Calibration:** Platt 1999; Zadrozny & Elkan 2002 (isotonic); Guo et al. 2017 (temperature scaling, ECE/MCE); Naeini et al. 2015 (ECE); Murphy 1973 (Brier decomposition); Kull et al. 2017 (beta calibration). R: `stats::isoreg`, `betacal`, `CalibratR`, `probably`.
- **Agreement / reliability:** Krippendorff 2004 & Hayes–Krippendorff 2007 (α); Cohen 1968 (weighted κ); Shrout & Fleiss 1979 (ICC); Spearman ρ. R: `irr`, `psych`, `icr`.
- **Uncertainty / abstain:** Vovk et al. & Angelopoulos–Bates 2023 (conformal); El-Yaniv & Wiener 2010 (selective prediction / risk–coverage). R: `conformalClassification`, `crepes`-style, `probably`.
- **Bivariate affect (neutral vs mixed):** Cacioppo & Berntson 1994; PANAS (Watson, Clark, Tellegen 1988).
- **Behavioural / robustness testing:** Ribeiro et al. 2020 (CheckList).
- **Fairness:** Sap et al. 2019 & Blodgett et al. 2016 (AAVE/TwitterAAE); Dixon et al. 2018 (identity-term bias); Kiritchenko & Mohammad 2018 (Equity Evaluation Corpus).
- **Validity theory:** Messick 1995; Kane 2013.
- **Datasets (public, multi-annotator where noted):** SemEval-2017 Task 4 (Rosenthal et al.); SST (Socher et al. 2013, graded scores); GoEmotions (Demszky et al. 2020, multi-rater); IMDB (Maas et al. 2011); EEC (Kiritchenko & Mohammad 2018); TwitterAAE (Blodgett et al. 2016).
- **R incumbents to benchmark against (convergent validity):** `vader` (Hutto & Gilbert 2014), `sentimentr` (Rinker, valence shifters), `syuzhet` (NRC/AFINN/Bing), AFINN (Nielsen 2011).
