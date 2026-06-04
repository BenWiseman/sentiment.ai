# sentiment.ai — senior ML/NLP data-science review

> Reviewer role: senior ML/NLP data scientist. Scope: the embed + light-scorer
> architecture, the 3-class → [-1,1] scoring math, evaluation rigor, the model
> lineup/bake-off, synthetic-data construct validity, calibration, and
> SOTA-worth-adopting vs scope-creep.
>
> Method note: I read the actual code and artefacts before opining —
> `R/sentiment.R`, `R/embedding.R`, `R/matrix_helpers.R`, `R/constants.R`, the
> bake-off harness + logs (`rigor.py`, `rigor.log`, `full_train.py`,
> `full_results.log`, `sub_embed.py`, the `*.log` embed headers), the shipped
> scorers in `bakeoff/scorers/`, the package's e5 prefix contract
> (`inst/get_embedder.py`, `tests/testthat/test-e5-prefix.R`), and the five lens
> docs + three adversarial reviews already in `planning/`. Every number quoted
> below is read from those files, not assumed. Date: 2026-06-03.
>
> This review BUILDS ON the prior work rather than repeating it. The
> calibration / 3-class-output / abstention / agreement / provenance programme in
> `better-sentiment-roadmap.md` and the two NLP/psychometric lenses is **correct and
> I endorse it** — I do not re-derive it here. The methodology review
> (`review-methodology.md`) is genuinely good and `rigor.py` already closes several
> of its findings. What follows is the *delta*: (1) a load-bearing correctness bug
> the prior reviews flagged as a risk but that the artefacts now show is **live and
> still unfixed** in the shipped scorer; (2) places where the existing roadmap is
> right but **under-specifies the math**, where a wrong implementation would quietly
> reintroduce the very dishonesty the brand is built against; (3) a few genuinely
> new DS ideas that fit the frozen-embedding + light-head recipe.

---

## 0. The architecture is sound and correctly positioned — start from agreement

"Linear/shallow probe on a frozen sentence representation" (the SentEval probing
family, Conneau & Kiela 2018) is a real, well-understood architecture, and the
`better-lens-nlp.md` framing of where it wins (tunable in a `xgb.train()` call,
on-device, deterministic, auditable, anchor-axis generalisation) vs loses (negation
scope, contrastive discourse, sarcasm, single-axis collapse) is accurate and I have
nothing to add to that map. The strategic call — **do not chase the cardiffnlp/SST
leaderboard with a black box; win on calibration + honesty + tunability** — is the
right call for this brand. The roadmap's "stop discarding the 3-class signal,
calibrate it, return it in one honest shape, make it reproducible" spine is exactly
what I would have written. So this review spends its budget on what is *wrong or
under-specified*, not on re-litigating what is right.

One framing correction that matters for everything below: the package is sold as
**honest measurement**. In a measurement instrument, the single worst failure is not
low accuracy — it's a **silent systematic error the user cannot see**. Two of my
three top findings are exactly that class of bug. They outrank the (correct, but
already-planned) calibration/output-shape work because a beautifully calibrated
probability computed on a *mis-served embedding* is precisely calibrated nonsense.

---

## TOP 3 (the return value)

### DS-1 — CRITICAL, LIVE: train/serve skew on the e5 prefix. The shipped scorer was trained on UN-prefixed e5 embeddings; the package serves PREFIXED ones. The two disagree at inference, silently.

This is the sharpest thing in the repo and it is not hypothetical. The pieces:

- **The package mandates the prefix at serving time.** `R/constants.R:32`
  `model_prefix <- c(`e5-small` = "query: ", `e5-base` = "query: ")`;
  `inst/get_embedder.py:31` `load_st_embedder(hf_id, prefix="")` prepends it;
  and there is a **CI gate** (`tests/testthat/test-e5-prefix.R`) that *fails the
  build* if `embed_text("good service", model="e5-small")` does not send
  `"query: good service"` to the encoder. So production embeddings are
  **prefixed**, by design and by test.

- **The shipped scorer was trained on UN-prefixed embeddings.** The scorers in
  `../sentiment.ai_training/.../bakeoff/scorers/xgb_model_me5_{small,base}_full.xgb`
  are the heads `full_train.py` fits on `sub_emb_me5_*_full.csv`. Those embedding
  CSVs were produced by `sub_embed.py`, whose `--prefix` defaults to `""` and which
  only prefixes when `--prefix "query: "` is passed explicitly. **Every e5 embed log
  header reads `... | intfloat/multilingual-e5-base | tag=me5_base_full | dev=mps:0 |
  maxseq=512` with no prefix**, and `grep -ri "query:"` across the entire bake-off
  directory matches only the *script source* and the *corpus text* — **zero** log
  headers, zero captured `--prefix` invocations. The e5 heads were trained on the
  raw string.

- **Therefore the serving path and the scorer are on different embedding
  manifolds.** e5 is trained with mandatory `query:`/`passage:` instructions (Wang
  et al. 2024, *Multilingual E5*, arXiv:2402.05672); the prefix is not cosmetic — it
  shifts the embedding. So at inference the package computes `embed("query: " + text)`
  and feeds it to an xgb head fit on `embed(text)`. **The features are
  distribution-shifted relative to training.** xgboost will still return a number; it
  will just be a quietly degraded one, with no error and no warning. This is the
  canonical train/serve skew, and it is invisible precisely because the output still
  looks like a valid score in [-1,1].

**Why the prior reviews didn't fully catch this.** `review-methodology.md` S2 raised
the prefix, but framed it as a *benchmark fairness / is-the-headline-number-correct*
problem ("e5 may be under-estimated; re-run with the prefix"). The more dangerous
version is the *product* one: it does not matter whether the benchmark used the
prefix — what matters is that **the benchmark, the scorer, and the serving path all
agree.** Right now the serving path (prefixed, CI-enforced) disagrees with the
scorer (unprefixed). The benchmark agreeing with the scorer (both unprefixed) is the
*worse* of the two consistent worlds, because it means the published macro-F1
(0.8902 for me5_base in `full_results.log`) was measured under a configuration the
product **will not use** — so even the honest "ties OpenAI" number does not describe
what a user gets.

**The fix (and it is cheap, but it is a re-train, not a flag):**

1. **Pick one prefix convention and make it true end-to-end.** For symmetric
   document classification the e5 card sanctions `query:` on every input (do **not**
   mix `query:`/`passage:`). The package has already committed to `query:` at serve
   time and gated it in CI, so the convention is decided — **re-embed the training
   corpus WITH `--prefix "query: "` and re-fit the e5 heads.** The shipped
   `xgb_model_me5_*_full.xgb` must be regenerated; the current ones are not safe to
   ship.
2. **Add a train/serve-parity guard so this can never recur.** Stamp the prefix used
   to train each scorer into the scorer's metadata (alongside the embedder id and
   dim), and **assert at score time that `model_prefix[[model]]` equals the scorer's
   recorded training prefix** — fail loud on mismatch. This is the same
   "prefix-as-scorer-property" contract `review-lineup.md` calls SEV-1 and
   `better-sentiment-roadmap.md` D1/Move-3 wants for provenance; it is also the
   mechanical guarantee that closes *this* bug. One assertion turns a silent skew
   into a build/test failure.
3. **The `test-e5-prefix.R` "KNOWN-STRING regression" test is currently skipped**
   (`skip_if_no_fixture`) because the prefixed/un-prefixed e5 fixtures aren't
   committed. Commit them — but note that as written it only proves the *serving*
   path prefixes; it does **not** prove the *scorer* was trained prefixed. Add a
   parity assertion test (point 2) that is the actual guard.
4. **Re-measure the headline after the re-train.** The "0.890 ties OpenAI 0.897"
   line should be the prefixed number or it is describing a model nobody runs.
   (Good news for the on-device story: e5 unprefixed is the *degraded* mode, so the
   corrected number is likely ≥ the published one — but it must be measured, not
   assumed.)

This is #1 because it is a **live correctness defect in the thing about to ship**,
it produces a silently-wrong score in exactly the brand whose entire pitch is "you
can trust the number," and the fix is a known, bounded re-train + one assertion.

---

### DS-2 — The `P(pos) − P(neg)` collapse is not just lossy (the roadmap covers that) — it is *miscalibrated in a class-dependent way*, and the planned isotonic fix will silently make it worse unless the math is specified correctly.

The roadmap (Move 1/2) and both NLP/psychometric lenses correctly say: stop
discarding the 3-class signal, calibrate it. I endorse that. But the **calibration
math as currently sketched across those docs is under-specified in a way that, if
implemented naively, reintroduces dishonesty** — so here is the senior-DS detail the
plans are missing.

**What the code actually does** (`R/sentiment.R:277-278`): for the v2 3-class
`multi:softprob` head it returns `score = probs[,3] - probs[,1]` (i.e.
`P(pos) − P(neg)`, neutral mass implicitly pulling toward 0). The legacy binary path
returns `(P − 0.5)*2`. Two distinct, non-comparable score definitions live behind
one function — already a provenance hazard.

**The three things the calibration plan must get right (and currently doesn't say):**

1. **`P(pos) − P(neg)` is a difference of two raw xgboost softprob outputs, and
   xgboost multiclass probabilities are not calibrated** (Niculescu-Mizil & Caruana
   2005 is specifically about boosted trees). The miscalibration is **not uniform
   across the axis**: near the poles the softmax saturates (over-confident), and the
   neutral-heavy middle is where the synthetic-data artefact (DS-3) lives. So a
   *single global* monotone transform on the scalar `P(pos)−P(neg)` (the naive
   reading of "isotonic on the score") will **average two different miscalibration
   regimes** and can make the middle *worse* while fixing the ends. The correct move
   is to **calibrate the 3-class probability vector, then derive the scalar** — not
   calibrate the scalar. Specifically: temperature-scale the logits (one scalar `T`,
   can't reorder classes; Guo et al. 2017) *or* per-class isotonic + renormalise
   (Dirichlet calibration, Kull et al. 2019, is the principled multiclass form), then
   recompute `P(pos)−P(neg)` from the *calibrated* vector. Calibrate upstream of the
   collapse, never downstream of it.

2. **What does "calibrated" even mean for a score in [-1,1]?** A probability is
   calibrated if among items scored 0.8, ~80% are positive. A bipolar score has no
   such referent — so the package must be explicit about *which* quantity it
   calibrates. The honest answer: calibrate `P(pos)`, `P(neu)`, `P(neg)` as
   probabilities (ECE on each, reliability diagram per class), and treat the scalar
   as a *derived convenience*, documented as "not itself a calibrated probability."
   If the docs instead imply `-0.9` means "90% negative," that is the exact
   `(prob-.5)*2` dishonesty the brand can't ship, just relocated. The psychometric
   lens gestures at this; make it a hard rule: **the calibrated objects are the three
   class probabilities; the [-1,1] scalar is explicitly a reporting transform.**

3. **Calibrate on a held-out split that is real, not synthetic.** Calibration fit on
   the 93%-synthetic-neutral pool (DS-3) calibrates the model to GPT-4o's neutral
   style, not to neutral. The calibration set must be drawn from the real-only slices
   the `real-neutral-eval-plan.md` already specifies. (This is the one place where
   the calibration programme and the synthetic-data problem couple — flag it so they
   aren't built independently.)

Net: the roadmap's "add isotonic via `stats::isoreg`, zero new dep" line is *true
but dangerous as stated*. Isotonic on the scalar is the wrong object; the right
object is the probability vector, and the cheapest defensible default is
**temperature scaling on the logits** (one parameter, monotone, accuracy-preserving),
with per-class isotonic/Dirichlet as the opt-in. Ship `reliability_report()` with
**per-class** ECE + reliability diagrams (not one scalar curve), because a single
aggregate ECE will hide that neutral is the miscalibrated class.

---

### DS-3 — The synthetic-neutral construct-validity problem is real, partially mitigated, but the *positive evidence the package wants to publish does not yet exist* — and there's a cheaper, more honest scorer-side fix than the planned external-corpus eval.

`review-methodology.md` S1 nailed it: of 43,437 neutral rows, **40,576 (93.4%) are
GPT-synthetic**, 2,861 real. `rigor.py`'s ablation is a real improvement and its
`rigor.log` result is honest about its own limitation — WITH synthetic-neutral train
gives real-test neg-F1 0.931 / pos-F1 0.922; WITHOUT gives **0.938 / 0.934** (i.e.
on this run, dropping synthetic neutral *helped* real pos/neg by ~0.01), and the log
correctly caveats that WITH/WITHOUT also changes the neutral *count*, so it can't
cleanly attribute the effect. `real-neutral-eval-plan.md` then specifies the proper
external-corpus eval (Financial PhraseBank + SemEval/tweet_eval) to measure F1(0) on
natural neutral. All of that is good and I endorse the plan. Three additions:

1. **State plainly what the current evidence shows, because it is the opposite of the
   drafted headline.** The roadmap and `v2-roadmap.md` lean on "synthetic neutral
   *raises* pos/neg F1" as a credibility asset / publishable finding. The one
   controlled run in `rigor.log` shows synthetic neutral **lowering** real pos/neg F1
   by ~0.01 (0.938→0.931 neg, 0.934→0.922 pos). That single run is noisy and
   count-confounded — but it means **the package currently has zero clean evidence
   for the finding it wants to publish, and one data point against it.** Do not put
   "synthetic neutral helps" in NEWS/JOSS until the count-matched, real-test version
   exists and actually shows it. (This is a "never fabricate / never overclaim"
   guardrail, applied to the package's own marquee finding.)

2. **A count-matched ablation is the missing arm, and it's cheaper than the full
   external eval.** The clean experiment: hold the neutral *count* fixed and vary
   only *synthetic-ness* — i.e. train arm A on `k` real neutral + `m` synthetic
   neutral, arm B on `k+m` real neutral (needs sourcing more real neutral, which is
   exactly what PhraseBank provides), evaluate both on a 100%-real held-out neutral
   slice. That isolates the construct claim. The `real-neutral-eval-plan.md` Step 5
   already half-specifies this; the addition is **count-matching the arms**, which its
   own caveat admits is the gap.

3. **A scorer-side honesty fix that needs no external corpus at all (and is more
   on-brand than the eval): detect synthetic-neutral *style* as an OOD signal at
   serve time.** The 2,861 real-neutral embeddings and the 40k synthetic-neutral
   embeddings are both on disk. If a light probe (or even the cosine-to-centroid code
   already in `R/matrix_helpers.R`) can separate real-neutral from synthetic-neutral
   embeddings — and the S1 argument is precisely that it *can*, because GPT-4o neutral
   has detectable stylistic regularity — then the package can ship that exact probe as
   a **"this neutral looks like training-distribution neutral / unlike it" drift
   warning**. It converts the synthetic-data weakness into the OOD-warning feature the
   roadmap (D4) already wants, using data already on disk, with no new corpus
   dependency. The construct-validity *liability* becomes an auditability *feature*.
   That is the most on-brand resolution available.

---

## Secondary DS findings (build-on, not headline)

### S-A — Two score definitions behind one function is a provenance bug, not just a style nit.
`find_sentiment_score()` returns `P(pos)−P(neg)` for 3-class heads and `(P−0.5)*2`
for binary/legacy/glm. These are **different estimators on different scales** — the
legacy binary path has no neutral mass to pull the middle, so a 0.0 means different
things in the two paths. Any cross-model comparison (USE-legacy vs e5-default) is
comparing two metrics. The provenance stamp (roadmap Move 3) must include the
**score definition**, and ideally the package should converge all heads on the same
3-class softprob formulation so legacy and default are comparable. Right now they are
silently not.

### S-B — The `glm` scoring path is dead weight that dilutes the audit story; consider retiring it or making it a real calibrated baseline.
`find_sentiment_score()` still carries a CSV-weights logistic GLM
(`R/sentiment.R:262-268`). It exists for legacy USE spaces. For v2 it's neither the
default nor benchmarked in the bake-off (the lineup is all xgb). A half-supported
second scorer is audit-surface you have to defend and test for no accuracy benefit.
Either (a) retire it from the v2 default surface, or (b) repurpose it as the
**inherently-calibrated, fully-inspectable linear baseline** — a multinomial logistic
(`glmnet`) on the embeddings *is* a legitimate, more-auditable-than-xgb head, and
"here is the linear model's number next to the boosted one" is a genuine honesty
artefact (if they agree, trust up; if they diverge, the case is hard). Don't leave it
as an untested third thing.

### S-C — The neutral class is the weak class everywhere; consider an ordinal head, not 3-way softmax.
Across every row of `full_results.log` and `rigor.log`, neutral F1 is the lowest of
the three (e.g. me5_base 0.927/0.876/0.868 neg/neu/pos). neg/neu/pos is **ordinal**,
and 3-way softmax throws that ordering away (it can confidently flip neg↔pos as
easily as neg↔neu). An ordinal formulation — `xgboost` with `rank:pairwise` is wrong
here, but **two cumulative-threshold binary heads** (P(≥neutral), P(≥positive),
i.e. the classic ordinal-regression-by-binary-decomposition, Frank & Hall 2001) or a
single regression head to {-1,0,1} with thresholded readout — respects the ordering
and typically helps exactly the middle class. This is *on-recipe* (still a light head
on frozen embeddings) and is a real, cheap, defensible accuracy lever on the one
class that needs it — distinct from the leaderboard-chasing the brand correctly
forbids, because it improves the *neutral construct* the package most needs.

### S-D — Evaluation rigor: `rigor.py` closed S3/S5/S6, but two methodology findings are still open in the artefacts.
Credit where due — `rigor.py` implements paired bootstrap CIs (S3), np_tree=24
shipping config (S5), and no-early-stopping (S6), and `rigor.log` reports them
cleanly (me5_base − oai 3-small ΔF1 = −0.0020 [−0.0126, +0.0085], CI includes 0 →
the "statistically indistinguishable" framing is now *earned*, not asserted). Two
remain open and should be tracked, not lost: **S2 (prefix) is DS-1 above and is NOT
closed** (rigor.py reads the unprefixed `sub_emb_me5_*.csv`, so its e5 numbers are
also unprefixed). **S8 (the silent 130310-vs-130311 off-by-one)** is still present
(`full_subset.log` shows one row dropped with only a printed warning) — add the
hard `assert n_written == n_wanted` and the row-for-row index/label alignment guard,
because that float→int index coercion is exactly where an embedding-row/label
misalignment (which would corrupt F1 without changing row counts) would hide.

### S-E — `sentiment_match()` cosine is uncalibrated and the README admits it; this is a quiet validity hole in the package's most differentiated feature.
The anchor-axis scorer (the category-of-one feature, per the NLP lens) returns raw
cosine, and `R/matrix_helpers.R` `cosine()`/`rescale()` is a bare normalised dot
product. Raw cosine is **not comparable across texts** (length/norm effects — the
README itself warns longer text → lower similarity). So `sentiment_match()` ships an
*uncalibrated similarity* presented as a score, which is the same sin as
`(prob-.5)*2` in the package's flagship differentiator. The NLP lens' "multi-anchor
centroid + calibrated margin + NLI-style templates" fix is right; the DS-specific
addition is that the **margin** (cos-to-pos-centroid − cos-to-neg-centroid) is the
quantity to calibrate (it's length-robust in a way absolute cosine isn't), and it
should be calibrated with the *same* monotone-fit machinery as the main scorer so the
package has **one** calibration story, not two.

### S-F — Confidence/abstention needs a metric the package doesn't yet compute: the risk–coverage curve, and it should be shipped as an artefact, not a threshold.
The roadmap's abstention (D3/Move 4) is right. The DS detail: don't ship a *magic
threshold* ("abstain if max prob < 0.5"), ship the **risk–coverage curve** (accuracy
on the answered fraction vs fraction answered; El-Yaniv & Wiener 2010) computed on
the real-neutral held-out set, and let the user choose their operating point. The
curve *is* the honesty artefact (it shows the accuracy/coverage trade the user is
buying); a single baked threshold hides it. Cheap — it's a sort of the calibrated
confidences + a cumulative accuracy. Same point applies to conformal sets if/when
adopted (ship the coverage-vs-set-size curve).

---

## Scope-creep ledger (DS lens) — confirming and slightly amending the roadmap's calls

I agree with every "No" in `better-sentiment-roadmap.md`'s scope-creep table
(LLM-in-scoring, encoder fine-tune, full ABSA, invented composite index,
leaderboard-chasing). Three amendments from the DS chair:

- **Ordinal head (S-C) is NOT leaderboard-chasing — promote it from "no" to "yes,
  on-recipe."** It improves the neutral *construct* (the documented weak class) with a
  cheap light head, which is squarely the brand's "measure the thing honestly," not a
  black-box F1 point. This is the one place I'd *add* to the accuracy work the roadmap
  otherwise (correctly) deprioritises.
- **Multi-seed (methodology S3 fix #2) — skip it, `rigor.py`'s paired bootstrap
  already dominates it.** The paired bootstrap on fixed test predictions answers "is
  the ordering real" more cheaply and more correctly than 5×10 re-trains; don't spend
  the compute. (Amends the methodology review's own priority list — its bootstrap is
  the better tool and it's already done.)
- **VAD / emotion / aspect heads — agree they're deferred, but note the *sequencing*
  reason is calibration, not effort.** Every extra head needs its own calibration +
  agreement story or it's just more uncalibrated outputs. Ship the calibration
  *machinery* (DS-2) first as a reusable component, then heads are cheap; ship heads
  first and you have N uncalibrated scores and N honesty debts.

---

## One-paragraph bottom line

The architecture and the strategic posture are right, the adversarial reviews are
strong, and `rigor.py` has already done real work. But the artefacts show a **live,
silent train/serve skew** — the shipped e5 scorer was trained on un-prefixed
embeddings while the package serves prefixed ones and CI-enforces the prefix — and
that must be fixed by re-training + a prefix-parity assertion **before any of the
(correct) calibration/output-shape work**, because calibrating a mis-served
embedding is precise nonsense. The calibration plan itself is right but must
calibrate the **3-class probability vector upstream of the `P(pos)−P(neg)` collapse**
(temperature scaling default; per-class ECE), on a **real** held-out set, or it will
quietly worsen the neutral middle. And the marquee "synthetic neutral helps pos/neg"
finding **currently has one data point against it and none for it** — it cannot be
published until the count-matched, real-test arm exists; meanwhile the
real-vs-synthetic-neutral separability can be shipped as an OOD drift *feature*,
turning the liability into the auditability the brand sells.

---

## TOP 3 (return value, verbatim)

1. **CRITICAL LIVE BUG — e5 prefix train/serve skew.** The package serves
   `"query: " + text` to e5 (CI-gated in `test-e5-prefix.R`, `R/constants.R:32`),
   but the shipped scorers `xgb_model_me5_{small,base}_full.xgb` were trained on
   **un-prefixed** embeddings (every embed-log header shows no prefix; no `--prefix`
   in any captured bake-off command). Inference feeds prefixed features to a head fit
   on un-prefixed ones — a silent, systematically-wrong score in the one brand that
   sells trustworthy numbers. Fix = re-embed the training corpus with `query:` and
   re-fit the e5 heads, **plus** stamp the training prefix into the scorer and assert
   `serve_prefix == scorer_prefix` at score time (the same SEV-1 prefix-as-property
   contract `review-lineup.md`/Move-3 want — it both fixes this and guarantees it can't
   recur). Do this before any calibration work; calibration on a mis-served embedding
   is calibrated nonsense.

2. **Calibrate the 3-class probability vector UPSTREAM of the `P(pos)−P(neg)`
   collapse — not the scalar.** The roadmap is right to calibrate, but "isotonic on
   the score via `isoreg`" calibrates the wrong object: `P(pos)−P(neg)` averages two
   different miscalibration regimes (saturated poles vs the synthetic-data-polluted
   neutral middle), so a single monotone scalar fit can worsen neutral while fixing
   the ends. Correct: temperature-scale the logits (one scalar, monotone,
   accuracy-preserving; Guo 2017) or per-class isotonic/Dirichlet (Kull 2019), then
   re-derive the scalar from the *calibrated* vector; report **per-class** ECE +
   reliability diagrams; fit calibration on a **real** held-out slice, never the
   93%-synthetic neutral pool. Document the [-1,1] scalar as a reporting transform,
   not a calibrated probability.

3. **The "synthetic neutral raises pos/neg F1" headline currently has one data point
   AGAINST it and none for it — don't publish it yet; turn the construct-validity
   liability into an OOD feature instead.** `rigor.log` shows synthetic neutral
   *lowering* real pos/neg F1 (neg 0.938→0.931, pos 0.934→0.922), count-confounded
   and noisy. The clean test is a **count-matched** ablation (real-neutral substituted
   for synthetic, fixed count, evaluated on 100%-real held-out neutral) — cheaper than,
   and a prerequisite for, the external-corpus eval already planned. Separately: the
   real-vs-synthetic-neutral embeddings are both on disk and (per S1's own argument)
   separable, so ship that separation as a **"this neutral is unlike training-distribution
   neutral" drift warning** (reusing `R/matrix_helpers.R` cosine-to-centroid) — the
   on-brand move that converts the weakness into the auditability feature D4 already wants.
