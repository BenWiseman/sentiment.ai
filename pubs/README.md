# sentiment.ai — Publications Overview

Working notes on what is *genuinely* publishable from sentiment.ai + the v2 training work,
where it could go, and what the benchmarks need before any of it is submittable. Written
2026-06-03. Honest calibration, not a hype sheet — the answer to "worth publishing, really?"
is **yes, but be precise about which thing and how much work each needs.**

---

## The honest calibration

There are three separable assets here, with very different publication readiness:

| Asset | What it is | Readiness | Best home |
|---|---|---|---|
| **The software** | The R package (multi-backend, no-TF default, context-tunable) | **High** — exists, works, novel-enough engineering | JOSS / R Journal / Behavior Research Methods |
| **The synthetic-neutral finding** | GPT-4o synthetic neutral data raises F1 on *pos/neg* classes | **Medium** — real, interesting, but the current experiment is one-shot (no seeds/CIs/ablations) | WASSA / short paper / methods journal |
| **The benchmark study** | Embedding-vs-lexicon-vs-LLM sentiment across many corpora | **Low-medium** — needs hardening to be standalone-worthy | Folded into the above, not its own paper |

The trap to avoid: treating "we got good benchmark numbers on our github.io page" as
publication-grade. It isn't — public-page benchmarks ≠ peer-reviewable benchmarks (see the
benchmark plan below). The numbers are promising; they are not yet *defensible.*

---

## Candidate 1 — The software paper (do this first; lowest risk)

**Contribution:** an R package that turns transformer embeddings into tunable sentiment
scores, now with (a) a no-TensorFlow on-device default, (b) multiple swappable backends
(sentence-transformers / OpenAI / legacy USE), (c) **context-tunable** sentiment
(`sentiment_match` — define what positive/negative *mean* per domain), and (d) explicit
neutral modelling.

**Novelty honesty:** the v0.1 package has been on CRAN since ~2021, so a paper now must be
about the **substantially new v2** — the multi-backend engine + synthetic-neutral-trained
models + the dependency-hell removal. You cannot re-publish the same software; the new
contribution has to be real and stated plainly. It is.

**Venues, ranked for *this* project's goals:**
1. **Behavior Research Methods (BRM)** — *strategically the best.* Reaches the exact
   IO/psych/HR researchers and students the brand wants; R tools for behavioral research are
   squarely in scope; this is the same audience-lineage as the competitor packages. A BRM
   citation is brand credibility *and* a real paper.
2. **Journal of Open Source Software (JOSS)** — fastest, free, peer-reviewed, citable. Low
   bar but legitimate; good for priority + a DOI while a bigger paper cooks. Requires the new
   version be feature-complete and more than a thin wrapper (it is).
3. **The R Journal** — well-respected in the R world, methods-friendly, moderate bar/speed.
4. **Journal of Statistical Software (JSS)** — highest prestige in the R world, but slow and
   demanding; only if we want the flagship version.

**Recommendation:** JOSS for speed + DOI now; BRM as the "real" paper aimed at the target
audience. Don't double-submit the same content — JOSS = software artifact, BRM = method +
validation + use-case.

---

## Candidate 2 — The synthetic-neutral-data finding (the actual research)

**The claim (from `neutral_mix_experiment.Rmd`):** neutral instances are scarce in real
sentiment corpora; injecting GPT-4o-generated synthetic neutral examples not only improves
neutral-class performance but **paradoxically raises F1 on the positive and negative
classes.** If robust, that's a genuinely interesting, slightly counterintuitive result.

**Why it's only *medium* readiness — and what's missing:**
- The current experiment is essentially single-condition: one embedding space, XGBoost,
  sampled data, (apparently) one seed. A reviewer will ask for variance.
- No comparison to the obvious alternatives: class weights, undersampling, SMOTE, or
  real (non-synthetic) neutral data. The interesting claim is *synthetic* neutral helps —
  that needs the contrast to land.
- No mechanism/analysis for *why* (does the synthetic neutral sharpen the decision boundary?
  An embedding-space visualization would carry a lot here).
- Contamination risk must be addressed head-on (see benchmark plan).

**Novelty positioning:** LLM-based data augmentation is a crowded, hot area (2023-2025). The
defensible angle is **not** "LLMs can augment data" (known) but the **specific, measured,
cross-class effect of *neutral* augmentation on a 3-class sentiment boundary** — and that it
generalizes across embedding backends. Frame narrowly and it's novel; frame broadly and a
reviewer name-drops ten prior augmentation papers.

**Venues, ranked:**
1. **arXiv preprint — immediately.** Establishes priority (matters given the copying history),
   citable now, costs nothing, and you can cite it from the package docs.
2. **WASSA** (Workshop on Computational Approaches to Subjectivity, Sentiment & Social Media
   Analysis, ACL/EMNLP-affiliated) — the natural NLP home for sentiment-specific work, more
   accessible than main-conference ACL/EMNLP, and respected.
3. **Behavior Research Methods** — if framed as a methods contribution for behavioral text
   analysis (can be the *same* BRM paper as Candidate 1, with this as the validation study).
4. SemEval-adjacent workshops if there's a relevant shared task to benchmark against.

**Recommendation:** arXiv first (priority), then either WASSA (if we want NLP-community
credibility) or fold into the BRM paper (if we want the IO/psych audience). Given the brand
strategy, **BRM-with-this-as-the-validation-study is the highest-leverage single artifact.**

---

## Candidate 3 — The strategic prize: an applied-IO methods paper

This is the one that pays the *brand* rent, not just the *academic* rent.

**Contribution:** "Measuring sentiment in open-ended organizational text (engagement-survey
verbatims, exit interviews, reviews) with tunable, neutral-aware, embedding-based methods —
and an open R tool to do it." This is the bridge between sentiment.ai and the planned
`linnetlabs` text-network work, and it seeds the exact people (IO researchers, HR analytics
practitioners) the company wants using Linnet methods.

**Venues:**
1. **Organizational Research Methods (ORM)** — *the* methods journal for IO/management.
   A tool+method paper on organizational text sentiment here is perfect strategic
   positioning. High bar; ideally needs an academic co-author (see authorship note).
2. **Behavior Research Methods** — again, also viable here.
3. *Journal of Business and Psychology* / *Personnel Assessment and Decisions* — applied,
   accessible, target-audience-adjacent.

**Honesty:** ORM is a stretch without (a) an IO co-author and (b) a real organizational
validation dataset (not just public Twitter/IMDb corpora). This is a *post-IO-psych-hire*
paper — it should wait for the collaborator who can co-sign it credibly. Flag, don't force.

---

## Better benchmarks — the work that gates everything

Every candidate above is bottlenecked on the same thing: the benchmarks must move from
"good demo numbers" to "defensible evidence." Concretely:

**1. Honest baselines (currently thin).** Add, at minimum:
- Lexicon/classical: VADER, TextBlob, and the R-native `syuzhet` / `sentimentr` / tidytext
  (bing/afinn/nrc). These are what the target audience uses today — beating them is the point.
- Modern transformer baselines: a fine-tuned RoBERTa sentiment head
  (e.g. `cardiffnlp/twitter-roberta-base-sentiment-latest`), the HuggingFace default sentiment
  pipeline, and Flair. *This is the credibility-critical comparison* — "embeddings + a light
  classifier vs a fine-tuned transformer" is the question a reviewer will ask first.

**2. Statistical rigor.** Multiple seeds; report mean ± CI, not point estimates. Significance
tests between methods (bootstrap or McNemar). Without this the "paradox" is anecdote.

**3. The neutral story needs the right corpora.** Most sentiment benchmarks are binary, which
buries the 3-class contribution. Lean on the neutral-heavy ones already in the training repo —
especially **Financial PhraseBank**-style financial news (3-class, neutral-dominant) and the
COVID-survey data — and report per-class F1, not just accuracy.

**4. Contamination, addressed head-on.** Two real threats a reviewer *will* raise:
- LLM embedding models (OpenAI, sentence-transformers) may have seen public test sets
  (sentiment140, IMDb) during pretraining → inflated scores. Acknowledge; ideally include at
  least one *recent/private* held-out set the models can't have seen.
- Synthetic training data must be checked for overlap/leakage against test sets.

**5. Cross-domain generalization.** Train on one domain, test on another (e.g. train
social-media, test financial). This is where embedding-based methods should shine over
lexicons, and it's a stronger story than same-domain accuracy.

**6. Practical axes the audience cares about.** Latency + cost per 1k docs (on-device
paraphrase vs API), and "works on a student laptop with no GPU" — these are legitimate,
publishable, and on-brand contributions, not afterthoughts.

**7. The on-device embedder bake-off is itself a contribution.** Measured macro-F1 so far:
oai_3_small 0.893 > use_lg 0.832 > use 0.817 > paraphrase-MiniLM 0.793. The open question —
*can a modern no-TF on-device embedder (mpnet / bge / gte / e5) match the API or USE-large
without a GPU?* — is a genuinely useful result for the target audience and the natural backbone
of the benchmark paper. (It's also v2's deciding engineering experiment; see roadmap Phase 3.5.)
Keep this distinct from the synthetic-neutral finding — they're orthogonal (embedder choice vs
training-mix), and conflating them weakens both.

**Deliverable:** a single reproducible `benchmarks/` pipeline (extend the training repo's
`make_benchmarks.R`) that emits a results table + figures both papers can cite. Build this
*once*, well; it underwrites Candidates 1–3.

---

## Sequencing (realistic, pre-launch, time-poor)

1. **Harden the benchmark suite** (the gate; ~the bulk of the real work).
2. **arXiv preprint** of the synthetic-neutral finding + benchmark table — priority + citable now.
3. **JOSS** submission for the v2 package — fast DOI, low risk.
4. **BRM paper** = the package + the synthetic-neutral validation, aimed at the IO/psych
   audience. This is the highest-leverage single artifact.
5. **ORM applied paper** — *later*, gated on an IO-psych co-author + a real org dataset.

## Authorship / collaborators
- Existing sentiment.ai co-authors are natural here: **Steven Nydick** (psychometrician, ORCID)
  is a strong methods co-author; Tristan Wisner contributed originally.
- The applied-IO paper (Candidate 3) should wait for the planned IO-psych hire — same gating
  logic as linnetIO. An academic co-signer is what makes ORM credible.
- Korn Ferry Institute funded/attributed the original package; keep that history intact in any
  paper's acknowledgements.

## What I would *not* claim
- Don't pitch the benchmark study as a standalone novelty — it's evidence, not a paper.
- Don't overclaim the augmentation finding as general ("LLMs improve sentiment models"); keep
  it narrow and measured or reviewers will bury it under prior augmentation work.
- Don't re-sell v0.1 as new — the paper is about the v2 engine + method.
