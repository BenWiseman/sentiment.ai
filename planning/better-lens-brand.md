# A better sentiment.ai — the strategy/brand lens

> Written 2026-06-03. Planning-only (writes to `planning/`, never `R/`). Companion to
> `v2-roadmap.md` and the existing planning set (`honest-copy-rewrites.md`,
> `review-methodology.md`, `real-neutral-eval-plan.md`, `og-textgraph-design.md`).
> Those docs answer *"how do we migrate off TF honestly?"* This doc answers a different
> question: **assuming v2 is done, what makes sentiment.ai a better TOOL — and what is the
> ONE differentiator that makes it the trustworthy sentiment package rather than another one?**

---

## The single sharpest differentiator (the headline)

> **A sentiment score you can audit: reproducible to the bit, pinned by version, and
> explainable per call.** Every other option in R — lexicons aside — hands you a number you
> cannot reproduce next year, cannot pin to a model version, and cannot explain. sentiment.ai
> is the only one that turns "trustworthy" from a tagline into a *property of the API*.

Lean into **auditability, not accuracy.** Accuracy is already at parity (the bake-off shows
e5-base within noise of paid OpenAI), and accuracy is a treadmill anyone with a bigger model
wins next quarter. Auditability is a *moat the architecture already gives you for free* and
that the paid APIs **structurally cannot match** — a closed API can never be reproducible or
inspectable, by definition. This is the honest-measurement brand made literal.

Everything below is in service of that one line.

---

## Why "accuracy" is the wrong hill (and auditability is the right one)

The instinct for "a better sentiment package" is "make it more accurate." Resist it:

1. **Accuracy is at parity and capped.** Per `review-lineup.md`, on-device e5-base (0.860)
   is within 0.001 of paid OpenAI (0.861). You are not going to out-accuracy a frontier lab,
   and you don't need to — you've already neutralised accuracy as a reason to pick a paid API.
2. **Absolute accuracy is unpublishable as a headline anyway.** Your own
   `review-methodology.md` (S1) shows the neutral class is ~93% synthetic and the corpora are
   partially public — so the honest move is to call F1 an *upper bound*. A brand built on
   "we're the most accurate" would be a brand built on the one number your own rigor forbids
   you to claim. **Auditability is the claim you *can* stand behind without an asterisk.**
3. **Accuracy doesn't differentiate; reproducibility does.** Every transformer-sentiment tool
   is "embeddings → classifier." The thing none of them give you is a score you can *defend in
   a methods section*. That is the IO-psych / research buyer's actual pain, and it is Linnet's
   actual brand.

So the better tool is not the more accurate one. It is the one a researcher can put in a
paper, a regulated org can put in an audit trail, and a student can re-run in 2028 and get the
identical number. That is **auditability**, and it has four concrete tool properties.

---

## What "auditable sentiment" means as concrete tool features

This is the differentiator decomposed into things a user can actually touch. Each is a
*capability claim*, grounded in what the architecture already supports.

### 1. Reproducible-by-version: the score is a pinned function of (text, embedder, scorer)
The score = `scorer_v(embed_m(text))`. Both halves are *pinnable open artifacts*: an open
embedder (e5, with a known revision) and an open xgboost scorer shipped in the package. So a
user can record "sentiment.ai 2.0.1, model=`e5-base`, scoring=`xgb` v1.0" and **get the
identical number forever** — the literal opposite of a paid API where `text-embedding-3-small`
can change under you with no notice and no version handle.
- **Tool implication:** make the version triple *first-class and returned*, not buried.
  `attr(scores, "provenance")` (or a `sentiment_provenance()` accessor) returning
  `{package_version, embedder, embedder_revision, scoring, scoring_version, prefix}`. A
  one-call **stamp** the user can paste into a methods section. This is a small surface that
  *is* the brand.
- **Why it's defensible:** a closed API cannot do this. It is a structural advantage, not a
  feature race.

### 2. Explainable per call: `sentiment_match()` is the audit trail, promote it
`sentiment_match()` already returns, per row, the **nearest anchor phrase** and its
**similarity** — i.e. *"this was scored negative because it is most similar to the anchor
'terrible service' at cos = 0.71."* That is a built-in, deterministic explanation. It is
currently framed as a secondary feature; under the auditability thesis it is **co-headline
with `sentiment_score()`**: the package that tells you *why*, in cited, inspectable terms.
- **Tool implication:** let the user supply their **own** anchor set as the contract (already
  supported via `phrases`), and treat the anchors as a *versionable, shareable artifact* —
  "here are the exact phrases this study scored against." That is reviewer-grade transparency
  no API offers.
- **Guardrail:** keep the explanation *honest* — it is "nearest labelled anchor," not "the
  reason the model decided." Don't dress similarity-to-anchor as causal reasoning. (See
  off-brand list.)

### 3. Offline / on-device by default: auditable *because* nothing leaves the machine
v2's no-TF on-device default isn't just an install-friction win — it's an **auditability and
privacy** win, and that's the framing that ties it to the brand. A score computed locally from
open weights is one you can inspect end to end; a score from an API is a black box you rent.
- **Tool implication / framing:** position "no API, no data egress" as part of the *trust*
  story (research ethics boards, regulated HR data, GDPR), not just the *cost/offline* story.
  This is also exactly the Linnet posture ("math layer is IP, stays server-side; browser gets
  display-ready JSON") — sentiment.ai is the *open* expression of the same value: you own the
  computation.

### 4. The model card / "what this measures" honesty surface
Ship a short, in-package **model card** (vignette + `?sentiment.ai` "Limitations" section)
that states plainly: 3-class scorer; trained on {sentiment140, IMDb, Amazon, financial news,
TweetSemEval, ...} + synthetic neutral; F1 are *upper bounds*; neutral is the weak class;
multilingual is *inherited from the e5 backbone, not independently benchmarked* (the exact
scoping `real-neutral-eval-plan.md` builds the evidence for). **The package that tells you
where it's weak is the trustworthy one.** No competitor ships its own limitations.

---

## How this feeds the three strategic surfaces

### A. Linnet's honest-measurement brand
The brand line is "Clear psychometrics for the team you have"; the repeated memory is
*deterministic, computed-not-AI, auditable, grounded-not-invented*. An **auditable sentiment
score** is that brand expressed in the smallest possible unit — a single number — with a
**reproducible provenance stamp**, an **inspectable explanation** (`sentiment_match`), and a
**stated-limitations model card**. sentiment.ai becomes the public, MIT proof-of-character for
the whole company: *this is how we measure — you can check our work.* It is the credibility
artifact that makes the closed, commercial surfaces believable.

### B. The downstream text-graph / coherence package (`textgraph`)
`og-textgraph-design.md` already commits to the same discipline: *no invented index*, every
read maps to a published construct (Foltz/Kintsch/Landauer coherence, Halliday & Hasan
cohesion, Coh-Metrix panel philosophy), and it **reuses `embed_text()` + `cosine()` as the
engine.** The differentiator chains cleanly:
- sentiment.ai proves the *embedding engine* is reproducible, pinned, and offline.
- `textgraph` inherits that substrate and applies the *same* "grounded, not invented; a panel
  of cited reads, never one black-box score" rule to documents.
- **One sentence ties them:** *"Two packages, one auditable embedding engine — one turns text
  into a score you can defend, one turns it into a graph of reads you can cite."* That is a
  product line with a single, legible value (auditability), not two unrelated tools.
- **Concrete reuse to protect:** the provenance stamp (feature 1) should be a *shared* idiom —
  a `textgraph` result carries the same `{embedder, revision, prefix}` stamp, so a coherence
  number and a sentiment number computed in the same study are *provably on the same vectors.*
  That cross-package reproducibility is a real, ownable capability.

### C. The publications
The honest, publishable assets are *already* in the planning set and they are all
auditability stories, not accuracy stories:
- **The synthetic-neutral finding, told honestly** (`real-neutral-eval-plan.md`): "adding
  synthetic neutral training data improves *real* pos/neg F1 — measured on held-out *natural*
  neutral (Financial PhraseBank, SemEval/tweet_eval), not on the synthetic test set." That is
  a methods-contribution paper *because* it is evaluated rigorously, which is the brand.
- **The multilingual-scoping evaluation** (per-language F1 on
  `cardiffnlp/tweet_sentiment_multilingual`) — the first real cross-lingual evidence, framed
  as "here is exactly how far the multilingual claim goes."
- **The provenance/reproducibility argument itself** — a short note / blog: *"Why your
  sentiment numbers aren't reproducible, and what a pinned open pipeline fixes."* This is the
  thought-leadership piece that *names the differentiator* and lets the reader conclude the
  paid APIs can't do it (without you naming them — consistent with the no-namecheck rule).
- Provenance for all of it: **publish the harness**, date-stamp every number, label F1 as
  upper bounds. The publications *are* the audit trail; that's the point.

---

## What would be OFF-BRAND (the anti-pattern list)

These would *destroy* the differentiator faster than any competitor could. Grounded in the
existing reviews and the Linnet memory.

1. **Calling it "AI."** Never "AI sentiment," "AI you can trust," "AI-powered." The output is
   *computed* — embeddings + gradient boosting, deterministic. The brand is explicitly
   computed-not-AI-hype. Describing a competitor's API as "AI" is fine; describing your own
   output as AI is the cardinal sin (user has been angry about this repeatedly).
2. **A single invented "quality/emotion index."** No `sentiment_intensity_index`, no coined
   construct, no 0–100 mashup. sentiment.ai measures *one* thing on a defensible 3-class basis
   rescaled to [-1,1]; `textgraph` reports a *panel* of cited reads. Inventing a proprietary
   composite is exactly the slop the brand is defined against (no invented constructs).
3. **Emotion / personality / "tone" expansion without a citation.** Do not bolt on
   "emotion detection (joy/anger/fear)" or "Big-Five-from-text" to look feature-rich. Either
   it's grounded in a named, validated scheme with real eval, or it doesn't ship. Feature
   breadth that outruns evidence is the opposite of the differentiator.
4. **Accuracy leaderboard chest-beating.** "Beats every R sentiment package," "SOTA," a hero
   F1 number with no CI. Your own methodology review forbids the unqualified headline; making
   it the pitch contradicts the rigor that *is* the product.
5. **Namechecking the imitators / paid APIs as a put-down.** Reads insecure (Linnet rule).
   Let reproducibility + publish dates do the talking; say "closed APIs can't be pinned or
   inspected," never "unlike $COMPETITOR."
6. **Dressing `sentiment_match` similarity as causal explanation.** It is "nearest labelled
   anchor by cosine," which is honest and useful. "The model decided X because Y" overclaims.
   The auditability brand dies the instant the explanation is itself untrustworthy.
7. **Silent score changes.** Changing the default embedder/scorer without a major version bump
   and a loud NEWS entry breaks reproducibility — the one thing you're selling. Version
   discipline is *brand-critical*, not just hygiene (already flagged in `v2-roadmap.md`).

---

## The one-paragraph positioning (drop-in)

> sentiment.ai is the **auditable** sentiment package for R. It scores text from −1 to +1 with
> a modern, on-device, no-TensorFlow embedder and an open gradient-boosted scorer — so the
> score is **reproducible to the version**, **explainable per row** (it tells you the nearest
> labelled anchor and how close it is), and **computed entirely on your machine** with nothing
> sent to an API. Accuracy is at parity with paid embeddings on our benchmark (figures
> provisional, reported as upper bounds, harness public); what no paid API can give you is a
> number you can pin, inspect, and put in a methods section. The same engine powers a companion
> document-coherence package, so a sentiment read and a coherence read in the same study are
> provably computed on the same vectors. Clear, computed, checkable — not a black box you rent.

---

## If you do ONE thing for the tool

Ship the **provenance stamp** (feature 1): make `sentiment_score()` / `sentiment_match()`
carry a returnable, paste-into-a-paper record of `{package_version, embedder, embedder_revision,
scoring, scoring_version, prefix}`, and make the **prefix-as-scorer-property contract**
(`review-lineup.md` SEV-1: store the e5 prefix in the scoring-model metadata and assert
embedder-prefix == scorer-prefix at score time) the mechanism that *guarantees* the stamp is
true. That single accessor is the differentiator made tangible: it is the smallest possible
feature that no closed API can copy, and it's the literal API-level expression of
"trustworthy, computed, auditable." Everything else (explanation, offline, model card,
publications, textgraph reuse) hangs off it.
