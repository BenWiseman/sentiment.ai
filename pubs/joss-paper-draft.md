---
title: 'sentiment.ai: Tunable, multilingual, multi-backend sentiment analysis in R without a TensorFlow dependency'
tags:
  - R
  - natural language processing
  - sentiment analysis
  - text mining
  - sentence embeddings
authors:
  - name: Ben Wiseman
    orcid: 0000-0000-0000-0000   # TODO: add your real ORCID before submission
    corresponding: true
    affiliation: 1
  - name: Steven W. Nydick
    orcid: 0000-0002-2908-1188
    affiliation: 2
  - name: Tristan Wisner
    affiliation: 1
affiliations:
  - name: Korn Ferry Institute
    index: 1
  - name: Independent contributor
    index: 2
date: 3 June 2026
bibliography: paper.bib
---

# Summary

`sentiment.ai` is an R package that turns sentence embeddings into three-class
(negative / neutral / positive) sentiment scores. Rather than matching words
against a fixed lexicon, it embeds each document with a transformer model and
then maps that embedding to a sentiment score with a small, pure-R scoring head
(a two-layer MLP or logistic classifier, stored as a bundled JSON file — no
`xgboost` and no TensorFlow at score time). This decoupling — a swappable
*embedding backend* feeding a lightweight, portable *scoring head* — lets the
same package score text on a student laptop with no GPU, score it through a paid
embedding API, or, for backward compatibility, use the legacy TensorFlow
Universal Sentence Encoder backend the original release shipped with.

Version 2 is a substantial rewrite of the engine first released in 2021
[@sentimentai2021]. The headline changes are:

- **TensorFlow-free by default.** The original release pinned TensorFlow,
  TensorFlow Hub, and `tensorflow-text` to specific versions, making
  installation the single largest source of user friction (especially on Apple
  Silicon). The v2 default backend is `multilingual-e5-small` running on-device
  via PyTorch with no TensorFlow; TensorFlow is demoted to an opt-in
  compatibility layer.

- **Tidy, calibrated output.** The new `sentiment()` verb returns a per-row
  data frame with `text`, `sentiment` (score in $[-1, 1]$), `prob_neg`,
  `prob_neu`, `prob_pos`, `class`, and `confidence`. The class probabilities are
  **temperature-scaled** (expected calibration error ≈ 0.015 for `e5-small` on
  held-out real text), so `confidence` means what it says and can be used to
  triage rows for human review.

- **Context-tunable matching.** `sentiment_match()` lets a user define what
  *positive* and *negative* mean for their domain via a set of poles; the same
  calibrated score is returned alongside a nearest-phrase explanation.

- **Diagnostic and validation surfaces.** `sentiment_diagnostics()` augments
  every scored row with entropy, a calibrated confidence band, a mixed-sentiment
  flag, and an out-of-distribution similarity signal. `sentiment_agreement()`
  compares model scores to human labels, returning Spearman correlation,
  percent agreement, quadratic-weighted Cohen's $\kappa$, Krippendorff's
  $\alpha$, and ICC(2,1) — the statistics needed to validate a scoring pipeline
  and quote a defensible number in a methods section.

- **Python sibling.** The same scoring heads ship as `sentimentai-py` on PyPI,
  with a forward pass verified bit-for-bit against the R package.

# Statement of need

R users analysing open-ended text — survey verbatims, reviews, social-media
posts, interview transcripts — have historically reached for lexicon-based tools
such as `syuzhet` [@jockers2017syuzhet], `sentimentr` [@rinker2019sentimentr],
and the `tidytext` sentiment lexicons (AFINN, Bing, NRC) [@silge2016tidytext].
These tools are fast and transparent, but they share well-documented limitations:
they score words from a fixed dictionary, so out-of-vocabulary and misspelled
tokens are missed, negation and context are handled only heuristically, and most
of the convenient lexicons are English-only. Transformer sentence embeddings
address the vocabulary and context problems directly, but wiring them up in R has
required either a heavy deep-learning toolchain or sending text to a commercial
API.

`sentiment.ai` exists to close that gap with an interface that is simple enough
for non-specialists while staying honest about its components. Three needs in
particular motivate v2:

1. **Installability.** The dominant complaint about v1 was the TensorFlow
   dependency. Removing TensorFlow from the default install path — while keeping
   it available for users who depend on it — is the difference between a tool
   students can install in one line and one they abandon.

2. **A neutral class.** Many R sentiment tools, and many benchmark corpora,
   collapse to a binary positive/negative distinction. Real organizational and
   survey text is full of genuinely neutral statements, and neutral instances are
   scarce in most training corpora. `sentiment.ai` models neutral explicitly as a
   third class and returns scores on $[-1, 1]$ with $0$ as a meaningful neutral
   point.

3. **Domain control.** "Positive" is not domain-invariant. `sentiment_match()`
   lets the analyst supply named example phrases (e.g. mapping "high quality"
   vs. "low quality" for product reviews) and scores text by similarity in the
   embedding space, so the polarity scale can be re-pointed without retraining.

# Key features

**Multi-backend embeddings behind one interface.** A single function call
(`sentiment_score()` / `sentiment_match()`) works across three classes of
embedding backend, selected by the `model` argument:

- *On-device, no TensorFlow (default):* `multilingual-e5-small` (384-D) and
  `multilingual-e5-base` (768-D), resolved through `sentence-transformers`
  [@reimers2019sentencebert]. The E5 models [@wang2024multilingual] cover roughly
  100 languages and require a `"query: "` text prefix, which the package adds
  automatically.
- *Paid API:* OpenAI `text-embedding-3-small` / `-3-large` / `ada-002`, for users
  who prefer a hosted embedder and have an API key.
- *Legacy / compatibility (opt-in):* the TensorFlow Universal Sentence Encoder
  models (`en`, `en.large`, `multi`, `multi.large`), loaded lazily only when
  requested via `install_sentiment.ai(legacy = TRUE)`. Each legacy model has a
  documented TF-free replacement.

Each backend has its own embedding width and its own pre-trained scoring head;
v2 removes the v1 assumption that every embedding is 512-dimensional and instead
selects the correct dimension and scoring model from a backend registry.

**TensorFlow-free by default.** TensorFlow and TensorFlow Hub move from required
dependencies to an optional compatibility layer. The default installation
provisions only a lightweight CPU Python environment (numpy +
`sentence-transformers`), avoiding the version-pinned TensorFlow stack that
caused most v1 installation failures.

**Tunable, three-class scoring.** `sentiment_score()` returns a numeric score in
$[-1, 1]$ from an `xgboost` head trained on a balanced three-class corpus.
`sentiment_match()` additionally returns, for each document, the nearest
user-supplied reference phrase and its cosine similarity, providing a transparent
*explanation* of the assignment and allowing the positive/negative anchors to be
redefined per domain.

**Multilingual coverage on-device.** Because the default backends are
multilingual sentence-transformers, non-English text is scored without a separate
model or API, on a CPU-only machine.

**Community-extensible scoring heads.** Scoring models are plain serialized
`xgboost` (or CSV-weighted GLM) artifacts keyed by backend and version, so the
community can contribute or swap scoring heads without touching the embedding
layer.

# Performance and provisional benchmarks

On an internal three-class benchmark drawn from public sentiment corpora
(macro-F1 over negative/neutral/positive on a held-out subsample of 3,255 test
documents; the full-corpus run is in progress and these figures are
**provisional**), the on-device `multilingual-e5-base` backend (macro-F1 ≈ 0.86)
matches the OpenAI `text-embedding-3-small` API backend (≈ 0.86) and clearly
exceeds the legacy TensorFlow USE-large model (≈ 0.79) that was the v1 default.
The smaller default, `multilingual-e5-small` (≈ 0.81), already surpasses the
legacy USE-large baseline while running on-device with no TensorFlow. We report
these as engineering benchmarks for backend selection, not as a peer-reviewed
evaluation; a hardened benchmark study with multiple seeds, confidence intervals,
classical/LLM baselines, and contamination checks is being prepared separately.

Separately, we observed that augmenting the training corpus with synthetic
neutral examples (to counter the scarcity of neutral instances in public corpora)
improved F1 on the positive and negative classes as well as the neutral class.
We flag this finding here only as motivation for the explicit neutral modelling;
its rigorous evaluation is out of scope for this software paper.

# Differences from prior R text tools

Compared with lexicon-based R packages (`syuzhet`, `sentimentr`, `tidytext`
lexicons), `sentiment.ai` is embedding-based rather than dictionary-based: it
handles out-of-vocabulary and misspelled tokens, captures context beyond
hand-crafted negation rules, and is multilingual out of the box. Compared with R
front-ends to large language or transformer models (e.g. `text`
[@kjell2023text]), `sentiment.ai` is narrower and lighter — it targets the
specific task of tunable three-class sentiment with a small classifier on top of
embeddings, runs CPU-only by default, and does not require a fine-tuned
classification transformer or a GPU. Compared with its own v1, the v2 engine adds
multiple swappable backends, drops the mandatory TensorFlow dependency, and
generalizes the scoring layer beyond a single fixed embedding width.

# Availability and license

`sentiment.ai` is released under the MIT license and developed at
<https://github.com/BenWiseman/sentiment.ai> with documentation at
<https://benwiseman.github.io/sentiment.ai/>. Development of the original package
was funded by and attributed to the Korn Ferry Institute.

# Acknowledgements

We thank the Korn Ferry Institute, which funded the original development of
`sentiment.ai`, and the contributors Fiona Lodge, Yu-Ann Wang, and Veronica Ge.

# References
