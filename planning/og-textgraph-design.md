# textgraph — design + API (the "OG idea")

> A small R package that turns a **document into a semantic-similarity network** of its
> sentences (embeddings from the `sentiment.ai` v2 / e5 engine) → **community detection**
> (themes) → a **panel of grounded document-quality reads** (coherence, redundancy,
> thematic coverage). Aimed at IO/HR students who already write a lot of open-text
> (survey comments, interview notes, competency models, performance reviews, policy docs)
> and have no principled way to ask *"is this document actually coherent?"*

Status: design spec, written 2026-06-03. Sibling to `sentiment.ai` (shares the embedding
engine). Author: Ben Wiseman. License intent: MIT, Korn Ferry Institute attribution kept
(same lineage as `sentiment.ai`).

---

## 0. The one-sentence pitch

`sentiment.ai` already turns text into embeddings and embeddings into *one* number
(sentiment). `textgraph` reuses the **same embedding engine** to turn text into a *graph*,
and the graph into a small, **literature-grounded panel of quality reads** — not a single
invented "quality score."

The hard intellectual discipline of this package: **every read maps to a published
construct.** We do not ship a black-box "document quality index." We ship a handful of
named, citable measures and show the user how to interpret each one.

---

## 1. What we are NOT building (anti-slop guardrails)

- **No invented index.** There is no `textgraph_quality_score()` that mashes three signals
  into one opaque 0–100. Each read is reported and interpreted on its own, exactly like
  Coh-Metrix reports a *panel* of indices rather than one number
  (Graesser, McNamara, Louwerse & Cai, 2004; McNamara et al., 2014).
- **No "AI" framing.** The output is *computed*, deterministic, and auditable — cosine
  similarities, a graph, a community partition, summary statistics. We describe it as a
  measurement instrument, never as "AI that reads your document."
- **No new psychometrics.** We re-implement *existing*, cited constructs on a *modern*
  embedding backbone; we do not coin new constructs. The novelty is the **substrate**
  (sentence-transformer embeddings + graph community detection), not the constructs.

---

## 2. Literature grounding (the panel is real, not invented)

Three established lines of work give us a defensible panel. Each maps to exactly one
function and one interpretation.

| Read | Construct | Grounded in | What it operationalises |
|---|---|---|---|
| **Coherence** | Local + global textual coherence | Foltz, Kintsch & Landauer (1998); Landauer, Foltz & Laham (1998) | LSA-style coherence = the **cosine between adjacent text segments**, averaged over the document. We compute the *same quantity* with modern e5 sentence vectors instead of an LSA space. Foltz et al. show this adjacent-sentence cosine predicts comprehension (r ≈ .93 in their heart-text reanalysis) and beats term-overlap. |
| **Redundancy / lexical cohesion** | Cohesion via repetition/reiteration; over-cohesion as a defect | Halliday & Hasan (1976), *Cohesion in English*; Coh-Metrix cohesion indices (McNamara, Graesser, McCarthy & Cai, 2014) | Near-duplicate sentence pairs (very high off-diagonal cosine) = lexical/semantic reiteration. *Some* cohesion is good; *too much* is padding/circularity. We report the **distribution**, not a verdict. |
| **Thematic coverage / structure** | Situation-model structure; topic segmentation | Coh-Metrix multilevel framing (Graesser et al., 2004); TextTiling (Hearst, 1997) for the segmentation lineage | Community detection on the similarity graph recovers **themes**. Coverage = how many themes, how evenly the document distributes across them, and whether themes are connected or fragmented (one giant blob vs. disconnected islands). |

**The honest framing we ship in the docs (verbatim guidance for the vignette/README):**

> These are *diagnostic reads*, not grades. High adjacent-sentence coherence (Foltz,
> Kintsch & Landauer, 1998) means consecutive ideas hang together; it does not mean the
> document is *good*. Low coherence can be a genuine defect **or** a deliberately
> wide-ranging document. Redundancy grounded in lexical cohesion (Halliday & Hasan, 1976)
> flags reiteration, which can be emphasis **or** padding. Interpret the panel together,
> with the text in front of you.

Why these three and not more: they are the three Coh-Metrix-class signals that fall out
*naturally and cheaply* from a single embedding pass + one graph, with a clean published
citation each. Anything beyond this (syntactic complexity, connectives, referential
overlap by part-of-speech) is Coh-Metrix's turf and needs a parser — out of scope for v1.

---

## 3. Data flow (one pass, one graph, one panel)

```
                                 ┌─────────────────────────────────────────────┐
 raw text  ──►  segment()  ──►   │  sentiment.ai::embed_text(model="e5-small")  │  reuse
 (1 doc, a       sentences       │  → n × d matrix, sentences in rownames       │  the
  char vector    (char vec)      └─────────────────────────────────────────────┘  engine
  or a file)                                       │
                                                   ▼
                            sentiment.ai::cosine(E, E)   → n × n similarity matrix S
                            (rescaled cosine; self-sim on the diagonal)
                                                   │
                          ┌────────────────────────┼────────────────────────────┐
                          ▼                         ▼                            ▼
                   build_textgraph()         coherence reads               redundancy reads
                   threshold/kNN sparsify    diag-offset of S              upper-triangle of S
                   → igraph object G         (adjacent-pair cosine)        (near-dup pairs)
                          │
                          ▼
                   detect_themes(G)          → community partition (igraph cluster_leiden)
                          │
                          ▼
                   textgraph_panel()         → assembles ALL reads into one tidy object
                          │
                          ▼
            print() / summary() / plot() / autoplot()  (ggraph)  +  as.data.frame()
```

Key property: **one embedding call, reused for everything.** The `n × n` cosine matrix `S`
is the single shared intermediate — coherence is its sub/super-diagonal, redundancy is its
upper triangle, themes are a graph built from it. No second model, no second pass.

---

## 4. Reuse of the `sentiment.ai` embedding engine (the load-bearing dependency)

`textgraph` does **not** re-implement embedding. It calls into `sentiment.ai` for exactly
two things, both already exported and stable:

1. **`sentiment.ai::embed_text(text, model)`** — returns an `n × d` numeric matrix with the
   original text in `rownames()`. This is *already* the node table we want: row *i* is the
   embedding of sentence *i*, and its name is the sentence text. (See `R/embedding.R`.)
2. **`sentiment.ai::cosine(x, y)`** — rescaled cosine similarity, faster than `text2vec::sim2`
   (see `R/matrix_helpers.R`). This is *already* our weighted adjacency builder.

Engine details `textgraph` inherits for free (do not re-solve):

- **Default model = `e5-small`** (`intfloat/multilingual-e5-small`, 384-D), on-device, no
  TensorFlow, ~100 languages. `e5-base` (768-D) for higher fidelity; `text-embedding-3-small`
  for the paid-API path. These are exactly `sentiment.ai`'s v2 `default_models` / `openai_models`.
- **The `query: ` prefix** the e5 family requires is handled *inside* `sentiment.ai`
  (`model_prefix` in `constants.R`). `textgraph` must NOT prepend it itself — it would
  double-prefix. We pass plain sentences and let the engine prefix.
- **Dimension-agnostic.** `textgraph` never hardcodes a width; it reads `ncol(E)`. (This is
  the exact bug v2 fixed in `sentiment.ai` — we inherit the fix by never assuming 512/384.)
- **Init/install is the engine's job.** `textgraph` calls `sentiment.ai::check_sentiment.ai()`
  (already exported) at the top of `text_to_graph()` so the user gets the engine's own clear
  install guidance, not a second confusing setup path.

Dependency declaration:

```r
# DESCRIPTION
Imports:
    sentiment.ai (>= 0.2.0),   # the embedding engine — REQUIRED, this is the whole point
    igraph (>= 1.3),           # graph + community detection
    data.table (>= 1.12.8)     # tidy assembly (same as sentiment.ai)
Suggests:
    ggraph, ggplot2,           # plotting (autoplot); package is fully usable without them
    tokenizers,                # better sentence segmentation than the regex fallback
    knitr, rmarkdown, testthat
```

`sentiment.ai` stays a hard `Imports`: without it there is no package. Everything else is
`Suggests` so a student can install and get numbers with a minimal footprint, then add
`ggraph` for pictures.

---

## 5. IP boundary (what's open vs. what stays held)

Same posture as `sentiment.ai` and the broader product line: **methods open, heavy assets
held.** Concretely for `textgraph`:

- **OPEN (MIT, in the package):** segmentation, graph construction, community detection
  wiring, the panel definitions, all three grounded reads, plotting, the full API surface.
  This is textbook method on a modern substrate — its value to students *is* its openness,
  and it drives `sentiment.ai` adoption (the engine is the dependency).
- **HELD (not in this repo):** (a) the **trained sentiment/scoring models** and synthetic-
  neutral training mix — those live in `sentiment.ai_training` and are the `sentiment.ai`
  IP, not `textgraph`'s; `textgraph` only ever touches *embeddings*, never the scorer.
  (b) Any **org-calibrated norms / benchmark thresholds** ("a competency model below this
  coherence is an outlier") — `textgraph` ships *no* magic cutoffs; it ships distributions
  and percentiles relative to the document itself. Norming against a real corpus of HR
  documents is a downstream, potentially commercial artifact and stays out of the OSS core.
- **The boundary line:** `textgraph` is a *graph + statistics* layer over an *embedding API*.
  It contains no proprietary weights. The embeddings come from third-party open models
  (e5) or a paid API (OpenAI) via `sentiment.ai`. So `textgraph` itself is cleanly,
  fully MIT-able with nothing to protect — the protectable assets are one layer down
  (scorer) and one layer up (norms), neither of which lives here.

---

## 6. Core API (functions + signatures)

Design rules: one obvious entry point (`text_to_graph()`), an S3 object (`textgraph`) that
prints/plots/coerces sensibly, and small composable internals a power user can reach.

```r
# ── 6.1  ENTRY POINT ────────────────────────────────────────────────────────
# Raw text in, fully-analysed `textgraph` object out. The 90% call.
text_to_graph <- function(text,                     # length-1 char (a document) OR
                                                    #   a char vector of pre-split units
                          model    = "e5-small",    # passed straight to sentiment.ai
                          unit     = c("sentence", "paragraph", "none"),
                          method   = c("knn", "threshold"),  # how to sparsify S into G
                          k        = 5,              # kNN: edges per node
                          cutoff   = 0.5,            # threshold: keep cosine >= cutoff
                          resolution = 1.0,          # Leiden resolution (theme granularity)
                          batch_size = 100,
                          ...)                       # forwarded to sentiment.ai::embed_text
# returns: object of class "textgraph" (see 6.5)

# ── 6.2  PIPELINE INTERNALS (exported, composable) ──────────────────────────
segment_text <- function(text,
                         unit = c("sentence", "paragraph", "none"))
# -> character vector of segments. Uses tokenizers::tokenize_sentences if available,
#    else a documented regex fallback. `unit = "none"` means caller pre-segmented.

embed_segments <- function(segments, model = "e5-small", batch_size = 100, ...)
# -> n x d matrix, segments in rownames. THIN wrapper over sentiment.ai::embed_text;
#    its only job is to guarantee init via sentiment.ai::check_sentiment.ai() and to keep
#    the e5 `query:` prefix the engine's responsibility (never prefix here).

similarity_matrix <- function(embeddings)
# -> n x n rescaled-cosine matrix S. Wraps sentiment.ai::cosine(embeddings, embeddings).

build_graph <- function(S,
                        method = c("knn", "threshold"),
                        k = 5, cutoff = 0.5,
                        drop_self = TRUE)
# -> igraph object G. Diagonal dropped; weights = cosine. kNN keeps each node's top-k
#    neighbours (symmetrised); threshold keeps all edges >= cutoff. Both are standard,
#    documented sparsifications of a dense similarity graph.

detect_themes <- function(G, resolution = 1.0)
# -> integer vector of community membership (named by segment), via
#    igraph::cluster_leiden(G, objective_function = "modularity",
#                           resolution_parameter = resolution).
#    Leiden over Louvain because it guarantees well-connected communities
#    (Traag, Waltman & van Eck, 2019).

# ── 6.3  THE GROUNDED PANEL (each read = one cited construct; see §2) ────────
read_coherence <- function(S, order = NULL)
# Foltz, Kintsch & Landauer (1998): mean cosine between ADJACENT segments in reading
# order (the super-diagonal of S). `order` lets the user supply the original sequence
# if `text` was reshuffled. Returns:
#   list(local = <mean adjacent-pair cosine>,           # local coherence
#        global = <mean off-diagonal cosine>,           # global cohesion (whole-doc)
#        per_gap = <numeric vector of each adjacent cosine>,   # for spotting the break
#        weakest_link = <index + the two sentences at the lowest-cosine gap>)

read_redundancy <- function(S, near_dup = 0.85)
# Halliday & Hasan (1976) lexical cohesion / reiteration, operationalised as high-cosine
# off-diagonal pairs. Returns:
#   list(redundancy = <fraction of upper-triangle pairs >= near_dup>,
#        mean_pairwise = <mean upper-triangle cosine>,
#        duplicate_pairs = <data.table: i, j, sim, sentence_i, sentence_j>)

read_coverage <- function(G, membership)
# Coh-Metrix multilevel structure (Graesser et al., 2004) + topic segmentation lineage
# (Hearst, 1997), via the community partition. Returns:
#   list(n_themes = <count>,
#        modularity = <igraph::modularity(G, membership)>,   # how cleanly themed
#        balance = <normalised entropy of theme sizes, 0..1>,  # even vs. lopsided
#        n_components = <connected components of G>,           # fragmentation
#        theme_table = <data.table: theme, size, exemplar_sentence>)
#    where `exemplar_sentence` = the segment with highest within-theme centrality
#    (igraph::strength), i.e. the most representative sentence per theme.

# ── 6.4  ASSEMBLY ───────────────────────────────────────────────────────────
textgraph_panel <- function(tg)   # tg: a `textgraph` object
# -> the three reads bundled into one tidy `data.frame` (one row per metric:
#    construct, citation, value, interpretation_hint). This is the printable scorecard.

# ── 6.5  S3 METHODS on class "textgraph" ────────────────────────────────────
# A `textgraph` object is a list with: $segments, $embeddings (n x d), $S (n x n),
# $graph (igraph), $themes (membership), $reads (the §6.3 outputs), $model, $call.
print.textgraph    <- function(x, ...)   # compact: n sentences, n themes, the panel table
summary.textgraph  <- function(object, ...)  # the full §6.4 scorecard + theme_table
as.data.frame.textgraph <- function(x, ...)  # one row per sentence: theme, in-theme
                                             #   centrality, coherence-gap-to-next
plot.textgraph     <- function(x, ...)   # base-graphics fallback (igraph::plot)
autoplot.textgraph <- function(object, ...)  # ggraph: nodes coloured by theme, edge
                                             #   alpha = cosine, layout = "fr"; needs ggraph
```

**Why this shape.** `text_to_graph()` is the single call a student needs. The internals are
exported so the same person can, three weeks later, swap `build_graph(method="threshold")`,
re-run `detect_themes()` at a new resolution, or feed a *precomputed* embedding matrix
straight into `similarity_matrix()` — without re-embedding. The S3 object means
`print`/`summary`/`autoplot` "just work," which is what gets a package adopted in a class.

---

## 7. The killer vignette (~15 lines, copy-paste runnable)

> Scenario a student instantly gets: *"I wrote a competency model / a survey free-text
> summary. Does it actually hang together, or is it three disconnected topics pretending to
> be one?"* The graph answers in one picture; the panel answers in three cited numbers.

```r
library(textgraph)

# One document — here, a competency model draft a student is reviewing.
doc <- "Effective managers communicate goals clearly to their teams.
        They give frequent, specific feedback that helps people grow.
        Strong communication builds trust across the group.
        Quarterly budgets must be filed in the finance portal by the 5th.
        Travel reimbursements require two levels of approval."

tg <- text_to_graph(doc, model = "e5-small")   # e5 on-device, no TensorFlow

summary(tg)        # the grounded panel:
#   coherence$local  = 0.71  (Foltz, Kintsch & Landauer 1998) — first 3 sentences cohere
#   coherence$weakest_link -> the jump from "trust" to "budgets" (the seam)
#   coverage$n_themes = 2, modularity = 0.48 — TWO themes wearing one hat:
#                       {communication/feedback/trust} vs {budgets/reimbursements}
#   redundancy        = 0.00 — no padding

autoplot(tg)       # two coloured clusters, a thin bridge between them:
                   # the "communication" cluster and the "admin" cluster are barely linked
```

The "oh, it's literally two documents" moment is the gut-punch: the student *sees* the seam
and *reads* the cited coherence drop at exactly that sentence boundary. That is the whole
product in fifteen lines.

---

## 8. Build order (smallest shippable first)

1. `segment_text` + `embed_segments` + `similarity_matrix` — get to an `S` matrix. (Half a
   day; it's three thin wrappers over the engine.)
2. `build_graph` + `detect_themes` — get to a `textgraph` object with themes. (`igraph` does
   the heavy lifting.)
3. The three `read_*` functions + `textgraph_panel` — the grounded panel. *This is the
   intellectual core; write the citations into the roxygen as you go.*
4. S3 `print`/`summary`/`as.data.frame`, then `autoplot` (Suggests-gated on `ggraph`).
5. The §7 vignette + a README that leads with the two-cluster picture.
6. Tests: deterministic fixtures (a known-coherent doc, a known-disjoint doc, a
   known-redundant doc) asserting the panel moves in the expected direction. No network in
   tests — fixture embeddings are checked in, or `e5-small` is run behind `skip_on_cran()`.

---

## 9. References (verify-on-write; do not paraphrase into claims beyond these)

- Foltz, P. W., Kintsch, W., & Landauer, T. K. (1998). The measurement of textual coherence
  with latent semantic analysis. *Discourse Processes, 25*(2–3), 285–307.
  — adjacent-segment cosine as the coherence operationalisation; r ≈ .93 vs comprehension.
- Landauer, T. K., Foltz, P. W., & Laham, D. (1998). An introduction to latent semantic
  analysis. *Discourse Processes, 25*(2–3), 259–284. — the LSA-space backdrop.
- Halliday, M. A. K., & Hasan, R. (1976). *Cohesion in English.* London: Longman.
  — lexical cohesion / reiteration; the grounding for the redundancy read.
- Graesser, A. C., McNamara, D. S., Louwerse, M. M., & Cai, Z. (2004). Coh-Metrix: Analysis
  of text on cohesion and language. *Behavior Research Methods, Instruments, & Computers,
  36*(2), 193–202. — the *panel-of-indices* philosophy + LSA sentence-overlap measures.
- McNamara, D. S., Graesser, A. C., McCarthy, P. M., & Cai, Z. (2014). *Automated Evaluation
  of Text and Discourse with Coh-Metrix.* Cambridge University Press. — the book-length
  validation that cohesion indices distinguish high/low-cohesion text and track writing
  quality.
- Hearst, M. A. (1997). TextTiling: Segmenting text into multi-paragraph subtopic passages.
  *Computational Linguistics, 23*(1), 33–64. — the topic-segmentation lineage behind the
  coverage read.
- Traag, V. A., Waltman, L., & van Eck, N. J. (2019). From Louvain to Leiden: guaranteeing
  well-connected communities. *Scientific Reports, 9*, 5233. — why `detect_themes` uses Leiden.
