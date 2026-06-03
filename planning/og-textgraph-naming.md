# Naming the OG text-graph package

Working doc, 2026-06-03. Goal: pick a CRAN-publishable name for the planned
linnet-family **text-network / semantic-coherence** R package (see linnet memory
`project_linnetlabs_text_package`; sentiment.ai v2 is its embedding+sentiment engine —
`v2-roadmap.md` Phase 8). Not a hype sheet — names ranked on differentiation, brand fit,
and *real* dual-registry availability.

---

## What the package actually is (so the name is honest)

Turns documents into **semantic networks + clusters**, and reads structure off the graph:
lexical **cohesion**, redundancy, thematic coverage, sentiment **overlaid on the graph**
(colour clusters by tone; find the uniformly-negative vs tonally-split theme). Grounded in
real literature — **LSA** (Landauer & Dumais 1997), **Coh-Metrix** (Graesser, McNamara,
Louwerse & Cai 2004; McNamara et al. 2014), **lexical cohesion** (Halliday & Hasan 1976) —
**not** an invented "quality index" (respects `feedback_no_invented_constructs`). The
embeddings + 3-class sentiment come from sentiment.ai (no-TF default; MIT, KFI-attributed).

Naming implications that follow directly:
1. The name should signal **graph/network of text**, ideally with a **cohesion/coherence**
   tilt — that combination is the differentiation, not generic "text mining".
2. **Monorepo constraint** (memory `project_linnetlabs_text_package` + the sentiment.ai
   PyPI scaffold): R **and** Python ship under the same brand. So the name should be free on
   **both** CRAN and PyPI, or the Python twin needs an explicit suffix. This kills several
   otherwise-clean CRAN names.
3. **No competitor namecheck / no trademark collision** (`feedback_no_competitor_namecheck`).
   This kills `cohere` outright.

---

## Differentiation vs the incumbents (the map)

| Package | Author / status | What it does | Gap we fill |
|---|---|---|---|
| **quanteda** | Benoit et al., CRAN | Bag-of-words / DFM text-analysis framework | No embeddings, no semantic graph, no coherence |
| **tidytext** | Silge & Robinson, CRAN | Tidy token/lexicon workflows, incl. lexicon sentiment | Token-level, not embedding-network; lexicon sentiment we beat |
| **text** (Kjell) | CRAN | Transformer embeddings → predict/analyse in R | Embeddings yes, but **no semantic-network / cohesion layer**, no graph-overlaid sentiment |
| **textnets** (Bail) | **GitHub-only, not on CRAN** | Document↔term bipartite networks + community detection | Co-occurrence networks, **not embedding-space**; no coherence metrics; no sentiment; no CRAN install |
| **stm** | Roberts/Stewart/Tingley, CRAN | Structural topic models | Topics not networks; no embeddings; no cohesion/coherence reads |
| **corpustools** | Welbers/van Atteveldt, CRAN | Token-index corpus search / co-occurrence | Lexical co-occurrence, not embedding-semantic graph or coherence |

**The wedge in one line:** *embedding-space* semantic networks **+** literature-grounded
**cohesion/coherence** reads **+** sentiment **overlaid on the same graph** — and a clean
`install.packages()`. No incumbent bundles all of that; `text` is closest but stops at
embeddings, `textnets` does networks but off co-occurrence, GitHub-only, no sentiment.
**That fusion is what the name should telegraph.**

---

## Availability — verified dual-registry (2026-06-03)

CRAN: `curl ... cran.r-project.org/package=NAME` **and** `/src/contrib/Archive/NAME/`
(404 archive = never removed, truly free). PyPI: `pypi.org/pypi/NAME/json` (true 404).
`200 = taken, 404 = free`.

| Candidate | CRAN | CRAN archive | PyPI | Verdict |
|---|---|---|---|---|
| textgraph | 404 free | 404 clean | **200 TAKEN** | CRAN-free but no Python parity |
| semnet | 404 free | 404 clean | **200 TAKEN** | CRAN-free but no Python parity |
| semgraph | 404 free | 404 clean | **200 TAKEN** | CRAN-free but no Python parity |
| cohere | 404 free | 404 clean | **200 TAKEN** | **DEAD — Cohere Inc. trademark + their SDK owns PyPI** |
| textnets | 404 free | 404 clean | **200 TAKEN** | Avoid — it *is* Bail's package name (collision/confusion) |
| linnet | 404 free | 404 clean | **200 TAKEN** | Python `linnet` exists; also clashes w/ `spatstat.linnet` mental model in R |
| textloom | 404 free | 404 clean | **200 TAKEN** | No parity |
| **docnet** | 404 free | 404 clean | **404 free** | Clean both |
| **embednet** | 404 free | 404 clean | **404 free** | Clean both |
| **corpusgraph** | 404 free | 404 clean | **404 free** | Clean both |
| **coherenet** | 404 free | 404 clean | **404 free** | Clean both |
| **sentigraph** | 404 free | 404 clean | **404 free** | Clean both |
| **textmap** | 404 free | 404 clean | **404 free** | Clean both |
| **cohgraph** | 404 free | 404 clean | **404 free** | Clean both |
| **textcohere** | 404 free | — | **404 free** | Clean both (evokes "coherence" w/o the trademark) |
| **semcohere** | 404 free | — | **404 free** | Clean both |
| **cohesio** | 404 free | — | **404 free** | Clean both (cohesion, brandable) |
| **docgraph** | 404 free | — | **404 free** | Clean both |
| **threadnet** | 404 free | — | **404 free** | Clean both |
| **verbanet** | 404 free | — | **404 free** | Clean both |
| linnettext | 404 free | 404 clean | 404 free | Clean both (brand-prefixed) |
| linnetlabs | 404 free | — | 404 free | Clean both (brand, but reads like an org not a pkg) |
| cohnet | 404 free | — | 404 free | Clean both (terse) |
| semweave | 404 free | — | 404 free | Clean both |

(`cohesionr`, `textcohesion`, `semcohesion`, `lexcohesion`, `weftnet`, `skeingraph`,
`latticetext`, `semloom`, `textfabric`, `semfabric`, `textmesh`, `semlattice`,
`cohesiongraph` also all free on both — kept as fallbacks, weaker on memorability or clarity.)

---

## Scoring rubric

Names judged on: (1) **honest fit** to what it does (graph + cohesion/coherence + sentiment
overlay), (2) **differentiation** from incumbents and no confusion with `textnets`/`text`,
(3) **dual-registry clean** (CRAN + PyPI), (4) **no trademark/competitor collision**,
(5) **brand fit / memorability** for an IO-HR-student audience.

Hard cuts: `cohere` (Cohere Inc. trademark — also the single biggest "don't" here),
`textnets` (Bail's actual package), `linnet` bare (taken on PyPI + R `spatstat.linnet`
overload), and anything PyPI-taken given the monorepo constraint (`textgraph`, `semnet`,
`semgraph`, `textloom`).

---

## RANKED SHORTLIST

| # | Name | CRAN | PyPI | One-line positioning |
|---|---|---|---|---|
| 1 | **coherenet** | free | free | Semantic-network + **coherence** reader for documents — the cohesion/coherence wedge is in the name, dual-registry clean, no trademark on "coherence" (vs the dead `cohere`). |
| 2 | **corpusgraph** | free | free | The corpus-as-graph package — most literal description of the engine, instantly legible to quanteda/text/textnets users, clean on both registries. |
| 3 | **embednet** | free | free | Embedding-space semantic networks (+ sentiment overlay) — names the actual differentiator vs co-occurrence `textnets`, short and clean both registries. |
| 4 | textcohere | free | free | Text coherence/cohesion networks — evokes "coherence" while sidestepping the Cohere trademark; slightly cuter than #1. |
| 5 | docnet | free | free | Document networks — clean and short, but generic (doesn't signal embeddings or coherence). |
| 6 | sentigraph | free | free | Sentiment-over-the-graph — true to the sentiment-overlay differentiator, but under-sells the coherence/structure half. |
| 7 | linnettext | free | free | Brand-prefixed fallback — guaranteed-unique, signals the linnet-family lineage, but leans on brand rather than describing the function. |

**Recommendation:** lead with **`coherenet`**. It is the only top option whose name carries
the *actual* differentiation — a network/graph view of text **plus** the literature-grounded
**coherence/cohesion** read (LSA / Coh-Metrix / Halliday & Hasan) — and it does so without
the `cohere` trademark landmine. `corpusgraph` is the safe, maximally-legible runner-up if a
plainer "this is a graph of your corpus" framing is preferred. `embednet` if the pitch should
foreground *embedding-space* networks as the thing that beats co-occurrence `textnets`.

**Flag for the user:** the monorepo (R + Python, one brand) is the binding constraint that
eliminated the otherwise-clean CRAN names `textgraph`, `semnet`, `semgraph` (all PyPI-taken).
If the Python twin is allowed a suffix (e.g. PyPI `textgraph-io` / `pytextgraph`), `textgraph`
re-enters as a strong, maximally-descriptive #1 — worth a yes/no before locking.
