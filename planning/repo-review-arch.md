# sentiment.ai — Architecture review (senior software architect lens)

> Written 2026-06-03. Planning-only; **writes to `planning/` only, touches no source.**
> Scope: the *overall design* — the swappable embed backend (`sentiment.env$embed`), the
> scoring-head abstraction (xgb/glm/…), R↔Python parity, extensibility toward the multi-head
> measurement platform (sentiment / custom-axis / readability), maintainability, and tech debt.
> Read first, opinions second: I read the full R embed+score+init path
> (`R/{embedding,sentiment,init_and_install,choose_model,constants,model_meta,object-sentiment_env,
> matrix_helpers,local_from_reticulate,onload}.R`), the canonical `inst/get_embedder.py` and the
> legacy `get_embedder_v2.py`, the Python scaffold (`pypackage/sentimentai/*`), `PARITY.md`, and the
> three adversarial reviews + `better-sentiment-roadmap.md`.
>
> **This doc does not re-litigate** the lineup (`review-lineup.md`), the migration breakage
> (`review-migration.md` S0–S7), the methodology (`review-methodology.md`), or the feature roadmap
> (`better-sentiment-roadmap.md`). Those are correct and I build on them. My job is the **structural**
> layer those reviews assume but don't design: *what are the seams, are they the right seams, and what
> is the target architecture that makes the multi-head platform cheap instead of a rewrite.*

---

## TL;DR for the impatient

The package has **one genuinely good architectural idea** (frozen embedder as a swappable function
pointer + a pure, portable light-scorer reading artifacts off disk) wrapped in **a procedural init
layer that hard-codes every assumption the good idea was supposed to make swappable** (the backend
type, the dimension, the prefix, the scorer path, the score collapse). The v2 work added the *data*
for a real abstraction (`constants.R`: `default_models`/`model_dims`/`model_prefix`/`model_class`;
`model_meta.R`: license/source/revision) but **never built the object that owns that data** — so the
same five facts about a model are re-derived, by different code, in `choose_model()`, `model_class()`,
`init_sentiment.ai()`, `install_scoring_model()`, `find_sentiment_score()`, and `embed_text()`. That
scatter *is* the tech debt, and it is the reason `review-migration.md` found three independent fatal
breaks on the new default path: they are three call sites that each re-derived "what is this model"
and disagreed.

The single highest-leverage structural move is to **introduce one `Backend`/`Model` descriptor object
(a registry row) that every other function takes as input instead of re-deriving** — in *both*
languages. Python already has exactly the right shape (`_models.Backend` dataclass + `resolve()`);
R has the data but not the object. Make R's `choose_model()` return that object, make every
downstream function consume it, and ~80% of the migration's class of bug becomes structurally
impossible. Everything else in this review hangs off that move.

---

## 1. The architecture as it actually is (the seam map)

Three layers, as `v2-roadmap.md` says — but the seams between them are leaky. Here is the real
data/control flow for `sentiment_score("good")` today (v2 branch, default `e5-small`):

```
sentiment_score(x)                                  [R/sentiment.R]
  ├─ match.arg(scoring)                              -- scoring head selected by string
  ├─ install_scoring_model(model, scoring, ver)     [R/init_and_install.R]
  │     └─ match.arg(model = c("en.large",...))      -- ❌ rejects "e5-small" (re-derives model set #1)
  ├─ check_sentiment.ai(model)                       -- lazy init guard
  │     └─ init_sentiment.ai(model)                  [R/init_and_install.R]
  │           ├─ source_python("get_embedder.py")
  │           ├─ choose_model(model)                 -- resolves name→id (re-derives model set #2)
  │           ├─ if openai → load_openai_embedding   -- ❌ only 2 branches; no `st` branch
  │           └─ else      → load_hub_embedding (TF) -- ❌ e5 id handed to TF-Hub loader
  │           └─ writes sentiment.env$embed          -- the swappable pointer (the GOOD idea)
  ├─ embed_text(x, batch_size, model)                [R/embedding.R]
  │     ├─ if sentiment.env$openai → openai_embed
  │     └─ else                    → hub_embed       -- ❌ assumes the pointer is a TF-hub callable
  │           └─ sentiment.env$embed(as_py_list(x))  -- NO prefix applied (re-derives nothing; drops it)
  └─ find_sentiment_score(embeddings, scoring, ver, model)   [R/sentiment.R]
        ├─ path = inst/scoring/<scoring>/<ver>/<model>  -- re-derives scorer location (model set #3)
        ├─ xgb.load(...) ; predict
        └─ probs[,3] - probs[,1]                    -- collapse 3-class → scalar (signal discarded)
```

**What's genuinely good and must be preserved:**

1. **`sentiment.env$embed` as a function pointer.** Decoupling "how do I turn text into vectors"
   from "how do I score vectors" is the correct primary seam. It is *why* v2 is "integration not new
   ML" (roadmap is right about this). Keep it.
2. **The scorer is pure and artifact-driven.** `find_sentiment_score()` is ~35 lines, no Python, no
   TF, reads a file off disk by `(scoring, version, model)` and runs `predict`. This is the portable
   core that makes R↔Python parity *achievable at all* (PARITY.md §0 leans entirely on this). Keep it.
3. **The artifact layout `scoring/<head>/<version>/<model>.<ext>`** is a good content-addressed-ish
   scheme: it already encodes the three axes that vary (head, version, embedding space). It is, in
   embryo, the multi-head platform's storage model. Keep and generalize it (see §5).

**What's wrong is not the layers — it's that the layers don't share a contract.** The embed backend,
the scorer, and the init machinery each independently answer "what is model `m`?" by re-deriving it
from the raw constant vectors. There is no object that says *"`e5-small` is: kind=st,
id=intfloat/multilingual-e5-small, dim=384, prefix='query: ', scorer=e5-small.xgb, license=MIT,
revision=…"* and gets passed down the call chain. So:

- `choose_model()` knows the **id** but throws away kind/dim/prefix.
- `model_class()` re-derives the **kind** from the same vectors, separately.
- `install_scoring_model()` re-derives the **valid model set** with a *stale, hard-coded* `match.arg`.
- `embed_text()` re-derives the **backend** from a boolean flag (`sentiment.env$openai`) instead of
  the kind, and has **no concept of the prefix at all**.
- `find_sentiment_score()` re-derives the **scorer path** and infers class-count from `length(preds)`.

This is the structural root cause of `review-migration.md`'s S0 (3 breaks), S5 (512 coupling), and S6
(prefix drop). They are not five bugs; they are **one missing abstraction, observed five times.**

---

## 2. Scoring-head abstraction — under-built (string dispatch, no head contract)

The scoring head is selected by `scoring = match.arg(c("xgb","glm"))` and dispatched by an
`if (scoring == "glm") … else …` inside `find_sentiment_score()`. This works for two heads but is
the wrong shape for the **multi-head measurement platform** (sentiment + custom-axis + readability),
because:

- **The head and the *task* are conflated.** `xgb`/`glm` are *estimator families*, not *measurement
  heads*. "Sentiment" is the task; "xgb" is how it's fit. A readability head or a custom-axis head is
  a *different output contract* (different classes, different rescale, different columns), not just a
  different estimator. The current `scoring` arg cannot express "readability via xgb" vs "sentiment
  via xgb" — both would collide on the same `(scoring, version, model)` path. **The platform needs a
  `task`/`head` axis the storage layout and dispatch do not currently have.**
- **The output transform is hard-wired per branch.** The glm branch does `(p_pos-0.5)*2`; the xgb
  branch does `probs[,3]-probs[,1]` (3-class) **or** `(preds-0.5)*2` (binary), chosen by inspecting
  `length(preds)`. Inferring the output contract from the *shape of the prediction* is fragile — a
  3-class readability head (low/med/high) would be silently treated as neg/neu/pos sentiment and
  collapsed with a sentiment rescale. The transform must be a **property of the head**, declared, not
  sniffed.
- **The 3-class signal is destroyed at the seam.** `probs[,3]-probs[,1]` throws away `prob_neu`,
  the entropy, and the pos/neg *intensities* — which `better-sentiment-roadmap.md` correctly
  identifies as the precondition for *every* differentiator (calibration, abstention, mixed-flag,
  agreement). Architecturally: **the scorer's seam returns the wrong type.** It should return the full
  probability vector (or a small struct), and the scalar collapse should be a *downstream, optional,
  named* transform — not baked into the one function every caller goes through.

So the head abstraction is simultaneously **over-coupled** (transform welded to estimator) and
**under-specified** (no task axis, no declared output contract). For two sentiment heads it's fine;
for a platform it's a rewrite waiting to happen unless restructured now while there are only two heads.

---

## 3. The embed backend — right seam, wrong dispatch, leaky abstraction

The `sentiment.env$embed` pointer is the right seam, but the way backends are *selected* and *called*
breaks the abstraction it's supposed to provide:

- **Dispatch is a boolean, not a type.** `embed_text()` branches on `sentiment.env$openai` (TRUE/FALSE).
  A two-state flag cannot represent three backend kinds (st / openai / legacy-tf), let alone a fourth
  (onnx, the Phase-6 north star) or a fifth (pure-R text2vec fallback). The moment you have >2
  backends, a boolean flag is the wrong representation — and v2 *already* has three. `model_class()`
  returns the right 4-valued kind; `embed_text()` should dispatch on **that**, not on a bool. (This is
  the direct cause of `review-migration.md` S0.1: the `st` kind has no branch because the dispatch
  vocabulary is binary.)
- **The pointer's *calling convention* is not part of the contract.** `hub_embed()` assumes the
  pointer takes `as_py_list(text)` and returns a TF tensor it transposes; `openai_embed()` assumes it
  takes one string and returns a list; the new `load_st_embedder` returns `f(text)->np.ndarray
  (n,dim)`. Three backends, three *incompatible* call/return shapes, and the code that adapts between
  them is smeared across `hub_embed`/`openai_embed`/`embed_text`. **A function pointer is only a clean
  seam if its signature is fixed.** Right now "the embed pointer" means three different signatures
  depending on which branch installed it. The abstraction leaks: callers must know which backend is
  live to call the pointer correctly.
- **The prefix lives outside the pointer.** `model_prefix["e5-small"]="query: "` is defined in
  `constants.R` and read **nowhere** (review-lineup SEV-1, review-migration S6). Architecturally the
  fix is not "remember to prepend it in `embed_text`" — it's that **the prefix is a property of the
  embedder, so it must be baked into the pointer at construction time**, exactly like `load_st_embedder(hf_id, prefix)` already does in `get_embedder.py`. Whoever builds the pointer owns the
  prefix; no downstream caller should be able to embed e5 without it, because they never see the raw
  model. (PARITY.md §3 and better-sentiment-roadmap D1/Move 3 both want this as a *guarantee* — that
  guarantee is only structural if the prefix is inside the closure, not a constant someone must
  remember to read.) `get_embedder.py` does this correctly; `init_sentiment.ai()` just never calls it.

Net: the embed seam is the right *idea*, executed as a leaky abstraction. The fix is to (a) dispatch
on `kind`, not a bool, and (b) make every backend's constructor return a pointer with **one fixed
signature** `f(character) -> matrix(n, dim)` with the prefix and normalization already inside it. Then
`embed_text` is backend-agnostic and the OpenAI/legacy/onnx branches collapse to "pick the constructor."

---

## 4. R↔Python parity — the parity spec is excellent; the *mechanism* of parity is undesigned

`PARITY.md` is genuinely strong: it correctly identifies that parity is *achievable because the heavy
components are shared artifacts* (same HF model, same `.xgb` file), tags every surface MATCH/SHAPE/
MAY-DIFFER, and nails the asymmetry (`""` special only in `sentiment_match`). As a **contract** it's
the best doc in the repo. But as an **architecture** there are three structural gaps:

1. **Parity is asserted by prose, not enforced by a shared artifact.** The spec says "same registry,
   same dims, same prefixes, same default" — but the R registry (`constants.R`) and the Python
   registry (`_models.py`) are **two hand-maintained copies of the same table**, already drifting:
   Python's `BACKENDS` omits the `text-embedding-3-large`/`ada-002` aliases R accepts (PARITY.md §2
   flags this), and the unknown-name behaviour differs (R warns-through "cowboy mode", Python raises
   `KeyError`). **Two registries that must agree but are maintained independently will diverge** —
   that's not a risk, it's a certainty, and it's the *same class of bug* as §1's intra-R scatter, now
   cross-language. The registry should be **one language-neutral data file** (a JSON/CSV
   `models.json` shipped in `inst/` and read by both R and Python), with the dataclass/named-vector
   being a thin typed view over it. Then dim/prefix/default/license/revision **cannot** disagree
   across languages because there is one source.
2. **There is no executable parity gate.** PARITY.md §10.9 *names* the golden cross-language test
   ("fixed corpus scored in R and Python, asserted to 1e-4") as "the regression gate that defines
   parity" — but it doesn't exist, and nothing structurally forces it to be run. A parity spec with no
   CI artifact is a wish. The architecture needs a committed **golden fixture**: `(model, scoring,
   text) → expected_score` as a CSV/JSON in a shared `tests/fixtures/`, generated once, asserted by
   *both* the R `testthat` suite and the Python `pytest` suite. That file *is* the parity contract made
   executable; the prose spec is its documentation.
3. **The Python scaffold collapsed the wrong thing.** Collapsing R's three setup functions into one
   `ensure_model()` is correct (PARITY.md §6 — install ergonomics are the sanctioned divergence). But
   the scaffold left the **core scorer pipeline** (`embed→xgb→rescale`) entirely as
   `NotImplementedError`, while the smoke test (per the brief) proved that exact pipeline works in
   Python with zero TF. The risk: the Python side reimplements the score collapse / NA handling /
   prefix slightly differently from R because it's writing from the prose spec rather than from a
   shared, tested kernel. **The collapse-and-rescale logic is ~10 lines of pure arithmetic that should
   live in a tiny shared spec the two ports implement against a shared fixture**, not be independently
   re-derived twice.

The parity *doc* is done; the parity *machinery* (one registry file, one golden fixture, one
asserted-in-both-CI gate) is the missing structure that makes the doc true and keeps it true.

---

## 5. Extensibility toward the multi-head platform — the current shape blocks it in 3 specific places

Vision (from the brief + roadmap): a multi-head measurement platform — sentiment + custom-axis +
readability — on **one embed engine + light scorers**, in R *and* Python. The good news: the
embed-once/light-head-many recipe is *exactly* the right architecture for this, and it's already
half-present. The blocking issues are precise and structural:

- **(a) No `task`/`head` axis in storage or dispatch (see §2).** The artifact path is
  `scoring/<scoring>/<version>/<model>` where `<scoring>∈{xgb,glm}`. A readability head and a
  sentiment head fit by xgb on e5-small vectors **collide on the same path**. The platform needs
  `<task>/<head>/<version>/<embedding-space>` (e.g. `sentiment/xgb/1.0/e5-small.xgb`,
  `readability/xgb/1.0/e5-small.xgb`). This is a *one-time* relayout while there's only sentiment in
  the tree; doing it after readability ships is a migration. **Do it now.**
- **(b) The output contract is implicit and sentiment-shaped.** Everything downstream of the scorer
  assumes "3 classes → scalar in [-1,1]". A readability head might be "ordinal 1–5 → scalar 0–100";
  `sentiment_match`'s custom-axis is "cosine to anchors → [-1,1]". These are **different output
  contracts**, and the current code expresses none of them as data — the transform is hard-coded in
  `find_sentiment_score`. A head needs to *declare* its output contract (`{classes, transform,
  range, columns}`) so the rich tidy output (better-sentiment Move 1) is generic across heads, not
  re-hand-written per task.
- **(c) `sentiment_match` is the platform's custom-axis head wearing a different costume.** This is
  the most interesting structural observation. `sentiment_match` *already is* a general zero-shot
  custom-axis scorer (better-sentiment D2 says so): embed text, embed anchor phrases, cosine-match,
  return nearest + similarity. But it's implemented as a **parallel monolith** to `sentiment_score`
  (own NA handling, own embedding call, own return shape) rather than as *another head over the same
  embed engine*. The platform wants: `embed(text)` once → fan out to {sentiment head, custom-axis
  head (=match), readability head} → unified tidy output. Today `sentiment_match` re-embeds, re-does
  NA logic, and even calls `sentiment_score(text_embed)` internally — the seams are crossed. **The
  custom-axis "head" should consume the same embedding matrix the sentiment head does**, not own a
  duplicate pipeline. Refactoring `sentiment_match` to sit *on* the head abstraction (rather than
  beside it) is what turns "two functions" into "a platform."

The encouraging part: none of this needs an encoder change or new ML (consistent with the roadmap's
"integration not ML"). It's a **storage relayout + a head descriptor + routing `sentiment_match`
through the head seam.** All three are cheap *now* and expensive after a second task ships.

---

## 6. Maintainability & tech debt (the concrete, named list)

Structural debt, ranked by how much it will hurt the platform (not re-listing review-migration's
behavioral breaks except where the *fix is structural*):

| # | Debt | Why it's structural | Fix |
|---|---|---|---|
| D1 | **Five re-derivations of "what is model `m`"** (§1) | one missing object → N drift points | `Backend` descriptor (Move 1) |
| D2 | **Two hand-maintained registries** (R `constants.R` ↔ Py `_models.py`) (§4.1) | guaranteed cross-lang drift | one `inst/models.json`, typed views over it (Move 1) |
| D3 | **`sentiment.env` is a mutable global singleton** | one live embedder per session; can't hold sentiment+readability backends at once; not thread-safe; test pollution (tests must set/reset it) | make the model an *object you pass*, not global state mutated in place; keep a session-default for convenience but allow explicit handles |
| D4 | **`library(parallel)` at top of `embedding.R`** + `Imports: parallel` attaches a base pkg the wrong way; `openai_embed_parallel` is dead (never called; both branches of the `if(parallel>2)` call the *serial* `openai_embed`) | dead code + namespace smell + a no-op branch pretending to be parallelism | delete `openai_embed_parallel` and the no-op branch; `parallel::` qualify; drop the `library()` call |
| D5 | **`Imports: openai`** (DESCRIPTION) is a dead dep (review-migration S4); OpenAI path is hand-rolled `httr` | declared-not-used; CRAN smell; install weight | drop from Imports |
| D6 | **`reticulate:::` internals vendored in `local_from_reticulate.R`** | necessary today (no public API) but it's a *liability surface*: ~150 lines tracking reticulate's private heuristic, must be re-synced when reticulate changes | acceptable *if* isolated behind one `resolve_python_env()` function and tested against the reticulate version in CI; the long-term escape is the Phase-6 no-Python path for the default backend |
| D7 | **`create_error_text()` is `cat()`-based** (per better-sentiment table-stakes #7) | not catchable, not classed, prints to console mid-pipeline | `cli::cli_abort()` + classed conditions; this is also a *parity* concern (Python should raise typed exceptions with the same taxonomy) |
| D8 | **`find_sentiment_score` infers class-count from `length(preds)==n*3`** (§2) | output contract sniffed from prediction shape; a 3-class non-sentiment head is mis-collapsed | head declares its class-count + transform |
| D9 | **Stale `.onLoad`/roxygen** (review-migration S7) — but note **`onload.R` currently has only `.onAttach`**, and it's already e5-correct; the stale `paraphrase`/`use`-default messaging S7 cites is *not in the file I read* | partially already fixed; verify no second `.onLoad` elsewhere and sweep roxygen `512`/`en.large` refs | doc sweep (low) |
| D10 | **`embed_topics`/`read_embedding`/`get_default_embedding` still key on package version + assume USE-shaped JSON** | the default-anchor cache for `sentiment_match` is a versioned-JSON side channel that predates the registry | fold into the head/registry model; or (PARITY.md §5) drop the precompute and always embed defaults (slower, identical, far simpler) |

The good news in this table: most debt is **localized to the init/embedding procedural layer** — the
pure scorer and the registry *data* are clean. The platform is reachable by **consolidating**, not
rewriting.

---

## 7. Over-built / speculative surfaces (cut or quarantine)

Being even-handed: some structure is *too much* for where the product is.

- **The whole `reticulate` install-method machinery** (`install_method_detect`, the conda/virtualenv
  failover in `.activate_env`, `check_virtualenv_py`, miniconda prompts) is a large, fragile surface
  that exists to solve "which Python env" — a problem the Python package *doesn't have at all* and the
  roadmap's Phase-6 ONNX/no-Python north star aims to delete for the default path. It's load-bearing
  *today* for the st/legacy backends, so don't rip it out — but **do not invest further in it**, keep
  it behind one `resolve_python_env()` function, and treat it as the thing the no-Python default
  eventually retires. Don't let it grow.
- **`openai_embed_parallel`** — dead, delete (D4).
- **The precomputed default-embedding JSON path** (`get_default_embedding`/`install_default_embeddings`
  /versioned JSON) is an optimization for one specific case (default anchors in `sentiment_match`) that
  adds a download, a version-keying scheme, and a `read_embedding` JSON-rownames dance. For the value
  it returns (skipping ~30 short embeds) it's over-built; PARITY.md §5 already says Python should just
  embed the defaults. **Strongly consider deleting it in R too** — it's pure complexity reduction with
  an identical result, and it removes a whole versioned-artifact axis.
- **`api_engine = "text-davinci-002"`/`"text-embedding-ada-002"` defaults scattered across
  `init_sentiment.ai`/`embed_text`/`load_openai_embedding`** with *different* defaults in each — three
  copies of the OpenAI defaults that disagree. Consolidate into the registry's openai row.

Conversely, the parts people might be tempted to cut but **should keep**: the artifact-path scheme
(generalize it, §5a), the `model_meta.R` license/source/revision registry (it's the spine of the
provenance differentiator D1/Move 3 — under-used today, but correctly built), and `cosine_match`
(it's the custom-axis head's engine *and* the OOD-warning engine per better-sentiment D4).

---

## 8. Target architecture

A single diagram of where this should land. Same recipe (embed once, light heads), but with the seams
made explicit and language-neutral.

```
                       ┌──────────────────────────────────────────────┐
                       │   inst/models.json   (ONE registry, shared)   │
                       │   per model: {name, kind, id, dim, prefix,    │
                       │   license, source_url, revision}              │
                       └───────────────┬──────────────────────────────┘
              R: thin typed view       │        Py: thin typed view
            (named vectors / S3)       │        (Backend dataclass)
                                       ▼
                        resolve(name) ──► Backend descriptor object
                                       │   (the ONE thing passed downstream)
            ┌──────────────────────────┼───────────────────────────────┐
            ▼                          ▼                                ▼
   ┌─────────────────┐      ┌────────────────────────┐       ┌──────────────────┐
   │ EMBED ENGINE    │      │ HEAD REGISTRY          │       │ PROVENANCE       │
   │ make_embedder(  │      │ task/head descriptors: │       │ stamp() from the │
   │   backend)      │      │  {task, est, version,  │       │ Backend + head + │
   │ → f(chr)->mat   │      │   classes, transform,  │       │ pkg version      │
   │   ONE signature │      │   range, columns}      │       │ (returnable)     │
   │  prefix+norm    │      │ artifact path:         │       └──────────────────┘
   │  baked inside   │      │  <task>/<head>/<ver>/  │
   │ kinds: st|openai│      │  <embedding-space>.<ext>│
   │  |legacy|onnx*  │      └───────────┬────────────┘
   └────────┬────────┘                  │
            │  X = embed(text)          │  load scorer for (task, head, ver, space)
            └──────────────┬────────────┘
                           ▼
                 score_head(X, head) ──► full probability/where-it-lands struct
                           │             (NOT pre-collapsed)
       ┌───────────────────┼───────────────────────────┬───────────────────────┐
       ▼                   ▼                           ▼                       ▼
  sentiment head     custom-axis head            readability head        (future heads)
  (xgb 3-class)      (= sentiment_match,          (xgb/ordinal)
  transform:         cosine-to-anchors)
  p_pos - p_neg      transform: margin
       └───────────────────┴───────────────────────────┴───────────────────────┘
                                       ▼
                        UNIFIED TIDY OUTPUT (generic over heads)
              text, value, prob_*, class, confidence, + provenance attr
                                       ▼
                   optional scalar collapse (named, downstream, per head)
```

Key properties of this target:
- **One descriptor flows down the call chain**; nothing re-derives "what is model `m`."
- **One embedder signature** `f(character)->matrix(n,dim)` with prefix+normalize *inside* — backend
  kind is the only thing that varies, and it's chosen by `kind`, not a bool.
- **Heads are data** (`{task, est, version, classes, transform, range, columns}`), so adding
  readability is "add a descriptor + train an artifact," not "edit `find_sentiment_score`'s if-tree."
- **The scorer seam returns the full vector**; the [-1,1] collapse is one named transform among many,
  not the only thing the scorer can emit. This *is* better-sentiment Move 1, expressed structurally.
- **`sentiment_match` is the custom-axis head**, consuming the same `X` the sentiment head does.
- **Provenance is a function of the descriptor + head**, so it's *guaranteed true* (D1/Move 3),
  not a separately-maintained string.
- **Registry + golden fixture are language-neutral files**, so R and Python can't drift.

This is not a rewrite. It's: (1) build the descriptor + json, (2) route everything through it, (3)
relayout the artifact path, (4) lift the collapse out of the scorer, (5) make `match` a head. The
pure scorer, the embed-pointer idea, and the artifact scheme all survive intact.

---

## 9. The 3–5 highest-leverage structural moves

Ordered by leverage × how much they de-risk the platform. Each is a *structural* move (it changes
seams), distinct from the *feature* moves in `better-sentiment-roadmap.md` — though they unblock those.

### Move A — One `Backend`/`Model` descriptor object, sourced from one language-neutral registry file. **[the keystone]**
Build the `Backend` descriptor in R (it exists in Python already as `_models.Backend`); make
`choose_model()` return it (not a bare string); make `embed_text`, `install_scoring_model`,
`find_sentiment_score`, and `init_sentiment.ai` **consume the descriptor** instead of re-deriving
kind/dim/prefix/scorer-path/license. Ship the registry as **`inst/models.json`** read by both
languages; the R named-vectors and the Python dataclass become thin typed *views* over it.
**Why #1:** this single move dissolves the entire class of bug `review-migration.md` found (S0's three
breaks, S5's 512, S6's prefix are all "a call site re-derived the model and got it wrong"), kills the
two-registry drift (§4.1/D2), and is the precondition for heads, provenance, and parity. Highest
structural compounding in the repo: ~5 scattered derivations → 1 object, in both languages.

### Move B — Make the embed seam a single fixed signature with the prefix baked in; dispatch on `kind`. **[fixes the leaky abstraction]**
Every backend constructor returns `f(character) -> matrix(n, dim)` with the model's prefix and
L2-normalize **inside the closure** (as `get_embedder.py::load_st_embedder` already does). `embed_text`
dispatches on `backend$kind ∈ {st, openai, legacy, onnx}` — not on the `sentiment.env$openai` bool —
and never touches the prefix or backend internals. Wire `model_class()`'s 4-valued kind into the
actual dispatch (it's tested but unused).
**Why:** turns the "right seam, wrong dispatch" embed layer (§3) into a real abstraction; makes it
*structurally impossible* to embed e5 without its prefix (the guarantee D1/Move 3 wants); makes adding
the ONNX/no-Python backend (Phase 6) a new constructor, not a new branch in three functions. Directly
closes review-lineup SEV-1 and review-migration S0.1/S6 at the architecture level, not the patch level.

### Move C — Promote the scorer to a declared **head** with a `task` axis; lift the [-1,1] collapse out of the scorer; relayout artifacts to `<task>/<head>/<version>/<space>`. **[unlocks the platform]**
Make a head a descriptor `{task, est(xgb|glm|cosine), version, classes, transform, range, columns}`.
`find_sentiment_score` becomes `score_head(X, head)` returning the **full** probability/where-it-lands
struct; the sentiment scalar collapse becomes one named transform applied downstream. Relayout
`inst/scoring/` to carry the `task` axis **now**, while sentiment is the only task.
**Why:** this is the move that makes "platform" cheap. It simultaneously (a) stops discarding the
3-class signal — *exactly* better-sentiment Move 1, but as a seam change so it benefits every head, (b)
gives readability/custom-axis a home that doesn't collide with sentiment, and (c) makes the unified
tidy output generic. Doing the artifact relayout before a second task ships avoids a migration later.

### Move D — Land the parity *mechanism*: one golden fixture asserted in both R and Python CI, plus fill the Python scorer kernel from the shared spec. **[makes parity real, not aspirational]**
Commit a shared `tests/fixtures/golden.csv`: `(model, scoring, text) → expected_score`, generated once
from the working R pipeline. Assert it to 1e-4 in *both* `testthat` and `pytest`. Implement the Python
`embed→score→collapse→NA` kernel (the smoke test already proved it runs) against that fixture, not
against prose. Reconcile the `resolve()`/`choose_model()` unknown-name behaviour (pick warn-through or
raise — *one*).
**Why:** PARITY.md is the best doc in the repo but a doc can't fail CI. The golden fixture is the parity
contract made executable; without it, the two ports drift exactly as the two registries did. This is
the cheapest insurance for the "R AND Python" half of the platform vision, and it gates Move A/B/C from
silently breaking parity.

### Move E — Provenance stamp as a function of the descriptor + head (cash in `model_meta.R`). **[brand moat, structural form]**
A `sentiment_provenance()` accessor / `attr(out, "provenance")` computed *from* the Backend descriptor
(license/source/revision already in `model_meta.R`) + the head descriptor + package version — so it is
**guaranteed true** because it's derived from the same objects that did the work, not a parallel string.
Assert at score time that the live embedder's prefix == the scorer's recorded prefix (review-lineup
SEV-1's "prefix-as-scorer-property").
**Why:** this is `better-sentiment-roadmap.md`'s headline differentiator (D1/Move 3) — "re-run in 2028,
identical number" — but it's *only* a real guarantee if it's structural. Moves A+C give it for free
(the descriptor and head already carry everything the stamp needs); E is the small surface that exposes
it. It's the smallest move with the largest brand compounding, and `model_meta.R` is already built and
waiting for a consumer.

**Sequencing:** A is the keystone — do it first; B and C both depend on the descriptor existing. A→B→C
is the structural spine (it also happens to deliver better-sentiment Move 1 as a side effect of C). D
runs in parallel and gates the rest from breaking parity. E is a thin cash-in once A+C land. None
require new ML or an encoder change — consistent with the roadmap's "integration, not ML."

---

## 10. One-paragraph verdict

The package's core architectural instinct is **correct and rare**: a frozen embedder behind a swappable
pointer feeding a pure, artifact-driven light scorer is *exactly* the right shape for a multi-head
measurement platform, and it's why v2 is integration rather than new ML. The problem is that the good
seam was never given a **contract**: there is no descriptor object that says what a model *is*, so five
different functions re-derive it and disagree (that scatter is the literal cause of the migration's
three fatal breaks), the embed pointer has three incompatible signatures depending on backend, the
prefix lives outside the closure that needs it, the scorer destroys the 3-class signal at the seam, the
head abstraction can't express a second task, and R and Python maintain two copies of a registry that
must agree. **Introduce one `Backend` descriptor + one language-neutral registry file, route everything
through it, give the scorer a declared `task`/head contract that returns the full probability vector,
and back the parity spec with a golden fixture asserted in both CIs.** Do those and the multi-head
platform (sentiment + custom-axis + readability, R + Python) becomes additive — a new descriptor and a
new artifact — instead of a rewrite, and the "auditable, computed-not-AI, reproducible" brand becomes a
*property of the types* rather than a promise in the docs.
```

---

### Appendix — facts verified while reviewing (so claims above are grounded, not assumed)

- `find_sentiment_score()` (not `find_sentiment_probs` — the roadmap/other reviews use the old name)
  is the live scorer; `R/sentiment.R:111` calls it, `:250` defines it. It **already** does the v2
  3-class collapse `probs[,3]-probs[,1]` and has a 512-free dim check at `:99-104` using
  `model_dims[[model]]` — so the §S5 512-passthrough is **partially fixed** in the tree I read (the
  `is.matrix` branch validates against `model_dims`, not a `==512` literal). The `numeric(512)` seed is
  gone from this version. (Earlier reviews predate this edit; flag the delta.)
- `inst/get_embedder.py` **is** the canonical v2 embedder (st + lazy-TF, `USE_TF=0/USE_TORCH=1`,
  `load_st_embedder(hf_id, prefix)` with prefix applied inside). `get_embedder_v2.py` at the package
  root is the **legacy/superseded** one (the `paraphrase/mpnet/instructorXL` dict, no e5) — the naming
  is inverted vs intuition and is itself a cleanup item (the "v2" file is the *old* one).
- `init_sentiment.ai()` still only branches openai-vs-`load_hub_embedding` (no `st` branch) and
  `install_scoring_model()` still has `match.arg(model = c("en.large",...))` — so the dispatch wiring
  (review-migration S0.1/S0.2) is genuinely still open; `model_class()` exists and is unit-tested
  (`test-model-class.R`) but **unused by the init path**. This is the gap Move A/B close.
- `onload.R` contains **only `.onAttach`**, and it is already e5-correct (no `paraphrase`/`use`-default
  text). The stale-`.onLoad` finding in review-migration S7 does not match this file — likely already
  fixed; verify no second onload elsewhere before acting on S7.
- `openai_embed_parallel` is defined (`R/embedding.R:293`) and **never called**; both branches of the
  `if(sentiment.env$parallel > 2)` in `embed_text` call the **serial** `openai_embed` — the parallel
  path is dead and the branch is a no-op. (D4.)
- `DESCRIPTION` is still `Version: 0.2.0` with `Imports:` including `openai` (dead) and `parallel`;
  `Korn Ferry Institute` is the funder (`fnd`) — keep that authorship intact per roadmap Open
  Decision #4.
