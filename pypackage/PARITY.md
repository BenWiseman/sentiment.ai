# sentiment.ai — R ↔ Python parity spec (v2)

> **Scope.** This document specifies, function by function, how the Python package
> (`sentimentai`) must behave so that it is **1:1 with the R package (`sentiment.ai`)
> v2** for everything that is *load-bearing* — i.e. the **numbers a user gets out**.
> It also states explicitly where the two packages are *allowed* to diverge: namely
> **install/setup ergonomics**, because Python has direct deps and therefore none of
> R's `reticulate`/conda/virtualenv environment dance.
>
> Status when written (2026-06-03): R v2 is mid-migration (TF demoted to opt-in,
> e5 default, registry in `R/constants.R` + `R/choose_model.R`). Python side is a
> scaffold (`NotImplementedError`). This spec is the contract the Python bodies are
> filled in against, and the checklist the R side is held to where it still lags
> (e.g. the hardcoded `512` passthrough — see §4 and §10).
>
> Authoritative R sources: `rpackage/sentiment.ai/R/{sentiment,embedding,
> init_and_install,choose_model,constants,object-sentiment_env}.R`.
> Authoritative Py sources: `pypackage/sentimentai/{sentiment,embedding,install,
> _models}.py`.

---

## 0. Parity tiers — what "1:1" means here

Every behaviour below is tagged with one of three tiers:

- **[MATCH]** — must be bit-for-bit / value-for-value identical across languages
  (up to floating-point tolerance, see §9). These are the **scores and the scored
  shapes**. A divergence here is a bug.
- **[SHAPE]** — the *container* differs by language idiom (R `data.table` vs pandas
  `DataFrame`; R named numeric vector vs numpy array), but the **values, column
  names, ordering, and NA semantics inside it must match**.
- **[MAY-DIFFER]** — deliberately allowed to diverge. Install/setup ergonomics,
  progress/log chatter, parallelism, and the existence of `reticulate`-specific
  machinery. These are *explicitly not* parity surfaces.

The single hard rule: **given the same `model`, the same `scoring`/`scoring_version`,
and the same input text, `sentiment_score()` must return the same numbers in R and
Python.** Everything else is in service of that, or is allowed to differ.

### Why scores *can* match exactly
The score is a pure pipeline with no language-specific math:

```
text ──embed──▶ X (n × dim float matrix) ──xgb/glm──▶ p (P[positive], in [0,1])
                                                      └─ rescale: score = (p − 0.5) · 2  ∈ [−1, 1]
```

- The **embedder** is the *same artifact* in both languages: the same HuggingFace
  model id (`intfloat/multilingual-e5-small`, etc.) loaded by the same
  `sentence-transformers` library (R reaches it through `reticulate`; Python imports
  it directly — same Python wheel underneath). For `openai`, it is the same API model
  id hitting the same endpoint.
- The **scorer** is the *same `.xgb` / `.csv` artifact* downloaded from the same
  GitHub location, loaded by the same xgboost predictor (`xgb.load`/`Booster`).
- The **rescale** `(p − 0.5) · 2` is trivial arithmetic.

So parity is achievable *because the heavy components are shared artifacts*, not
reimplementations. Python must **never reimplement** the embedder or the scorer in a
way that changes outputs; it loads the identical files.

---

## 1. Public surface (the five entry points)

| Concept | R (`sentiment.ai`) | Python (`sentimentai`) | Tier |
|---|---|---|---|
| Embed text → matrix | `embed_text()` | `embed_text()` | [MATCH] values; [SHAPE] container |
| Score sentiment → −1..1 | `sentiment_score()` | `sentiment_score()` | **[MATCH]** |
| Score + nearest-phrase explanation | `sentiment_match()` | `sentiment_match()` | [MATCH] sentiment/similarity; [SHAPE] table |
| Provision embedder + scorer | `install_sentiment.ai()` + `init_sentiment.ai()` + `install_scoring_model()` | `ensure_model()` | **[MAY-DIFFER]** |
| Model registry | `default_models` / `openai_models` / `legacy_models` / `model_dims` / `model_prefix` (`R/constants.R`) + `choose_model()` | `BACKENDS` / `DEFAULT_MODEL` / `resolve()` (`_models.py`) | [MATCH] contents; [SHAPE] form |

The Python package collapses R's **three** setup functions
(`install_sentiment.ai` → `init_sentiment.ai` → `install_scoring_model`, plus the
internal `check_sentiment.ai`) into a **single** `ensure_model()`. This collapse is
*intentional and allowed* — see §6.

---

## 2. Model registry — `BACKENDS` / `default_models`

**Must match: the set of user-facing model names, each one's dim, each one's prefix,
each one's backend kind, and the default.** [MATCH]

### Canonical name → (kind, hf/api id, dim, prefix, needs_tf)

| user name | kind | embedder id | dim | prefix | needs TF | tier |
|---|---|---|---|---|---|---|
| `e5-small` **(DEFAULT)** | sentence-transformers | `intfloat/multilingual-e5-small` | 384 | `query: ` | no | [MATCH] |
| `e5-base` | sentence-transformers | `intfloat/multilingual-e5-base` | 768 | `query: ` | no | [MATCH] |
| `openai` | openai (API) | `text-embedding-3-small` | 1536 | `` | no | [MATCH] |
| `text-embedding-3-large` | openai (API) | same | 3072 | `` | no | [MATCH] |
| `text-embedding-ada-002` | openai (API) | same | 1536 | `` | no | [MATCH] |
| `en.large` | tfhub-legacy | `universal-sentence-encoder-large/5` | 512 | `` | **yes** | [MATCH] |
| `en` | tfhub-legacy | `universal-sentence-encoder/4` | 512 | `` | **yes** | [MATCH] |
| `multi.large` | tfhub-legacy | `universal-sentence-encoder-multilingual-large/3` | 512 | `` | **yes** | [MATCH] |
| `multi` | tfhub-legacy | `universal-sentence-encoder-multilingual/3` | 512 | `` | **yes** | [MATCH] |

`DEFAULT_MODEL == "e5-small"` in **both** (`R/constants.R`; `_models.py`). [MATCH]

> **Registry reconciliation gaps to fix (not yet 1:1):**
> 1. R's `_models` exposes `openai` as a *single* entry resolving to `3-small`, plus
>    the raw `text-embedding-3-*` ids via `openai_models`. Python's `BACKENDS` has
>    `"openai"` but omits the explicit `text-embedding-3-large` / `ada-002` /
>    `text-embedding-3-small` aliases that R's `choose_model()` accepts. **Python
>    `resolve()` must accept the same aliases R's `choose_model()` accepts** (both
>    the friendly key *and* the raw id), or scripts that name `text-embedding-3-large`
>    will work in R and fail in Python. Add the missing OpenAI aliases to `BACKENDS`.
> 2. R `choose_model()` has **"cowboy mode"**: an unknown name passes through with a
>    `warning()` rather than erroring. Python `resolve()` currently **raises**
>    `KeyError`. Decide one behaviour and match it (recommendation: keep R's
>    pass-through-with-warning so a power user can name an arbitrary HF id; Python
>    should `warnings.warn(...)` and return a synthesized `Backend`, **not** raise).
>    This is a [MATCH] surface — same input must not error in one language and succeed
>    in the other.

### `resolve()` ↔ `choose_model()` contract
- Input: a user-facing model name (`str`).
- Output: enough to (a) pick the embedder backend, (b) know `dim`, (c) know `prefix`,
  (d) pick the matching scorer artifact name. In R this is split (`choose_model()`
  returns the resolved id; `model_dims`/`model_prefix`/`model_class` give the rest).
  In Python `resolve()` returns a single `Backend` dataclass carrying all of it.
  **[SHAPE]** — different return form, identical information.

---

## 3. `embed_text()`

Turns text into an `n × dim` float matrix. Power-user surface; most users call
`sentiment_score()` instead.

### R signature (`R/embedding.R`)
```r
embed_text(text,
           batch_size    = NULL,        # NULL => one batch of length(text)
           model         = NULL,        # NULL => falls through to init default
           api_key       = NULL,
           api_base      = "https://api.openai.com",
           api_version   = "v1",
           api_type      = NULL,        # "azure" triggers azure URL/header form
           api_engine    = "text-embedding-ada-002",
           request_limit = 3000,        # OpenAI: max requests/min
           token_limit   = 300000)      # OpenAI: max tokens/min
```
Returns a numeric **matrix**, `nrow = length(text)`, `ncol = dim`, with the **original
text stored as `rownames`** (`rownames(text_embed) <- text`).

### Python signature (target)
```python
embed_text(
    text: str | Sequence[str],
    model: str = DEFAULT_MODEL,
    batch_size: int | None = None,      # None => single batch
    *,
    api_key: str | None = None,
    api_base: str = "https://api.openai.com",
    api_version: str = "v1",
    api_type: str | None = None,
    api_engine: str = "text-embedding-ada-002",
    request_limit: int = 3000,
    token_limit: int = 300000,
) -> np.ndarray            # shape (n, dim), float32/float64
```

> **Argument-order divergence to fix.** R's positional order is
> `(text, batch_size, model)`; the *internal* R callers invoke it positionally as
> `embed_text(x, batch_size, model)` (see `sentiment.R` line 96 / 194). Python's
> scaffold currently has `(text, model, batch_size)`. **Keyword calls are unaffected;
> positional calls would diverge.** Resolve by making Python keyword-only after
> `text` (shown above with `*`), OR matching R's positional order. Pick one and lock
> it. [MATCH] for the resolved-value, [MAY-DIFFER] only for the *call convention* as
> long as documented.

### Behaviour — both languages [MATCH on values]
1. Resolve `model` via the registry (§2). Apply the model's **`prefix`** to every
   string before embedding (e5 → `"query: " + text`). **This is load-bearing for
   scores** — embedding e5 without the prefix changes the vector and therefore the
   score. Both languages **must** prepend the prefix; neither may skip it.
2. `batch_size = None/NULL` ⇒ embed all of `text` in one call. Otherwise chunk into
   `ceil(n / batch_size)` batches, embed each, concatenate **in input order**.
3. Backend dispatch by `kind`:
   - `sentence-transformers` → `SentenceTransformer(id).encode(...)` (Python: direct;
     R: same call via reticulate). Default backend.
   - `openai` → POST to `{api_base}/{api_version}/embeddings` (or the azure
     deployment URL when `api_type == "azure"`), one row per input, respecting
     `request_limit` / `token_limit` (sleep 60s on limit). Token estimate is
     `nchar / 3` in R — Python must use the **same** estimate to throttle identically
     (throttling doesn't change outputs, but keep it identical to avoid behavioural
     surprises). [MAY-DIFFER] on parallelism only.
   - `tfhub-legacy` → only reachable if TF is installed (opt-in). Python may **defer**
     this backend (raise a clear "legacy USE requires the optional TF extra"); R keeps
     it working lazily. The *vectors* must still match if both are run. [MATCH] if
     present.
4. **Return value [SHAPE].** R returns a matrix with text in `rownames`. Python
   returns a 2-D `np.ndarray` (no row labels in numpy). To preserve the information
   that R carries in `rownames`, Python's internal scorer path must keep the
   text↔row association separately (it already has `text` in hand). The **numeric
   block must be identical** row-for-row, column-for-column, in input order.

### NA / empty handling [MATCH]
`embed_text` itself does not special-case NA in R (its callers do — see §4/§5).
Python `embed_text` should mirror that: it embeds what it's given. NA/empty handling
lives in `sentiment_score` / `sentiment_match`.

---

## 4. `sentiment_score()` — the core [MATCH] surface

This is the function whose output **must be identical** across languages.

### R signature (`R/sentiment.R`)
```r
sentiment_score(x          = NULL,
                model      = names(default_models),   # default => "e5-small"
                scoring    = c("xgb", "glm"),         # match.arg => default "xgb"
                scoring_version = "1.0",
                batch_size = 100,
                ...)
```

### Python signature (target — align defaults to R)
```python
sentiment_score(
    x: str | Sequence[str] | np.ndarray | None = None,
    model: str = DEFAULT_MODEL,         # "e5-small"
    scoring: str = "xgb",               # "xgb" | "glm"
    scoring_version: str = "1.0",
    batch_size: int = 100,
    **kwargs,
) -> np.ndarray | None                  # float array length n, values in [-1, 1]
```

> **Scaffold gaps to fix in Python:** add `scoring_version="1.0"` (currently missing);
> default `batch_size` is `100` in R, not the scaffold's other values. Keep `scoring`
> default `"xgb"`.

### Behaviour contract — every step is [MATCH]
1. **`x is None` ⇒ return `None`.** R returns `NULL` after ensuring the scoring model
   is installed (it still triggers `install_scoring_model` as a side effect; Python's
   `ensure_model` side effect is [MAY-DIFFER] but the **return must be `None`**).
2. **Input polymorphism — text vs pre-computed embedding.** [MATCH]
   - If `x` is text (`str`/sequence of str): embed it via `embed_text` (applying the
     model prefix, §3).
   - If `x` is **already an embedding matrix**, pass it straight to the scorer
     **without re-embedding**. R currently gates this on `is.matrix(x) && ncol(x)==512`
     — a **legacy bug**: it only accepts the 512-D USE width and silently mis-handles
     384/768/1536-D matrices. **Python must gate on `ncol(x) == dim(model)`** (the
     registry dim), and **R must be fixed to the same** (v2 roadmap Phase 2). The
     parity rule: *both accept a pre-computed matrix iff its width equals the selected
     model's dim.* Do **not** copy R's hardcoded 512.
3. **Missing-value handling.** [MATCH]
   - Record `na_index = which(is.na(x))`.
   - For text input, R replaces NA entries with their index-as-string so the embedder
     gets *something*, then **overwrites the final score at those positions with NA**.
     Python must reproduce: NA/`None`/`nan` rows ⇒ output is `NaN` at exactly those
     positions, all other positions unaffected.
   - For matrix input, R zero-fills NA rows before scoring then re-NAs them; Python
     same.
4. **Scoring.** [MATCH] — `find_sentiment_probs()`:
   - `scoring == "xgb"`: load `{model}.xgb` (`xgb.load` in R; `xgboost.Booster` in
     Python) and `predict` → `p` = P[positive]. **Same artifact file** ⇒ same `p`.
   - `scoring == "glm"`: read `{model}.csv` (param,name → weight), prepend a `1`
     intercept column to `X`, compute `p = 1 / (1 + exp(-(Xβ)))`. The CSV column
     order and the intercept-first convention are load-bearing — Python must replicate
     `embeddings <- cbind(1, embeddings)` (intercept **first**) exactly.
   - Scorer artifact is selected by `(scoring, scoring_version, model)` and lives at
     `scoring/{scoring}/{scoring_version}/{model}.{xgb|csv}`. Same path layout, same
     file, both languages.
5. **Rescale.** `score = (p − 0.5) · 2`. [MATCH] — identical formula, gives `[-1, 1]`.
6. **Re-insert NA** at `na_index`. [MATCH]
7. **Return.** [SHAPE] R returns a **named numeric vector** (`setNames(probs,
   rownames(embeddings))` → names are the input text). Python returns a plain
   `np.ndarray` (1-D, length `n`). The **values and order are [MATCH]**; the *names*
   are R-idiomatic and Python need not attach them (a power user wanting them can zip
   with the input). If Python wants to preserve them, return a pandas `Series` indexed
   by text — but the **default return is a bare float array** to stay numpy-clean.

### Worked invariant (use as a cross-language test)
```
sentiment_score("I love this", model="e5-small", scoring="xgb")
  R   →  named num [1]  "I love this" = 0.87…
  Py  →  array([0.87…])
```
Same artifact + same prefix + same rescale ⇒ same number (tolerance §9).

---

## 5. `sentiment_match()`

Score **plus** a nearest-phrase explanation against tunable poles. Lets the caller
define what positive/negative *mean* per domain via `phrases`.

### R signature (`R/sentiment.R`)
```r
sentiment_match(x          = NULL,
                phrases    = NULL,                    # named list; NULL => package default poles
                model      = names(default_models),   # "e5-small"
                batch_size = 100,
                ...)
```
Returns a **`data.table`** with **exactly these columns, in this order**:
`text, sentiment, phrase, class, similarity`.

### Python signature (target)
```python
sentiment_match(
    x: str | Sequence[str] | None = None,
    phrases: Mapping[str, Sequence[str]] | None = None,
    model: str = DEFAULT_MODEL,
    batch_size: int = 100,
    **kwargs,
) -> "pandas.DataFrame | None"
```
Returns a **pandas `DataFrame`** with the **same five columns, same order, same
names**: `text, sentiment, phrase, class, similarity`. [SHAPE]

### Behaviour contract
1. **`x is None` ⇒ return `None`/`NULL`.** [MATCH]
2. **NA / empty handling.** [MATCH] R tracks `na_index` (NA) and `x_index` (empty
   string `""`), substitutes tokens for embedding, then **NA-blanks** `sentiment`,
   `similarity`, `phrase`, `class` and restores the original text for those rows.
   Python must reproduce: rows that were NA or `""` get `NaN` sentiment/similarity and
   `None`/`NaN` phrase/class, with original text preserved.
3. **`phrases`.** [MATCH on meaning]
   - `None` + a built-in model ⇒ use the **shared default poles**, bundled in both
     packages as `default_poles.json` (a curated, balanced 40/40 set, byte-identical
     across R and Python). Both packages embed them **live on-device** and cache the
     result for the session — there is no pre-computed default-embedding download on
     either side (the old `get_default_embedding()` path is gone). [MATCH on poles + result]
   - Provided ⇒ a `dict[str, list[str]]` (e.g. `{"positive": [...], "negative": [...]}`,
     or any domain poles like `{"high quality": [...], "low quality": [...]}`). Embed
     each pole's phrases, build a `(phrase, class)` lookup, **de-dup on `phrase`**.
4. **Matching.** [MATCH] cosine-similarity each input embedding against every
   reference-phrase embedding; keep **rank-1** (nearest) phrase per input
   (`cosine_match(..., keep_target_order = TRUE)` → `rank == 1`). `similarity` is that
   top cosine. Join back to the lookup to attach `class`. **Re-impose input order**
   (R sorts/sets keys then `setorderv(..., "temp_id")`; Python must return rows in
   original input order).
5. **`sentiment` column.** [MATCH] — it is exactly `sentiment_score(text_embed)` on
   the **already-computed** embeddings (R calls `sentiment_score(text_embed)`, reusing
   the matrix so it is **not embedded twice**). Python must likewise compute the
   embedding once and pass the matrix into the scorer. The `sentiment` values here are
   identical to a standalone `sentiment_score()` call on the same text/model.
6. **Return columns & order.** `text, sentiment, phrase, class, similarity` — exact,
   both languages. [SHAPE container, MATCH content]

> **Note (default model has no `512` issue here):** `sentiment_match` embeds fresh
> from text, so the §4 512-gate doesn't bite it — but it does call `sentiment_score`
> on a matrix internally, so once §4's dim-gate is fixed, this path is correct for all
> dims. Until fixed in R, `sentiment_match` on a non-512 model relies on the text
> branch and the internal matrix call; verify the matrix call respects model dim.

---

## 6. Install / provisioning — `ensure_model()` vs the R trio **[MAY-DIFFER]**

**This is the one surface where divergence is intended and correct.** R's setup is
dominated by `reticulate` environment management (conda/virtualenv creation, Python
version pinning, `RETICULATE_PYTHON` hazards, Apple-Silicon TF special-casing,
session restarts). **None of that exists in Python** — `sentence-transformers`,
`xgboost`, `numpy`, `requests` are ordinary `pip` dependencies declared in
`pyproject.toml`. So Python has no `install_sentiment.ai()` analogue and **should not
grow one.**

### R surface (`R/init_and_install.R`)
| R function | what it does | Python equivalent |
|---|---|---|
| `install_sentiment.ai(envname, method, gpu, python_version, modules, fresh_install, restart_session, ...)` | Creates a conda/virtualenv env, pip-installs pinned Python deps, then downloads default scorers. **Pure environment plumbing.** | **None** — deps come from `pip install sentimentai`. [MAY-DIFFER] |
| `init_sentiment.ai(model, envname, method, silent, api_key, api_base, api_version, api_type, api_engine)` | Activates the env, loads the embedder into `sentiment.env$embed`, sets the `openai`/`parallel` flags. | Implicit: first `embed_text`/`sentiment_score` call lazy-loads the embedder. Optionally `ensure_model()` to pre-warm. [MAY-DIFFER] |
| `install_scoring_model(model, scoring, scoring_version, ...)` | Downloads `{model}.{xgb\|csv}` from the GitHub `models/` repo to `inst/scoring/{scoring}/{scoring_version}/`. Returns `0` (already present) / `1` (downloaded). | **`ensure_model()` does exactly this part.** [MATCH on artifact + location semantics] |
| `check_sentiment.ai(...)` | If `sentiment.env$embed` is NULL, call `init_sentiment.ai()`. | Implicit lazy-load guard inside `embed_text`. [MAY-DIFFER] |
| `legacy = TRUE` path | Installs/loads the TF stack for USE models. | Optional extra: `pip install "sentimentai[legacy]"`. [MAY-DIFFER] |

### Python `ensure_model()` — target
```python
ensure_model(
    model: str = DEFAULT_MODEL,
    scoring: str = "xgb",
    scoring_version: str = "1.0",
    *,
    repo_url: str | None = None,   # default: BenWiseman/sentiment.ai raw models/
) -> None
```
Behaviour:
1. Resolve `model` (§2).
2. Ensure the **embedder** is cached: for `sentence-transformers`, instantiate
   `SentenceTransformer(hf_id)` which triggers HuggingFace's own download/cache (no
   bespoke download code needed). For `openai`, nothing to download (API). For
   `tfhub-legacy`, require the optional `[legacy]` extra and TF.
3. Ensure the **scorer** is cached: download
   `{repo_url}/{scoring}/{scoring_version}/{model}.{xgb|csv}` to a local cache dir if
   absent. **This is the one piece that must match R's `install_scoring_model()`** —
   same repo, same path layout, **same artifact bytes**, so that the scorer loaded is
   identical and §4 stays [MATCH]. The default `repo_url` is
   `https://github.com/BenWiseman/sentiment.ai/raw/main/models`.
4. Idempotent: a no-op if everything is already present (R returns `0`; Python returns
   `None`, may log "already cached"). Return type/log text [MAY-DIFFER].

**What [MAY-DIFFER] here:** existence/signature of env-creation functions, `method`/
`envname`/`python_version`/`fresh_install`/`restart_session` args (R-only),
GPU/Apple-Silicon handling, all progress/log chatter, parallelism, and **whether
provisioning is explicit (`ensure_model`) or lazy (first call)**. Python's lazy
default is fine; R's explicit-install default is fine.

**What must NOT differ:** the **scorer artifact** that ends up loaded (same file from
the same place) and the **embedder model id**. If those match, scores match.

---

## 7. The OpenAI path — keep API behaviour aligned [MATCH on vectors]

Both languages may talk to OpenAI. To keep vectors (and thus scores) identical:
- Same endpoint shape: `POST {api_base}/{api_version}/embeddings`, body
  `{"input": ..., "model": <id>}`; azure override when `api_type == "azure"`
  (deployment-URL form). R: `load_openai_embedding()`. Python: mirror it (the
  long-term plan is pure-`requests`, no reticulate — already half-done in R via
  `httr`).
- Same model id resolution: `openai` → `text-embedding-3-small` (the bake-off
  winner / default API model), with `text-embedding-3-large` and
  `text-embedding-ada-002` selectable by name.
- Rate-limit/throttle params (`request_limit`, `token_limit`) exist in both; their
  *effect on throughput* is [MAY-DIFFER] (Python may parallelize differently), their
  *effect on output* is none.

---

## 8. NA / empty-string semantics — consolidated [MATCH]

A single table because this is the most error-prone parity surface:

| input element | `sentiment_score` output | `sentiment_match` output |
|---|---|---|
| valid text | score in `[-1,1]` | full row (text, score, nearest phrase, class, sim) |
| `NA` / `None` / `nan` | `NaN` at that position | text=original, sentiment=`NaN`, similarity=`NaN`, phrase=`NaN`/`None`, class=`NaN`/`None` |
| empty string `""` | scored as-is (R does **not** special-case `""` in `sentiment_score`) | treated like NA: blanked sentiment/similarity/phrase/class, text restored (R **does** special-case `""` here via `x_index`) |
| pre-computed embedding row that was NA | zero-filled for scoring, then re-NA'd | n/a (match embeds from text) |

Python must reproduce this asymmetry exactly: `""` is special **only** in
`sentiment_match`, not in `sentiment_score`.

---

## 9. Numerical tolerance for [MATCH]

"Identical scores" means **identical to within floating-point and library-version
noise**, not necessarily bit-identical:
- The xgboost `.xgb` artifact is the same file; `predict` is deterministic given the
  same booster + input, so `p` should agree to ~1e-6 across R/Python xgboost builds.
- The embedder is the same model; `sentence-transformers` `.encode` is deterministic
  on CPU. Cross-platform/BLAS differences can introduce ~1e-5–1e-6 embedding noise,
  which propagates to a similarly small score delta.
- **Test tolerance recommendation:** assert `abs(score_R − score_Py) < 1e-4` per
  element on a fixed corpus, per `(model, scoring)`. A larger gap means a **prefix
  bug, a wrong artifact, a dim/feature-order mismatch, or a rescale error** — all
  parity bugs, not noise. `glm` should agree tighter (~1e-8) since it's pure linear
  algebra on a CSV.

---

## 10. Known parity gaps to close (actionable checklist)

Ordered by score-impact. Items marked **(R)** are fixes the R v2 side still owes;
**(Py)** are the Python bodies to fill.

1. **(R) Hardcoded `512` passthrough** in `sentiment_score()` (`ncol(x)==512`) and
   `find_sentiment_probs()` (`numeric(512)`) — must become registry-`dim`-driven, or
   non-USE pre-computed matrices score wrong / are rejected. [MATCH-breaking] (v2
   roadmap Phase 2.)
2. **(Py) Apply the model `prefix`** (`"query: "` for e5) inside `embed_text` before
   encoding. Skipping it silently changes every e5 score. [MATCH-breaking]
3. **(Py) Implement `sentiment_score` rescale + NA semantics** exactly per §4/§8.
4. **(Py/R) Reconcile `resolve()` vs `choose_model()` unknown-name behaviour** — same
   input must not error in one and warn-through in the other (§2).
5. **(Py) Add OpenAI aliases** (`text-embedding-3-large`, `ada-002`,
   `text-embedding-3-small`) to `BACKENDS` so name resolution matches R.
6. **(Py) Align signatures/defaults:** add `scoring_version="1.0"` to
   `sentiment_score`; fix `embed_text` positional order vs R (or make keyword-only);
   default `batch_size` (`sentiment_score`/`sentiment_match` = 100, `embed_text`
   inherits R's `NULL` = one batch). (§3, §4)
7. **(Py) `ensure_model` scorer download** must hit the **same repo/path** as R's
   `install_scoring_model` so the loaded `.xgb`/`.csv` is byte-identical (§6).
8. **(Py) `sentiment_match` table:** five columns `text, sentiment, phrase, class,
   similarity`, input order, de-duped phrase lookup, reuse the embedding for the
   `sentiment` column (no double-embed). (§5)
9. **(Both) Cross-language golden test:** a fixed corpus scored in R and Python per
   `(model ∈ {e5-small, e5-base, openai}, scoring ∈ {xgb, glm})`, asserted to §9
   tolerance. This is the regression gate that *defines* parity.

---

## 11. One-paragraph summary of the contract

`sentiment_score` is the parity spine: **same model + same scorer artifact + same
prefix + the `(p−0.5)·2` rescale ⇒ identical numbers in R and Python** (tolerance
1e-4), because both languages load the *same* HuggingFace embedder and the *same*
`.xgb`/`.csv` scorer rather than reimplementing them. `embed_text` and
`sentiment_match` match on **values, ordering, NA semantics, and column set**, while
their *containers* follow each language's idiom (named vector / `data.table` ↔
`ndarray` / `DataFrame`). The registry must expose the **same model names, dims,
prefixes, and default (`e5-small`)**. Install/provisioning is the **only sanctioned
divergence**: R carries the full `reticulate` env dance across three functions; Python
collapses provisioning into a single lazy `ensure_model()` whose **sole parity duty**
is to fetch the **same scorer artifact from the same place** so the scores stay 1:1.
