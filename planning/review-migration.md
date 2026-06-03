# Red-team: TF-removal migration + legacy plan (sentiment.ai v2)

**Reviewer:** adversarial pass, 2026-06-03. **Scope:** the actual code on disk, not the roadmap's
description of it. **Verdict:** the migration is *half-landed and currently broken on its own new
default path*. `constants.R`/`choose_model.R` were rewritten for v2 (e5 defaults, legacy split,
`model_dims`, `model_class`), but `init_and_install.R`, `embedding.R`, `sentiment.R`, `onload.R`,
and `inst/get_embedder.py` are still the pre-migration TF code. The "legacy flag" the brief asks
me to validate **does not exist in code**, and the new default model **cannot load or score** today.

Files read: `R/init_and_install.R`, `DESCRIPTION`, `R/constants.R`, `R/choose_model.R`,
`R/embedding.R`, `R/sentiment.R`, `R/onload.R`, `inst/get_embedder.py`, `NEWS.md`, `v2-roadmap.md`,
plus `inst/scoring/` contents and git state.

---

## Severity-ranked findings

### S0 — BLOCKER. The new default (`e5-small`) has no code path that can load OR score it.

This is not a migration *risk*; the migration's own happy path is dead. Three independent breaks,
each fatal on its own:

1. **No embedder loader for sentence-transformers.** `choose_model()` (correctly) returns the
   HuggingFace id `intfloat/multilingual-e5-small` for `e5-small`. But `init_sentiment.ai()`
   (`init_and_install.R:496–513`) only branches two ways: OpenAI (`load_openai_embedding`) or
   **`load_hub_embedding`** — the TF-Hub loader. There is no `st`/sentence-transformers branch.
   So an e5 id gets handed to `load_hub_embedding()`, which calls `tensorflow_hub.load("intfloat/...")`
   and fails. `inst/get_embedder.py` is **TF-only**; the promised `get_embedder_v2.py` **does not
   exist on disk** (only `get_embedder.py`, dated Nov 2024). `model_class()` exists in `constants.R`
   but is **never called** by `init_sentiment.ai()` — the dispatch was written but not wired in.
2. **`install_scoring_model()` rejects the new default at `match.arg`.** Signature is
   `model = c("en.large","en","multi.large","multi")` and line ~288 does `model <- match.arg(model)`.
   `sentiment_score()` now defaults `model = names(default_models)` → `"e5-small"`, calls
   `install_scoring_model(model, ...)`, and `match.arg` **errors**: `'arg' should be one of "en.large"...`.
   The comment on line ~287 says "Remove match.arg not used - give flexibility" but the `match.arg`
   call is still right there. The default path throws before it downloads anything.
3. **No e5 scoring model is packaged or fetchable.** `inst/scoring/` contains **only `readme.txt`** —
   no `xgb/1.0/*.xgb`. `find_sentiment_probs()` builds `file.path(score_dir, "e5-small")` → expects
   `e5-small.xgb`, which doesn't exist. The roadmap's `available_models.csv` (Phase 3) **does not
   exist** either. The training repo's e5 scorer hasn't been copied in or wired to a download URL.
   Net: even if 1 and 2 were fixed, scoring fails with a missing-file/`xgb.load` error.

**Bottom line:** with the tree as-is, `sentiment_score("good")` (no args) errors. Existing
USE-named scripts *also* break (see S1). There is currently **no working path at all** except an
explicit OpenAI call with an API key. The migration looks 70% done but is functionally regressed.

**Fix (all three, in order):**
- Add the `st` branch to `init_sentiment.ai()`: dispatch on `model_class(model)` (it already
  exists), and write the real `get_embedder.py` sentence-transformers loader
  (`SentenceTransformer(hf_id)`, apply `model_prefix[model]` i.e. the `"query: "` prefix for e5,
  `normalize_embeddings=True`). Reconcile to ONE embedder file; delete the dead TF-only assumptions
  from the default path.
- Replace `match.arg(model)` in `install_scoring_model()` with a membership check against
  `c(names(default_models), names(openai_models), names(legacy_models))`, or drop the constraint
  entirely (the comment already intended this). Map user name → scoring-file name explicitly.
- Package or wire-download `e5-small.xgb` / `e5-base.xgb` (and the OpenAI scorer) from the training
  repo into `inst/scoring/xgb/1.0/`; create `available_models.csv`. **No green tests until a
  no-arg `sentiment_score()` returns a number with zero TF installed** — that is the one CI gate
  that matters (roadmap Phase 7 is right about this).

---

### S1 — BLOCKER for the stated goal. The "legacy flag" does not exist; legacy scripts break *worse*, not gracefully.

The brief asks "will `model="en.large"`/`"multi.large"` break in ways the legacy flag misses?" —
the premise is false: **there is no legacy flag.** `install_sentiment.ai()`
(`init_and_install.R:98–112`) has no `legacy` parameter. The only reference to
`install_sentiment.ai(legacy = TRUE)` is a **comment** in `constants.R:17` and the roadmap. So the
opt-in mechanism the lineup depends on is unimplemented.

What actually happens to a legacy script today:
- `init_sentiment.ai(model="en.large")` → `choose_model()` returns the TF-Hub URL → `load_hub_embedding()`
  is called → requires TF/`tfhub`/`tensorflow_text` to be importable. If the user installed v2 via a
  TF-free install, **TF isn't there and this hard-errors** with a Python ImportError, not a friendly
  "run install_sentiment.ai(legacy=TRUE)" message.
- Worse: `install_sentiment.ai()` *still hard-pins TF unconditionally* (lines 102–109 / Apple-silicon
  branch 121–127) — so the **default** install still pays the full TF tax the migration exists to
  remove. The dependency surgery (roadmap Phase 1) hasn't touched this function at all. The migration's
  headline ("TF is no longer the default") is **not true of the install path**, only of the
  `constants.R` name table.

**Fix:**
- Implement `legacy = FALSE` on `install_sentiment.ai()`. When `FALSE`: install only
  numpy + `sentence-transformers` + sentencepiece (+ `httr` covers OpenAI in R, no python openai
  needed). When `TRUE`: install the quarantined TF pins. Strip TF from the default module list.
- Make `init_sentiment.ai()` detect "legacy model requested but TF env absent" and raise a *directed*
  error: `"en.large is a legacy TensorFlow model. Run install_sentiment.ai(legacy = TRUE) first."`
  — caught before reticulate emits a raw ImportError.
- Keep `install_scoring_model()`'s USE names working in parallel (those scorers DO exist in the repo),
  so a legacy user who *does* install TF gets identical numbers to v0.2 (reproducibility, see S4).

---

### S2 — HIGH. Silent default-model change with no version discipline → silent score changes for everyone.

`sentiment_score()` / `sentiment_match()` defaults moved from `"en.large"` (USE-512, TF) to
`names(default_models)[1]` = **`e5-small`** (384-D, different model family). Meanwhile
`init_sentiment.ai()`'s own signature *still* defaults to `c("en.large", ...)` and its roxygen
`@examples` still show `model="en.large"` — the package is internally inconsistent about what the
default even is.

Backward-compat impact (once S0/S1 are fixed and e5 actually runs): **every user who relied on the
default gets different numbers**, from a different embedding space and a different scorer, with no
warning. This is a textbook semver-major event. Current `Version: 0.2.0` and the change is being
slipped in as if it were a minor bump. NEWS.md still describes 0.2.0 as "set version argument to 1 to
use original models" — a knob (`scoring_version` / `version`) that does **not** restore the old
*embedding model*, only (maybe) an old scorer. That sentence overpromises reproducibility.

**Fix:**
- Bump to **1.0.0** at the default flip (roadmap Phase 4 agrees). One source of truth for the default:
  use `DEFAULT_MODEL` from `constants.R` everywhere — `sentiment_score`, `sentiment_match`,
  `init_sentiment.ai` signature, AND the roxygen examples. Right now `DEFAULT_MODEL` is defined and
  **never used**.
- Add a **one-release transition**: if the user didn't pass `model`, emit a one-time message naming
  the new default and how to pin the old one (`model="en"`/`"en.large"` + `legacy=TRUE`). Do NOT
  silently change numbers in a patch/minor.
- Fix NEWS.md: be honest that old *scores* are reproducible only via the legacy USE model + its
  scorer, not via a `version` argument on the new default.

---

### S3 — HIGH (external, time-bombed). Legacy reproducibility is NOT guaranteed: TF-Hub → Kaggle deprecation can make USE unloadable.

The legacy lineup's promise is "your old scripts still work via `legacy=TRUE`." That promise depends
on something **outside this repo**: the USE models loading at all. Real, documented risk (not
fabricated):

- `tfhub.dev` was **deprecated and redirected to Kaggle Models (kaggle.com/models)** by Google in
  **2024**; new resolution of `https://tfhub.dev/google/...` URLs is on borrowed time. The package
  hardcodes `https://tfhub.dev/google/universal-sentence-encoder.../N` in `choose_model.R:25`.
  If/when those redirects stop, `load_hub_embedding()` fails at download with no fallback.
- The pinned stack (`tensorflow==2.4.1` / `tensorflow_hub==0.12.0` / `tensorflow-text==2.4.3`,
  `python 3.8.10`) is from ~2021. These wheels are increasingly **unavailable on modern Python/OS**
  (notably no `tensorflow-text` for Apple Silicon — the code already special-cases this by *dropping*
  tf-text, which silently changes which USE variants can even run). "latest" TF (the Apple-silicon
  branch) is **not** the version the legacy scorers were trained against — so "latest" can shift
  embeddings vs the original 2.4.1, breaking reproducibility *even when it loads*.

So "legacy reproducibility is achievable" is **conditionally false**: it works only on
environments where (a) the exact old TF wheels still install and (b) tfhub.dev still resolves.
Neither is guaranteed forward, and the second is actively decaying.

**Fix:**
- **Vendor the USE assets** rather than depending on live tfhub.dev: cache the SavedModel(s) in the
  package's release artifacts / a Box/GitHub-release mirror, and point `load_hub_embedding()` at the
  mirror with tfhub.dev as fallback (not the reverse). This is the only way to keep the legacy promise.
- Pin the *Kaggle* URLs as the primary, document that legacy is "best-effort, frozen-2021 stack."
- In NEWS/docs, state plainly: legacy USE is a **frozen compatibility shim**, may stop installing on
  future OS/Python, and reproducibility is guaranteed only against the documented pinned env. Don't
  claim more than is true (brand: grounded, not invented).
- Consider shipping a **migration table** of expected score deltas USE→e5 on the benchmark corpora so
  users can recalibrate instead of needing bit-exact legacy.

---

### S4 — MEDIUM. CRAN viability of Suggests-guarded TF + reticulate + runtime downloads is shaky as written.

The plan (TF in `Suggests`, models downloaded at runtime) is the right *shape* for CRAN, but the
current code violates several CRAN policies and will not pass `R CMD check --as-cran` as-is:

- **Runtime downloads to the package install dir.** `install_scoring_model()` /
  `install_default_embeddings()` `download.file()` into `system.file(package=...)` subdirs
  (`init_and_install.R:307`, `:369`). CRAN **forbids writing into the package library** at runtime;
  this must go to `tools::R_user_dir("sentiment.ai", "cache")` (or `rappdirs`, already in Suggests).
  As written, on a normal (non-writable lib) install this throws a permission error.
- **`reticulate:::py_install_method_detect`** and **`reticulate:::...`** — `init_and_install.R:141`
  and `local_from_reticulate.R` use `:::` into another package's internals. CRAN rejects `:::` on
  non-base packages. The roadmap doesn't flag this; it's a hard check failure.
- **Examples / Suggests guarding.** Every TF/reticulate-touching example must be `\dontrun{}` (mostly
  are) AND code that uses a Suggests package must guard with `requireNamespace("tensorflow", quietly=TRUE)`
  before use, else `--as-cran` NOTES/ERRORs when TF is absent. The TF-load path needs that guard.
- **`Imports: openai`** (DESCRIPTION:47) is a **dead dependency** — the `openai` R package is imported
  nowhere (NAMESPACE has no `openai` entry; the R OpenAI path is hand-rolled `httr` in
  `load_openai_embedding()`, and the *python* `openai` usage is commented out in `get_embedder.py`).
  It's install weight + a CRAN "declared-not-used" smell. **Drop it from Imports.** (Directly answers
  the brief: no, the `openai` R dependency is not needed — `httr` + `jsonlite` already cover it.)
- **Install functions trigger network + session restart.** `rstudioapi::restartSession()` and
  interactive `readline()` prompts (`check_virtualenv_py:706`) can't run in CRAN's non-interactive
  check; gate on `interactive()`.

**Fix:** move all downloads to `R_user_dir` cache; replace every `reticulate:::`/internal `:::` with
public API or vendored copies that don't reach into internals; `requireNamespace`-guard all TF use;
remove `openai` from Imports; gate interactivity. None are blockers for *function*, all are blockers
for *CRAN*.

---

### S5 — MEDIUM. Dimensionality coupling still assumes 512 in the live scoring path.

`constants.R` added `model_dims` (384/768/1536/3072/512) — but it's **not used** by the code that
matters. `sentiment_score()` passthrough still hard-checks `is.matrix(x) && ncol(x)==512`
(`sentiment.R:98`): a user who passes a **precomputed e5 (384-D) or OpenAI (1536-D) matrix** falls
through the `if/else if` with `text_embed` **undefined** → error, or worse silently scores the wrong
thing. `find_sentiment_probs()` seeds `probs <- numeric(512)` (`sentiment.R:261`) — harmless only
because it's overwritten, but it signals the 512 assumption is still baked in. The roadmap (Phase 2)
identifies this; the code change hasn't been made.

**Fix:** replace the `==512` literal with `ncol(x) == model_dims[[model]]`; drop the `numeric(512)`
seed; pick the scoring model by `model` via the registry, not by assuming USE.

---

### S6 — LOW/MEDIUM. e5 prefix correctness is a silent-accuracy trap.

`model_prefix` correctly encodes that e5 needs `"query: "`. But since there is no st embedder code
yet (S0), nothing applies it. If the loader is added without applying the prefix, e5 will embed
**out of distribution** vs how the scorer was trained — F1 quietly drops with no error. The bake-off
numbers (e5-base 0.860, ties OpenAI) are only real **if the prefix is applied identically at train
and inference time.** This must be asserted in a test, not assumed.

**Fix:** apply `model_prefix[model]` inside the st embedder; add a snapshot test that a known string
under e5 produces the expected embedding/score, so a dropped prefix fails CI.

---

### S7 — LOW. Stale/inconsistent surface area that will confuse users mid-migration.

- `onload.R` `.onLoad` message tells users to set `model="paraphrase"` and says **"`use` ... is still
  the default"** — both false now (default is `e5-small`; `paraphrase` isn't in `default_models`).
  The Apple-silicon `.onLoad` warning still hard-pushes a TF install recipe. These fire on **every
  package load** and actively misdirect.
- `embed_topics()` / `read_embedding()` default `model="en.large"`; `get_default_embedding()` keys the
  default-embeddings JSON on package version — these still assume USE.
- Docstrings throughout (`sentiment_score` `@param x` says "512-D numeric embedding"; `init` examples
  say `en.large`; install_and_setup module table lists the TF stack as *the* requirement) describe the
  old world.

**Fix:** rewrite `.onLoad` messaging to be backend-aware and truthful about the real default; sweep
roxygen for `en.large`/512/`paraphrase` references; regenerate docs.

---

## Direct answers to the brief's challenges

- **Will `en.large`/`multi.large` scripts break in ways the legacy flag misses?** Yes — because the
  legacy flag **doesn't exist yet**, *and* even once added, the live tfhub.dev dependency (S3) and the
  unconditional-TF install (S1) mean they break before any flag can help. They break *today* on a
  TF-free install with a raw ImportError, not a guided message.
- **Is legacy reproducibility achievable given tfhub.dev→Kaggle deprecation / can USE even still
  load?** Only conditionally and decaying. tfhub.dev is deprecated (redirected to Kaggle, 2024); the
  hardcoded URLs and 2021 pinned wheels are a time bomb, and the Apple-silicon "latest TF" path isn't
  even the version the scorers were trained on. Must vendor/mirror the assets to keep the promise.
- **CRAN implications of Suggests-guarded TF + reticulate + runtime downloads?** Right shape, wrong
  details: runtime writes into the package dir, `reticulate:::` internals, an unused `openai` Import,
  and unguarded Suggests usage will all fail `--as-cran`. Fixable, listed in S4.
- **Is the `openai` R package dependency still needed?** No. Remove from Imports; the path is pure
  `httr`/`jsonlite`.
- **Backward-compat of the default change + semver?** It's a silent breaking change shipped without a
  major bump and with internally contradictory defaults. Needs 1.0.0, a single `DEFAULT_MODEL` source
  of truth, a warn-first transition, and honest NEWS.

---

## TOP 3 (do these first)

1. **S0 — Make the new default actually work end-to-end before anything else.** Wire
   `model_class()`-based dispatch into `init_sentiment.ai()`, write the real sentence-transformers
   loader (with the e5 `"query: "` prefix), remove the `match.arg` that rejects `e5-small` in
   `install_scoring_model()`, and package/fetch `e5-small.xgb`. Gate: a no-arg `sentiment_score("good")`
   returns a number with **zero TensorFlow installed**. Today it errors three different ways.

2. **S1 — Implement the `legacy=` opt-in that the whole plan assumes exists, and stop hard-pinning TF
   in the default install.** Add `legacy = FALSE` to `install_sentiment.ai()` (TF pins quarantined
   behind `TRUE`), strip TF from the default module list, and make `init_sentiment.ai()` raise a
   directed "run install_sentiment.ai(legacy=TRUE)" error when a USE model is requested without TF —
   instead of a raw reticulate ImportError.

3. **S3 — De-risk legacy reproducibility against the tfhub.dev→Kaggle deprecation by vendoring the USE
   assets** (mirror the SavedModels in a GitHub release; tfhub.dev/Kaggle as fallback, not primary),
   pin the env honestly, and ship a USE→e5 score-delta table so users can recalibrate. Without this,
   the "compatibility layer" is a promise the package can't keep once Google finishes the migration.
