# sentiment.ai v2 — repo-review synthesis

> Written 2026-06-03. Planning-only (writes to `planning/` only; touches no source).
> **Consolidates the six senior repo-reviews** — `repo-review-arch.md` (software architect),
> `repo-review-ds.md` (ML/NLP data scientist), `repo-review-dx.md` (developer experience),
> `repo-review-mlops.md` (MLOps / distribution), `repo-review-rpkg.md` (R-package / CRAN
> maintainer), `repo-review-shiplead.md` (skeptical ship-lead, *ran the code*) — and reconciles
> them against the prior adversarial set (`review-migration.md` S0–S9, `review-lineup.md`
> SEV-1–3, `review-methodology.md` S1–S10) and `better-sentiment-roadmap.md`.
>
> Deliverable structure: (1) prioritised, de-duplicated issues with the fix; (2) best ideas
> ranked by impact × effort, favouring honest-measurement-brand + platform compounding; (3)
> net-new vs already-covered; (4) reviewer disagreements resolved; (5) the IF-YOU-DO-5-THINGS list.
>
> **Brand invariant** running through every call: *honest measurement* — "matches OpenAI, not
> beats", computed-not-AI, auditable, never overclaim, never ship a silent wrong number. The
> single most useful lens for triage is: **a measurement instrument's worst failure is a silent
> systematic error the user can't see** (`repo-review-ds.md`). Two of the top issues are exactly
> that class, which is why they outrank otherwise-larger work.

---

## 0. The one-paragraph state of the package (consensus across all six)

The architecture is **right and rare** and ~80% landed: a frozen embedder behind a swappable
function-pointer (`sentiment.env$embed`) feeding a pure, artifact-driven light scorer
(`find_sentiment_score`, ~35 lines, no Python/TF) — exactly the shape a multi-head measurement
platform needs, which is *why v2 is integration, not new ML*. The test suite is TDD-first and
genuinely excellent (no-Python fake embedder that records the strings it's handed, prefix
assertion, no-512 tripwire, TF-in-NAMESPACE tripwire). **But the new default cannot complete a
single `sentiment_score("good")` call today** — `repo-review-shiplead.md` reproduced the error —
because the `st`/e5 dispatch branch was designed (`model_class()`, `load_st_embedder`) but never
wired into `init_sentiment.ai()`, and `install_scoring_model()` still `match.arg`-rejects the new
default. Beneath that wiring gap sit two *silent-wrong-number* hazards (a train/serve prefix skew,
and uncalibrated probabilities sold as honest measurement), a distribution layer that writes to a
read-only library and never checksums downloads, an unguarded runtime OpenMP collision on the
default path, and a README that describes a v1 package that no longer exists. **None of this is a
rewrite.** The work is wiring, validation, consolidation, and surfacing — the design is sound.

---

## 1. PRIORITISED ISSUES (severity-ranked, de-duplicated, each with the fix)

Severity scale: **P0 release-fatal** (default path is broken or silently wrong) · **P1 ship-gate**
(must be true before tag/CRAN/publish) · **P2 important** (quality, debt, compounding) · **P3
hygiene**. Each issue lists the reviewers that raised it so de-duplication is auditable.

### P0 — release-fatal (the default path is broken or scores silently wrong)

**ISSUE-1 — The default path doesn't run end-to-end. Wire the `st` dispatch + drop the stale
`match.arg`.**
*Raised by: shiplead BLOCKER-1, rpkg #1, migration S0, arch §1/§3 (root-cause framing), ds (assumes it as precondition).*
Two independent breaks on the no-arg path, both reproduced in the live tree: (a) `init_sentiment.ai()`
(`init_and_install.R:496–513`) branches only OpenAI vs `load_hub_embedding` (the **TF** loader);
`model="e5-small"` falls into the TF branch and fails — `model_class()` and `load_st_embedder` exist
and are **never called from R**; (b) `install_scoring_model()` `match.arg(model = c("en.large",…))`
rejects `e5-small` (`init_and_install.R:288`; comment above the line literally says "Remove match.arg").
**Fix:** in `init`, dispatch on `model_class(model) ∈ {st, openai, legacy}`; the `st` arm sources
`get_embedder.py` and calls `load_st_embedder(hf_id, prefix = model_prefix[[model]] %||% "")` and sets
`sentiment.env$backend <- "st"`. Replace `match.arg(model)` with a membership check against the registry.
**Exit gate (the one that matters):** a no-arg `sentiment_score("good")` returns a number with **zero
TensorFlow importable**. Effort: hours.

**ISSUE-2 — Train/serve skew on the e5 prefix: the shipped scorer was trained UN-prefixed, the package
serves PREFIXED. Silent, systematic, CI-green wrong number.** **(the sharpest finding in the set)**
*Raised by: ds DS-1 (escalated to LIVE), shiplead BLOCKER-2, lineup SEV-1 (as "prefix is dead config"), methodology S2 (as benchmark fairness), migration S6 (as future trap).*
The escalation `repo-review-ds.md` adds over the priors is decisive: this is **not** "users might
forget the prefix" or "the benchmark may be understated" — it is a *live distribution mismatch in the
thing about to ship*. Production embeds `"query: " + text` (CI-gated by `test-e5-prefix.R`,
`constants.R:32`); the shipped `xgb_model_me5_{small,base}_full.xgb` were fit on **un-prefixed**
embeddings (every e5 embed-log header shows no prefix; `grep -c "query:"` over all bake-off logs = 0;
`sub_embed.py --prefix` defaults `""`). xgboost returns a number either way — a quietly degraded one.
**This is the canonical silent systematic error in a brand that sells trustworthy numbers, and it
must be fixed before any calibration work** (calibrating a mis-served embedding is precise nonsense).
**Fix, in order:** (1) commit to one recipe end-to-end — `query:` on all sentiment inputs (symmetric
docs; do **not** mix `query:`/`passage:`); (2) **re-embed the training corpus WITH `query:` and re-fit
the e5 heads** (embeddings re-extract + xgb re-fit = minutes); (3) re-measure the "ties OpenAI"
headline on the *prefixed* recipe and restate whatever is true; (4) **stamp the training prefix into
the scorer metadata and assert `serve_prefix == scorer_prefix` at score time** — one assertion that
turns this skew into a build failure forever. Effort: half a day.

**ISSUE-3 — Distribution is not safe end-to-end: big artifacts can ship in the tarball, the cache
writes to a read-only library, and downloads are never checksummed.** **(three coupled failure modes)**
*Raised by: mlops #1.1/#1.2/#1.3 + TOP-1, cran-checklist A1–A5 (cache), migration S4 (CRAN), shiplead Phase-B #6.*
Three distinct production failures, not one: (a) **no `.Rbuildignore` guard** on the ~58 MB of dev
`.xgb` in `inst/scoring/xgb/1.0/` — `R CMD build` tars the *working dir*, not the git index, so a 73 MB
tarball ships the day a maintainer forgets, and CRAN auto-rejects (>5 MB); (b) `install_scoring_model()`/
`install_default_embeddings()` `download.file()` into `system.file(package=…)` — **fails on any
read-only install** (RStudio Server, Docker, `renv`, HPC), a paying-user permission error independent of
CRAN; (c) **no SHA-256 on the download** — a GitHub 200-HTML-error page or a dropped connection leaves a
file that `file.exists()` passes and `xgb.load()` either rejects cryptically or *loads truncated and
scores silently wrong*, and the early-return-on-`file.exists` means it's never re-fetched. For a brand
selling auditable measurement, (c) is the sharpest contradiction.
**Fix:** narrow `.Rbuildignore` (`^inst/scoring/(xgb|glm)/.*\.(xgb|csv)$`, `^inst/default_embeddings/.*\.json$`
— keep `available_models.csv`/`readme.txt`) **plus a CI tarball-size gate (≤4900 KB)**; route every
read+write through one `.sentiment_cache_dir()` helper on `tools::R_user_dir("sentiment.ai","cache")`,
and pin `HF_HOME`/`SENTENCE_TRANSFORMERS_HOME`/`TRANSFORMERS_CACHE` to it; add `sha256,bytes` to
`available_models.csv`, verify after an atomic `.part`→rename, delete-and-error on mismatch; serve
scorers from an **immutable GitHub Release tag**, not `raw/main` (a moving target = silent score drift).
Effort: ~1 day.

### P1 — ship-gate (must be true before tag / CRAN / publish)

**ISSUE-4 — The runtime OpenMP collision (R-xgboost ⊕ python-torch in one process) has zero mitigation,
is on the default path, and the dev Mac structurally cannot reproduce it.**
*Raised by: mlops §2 + TOP-2 (net-new), rpkg secondary (records the mitigation caveat). Distinct from methodology:182's build-time no-OpenMP note.*
Every default `sentiment_score()` loads torch (embed) then R-xgboost (score) in one process; two OpenMP
runtimes in one process → `OMP: Error #15` abort (takes the R session + unsaved work), or silent wrong
results, or fork deadlock. Grep confirms **no `OMP_*`/`KMP_*`/`nthread`/torch thread guard anywhere**.
The trap: the maintainer's Mac builds xgboost **without** OpenMP, so it passes locally and CI while
aborting for stock-OpenMP-xgboost Linux users.
**Fix (layered):** floor — set `KMP_DUPLICATE_LIB_OK=TRUE` + `torch.set_num_threads(1)` + explicit
single-thread xgboost `predict` *before* Python initialises (document that `KMP_DUPLICATE_LIB_OK` is a
blunt instrument that permits a duplicate load); robust fallback — run the pure-R scorer in a forked
`callr::r()` process with clean OpenMP state (~50 ms, byte-identical, amortised over a batch); **CI
must exercise a real embed-then-score in one session on Linux with stock-OpenMP xgboost** or the bug
ships invisibly. Note: rpkg correctly warns *not* to ship `KMP_DUPLICATE_LIB_OK` as the *only* fix
(can mask corruption) — so the thread-pinning + callr fallback is the real answer, the env var is the
floor. Effort: half a day + a CI leg.

**ISSUE-5 — Nothing pins the scores; the golden parity tests are all skipped (no fixtures), and there
is no cross-language golden gate.**
*Raised by: shiplead BLOCKER-3, arch §4.2 + Move-D, mlops §5 + TOP-3, ds (calibration needs a real held-out), PARITY.md §10.9 (named but unbuilt).*
`test-snapshot-scorer.R` and the prefix-regression test are well-designed but `fixtures/` contains only
`README.md`, so every score-pinning test `skip_if_no_fixture()`s out. A package whose brand is
"reproducible, auditable measurement" ships with **zero pinned-score tests** — and the same fixture is
the only thing that detects ISSUE-2 regressing or R↔Python drifting.
**Fix:** commit `tests/fixtures/golden.csv` — `(model, scoring, text) → expected_score` — generated
once from the **final prefixed recipe (ISSUE-2)**; assert to 1e-4 in **both** `testthat` and `pytest`;
gate the first PyPI publish on it. This is the parity spec (PARITY.md) made executable and the
"re-run in 2028, identical number" guarantee made real. Sequence it *after* ISSUE-2's recipe is fixed.
Effort: ~1 hour once the recipe is settled.

**ISSUE-6 — Implement the `legacy=` opt-in the whole plan assumes, and stop hard-pinning TF in the
default install.**
*Raised by: migration S1, rpkg #1 (gate design), shiplead Phase-B #9, mlops (install surface).*
`install_sentiment.ai()` has **no `legacy=` argument** and **still installs TensorFlow 2.4.1
unconditionally** — so "no TF by default" is true of the name table but **false of the installer**. A
legacy script (`en.large`) on a TF-free install hard-errors with a raw Python `ImportError`, not a
guided message.
**Fix:** add `legacy = FALSE` to `install_sentiment.ai()` (default installs only
`sentence_transformers`+`torch`+`numpy`; TF/tfhub/tensorflow-text quarantined behind `TRUE`); make
`init` raise a *directed* R-level error before any `source_python` when a USE model is requested without
TF (`"en.large is a legacy TensorFlow model — run install_sentiment.ai(legacy = TRUE)"`); promote the
`legacy_repl` map from the test into `constants.R`. This also shrinks the install surface feeding
ISSUE-4. Effort: half a day.

**ISSUE-7 — CRAN hygiene blockers (the `cran-checklist.md` set, current-tree-confirmed).**
*Raised by: cran-checklist (authoritative), rpkg (confirms live + one correction), migration S4, mlops, shiplead Phase-B.*
Confirmed still live: `@import tensorflow`/`@import tfhub` in NAMESPACE (hard `--as-cran` ERROR; the
hardest bounce); one live `reticulate:::py_install_method_detect` (`init_and_install.R:141`) despite the
vendored replacement existing; `Imports: openai` is dead (R path is hand-rolled `httr`); `library(parallel)`
at `embedding.R:1`; runtime writes to library (= ISSUE-3b); `print(url)` debug leak (`embedding.R:399`)
+ `print()`s in `get_embedder.py`; interactive `readline()`/`restartSession()` ungated. **Correction
(rpkg):** cran-checklist E3 ("no `tests/` dir") is now **stale** — 21 test files + `helper-skips.R`
exist; downgrade E3 to done. **Fix:** delete the roxygen `@import` tags + re-`document()`; swap the
`:::` call for the vendored copy; drop `openai` from Imports; qualify `parallel::`; remove debug prints;
`interactive()`-gate. Effort: ~1 day total, mostly mechanical.

**ISSUE-8 — Stale docs/README/roxygen describe a v1 package that no longer exists (every-user trust
leak at first contact).**
*Raised by: dx Fix-1 (escalated as the #1 DX leak), migration S7, rpkg secondary, shiplead Phase-C, roadmap table-stakes #5.*
The most-read files contradict the brand loudest: `README.md:20` "head-and-shoulders above … toe-to-toe
with Azure (only we're free!)" overclaims (brand says *matches, not beats*); `:36` "dieciséis (16)
languages", `:32` "Universal Sentence Encoder", `:45` "512-D embeddings generated with tensorflow" are
**every word wrong for v2** (e5, ~100 langs, 384/768-D, no TF) — and the **same stale copy is duplicated
verbatim in `vignettes/vignette.Rmd`**. Roxygen is as bad: `@param x` says "512-D numeric embedding";
`init`'s `@param model` default list is the USE handles; `@details` lists `tensorflow 2.4.1` as required;
`DESCRIPTION` overclaims "out-performing traditional lexicon-based". The honesty lives only in `planning/`,
where no user looks. **Fix:** purge every v1 fact from README + vignette + roxygen; replace the overclaim
with the brand line; lead with the tidy one-liner; add a "what it is / isn't" honesty box; full roxygen
rewrite pointing at `DEFAULT_MODEL`. Zero code, biggest funnel. Effort: ~1 day.

**ISSUE-9 — Silent default-model change with no semver discipline; `DEFAULT_MODEL` defined and never
used.**
*Raised by: migration S2, shiplead Phase-A #5/Phase-C #11, rpkg secondary, lineup SEV-2 (the choice itself — see §4).*
The default moved `en.large` (USE-512, TF) → `e5-small` (384-D), a different embedding space and scorer —
a textbook semver-major event being slipped into a `0.2.0` tree, while `init`'s own signature *still*
defaults to the USE list and examples still say `en.large`. `DEFAULT_MODEL` exists and is referenced
nowhere. **Fix:** one source of truth — `DEFAULT_MODEL` in `sentiment_score`/`sentiment_match`/`init`
signature + roxygen; bump to **1.0.0** at the flip; a one-release warn-first transition naming the new
default and how to pin the old (`legacy=TRUE`); honest NEWS (old *scores* reproduce only via the legacy
USE model + scorer, not a `version` arg). Effort: hours.

### P2 — important (quality, debt, platform compounding)

**ISSUE-10 — Five re-derivations of "what is model `m`" → one missing `Backend` descriptor; two
hand-maintained registries (R `constants.R` ↔ Py `_models.py`).** **(the structural root cause of the
P0/P1 dispatch bugs)**
*Raised by: arch §1 + Move-A (keystone), rpkg #3 ($backend/state object), mlops §5 (one registry), dx Fix-5 (one vocabulary), ds S-A (score-definition provenance).*
`choose_model()` knows the id but throws away kind/dim/prefix; `model_class()` re-derives kind;
`install_scoring_model()` re-derives the valid set with a stale `match.arg`; `embed_text()` dispatches on
a *boolean* `sentiment.env$openai` (can't represent 3+ backends); `find_sentiment_score()` re-derives the
scorer path and sniffs class-count from `length(preds)`. **That scatter is the literal cause of ISSUE-1's
three breaks** — they are *one missing abstraction observed five times*. And R + Python keep two copies of
a table that must agree (already drifting: Python omits OpenAI aliases; unknown-name behaviour differs —
R warns-through, Python raises). **Fix:** build one `Backend` descriptor (Python has it:
`_models.Backend`; R has the data, not the object) sourced from one language-neutral `inst/models.json`;
`choose_model()` returns it; every downstream fn consumes it; R named-vectors + Py dataclass become thin
typed views. ~80% of the migration's bug class becomes structurally impossible. Effort: ~1–2 days; do it
*as* the ISSUE-1 fix, not after.

**ISSUE-11 — The scorer destroys the 3-class signal at the seam; the output transform is welded to the
estimator and sniffed from prediction shape; no `task`/head axis in storage or dispatch.**
*Raised by: arch §2/§5 + Move-C, roadmap Move-1 (the one all five lenses name), ds DS-2/S-A, dx (return contract, treated as decided), rpkg (consumes it).*
`find_sentiment_score` returns `probs[,3]-probs[,1]` (or `(p-.5)*2` for binary, chosen by
`length(preds)`), discarding `prob_neu`, entropy, and pos/neg intensities — the precondition for *every*
differentiator (calibration, abstention, mixed-flag, agreement, drift). The transform is hard-wired per
branch and the output contract is *sniffed*, so a 3-class readability head would be silently collapsed as
sentiment. The artifact path `scoring/<scoring>/<version>/<model>` has no `task` axis, so a readability
head and a sentiment head on e5 vectors **collide**. **Fix:** make the scorer return the **full
probability struct** (the [-1,1] collapse becomes one named downstream transform); make a head a declared
descriptor `{task, est, version, classes, transform, range, columns}`; relayout to
`<task>/<head>/<version>/<space>` **now**, while sentiment is the only task. This *is* roadmap Move-1,
expressed as a seam change so it benefits every future head. Effort: ~2–3 days; cheap now, a migration
after a second task ships.

**ISSUE-12 — `find_sentiment_score` carries two non-comparable score definitions behind one function +
a half-supported `glm` head — a provenance/audit hazard.**
*Raised by: ds S-A/S-B, arch §2, roadmap (provenance must include score definition).*
3-class heads return `P(pos)-P(neg)`; binary/legacy/glm return `(P-.5)*2` — different estimators on
different scales (the binary path has no neutral mass pulling the middle), so a `0.0` means different
things, and any USE-legacy-vs-e5 comparison compares two metrics. The `glm` CSV-weights path is neither
default nor benchmarked. **Fix:** the provenance stamp must record the **score definition**; converge
heads on the 3-class softprob formulation so they're comparable; either retire `glm` from the v2 default
surface **or** repurpose it as the *inherently-calibrated, fully-inspectable linear baseline* ("here's
the linear model's number next to the boosted one" is a genuine honesty artefact). Effort: hours to
decide + document; days to converge heads.

**ISSUE-13 — Embedding-orientation contract is fragile (cancelling transposes); the OpenAI-parallel
branch is dead and the live path is accidentally serial *and* un-batched.**
*Raised by: rpkg #2 (net-new orientation hazard), arch D4, mlops §7, cran-checklist F2.*
`embed_text`'s `(n,dim)` contract is achieved by two cancelling `t()`s across backends — and
`load_st_embedder` already returns `(n,dim)`, so routing it through the `hub_embed`/`t()` path would
transpose it **wrong**. Both arms of the `if(parallel>2)` call the *serial* `openai_embed`;
`openai_embed_parallel` (the only `pbapply` consumer) is **never called**, and its rate-limit counters
are a no-op even if it were. And `openai_embed` sends **one HTTP request per text** when the API takes a
batched array `input`. **Fix:** each backend returns canonical `(n,dim)` itself, delete the outer `t()`,
`embed_text` only sets rownames; **delete** `openai_embed_parallel` + drop `pbapply`; **batch**
`openai_embed` (one request per batch = 10–100× throughput + cost win on the paid path). Effort: half a day.

**ISSUE-14 — `sentiment.env` is an exported, user-writable mutable global with a boolean where a
4-valued backend belongs; no accessor/print/provenance.**
*Raised by: rpkg #3, arch D3, dx (status surface), roadmap table-stakes #4/#7.*
`export(sentiment.env)` means a user can do `sentiment.env$embed <- function(x) "lol"` and corrupt
scoring; the env is the central state yet has no class, no accessor, no print method, and its `$openai`
boolean can't represent `st`/legacy. **Fix:** un-export (reach it via `asNamespace`, as `helper-skips.R`
already does); replace the boolean with `$backend`; add a constructor + `sentiment_status()` that prints
`backend · model · revision · scorer · cache-loc` (the install-legibility fix **and** the provenance
surface in one); store `$prefix` here so the ISSUE-2 assertion is one line. Effort: ~1 day.

**ISSUE-15 — Revision pinning is half-wired: `model_meta.R` carries per-model SHAs but
`get_embedder.py:54` calls `SentenceTransformer(hf_id)` with **no `revision=`** — nothing consumes the
metadata; every e5 row is still `revision="main"`.**
*Raised by: mlops §3 + TOP-3, arch (provenance), roadmap Move-3.*
intfloat has historically re-pushed model formats; an unpinned user silently gets different embeddings →
different scores → an unreproducible published number. **Fix:** thread `revision = model_revision[model]`
through `init` → `load_st_embedder(hf_id, prefix, revision)` → `SentenceTransformer(hf_id, revision=)`;
pin every default row to a commit SHA before release; **CI gate: fail if any shipped default model's
revision is `"main"`/`NA`**; the provenance stamp records the **resolved** HF commit + the **scorer's
verified sha256** (what actually loaded, not what was requested) — a stamp that echoes `"main"` is
theatre. Effort: hours.

**ISSUE-16 — The "synthetic neutral raises pos/neg F1" headline has one data point AGAINST it and none
for it — do not publish; convert the liability into an OOD-drift feature instead.**
*Raised by: ds DS-3 (net-new: read the actual `rigor.log` numbers), methodology S1 (the 93%-synthetic finding), roadmap D4/Move-4 + scope-creep (don't fabricate).*
methodology S1 nailed that 40,576/43,437 (93.4%) neutral rows are GPT-synthetic. The net-new escalation:
`rigor.log`'s one controlled run shows synthetic neutral **lowering** real pos/neg F1 (neg 0.938→0.931,
pos 0.934→0.922), count-confounded and noisy — so the marquee finding the package wants to publish
currently has **zero clean evidence for and one point against**. Publishing it would violate the
never-fabricate / never-overclaim rule on the package's *own* headline. **Fix:** (1) don't put "synthetic
neutral helps" in NEWS/JOSS until a **count-matched** ablation (real-neutral substituted for synthetic,
fixed count, evaluated on 100%-real held-out neutral — cheaper than, and prerequisite to, the full
external-corpus eval) actually shows it; (2) the real-vs-synthetic-neutral embeddings are both on disk
and (per S1's own argument) separable — ship that separation as a **"this neutral is unlike
training-distribution neutral" drift warning** (reusing `R/matrix_helpers.R` cosine-to-centroid),
turning the construct-validity weakness into the auditability feature D4 already wants. Effort: a day for
the ablation; the drift probe is near-free.

### P3 — hygiene (grouped, fix in passing)

- **`scoring_version = match.arg` on a single value** (`sentiment.R:71`) — breaks the moment v1.1 ships;
  make it a membership check against the manifest version column. *(rpkg, mlops §7)*
- **`@export as_py_list`** — internal reticulate shim in the public API; `@keywords internal`. *(rpkg)*
- **`create_error_text()` is `cat()`-based** — not a catchable/classed condition; `cli::cli_abort()` +
  classes (`sentiment_ai_not_installed`, `…_model_missing`, `…_offline`) so downstream Shiny/pipelines
  can `tryCatch`. Also a parity concern (Python should raise the same taxonomy). *(rpkg, dx, ds, roadmap #7)*
- **`embed_text` warning-then-silently-init** (`embedding.R:187–198`) — recovers by *guessing* args (loads
  `e5-small` even if the user wanted `e5-base`); should `cli_abort(class="sentiment_ai_not_initialised")`
  with the next command. *(rpkg, dx, lineup)*
- **`test_parallel_support()` spins a full cluster on every init** just to count cores; `detectCores()`
  alone answers it and won't hang on locked-down boxes. *(mlops §4)*
- **Collate order:** `constants.R`/`model_meta.R` load 11th, after files that reference the registry;
  move them to the top of Collate. *(rpkg)*
- **`python_version` default `"3.8.10"`** is below what torch/sentence-transformers want (≥3.9). *(rpkg)*
- **`get_default_embedding`/versioned-JSON default-anchor cache** is an over-built optimisation (skips
  ~30 short embeds at the cost of a download + a version-keying scheme + a rownames dance); PARITY.md §5
  says Python should just embed defaults — strongly consider deleting it in R too. *(arch §7)*
- **`virtualenv_default_python()` drops reticulate's version check** — can pick a Python the e5 wheel
  doesn't support; pin a minimum. *(rpkg)*

---

## 2. BEST IDEAS (ranked by impact × effort, favouring honest-measurement + platform compounding)

These are the *generative* moves — the ones that make the package better, not just unbroken. Ranked so
the top of the list maximises (brand compounding × platform leverage) ÷ effort. Where an idea is
"already in the roadmap", the value-add here is the *structural form* that makes it true rather than
aspirational.

**IDEA-1 — One `Backend` descriptor + one language-neutral `inst/models.json`, routed everywhere.**
**[impact: very high · effort: medium · compounding: maximal]**
The keystone (= ISSUE-10's fix, stated as an opportunity). One object that says *"e5-small is:
kind=st, id=…, dim=384, prefix='query: ', scorer=…, license=MIT, revision=…"* passed down the call chain,
sourced from one JSON both languages read. It dissolves the entire class of dispatch bug, kills R↔Python
drift, and is the precondition for heads, provenance, and parity. **Do it as the ISSUE-1 wiring, not
after** — the patch and the structural fix are the same edit if sequenced right. This is the single
highest-compounding move in the repo: ~5 scattered derivations → 1 object, in both languages.

**IDEA-2 — Auditability as a returnable property: `sentiment_provenance()` derived from the descriptor +
head + resolved-commit + verified-scorer-sha256, with the prefix-as-scorer-property assertion.**
**[impact: very high · effort: low (once IDEA-1 lands) · compounding: maximal — this IS the brand]**
roadmap Move-3, made *structurally guaranteed*. A one-call stamp `{package_version, embedder,
embedder_revision (resolved, not requested), scoring, scoring_version, scorer_sha256, prefix, score_def}`
to paste into a methods section — and because it's *derived from the same objects that did the work*, it
**can't lie**. The `serve_prefix == scorer_prefix` assertion (ISSUE-2's permanent guard) is the same
contract. "Re-run in 2028, identical number" is the sharpest line a closed API *structurally cannot
match* — and it's tiny surface once IDEA-1 + the head descriptor exist. `model_meta.R` is already built
and waiting for this consumer. **This is the highest-impact idea in the set (see §5).**

**IDEA-3 — Structured tidy output that stops discarding the 3-class signal, as a *seam change*.**
**[impact: very high · effort: low–medium · compounding: high]**
roadmap Move-1 + arch Move-C. Default return one tidy row per input: `text, sentiment[-1,1], prob_neg,
prob_neu, prob_pos, class, confidence` (`output=c("tibble","vector")` for back-compat). The value-add over
"just return a tibble": make the **scorer seam return the full probability struct** and the [-1,1] collapse
a named downstream transform — so the rich output is *generic over heads*, not re-hand-written per task,
and every differentiator (calibration, abstention, mixed, agreement, drift) becomes reachable. The single
highest-compounding *feature* move: nearly free (delete one collapsing line at the seam), and it's the
precondition for everything below it.

**IDEA-4 — Promote `sentiment_match()` to the co-headline named-axis classifier — and refactor it to sit
*on* the head abstraction, not beside it.**
**[impact: high · effort: low (DX) → medium (structural) · compounding: high — platform + differentiation]**
Two reviewers converge from different angles: dx Fix-3 says it's the one feature *no competitor has*
(score any axis you name by example — urgency, churn-risk, formality — label-free) and it's buried as
"you can also be tricky"; arch §5c says it's *already* the platform's custom-axis head wearing a costume
(it re-embeds, re-does NA logic, even calls `sentiment_score` internally — a parallel monolith). **Fix
both at once:** add `classify_text(x, labels=…)` as the documented verb; surface the nearest-anchor +
cosine as the built-in *receipt* ("scored −0.66; nearest anchor 'sad' at cos 0.15" — the auditable-not-AI
brand made visible); unify its output shape with `sentiment_score` (same frame + `anchor`/`anchor_similarity`);
and route it through the **same embedding matrix** the sentiment head consumes. That refactor is what turns
"two functions" into "a platform." Honesty guardrail: calibrate the **margin** (cos-to-pos-centroid −
cos-to-neg-centroid, length-robust), not raw cosine, with the *same* machinery as the main scorer — one
calibration story, not two (ds S-E).

**IDEA-5 — Calibrate the 3-class probability VECTOR upstream of the collapse (not the scalar), with
per-class ECE on a real held-out set.**
**[impact: high · effort: medium · compounding: high — the "honest measurement" core]**
roadmap Move-2, with the net-new DS correction that prevents it from *re-introducing* dishonesty:
"isotonic on the score via `isoreg`" calibrates the **wrong object** — `P(pos)−P(neg)` averages two
miscalibration regimes (saturated poles vs the synthetic-polluted neutral middle), so a single monotone
scalar fit can worsen neutral while fixing the ends. **Correct:** temperature-scale the logits (one
scalar, monotone, accuracy-preserving; Guo 2017) as the cheap default, per-class isotonic/Dirichlet
(Kull 2019) as opt-in; re-derive the scalar from the *calibrated* vector; report **per-class** ECE +
reliability diagrams (a single aggregate ECE hides that neutral is the miscalibrated class); fit on a
**real** held-out slice, never the 93%-synthetic pool; document the [-1,1] scalar as a *reporting
transform*, not a calibrated probability. Depends on IDEA-3 (the exposed probs) + ISSUE-2 (a correctly
served embedding). "The only R sentiment package that publishes its calibration" is a line no competitor
has.

**IDEA-6 — The "knows its limits" trio: mixed flag + OOD/drift warning + abstention (risk–coverage
curve, not a magic threshold).**
**[impact: high · effort: medium · compounding: high — three warnings no competitor ships]**
roadmap Move-4, with the DS sharpening: ship the **risk–coverage curve** (El-Yaniv & Wiener 2010) as the
honesty *artefact* and let the user pick the operating point — a single baked "abstain if max prob < 0.5"
threshold *hides* the accuracy/coverage trade the user is buying. The OOD/drift warning reuses the
cosine-to-centroid code already in `R/matrix_helpers.R` **and** doubles as the ISSUE-16 synthetic-neutral
drift probe (same machinery, two payoffs). Separate the three zeros (neutral / mixed / model-uncertain).
Each warning fixes a real failure that is exactly Linnet's HR-verbatim home turf (bimodal "loved the
food, hated the service", silent domain drift, false confidence).

**IDEA-7 — One front door (`sentiment_ready()` ≡ Python `ensure_model()`) + a designed, progress-barred
first run; demote the conda dance to a legacy appendix.**
**[impact: high (adoption) · effort: low–medium · compounding: medium — DX = the install win made visible]**
dx Fix-2. Collapse install→init→check into one readiness verb that detects backend, reports what's
present, offers to fix it (one consented `cli` prompt + real progress bar, cached to `R_user_dir`), and
returns a structured object that doubles as support-ticket triage. Make the **existing lazy-load path the
documented path** — turn the scary `warning("...embed not found!")` into "first run: downloading e5-small
(~120 MB), one time…". v2 *already* made setup dramatically simpler by dropping TF; the DX should *show*
that win, not carry v1's reticulate scar tissue. Same status vocabulary in both languages.

**IDEA-8 — `sentiment_agreement()` + a fairness probe, framed against the human–human ceiling.**
**[impact: high (publishable) · effort: deep · compounding: very high — the rigor moat + a paper]**
roadmap Move-5. Replace `cosine(score, label)` as the headline metric with Krippendorff α / quadratic-κ /
ICC / confusion matrix against *human–human* agreement on multi-annotator corpora; `sentiment_fairness()`
on the Equity Evaluation Corpus / TwitterAAE — run it, publish the result including the bad parts. The
human-ceiling reframe ("κ=0.55 where humans reach 0.60 is excellent") is genuinely differentiating and the
authorship (IO-psych) can write the validity argument authentically. Sequence it *last* — it needs the
exposed, calibrated probabilities (IDEA-3/5) to compute on — but it's the move most likely to become a
JOSS/companion publication.

**IDEA-9 — An ordinal neutral head (promote from the roadmap's "defer" to "yes, on-recipe").**
**[impact: medium · effort: medium · compounding: medium — fixes the documented weak class honestly]**
ds S-C + scope-creep amendment. Neutral is the lowest-F1 class in *every* row of `full_results.log`/
`rigor.log`, and 3-way softmax throws away the neg<neu<pos ordering (it can confidently flip neg↔pos as
easily as neg↔neu). Two cumulative-threshold binary heads (Frank & Hall 2001) or a thresholded
regression-to-{-1,0,1} respects the ordering and typically helps exactly the middle class. This is
*on-recipe* (a light head on frozen embeddings) and improves the *neutral construct* the package most
needs — distinct from leaderboard-chasing because it measures the thing more honestly, not just for an
F1 point. **This is the one place to *add* to the accuracy work the roadmap (correctly) deprioritises.**

**IDEA-10 — A comparison vignette that shows where a competitor WINS.**
**[impact: medium · effort: medium · compounding: high — the honesty brand as docs]**
dx Fix-4. `vignette("vs-other-packages")`: the same sentences scored by `sentiment.ai`, `vader`,
`sentimentr`, `tidytext` side by side — **including a case where vader wins on a tweet**. Showing where
you lose is the most credible thing you can do, and it's the MEMORY rule (let the reader judge, don't
namecheck as a put-down). One artifact, two jobs: DX onboarding + brand proof. Pair with a runnable
`citation()` + provenance cell so "I can defend this number" is a *tutorial step*.

---

## 3. NET-NEW vs ALREADY-COVERED by the prior adversarial reviews

The prior set (`review-migration.md`, `review-lineup.md`, `review-methodology.md`,
`better-sentiment-roadmap.md`) is strong and the six new reviews largely *build on* it. Sorting honestly:

### Genuinely NET-NEW (not in the prior set — these justify the six-review pass)

1. **The runtime OpenMP collision (R-xgboost ⊕ python-torch) on the default path, with zero mitigation
   and a dev-Mac blind spot.** *(mlops §2)* The priors only discussed the *build-time* no-OpenMP xgboost
   (methodology:182) — a different, milder problem. This is a session-aborting runtime landmine nobody
   had named, on the happy path, invisible to the maintainer's own machine. **Highest-value net-new
   finding.**
2. **Train/serve prefix skew as a LIVE distribution bug, not a benchmark-fairness question.** *(ds DS-1,
   shiplead BLOCKER-2)* lineup SEV-1 framed the prefix as "dead config"; methodology S2 framed it as
   "the headline may be understated." The new reviews establish that the *shipped scorer* and the
   *serving path* are on different manifolds **right now** — the dangerous framing the priors missed.
3. **The `rigor.log` evidence actually contradicts the marquee finding.** *(ds DS-3)* methodology S1
   flagged the 93%-synthetic risk; the new pass *read the controlled run* and found synthetic neutral
   **lowering** real pos/neg F1 — i.e. one data point *against* the thing the package wants to publish.
   That's a never-overclaim guardrail on the package's own headline, newly grounded in numbers.
4. **Calibrate the probability VECTOR, not the scalar.** *(ds DS-2)* The roadmap said "calibrate (isotonic
   via `isoreg`)"; the new pass shows that calibrating the *scalar* `P(pos)−P(neg)` is the wrong object
   and can worsen the neutral middle. A correctness refinement that prevents the roadmap's own fix from
   re-introducing dishonesty.
5. **Download integrity / no checksum / `raw/main` is a moving target.** *(mlops §1.3/§1.4)* cran-checklist
   covered the *write-location* (A1–A5); nobody covered the *integrity* of the bytes that arrive — a
   silent-wrong-number path squarely against the brand.
6. **`.Rbuildignore` doesn't exclude the 58 MB of dev `.xgb` (tarball lands at 73 MB).** *(mlops §1.1)* A
   maintainer footgun distinct from the runtime-write issue; needs a CI tarball-size gate.
7. **Embedding-orientation contract via cancelling transposes (and `load_st_embedder` would be transposed
   wrong by the hub path).** *(rpkg #2)* A latent correctness hazard in no prior doc.
8. **The `Backend` descriptor as the keystone abstraction + one language-neutral `models.json`.** *(arch
   §1/Move-A)* The priors said "wire `model_class` dispatch" (a patch); arch reframes it as "build the
   object so the patch is structural and R↔Python can't drift" — a genuinely higher-altitude design move.
9. **Head/task axis + scorer-returns-full-struct as a storage relayout to do NOW.** *(arch §2/§5/Move-C)*
   The roadmap wanted the rich output; arch shows the *artifact path collides* for a second task and the
   relayout is cheap now / a migration later.
10. **`sentiment_match` is the custom-axis head wearing a costume — route it through the same embed
    matrix.** *(arch §5c)* A platform-structural observation the roadmap's "promote match" didn't make.
11. **Revision pinning is half-wired (`get_embedder.py:54` has no `revision=`).** *(mlops §3)* The priors
    knew `model_meta.R` carried SHAs; nobody noticed the loader never reads them.
12. **`callr` forked-process scorer as the bulletproof OpenMP fallback; resolved-commit + verified-sha256
    in the provenance stamp.** *(mlops §2.3/§3)* Concrete mechanisms beyond the roadmap's "provenance stamp."
13. **The DX funnel framing: the package is most honest in `planning/` and least honest in the README.**
    *(dx)* A net-new *organising* insight even where individual stale-copy facts overlap migration S7.
14. **OpenAI embed is un-batched (one HTTP request per text) — 10–100× throughput/cost win.** *(mlops §7,
    rpkg #2)* A real performance bug in the paid path.

### REINFORCED / GROUNDED with new evidence (priors flagged it; new reviews ran/located it)

- **Default path is broken end-to-end** — migration S0 *flagged* it; shiplead **reproduced** it live
  (`install_scoring_model("e5-small",…)` → the exact `match.arg` error) and counted tests (49 pass/5 fail).
- **CRAN blockers** — cran-checklist owns the list; rpkg **confirmed each is still live in the current
  tree** and **corrected** one stale premise (E3: `tests/` now exists → done).
- **e5-small-is-the-weakest-default** — lineup SEV-2 owns it; shiplead **re-escalates** it as a
  positioning own-goal and adds the bge-small same-footprint embarrassment (see §4).
- **The 3-class signal is discarded** — the roadmap's spine (all five lenses); the new reviews add the
  *seam-change* structural form (arch) and the *calibration-math* correctness (ds).

### ALREADY-COVERED (the new reviews correctly defer rather than repeat)

The migration S0–S7 breaks, the cran-checklist 14 blockers, the methodology S1–S10 rigor findings, and
the roadmap's table-stakes/differentiator/scope-creep structure are all **explicitly built-on, not
re-litigated** by the six reviews — each opens with a "what I deliberately don't repeat" section. The
synthesis preserves that discipline: the prior docs remain authoritative for their domains.

---

## 4. DISAGREEMENTS BETWEEN REVIEWERS — resolved

**D1 — Is the default model wrong? (e5-small vs e5-base)** *lineup SEV-2 + shiplead Challenge-1 say
e5-small (0.813) is the weakest no-TF option and a positioning own-goal; the locked lineup ships it as
default.*
**Resolution: do not ship the weakest option *silently* — but the resolution is a positioning decision,
not necessarily a model swap.** Two defensible paths: (a) **make e5-base the default**, demote e5-small
to the explicit "fast/light" opt-in → the headline becomes "ties OpenAI out of the box" and aligns with
what defaulting users actually run; or (b) **keep e5-small for footprint/speed/multilingual but state it
explicitly** in the model card ("default favours footprint; e5-base for max accuracy; e5's ~100-language
coverage is the reason to prefer it over the English-only bge-small that beats it at 384-D"). The one
thing forbidden by the brand is shipping the weakest model while the headline ("ties OpenAI" — which is
*e5-base*) sits next to it. **My call: (a) if multilingual is not the wedge; (b) with an explicit model
card if it is.** Either way this is gated on ISSUE-2 (the prefixed re-measure may move all these numbers).

**D2 — How big is the dispatch fix: a patch or a new abstraction?** *migration S0 / shiplead BLOCKER-1 /
rpkg #1 say "wire `model_class` dispatch + drop `match.arg`" (a bounded patch, hours); arch Move-A says
"build a `Backend` descriptor + `models.json`, route everything through it" (medium, days).*
**Resolution: they are the same edit at two altitudes — do the patch *as* the abstraction.** The
release-gate is the patch (ISSUE-1, hours, unblocks the no-arg call). But the patch *re-derives the model
in a sixth place* unless it's done by introducing the descriptor (ISSUE-10/IDEA-1). Sequencing: build the
descriptor, make `init` dispatch on it — that single move *is* the BLOCKER-1 fix **and** the structural
one. There is no real conflict, only a sequencing choice, and doing the cheap patch in a way that doesn't
add a seventh derivation costs ~1 extra day for permanent payoff.

**D3 — Is the prefix a "wire it in" task or a "retrain" task?** *lineup SEV-1 / migration S6 read it as
"apply the prefix at inference"; ds DS-1 / shiplead BLOCKER-2 read it as "the scorer was trained
un-prefixed, so wiring the prefix at inference *creates* skew."*
**Resolution: the ds/shiplead reading is correct and dominant — it's a retrain, not a flag.** Applying the
prefix at serve time (which CI now enforces) against an un-prefixed-trained scorer makes scores *worse*
with a green test. The fix order is fixed: settle the recipe → **re-embed + re-fit the e5 heads
prefixed** → re-measure the headline → wire the prefix idempotently → assert serve==scorer prefix → pin
with a golden fixture. "Apply the prefix" without the retrain is actively harmful. (ISSUE-2.)

**D4 — Calibration: isotonic-on-the-score (roadmap) vs calibrate-the-vector (ds)?**
**Resolution: ds wins.** Calibrate the 3-class probability vector upstream of the collapse (temperature
scaling default, per-class isotonic/Dirichlet opt-in), re-derive the scalar; the roadmap's "isotonic via
`isoreg` on the score" is *true but dangerous as stated* and would average two miscalibration regimes.
(IDEA-5.)

**D5 — Conformal prediction: near-term must (psychometric) or second-wave opt-in (roadmap/nlp)?**
**Resolution (already settled in the roadmap, upheld here): second-wave.** Ship calibrated-margin/entropy
abstention + the risk–coverage curve first (base R, table-stakes); conformal as the rigorous opt-in. The
multi-seed methodology fix is also *skippable* — ds notes `rigor.py`'s paired bootstrap already dominates
it (don't spend the 5×10-retrain compute).

**D6 — Legacy USE vendoring: v2 blocker (migration S3 ranks it TOP-3) or fast-follow (shiplead
Challenge-4)?**
**Resolution: shiplead — down-rank it to a documented caveat + a fast-follow.** tfhub.dev→Kaggle decay is
a real external time bomb, but vendoring the USE SavedModels is meaningful work for the *opt-in legacy*
path fewer users hit. Ship v2 with legacy honestly labelled ("best-effort, frozen-2021 stack, may stop
installing") + a directed error (ISSUE-6); defer the mirror. *Don't let perfecting the path you're
deprecating delay the path you're shipping.*

**D7 — `KMP_DUPLICATE_LIB_OK=TRUE`: the fix, or a dangerous floor?** *mlops offers it as the cheapest
mitigation; rpkg warns "don't ship it, it can mask corruption."*
**Resolution: it's the floor, not the fix.** Set it to convert an abort into a runnable session *and*
pin threads (`torch.set_num_threads(1)`, single-thread xgboost predict) *and* keep the `callr` forked-
scorer as the bulletproof fallback, *and* test under a stock-OpenMP-xgboost Linux CI leg. The env var
alone is insufficient and must ship with its caveat documented. (ISSUE-4.)

**D8 — Is "smoke-tested, produces correct scores, zero TF" evidence of readiness?** *the brief/roadmap
lean on it; shiplead Challenge-2 says it's misleading.*
**Resolution: shiplead — it exercised the Python function *directly*, bypassing `init → embed_text →
sentiment_score`, which is the path that errors.** A green smoke test on a bypassed path is *how the
`match.arg` bug survived*. Gate on the no-arg public call + the un-skipped golden test, not the smoke test.

---

## 5. IF YOU DO 5 THINGS

In dependency order. This is the minimum that converts "strong architecture, honest docs, broken default,
unproven numbers" into "a runnable, reproducible, honest v2 you'd depend on for batch analysis" — and it
front-loads the two silent-wrong-number hazards because a measurement brand cannot ship those.

1. **Make the default path run end-to-end — and do it by building the `Backend` descriptor, not just
   patching the dispatch.** Wire `init` to dispatch on the descriptor's `kind ∈ {st, openai, legacy}`,
   set `sentiment.env$backend`, drop the stale `match.arg(model)`. *Gate: a no-arg `sentiment_score("good")`
   returns a number with zero TensorFlow importable.* (ISSUE-1 + IDEA-1; ~1–2 days)

2. **Kill the train/serve prefix skew: re-embed the training corpus WITH `query:`, re-fit the e5 heads,
   re-measure the "ties OpenAI" headline on the prefixed recipe, and assert `serve_prefix == scorer_prefix`
   at score time.** This is the live silent-wrong-number bug; it must precede any calibration work.
   (ISSUE-2; half a day)

3. **Make distribution safe and the numbers pinned: `.Rbuildignore` + CI tarball-size gate; cache to
   `R_user_dir` via one helper with `HF_HOME` pinned; SHA-256 every download (atomic `.part`→rename,
   delete-and-error) served from an immutable Release tag; and commit `tests/fixtures/golden.csv` asserted
   to 1e-4 in both R and Python.** Distribution + reproducibility in one stroke; the golden fixture is what
   makes ISSUE-2's recipe permanent and "re-run in 2028, identical number" true. (ISSUE-3 + ISSUE-5;
   ~1–1.5 days, golden fixture after step 2's recipe is settled)

4. **Defuse the runtime OpenMP collision before tag, with a CI leg that can actually see it.** Set
   `KMP_DUPLICATE_LIB_OK=TRUE` + pin threads (`torch.set_num_threads(1)`, single-thread xgboost predict)
   as the floor; add the `callr` forked-scorer fallback; **add a Linux CI job with stock-OpenMP xgboost
   that embeds-then-scores in one session** (the dev Mac's no-OpenMP build cannot reproduce the bug).
   (ISSUE-4; half a day + a CI leg)

5. **Ship the honesty surface that the whole brand rests on: the tidy 3-class output (scorer returns the
   full struct, [-1,1] becomes a named transform) + the returnable `sentiment_provenance()` stamp +
   rewrite the README/vignette/roxygen to stop describing a v1 package.** This is the smallest set that
   turns "honest measurement" from a tagline in `planning/` into a property of the API the user holds — and
   it unblocks calibration, abstention, mixed-flag, agreement, and the platform's future heads.
   (IDEA-2 + IDEA-3 + ISSUE-8; ~2 days)

**Everything else is post-tag and sequenced after this core:** calibration (IDEA-5), the knows-its-limits
trio + synthetic-neutral drift probe (IDEA-6/ISSUE-16), `classify_text` + match-as-a-head (IDEA-4), the
ordinal neutral head (IDEA-9), agreement + fairness (IDEA-8), the comparison vignette (IDEA-10), CRAN
hygiene (ISSUE-7), semver/legacy gate (ISSUE-6/9). None of it should block the five above, and none of it
is buildable on a pipeline whose base scores aren't yet correct and reproducible.

---

### The single highest-impact idea (if only one thing)

**Make auditability a returnable, structurally-guaranteed property — `sentiment_provenance()` derived from
the `Backend` descriptor + head + the *resolved* HF commit + the *verified* scorer SHA-256, with the
`serve_prefix == scorer_prefix` assertion enforced at score time (IDEA-2).** It is the one move that is
simultaneously: the brand's sharpest line ("re-run in 2028, get the identical number" — a thing a closed
API *structurally cannot* offer); the permanent fix that makes the train/serve prefix skew (the worst live
bug) impossible to reintroduce; nearly free once the descriptor and head exist because `model_meta.R` is
already built and waiting for a consumer; and the shared idiom the Python port and the companion
`textgraph` package inherit, so every measurement in a study is provably on the same vectors. It turns
"honest measurement" from a claim in the docs into a property of the types — the highest brand-compounding
per line of code in the entire repo.
