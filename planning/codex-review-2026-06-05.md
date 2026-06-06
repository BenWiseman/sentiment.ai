# sentiment.ai v2 Codex review swarm - 2026-06-05

Scope: read + report review of branch `v2` at `f903a37f081c72ab47271e71c1bc8d6c4c9dd8ce`.

Repo surfaces reviewed:

- R package: `rpackage/sentiment.ai/`
- Python package: `pypackage/`
- Product/roadmap docs: `planning/better-sentiment-roadmap.md`, `planning/repo-review-*.md`, `planning/cran-checklist.md`, `planning/v2-test-plan.md`
- User-facing docs: root `README.md`, `docs/index.rmd`, `rpackage/sentiment.ai/README.md`, `pypackage/README.md`

Method: six specialist reviewers were dispatched. The first TESTER agent errored during compaction, so a replacement TESTER was spawned and completed. P0/P1 items below were included only when reproduced locally or independently corroborated by another reviewer.

## Verification run

Commands run locally from `/Users/benwiseman/sentiment.ai`:

```sh
git status --short --branch
git branch --show-current
git rev-parse --short HEAD
PYTHONPATH=pypackage python3 -m pytest pypackage/tests -q
Rscript -e 'pkgload::load_all("rpackage/sentiment.ai", quiet=TRUE); testthat::test_dir("rpackage/sentiment.ai/tests/testthat", package="sentiment.ai", reporter="summary")'
tmp=$(mktemp -d); (cd "$tmp" && R CMD build /Users/benwiseman/sentiment.ai/rpackage/sentiment.ai --no-build-vignettes --no-manual)
tmp=$(mktemp -d); (cd "$tmp" && _R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran --no-manual sentiment.ai_1.0.0.tar.gz)
```

Observed:

- Git: `## v2...origin/v2`, clean before report creation.
- Local R source tests: passed, with 14 skips for live backend / fixture-gated tests.
- Python tests: `1 failed, 12 passed, 1 skipped`; failure is `sentiment_match()` similarity slightly above `1.0`.
- `R CMD build --no-build-vignettes --no-manual`: built tarball at about `1.9M`; xgb files were excluded by `.Rbuildignore`.
- `R CMD check --as-cran` with full suggests: stopped on missing local `text2vec`.
- `R CMD check --as-cran` with `_R_CHECK_FORCE_SUGGESTS_=false`: `3 WARNINGs, 6 NOTEs`.

## Reviewer summaries

### TESTER

1. [P1 should-fix] R `sentiment_match()` can blank valid rows when a real text equals a generated missing-row placeholder. Evidence: `rpackage/sentiment.ai/R/sentiment.R:176`, `rpackage/sentiment.ai/R/sentiment.R:229`. Repro: `sentiment_match(c("2", NA, "ok"), ...)` returns row 1 as missing. Fix: restore missing rows by integer row id / `temp_id`, not keyed `text`; add this exact regression test.
2. [P1 should-fix] Python `sentiment_match()` emits cosine values just above `1.0` and fails its own test. Evidence: `pypackage/sentimentai/sentiment.py:132`, `pypackage/sentimentai/sentiment.py:146`, `pypackage/tests/test_sentiment_match.py:61`. Fix: clamp similarity to `[-1, 1]` after the cosine matrix or at assignment.
3. [P1 should-fix] R golden/scoring parity tests do not protect the current default JSON scoring heads. Evidence: `rpackage/sentiment.ai/tests/testthat/helper-skips.R:64`, `rpackage/sentiment.ai/tests/testthat/test-snapshot-scorer.R:12`, `rpackage/sentiment.ai/tests/testthat/test-scoring-parity.R:6`, `rpackage/sentiment.ai/R/sentiment.R:52`. Fix: commit offline fixtures for `e5-small` / `e5-base` with `mlp` / `logistic`; fail CI when release fixtures are absent.
4. [P1 should-fix] R and Python OpenAI model resolution diverge. Evidence: `rpackage/sentiment.ai/R/constants.R:9`, `rpackage/sentiment.ai/R/choose_model.R:19`, `pypackage/sentimentai/_models.py:28`, `pypackage/sentimentai/_models.py:60`. Fix: both languages should accept `openai` and raw OpenAI IDs, with `openai` mapping to `text-embedding-3-small`.
5. [P1 should-fix] e5 revision/provenance is not reproducibility-grade. Evidence: `rpackage/sentiment.ai/R/model_meta.R:62`, `rpackage/sentiment.ai/inst/get_embedder.py:54`, `pypackage/sentimentai/embedding.py:35`, `rpackage/sentiment.ai/R/provenance.R:41`. Fix: pin HF SHAs, pass `revision=`, and compare serve prefix against scorer metadata.
6. [P2 nice] Default-poles md5 tests guard each package's local copy, not direct cross-package drift. Evidence: `rpackage/sentiment.ai/tests/testthat/test-sentiment-match.R:98`, `pypackage/tests/test_sentiment_match.py:117`. Fix: add a repo-level direct `cmp`/hash test between R and Python JSON files.
7. [P2 nice] Single-pole custom `phrases` diverge: R returns a one-pole result; Python raises. Evidence: `rpackage/sentiment.ai/R/sentiment.R:152`, `pypackage/sentimentai/sentiment.py:118`. Fix: choose one contract; recommendation is to require at least two poles in both.

### R EXPERT

1. [P0 blocker] Public serve surface still exposes `xgb` and legacy TF paths despite the v2 invariant. Evidence: `rpackage/sentiment.ai/R/sentiment.R:52`, `rpackage/sentiment.ai/R/sentiment.R:276`, `rpackage/sentiment.ai/R/init_and_install.R:389`, `rpackage/sentiment.ai/DESCRIPTION:53`. Fix: remove `xgb`/`glm` from public release scoring choices and default installer, or stop claiming no xgboost at serve.
2. [P0 blocker] Package metadata overclaims performance. Evidence: `rpackage/sentiment.ai/DESCRIPTION:30`. Fix: replace "out-performing" with measured, scoped "matches/compares on documented benchmark" language, or remove it.
3. [P1 should-fix] Confirmed: `sentiment_score()` still discards the 3-class signal. Evidence: `rpackage/sentiment.ai/R/sentiment.R:100`, `rpackage/sentiment.ai/R/sentiment.R:282`, `rpackage/sentiment.ai/R/sentiment.R:315`, `rpackage/sentiment.ai/R/sentiment.R:316`. Fix: return `prob_neg`, `prob_neu`, `prob_pos`, `class`, `confidence`, and scalar `sentiment`; keep vector output as explicit legacy mode.
4. [P1 should-fix] R `sentiment_match()` placeholder-row bug. Evidence and fix match TESTER item 1.
5. [P1 should-fix] Default installer downloads legacy scorers into the installed package directory. Evidence: `rpackage/sentiment.ai/R/init_and_install.R:203`, `rpackage/sentiment.ai/R/init_and_install.R:279`, `rpackage/sentiment.ai/R/init_and_install.R:300`. Fix: stop default `xgb`/`glm` downloads; optional downloads must use `tools::R_user_dir()`, immutable release URLs, and checksums.
6. [P1 should-fix] Current `R CMD check` still reports CRAN hygiene issues. Evidence: `rpackage/sentiment.ai/R/init_and_install.R:140`, `rpackage/sentiment.ai/R/embedding.R:60`, `rpackage/sentiment.ai/R/embedding.R:238`, `rpackage/sentiment.ai/R/embedding.R:75`. Fix: replace `reticulate:::`, declare/fix `n_class`, qualify/import progress-bar utils, and document or hide `hub_embed()`.
7. [P1 should-fix] Provenance still reports moving HF revisions and loaders do not consume revisions. Evidence and fix match TESTER item 5.
8. [P2 nice] `sentiment.env` remains exported mutable global state. Evidence: `rpackage/sentiment.ai/R/object-sentiment_env.R:8`, `rpackage/sentiment.ai/NAMESPACE:11`. Fix: unexport it, add public `sentiment_status()` / provenance accessors, keep mutation internal.

### CRAN REVIEWER

Verdict: block submission. `R CMD check --as-cran` is not clean.

1. [P0 blocker] `hub_embed.Rd` is malformed and causes an Rd usage WARNING. Evidence: `rpackage/sentiment.ai/R/embedding.R:75`, `rpackage/sentiment.ai/man/hub_embed.Rd:6`. Fix: mark `hub_embed()` `@noRd` if internal, or document `text` and `batch_size`, then regenerate docs.
2. [P0 blocker] `reticulate:::` is still used. Evidence: `rpackage/sentiment.ai/R/init_and_install.R:140`. Fix: call the local `py_install_method_detect()` wrapper at `rpackage/sentiment.ai/R/init_and_install.R:577`.
3. [P0 blocker] Runtime downloads write into the installed package directory. Evidence: `rpackage/sentiment.ai/R/init_and_install.R:278`, `rpackage/sentiment.ai/R/init_and_install.R:300`, `rpackage/sentiment.ai/R/init_and_install.R:408`. Fix: route all downloaded scorer/model/cache writes through `tools::R_user_dir("sentiment.ai", "cache")`; update read paths accordingly.
4. [P0 blocker] Benchmark copy still overclaims. Evidence: `rpackage/sentiment.ai/DESCRIPTION:30`, `rpackage/sentiment.ai/NEWS.md:126`. Fix: replace "out-performing" and "meets or beats" with measured, scoped "matches" language unless a paired test supports the exact claim.
5. [P1 should-fix] R code NOTE for undefined globals / unqualified utils. Evidence: `rpackage/sentiment.ai/R/embedding.R:60`, `rpackage/sentiment.ai/R/init_and_install.R:410`, `rpackage/sentiment.ai/R/init_and_install.R:437`, `rpackage/sentiment.ai/R/embedding.R:238`. Fix: add `utils::` qualifiers/imports and either declare dynamic Python names or capture sourced Python functions explicitly.
6. [P1 should-fix] Non-standard top-level files trigger a NOTE. Evidence: `rpackage/sentiment.ai/get_embedder_v2.py:1`, `rpackage/sentiment.ai/m2_mbp_requirements.txt:1`, `rpackage/sentiment.ai/m3_mbp_requirements.txt:1`. Fix: move under `inst/` if intentional or add narrow `.Rbuildignore` entries.
7. [P1 should-fix] `rstudioapi` is in Suggests but used unconditionally. Evidence: `rpackage/sentiment.ai/DESCRIPTION:61`, `rpackage/sentiment.ai/R/init_and_install.R:208`. Fix: guard with `interactive() && requireNamespace("rstudioapi", quietly=TRUE)`; consider `restart_session = interactive()`.
8. [P1 should-fix] Reproducibility pinning is not actually pinned. Evidence and fix match TESTER item 5.

### SENIOR ENGINEER

1. [P0 blocker] R still exposes and installs an xgboost serve path despite the frozen-embedding + JSON-head contract. Evidence: `rpackage/sentiment.ai/DESCRIPTION:53`, `rpackage/sentiment.ai/R/sentiment.R:52`, `rpackage/sentiment.ai/R/sentiment.R:276`, `rpackage/sentiment.ai/R/init_and_install.R:204`. Fix: remove `xgb`/`glm` from release public scoring and default installer, or change the claim.
2. [P0 blocker] Release metadata violates no-overclaim/no-AI-frame rule. Evidence: `rpackage/sentiment.ai/DESCRIPTION:3`, `rpackage/sentiment.ai/DESCRIPTION:29`, `pypackage/pyproject.toml:17`. Fix: rewrite as deterministic sentence-embedding scoring with bundled JSON heads; replace "out-performing"; remove the "Artificial Intelligence" classifier if that framing is banned.
3. [P1 should-fix] R/Python model registries are not parity-compatible for OpenAI names. Evidence and fix match TESTER item 4.
4. [P1 should-fix] `prefix_ok` does not verify the scorer's training prefix. Evidence: `rpackage/sentiment.ai/R/provenance.R:41`, `rpackage/sentiment.ai/R/provenance.R:31`. Fix: stamp JSON heads with `embedding_model`, `train_prefix`, `model_revision`, `score_definition`, and `sha256`; assert before scoring in R and Python.
5. [P1 should-fix] Embedding revisions remain unpinned and ignored. Evidence and fix match TESTER item 5.
6. [P1 should-fix] Score parity is under-proven; R fixture tests skip and Python tests fail. Evidence: `rpackage/sentiment.ai/tests/testthat/test-snapshot-scorer.R:12`, `pypackage/tests/test_sentiment_match.py:61`. Fix: commit current JSON-head fixtures and clamp/tolerate cosine.
7. [P1 should-fix] Non-default scorer download path is unsafe. Evidence: `rpackage/sentiment.ai/R/init_and_install.R:253`, `rpackage/sentiment.ai/R/init_and_install.R:277`, `rpackage/sentiment.ai/R/init_and_install.R:298`. Fix: `R_user_dir`, immutable release assets, checksum/bytes verification, atomic `.part` rename.
8. [P2 nice] Error handling is not classed across R/Python. Evidence: `rpackage/sentiment.ai/R/create_error_text.R:12`, `rpackage/sentiment.ai/R/embedding.R:151`, `rpackage/sentiment.ai/R/init_and_install.R:378`, `pypackage/sentimentai/embedding.py:41`. Fix: classed R conditions and matching Python exception classes.

### ADHD CREATIVE GENIUS

1. [P0 blocker] xgboost still exists as a public serve path. Evidence and fix match SENIOR ENGINEER item 1. Synthesis downgrades this only if the project explicitly treats it as legacy-only; otherwise it is a release invariant violation.
2. [P0 blocker] Auditability claim is not true while revisions are `main` and no loader consumes `revision=`. Evidence and fix match TESTER item 5.
3. [P0 blocker] Public metadata violates brand voice. Evidence: `rpackage/sentiment.ai/DESCRIPTION:29`, `rpackage/sentiment.ai/DESCRIPTION:30`. Fix: deterministic/on-device wording; remove unqualified "out-performing".
4. [P1 should-fix] "Calibrated score" is claimed without shipped calibration evidence. Evidence: `rpackage/sentiment.ai/README.md:121`, `rpackage/sentiment.ai/NEWS.md:148`, `rpackage/sentiment.ai/R/sentiment.R:313`. Fix: say "temperature-scaled head score" or "head score" until reliability/ECE/Brier evidence ships.
5. [P1 should-fix] Roadmap D3/D4 overpromise mixed sentiment from a 3-class softmax. Evidence: `planning/better-sentiment-roadmap.md:93`, `planning/better-sentiment-roadmap.md:100`, `rpackage/sentiment.ai/R/sentiment.R:313`. Fix: use sentence-variance first; use independent one-vs-rest heads or calibrated margins for true bivariate affect.
6. [P1 should-fix] `sentiment_match()` framing is too broad as "any user-named axis." Evidence: `planning/better-sentiment-roadmap.md:81`, `planning/better-sentiment-roadmap.md:83`, `rpackage/sentiment.ai/README.md:136`. Fix: frame as "anchored semantic axis"; add pole separation and stability diagnostics.
7. [P1 should-fix] Python registry benchmark notes are stale/provisional while README/NEWS carry full-data numbers. Evidence: `pypackage/sentimentai/_models.py:8`, `pypackage/sentimentai/_models.py:31`, `pypackage/sentimentai/_models.py:35`, `pypackage/sentimentai/_models.py:39`, `rpackage/sentiment.ai/NEWS.md:71`. Fix: remove F1 values from runtime registries or generate from one benchmark manifest.
8. [P2 nice] Planning docs still contain pre-`f903a37` facts that cause review churn. Evidence: `planning/cran-checklist.md:7`, `planning/v2-test-plan.md:29`. Fix: add superseded banners or a current `planning/f903a37-state.md`.

Roadmap deltas from this reviewer are folded into the final "Roadmap deltas" section.

### STUDENT / END USER

1. [P0 blocker] Default e5 embedding is not actually frozen. Evidence and fix match TESTER item 5.
2. [P1 should-fix] R quick start installs the old CRAN package, not this v2 experience. Evidence: root `README.md:32`, `docs/index.rmd:90`, `rpackage/sentiment.ai/README.md:17`. Fix: until v2 is on CRAN, show `remotes::install_github(..., ref="v2")` or add a clear "CRAN is still 0.1.1" banner.
3. [P1 should-fix] Python README says `0.1.0a2`, but `pip install --pre sentimentai-py` resolves an older alpha for the reviewer. Evidence: `pypackage/README.md:7`, `pypackage/README.md:16`. Fix: publish `0.1.0a2` or change the README to the published version and add a post-install version check.
4. [P1 should-fix] Score meaning and trust boundaries are underexplained while "calibrated" is asserted. Evidence: root `README.md:88`, `docs/index.rmd:194`, `rpackage/sentiment.ai/README.md:108`. Fix: add "Interpreting scores / When not to trust them"; define `P(pos) - P(neg)`, zero ambiguity, neutral/mixed/uncertain/domain shift/sarcasm limits; remove "calibrated" unless linked to evidence.
5. [P1 should-fix] Benchmark evidence links are not newcomer-usable. Evidence: root `README.md:76` points to missing root `NEWS.md`; `docs/index.rmd:152` points to plain planning markdown. Fix: link to stable rendered benchmark page and/or `rpackage/sentiment.ai/NEWS.md`.
6. [P1 should-fix] R setup still downloads legacy scoring artifacts despite docs saying bundled JSON heads. Evidence: `rpackage/sentiment.ai/README.md:62`, `rpackage/sentiment.ai/R/init_and_install.R:203`, `rpackage/sentiment.ai/R/init_and_install.R:253`. Fix: stop default `xgb`/`glm` fetches.
7. [P2 nice] R no-setup failure is cryptic. Evidence: `rpackage/sentiment.ai/README.md:94`, `rpackage/sentiment.ai/R/init_and_install.R:450`, `rpackage/sentiment.ai/R/init_and_install.R:485`. Fix: add `sentiment_status()` and directed "run install_sentiment.ai()" error.
8. [P2 nice] Example outputs are stale/rounded. Evidence: `pypackage/README.md:27`, root `README.md:41`. Fix: update to exact current outputs or mark illustrative.

## Synthesized action list

### P0 blockers

1. **Make the release invariant true: no xgboost at serve, or stop claiming it.**  
   Evidence: `rpackage/sentiment.ai/R/sentiment.R:52`, `rpackage/sentiment.ai/R/sentiment.R:276`, `rpackage/sentiment.ai/R/init_and_install.R:203`, `rpackage/sentiment.ai/DESCRIPTION:53`.  
   Verified by: R EXPERT, SENIOR ENGINEER, ADHD CREATIVE GENIUS, local source read.  
   Fix: remove `xgb`/`glm` from public v2 scoring choices and default installer; drop `xgboost` from `Imports` if no runtime path remains. If retained for legacy compatibility, move behind explicit legacy/dev API and remove "no xgboost at serve" from release copy.

2. **Fix CRAN check blockers before submission.**  
   Evidence: `hub_embed.Rd` undocumented args at `rpackage/sentiment.ai/man/hub_embed.Rd:6`; `reticulate:::` at `rpackage/sentiment.ai/R/init_and_install.R:140`; visible globals/utils at `rpackage/sentiment.ai/R/embedding.R:60`, `rpackage/sentiment.ai/R/embedding.R:238`; vignette output warnings from `rpackage/sentiment.ai/vignettes/vignette.Rmd:1`; top-level file NOTE at `rpackage/sentiment.ai/get_embedder_v2.py:1`.  
   Verified by: CRAN REVIEWER, R EXPERT, local `R CMD check --as-cran`.  
   Fix: mark/document `hub_embed`; replace `reticulate:::` with the local wrapper; qualify/import `utils::txtProgressBar` and `utils::setTxtProgressBar`; declare/fix `n_class` and dynamic Python functions; either build/preinstall vignette output or adjust vignette handling; ignore/move non-standard top-level files; install/remove `text2vec` from Suggests as appropriate.

3. **Remove brand/metadata overclaims and banned framing.**  
   Evidence: `rpackage/sentiment.ai/DESCRIPTION:3`, `rpackage/sentiment.ai/DESCRIPTION:29`, `rpackage/sentiment.ai/DESCRIPTION:30`, `pypackage/pyproject.toml:17`, `rpackage/sentiment.ai/NEWS.md:126`.  
   Verified by: CRAN REVIEWER, SENIOR ENGINEER, ADHD CREATIVE GENIUS, R EXPERT.  
   Fix: describe the package as deterministic sentiment scoring from frozen sentence embeddings and small JSON heads. Replace "out-performing" / "meets or beats" with measured, scoped "matches" language or cite a paired test for the exact claim. Remove "Artificial Intelligence" classifier if the project wants no AI framing for its output.

4. **Make "frozen/auditable" real: pin e5 revisions and score metadata.**  
   Evidence: `rpackage/sentiment.ai/R/model_meta.R:62`, `rpackage/sentiment.ai/R/model_meta.R:63`, `rpackage/sentiment.ai/inst/get_embedder.py:54`, `pypackage/sentimentai/embedding.py:35`, `rpackage/sentiment.ai/R/provenance.R:41`.  
   Verified by: TESTER, STUDENT, SENIOR ENGINEER, ADHD CREATIVE GENIUS, local provenance/loader read.  
   Fix: pin e5-small/base to immutable HF SHAs, pass `revision=` in R and Python loaders, record resolved revision. Add scorer metadata (`embedding_model`, `train_prefix`, `score_definition`, head hash) and assert serve prefix/model against the scorer before scoring.

5. **Stop unsafe optional downloads to installed package paths and moving `raw/main`.**  
   Evidence: `rpackage/sentiment.ai/R/init_and_install.R:253`, `rpackage/sentiment.ai/R/init_and_install.R:277`, `rpackage/sentiment.ai/R/init_and_install.R:298`, `rpackage/sentiment.ai/R/init_and_install.R:408`.  
   Verified by: CRAN REVIEWER, SENIOR ENGINEER, R EXPERT, STUDENT.  
   Fix: do not download `xgb`/`glm` by default. Any optional external artifact should use `tools::R_user_dir("sentiment.ai", "cache")`, immutable release assets, `sha256`/byte checks, and atomic `.part` rename.

### P1 should-fix

1. **Fix R `sentiment_match()` placeholder-row corruption.**  
   Evidence: `rpackage/sentiment.ai/R/sentiment.R:176`, `rpackage/sentiment.ai/R/sentiment.R:229`.  
   Verified by: TESTER, R EXPERT, local repro.  
   Fix: restore missing/empty rows by position (`temp_id`), not by text key; add regression for `sentiment_match(c("2", NA, "ok"), ...)`.

2. **Fix Python `sentiment_match()` similarity bounds.**  
   Evidence: `pypackage/sentimentai/sentiment.py:132`, `pypackage/sentimentai/sentiment.py:146`, `pypackage/tests/test_sentiment_match.py:61`.  
   Verified by: TESTER, SENIOR ENGINEER, local pytest.  
   Fix: clamp cosine similarities to `[-1, 1]` or loosen test tolerance while returning bounded public values.

3. **Unify R/Python OpenAI model aliases.**  
   Evidence: `rpackage/sentiment.ai/R/constants.R:9`, `rpackage/sentiment.ai/R/choose_model.R:19`, `pypackage/sentimentai/_models.py:28`, `pypackage/sentimentai/_models.py:60`, `rpackage/sentiment.ai/README.md:59`.  
   Verified by: TESTER, SENIOR ENGINEER, local resolver checks.  
   Fix: both packages accept `openai`, `text-embedding-3-small`, `text-embedding-3-large`, and `text-embedding-ada-002`; add parity tests.

4. **Commit real offline golden fixtures for the current JSON-head default.**  
   Evidence: only `rpackage/sentiment.ai/tests/testthat/fixtures/README.md` exists; fixture-gated tests at `rpackage/sentiment.ai/tests/testthat/test-snapshot-scorer.R:12` and `rpackage/sentiment.ai/tests/testthat/test-scoring-parity.R:6` skip.  
   Verified by: TESTER, SENIOR ENGINEER, local fixture listing.  
   Fix: commit `e5-small`/`e5-base` embeddings and expected scores for `mlp`/`logistic`; make release CI fail if required fixtures are absent.

5. **Stop calling the score "calibrated" until calibration evidence ships.**  
   Evidence: `rpackage/sentiment.ai/README.md:121`, `docs/index.rmd:204`, `rpackage/sentiment.ai/NEWS.md:148`, `pypackage/sentimentai/sentiment.py:105`, `rpackage/sentiment.ai/R/sentiment.R:313`.  
   Verified by: ADHD CREATIVE GENIUS, STUDENT, local code/doc read.  
   Fix: use "temperature-scaled head score" or "head score" for now. Keep "calibrated" only when the 3-class probability vector has a measured calibration report (ECE/Brier/reliability diagram).

6. **Expose the 3-class signal instead of only `P(pos) - P(neg)`.**  
   Evidence: `rpackage/sentiment.ai/R/sentiment.R:282`, `rpackage/sentiment.ai/R/sentiment.R:315`, `rpackage/sentiment.ai/R/sentiment.R:316`, `pypackage/sentimentai/_scoring.py:78`.  
   Verified by: R EXPERT and prior roadmap consensus.  
   Fix: scorer returns full probabilities and a derived scalar. Public API should offer `prob_neg`, `prob_neu`, `prob_pos`, `class`, `confidence`, and `sentiment`; keep legacy vector output explicit.

7. **Fix newcomer install/version truth.**  
   Evidence: root `README.md:32`, `docs/index.rmd:90`, `rpackage/sentiment.ai/README.md:17`, `pypackage/README.md:7`, `pypackage/README.md:16`.  
   Verified by: STUDENT.  
   Fix: until v2 is actually on CRAN/PyPI, show branch/pre-release install commands or add a visible "CRAN/PyPI may still be older" banner.

8. **Make benchmark evidence stable and reachable.**  
   Evidence: root `README.md:76` points to missing root `NEWS.md`; `docs/index.rmd:152` points to `planning/benchmark-v2.md`.  
   Verified by: STUDENT, local `ls`.  
   Fix: link root README and site to a rendered benchmark page with method, dataset counts, CIs, score provenance, and limitations.

9. **Align single-pole `sentiment_match()` behaviour.**  
   Evidence: `rpackage/sentiment.ai/R/sentiment.R:152`, `pypackage/sentimentai/sentiment.py:118`.  
   Verified by: TESTER.  
   Fix: choose the contract; recommendation is to require at least two poles in both packages.

10. **Add score interpretation and limitations to user docs.**  
    Evidence: no "when not to trust" section in root README/site/R README; current score copy at root `README.md:88`, `docs/index.rmd:194`, `rpackage/sentiment.ai/README.md:108`.  
    Verified by: STUDENT, ADHD CREATIVE GENIUS.  
    Fix: explain `P(pos) - P(neg)`, zero ambiguity, neutral/mixed/uncertain cases, domain shift, sarcasm/negation limitations, and text-leaves-device for OpenAI.

### P2 nice

1. Unexport mutable `sentiment.env`; replace with `sentiment_status()` and accessors. Evidence: `rpackage/sentiment.ai/R/object-sentiment_env.R:8`, `rpackage/sentiment.ai/NAMESPACE:11`.
2. Add classed R conditions and Python exception classes. Evidence: `rpackage/sentiment.ai/R/create_error_text.R:12`, `rpackage/sentiment.ai/R/embedding.R:151`, `pypackage/sentimentai/embedding.py:41`.
3. Add repo-level default-poles file equality test instead of only per-package md5 constants. Evidence: `rpackage/sentiment.ai/tests/testthat/test-sentiment-match.R:98`, `pypackage/tests/test_sentiment_match.py:117`.
4. Remove F1 values from runtime registries or generate them from a single benchmark manifest. Evidence: `pypackage/sentimentai/_models.py:8`, `pypackage/sentimentai/_models.py:31`, `pypackage/sentimentai/_models.py:35`, `pypackage/sentimentai/_models.py:39`.
5. Mark stale planning docs as superseded or add a current `planning/f903a37-state.md`. Evidence: `planning/cran-checklist.md:7`, `planning/v2-test-plan.md:29`.
6. Improve no-setup R error with a directed "run install_sentiment.ai()" message. Evidence: `rpackage/sentiment.ai/R/init_and_install.R:450`, `rpackage/sentiment.ai/R/init_and_install.R:485`.
7. Update rounded/example outputs or mark illustrative. Evidence: root `README.md:41`, `pypackage/README.md:27`.

## Roadmap deltas

1. **Keep D1, but make it "measurement receipts."**  
   Add a per-result receipt containing input hash, model handle, HF revision, scorer hash, score definition, prefix, and optional nearest anchor. Cheap first version: `sentiment_receipt()` returns JSON-able metadata for one result. Measurable claim: receipt + fixture reproduces score within tolerance in R and Python.

2. **Keep D2, but narrow the promise to anchored semantic axes.**  
   Avoid marketing "any user-named axis" or examples like churn-risk as a validated construct. Cheap first version: `axis_diagnostics()` reports pole separation, nearest-anchor stability, and leave-one-anchor-out variance.

3. **Move calibration before confidence/abstention claims.**  
   Calibrate the 3-class probability vector, not the collapsed scalar. Report per-class ECE/Brier/reliability diagrams. The scalar remains a derived convenience, not itself a calibrated probability.

4. **Replace "mixed from high p_pos and p_neg" with a defensible first version.**  
   A 3-class softmax simplex does not give independent positive and negative intensities. Cheap first version: sentence-level variance and contrast flags. True bivariate affect needs independent one-vs-rest heads or calibrated positive/negative anchor margins.

5. **Add a known-failure deck before a broad fairness/validity program.**  
   D5 is valuable but deeper than the immediate release gate. A cheaper visible differentiator is a committed challenge set for negation, sarcasm, mixed text, domain drift, and competitor-win examples, with honest failures published.

6. **Promote "limits it knows" only after the telemetry exists.**  
   Entropy/margin, OOD centroid similarity, sentence variance, and risk-coverage curves can become a `sentiment_diagnostics()` surface, but should not be implied by current scalar output.

## Disagreements resolved

- Reviewers labeled xgboost public availability as P0. I keep it P0 only relative to the explicit v2 invariant "no xgboost at serve." If the project intentionally preserves xgb as a legacy escape hatch, then the fix is to quarantine it and change release copy rather than delete it outright.
- `R CMD check` severity varied across reviewers because one run stopped on missing `text2vec`, while another used `_R_CHECK_FORCE_SUGGESTS_=false`. The actionable conclusion is the same: not submit-ready.
- The old roadmap's "structured 3-class output" remains correct, but it is not a CRAN blocker by itself. It is P1 because it underlies calibration, confidence, abstention, and honest interpretation.
- "Calibrated" wording is treated as P1 rather than P0 because it is fixable by copy changes if calibration is not part of this release. It becomes P0 if release copy claims measured calibration.
