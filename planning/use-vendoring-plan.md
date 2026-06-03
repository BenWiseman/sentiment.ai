# Legacy USE vendoring plan (sentiment.ai v2)

**Author:** planning pass, 2026-06-03. **Scope:** keep the four legacy Universal Sentence
Encoder (USE) SavedModels loadable after `tfhub.dev` deprecation, so `legacy = TRUE` users get
the same numbers as v0.2. **This file does NOT edit `R/`** — it specifies the loader contract,
URL/file scheme, framing, and migration table for the main thread to implement.

This is the concrete build-out of review-migration.md **S3** ("legacy reproducibility is NOT
guaranteed; tfhub.dev → Kaggle deprecation can make USE unloadable").

---

## RECOMMENDED PRIMARY SOURCE (the headline answer)

**Vendor the four SavedModels ourselves and serve them from a GitHub Release on
`BenWiseman/sentiment.ai`, as the *primary* source. Kaggle Models is the *fallback*, live
tfhub.dev is the last-resort fallback.**

Rationale, in order of weight:

1. **Only a source we control survives the deprecation.** `tfhub.dev` was deprecated and
   redirected to Kaggle Models on **2023-11-15**, and **unmigrated assets were deleted on
   2024-03-18** (Kaggle's own FAQ). The package currently hardcodes
   `https://tfhub.dev/google/universal-sentence-encoder-large/5` in `choose_model.R:25` and
   `get_embedder_v2.py:105`. The day Google stops honouring those redirects, `legacy = TRUE`
   breaks with no recourse. A GitHub Release we own cannot be deprecated out from under us.
2. **Kaggle is a real source but not a *stable contract*.** Kaggle re-homed all four models
   under one slug (`google/universal-sentence-encoder`) with `framework/variation/version`
   triples whose **version numbers do not match the tfhub version numbers** (see mapping
   table below — tfhub `large/5` is Kaggle `tensorFlow2/large/2`). Kaggle can re-version,
   gate behind auth/consent, or rate-limit. Good as a fallback and as our *re-vendoring
   upstream*, bad as the thing a user's `library(sentiment.ai)` depends on at runtime.
3. **The assets are MIT/Apache-2.0 and small enough to release.** USE-large/5 and the two
   multilingual SavedModels are on the order of ~250 MB–1 GB uncompressed each; well within a
   GitHub Release's 2 GB-per-file limit. We are already a GitHub-hosted package
   (`URL: https://github.com/BenWiseman/sentiment.ai`), so a Release asset is zero new infra.

If, after a license re-check (step 0 below), redistribution is *not* clearly permitted, fall
back to recommendation 1b: **Kaggle as primary via `kagglehub`**, GitHub Release omitted, and
state honestly in NEWS that legacy depends on a third party we don't control. Recommendation 1
(self-vendor) is strongly preferred and is the default of this plan.

---

## 0. Pre-work: confirm we may redistribute

Before mirroring anything, verify each SavedModel's license permits redistribution. The USE
family is published by Google under **Apache-2.0** on both tfhub.dev and Kaggle, which permits
redistribution with attribution + license text. **Do NOT assume — capture the license file per
model at vendoring time** and ship it in the release (`LICENSE-USE-large.txt`, etc.). This is a
grounded-claims requirement, not a formality: if a variation is ever found to be CC-BY-NC or
"non-commercial research", it must drop to Kaggle-fallback-only and be flagged in docs. Record
the resolved license per model in `inst/legacy/MANIFEST.csv` (below).

---

## 1. The four legacy models — canonical identity table

The single source of truth the loader and docs share. **Note the version-number trap:** the
package's internal id (from `constants.R::legacy_models`) carries the *tfhub* version; Kaggle
renumbered. Getting this wrong silently loads a *different* SavedModel and breaks reproducibility
with no error.

| user handle  | pkg internal id (tfhub)                          | tfhub URL (legacy/dead)                                                  | Kaggle variation / ver        | dim | needs `tensorflow_text` |
|--------------|--------------------------------------------------|-------------------------------------------------------------------------|-------------------------------|-----|-------------------------|
| `en`         | `universal-sentence-encoder/4`                   | `tfhub.dev/google/universal-sentence-encoder/4`                          | `universal-sentence-encoder/2`| 512 | no                      |
| `en.large`   | `universal-sentence-encoder-large/5`             | `tfhub.dev/google/universal-sentence-encoder-large/5`                    | `large/2`                     | 512 | no                      |
| `multi`      | `universal-sentence-encoder-multilingual/3`      | `tfhub.dev/google/universal-sentence-encoder-multilingual/3`             | `multilingual/2`              | 512 | **yes**                 |
| `multi.large`| `universal-sentence-encoder-multilingual-large/3`| `tfhub.dev/google/universal-sentence-encoder-multilingual-large/3`       | `multilingual-large/2`        | 512 | **yes**                 |

Kaggle framework prefix for all four: `tensorFlow2`. Full Kaggle handle pattern:
`google/universal-sentence-encoder/tensorFlow2/<variation>/<version>`. Kaggle download API:
`https://www.kaggle.com/api/v1/models/google/universal-sentence-encoder/tensorFlow2/<variation>/<version>/download`.

> **Verification flag (do at vendoring time, do not trust this table blind):** the Kaggle
> version integers above are taken from the redirect URLs surfaced by search
> (`.../tensorFlow2/multilingual-large/2`, `.../tensorFlow2/large/2`) and may have advanced.
> Resolve each live with `kagglehub` / the Kaggle API and **record the SHA-256 of the
> downloaded SavedModel** in the manifest so a future Kaggle re-version can't silently change
> the bytes we vendored. Reproducibility = pin the hash, not the version string.

---

## 2. File + URL scheme

### 2.1 GitHub Release (PRIMARY mirror)

One release dedicated to frozen legacy assets, decoupled from the package's code releases so we
never re-upload ~2 GB on every patch:

- **Release tag:** `legacy-use-v1` on `BenWiseman/sentiment.ai`
  (title: "Frozen legacy USE SavedModels (2021 stack)").
- **Asset naming** (one gzipped tarball per model, plus per-model license + a manifest):

  ```
  use-en-4.tar.gz                 # universal-sentence-encoder/4
  use-en-large-5.tar.gz           # universal-sentence-encoder-large/5
  use-multi-3.tar.gz              # universal-sentence-encoder-multilingual/3
  use-multi-large-3.tar.gz        # universal-sentence-encoder-multilingual-large/3
  LICENSE-USE-en.txt  ...         # Apache-2.0 text per model
  MANIFEST.csv                    # handle, tfhub_id, kaggle_handle, sha256, bytes, license
  ```

  Asset names carry the **tfhub** version (the package's internal id), so the loader maps
  `legacy_models[handle]` → asset name by a pure string transform (`/` → `-`, prefix `use-`),
  no second lookup table to drift.

- **Resolved URL pattern (stable):**

  ```
  https://github.com/BenWiseman/sentiment.ai/releases/download/legacy-use-v1/<asset>.tar.gz
  ```

  GitHub release-download URLs are permanent for a published release; this is the string the
  loader hits first.

### 2.2 Manifest shipped *inside* the package

`inst/legacy/MANIFEST.csv` (tiny, lives in the installed package — NOT the multi-GB models):

```
handle,tfhub_id,kaggle_handle,asset,sha256,bytes,dim,needs_tf_text,license
en,universal-sentence-encoder/4,google/universal-sentence-encoder/tensorFlow2/universal-sentence-encoder/2,use-en-4.tar.gz,<sha>,...,512,FALSE,Apache-2.0
en.large,universal-sentence-encoder-large/5,google/universal-sentence-encoder/tensorFlow2/large/2,use-en-large-5.tar.gz,<sha>,...,512,FALSE,Apache-2.0
multi,universal-sentence-encoder-multilingual/3,google/universal-sentence-encoder/tensorFlow2/multilingual/2,use-multi-3.tar.gz,<sha>,...,512,TRUE,Apache-2.0
multi.large,universal-sentence-encoder-multilingual-large/3,google/universal-sentence-encoder/tensorFlow2/multilingual-large/2,use-multi-large-3.tar.gz,<sha>,...,512,TRUE,Apache-2.0
```

The loader reads this CSV to get the expected SHA-256 (integrity), the Kaggle handle (fallback),
and the `needs_tf_text` flag (so it can fail *early and clearly* on Apple-silicon-no-tf-text
rather than mid-load). This keeps every fragile string in **one data file**, not scattered
across R + Python.

### 2.3 Runtime cache location (CRAN-clean)

Per review-migration **S4**: do **not** write into `system.file(package=...)` (the current
`cache_dir <- file.path(pkg_path, "tfhub_modules")` at `init_and_install.R:479` violates CRAN
policy). Extract the SavedModel to:

```
tools::R_user_dir("sentiment.ai", which = "cache") / legacy / <handle> / <kaggle_or_tfhub_ver> /
```

`TFHUB_CACHE_DIR` (env var the TF-Hub loader honours, already set in `get_embedder_v2.py:93`) is
pointed at this dir so a tfhub-fallback load lands in the same cache. The cache key includes the
version so a re-vendor doesn't collide with an old extraction.

---

## 3. Loader fallback order — `load_hub_embedding` changes (described, not coded)

Today the path is: `choose_model()` returns a hardcoded `tfhub.dev` URL →
`get_embedder_v2.py::initiate_model` calls `hub.load("https://tfhub.dev/google/<id>")` (single
source, no fallback, `get_embedder_v2.py:105`). Change it to a **resolve-then-load** with a
three-tier waterfall. The resolution should happen in R (it owns the manifest + cache paths +
`download.file`); Python only ever receives a **local directory path** to `hub.load()`.

### 3.1 New resolution function (R side, conceptual: `resolve_legacy_use(handle)`)

Returns a local SavedModel directory, trying sources in this order. Each tier: download →
verify SHA-256 against manifest → extract → return path. On failure, log a one-line reason and
fall through. Only after all three fail does it raise.

1. **Cache hit (no network).** If `R_user_dir/.../legacy/<handle>/<ver>/saved_model.pb` exists
   AND its parent's recorded hash matches the manifest, return it. (Re-verify cheaply via a
   stored `.sha256` sidecar, not by re-hashing a 1 GB tree every call.)

2. **Tier 1 — GitHub Release (PRIMARY).**
   `download.file("https://github.com/BenWiseman/sentiment.ai/releases/download/legacy-use-v1/<asset>")`
   → check SHA-256 == manifest → `untar()` into the cache dir. This is the path we control and
   the one that gives byte-identical reproducibility.

3. **Tier 2 — Kaggle Models (FALLBACK).** Resolve via the Kaggle handle from the manifest.
   Prefer `kagglehub::model_download("google/universal-sentence-encoder/tensorFlow2/<variation>/<version>")`
   if `kagglehub` (the reticulate-available Python lib) is importable; else hit the public
   download API
   `https://www.kaggle.com/api/v1/models/.../tensorFlow2/<variation>/<version>/download`.
   **Do NOT verify against our SHA here** (Kaggle's packaging/compression may differ byte-wise
   from our tarball); instead verify *load succeeds* and *output dim == 512* as the integrity
   check, and emit a one-time message that this is the upstream-fallback path, numbers should
   match but were not byte-pinned.

4. **Tier 3 — live tfhub.dev (LAST RESORT, may already be dead).**
   `hub.load("https://tfhub.dev/google/<tfhub_id>")` exactly as today. Wrapped so its failure is
   the trigger to raise the final directed error, not an uncaught reticulate stack trace.

5. **All failed → directed error**, e.g.:
   `"Could not obtain legacy model 'en.large' from the bundled mirror, Kaggle, or tfhub.dev.
   tfhub.dev is deprecated (assets deleted 2024-03-18). Check your network, or pin
   sentiment.ai's GitHub Release 'legacy-use-v1'. See ?legacy_models."`

### 3.2 What changes in `load_hub_embedding` / `initiate_model`

- **Signature:** accept a **local path** (the resolved SavedModel dir), not a URL. The URL/Kaggle
  waterfall moves *up and out* into the R `resolve_legacy_use()`; the Python loader becomes a
  dumb `hub.load(local_dir)`. This kills the "no fallback" single point of failure in
  `get_embedder_v2.py:105` and keeps network policy in R where CRAN expects it.
- **`tensorflow_text` guard:** before loading `multi` / `multi.large`, check `needs_tf_text` from
  the manifest; if tf-text isn't importable (the documented Apple-silicon gap that
  `get_embedder_v2.py` currently swallows), raise a *directed* error naming the constraint
  instead of a deep TF crash. `en` / `en.large` do **not** need tf-text and must still work on
  Apple silicon.
- **Cache dir:** replace `file.path(pkg_path, "tfhub_modules")` with the `R_user_dir` cache from
  §2.3 (fixes S4 CRAN write-into-package-dir for the legacy path too).
- **Dim assertion:** after first embed, assert output width == 512 (manifest `dim`); a wrong-tier
  / wrong-version load that returns a different width fails loudly. Ties into S5/S6's "stop
  assuming 512" — here 512 is *correct and asserted* specifically for legacy USE.

### 3.3 Vendoring script (one-time, lives in `data-raw/`, not shipped)

A `data-raw/vendor_legacy_use.R` (+ small py helper) that: pulls each of the four from Kaggle via
`kagglehub`, tars them, computes SHA-256, writes `MANIFEST.csv`, and `gh release upload`s to
`legacy-use-v1`. Run once by a maintainer; never at user runtime. This is how the GitHub Release
gets populated and re-populated if Kaggle ever changes upstream.

---

## 4. Honest framing — "frozen-2021 best-effort"

Per the brand rule (grounded, not over-claimed) and review-migration S3, the docs/NEWS must say
**exactly** what is and isn't guaranteed. Proposed copy for `NEWS.md` + a `?legacy_models` help
note + the `install_sentiment.ai(legacy = TRUE)` startup message:

> **Legacy Universal Sentence Encoder models (`en`, `en.large`, `multi`, `multi.large`) are a
> frozen, best-effort compatibility shim.** They reproduce sentiment.ai ≤0.2 results and run on
> the original 2021 TensorFlow stack (TF 2.4.x / tensorflow-hub 0.12 / tensorflow-text 2.4.x,
> Python 3.8). We mirror the SavedModels from our own GitHub Release so they keep loading after
> Google's deprecation of tfhub.dev (assets there were deleted 2024-03-18). We do **not** promise
> these install on every future OS/Python — notably `tensorflow-text` has no Apple-silicon wheel,
> so `multi` / `multi.large` may be unavailable there. Reproducibility is guaranteed **only**
> against the documented pinned environment and the byte-pinned SavedModels in our release. For
> new work, use the default `e5-small` or `e5-base` (no TensorFlow, multilingual, actively
> maintained). See the migration table below to recalibrate existing thresholds.

Three honesty guardrails baked into the framing:

- **"Best-effort," not "supported."** No claim that legacy will work forever or on new hardware.
- **Name the real failure modes** (Apple-silicon tf-text gap; future Python/OS wheel rot) rather
  than discovering them as user bug reports.
- **Pin the bytes, not just the version string.** "Reproducible against the SHA-256 in our
  manifest" is a claim we can actually keep; "reproducible against tfhub.dev" is not.

---

## 5. USE → e5 score-delta migration table plan

Legacy exists so old scripts don't silently change numbers. But the *right* migration is to move
users **off** legacy onto e5. To do that without them flying blind, ship a measured delta table
so they can recalibrate thresholds (e.g. someone who treated `score > 0.6` as "positive" under
`en.large` needs to know what that maps to under `e5-small`).

### 5.1 What to compute (grounded, on real test data — do NOT fabricate)

Reuse the bake-off corpus already on disk
(`sentiment.ai_training/.../training_data/all_data.csv` + the 3,255-row real test split used in
review-lineup.md). For each of the four legacy USE models **and** `e5-small` / `e5-base`:

- macro-F1 and per-class F1 (neg/neu/pos) — these already partly exist
  (`review-lineup.md`: `use.large` = **0.790**, `e5-small` = **0.813**, `e5-base` = **0.860**).
- the **score distribution shift**: on the *same sentences*, the mean/median continuous
  `sentiment_score()` and the correlation (Pearson + Spearman) between USE-model score and
  e5-model score. Spearman matters most: if ranking is preserved, users mostly need to move a
  threshold, not rethink the model.
- a **threshold cross-walk**: the e5 score at which precision/recall on the positive (and
  negative) class matches what the user got at their old USE threshold. This is the actionable
  column — "your old `en.large` cutoff of 0.5 ≈ `e5-small` cutoff of 0.46".

### 5.2 Table shape (one row per legacy→e5 pair)

| from (legacy) | to (default) | macro-F1 from → to | Spearman ρ (same texts) | mean score Δ | old thr → new thr (pos) | notes |
|---------------|--------------|--------------------|--------------------------|--------------|--------------------------|-------|
| `en.large`    | `e5-small`   | 0.790 → 0.813      | _measure_                | _measure_    | _measure_                | default upgrade |
| `en.large`    | `e5-base`    | 0.790 → 0.860      | _measure_                | _measure_    | _measure_                | best on-device  |
| `en`          | `e5-small`   | _measure_ → 0.813  | _measure_                | _measure_    | _measure_                | smallest→smallest |
| `multi`       | `e5-small`   | _measure_ → 0.813  | _measure_                | _measure_    | _measure_                | multilingual    |
| `multi.large` | `e5-base`    | _measure_ → 0.860  | _measure_                | _measure_    | _measure_                | multilingual best |

(`_measure_` = compute from the bake-off corpus; leave blank rather than invent. The two F1
anchors shown are the only numbers already grounded in review-lineup.md.)

### 5.3 Where it lives / how it ships

- Computed by a script in `data-raw/` (`build_migration_table.R`) from the bake-off CSVs,
  emitting `inst/legacy/use_e5_migration.csv`.
- Surfaced three ways: (a) a row in the docs benchmark page
  (docs-benchmark-page-spec.md already exists in planning/), (b) a `?migrate_from_legacy` help
  topic, (c) optionally a tiny exported helper `legacy_score_map(from, to)` that returns the
  cross-walk row so users can adjust thresholds programmatically.
- **Honesty note in the table caption:** deltas are corpus-specific (the bake-off test set); a
  user's own domain may shift them. Frame as "recalibrate and re-validate on your data," not
  "subtract 0.04 and you're done."

---

## 6. Sequencing (smallest credible slices)

1. **Step 0** — license re-check per model; record in manifest. (Gate: if any is non-redist,
   that model drops to Kaggle-fallback-only.)
2. **Vendor** the four via `data-raw/vendor_legacy_use.R`; compute SHA-256; create GitHub Release
   `legacy-use-v1`; commit `inst/legacy/MANIFEST.csv`.
3. **Loader** — implement `resolve_legacy_use()` (R) + simplify Python loader to take a local
   path; wire the three-tier waterfall + tf-text guard + dim assertion; move cache to
   `R_user_dir`.
4. **Framing** — land the NEWS / `?legacy_models` / startup-message copy from §4.
5. **Migration table** — `build_migration_table.R` → `inst/legacy/use_e5_migration.csv` → docs +
   `?migrate_from_legacy`.
6. **Test gate** — with the GitHub Release live: `init_sentiment.ai("en.large")` loads from the
   mirror (network mock or a small smoke test), returns a 512-d embedding, and a known sentence's
   score matches the v0.2 baseline within tolerance. Kaggle/tfhub tiers covered by unit tests that
   stub the resolver. No green CI until the **mirror** tier (not tfhub) is the one exercised.

---

## Cross-references

- review-migration.md **S3** (the finding this plan closes), **S4** (R_user_dir cache, CRAN),
  **S5/S6** (512 / dim assumptions — here 512 is asserted, correctly, for legacy only).
- `constants.R::legacy_models` (the internal ids this plan maps to Kaggle/asset names).
- `choose_model.R:24–29` and `get_embedder_v2.py:105` (the hardcoded `tfhub.dev` strings this
  plan replaces with manifest-driven resolution).
- review-lineup.md (the F1 anchors 0.790 / 0.813 / 0.860 used in the migration table).

---

## Sources

- [TensorFlow Hub Moving to Kaggle Models — FAQs (deprecation 2023-11-15, assets deleted 2024-03-18)](https://www.kaggle.com/tfhub-dev-faqs)
- [Google | universal-sentence-encoder | Kaggle (canonical model home)](https://www.kaggle.com/models/google/universal-sentence-encoder)
- [USE tensorFlow2 / large / 2 (Kaggle redirect URL)](https://www.kaggle.com/models/google/universal-sentence-encoder/frameworks/tensorFlow2/variations/large/versions/2?tfhub-redirect=true)
- [USE tensorFlow2 / multilingual-large / 2 (Kaggle)](https://www.kaggle.com/models/google/universal-sentence-encoder/tensorFlow2/multilingual-large/2?tfhub-redirect=true)
- [USE tensorFlow2 / multilingual / 2 (Kaggle)](https://www.kaggle.com/models/google/universal-sentence-encoder/tensorFlow2/multilingual/2?tfhub-redirect=true)
- [Universal Sentence Encoder | TensorFlow Hub (model card / 512-d / DAN)](https://www.tensorflow.org/hub/tutorials/semantic_similarity_with_tf_hub_universal_encoder)
