## Submission: sentiment.ai 2.0.0

This is a feature release on top of the TensorFlow-free v2 backend. It adds:

* post-processing **hate / mixed / style** flag columns on `sentiment()` (small bundled
  per-encoder heads, applied to the same embedding),
* **intent-based profiles** (`use_profile()`, `sentiment_profiles()`) with a persisted
  default model,
* **opt-in end-to-end transformer backends** (`model = "twitter-roberta"` /
  `"xlm-roberta"`) via the existing reticulate bridge,
* an interactive **`plot_sentiment()`** sentiment map (suggests `plotly`; optional
  `uwot` / `Rtsne`, falling back to PCA), and
* updated **v2.0** bundled scoring heads (the 1.0 heads remain available).

The Python sibling on PyPI (`sentimentai`) ships the same scoring artifacts; the pure-R
forward pass is verified bit-for-bit against it.

## Test environments

* local: macOS (aarch64), R 4.4: `R CMD check` 0 errors | 0 warnings | 4 notes

(Maintainer note: please also run win-builder (devel + release) and a Linux check via
R-hub / GitHub Actions before submitting, as is standard.)

## R CMD check results

0 errors | 0 warnings | 4 notes. The notes are all expected:

* **installed size is ~32 Mb** (`inst/scoring` ~30 Mb). The package deliberately bundles
  its small JSON scoring heads and class centroids so that scoring runs entirely
  on-device (no model download and no network access at score time). This on-device data
  is the core value of the package; only the optional embedder is fetched on first use.

* **found 836 marked UTF-8 strings.** These are in the bundled `airline_tweets` example
  dataset (real tweets with accented characters and emoji). `Encoding: UTF-8` is
  declared in DESCRIPTION.

* **Suggests packages 'uwot' and 'Rtsne' not available for checking** (in a minimal
  environment). Both are on CRAN and are optional 2-D projection backends for
  `plot_sentiment()`; the function falls back to `stats::prcomp` (PCA) when they are
  absent, so they are genuinely optional.

* **`unlockBinding()` calls** in `R/onload.R` and `R/init_and_install.R`. These update two
  package-level, option-backed constants (the default model and the default scoring head)
  after the namespace is sealed, so that `options(sentiment.ai.model = )` /
  `options(sentiment.ai.scoring = )` set in a user's `.Rprofile` take effect on attach.
  This is a deliberate, documented pattern.

## Reverse dependencies

None.
