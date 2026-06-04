# Model provenance metadata: license, source URL, pinned revision.
#
# Internal registry that travels with the package so it can (a) carry the
# THIRD-PARTY MODEL NOTICE data structurally, (b) report license/source to the
# user, and (c) pin exactly which weights get downloaded. None of these models
# are bundled -- sentence-transformers / huggingface_hub fetch them on first use
# -- so the obligation is attribution + notice, not redistribution. Pinning a
# revision is still required for reproducibility and to make "what you got"
# auditable.
#
# Keyed by the user-facing handle, to match `default_models`, `openai_models`,
# `legacy_models`, `model_dims`, and `model_prefix` in R/constants.R. Verified
# 2026-06-03 from each model's HuggingFace card YAML `license:` field (and README
# body where present), cross-checked against the HF `/api/models/<id>` endpoint.
# See planning/licenses-and-notice.md for the full evidence table.
# (internal; not exported)

# License SPDX identifier (or governing terms) per model. Drives NOTICE
# generation and a user-facing license accessor.
#   - e5 family: MIT (intfloat).
#   - openai family: API-only -- governed by the OpenAI API Terms of Service,
#     not a model-weights license (weights are never downloaded).
#   - legacy USE family: Apache-2.0, distributed via TF Hub (opt-in, TensorFlow).
model_license <- c(
  `e5-small`               = "MIT",
  `e5-base`                = "MIT",
  `text-embedding-3-small` = "OpenAI API ToS",
  `text-embedding-3-large` = "OpenAI API ToS",
  `text-embedding-ada-002` = "OpenAI API ToS",
  en.large                 = "Apache-2.0",
  en                       = "Apache-2.0",
  multi.large              = "Apache-2.0",
  multi                    = "Apache-2.0"
)

# Canonical source URL per model (used in NOTICE + error/help messages).
#   - e5 family: HuggingFace model card.
#   - openai family: OpenAI embeddings docs (no model-card; API-only).
#   - legacy USE family: TF Hub today; tfhub.dev is deprecating to Kaggle Models,
#     so these source URLs should migrate to the Kaggle listing at release.
model_source_url <- c(
  `e5-small`               = "https://huggingface.co/intfloat/multilingual-e5-small",
  `e5-base`                = "https://huggingface.co/intfloat/multilingual-e5-base",
  `text-embedding-3-small` = "https://platform.openai.com/docs/guides/embeddings",
  `text-embedding-3-large` = "https://platform.openai.com/docs/guides/embeddings",
  `text-embedding-ada-002` = "https://platform.openai.com/docs/guides/embeddings",
  en.large                 = "https://tfhub.dev/google/universal-sentence-encoder-large/5",
  en                       = "https://tfhub.dev/google/universal-sentence-encoder/4",
  multi.large              = "https://tfhub.dev/google/universal-sentence-encoder-multilingual-large/3",
  multi                    = "https://tfhub.dev/google/universal-sentence-encoder-multilingual/3"
)

# Pinned HuggingFace revision (git commit SHA) per model. PIN THESE before
# release: resolve each model's current `main` to its immutable commit SHA and
# freeze it here (HfApi().model_info(repo_id).sha, or GET
# https://huggingface.co/api/models/<id> -> `sha`). The ST loader forwards
# `revision=` to snapshot_download, so the pin only binds the on-device e5 path;
# OpenAI is an API (no downloadable artifact -> NA) and the legacy USE models are
# fetched via TF Hub, not HF revisions (-> NA).
# Until pinned, e5 rows default to "main" -- a moving target -- and the loader
# should warn once that an unpinned revision is in use.
model_revision <- c(
  `e5-small`               = "main",  # TODO: pin to commit SHA at release
  `e5-base`                = "main",  # TODO: pin to commit SHA at release
  `text-embedding-3-small` = NA,      # OpenAI API -- not a downloadable artifact
  `text-embedding-3-large` = NA,      # OpenAI API -- not a downloadable artifact
  `text-embedding-ada-002` = NA,      # OpenAI API -- not a downloadable artifact
  en.large                 = NA,      # TF Hub artifact -- not an HF revision
  en                       = NA,      # TF Hub artifact -- not an HF revision
  multi.large              = NA,      # TF Hub artifact -- not an HF revision
  multi                    = NA       # TF Hub artifact -- not an HF revision
)

# ---------------------------------------------------------------------------
# Bakeoff candidates (NOT in the LOCKED v2 lineup; no constants.R handle yet).
# Verified license/source data is parked here so a bakeoff winner can be promoted
# into the registry without a fresh legal pass -- add its row above, plus to
# default_models / model_dims / model_prefix in constants.R, and pin its SHA.
# See planning/licenses-and-notice.md §1-§3 for the evidence.
#
# bge-base          MIT          https://huggingface.co/BAAI/bge-base-en-v1.5
# bge-small         MIT          https://huggingface.co/BAAI/bge-small-en-v1.5
# gte-base          MIT          https://huggingface.co/thenlper/gte-base   (re-confirm license at pinned SHA on promotion)
# mpnet-base        Apache-2.0   https://huggingface.co/sentence-transformers/all-mpnet-base-v2
# paraphrase-minilm Apache-2.0   https://huggingface.co/sentence-transformers/paraphrase-MiniLM-L6-v2
# ---------------------------------------------------------------------------
