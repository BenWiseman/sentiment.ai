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
  openai                   = "OpenAI API ToS",   # shorthand alias -> text-embedding-3-small
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
  openai                   = "https://platform.openai.com/docs/guides/embeddings",
  `text-embedding-3-small` = "https://platform.openai.com/docs/guides/embeddings",
  `text-embedding-3-large` = "https://platform.openai.com/docs/guides/embeddings",
  `text-embedding-ada-002` = "https://platform.openai.com/docs/guides/embeddings",
  en.large                 = "https://tfhub.dev/google/universal-sentence-encoder-large/5",
  en                       = "https://tfhub.dev/google/universal-sentence-encoder/4",
  multi.large              = "https://tfhub.dev/google/universal-sentence-encoder-multilingual-large/3",
  multi                    = "https://tfhub.dev/google/universal-sentence-encoder-multilingual/3"
)

# Pinned HuggingFace revision (immutable git commit SHA) per model. The e5 rows
# are frozen to the exact commit a user downloads, so "what you got" is auditable
# and reproducible: load_st_embedder() forwards `revision=` to sentence-transformers
# (-> huggingface_hub), which resolves to this commit instead of a moving `main`.
# Re-pin (and re-run the parity fixtures) only on a deliberate model upgrade.
# OpenAI is an API (no downloadable artifact -> NA); legacy USE models are fetched
# via TF Hub, not HF revisions (-> NA). SHAs resolved from
# https://huggingface.co/api/models/<id> on 2026-06-06.
model_revision <- c(
  `e5-small`               = "614241f622f53c4eeff9890bdc4f31cfecc418b3",
  `e5-base`                = "d128750597153bb5987e10b1c3493a34e5a4502a",
  openai                   = NA,      # OpenAI API shorthand alias
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
