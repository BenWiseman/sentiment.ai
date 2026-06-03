# Model licenses, attribution & provenance (sentiment.ai v2)

Status: verification pass, 2026-06-03. Licenses read directly from each model's
HuggingFace model card (YAML `license:` field) and, where present, the README
body. Source of truth is the model card metadata, cross-checked against the HF
`/api/models/<id>` endpoint for the two that needed disambiguation.

These are **shipped-by-reference** models: the package does **not** bundle
weights. Each is fetched from HuggingFace on first use (sentence-transformers /
`snapshot_download`). Licensing obligations are therefore about **attribution
and notice in the package**, not redistribution of weights — but pinning a
revision is still required for reproducibility and to make the "what you got"
auditable.

---

## 1. One-line license summary (per model)

- `intfloat/multilingual-e5-small` — **MIT**. Permissive; commercial use OK; attribution only.
- `intfloat/multilingual-e5-base` — **MIT**. Permissive; commercial use OK; attribution only.
- `BAAI/bge-base-en-v1.5` — **MIT**. README explicitly: "can be used for commercial purposes free of charge."
- `BAAI/bge-small-en-v1.5` — **MIT**. README explicitly: "can be used for commercial purposes free of charge."
- `thenlper/gte-base` — **MIT** (per current card YAML + HF API). See caveat in §4.
- `sentence-transformers/all-mpnet-base-v2` — **Apache-2.0**. Permissive; NOTICE/attribution obligations apply.
- `sentence-transformers/paraphrase-MiniLM-L6-v2` — **Apache-2.0**. Permissive; NOTICE/attribution obligations apply.

All seven are OSI-permissive (MIT or Apache-2.0). None impose use-based
restrictions (no RAIL/OpenRAIL, no non-commercial clause, no model-derived-output
restriction). This is compatible with sentiment.ai's own MIT license and with
commercial downstream use.

### Verification evidence

| Model | Card YAML `license:` | README body statement | Confirmed via |
|---|---|---|---|
| intfloat/multilingual-e5-small | `mit` | (none in body) | model card + `raw/main/README.md` frontmatter |
| intfloat/multilingual-e5-base  | `mit` | (none in body) | model card + `/api/models` tags (`license:mit`) |
| BAAI/bge-base-en-v1.5          | `mit` | "FlagEmbedding is licensed under the MIT License. The released models can be used for commercial purposes free of charge." | model card |
| BAAI/bge-small-en-v1.5         | `mit` | same FlagEmbedding MIT statement | model card |
| thenlper/gte-base              | `mit` | (none in body) | model card YAML + `/api/models` tags (`license:mit`) |
| sentence-transformers/all-mpnet-base-v2 | `apache-2.0` | (none required) | model card |
| sentence-transformers/paraphrase-MiniLM-L6-v2 | `apache-2.0` | (none required) | model card |

Note on scope: of these seven, only `e5-small` and `e5-base` are in the current
LOCKED v2 lineup (`default_models` in `R/constants.R`). The four BGE/GTE/MPNet/
MiniLM entries are **bakeoff candidates** evaluated for the lineup; documenting
their licenses now means a candidate can be promoted into the registry without a
fresh legal pass. `openai/text-embedding-3-*` is API-only (governed by the OpenAI
API terms, not a model-weights license) and the legacy USE models are Apache-2.0
via TF Hub / Kaggle — both are out of scope for this NOTICE block but flagged in
§5 so nothing is silently uncovered.

---

## 2. NOTICE / attribution block

Drop the following into the package as `inst/NOTICE.md` (or append to a
`THIRD-PARTY-NOTICES` section). MIT requires the copyright/permission notice to
travel with the work; Apache-2.0 requires preserving attribution notices. Since
weights are fetched at runtime (not redistributed), the obligation is satisfied
by carrying these notices in-package and surfacing the source/license to the
user. Keep this in sync with the registry in §3.

```
THIRD-PARTY MODEL NOTICES
=========================

sentiment.ai fetches embedding models from HuggingFace on first use. It does not
bundle or redistribute model weights. Each model below is the property of its
authors and is provided under the stated license. By downloading a model you
also agree to that model's license.

----------------------------------------------------------------------
Model:    intfloat/multilingual-e5-small  (handle: e5-small)  [v2 DEFAULT]
License:  MIT
Source:   https://huggingface.co/intfloat/multilingual-e5-small
Cite:     Wang et al., "Multilingual E5 Text Embeddings: A Technical Report",
          arXiv:2402.05672.
----------------------------------------------------------------------
Model:    intfloat/multilingual-e5-base   (handle: e5-base)
License:  MIT
Source:   https://huggingface.co/intfloat/multilingual-e5-base
Cite:     Wang et al., "Multilingual E5 Text Embeddings: A Technical Report",
          arXiv:2402.05672.
----------------------------------------------------------------------
Model:    BAAI/bge-base-en-v1.5           (bakeoff candidate)
License:  MIT  (FlagEmbedding; commercial use permitted free of charge)
Source:   https://huggingface.co/BAAI/bge-base-en-v1.5
Cite:     Xiao et al., "C-Pack: Packed Resources For General Chinese Embeddings",
          arXiv:2309.07597.
----------------------------------------------------------------------
Model:    BAAI/bge-small-en-v1.5          (bakeoff candidate)
License:  MIT  (FlagEmbedding; commercial use permitted free of charge)
Source:   https://huggingface.co/BAAI/bge-small-en-v1.5
Cite:     Xiao et al., "C-Pack: Packed Resources For General Chinese Embeddings",
          arXiv:2309.07597.
----------------------------------------------------------------------
Model:    thenlper/gte-base               (bakeoff candidate)
License:  MIT  (per model-card metadata; see provenance note)
Source:   https://huggingface.co/thenlper/gte-base
Cite:     Li et al., "Towards General Text Embeddings with Multi-stage
          Contrastive Learning", arXiv:2308.03281.
----------------------------------------------------------------------
Model:    sentence-transformers/all-mpnet-base-v2   (bakeoff candidate)
License:  Apache-2.0
Source:   https://huggingface.co/sentence-transformers/all-mpnet-base-v2
Cite:     Reimers & Gurevych, "Sentence-BERT", EMNLP 2019; sentence-transformers
          library. Base model: microsoft/mpnet-base (MIT).
----------------------------------------------------------------------
Model:    sentence-transformers/paraphrase-MiniLM-L6-v2  (bakeoff candidate)
License:  Apache-2.0
Source:   https://huggingface.co/sentence-transformers/paraphrase-MiniLM-L6-v2
Cite:     Reimers & Gurevych, "Sentence-BERT", EMNLP 2019; sentence-transformers
          library.
----------------------------------------------------------------------

Full license texts:
  MIT          https://opensource.org/license/mit
  Apache-2.0   https://www.apache.org/licenses/LICENSE-2.0
```

This package (sentiment.ai) is MIT-licensed and attributed to the Korn Ferry
Institute. None of the model licenses above conflict with that.

---

## 3. Registry fields to add

Add three provenance fields per model so the package can (a) carry the NOTICE
data structurally, (b) report license/source to the user, and (c) pin exactly
what gets downloaded. Recommended R representation in `R/constants.R`, mirroring
the existing named-vector style (`model_dims`, `model_prefix`). Keyed by the
**user-facing handle** to match `default_models` / `model_dims`.

```r
# License SPDX identifier per model (drives NOTICE generation + a user-facing
# `model_license()` accessor). Keyed by user-facing handle.
model_license <- c(
  `e5-small`            = "MIT",
  `e5-base`             = "MIT",
  `bge-base`            = "MIT",          # BAAI/bge-base-en-v1.5
  `bge-small`           = "MIT",          # BAAI/bge-small-en-v1.5
  `gte-base`            = "MIT",          # thenlper/gte-base (see provenance note)
  `mpnet-base`          = "Apache-2.0",   # sentence-transformers/all-mpnet-base-v2
  `paraphrase-minilm`   = "Apache-2.0"    # sentence-transformers/paraphrase-MiniLM-L6-v2
)

# Canonical HuggingFace model-card URL (used in NOTICE + error/help messages).
model_source_url <- c(
  `e5-small`          = "https://huggingface.co/intfloat/multilingual-e5-small",
  `e5-base`           = "https://huggingface.co/intfloat/multilingual-e5-base",
  `bge-base`          = "https://huggingface.co/BAAI/bge-base-en-v1.5",
  `bge-small`         = "https://huggingface.co/BAAI/bge-small-en-v1.5",
  `gte-base`          = "https://huggingface.co/thenlper/gte-base",
  `mpnet-base`        = "https://huggingface.co/sentence-transformers/all-mpnet-base-v2",
  `paraphrase-minilm` = "https://huggingface.co/sentence-transformers/paraphrase-MiniLM-L6-v2"
)

# Pinned HuggingFace revision (git commit SHA) per model. PIN THESE before
# release -- see "Suggested pinned-revision approach" below. Until pinned,
# leave as "main" but emit a one-time message that an unpinned revision is in
# use (reproducibility caveat). A SHA makes the download auditable & immutable.
model_revision <- c(
  `e5-small`          = "main",   # TODO: replace with commit SHA at release
  `e5-base`           = "main",   # TODO
  `bge-base`          = "main",   # TODO
  `bge-small`         = "main",   # TODO
  `gte-base`          = "main",   # TODO
  `mpnet-base`        = "main",   # TODO
  `paraphrase-minilm` = "main"    # TODO
)
```

Notes:
- Only `e5-small` / `e5-base` are wired into the LOCKED lineup today. The other
  five rows are pre-staged so a bakeoff winner can be promoted by adding it to
  `default_models` (+ `model_dims`, `model_prefix`) without re-deriving license
  data. If you would rather not ship dead rows, keep just the e5 entries live
  and park the rest behind a comment — the verified data stays here either way.
- Field names (`model_license`, `model_source_url`, `model_revision`) follow the
  existing `model_<attr>` convention in `constants.R`. The task asked for
  `revision`; `model_revision` is the consistent name in this file's style — use
  whichever the main thread prefers, just keep it one word if it becomes a
  registry column.
- The download path (`sentence_transformers.SentenceTransformer(...)` /
  `huggingface_hub.snapshot_download(...)`) should pass `revision =
  model_revision[[handle]]` so the pin is actually enforced, not just recorded.

---

## 4. Suggested pinned-revision approach

Goal: every download is reproducible and auditable — the user can prove which
bytes they got. "main" is a moving target; a model author can re-upload weights
under the same name.

1. **Pin to an immutable commit SHA, not a tag or `main`.** HF revisions can be a
   branch, tag, or full git commit SHA. Only the SHA is immutable. At release,
   resolve each model's current `main` to its commit SHA and freeze it in
   `model_revision`. Resolve with either:
   - `huggingface_hub.HfApi().model_info(repo_id).sha`  (Python, via reticulate), or
   - `GET https://huggingface.co/api/models/<id>` → `sha` field.
2. **Pass the revision through to the loader.** sentence-transformers forwards
   `revision=` to `snapshot_download`; OpenAI/legacy paths don't use this and
   should ignore the field. So the pin only binds the on-device (ST) backend,
   which is exactly the set of models in this NOTICE.
3. **Record the SHA in NOTICE.md too** (append `Revision: <sha>` under each block)
   so the notice and the registry agree and a reader can verify provenance
   offline.
4. **Until pinned, default to `"main"` but warn once.** On first download with an
   unpinned revision, emit a single informative message ("using unpinned
   revision 'main' for <model>; results may differ if the upstream weights
   change"). This keeps dev ergonomics while making the reproducibility gap
   explicit. Flip all rows to SHAs as a release-gate checklist item.
5. **Re-pinning is a deliberate, versioned act.** Bumping a SHA = a NEWS entry,
   because embeddings (and therefore scores/norms) can shift. Pre-launch this is
   cheap (no real norms shipped yet); post-launch it is a compatibility event.

Provenance caveat — `thenlper/gte-base`: the current model card declares
`license: mit` (confirmed in both the card YAML and the HF `/api/models`
metadata), and that is what we record. GTE is the one model in this set with a
less-institutional license trail than the e5 (intfloat), BGE/FlagEmbedding
(BAAI), and sentence-transformers families. Pinning a commit SHA also pins the
license-as-declared-at-that-revision, so the pin doubles as a license snapshot —
if GTE is promoted from candidate to lineup, re-confirm the card license at the
exact pinned SHA and capture it in the NEWS/notice at that time.

---

## 5. Out-of-scope models (flagged so nothing is silently uncovered)

- `text-embedding-3-small` / `-large` / `ada-002` (OpenAI): no model-weights
  license — governed by the **OpenAI API Terms of Service**, not redistributed.
  Registry should carry `model_license = "OpenAI API ToS"` and a `model_source_url`
  pointing at the OpenAI pricing/models docs, with `model_revision = NA` (API,
  not a downloadable artifact).
- Legacy Universal Sentence Encoder (`en`, `en.large`, `multi`, `multi.large`):
  **Apache-2.0**, distributed via TF Hub. tfhub.dev is deprecating to Kaggle
  Models, so the `model_source_url` for these should target the Kaggle Models
  listing at release, and they require TensorFlow (opt-in). Treat their license
  row as Apache-2.0 but track the source-URL migration separately.

These two groups are intentionally excluded from the §2 NOTICE block (one is an
API, the others are opt-in legacy) but are listed here so the registry can be
completed without re-research.
