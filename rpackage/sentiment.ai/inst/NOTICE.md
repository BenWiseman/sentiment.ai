# THIRD-PARTY MODEL NOTICES

sentiment.ai fetches embedding models from HuggingFace on first use. It does
**not** bundle or redistribute model weights. Each model below is the property
of its authors and is provided under the stated license. By downloading a model
you also agree to that model's license.

All models listed are OSI-permissive (MIT or Apache-2.0). None impose use-based
restrictions (no RAIL/OpenRAIL, no non-commercial clause, no model-derived-output
restriction). This is compatible with sentiment.ai's own MIT license (attributed
to the Korn Ferry Institute) and with commercial downstream use.

Licenses were read from each model's HuggingFace model-card metadata (the YAML
`license:` field) and, where present, the README body, and cross-checked against
the HF `/api/models/<id>` endpoint. Verification pass: 2026-06-03.

`Revision:` is the pinned HuggingFace git revision the package downloads. Until a
release pins immutable commit SHAs, this is `main` (a moving target); the loader
warns once when an unpinned revision is in use. Pinning a SHA makes the download
reproducible and auditable, and snapshots the license-as-declared at that
revision.

---

## Shipped lineup (v2)

Only `e5-small` and `e5-base` are wired into the locked v2 registry
(`default_models` in `R/constants.R`).

```
----------------------------------------------------------------------
Model:    intfloat/multilingual-e5-small  (handle: e5-small)  [v2 DEFAULT]
License:  MIT
Source:   https://huggingface.co/intfloat/multilingual-e5-small
Revision: main   (pin a commit SHA at release)
Cite:     Wang et al., "Multilingual E5 Text Embeddings: A Technical Report",
          arXiv:2402.05672.
----------------------------------------------------------------------
Model:    intfloat/multilingual-e5-base   (handle: e5-base)
License:  MIT
Source:   https://huggingface.co/intfloat/multilingual-e5-base
Revision: main   (pin a commit SHA at release)
Cite:     Wang et al., "Multilingual E5 Text Embeddings: A Technical Report",
          arXiv:2402.05672.
----------------------------------------------------------------------
```

## Pre-staged bakeoff candidates (not in the shipped lineup)

Evaluated for the lineup; licenses documented now so a candidate can be promoted
into the registry without a fresh legal pass. Not downloaded unless explicitly
selected.

```
----------------------------------------------------------------------
Model:    BAAI/bge-base-en-v1.5           (bakeoff candidate)
License:  MIT  (FlagEmbedding; commercial use permitted free of charge)
Source:   https://huggingface.co/BAAI/bge-base-en-v1.5
Revision: main
Cite:     Xiao et al., "C-Pack: Packed Resources For General Chinese Embeddings",
          arXiv:2309.07597.
----------------------------------------------------------------------
Model:    BAAI/bge-small-en-v1.5          (bakeoff candidate)
License:  MIT  (FlagEmbedding; commercial use permitted free of charge)
Source:   https://huggingface.co/BAAI/bge-small-en-v1.5
Revision: main
Cite:     Xiao et al., "C-Pack: Packed Resources For General Chinese Embeddings",
          arXiv:2309.07597.
----------------------------------------------------------------------
Model:    thenlper/gte-base               (bakeoff candidate)
License:  MIT  (per model-card metadata; see provenance note below)
Source:   https://huggingface.co/thenlper/gte-base
Revision: main
Cite:     Li et al., "Towards General Text Embeddings with Multi-stage
          Contrastive Learning", arXiv:2308.03281.
----------------------------------------------------------------------
Model:    sentence-transformers/all-mpnet-base-v2   (bakeoff candidate)
License:  Apache-2.0
Source:   https://huggingface.co/sentence-transformers/all-mpnet-base-v2
Revision: main
Cite:     Reimers & Gurevych, "Sentence-BERT", EMNLP 2019; sentence-transformers
          library. Base model: microsoft/mpnet-base (MIT).
----------------------------------------------------------------------
Model:    sentence-transformers/paraphrase-MiniLM-L6-v2  (bakeoff candidate)
License:  Apache-2.0
Source:   https://huggingface.co/sentence-transformers/paraphrase-MiniLM-L6-v2
Revision: main
Cite:     Reimers & Gurevych, "Sentence-BERT", EMNLP 2019; sentence-transformers
          library.
----------------------------------------------------------------------
```

Full license texts:
  MIT          https://opensource.org/license/mit
  Apache-2.0   https://www.apache.org/licenses/LICENSE-2.0

---

## Provenance note — `thenlper/gte-base`

The current model card declares `license: mit` (confirmed in both the card YAML
and the HF `/api/models` metadata), and that is what is recorded here. GTE is the
one model in this set with a less-institutional license trail than the e5
(intfloat), BGE/FlagEmbedding (BAAI), and sentence-transformers families.
Pinning a commit SHA also pins the license-as-declared-at-that-revision. If GTE
is promoted from candidate to lineup, re-confirm the card license at the exact
pinned SHA and capture it in the NEWS/notice at that time.

## Out of scope for this notice

- **OpenAI embeddings** (`text-embedding-3-small` / `-large`, `ada-002`): no
  model-weights license — governed by the OpenAI API Terms of Service, accessed
  via API and not redistributed.
- **Legacy Universal Sentence Encoder** (`en`, `en.large`, `multi`,
  `multi.large`): Apache-2.0, distributed via TF Hub (migrating to Kaggle
  Models). Opt-in legacy path; requires TensorFlow.

These are intentionally excluded from the model-notice blocks above (one is an
API, the others are opt-in legacy) and are flagged here so nothing is silently
uncovered.
