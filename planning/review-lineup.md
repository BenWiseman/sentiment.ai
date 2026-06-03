# sentiment.ai v2 — Adversarial review of the MODEL LINEUP

> Red-team of the LOCKED lineup (e5-small default · e5-base · openai · legacy USE opt-in).
> Scope: the *lineup decision*, not the whole package. Every accuracy number below is the
> **provisional subsample** macro-F1 (18,000 train / 3,255 real test split, single-threaded
> xgboost, `num_parallel_tree=1`); the full-data `num_parallel_tree=24` run is in progress
> (`bakeoff/full_train.py`, e5 embeddings finished, oai/use_lg full-subset extraction came up
> empty — `wrote ... 0 rows` in `full_embed.log` — and must be re-run before any headline).
> Written 2026-06-03. Author: Ben Wiseman.

Provisional macro-F1, from `bakeoff/sub_f1_*.csv` (mean of the three per-class F1s):

| model | id | macro-F1 | neg / neu / pos | dim | on-device | no-TF |
|---|---|---:|---|---:|:---:|:---:|
| openai 3-small | `text-embedding-3-small` | **0.861** | 0.914 / 0.834 / 0.834 | 1536 | no (API) | yes |
| e5-base | `intfloat/multilingual-e5-base` | **0.860** | 0.915 / 0.833 / 0.831 | 768 | yes | yes |
| bge-base | `BAAI/bge-base-en-v1.5` | 0.836 | 0.898 / 0.809 / 0.801 | 768 | yes | yes |
| gte-base | `thenlper/gte-base` | 0.825 | 0.892 / 0.791 / 0.791 | 768 | yes | yes |
| bge-small | `BAAI/bge-small-en-v1.5` | 0.824 | 0.885 / 0.794 / 0.793 | 384 | yes | yes |
| **e5-small (default)** | `intfloat/multilingual-e5-small` | **0.813** | 0.861 / 0.800 / 0.779 | 384 | yes | yes |
| use.large (old TF default) | USE-large/5 | 0.790 | 0.845 / 0.782 / 0.744 | 512 | yes | no |
| mpnet | `all-mpnet-base-v2` | 0.781 | 0.838 / 0.763 / 0.742 | 768 | yes | yes |
| paraphrase | `paraphrase-MiniLM-L6-v2` | 0.751 | 0.786 / 0.759 / 0.708 | 384 | yes | yes |

The lineup's *thesis* is sound: e5-base genuinely ties OpenAI on-device with no TF, and the
whole no-TF family clears the old USE-large bar. The problems below are not "the models are
wrong" — they are **the prefix path is broken in shipping code**, **the default is the weakest
no-TF option in the table**, and **a set of claims in NEWS/DESCRIPTION that the current tree
does not yet support**. Ranked by severity.

---

## SEV-1 (BLOCKER) — The `"query: "` prefix is dead config; the shipping embedder can't even load e5

This is the footgun, and it is worse than "users might forget the prefix." **The package
itself forgets it**, in two compounding ways:

1. **`model_prefix` is defined and consumed nowhere.** `R/constants.R:32` defines
   `model_prefix <- c(\`e5-small\` = "query: ", \`e5-base\` = "query: ")`. A full grep of `R/`,
   `inst/get_embedder.py`, and `get_embedder_v2.py` finds **zero** reads of it. The only other
   `prefix` hit in the embedding path is `py_discover_config()$exec_prefix` — unrelated. So the
   prefix is documented in NEWS ("`sentiment.ai` applies this automatically") but is **not
   applied anywhere**. The NEWS claim is currently false.

2. **The canonical embedder doesn't know e5 exists.** `get_embedder_v2.py`'s
   `sentence_transformers_models` dict contains only `paraphrase_miniLM / mpnet / distilroberta /
   instructorXL`. There is no `e5-small` / `e5-base` / `intfloat/...` entry, and `embed_text()`'s
   sentence-transformers branch is a bare `model.encode(text)` with no prefix hook. So the
   LOCKED default (`e5-small`) **cannot be embedded by the committed embedder at all** — it
   raises `Unsupported model`. The registry in `constants.R` and the embedder in
   `get_embedder_v2.py` have diverged; the roadmap's Phase 0 ("reconcile the two embedders,
   pick one canonical entry point") was never finished, and the lineup was locked on top of the
   gap.

**Why the prefix is genuinely load-bearing, not cosmetic.** e5 models were contrastively
trained with **asymmetric** prefixes — `query: ` for one side, `passage: ` for the other — and
intfloat's own card states embeddings degrade if you drop them, because unprefixed text lands
off the manifold the model was tuned on. Two failure modes follow:

- **Train/serve skew.** The xgb scorer is trained on e5 vectors of one prefixing convention.
  If production embeds with a *different* convention (or none), every inference vector is
  shifted relative to the decision boundary. The model still returns a class — it just returns
  a *quietly wrong* one. No exception, no warning: the worst kind of bug for a package whose
  pitch is "trustworthy sentiment."
- **The bake-off may not have exercised the prefix end-to-end.** `bakeoff/sub_embed.py`
  defaults `--prefix ""`, and `mml_headtohead.log` does not echo the prefix arg (it logs
  model/tag/device/maxseq only). I cannot confirm from the logs that the e5 runs passed
  `--prefix "query: "`. If they did **not**, the 0.860/0.813 figures are the *unprefixed* path
  and are understated — but then "production must use the prefix" would itself introduce skew
  vs the trained scorer. Either way, **the prefix convention used to generate the shipped
  `xgb_model_e5_*` must be pinned and identical in production**, and right now neither half of
  that contract is enforced in code.

**Sub-issue: query-vs-passage.** Even once wired, hardcoding `query: ` may be the wrong choice.
For single-document classification, intfloat's guidance is to use one consistent prefix; many
practitioners use `query: ` for short inputs, but this should be an explicit, tested decision
recorded next to the scorer, not an unread constant. Whatever is chosen must match what trained
the scorer — that is the only invariant that matters.

**Recommendation (must fix before any release):**
- Wire `model_prefix` into the single canonical embedder. Merge `get_embedder_v2.py` and
  `inst/get_embedder.py` into one entry point (finish roadmap Phase 0) that adds the e5 ids and
  prepends the registry prefix **inside** the embed call, so it is impossible to embed e5
  without it.
- Pin the prefix as a property of the **scoring model**, not a user choice. Store the prefix
  string in the scoring-model metadata and assert at score time that the embedder's prefix
  matches the scorer's expected prefix; refuse with a clear error on mismatch. This is the only
  way "applies it automatically" becomes true rather than aspirational.
- Re-run the bake-off for e5 with the prefix **explicitly logged** so the headline figure and
  the trained scorer share one provable convention. Add a snapshot test: known e5 text →
  known vector → known score, that fails if the prefix is dropped.
- Until both are done, the NEWS/DESCRIPTION sentence "`sentiment.ai` applies this automatically"
  is unsupported and should not ship.

---

## SEV-2 (HIGH) — e5-small (0.813) is too weak to be the flagship default

The default is the model 95% of users will run without thinking. e5-small at **0.813** is the
**weakest no-TF model in the table except mpnet and paraphrase** — it is beaten by bge-base
(0.836), gte-base (0.825), and bge-small (0.824), all no-TF and on-device, and it trails its
own sibling e5-base by **4.7 macro-F1 points (0.813 vs 0.860)**. The damage is concentrated
exactly where it hurts a 3-class sentiment tool: **positive-class F1 = 0.779**, the second-worst
positive score of any no-TF model above paraphrase, and a full 5 points below e5-base's 0.831
and bge-small's 0.793 at the *same* 384 dims. Neutral (0.800) is also soft.

The strategic case for e5-small as default is "fast + lightweight + multilingual + same family
as e5-base." That is real, but it ships the **second-weakest** no-TF default the data allows,
which undercuts the v2 headline. The honest framing tension: NEWS leads with "the new default
**ties paid OpenAI**" — but that is **e5-base**, not the default. The *actual default*
(e5-small) is 4.8 points below OpenAI and below three models it's shipped alongside. A reader
who installs defaults and reads the headline will be mismatched on expectations.

There is also a same-footprint embarrassment: **bge-small (384-D, English) beats e5-small
(384-D) 0.824 vs 0.813** — so at the small tier specifically, e5 is not the strongest no-TF
choice for English. e5-small's justification has to rest on multilinguality, not accuracy.

**Recommendations (pick one, in priority order):**
1. **Make e5-base the default**, demote e5-small to the explicit "fast/light" opt-in. e5-base
   is the actual ties-OpenAI story; making it the default aligns the headline with what users
   get. Cost: ~1.1GB download and 768-D vectors (slower scoring). For a research/HR audience on
   modern laptops this is acceptable; gate it behind a one-time download (see SEV-3) rather than
   bundling.
2. **Keep e5-small default but stop claiming the headline on it.** Reframe NEWS so the
   "ties OpenAI" line is unambiguously about e5-base, and the default is sold honestly as
   "good, fast, multilingual, no-TF — a clear upgrade on the old USE default (0.790 → 0.813),
   with e5-base one flag away for top accuracy." Do **not** let "ties OpenAI" sit next to "the
   default."
3. **If English-first matters more than multilingual**, consider bge-small (0.824) for the
   small tier — but only after the multilingual-vs-English tradeoff (SEV-4) is decided, because
   that, not 1 F1 point, is the real axis.

My call: **option 1 if the audience is laptops-with-disk-space (most R/HR researchers), option
2 if the install-size budget is hard**. Either way, decouple "default" from "ties OpenAI" in
the copy — that conflation is the credibility risk.

---

## SEV-3 (HIGH) — e5-base ~1.1GB is incompatible with shipping weights via CRAN; the download path must be the design, not an afterthought

CRAN packages have a hard ~5MB source-tarball expectation; the e5-base weights (~1.1GB
multilingual, fp32) and even e5-small (~470MB) cannot live in the package. This is fine **only
if** the model is fetched on first use to a user cache — which is how the legacy USE path
already worked — but the lineup was locked without the fetch-vs-bundle decision being made
(roadmap Phase 3 still lists it open). Risks if not designed deliberately:

- **CRAN policy.** CRAN dislikes packages that download large payloads on load and forbids
  writing outside the session temp dir without consent. The fetch must be: explicit user
  action (`install_sentiment.ai(...)`), into a `rappdirs`/`tools::R_user_dir` cache, with the
  size disclosed up front, and tests/examples that **skip** when weights aren't present
  (`skip_on_cran`). The scoring-model bundling (`xgb_model_e5_*.xgb`) is small and can ship; the
  **embedder weights cannot**.
- **Reproducibility / pinning.** HuggingFace ids without a revision pin can change under you.
  Pin a specific commit SHA per model id so a future re-upload can't silently shift embeddings
  (and therefore scores) for everyone.
- **First-run UX.** A 1.1GB silent download is its own install-hell echo — exactly what v2
  exists to kill. It must be a clear, consented, resumable step with a progress indicator, not
  a hang.

**Recommendation:** treat weight-fetch as a first-class part of `install_sentiment.ai(backend=)`:
disclose size, fetch to a user cache, pin revision SHAs, `skip_on_cran` everywhere weights are
needed, ship only the small xgb scorers in the tarball. Make e5-small (smaller) the
zero-friction path and e5-base the "I want top accuracy, here's 1.1GB" path. The size is
**acceptable** for the audience *only* behind a consented download — it is **not** acceptable
bundled, and "1.1GB on CRAN" is a non-starter that would block submission.

---

## SEV-4 (MEDIUM) — Dropping the bge family loses the best *English* model; the e5-vs-bge call should be made on the multilingual axis explicitly

On these data, **e5-base (0.860) beats bge-base (0.836) by 2.4 points**, so dropping bge as the
*top* on-device slot is defensible. But the lineup frames it as "e5 simply wins," and the data
say something more nuanced worth recording:

- The bakeoff's bge candidates are **English-only** (`bge-*-en-v1.5`), while the shipped e5
  models are **multilingual** (~100 langs, larger). That is not an apples-to-apples win: e5
  spends parameters on multilinguality; bge-en spends them on English. On a predominantly
  **English** test corpus, e5-base *still* winning by 2.4 points is a real result — but it means
  the English-vs-multilingual tradeoff was never actually isolated, because no **monolingual
  English e5** (`intfloat/e5-base-v2`) was benchmarked. It's plausible English-e5 would widen
  the gap further, or that bge-base would close it on a purely-English deployment.
- At the **small** tier the English model already wins: bge-small 0.824 > e5-small 0.813. So
  "e5 dominates" is false at 384-D for English.

So dropping bge is **not** a mistake *for a multilingual default* — e5 is the right family when
~100-language support is a feature (and it broadens the old USE "16 languages" claim, a genuine
NEWS win). It **would** be a mistake to claim e5 is strictly better for English-only users; for
them bge-base/bge-small are competitive-to-better at smaller footprints.

**Recommendation:** keep e5 as the multilingual default family, but (a) state the tradeoff
honestly in NEWS — "e5 chosen for multilingual coverage; bge is competitive on English-only and
smaller" rather than implying a clean sweep; (b) if a cheap experiment is affordable, embed the
**monolingual** `intfloat/e5-base-v2` to confirm the multilingual variant isn't leaving English
accuracy on the table (if the English variant is much stronger, consider it for an
English-default tier); (c) optionally keep one bge model as an opt-in for English-only users who
want the smallest footprint. Do not publish "e5 beats bge" without the English-vs-multilingual
caveat.

---

## SEV-5 (MEDIUM) — License/redistribution: the weights' terms must be verified and recorded before publishing the lineup

The package is MIT and the lineup leans on third-party weights fetched at install. The licenses
are favorable but must be **confirmed against each model card at a pinned revision and written
into the package**, not assumed:

- `intfloat/multilingual-e5-small` / `-base` — published under **MIT** on their HuggingFace
  cards. e5 is trained partly on web/mined pairs; MIT covers the weights. Confirm at the pinned
  SHA.
- `BAAI/bge-*-en-v1.5` — **MIT** per BAAI's cards.
- `thenlper/gte-base` — **MIT** (Alibaba/`thenlper`).
- `all-mpnet-base-v2`, `paraphrase-MiniLM-L6-v2` — **Apache-2.0** (sentence-transformers / UKP).
- OpenAI 3-small — no redistribution issue (API; nothing shipped). The user's content is sent
  to OpenAI; that's a privacy/ToS note for docs, not a license problem.

The package never **redistributes** weights (it fetches them), which keeps it clean even if a
license were restrictive — but two things are still required: (1) since `install_sentiment.ai`
causes the download, the package is the agent of acquisition and should surface each model's
license and origin to the user (a `model_license` field in the registry + a line in the install
output); (2) MIT/Apache both require attribution — carry a NOTICE/attribution list for the
bundled-by-reference models. None of this blocks the lineup; **omitting it is a publication
risk** (JOSS/CRAN reviewers will ask "where do the weights come from and under what license").

**Recommendation:** add a `model_license` + `model_source_url` + pinned `revision` to the
registry; print license+size on install; ship an attribution NOTICE; verify each card at the
pinned SHA before release. Low effort, removes a reviewer objection.

---

## SEV-6 (LOW) — Legacy USE replacement-mapping overstates "meets or beats" per-tier

NEWS maps `en`→e5-small and `en.large/multi/multi.large`→e5-base with "each legacy model has a
better TF-free replacement." Mostly true on macro-F1, but the **`en` → e5-small** leg is the
weakest claim: USE-large (the strongest legacy) is 0.790 and e5-small is 0.813, fine — but a
user pinned to plain `en`/`multi` (USE-small/multilingual, not benchmarked here) being told
e5-small is "better" is an unmeasured assertion for those specific models. The macro story holds
in aggregate; the per-model "better replacement" table implies measurements that weren't all
taken.

**Recommendation:** soften the per-model table to "a no-TF replacement at or above the old USE
default's accuracy" rather than a model-for-model "better," unless each named USE variant was
actually benchmarked. Keeps the honest-accounting tone the NEWS draft otherwise nails.

---

## Cross-cutting: the full-data run is not yet a headline

`full_embed.log` shows the oai_3_small and use_lg **full** subset extractions wrote **0 rows**
("wrote ... 0 rows (wanted 130311)") — the index-join in the full-subset step failed for those
two tags, so there is currently **no full-data baseline** to compare the e5 full run against.
The e5 full embeddings completed (130,311 rows). Before publishing any headline macro-F1:
re-run the full-subset extraction for oai/use_lg, run `full_train.py` for e5-small/e5-base/oai/
use_lg on the full pool with `num_parallel_tree=24`, and only then finalize the NEWS table. The
NEWS draft already flags figures as provisional — keep that until the full run is clean.

---

## Verdict on the lineup

**Keep the family (e5 + openai + legacy USE) — the thesis is right and e5-base genuinely ties
OpenAI on-device with no TF.** But the lineup as *locked* is not yet shippable:

- **SEV-1 is a release blocker** — the default model cannot be embedded by the committed
  embedder, and the prefix it depends on is dead config. Fix before anything.
- **SEV-2** — either promote e5-base to default or stop letting "ties OpenAI" sit next to a
  default that's 4.8 points behind it.
- **SEV-3** — design the weight-fetch (size, cache, revision pin, `skip_on_cran`) as the
  mechanism, not an afterthought; e5-base's 1.1GB is fine fetched, fatal bundled.

SEV-4/5/6 are honesty-and-completeness fixes that protect the publication story (BRM/JOSS
reviewers will probe exactly these). None change the family; all change the claims.
