# sentiment.ai — DX / developer-experience review

> **Lens:** senior product / developer-experience engineer. Not the model migration,
> not the rigor programme — the *experience of adopting and trusting the tool*: the
> first 5 minutes, the API ergonomics, the docs, error legibility, and whether the
> "honest measurement instrument" positioning survives contact with the README.
> **Scope discipline:** every claim about the package is read from
> `rpackage/sentiment.ai/R/`, `README.md`, `vignettes/vignette.Rmd`, and
> `pypackage/` (cited inline). Written 2026-06-03. Author: Ben Wiseman.
> **Constraint:** planning-only — writes to `planning/`, does not edit source.
>
> **What I'm deliberately NOT re-litigating** (the prior docs own these; I build on
> them, I don't repeat them):
> - The return-contract fix (stop discarding `probs` at `R/sentiment.R:111`, return a
>   tidy shape with `prob_*`/`class`/`confidence`) — owned by `better-lens-rcraft.md` #1,
>   `better-lens-practitioner.md` #1, `better-sentiment-roadmap.md` Move 1. I treat it
>   as **decided** and only add the *ergonomic* details those docs left open (the
>   `output=` default flip risk, the one-liner that goes above the fold, the print method).
> - `cli`-grade errors + `sentiment_status()` — owned by `better-lens-rcraft.md` #3. I
>   extend it into a *staged first-run* and a *single readiness contract across R+Python*,
>   which those docs gesture at but don't design.
> - Calibration / abstention / agreement / provenance internals — owned by the roadmap.
>   I only touch them where they surface to the user (what a *number means* on screen).

---

## The DX thesis in one paragraph

`sentiment.ai`'s adoption problem is **not** capability — the bake-off says it rivals
paid APIs — and it is **not**, after the rcraft/practitioner docs, an unidentified
return-shape problem. The remaining, unowned DX problem is **trust-at-first-contact**:
the three surfaces a new user actually hits in their first five minutes — the README,
the install call, and the first error — are each, today, *actively undermining the one
thing the package is selling* (honest, auditable measurement). The README overclaims
and is full of stale "16 languages / tf hub / 512-D" copy that a careful analyst reads
as broken windows. The install path asks for a conda environment and a session restart
before the user has scored a single sentence. The first error is `cat()`'d padded text,
not a condition. **A package whose entire wedge is "trust the number" is leaking trust
in exactly the moments trust is formed.** The fixes below are ordered by *how early in
the funnel the leak happens*, because an early leak invalidates everything downstream —
the most rigorous calibration in the world is wasted on a user who bounced at the README.

---

## Where a new user forms their judgement (the funnel, with the leak at each stage)

I walked the actual first-contact path. Each stage has a concrete, source-cited leak.

| Stage | What the user does | What they hit today | The trust leak |
|---|---|---|---|
| **0. Discover** | reads the README / CRAN page | `README.md:20` "head-and-shoulders above … go toe-to-toe with Azure … (only we're free!)"; `:36` "dieciséis (16) languages"; `:32` "Universal Sentence Encoder"; `:45` "trained on the 512-D embeddings generated with tensorflow" | Overclaim ("head-and-shoulders") **contradicts the brand** (`better-lens-brand.md`: "matches, not beats"). And the multilingual/USE/512/TF copy is **v1 — every word is wrong for v2** (e5, ~100 langs, 384/768-D, no TF). A careful reader sees stale docs = untrustworthy package. |
| **1. Install** | `install.packages()` then `install_sentiment.ai()` | a function that creates a conda/virtualenv env, pins Python versions, and **restarts your R session** (`init_and_install.R:111,236`) | The competitor (`vader`/`sentimentr`) is `install.packages(); library(); score()` in 10s. Asking for an env + a restart **before the first score** is the single biggest bounce point, and the README's own Troubleshooting section (`README.md:525-585`, ~60 lines of reticulate dragons) advertises the pain. |
| **2. First score** | `sentiment_score("I love this")` | works *if* init succeeded; otherwise `embed_text` `warning()`s and **silently tries to init** (`embedding.R:187-198`), or a raw reticulate/Python traceback | The happy path is genuinely good (one line). The unhappy path is a `cat()`-padded blob (`create_error_text.R`) or a Python stack trace — neither is a classed condition, neither names the fix. |
| **3. First *interesting* call** | tries `sentiment_match()` with custom poles | returns a `data.table` (different shape from `sentiment_score`'s vector), with a `cowboy mode` `warning()` if they fat-finger a model name (`choose_model.R:32-36`) | The package's **best, most differentiated feature** (tunable zero-shot axis) is under-sold as a footnote (`README.md:329` "you can also be tricky and pass in arbitrary themes") and returns an inconsistent shape. |
| **4. Decide / cite** | "do I trust this enough to put in a report?" | no `citation()` story for the model, no one-call provenance, no honest "here's where it's weak" surface | The user who *would* be won by the honesty brand has nothing concrete to grab — the honesty is in `planning/`, not in the package they're holding. |

The pattern: **the package is most honest in its planning docs and least honest in its
README**, and the user only ever sees the README. The DX work is to push the honesty
*forward* to the surfaces of first contact.

---

## API ergonomics: the tensions a DX lens sees that the others didn't

The rcraft/practitioner docs nailed the *return shape*. Three *ergonomic* tensions
remain that are specifically about how the API **feels to call**, not what it returns:

### Tension A — the function name is a brand liability *and* a namespace footgun
`sentiment_score()` / `init_sentiment.ai()` / `install_sentiment.ai()` — the `.ai`
suffix is in the package name, the function names, and the env name. Two DX costs:
1. **It dates the package and reads as hype** to exactly the skeptical, methods-first
   audience the brand targets (the `MEMORY.md` rule: "product surfaces are deterministic,
   never AI"). The package literally calls a frozen-encoder-plus-xgboost pipeline ".ai".
   You can't rename the package (CRAN), but you *can* and should stop leaning on it in
   copy and add **plain-language aliases** for the verbs (see Fix 1).
2. `init_sentiment.ai` / `install_sentiment.ai` are **awkward to type and to autocomplete**
   (the `.` mid-name fights RStudio completion and every Python/tidy convention). The
   Python side already escaped this (`ensure_model()`, `sentimentai`) — the asymmetry is
   itself a DX tell.

### Tension B — three setup functions where the user wants zero
`install_sentiment.ai()` → `init_sentiment.ai()` → (`check_sentiment.ai()` auto-recovers)
is **three nouns for one intent** ("be ready to score"). The README spends ~50 lines
(`:163-253`) explaining the difference between install and init and envname and method.
The user's mental model is binary: *ready / not ready*. The Python scaffold collapsed
this to one lazy `ensure_model()` and a first-call auto-load (`PARITY.md §6`) — that is
strictly better DX, and **R should expose the same single-verb front door** even though
it keeps the trio underneath (see Fix 2). The fact that `sentiment_score()` *already*
auto-recovers via `check_sentiment.ai()` proves the lazy path works — it's just
undocumented and emits a scary warning instead of a progress bar.

### Tension C — `model =` is a string with no discoverability or validation-at-the-door
`sentiment_score(x, model = "e5-base")` — but if you type `"e5_base"` or `"e5-Base"` you
fall into `choose_model()`'s **cowboy-mode pass-through-with-warning** (`choose_model.R:32`),
which then fails *deep* inside the embedder with a confusing HuggingFace error, not at
the call site. There is no `list_models()`, no `?models`, no tab-completable enum. A user
discovers the model menu only by reading the README. Two cheap DX fixes: a `list_models()`
that prints the registry (the data already exists in `constants.R` + `model_meta.R`), and
a **did-you-mean** on near-miss names (`agrep` against the registry) before cowboy mode.

---

## R ↔ Python: parity is a DX feature, not just a correctness spec

`PARITY.md` is excellent on *numeric* parity ([MATCH]/[SHAPE]/[MAY-DIFFER]). But there's
a **DX parity** layer it doesn't name, and it's a positioning asset if you get it right:
the package is becoming a **measurement platform with two front doors (R, Python)** and one
promise. For that to land, the *vocabulary* must be identical even where the *machinery*
differs. Today it isn't:

- R: `install_sentiment.ai()` + `init_sentiment.ai()` + `check_sentiment.ai()`.
  Python: `ensure_model()`. — different verbs, different counts.
- R: `sentiment_score()` returns a **named numeric vector**; Python returns a **bare
  `np.ndarray`** (`PARITY.md §4.7`). A user who reads the R docs and switches to Python
  finds the score isn't labelled by text — a small but real "wait, where'd my labels go".
- The two READMEs tell **different origin stories** (R README: "from tf hub", v1 voice;
  Python README: "TensorFlow-free … same trained scorer artifacts", v2 voice). Same
  product, two personalities.

The DX win (Fix 5): a **single shared concept glossary** — `score`, `match`, `embed`,
`ready` — named the same in both languages and on both READMEs, with the *one* sanctioned
divergence (install ergonomics) called out explicitly *to the user*, not just in the spec.
"It's the same five verbs in both languages; only the install differs, because Python
doesn't need the conda dance" is a *sentence a buyer likes hearing*. It turns the
two-package maintenance burden into a credibility signal.

---

## TOP DX FIXES (ordered by where in the funnel the trust leak happens)

### Fix 1 — Rewrite the README as the honesty instrument, not the hype sheet. **[quick, highest leverage]**

This is #1 because it's **stage 0** — every user hits it, and right now it's the loudest
contradiction of the brand in the entire repo. Concretely:

1. **Kill the overclaim.** `README.md:20` "head-and-shoulders above … go toe-to-toe with
   Azure (only we're free!)" → the brand line: *"matches paid APIs on accuracy, on-device,
   and tells you when it's unsure."* (`honest-copy-rewrites.md` already drafts this tone —
   wire it into the actual README, which it hasn't been.)
2. **Purge every v1 fact.** Search-and-replace the stale truths: "dieciséis (16) languages"
   → "~100 languages (inherited from multilingual-e5)"; "Universal Sentence Encoder /
   tf hub" → "on-device sentence-transformers, no TensorFlow"; "512-D embeddings generated
   with tensorflow" (`:45`) → the e5 384/768 reality. **The same stale copy is duplicated
   verbatim in `vignettes/vignette.Rmd`** (`:30-45`) — fix both, they are the two most-read
   files and both currently describe a package that no longer exists.
3. **Lead with the one-liner that wins.** Above the fold, the *single* example that beats
   every competitor in one screen — the tidy-output dplyr pipe from `better-lens-rcraft.md`:
   ```r
   library(sentiment.ai)
   reviews |> mutate(sentiment(text)) |> count(class)   # prob_*, class, confidence as columns
   ```
   Not the 17-row `data.table` cross-package benchmark that currently opens (`:50-120`) —
   that's a *proof*, move it below the fold; the *hook* is "one line, tidy, trustworthy."
4. **Add a "What this is / isn't" honesty box** near the top — three bullets each. *Is:*
   document-level semantic sentiment, custom anchors, on-device, reproducible. *Isn't:*
   aspect-extraction, emotion taxonomy, a leaderboard chaser, magic. **Shipping your own
   limitations at the top of the README is the single most on-brand DX move available** —
   it's the `better-sentiment-roadmap.md` model-card idea, surfaced where the skeptic
   actually looks. No competitor README does this; it disarms the exact reader you want.

> Why highest leverage: zero code, biggest funnel, and it's the cheapest possible
> alignment of the artifact-the-user-holds with the brand-the-planning-docs-promise.
> Every other fix is wasted if the README has already lost the reader.

### Fix 2 — One front door: `sentiment_ready()` (R) ≡ `ensure_model()` (Python), with a *designed* first run. **[quick→medium]**

Stage 1, the install cliff — the biggest bounce. Build on rcraft #3's `sentiment_status()`
but go further: collapse the **intent** to one verb and design the first-run as a *step*,
not a *hang*.

- **A single readiness verb** `sentiment_ready()` (or fold into the existing
  `check_sentiment.ai`, re-exported under a plain name) that: detects backend, reports what
  is/isn't present (env, weights, which model+revision, cache location, size), **and offers
  to fix it** — `cli`-styled, one consented prompt, a real progress bar (the code already
  has `txtProgressBar`/`pbapply`, `embedding.R:133,255`), cached to
  `tools::R_user_dir("sentiment.ai","cache")`. It returns a structured object *and* prints
  a status block, so it doubles as the support-ticket triage tool.
- **Make the lazy path the documented path.** `sentiment_score()` already auto-recovers
  via `check_sentiment.ai()` (`init_and_install.R:523`). Stop hiding that behind a
  `warning("...embed not found!")` (`embedding.R:188`) — turn it into a friendly
  *"first run: downloading e5-small (~120 MB), one time…"* with a progress bar. The
  *capability* exists; it's the *presentation* that screams "error" when it should say
  "setting up."
- **Demote the conda dance to an "advanced / legacy" appendix.** For the v2 default (e5,
  no TF), most users should never see `method`/`envname`/`python_version`/`fresh_install`/
  `restart_session`. Those belong on the legacy USE path. The README's 60-line
  Troubleshooting section (`:525-585`) should shrink to ~10 lines for the default path and
  move the reticulate-dragons content under a clearly-labelled "Legacy TensorFlow models"
  collapsible. **The install story should match the v2 reality: it got much simpler — say so.**
- **Parity:** R `sentiment_ready()` and Python `ensure_model()` print the *same* status
  vocabulary and the *same* next-command hints. One mental model, two languages.

> Why #2: it's the difference between "weird Python error, typed `install.packages('sentimentr')`
> instead" and "oh, it just downloaded a model and worked." v2 already *made* this better
> by dropping TF — this fix makes the DX *reflect* that win instead of carrying v1's scar tissue.

### Fix 3 — Promote `sentiment_match()` from footnote to co-headline: the named-axis classifier. **[quick, positioning]**

Stage 3, the differentiated call. This is the one feature **no lexicon package and no
transformer-head model has** (the roadmap's D2), and the README buries it as "you can also
be tricky" (`:329`). DX-wise it's mis-*framed*, not mis-built. Concrete moves:

- **Give it a verb that says what it is.** Add `classify_text(x, labels = list(...))` as a
  documented alias/wrapper over `sentiment_match()` (the runner-up in `better-lens-rcraft.md`,
  promoted here to top-3 because it's pure DX/positioning with code that already exists).
  "Score *any* axis you can name by example — urgency, churn-risk, quality, formality —
  label-free" is a headline, not a footnote.
- **Make the explanation legible by default.** It already returns `phrase` + `similarity`
  (the nearest anchor + cosine). Document that *as the receipt*: "scored −0.66; nearest
  anchor 'sad' at cos 0.15." That sentence **is** the auditable-not-AI brand made tangible
  in the output a user can see — surface it in the README example, not just the column docs.
- **Unify the shape** (depends on the rcraft return-contract fix): once `sentiment_score()`
  returns a tidy frame, `sentiment_match()`/`classify_text()` should be the *same frame plus
  `anchor`/`anchor_similarity` columns* — one shape, progressively enriched, never two
  mental models.
- **Honesty guardrail** (keep the brand clean): cosine similarity is length-sensitive and
  the anchor is an *approximation*, not a proof — say so in one line, exactly as
  `better-lens-rcraft.md #2` insists. The feature is differentiating *because* it's framed
  honestly.

> Why #3: it costs almost no code (the function exists), and it's the clearest answer to
> "why this over vader/sentimentr/tidytext/text" — none of them can redefine the axis. Right
> now the package's sharpest selling point is its most under-documented surface.

### Fix 4 — A `compare`/onboarding vignette that *is* the differentiator demo. **[medium]**

Stage 4, the decide-to-adopt moment. The current vignette is a stale clone of the README
(`vignette.Rmd:30-45` repeats the v1 overclaims verbatim). Replace it with the doc a
skeptical analyst actually needs to convert:

- **`vignette("sentiment-ai")` = the 5-minute path:** install-or-lazy-load → score one
  line → read the tidy output (what `class`/`confidence`/`prob_*` mean) → custom anchors →
  done. No conda, no benchmark tables, no "16 languages" — just the happy path a beginner
  can complete without bouncing.
- **`vignette("vs-other-packages")` = the honest comparison** the rcraft doc already wrote
  the table for (`better-lens-rcraft.md` competitor matrix). Show the *same* sentences scored
  by `sentiment.ai`, `vader`, `sentimentr`, `tidytext` side by side — including a case where
  a **competitor wins** (vader on a tweet, sentimentr's term attribution). *Showing where you
  lose is the most credible thing you can do* and it's the exact `MEMORY.md` rule ("don't
  namecheck competitors as a put-down; let the reader judge"). This vignette is both DX
  onboarding *and* the brand's honesty proof — one artifact, two jobs.
- **A runnable `cite`/provenance cell:** show `citation("sentiment.ai")` *and* a one-call
  provenance stamp (the roadmap's Move 3) so the analyst sees, in the onboarding doc, exactly
  what they'd paste into a methods section. Make "I can defend this number" a *tutorial step*,
  not a buried capability.

> Why #4: vignettes are where R users decide. A stale one signals neglect; a comparison-led,
> honesty-led one *is* the sales pitch, written as docs, indexed on the pkgdown site, found
> by the exact person evaluating you. It's medium-effort because it needs the tidy-output fix
> (Fix 1/rcraft #1) underneath to demo the good shape.

### Fix 5 — One vocabulary across R + Python (two doors, one promise). **[quick, compounding]**

Cross-cutting, low-effort, high brand-compounding. Make the *DX* parity explicit, not just
the numeric parity:

- **Name the verbs identically** where the user types them: `score` / `match` (or
  `classify`) / `embed` / `ready`. R keeps its trio under the hood but exposes the same
  four-verb façade as Python. The `.ai`-in-function-names asymmetry between the two packages
  (Tension A) disappears.
- **Unify the two READMEs' origin story** to the v2 voice (Fix 1 fixes R; the Python README
  is already there — make R match it, not the reverse).
- **State the one sanctioned divergence to the user**, not just in `PARITY.md`: a short
  "R vs Python" box on both READMEs — *"identical scores, identical verbs; the only difference
  is setup, because Python skips R's conda environment dance."* That single honest sentence
  converts the maintenance cost of two packages into a trust signal ("they kept them in sync
  and told me exactly where they differ").
- **Mirror the structured output:** when R returns the tidy frame (rcraft #1), Python returns
  the pandas equivalent with the *same column names/order* (`PARITY.md §5` already mandates
  this for `match`; extend it to `score`). A user moving between languages should recognise
  the output instantly.

> Why #5: it's nearly free and it's the move that makes "measurement *platform*, R and Python"
> credible rather than aspirational. Parity-as-DX is a feature you can put on the box.

---

## Runner-ups (documented so they aren't lost; below the top-5 cut line)

- **`print`/`format` method for the result object.** Once `sentiment_score()` returns a
  tidy frame, a custom print that shows score + a tiny sparkline/bar of `prob_*` and flags
  low-confidence rows turns "a table of numbers" into "a thing you can read at a glance."
  Cheap, and it's the moment the output *feels* like an instrument.
- **`list_models()` + did-you-mean.** Print the registry (`constants.R`/`model_meta.R` data
  already exists: name, dim, langs, license, on-device?); `agrep` near-miss correction before
  cowboy-mode (`choose_model.R:32`). Kills the "typo → deep HuggingFace error" failure.
- **Classed conditions, not just nicer text.** Beyond rcraft #3's `cli_abort`: give the
  conditions classes (`sentiment_ai_not_installed`, `sentiment_ai_model_missing`,
  `sentiment_ai_offline`) so downstream pipelines/Shiny apps can `tryCatch` on them. DX for
  the *developers building on top of you*, not just end users.
- **A 30-second asciinema / GIF in the README** of the happy path (lazy first-run → score →
  tidy output). "See it work before you install it" is the highest-converting README element
  and costs one recording.
- **`Suggests`-gate the heavy stuff, document the light core.** Make the no-TF, on-device
  path the one with the smallest dependency footprint and *say so* — "the default needs no
  TensorFlow and makes zero network calls at score time" is both a DX fact and the
  on-prem/privacy selling point the practitioner doc (Job 4) flagged.
- **Fix the `embed_text()` warning-then-silently-init path** (`embedding.R:187-198`) — it's
  both a correctness smell (rcraft #3 / lineup SEV-1) *and* a DX smell (a warning that means
  "I'm quietly doing something big"). Make it an explicit, progress-barred, consented step.

---

## The one-sentence DX north star

After these fixes the first-five-minutes story is: *"Install it, score one line, get a tidy
table with a real label and a confidence and the anchor that explains it — same five verbs
in R or Python, no TensorFlow, no network call, and a README that tells you where it's weak
before you ask."* That is a developer experience **no competitor can narrate in full**, and
crucially it's the *honesty brand made operable* — the trust the planning docs promise,
finally delivered in the first surfaces a user touches instead of buried where only the
author reads.

---

## TOP 3 (the ones that most move adoption)

1. **Rewrite the README/vignette as the honesty instrument (Fix 1).** Stage-0, every user,
   zero code. Today the most-read files overclaim ("head-and-shoulders above Azure") and
   describe a v1 package that no longer exists ("16 languages / tf hub / 512-D TensorFlow")
   — the loudest contradiction of the "honest, computed-not-AI" brand in the whole repo.
   Lead with the tidy one-liner and a "what it is / isn't" box; the honesty currently lives
   only in `planning/`, where no user looks.

2. **One front door + a designed first-run (Fix 2).** Collapse install→init→check into a
   single `sentiment_ready()` ≡ Python `ensure_model()`, make the existing lazy-load path
   the *documented* path with a friendly progress-barred first download instead of a scary
   `warning()`, and demote the conda/restart dance to a legacy appendix. v2 already made
   setup dramatically simpler by dropping TF — the DX should *show* that win, not carry v1's
   reticulate scar tissue.

3. **Promote `sentiment_match()` to a headline named-axis classifier (Fix 3).** The one
   feature no competitor has is buried as "you can also be tricky." Add a `classify_text(x,
   labels=...)` verb over the existing code, surface the nearest-anchor + cosine as the
   built-in *receipt* ("scored −0.66; nearest anchor 'sad' at cos 0.15"), and unify its
   output shape with `sentiment_score()`. It's the clearest answer to "why this over
   vader/sentimentr/tidytext/text," it's the brand's auditability made visible, and the code
   already exists — it just needs to be framed as the differentiator it is.
