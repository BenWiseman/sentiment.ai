#!/usr/bin/env python3
"""
Test 3: Multilingual benchmark — sentiment.ai e5-small/e5-base vs vader/roberta.
First tries to load cardiffnlp/tweet_sentiment_multilingual via HuggingFace datasets.
Falls back to generating synthetic examples with OpenAI GPT-4o if dataset unavailable
or if --synthetic flag is passed.

Languages: English, Spanish, French, German, Portuguese, Arabic
(roberta = English-only; vader = English-only; sentiment.ai handles all ~100 languages)

Saves: planning/multilingual_results.json
"""
import sys, os, json, time, re, csv
import numpy as np

sys.path.insert(0, os.path.join(os.path.dirname(__file__), "../pypackage"))
OPENAI_KEY = os.environ.get("OPENAI_API_KEY", "")

LANGUAGES = ["english", "spanish", "french", "german", "portuguese", "arabic"]
N_PER_LANG_PER_CLASS = 50   # 50 neg + 50 neu + 50 pos per language = 900 total

# ── metric helpers ─────────────────────────────────────────────────────────────
def macro_f1(preds, trues):
    f1s = []
    for cls in ["negative","neutral","positive"]:
        tp=sum(1 for p,t in zip(preds,trues) if p==cls and t==cls)
        fp=sum(1 for p,t in zip(preds,trues) if p==cls and t!=cls)
        fn=sum(1 for p,t in zip(preds,trues) if p!=cls and t==cls)
        if tp+fp==0 or tp+fn==0: continue
        pr=tp/(tp+fp); rc=tp/(tp+fn)
        if pr+rc>0: f1s.append(2*pr*rc/(pr+rc))
    return round(sum(f1s)/len(f1s),3) if f1s else None

def pct_agree(preds,trues):
    return round(sum(1 for p,t in zip(preds,trues) if p==t)/len(preds),3)

def to_3class(s, pos=1/3, neg=-1/3):
    if s is None: return "neutral"
    return "positive" if s>pos else ("negative" if s<neg else "neutral")

# ── skip HuggingFace (dataset requires deprecated trust_remote_code) ───────────
corpus = None
print("Using OpenAI GPT-4o-mini for synthetic multilingual corpus generation.")

# ── fallback: generate synthetic corpus with OpenAI ───────────────────────────
if corpus is None:
    if not OPENAI_KEY:
        print("No OpenAI key and no HuggingFace dataset. Cannot run multilingual benchmark.")
        sys.exit(1)

    print("\nGenerating synthetic multilingual corpus with GPT-4o...")
    from openai import OpenAI
    client = OpenAI(api_key=OPENAI_KEY)

    corpus = []
    for lang in LANGUAGES:
        for sentiment in ["negative","neutral","positive"]:
            print(f"  Generating {N_PER_LANG_PER_CLASS} {sentiment} {lang} examples...")
            prompt = f"""Generate exactly {N_PER_LANG_PER_CLASS} short {sentiment} sentiment sentences in {lang}.
Rules:
- Each sentence on its own line, no numbering or bullets
- 10-25 words each, natural everyday language
- {sentiment.upper()} sentiment only — avoid ambiguity
- Cover diverse topics: products, food, travel, work, relationships, news
- DO NOT include English translations or explanations
- Output ONLY the {lang} sentences, one per line"""

            resp = client.chat.completions.create(
                model="gpt-4o-mini",
                messages=[{"role":"user","content":prompt}],
                temperature=0.9,
                max_tokens=2000)
            lines = [l.strip() for l in resp.choices[0].message.content.strip().split("\n")
                     if l.strip() and not l.strip().startswith(("#","-","•","*"))]
            lines = lines[:N_PER_LANG_PER_CLASS]
            for line in lines:
                corpus.append({"text": line, "label": sentiment, "language": lang})
            print(f"    got {len(lines)} examples")

    out_corpus = os.path.join(os.path.dirname(__file__), "multilingual_corpus.json")
    with open(out_corpus,"w",encoding="utf-8") as f:
        json.dump(corpus, f, ensure_ascii=False, indent=2)
    print(f"Saved corpus: {len(corpus)} examples → {out_corpus}")

# summary
print(f"\nCorpus: {len(corpus)} examples")
by_lang = {}
for r in corpus:
    by_lang.setdefault(r["language"],[]).append(r["label"])
for lang, lbls in by_lang.items():
    counts = {s: lbls.count(s) for s in ["negative","neutral","positive"]}
    print(f"  {lang}: {counts}")

texts_all = [r["text"]  for r in corpus]
labels_all= [r["label"] for r in corpus]
langs_all = [r["language"] for r in corpus]

# ── sentiment.ai ──────────────────────────────────────────────────────────────
import importlib
sa = importlib.import_module("sentimentai")

results = {}
for model in ("e5-small","e5-base"):
    print(f"\nscoring with sentimentai {model}...")
    t0     = time.perf_counter()
    scores = sa.sentiment_score(texts_all, model=model)
    elapsed= time.perf_counter()-t0
    preds  = [to_3class(float(s)) for s in scores]
    overall_f1 = macro_f1(preds, labels_all)
    print(f"  overall macro-F1={overall_f1}  {round(len(texts_all)/elapsed)} t/s")

    by_lang_f1 = {}
    for lang in LANGUAGES:
        idx = [i for i,l in enumerate(langs_all) if l==lang]
        if not idx: continue
        p = [preds[i] for i in idx]; t_=[labels_all[i] for i in idx]
        by_lang_f1[lang] = macro_f1(p, t_)
        print(f"    {lang}: F1={by_lang_f1[lang]}")

    results[f"sentimentai_{model.replace('-','_')}"] = {
        "macro_f1_overall": overall_f1,
        "by_language": by_lang_f1,
        "pct_agree": pct_agree(preds, labels_all)}

# ── vader (English only) ───────────────────────────────────────────────────────
print("\nScoring with vader...")
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
sia = SentimentIntensityAnalyzer()
vader_label_map = lambda c: "positive" if c>=0.05 else ("negative" if c<=-0.05 else "neutral")
preds_vader = [vader_label_map(sia.polarity_scores(t)["compound"]) for t in texts_all]
print(f"  overall macro-F1={macro_f1(preds_vader, labels_all)}")
vader_by_lang = {}
for lang in LANGUAGES:
    idx=[i for i,l in enumerate(langs_all) if l==lang]
    p=[preds_vader[i] for i in idx]; t_=[labels_all[i] for i in idx]
    vader_by_lang[lang]=macro_f1(p,t_)
    print(f"    {lang}: F1={vader_by_lang[lang]}")
results["vader"] = {
    "macro_f1_overall": macro_f1(preds_vader, labels_all),
    "by_language": vader_by_lang,
    "pct_agree": pct_agree(preds_vader, labels_all),
    "note": "English-only rule-based lexicon"}

# ── twitter-roberta (English-only model) ─────────────────────────────────────
print("\nScoring with twitter-roberta (English-only model)...")
from transformers import pipeline as hf_pipeline
pipe = hf_pipeline("text-classification",
                   model="cardiffnlp/twitter-roberta-base-sentiment-latest",
                   truncation=True, max_length=128, device=-1)
label_map_rb = {"positive":"positive","neutral":"neutral","negative":"negative",
                "POSITIVE":"positive","NEUTRAL":"neutral","NEGATIVE":"negative",
                "LABEL_2":"positive","LABEL_1":"neutral","LABEL_0":"negative"}

# only run roberta on English (it's meaningless on Arabic/German etc)
rb_by_lang = {}
eng_idx = [i for i,l in enumerate(langs_all) if l=="english"]
eng_texts = [texts_all[i] for i in eng_idx]
eng_labels= [labels_all[i] for i in eng_idx]
print(f"  running on {len(eng_texts)} English examples...")
rb_out = pipe(eng_texts, batch_size=64)
rb_preds_eng = [label_map_rb.get(r["label"],"neutral") for r in rb_out]
f1_eng_rb = macro_f1(rb_preds_eng, eng_labels)
rb_by_lang["english"] = f1_eng_rb
print(f"    english: F1={f1_eng_rb}")
print(f"    other languages: N/A (model is English-only)")
results["roberta"] = {
    "macro_f1_overall": None,
    "by_language": rb_by_lang,
    "note": "English-only; non-English results omitted",
    "pct_agree_english": pct_agree(rb_preds_eng, eng_labels)}

# ── save ──────────────────────────────────────────────────────────────────────
out = os.path.join(os.path.dirname(__file__), "multilingual_results.json")
with open(out,"w",encoding="utf-8") as f:
    json.dump(results, f, indent=2)
print(f"\nSaved to {out}")

print("\n=== MULTILINGUAL SUMMARY ===")
print(f"{'method':<28} {'overall F1':>12}  {'eng':>6}  {'spa':>6}  {'fra':>6}  {'deu':>6}  {'por':>6}  {'ara':>6}")
for k, v in results.items():
    overall = f"{v['macro_f1_overall']:.3f}" if v.get('macro_f1_overall') else "  —  "
    lang_vals = []
    for lang in LANGUAGES:
        val = v.get("by_language",{}).get(lang)
        lang_vals.append(f"{val:.3f}" if val else "  — ")
    print(f"  {k:<26} {overall:>12}  {'  '.join(lang_vals)}")
