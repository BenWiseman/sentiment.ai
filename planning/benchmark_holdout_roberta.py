#!/usr/bin/env python3
"""
Test 1: Run twitter-roberta on the SAME mixed-domain held-out set used for the ★ numbers.
Fair comparison: exact same test texts, exact same labels, no domain contamination argument.
Saves: planning/holdout_results.json
"""
import sys, os, time, json, csv, random
import numpy as np

TRAINING_BASE = os.path.expanduser(
    "~/sentiment.ai_training/sentiment.ai_training")
PYPACKAGE     = os.path.join(os.path.dirname(__file__), "../pypackage")
sys.path.insert(0, PYPACKAGE)

# ── load held-out real-only test split ─────────────────────────────────────────
def load_holdout():
    idx_path  = os.path.join(TRAINING_BASE, "bakeoff", "full_idx.csv")
    text_path = os.path.join(TRAINING_BASE, "training_data", "all_data.csv")

    def to_int(s):
        try: return int(s)
        except ValueError: return int(float(s))

    # read split info
    idx = {}
    with open(idx_path, newline="", encoding="utf-8") as f:
        for row in csv.DictReader(f):
            idx[to_int(row["index"])] = {
                "split": row["split"],
                "label": int(float(row["sentiment_score"]))}

    # read texts + is_synthetic flag
    rows = []
    with open(text_path, newline="", encoding="utf-8") as f:
        for row in csv.DictReader(f):
            i = to_int(row["index"])
            if i not in idx: continue
            if idx[i]["split"] == "train": continue          # test split only
            is_syn = row.get("is_synthetic","False").strip().lower()
            if is_syn in ("true","1","yes"): continue        # real only
            lbl = idx[i]["label"]
            if lbl not in (-1, 0, 1): continue
            rows.append({"index": i, "text": row["text"], "label": lbl})

    random.seed(42)
    random.shuffle(rows)
    return rows

print("Loading held-out real-only test set...")
rows   = load_holdout()
texts  = [r["text"] for r in rows]
labels = [r["label"] for r in rows]
print(f"  {len(rows)} real-only test examples")
label_counts = {-1: labels.count(-1), 0: labels.count(0), 1: labels.count(1)}
print(f"  neg={label_counts[-1]} neu={label_counts[0]} pos={label_counts[1]}")

# ── metrics ────────────────────────────────────────────────────────────────────
def to_3class_num(s, pos=1/3, neg=-1/3):
    if s is None or (isinstance(s, float) and np.isnan(s)): return 0
    return 1 if s > pos else (-1 if s < neg else 0)

def macro_f1(preds, trues):
    f1s = []
    for cls in [-1, 0, 1]:
        tp = sum(1 for p, t in zip(preds, trues) if p == cls and t == cls)
        fp = sum(1 for p, t in zip(preds, trues) if p == cls and t != cls)
        fn = sum(1 for p, t in zip(preds, trues) if p != cls and t == cls)
        if tp+fp == 0 or tp+fn == 0: continue
        pr = tp/(tp+fp); rc = tp/(tp+fn)
        if pr+rc > 0: f1s.append(2*pr*rc/(pr+rc))
    return round(sum(f1s)/len(f1s), 4) if f1s else None

def bootstrap_ci(preds_a, preds_b, trues, n_boot=2000, seed=0):
    """Bootstrap 95% CI on macro-F1(a) - macro-F1(b)."""
    rng = np.random.default_rng(seed)
    n   = len(trues)
    diffs = []
    pa = np.array(preds_a); pb = np.array(preds_b); pt = np.array(trues)
    for _ in range(n_boot):
        idx = rng.integers(0, n, size=n)
        fa = macro_f1(pa[idx].tolist(), pt[idx].tolist())
        fb = macro_f1(pb[idx].tolist(), pt[idx].tolist())
        if fa is not None and fb is not None:
            diffs.append(fa - fb)
    diffs.sort()
    lo, hi = diffs[int(0.025*len(diffs))], diffs[int(0.975*len(diffs))]
    return round(lo, 4), round(hi, 4)

# ── sentiment.ai (e5-small + e5-base) ─────────────────────────────────────────
import importlib
sa = importlib.import_module("sentimentai")

results = {}

for model in ("e5-small", "e5-base"):
    print(f"\nScoring with sentimentai {model}...")
    t0     = time.perf_counter()
    scores = sa.sentiment_score(texts, model=model)
    elapsed = time.perf_counter() - t0
    preds  = [to_3class_num(float(s)) for s in scores]
    f1     = macro_f1(preds, labels)
    pct    = round(sum(1 for p,t in zip(preds,labels) if p==t)/len(preds), 4)
    print(f"  macro-F1={f1}  agree={pct}  {round(len(texts)/elapsed)} t/s")
    results[f"sentimentai_{model.replace('-','_')}"] = {
        "macro_f1": f1, "pct_agree": pct,
        "n": len(texts), "preds": preds}

# ── twitter-roberta ────────────────────────────────────────────────────────────
print("\nLoading twitter-roberta...")
from transformers import pipeline
pipe = pipeline("text-classification",
                model="cardiffnlp/twitter-roberta-base-sentiment-latest",
                truncation=True, max_length=128, device=-1)
label_map = {"positive": 1, "neutral": 0, "negative": -1,
             "POSITIVE": 1, "NEUTRAL": 0, "NEGATIVE": -1,
             "LABEL_2":  1, "LABEL_1": 0, "LABEL_0":  -1}

print("Scoring with twitter-roberta (this takes a few minutes)...")
t0 = time.perf_counter()
roberta_out = pipe(texts, batch_size=64)
elapsed_rb  = time.perf_counter() - t0
preds_rb    = [label_map.get(r["label"], 0) for r in roberta_out]
f1_rb  = macro_f1(preds_rb, labels)
pct_rb = round(sum(1 for p,t in zip(preds_rb,labels) if p==t)/len(preds_rb), 4)
print(f"  macro-F1={f1_rb}  agree={pct_rb}  {round(len(texts)/elapsed_rb)} t/s")
results["roberta"] = {
    "macro_f1": f1_rb, "pct_agree": pct_rb,
    "n": len(texts), "preds": preds_rb}

# ── bootstrap CIs ─────────────────────────────────────────────────────────────
print("\nBootstrap CIs (2000 resamples)...")
for model_key in ("e5-small", "e5-base"):
    k      = f"sentimentai_{model_key.replace('-','_')}"
    preds_sa = results[k]["preds"]
    lo, hi   = bootstrap_ci(preds_sa, preds_rb, labels)
    f1_diff  = round(results[k]["macro_f1"] - f1_rb, 4)
    print(f"  sentimentai {model_key} vs roberta: Δ={f1_diff:+.4f}  95%CI [{lo:+.4f}, {hi:+.4f}]")
    results[k]["vs_roberta_delta"]    = f1_diff
    results[k]["vs_roberta_ci_lower"] = lo
    results[k]["vs_roberta_ci_upper"] = hi

# also e5-small vs e5-base CI
lo2, hi2 = bootstrap_ci(
    results["sentimentai_e5_base"]["preds"],
    results["sentimentai_e5_small"]["preds"], labels)
f1_diff2 = round(results["sentimentai_e5_base"]["macro_f1"] -
                 results["sentimentai_e5_small"]["macro_f1"], 4)
print(f"  e5-base vs e5-small: Δ={f1_diff2:+.4f}  95%CI [{lo2:+.4f}, {hi2:+.4f}]")

# strip preds before saving (large arrays)
for k in results:
    results[k].pop("preds", None)

out = os.path.join(os.path.dirname(__file__), "holdout_results.json")
with open(out, "w") as f:
    json.dump(results, f, indent=2)
print(f"\nSaved to {out}")
print("\n=== SUMMARY ===")
for k, v in results.items():
    ci_str = ""
    if "vs_roberta_delta" in v:
        ci_str = f"  vs roberta Δ={v['vs_roberta_delta']:+.4f} 95%CI [{v['vs_roberta_ci_lower']:+.4f},{v['vs_roberta_ci_upper']:+.4f}]"
    print(f"  {k}: F1={v['macro_f1']} agree={v['pct_agree']}{ci_str}")
