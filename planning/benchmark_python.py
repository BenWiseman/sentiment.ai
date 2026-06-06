#!/usr/bin/env python3
"""
Python sentiment benchmark — speed + accuracy on 1000 airline tweets.
Compares: sentimentai-py, vaderSentiment, TextBlob, HuggingFace pipeline
Saves: planning/benchmark_python_results.json
"""
import sys, time, json, os, csv, random

# ── load data ──────────────────────────────────────────────────────────────────
DATA_CSV = os.path.join(os.path.dirname(__file__),
                        "../rpackage/sentiment.ai/data-raw")
# airline_tweets is also in the package as an .rda; easiest to just regenerate from R
# or use the CSV we'll create below. We'll pass it via stdin args instead.

def load_tweets_from_r():
    """Run a quick R snippet to dump airline_tweets to /tmp/airline_tweets.csv."""
    import subprocess
    r_code = """
suppressMessages(devtools::load_all('rpackage/sentiment.ai', quiet=TRUE))
data(airline_tweets)
set.seed(42)
idx <- sample(nrow(airline_tweets), 1000, replace=FALSE)
sub <- airline_tweets[idx, c('text','airline_sentiment')]
write.csv(sub, '/tmp/airline_bench_tweets.csv', row.names=FALSE)
cat('done\n')
"""
    result = subprocess.run(['Rscript', '-e', r_code],
                           capture_output=True, text=True,
                           cwd=os.path.join(os.path.dirname(__file__), ".."))
    if result.returncode != 0:
        print("R export failed:", result.stderr[:500], file=sys.stderr)
        sys.exit(1)

def read_tweets():
    rows = []
    with open('/tmp/airline_bench_tweets.csv', newline='', encoding='utf-8') as f:
        for row in csv.DictReader(f):
            rows.append(row)
    texts = [r['text'] for r in rows]
    labels = [r['airline_sentiment'] for r in rows]
    return texts, labels

# ── metric helpers ──────────────────────────────────────────────────────────────
def to_3class(score, pos_thr=1/3, neg_thr=-1/3):
    if score is None or score != score:  # None or NaN
        return None
    if score > pos_thr:   return 'positive'
    if score < neg_thr:   return 'negative'
    return 'neutral'

def macro_f1(preds, trues):
    classes = ['negative', 'neutral', 'positive']
    f1s = []
    for cls in classes:
        tp = sum(1 for p, t in zip(preds, trues) if p == cls and t == cls)
        fp = sum(1 for p, t in zip(preds, trues) if p == cls and t != cls)
        fn = sum(1 for p, t in zip(preds, trues) if p != cls and t == cls)
        if tp + fp == 0 or tp + fn == 0:
            continue
        prec = tp / (tp + fp); rec = tp / (tp + fn)
        if prec + rec == 0: continue
        f1s.append(2 * prec * rec / (prec + rec))
    return round(sum(f1s) / len(f1s), 3) if f1s else None

def pct_agree(preds, trues):
    valid = [(p, t) for p, t in zip(preds, trues) if p is not None]
    return round(sum(1 for p, t in valid if p == t) / len(valid), 3) if valid else None

def time_fn(fn, *args, n_reps=3):
    times = []
    for _ in range(n_reps):
        t0 = time.perf_counter()
        fn(*args)
        times.append(time.perf_counter() - t0)
    return sorted(times)[n_reps // 2]  # median

# ── method implementations ─────────────────────────────────────────────────────

def run_sentimentai(texts):
    """sentimentai-py: our package."""
    sys.path.insert(0, os.path.join(os.path.dirname(__file__),
                                    "../pypackage"))
    import importlib
    sa_mod = importlib.import_module("sentimentai")
    scores = sa_mod.sentiment_score(texts)
    return [to_3class(float(s)) for s in scores]

def run_vader(texts):
    from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
    sia = SentimentIntensityAnalyzer()
    preds = []
    for t in texts:
        c = sia.polarity_scores(t)['compound']
        preds.append(to_3class(c, pos_thr=0.05, neg_thr=-0.05))
    return preds

def run_textblob(texts):
    from textblob import TextBlob
    preds = []
    for t in texts:
        score = TextBlob(t).sentiment.polarity
        preds.append(to_3class(score))
    return preds

def run_hf_pipeline(texts):
    """cardiffnlp/twitter-roberta-base-sentiment-latest via HF pipeline."""
    from transformers import pipeline
    pipe = pipeline("text-classification",
                    model="cardiffnlp/twitter-roberta-base-sentiment-latest",
                    truncation=True, max_length=128, device=-1)
    label_map = {'positive': 'positive', 'neutral': 'neutral', 'negative': 'negative',
                 'POSITIVE': 'positive', 'NEUTRAL': 'neutral', 'NEGATIVE': 'negative',
                 'LABEL_0': 'negative', 'LABEL_1': 'neutral', 'LABEL_2': 'positive'}
    # batch inference
    results = pipe(texts, batch_size=32)
    return [label_map.get(r['label'], 'neutral') for r in results]

# ── main ───────────────────────────────────────────────────────────────────────
if __name__ == '__main__':
    print("Exporting airline tweets from R...")
    load_tweets_from_r()
    texts, labels = read_tweets()
    print(f"Loaded {len(texts)} tweets")

    results = []

    # sentimentai-py
    print("\nBenchmarking sentimentai-py...")
    try:
        preds_sa = run_sentimentai(texts)
        t_sa = time_fn(run_sentimentai, texts, n_reps=3)
        results.append({
            'method': 'sentimentai-py (e5-small)',
            'texts_per_sec': round(len(texts) / t_sa),
            'macro_f1': macro_f1(preds_sa, labels),
            'pct_agree': pct_agree(preds_sa, labels),
            'median_sec': round(t_sa, 2)
        })
        print(f"  {results[-1]['texts_per_sec']} texts/sec, F1={results[-1]['macro_f1']}")
    except Exception as e:
        print(f"  FAILED: {e}")
        results.append({'method': 'sentimentai-py (e5-small)', 'error': str(e)})

    # vaderSentiment
    print("\nBenchmarking vaderSentiment...")
    try:
        preds_vader = run_vader(texts)
        t_vader = time_fn(run_vader, texts, n_reps=3)
        results.append({
            'method': 'vaderSentiment',
            'texts_per_sec': round(len(texts) / t_vader),
            'macro_f1': macro_f1(preds_vader, labels),
            'pct_agree': pct_agree(preds_vader, labels),
            'median_sec': round(t_vader, 3)
        })
        print(f"  {results[-1]['texts_per_sec']} texts/sec, F1={results[-1]['macro_f1']}")
    except Exception as e:
        print(f"  FAILED: {e}")
        results.append({'method': 'vaderSentiment', 'error': str(e)})

    # TextBlob
    print("\nBenchmarking TextBlob...")
    try:
        preds_tb = run_textblob(texts)
        t_tb = time_fn(run_textblob, texts, n_reps=3)
        results.append({
            'method': 'TextBlob',
            'texts_per_sec': round(len(texts) / t_tb),
            'macro_f1': macro_f1(preds_tb, labels),
            'pct_agree': pct_agree(preds_tb, labels),
            'median_sec': round(t_tb, 3)
        })
        print(f"  {results[-1]['texts_per_sec']} texts/sec, F1={results[-1]['macro_f1']}")
    except Exception as e:
        print(f"  FAILED: {e}")
        results.append({'method': 'TextBlob', 'error': str(e)})

    # HuggingFace pipeline (cardiffnlp twitter-roberta)
    print("\nBenchmarking HuggingFace twitter-roberta...")
    try:
        preds_hf = run_hf_pipeline(texts)
        t_hf = time_fn(run_hf_pipeline, texts, n_reps=1)
        results.append({
            'method': 'HF twitter-roberta (fine-tuned)',
            'texts_per_sec': round(len(texts) / t_hf),
            'macro_f1': macro_f1(preds_hf, labels),
            'pct_agree': pct_agree(preds_hf, labels),
            'median_sec': round(t_hf, 2)
        })
        print(f"  {results[-1]['texts_per_sec']} texts/sec, F1={results[-1]['macro_f1']}")
    except Exception as e:
        print(f"  FAILED: {e}")
        results.append({'method': 'HF twitter-roberta (fine-tuned)', 'error': str(e)})

    out = os.path.join(os.path.dirname(__file__), "benchmark_python_results.json")
    with open(out, 'w') as f:
        json.dump(results, f, indent=2)
    print(f"\nSaved to {out}")
    for r in results:
        if 'error' not in r:
            print(f"  {r['method']:40s} {r['texts_per_sec']:>6} t/s  F1={r['macro_f1']}")
