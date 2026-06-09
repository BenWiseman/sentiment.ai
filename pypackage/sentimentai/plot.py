"""Interactive sentiment map.  Mirrors R: plot_sentiment().

The one picture a sentiment toolkit should ship: every comment embedded, projected to
2-D, and drawn as a point coloured by its sentiment, with the full text on hover and
human-readable cluster labels. It reuses the embedding the scorer already computes — the
expensive part — so the marginal cost is a projection and a clustering.

Design choices, all deliberate:
  * The geometry is a *projection* (UMAP if installed, else t-SNE, else PCA) — explore
    with it, don't measure off it. Axis titles say which method was used.
  * Cluster labels are c-TF-IDF (deterministic, computed-not-generated, auditable) by
    default; ``labels="openai"`` spends a fraction of a cent on gpt-4o-mini for a tidier
    2–4 word topic, and silently falls back to c-TF-IDF if the call fails.
  * Plotting deps (plotly, scikit-learn) are an OPTIONAL extra: ``pip install
    sentimentai[plot]``. Importing :mod:`sentimentai` never imports them; they load lazily
    the first time you call :func:`plot_sentiment`.

Returns a plotly ``Figure`` — ``.show()`` it in a notebook, or ``.write_html("map.html")``
/ ``.write_image("map.png")`` to a file.
"""
from __future__ import annotations

import textwrap
from typing import Sequence

import numpy as np

# diverging sentiment colour scale: negative red -> neutral grey -> positive green
_SENTIMENT_SCALE = [[0.0, "#d62728"], [0.5, "#c9ccd1"], [1.0, "#2ca02c"]]
_CLASS_COLOURS = {"negative": "#d62728", "neutral": "#9aa0a6", "positive": "#2ca02c"}


def _need(pkg: str, extra: str = "plot"):
    try:
        return __import__(pkg)
    except ImportError as exc:  # pragma: no cover - env-dependent
        raise ImportError(
            f"plot_sentiment() needs `{pkg}`. Install the optional plotting extra:\n"
            f"    pip install sentimentai[{extra}]\n"
            f"(this keeps the core package tiny for on-device scoring).") from exc


def _reduce_2d(emb: np.ndarray, reducer: str, seed: int) -> tuple[np.ndarray, str]:
    """Project (n, dim) -> (n, 2). Returns the coords and the method actually used."""
    n = emb.shape[0]
    if n < 3 or emb.shape[1] == 2:                       # nothing to gain from a projection
        xy = emb[:, :2] if emb.shape[1] >= 2 else np.column_stack([emb[:, 0], np.zeros(n)])
        return xy.astype(float), "raw"

    want = reducer
    if want == "auto":
        try:
            __import__("umap")
            want = "umap"
        except ImportError:
            want = "tsne"

    if want == "umap":
        try:
            import umap                                   # umap-learn (optional within [plot])
            r = umap.UMAP(n_components=2, random_state=seed,
                          n_neighbors=int(min(15, max(2, n - 1))))
            return np.asarray(r.fit_transform(emb), dtype=float), "UMAP"
        except Exception:                                # not installed / failed -> t-SNE
            want = "tsne"

    if want == "tsne":
        try:
            from sklearn.manifold import TSNE
            perp = float(min(30, max(2, (n - 1) / 3)))
            r = TSNE(n_components=2, random_state=seed, perplexity=perp, init="pca")
            return np.asarray(r.fit_transform(emb), dtype=float), "t-SNE"
        except Exception:
            want = "pca"

    _need("sklearn")
    from sklearn.decomposition import PCA
    r = PCA(n_components=2, random_state=seed)
    return np.asarray(r.fit_transform(emb), dtype=float), "PCA"


def _auto_k(n: int) -> int:
    """A handful of clusters: ~sqrt(n/2), clamped to [2, 12] and < n."""
    return int(max(2, min(12, n - 1, round((n / 2) ** 0.5))))


def _cluster(emb: np.ndarray, n_clusters, seed: int) -> np.ndarray:
    """KMeans on the EMBEDDING (not the 2-D projection) for stable, meaningful groups."""
    n = emb.shape[0]
    if n < 3:
        return np.zeros(n, dtype=int)
    k = _auto_k(n) if n_clusters in (None, "auto") else int(min(n_clusters, n))
    _need("sklearn")
    from sklearn.cluster import KMeans
    return KMeans(n_clusters=k, random_state=seed, n_init=10).fit_predict(emb)


def _ctfidf_labels(texts: list[str], clusters: np.ndarray, k: int,
                   max_terms: int) -> dict[int, str]:
    """Deterministic class-based TF-IDF labels (the BERTopic c-TF-IDF idea).

    One pseudo-document per cluster; weight = term-frequency-in-cluster * log(1 + k/df).
    Top ``max_terms`` terms joined with ' · '. Falls back to 'cluster i' if the cluster's
    vocabulary is all stop-words.
    """
    from sklearn.feature_extraction.text import CountVectorizer
    out = {c: f"cluster {c + 1}" for c in range(k)}
    docs = {c: " ".join(t for t, cl in zip(texts, clusters) if cl == c) for c in range(k)}
    order = [c for c in range(k) if docs[c].strip()]
    if not order:
        return out
    try:
        cv = CountVectorizer(stop_words="english", ngram_range=(1, 2),
                             token_pattern=r"(?u)\b[a-zA-Z][a-zA-Z]+\b")
        X = cv.fit_transform([docs[c] for c in order]).toarray().astype(float)
    except ValueError:                                   # empty vocabulary
        return out
    vocab = np.asarray(cv.get_feature_names_out())
    tf = X / np.clip(X.sum(axis=1, keepdims=True), 1, None)
    df = (X > 0).sum(axis=0)
    idf = np.log(1.0 + X.shape[0] / np.clip(df, 1, None))
    weight = tf * idf
    for row, c in enumerate(order):
        top = np.argsort(weight[row])[::-1][:max_terms]
        terms = [vocab[t] for t in top if weight[row, t] > 0]
        if terms:
            out[c] = " · ".join(terms)
    return out


def _openai_labels(texts: list[str], clusters: np.ndarray, k: int,
                   tfidf: dict[int, str], api_key, model: str) -> dict[int, str]:
    """A 2–4 word human label per cluster from gpt-4o-mini (~a fraction of a cent).

    One batched chat call; on ANY failure we keep the deterministic c-TF-IDF labels, so
    this is a pure best-effort upgrade.
    """
    import json
    import os
    key = api_key or os.environ.get("OPENAI_API_KEY")
    if not key:
        return tfidf
    blocks = []
    for c in range(k):
        samples = [t for t, cl in zip(texts, clusters) if cl == c][:3]
        samples = [textwrap.shorten(s, width=160, placeholder="…") for s in samples]
        blocks.append(f"Cluster {c}: key terms = {tfidf.get(c, '')}; examples = {samples}")
    prompt = (
        "Label each cluster of texts with a concise 2-4 word human-readable topic. "
        "Return STRICT JSON: {\"labels\": {\"0\": \"...\", \"1\": \"...\"}} with one entry "
        "per cluster index.\n\n" + "\n".join(blocks))
    try:
        import requests
        resp = requests.post(
            "https://api.openai.com/v1/chat/completions",
            headers={"Authorization": f"Bearer {key}"},
            json={"model": model, "temperature": 0,
                  "response_format": {"type": "json_object"},
                  "messages": [
                      {"role": "system", "content": "You name clusters of short texts."},
                      {"role": "user", "content": prompt}]},
            timeout=30)
        resp.raise_for_status()
        got = json.loads(resp.json()["choices"][0]["message"]["content"])["labels"]
        out = dict(tfidf)
        for c in range(k):
            v = got.get(str(c)) or got.get(c)
            if isinstance(v, str) and v.strip():
                out[c] = v.strip()
        return out
    except Exception:
        return tfidf


def _hover(text: str, row: dict) -> str:
    body = "<br>".join(textwrap.wrap(str(text), width=64)) or "(empty)"
    head = f"<b>{row.get('class', '?')}</b> · sentiment {row.get('sentiment', float('nan')):+.2f}"
    extra = []
    if row.get("hate_speech"):
        extra.append("⚑ hate speech")
    if row.get("mixed"):
        extra.append("mixed")
    if row.get("style"):
        extra.append(f"style: {row['style']}")
    tail = ("<br><i>" + " · ".join(extra) + "</i>") if extra else ""
    return f"{head}<br>{body}{tail}"


def plot_sentiment(
    texts: "Sequence[str]",
    model: "str | None" = None,
    *,
    embeddings: "np.ndarray | None" = None,
    rows: "list[dict] | None" = None,
    reducer: str = "auto",
    n_clusters="auto",
    labels: str = "tfidf",
    color_by: str = "sentiment",
    api_key: "str | None" = None,
    openai_model: str = "gpt-4o-mini",
    max_label_terms: int = 3,
    title: str = "Sentiment map",
    width: int = 900,
    height: int = 640,
    point_size: int = 8,
    seed: int = 0,
    return_data: bool = False,
    batch_size: int = 100,
    **embed_kwargs,
):
    """Plot an interactive semantic map of ``texts`` coloured by sentiment.

    Each point is one text, positioned by a 2-D projection of its embedding, coloured by
    sentiment, labelled on hover with its full text + class + any hate/mixed/style flags,
    and grouped into auto-labelled clusters.

    Parameters
    ----------
    texts : sequence of str
        The texts to map.
    model : str, optional
        Embedding model (defaults to the configured profile). Must be an embedding model
        (e5/openai) — the end-to-end transformer classifiers have no embedding space, so
        pass an e5 model or precomputed ``embeddings`` instead.
    embeddings : (n, dim) array, optional
        Precomputed embeddings — skips the embed step (and lets the plot run fully offline).
    rows : list of dict, optional
        Precomputed :func:`sentiment` output to reuse for colour/flags (also enables
        offline use). If omitted it is computed from the embeddings.
    reducer : {"auto", "umap", "tsne", "pca"}
        2-D projection. "auto" uses UMAP if installed, else t-SNE, else PCA.
    n_clusters : int or "auto"
        Number of clusters for the labels ("auto" ≈ sqrt(n/2), clamped to [2, 12]).
    labels : {"tfidf", "openai", "none"}
        Cluster labelling. "tfidf" is deterministic c-TF-IDF; "openai" spends ~a cent on
        gpt-4o-mini for a tidier topic (needs an API key; falls back to "tfidf").
    color_by : {"sentiment", "class", "style", "hate"}
        What the point colour encodes.
    return_data : bool
        If True, return ``(figure, pandas.DataFrame)`` with the underlying coordinates,
        scores, clusters and labels.

    Returns
    -------
    plotly.graph_objects.Figure (or ``(Figure, DataFrame)`` if ``return_data``).
    """
    try:
        import plotly.graph_objects as go
    except ImportError as exc:  # pragma: no cover - env-dependent
        raise ImportError(
            "plot_sentiment() needs `plotly`. Install the optional plotting extra:\n"
            "    pip install sentimentai[plot]") from exc
    pd = _need("pandas")

    texts = [texts] if isinstance(texts, str) else list(texts)
    keep = [i for i, t in enumerate(texts)
            if isinstance(t, str) and t.strip()]
    if len(keep) < 2:
        raise ValueError("plot_sentiment needs at least 2 non-empty texts.")
    if len(keep) < len(texts):
        import warnings
        warnings.warn(f"dropped {len(texts) - len(keep)} empty/missing text(s) from the plot.")
    kept_texts = [texts[i] for i in keep]

    # --- embeddings (geometry) -------------------------------------------------
    if embeddings is not None:
        emb = np.asarray(embeddings, dtype=np.float64)[keep]
    else:
        from ._models import resolve
        from ._profiles import get_default_model
        mdl = model or get_default_model()
        if resolve(mdl).kind == "hf-classifier":
            raise ValueError(
                f"plot_sentiment needs an embedding space to position points; {mdl!r} is an "
                f"end-to-end classifier. Use an e5/openai model (e.g. model='e5-base') or "
                f"pass embeddings=.")
        from .embedding import embed_text
        emb = np.asarray(embed_text(kept_texts, model=mdl, batch_size=batch_size,
                                    **embed_kwargs), dtype=np.float64)

    # --- sentiment + flags (colour / hover) ------------------------------------
    if rows is not None:
        srows = [rows[i] for i in keep]
    else:
        from . import _scoring
        from ._profiles import get_default_model
        if model is not None:
            mdl = model
        elif embeddings is not None:                             # infer head from emb width
            mdl = {384: "e5-small", 768: "e5-base", 1536: "openai"}.get(emb.shape[1])
            if mdl is None:
                raise ValueError(
                    f"can't infer the scoring model for {emb.shape[1]}-dim embeddings; "
                    f"pass model= or rows=.")
        else:
            mdl = get_default_model()
        p = _scoring.probs(emb, model=mdl)                       # (n, 3)
        hate_h = _scoring.load_aux("hate", mdl)
        mixed_h = _scoring.load_aux("mixed", mdl)
        style_h = _scoring.load_aux("style", mdl)
        p_hate = _scoring.logistic_binary_prob(emb, hate_h) if hate_h else None
        p_mixed = _scoring.logistic_binary_prob(emb, mixed_h) if mixed_h else None
        style_p = _scoring.multilabel_probs(emb, style_h) if style_h else None
        cls = ("negative", "neutral", "positive")
        srows = []
        for r in range(emb.shape[0]):
            j = int(np.argmax(p[r]))
            d = {"sentiment": float(p[r, 2] - p[r, 0]), "class": cls[j]}
            if hate_h is not None:
                d["hate_speech"] = bool(p_hate[r] >= hate_h["recommended_threshold"])
            if mixed_h is not None:
                d["mixed"] = bool(cls[j] == "neutral"
                                  and p_mixed[r] >= mixed_h["recommended_threshold"])
            if style_h is not None:
                d["style"] = style_h["classes"][int(np.argmax(style_p[r]))]
            srows.append(d)

    sent = np.array([float(r.get("sentiment", np.nan)) for r in srows])

    # --- project + cluster + label ---------------------------------------------
    xy, method = _reduce_2d(emb, reducer, seed)
    clusters = _cluster(emb, n_clusters, seed)
    k = int(clusters.max()) + 1
    if labels == "none":
        lab = {c: f"cluster {c + 1}" for c in range(k)}
    else:
        lab = _ctfidf_labels(kept_texts, clusters, k, max_label_terms)
        if labels == "openai":
            lab = _openai_labels(kept_texts, clusters, k, lab, api_key, openai_model)

    df = pd.DataFrame({
        "x": xy[:, 0], "y": xy[:, 1], "text": kept_texts, "sentiment": sent,
        "class": [r.get("class") for r in srows],
        "cluster": clusters, "cluster_label": [lab[c] for c in clusters],
        "hate_speech": [bool(r.get("hate_speech", False)) for r in srows],
        "mixed": [bool(r.get("mixed", False)) for r in srows],
        "style": [r.get("style") for r in srows],
        "hover": [_hover(t, r) for t, r in zip(kept_texts, srows)],
    })

    # --- figure ----------------------------------------------------------------
    fig = go.Figure()
    if color_by == "sentiment":
        fig.add_trace(go.Scatter(
            x=df.x, y=df.y, mode="markers", text=df.hover, hoverinfo="text",
            marker=dict(size=point_size, color=df.sentiment, colorscale=_SENTIMENT_SCALE,
                        cmin=-1, cmax=1, line=dict(width=0.5, color="rgba(40,40,40,0.35)"),
                        colorbar=dict(title="sentiment", tickvals=[-1, 0, 1],
                                      ticktext=["neg", "neu", "pos"])),
            showlegend=False))
    else:
        if color_by == "class":
            groups = {c: _CLASS_COLOURS.get(c, "#666") for c in df["class"].dropna().unique()}
            keycol = "class"
        elif color_by == "style":
            keycol = "style"
            palette = ["#4c78a8", "#f58518", "#54a24b", "#b279a2", "#e45756", "#72b7b2"]
            uq = [s for s in df["style"].dropna().unique()]
            groups = {s: palette[i % len(palette)] for i, s in enumerate(uq)}
        elif color_by == "hate":
            keycol = "hate_speech"
            groups = {True: "#d62728", False: "#9aa0a6"}
        else:
            raise ValueError(f"unknown color_by={color_by!r}")
        for val, col in groups.items():
            sub = df[df[keycol] == val]
            if not len(sub):
                continue
            fig.add_trace(go.Scatter(
                x=sub.x, y=sub.y, mode="markers", text=sub.hover, hoverinfo="text",
                name=str(val), marker=dict(size=point_size, color=col,
                                           line=dict(width=0.5, color="rgba(40,40,40,0.35)"))))

    # cluster labels at centroids
    for c in range(k):
        sub = df[df.cluster == c]
        if not len(sub):
            continue
        fig.add_annotation(x=sub.x.mean(), y=sub.y.mean(), text=f"<b>{lab[c]}</b>",
                           showarrow=False, font=dict(size=11, color="#1a1a1a"),
                           bgcolor="rgba(255,255,255,0.72)", bordercolor="rgba(0,0,0,0.18)",
                           borderpad=2)

    fig.update_layout(
        title=dict(text=title, x=0.5, xanchor="center"),
        width=width, height=height, template="plotly_white",
        xaxis=dict(title=f"dim 1 ({method} projection)", showgrid=False, zeroline=False),
        yaxis=dict(title=f"dim 2 ({method} projection)", showgrid=False, zeroline=False),
        hoverlabel=dict(align="left", bgcolor="white"),
        margin=dict(l=40, r=20, t=50, b=40))

    return (fig, df) if return_data else fig
