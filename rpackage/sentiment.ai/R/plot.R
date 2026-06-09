# Interactive sentiment map -- the R twin of the Python package's plot_sentiment().
# Every comment is embedded, projected to 2-D, and drawn as a point coloured by its
# sentiment, with the full text on hover and human-readable cluster labels. It reuses the
# embedding the scorer already computes, so the marginal cost is a projection + a
# clustering. plotly is the only hard plotting dependency (Suggested); uwot / Rtsne are
# optional better projections, and an OpenAI key buys tidier cluster labels.

# A compact English stop-word list for the deterministic c-TF-IDF cluster labels.
.sentiment_stopwords <- c(
  "the","a","an","and","or","but","if","then","so","of","to","in","on","for","with",
  "at","by","from","up","down","out","over","under","again","is","are","was","were",
  "be","been","being","have","has","had","do","does","did","this","that","these",
  "those","i","you","he","she","it","we","they","me","him","her","us","them","my",
  "your","his","its","our","their","as","not","no","very","just","too","also","can",
  "will","would","could","should","there","here","what","which","who","when","where",
  "how","all","any","some","more","most","than","into","about","after","before")

# Deterministic class-based TF-IDF labels (the BERTopic c-TF-IDF idea): one pseudo-doc per
# cluster, weight = term-freq-in-cluster * log(1 + k/df). Returns a character vector of
# length k. Pure base R -- no extra dependency.
.ctfidf_labels <- function(texts, clusters, k, max_terms = 3L){
  out <- paste("cluster", seq_len(k))
  tok <- function(s){
    w <- unlist(strsplit(tolower(s), "[^a-z]+"))
    w <- w[nchar(w) >= 2L & !(w %in% .sentiment_stopwords)]
    w
  }
  # term counts per cluster
  per <- lapply(seq_len(k) - 1L, function(c) table(tok(paste(texts[clusters == c],
                                                            collapse = " "))))
  vocab <- sort(unique(unlist(lapply(per, names))))
  if(length(vocab) == 0L) return(out)
  M <- matrix(0, nrow = k, ncol = length(vocab), dimnames = list(NULL, vocab))
  for(c in seq_len(k)){
    tc <- per[[c]]
    if(length(tc)) M[c, names(tc)] <- as.numeric(tc)
  }
  tf  <- M / pmax(rowSums(M), 1)
  df  <- colSums(M > 0)
  idf <- log(1 + k / pmax(df, 1))
  w   <- sweep(tf, 2, idf, `*`)
  for(c in seq_len(k)){
    ord   <- order(w[c, ], decreasing = TRUE)
    terms <- vocab[ord][w[c, ord] > 0][seq_len(max_terms)]
    terms <- terms[!is.na(terms)]
    if(length(terms)) out[c] <- paste(terms, collapse = " \u00b7 ")
  }
  out
}

# A 2-4 word human label per cluster from gpt-4o-mini (~a fraction of a cent). One batched
# call; on ANY failure we keep the deterministic c-TF-IDF labels (best-effort upgrade).
.openai_labels <- function(texts, clusters, k, tfidf, api_key, model){
  key <- api_key %||% Sys.getenv("OPENAI_API_KEY")
  if(!nzchar(key) || !requireNamespace("httr", quietly = TRUE) ||
     !requireNamespace("jsonlite", quietly = TRUE)) return(tfidf)
  blocks <- vapply(seq_len(k), function(c){
    samp <- utils::head(texts[clusters == (c - 1L)], 3L)
    samp <- substr(samp, 1, 160)
    sprintf("Cluster %d: key terms = %s; examples = %s", c - 1L, tfidf[c],
            paste(samp, collapse = " | "))
  }, character(1))
  prompt <- paste0(
    "Label each cluster of texts with a concise 2-4 word human-readable topic. ",
    "Return STRICT JSON: {\"labels\": {\"0\": \"...\", \"1\": \"...\"}} with one entry ",
    "per cluster index.\n\n", paste(blocks, collapse = "\n"))
  res <- tryCatch({
    r <- httr::POST(
      "https://api.openai.com/v1/chat/completions",
      httr::add_headers(Authorization = paste("Bearer", key)),
      body = list(model = model, temperature = 0,
                  response_format = list(type = "json_object"),
                  messages = list(
                    list(role = "system", content = "You name clusters of short texts."),
                    list(role = "user", content = prompt))),
      encode = "json", httr::timeout(30))
    if(httr::status_code(r) != 200) stop("bad status")
    txt <- httr::content(r, "parsed")$choices[[1]]$message$content
    jsonlite::fromJSON(txt)$labels
  }, error = function(e) NULL)
  if(is.null(res)) return(tfidf)
  out <- tfidf
  for(c in seq_len(k)){
    v <- res[[as.character(c - 1L)]]
    if(!is.null(v) && is.character(v) && nzchar(v)) out[c] <- v
  }
  out
}

# 2-D projection: returns list(xy = matrix(n,2), method = label). UMAP (uwot) if available,
# else t-SNE (Rtsne), else PCA (always available). Mirrors the Python "auto" order.
.reduce_2d <- function(emb, reducer, seed){
  n <- nrow(emb)
  if(n < 3L || ncol(emb) == 2L){
    xy <- if(ncol(emb) >= 2L) emb[, 1:2, drop = FALSE] else cbind(emb[, 1], 0)
    return(list(xy = xy, method = "raw"))
  }
  want <- reducer
  if(want == "auto")
    want <- if(requireNamespace("uwot", quietly = TRUE)) "umap"
            else if(requireNamespace("Rtsne", quietly = TRUE)) "tsne" else "pca"

  if(want == "umap" && requireNamespace("uwot", quietly = TRUE)){
    set.seed(seed)
    xy <- uwot::umap(emb, n_components = 2, n_neighbors = min(15L, max(2L, n - 1L)))
    return(list(xy = xy, method = "UMAP"))
  }
  if(want == "tsne" && requireNamespace("Rtsne", quietly = TRUE)){
    set.seed(seed)
    perp <- min(30, max(2, (n - 1) / 3))
    xy <- Rtsne::Rtsne(emb, dims = 2, perplexity = perp,
                       check_duplicates = FALSE)$Y
    return(list(xy = xy, method = "t-SNE"))
  }
  xy <- stats::prcomp(emb, center = TRUE, scale. = FALSE)$x[, 1:2, drop = FALSE]
  list(xy = xy, method = "PCA")
}

# KMeans on the EMBEDDING (not the 2-D projection) for stable, meaningful groups.
.cluster_emb <- function(emb, n_clusters, seed){
  n <- nrow(emb)
  if(n < 3L) return(rep(0L, n))
  k <- if(identical(n_clusters, "auto") || is.null(n_clusters))
         max(2L, min(12L, n - 1L, round(sqrt(n / 2))))
       else min(as.integer(n_clusters), n)
  set.seed(seed)
  stats::kmeans(emb, centers = k, nstart = 10)$cluster - 1L   # 0-based to match labels
}

.wrap_hover <- function(text, row){
  body <- paste(strwrap(as.character(text), width = 64), collapse = "<br>")
  if(!nzchar(body)) body <- "(empty)"
  head <- sprintf("<b>%s</b> \u00b7 sentiment %+.2f",
                  row$class %||% "?", row$sentiment %||% NA_real_)
  extra <- character(0)
  if(isTRUE(row$hate_speech)) extra <- c(extra, "\u2691 hate speech")
  if(isTRUE(row$mixed))       extra <- c(extra, "mixed")
  if(!is.null(row$style) && !is.na(row$style)) extra <- c(extra, paste0("style: ", row$style))
  tail <- if(length(extra)) paste0("<br><i>", paste(extra, collapse = " \u00b7 "), "</i>") else ""
  paste0(head, "<br>", body, tail)
}

# small null-coalesce helper (kept local so it doesn't depend on roperators' export)
`%||%` <- function(a, b) if(is.null(a) || length(a) == 0L || (length(a) == 1L && is.na(a))) b else a

#' Interactive sentiment map
#'
#' @description Plot an interactive semantic map of \code{x} coloured by sentiment. Each
#' point is one text, positioned by a 2-D projection of its embedding, coloured by
#' sentiment, labelled on hover with its full text + class + any hate/mixed/style flags,
#' and grouped into auto-labelled clusters. The R twin of the Python package's
#' \code{plot_sentiment()}.
#'
#' @details
#' Honest by design: the geometry is a \emph{projection} (UMAP via \pkg{uwot} if installed,
#' else t-SNE via \pkg{Rtsne}, else PCA) -- explore with it, do not measure off it; the
#' axis titles name the method used. Cluster labels are deterministic c-TF-IDF by default
#' (\code{labels = "tfidf"}); \code{labels = "openai"} spends a fraction of a cent on
#' gpt-4o-mini for a tidier 2-4 word topic and silently falls back to c-TF-IDF on failure.
#'
#' \pkg{plotly} is required (Suggested); \pkg{uwot} and \pkg{Rtsne} are optional.
#'
#' @inheritParams sentiment_score
#' @param x Character vector of texts to map. (Required.)
#' @param embeddings Optional precomputed \code{(n, dim)} embedding matrix -- skips the
#'   embed step (and lets the plot run fully offline).
#' @param rows Optional precomputed \code{\link{sentiment}()} data.frame to reuse for the
#'   colour / flags; if omitted it is computed from the embeddings.
#' @param reducer One of \code{"auto"}, \code{"umap"}, \code{"tsne"}, \code{"pca"}.
#' @param n_clusters Number of clusters for the labels, or \code{"auto"}
#'   (\eqn{\approx \sqrt{n/2}}, clamped to \code{[2, 12]}).
#' @param labels One of \code{"tfidf"} (deterministic), \code{"openai"} (needs a key), or
#'   \code{"none"}.
#' @param color_by One of \code{"sentiment"} (default), \code{"class"}, \code{"style"},
#'   \code{"hate"}.
#' @param api_key Optional OpenAI key for \code{labels = "openai"}.
#' @param openai_model OpenAI model for the labels (default \code{"gpt-4o-mini"}).
#' @param max_label_terms Max terms in a c-TF-IDF label (default 3).
#' @param title,width,height,point_size,seed Cosmetic / reproducibility controls.
#' @param return_data If \code{TRUE}, return \code{list(plot, data)} where \code{data} is
#'   the underlying data.frame (coordinates, scores, clusters, labels).
#'
#' @return A \pkg{plotly} htmlwidget (or \code{list(plot, data)} if \code{return_data}).
#' @examples
#' \dontrun{
#'   init_sentiment.ai(model = "e5-small")
#'   p <- plot_sentiment(airline_tweets$text)
#'   p                                   # interactive in the viewer
#'   htmlwidgets::saveWidget(p, "map.html")
#' }
#' @export
plot_sentiment <- function(x,
                           model       = DEFAULT_MODEL,
                           embeddings  = NULL,
                           rows        = NULL,
                           reducer     = c("auto", "umap", "tsne", "pca"),
                           n_clusters  = "auto",
                           labels      = c("tfidf", "openai", "none"),
                           color_by    = c("sentiment", "class", "style", "hate"),
                           api_key     = NULL,
                           openai_model = "gpt-4o-mini",
                           max_label_terms = 3L,
                           title       = "Sentiment map",
                           width       = 900,
                           height      = 640,
                           point_size  = 8,
                           seed        = 0,
                           return_data = FALSE,
                           batch_size  = 100,
                           ...){
  model    <- model[1]
  reducer  <- match.arg(reducer)
  labels   <- match.arg(labels)
  color_by <- match.arg(color_by)
  if(!requireNamespace("plotly", quietly = TRUE))
    stop("plot_sentiment() needs the 'plotly' package. install.packages(\"plotly\").",
         call. = FALSE)

  x    <- as.character(x)
  keep <- which(!is.na(x) & nzchar(trimws(x)))
  if(length(keep) < 2L)
    stop("plot_sentiment() needs at least 2 non-empty texts.", call. = FALSE)
  if(length(keep) < length(x))
    warning("plot_sentiment(): dropped ", length(x) - length(keep),
            " empty/missing text(s).", call. = FALSE)
  xk <- x[keep]

  # --- embeddings (geometry) -------------------------------------------------
  if(!is.null(embeddings)){
    emb <- as.matrix(embeddings)[keep, , drop = FALSE]
  } else {
    if(model_class(model) == "classifier")
      stop("plot_sentiment() needs an embedding space to position points; '", model,
           "' is an end-to-end classifier. Use an e5/openai model or pass embeddings=.",
           call. = FALSE)
    check_sentiment.ai(model = model, ...)
    emb <- embed_text(xk, batch_size, model)
  }
  rownames(emb) <- xk

  # --- sentiment + flags (colour / hover): reuse the tested matrix path ------
  if(!is.null(rows)){
    sdf <- rows[keep, , drop = FALSE]
  } else {
    sdf <- sentiment(emb, model = model)         # matrix path -> full tidy df incl. flags
  }

  # --- project + cluster + label ---------------------------------------------
  red      <- .reduce_2d(emb, reducer, seed)
  clusters <- .cluster_emb(emb, n_clusters, seed)
  k        <- max(clusters) + 1L
  lab <- if(labels == "none") paste("cluster", seq_len(k))
         else .ctfidf_labels(xk, clusters, k, max_label_terms)
  if(labels == "openai")
    lab <- .openai_labels(xk, clusters, k, lab, api_key, openai_model)

  df <- data.frame(
    x = red$xy[, 1], y = red$xy[, 2], text = xk,
    sentiment = sdf$sentiment, class = as.character(sdf$class),
    cluster = clusters, cluster_label = lab[clusters + 1L],
    hate_speech = if(!is.null(sdf$hate_speech)) sdf$hate_speech else FALSE,
    mixed       = if(!is.null(sdf$mixed)) sdf$mixed else FALSE,
    style       = if(!is.null(sdf$style)) sdf$style else NA_character_,
    stringsAsFactors = FALSE)
  df$hover <- vapply(seq_len(nrow(df)), function(i) .wrap_hover(df$text[i], df[i, ]),
                     character(1))

  # --- figure ----------------------------------------------------------------
  if(color_by == "sentiment"){
    fig <- plotly::plot_ly(
      df, x = ~x, y = ~y, type = "scatter", mode = "markers",
      width = width, height = height,
      text = ~hover, hoverinfo = "text",
      marker = list(
        size = point_size, color = ~sentiment,
        colorscale = list(list(0, "#d62728"), list(0.5, "#c9ccd1"), list(1, "#2ca02c")),
        cmin = -1, cmax = 1,
        line = list(width = 0.5, color = "rgba(40,40,40,0.35)"),
        colorbar = list(title = "sentiment", tickvals = c(-1, 0, 1),
                        ticktext = c("neg", "neu", "pos"))))
  } else {
    keycol <- switch(color_by, class = "class", style = "style", hate = "hate_speech")
    fig <- plotly::plot_ly(
      df, x = ~x, y = ~y, type = "scatter", mode = "markers",
      width = width, height = height,
      color = stats::as.formula(paste0("~", keycol)),
      text = ~hover, hoverinfo = "text",
      marker = list(size = point_size,
                    line = list(width = 0.5, color = "rgba(40,40,40,0.35)")))
  }

  # cluster labels at centroids
  for(c in seq_len(k) - 1L){
    sub <- df[df$cluster == c, ]
    if(!nrow(sub)) next
    fig <- plotly::add_annotations(
      fig, x = mean(sub$x), y = mean(sub$y),
      text = paste0("<b>", lab[c + 1L], "</b>"), showarrow = FALSE,
      font = list(size = 11, color = "#1a1a1a"),
      bgcolor = "rgba(255,255,255,0.72)", bordercolor = "rgba(0,0,0,0.18)", borderpad = 2)
  }

  fig <- plotly::layout(
    fig, title = list(text = title, x = 0.5, xanchor = "center"),
    xaxis = list(title = paste0("dim 1 (", red$method, " projection)"),
                 showgrid = FALSE, zeroline = FALSE),
    yaxis = list(title = paste0("dim 2 (", red$method, " projection)"),
                 showgrid = FALSE, zeroline = FALSE),
    hoverlabel = list(align = "left", bgcolor = "white"),
    margin = list(l = 40, r = 20, t = 50, b = 40))

  if(return_data) list(plot = fig, data = df) else fig
}
