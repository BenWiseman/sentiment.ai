# get_embedder.py  (sentiment.ai v2)
#
# Canonical embedder factory, sourced from R via reticulate::source_python().
#
# Two embedding back-ends live here:
#   * load_st_embedder(hf_id, prefix) -- sentence-transformers path (e5 family
#     and any other HuggingFace SentenceTransformer model). This is the v2
#     default and runs entirely on-device (CPU/GPU) with no TensorFlow.
#   * load_hub_embedder(tfhub_url, cache_dir) -- LEGACY TF-Hub path for the
#     old Universal Sentence Encoder models. TensorFlow / tensorflow_hub are
#     imported lazily *inside* the function so they never load for v2 users.
#
# OpenAI embedding deliberately is NOT implemented here -- it lives in R
# (R/embedding.R::load_openai_embedding) and is wired up by init_and_install.R.
#
# IMPORTANT: USE_TF / USE_TORCH are forced at import time so that the
# HuggingFace `transformers` library (pulled in by sentence-transformers)
# never tries to import its TensorFlow backend. On machines with a broken /
# NumPy-2-incompatible TF build that import otherwise crashes the whole
# interpreter, taking sentence-transformers down with it.

import os

os.environ["USE_TF"] = "0"      # transformers: do not import the TF backend
os.environ["USE_TORCH"] = "1"   # transformers: use the PyTorch backend


# ---------------------------------------------------------------------------
# v2 default: sentence-transformers (e5 and friends)
# ---------------------------------------------------------------------------
def load_st_embedder(hf_id, prefix="", revision=None):
    """
    Build a sentence-transformers embedding callable.

    Parameters
    ----------
    hf_id : str
        HuggingFace model id, e.g. "intfloat/multilingual-e5-small".
    prefix : str
        Literal string prepended to every input before encoding. The e5
        family expects "query: " (note the trailing space); pass "" for
        models that need no prefix. R supplies this from model_prefix.
    revision : str or None
        Immutable HuggingFace commit SHA to pin (R supplies it from
        model_revision). When given, the model is resolved to exactly this
        commit instead of a moving ``main`` -- this is what makes the
        downloaded weights auditable/reproducible. ``None`` -> ``main``.

    Returns
    -------
    callable
        f(text) -> numpy.ndarray of shape (n_rows, dim), L2-normalised
        float32. Accepts a single string or a list/iterable of strings;
        a single string is treated as one row.
    """
    from sentence_transformers import SentenceTransformer

    # Device is chosen automatically by sentence-transformers (CUDA / MPS / CPU).
    # `revision` pins the exact commit (auditable); None falls back to main.
    model = SentenceTransformer(hf_id, revision=revision)

    pre = prefix if prefix is not None else ""

    def embed(text):
        # Normalise input to a list of strings so the output is always 2-D.
        if isinstance(text, str):
            inputs = [text]
        else:
            inputs = list(text)

        if pre:
            inputs = [pre + str(t) for t in inputs]
        else:
            inputs = [str(t) for t in inputs]

        return model.encode(
            inputs,
            normalize_embeddings=True,
            convert_to_numpy=True,
        )

    return embed


# ---------------------------------------------------------------------------
# LEGACY: TensorFlow Hub (Universal Sentence Encoder)
# ---------------------------------------------------------------------------
def load_hub_embedder(
    tfhub_url="https://tfhub.dev/google/universal-sentence-encoder-multilingual-large/3",
    cache_dir=None,
):
    """
    Load a TensorFlow Hub embedding model (legacy USE path).

    tensorflow / tensorflow_hub are imported here, lazily, so that users on
    the v2 sentence-transformers path never trigger a TensorFlow import.

    Parameters
    ----------
    tfhub_url : str
        Full tfhub.dev URL for the model (R passes the resolved path from
        legacy_models in constants.R).
    cache_dir : str or None
        If given, models are cached here (TFHUB_CACHE_DIR) rather than /tmp.

    Returns
    -------
    The loaded TF-Hub model. It is directly callable as f(list_of_text) and
    returns a tensor / array of shape (n_rows, dim).
    """
    import ssl

    # tensorflow_text registers ops some USE models need at load time; it is
    # optional, so only warn (do not fail) when it is unavailable.
    try:
        import tensorflow_text as _text  # noqa: F401
    except ImportError:
        print("tensorflow_text not available; some TF-Hub models may fail.")

    import certifi
    from tensorflow_hub import load as hub_load

    os.environ["SSL_CERT_FILE"] = certifi.where()
    print(ssl.get_default_verify_paths())

    # macOS sometimes blocks the tfhub.dev download on cert verification;
    # fall back to an unverified context so the user is not stuck.
    ssl._create_default_https_context = ssl._create_unverified_context

    if cache_dir is not None:
        print("Setting local TFHUB cache dir:")
        os.environ["TFHUB_CACHE_DIR"] = cache_dir
        print(os.environ["TFHUB_CACHE_DIR"])

    return hub_load(tfhub_url)


# ---------------------------------------------------------------------------
# Backwards-compatibility alias.
#
# init_and_install.R (main-thread owned) still calls load_hub_embedding(...)
# for the legacy path. Keep that name working so the legacy USE path does not
# break while the canonical contract name is load_hub_embedder.
# ---------------------------------------------------------------------------
load_hub_embedding = load_hub_embedder
