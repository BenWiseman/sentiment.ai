import tensorflow_text
from tensorflow_hub import load as hub_load

# LOAD embedding
def load_language_model(hub_path = "https://tfhub.dev/google/universal-sentence-encoder-multilingual-large/3"):
    return hub_load(hub_path)
