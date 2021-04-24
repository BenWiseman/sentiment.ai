import tensorflow_text
from tensorflow_hub import load as hub_load

# LOAD embedding
def load_language_model(hub_model = "https://tfhub.dev/google/universal-sentence-encoder-multilingual-large/3"):
    '''
    Load model from tensorflow hub. Added shortcuts for multilingual and english. 
    Designed for Google Universal Sentence Encoder
    Other Embedding models may require specific pre processing.
    '''
    return hub_load(hub_model)

