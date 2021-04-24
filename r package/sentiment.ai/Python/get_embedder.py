import tensorflow_text
from tensorflow_hub import load as hub_load

# LOAD embedding
def load_language_model(model = "multilingual"):
    
    if model.lower() in ["multilingual", "multi"]: 
        hub_path = "https://tfhub.dev/google/universal-sentence-encoder-multilingual-large/3"
        
    if model.lower() in ["english", "en"]:
        hub_path = "https://tfhub.dev/google/universal-sentence-encoder-large/5"
    
    print("loading model from {}".format(hub_path))
    
    return hub_load(hub_path)

