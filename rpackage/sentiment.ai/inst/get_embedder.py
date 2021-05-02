import ssl
import os
import tensorflow_text
from tensorflow_hub import load as hub_load

# LOAD embedding
def load_language_model(hub_model = "https://tfhub.dev/google/universal-sentence-encoder-multilingual-large/3", 
                        cache_dir = None):
    '''
    Load model from tensorflow hub. Added shortcuts for multilingual and english. 
    Designed for Google Universal Sentence Encoder
    Other Embedding models may require specific pre processing.
    
    cache_dir - should be in hub_load. Need to specify where to save models (not in temp!)
    '''
    
    # Sometimes OSX will be a bastard with downlaods. Hack/workaround to save user some hassle!
    ssl._create_default_https_context = ssl._create_unverified_context
    
    # if cache_dir is provided, set specific caching folder!
    if cache_dir is not load_language_model.__defaults__[1]:
        print("Setting Local cache dir:")
        os.environ["TFHUB_CACHE_DIR"] = cache_dir
        print(os.environ["TFHUB_CACHE_DIR"])
    
    return hub_load(hub_model)

