import ssl
import openai
import os
import re
import requests

try:
    import tensorflow_text as text
except ImportError:
    print("TensorFlow Text is not available. Are you using Apple Silicone? Proceeding without it.")
from tensorflow_hub import load as hub_load

# LOAD embedding from tfhub
def load_hub_embedding(hub_model = "https://tfhub.dev/google/universal-sentence-encoder-multilingual-large/3", 
                        cache_dir = None):
    '''
    TF HUB models
    Load model from tensorflow hub. Added shortcuts for multilingual and english. 
    Designed for Google Universal Sentence Encoder
    Other Embedding models may require specific pre processing.
    
    cache_dir - should be in hub_load. Need to specify where to save models (not in temp!)
    '''
    
    # Sometimes OSX will be a bastard with downlaods. Hack/workaround to save user some hassle!
    ssl._create_default_https_context = ssl._create_unverified_context
    
    # if cache_dir is provided, set specific caching folder!
    if cache_dir is not load_hub_embedding.__defaults__[1]:
        print("Setting Local cache dir:")
        os.environ["TFHUB_CACHE_DIR"] = cache_dir
        print(os.environ["TFHUB_CACHE_DIR"])
    
    return hub_load(hub_model)




"""
# Use openAI for embedding
def load_openai_embedding(model_name, api_key, api_base="https://api.openai.com", api_version="v1", api_type=None, api_engine="text-davinci-002"):
    def embed(text):
        openai.api_key = api_key
        openai.api_base = api_base
        
        print("\n")
        print(api_base)
        print(api_version)
        print(api_type)
        print(api_engine)
        print(model_name)
        
        print("Type of api_key:", type(api_key))
        print("Type of api_base:", type(api_base))
        print("Type of api_version:", type(api_version))
        print("Type of api_type:", type(api_type))
        print("Type of api_engine:", type(api_engine))
        print("Type of model_name:", type(model_name))
        print("Type of text:", type(text))
        
        print("Open AI version:", openai.__version__)

        # for azure
        if api_version is not None:
          openai.api_version = api_version  # Set the API version only if it's not None
                  
        if api_type is not None:
          openai.api_type = api_type  # Set the API version only if it's not None
        
        
        try:
            response = openai.Embedding.create(
                model=model_name,
                engine=api_engine,
                input=text
            )
            embedding = response['data'][0]['embedding']
            return embedding
        except Exception as e:
            raise Exception(f"OpenAI API call failed: {str(e)}")
          
          
        try:
            headers = {
                'Authorization': f'Bearer {api_key}',
                'OpenAI-Version': api_version
            }
            
            data = {
                'model': model_name,
                'engine': api_engine,
                'input': text
            }
            
            response = requests.post(f"{api_base}/v1/engines/{api_engine}/completions", headers=headers, json=data)
            
            if response.status_code == 200:
                embedding = response.json()['choices'][0]['text']
                return embedding
            else:
                raise Exception(f"API call failed: {response.json()}")
        except Exception as e:
            raise Exception(f"API call failed: {str(e)}")



    return embed
  
"""

"""
Nope - exporting to python made it not work with pbapply. 

import requests

def load_openai_embedding(model_name, api_key, api_base="https://api.openai.com", api_version="v1", api_type=None, api_engine=None):
    def embed(text):
        # Debugging prints
        print(f"API Base: {api_base}")
        print(f"API Version: {api_version}")
        print(f"API Type: {api_type}")
        print(f"API Engine: {api_engine}")
        print(f"Text inside embed(): {text}")
        
        headers = {
            'Authorization': f'Bearer {api_key}',
            'Content-Type': 'application/json'
        }
        
        data = {
            'input': text,
            'model': model_name
        }
        
        url = f"{api_base}/{api_version}/embeddings"
        print(f"Constructed URL: {url}")  # Debugging print
        
        try:
            response = requests.post(url, headers=headers, json=data)
            if response.status_code == 200:
                embedding = response.json()['data'][0]['embedding']
                return embedding
            else:
                raise Exception(f"API call failed: {response.json()}")
        except Exception as e:
            raise Exception(f"API call failed: {str(e)}")
            
    return embed

 
# Example usage via reticulate in R would be something like:
# py_run_string("import get_openai_embedding from your_python_script")
# py$get_openai_embedding("your text", "text-embedding-ada-002", "your_api_key")
"""
