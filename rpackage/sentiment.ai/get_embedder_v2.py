import os
import json
import requests
import argparse
import logging

def initiate_model(model_name, cache_dir=None):
    """
    Initializes and returns the embedding model based on the specified model_name.

    Args:
        model_name (str): The name or identifier of the model to initialize.
        cache_dir (str, optional): The directory to cache models for tfhub and sentence_transformers.

    Returns:
        dict: A dictionary containing the model type and the initialized model or necessary credentials.

    Raises:
        ValueError: If an unsupported model_name is provided.
    """
    # Define OpenAI models with short names mapping to full model identifiers
    openai_models = {
        "ada": "text-embedding-ada-002",
        "oai_3_small": "text-embedding-3-small",
        "oai_3_large": "text-embedding-3-large"
    }

    # Define USE models
    use_models = {
        "use": "universal-sentence-encoder/4",
        "en.large": "universal-sentence-encoder-large/5",
        "en": "universal-sentence-encoder/4",
        "multi.large": "universal-sentence-encoder-multilingual-large/3",
        "multi": "universal-sentence-encoder-multilingual/3"
    }

    # Define Paraphrase models from Sentence Transformers
    sentence_transformers_models = {
        "paraphrase_miniLM": "paraphrase-MiniLM-L6-v2",
        "mpnet": "all-mpnet-base-v2",
        "distilroberta": "all-distilroberta-v1",
        "instructorXL": "instructor-xl"  # Add InstructorXL
    }

    # Check if the model_name is a key or value in the model dictionaries
    if model_name in openai_models:
        # User provided short name
        selected_model_id = openai_models[model_name]
        logging.info(f"Selected OpenAI model: {selected_model_id}")
    elif model_name in openai_models.values():
        # User provided full model identifier
        selected_model_id = model_name
        logging.info(f"Selected OpenAI model: {selected_model_id}")
    elif model_name in use_models:
        # User provided USE short name
        selected_model_id = use_models[model_name]
        logging.info(f"Selected USE model: {selected_model_id}")
    elif model_name in use_models.values():
        # User provided full USE model identifier
        selected_model_id = model_name
        logging.info(f"Selected USE model: {selected_model_id}")
    elif model_name in sentence_transformers_models:
        # User provided Paraphrase short name
        selected_model_id = sentence_transformers_models[model_name]
        logging.info(f"Selected Paraphrase model: {selected_model_id}")
    elif model_name in sentence_transformers_models.values():
        # User provided full Paraphrase model identifier
        selected_model_id = model_name
        logging.info(f"Selected Paraphrase model: {selected_model_id}")
    else:
        raise ValueError(f"Unsupported model '{model_name}'. Choose a valid model name or identifier.")

    # Initialize the model based on its type
    # Determine the type by checking which dictionary contains the model_id
    if selected_model_id in openai_models.values():
        # Initialize OpenAI model
        openai_api_key = os.getenv('OPENAI_API_KEY')
        if not openai_api_key:
            raise ValueError("OpenAI API key not found. Please set the 'OPENAI_API_KEY' environment variable.")

        api_url = "https://api.openai.com/v1/embeddings"
        headers = {
            "Content-Type": "application/json",
            "Authorization": f"Bearer {openai_api_key}"
        }
        logging.info(f"Initialized OpenAI model: {selected_model_id}")
        return {"type": "openai", "model_id": selected_model_id, "api_url": api_url, "headers": headers}

    elif selected_model_id in use_models.values():
        # Initialize USE model
        try:
            if cache_dir:
                os.environ['TFHUB_CACHE_DIR'] = cache_dir
                logging.info(f"Set TFHUB_CACHE_DIR to {cache_dir}")

            import tensorflow as tf
            import tensorflow_hub as hub
            try:
                import tensorflow_text  # Needed for some USE models
            except ImportError as e:
                logging.error("Failed to import tensorflow_text. Please install tensorflow_text.")
                raise e

            logging.info(f"Loading Universal Sentence Encoder model: {selected_model_id}...")
            use_model = hub.load(f"https://tfhub.dev/google/{selected_model_id}")
            logging.info("Universal Sentence Encoder model loaded successfully.")
            return {"type": "use", "model": use_model}
        except Exception as e:
            logging.error(f"Failed to load USE model '{selected_model_id}': {e}")
            raise

    elif selected_model_id in sentence_transformers_models.values():
        # Initialize Paraphrase model
        try:
            if cache_dir:
                os.environ['TRANSFORMERS_CACHE'] = cache_dir
                logging.info(f"Set TRANSFORMERS_CACHE to {cache_dir}")

            from sentence_transformers import SentenceTransformer
            logging.info(f"Loading Paraphrase model: {selected_model_id}...")
            paraphrase_model = SentenceTransformer(selected_model_id, cache_folder=cache_dir)
            logging.info("Paraphrase model loaded successfully.")
            return {"type": "paraphrase", "model": paraphrase_model}
        except Exception as e:
            logging.error(f"Failed to load Paraphrase model '{selected_model_id}': {e}")
            raise
    else:
        raise ValueError(f"Model '{model_name}' could not be categorized into OpenAI, USE, or Paraphrase models.")

def embed_text(text, model_info):
    """
    Embeds the given text using the specified model.

    Args:
        text (str): The text to embed.
        model_info (dict): The initialized model information returned by initiate_model.

    Returns:
        list: The embedding vector.

    Raises:
        Exception: If embedding fails.
    """
    model_type = model_info["type"]

    if model_type == "openai":
        try:
            payload = {
                "input": text,
                "model": model_info["model_id"]
            }
            response = requests.post(model_info["api_url"], headers=model_info["headers"], json=payload)
            if response.status_code == 200:
                embedding = response.json()['data'][0]['embedding']
                return embedding
            else:
                logging.error(f"OpenAI API request failed with status {response.status_code}: {response.text}")
                raise Exception(f"OpenAI API request failed: {response.text}")
        except Exception as e:
            logging.error(f"Error embedding text with OpenAI model '{model_info['model_id']}': {e}")
            raise

    elif model_type == "use":
        try:
            # Ensure necessary imports
            import tensorflow as tf
            import numpy as np

            embedding = model_info["model"]([text])[0].numpy().tolist()
            return embedding
        except Exception as e:
            logging.error(f"Error embedding text with USE model: {e}")
            raise

    elif model_type == "paraphrase":
        try:
            embedding = model_info["model"].encode(text).tolist()
            return embedding
        except Exception as e:
            logging.error(f"Error embedding text with Paraphrase model: {e}")
            raise

    else:
        raise ValueError(f"Unsupported model type '{model_type}'.")
