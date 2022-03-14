# sentiment.ai 0.1.1


Patch for case when no conda binary is presen

updated default python to 3.8.10 for virtualenv and conda compatibility

* Fixed typos. Note that `get_default_embedding` had a typo in the initial launch and was called `get_defualt_embedding`. Make sure to update your code if you used this function.


# sentiment.ai 0.1.0

INITIAL RELEASE 
see [github page](https://benwiseman.github.io/sentiment.ai/) for details 

[Korn Ferry Institute](https://www.kornferry.com/institute)'s AITMI team made `sentiment.ai` for researchers and tinkerers who want a straight-forward way to
use powerful, open source deep learning models to improve their sentiment analyses. Our approach is relatively simple and out performs the current best offerings on CRAN and even Microsoft's Azure Cognitive Services. Given that we felt the current norm for sentiment analysis isn't quite good enough, we decided to open-source our simplified interface to turn Universal Sentence Encoder embedding vectors into sentiment scores. 

We've wrapped a lot of the underlying hassle up to make the process as simple as possible. In addition to just being cool, this approach solves several problems with traditional sentiment analysis, namely: 

1) **More robust**, can handle spelling mitsakes and mixed case, and can be applied to **dieciséis (16) languages**! 

2) **Doesn't need a ridged lexicon**, rather it matches to an embedding vector (reduces language to a vector of numbers that capture the information, kind of like a PCA). This means you can get scores for words that are not in the lexicon but are similar to existing words! 

3) **Choose the context** for what negative and positive mean using the `sentiment_match()` function. For example, you could set `positive` to mean `"high quality"` and negative to mean `"low quality"` when looking at product reviews.

4) **Power** Because it draws from language embedding models trained on billions of texts, news articles, and wikipedia entries, it is able to detect things such as *"I learned so much on my trip to Hiroshima museum last year!"* is associated with something positive and that *"What happeded to the people of Hiroshima in 1945"* is associated with something negative.

5) **The power is yours** We've designed `sentiment.ai` such that the community can contribute sentiment models via [github](https://github.com/BenWiseman/sentiment.ai/tree/main/models). This way, it's easier for the community to work together to make sentiment analysis more reliable! 
Currently only xgboost and glms (trained on the 512-D embeddings generated with tensorflow) are supported, however in a future update we will add functionality to allow arbitrary sentiment scoring models. 
