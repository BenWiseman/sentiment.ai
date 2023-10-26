library(parallel)
#' Read embedding file.
#' Take json path, return single embedding object for specific model.
#' @param model character - which model's default embeddings are needed.
#' @param file character - the filepath to the json object.
#' @param version PLACEHOLDER - may be necessary in future.
read_embedding <- function(file, model = "en.large", version = NULL){

  # json matrices can't have rownames so need to:
  # 1) Get the correct embeddings for the given model - saved in json.rownames
  # 2) restore the dim names from the json's rowname and columnnames fields
  # 3) return eme: a list of two embedding matrices with terms as rownames

  x <- jsonlite::fromJSON(readLines(file))

  emb <- x[[model]]
  rownames(emb$positive) <- x$rownames$positive
  colnames(emb$positive) <- x$colnames$positive

  rownames(emb$negative) <- x$rownames$negative
  colnames(emb$negative) <- x$colnames$negative

  return(emb)
}

#' get default embedding
#' If it exists, return the object.
#' If not, try downloading it.
#' If download works, return object.
#' Else return `NULL` (to be handles in `embed_topics()`).
#' @param model character - whic model's default embeddings are needed
get_default_embedding <- function(model){

  emb <- NULL

  # to find the defaults for this version of sentiment.ai
  version  <- utils::packageDescription("sentiment.ai", fields = "Version")
  pkg_path <- system.file(package = "sentiment.ai")
  emb_file <- file.path(pkg_path, "default_embeddings", paste0(version, ".json"))

  emb_exists <- file.exists(emb_file)

  # not there? try downloading it
  if(!emb_exists) emb_exists <- install_default_embeddings() #1 if successful, 0 if fail

  # double check - has downloaded and is where it *should* be!
  if(emb_exists & file.exists(emb_file)) emb <- read_embedding(emb_file, model=model)

  # now return
  return(emb)

}


# Create Pos/Neg Embeddings
embed_topics <- function(phrases = NULL,
                         model   = c("en.large", "multi.large", "en", "multi")){

  # fix global variable declaration for using data.table (to pass CRAN checks)
  phrase <- NULL

  # make vector of repeating phrase labels per entry
  class_to_vec <- function(phrases){
    # make sure phrases has names
    nms   <- names(phrases)

    if(length(nms) == 0) nms <- seq_along(phrases)

    rep(nms, times = lengths(phrases))
  }

  # DEFAULT - now needs to check
  # not supplied phrases & names match default models
  # and default pos/neg embedding has been installed. if not, make phrases pos/neg and embed_text on them
  if(is.null(phrases) && model[1] %in% names(default_models)) {

    default_embeddings <- get_default_embedding(model[1])

    # if NULL then use default phrases
    phrases  <- list(positive = sentiment.ai::default$positive,
                     negative = sentiment.ai::default$negative)

    # if defaults didn't exist, embed them
    if(is.null(default_embeddings)){
      default_embeddings <- lapply(X     = phrases,
                                   FUN   = embed_text,
                                   model = model[1])
    }

    # return and combine pos/neg embeddings
    # rbind - must be matrix!
    mx_embed <- do.call(rbind, default_embeddings)

  } else{
    # NOT DEFAULT, do custom
    if(!length(phrases)) return(NULL)

    custom_embeddings <- lapply(X     = phrases,
                                FUN   = embed_text,
                                model = model[1])

    mx_embed <- do.call(rbind, custom_embeddings)
  }


  lookup  <- data.table(phrase = unname(unlist(phrases)),
                        class  = class_to_vec(phrases),
                        key    = "phrase")

  # de-dupe
  lookup  <- lookup[!duplicated(phrase), ]

  # RETURN
  return(list(embeddings = mx_embed, lookup = lookup))
}

#' internal function to handle tfhub embeddings
hub_embed <- function(text, batch_size = NULL) {

  if(is.null(batch_size)){
    batch_size <- length(text)
  }

  text_size <- length(text)
  batches <- rep(1:ceiling(text_size / batch_size), each = batch_size, length.out = text_size)
  batches <- split(x = seq_along(text), f = batches)

  n_batches <- length(batches)
  do_talk <- n_batches > 1

  if(do_talk){
    message("Model Running ...")
    pb <- utils::txtProgressBar(min = 0, max = n_batches, char = "|", style = 3)
  }

  text_embed <- NULL

  for(this_batch in seq_len(n_batches)){
    this_inds <- batches[[this_batch]]
    this_embed <- sentiment.ai::sentiment.env$embed(as_py_list(text[this_inds]))
    this_embed <- data.table(t(as.matrix(this_embed)))

    if(is.null(text_embed)){
      text_embed <- this_embed
    } else {
      text_embed[, paste0("V", this_inds) := this_embed]
    }

    if(do_talk){
      utils::setTxtProgressBar(pb, this_batch)
    }
  }

  return(text_embed)
}

#' Create Text Embedding Matrix
#'
#' @description Turns a character vector into a `length(text) x length(embedding)` embedding matrix.
#' For power users. Requires `init_sentiment.ai()` to have been called or model and api info to be supplied.
#'
#' @param text character vector to be embedded. Note that longer comments take longer.
#' @param batch_size integer - how many to embed at once. Higher numbers are faster but use more memory.
#' @param model character - the embedding model to use (same as `sentiment_score()`).
#' @param request_limit integer - maximum number of requests per minute for OpenAI models. Default is 60.
#' @param token_limit integer - maximum number of tokens per minute for OpenAI models. Default is 60000.
#' @param api_key character string specifying the API key for OpenAI, if using an OpenAI model.
#' @param api_base optional character string specifying custom base URL for the OpenAI API.
#' @param api_version  optional character string specifying custom version for the OpenAI API.
#' @param api_type optional character specifying type of API instance, e.g. "azure"
#' @param api_engine optional to change openai engine, e.g. "gpt-35-turbo"
#' @return numeric matrix of `length(text) x length(embedding)`. Original text is stored in the row names attribute.
#' @importFrom data.table data.table
#' @export
embed_text <- function(text,
                       batch_size    = NULL,
                       model         = NULL,
                       api_key       = NULL,
                       api_base      ="https://api.openai.com",
                       api_version   = "v1",
                       api_type      = NULL,
                       api_engine    = "text-embedding-ada-002",
                       request_limit = 3000,
                       token_limit   = 300000
                       ) {

  if(is.null(sentiment.ai::sentiment.env$embed)){
    warning("Embedding model: sentiment.env$embed not found!")
    create_error_text(paste0("Initiating an instance now with model = ", model),
                      "",
                      "If you have not run install_sentiment.ai() yet, this will probably cause an error!")
    init_sentiment.ai(model       = model,
                      api_key     = api_key,
                      api_base    = api_base,
                      api_version = api_version,
                      api_type    = api_type,
                      api_engine  = api_engine)
  }

  if (is.null(batch_size)) {
    batch_size <- length(text)
  }

  # if open ai was set in init_sentiment.ai()
  if (sentiment.ai::sentiment.env$openai) {
    if(sentiment.env$parallel > 2) {
      # will use n-1 threads to ping API
      text_embed <- openai_embed(text, request_limit, token_limit)
    } else {
      # don't try to run in parallel if that won't work
      text_embed <- openai_embed(text, request_limit, token_limit)
    }

  } else {
    # DEFAULT TO TFHUB
    text_embed <- hub_embed(text, batch_size)
  }

  text_embed <- t(as.matrix(text_embed))
  rownames(text_embed) <- text

  return(text_embed)
}


#' Embed text using OpenAI
#'
#' @param text A character vector of text to embed.
#' @param request_limit The maximum number of requests allowed.
#' @param token_limit The maximum number of tokens allowed.
#'
#' @return A data.table containing the embeddings.
#'
#' @examples
#' \dontrun{
#'   init_sentiment.ai(...)
#'   result <- openai_embed(c("hello", "world"))
#' }
openai_embed <- function(text, request_limit=3000, token_limit=300000) {

  # embedding fuunction
  embed_function <- sentiment.ai::sentiment.env$embed

  # Initialize counters for requests and tokens
  request_count <- 0
  token_count <- 0

  # Initialize an empty matrix to store results
  n <- length(text)

  # initialize matrix from text[1]
  embedings <- matrix(unlist(embed_function(text[1])), ncol=1)

  # Initialize progress bar
  pb <- txtProgressBar(min = 0, max = n, style = 3, char = "|")

  for(i in seq_along(text[-1])) {
    # Update progress bar
    index <- i+1

    this_tokens <- sum(nchar(text[index])) %/% 3  # Simplified token count

    # Check rate limits
    if ((request_count + 1) > request_limit || (token_count + this_tokens) > token_limit) {
      Sys.sleep(60)  # Sleep for 60 seconds
      request_count <- 0
      token_count <- 0
    }

    # Embed function remains the same as in your original code
    #message(text[index])
    this_embed <- matrix(unlist(embed_function(text[index])), ncol=1)

    # Assuming this_embed is a list of numerical vectors
    embedings <- cbind(embedings, this_embed)

    # Update request and token counts
    request_count <- request_count + 1
    token_count <- token_count + this_tokens

    # increment progress bar
    setTxtProgressBar(pb, index)
  }

  # Close progress bar
  close(pb)

  return(embedings)
}


# Parallelized OpenAI Embed Function
openai_embed_parallel <- function(text, request_limit=3000, token_limit=300000) {
  # Initialize counters for requests and tokens
  request_count <- 0
  token_count <- 0

  # Create a cluster
  cl <- makeCluster(parallel::detectCores() - 1)

  # Export necessary variables to the cluster
  embed_function <- sentiment.ai::sentiment.env$embed

  clusterExport(cl,
                c("request_count",
                  "token_count",
                  "request_limit",
                  "token_limit",
                  "embed_function"),
                envir=environment())

  # Use pbapply for parallel processing with a progress bar
  pbapply::pboptions(type = "txt", style = 3, char = "|")
  text_embed_list <- pbapply::pbsapply(text, function(text_i) {

    this_tokens <- sum(nchar(text_i)) %/% 3  # Simplified token count

    # Check rate limits
    if ((request_count + 1) > request_limit || (token_count + this_tokens) > token_limit) {
      Sys.sleep(60)  # Sleep for 60 seconds
      request_count <- 0
      token_count <- 0
    }

    this_embed <- matrix(unlist(embed_function(text_i)), ncol=1)

    return(list(this_embed))

  }, cl = cl)

  # Stop the cluster
  stopCluster(cl)

  # Combine all the results into a matrix
  text_embed_matrix <- do.call(cbind, text_embed_list)

  return(text_embed_matrix)
}



#' Load OpenAI Embedding Function
#'
#' This function returns another function (`embed_function`) that can be used to get embeddings from OpenAI.
#'
#' @param model_name The name of the OpenAI model to use for embeddings.
#' @param api_key The API key for OpenAI.
#' @param api_base The base URL for the OpenAI API. Default is "https://api.openai.com".
#' @param api_version The API version to use. Default is "v1".
#' @param api_type Optional API type parameter.
#' @param api_engine Optional API engine parameter.
#'
#' @return A function (`embed_function`) that takes a text string and returns its embedding.
#'
#' @examples
#' # this allows a user-friendly embedding function
#' embed_text <- load_openai_embedding("text-embedding-ada-002", "your_api_key_here")
#' result <- embed_text("some text")
load_openai_embedding <- function(model_name,
                                  api_key,
                                  api_base = "https://api.openai.com",
                                  api_version = "v1",
                                  api_type = NULL,
                                  api_engine = NULL) {

  embed_function <- function(text) {
    # Debugging prints
    headers <- httr::add_headers(
      'Authorization' = paste('Bearer', api_key),
      'Content-Type' = 'application/json'
    )

    data <- list(
      input = text,
      model = model_name
    )

    url <- paste0(api_base, "/", api_version, "/embeddings")

    # override for azure instance
    if(!is.null(api_type) && tolower(api_type)=="azure"){
      # check if version is a date
      dversion <- grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", api_version)
      if(!dversion){
        # fallback onto recent version
        api_version <- "2023-05-15"
      }

      # Debugging prints
      headers <- httr::add_headers(
        'api-key' = paste('Bearer', api_key),
        'Content-Type' = 'application/json'
      )

      data <- list(input = text)

      # POST https://{your-resource-name}.openai.azure.com/openai/deployments/{deployment-id}/embeddings?api-version={api-version}
      url <- paste0(api_base, "/openai/deployments/", model_name, "/embeddings?api-version=", api_version)
      print(url)

    }


    response <- httr::POST(url, headers, body = data, encode = "json")

    if (httr::status_code(response) == 200) {
      embedding <- httr::content(response, "parsed")$data[[1]]$embedding
      return(embedding)
    } else {
      stop(paste("API call failed:", httr::content(response, "text")))
    }
  }

  return(embed_function)
}



# Z. UTILITY FUNCTIONS =========================================================

#' as py list
#' because R to Python conversion doesn't work with list is of length 1
#' @param x character vector that is to be passed into tensorflowtext via reticulate.
#' @return List if x is length 1 else x.
#' @export
as_py_list <- function(x){

  # do we need to convert this to python? reticulate::r_to_py
  if(length(x) <= 1){
    as.list(x)
  } else{
    x
  }
}
