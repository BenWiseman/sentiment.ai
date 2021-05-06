#' Default sentiment matching dictionary
#'
#' This is a default sentiment matching dictionary with over 100 pairs of positive:negative examples
#' Feel free to use as-is or use as an example to create your own.
#' The main point is that there needs to be a corresponding negative for each positive.
#' For example:
#' pos <- c("good apples", "fresh", "delicious")
#' neg <- c("bad apples", "not fresh", "not delicious")
#'
#' if, for example, positive was:
#' c("good", "fresh", "delicious")
#'
#' then
#'
#' "These were some good apples"
#'
#' would be seen as closer to a negative example!
#'
#' @docType data
#'
#' @usage data(default)
#'
#' @format An object of class `"data.table"`
#'
#' @keywords datasets
#'

#' @examples
#' data(default)
#' View(default)
"default"



#' Default sentiment embeddings
#'
#' This is a default sentiment embeddings for the default data. You can use them for vector ordination
#' otherwise they exist to speed up sentiment analysis when you have posivie = NULL, negative = NULL, and are using either a default "en" or "multi" model.
#'
#' @docType data
#'
#' @usage data(default_embeddings)
#'
#' @format An object of class `"list"`
#'
"default_embeddings"
