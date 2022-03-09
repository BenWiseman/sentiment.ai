#' Default sentiment matching dictionary
#'
#' This is a default sentiment matching dictionary with over 100 pairs of
#' positive:negative examples. Feel free to use as-is or use as an example to
#' create your own. The main point is that there needs to be a corresponding
#' negative for each positive.
#'
#' @examples
#' # For Example
#' pos <- c("good apples", "fresh", "delicious")
#' neg <- c("bad apples", "not fresh", "not delicious")
#'
#' # If positive was:
#' c("good", "fresh", "delicious")
#'
#' # Then "These were some good apples" would be seen as closer to a negative example!
#'
#' @docType data
#'
#' @usage data(default)
#'
#' @format An object of class `"data.table"`
#'
"default"

