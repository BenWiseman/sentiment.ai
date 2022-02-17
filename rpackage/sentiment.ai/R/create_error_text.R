# Function to create error text (so consistent spacing)
create_error_text <- function(..., n_pad = 10){

  # put everything together
  x   <- c(...)

  # add padding
  pad <- paste0(rep(" ", n_pad), collapse = "")
  x   <- paste0(pad, x, "\n")

  # add new lines at beginning and end and cat everything together
  cat(c("\n", x, "\n"))
}
