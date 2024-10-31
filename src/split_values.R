library(stringr)

split_values <- function(x, sep=';', unlist=FALSE) {
  #' split_values
  #' 
  #' Function splits char elements inside char vectors and removes surrounding whitespace
  #' 
  #' @param x character or vector of characters
  #' @param sep string to split on; default ";"
  #' @param unlist whether or not to unlist the character list; default TRUE
  #' @return a list (or vector, if unlist=TRUE) of character elements
  #' 
  #' @example split_values(x=c('this', 'string; is', 'split')) 
  #' @example split_values(x=c('This will be return as a vector'), sep=',', unlist=TRUE)
  #' 
  x = stringr::str_split(x, sep) # split by sep and return string
  x = sapply(x, trimws) # trim whitespace of strings in list
  if (unlist) {
    x = unlist(x)
  }
  return(x)
}