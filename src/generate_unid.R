#' generate_unid
#' description:
#'   Produces a new id's as a vector of 10-digit integers unique to the existing id's and each other
#' params:
#'   existing_unid: vector of existing unid ints
#' returns:
#'   unid: vector of int, 10-digit ints unique to input vector and each other
generate_unid <- function(x, existing_unid=c()) {
  len <- ifelse(is.data.frame(x), nrow(x), length(x)) # accepts length (rows) of dataframe or length of vector
  unid <- c() # create empty set of unid
  for (i in 1:len) {
    new_unid <- sample(1e9:(1e10-1), 1)  # generate a random 10-digit integer
    while (new_unid %in% existing_unid) {  # check that random int is unique
      new_unid <- sample(1e9:(1e10-1), 1)  # if not, generate a new random int
    }
    existing_unid <- c(existing_unid, new_unid) # add newly assigned unid to list of existing unids
    unid <- c(unid, new_unid) # add newly assigned unid to list of newly assigned unids
  } 
  return(unid)
}
