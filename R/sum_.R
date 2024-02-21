#' Sum values in a vector and preserve NA if all are NA.
#'
#' This function is a wrapper around sum(). The sum() function returns 0 for
#' vectors that have all NA values, which is not desired behavior especially
#' for the Feed the Future indicator analysis.
#' @param x A vector that you want to sum
#' @export
sum_ <- function(...) {
  x <- c(...)
  if(all(is.na(x))) NA_real_ else return(sum(x, na.rm=T))
}

