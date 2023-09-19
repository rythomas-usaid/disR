#' Return unique IDs based on the row number.
#'
#' This function is a helper used to write IDs for the FTF indicator database.
#'
#' @param used in mutate, a data.frame or vector
#' @return Vector of IDs
#'
#' @export
return_id <- function(row) {
  l <- round(log10(nrow(row))+1,0)
  id <- str_pad(rownames(row), width = l, pad = "0", side = "left")
  return(id)
}
