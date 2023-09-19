#' Load DIS and FTFMS data in one of two formats \"magnus\" or \"extract\".
#'
#' This function is a wrapper around sum(). The sum() function returns 0 for
#' vectors that have all NA values, which is not desired behavior especially
#' for the DIS analysis.
#'
#' @export
load_data <- function(database = "extract") {

  if (database == "magnus") {

    rm(list = ls())
    load("data/magnus.Rdata")

  } else if(database == "extract") {

    rm(list = ls())
    load("data/extract.Rdata")

    } else stop("Parameter \"database\" must be either \"magnus\" or \"extract\" (default.")
  }
