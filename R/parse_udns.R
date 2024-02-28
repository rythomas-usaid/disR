#' Functions to parse udns that are the unique udns to calculate indicator totals
#'
#'
#' @import tidyverse
#' @export filter_udns
filter_udns <- function(ic, udn) {
  filtered <- udns %>%
    filter(ic %in% ind_code & str_detect(Disaggregate.Name, udn)) %>%
    ungroup() %>%
    filter(nchar(udn) == max(nchar(udn))) %>%
    select(udn_formulas) %>%
    unlist()

  names(filtered) <- NULL

  return(filtered)
}

#' @export print_udns
print_udns <- function(ic) {
  if(!exists("udns")) stop("Load the `udns` data.frame by calling: \n
                         load('../data/extract.Rdata').")
  out <- udns %>%
    filter(ic %in% ic) %>%
    select(ic, Disaggregate.Name, uudn, udn_formulas) %>%
    arrange(ic, uudn)

  return(out)
}

#' @export extract_udns
extract_udns <- function(x) {
  stringr::str_replace_all(x, "[()/+*]|100|manual|na", " ") %>%
    stringr::str_extract_all(stringr::boundary("word"))
}

#' @export filter_indicators
filter_indicators <- function(ic, udn) {

  warning("using extract version of filter_indicators")

  uudns <- filter_udns(ic = ic, udn = udn)

  filtered <- indicators %>%
    filter(uudn %in% uudns)

  return(filtered)
}
