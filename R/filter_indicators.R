#' Filter indicators with keywords
#'
#' @export
filter_indicators <- function(ind_code, dn) {

  warning("using extract version of filter_indicators")

  uudns <- filter_udns(ind_code = ind_code, dn = dn)

  filtered <- indicators %>%
    filter(uudn %in% uudns)

  return(filtered)

}

