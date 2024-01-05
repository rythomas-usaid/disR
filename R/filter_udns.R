#' Returns a vector of udns that are the unique udns to calculate indicator totals
#'
#' @export
filter_udns <- function(ind_code, dn) {
  filtered <- udns %>%
    filter(ic %in% ind_code & str_detect(Disaggregate.Name, dn)) %>%
    ungroup() %>%
    filter(nchar(udn) == max(nchar(udn))) %>%
    select(udn_formulas) %>%
    unlist()

  names(filtered) <- NULL

  return(filtered)
}
