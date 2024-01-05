#' Works with udn table to find the right UDNs
#'
#'
#' @export
print_udns <- function(ind_code) {
  if(!exists("udns")) stop("Load the `udns` data.frame by calling: \n
                         load('../database/extract/indicators_db.Rdata').")
  out <- udns %>%
    filter(ic %in% ind_code) %>%
    select(ic, Disaggregate.Name, uudn, udn_formulas) %>%
    arrange(ic, uudn)

  return(out)
}
