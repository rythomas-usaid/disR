#' Works with udn table to find the right UDNs
#'
#'
#' @export

print_udns <- function(ind_code) {
  if(!exists("udns")) stop("Load the `udns` data.frame by calling: \n
                         load('../database/extract/indicators_db.Rdata').")
  out <- udns %>%
    filter(ic %in% ind_code) %>%
    select(ic, Disaggregate.Name, uudn, udn_formulas)

  return(out)
}

#' @export
filter_udns <- function(ind_code, dn) {
  filter_udns <- udns %>%
    filter(ic %in% ind_code & Disaggregate.Name == dn) %>%
    ungroup() %>%
    filter(nchar(udn) == max(nchar(udn))) %>%
    select(udn_formulas) %>%
    unlist()

  names(filter_udns) <- NULL

  return(filter_udns)
}

#' @export
filter_indicators <- function(ind_code, dn) {
  uudns <- filter_udns(ind_code = ind_code, dn = dn)

  out <- indicators %>%
    filter(uudn %in% uudns)

  return(out)

}


#### Replicating from Extract Database
#' @export
join_tables <- function(x) {
  x %>% #distinct(udn)

    # join values
    left_join(x = ., y = values
              , by = join_by(id == id_ind)
              , suffix = c("_ind", "_val") # label variables
              , keep = FALSE)  %>%

    # join activities details
    left_join(activities
              , by = join_by(id_ac == id)
              , suffix = c("_ind", "_val") # label variables
              , keep = FALSE)
}

