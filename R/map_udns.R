#' Standardize FTFMS disaggregates
#'
#' These four functions are used in a mutate statement in 'rbind_dis_to_ftfms.R'. They use case_when to create standardized disaggregates for d1, d2, d3, d4 to match the DIS disaggregates.
#' @param udns a vector (column) of indicator codes
#' @export map_udns

# load("../data/basic.rdata")

# udns %>%
#   filter(ic == "EG.3-2") %>%
#   distinct(ic, udn, d_name) %>%
#   left_join(disaggregate_crosswalk) %>%
#   filter(if_any(c(d1, d2, d3, d4), ~ ! is.na(.)))
