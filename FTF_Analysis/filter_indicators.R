#' Filter indicators with keywords
#'
#'
#'
#' @export

filter_ <- function(data, ic_1819 = NA, search = NA, d1 = NA, d2 = NA, d3 = NA, d4 = NA) {
  out <- data

  if (!is.na(ic_1819) || !is.na(search)) {
    out <- out %>%
      filter(ic_1819 == ic_1819) %>%
      filter(if_any(everything(), ~str_detect(., search)))
  }

  if (!is.na(d1) || !is.na(d2) || !is.na(d3) || !is.na(d4)) {
    out <- out %>%
      filter(if_all(any_of(c("d1", "d2", "d3", "d4")), ~str_detect(., paste(d1, d2, d3, d4, collapse = "|"))))
  }

  return(out)
}
