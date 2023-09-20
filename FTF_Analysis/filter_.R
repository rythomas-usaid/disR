#' Filter indicators with keywords
#'
#' @import tidyverse
#'
#'
#' @export
filter_ <- function(x, ic_1819 = NA, search=NA, d1 = NA,
                    d2 = NA, d3 = NA, d4 = NA, ...) {
  paste_ <- function(...) {
    paste0(..., collapse = "|")
  }

  ic_1819 <- paste_(ic_1819)
  search <- paste_(search)
  d1 <- paste_(d1)
  d2 <- paste_(d2)
  d3 <- paste_(d3)
  d4 <- paste_(d4)

  if (any(!is.na(c(ic_1819, search)))) {

    filter_first <- function(...) {
      if (!is.na(ic_1819) & is.na(search)) {

        return <- x %>% filter(ic_1819 == ic_1819)

      } else if (!is.na(ic_1819) & !is.na(search)) {

        return <- x  %>% filter(ic_1819 == ic_1819) %>%
          filter(if_any(c("d1", "d2", "d3", "d4"),
                        ~ str_detect(., search)))
      }
      return(return)
    }

    out <- filter_first()

  } else {

    out <- x
  }

  if (any(!is.na(c(d1, d2, d3, d4)))) {

  # Single
  if (!is.na(all(d1)) & all(is.na(c(d2, d3, d4)))) {
    out <- out %>% filter(str_detect(d1, d1))

  } else if (!is.na(all(d2)) & all(is.na(c(d1, d3, d4)))) {
    out <- out %>% filter(if_any(d2), str_detect(d2))

  } else if (!is.na(all(d3)) & all(is.na(c(d1, d3, d4)))) {
    out <-  out %>% filter(str_detect(d3, d3))

  } else if (!is.na(all(d4)) & all(is.na(c(d1, d3, d2)))) {
    out <-  out %>% filter(str_detect(d4, d4))

  # Double
  } else if (all(!is.na(c(d1, d2))) & all(is.na(c(d4, d3)))){
    out <- out %>% filter(str_detect(d1, d1) & str_detect(d2, d2))

  } else if (all(!is.na(c(d1, d3))) & all(is.na(c(d4, d2)))){
    out <- out %>% filter(str_detect(d1, d1) & str_detect(d3, d3))

  } else if (all(!is.na(c(d1, d4))) & all(is.na(c(d2, d3)))){
    out <- out %>% filter(d1 %in% d1 & d4 %in% d4)

  } else if (all(!is.na(c(d2, d3))) & all(is.na(c(d1, d4)))){
    out <- out %>% filter(str_detect(d3, d3) & str_detect(d2, d2))

  } else if (all(!is.na(c(d2, d4))) & all(is.na(c(d1, d2, d3)))){
    out <- out %>% filter(str_detect(d4, d4) & str_detect(d2, d2))

  } else if (all(!is.na(c(d3, d4))) & all(is.na(c(ic_1819, d1, d2, d3)))){
    out <- out %>% filter(str_detect(d4, d4) & str_detect(d3, d3))

  # Triple
  } else if (all(!is.na(c(d1, d2, d3))) & all(is.na(c(d4)))){
    out <- out %>% filter(str_detect(d1, d1) & str_detect(d2, d2) & str_detect(d3, d3))

  } else if (all(!is.na(c(d1, d2, d4))) & all(is.na(c(d3)))){
    out <- out %>% filter(str_detect(d1, d1) & str_detect(d2, d2) & str_detect(d4, d4))

  } else if (all(!is.na(c(d1, d4, d3))) & all(is.na(c(d2)))){
    out <- out %>% filter(str_detect(d1, d1) & str_detect(d4, d4) & str_detect(d3, d3))

  } else if (all(!is.na(c(d4, d2, d3))) & all(is.na(c(d1)))){
    out <- out %>% filter(str_detect(d4, d4) & str_detect(d2, d2) & str_detect(d3, d3))
  }

    return(out)
  }
}


#filter_indicators(indicators, ic_1819 = "EG.3.2-25", d1 = c("Crop Land"))


