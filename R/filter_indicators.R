#' Filter indicators with keywords
#'
#' @import dplyr
#' @export
filter_indicators <- function(x, ic_1819 = NA, d1 = NA,
                                 d2 = NA, d3 = NA, d4 = NA, search=NA, ...) {

  if (!is.na(search)) {
    out <- x %>% dplyr::filter(if_any(c("d1", "d2", "d3", "d4"),
                 ~ stringr::str_detect(., regex(search, ignore.case=T))))
  }

  # Single
  else if (!is.na(ic_1819) & all(is.na(c(d1, d2, d3, d4)))) {
    out <- x[x$ic_1819 %in% ic_1819, ]

  } else if (!is.na(all(d1)) & all(is.na(c(ic_1819, d2, d3, d4)))) {
    out <- x[x$d1 %in% d1, ]

  } else if (!is.na(all(d2)) & all(is.na(c(ic_1819, d1, d3, d4)))) {
    out <- x[x$d2 %in% d2, ]

  } else if (!is.na(all(d3)) & all(is.na(c(ic_1819, d1, d3, d4)))) {
    out <- x[x$d3 %in% d3, ]

  } else if (!is.na(all(d4)) & all(is.na(c(ic_1819, d1, d3, d2)))) {
    out <- x[x$d4 %in% d4, ]

  # Double
  } else if (all(!is.na(c(ic_1819, d1))) & all(is.na(c(d2, d3, d4)))){
    out <- x[x$ic_1819 %in% ic_1819 & x$d1 %in% d1, ]

  } else if (all(!is.na(c(ic_1819, d2))) & all(is.na(c(d1, d3, d4)))){
    out <- x[x$ic_1819 %in% ic_1819 & x$d2 %in% d2, ]

  } else if (all(!is.na(c(ic_1819, d3))) & all(is.na(c(d1, d2, d4)))){
    out <- x[x$ic_1819 %in% ic_1819 & x$d3 %in% d3, ]

  } else if (all(!is.na(c(ic_1819, d4))) & all(is.na(c(d1, d3, d2)))){
    out <- x[x$ic_1819 %in% ic_1819 & x$d4 %in% d4, ]

  } else if (all(!is.na(c(d1, d2))) & all(is.na(c(ic_1819, d4, d3)))){
    out <- x[x$d1 %in% d1 & x$d2 %in% d2, ]

  } else if (all(!is.na(c(d1, d3))) & all(is.na(c(ic_1819, d4, d2)))){
    out <- x[x$d1 %in% d1 & x$d3 %in% d3, ]

  } else if (all(!is.na(c(d1, d4))) & all(is.na(c(ic_1819, d2, d3)))){
    out <- x[x$d1 %in% d1 & x$d4 %in% d4, ]

  } else if (all(!is.na(c(d2, d3))) & all(is.na(c(ic_1819, d1, d4)))){
    out <- x[x$d2 %in% d2 & x$d3 %in% d3, ]

  } else if (all(!is.na(c(d2, d4))) & all(is.na(c(ic_1819, d1, d2, d3)))){
    out <- x[x$d2 %in% d2 & x$d4 %in% d4, ]

  } else if (all(!is.na(c(d3, d4))) & all(is.na(c(ic_1819, d1, d2, d3)))){
    out <- x[x$d3 %in% d3 & x$d4 %in% d4, ]

  # Triple
  } else if (all(!is.na(c(ic_1819, d1, d2))) & all(is.na(c(d3, d4)))){
    out <- x[x$ic_1819 %in% ic_1819 & x$d1 %in% d1 & x$d2 %in% d2, ]

  } else if (all(!is.na(c(ic_1819, d1, d3))) & all(is.na(c(d2, d4)))){
    out <- x[x$ic_1819 %in% ic_1819 & x$d1 %in% d1 & x$d3 %in% d3 , ]

  } else if (all(!is.na(c(ic_1819, d1, d4))) & all(is.na(c(d2, d3)))){
    out <- x[x$ic_1819 %in% ic_1819 & x$d1 %in% d1 & x$d4 %in% d4 , ]

  } else if (all(!is.na(c(d1, d2, d3))) & all(is.na(c(ic_1819, d4)))){
    out <- x[x$d1 %in% d1 & x$d2 %in% d2 & x$d3 %in% d3 , ]

  } else if (all(!is.na(c(d1, d2, d4))) & all(is.na(c(ic_1819, d3)))){
    out <- x[x$d1 %in% d1 & x$d2 %in% d2 & x$d4 %in% d4 , ]

  } else if (all(!is.na(c(d1, d4, d3))) & all(is.na(c(ic_1819, d2)))){
    out <- x[x$d1 %in% d1 & x$d4 %in% d4 & x$d3 %in% d3 , ]

  } else if (all(!is.na(c(d4, d2, d3))) & all(is.na(c(ic_1819, d1)))){
    out <- x[x$d4 %in% d4 & x$d2 %in% d2 & x$d3 %in% d3 , ]

  # Quadruple
  } else if (all(!is.na(c(ic_1819, d1, d2, d3))) & all(is.na(c(d4)))){
    out <- x[x$ic_1819 %in% ic_1819 & d1 %in% d1 & d2 %in% d2 & d3 %in% d3, ]

  } else if (all(!is.na(c(d1, d2, d3, d4))) & all(is.na(c(ic_1819)))){
    out <- x[x$d4 %in% d4 & x$d1 %in% d1 & x$d2 %in% d2 & x$d3 %in% d3, ]

  } else if (all(!is.na(c(ic_1819, d2, d3, d4))) & all(is.na(c(d1)))){
    out <- x[x$d4 %in% d4 & x$ic_1819_1819 %in% ic_1819 & x$d2 %in% d2 & x$d3 %in% d3, ]

  } else if (all(!is.na(c(d1, ic_1819, d3, d4))) & all(is.na(c(d2)))){
    out <- x[x$d4 %in% d4 & x$d1 %in% d1 & x$ic_1819_1819 %in% ic_1819 & x$d3 %in% d3, ]

  } else if (all(!is.na(c(d1, ic_1819, d2, d4))) & all(is.na(c(d3)))){
    out <- x[x$d4 %in% d4 & x$d1 %in% d1 & x$ic_1819_1819 %in% ic_1819 & x$d2 %in% d2, ]

  # Quintuple
  } else if (all(!is.na(c(ic_1819, d1, d2, d3, d4)))){
    out <- x[x$ic_1819 %in% ic_1819 & d1 %in% d1 & d2 %in% d2 & d3 %in% d3 & d4 %in% d4, ]
  }

  return(out)

}


#filter_indicators(indicators, ic_1819 = "EG.3.2-25", d1 = c("Crop Land"))


