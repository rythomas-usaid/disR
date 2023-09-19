#' Combine legacy and current indicators
#'
#' This function combines legacy indicators with the current FTF indicators.
#' The names of the indicators are also adjusted to reflect the combination. Adds
#' a new column named indicator_name.
#' @param x A data.frame in the format of indicators_df. Required columns are ro, ou, code, and year.
#'
#' @import tidyverse
#'
#' @export
combine_indicators <- function(x, ...) {

  warning("All matching indicators will be summed! This can produce incorrect
          results if used indescriminately.")
  warning("Combining",
                unique(x$ic[x$ic %in%
                              c("EG.3.2-24",  "EG.3.2-25", "EG.3.2-26",
                                "EG.3.2-x17", "EG.3.2-x18", "EG.3.2-x19")]))

  needed_names <- c("ro", "ou", "code", "year")
  if(all(needed_names %in% names(x))) {

    result <- x %>%

      # Summarize old and new indicators
      group_by(ro, ou, code, ic_1819, year) %>%
      summarise(ic = paste(sort(unique(ic)), collapse = " /")
                , value = sum(value, na.rm=T)
                , ...)
    return(result)
  } else {
  # if conditions not met
  stop('Make sure x contains the following columns: c("ro", "ou", "code", "indicator_name", "year")')
  }
}

