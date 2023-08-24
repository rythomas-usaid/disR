#' Summarize annual totals
#'
#' @import tidyverse

#' @export
sum_indicators <- function(x,
                           #years = paste0("FY", 2011:2022),
                           total = TRUE,
                           disaggregate = NA,
                           ind_code = NA,
                           #TODO: allow entering ic_1819 indicator codes
                           reporting_org = NA,
                           ind_key = NA,  ...) {
  #warning("")
  # ind_code <- "EG.3.2-26"
  # reporting_org <- "USAID"
  # ind_key <- "Sex"
  if(is.na(ind_code)) stop("Please specify one or more indicator code(s).")
  if(isTRUE(total)) ind_key <- "Sex"
   {
    # disaggregate <- "Overall"

    result <- x %>%
      #Select all the indicators you want
      filter(str_detect(ic_1819, ind_code) &
               if_any(everything(), ~str_detect(., "Sex"))) %>%

      # Join **values** using the indicators ID
      left_join(x = ., y = filter(values, type=="Actual")
                , by = join_by(id == id_ind)
                , suffix = c("_ind", "_val") # label variables with
                , keep = FALSE) %>%

      # # Join **ims** table with IM details
      # left_join(x = ., y = ims
      #           , by = join_by(id_im == id)) %>%
      #
      #       filter(year %in% years) %>%


      # Put years as columns for spreadsheet viewing
      group_by(year) %>%
      summarise(value = sum(value, na.rm=T)) %>%
      pivot_wider(names_from = c("year")
                  , values_from = "value"
                  , values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
      mutate(disaggregate = disaggregate, .before=everything())

    return(result)
  } else if(isTRUE(total) & !is.na(disaggregates)) {

    result <- x %>%

      # Join **values** using the indicators ID
      left_join(x = ., y = filter(values, type=="Actual")
                , by = join_by(id == id_ind)
                , suffix = c("_ind", "_val") # label variables with
                , keep = FALSE) %>%

      #Select all the indicators you want
      filter(str_detect(ic_1819, ind_code) &
               if_any(everything(), ~str_detect(., ind_key)) &
               if_any(c(d1, d2, d3, d4), ~str_detect(., disaggregate))) %>%
      group_by(year) %>%
      summarise(value = sum(value, na.rm=T)) %>%
      pivot_wider(names_from = c("year")
                  , values_from = "value"
                  , values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
      mutate(disaggregate = disaggregate, .before=everything())

    # # Join **ims** table with IM details
    # left_join(x = ., y = ims
    #           , by = join_by(id_im == id)) %>%

    # filter(year %in% years & ro %in% reporting_org) %>%

    # # Put years as columns for spreadsheet viewing
    # pivot_wider(id_cols = c(ic, ro, ou, code)
    #             , names_from = c("year")
    #             , values_from = "value"
    #             , values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
    # # Tidy up the order of columns for ease of reading.
    # relocate(all_of(years), .after = everything()) %>%
    #
    # # https://stackoverflow.com/questions/48062213/using-column-names-as-function-arguments
    # group_by(ic, ou) %>%
    # summarise(ro = paste(unique(ro), collapse = ' / ')
    #           , across(starts_with("FY"), ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T))
    #           , .groups = "drop")

    return(result)

  } else  {
    # if conditions not met
    stop('Error:')

  }
}
