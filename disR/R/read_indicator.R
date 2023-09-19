#' Read Google Sheets data and select columns
#'
#'
#' @export
read_indicator <- function(x, sheet = "IM Full Disaggs"){

  require(openxlsx)
  require(tidyverse)


  if("Indicator.Disaggregates:.4th.Order" %in%
     colnames(read.xlsx(x, sheet))) {

    read.xlsx(x,
            sheet = sheet, fillMergedCells=T) %>%
    rename(ro = RO,
         ou = OU,
         code = `Pa.Id`,
         #year = `Fiscal Year`,
         # target = Target,
         #actual = Actual,
         # deviation = Deviation,

         ac_name = `Activity.Name`,
         # status = `Activity Status`,
         # start = `Activity Start Date`,
         # end = `Activity End Date`,
         # country = `Disaggregate Country`,
         # frequency = `Indicator Collection Frequency`,
         # period = `Collection Period Name`,
         ic = `Indicator.Code`,
         # ind_name = `Indicator Name`,
         # udn = UDN,
         # data_type = `Data Type`,
         d1 = `Indicator.Disaggregates:.1st.Order`,
         d2 = `Indicator.Disaggregates:.2nd.Order`,
         d3 = `Indicator.Disaggregates:.3rd.Order`,
         d4 = `Indicator.Disaggregates:.4th.Order`
  ) %>%
      #mutate(across(starts_with("Actual|Target"), is.numeric(.x))) %>%
      filter(if_any(c("ic", "ro", "ou"), ~ !str_detect(., "Total")))  %>%
      pivot_longer(starts_with("Actual") | starts_with("Target")) %>%
      separate(name, into = c("type", "year"))


  } else if("Indicator.Disaggregates:.3rd.Order" %in%
            colnames(read.xlsx(x, sheet))) {

    read.xlsx(x,
              sheet = sheet, fillMergedCells=T) %>%
      rename(ro = RO,
             ou = OU,
             code = `Pa.Id`,
             #year = `Fiscal Year`,
             # target = Target,
             #actual = Actual,
             # deviation = Deviation,

             ac_name = `Activity.Name`,
             # status = `Activity Status`,
             # start = `Activity Start Date`,
             # end = `Activity End Date`,
             # country = `Disaggregate Country`,
             # frequency = `Indicator Collection Frequency`,
             # period = `Collection Period Name`,
             ic = `Indicator.Code`,
             # ind_name = `Indicator Name`,
             # udn = UDN,
             # data_type = `Data Type`,
             d1 = `Indicator.Disaggregates:.1st.Order`,
             d2 = `Indicator.Disaggregates:.2nd.Order`,
             d3 = `Indicator.Disaggregates:.3rd.Order`,
             # d4 = `Indicator.Disaggregates:.4th.Order`
      ) %>%
      #mutate(across(starts_with("Actual|Target"), is.numeric(.x))) %>%
      filter(if_any(c("ic", "ro", "ou"), ~ !str_detect(., "Total")))  %>%
      pivot_longer(starts_with("Actual") | starts_with("Target")) %>%
      separate(name, into = c("type", "year"))


    } else if("Indicator.Disaggregates:.2nd.Order" %in%
              colnames(read.xlsx(x, sheet ))) {

      read.xlsx(x,
                sheet = sheet, fillMergedCells=T) %>%
      rename(ro = RO,
             ou = OU,
             code = `Pa.Id`,
             #year = `Fiscal Year`,
             # target = Target,
             #actual = Actual,
             # deviation = Deviation,

             ac_name = `Activity.Name`,
             # status = `Activity Status`,
             # start = `Activity Start Date`,
             # end = `Activity End Date`,
             # country = `Disaggregate Country`,
             # frequency = `Indicator Collection Frequency`,
             # period = `Collection Period Name`,
             ic = `Indicator.Code`,
             # ind_name = `Indicator Name`,
             # udn = UDN,
             # data_type = `Data Type`,
             d1 = `Indicator.Disaggregates:.1st.Order`,
             d2 = `Indicator.Disaggregates:.2nd.Order`,
             # d3 = `Indicator.Disaggregates:.3rd.Order`,
             # d4 = `Indicator.Disaggregates:.4th.Order`
      )  %>%
        #mutate(across(starts_with("Actual|Target"), is.numeric(.x))) %>%
        filter(if_any(c("ic", "ro", "ou"), ~ !str_detect(., "Total")))  %>%
        pivot_longer(starts_with("Actual") | starts_with("Target")) %>%
        separate(name, into = c("type", "year"))

  } else if("Indicator.Disaggregates:.1st.Order" %in%
            colnames(read.xlsx(x, sheet))) {
    read.xlsx(x,
              sheet = sheet, fillMergedCells=T) %>%
      rename(ro = RO,
             ou = OU,
             code = `Pa.Id`,
             #year = `Fiscal Year`,
             # target = Target,
             #actual = Actual,
             # deviation = Deviation,

             ac_name = `Activity.Name`,
             # status = `Activity Status`,
             # start = `Activity Start Date`,
             # end = `Activity End Date`,
             # country = `Disaggregate Country`,
             # frequency = `Indicator Collection Frequency`,
             # period = `Collection Period Name`,
             ic = `Indicator.Code`,
             # ind_name = `Indicator Name`,
             # udn = UDN,
             # data_type = `Data Type`,
             d1 = `Indicator.Disaggregates:.1st.Order`,
             # d2 = `Indicator.Disaggregates:.2nd.Order`,
             # d3 = `Indicator.Disaggregates:.3rd.Order`,
             # d4 = `Indicator.Disaggregates:.4th.Order`
      )  %>%
      #mutate(across(starts_with("Actual|Target"), is.numeric(.x))) %>%
      filter(if_any(c("ic", "ro", "ou"), ~ !str_detect(., "Total")))  %>%
      pivot_longer(starts_with("Actual") | starts_with("Target")) %>%
      separate(name, into = c("type", "year"))

    } else stop()

}


