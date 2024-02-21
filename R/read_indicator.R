#' Read indicator spreadsheets modified from the format of Magnus's Google Sheets
#'
#'
#' @export
read_indicator <- function(x, sheet = "IM Full Disaggs"){

  if("Indicator.Disaggregates:.4th.Order" %in%
     colnames(openxlsx::read.xlsx(x, sheet))) {

    openxlsx::read.xlsx(x, sheet = sheet, fillMergedCells=T) %>%
    dplyr::rename(ro = RO
                  , ou = OU
                  , a_code = `Pa.Id`
         # , year = `Fiscal Year`
         # , target = Target
         # , actual = Actual
         # , deviation = Deviation

         , a_name = `Activity.Name`
         # , status = `Activity Status`
         # , start = `Activity Start Date`
         # , end = `Activity End Date`
         # , country = `Disaggregate Country`
         # , frequency = `Indicator Collection Frequency`
         # , period = `Collection Period Name`
         , ic = `Indicator.Code`
         # , ind_name = `Indicator Name`
         # , udn = UDN
         # , data_type = `Data Type`
         , d1 = `Indicator.Disaggregates:.1st.Order`
         , d2 = `Indicator.Disaggregates:.2nd.Order`
         , d3 = `Indicator.Disaggregates:.3rd.Order`
         , d4 = `Indicator.Disaggregates:.4th.Order`
  ) %>%
      #mutate(across(starts_with("Actual|Target"), is.numeric(.x))) %>%
      dplyr::filter(dplyr::if_any(c("ic", "ro", "ou"), ~ !stringr::str_detect(., "Total")))  %>%
      tidyr::pivot_longer(tidyselect::starts_with("Actual") | tidyselect::starts_with("Target")) %>%
      tidyr::separate(name, into = c("type", "year")) %>%
      dplyr::mutate(across(-value, ~ trimws(.)))


  } else if("Indicator.Disaggregates:.3rd.Order" %in%
            colnames(openxlsx::read.xlsx(x, sheet))) {

    openxlsx::read.xlsx(x, sheet = sheet, fillMergedCells=T) %>%
      dplyr::rename(ro = RO
                    , ou = OU
                    , a_code = `Pa.Id`
                    # , year = `Fiscal Year`
                    # , target = Target
                    # , actual = Actual
                    # , deviation = Deviation

                    , a_name = `Activity.Name`
                    # , status = `Activity Status`
                    # , start = `Activity Start Date`
                    # , end = `Activity End Date`
                    # , country = `Disaggregate Country`
                    # , frequency = `Indicator Collection Frequency`
                    # , period = `Collection Period Name`
                    , ic = `Indicator.Code`
                    # , ind_name = `Indicator Name`
                    # , udn = UDN
                    # , data_type = `Data Type`
                    , d1 = `Indicator.Disaggregates:.1st.Order`
                    , d2 = `Indicator.Disaggregates:.2nd.Order`
                    , d3 = `Indicator.Disaggregates:.3rd.Order`
                    # , d4 = `Indicator.Disaggregates:.4th.Order`
      ) %>%
      #mutate(across(starts_with("Actual|Target"), is.numeric(.x))) %>%
      dplyr::filter(dplyr::if_any(c("ic", "ro", "ou"), ~ !stringr::str_detect(., "Total")))  %>%
      tidyr::pivot_longer(tidyselect::starts_with("Actual") | tidyselect::starts_with("Target")) %>%
      tidyr::separate(name, into = c("type", "year")) %>%
      dplyr::mutate(across(-value, ~ trimws(.)))



    } else if("Indicator.Disaggregates:.2nd.Order" %in%
              colnames(openxlsx::read.xlsx(x, sheet ))) {

      openxlsx::read.xlsx(x, sheet = sheet, fillMergedCells=T) %>%
        dplyr::rename(ro = RO
                      , ou = OU
                      , a_code = `Pa.Id`
                      # , year = `Fiscal Year`
                      # , target = Target
                      # , actual = Actual
                      # , deviation = Deviation

                      , a_name = `Activity.Name`
                      # , status = `Activity Status`
                      # , start = `Activity Start Date`
                      # , end = `Activity End Date`
                      # , country = `Disaggregate Country`
                      # , frequency = `Indicator Collection Frequency`
                      # , period = `Collection Period Name`
                      , ic = `Indicator.Code`
                      # , ind_name = `Indicator Name`
                      # , udn = UDN
                      # , data_type = `Data Type`
                      , d1 = `Indicator.Disaggregates:.1st.Order`
                      , d2 = `Indicator.Disaggregates:.2nd.Order`
                      # , d3 = `Indicator.Disaggregates:.3rd.Order`
                      # , d4 = `Indicator.Disaggregates:.4th.Order`
        ) %>%
        #mutate(across(starts_with("Actual|Target"), is.numeric(.x))) %>%
        dplyr::filter(dplyr::if_any(c("ic", "ro", "ou"), ~ !stringr::str_detect(., "Total")))  %>%
        tidyr::pivot_longer(tidyselect::starts_with("Actual") | tidyselect::starts_with("Target")) %>%
        tidyr::separate(name, into = c("type", "year")) %>%
        dplyr::mutate(across(-value, ~ trimws(.)))

  } else if("Indicator.Disaggregates:.1st.Order" %in%
            colnames(openxlsx::read.xlsx(x, sheet))) {
    openxlsx::read.xlsx(x, sheet = sheet, fillMergedCells=T) %>%
      dplyr::rename(ro = RO
                    , ou = OU
                    , a_code = `Pa.Id`
                    # , year = `Fiscal Year`
                    # , target = Target
                    # , actual = Actual
                    # , deviation = Deviation

                    , a_name = `Activity.Name`
                    # , status = `Activity Status`
                    # , start = `Activity Start Date`
                    # , end = `Activity End Date`
                    # , country = `Disaggregate Country`
                    # , frequency = `Indicator Collection Frequency`
                    # , period = `Collection Period Name`
                    , ic = `Indicator.Code`
                    # , ind_name = `Indicator Name`
                    # , udn = UDN
                    # , data_type = `Data Type`
                    , d1 = `Indicator.Disaggregates:.1st.Order`
                    # , d2 = `Indicator.Disaggregates:.2nd.Order`
                    # , d3 = `Indicator.Disaggregates:.3rd.Order`
                    # , d4 = `Indicator.Disaggregates:.4th.Order`
      ) %>%
      #mutate(across(starts_with("Actual|Target"), is.numeric(.x))) %>%
      dplyr::filter(dplyr::if_any(c("ic", "ro", "ou"), ~ !stringr::str_detect(., "Total")))  %>%
      tidyr::pivot_longer(tidyselect::starts_with("Actual") | tidyselect::starts_with("Target")) %>%
      tidyr::separate(name, into = c("type", "year")) %>%
      dplyr::mutate(across(-value, ~ trimws(.)))

    } else stop()

}


