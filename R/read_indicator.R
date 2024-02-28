#' Read indicator spreadsheets modified from the format of Magnus's Google Sheets
#'
#'
#' @export read_indicator
read_indicator <- function(x, sheet = "IM Full Disaggs", version = "relational") {

  if(version == "relational") {
      if("Indicator.Disaggregates:.4th.Order" %in% colnames(openxlsx::read.xlsx(x, sheet))) {
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
      } else if(version == "tidy") {
        if(sheet %in% openxlsx::getSheetNames(x)) {
          openxlsx::read.xlsx(x, sheet = sheet, fillMergedCells=T) %>%
            mutate(organization_level = sheet)
        }
      }
  }


# openxlsx::read.xlsx( mapper$indicator_files[1], sheet = mapper$worksheet_names[1], fillMergedCells=T) %>%
#   tidyr::pivot_longer(tidyselect::starts_with("Actual") | tidyselect::starts_with("Target")) %>%
#   tidyr::separate(name, into = c("type", "year")) %>%
#   dplyr::mutate(across(-value, ~ trimws(.)))


#' @export read_export
# library(tidyverse)
read_export <- function(x) {
  openxlsx::read.xlsx(x) %>%
    mutate(across(c(Target, Actual), ~ parse_double(gsub("[%USD,$ -]", "", .)))) %>%
    rename_extract_columns()
}

#' @export rename_extract_columns
#'
rename_extract_columns <- function(x) {
  names(x) <- tolower(names(x))
  #dat$x <- NULL
  names(x)[names(x) == "reporting.organization"] <- "ro"
  names(x)[names(x) == "operating.unit"] <- "ou"
  names(x)[names(x) == "activity.code"] <- "a_code"
  names(x)[names(x) == "activity.name"] <- "a_name"
  names(x)[names(x) == "indicator.code"] <- "ic"
  names(x)[names(x) == "indicator.name"] <- "i_name"
  names(x)[names(x) == "activity.vendor"] <- "ip"
  names(x)[names(x) == "activity.end.date"] <- "a_end"
  names(x)[names(x) == "activity.start.date"] <- "a_start"
  names(x)[names(x) == "indicator.end.date"] <- "i_end"
  names(x)[names(x) == "indicator.start.date"] <- "i_start"
  names(x)[names(x) == "disaggregate.end.date"] <- "d_end"
  names(x)[names(x) == "disaggregate.start.date"] <- "d_start"
  names(x)[names(x) == "disaggregate.name"] <- "d_name"
  names(x)[names(x) == "disaggregate.country"] <- "country"
  names(x)[names(x) == "disaggregate.commodity"] <- "commodity"
  names(x)[names(x) == "activity.office"] <- "a_office"
  names(x)[names(x) == "activity.status"] <- "status"
  names(x)[names(x) == "activity.tags"] <- "tags"
  names(x)[names(x) == "id.managing.office"] <- "id"
  names(x)[names(x) == "managing.office.code"] <- "m_code"
  names(x)[names(x) == "managing.office.name"] <- "m_office"
  names(x)[names(x) == "udn"] <- "udn"
  names(x)[names(x) == "fiscal.year"] <- "year"
  names(x)[names(x) == "target.value"] <- "target"
  names(x)[names(x) == "actual.value"] <- "actual"
  return(x)
}


#' @export make_human_readable_names
#'
make_human_readable_names <- function(x) {

  names(x)[names(x) == "ro"] <- "ro"
  names(x)[names(x) == "ou"] <- "ou"
  names(x)[names(x) == "a_code"] <- "a_code"
  names(x)[names(x) == "a_name"] <- "a_name"
  names(x)[names(x) == "ic"] <- "ic"
  names(x)[names(x) == "i_name"] <- "i_name"
  names(x)[names(x) == "ip"] <- "ip"
  names(x)[names(x) == "a_end"] <- "a_end"
  names(x)[names(x) == "a_start"] <- "a_start"
  names(x)[names(x) == "i_end"] <- "i_end"
  names(x)[names(x) == "i_start"] <- "i_start"
  names(x)[names(x) == "d_end"] <- "d_end"
  names(x)[names(x) == "d_start"] <- "d_start"
  names(x)[names(x) == "d_name"] <- "d_name"
  names(x)[names(x) == "country"] <- "country"
  names(x)[names(x) == "commodity"] <- "commodity"
  names(x)[names(x) == "a_office"] <- "a_office"
  names(x)[names(x) == "a_status"] <- "status"
  names(x)[names(x) == "tags"] <- "tags"
  names(x)[names(x) == "m_office_id"] <- "id"
  names(x)[names(x) == "managing.office.code"] <- "m_code"
  names(x)[names(x) == "managing.office.name"] <- "m_office"
  names(x)[names(x) == "udn"] <- "udn"
  names(x)[names(x) == "fiscal.year"] <- "year"
  names(x)[names(x) == "target.value"] <- "target"
  names(x)[names(x) == "actual.value"] <- "actual"
  return(x)
}

