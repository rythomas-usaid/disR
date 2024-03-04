#' Read indicator spreadsheets modified from the format of Magnus's Google Sheets
#'
#'
#' @export read_indicator
read_indicator <- function(x, sheet = "IM Full Disaggs", format) {

  if(format == "relational") {
      openxlsx::read.xlsx(x, sheet = sheet, fillMergedCells=T) %>%
      rename_extract_columns() %>%
          #mutate(across(starts_with("Actual|Target"), is.numeric(.x))) %>%
          dplyr::filter(dplyr::if_any(c("ic", "ro", "ou"), ~ !stringr::str_detect(., "Total")))  %>%
          tidyr::pivot_longer(tidyselect::starts_with("Actual") | tidyselect::starts_with("Target")) %>%
          tidyr::separate(name, into = c("type", "year")) %>%
          dplyr::mutate(across(-value, ~ trimws(.)))

  } else if(format == "tidy") {
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
  names(x)[names(x) %in% c("reporting organization", "reporting.organization")] <- "ro"
  names(x)[names(x) %in% c("operating unit", "operating.unit")] <- "ou"
  names(x)[names(x) %in% c("pa id", "pa.id")] <- "a_code"
  names(x)[names(x) %in% c("activity code", "activity.code")] <- "a_code"
  names(x)[names(x) %in% c("activity name", "activity.name")] <- "a_name"
  names(x)[names(x) %in% c("indicator code", "indicator.code")] <- "ic"
  names(x)[names(x) %in% c("indicator name", "indicator.name")] <- "i_name"
  names(x)[names(x) %in% c("activity vendor", "activity.vendor")] <- "ip"
  names(x)[names(x) %in% c("activity end date", "activity.end.date")] <- "a_end"
  names(x)[names(x) %in% c("activity start date", "activity.start.date")] <- "a_start"
  names(x)[names(x) %in% c("indicator end date", "indicator.end.date")] <- "i_end"
  names(x)[names(x) %in% c("indicator start date", "indicator.start.date")] <- "i_start"
  names(x)[names(x) %in% c("disaggregate end date", "disaggregate.end.date")] <- "d_end"
  names(x)[names(x) %in% c("disaggregate start date", "disaggregate.start.date")] <- "d_start"
  names(x)[names(x) %in% c("disaggregate name", "disaggregate.name")] <- "d_name"
  names(x)[names(x) %in% c("disaggregate country", "disaggregate.country")] <- "country"
  names(x)[names(x) %in% c("disaggregate commodity", "disaggregate.commodity")] <- "commodity"
  names(x)[names(x) %in% c("activity office", "activity.office")] <- "a_office"
  names(x)[names(x) %in% c("activity status", "activity.status")] <- "status"
  names(x)[names(x) %in% c("activity tags", "activity.tags")] <- "tags"
  names(x)[names(x) %in% c("id managing office", "id.managing.office")] <- "id"
  names(x)[names(x) %in% c("deviation narrative", "deviation.narrative")] <- "deviation_narrative"
  names(x)[names(x) %in% c("deviation", "deviation")] <- "deviation"
  names(x)[names(x) %in% c("collection review status", "collection.review.status")] <- "collection_review_status"
  names(x)[names(x) %in% c("managing office code", "managing.office.code")] <- "m_code"
  names(x)[names(x) %in% c("managing office name", "managing.office.name")] <- "m_office"
  names(x)[names(x) %in% c("udn", "udn")] <- "udn"
  names(x)[names(x) %in% c("fiscal year", "fiscal.year")] <- "year"
  names(x)[names(x) %in% c("indicator disaggregates: 1st order", "indicator.disaggregates:.1st.order")] <- "d1"
  names(x)[names(x) %in% c("indicator disaggregates: 2nd order", "indicator.disaggregates:.2nd.order")] <- "d2"
  names(x)[names(x) %in% c("indicator disaggregates: 3rd order", "indicator.disaggregates:.3rd.order")] <- "d3"
  names(x)[names(x) %in% c("indicator disaggregates: 4th order", "indicator.disaggregates:.4th.order")] <- "d4"
  names(x)[names(x) %in% c("target value", "target.value")] <- "target"
  names(x)[names(x) %in% c("actual value", "actual.value")] <- "actual"
   return(x)
}


#' @export make_human_readable_names
#'
make_human_readable_names <- function(x) {

  names(x)[names(x) == "organization_level"] <- "Organization Level"
  names(x)[names(x) == "ro"] <- "Reporting Organization"
  names(x)[names(x) == "ou"] <- "Operating Unit"
  names(x)[names(x) == "a_code"] <- "Activity Code"
  names(x)[names(x) == "a_name"] <- "Activity Name"
  names(x)[names(x) == "ic"] <- "Indicator Code"
  names(x)[names(x) == "i_name"] <- "Indicator Name"
  names(x)[names(x) == "ip"] <- "Implementing Partner"
  names(x)[names(x) == "a_end"] <- "Activity End Date"
  names(x)[names(x) == "a_start"] <- "Activity Start Date"
  names(x)[names(x) == "i_end"] <- "Indicator End Date"
  names(x)[names(x) == "i_start"] <- "Indicator Start Date"
  names(x)[names(x) == "d_end"] <- "Disaggregated End Date"
  names(x)[names(x) == "d_start"] <- "Disaggregate Start Date"
  names(x)[names(x) == "d_name"] <- "Disaggregate Name"
  names(x)[names(x) == "country"] <- "Disaggregate Country"
  names(x)[names(x) == "commodity"] <- "Commodity"
  names(x)[names(x) == "a_office"] <- "Activity Office"
  names(x)[names(x) == "a_status"] <- "Activity Status"
  names(x)[names(x) == "tags"] <- "Tags"
  names(x)[names(x) == "deviation_narrative"] <- "Deviation Narrative"
  names(x)[names(x) == "deviation"] <- "Deviation"
  names(x)[names(x) == "collection_review_status"] <- "Collection Review Status"
  names(x)[names(x) == "m_office_id"] <- "Managing Office ID"
  names(x)[names(x) == "managing.office.code"] <- "Managing Office Code"
  names(x)[names(x) == "managing.office.name"] <- "Managing Office Name"
  names(x)[names(x) == "udn"] <- "UDN"
  names(x)[names(x) == "fiscal.year"] <- "Fiscal Year"
  names(x)[names(x) == "year"] <- "Fiscal Year"
  names(x)[names(x) == "target"] <- "Target"
  names(x)[names(x) == "actual"] <- "Actual"
  names(x)[names(x) == "target.value"] <- "Target"
  names(x)[names(x) == "actual.value"] <- "Actual"
  return(x)
}

