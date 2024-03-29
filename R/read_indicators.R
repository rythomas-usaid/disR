#' parse DIS extracts into usable data
#'
#' create data set from DIS exports and FTFMS data
#' @param ftfms_input_dir character. Directory path to ftfms Excel files
#' @param dis_input character. File path of DIS extract CSV.
#' @param output_dir character. Directory path to store outputs combined_extracts.csv and combined_extracts.Rdata
#' @return data.frame with combined ftfms and DIS data
#' @examples
#' #' combine_extracts()
#' @import tidyverse
#'
#' @export rbind_dis_to_ftfms
rbind_dis_to_ftfms <- function(dis_input, ftfms_input_dir = "../../indicators/extract/", output_dir = "../data/") {
  dis <- read_dis(input = dis_input)
  ms <- read_ftfms(input_dir = ftfms_input_dir)

  df <- dplyr::bind_rows(dis, ms) %>%
    dplyr::mutate(uic = make_uic(tolower(ic), tolower(d1), tolower(d2)))

  write.csv(df, paste0(output_dir, "combined_extracts.csv"))
  save(df, file = "../data/combined_extracts.Rdata")
  # return(df)
}


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


# @export read_export
# This function may not necessary. Test later to confirm.
# read_export <- function(x) {
#   openxlsx::read.xlsx(x) %>%
#     mutate(across(c(Target, Actual), ~ parse_double(gsub("[%USD,$ -]", "", .)))) %>%
#     rename_extract_columns()
# }

#' parse DIS extracts into usable data
#'
#' create data set from DIS exports and FTFMS data
#' @param input a directory with raw ftfms and dis (extract) data
#' @return data.frame with combined ftfms data
#' @examples
#' #' ms <- read_ftfms()
#' @import tidyverse
#'
#' @export read_ftfms
read_ftfms <- function(input = "../../indicators/extract/") {

  ms1 <- openxlsx::read.xlsx(paste0(input, "ftfms/FTFMS Full Dataset 2011-2019 1.xlsx"))
  ms1$DisaggregationName5 <- as.character(ms1$DisaggregationName5)
  ms2 <- openxlsx::read.xlsx(paste0(input, "ftfms/FTFMS Full Dataset 2011-2019 2.xlsx"))
  ms2$DisaggregationName5 <- as.character(ms2$DisaggregationName5)

  ms <- rbind(ms1, ms2)
  rm(ms1, ms2)

  ms$system <- "ms"

  names(ms) <- tolower(names(ms))

  split_columns <- strsplit(ms$indicatorname, ": ")
  result_df <- data.frame(
    ic = sapply(split_columns, `[`, 1),
    i_name = sapply(split_columns, `[`, 2)
  )
  ms$ic <- result_df$ic
  ms$i_name <- result_df$i_name
  ms$indicatorname <- NULL
  rm(result_df)

  names(ms)[names(ms) == "reportingorganization"] <- "ro"
  names(ms)[names(ms) == "bureau"] <- "m_office"
  names(ms)[names(ms) == "operatingunit"] <- "ou"
  names(ms)[names(ms) == "bureau/region" ] <- "a_code"
  names(ms)[names(ms) == "pa-id"] <- "program"
  names(ms)[names(ms) == "country.(usda)" ] <- "country"
  names(ms)[names(ms) == "disaggregationname1"] <- "ms_d1"
  names(ms)[names(ms) == "disaggregationname2" ] <- "ms_d2"
  names(ms)[names(ms) == "disaggregationname3"] <- "ms_d3"
  names(ms)[names(ms) == "disaggregationname4" ] <- "ms_d4"
  names(ms)[names(ms) == "disaggregationname5"] <- "ms_d5"
  names(ms)[names(ms) == "targetvalue"] <- "target"
  names(ms)[names(ms) == "actualvalue"] <- "actual"
  names(ms)[names(ms) == "deviationnarrative"] <- "deviation.narrative"

  #as_tibble(ms)
  ms <- ms %>%
    dplyr::mutate(dplyr::across(c(year, baselineyear),as.integer)
                  , dplyr::across(c(target, actual), as.numeric))


  # make ordered disaggregates
  ms <- ms %>%
    dplyr::mutate(d1 = first_order(ic, ms_d1, ms_d2, ms_d3, ms_d4)
                  , d2 = second_order(ic, ms_d1, ms_d2, ms_d3, ms_d4)
                  , d3 = third_order(ic, ms_d1, ms_d2, ms_d3, ms_d4)
                  , d4 = fourth_order(ic, ms_d1, ms_d2, ms_d3, ms_d4)
                  , indicator.collection.frequency = "annual")
  #distinct(ms, ic, first_order, second_order, third_order, fourth_order)

  #dat[, setdiff(names(ms), names(dat))] <- NA

  return(ms)


}


#' parse DIS extracts into usable data
#'
#' create data set from DIS exports and FTFMS data
#' @param input_dir a directory with raw dis (extract CSV) data
#' @param output_dir a directory to store outputs
#' @return data.frame with combined DIS data
#' @examples
#' #' dis <- read_dis()
#' @import tidyverse
#'
#' @export read_dis
read_dis <- function(input) {

  # load("../database/extract/indicators_db.rdata")
  print("Reading DIS extract ... ")
  # data.table is faster at reading large files
  dat <- data.table::fread(input, na.strings = "") %>% as.data.frame() %>%
    rename_extract_columns()

  # print("Changing all characters to lower case ...")
  # make sure all the values are lowercase
  # dat[, sapply(dat, is.character)] <- lapply(dat[, sapply(dat, is.character)], tolower)

  dat$system <- "dis"


  # make_uic <- function(ic) {
  #   ifelse(ic %in% c("eg.3.2-24", "eg.3.2-x17"), "eg.3.2-24/eg.3.2-x17"
  #   , ifelse(ic %in% c("eg.3.2-25","eg.3.2-x18"), "eg.3.2-25/eg.3.2-x18"
  #   , ifelse(ic %in% c("eg.3.2-26", "eg.3.2-x19"), "eg.3.2-26/eg.3.2-x19"
  #   , ifelse(ic %in% c("eg.3.2-27","eg.3.2-x6"), "eg.3.2-27/eg.3.2-x6"
  #   , ifelse(ic %in% c("eg.3.1-14/-15","eg.3.2-x22"),"eg.3.1-14/-15/eg.3.2-x22"
  #   , ic)))))
  # }

  # dat$uic <- make_uic(dat$ic)
  dat$actual <- as.numeric(dat$actual)
  dat$target <- as.numeric(dat$target)
  dat$uudn <- paste(dat$ic, dat$udn, sep = "_")
  dat$commodity <- trimws(dat$commodity)

  # dcw <- make_dis_crosswalk() #%>%
  #   dplyr::mutate(ic = stringr::str_to_lower(ic))

  # commodity_udns <- dcw %>%
  #   dplyr::filter(if_any(ends_with("order"), ~ stringr::str_detect(., "\\["))) %>%
  #   dplyr::distinct(ic, udn) %>%
  #   dplyr::left_join(
  #     dplyr::select(
  #       dplyr::filter(
  #         dat, !is.na(commodity)), ic, udn, commodity) %>%
  #       dplyr::distinct())

  dat$id <- NULL
  dis <- as_tibble(dis)
  # dat$first_order <- NULL
  # dat$second_order <- NULL
  # dat$third_order <- NULL
  # dat$fourth_order <- NULL
  return(dis)
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

prefix_ic <- function(ic, x) {
  new_name <- gsub(tolower(ic), toupper(ic), x)
  return(new_name)
}

get_names <- function(x, ic) {
    sapply(names(im_disags), function(x) strsplit(x, split = "[.]")[[1]][1] == ic)
  }
rename_ic <- function(x, ic) {
  names_lgl <- get_ic_names(x, ic)
  names(x)[names_lgl] <- prefix_ic(ic, names(x)[names_lgl])
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

  names(x)[names(x) == "d1"] <- "First order disaggregate"
  names(x)[names(x) == "d2"] <- "Second order disaggregate"
  names(x)[names(x) == "d3"] <- "Third order disaggregate"
  names(x)[names(x) == "d4"] <- "Fourth order disaggregate"
  names(x)[names(x) == "sex"] <- "Sex"
  names(x)[names(x) == "age"] <- "Age"
  names(x)[names(x) == "size"] <- "Size"

  names(x)[names(x) == "unit"] <- "Unit"
  names(x)[names(x) == "typeof"] <- "Type of unit"

  names(x)[names(x) == "fiscal.year"] <- "Fiscal Year"
  names(x)[names(x) == "year"] <- "Fiscal Year"
  names(x)[names(x) == "target"] <- "Target"
  names(x)[names(x) == "actual"] <- "Actual"
  names(x)[names(x) == "type"] <- "Type"
  names(x)[names(x) == "target.value"] <- "Target"
  names(x)[names(x) == "actual.value"] <- "Actual"
  x <- rename_ic(x, "eg")
  return(x)
}

