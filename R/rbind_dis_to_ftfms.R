#' parse DIS extracts into usable data
#'
#' create data set from DIS exports and FTFMS data
#' @param input_dir a directory with raw ftfms and dis (extract) data
#' @param output_dir a directory to store outputs
#' @return data.frame with combined ftfms and DIS data
#' @examples
#' #' combine_extracts()
#'
#' @export
rbind_dis_to_ftfms <- function(input_dir = "../../indicators/extract/"
                               , output_dir = "../data/") {

  # gs4_auth()
  # load("../database/extract/indicators_db.rdata")
  print("Reading DIS extract ... ")
  dat <- data.table::fread( # data.table is faster at reading large files
    paste0(input_dir
           , "DIS ENT - OU Activity Indicator Results (All Data)_20230622 Extract_Extract.csv")
    , colClasses=c("Activity.Code"="character")
    , na.strings = "") %>% as.data.frame()

  print("Renaming and columns ... ")
  # make lowercase names
  names(dat) <- tolower(names(dat))
  #dat$x <- NULL
  names(dat)[names(dat) == "reporting.organization"] <- "ro"
  names(dat)[names(dat) == "operating.unit"] <- "ou"
  names(dat)[names(dat) == "activity.code"] <- "a_code"
  names(dat)[names(dat) == "activity.name"] <- "a_name"
  names(dat)[names(dat) == "indicator.code"] <- "ic"
  names(dat)[names(dat) == "indicator.name"] <- "i_name"
  names(dat)[names(dat) == "activity.vendor"] <- "ip"
  names(dat)[names(dat) == "activity.end.date"] <- "a_end"
  names(dat)[names(dat) == "activity.start.date"] <- "a_start"
  names(dat)[names(dat) == "indicator.end.date"] <- "i_end"
  names(dat)[names(dat) == "indicator.start.date"] <- "i_start"
  names(dat)[names(dat) == "disaggregate.end.date"] <- "d_end"
  names(dat)[names(dat) == "disaggregate.start.date"] <- "d_start"
  names(dat)[names(dat) == "disaggregate.name"] <- "d_name"
  names(dat)[names(dat) == "disaggregate.country"] <- "country"
  names(dat)[names(dat) == "disaggregate.commodity"] <- "commodity"
  names(dat)[names(dat) == "activity.office"] <- "a_office"
  names(dat)[names(dat) == "activity.status"] <- "status"
  names(dat)[names(dat) == "activity.tags"] <- "tags"
  names(dat)[names(dat) == "id.managing.office"] <- "id"
  names(dat)[names(dat) == "managing.office.code"] <- "m_code"
  names(dat)[names(dat) == "managing.office.name"] <- "m_office"
  names(dat)[names(dat) == "udn"] <- "udn"
  names(dat)[names(dat) == "fiscal.year"] <- "year"
  names(dat)[names(dat) == "target.value"] <- "target"
  names(dat)[names(dat) == "actual.value"] <- "actual"

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
  # dat$first_order <- NULL
  # dat$second_order <- NULL
  # dat$third_order <- NULL
  # dat$fourth_order <- NULL

  # as_tibble(dat) %>% left_join(dcw, relationship = "many-to-one")


  ## Read FTFMS data ####
  ms1 <- openxlsx::read.xlsx(paste0(input_dir, "ftfms/FTFMS Full Dataset 2011-2019 1.xlsx"))
  ms1$DisaggregationName5 <- as.character(ms1$DisaggregationName5)
  ms2 <- openxlsx::read.xlsx(paste0(input_dir, "ftfms/FTFMS Full Dataset 2011-2019 2.xlsx"))
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

  df <- dplyr::bind_rows(dat, ms) %>%
    dplyr::mutate(uic = make_uic(tolower(ic), tolower(d1), tolower(d2)))

  # write.csv(df, paste0(output_dir, "combined_extracts.csv"))
  return(df)
}
