#' make ftf indicator database
#'
#' create a single database from all the final indicator spreadsheets.
#' @param input_dir character. The directory with all of the indicator files in the same format (xlsx by default).
#' @param output_dir character. The directory where you want to store the output files.
#' @param input_pattern character. The file format for all input files.
#' @param format character. One or both (default) of c("relational", "tidy") to specify the format of the data. The default is "relational".
#' @param version character. The folder where to locate the output_dir. The default is `sys.Date()`.
#' @return saves database in .rdata format in the specified directory.
#' @examples
#' make_database()
#'
#' @import googlesheets4
#' @import tidyverse
#'
#' @export make_database
make_database <- function(input_dir = "../../indicators/basic"
                          , output_dir = "../data/", format = c("relational", "tidy"), version = Sys.Date()) {

  indicator_files <- list.files(paste0(input_dir), pattern = "xlsx", full.names = T)
  output_dir <- paste0(output_dir, version)
  if(!exists(paste0(output_dir))) dir.create(output_dir)
  if(!exists("disags_replace_all")) stop("disags_replace_all does not exist.")

  make_relational <- function() {
    # TODO: eg.3-x6-7-8 has mixed units
    # READ Files ####
    format <- "relational"

    print(paste("About to read", length(indicator_files), "files..."))

    # START ftf_full_join ####
    ftf_full_join <- purrr::map(
      indicator_files, ~ disR::read_indicator(.x, format = format)
      ) %>% purrr::list_rbind()

    print(paste("Files read successfully..."))

    ftf_full_join <- ftf_full_join %>%
      # fix 2018 and 2019 FTFMS values when there was no EG.3-2_OULevel
      # about 25 OUS reported OU level values under EG.3-2
      # They all used the activity name 'High-level indicators - *OU NAME*'
      mutate(ic = case_when(
        year %in% 2018:2019 & stringr::str_detect(a_name, "High-level indicators") ~ "EG.3-2_OULevel"
        , .default = ic)) %>%
      dplyr::mutate(across(c(ic, d1, d2, d3, d4), ~ stringr::str_to_lower(.))) %>%
      dplyr::relocate(c(d3, d4), .after=d2) %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::mutate(year = as.integer(year)) %>%
      dplyr::mutate(uic = make_uic(ic, d1, d2), .after = ic) %>%
      dplyr::mutate(across(c(d1, d2, d3, d4),
           ~ stringr::str_replace_all(stringr::str_trim(.), disags_replace_all))) %>%
      ### Remove total values for Financing ####
    dplyr::filter(!(ic == "eg.3.2-27" & ! stringr::str_detect(d1, "type of")))

    print("Finished basic cleaning...")

    disaggregate_crosswalk <- make_disaggregate_crosswalk() %>%
      add_disaggregate_categories()
    # TODO: make these based on the disag columns that are more standardized
    ftf_full_join <- ftf_full_join %>%
      add_disaggregate_categories() %>%
      dplyr::mutate(across(-value, ~ trimws(.)))

    ## END ftf_full_join ####
    # create disaggregates lookup
    indicators <- ftf_full_join %>%
      dplyr::distinct(ic, uic, unit, typeof
                      , sex, age, size, mgmt_practice, item
                      , outcome_output, phase, location
                      , disag1, disag2, disag3, disag4
                      , d1, d2, d3, d4) %>%
      dplyr::mutate(id = disR::return_id(.), .before = everything())

     # create target values lookup
     values <- ftf_full_join %>% dplyr::left_join(indicators) %>% dplyr::rename(id_ind = id)

     ### Other tables #########
     # create implementing mechanisms lookup
     ims <- ftf_full_join %>%  dplyr::distinct(ro, ou, a_code, a_name) %>%
       dplyr::mutate(id = disR::return_id(.), .before = everything())

     values <- values %>% dplyr::left_join(ims) %>%
       dplyr::select(id_im = id, id_ind, type, year, value) %>%
       dplyr::relocate(c(id_im, id_ind), .before=tidyselect::everything()) %>%
       dplyr::mutate(id = disR::return_id(.), .before = tidyselect::everything())


     ftf_target_countries <- data.frame(
       country = c("Bangladesh","Democratic Republic of the Congo"
                   , "Ethiopia","Ghana","Guatemala","Honduras","Kenya","Liberia"
                   , "Madagascar","Malawi", "Mali","Mozambique", "Nepal","Niger"
                   , "Nigeria","Rwanda", "Senegal","Tanzania", "Uganda","Zambia")
       , ftf_target = TRUE)

     ### specific indicator tables ################
     sex_indicators <- indicators %>% filter(!is.na(sex)) %>%
       select(where(function(x) any(!is.na(x))))

     age_indicators <- indicators %>%  filter(!is.na(age)) %>%
       select(where(function(x) any(!is.na(x))))

     size_indicators <- indicators %>% filter(!is.na(size)) %>%
       select(where(function(x) any(!is.na(x))))

     item_indicators <- indicators %>% filter(!is.na(item)) %>%
       select(where(function(x) any(!is.na(x))))

     mgmt_indicators <- indicators %>% filter(!is.na(mgmt_practice)) %>%
       select(where(function(x) any(!is.na(x))))

     disaggregate_crosswalk <- make_disaggregate_crosswalk()

     print(paste0("Relational database complete. Writing files to", output_dir," ..."))
     save(ftf_full_join, disaggregate_crosswalk, ims, values, sex_indicators,
          mgmt_indicators, age_indicators, indicators, item_indicators,
          ftf_target_countries, file = paste0(output_dir, "/basic.rdata"))

    write.csv(ftf_full_join, paste0(output_dir, "/", "ftf_full_join.csv"))
    write.csv(disaggregate_crosswalk, paste0(output_dir, "/", "disaggregate_crosswalk.csv"))
    write.csv(indicators, paste0(output_dir, "/indicators.csv"))
    # write.csv(ims,                  paste0(output_dir, "/ims.csv"))
    # write.csv(values,               paste0(output_dir, "/values.csv"))
    # write.csv(ftf_target_countries, paste0(output_dir, "/ftf_target_countries.csv") )
    # write.csv(sex_indicators,       paste0(output_dir, "/sex_indicators.csv"))
    # write.csv(age_indicators,       paste0(output_dir, "/age_indicators.csv"))
    # write.csv(item_indicators,      paste0(output_dir, "/item_indicators.csv"))
    # write.csv(mgmt_indicators,      paste0(output_dir, "/mgmt_indicators.csv"))
    # write.csv(size_indicators,      paste0(output_dir, "/size_indicators.csv"))
    print("make_relational() complete!")

  }

  make_tidy <- function() {
    format <- "tidy"
    # READ Files ####
    indicator_files <- list.files(paste0(input_dir), pattern = "xlsx", full.names = T)
    indicator_files <- indicator_files[! stringr::str_detect(indicator_files, "-x")]
    # indicator_files <- indicator_files[! stringr::str_detect(indicator_files, "EG.3.2-28")]

    worksheet_names <- c("RO Total", "RO Full Disaggs", "OU Total",
                     "OU Full Disaggs", "IM Total", "IM Full Disaggs")
    paste0(worksheet_names, collapse = ", ")

    print(paste("About to read--", paste0(worksheet_names, collapse = ", ")
                , "--worksheets for", length(indicator_files), "files..."))

    mapper <- tidyr::expand_grid(indicator_files, worksheet_names)

    # START ftf_tidy_format ####
    ftf_tidy <- purrr::map2(.x = mapper$indicator_files, .y = mapper$worksheet_names
                                , ~ disR::read_indicator(.x, sheet = .y, format = format)) %>%
      purrr::list_rbind() %>%
      mutate(ic = case_when(
        year %in% 2018:2019 & stringr::str_detect(a_name, "High-level indicators|High-level Indicators") ~ "EG.3-2_OULevel"
        , .default = ic)) %>%
      dplyr::filter(dplyr::if_any(c("ic", "ro", "ou"), ~ !stringr::str_detect(., "Total")))  %>%
      tidyr::pivot_longer(tidyselect::starts_with("Actual") | tidyselect::starts_with("Target")) %>%
      tidyr::separate(name, into = c("type", "year")) %>%
      dplyr::mutate(across(-value, ~ trimws(.))) %>%
      filter(!is.na(value)) %>% relocate(organization_level, .before=everything())

    print(paste0("Tidy database complete. Writing files to", output_dir," ..."))
    save(ftf_tidy, file = paste0(output_dir, "/ftf_tidy.rdata"))
    write.csv(ftf_tidy, paste0(output_dir, "/ftf_tidy.csv"))

  }

  if(all(c("relational", "tidy") %in% format)) {
    make_relational()
    make_tidy()
  } else if(format == "relational") {
    make_relational()
  } else if(format == "tidy") {
    make_tidy()
  }
}

#' @export make_sqlite_database
make_sqlite_database <- function(x, d = Sys.Date(), overwrite = TRUE) {
  # "../indicators/DIS ENT - OU Activity Indicator Results (All Data)_20240528.csv"
  data <- data.table::fread(x, na.strings = c("", "-")) %>% as.data.frame() %>%
    rename_extract_columns() %>%
    dplyr::filter(collection_period_frequency  == "Annual")

  pt_udns <- tibble::tribble(
    ~ic,        ~udn,
    "EG.3.2-25",  "3.1.3.12",
    "EG.3.2-25",  "3.2.3.12",
    "EG.3.2-26",         "3",
    "EG.3.2-27", "3.5.1.2.1",
    "EG.3.2-27", "3.6.1.2.1",
    "EG.3.2-27", "3.7.1.2.1",
    "EG.3.2-27", "3.5.1.2.2",
    "EG.3.2-27", "3.6.1.2.2",
    "EG.3.2-27", "3.7.1.2.2",
    "EG.3.2-27", "3.5.2.2.1",
    "EG.3.2-27", "3.6.2.2.1",
    "EG.3.2-27", "3.7.2.2.1",
    "EG.3.2-27", "3.5.2.2.2",
    "EG.3.2-27", "3.6.2.2.2",
    "EG.3.2-27", "3.7.2.2.2",
    "EG.3.1-15",         "3",
    "HL.9.1-d",         "3",
    "EG.3.1-14",     "3.1.2"
  )
  drv <- DBI::dbDriver("SQLite")
  tfile <- paste0("../data/", d)
  if(!dir.exists(tfile)) dir.create(tfile)
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = paste0(tfile, "/dis_extract.db"))
  DBI::dbWriteTable(con, "extract", as.data.frame(data), overwrite = overwrite)
  DBI::dbWriteTable(con, "pt_udns", as.data.frame(pt_udns), overwrite = overwrite)
  DBI::dbDisconnect(con)
}
