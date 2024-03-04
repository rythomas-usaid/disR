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

  if( !exists(output_dir) ) dir.create(output_dir)

  if(!exists("disags_replace_all")) stop("disags_replace_all does not exist.")

  make_relational <- function() {
    # TODO: eg.3-x6-7-8 has mixed units
    # READ Files ####

    format <- "relational"

    print(paste("About to read", length(indicator_files), "files..."))

    # START ftf_full_join ####
    ftf_full_join <- purrr::map(indicator_files
                                , ~ disR::read_indicator(.x)) %>%
      purrr::list_rbind()

    print(paste("Files read successfully..."))

    ftf_full_join <- ftf_full_join %>%

      dplyr::mutate(across(c(ic, d1, d2, d3, d4), ~ stringr::str_to_lower(.))) %>%
      dplyr::relocate(c(d3, d4), .after=d2) %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::mutate(year = as.integer(year)) %>%
      dplyr::mutate(uic = make_uic(ic, d1, d2)
                    , .after = ic) %>%
      dplyr::mutate(across(c(d1, d2, d3, d4),
           ~ stringr::str_replace_all(stringr::str_trim(.), disags_replace_all))) %>%
      ### Remove total values for Financing ####
    dplyr::filter(!(ic == "eg.3.2-27" & ! stringr::str_detect(d1, "type of")))

    print("Finished basic cleaning...")

    ftf_full_join <- ftf_full_join %>%
      # Encode combination categories
      # https://docs.google.com/document/d/1qwfblce3uummzky7abcmcb_tx_mb7d7iekhun-5zhlu/edit
      # dplyr::mutate(group = dplyr::case_when(
      #   ic %in% c("eg.3.2-25", "eg.3.2-x18") &
      #     d1 %in% c("crop land", "cultivated pasture", "cultivated land") &
      #     d2 =="sex" ~ "cultivated land"
      #   , ic %in% c("eg.3.2-25", "eg.3.2-x18") & d1 %in% c("aquaculture") &
      #     d2 =="sex" ~  "aquaculture"
      #   , ic %in% c("eg.3.2-25", "eg.3.2-x18") & d1 %in% c("other") &
      #     d2 =="sex" ~  "other"
      #   , ic %in% c("eg.3.2-25", "eg.3.2-x18") &
      #  d1 %in% c("conservation/protected area"
      #               , "freshwater or marine ecosystems", "rangeland") &
      #  d2 =="sex" ~ "extensively managed"
      #  , .default = NA)
      #  , .after = everything()) %>%

    ### Make intuitive categories ####
    dplyr::mutate(unit = reclassify_unit(ic, d1, d2, d3, d4)
                  , typeof = reclassify_type(ic, d1, d2, d3, d4)
                  # , uid = make_uid(uic, unit, typeof)
                  , disag1 = reclassify_disag1(ic, d1, d2, d3, d4)
                  , disag2 = reclassify_disag2(ic, d1, d2, d3, d4, typeof)
                  , disag3 = reclassify_disag3(ic, d1, d2, d3, d4)
                  , disag4 = reclassify_disag4(ic, d1, d2, d3, d4))

    print("Finished standarizing disaggregates...")

    disaggregate_crosswalk <- make_disaggregate_crosswalk() %>%
      add_disaggregate_categories()

    # TODO: make these based on the disag columns that are more standardized
    ftf_full_join <- ftf_full_join %>%
      add_disaggregate_categories() %>%
      dplyr::mutate(across(-value, ~ trimws(.)))


    # ## END ftf_full_join ####
    # # create disaggregates lookup
    indicators <- ftf_full_join %>%
      dplyr::distinct(ic, uic, unit, typeof
                      #, group
                      , sex, age, size, mgmt_practice, item, outcome_output, phase, location
                      , disag1, disag2, disag3, disag4
                      , d1, d2, d3, d4) %>%
      dplyr::mutate(id = disR::return_id(.), .before = everything())

     # create target values lookup
     values <- ftf_full_join %>%
       # filter(type == "target") %>%
       dplyr::left_join(indicators) %>%
       dplyr::rename(id_ind = id)


     ### other tables #########
     # create implementing mechanisms lookup
     ims <- ftf_full_join %>%
       dplyr::distinct(ro, ou, a_code, a_name) %>%
       dplyr::mutate(id = disR::return_id(.), .before = everything())

     values <- values %>%
       dplyr::left_join(ims) %>%
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
     sex_indicators <- indicators %>%
       filter(!is.na(sex)) %>%
       select(where(function(x) any(!is.na(x))))

     age_indicators <- indicators %>%
       filter(!is.na(age)) %>%
       select(where(function(x) any(!is.na(x))))

     size_indicators <- indicators %>%
       filter(!is.na(size)) %>%
       select(where(function(x) any(!is.na(x))))

     item_indicators <- indicators %>%
       filter(!is.na(item)) %>%
       select(where(function(x) any(!is.na(x))))

     mgmt_indicators <- indicators %>%
       filter(!is.na(mgmt_practice)) %>%
       select(where(function(x) any(!is.na(x))))

     disaggregate_crosswalk <- make_disaggregate_crosswalk()



    save(ftf_full_join, disaggregate_crosswalk, ims, values, sex_indicators,
         mgmt_indicators, age_indicators, indicators, item_indicators,
         ftf_target_countries, file = paste0(output_dir, "/basic.rdata"))

    write.csv(ftf_full_join, paste0(output_dir, "/", "ftf_full_join.csv"))
    write.csv(disaggregate_crosswalk, paste0(output_dir, "/", "disaggregate_crosswalk.csv"))
    write.csv(indicators, paste0(output_dir, "/", "indicators.csv"))
    # write.csv(ims, paste0(output_dir, "/", "ims.csv"))
    # write.csv(values, paste0(output_dir, "/", "values.csv"))
    # write.csv(ftf_target_countries, paste0(output_dir, "/", "ftf_target_countries.csv") )
    # write.csv(sex_indicators, paste0(output_dir, "/", "sex_indicators.csv"))
    # write.csv(age_indicators, paste0(output_dir, "/", "age_indicators.csv"))
    # write.csv(item_indicators, paste0(output_dir, "/", "item_indicators.csv"))
    # write.csv(mgmt_indicators, paste0(output_dir, "/", "mgmt_indicators.csv"))
    # write.csv(size_indicators, paste0(output_dir, "/", "size_indicators.csv"))

    # sheet_write(ftf_full_join, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "indicators full")
    # sheet_write(indicators, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "indicators")
    # sheet_write(age_indicators, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "age indicators")
    # sheet_write(sex_indicators, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "sex indicators")
    # sheet_write(mgmt_indicators, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "mgmt indicators")
    # sheet_write(ims, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "ims")
    # sheet_write(values, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "values")
    # sheet_write(item_indicators, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "item indicators")
    # sheet_write(ftf_target_countries, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "ftf target countries")

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
      ) %>% select(-Indicator.Disagg) %>%
      #mutate(across(starts_with("Actual|Target"), is.numeric(.x))) %>%
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



