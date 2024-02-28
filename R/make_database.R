#' make ftf indicator database
#'
#' create a single database from all the final indicator spreadsheets.
#' @param input_dir character. The directory with all of the indicator files in the same format (xlsx by default).
#' @param output_dir character. The directory where you want to store the output files.
#' @param input_pattern character. The file format for all input files.
#' @param version character. One of c("relational", "tidy") to specify the format of the data. The default is "relational".
#' @return saves database in .rdata format in the specified directory.
#' @examples
#' make_database()
#'
#' @import googlesheets4
#' @import tidyverse
#'
#' @export make_database
make_database <- function(input_dir = "../../indicators/basic"
                          , output_dir = "../data/", version = "relational") {

  indicator_files <- list.files(paste0(input_dir), pattern = "xlsx", full.names = T)

  if(!exists("disags_replace_all")) stop("disags_replace_all does not exist.")

  if(version == "relational") {
    # TODO: eg.3-x6-7-8 has mixed units
    # READ Files ####

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
      dplyr::mutate(year = paste0("FY", year)) %>%
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
    #     d1 %in% c("conservation/protected area"
    #               , "freshwater or marine ecosystems", "rangeland") &
    #     d2 =="sex" ~ "extensively managed"
    #   , .default = NA)
    #   , .after = everything()) %>%


    ### Make intuitive categories ####
    dplyr::mutate(unit = reclassify_unit(uic, ic, d1, d2, d3, d4)
                  , typeof = reclassify_type(uic, ic, d1, d2, d3, d4)
                  , uid = make_uid(uic, unit, typeof)
                  , disag1 = reclassify_disag1(uic, ic, d1, d2, d3, d4)
                  , disag2 = reclassify_disag2(uic, ic, d1, d2, d3, d4, typeof)
                  , disag3 = reclassify_disag3(uic, ic, d1, d2, d3, d4)
                  , disag4 = reclassify_disag4(ic, d1, d2, d3, d4)) # %>% filter(uic == "eg.3.2-27/eg.3.2-x6") %>% distinct(disag1)

    print("Finished standarizing disaggregates...")

    sexes <- unique(cat_lookup$disaggregate[cat_lookup$standard == "sex"])
    # TODO: make these based on the disag columns that are more standardized
    ftf_full_join <- ftf_full_join %>%
      #### sexes ####

    mutate(
      sex = dplyr::case_when(
        d1 == "sex"  ~ d2
        , d2 == "sex"  ~ d3
        , d3 == "sex"  ~ d4)      #### ages ####
      , age = dplyr::case_when(
        d1 == "age"  ~ d2
        , d2 == "age"  ~ d3
        , d3 == "age"  ~ d4)
      #### sizes ####
      , size = dplyr::case_when(
        ic == "eg.3.2-24" & d1 == "non-smallholder producers" ~ "non-smallholder"
        , ic == "eg.3.2-24" & d1 == "smallholder producers" ~ "smallholder"
        , ic == "eg.3.2-x17" & d1 == "smallholder producers" ~ "smallholder"
        , ic == "eg.3.2-x17" & d1 == "non-smallholder producers" ~ "non-smallholder"
        , ic == "eg.3.2-26" ~ sub(" (row label implied)", "", sub(".* - ", "", d2))
        , ic == "eg.3.2-27" & d3 == "size" ~ d4
        , stringr::str_detect(ic, "eg.3-2") &
          d2 %in% c("producer: non-smallholder farmer", "producer: smallholder farmer"
                    , "producer: size disaggregate not available") ~ d2
        , stringr::str_detect(ic, "eg.3-2") &
          ! d2 %in% c("producer: non-smallholder farmer", "producer: smallholder farmer"
                      , "producer: size disaggregate not available") ~ NA)

      #### mgmt_practices ####
      , mgmt_practice = dplyr::case_when(
        disag1 == "management practice or tech type" &
          disag2 %in% cat_lookup$disaggregate[cat_lookup$standard == "mgmt_practice"] ~ disag2)

      #### items ####
      , item = dplyr::case_when(
        ic == "eg.3.2-26" ~ sub(".*: ", "", d1)
        , disag1 == "commodity" & disag2 %in% cat_lookup$disaggregate[cat_lookup$standard == "commodity"] ~ disag2
        , ic == "eg.3.2-x23" ~ d2
        , ic == "eg.3.3-x11" ~ d2
        , ic == "eg.3.3-x11" ~ d2)

      #### phase ####
      , phase = dplyr::case_when(
        (ic == "eg.3-x1" | ic == "eg.3-x9" | ic == "eg.5.2-x1") & d1 == "duration" ~ d2
        , ic == "es.5-1" & stringr::str_detect(d1, "duration") ~ d2
        , ic == "hl.9-4" & stringr::str_detect(disag2, "^degree") ~ sub(".*: ", "", disag2)
        , ic == "resil-1" & stringr::str_detect(disag2, "total", negate = TRUE) ~ disag2)

      #### location ####
      , location = dplyr::case_when(
        (ic == "eg.3-x1" | ic == "eg.3-x9") & d1 == "location" ~ d2
        , ic == "eg.10.4-7" & d2 == "location" ~ d3
        , ic == "hl.8.2-2" & d1 == "location" ~ d2
        , ic == "hl.8.2-5" ~ d1)

      #### outcomes_output ####
      , outcome_output = dplyr::case_when(
        uic == "cbld-9" & stringr::str_detect(d2, "improved performance") ~ "improved performance"
        , uic == "cbld-9" & stringr::str_detect(d2, "capacity development support") ~ "capacity development support"
        , ic == "eg.3.2-x41" ~ d2
        , (ic == "eg.10.4-7" | ic == "eg.10.4-8") & stringr::str_detect(d2, "type") ~ d3
        , ic == "es.5-1" & stringr::str_detect(d1, "type") ~ d2
        , ic == "hl.9-1" & stringr::str_detect(d1, "intervention") ~ d2)) %>%

      #### total ####
      # , is_total = dplyr::case_when(
      #   # This is not correct. Need to add logical for "value" for eg.3.2-26, among others.
      #   !is.na(sex) ~ TRUE
      #   , is.na(sex) & !is.na(age) ~ TRUE
      #   , is.na(sex) & is.na(age) ~ TRUE
      # )
      # , .after = disag4) %>%

      relocate(d1, d2, d3, d4, .after = sex) %>%
      #select(-c(disag1, disag2, disag3, disag4)) %>%
      dplyr::mutate(across(-value, ~ trimws(.)))


    ## END ftf_full_join ####
    # create disaggregates lookup
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

    save(ftf_full_join, indicators, values, ims, ftf_target_countries

         , sex_indicators, age_indicators, item_indicators, mgmt_indicators

         , size_indicators, disaggregate_crosswalk, file = paste0(output_dir, "/basic.rdata"))

    write.csv(ftf_full_join, paste0(output_dir, "/", "ftf_full_join.csv"))
    write.csv(indicators, paste0(output_dir, "/", "indicators.csv"))
    write.csv(ims, paste0(output_dir, "/", "ims.csv"))
    write.csv(values, paste0(output_dir, "/", "values.csv"))
    write.csv(ftf_target_countries, paste0(output_dir, "/", "ftf_target_countries.csv") )
    write.csv(sex_indicators, paste0(output_dir, "/", "sex_indicators.csv"))
    write.csv(age_indicators, paste0(output_dir, "/", "age_indicators.csv"))
    write.csv(item_indicators, paste0(output_dir, "/", "item_indicators.csv"))
    write.csv(mgmt_indicators, paste0(output_dir, "/", "mgmt_indicators.csv"))
    write.csv(size_indicators, paste0(output_dir, "/", "size_indicators.csv"))

    # sheet_write(ftf_full_join, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "indicators full")
    # sheet_write(indicators, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "indicators")
    # sheet_write(age_indicators, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "age indicators")
    # sheet_write(sex_indicators, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "sex indicators")
    # sheet_write(mgmt_indicators, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "mgmt indicators")
    # sheet_write(ims, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "ims")
    # sheet_write(values, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "values")
    # sheet_write(item_indicators, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "item indicators")
    # sheet_write(ftf_target_countries, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "ftf target countries")

  } else if(version == "tidy") {
    # READ Files ####
    indicator_files <- list.files(paste0(input_dir), pattern = "xlsx", full.names = T)
    indicator_files <- indicator_files[! str_detect(indicator_files, "-x")]
    indicator_files <- indicator_files[! str_detect(indicator_files, "EG.3.2-28")]

    worksheet_names <- c("RO Total", "RO Full Disaggs", "OU Total",
                     "OU Full Disaggs", "IM Total", "IM Full Disaggs")
    paste0(worksheet_names, collapse = ", ")

    print(paste("About to read--", paste0(worksheet_names, collapse = ", ")
                , "--worksheets for", length(indicator_files), "files..."))

    mapper <- expand_grid(indicator_files, worksheet_names)

    # START ftf_tidy_version ####
    ftf_tidy_version <- purrr::map2(.x = mapper$indicator_files, .y = mapper$worksheet_names
                                , ~ disR::read_indicator(.x, sheet = .y, version = version)) %>%
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

    print("Tidy database complete. Writing files ...")
    save(ftf_tidy_version, file = paste0(output_dir, "/tidy_version.rdata"))
    write.csv(ftf_tidy_version, paste0(output_dir, "/ftf_tidy_Version.csv"))
    }

}
#
#
# This allows to troubleshoot the column names for all files.
# test <- purrr::map2(.x = mapper$indicator_files, .y = mapper$worksheet_names
#             , ~ if(.y %in% openxlsx::getSheetNames(.x)) {
#               colnames(openxlsx::read.xlsx(.x, .y))
#               }
#             )


