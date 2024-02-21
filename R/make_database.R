#' make ftf indicator database
#'
#' create a single database from all the final indicator spreadsheets.
#' @param input_dir the directory with all of the indicator files in the same format (xlsx by default).
#' @param output_dir the directory where you want to store the output files.
#' @param input_pattern the file format for all input files.
#' @return saves database in .rdata format in the specified directory.
#' @examples
#' make_database()
#'
#' @import googlesheets4
#'
#' @export make_database
make_database <- function(input_dir = "../../indicators/basic"
                          , output_dir = "../data/") {

  if(!exists("disags_replace_all")) stop("disags_replace_all does not exist.")


  # TODO: eg.3-x6-7-8 has mixed units
  # READ Files ####
    indicator_files <- list.files(paste0(input_dir), pattern = "xlsx", full.names = T)

    print(paste("About to read", length(indicator_files), "files..."))

  # reclassify_unit <- function(uic, ic, d1, d2, d3, d4) {
  #   dplyr::case_when(
  #     ################### producers and participants are all people
  #     ic %in% c("eg.3.2-24", "eg.3.2-x17", "eg.3-2", "eg.3-2_oulevel") ~ "people"
  #
  #     ################### hectares
  #     , uic == "eg.3.2-25/eg.3.2-x18" ~ "area"
  #
  #     ################### sales
  #     , uic == "eg.3.2-26/eg.3.2-x19" & stringr::str_detect(d4, "value") ~ "dollars"
  #     , uic == "eg.3.2-26/eg.3.2-x19" & stringr::str_detect(d4, "number") ~ "people"
  #     , uic == "eg.3.2-26/eg.3.2-x19" & stringr::str_detect(d4, "volume") ~ "metric tons"
  #
  #     ################### financing
  #     , ic == "eg.3.2-27" & stringr::str_detect(d2, "value") ~ "dollars"
  #     , ic == "eg.3.2-27" & stringr::str_detect(d2, "number") ~  "people"
  #     , ic == "eg.3.2-27" & stringr::str_detect(d2, "volume") ~ "metric tons"
  #
  #     ################### other indicators
  #     # dollars
  #     , ic == "eg.3.2-x6" | ic == "eg.3.1-14/-15" | ic == "eg.3.2-x22" |
  #       ic == "eg.3.2-x23" & d1 == "value of exports (in usd)" ~ "dollars"
  #
  #
  #     ################### people
  #     , ic == "eg.3.3-10" & stringr::str_detect(d1, "number") ~ "people"
  #     , ic == "eg.3.3-10" & stringr::str_detect(d1, "percentage") | ic == "cbld-9" ~ "percent"
  #     , ic == "eg.3.2-x1" | ic == "eg.4.2-7" | ic == "eg.5.2-x1" |
  #       ic == "eg.10.4-7" | ic == "eg.10.4-8" | ic == "eg.11-x6" |
  #       ic == "gndr-2" | ic == "hl.8.2-2" | ic == "hl.9-4" ~ "people"
  #     , ic == "eg.3.3-x11"  & stringr::str_detect(d1, "number") ~ "people"
  #     , ic == "hl.8.2-5" ~ "households"
  #
  #     ################### metric tons
  #     , ic == "eg.3.2-x23" & d1 == "volume of exports (mt)" ~ "metric tons"
  #     , ic == "eg.3.3-x11"  & stringr::str_detect(d1, "quantity") ~ "metric tons"
  #     , ic == "eg.3-x6-7-8" ~ d1
  #
  #   )
  # }
  #
  # household_types <- c("school-aged children", "household members",
  #                      "parents/caregivers")
  # reclassify_type <- function(uic, ic, d1, d2, d3, d4) {
  #   dplyr::case_when(
  #     # headcount
  #     stringr::str_starts(d1, "type of") ~ sub("type of ", "", d1) # this gets most of the financing values
  #     # , uic %in% c("eg.3-2", "eg.3-2_oulevel") &
  #     #   stringr::str_detect(d2, "government|civil society") ~ "organization"
  #     # , uic %in% c("eg.3-2", "eg.3-2_oulevel") & stringr::str_detect(d2, "firm") ~ "firm"
  #     # , uic %in% c("eg.3-2", "eg.3-2_oulevel") &
  #     #    d2 == "type of individual disaggregates not available" ~ "disaggregates not available"
  #     # , uic %in% c("eg.3-2", "eg.3-2_oulevel") &
  #     #    d2 == "type of individual not applicable" ~ "not applicable"
  #     , uic %in% c("eg.3-2", "eg.3-2_oulevel") & stringr::str_starts(d2, "producer") ~ "producer"
  #     , uic %in% c("eg.3-2", "eg.3-2_oulevel") & stringr::str_starts(d2, "laborer") ~ "laborer"
  #     , uic %in% c("eg.3-2", "eg.3-2_oulevel") & d2 %in% household_types ~ "household member"
  #     , ic == "eg.3-x1" & stringr::str_detect(d1, "type") ~ sub(": .*", "", d2)
  #
  #     # producers
  #     , ic == "eg.3.2-24" & stringr::str_detect(d1, "producers") ~ "producer"
  #     , ic == "eg.3.2-24" & stringr::str_detect(d1, "producers", negate=TRUE) ~ sub(".*people in ", "", d1)
  #     , ic == "eg.3.2-x17" & stringr::str_detect(d1, "producers") ~ "producer"
  #     , ic == "eg.3.2-x17" & stringr::str_detect(d1, "producers", negate=TRUE) ~ sub(".*people in ", "", d1)
  #
  #     # hectares
  #     , uic == "eg.3.2-25/eg.3.2-x18" ~ d1
  #
  #     # sales
  #     , uic == "eg.3.2-26/eg.3.2-x19" &
  #       stringr::str_detect(d1, "type of product") &
  #       stringr::str_detect(d4, "number") ~ "producer"
  #     , uic == "eg.3.2-26/eg.3.2-x19" &
  #       stringr::str_detect(d1, "type of service") &
  #       stringr::str_detect(d4, "number") ~ "service provider"
  #     , uic == "eg.3.2-26/eg.3.2-x19" &
  #       stringr::str_detect(d1, "type of product") ~ "product"
  #     , uic == "eg.3.2-26/eg.3.2-x19" &
  #       stringr::str_detect(d1, "type of service") &
  #       stringr::str_detect(d4, "number", negate = TRUE) ~ "service"
  #     # , uic == "eg.3.2-26/eg.3.2-x19" &
  #     #   stringr::str_detect(d1, "commodity") ~ "commodity"
  #
  #     # financing
  #     , ic == "eg.3.2-27" ~  sub(".*: ", "", d1)
  #     , ic == "eg.3.2-x6" ~ "financing accessed: cash debt"
  #
  #     # other indicators
  #     , uic == "cbld-9" | uic == "eg.3.1-1" ~ d1
  #     , ic == "eg.3.1-14/-15" ~ sub(" amount", "", d1)
  #     , ic == "eg.3.2-x22" ~ "capital investment"
  #     , ic == "eg.3.3-10" & stringr::str_detect(d1, "number") ~ "participants"
  #     , ic == "eg.3.3-10" & stringr::str_detect(d1, "number") ~ "consuming diet of minimum diversity"
  #     , ic == "eg.3.2-x22" ~ "privte sector capital investment"
  #     , ic == "eg.3.2-25" & stringr::str_detect(d1, "account-policy") ~ "account-policy"
  #     , ic == "eg.3.2-x1" & !str_detect(d1, "sex") ~ d1
  #     , ic == "eg.3.2-x27" & stringr::str_detect(d1, "type of") ~ d2
  #     , ic == "eg.3.2-x29" & d1 == "type of organization" ~ d2
  #     , ic == "eg.3.2-x24" & stringr::str_detect(d1, "type of risk reducing practice") ~ "risk reducing practice"
  #     , ic == "eg.3.2-x37" & d1 == "msme type" ~ "msme"
  #     , ic == "eg.3.2-x41" ~ d1
  #     , ic == "eg.4.2-7" & stringr::str_detect(d1, "product type") ~ d2
  #     , ic == "eg.5.2-x1" & d1 == "type of firm" ~ paste(d2, "firm")
  #     , ic == "eg.10.4-7" | ic == "eg.10.4-8" ~ sub(".*: ", "", d1)
  #     , ic == "es.5-1" & stringr::str_detect(d1, "type") ~ "asset strengthened"
  #     , ic == "hl.8.2-5" ~ "household"
  #     , ic == "resil-1" ~ sub(" - .*", "", d1)
  #
  #   )
  # }
  #
  # make_uid <- function(uic, unit, typeof) {
  #   dplyr::case_when(is.na(unit) & is.na(typeof) ~ uic
  #             , !is.na(unit) & is.na(typeof) ~ paste0(uic, "_", unit)
  #             ,  is.na(unit) & !is.na(typeof) ~ paste0(uic, "_", typeof)
  #             , !is.na(unit) & !is.na(typeof) ~ paste0(uic, "_", unit, "_", typeof)
  #   )
  # }
  #
  #
  # # This should be imported from helpers.R
  # # make_uic <- function(ic, d1, d2) {
  # #   warning("make_dis_uic() can only be applied after combining ftfms and DIS data")
  # #   dplyr::case_when(
  # #     ic == "eg.3.1-14/-15" & d1 == "private sector partner leveraged amount" ~ "eg.3.1-14/-15/eg.3.2-x22"
  # #     , ic == "eg.3.2-x22" ~ "eg.3.1-14/-15/eg.3.2-x22"
  # #
  # #     , ic == "eg.3.2-x17" & d1 == "producers" ~ "eg.3.2-24/eg.3.2-x17"
  # #     , ic == "eg.3.2-24" & stringr::str_ends(d1, "producers") ~ "eg.3.2-24/eg.3.2-x17"
  # #
  # #     , ic == "eg.3.2-25" & d1 %in% c("type of hectare: crop land"
  # #                                     , "type of hectare: cultivated pasture") ~ "eg.3.2-25/eg.3.2-x18"
  # #     , ic == "eg.3.2-x18" ~ "eg.3.2-25/eg.3.2-x18"
  # #
  # #     , ic == "e.g.3.2-26" & d2 == "producer - smallholder" ~ "eg.3.2-26/eg.3.2-x19"
  # #     , ic == "eg.3.2-x19" ~ "eg.3.2-26/eg.3.2-x19"
  # #     , ic %in% c("eg.3.2-27","eg.3.2-x6") ~ "eg.3.2-27/eg.3.2-x6"
  # #     , .default = ic)
  # # }


  # START ftf_full_join ####
  ftf_full_join <- purrr::map(indicator_files
                              , ~ disR::read_indicator(.x)) %>%
    purrr::list_rbind()

  print(paste("Files read successfully..."))

  ftf_full_join <- ftf_full_join %>%

    # dplyr::mutate(across(-value, ~ stringr::str_to_lower(.))) %>%
    dplyr::relocate(c(d3, d4), .after=d2) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(year = paste0("FY", year)) %>%
    dplyr::mutate(uic = make_uic(ic, d1, d2)
      , .after = ic) %>% #filter(uic == "eg.3.2-27/eg.3.2-x6") %>% distinct(d1)
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
           , disag2 = sub(" years", "", reclassify_disag2(uic, ic, d1, d2, d3, d4, typeof))
           , disag3 = reclassify_disag3(uic, ic, d1, d2, d3, d4)
           , disag4 = reclassify_disag4(ic, d1, d2, d3, d4)) # %>% filter(uic == "eg.3.2-27/eg.3.2-x6") %>% distinct(disag1)

  print("Finished standarizing disaggregates...")

  sexes <- unique(cat_lookup$disaggregate[cat_lookup$standard == "sex"])
  # TODO: make these based on the disag columns that are more standardized
  ftf_full_join <- ftf_full_join %>%
           #### sexes ####

  mutate(
    sex = dplyr::case_when(
      disag1 == "sex"  ~ disag2
      , disag2 == "sex"  ~ disag3
      , disag3 == "sex"  ~ disag4
      # , ic == "eg.3.2-x22" ~ disag1

      , ic == "eg.3.2-x1" ~ d2
      , ic == "eg.3.2-x37" & d1 == "sex" ~ d2
      , ic == "eg.3-x6-7-8" ~ d2
      , ic == "gndr-2" & stringr::str_detect(d1, "female") ~ "female"
      , ic == "gndr-2" & stringr::str_detect(d1, "total") ~ "other"
      , ic == "hl.9-1" & disag1 == "sex" ~ disag2
      , ic == "hl.9-2" & disag1 == "sex" ~ disag2
      , ic == "hl.9-4" & disag1 == "sex" ~ disag2
      , ic == "hl.9-x1" | ic == "hl.9-x15" ~ disag2
      , ic == "eg.11-x6" ~ d1)
    #### ages ####
    , age = dplyr::case_when(
      disag1 == "age" ~ disag2
      , disag2 == "age" ~ disag3
      , disag4 == "age" ~ disag4
      , ic == "hl.9-3" & disag1 == "age" ~ disag2)
    #### sizes ####
    , size = dplyr::case_when(
      ic == "eg.3.2-24" & d1 == "non-smallholder producers" ~ "non-smallholder"
      , ic == "eg.3.2-24" & d1 == "smallholder producers" ~ "smallholder"
      , ic == "eg.3.2-x17" & d1 == "smallholder producers" ~ "smallholder"
      , ic == "eg.3.2-x17" & d1 == "non-smallholder producers" ~ "non-smallholder"
      , ic == "eg.3.2-26/eg.3.2-x19" ~ sub(" (row label implied)", "", sub(".* - ", "", disag3))
      , ic == "eg.3.2-27" & disag1 == "size" ~ disag2
      , stringr::str_detect(ic, "eg.3-2") &
        d2 %in% c("producer: non-smallholder farmer", "producer: smallholder farmer"
                  , "producer: size disaggregate not available") ~ d2
      , stringr::str_detect(ic, "eg.3-2") &
        ! d2 %in% c("producer: non-smallholder farmer", "producer: smallholder farmer"
                  , "producer: size disaggregate not available") ~ "not applicable")

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
        , ic == "hl.9-1" & stringr::str_detect(d1, "intervention") ~ d2)

      #### total ####
      , is_total = dplyr::case_when(
        !is.na(sex) ~ TRUE
        , is.na(sex) & !is.na(age) ~ TRUE
        , is.na(sex) & is.na(age) ~ TRUE
      )
      , .after = disag4) %>%

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
    country = c("bangladesh","democratic republic of the congo"
                , "ethiopia","ghana","guatemala","honduras","kenya","liberia"
                , "madagascar","malawi", "mali","mozambique", "nepal","niger"
                , "nigeria","rwanda", "senegal","tanzania", "uganda","zambia")
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

  save(ftf_full_join, indicators, values, ims, ftf_target_countries

       , sex_indicators, age_indicators, item_indicators, mgmt_indicators

       , size_indicators, file = paste0(output_dir, "/basic.rdata"))

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

}
