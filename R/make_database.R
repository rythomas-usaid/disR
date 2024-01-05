#' make ftf indicator database
#'
#' create a single database from all the final indicator spreadsheets.
#' @param input_dir the directory with all of the indicator files in the same format (xlsx by default).
#' @param output_dir the directory where you want to store the output files.
#' @param db the database format ("magnus" by default, or "extract")
#' @param input_pattern the file format for all input files.
#' @return saves database in .rdata format in the specified directory.
#' @examples
#' make_database()
#' make_database(input_dir = "../../indicators/"
#'               , output_dir = "../downloads/"
#'               , db = "extract")
#'
#' @import googlesheets4
#'
#' @export
make_database <- function(input_dir = "../../indicators/basic/"
                          , output_dir = "../data/"
                          , db = "basic") {

  #TODO: eg.3-x6-7-8 has mixed units
  source("R/standard_disaggregates.R")

    return_id <- function(row) {
    l <- round(log10(nrow(row))+1,0)
    id <- str_pad(rownames(row), width = l, pad = "0", side = "left")
    return(id)
    }

    ## join udn formulas #################
    list_udns <- function(x) {
      stringr::str_replace_all(x, "[()/+*]|100|manual|na", " ") %>%
        stringr::str_extract_all(stringr::boundary("word"))
    }

    # reclassify disaggregates
    reclassify_disag1 <- function(uic, ic, d1, d2 , d3, d4) {
      dplyr::case_when(
        # headcount
        uic %in% c("eg.3-2", "eg.3-2_oulevel") ~ d1 # this is the default
        , ic == "eg.3.2-x23" ~ "commodity"
        # producers
        , uic == "eg.3.2-24/eg.3.2-x17"  ~ d2
        # hectares
        , uic == "eg.3.2-25/eg.3.2-x18" ~ d2
        # sales
        , uic == "eg.3.2-26/eg.3.2-x19" ~ d3
        # financing
        , ic == "eg.3.2-27" ~ d3
        # assuming below the "sex of recipient(s)" has already been replaced with "sex"
        # , ic == "eg.3.2-x6" & d1 != "sex" ~ NA
        #, ic == "eg.3.2-x22" ~ str_remove(d1, typeof) # this is the default
        # , ic == "eg.3.2-x27" & d1 == "type of organization" ~ NA
        # , ic == "eg.3.2-x29" & d1 == "type of organization" ~ NA
        , ic == "eg.3.2-x32" ~ "sex"
        # , ic == "eg.3.2-x24" & d1 == "type of risk reducing practice" ~ NA
        , ic == "eg.10.4-7" & d2 != "type of documentation" ~ d2
        , ic == "eg.10.4-8" & d2 != "tenure type" ~ d2
        , ic == "eg.11-x6" ~ "sex"
        , ic == "hl.8.2-5" ~ "location"
        , ic == "hl.9-2" ~ "sex"
        , ic == "hl.9-3" & d1 == "age" ~ d1
        , ic == "hl.9.3" & d1 != "age" ~ NA
        , ic == "hl.9-4" & d1 == "sex" ~ d1
        , ic == "hl.9-4" & d1 == "sex" ~ NA
        , ic == "hl.9-x1" | ic == "hl.9-x15" ~ "sex"
        , ic == "eg.3.3-x11" ~ "commodity"
        #, .default = d1
      )
    }

   # headcount_types <- c("producer", "organization participant", "household")
    reclassify_disag2 <- function(uic, ic, d1, d2, d3, d4, typeof) {
      dplyr::case_when(
        # headcount
        uic %in% c("eg.3-2", "eg.3-2_oulevel") ~ str_replace_all(
          d2, pattern = c("household " = ""
                          , "people in " = ""
                          , "producer: " = ""
                          , "type of individual " = ""))
        , uic %in% c("eg.3-2", "eg.3-2_oulevel") & d1 == "sex" ~ d2

        # producers
        , uic == "eg.3.2-24/eg.3.2-x17"  ~ d3
        # hectares
        , uic == "eg.3.2-25/eg.3.2-x18"  ~ d3
        # sales
        , uic == "eg.3.2-26/eg.3.2-x19" ~ sub(" -.*", "", d4)
        # financing
        , uic == "eg.3.2-27/eg.3.2-x6" ~ d4
        , ic == "eg.3.2-x32" ~ d1
        , ic == "eg.11-x6" ~ d2
        , ic == "hl.8.2-5" ~ d1
        , ic == "hl.9-2" ~ d1
        , ic == "hl.9-x1" | ic == "hl.9-x15" ~ d2
        , .default = d2
      )
    }

    reclassify_disag3 <- function(uic, ic, d1, d2, d3, d4) {
      dplyr::case_when(
        # procuders
        uic == "eg.3.2-24/eg.3.2-x17"  ~ d1
        # hectares
        , uic == "eg.3.2-25/eg.3.2-x18"  ~ d1
        # sales
        , uic == "eg.3.2-26/eg.3.2-x19" ~ d2
        # financing
        , uic == "eg.3.2-27/eg.3.2-x6" ~ sub(".*: ", "", d1)
        , ic == "hl.8.2-5" & str_detect(d2, "both") ~ "water and soap available"
        , ic == "hl.8.2-5" & str_detect(d2, "both", negate=TRUE) ~ "covered households"
        , .default = d3
      )
    }

    reclassify_disag4 <- function(uic, ic, d1, d2, d3, d4) {
      dplyr::case_when(
        uic == "eg.3.2-26/eg.3.2-x19" ~ trimws(sub(".*: ", "", d1))
        , uic == "eg.3.2-27/eg.3.2-x6" ~ sub(" of .*", "", d2)
        , .default = d4
        )
    }

    # REOGRANIZATION FUNCTIONS ####
    # reclassify disaggregates
    # NOT USING AT THIS TIME
    # reclassify_category <- function(uic, ic, d1, d2 , d3, d4) {
    #   dplyr::case_when(
    #     # headcount
    #     uic %in% c("eg.3-2", "eg.3-2_oulevel") ~ "actor"
    #     # producers
    #     , uic == "eg.3.2-24/eg.3.2-x17"  ~ "actor"
    #     # hectares
    #     , uic == "eg.3.2-25/eg.3.2-x18" ~ "land"
    #     # sales
    #     , uic == "eg.3.2-26/eg.3.2-x19" & ! stringr::str_detect(d4, "number") ~ "transaction"
    #     , uic == "eg.3.2-26/eg.3.2-x19" & stringr::str_detect(d4, "number") ~ "actor"
    #     # financing
    #     , uic == "eg.3.2-27/eg.3.2-x6" ~ d3
    #   )
    # }
    reclassify_unit <- function(uic, ic, d1, d2, d3, d4) {
      dplyr::case_when(
        ################### producers and participants are all people
        uic == "eg.3.2-24/eg.3.2-x17" | uic == "eg.3-2" | uic == "eg.3-2_oulevel" ~ "people"

        ################### hectares
        , uic == "eg.3.2-25/eg.3.2-x18" ~ "area"

        ################### sales
        , uic == "eg.3.2-26/eg.3.2-x19" & stringr::str_detect(d4, "value") ~ "dollars"
        , uic == "eg.3.2-26/eg.3.2-x19" & stringr::str_detect(d4, "number") ~ "people"
        , uic == "eg.3.2-26/eg.3.2-x19" & stringr::str_detect(d4, "volume") ~ "metric tons"

        ################### financing
        , ic == "eg.3.2-27" & stringr::str_detect(d2, "value") ~ "dollars"
        , ic == "eg.3.2-27" & stringr::str_detect(d2, "number") ~  "people"
        , ic == "eg.3.2-27" & stringr::str_detect(d2, "volume") ~ "metric tons"

        ################### other indicators
        # dollars
        , ic == "eg.3.2-x6" | ic == "eg.3.1-14/-15" | ic == "eg.3.2-x22" |
            ic == "eg.3.2-x23" & d1 == "value of exports (in usd)" ~ "dollars"


        ################### people
        , ic == "eg.3.3-10" & str_detect(d1, "number") ~ "people"
        , ic == "eg.3.3-10" & str_detect(d1, "percentage") | ic == "cbld-9" ~ "percent"
        , ic == "eg.3.2-x1" | ic == "eg.4.2-7" | ic == "eg.5.2-x1" |
            ic == "eg.10.4-7" | ic == "eg.10.4-8" | ic == "eg.11-x6" |
            ic == "gndr-2" | ic == "hl.8.2-2" | ic == "hl.9-4" ~ "people"
        , ic == "eg.3.3-x11"  & str_detect(d1, "number") ~ "people"
        , ic == "hl.8.2-5" ~ "households"

        ################### metric tons
        , ic == "eg.3.2-x23" & d1 == "volume of exports (mt)" ~ "metric tons"
        , ic == "eg.3.3-x11"  & str_detect(d1, "quantity") ~ "metric tons"
        , ic == "eg.3-x6-7-8" ~ d1

      )
    }

    household_types <- c("school-aged children", "household members",
                         "parents/caregivers")
    reclassify_type <- function(uic, ic, d1, d2, d3, d4) {
      dplyr::case_when(
        # headcount
        str_starts(d1, "type of") ~ sub("type of ", "", d1)
        # , uic %in% c("eg.3-2", "eg.3-2_oulevel") &
        #   str_detect(d2, "government|civil society") ~ "organization"
        # , uic %in% c("eg.3-2", "eg.3-2_oulevel") & str_detect(d2, "firm") ~ "firm"
        # , uic %in% c("eg.3-2", "eg.3-2_oulevel") &
        #    d2 == "type of individual disaggregates not available" ~ "disaggregates not available"
        # , uic %in% c("eg.3-2", "eg.3-2_oulevel") &
        #    d2 == "type of individual not applicable" ~ "not applicable"
        , uic %in% c("eg.3-2", "eg.3-2_oulevel") & str_starts(d2, "laborers") ~ "laborers"
        , uic %in% c("eg.3-2", "eg.3-2_oulevel") & d2 %in% household_types ~ "household"
        , ic == "eg.3-x1" & str_detect(d1, "type") ~ sub(": .*", "", d2)

        # producers
        , uic == "eg.3.2-24/eg.3.2-x17" & str_detect("producers", d1) ~ "producers"
        , uic == "eg.3.2-24/eg.3.2-x17" & str_detect("producers", d1, negate=TRUE) ~ d1

        # hectares
        , uic == "eg.3.2-25/eg.3.2-x18" ~ d1

        # sales
        , uic == "eg.3.2-26/eg.3.2-x19" &
          stringr::str_detect(d1, "type of product") &
          stringr::str_detect(d4, "number") ~ "producer"
        , uic == "eg.3.2-26/eg.3.2-x19" &
          stringr::str_detect(d1, "type of service") &
          stringr::str_detect(d4, "number") ~ "service provider"
        , uic == "eg.3.2-26/eg.3.2-x19" &
          stringr::str_detect(d1, "type of product") ~ "product"
        , uic == "eg.3.2-26/eg.3.2-x19" &
          stringr::str_detect(d1, "type of service") ~ "service"
        , uic == "eg.3.2-26/eg.3.2-x19" &
          stringr::str_detect(d1, "commodity") ~ "commodity"

        # financing
        , ic == "eg.3.2-27" ~  sub(".*: ", "", d1)

        # TODO: Ask Katie and Magnus if we should recode the eg.3.2-x6 types:
        # producers, joint, local traders/assemblers, wholesalers/processors, others
        , ic == "eg.3.2-x6" &
           stringr::str_detect(d1, "producers") ~ "producer"
        , ic == "eg.3.2-x6" & stringr::str_detect(d1, "type of") &
           !stringr::str_detect(d2, "producers") ~  d2

        # other indicators
        , uic == "cbld-9" | uic == "eg.3.1-1" ~ d1
        , ic == "eg.3.1-14/-15" ~ sub(" amount", "", d1)
        , ic == "eg.3.2-x22" ~ "capital investment"
        , ic == "eg.3.3-10" & str_detect(d1, "number") ~ "participants"
        , ic == "eg.3.3-10" & str_detect(d1, "number") ~ "consuming diet of minimum diversity"
        , ic == "eg.3.2-x22" ~ "privte sector capital investment"
        , ic == "eg.3.2-25" & str_detect(d1, "account-policy") ~ "account-policy"
        , ic == "eg.3.2-x1" & !str_detect(d1, "sex") ~ d1
        , ic == "eg.3.2-x27" & str_detect(d1, "type of") ~ d2
        , ic == "eg.3.2-x29" & d1 == "type of organization" ~ d2
        , ic == "eg.3.2-x24" & str_detect(d1, "type of risk reducing practice") ~ "risk reducing practice"
        , ic == "eg.3.2-x37" & d1 == "msme type" ~ "msme"
        , ic == "eg.3.2-x41" ~ d1
        , ic == "eg.4.2-7" & str_detect(d1, "product type") ~ d2
        , ic == "eg.5.2-x1" & d1 == "type of firm" ~ paste(d2, "firm")
        , ic == "eg.10.4-7" | ic == "eg.10.4-8" ~ sub(".*: ", "", d1)
        , ic == "es.5-1" & str_detect(d1, "type") ~ "asset strengthened"
        , ic == "hl.8.2-5" ~ "household"
        , ic == "resil-1" ~ sub(" - .*", "", d1)

        )
    }

    make_uid <- function(uic, unit, typeof) {
      case_when(
        is.na(unit) & is.na(typeof) ~ uic
        , !is.na(unit) & is.na(typeof) ~ str_c(uic, "_", unit)
        , is.na(unit) & !is.na(typeof) ~ str_c(uic, "_", typeof)
        , !is.na(unit) & !is.na(typeof) ~ str_c(uic, "_", unit, "_", typeof)
      )
    }

    make_uic <- function(ic) {
      dplyr::case_when(
        ic %in% c("eg.3.2-24", "eg.3.2-x17") ~ "eg.3.2-24/eg.3.2-x17"
        , ic %in% c("eg.3.2-25","eg.3.2-x18") ~ "eg.3.2-25/eg.3.2-x18"
        , ic %in% c("eg.3.2-26", "eg.3.2-x19") ~ "eg.3.2-26/eg.3.2-x19"
        , ic %in% c("eg.3.2-27","eg.3.2-x6") ~ "eg.3.2-27/eg.3.2-x6"
        , ic %in% c("eg.3.1-14/-15","eg.3.2-x22") ~ "eg.3.1-14/-15/eg.3.2-x22"
        , .default = ic)
    }

# basic db ################
  if(db == "basic") {
    ### Read files ####
    indicator_files <- list.files(input_dir, pattern = "xlsx")
    print(paste("About to read", length(indicator_files), "files..."))

  indicators_full <- purrr::map(

    stringr::str_c(input_dir, indicator_files), ~ disR::read_indicator(.x)) %>%
    purrr::list_rbind()

  print(paste("Files read successfully..."))

  indicators_full <- indicators_full %>%

    dplyr::mutate(across(-value, ~ str_to_lower(.))) %>%
    dplyr::relocate(c(d3, d4), .after=d2) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(year = paste0("fy", year)) %>%
    dplyr::mutate(uic = make_uic(ic)
      , .after = ic) %>% #filter(uic == "eg.3.2-27/eg.3.2-x6") %>% distinct(d1)
    dplyr::mutate(across(c(d1, d2, d3, d4),
                         ~ str_replace_all(str_trim(.), disags_replace_all))) %>%
 ### Remove total values for Financing ####
    dplyr::filter(!(ic == "eg.3.2-27" & ! stringr::str_detect(d1, "type of")))

  print("Finished basic cleaning...")

  indicators_full <- indicators_full %>%
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
           , disag4 = reclassify_disag4(uic, ic, d1, d2, d3, d4)) # %>% filter(uic == "eg.3.2-27/eg.3.2-x6") %>% distinct(disag1)

  print("Finished standarizing disaggregates...")

  # TODO: make these based on the disag columns that are more standardized
  indicators_full <- indicators_full %>%
           #### sexes ####
  mutate(
    sex = case_when(
      disag1 == "sex" & disag2 %in% sex_lookup$disaggregate ~ disag2
      , disag2 == "sex" & disag3 %in% sex_lookup$disaggregate ~ disag3
      , disag3 == "sex" & disag4 %in% sex_lookup$disaggregate ~ disag4
      # , ic == "eg.3.2-x22" ~ disag1
      , ic == "eg.3.2-x1" ~ d2
      , ic == "eg.3.2-x37" & d1 == "sex" ~ d2
      , ic == "eg.3-x6-7-8" ~ d2
      , ic == "gndr-2" & str_detect(d1, "female") ~ "female"
      , ic == "gndr-2" & str_detect(d1, "total") ~ "other"
      , ic == "hl.9-1" & disag1 == "sex" ~ disag2
      , ic == "hl.9-2" & disag1 == "sex" ~ disag2
      , ic == "hl.9-4" & disag1 == "sex" ~ disag2
      , ic == "hl.9-x1" | ic == "hl.9-x15" ~ disag2
      , ic == "eg.11-x6" ~ d1)
    #### ages ####
    , age = case_when(
      disag1 == "age" ~ disag2
      , disag2 == "age" ~ disag3
      , disag4 == "age" ~ disag4
      , ic == "hl.9-3" & disag1 == "age" ~ disag2)
    #### sizes ####
    , size = case_when(
      uic == "eg.3.2-24/eg.3.2-x17" & str_detect("small", d1) ~ d1
      , uic == "eg.3.2-24/eg.3.2-x17" & str_detect("small", d1, negate = TRUE) ~ "not applicable"
      , uic == "eg.3.2-26/eg.3.2-x19" ~ sub(" (row label implied)", "", sub(".* - ", "", disag3))
      , ic == "eg.3.2-27" & disag1 == "size" ~ disag2
      , ic == "eg.3-2" &
        disag2 == "producer: size disaggregate not available" ~ "disaggregate not available"
      , ic == "eg.3-2" &
        str_detect(disag2, "smallholder farmer") ~ sub(" farmer", "",
                                                      sub(".*: ", "", disag3)))

      #### mgmt_practices ####
      , mgmt_practice = case_when(
        disag1 == "management practice or tech type" &
        disag2 %in% mgmt_prac_lookup$disaggregate ~ disag2)

      #### items ####
      , item = case_when(
        ic == "eg.3.2-26" ~ sub(".*: ", "", d1)
        , disag1 == "commodity" & disag2 %in% commodities_lookup$disaggregate ~ disag2
        , ic == "eg.3.2-x23" ~ d2
        , ic == "eg.3.3-x11" ~ d2
        , ic == "eg.3.3-x11" ~ d2)

    #### phase ####
    , phase = case_when(
      (ic == "eg.3-x1" | ic == "eg.3-x9" | ic == "eg.5.2-x1") & d1 == "duration" ~ d2
      , ic == "es.5-1" & str_detect(d1, "duration") ~ d2
      , ic == "hl.9-4" & str_detect(disag2, "^degree") ~ sub(".*: ", "", disag2)
      , ic == "resil-1" & str_detect(disag2, "total", negate = TRUE) ~ disag2)

      #### location ####
      , location = case_when(
        (ic == "eg.3-x1" | ic == "eg.3-x9") & d1 == "location" ~ d2
        , ic == "eg.10.4-7" & d2 == "location" ~ d3
        , ic == "hl.8.2-2" & d1 == "location" ~ d2
        , ic == "hl.8.2-5" ~ d1)

      #### outcomes_output ####
      , outcome_output = case_when(
        uic == "cbld-9" & str_detect(d2, "improved performance") ~ "improved performance"
        , uic == "cbld-9" & str_detect(d2, "capacity development support") ~ "capacity development support"
        , ic == "eg.3.2-x41" ~ d2
        , (ic == "eg.10.4-7" | ic == "eg.10.4-8") & str_detect(d2, "type") ~ d3
        , ic == "es.5-1" & str_detect(d1, "type") ~ d2
        , ic == "hl.9-1" & str_detect(d1, "intervention") ~ d2)

      #### total ####
      , total = case_when(
        !is.na(sex) ~ TRUE
        , is.na(sex) & !is.na(age) ~ TRUE
        # , is.na(sex) & is.na(age) ~ TRUE
      )
      , .after = disag4) %>%

    relocate(d1, d2, d3, d4, .after = sex) %>%
    select(-c(disag1, disag2, disag3, disag4)) %>%
    dplyr::mutate(across(-value, ~ trimws(.)))


  # FINISH INDICATORS_FULL ####
  # create disaggregates lookup
  indicators <- indicators_full %>%
    dplyr::distinct(ic, uic, unit, typeof
                    #, group
                    , sex, age, size, mgmt_practice, item, outcome_output, phase, location
                    #, disag1, disag2, disag3, disag4
                    , d1, d2, d3, d4) %>%
    dplyr::mutate(id = disR::return_id(.), .before = everything())

  # create target values lookup
  values <- indicators_full %>%
    # filter(type == "target") %>%
    dplyr::left_join(indicators) %>%
    dplyr::rename(id_ind = id)


###### other tables #########
  # create implementing mechanisms lookup
  ims <- indicators_full %>%
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

  ## specific indicators ################
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

  save(indicators_full, indicators, values, ims, ftf_target_countries

       , sex_indicators, age_indicators, item_indicators, mgmt_indicators

       , size_indicators, file = paste0(output_dir, "/", db, ".rdata"))

  write.csv(indicators_full, paste0(output_dir, "/", "indicators_full.csv"))
  write.csv(indicators, paste0(output_dir, "/", "indicators.csv"))
  write.csv(ims, paste0(output_dir, "/", "ims.csv"))
  write.csv(values, paste0(output_dir, "/", "values.csv"))



  # sheet_write(indicators_full, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "indicators full")
  # sheet_write(indicators, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "indicators")
  # sheet_write(age_indicators, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "age indicators")
  # sheet_write(sex_indicators, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "sex indicators")
  # sheet_write(mgmt_indicators, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "mgmt indicators")
  # sheet_write(ims, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "ims")
  # sheet_write(values, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "values")
  # sheet_write(item_indicators, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "item indicators")
  # sheet_write(ftf_target_countries, "1AW1iQWhlXQ-lZKh7KvvdH6ZpPDLPR2PER0hXU-2VafQ", "ftf target countries")

# detailed db #########

  } else if (db == "extract") {

    gs4_auth()

    # load("../database/extract/indicators_db.rdata")
    # file name is: "dis ent - ou activity indicator results (all data)_20230622 extract_extract.csv"
    input_file <- paste0(input_dir, "DIS ENT - OU Activity Indicator Results (All Data)_20230622 Extract_Extract.csv")
    dat <- read.csv(input_file, colClasses=c("activity.code"="character"))

    dat <- dat %>%
      dplyr::rename_with(str_to_lower) %>%
      dplyr::mutate(across(where(is_character), ~ str_to_lower(.))) %>%
      dplyr::rename(ro = reporting.organization
                    , ou = operating.unit
                    , a_code = activity.code
                    , a_name = activity.name
                    , ic = indicator.code
                    , ip = activity.vendor
                    , a_end = activity.end.date
                    , a_start = activity.start.date
                    , i_end = indicator.end.date
                    , i_start = indicator.start.date
                    , d_end = disaggregate.end.date
                    , d_start = disaggregate.start.date
                    , d_name = disaggregate.name
                    , country = disaggregate.country
                    , commodity = disaggregate.commodity
                    , a_office = activity.office
                    , status = activity.status
                    , tags = activity.tags
                    #, id = id.managing.office
                    , m_code = managing.office.code
                    , m_office = managing.office.name
                    , udn = udn
                    , year = fiscal.year
                    , target = target.value
                    , actual = actual.value)  %>%
      #dplyr::select(-id) %>%
      dplyr::mutate(system = "dis") %>%
      dplyr::mutate(uic = make_uic(ic), .after = ic) %>%
      dplyr::mutate(dplyr::across(c(actual, target), ~ as.numeric(.))) %>%
      rowwise() %>% mutate(uudn = paste0(ic, udn), .after=udn) %>%
      ungroup()

    # read.ftfms <- function(x) {
    #   out <- read.xlsx(x) %>%
    #     separate_wider_delim(indicatorname, ":",
    #                          names = c("indicatorname", "indicator.code")) %>%
    #     rename(indicator.name = indicatorname
    #          , reporting.organization = reportingorganization
    #          , managing.office.name = `bureau/region`
    #          , operating.unit = operatingunit
    #          , activity.code = `pa-id`
    #
    #          	, d1 = disaggregationname1
    #          	, d2 = disaggregationname2
    #          	, d3 = disaggregationname3
    #          	, d4 = disaggregationname4
    #          	, d5 = disaggregationname5
    #
    #          , target.value = targetvalue
    #          , actual.value = actualvalue
    #          , deviation.narrative = deviationnarrative	) %>%
    #     mutate(across(starts_with("d"), ~ as.character(.))) %>%
    #     mutate(system = "ftfms")
    #   return(out)
    # }

    # files <- c("../indicators/ftfms full dataset 2011-2019 1.xlsx",
    #            "../indicators/ftfms full dataset 2011-2019 2.xlsx")
    #
    # ftfms <- map(files, read.ftfms) %>%
    #  list_rbind()


    ##### indicators #############
    indicators <- dat %>% #select(-disaggregate.commodity) %>%
      dplyr::distinct(uudn, ic, uic, udn, d_start, d_end
               , dplyr::across(tidyselect::starts_with("disaggregate"))
               , indicator.origin, indicator.tags, is.ftf, is.ppr, is.pmp
               , is.neither.ppr.pmp, is.covid, indicator.collection.frequency
               , i_end) %>%
      dplyr::mutate(id = disR::return_id(.)
             , .before = tidyselect::everything())


    ##### get udn formulas from dis (from paul) ###########
    url <- "https://docs.google.com/spreadsheets/d/14iw2pritdeb-oy8deb5klnn9hwbnhiawv7dits5ewvo/edit#gid=1094963191"
    udns <- googlesheets4::read_sheet(url,  sheet = "ftf 20230915")  %>%
      dplyr::mutate(across(is.list, .fn = ~ as.character(unlist(.)))
                    , uudn = paste0(ic, udn), .after=ic) %>%
      dplyr::rename_with(.cols = everything(.)
                         , .fn = ~ stringr::str_replace_all(., " ", ".")) %>%
      dplyr::rename(#ro = reporting.organization
        #, ou = operating.unit
        #, a_code = activity.code
        #, a_name = activity.name
        , ic=indicator.code
        #, ip = activity.vendor
        #, a_end = activity.end.date
        #, a_start = activity.start.date
        #, i_end = indicator.end.date
        #, i_start = indicator.start.date
        , d_end = disaggregate.end.date
        , d_start = disaggregate.start.date
        #, a_office = activity.office
        #, status = activity.status
        #, tags = activity.tags
        #, id = id.managing.office
        #, m_code = managing.office.code
        #, m_office = managing.office.name
        #, country = disaggregate.country
        , udn = udn
        #, year = fiscal.year
        #, target = target.value
        #, actual = actual.value
      ) %>% # a tibble:4,129 × 21
      dplyr::group_by(ic, udn) %>%
      dplyr::slice(which.max(as.date(formula.start.date))) # a tibble:3,993 × 21


    cols <- c(names(udns)[names(udns) %in% names(indicators)]
              , "formula")
    udns <- dplyr::select(udns, tidyselect::all_of(cols))

    lapply(list_udns(udns$formula)
           , fun = function(x) paste(udns$ic, x))

    list_udns(udns$formula) %>% length()

    udns <- udns %>%
      dplyr::mutate(uudn = paste(ic, udn, sep="_")
             , udn_formulas = purrr::map2_vec(formula, ic,
               ~ dplyr::case_when(lengths(list_udns(.x)) > 0
                    ~ lapply(list_udns(.x), function(x) paste(.y, x, sep="_"))
                 , .default = list(na_character_)))
             , .after=ic) %>%
      dplyr::ungroup()

    indicators <- indicators %>%
      dplyr::left_join(
        dplyr::select(udns, -c(d_end, d_start, disaggregate.code, disaggregate.name)))


    # activities/ implemenitng mechanisms lookup ##################
    activities <- dat %>%
      dplyr::distinct(ro , ou, a_code, a_name, ip
               , a_end, a_start
               , a_office, status, tags
               , is.covid, is.disaggregate.blank, is.ftf, is.ips, is.neither.ppr.pmp
               , is.pmp, is.ppr) %>%
      dplyr::mutate(id = return_id(.), .before = tidyselect::everything())


    ###### countries#####
    ftf_target_countries <- data.frame(
      country = c("bangladesh","democratic republic of the congo",
                  "ethiopia","ghana", "guatemala","honduras",
                  "kenya","liberia", "madagascar","malawi",
                  "mali","mozambique", "nepal","niger",
                  "nigeria","rwanda","senegal","tanzania",
                  "uganda","zambia"), ftf_target = TRUE)
    file_path <- paste0(input_dir, "ppp_countries.csv")
    ppp_countries <- read.csv(file_path)

    countries <-  dat %>%
      dplyr::distinct(country = stringr::str_to_title(disaggregate.country)) %>%
      dplyr::filter(country != "") %>%
      dplyr::mutate(id = rownames(.), .before = tidyselect::everything()) %>%
      dplyr::left_join(ftf_target_countries) %>%
      dplyr::left_join(ppp_countries, dplyr::join_by(country == disaggregate.country))

    ##### countries disaggregate lookup ######
    disaggregate_countries <- dat %>%
      dplyr::distinct(ic, a_code, udn, disaggregate.country) %>%
      dplyr::mutate(id = disR::return_id(.)
                    , disaggregate.country = dplyr::na_if(disaggregate.country, "")
                    , .before = everything()) %>%
      dplyr::filter(!is.na(disaggregate.country)) %>%
      dplyr::arrange(a_code)

    # offices
    offices <- dat %>%
      dplyr::distinct(m_code, m_office) %>%
      dplyr::mutate(id = return_id(.), .before = tidyselect::everything())


    # values
    values <- dat %>% # 3,653,389
      # indicators
      dplyr::left_join(indicators) %>%
      dplyr::rename(id_ind = id) %>% # 310,803
      # activities
      dplyr::left_join(activities) %>%
      dplyr::rename(id_ac = id) %>%
      # offices
      dplyr::left_join(offices, dplyr::join_by(m_code, m_office)) %>%
      dplyr::rename(id_off = id) %>%
      dplyr::select(id_ind, id_ac, id_off, year, actual, target) %>%
      dplyr::mutate(id = return_id(.), .before = tidyselect::everything())


    file_path <- paste0(input_dir, "/api_pa.nus.ppp_ds2_en_csv_v2_5734723.csv")
    ppp <- read.csv(file_path, skip = 4) %>%
      dplyr::select(-`indicator.name`,- `indicator.code`) %>%
      tidyr::pivot_longer(-c(`country.name`, `country.code`),
                   names_to = "year", values_to = "ppp_multiplier") %>%
      dplyr::mutate(year = as.integer(stringr::str_remove(year, "x"))) %>%
      dplyr::filter(!is.na(ppp_multiplier))

    file_path <- paste0(input_dir, "/api_pa.nus.prvt.pp_ds2_en_csv_v2_5734724.csv")
    prvt_pp <- read.csv(file_path, skip = 4) %>%
      dplyr::select(-`indicator.name`,- `indicator.code`) %>%
      tidyr::pivot_longer(-c(`country.name`, `country.code`),
                   names_to = "year", values_to = "prvt_pp") %>%
      dplyr::mutate(year = as.integer(stringr::str_remove(year, "x"))) %>%
      dplyr::filter(!is.na(prvt_pp))

    file_path <- paste0(input_dir, "/api_pa.nus.fcrf_ds2_en_csv_v2_5729637.csv")
    exch_rate <- read.csv(file_path, skip = 4) %>%
      dplyr::select(-`indicator.name`,- `indicator.code`) %>%
      tidyr::pivot_longer(-c(`country.name`, `country.code`),
                   names_to = "year", values_to = "exch_rate") %>%
      dplyr::mutate(year = as.integer(stringr::str_remove(year, "x"))) %>%
      dplyr::filter(!is.na(exch_rate))

    # save all objects to an rdata file
    save(indicators, activities, countries, disaggregate_countries, ppp, prvt_pp, exch_rate
         , ppp_countries, ftf_target_countries, offices, values, udns
         , file = paste0(output_dir, db, ".rdata"))

  }
}
