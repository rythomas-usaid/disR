#' helpers for parsing disaggregates
#'
#' @import tidyverse
#' @export return_id
return_id <- function(row) {
  l <- round(log10(nrow(row))+1,0)
  id <- stringr::str_pad(rownames(row), width = l, pad = "0", side = "left")
  return(id)
}

# reclassify disaggregates
#' @export reclassify_disag1
reclassify_disag1 <- function(ic, d1, d2 , d3, d4) {
  dplyr::case_when(
    # headcount
    # ic %in% c("eg.3-2", "eg.3-2_oulevel") ~ d1 # this is the default
    ic == "eg.3.2-x23" ~ "commodity"
    # producers
    , ic %in% c("eg.3.2-24", "eg.3.2-x17") ~ d2
    # hectares
    , ic %in% c("eg.3.2-25", "eg.3.2-x18") ~ d2
    # sales
    , ic == "eg.3.2-26" ~ d3
    # , ic == "eg.3.2-x19" ~ d1
    # financing
    , ic == "eg.3.2-27" ~ d2
    # , ic == "eg.3.2-x6" ~ d1
    # assuming below the "sex of recipient(s)" has already been replaced with "sex"
    # , ic == "eg.3.2-x6" ~ d1
    # , ic == "eg.3.2-x22" ~ stringr::str_remove(d1, typeof) # this is the default
    # , ic == "eg.3.2-x27" & d1 == "type of organization" ~ NA
    # , ic == "eg.3.2-x29" & d1 == "type of organization" ~ NA
    , ic == "eg.3.2-x32" ~ "sex"
    , ic == "eg.10.4-7" & d2 != "type of documentation" ~ d2
    , ic == "eg.10.4-8" & d2 != "tenure type" ~ d2
    , ic == "eg.11-x6" ~ "sex"
    , ic == "hl.8.2-5" ~ "location"
    , ic == "hl.9-a" ~ "age"
    , ic == "hl.9-b" ~ "age"
    , ic == "hl.9-2" ~ "sex"
    , ic == "hl.9-3" & d1 == "age" ~ d1
    , ic == "hl.9.3" & d1 != "age" ~ NA
    , ic == "hl.9-4" & d1 == "sex" ~ d1
    , ic == "hl.9-4" & d1 != "sex" ~ NA
    , ic == "hl.9-x1" | ic == "hl.9-x15" ~ "sex"
    , ic == "eg.3.3-x11" ~ "commodity"
    , .default = d1
  )
}


# headcount_types <- c("producer", "organization participant", "household")
#' @export reclassify_disag2
reclassify_disag2 <- function(ic, d1, d2, d3, d4, typeof) {
  dplyr::case_when(
    # headcount
    ic %in% c("eg.3-2", "eg.3-2_oulevel") ~ stringr::str_replace_all(
      d2, pattern = c("household " = ""
                      , "people in " = ""
                      , "producer: " = ""
                      , "type of individual " = ""))
    , ic %in% c("eg.3-2", "eg.3-2_oulevel") & d1 == "sex" ~ d2

    # producers
    , ic %in% c("eg.3.2-24", "eg.3.2-x17")  ~ d3
    # hectares
    , ic %in% c("eg.3.2-25", "eg.3.2-x18")  ~ d3
    # sales
    , ic %in% c("eg.3.2-26", "eg.3.2-x19") ~ sub(" -.*", "", d4)
    # financing
    # , ic == "eg.3.2-x6" ~ d2
    , ic == "eg.3.2-27" ~ d4
    , ic == "eg.3.2-x32" ~ d1
    , ic == "eg.11-x6" ~ d2
    , ic == "hl.8.2-5" ~ d1
    , ic == "hl.9-a" ~ d1
    , ic == "hl.9-b" ~ d1
    , ic == "hl.9-2" ~ d1
    , ic == "hl.9-x1" | ic == "hl.9-x15" ~ d2
    , .default = d2
  )
}

#' @export reclassify_disag3
reclassify_disag3 <- function(ic, d1, d2, d3, d4) {
  dplyr::case_when(
    # producers
    ic == "eg.3.2-24"  ~ d1
    # hectares
    , ic == "eg.3.2-25"  ~ d1
    , ic == "eg.3.2-x18"  ~ d1
    # sales
    , ic == "eg.3.2-26" ~ d2
    , ic == "eg.3.2-x19" ~ d4
    # financing
    , ic == "eg.3.2-27" ~ sub(".*: ", "", d1)
    , ic == "hl.8.2-5" & stringr::str_detect(d2, "both") ~ "water and soap available"
    , ic == "hl.8.2-5" & stringr::str_detect(d2, "both", negate=TRUE) ~ "covered households"
    , .default = d3
  )
}

#' @export reclassify_disag4
reclassify_disag4 <- function(ic, d1, d2, d3, d4) {
  dplyr::case_when(
    ic == "eg.3.2-26" ~ trimws(sub(".*: ", "", d1))
    , ic == "eg.3.2-27" ~ sub(" of .*", "", d2)
    , ic == "eg.3.2-x6" ~ NA
    , .default = d4
  )
}

# Standardize disaggregates ---------------
#' @export
join_disaggregate_crosswalk <- function(x) {
  disaggregate_crosswalk <- make_disaggregate_crosswalk()
  dis <- tibble::as_tibble(x) %>%
    dplyr::left_join(disaggregate_crosswalk, relationship = "many-to-one")
}

#' @export add_disaggregate_categories
add_disaggregate_categories <- function(x) {
  x %>%
    dplyr::mutate(indicator_code = ic, first_order = d1, second_order = d2, third_order = d3, fourth_order = d4) %>%
    dplyr::mutate(
      across(
        c(ic, d1, d2, d3, d4)
        , ~ stringr::str_trim(.) %>%
          stringr::str_to_lower() %>%
          stringr::str_replace_all(disags_replace_all)
        )
      ) %>%
    dplyr::mutate(unit = reclassify_unit(ic, d1, d2, d3, d4)
                  , typeof = reclassify_type(ic, d1, d2, d3, d4)
                  # , uid = make_uid(uic, unit, typeof)
                  , disag1 = reclassify_disag1(ic, d1, d2, d3, d4)
                  , disag2 = reclassify_disag2(ic, d1, d2, d3, d4, typeof)
                  , disag3 = reclassify_disag3(ic, d1, d2, d3, d4)
                  , disag4 = reclassify_disag4(ic, d1, d2, d3, d4)
                  , .after = everything()) %>%
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
      ic == "cbld-9" & stringr::str_detect(d2, "improved performance") ~ "improved performance"
      , ic == "cbld-9" & stringr::str_detect(d2, "capacity development support") ~ "capacity development support"
      , ic == "eg.3.2-x41" ~ d2
      , (ic == "eg.10.4-7" | ic == "eg.10.4-8") & stringr::str_detect(d2, "type") ~ d3
      , ic == "es.5-1" & stringr::str_detect(d1, "type") ~ d2
      , ic == "hl.9-1" & stringr::str_detect(d1, "intervention") ~ d2)
    , .after = everything()) %>%

    #### total ####
  # , is_total = dplyr::case_when(
  #   # This is not correct. Need to add logical for "value" for eg.3.2-26, among others.
  #   !is.na(sex) ~ TRUE
  #   , is.na(sex) & !is.na(age) ~ TRUE
  #   , is.na(sex) & is.na(age) ~ TRUE
  # )
  # , .after = disag4) %>%

  relocate(d1, d2, d3, d4, .after = sex)
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


#' @export reclassify_unit
reclassify_unit <- function(ic, d1, d2, d3, d4) {
  dplyr::case_when(
    ################### producers and participants are all people
    ic == "eg.3.2-24" | ic == "eg.3.2-x17" | ic == "eg.3-2" | ic == "eg.3-2_oulevel" ~ "people"

    ################### hectares
    , ic == "eg.3.2-25" | ic == "eg.3.2-x18" ~ "area"

    ################### sales
    , (ic == "eg.3.2-26"| ic == "eg.3.2-x19") & stringr::str_detect(d4, "value") ~ "dollars"
    , (ic == "eg.3.2-26" | ic == "eg.3.2-x19") & stringr::str_detect(d4, "number") ~ "people"
    , (ic == "eg.3.2-26" | ic == "eg.3.2-x19") & stringr::str_detect(d4, "volume") ~ "metric tons"

    ################### financing
    , ic == "eg.3.2-27" & stringr::str_detect(d2, "value") ~ "dollars"
    , ic == "eg.3.2-27" & stringr::str_detect(d2, "number") ~  "people"
    , ic == "eg.3.2-27" & stringr::str_detect(d2, "volume") ~ "metric tons"

    ################### other indicators
    # dollars
    , ic == "eg.3.2-x6" | ic == "eg.3.1-14/-15" | ic == "eg.3.2-x22" |
      ic == "eg.3.2-x23" & d1 == "value of exports (in usd)" ~ "dollars"


    ################### people
    , ic == "eg.3.3-10" & stringr::str_detect(d1, "number") ~ "people"
    , ic == "eg.3.3-10" & stringr::str_detect(d1, "percentage") | ic == "cbld-9" ~ "percent"
    , ic == "eg.3.2-x1" | ic == "eg.4.2-7" | ic == "eg.5.2-x1" |
      ic == "eg.10.4-7" | ic == "eg.10.4-8" | ic == "eg.11-x6" |
      ic == "gndr-2" | ic == "hl.8.2-2" | ic == "hl.9-4" ~ "people"
    , ic == "eg.3.3-x11"  & stringr::str_detect(d1, "number") ~ "people"
    , ic == "hl.8.2-5" ~ "households"

    ################### metric tons
    , ic == "eg.3.2-x23" & d1 == "volume of exports (mt)" ~ "metric tons"
    , ic == "eg.3.3-x11"  & stringr::str_detect(d1, "quantity") ~ "metric tons"
    , ic == "eg.3-x6-7-8" ~ d1

  )
}

#' @export household_types
household_types <- c("school-aged children", "household members",
                     "parents/caregivers")
#' @export reclassify_type
reclassify_type <- function(ic, d1, d2, d3, d4) {
  dplyr::case_when(
    # headcount
    stringr::str_starts(d1, "type of") ~ sub("type of ", "", d1)
    # , uic %in% c("eg.3-2", "eg.3-2_oulevel") &
    #   stringr::str_detect(d2, "government|civil society") ~ "organization"
    # , uic %in% c("eg.3-2", "eg.3-2_oulevel") & stringr::str_detect(d2, "firm") ~ "firm"
    # , uic %in% c("eg.3-2", "eg.3-2_oulevel") &
    #    d2 == "type of individual disaggregates not available" ~ "disaggregates not available"
    # , uic %in% c("eg.3-2", "eg.3-2_oulevel") &
    #    d2 == "type of individual not applicable" ~ "not applicable"
    , ic %in% c("eg.3-2", "eg.3-2_oulevel") & stringr::str_starts(d2, "producer") ~ "producer"
    , ic %in% c("eg.3-2", "eg.3-2_oulevel") & stringr::str_starts(d2, "laborer") ~ "laborer"
    , ic %in% c("eg.3-2", "eg.3-2_oulevel") & d2 %in% household_types ~ "household member"
    , ic == "eg.3-x1" & stringr::str_detect(d1, "type") ~ sub(": .*", "", d2)

    # producers
    , ic %in% c("eg.3.2-24", "eg.3.2-x17") & stringr::str_detect("producer", d1) ~ "producer"
    , ic %in% c("eg.3.2-24", "eg.3.2-x17") & stringr::str_detect("producer", d1, negate=TRUE) ~ d1

    # hectares
    , ic %in% c("eg.3.2-25", "eg.3.2-x18") ~ d1

    # sales
    , ic %in% c("eg.3.2-26", "eg.3.2-x19") &
      stringr::str_detect(d1, "type of product") &
      stringr::str_detect(d4, "number") ~ "producer"
    , ic  %in% c("eg.3.2-26", "eg.3.2-x19") &
      stringr::str_detect(d1, "type of service") &
      stringr::str_detect(d4, "number") ~ "service provider"
    , ic  %in% c("eg.3.2-26", "eg.3.2-x19") &
      stringr::str_detect(d1, "type of product") ~ "product"
    , ic  %in% c("eg.3.2-26", "eg.3.2-x19") &
      stringr::str_detect(d1, "type of service") &
      stringr::str_detect(d4, "number", negate = TRUE) ~ "service"
    # , uic == "eg.3.2-26/eg.3.2-x19" &
    #   stringr::str_detect(d1, "commodity") ~ "commodity"

    # financing
    , ic == "eg.3.2-27" ~  sub(".*: ", "", d1)

    # TODO: Ask Katie and Magnus if we should recode the eg.3.2-x6 types:
    # producers, joint, local traders/assemblers, wholesalers/processors, others
    , ic == "eg.3.2-x6" &
      stringr::str_detect(d1, "producers") ~ "producer"
    , ic == "eg.3.2-x6" & stringr::str_detect(d1, "type of") &
      ! stringr::str_detect(d2, "producers") ~  d2

    # other indicators
    , ic  %in% c("cbld-9", "eg.3.1-1") ~ d1
    , ic == "eg.3.1-14/-15" ~ sub(" amount", "", d1)
    , ic == "eg.3.2-x22" ~ "capital investment"
    , ic == "eg.3.3-10" & stringr::str_detect(d1, "number") ~ "participants"
    , ic == "eg.3.3-10" & stringr::str_detect(d1, "number") ~ "consuming diet of minimum diversity"
    , ic == "eg.3.2-x22" ~ "privte sector capital investment"
    , ic == "eg.3.2-25" & stringr::str_detect(d1, "account-policy") ~ "account-policy"
    , ic == "eg.3.2-x1" & ! stringr::str_detect(d1, "sex") ~ d1
    , ic == "eg.3.2-x27" & stringr::str_detect(d1, "type of") ~ d2
    , ic == "eg.3.2-x29" & d1 == "type of organization" ~ d2
    , ic == "eg.3.2-x24" & stringr::str_detect(d1, "type of risk reducing practice") ~ "risk reducing practice"
    , ic == "eg.3.2-x37" & d1 == "msme type" ~ "msme"
    , ic == "eg.3.2-x41" ~ d1
    , ic == "eg.4.2-7" & stringr::str_detect(d1, "product type") ~ d2
    , ic == "eg.5.2-x1" & d1 == "type of firm" ~ paste(d2, "firm")
    , ic == "eg.10.4-7" | ic == "eg.10.4-8" ~ sub(".*: ", "", d1)
    , ic == "es.5-1" & stringr::str_detect(d1, "type") ~ "asset strengthened"
    , ic == "hl.8.2-5" ~ "household"
    , ic == "resil-1" ~ sub(" - .*", "", d1)

  )
}

# @export make_uid
#' Should not have a function with uic in it.
# make_uid <- function(uic, unit, typeof) {
#   ifelse(is.na(unit) & is.na(typeof), uic
#   , ifelse(!is.na(unit) & is.na(typeof), paste0(uic, "_", unit)
#   , ifelse(is.na(unit) & !is.na(typeof), paste0(uic, "_", typeof)
#   , ifelse(!is.na(unit) & !is.na(typeof), paste0(uic, "_", unit, "_", typeof)
#   , NA))))
# }

# this uses the green row from Magnus's Tableau worksheet
# parse_uic <- function(ic, udn, d1) {
#   warning("make_dis_uic() can only be applied after combining ftfms and DIS data")
#   dplyr::case_when(
#     ic == "eg.3.1-14/-15" & udn == "3.1.2" ~ "eg.3.1-14/-15/eg.3.2-x22" # Private Sector Partner Leveraged Amount
#     , ic == "eg.3.1-14/-15" & d1 == "private sector partner leveraged amount" ~ "eg.3.1-14/-15/eg.3.2-x22"
#
#     , ic == "eg.3.2-x22" ~ "eg.3.1-14/-15/eg.3.2-x22"
#
#     , ic == "eg.3.2-x17" & d1 == "producers" ~ "eg.3.2-24/eg.3.2-x17"
#     , ic == "eg.3.2-24" &
#       udn %in% c("3.1.1.1", "3.1.1.2", "3.1.1.3","3.1.1.4", "3.2.1.1"
#                  , "3.2.1.2", "3.2.1.3", "3.2.1.4") ~ "eg.3.2-24/eg.3.2-x17"
#
#     , ic == "eg.3.2-25" &
#       d1 %in% c("type of hectare: crop land"
#                 , "type of hectare: cultivated pasture") ~ "eg.3.2-25/eg.3.2-x18"
#     , ic == "eg.3.2-25" &
#       udn %in% c("3.1.1.1",  "3.1.1.2", "3.1.1.3", "3.1.1.4","3.1.1.5", "3.2.1.1"
#                  , "3.2.1.2", "3.2.1.3", "3.2.1.4", "3.2.1.5") ~ "eg.3.2-25/eg.3.2-x18"
#     , ic == "eg.3.2-x18" ~ "eg.3.2-25/eg.3.2-x18"
#
#     , ic == "e.g.3.2-26" &
#       udn %in% c("3.1.1.1.3.1", "3.1.1.1.3.3", "3.1.1.1.3.5",
#                  "3.1.1.1.3.7", "3.1.1.1.3.2", "3.1.1.1.3.4", "3.1.1.1.3.6", "3.1.1.1.3.8",
#                  "3.1.2.1.3.1", "3.1.2.1.3.3", "3.1.2.1.3.5", "3.1.2.1.3.7", "3.1.2.1.3.2",
#                  "3.1.2.1.3.4", "3.1.2.1.3.6", "3.1.2.1.3.8", "3.1.3.1.3.1", "3.1.3.1.3.3",
#                  "3.1.3.1.3.5", "3.1.3.1.3.7", "3.1.3.1.3.2", "3.1.3.1.3.4", "3.1.3.1.3.6",
#                  "3.1.3.1.3.8", "3.1.4.1.3.1", "3.1.4.1.3.3", "3.1.4.1.3.5", "3.1.4.1.3.7",
#                  "3.1.4.1.3.2", "3.1.4.1.3.4", "3.1.4.1.3.6", "3.1.4.1.3.8", "3.1.5.1.3.1",
#                  "3.1.5.1.3.3", "3.1.5.1.3.5", "3.1.5.1.3.7", "3.1.5.1.3.2", "3.1.5.1.3.4",
#                  "3.1.5.1.3.6", "3.1.5.1.3.8", "3.1.6.1.3.1", "3.1.6.1.3.3", "3.1.6.1.3.5",
#                  "3.1.6.1.3.7", "3.1.6.1.3.2", "3.1.6.1.3.4", "3.1.6.1.3.6", "3.1.6.1.3.8",
#                  "3.3.1.4.1", "3.3.1.4.4", "3.3.1.4.7", "3.3.1.4.10", "3.3.1.4.3",
#                  "3.3.1.4.6", "3.3.1.4.9", "3.3.1.4.12", "3.3.1.4.2", "3.3.1.4.5",
#                  "3.3.1.4.8", "3.3.1.4.11") ~ "eg.3.2-26/eg.3.2-x19"
#     , ic == "eg.3.2-x19" ~ "eg.3.2-26/eg.3.2-x19"
#     , ic %in% c("eg.3.2-27","eg.3.2-x6") ~ "eg.3.2-27/eg.3.2-x6"
#     , .default = ic)
# }

#' @export make_uic
make_uic <- function(ic, d1, d2) {
  dplyr::case_when(
    ic == "eg.3.1-14/-15" & d1 == "private sector partner leveraged amount" ~ "eg.3.1-14/-15/eg.3.2-x22"
    , ic == "eg.3.2-x22" ~ "eg.3.1-14/-15/eg.3.2-x22"

    , ic == "eg.3.2-x17" & d1 == "producers" ~ "eg.3.2-24/eg.3.2-x17"
    , ic == "eg.3.2-24" & stringr::str_ends(d1, "producers") ~ "eg.3.2-24/eg.3.2-x17"

    , ic == "eg.3.2-25" & d1 %in% c("type of hectare: crop land"
                                    , "type of hectare: cultivated pasture") ~ "eg.3.2-25/eg.3.2-x18"
    , ic == "eg.3.2-x18" ~ "eg.3.2-25/eg.3.2-x18"

    , ic == "e.g.3.2-26" & d2 == "producer - smallholder" ~ "eg.3.2-26/eg.3.2-x19"
    , ic == "eg.3.2-x19" ~ "eg.3.2-26/eg.3.2-x19"
    , ic == "eg.3.2-27" & d1 == "type of financing accessed: cash debt" ~ "eg.3.2-27/eg.3.2-x6"
    , ic == "eg.3.2-x6" ~ "eg.3.2-27/eg.3.2-x6"
    , .default = ic)
}

#' @export parse_ftfms_uic
parse_ftfms_uic <- function(ic, d1) {
  dplyr::case_when(
    ic == "eg.3.1-14/-15" &
      d1 == "private sector partner leveraged amount" ~ "eg.3.1-14/-15/eg.3.2-x22"
    , ic == "eg.3.2-x22" ~ "eg.3.1-14/-15/eg.3.2-x22"
    , ic == "eg.3.2-x17" & d1 == "producers" ~ "eg.3.2-24/eg.3.2-x17"
    , ic == "eg.3.2-24" &
      d1 %in% c("smallholder producers"
                , "non-smallholder producers") ~ "eg.3.2-24/eg.3.2-x17"
    , ic == "eg.3.2-25" &
      d1 %in% c("Type of Hectare: Crop land"
                , "Type of Hectare: Cultivated pasture") ~ "eg.3.2-25/eg.3.2-x18"
    , ic == "eg.3.2-25" &
      # crop land
      udn %in% c("3.1.1.1", "3.1.1.2", "3.1.1.3", "3.1.1.4", "3.1.2.1"
                 , "3.1.2.2", "3.1.2.3", "3.1.2.4", "3.1.3.1", "3.1.3.2"
                 , "3.1.3.3", "3.1.3.4", "3.1.3.5", "3.1.3.6", "3.1.3.7"
                 , "3.1.3.8", "3.1.3.9", "3.1.3.10", "3.1.3.11", "3.1.3.12"
                 , "3.1.3.13", "3.1.4"
                 # cultivated pasture
                 , "3.2.1.1", "3.2.1.2", "3.2.1.3", "3.2.1.4", "3.2.2.1"
                 , "3.2.2.2", "3.2.2.3", "3.2.2.4", "3.2.3.1", "3.2.3.2"
                 , "3.2.3.3", "3.2.3.4", "3.2.3.5", "3.2.3.6", "3.2.3.7"
                 , "3.2.3.8", "3.2.3.9", "3.2.3.10", "3.2.3.11", "3.2.3.12"
                 , "3.2.3.13", "3.2.4") ~ "eg.3.2-25/eg.3.2-x18"
    # Why would these be different???
    # c("3.1.1.1", "3.2.1.1", "3.1.1.2", "3.1.1.3", "3.2.1.3", "3.1.1.4"
    #              , "3.2.1.4")
    , ic == "eg.3.2-x18" ~ "eg.3.2-25/eg.3.2-x18"

    , ic %in% c("eg.3.2-26", "eg.3.2-x19") ~ "eg.3.2-26/eg.3.2-x19"
    , ic %in% c("eg.3.2-27","eg.3.2-x6") ~ "eg.3.2-27/eg.3.2-x6"


    , .default = ic)
}

# STANDARDIZE DISAGREGATES ####
# This is a list of all the sex disaggregates for sex grouped by the label field
cat_lookup <- dplyr::bind_rows(
  tibble::tibble(standard = "sex"
         , category = c("sex")
         , disaggregate = c("female", "male", "disaggregates not available"
                            , "association-applied", "joint", "neither"
                            , "mixed sexes")
  ),
  tibble::tibble(standard = "sex"
         , category = "sex of owner / producer"
         , disaggregate = c("female", "male", "joint", "n/a"
                            , "disaggregates not available", "neither")
  ),
  tibble::tibble(standard = "sex"
         , category = "sex of recipient(s)"
         , disaggregate = c("female", "male", "joint","n/a", "not applicable"
                            , "disaggregates not available", "neither"
                            , "mixed (for enterprises)")
  ),
  tibble::tibble(standard = "sex"
         , category = c("sex of participant (no double-counting)")
         , disaggregate = c("female", "male"
                            , "disaggregates not available", "neither")
  ),
  tibble::tibble(standard = "sex"
         , category=c("sex of account owner or policy holder")
         , disaggregate = c("female", "male"
                            , "disaggregates not available", "neither"
                            , "jointly-held")
  ),
  tibble::tibble(standard = "sex"
         , category = "sex (no double-counting)"
         , disaggregate = c("female", "male"
                            , "disaggregates not available", "neither")
  ),
  tibble::tibble(standard = "sex"
         , category = "sex of individuals participating"
         , disaggregate = c("female", "male"
                            , "disaggregates not available"
                            ,"not applicable", "neither")
  ),
  tibble::tibble(standard = "sex"
         , category = "sex of job-holder"
         , disaggregate = c("female", "male"
                            , "disaggregates not available")
  ),

  # this one is not really a sex disaggregate->
  # tibble::tibble(category = "no age/sex information collected"
  #              , disaggregate = c("value of sales", "baseline sales"
  #              , "number of participants", "volume of sales (mt)"
  #              , "type of sales"))

  ## Age ####
  tibble::tibble(standard = "age"
         , category = c("age category of individuals participating"),
         disaggregate = c("disaggregates not available", "30+"
                          , "school-aged children", "15-29", "not applicable"
                          , "mixed ages (for enterprises)")
  ),
  tibble::tibble(standard = "age"
         , category = "age",
         disaggregate = c("women < 19", "women >= 19",
                          "disaggregates not available", "15-29", "30+"
                          , "disaggregates not available - value of sales"
                          , "15-29 years - value of sales"
                          , "15-29 years - number of participants"
                          , "30+ years - value of sales"
                          , "30+ years - number of participants"
                          , "mixed ages - value of sales"
                          , "mixed ages - number of participants"
                          , "disaggregates not available - number of participants"
                          , "disaggregates not available - number of participant producers"
                          , "30+ years - volume of sales"
                          , "15-29 years - volume of sales"
                          , "disaggregates not available - volume of sales")
  ),
  tibble::tibble(standard = "age"
         , category = c("age category"),
         disaggregate = c("15-29", "30+",
                          "disaggregates not available")
  ),
  tibble::tibble(standard = "age"
         , category = c("age of participant (no double-counting)"),
         disaggregate = c("15-29", "30+",
                          "disaggregates not available")
  ),
  tibble::tibble(standard = "age"
         , category = "age group"
  ),
  # a5 <- tibble::tibble(category = c(
  #   rep("number of female participants of the nutrition-sensitive agriculture activity", 2),
  #   rep("percentage of female participants consuming a diet of minimum diversity", 2)),
  #              disaggregate = rep(c("age less than 19 years", "age 19+ years"), 2))

  tibble::tibble(standard = "age"
         , category = "age of recipient(s)",
         disaggregate = c("15-29", "30+", "mixed ages (for enterprises)"
                          , "disaggregates not available"
                          , "mixed (for enterprises)")
  ),

  ## Mgmt Prac ####
  tibble::tibble(standard = "mgmt_practice"
         , category = "management practice or tech type"
         , disaggregate = c(
           "irrigation","agriculture water management-non-irrigation"
           , "cultural practices", "livestock management", "marketing and distribution"
           ,"pest and disease management", "post-harvest handling and storage"
           , "soil-related fertility and conservation", "other"
           , "climate adaptation/climate risk management", "climate mitigation"
           , "crop genetics", "value-added processing", "aquaculture management"
           , "natural resource or ecosystem management"
           , "wild-caught fisheries management", "disaggregates not available"
           , "total with one or more improved technology", "climate adaptation"
           , "disease management", "pest management"
           , "water management (non-irrigation)", "wild fishing gear/technique"
           , "climate mitigation or adaptation", "animal genetics"
           , "fishing gear/technique", "processing")
  ),

  ## Commodities ####
  tibble::tibble(standard = "commodity"
         , category = "commodity"
         , disaggregate =
           c("disaggregates not available or other", "jute", "lentil (nrvcc)"
             , "maize", "not applicable", "onions/shallots", "rice", "wheat"
             , "horticulture", "carp (ponds) (nrvcc)", "tilapia (ponds) (nrvcc)"
             , "garlic", "mung bean (nrvcc)", "cattle (live) (nrvcc)", "forage/fodder"
             , "milk (cow) (nrvcc)", "groundnuts/peanuts (nrvcc)", "sesame seed (oil)"
             , "sunflower (oil)", "chickens (poultry) (nrvcc)", "gourd, bitter (nrvcc)",
             "gourd, bottle (nrvcc)", "watermelon", "chilies (nrvcc)", "goat (live) (nrvcc)",
             "gourd, sweet (nrvcc)", "vegetables", "bananas (nrvcc)", "cashews (nrvcc)",
             "coconut (flesh, milk)", "coffee, green beans", "flowers", "fruits",
             "mango (nrvcc)", "potatoes", "papaya (nrvcc)", "fish (ponds) (nrvcc)",
             "chickpea (nrvcc)", "maize grain", "dark green leafy vegetables (nrvcc)",
             "african leafy vegetables group (nrvcc)", "eggplant", "cattle (beef) (nrvcc)",
             "dairy (non-milk products, e.g. yogurt) (nrvcc)", "eggs (nrvcc)",
             "sorghum", "pork (meat) (nrvcc)", "beans and pulses (nrvcc)",
             "sheep (live) (nrvcc)", "cocoa", "coffee", "cassava", "pulses (nrvcc)",
             "soybeans (nrvcc)", "beans (biofortified) (nrvcc)", "beans (non-biofortified) (nrvcc)",
             "cabbage (nrvcc)", "cowpeas (nrvcc)", "millet", "okra (nrvcc)",
             "pigeon peas (nrvcc)", "tomatoes", "sesame seed (nrvcc)", "basil",
             "fennel", "green beans", "pomegranate", "grapes", "honey", "peppers, chile (nrvcc)",
             "animal feed", "goat (meat) (nrvcc)", "sheep (lamb/mutton) (nrvcc)",
             "avocado", "hazelnuts (nrvcc)", "soybean rain-fed (nrvcc)", "apples",
             "carrots (nrvcc)", "cauliflower (nrvcc)", "cucumber", "fava beans (nrvcc)",
             "lettuce", "paprika", "passion fruit (nrvcc)", "peas, green (nrvcc)",
             "sweet potatoes - orange/dark yellow - biofortified (nrvcc)",
             "camel (live) (nrvcc)", "milk (general, not animal-specific) (nrvcc)",
             "moringa (nrvcc)", "pineapples (nrvcc)", "sweet potatoes", "soybeans (oil)",
             "cotton", "peanuts (oil)", "rice grain", "sorghum/millet", "tomatoes, fresh",
             "sweet potatoes - white/pale yellow", "sweet potatoes - orange/dark yellow - non biofortified (nrvcc)",
             "fish (open-water cages) (nrvcc)", "rice-irrigated", "rice-rainfed",
             "kale (nrvcc)", "sunflower seed (nrvcc)", "chia", "quinoa", "maize, fresh (green mealies)",
             "peppers, various types or type unknown (nrvcc)", "sacha inchi",
             "ginger", "handicrafts", "camel (meat) (nrvcc)", "milk (camel) (nrvcc)",
             "milk (goat) (nrvcc)", "rice-lowland", "maize flour", "maize, orange (nrvcc)",
             "butternut squash (nrvcc)")
  ),

  ## duration ####
  tibble::tibble(standard = "duration"
         , category = "duration"
         , disaggregate = c("new", "continuing")
  )
)


#' @export disags_replace_all
disags_replace_all <- c(
  "sex of owner / producer" = "sex"
  , "sex of recipient\\(s\\)" = "sex"
  , "sex of participant \\(no double-counting\\)" = "sex"
  , "sex of account owner or policy holder" = "sex"
  , "sex \\(no double-counting\\)" = "sex"
  , "sex of individuals participating" = "sex"
  , "sex of job-holder" = "sex"

  , "management practice or tech type \\(double-counting allowed\\)" =
    "management practice or tech type"

  , "age category of individuals participating" = "age"
  , "age category" = "age"
  , "age of participant \\(no double-counting\\)" = "age"
  , "age of recipient\\(s\\)" = "age"
  , "age of participant \\(no double-counting\\)" = "age"
  , "mixed ages \\(for enterprises\\)" = "mixed", "mixed ages" = "mixed"
  , "mixed \\(for enterprises\\)" = "mixed"
  , "age less than 19 years" = "< 19", "age 19+ years" = "19+"
  , "women < 19" = "< 19", "women >= 19" = ">= 19"

  , "size of recipient\\(s\\)" = "size"
  , "size of msme" = "size"
  , "smallholder \\(row label implied\\)" = "smallholder")
