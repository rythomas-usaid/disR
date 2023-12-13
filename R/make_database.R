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
    reclassify_disag1 <- function(uic, d1, d2 , d3, d4) {
      dplyr::case_when(
        # headcount
        uic %in% c("eg.3-2", "eg.3-2_oulevel") ~ d1
        # producers
        , uic == "eg.3.2-24/eg.3.2-x17"  ~ d2
        # hectares
        , uic == "eg.3.2-25/eg.3.2-x18" ~ d2
        # sales
        , uic == "eg.3.2-26/eg.3.2-x19" ~ d3
        # financing
        , uic == "eg.3.2-27/eg.3.2-x6" ~ d3
      )
    }

    headcount_types <- c("producer", "organization participant", "household")
    reclassify_disag2 <- function(uic, d1, d2, d3, d4, typeof) {
      dplyr::case_when(
        # headcount
        uic %in% c("eg.3-2", "eg.3-2_oulevel") &
          typeof %in% headcount_types ~ str_replace_all(
            string=d2
            , pattern = c("household " = ""
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
      )
    }

    reclassify_disag3 <- function(uic, d1, d2, d3, d4) {
      dplyr::case_when(
        # procuders
        uic == "eg.3.2-24/eg.3.2-x17"  ~ d1
        # hectares
        , uic == "eg.3.2-25/eg.3.2-x18"  ~ d1
        # sales
        , uic == "eg.3.2-26/eg.3.2-x19" ~ d2
        # financing
        # financing
        , uic == "eg.3.2-27/eg.3.2-x6" ~ sub(".*: ", "", d1)
      )
    }

    reclassify_disag4 <- function(uic, d1, d2, d3, d4) {
      dplyr::case_when(uic == "eg.3.2-26/eg.3.2-x19" ~ sub(".*: ", "", d1)
                       , uic == "eg.3.2-27/eg.3.2-x6" ~ d3

      )
    }

    # REOGRANIZATION
    # reclassify disaggregates
    reclassify_category <- function(uic, d1, d2 , d3, d4) {
      dplyr::case_when(
        # headcount
        uic %in% c("eg.3-2", "eg.3-2_oulevel") ~ "actor"
        # producers
        , uic == "eg.3.2-24/eg.3.2-x17"  ~ "actor"
        # hectares
        , uic == "eg.3.2-25/eg.3.2-x18" ~ "land"
        # sales
        , uic == "eg.3.2-26/eg.3.2-x19" & ! stringr::str_detect(d4, "number") ~ "transaction"
        , uic == "eg.3.2-26/eg.3.2-x19" & stringr::str_detect(d4, "number") ~ "actor"
        # financing
        , uic == "eg.3.2-27/eg.3.2-x6" ~ d3
      )
    }


    household_types <- c("school-aged children", "household members",
                         "parents/caregivers")
    reclassify_type <- function(uic, d1, d2, d3, d4) {
      dplyr::case_when(
        # headcount
        uic %in% c("eg.3-2", "eg.3-2_oulevel") & str_starts(d2, "producer: ") ~ "producer"
        , uic %in% c("eg.3-2", "eg.3-2_oulevel") & str_detect(d2, "government|civil society") ~ "organization"
        , uic %in% c("eg.3-2", "eg.3-2_oulevel") & str_detect(d2, "firm") ~ "firm"
        , uic %in% c("eg.3-2", "eg.3-2_oulevel") &
          d2 == "type of individual disaggregates not available" ~ "disaggregates not available"
        , uic %in% c("eg.3-2", "eg.3-2_oulevel") &
          d2 == "type of individual not applicable" ~ "not applicable"
        , uic %in% c("eg.3-2", "eg.3-2_oulevel") & str_starts(d2, "laborers") ~ "laborers"
        , uic %in% c("eg.3-2", "eg.3-2_oulevel") & d2 %in% household_types ~ "household"

        # producers
        , uic == "eg.3.2-24/eg.3.2-x17" & str_detect(d1, "producers") ~ "producer"
        , uic == "eg.3.2-24/eg.3.2-x17" &
          # Confirm
          str_detect(d1, c("government|civil society|other|not available")) ~ "organization"
        , uic == "eg.3.2-24/eg.3.2-x17" &
          # Confirm
          str_detect(d1, c("firm")) ~ "firm"

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

        # financing
        , uic == "eg.3.2-27/eg.3.2-x6" &
          stringr::str_detect(d3, "producer") ~ "producer"
        , uic == "eg.3.2-27/eg.3.2-x6" &
          stringr::str_detect(d3, "firm") ~  "firm"
        , uic == "eg.3.2-27/eg.3.2-x6" &
          stringr::str_detect(d3, "size") ~  "size"
      )
    }

    reclassify_unit <- function(uic, d1, d2, d3, d4) {
      dplyr::case_when(
        # producers are all people
        uic == "eg.3.2-24/eg.3.2-x17" ~ "people"

        # hectares
        , uic == "eg.3.2-25/eg.3.2-x18" ~ "area"

        # sales
        , uic == "eg.3.2-26/eg.3.2-x19" &
          stringr::str_detect(d4, "value") ~ "dollars"
        , uic == "eg.3.2-26/eg.3.2-x19" &
          stringr::str_detect(d4, "number") ~ "people"
        , uic == "eg.3.2-26/eg.3.2-x19" &
          stringr::str_detect(d4, "volume") ~ "metric tons"

        # financing
        , uic == "eg.3.2-27/eg.3.2-x6" &
          stringr::str_detect(d2, "value") ~ "dollars"
      , uic == "eg.3.2-27/eg.3.2-x6" &
        stringr::str_detect(d2, "number") ~  "people"
      , uic == "eg.3.2-27/eg.3.2-x6" &
        stringr::str_detect(d2, "volume") ~ "metric tons"
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


# basic db ################
  if(db == "basic") {
    indicator_files <- list.files(input_dir, pattern = "xlsx")

  indicators_df <- purrr::map(
    stringr::str_c(input_dir, indicator_files), ~ disR::read_indicator(.x)) %>%
    purrr::list_rbind() %>%
    mutate(across(-value, ~ str_to_lower(.))) %>%
    relocate(c(d3, d4), .after=d2) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(year = paste0("fy", year)) %>%

    dplyr::mutate(uic = dplyr::case_when(
      ic %in% c("eg.3.2-24", "eg.3.2-x17") ~ "eg.3.2-24/eg.3.2-x17"
      , ic %in% c("eg.3.2-25","eg.3.2-x18") ~ "eg.3.2-25/eg.3.2-x18"
      , ic %in% c("eg.3.2-26", "eg.3.2-x19") ~ "eg.3.2-26/eg.3.2-x19"
      , ic %in% c("eg.3.2-27","eg.3.2-x6") ~ "eg.3.2-27/eg.3.2-x6"
      , ic %in% c("eg.3.1-14/-15","eg.3.2-x22") ~ "eg.3.1-14/-15/eg.3.2-x22"
      , .default = ic)
      , .after = ic) %>%
    dplyr::mutate(across(c(d1, d2, d3, d4),
                         ~ str_replace_all(str_trim(.), disags_replace_all))) %>%

    #encode combination categories
    # https://docs.google.com/document/d/1qwfblce3uummzky7abcmcb_tx_mb7d7iekhun-5zhlu/edit
    dplyr::mutate(group = dplyr::case_when(
      ic %in% c("eg.3.2-25", "eg.3.2-x18") &
        d1 %in% c("crop land", "cultivated pasture", "cultivated land") &
        d2 =="sex" ~ "cultivated land"
      , ic %in% c("eg.3.2-25", "eg.3.2-x18") & d1 %in% c("aquaculture") &
        d2 =="sex" ~  "aquaculture"
      , ic %in% c("eg.3.2-25", "eg.3.2-x18") & d1 %in% c("other") &
        d2 =="sex" ~  "other"
      , ic %in% c("eg.3.2-25", "eg.3.2-x18") &
        d1 %in% c("conservation/protected area"
                  , "freshwater or marine ecosystems", "rangeland") &
        d2 =="sex" ~ "extensively managed"
      , .default = NA)
      , .after = everything()) %>%

    #filter(uic != "eg.3.2-27/eg.3.2-x6" & !str_detect(d1, "total")) %>%

    mutate(unit = reclassify_unit(uic, d1, d2, d3, d4)
           , typeof = reclassify_type(uic, d1, d2, d3, d4)
           , uid = make_uid(uic, unit, typeof)
           , disag1 = reclassify_disag1(uic, d1, d2, d3, d4)
           , disag2 = reclassify_disag2(uic, d1, d2, d3, d4, typeof)
           , disag3 = reclassify_disag3(uic, d1, d2, d3, d4)
           , disag4 = reclassify_disag4(uic, d1, d2, d3, d4)) %>%
    relocate(c(d1, d2, d3, d4), .after=everything())

  # create disaggregates lookup
  indicators <- indicators_df %>%
    dplyr::distinct(ic, uic, d1, d2, d3, d4) %>%
    dplyr::mutate(id = disR::return_id(.), .before = everything())

  # create target values lookup
  values <- indicators_df %>%
    # filter(type == "target") %>%
    dplyr::left_join(indicators) %>%
    dplyr::rename(id_ind = id)

## sex and age indicators #####

  ##### eg.3-2 #####
  # headcount



  ##### eg.3.2-24/x17 #####
  producers_indicators <- indicators %>%
    dplyr::filter(uic == "eg.3.2-24/eg.3.2-x17" & d2 !=  "commodity") %>%
    dplyr::select(-c(d1, d2, d3, d4))

  ###### eg.3.2-25/x18 #####
  hectares_indicators <- indicators %>%
    dplyr::filter(uic == "eg.3.2-25/eg.3.2-x18" & d2 != "commodity") %>%
    dplyr::select(-c(d1, d2, d3, d4))

  ###### eg.3.2-26/x19 #####
  sales_indicators <- indicators %>%
    dplyr::filter(uic == "eg.3.2-26/eg.3.2-x19" & d1 != "commodity") %>%
    dplyr::select(-c(d1, d2, d3, d4))

  ###### eg.3.2-27/x6 ####
  financing_indicators <- indicators %>%
    dplyr::filter(uic == "eg.3.2-27/eg.3.2-x6") %>%
    # fix d1
    dplyr::filter(d1 %in% c("type of financing accessed: cash debt"
                            ,"type of financing accessed: in-kind debt"
                            , "type of financing accessed: non-debt" )) %>%
    dplyr::mutate(d1 = sub(".*: ", "", d1)) %>%
    dplyr::mutate(disag1 = d3
                  , disag2 = d4
                  , disag3 = sub("*.:", "", d1)) %>%

    dplyr::select(-c(d1, d2, d3, d4))

  ##### sex indicators #####
  # filter unique on the reorganized indicators
  sex_indicators <- hectares_indicators %>%
    dplyr::bind_rows(sales_indicators
                     , producers_indicators
                     , financing_indicators) %>%
    dplyr::filter(disag1 == "sex")

  ##### age indicators #####
  age_indicators <- hectares_indicators %>%
    dplyr::bind_rows(sales_indicators
                     , producers_indicators
                     , financing_indicators) %>%
    dplyr::filter(disag1 == "age")

  ##### commodity indicators #####
  commodity_indicators <- indicators %>%
    dplyr::filter(disag1 == "commodity")

  mgmt_tech_type_indicators <-
    dplyr::bind_rows(sales_indicators
                     , producers_indicators
                     , financing_indicators) %>%
    dplyr::filter(disag1 == "management practice or tech type")


###### other tables #########
  # create implementing mechanisms lookup
  ims <- indicators_df %>%
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

  save(indicators_df, indicators, values, ims, ftf_target_countries

       , hectares_indicators, sales_indicators, producers_indicators, financing_indicators

       , sex_indicators, age_indicators, commodity_indicators

       , file = paste0(output_dir, "/", db, ".rdata"))

  write.csv(indicators_df, paste0(output_dir, "/", "indicators_df.csv"))
  write.csv(indicators, paste0(output_dir, "/", "indicators.csv"))
  write.csv(ims, paste0(output_dir, "/", "ims.csv"))
  write.csv(values, paste0(output_dir, "/", "values.csv"))



# detailed db #########

  } else if (db == "extract") {

    gs4_auth()

    # load("../database/extract/indicators_db.rdata")
    # file name is: "dis ent - ou activity indicator results (all data)_20230622 extract_extract.csv"
    input_file <- paste0(input_dir, "dis ent - ou activity indicator results (all data)_20230622 extract_extract.csv")
    dat <- read.csv(input_file, colclasses=c("activity.code"="character"))
    dat <- dat %>%
      dplyr::rename(ro = reporting.organization
             , ou = operating.unit
             , a_code = activity.code
             , a_name = activity.name
             , ic=indicator.code
             , ip = activity.vendor
             , a_end = activity.end.date
             , a_start = activity.start.date
             , i_end = indicator.end.date
             , i_start = indicator.start.date
             , d_end = disaggregate.end.date
             , d_start = disaggregate.start.date
             , a_office = activity.office
             , status = activity.status
             , tags = activity.tags
             , id = id.managing.office
             , m_code = managing.office.code
             , m_office = managing.office.name
             #, country = disaggregate.country
             , udn = udn
             , year = fiscal.year
             , target = target.value
             , actual = actual.value)  %>%
      dplyr::select(-id) %>%
      dplyr::mutate(system = "dis") %>%
      dplyr::mutate(disaggregate.country = case_when(
        disaggregate.country == "mali" ~ "mali"
        , .default = disaggregate.country)) %>%
      dplyr::mutate(dplyr::across(c(actual, target), ~ as.numeric(.)))

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
      dplyr::distinct(ic, udn, d_end, d_start
               , dplyr::across(tidyselect::starts_with("disaggregate"))
               , indicator.origin, indicator.tags, is.ftf, is.ppr, is.pmp
               , is.neither.ppr.pmp, is.covid, indicator.collection.frequency
               , i_end) %>%
      dplyr::mutate(uudn = paste(ic, udn, sep="_")
             , id = disR::return_id(.)
             , .before = tidyselect::everything())


    ##### get udn formulas from dis (from paul) ###########
    url <- "https://docs.google.com/spreadsheets/d/14iw2pritdeb-oy8deb5klnn9hwbnhiawv7dits5ewvo/edit#gid=1094963191"
    udns <- googlesheets4::read_sheet(url,  sheet = "ftf 20230915")  %>%
      dplyr::mutate(across(is.list, .fn = ~ as.character(unlist(.)))) %>%
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
