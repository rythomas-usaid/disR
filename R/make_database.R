#' Make FtF indicator database
#'
#' Create a single database from all the final indicator spreadsheets.
#' @param input_dir The directory with all of the indicator files in the same format (xlsx by default).
#' @param output_dir The directory where you want to store the output files.
#' @param db The database format ("magnus" by default, or "extract")
#' @param input_pattern The file format for all input files.
#' @return Saves database in .Rdata format in the specified directory.
#' @examples
#' make_database(input_dir = "../../indicators/basic/"
#'               , output_dir = "../data/"
#'               , db = "basic")
#' make_database(input_dir = "../../indicators/"
#'               , output_dir = "../downloads/"
#'               , db = "extract")
#'
#' @import googlesheets4
#'
#' @export
make_database <- function(input_dir, output_dir, db = "basic") {

    return_id <- function(row) {
    l <- round(log10(nrow(row))+1,0)
    id <- str_pad(rownames(row), width = l, pad = "0", side = "left")
    return(id)
    }

    ## Join udn formulas #################
    list_udns <- function(x) {
      stringr::str_replace_all(x, "[()/+*]|100|Manual|NA", " ") %>%
        stringr::str_extract_all(stringr::boundary("word"))
    }

########## basic db ################

  if(db == "basic") {
    indicator_files <- list.files(input_dir, pattern = "xlsx")

  indicators_df <- purrr::map(
    stringr::str_c(input_dir, indicator_files), ~ disR::read_indicator(.x)) %>%
    purrr::list_rbind() %>%
    #relocate(c(d3, d4), .after=d2) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(year = paste0("FY", year)) %>%
    #indicators_df <-   indicators_df %>%
    dplyr::mutate(ic_1819 = dplyr::case_when(
      ic %in% c("EG.3.2-25","EG.3.2-x18") ~ "EG.3.2-25/EG.3.2-x18"
      , ic %in% c("EG.3.2-24", "EG.3.2-x17") ~ "EG.3.2-24/EG.3.2-x17"
      , ic %in% c("EG.3.2-26", "EG.3.2-x19") ~ "EG.3.2-26/EG.3.2-x19"
      , ic %in% c("EG.3.2-27","EG.3.2-x6") ~ "EG.3.2-27/EG.3.2-x6"
      , ic %in% c("EG.3.1-14/-15","EG.3.2-x22") ~ "EG.3.1-14/-15/EG.3.2-x22"
      , .default = ic)
      , .after = ic)  %>%
    dplyr::mutate(disagg_group = paste(d1, d2, d3, d4, sep=" / ")) %>%
    dplyr::mutate(indicator_name = dplyr::case_when(
      dplyr::if_any(tidyselect::everything()
                    , ~ stringr::str_detect(., "Value of Sales")) ~
        paste0(ic_1819, "_value")
      , dplyr::if_any(tidyselect::everything()
                      , ~ stringr::str_detect(., "Number of Participants")) ~
        paste0(ic_1819, "_number")
      , dplyr::if_any(tidyselect::everything()
                      , ~ stringr::str_detect(., "Volume of Sales")) ~
        paste0(ic_1819, "_volume")
      , .default = ic_1819))


  # Create disaggregates lookup
  indicators <- indicators_df %>%
    dplyr::distinct(ic, ic_1819, d1, d2, d3, d4) %>%
    dplyr::mutate(id = disR::return_id(.), .before = everything()) %>%
    #encode combination categories
    # https://docs.google.com/document/d/1qwfBlce3UUmMzKY7ABCMcb_Tx_Mb7d7IeKHUn-5zhlU/edit
    dplyr::mutate(group = dplyr::case_when(
      ic %in% c("EG.3.2-25", "EG.3.2-x18") &
            d1 %in% c("Crop Land", "Cultivated Pasture", "Cultivated Land") &
            d2 =="Sex" ~ "Cultivated Land"
      , ic %in% c("EG.3.2-25", "EG.3.2-x18") & d1 %in% c("Aquaculture") &
            d2 =="Sex" ~  "Aquaculture"
      , ic %in% c("EG.3.2-25", "EG.3.2-x18") & d1 %in% c("Other") &
            d2 =="Sex" ~  "Other"
      , ic %in% c("EG.3.2-25", "EG.3.2-x18") &
            d1 %in% c("Conservation/Protected Area"
                      , "Freshwater or Marine Ecosystems", "Rangeland") &
            d2 =="Sex" ~ "Extensively managed"
      , .default = NA))

  # resequence the “order” of disaggregates
  hectares_indicators <- indicators %>%
    dplyr::filter(ic_1819 == "EG.3.2-25/EG.3.2-x18" ) %>%
    dplyr::mutate(disag1 = d2
                  , disag2 = d3
                  , disag3 = d1) %>%
    dplyr::select(-c(d1, d2, d3, d4))

  sales_indicators <- indicators %>%
    dplyr::filter(ic_1819 == "EG.3.2-26/EG.3.2-x19" ) %>%
    dplyr::mutate(ic_1819 = case_when(
      stringr::str_detect(d4, "Value") ~ paste0(ic_1819, "_value")
      , stringr::str_detect(d4, "Number") ~ paste0(ic_1819, "_number")
      , stringr::str_detect(d4, "Volume") ~ paste0(ic_1819, "_volume"))
      , .after = ic_1819) %>%

    # encode combination categories
    # https://docs.google.com/document/d/1qwfBlce3UUmMzKY7ABCMcb_Tx_Mb7d7IeKHUn-5zhlU/edit
    dplyr::mutate(disag1 = d3
                  , disag2 = sub(" -.*", "", d4)
                  , disag3 = d2
                  , disag4 = d1) %>%
    dplyr::select(-c(d1, d2, d3, d4))

  producers_indicators <- indicators %>%
    dplyr::filter(ic_1819 == "EG.3.2-24/EG.3.2-x17" ) %>%
    dplyr::mutate(disag1 = d2
                  , disag2 = d3
                  , disag3 = d1) %>%
    dplyr::select(-c(d1, d2, d3, d4))

  financing_indicators <- indicators %>%
    dplyr::filter(ic_1819 == "EG.3.2-27/EG.3.2-x6") %>%
    dplyr::mutate(ic_1819 = case_when(
      stringr::str_detect(d2, "Value") ~ paste0(ic_1819, "_value")
      , stringr::str_detect(d2, "Number") ~ paste0(ic_1819, "_number")
      , stringr::str_detect(d2, "Volume") ~ paste0(ic_1819, "_volume"))
      , .after = ic_1819) %>%
    dplyr::mutate(d3 = case_when(
      stringr::str_detect(d3, "Sex") ~ "Sex"
      , stringr::str_detect(d3, "Age") ~ "Age"
      , stringr::str_detect(d3, "Size") ~ "Size")
      , .after = d2) %>%
    # distinct(d1, d2)
    dplyr::filter(d1 %in% c("Type of Financing Accessed: Cash Debt"
                            ,"Type of Financing Accessed: In-Kind Debt"
                            , "Type of Financing Accessed: Non-Debt" )) %>%
    dplyr::mutate(d1 = sub(".*: ", "", d1)) %>%
    dplyr::mutate(disag1 = d3
                  , disag2 = d4
                  , disag3 = d1) %>%
    dplyr::select(-c(d1, d2, d3, d4))

  # Filter unique on the reorganized indicators
  sex_indicators <- hectares_indicators %>%
    dplyr::bind_rows(sales_indicators
                     , producers_indicators
                     , financing_indicators) %>%
    dplyr::filter(disag1 == "Sex")

  age_indicators <- hectares_indicators %>%
    dplyr::bind_rows(sales_indicators
                     , producers_indicators
                     , financing_indicators) %>%
    dplyr::filter(disag1 == "Age")

  nonunique_indicators <- hectares_indicators %>%
    dplyr::bind_rows(sales_indicators
                     , producers_indicators
                     , financing_indicators) %>%
    dplyr::filter(! disag1 %in% c("Sex", "Age"))


  # Create implementing mechanisms lookup
  ims <- indicators_df %>%
    dplyr::distinct(ro, ou, a_code, a_name) %>%
    dplyr::mutate(id = disR::return_id(.), .before = everything())


  # Create target values lookup
  values <- indicators_df %>%
    # filter(type == "Target") %>%
    dplyr::left_join(indicators) %>%
    dplyr::rename(id_ind = id) %>%
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

  save(indicators_df, indicators, values, ims, ftf_target_countries
       , hectares_indicators, sales_indicators, sex_indicators
       , age_indicators, nonunique_indicators
       , file = paste0(output_dir, "/", db, ".Rdata"))

  write.csv(indicators_df, paste0(output_dir, "/", "indicators_df.csv"))
  write.csv(indicators, paste0(output_dir, "/", "indicators.csv"))
  write.csv(ims, paste0(output_dir, "/", "ims.csv"))
  write.csv(values, paste0(output_dir, "/", "values.csv"))


  ########## detailed db ################
  } else if (db == "extract") {

    gs4_auth()

    # load("../database/extract/indicators_db.Rdata")
    # File name is: "DIS ENT - OU Activity Indicator Results (All Data)_20230622 Extract_Extract.csv"
    input_file <- paste0(input_dir, "DIS ENT - OU Activity Indicator Results (All Data)_20230622 Extract_Extract.csv")
    dat <- read.csv(input_file, colClasses=c("Activity.Code"="character"))
    dat <- dat %>%
      dplyr::rename(ro = Reporting.Organization
             , ou = Operating.Unit
             , a_code = Activity.Code
             , a_name = Activity.Name
             , ic=Indicator.Code
             , ip = Activity.Vendor
             , a_end = Activity.End.Date
             , a_start = Activity.Start.Date
             , i_end = Indicator.End.Date
             , i_start = Indicator.Start.Date
             , d_end = Disaggregate.End.Date
             , d_start = Disaggregate.Start.Date
             , a_office = Activity.Office
             , status = Activity.Status
             , tags = Activity.Tags
             , id = ID.Managing.Office
             , m_code = Managing.Office.Code
             , m_office = Managing.Office.Name
             #, country = Disaggregate.Country
             , udn = UDN
             , year = Fiscal.Year
             , target = Target.Value
             , actual = Actual.Value)  %>%
      dplyr::select(-id) %>%
      dplyr::mutate(system = "dis") %>%
      dplyr::mutate(Disaggregate.Country = case_when(
        Disaggregate.Country == "MALI" ~ "Mali"
        , .default = Disaggregate.Country)) %>%
      dplyr::mutate(dplyr::across(c(actual, target), ~ as.numeric(.)))

    # read.ftfms <- function(x) {
    #   out <- read.xlsx(x) %>%
    #     separate_wider_delim(IndicatorName, ":",
    #                          names = c("IndicatorName", "Indicator.Code")) %>%
    #     rename(Indicator.Name = IndicatorName
    #          , Reporting.Organization = ReportingOrganization
    #          , Managing.Office.Name = `Bureau/Region`
    #          , Operating.Unit = OperatingUnit
    #          , Activity.Code = `PA-ID`
    #
    #          	, d1 = DisaggregationName1
    #          	, d2 = DisaggregationName2
    #          	, d3 = DisaggregationName3
    #          	, d4 = DisaggregationName4
    #          	, d5 = DisaggregationName5
    #
    #          , Target.Value = TargetValue
    #          , Actual.Value = ActualValue
    #          , Deviation.Narrative = DeviationNarrative	) %>%
    #     mutate(across(starts_with("d"), ~ as.character(.))) %>%
    #     mutate(system = "ftfms")
    #   return(out)
    # }

    # files <- c("../indicators/FTFMS Full Dataset 2011-2019 1.xlsx",
    #            "../indicators/FTFMS Full Dataset 2011-2019 2.xlsx")
    #
    # ftfms <- map(files, read.ftfms) %>%
    #  list_rbind()


    ##### Indicators #############
    indicators <- dat %>% #select(-Disaggregate.Commodity) %>%
      dplyr::distinct(ic, udn, d_end, d_start
               , dplyr::across(tidyselect::starts_with("Disaggregate"))
               , Indicator.Origin, Indicator.Tags, Is.FTF, Is.PPR, Is.PMP
               , Is.Neither.PPR.PMP, Is.COVID, Indicator.Collection.Frequency
               , i_end) %>%
      dplyr::mutate(uudn = paste(ic, udn, sep="_")
             , id = disR::return_id(.)
             , .before = tidyselect::everything())


    ##### Get UDN formulas from DIS (from Paul) ###########
    url <- "https://docs.google.com/spreadsheets/d/14iw2PRItDeB-OY8deb5klnN9HwbnhiAWv7DiTS5EwVo/edit#gid=1094963191"
    udns <- googlesheets4::read_sheet(url,  sheet = "FTF 20230915")  %>%
      dplyr::mutate(across(is.list, .fn = ~ as.character(unlist(.)))) %>%
      dplyr::rename_with(.cols = everything(.)
                         , .fn = ~ stringr::str_replace_all(., " ", ".")) %>%
      dplyr::rename(#ro = Reporting.Organization
        #, ou = Operating.Unit
        #, a_code = Activity.Code
        #, a_name = Activity.Name
        , ic=Indicator.Code
        #, ip = Activity.Vendor
        #, a_end = Activity.End.Date
        #, a_start = Activity.Start.Date
        #, i_end = Indicator.End.Date
        #, i_start = Indicator.Start.Date
        , d_end = Disaggregate.End.Date
        , d_start = Disaggregate.Start.Date
        #, a_office = Activity.Office
        #, status = Activity.Status
        #, tags = Activity.Tags
        #, id = ID.Managing.Office
        #, m_code = Managing.Office.Code
        #, m_office = Managing.Office.Name
        #, country = Disaggregate.Country
        , udn = UDN
        #, year = Fiscal.Year
        #, target = Target.Value
        #, actual = Actual.Value
      ) %>% # A tibble:4,129 × 21
      dplyr::group_by(ic, udn) %>%
      dplyr::slice(which.max(as.Date(Formula.Start.Date))) # A tibble:3,993 × 21


    cols <- c(names(udns)[names(udns) %in% names(indicators)]
              , "Formula")
    udns <- dplyr::select(udns, tidyselect::all_of(cols))

    lapply(list_udns(udns$Formula)
           , FUN = function(x) paste(udns$ic, x))

    list_udns(udns$Formula) %>% length()

    udns <- udns %>%
      dplyr::mutate(uudn = paste(ic, udn, sep="_")
             , udn_formulas = purrr::map2_vec(Formula, ic,
               ~ dplyr::case_when(lengths(list_udns(.x)) > 0
                    ~ lapply(list_udns(.x), function(x) paste(.y, x, sep="_"))
                 , .default = list(NA_character_)))
             , .after=ic) %>%
      dplyr::ungroup()

    indicators <- indicators %>%
      dplyr::left_join(
        dplyr::select(udns, -c(d_end, d_start, Disaggregate.Code, Disaggregate.Name)))


    # Activities/ Implemenitng mechanisms lookup ##################
    activities <- dat %>%
      dplyr::distinct(ro , ou, a_code, a_name, ip
               , a_end, a_start
               , a_office, status, tags
               , Is.COVID, Is.Disaggregate.Blank, Is.FTF, Is.IPS, Is.Neither.PPR.PMP
               , Is.PMP, Is.PPR) %>%
      dplyr::mutate(id = return_id(.), .before = tidyselect::everything())


    ###### Countries#####
    ftf_target_countries <- data.frame(
      country = c("Bangladesh","Democratic Republic of the Congo",
                  "Ethiopia","Ghana", "Guatemala","Honduras",
                  "Kenya","Liberia", "Madagascar","Malawi",
                  "Mali","Mozambique", "Nepal","Niger",
                  "Nigeria","Rwanda","Senegal","Tanzania",
                  "Uganda","Zambia"), ftf_target = TRUE)
    file_path <- paste0(input_dir, "ppp_countries.csv")
    ppp_countries <- read.csv(file_path)

    countries <-  dat %>%
      dplyr::distinct(country = stringr::str_to_title(Disaggregate.Country)) %>%
      dplyr::filter(country != "") %>%
      dplyr::mutate(id = rownames(.), .before = tidyselect::everything()) %>%
      dplyr::left_join(ftf_target_countries) %>%
      dplyr::left_join(ppp_countries, dplyr::join_by(country == Disaggregate.Country))

    ##### Countries disaggregate lookup ######
    disaggregate_countries <- dat %>%
      dplyr::distinct(ic, a_code, udn, Disaggregate.Country) %>%
      dplyr::mutate(id = disR::return_id(.)
                    , Disaggregate.Country = dplyr::na_if(Disaggregate.Country, "")
                    , .before = everything()) %>%
      dplyr::filter(!is.na(Disaggregate.Country)) %>%
      dplyr::arrange(a_code)

    # Offices
    offices <- dat %>%
      dplyr::distinct(m_code, m_office) %>%
      dplyr::mutate(id = return_id(.), .before = tidyselect::everything())


    # Values
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


    file_path <- paste0(input_dir, "/API_PA.NUS.PPP_DS2_en_csv_v2_5734723.csv")
    ppp <- read.csv(file_path, skip = 4) %>%
      dplyr::select(-`Indicator.Name`,- `Indicator.Code`) %>%
      tidyr::pivot_longer(-c(`Country.Name`, `Country.Code`),
                   names_to = "year", values_to = "ppp_multiplier") %>%
      dplyr::mutate(year = as.integer(stringr::str_remove(year, "X"))) %>%
      dplyr::filter(!is.na(ppp_multiplier))

    file_path <- paste0(input_dir, "/API_PA.NUS.PRVT.PP_DS2_en_csv_v2_5734724.csv")
    prvt_pp <- read.csv(file_path, skip = 4) %>%
      dplyr::select(-`Indicator.Name`,- `Indicator.Code`) %>%
      tidyr::pivot_longer(-c(`Country.Name`, `Country.Code`),
                   names_to = "year", values_to = "prvt_pp") %>%
      dplyr::mutate(year = as.integer(stringr::str_remove(year, "X"))) %>%
      dplyr::filter(!is.na(prvt_pp))

    file_path <- paste0(input_dir, "/API_PA.NUS.FCRF_DS2_en_csv_v2_5729637.csv")
    exch_rate <- read.csv(file_path, skip = 4) %>%
      dplyr::select(-`Indicator.Name`,- `Indicator.Code`) %>%
      tidyr::pivot_longer(-c(`Country.Name`, `Country.Code`),
                   names_to = "year", values_to = "exch_rate") %>%
      dplyr::mutate(year = as.integer(stringr::str_remove(year, "X"))) %>%
      dplyr::filter(!is.na(exch_rate))

    # save all objects to an Rdata file
    save(indicators, activities, countries, disaggregate_countries, ppp, prvt_pp, exch_rate
         , ppp_countries, ftf_target_countries, offices, values, udns
         , file = paste0(output_dir, db, ".Rdata"))

  }
}
