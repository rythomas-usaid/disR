#' Calculate key indicators
#'
#' create data set from DIS exports and FTFMS data
#' @param x a data.frame in the format
#' @param disaggregated logical. If FALSE (default), return totals for all levels. If TRUE, return disaggregate totals at the specified levels (IM, OU, and RO).
#' @return data.frame with combined ftfms and DIS data
#'
#'
#' @import tidyverse
#'
#' @export calculate_participation

# Participation EG.3-2 and HLI EG.3-2_OULevel ####
# load("../data/basic.rdata")
calculate_participation <- function(x, disaggregated = FALSE, level = NA, years = NA, ros = "all", types = "Actual") {

  x_names <- c("ic",  "ro", "ou", "a_code", "a_name", "type", "year", "value", "sex")

  if(is.na(years)) {
    years <- unique(x$year)
  }  else if(!is.numeric(years)) {
    stop("years must be a numeric vector or 'all' (the default)")
  }

  if(ros == "all") {
    ros <- unique(x$ro)
  }

  if( !all(x_names %in% names(x) ) ) {
    stop('x must contain the following columns:\n
    c("ic",  "ro", "ou", "a_code", "a_name", "type", "year", "value", "sex")')
  }

  if( !isTRUE( disaggregated )) {
    im_participants <- x %>%
      filter(ic == "eg.3-2"
             & ro %in% ros
             & type %in% types
             & !is.na(sex)
             & year %in% years) %>%
      group_by(ic, ro, ou, a_code, a_name, type, year) %>%
      summarise(`eg.3-2_final` = sum_(value))  %>%
      arrange(year, ro, ou) %>% ungroup() %>%
      mutate(year = as.numeric(str_remove(year, "FY"))
             , organization_level = "IM Total") %>%
      select(-ic) %>%
      # left_join(ou_lookup) %>%
      # select(organization_level, operating_unit, year, participants)
      select(organization_level, everything())

    ou_participants <- x %>%
      filter(ic %in% c("eg.3-2", "eg.3-2_oulevel")
         & ro %in% ros
         & type %in% types
         & !is.na(sex)
         & year %in% years) %>%
      group_by(ic, ro, ou, type, year) %>%
      summarise(value = sum_(value)) %>%
      pivot_wider(names_from = ic, values_from = value) %>%
      mutate(
        `eg.3-2_final` = case_when(
          is.na(`eg.3-2_oulevel`) ~ `eg.3-2`
          , .default = `eg.3-2_oulevel`
          )
    # , flag = case_when(`eg.3-2` < `eg.3-2_oulevel` ~ "Wrong relationship between IM and OU totals")
    ) %>%
      arrange(year, ro, ou) %>% ungroup() %>%
      mutate(year = as.numeric(str_remove(year, "FY"))
             , organization_level = "OU Total") %>%
      select(organization_level, everything())

    ro_participants <- ou_participants  %>%
      group_by(ro, type, year) %>%
      summarise(`eg.3-2` = sum_(`eg.3-2`)
                , `eg.3-2_oulevel` = sum_(`eg.3-2_oulevel`)
                , `eg.3-2_final` = sum_(`eg.3-2_final`)) %>%
      mutate(organization_level = "RO Total", .before = everything())

    aggregated_out <- ungroup(bind_rows(ro_participants, ou_participants, im_participants)) %>%
      relocate(organization_level, ro, ou, a_code, a_name, year, everything())

    return(aggregated_out)

    } else if(isTRUE(disaggregated)) {

      if( ! level %in% c("im", "ou", "ro")) {

        stop('Argument `level` cannot be "all" for disaggregated results. Must be one of "im", "ou", "ro".\n
             Most likely, you meant to set `level = "ou"`.'  )

      } else if(level == "im") {
        disag_out <- x %>%
          filter((ic == "eg.3-2" | ic == "eg.3-2_oulevel")
                 & ro %in% ros
                 & type %in% types
                 # & !is.na(sex)
                 & year %in% years) %>%
          group_by(ic, ro, ou, a_code, a_name, type, d1, d2, sex, size, typeof, year) %>%
          summarise(value = sum_(value)) %>%
          pivot_wider(names_from = ic, values_from = value) %>%
          mutate(
            `eg.3-2_final`= case_when(
              is.na(`eg.3-2_oulevel`) ~ `eg.3-2`
              , .default = `eg.3-2_oulevel`)) %>%
          arrange(year, ro, ou, d1, d2) %>% ungroup() %>%
          mutate(year = as.numeric(str_remove(year, "FY"))
                 , organization_level = "IM Disags") %>%
          select(organization_level, everything())

        } else if(level == "ou"){

        disag_out <- x %>%
          filter((ic == "eg.3-2" | ic == "eg.3-2_oulevel")
                 & ro %in% ros
                 & type %in% types
                 # & !is.na(sex)
                 & year %in% years) %>%
          group_by(ic, ro, ou, type, d1, d2, sex, size, typeof, year) %>%
          summarise(value = sum_(value)) %>%
          pivot_wider(names_from = ic, values_from = value) %>%
          mutate(
            `eg.3-2_final` = case_when(
              is.na(`eg.3-2_oulevel`) ~ `eg.3-2`
              , .default = `eg.3-2_oulevel`)) %>%
          arrange(year, ro, ou, d1, d2) %>% ungroup() %>%
          mutate(year = as.numeric(str_remove(year, "FY"))
                 , organization_level = "OU Disags") %>%
          select(organization_level, everything())

        } else if(level == "ro") {
          disag_out <-  x %>%
            filter((ic == "eg.3-2" | ic == "eg.3-2_oulevel")
                   & ro %in% ros
                   & type %in% types
                   # & !is.na(sex)
                   & year %in% years) %>%
            group_by(ic, ro, ou, type, d1, d2, sex, size, typeof, year) %>%
            summarise(value = sum_(value)) %>%
            pivot_wider(names_from = ic, values_from = value) %>%
            mutate(
              `eg.3-2_final` = case_when(
                is.na(`eg.3-2_oulevel`) ~ `eg.3-2`
                , .default = `eg.3-2_oulevel`)) %>%
            arrange(year, ro, ou, d1, d2) %>% ungroup() %>%
            mutate(year = as.numeric(str_remove(year, "FY"))) %>%

            # After calculating OU disags as above ...
            group_by(ro, type, d1, d2, sex, size, typeof, year) %>%
            summarise(`eg.3-2` = sum_(`eg.3-2`)
                      , `eg.3-2_oulevel` = sum_(`eg.3-2_oulevel`)
                      , `eg.3-2_final` = sum_(`eg.3-2_final`)) %>%
            mutate(organization_level = "RO Disags", .before = everything())

      }

      return(ungroup(disag_out))

    }

}

### from extract ####
# load("../data/combined_extracts.Rdata")
#' @export calculate_participation
calculate_participation_ <- function(df) {

  udns <- c("3.1.2", "3.1.1", "3.1.3", "3.1.4", "3.1.5") # sex UDNs
  ics <- c("EG.3-2", "EG.3-2_OULevel")

  df %>% select(-c(d1, d2, d3,d4)) %>%
    filter(year == 2022 & udn %in% udns & ic %in% ics & indicator.collection.frequency == "Annual") %>%
    filter(if_any(c(actual, target), ~ !is.na(.))) %>%
    left_join(dcw) %>%
    group_by(ic,ro, ou, year, d_name, d1, d2) %>%
    summarise(value = sum_(actual)) %>%
    pivot_wider(names_from = ic, values_from = value) %>%
    mutate(
      `eg.3-2_final` = case_when(
        is.na(`EG.3-2_OULevel`) ~ `EG.3-2`
        , .default = `EG.3-2_OULevel`
      )
      # , flag = case_when(`eg.3-2` < `eg.3-2_oulevel` ~ "Wrong relationship between IM and OU totals")
    ) %>%
    arrange(year, ro, ou, d1, d2) %>% ungroup() %>%
    mutate(year = as.numeric(str_remove(year, "fy")))

}


# Priority targets ####
#' Calculate all the priority target values
#'
#' Takes an export from DIS OU Activty Indicator Results Report
#' @return a list of two data frames, one with the summary of priority targets and one with all the data for each indicator nested by activity
#' @examples priority_targets(df)
#' @export priority_targets
priority_targets <- function(dat) {
dat <- read_export("../../priority_target_KIN_section4/20240902_data/Bureau for Resilience and Food Security - OU Activity Indicator Results Report - Export All.xlsx")
  ### sales ####
  sales_raw <- dat %>%
    select(`Activity Name`, `Activity Code`, `Indicator Code`, UDN
           , `Fiscal Year`, `Disaggregate Name`, Target, Actual, `Collection Review Status`
           , Deviation, `Deviation Narrative`) %>%
    filter(`Indicator Code` == "EG.3.2-26" & UDN == "3")

  if(nrow(sales_raw) > 0L) {
    sales <- sales_raw  %>%
      # pivot_longer(c(Target, Actual), names_to = "Type") %>%
      group_by(`Indicator Code`, `Fiscal Year`) %>%
      summarise(Target = sum_(Target)
                , Actual = sum_(Actual)
                , Pa.IDs = paste0(unique(`Activity Code`), collapse="; ")
                , .groups = "drop") %>%
      # pivot_wider(names_from = Type) %>%
      # reorder columns
      relocate(Target, .before=Actual) %>%
      mutate(`Priority Target` = "PT1: Value of annual sales")

  } else {

    sales <- NULL

  }

  ### gender_financing #####
  financing_udns <- c("3.5.1.2.1", "3.6.1.2.1 ", "3.7.1.2.1"   # value for males
                      , "3.5.1.2.2", "3.6.1.2.2", "3.7.1.2.2"  # value for females
                      , "3.5.2.2.1", "3.6.2.2.1", "3.7.2.2.1"  # number of males
                      , "3.5.2.2.2", "3.6.2.2.2", "3.7.2.2.2") # number of females

  gender_financing_raw <- dat %>%
    select(`Activity Name`, `Activity Code`, `Indicator Code`, UDN
           , `Fiscal Year`, `Disaggregate Name`, Target, Actual) %>%
    filter(`Indicator Code` == "EG.3.2-27" & UDN %in% financing_udns) %>%
    mutate(`Disaggregate Name` = case_when(
      UDN %in% c("3.5.1.2.2", "3.6.1.2.2", "3.7.1.2.2") ~ "Female Value"
      , UDN %in% c("3.5.2.2.2", "3.6.2.2.2", "3.7.2.2.2") ~ "Female Number"
      , UDN %in% c("3.5.1.2.1", "3.6.1.2.1 ", "3.7.1.2.1") ~ "Male Value"
      , UDN %in% c("3.5.2.2.1", "3.6.2.2.1", "3.7.2.2.1") ~ "Male Number"))

  if(nrow(gender_financing_raw) > 0L) {
    gender_financing <- gender_financing_raw %>%
      pivot_longer(c(Target, Actual))  %>%
      select(-UDN) %>%
      pivot_wider(names_from = c(`Disaggregate Name`)
                  , values_from = value
                  , values_fn = sum_) %>%
      mutate(`Male Value` = if_else(is.na(`Male Number`), NA, `Male Value`)
             , `Female Value` = if_else(is.na(`Female Number`), NA, `Female Value`)
             , `Male Number` = if_else(is.na(`Male Value`), NA, `Male Number`)
             , `Female Number` = if_else(is.na(`Female Value`), NA, `Female Number`))  %>%
      mutate(`Gender Finance Ratio` = ((`Female Value` / `Female Number`) / (`Male Value` / `Male Number`))
             , total_number = `Female Number`+`Male Number`
             , .by = c(`Activity Code`, `Fiscal Year`, name)) %>%
      group_by(`Indicator Code`, `Fiscal Year`, name) %>%
      summarise(`Female Value` = sum_(`Female Value`)
                , `Female Number` = sum_(`Female Number`)
                , `Male Value` = sum_(`Male Value`)
                , `Male Number` = sum_(`Male Number`)
                , `Gender Finance Ratio` = weighted.mean(`Gender Finance Ratio`
                                                         , w = total_number
                                                         , na.rm = TRUE)
                , Pa.IDs = paste0(unique(`Activity Code`), collapse="; ")
                , .groups = "drop")  %>%
      select(`Indicator Code`,  `Fiscal Year`, Pa.IDs, name, `Gender Finance Ratio`) %>%
      pivot_wider(names_from = name, values_from = `Gender Finance Ratio`) %>%
      # reorder columns
      relocate(Target, .before=Actual) %>%
      mutate(`Priority Target` = "PT2: Gender parity in access to financing (%)"
             , .before = everything())
  } else {
    gender_financing <- NULL
  }

  ### hectares ####
  hectares_raw <- dat %>%
    select(`Activity Name`, `Activity Code`, `Indicator Code`, UDN
           , `Fiscal Year`, `Disaggregate Name`, Target, Actual) %>%
    filter(`Indicator Code` == "EG.3.2-25" & UDN %in% c("3.1.3.12", "3.2.3.12"))

  if(nrow(hectares_raw) > 0L) {
    hectares <- hectares_raw %>%
      pivot_longer(c(Target, Actual)) %>%
      group_by(`Indicator Code`, `Fiscal Year`, name) %>%
      summarise(value = sum_(value)
                , Pa.IDs = paste0(unique(`Activity Code`), collapse="; ")
                , .groups = "drop") %>%
      pivot_wider(names_from = name) %>%
      # reorder columns
      relocate(Target, .before=Actual) %>%
      mutate(`Priority Target` = "PT3: Hectares under Climate Adaptation and Risk Management Practices")
  } else {
    hectares <- NULL
  }

  ### private sector investment ####
  private_sector_investment_raw <- dat %>%
    select(`Activity Name`, `Activity Code`, `Indicator Code`, UDN
           , `Fiscal Year`, `Disaggregate Name`, Target, Actual) %>%
    filter(`Indicator Code` == "EG.3.1-15" & UDN == "3")

  if(nrow(private_sector_investment_raw) > 0L) {
    private_sector_investment <- private_sector_investment_raw %>%
      pivot_longer(c(Target, Actual)) %>%
      group_by(`Indicator Code`, `Fiscal Year`, name) %>%
      summarise(value = sum_(value)
                , Pa.IDs = paste0(unique(`Activity Code`), collapse="; ")
                , .groups = "drop") %>%
      pivot_wider(names_from = name)  %>%
      # reorder columns
      relocate(Target, .before=Actual) %>%
      mutate(`Priority Target` = "PT4: Private sector investment")
  } else {
    private_sector_investment <- NULL
  }

  ## Organize data for upload ####
  raw_upload <- bind_rows(private_sector_investment_raw, hectares_raw
                          , gender_financing_raw, sales_raw)

  if(nrow(raw_upload) > 0L) {
    raw_upload <- raw_upload %>%
      mutate(activity = paste0(`Activity Code`, ": ", `Activity Name`)
             , .before = everything()) %>%
      select(-`Activity Code`, -`Activity Name`) %>%
      relocate(Deviation, .after = Actual) %>%
      filter(if_any(c("Target", "Actual"), ~ ! is.na(.)), .by = activity) %>%
      nest(.by = activity)
  }

  if(nrow(raw_upload) > 0L) {
    final_upload <- bind_rows(sales, gender_financing, hectares
                              , private_sector_investment) %>%
      mutate(`Fiscal Year` = as.character(`Fiscal Year`))
    if(nrow(final_upload) > 0L ) {
      final_upload <- final_upload %>%
        mutate(`Deviation %` = round( 1 + (Actual - Target) / Target , 2 )
               , Status = case_when(
                 `Fiscal Year` == "2023"  & `Deviation %` < 0.9 ~ "Unmet"
                 , `Fiscal Year` == "2023"  & between(`Deviation %`, 0.9, 1) ~ "Met"
                 , `Fiscal Year` == "2023"  & `Deviation %` > 1 ~ "Exceeded"
                 , is.na(`Deviation %`) ~ "Status undefined or not applicable.")
        ) %>%
        mutate(`Fiscal Year` = as.character(`Fiscal Year`))

      final_upload <- template %>%
        left_join(final_upload) %>%
        # reorder columns
        relocate(Target, .before=Actual) %>%
        relocate(`Priority Target`, Pa.IDs, .before=everything()) %>%
        mutate(`Fiscal Year` = as.integer(`Fiscal Year`)) %>%
        mutate(Pa.IDs = case_when(is.na(Pa.IDs) ~ "None reporting"
                                  , .default = Pa.IDs))
    }
  } else final_upload <- NULL

  return(list(final_upload, raw_upload))

}

## ... from extract ####
#' Calculate all the priority target values
#'
#' Takes an export from DIS OU Activty Indicator Results Report
#' @return a list of two data frames, one with the summary of priority targets and one with all the data for each indicator nested by activity
#' @examples priority_targets_(df)
#' @export priority_targets_
priority_targets_ <- function(df, level = "im") {

  ### gender_financing #####
  if(level == "im") {
    gender_financing <- df %>%
      get_im_financing_ratio()

  } else if (level == "ou") {

    gender_financing <- df %>%
      get_im_financing_ratio() %>%
      # For OU
      group_by(ro, ou, year, name) %>%
      summarize_financing_ratio()


  } else if(level == "ro") {

    if(length(unique(df$ro)) == 1 ) {
      ous <- unique(df$ou)
      warning(paste("Input data contains only one unique reporting organization. Results are for the following OUs:", paste(ous, collapse = ",")))
    }

    gender_financing <- df %>%

      get_im_financing_ratio() %>%
      # For OU
      group_by(ro, ou, year, name) %>%
      summarize_financing_ratio() %>%
      # For RO
      group_by(ro, year, name) %>%
      summarize_financing_ratio()

  }
  return(gender_financing)
}


# Helper functions ####
get_im_financing_ratio <- function(x){
  # if( ! all(col_names %in% names(x))) stop("! Check column names.")

  ### gender_financing_udns ###
  financing_udns <- c("3.5.1.2.1", "3.6.1.2.1 ", "3.7.1.2.1"   # value for males
                      , "3.5.1.2.2", "3.6.1.2.2", "3.7.1.2.2"  # value for females
                      , "3.5.2.2.1", "3.6.2.2.1", "3.7.2.2.1"  # number of males
                      , "3.5.2.2.2", "3.6.2.2.2", "3.7.2.2.2") # number of females
  x %>% select(ro, ou, a_name, a_code, ic, udn
               , year, d_name, target, actual) %>%
    filter(ic == "EG.3.2-27" & udn %in% financing_udns) %>%
    mutate(d_name = case_when(
      udn %in% c("3.5.1.2.2", "3.6.1.2.2", "3.7.1.2.2") ~ "Female Value"
      , udn %in% c("3.5.2.2.2", "3.6.2.2.2", "3.7.2.2.2") ~ "Female Number"
      , udn %in% c("3.5.1.2.1", "3.6.1.2.1 ", "3.7.1.2.1") ~ "Male Value"
      , udn %in% c("3.5.2.2.1", "3.6.2.2.1", "3.7.2.2.1") ~ "Male Number")) %>%
    pivot_longer(c(target, actual))  %>%
    select(-c(udn,ic)) %>%
    pivot_wider(names_from = c(d_name)
                , values_from = value
                , values_fn = sum_) %>%
    mutate(`Male Value` = if_else(is.na(`Male Number`), NA, `Male Value`)
           , `Female Value` = if_else(is.na(`Female Number`), NA, `Female Value`)
           , `Male Number` = if_else(is.na(`Male Value`), NA, `Male Number`)
           , `Female Number` = if_else(is.na(`Female Value`), NA, `Female Number`))  %>%
    mutate(`Female Per Person` = `Female Value` / `Female Number`
           , `Male Per person` = `Male Value` / `Male Number`
           , `Gender Finance Ratio` = (`Female Per Person` / `Male Per person`)
           , .by = c(a_code, year, name))
}

summarize_financing_ratio <- function(x) {
  # ---------- Additional summarizing from above IM-level
  x %>%
    summarise(`Female Value` = sum_(`Female Value`)
              , `Female Number` = sum_(`Female Number`)
              , `Male Value` = sum_(`Male Value`)
              , `Male Number` = sum_(`Male Number`)
              , `Female Per Person` = `Female Value` / `Female Number`
              , `Male Per person` = `Male Value` / `Male Number`
              , `Gender Finance Ratio` = (sum_(`Female Value`) / sum_(`Female Number`)) / ( sum_(`Male Value`) / sum_(`Male Number`))
              , a_code = paste0(unique(a_code), collapse="; ")
              , .groups = "drop")
}

