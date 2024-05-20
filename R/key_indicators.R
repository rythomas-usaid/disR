#' Calculate key indicators
#'
#' create data set from DIS exports and FTFMS data
#' @param x a dataframe that has been processed from "Magnus's" sheets
#' @param disaggregated logical. If FALSE (default), return totals for all levels. If TRUE, return disaggregate totals at the specified levels (IM, OU, and RO).
#' @return data.frame with combined ftfms and DIS data
#'
#'
#' @import tidyverse
#'
#' @export calculate_participation

# Participation EG.3-2 and HLI EG.3-2_OULevel ####
# load("../data/basic.rdata")
calculate_participation <- function(x, disaggregated = FALSE, level = NA, years = NA, ros = "all", types = "actual") {
  x_names <- c("ic",  "ro", "ou", "a_code", "a_name", "type", "year", "value", "sex")
  if(all(is.na(years))) {
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
             & str_to_lower(type) %in% str_to_lower(types)
             & !is.na(sex)
             & year %in% years) %>%
      pivot_wider(names_from = ic, values_from = value) %>%
      group_by(ro, ou, a_code, a_name, type, year) %>%
      summarise(`eg.3-2`=sum_(`eg.3-2`), `eg.3-2_final`=sum_(`eg.3-2`))  %>%
      arrange(year, ro, ou) %>% ungroup() %>%
      mutate(organization_level = "IM Total") %>%
      select(organization_level, everything())

    ou_participants <- x %>%
      filter(ic %in% c("eg.3-2", "eg.3-2_oulevel")
         & ro %in% ros
         & str_to_lower(type) %in% str_to_lower(types)
         & !is.na(sex)
         & year %in% years) %>%
      pivot_wider(names_from = ic, values_from = value) %>%
      group_by(ro, ou, a_code, a_name, type, year) %>%
      summarise(`eg.3-2`=sum_(`eg.3-2`)
                , `eg.3-2_oulevel`= sum_(`eg.3-2_oulevel`), .groups = "drop") %>%
      group_by(ro, ou) %>%
      mutate(`eg.3-2_final` = case_when(
        all(is.na(`eg.3-2_oulevel`)) ~ `eg.3-2`, .default = `eg.3-2_oulevel`)) %>%
      arrange(year, ro, ou) %>% ungroup() %>%
      mutate(organization_level = "OU Total") %>%
      select(organization_level, everything())

    ro_participants <- ou_participants  %>%
      group_by(ro, type, year) %>%
      summarise(`eg.3-2` = sum_(`eg.3-2`)
                , `eg.3-2_oulevel` = sum_(`eg.3-2_oulevel`)
                , `eg.3-2_final` = sum_(`eg.3-2_final`)) %>%
      mutate(organization_level = "RO Total", .before = everything())

    aggregated_out <- ungroup(bind_rows(
      ro_participants, ou_participants, im_participants)) %>%
      relocate(organization_level, ro, ou, a_code, a_name, year, everything())

    return(aggregated_out)
    } else if(isTRUE(disaggregated)) {
      if(!level %in% c("im", "ou", "ro")) {
        stop('Argument `level` cannot be "all" for disaggregated results. Must be one of "im", "ou", "ro".\n
             Most likely, you meant to set `level = "ou"`.'  )
      } else if(level == "im") {
        disag_out <- x %>%
          filter((ic == "eg.3-2" | ic == "eg.3-2_oulevel")
                 & ro %in% ros
                 & str_to_lower(type) %in% str_to_lower(types)
                 & year %in% years)  %>%
          select(ic, ro, ou, a_name, a_code, d1, d2, sex, size, typeof, type, year, value) %>%
          pivot_wider(names_from = ic, values_from = value) %>%
          mutate(`eg.3-2_final` = `eg.3-2`) %>%
          arrange(year, ro, ou, d1, d2) %>% ungroup() %>%
          mutate(organization_level = "IM Disags") %>%
          select(organization_level, everything())
        } else if(level == "ou"){
        disag_out <- x %>%
          filter((ic == "eg.3-2" | ic == "eg.3-2_oulevel")
                 & ro %in% ros
                 & str_to_lower(type) %in% str_to_lower(types)
                 & year %in% years) %>%
          select(ic, ro, ou, a_name, a_code, d1, d2, sex, size, typeof, type, year, value) %>%
          pivot_wider(names_from = ic, values_from = value) %>%   ungroup() %>%
          group_by(ro, ou, d1, d2, sex, size, typeof, type, year) %>%
          summarize(`eg.3-2` = sum_(`eg.3-2`)
                    , `eg.3-2_oulevel` = sum_(`eg.3-2_oulevel`)) %>%
          group_by(ro, ou, d1) %>%
          mutate(`eg.3-2_final` = case_when(
            all(is.na(`eg.3-2_oulevel`)) ~ `eg.3-2`, .default = `eg.3-2_oulevel`)) %>%
          arrange(year, ro, ou, d1, d2) %>% ungroup() %>%
          mutate(organization_level = "OU Disags") %>%
          select(organization_level, everything())
        } else if(level == "ro") {
          disag_out <-  x %>%
            filter((ic == "eg.3-2" | ic == "eg.3-2_oulevel")
                   & ro %in% ros
                   & str_to_lower(type) %in% str_to_lower(types)
                   & year %in% years) %>%
            select(ic, ro, ou, a_name, a_code, d1, d2, sex, size, typeof, type, year, value) %>%
            pivot_wider(names_from = ic, values_from = value) %>%   ungroup() %>%
            group_by(ro, ou, d1, d2, sex, size, typeof, type, year) %>%
            summarize(`eg.3-2` = sum_(`eg.3-2`)
                      , `eg.3-2_oulevel` = sum_(`eg.3-2_oulevel`)) %>%
            group_by(ro, ou, d1) %>%
            mutate(`eg.3-2_final` = case_when(
              all(is.na(`eg.3-2_oulevel`)) ~ `eg.3-2`, .default = `eg.3-2_oulevel`)) %>%
            # After calculating OU disags as above ...
            group_by(ro, d1, d2, sex, size, typeof, type, year) %>%
            summarise(`eg.3-2` = sum_(`eg.3-2`)
                      , `eg.3-2_oulevel` = sum_(`eg.3-2_oulevel`)
                      , `eg.3-2_final` = sum_(`eg.3-2_final`)
                      , .groups = "drop") %>%
            mutate(organization_level = "RO Disags", .before = everything())
      }
      return(ungroup(disag_out))
    }
}

### from extract ####
# load("../data/combined_extracts.Rdata")
#' @export calculate_participation_
calculate_participation_ <- function(x, udns = c("3.1.2", "3.1.1", "3.1.3", "3.1.4", "3.1.5"),
                                     disaggregated = FALSE, level = NA, years = NA, ros = "all", types = "actual") {

  # sex udns
  ics <- c("EG.3-2", "EG.3-2_OULevel")

  df %>% select(-c(d1, d2, d3,d4)) %>%
    filter(year %in% years & udn %in% udns & ic %in% ics & indicator.collection.frequency == "Annual") %>%
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
#' @examples priority_targets(data)
#' @export priority_targets
priority_targets <- function(x) {
# x <- read_excel_dis("../priority_target_KIN_section4/data/Bangladesh - OU Activity Indicator Results Report - Export All.xlsx")
  ### sales ####
  sales_raw <- x %>%
    select(a_name, a_code, ic, udn
           , year, d_name, target, actual, collection_review_status
           , deviation, deviation_narrative) %>%
    filter(ic == "EG.3.2-26" & udn == "3")

  if(nrow(sales_raw) > 0L) {
    sales <- sales_raw  %>%
      # pivot_longer(c(target, actual), names_to = "Type") %>%
      group_by(ic, year) %>%
      summarise(target = sum_(target)
                , actual = sum_(actual)
                , Pa.IDs = paste0(unique(a_code), collapse="; ")
                , .groups = "drop") %>%
      # pivot_wider(names_from = Type) %>%
      # reorder columns
      relocate(target, .before=actual) %>%
      mutate(`Priority target` = "PT1: Value of annual sales")
  } else {
    sales <- NULL
  }
  ### gender_financing #####
  financing_udns <- c("3.5.1.2.2", "3.6.1.2.2", "3.7.1.2.2"    # value for females
                      , "3.5.2.2.2", "3.6.2.2.2", "3.7.2.2.2"  # number of females
                      , "3.5.1.2.1", "3.6.1.2.1 ", "3.7.1.2.1" # value for males
                      , "3.5.2.2.1", "3.6.2.2.1", "3.7.2.2.1") # number of males

  gender_financing_raw <- x %>%
    select(a_name, a_code, ic, udn
           , year, d_name, target, actual) %>%
    filter(ic == "EG.3.2-27" & udn %in% financing_udns) %>%
    mutate(d_name = case_when(
      udn %in% c("3.5.1.2.2", "3.6.1.2.2", "3.7.1.2.2") ~ "Female Value"
      , udn %in% c("3.5.2.2.2", "3.6.2.2.2", "3.7.2.2.2") ~ "Female Number"
      , udn %in% c("3.5.1.2.1", "3.6.1.2.1 ", "3.7.1.2.1") ~ "Male Value"
      , udn %in% c("3.5.2.2.1", "3.6.2.2.1", "3.7.2.2.1") ~ "Male Number"))

  if(nrow(gender_financing_raw) > 0L) {
    gender_financing <- gender_financing_raw %>%
      pivot_longer(c(target, actual))  %>%
      select(-udn) %>%
      pivot_wider(names_from = c(d_name)
                  , values_from = value
                  , values_fn = sum_) %>%
      mutate(`Male Value` = if_else(is.na(`Male Number`), NA, `Male Value`)
             , `Female Value` = if_else(is.na(`Female Number`), NA, `Female Value`)
             , `Male Number` = if_else(is.na(`Male Value`), NA, `Male Number`)
             , `Female Number` = if_else(is.na(`Female Value`), NA, `Female Number`))  %>%
      mutate(`Gender Finance Ratio` = ((`Female Value` / `Female Number`) / (`Male Value` / `Male Number`))
             , total_number = `Female Number`+`Male Number`
             , .by = c(a_code, year, name)) %>%
      group_by(ic, year, name) %>%
      summarise(`Female Value` = sum_(`Female Value`)
                , `Female Number` = sum_(`Female Number`)
                , `Male Value` = sum_(`Male Value`)
                , `Male Number` = sum_(`Male Number`)
                , `Gender Finance Ratio` = weighted.mean(`Gender Finance Ratio`
                                                         , w = total_number
                                                         , na.rm = TRUE)
                , Pa.IDs = paste0(unique(a_code), collapse="; ")
                , .groups = "drop")  %>%
      select(ic,  year, Pa.IDs, name, `Gender Finance Ratio`) %>%
      pivot_wider(names_from = name, values_from = `Gender Finance Ratio`) %>%
      # reorder columns
      relocate(target, .before=actual) %>%
      mutate(`Priority target` = "PT2: Gender parity in access to financing (%)"
             , .before = everything())
  } else {
    gender_financing <- NULL
  }

  ### hectares ####
  hectares_raw <- x %>%
    select(a_name, a_code, ic, udn
           , year, d_name, target, actual) %>%
    filter(ic == "EG.3.2-25" & udn %in% c("3.1.3.12", "3.2.3.12"))

  if(nrow(hectares_raw) > 0L) {
    hectares <- hectares_raw %>%
      pivot_longer(c(target, actual)) %>%
      group_by(ic, year, name) %>%
      summarise(value = sum_(value)
                , Pa.IDs = paste0(unique(a_code), collapse="; ")
                , .groups = "drop") %>%
      pivot_wider(names_from = name) %>%
      # reorder columns
      relocate(target, .before=actual) %>%
      mutate(`Priority target` = "PT3: Hectares under Climate Adaptation and Risk Management Practices")
  } else {
    hectares <- NULL
  }

  ### private sector investment ####
  private_sector_investment_raw <- x %>%
    select(a_name, a_code, ic, udn
           , year, d_name, target, actual) %>%
    filter(ic == "EG.3.1-15" & udn == "3")

  if(nrow(private_sector_investment_raw) > 0L) {
    private_sector_investment <- private_sector_investment_raw %>%
      pivot_longer(c(target, actual)) %>%
      group_by(ic, year, name) %>%
      summarise(value = sum_(value)
                , Pa.IDs = paste0(unique(a_code), collapse="; ")
                , .groups = "drop") %>%
      pivot_wider(names_from = name)  %>%
      # reorder columns
      relocate(target, .before=actual) %>%
      mutate(`Priority target` = "PT4: Private sector investment")
  } else {
    private_sector_investment <- NULL
  }

  ## Organize data for upload ####
  raw_data <- bind_rows(private_sector_investment_raw, hectares_raw
                          , gender_financing_raw, sales_raw)

  if(nrow(raw_data) > 0L) {
    raw_data <- raw_data %>%
      mutate(activity = paste0(a_code, ": ", a_name)
             , .before = everything()) %>%
      select(-a_code, -a_name) %>%
      relocate(deviation, .after = actual) %>%
      filter(if_any(c("target", "actual"), ~ ! is.na(.)), .by = activity) %>%
      nest(.by = activity)
  }

  if(nrow(raw_data) > 0L) {
    final_upload <- bind_rows(sales, gender_financing, hectares
                              , private_sector_investment) %>%
      mutate(year = as.character(year))
    if(nrow(final_upload) > 0L ) {
      final_upload <- final_upload %>%
        mutate(`deviation %` = round( 1 + (actual - target) / target , 2 )
               , Status = case_when(
                 year == "2023"  & `deviation %` < 0.9 ~ "Unmet"
                 , year == "2023"  & between(`deviation %`, 0.9, 1) ~ "Met"
                 , year == "2023"  & `deviation %` > 1 ~ "Exceeded"
                 , is.na(`deviation %`) ~ "Status undefined or not applicable.")
        ) %>%
        mutate(year = as.character(year))
    }
  } else final_upload <- NULL

  return(list(final_upload, raw_data))
}

# ... PTs FROM EXTRACT  ####
#' Calculate all the priority target values
#'
#' Takes an export from DIS OU Activity Indicator Results Report
#' @return a list of two data frames, one with the summary of priority targets and one with all the data for each indicator nested by activity
#' @export gender_financing_
gender_financing_ <- function(x, level = "im") {
  if(level == "im") {
    gender_financing <- x %>%
      get_im_financing_ratio()
  } else if (level == "ou") {
    gender_financing <- x %>%
      get_im_financing_ratio() %>%
      # For OU
      group_by(ro, ou, year, name) %>%
      summarize_financing_ratio(level = level)
  } else if(level == "ro") {
    if(length(unique(x$ro)) == 1 ) {
      ous <- unique(x$ou)
      warning(paste("Input data contains only one unique reporting organization. Results are for the following OUs:", paste(ous, collapse = ",")))
    }
    gender_financing <- x %>%
      get_im_financing_ratio() %>%
      # For OU
      group_by(ro, ou, year, name) %>%
      summarize_financing_ratio(level = level) %>%
      # For RO
      group_by(ro, year, name) %>%
      summarize_financing_ratio(level = level)
  }
  return(gender_financing)
}

#' @export sales_
sales_ <- function(x, level = "ou") {
  sales <- x %>%
    select(ro, ou, a_name, a_code, ic, udn, year, d_name, target, actual) %>%
    filter(ic == "EG.3.2-26" & udn == "3" ) %>% pivot_longer(c(target, actual))
  if(level == "ou") {
    sales <- sales %>%
      group_by(ro, ou, ic, name, year) %>%
      summarise(value = sum_(value)
                , a_codes = paste0(unique(a_code), collapse="; ")
                , .groups = "drop")
  }
  return(sales)
}

#' @export hectares_
hectares_ <- function(x) {
  hectares <- x %>%
    select(ro, ou, a_name, a_code, ic, udn
           , year, d_name, target, actual) %>%
    filter(ic == "EG.3.2-25" & udn %in% c("3.1.3.12", "3.2.3.12")) %>%
    # HECTARES Raw is above.
    pivot_longer(c(target, actual)) %>%
      group_by(ro, ou, ic, year, name) %>%
      summarise(value = sum_(value)
                , a_codes = paste0(unique(a_code), collapse="; ")
                , .groups = "drop")
  return(hectares)
}

#' @export psi_
psi_ <- function(x, level = "ou", max_year = year(Sys.Date())) {
  if(level == "im") {
    psi <- x %>%
      select(ro, ou, a_name, a_code, ic, udn
             , year, d_name, target, actual) %>%
      filter((year >=2022 & ic == "EG.3.1-15" & udn == "3") |
               (year < 2022  & ic == "EG.3.1-14" & udn == "3.1.2")) %>%
      mutate(ic = "EG.3.1-15/-14") %>%
      # RAW ABOVE. perhaps split out later
      # pivot_longer(c(target, actual)) %>%
      group_by(ro, ou, a_code, a_name, ic, year) %>%
      summarise(target = sum_(target)
                , actual = sum_(actual)
                , .groups = "drop") %>%
      group_by(ro, ou, a_code, a_name, ic) %>%
      complete(year = first(year):max_year) %>%
      group_by(ro, ou, a_code, a_name) %>%
      mutate(actual.lag.2 = lag(actual, n=2, order_by = year),
             actual.lag.1 = lag(actual, n=1, order_by = year),
             target.lag.2 = lag(target, n=2, order_by = year),
             target.lag.1 = lag(target, n=1, order_by = year),) %>%
      rowwise() %>% mutate(actual_3y = mean(c(actual, actual.lag.1, actual.lag.2), na.rm=T)
                           , target_3y = mean(c(target, target.lag.1, target.lag.2), na.rm=T)
                           , .after = actual) %>%
      select(-contains("lag")) %>%
      pivot_longer(starts_with(c("target", "actual")))
  } else if(level == "ou") {
  psi <- x %>%
    select(ro, ou, a_name, a_code, ic, udn
           , year, d_name, target, actual) %>%
    filter((year >=2022 & ic == "EG.3.1-15" & udn == "3") |
             (year < 2022  & ic == "EG.3.1-14" & udn == "3.1.2")) %>%
    mutate(ic = "EG.3.1-15/-14") %>%
    # RAW ABOVE. perhaps split out later
    # pivot_longer(c(target, actual)) %>%
    group_by(ro, ou, ic, year) %>%
    summarise(target = sum_(target)
              , actual = sum_(actual)
              , a_codes = paste0(unique(a_code), collapse="; ")
              , .groups = "drop") %>%
    # psi_() %>%
    # filter(name == "actual") %>% #select(-c(a_codes, name)) %>%
    group_by(ro, ou, ic) %>%
    complete(year = first(year):year(Sys.Date())) %>%
    group_by(ro, ou) %>%
    mutate(actual.lag.2 = lag(actual, n=2, order_by = year),
           actual.lag.1 = lag(actual, n=1, order_by = year),
           target.lag.2 = lag(target, n=2, order_by = year),
           target.lag.1 = lag(target, n=1, order_by = year),) %>%
    rowwise() %>% mutate(actual_3y = mean(c(actual, actual.lag.1, actual.lag.2), na.rm=T)
                         , target_3y = mean(c(target, target.lag.1, target.lag.2), na.rm=T)
                         , .after = actual) %>%
    select(-contains("lag")) %>%
    pivot_longer(starts_with(c("target", "actual")))
  }

  return(psi)
  }

#' @export mddw_
mddw_ <- function(x) {
  mddw <- x %>%
    select(ro, ou, a_name, a_code, ic, udn
           , year, d_name, target, actual) %>%
    filter(ic == "HL.9.1-d" & udn == "3") %>%
    # RAW ABOVE. perhaps split out later
    pivot_longer(c(target, actual)) %>%
    group_by(ro, ou, ic, year, name) %>%
    summarise(value = sum_(value)
              , a_codes = paste0(unique(a_code), collapse="; ")
              , .groups = "drop")
  return(mddw)
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
           , `Male Per Person` = `Male Value` / `Male Number`
           , value = (`Female Per Person` / `Male Per Person`)
           , .by = c(a_code, year, name)) %>%
    ungroup()
}

summarize_financing_ratio <- function(x, level) {
  if (level == "im") stop("ERROR: Cannot summarize IM level.")
  # ---------- Additional summarizing from above IM-level
  x %>%
    summarise(`Female Value` = sum_(`Female Value`)
              , `Female Number` = sum_(`Female Number`)
              , `Male Value` = sum_(`Male Value`)
              , `Male Number` = sum_(`Male Number`)
              , `Female Per Person` = `Female Value` / `Female Number`
              , `Male Per Person` = `Male Value` / `Male Number`
              , value = (sum_(`Female Value`) / sum_(`Female Number`)) / ( sum_(`Male Value`) / sum_(`Male Number`))
              , a_codes = paste0(unique(a_code), collapse="; ")
              , .groups = "drop")
}

