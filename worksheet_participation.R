library(DBI)
library(tidyverse)
library(disR)
library(googlesheets4)

con <- dbConnect(RSQLite::SQLite(), dbname = "../data/2024_04_30/dis_extract.db")

dbListTables(con)

extract <- dbReadTable(con, "extract")

dcw <- read_sheet("1JAI0TZQU8JkkPWhDKZkAYOhWJzrsdRvlZvh0lcP2590"
                  , sheet = "Disaggregates and UDNs") %>%
  filter((ic == "EG.3-2" | ic == "EG.3-2_OULevel"))

# OU Disags ------------------
ou_disags <- extract  %>%
  filter((ic == "EG.3-2" | ic == "EG.3-2_OULevel") & collection_period_frequency  == "Annual") %>%
  inner_join(dcw) %>%
  select(ic, ro, ou, a_name, a_code, d1, d2, sex, size, typeof, year, actual) %>%
  # filter(a_code %in% c(2339, 4553) & year == 2023)
  # mutate(actual = case_when(ic == "EG.3-2" & a_code == 2012 & year == 2023 & d1 == "sex" & d2 == "disaggregates not available" ~ 0
  #                           , ic == "EG.3-2" & a_code == 2012 & year == 2023 & d2 == "male" ~ 743693
  #                           , ic == "EG.3-2" & a_code == 2012 & year == 2023 & d2 == "female" ~ 267469
  #                           , ic == "EG.3-2" & a_code == 2012 & year == 2023 & d2 == "producer: smallholder farmer" ~ 1011162
  #                           , .default = actual)) %>%
  # filter(!is.na(d1)) %>% #group_by(ro, ou, a_name, a_code, d1, d2, sex, size, typeof, year) %>% filter(n() > 1) %>% arrange(ro, ou, a_name, a_code, country, d1, d2, sex, size, typeof, year)
  pivot_wider(names_from = ic, values_from = actual, values_fn = sum_) %>% ungroup() %>%

  # filter(d1 == "sex") %>%

  group_by(ro, ou, d1, d2, year) %>%
  summarize(`EG.3-2` = sum_(`EG.3-2`)
            , `EG.3-2_OULevel` = sum_(`EG.3-2_OULevel`)) %>%
  group_by(ro, ou, d1, d2, year) %>%
  mutate(`EG.3-2_final` = case_when(
    all(is.na(`EG.3-2_OULevel`)) ~ `EG.3-2`, .default = `EG.3-2_OULevel`)) %>%

  arrange(year, ro, ou) %>% ungroup() %>% filter(if_any(c(`EG.3-2_final`, `EG.3-2`, `EG.3-2_OULevel`), ~ ! is.na(.))) %>%
  mutate(organization_level = "OU Disags") %>%
  select(organization_level, everything())

sheet_write(ou_disags, "1jqdQPUUKO5YW_cmXrtjOeMKRUfGJ81Fldd94bupWmnE",
            sheet = "FY23-OU Disags")

# RO Disags ------------------
ro_disags <- ou_disags %>%

    group_by(ro, d1, d2, year) %>%
    summarise(`EG.3-2` = sum_(`EG.3-2`)
              , `EG.3-2_OULevel` = sum_(`EG.3-2_OULevel`)
              , `EG.3-2_final` = sum_(`EG.3-2_final`)) %>%
    mutate(organization_level = "RO Disags", .before = everything())

sheet_write(ro_disags, "1jqdQPUUKO5YW_cmXrtjOeMKRUfGJ81Fldd94bupWmnE",
            sheet = "RO Disags")

ro_disags %>% filter(d1 == "sex") %>%
  group_by(year, d2) %>%
  summarise(`EG.3-2` = sum_(`EG.3-2`)
            , `EG.3-2_OULevel` = sum_(`EG.3-2_OULevel`)
            , `EG.3-2_final` = sum_(`EG.3-2_final`)) %>%
  select(-`EG.3-2`, -`EG.3-2_OULevel`) %>%
  pivot_wider(names_from = year, values_from = `EG.3-2_final`) %>%
  sheet_write("1jqdQPUUKO5YW_cmXrtjOeMKRUfGJ81Fldd94bupWmnE",
              sheet = "RO Disags -- corrected STMA -- Sex only")


# All totals w STMA correction -------------
## IM Total ---------------
im_total <- extract  %>%
  inner_join(dcw) %>%
  filter((ic == "EG.3-2" | ic == "EG.3-2_OULevel")
         & collection_period_frequency  == "Annual"
         & !is.na(sex)) %>%
   # correct for STMA
  mutate(actual = case_when(ic == "EG.3-2" & a_code == 2012 & year == 2023 & d1 == "sex" & d2 == "disaggregates not available" ~ 0
                            , ic == "EG.3-2" & a_code == 2012 & year == 2023 & d2 == "male" ~ 743693
                            , ic == "EG.3-2" & a_code == 2012 & year == 2023 & d2 == "female" ~ 267469
                            , ic == "EG.3-2" & a_code == 2012 & year == 2023 & d2 == "producer: smallholder farmer" ~ 1011162
                            , .default = actual)) %>%
    # filter(a_code %in% c(2339, 4553) & year == 2023)
  select(ic, ro, ou, a_name, a_code, year, actual) %>%

  pivot_wider(names_from = ic, values_from = actual, values_fn = sum_) %>%
  group_by(ro, ou, a_code, a_name, year) %>%
  summarise(`EG.3-2`=sum_(`EG.3-2`), `EG.3-2_final`=sum_(`EG.3-2`))  %>%
  arrange(year, ro, ou) %>% ungroup() %>%
  mutate(organization_level = "IM Total") %>%
  select(organization_level, everything())

## OU Total --------------
ou_total <- extract  %>%
  inner_join(dcw) %>%
  # correct for STMA
  # mutate(actual = case_when(ic == "EG.3-2" & a_code == 2012 & year == 2023 & d1 == "sex" & d2 == "disaggregates not available" ~ 0
  #
  #                           , ic == "EG.3-2" & a_code == 2012 & year == 2023 & d2 == "male" ~ 743693
  #                           , ic == "EG.3-2" & a_code == 2012 & year == 2023 & d2 == "female" ~ 267469
  #                           , ic == "EG.3-2" & a_code == 2012 & year == 2023 & d2 == "producer: smallholder farmer" ~ 1011162
  #                           , .default = actual)) %>%
  filter((ic == "EG.3-2" | ic == "EG.3-2_OULevel")
         & collection_period_frequency  == "Annual"
         & !is.na(sex)) %>%
  select(ic, ro, ou, a_name, a_code, sex, year, actual) %>%
  # filter(a_code %in% c(2339, 4553) & year == 2023)
  pivot_wider(names_from = ic, values_from = actual, values_fn = sum_) %>% ungroup() %>%
  group_by(ro, ou,  year) %>%
  summarize(`EG.3-2` = sum_(`EG.3-2`)
            , `EG.3-2_OULevel` = sum_(`EG.3-2_OULevel`)) %>%
  group_by(ro, ou, year) %>%
  mutate(`EG.3-2_final` = case_when(
    all(is.na(`EG.3-2_OULevel`)) ~ `EG.3-2`, .default = `EG.3-2_OULevel`)) %>%

  arrange(year, ro, ou) %>% ungroup() %>% filter(if_any(c(`EG.3-2_final`, `EG.3-2`, `EG.3-2_OULevel`), ~ ! is.na(.))) %>%
  mutate(organization_level = "OU Total") %>%
  select(organization_level, everything())

## RO Total --------------
ro_total <- ou_total %>%
  group_by(ro, year) %>%
  summarise(`EG.3-2` = sum_(`EG.3-2`)
            , `EG.3-2_OULevel` = sum_(`EG.3-2_OULevel`)
            , `EG.3-2_final` = sum_(`EG.3-2_final`)
            , .groups = "drop") %>%
  mutate(organization_level = "RO Total", .before = everything())

aggregated_out <- ungroup(bind_rows(
  ro_total, ou_total, im_total)) %>%
  relocate(organization_level, ro, ou, a_code, a_name, year, everything())

sheet_write(aggregated_out
            , "1jqdQPUUKO5YW_cmXrtjOeMKRUfGJ81Fldd94bupWmnE"
            , sheet = "RO, OU, IM Totals -- corrected STMA")



# All totals as in DIS -------------
## IM Total ---------------
im_total <- extract  %>%
  inner_join(dcw) %>%
  filter((ic == "EG.3-2" | ic == "EG.3-2_OULevel")
         & collection_period_frequency  == "Annual"
         & !is.na(sex)) %>%
  # # correct for STMA
  # mutate(actual = case_when(ic == "EG.3-2" & a_code == 2012 & year == 2023 & d2 == "female" ~ 267469
  #                           , ic == "EG.3-2" & a_code == 2012 & year == 2023 & d2 == "male" ~ 743693
  #                           , ic == "EG.3-2" & a_code == 2012 & year == 2023 & d2 == "producer: smallholder farmer" ~ 1011162
  #                           , .default = actual)) %>%
  # filter(a_code %in% c(2339, 4553) & year == 2023)
  select(ic, ro, ou, a_name, a_code, year, actual) %>%

  pivot_wider(names_from = ic, values_from = actual, values_fn = sum_) %>%
  group_by(ro, ou, a_code, a_name, year) %>%
  summarise(`EG.3-2`=sum_(`EG.3-2`), `EG.3-2_final`=sum_(`EG.3-2`))  %>%
  arrange(year, ro, ou) %>% ungroup() %>%
  mutate(organization_level = "IM Total") %>%
  select(organization_level, everything())

## OU Total --------------
ou_total <- extract  %>%
  inner_join(dcw) %>%
  # correct for STMA
  # mutate(actual = case_when(ic == "EG.3-2" & a_code == 2012 & year == 2023 & d2 == "female" ~ 267469
  #                           , ic == "EG.3-2" & a_code == 2012 & year == 2023 & d2 == "male" ~ 743693
  #                           , ic == "EG.3-2" & a_code == 2012 & year == 2023 & d2 == "producer: smallholder farmer" ~ 1011162
  #                           , .default = actual)) %>%
  filter((ic == "EG.3-2" | ic == "EG.3-2_OULevel")
         & collection_period_frequency  == "Annual"
         & !is.na(sex)) %>%
  select(ic, ro, ou, a_name, a_code, sex, year, actual) %>%
  # filter(a_code %in% c(2339, 4553) & year == 2023)
  pivot_wider(names_from = ic, values_from = actual, values_fn = sum_) %>% ungroup() %>%
  group_by(ro, ou,  year) %>%
  summarize(`EG.3-2` = sum_(`EG.3-2`)
            , `EG.3-2_OULevel` = sum_(`EG.3-2_OULevel`)) %>%
  group_by(ro, ou, year) %>%
  mutate(`EG.3-2_final` = case_when(
    all(is.na(`EG.3-2_OULevel`)) ~ `EG.3-2`, .default = `EG.3-2_OULevel`)) %>%

  arrange(year, ro, ou) %>% ungroup() %>% filter(if_any(c(`EG.3-2_final`, `EG.3-2`, `EG.3-2_OULevel`), ~ ! is.na(.))) %>%
  mutate(organization_level = "OU Total") %>%
  select(organization_level, everything())

## RO Total --------------
ro_total <- ou_total %>%
  group_by(ro, year) %>%
  summarise(`EG.3-2` = sum_(`EG.3-2`)
            , `EG.3-2_OULevel` = sum_(`EG.3-2_OULevel`)
            , `EG.3-2_final` = sum_(`EG.3-2_final`)
            , .groups = "drop") %>%
  mutate(organization_level = "RO Total", .before = everything())

aggregated_out <- ungroup(bind_rows(
  ro_total, ou_total, im_total)) %>%
  relocate(organization_level, ro, ou, a_code, a_name, year, everything())

sheet_write(aggregated_out
            , "1jqdQPUUKO5YW_cmXrtjOeMKRUfGJ81Fldd94bupWmnE"
            , sheet = "RO, OU, IM Totals -- in DIS now")






















# ## something with PSI
# eg_3_2_27 <- filter(extract, ic == "EG.3.2-27") %>% as_tibble()
#
# eg_3_2_27 %>%
#   select(ro, ou, a_name, a_code, ic, udn
#          , year, d_name, target, actual) %>%
#   filter(ic == "EG.3.2-27" & udn %in% financing_udns) %>%
#   mutate(d_name = case_when(
#     udn %in% c("3.5.1.2.2", "3.6.1.2.2", "3.7.1.2.2") ~ "Female Value"
#     , udn %in% c("3.5.2.2.2", "3.6.2.2.2", "3.7.2.2.2") ~ "Female Number"
#     , udn %in% c("3.5.1.2.1", "3.6.1.2.1 ", "3.7.1.2.1") ~ "Male Value"
#     , udn %in% c("3.5.2.2.1", "3.6.2.2.1", "3.7.2.2.1") ~ "Male Number")) %>%
#   pivot_longer(c(target, actual))  %>%
#   select(-c(udn,ic)) %>%
#   pivot_wider(names_from = c(d_name)
#               , values_from = value
#               , values_fn = sum_) %>%
#   mutate(`Male Value` = if_else(is.na(`Male Number`), NA, `Male Value`)
#          , `Female Value` = if_else(is.na(`Female Number`), NA, `Female Value`)
#          , `Male Number` = if_else(is.na(`Male Value`), NA, `Male Number`)
#          , `Female Number` = if_else(is.na(`Female Value`), NA, `Female Number`))  %>%
#   mutate(`Female Per Person` = `Female Value` / `Female Number`
#          , `Male Per Person` = `Male Value` / `Male Number`
#          , value = (`Female Per Person` / `Male Per Person`)
#          , .by = c(a_code, year, name)) %>%
#   ungroup()
