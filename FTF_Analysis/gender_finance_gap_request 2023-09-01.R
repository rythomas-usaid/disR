
library(disR)
library(tidyverse)
indicators_df

ind_code <- c("EG.3.2-25/EG.3.2-x18")
ind_key <- "Sex"



ind_code = "EG.3.2-27"


udns <- c("3.5.1.2.1", "3.5.1.2.2"    # Type of financing accessed: Cash Debt
            , "3.6.1.2.1", "3.6.1.2.2"  # Type of financing accessed: In-Kind Debt
            , "3.7.1.2.1", "3.7.1.2.2"  # Type of financing accessed: Non-Debt
          #
            , "3.5.2.2.1", "3.5.2.2.2"  # Type of financing accessed: Cash Debt
            , "3.6.2.2.1", "3.6.2.2.2"  # Type of financing accessed: In-Kind Debt
            , "3.7.2.2.1", "3.7.2.2.2") # Type of financing accessed: Non-Debt
load("../database/extract/")

#### Replicating from Extract Database
indicators %>%
  #Select all the indicators you want
  filter(str_detect(ic, ind_code) &  udn %in% udns) %>%
  mutate(unit = if_else(substr(udn, 5, 5) == "2", "Number"
                , if_else(substr(udn, 5, 5) == "1", "Value","PROCESS"))) %>% #distinct(udn)

  # join values
  left_join(x = ., y = values
            , by = join_by(id == id_ind)
            , suffix = c("_ind", "_val") # label variables
            , keep = FALSE)  %>%
    mutate(across(c(actual, target), ~ as.numeric(.))) %>%
    # join im details
    left_join(activities, by = join_by(id_ac == id)) %>%
  filter(ro == "USDA" & year == 2022 & actual  > 0)

  filter(str_detect(id_ac , "2339")) %>%
    # filter only 2022 values
    filter(year == "2022") %>% #
    select(id_ac, ro, ou, unit, Disaggregate.Name, year, actual) %>%
  pivot_wider(names_from = "unit", values_from = "actual"
              , values_fn = ~if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
  mutate(per_capita = Value/Number)  %>% #print(n=500)

    pivot_wider(id_cols = c("id_ac","ro", "ou")
                , names_from = c("Disaggregate.Name", "year")
                , values_from = c("per_capita")
                , values_fn = ~if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
  mutate(female_proportion =Female_2022/Male_2022 )

    # group and summarize by OU, disaggregate, and year
    group_by(ou) %>%
    summarise(across(female_proportion, ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T))) %>% print(n=34)



gender_finance_ratio_by_ou %>% summarize(mean(female_proportion, na.rm=T))


# write.xlsx(gender_finance_ratio_by_ou
#           , "../output/gender_finance_ratio_by_ou.xlsx"
#           , overwrite = T)


# NOT FILTERING MATCHING VALUES -->
# By activity
indicators_df %>%
  filter(ic == "EG.3.2-27" & str_detect(d3 , "Sex") & d4 %in% c("Male", "Female") & type == "Actual" & year == "FY2022") %>%
  mutate(unit = if_else(str_detect(d2, "Value"), "Value", if_else(str_detect(d2, "Number"), "Number", "PROCESS"))) %>%
  select(ro, ou, code, ac_name, unit, year, sex=d4, value ) %>%
  pivot_wider(id_cols = c(ro, ou, code, ac_name), names_from = c(unit, sex), values_from = "value",
              values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
  mutate(per_capita_male = Value_Male/Number_Male
         , per_capita_female = Value_Female/Number_Female
         , female_proportion = per_capita_female/per_capita_male)


# By ou (2339 removed)
indicators_df %>%
  filter(ic == "EG.3.2-27" & str_detect(d3 , "Sex") & d4 %in% c("Male", "Female") & type == "Actual" & year == "FY2022") %>%
  mutate(unit = if_else(str_detect(d2, "Value"), "Value", if_else(str_detect(d2, "Number"), "Number", "PROCESS"))) %>%
  select(ro, ou, code, ac_name, unit, year, sex=d4, value ) %>%
  pivot_wider(id_cols = c(ro, ou, code, ac_name), names_from = c(unit, sex), values_from = "value",
              values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
  filter(code != "002339") %>% # remove the large Nigeria activity
  group_by(ou) %>%
  summarize(across(c(Value_Male, Value_Female, Number_Male, Number_Female)
                   , ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T))) %>%
  mutate(per_capita_male = Value_Male/Number_Male
         , per_capita_female = Value_Female/Number_Female
         , female_proportion = per_capita_female/per_capita_male)

# By initiative (2339 removed)
indicators_df %>%
  filter(ic == "EG.3.2-27" & str_detect(d3 , "Sex") & d4 %in% c("Male", "Female") & type == "Actual" & year == "FY2022") %>%
  mutate(unit = if_else(str_detect(d2, "Value"), "Value", if_else(str_detect(d2, "Number"), "Number", "PROCESS"))) %>%
  select(ro, ou, code, ac_name, unit, year, sex=d4, value ) %>%
  pivot_wider(id_cols = c(ro, ou, code, ac_name), names_from = c(unit, sex), values_from = "value",
              values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
  filter(code != "002339") %>% # remove the large Nigeria activity
  #group_by(ou) %>%
  summarize(across(c(Value_Male, Value_Female, Number_Male, Number_Female)
                   , ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T))) %>%
  mutate(per_capita_male = Value_Male/Number_Male
         , per_capita_female = Value_Female/Number_Female
         , female_proportion = per_capita_female/per_capita_male)

# FILTERING MATCHING VALUES
# By initiative (2339 removed)
# Summarizing if either male or female are both not NA
indicators_df %>%
  filter(ic == "EG.3.2-27" & str_detect(d3 , "Sex") & d4 %in% c("Male", "Female") & type == "Actual" & year == "FY2022") %>%
  mutate(unit = if_else(str_detect(d2, "Value"), "Value", if_else(str_detect(d2, "Number"), "Number", "PROCESS"))) %>%
  select(ro, ou, code, ac_name, unit, year, sex=d4, value ) %>%
  pivot_wider(id_cols = c(ro, ou, code, ac_name), names_from = c(unit, sex), values_from = "value",
              values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
  filter(
    code != "002339" & # remove the large Nigeria activity
      (if_all(c(Value_Male, Number_Male), ~ !is.na(.)) | if_all(c( Value_Female, Number_Female), ~ !is.na(.)))
    # (if_all(c(Value_Male, Number_Male,Value_Female, Number_Female), ~ !is.na(.)) )
    ) %>%
  #group_by(ou) %>%
  summarize(across(c(Value_Male, Number_Male, Value_Female, Number_Female)
                   , ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T))) %>%
  mutate(per_capita_male = Value_Male/Number_Male
         , per_capita_female = Value_Female/Number_Female
         , female_proportion = per_capita_female/per_capita_male)


# By initiative (2339 removed)
# Summarizing if either male or female are both not NA

gender_finance_gap_by_ac <- indicators_df %>%
  filter(ic == "EG.3.2-27" & str_detect(d3 , "Sex") & d4 %in% c("Male", "Female") & type == "Actual" & year == "FY2022" ) %>%
  mutate(unit = if_else(str_detect(d2, "Value"), "Value", if_else(str_detect(d2, "Number"), "Number", "PROCESS"))) %>%
  select(ro, ou, code, ac_name, unit, year, sex=d4, value ) %>%
  pivot_wider(id_cols = c(ro, ou, code, ac_name, sex), names_from = c(unit), values_from = "value",
              values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
  filter(
    #code != "002339" & # remove the large Nigeria activity
      (if_all(c(Value, Number), ~ . > 0) )
     #(if_all(c(Value_Male, Number_Male,Value_Female, Number_Female), ~ . > 0) )
  ) %>%
  pivot_wider(id_cols = c(ro, ou, code, ac_name), names_from = c(sex), values_from = c(Value, Number),
              values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
  group_by(ro, ou, code, ac_name) %>%
  summarize(across(c(Value_Male, Number_Male)
                   , ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)),
            across(c(Value_Female, Number_Female)
                   , ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T))) %>%
  mutate(per_capita_male = Value_Male/Number_Male
         , per_capita_female = Value_Female/Number_Female
         , female_proportion = per_capita_female/per_capita_male)



gender_finance_gap_by_ou <-
  indicators_df %>%
  filter(ic == "EG.3.2-27" & str_detect(d3 , "Sex") & d4 %in% c("Male", "Female") & type == "Actual" & year == "FY2022" ) %>%
  mutate(unit = if_else(str_detect(d2, "Value"), "Value", if_else(str_detect(d2, "Number"), "Number", "PROCESS"))) %>%
  select(ro, ou, code, ac_name, unit, year, sex=d4, value ) %>%
  pivot_wider(id_cols = c(ro, ou, code, ac_name, sex), names_from = c(unit), values_from = "value",
              values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
  filter(
    code != "002339" & # remove the large Nigeria activity
    (if_all(c(Value, Number), ~ . > 0) )
    #(if_all(c(Value_Male, Number_Male,Value_Female, Number_Female), ~ . > 0) )
  ) %>%
  pivot_wider(id_cols = c(ro, ou, code, ac_name), names_from = c(sex), values_from = c(Value, Number),
              values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
    mutate(ou = if_else(ou == "International Food Assistance Division (IFA)", "Cambodia", ou)) %>%

  group_by(ou) %>%
  summarize(across(c(Value_Male, Number_Male)
                   , ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)),
            across(c(Value_Female, Number_Female)
                   , ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T))) %>%
  mutate(per_capita_male = Value_Male/Number_Male
         , per_capita_female = Value_Female/Number_Female
         , female_proportion = per_capita_female/per_capita_male)

"../output/gender_finance_gap_by_ac.xlsx"
write.xlsx(gender_finance_gap_by_ac, file="../output/gender_finance_gap_by_ac.xlsx")
write.xlsx(gender_finance_gap_by_ou,file="../output/gender_finance_gap_by_ou.xlsx")

