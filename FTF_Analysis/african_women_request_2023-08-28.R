library(tidydisR)
library(tidyverse)

load("../database/indicators_db.Rdata")

ind_code <- "EG.3.2-27/EG.3.2-x6"
ind_key <- "Sex"

indicators_df <- indicators_df %>%
  left_join(countries)


indicators
sum_indicators <-

bind_rows({
  indicators_df %>%
    #Select all the indicators you want
    filter(str_detect(ic_1819, "EG.3.2-27/EG.3.2-x6") &
              type == "Actual" &
             ro == "USAID"  &
              if_any(everything(), ~str_detect(., ind_key)) &
              if_any(everything(), ~ str_detect(., "Value"))) %>%

    filter(year == "FY2022" & Region == "Africa (sub-Saharan)" & d4 == "Female") %>%

    group_by(Sex=d4,Region, year) %>%
    summarise(value = sum(value, na.rm=T), .groups="drop") %>%
    pivot_wider(names_from = c("year")
                , values_from = "value"
                , values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
    mutate(Label = "Value of financing accessed",
           .before=everything())
}
,{
    indicators_df %>%
      #Select all the indicators you want
      filter(str_detect(ic_1819, "EG.3.2-27/EG.3.2-x6")  &
               ro == "USAID"  &
               type == "Actual" &
               if_any(everything(), ~str_detect(., ind_key)) &
               if_any(everything(), ~ str_detect(., "Number"))) %>%

      filter(year == "FY2022" & Region == "Africa (sub-Saharan)" & d4 == "Female") %>%

      group_by(Sex=d4,Region, year) %>%
      summarise(value = sum(value, na.rm=T), .groups="drop") %>%
      pivot_wider(names_from = c("year")
                  , values_from = "value"
                  , values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
      mutate(Label = "Number of female beneficiaries",
             .before=everything())
    }
,  {
  indicators_df %>%  #Select all the indicators you want
    filter(type == "Actual" & ro == "USAID" & str_detect(ic_1819, "EG.3.2-24/EG.3.2-x17") &
             if_any(everything(), ~str_detect(., ind_key)) ) %>%
    # sum_indicators(ind_code = ind_code, total = T,
    #                disaggregate = c("Crop Land|Cultivated Pasture|Aquaculture"))
    filter(year == "FY2022" & Region == "Africa (sub-Saharan)" & d3 == "Female") %>%

    filter(ou %in% c("Niger", "Zambia")) %>%
    group_by(ou) %>% print(n=50)

    group_by(Sex = d3, Region, year) %>%
    summarise(value = sum(value, na.rm=T)) %>%
    pivot_wider(names_from = c("year")
                , values_from = "value"
                , values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
    mutate(Label = "Number applied improved management practices or technologies", .before=everything())
  })


