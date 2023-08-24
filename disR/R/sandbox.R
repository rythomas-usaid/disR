---
  title: "Progress Snapshot"
author: "Ryan Thomas"
date: "2023-08-23"
output:
  html_document:
  df_print: paged
---


options(scipen=999)
library(disR) # load data and custom functions
library(tidyverse)


# This works for:
indicators %>%
  sum_indicators(ind_code = "EG.3.2-2", total = T, disaggregate = "EG.3.2-2")


# variables
ind_code <- "EG.3.2-2"
ind_key = "Sex" # For totals
type = "Actual"

# logical filters
# EG.3.2-24
if_any(d1, ~str_detect(., "Producer"))
# EG.3.2-25 Hectares
if_any(d1, ~str_detect(.,
  c("Crop Land|Cultivated Pasture|Cultivated Land|Aquaculture"))) # "Intensively Managed"
if_any(d1, ~str_detect(.,
  c("Crop Land|Cultivated Pasture|Cultivated Land"))) # "Cropland + Cultivated Pasture"
if_any(d1, ~str_detect(.,
  c("Conservation/Protected Area|Freshwater or Marine Ecosystems|Rangeland"))) # "Extensively managed"
if_any(d1, ~str_detect(., c("Other"))) # Other
## Type of hectare by sex and climate disaggs
filter(str_detect(ic_1819, ind_code) &
           if_any(everything(), ~str_detect(., c("Sex|Climate Adaptation"))) &
           if_any(d1, ~str_detect(., c("Crop Land|Cultivated Pasture|Aquaculture|Conservation/Protected Area|Freshwater or Marine Ecosystems|Rangeland|Other")))) %>%
    group_by(`Indicator Code`=ic, `First disagg`=d1, `Second disagg`=d2, `Third disagg`=d3, year) %>%
 # Legacy version
  if_any(d1, ~str_detect(., c("Cultivated Land")))
  group_by(`Indicator Code`=ic, `First disagg`=d1, `Second disagg`=d2, `Third disagg`=d3, year)

# groupings
year # total, always group by year
sex, year # sex disaggregates (d2, year)
ic, year
ic, sex, year
group_by(Sex=d3, year) # EG.3.2-25

# start with indicators

# filter indicator codes

# Join values
left_join(x = ., y = filter(values, type=="Actual")
         , by = join_by(id == id_ind)
         , suffix = c("_ind", "_val") # label variables
         , keep = FALSE)
#--> Optionally join IM info

# Group

# summarize

# Sort by year

# pivot_wider
pivot_wider(names_from = c("year")
            , values_from = "value"
            , values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T))

# mutate Label name
mutate(Label = "", .before=everything())
# Label = "# of which are producers"
# Label = "# of which ..."
###########################################


# EG.3.2-26

ind_code <- "EG.3.2-26/EG.3.2-x19"
ind_key <- "Sex"
bind_rows({
  indicators %>%
    #Select all the indicators you want
    filter(str_detect(ic_1819, ind_code) &
             if_any(everything(), ~str_detect(., ind_key)) &
             if_any(d4, ~str_detect(., "Value of Sales"))) %>%
    # sum_indicators(ind_code = ind_code, total = T,
    #                disaggregate = c("Crop Land|Cultivated Pasture|Aquaculture"))
    left_join(x = ., y = filter(values, type=="Actual")
              , by = join_by(id == id_ind)
              , suffix = c("_ind", "_val") # label variables
              , keep = FALSE)  %>% #distinct(year)

    group_by(year) %>%
    summarise(value = sum(value, na.rm=T)) %>%
    pivot_wider(names_from = c("year")
                , values_from = "value"
                , values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
    relocate(all_of(paste0("FY", 2011:2022)), .after=everything()) %>%
    mutate(Label = "Value of annual sales of producers and firms receiving USG assistance",
           .before=everything()) %>%
    mutate(`LOI Total` = rowSums(select(., starts_with("FY")), na.rm=T))
}
, {
  indicators %>%
    #Select all the indicators you want
    filter(str_detect(ic_1819, ind_code) &
             if_any(everything(), ~str_detect(., ind_key)) &
             if_any(d4, ~str_detect(., "Value of Sales"))) %>%
    # sum_indicators(ind_code = ind_code, total = T,
    #                disaggregate = c("Crop Land|Cultivated Pasture|Aquaculture"))
    left_join(x = ., y = filter(values, type=="Actual")
              , by = join_by(id == id_ind)
              , suffix = c("_ind", "_val") # label variables
              , keep = FALSE)  %>% #distinct(year)

    group_by(ic, year) %>%
    summarise(value = sum(value, na.rm=T)) %>%
    pivot_wider(names_from = c("year")
                , values_from = "value"
                , values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
    relocate(all_of(paste0("FY", 2011:2022)), .after=everything()) %>%
    mutate(Label = "Value of annual sales of producers and firms receiving USG assistance",
           .before=everything())
}
, {
  indicators %>%
    #Select all the indicators you want
    filter(str_detect(ic_1819, ind_code) &
             if_any(everything(), ~str_detect(., ind_key)) &
             if_any(d4, ~str_detect(., "Value of Sales")) &
             if_any(d2,~str_detect(., "Producer - Smallholder"))) %>%
    # sum_indicators(ind_code = ind_code, total = T,
    #                disaggregate = c("Crop Land|Cultivated Pasture|Aquaculture"))
    left_join(x = ., y = filter(values, type=="Actual")
              , by = join_by(id == id_ind)
              , suffix = c("_ind", "_val") # label variables
              , keep = FALSE)  %>% #distinct(year)

    group_by(year) %>%
    summarise(value = sum(value, na.rm=T)) %>%
    pivot_wider(names_from = c("year")
                , values_from = "value"
                , values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
    relocate(all_of(paste0("FY", 2011:2022)), .after=everything()) %>%
    mutate(Label = "$ of which are from smallholder producers",
           .before=everything()) %>%
    mutate(`LOI Total` = rowSums(select(., starts_with("FY")), na.rm=T))
}
, {
  indicators %>%
    #Select all the indicators you want
    filter(str_detect(ic_1819, ind_code) &
             if_any(everything(), ~str_detect(., ind_key)) &
             if_any(d4, ~str_detect(., "Value of Sales")) &
             if_any(d2,~str_detect(., "Producer - Non-Smallholder"))) %>%
    # sum_indicators(ind_code = ind_code, total = T,
    #                disaggregate = c("Crop Land|Cultivated Pasture|Aquaculture"))
    left_join(x = ., y = filter(values, type=="Actual")
              , by = join_by(id == id_ind)
              , suffix = c("_ind", "_val") # label variables
              , keep = FALSE)  %>% #distinct(year)

    group_by(year) %>%
    summarise(value = sum(value, na.rm=T)) %>%
    pivot_wider(names_from = c("year")
                , values_from = "value"
                , values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
    relocate(all_of(paste0("FY", 2018:2022)), .after=everything()) %>%
    mutate(Label = "$ of which are from non-smallholder producers",
           .before=everything()) %>%
    mutate(`LOI Total` = rowSums(select(., starts_with("FY")), na.rm=T))
}
,{
  indicators %>%
    #Select all the indicators you want
    filter(str_detect(ic_1819, ind_code) &
             if_any(everything(), ~str_detect(., ind_key)) &
             if_any(d4, ~str_detect(., "Value of Sales")) &
             if_any(d2,~str_detect(., "Firm"))) %>%
    # sum_indicators(ind_code = ind_code, total = T,
    #                disaggregate = c("Crop Land|Cultivated Pasture|Aquaculture"))
    left_join(x = ., y = filter(values, type=="Actual")
              , by = join_by(id == id_ind)
              , suffix = c("_ind", "_val") # label variables
              , keep = FALSE)  %>% #distinct(year)

    group_by(year) %>%
    summarise(value = sum(value, na.rm=T)) %>%
    pivot_wider(names_from = c("year")
                , values_from = "value"
                , values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
    relocate(all_of(paste0("FY", 2018:2022)), .after=everything()) %>%
    mutate(Label = "$ of which are from firms",
           .before=everything()) %>%
    mutate(`LOI Total` = rowSums(select(., starts_with("FY")), na.rm=T))
}) %>%
  relocate(ic, .after=Label)





# EG.3.2-27
{r echo = F}
ind_code <- "EG.3.2-27/EG.3.2-x6"
ind_key <- "Sex"
bind_rows({
  indicators %>%
    #Select all the indicators you want
    filter((str_detect(ic_1819, ind_code) &
              if_any(everything(), ~str_detect(., ind_key)) &
              if_any(everything(), ~ str_detect(., "Value"))) |
             ic == "EG.3.2-x6" & str_detect(d1, "Sex")) %>%
    # sum_indicators(ind_code = ind_code, total = T,
    #                disaggregate = c("Crop Land|Cultivated Pasture|Aquaculture"))
    left_join(x = ., y = filter(values, type=="Actual")
              , by = join_by(id == id_ind)
              , suffix = c("_ind", "_val") # label variables
              , keep = FALSE)  %>% #distinct(year)

    group_by(year) %>%
    summarise(value = sum(value, na.rm=T)) %>%
    pivot_wider(names_from = c("year")
                , values_from = "value"
                , values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
    mutate(Label = "Value of financing accessed",
           .before=everything()) %>%
    mutate(`LOI Total` = rowSums(select(., starts_with("FY")), na.rm=T))
}
,{
  indicators %>%
    #Select all the indicators you want
    filter(
      (str_detect(ic_1819, ind_code) &
         if_any(everything(), ~str_detect(., ind_key)) &
         if_any(everything(), ~ str_detect(., "Value"))
      ) |
        ic == "EG.3.2-x6" & str_detect(d1, "Sex")
    ) %>%
    # sum_indicators(ind_code = ind_code, total = T,
    #                disaggregate = c("Crop Land|Cultivated Pasture|Aquaculture"))
    left_join(x = ., y = filter(values, type=="Actual")
              , by = join_by(id == id_ind)
              , suffix = c("_ind", "_val") # label variables
              , keep = FALSE)  %>% #distinct(year)

    group_by(ic, year) %>%
    summarise(value = sum(value, na.rm=T), .groups="drop") %>%
    pivot_wider(names_from = c("year")
                , values_from = "value"
                , values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
    mutate(Label = "From ... ",
           .before=everything()) %>%
    mutate(`LOI Total` = rowSums(select(., starts_with("FY")), na.rm=T))
})




{r echo = F}
bind_rows({
  indicators %>%
    #Select all the indicators you want
    filter(
      (str_detect(ic_1819, ind_code) &
         if_any(everything(), ~str_detect(., ind_key)) &
         if_any(everything(), ~ str_detect(., "Value")) &
         if_any(everything(), ~ str_detect(., "Cash Debt")  )
      ) |
        ic == "EG.3.2-x6" & str_detect(d1, "Sex")
    ) %>%
    # sum_indicators(ind_code = ind_code, total = T,
    #                disaggregate = c("Crop Land|Cultivated Pasture|Aquaculture"))
    left_join(x = ., y = filter(values, type=="Actual")
              , by = join_by(id == id_ind)
              , suffix = c("_ind", "_val") # label variables
              , keep = FALSE)  %>% #distinct(year)

    group_by(year) %>%
    summarise(value = sum(value, na.rm=T), .groups="drop") %>%
    pivot_wider(names_from = c("year")
                , values_from = "value"
                , values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
    mutate(Label = "Cash Debt",
           .before=everything()) %>%
    mutate(`LOI Total` = rowSums(select(., starts_with("FY")), na.rm=T))
}
, {
  indicators %>%
    #Select all the indicators you want
    filter((str_detect(ic_1819, ind_code) &
              if_any(everything(), ~str_detect(., ind_key)) &
              if_any(everything(), ~ str_detect(., "Value"))) |
             ic == "EG.3.2-x6" & str_detect(d1, "Sex")) %>%
    mutate(d4 = if_else(ic == "EG.3.2-x6", d2 , d4)) %>%
    # sum_indicators(ind_code = ind_code, total = T,
    #                disaggregate = c("Crop Land|Cultivated Pasture|Aquaculture"))
    left_join(x = ., y = filter(values, type=="Actual")
              , by = join_by(id == id_ind)
              , suffix = c("_ind", "_val") # label variables
              , keep = FALSE)  %>% #distinct(year)

    group_by(Sex=d4, year) %>%
    summarise(value = sum(value, na.rm=T), .groups="drop") %>%
    pivot_wider(names_from = c("year")
                , values_from = "value"
                , values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
    mutate(Label = "Value of financing accessed",
           .before=everything()) %>%
    mutate(`LOI Total` = rowSums(select(., starts_with("FY")), na.rm=T))
}
, {
  indicators %>%
    #Select all the indicators you want
    filter((str_detect(ic_1819, ind_code) &
              if_any(everything(), ~str_detect(., ind_key)) &
              if_any(everything(), ~ str_detect(., "Value")) &
              if_any(everything(), ~ str_detect(., "Cash Debt") )) |
             ic == "EG.3.2-x6" & str_detect(d1, "Sex")) %>%
    mutate(d4 = if_else(ic == "EG.3.2-x6", d2 , d4)) %>%
    # sum_indicators(ind_code = ind_code, total = T,
    #                disaggregate = c("Crop Land|Cultivated Pasture|Aquaculture"))
    left_join(x = ., y = filter(values, type=="Actual")
              , by = join_by(id == id_ind)
              , suffix = c("_ind", "_val") # label variables
              , keep = FALSE)  %>% #distinct(year)

    group_by(Sex=d4, year) %>%
    summarise(value = sum(value, na.rm=T), .groups="drop") %>%
    pivot_wider(names_from = c("year")
                , values_from = "value"
                , values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
    mutate(Label = "Of which Cash Debt",
           .before=everything()) %>%
    mutate(`LOI Total` = rowSums(select(., starts_with("FY")), na.rm=T))
}) %>%
  relocate(c("Sex"), .after=Label)



{r echo = F}
bind_rows({
  indicators %>%
    #Select all the indicators you want
    filter(str_detect(ic_1819, ind_code) &
             if_any(everything(), ~str_detect(., ind_key)) &
             if_any(everything(), ~ str_detect(., "Number"))) %>%
    mutate(d4 = if_else(ic == "EG.3.2-x6", d2 , d4)) %>%
    # sum_indicators(ind_code = ind_code, total = T,
    #                disaggregate = c("Crop Land|Cultivated Pasture|Aquaculture"))
    left_join(x = ., y = filter(values, type=="Actual")
              , by = join_by(id == id_ind)
              , suffix = c("_ind", "_val") # label variables
              , keep = FALSE)  %>% #distinct(year)

    group_by(d4, year) %>%
    summarise(value = sum(value, na.rm=T), .groups="drop") %>%
    pivot_wider(names_from = c("year")
                , values_from = "value"
                , values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
    mutate(Label = "Number of financing recipients",
           .before=everything())

}
, {
  indicators %>%
    #Select all the indicators you want
    filter(str_detect(ic_1819, ind_code) &
             if_any(everything(), ~str_detect(., ind_key)) &
             if_any(everything(), ~ str_detect(., "Number")) &
             if_any(everything(), ~ str_detect(., "Cash Debt") )) %>%
    mutate(d4 = if_else(ic == "EG.3.2-x6", d2 , d4)) %>%
    # sum_indicators(ind_code = ind_code, total = T,
    #                disaggregate = c("Crop Land|Cultivated Pasture|Aquaculture"))
    left_join(x = ., y = filter(values, type=="Actual")
              , by = join_by(id == id_ind)
              , suffix = c("_ind", "_val") # label variables
              , keep = FALSE)  %>% #distinct(year)

    group_by(Sex=d4, year) %>%
    summarise(value = sum(value, na.rm=T), .groups="drop") %>%
    pivot_wider(names_from = c("year")
                , values_from = "value"
                , values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
    mutate(Label = "Of which Cash Debt",
           .before=everything())
})



# EG.3.1-14/15
{r echo = F}
ind_code <- "EG.3.1-14/-15"
ind_key <- "Sex"
bind_rows({
  indicators %>%
    #Select all the indicators you want
    filter(str_detect(ic_1819, ind_code) &
             if_any(everything(), ~ str_detect(., "Private Sector"))) %>%
    # sum_indicators(ind_code = ind_code, total = T,
    #                disaggregate = c("Crop Land|Cultivated Pasture|Aquaculture"))
    left_join(x = ., y = filter(values, type=="Actual")
              , by = join_by(id == id_ind)
              , suffix = c("_ind", "_val") # label variables
              , keep = FALSE)  %>% #distinct(year)

    group_by(ic, year) %>%
    summarise(value = sum(value, na.rm=T), .groups= "drop") %>%
    pivot_wider(names_from = c("year")
                , values_from = "value"
                , values_fn = ~ if(all(is.na(.))) NA_real_ else sum(., na.rm=T)) %>%
    mutate(Label = "Value of new private sector investment leveraged",
           .before=everything())
})


