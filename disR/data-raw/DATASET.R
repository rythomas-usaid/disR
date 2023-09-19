## code to prepare `DATASET` dataset goes here

#' export

library(tidyverse)
library(disR)

path <- "C:/Users/rythomas/Desktop/R scripts/indicators/"
indicator_files <- list.files(pattern = "xlsx")

print(paste("Reading xlsx files from", path))

indicator_files <- list.files(path, pattern = "xlsx")


indicators_df <- map(str_c(path, indicator_files), ~ disR::read_indicator(.x)) %>%
  list_rbind() %>%
  relocate(c(d3, d4), .after=d2) %>%
  dplyr::filter(!is.na(value)) %>%
  mutate(year = paste0("FY", year)) %>%
  #indicators_df <-   indicators_df %>%
  mutate(ic_1819 =
           if_else(ic %in% c("EG.3.2-25","EG.3.2-x18")
                   , "EG.3.2-25/EG.3.2-x18"
                   , if_else(ic %in% c("EG.3.2-24", "EG.3.2-x17")
                             , "EG.3.2-24/EG.3.2-x17"
                             , if_else(ic %in% c("EG.3.2-26", "EG.3.2-x19")
                                       , "EG.3.2-26/EG.3.2-x19"
                                       , if_else(ic %in% c("EG.3.2-27","EG.3.2-x6")
                                                 , "EG.3.2-27/EG.3.2-x6"
                                                 , if_else(ic %in% c("EG.3.1-14/-15","EG.3.2-x22")
                                                           , "EG.3.1-14/-15/EG.3.2-x22"
                                                           , ic))))),
         .after = ic)


# Create disaggregates lookup
indicators <- indicators_df %>%
  distinct(ic, ic_1819, d1, d2, d3, d4) %>%
  mutate(id = rownames(.), .before = everything()) %>%
  #encode combination categories
  # https://docs.google.com/document/d/1qwfBlce3UUmMzKY7ABCMcb_Tx_Mb7d7IeKHUn-5zhlU/edit
  mutate(group = if_else(ic %in% c("EG.3.2-25", "EG.3.2-x18") &
                           d1 %in% c("Crop Land", "Cultivated Pasture",
                                     "Cultivated Land") &
                           d2 =="Sex"
                         , "Cultivated Land"

                         , if_else(ic %in% c("EG.3.2-25", "EG.3.2-x18") &
                                     d1 %in% c("Aquaculture") &
                                     d2 =="Sex"
                                   , "Aquaculture"

                                   , if_else(ic %in% c("EG.3.2-25", "EG.3.2-x18") &
                                               d1 %in% c("Other") &
                                               d2 =="Sex"
                                             , "Other"

                                             , if_else(ic %in% c("EG.3.2-25", "EG.3.2-x18") &
                                                         d1 %in% c("Conservation/Protected Area"
                                                                   , "Freshwater or Marine Ecosystems"
                                                                   , "Rangeland") &
                                                         d2 =="Sex"
                                                       , "Extensively managed", NA)))))

# Create implementing mechanisms lookup
ims <- indicators_df %>%
  distinct(ro, ou, code, ac_name) %>%
  mutate(id = rownames(.), .before = everything())



# Create target values lookup
values <- indicators_df %>%
  # dplyr::(type == "Target") %>%
  left_join(indicators) %>%
  rename(id_ind = id) %>%
  left_join(ims) %>%
  select(id_im = id, id_ind, type, year, value) %>%
  relocate(c(id_im, id_ind), .before=everything()) %>%
  mutate(id = rownames(.), .before = everything())
# # TODO:
#  # Combine old and new indicators
#  # ---> THIS CAN BE RECYCLED TO COMBING ALL OLD AND NEW INDICATORS
#  mutate(indicator_name =
#           if_else(ic %in% c("EG.3.2-25","EG.3.2-x18")
#                   , "hectares",
#                   if_else(ic %in% c("EG.3.2-24", "EG.3.2-x17")
#                           , "producers", "PROCESS")),
#         .after = ic)
# EG.3.2-26 VALUE comes from sex disaggregates;
# smallholders =  EG.3.2-19 + EG.23.2-26 d1 == Producers & d2 == smallholders
# non-smallholders = EG.3.2-26 d1 == Producers & d2 == non-smallholders
# firms = EG.3.2-26 d1 == Firms & d2 %in% c("Microenterprise",
# "Small and Medium enterprise", "Large Enterprise and Corporations")
# TODO: ADD COMBINED INDICATORS HERE %>%
#  # Summarize old and new indicators
#  group_by(ro, ou, code, indicator_name, year) %>%
#  summarise(ic = paste(unique(ic), collapse = " /")
#            , value = sum(value, na.rm=T)) %>%
#  ungroup() %>%
#  # distinct(year, ic) %>% arrange(year)
#  # SANITY CHECK: there should only be combined values between 2018-2019
#  # <--- END HERE


ftf_target_countries <- data.frame(
  country = c("Bangladesh","Democratic Republic of the Congo",
              "Ethiopia","Ghana", "Guatemala","Honduras",
              "Kenya","Liberia", "Madagascar","Malawi", "Mali",
              "Mozambique", "Nepal","Niger", "Nigeria","Rwanda",
              "Senegal","Tanzania",  "Uganda","Zambia"),
  ftf_target = TRUE)

# load("../database/indicators_db.Rdata")
# data <- c(indicators_df, values, ims, indicators, ftf_target_countries)

usethis::use_data(indicators_df, overwrite = TRUE)
usethis::use_data(values, overwrite = TRUE)
usethis::use_data(indicators_df, overwrite = TRUE)
usethis::use_data(ims, overwrite = TRUE)
usethis::use_data(indicators, overwrite = TRUE)
usethis::use_data(ftf_target_countries, overwrite = TRUE)


