
library(tidyverse)
#library(disR)
library(openxlsx)
# make_official_data(input_dir = input_dir)

# change this to be the location of the files
input_dir <- "../../../indicators/basic/"

update_official_data <- function(input_dir, pattern = "_20230622.xlsx", years = 2011:2025, output_dir){

  indicator_files <- list.files(input_dir, pattern = pattern, full.names = T)

  sheet_names <- c("RO Total", "RO Full Disaggs", "OU Total", "OU Full Disaggs"
                   ,"IM Total", "IM Full Disaggs")

  sheets <- expand.grid(path = indicator_files
                        , name = sheet_names
                        , stringsAsFactors = F)

  read_multisheet <- function(x, y) {
    if(y %in% getSheetNames(x)) {

      read.xlsx(x, sheet = y, fillMergedCells=T) %>%
        mutate(sheet = y, .before=everything())
    }
  }

  dat <- purrr::map2(

    sheets$path, sheets$name, ~ read_multisheet(.x, .y), .progress = T

  ) %>%

    purrr::list_rbind() %>%

    select(sheet, RO, Indicator.Code, OU, Pa.Id, Activity.Name
           , `Indicator.Disaggregates:.1st.Order`
           , `Indicator.Disaggregates:.2nd.Order`
           , `Indicator.Disaggregates:.3rd.Order`
           , `Indicator.Disaggregates:.4th.Order`
           , ends_with(as.character(years))) %>%

    pivot_longer(cols = starts_with("Target") | starts_with("Actual")
                 , names_to = "type"
                 , values_to = "value"
                 , values_drop_na = TRUE) %>%

    separate(type, into = c("type", "year")) %>%

    rename(Organization.Level = sheet) %>%

    filter(if_all(c(RO, OU, Indicator.Code, Pa.Id, Activity.Name)
                  , ~ ! . %in% c("Total", "Grand Total")))

  saveRDS(dat, paste0(output_dir, "tidy_version.Rdata"))

}
