# # This script is to write the DIS extracted data into a SQLite databse
#
# library(DBI)
# library(tidyverse)
# library(disR)
# # load("../downloads/basic.Rdata")
#
# require("RSQLite")
# # Set up database
# data <- read_dis_extract("../indicators/extract/DIS ENT - OU Activity Indicator Results (All Data)_20230622 Extract_Extract.csv") %>%
# #   filter(indicator_collection_frequency == "Annual")
# drv <- dbDriver("SQLite")
#
# tfile <- paste0("../data/", Sys.Date(), "/dis_extract.db")
# con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "dis_extract.db")
# dbWriteTable(con, "extract", as.data.frame(dis))
# dbWriteTable(con, "pt_udns", as.data.frame(pt_udns))
# dbDisconnect(con)


#
# data <- read_dis_extract("../indicators/extract/DIS ENT - OU Activity Indicator Results (All Data)_20240305.csv") %>%
#   filter(indicator_collection_frequency == "Annual")
#
# extct <- extract22 %>% as_tibble() %>% left_join(overlap)
#
# fy22data <- extct %>% filter(is.na(overlap))
# fy23data <- bind_rows(fy22data %>%
#             mutate(a_code = str_pad("0000", a_code, "left")
#                    , deviation_percentage = parse_double(deviation_percentage)),
#           data)
#
#
# data
# overlap


