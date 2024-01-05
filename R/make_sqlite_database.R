


# library(DBI)
# library(tidyverse)
# load("../downloads/basic.Rdata")
#
# if(!exists("../../database/ftf.db")){
#   ftfdb <- dbConnect(RSQLite::SQLite(), "../../database/ftf.db")
# }
#
# RSQLite::dbWriteTable(conn = ftfdb, name = "indicators", value = indicators, overwrite = T)
# RSQLite::dbExecute(conn = ftfdb, 'DROP TABLE ims_db ;')
#
# RSQLite::dbExecute(conn = ftfdb, 'CREATE TABLE ims (
#                    id INTEGER PRIMARY KEY AUTOINCREMENT,
#                    ro varchar(50),
#                    ou varchar(255),
#                    a_code varchar(25),
#                    a_name varchar(255)
#                    )')
# RSQLite::dbWriteTable(conn = ftfdb, name = "ims", value = ims, append = T)
# RSQLite::dbWriteTable(conn = ftfdb, name = "values", value = values, overwrite = T)
# RSQLite::dbSendStatement(ftfdb, 'PRAGMA foreign_keys = ON;')
# RSQLite::dbSendStatement(ftfdb, 'UPDATE TABLE ims SET ')
#
#
# dbListTables(conn = ftfdb)
#
#
# if(!dbTableExists(conn = ftfdb, table_schema = "addhealth", table_name = "data_21600_0001")){
#   # unzip if necessary
#   if(!file.exists("../data/21600-0001-Data.dta")){
#     x <- unzip(zipfile = "../data/21600-0001-Data.zip", exdir = "../data")
#   }
#
#
#   dbDisconnect(ftfdb)
