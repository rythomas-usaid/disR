#' Load DIS and FTFMS data in one of two formats \"magnus\" or \"extract\".
#'
#' This function downloads and loads the DIS data in one of two formats.
#'
#' @export
fetch_data <- function(db , path=NULL) {

  if (db == "magnus") {

      url <- "https://drive.google.com/file/d/1QV5bkqlscKDZIecVPMtr_gylL2FmujKc/view?usp=drive_link"

      } else if(db == "extract") {

        url <- "https://drive.google.com/file/d/1MAs5i-j7sMVFg_asZpb194_drDYdaxXS/view?usp=drive_link"

          }

  #path <- paste0("../../downloads/", db, ".Rdata")

  # print(paste0("Working in ", path))

  googledrive::drive_download(url, path = path, overwrite = T)

  load(path)

}
