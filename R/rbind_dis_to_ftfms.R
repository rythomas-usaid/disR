#' parse DIS extracts into usable data
#'
#' create data set from DIS exports and FTFMS data
#' @param ftfms_input_dir character. Directory path to ftfms Excel files
#' @param dis_input character. File path of DIS extract CSV.
#' @param output_dir character. Directory path to store outputs combined_extracts.csv and combined_extracts.Rdata
#' @return data.frame with combined ftfms and DIS data
#' @examples
#' #' combine_extracts()
#' @import tidyverse
#'
#' @export rbind_dis_to_ftfms
rbind_dis_to_ftfms <- function(dis_input, ftfms_input_dir = "../../indicators/extract/", output_dir = "../data/") {
  dis <- read_dis(input_dir = dis_input)
  ms <- read_ftfms(input_dir = ftfms_input_dir)

  df <- dplyr::bind_rows(dis, ms) %>%
    dplyr::mutate(uic = make_uic(tolower(ic), tolower(d1), tolower(d2)))

  write.csv(df, paste0(output_dir, "combined_extracts.csv"))
  save(df, file = "../data/combined_extracts.Rdata")
  # return(df)

}

