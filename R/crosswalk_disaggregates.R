#' make 1st order disaggregates
#'
#' create a single database from all the final indicator spreadsheets.
#' @return a vector (column) of strings with disaggregates
#' @examples
#' dcw <- make_dis_crosswalk()
#' @import tidyverse



# FTFMS ####
#' Standardize FTFMS disaggregates
#'
#' These four functions are used in a mutate statement in `rbind_dis_to_ftfms.R`. They use case_when to
#' create standardized disaggregates for d1, d2, d3, d4 to match the
#' DIS disaggregates.
#' @param ic a vector (column) of indicator codes
#' @param ms_d1 a vector (column) of DisaggregationName1 from the FTFMS database
#' @param ms_d2 a vector (column) of DisaggregationName2 from the FTFMS database
#' @param ms_d3 a vector (column) of DisaggregationName3 from the FTFMS database
#' @param ms_d4 a vector (column) of DisaggregationName4 from the FTFMS database
#'
#' @return a vector (column) of strings with disaggregates
#' @examples
#' if(exists(ms)) ms %>% mutate(d4 = fourth_order())

## Fourth order####
#' @export fourth_order
fourth_order <- function(ic, ms_d1, ms_d2, ms_d3, ms_d4){
  ##FTFMS##
  dplyr::case_when(ic == "EG.3.2-26" & ms_d4 == "Females - Value of sales (in $USD)"          ~ "Female - Value of Sales"
                   , ic == "EG.3.2-26" & ms_d4 == "Females - Number of participant producers" ~ "Female - Number of Participants"
                   , ic == "EG.3.2-26" & ms_d4 == "Females - Number of participant firms"     ~ "Female - Number of Participants"
                   , ic == "EG.3.2-26" & ms_d4 == "Females - Volume of sales (in MT)"         ~ "Female - Volume of Sales"
                   , ic == "EG.3.2-26" & ms_d4 == "Males - Value of sales (in $USD)"          ~ "Male - Value of Sales"
                   , ic == "EG.3.2-26" & ms_d4 == "Males - Number of participant producers"   ~ "Male - Number of Participants"
                   , ic == "EG.3.2-26" & ms_d4 == "Males - Number of participant firms"       ~ "Male - Number of Participants"
                   , ic == "EG.3.2-26" & ms_d4 == "Males - Volume of sales (in MT)"           ~ "Male - Volume of Sales"
                   , ic == "EG.3.2-26" & ms_d4 == "Mixed Sexes - Value of sales (in $USD)"    ~ "Mixed Sexes - Value of Sales"
                   , ic == "EG.3.2-26" & ms_d4 == "Mixed Sexes - Number of participant producers" ~  "Mixed Sexes - Number of Participants"
                   , ic == "EG.3.2-26" & ms_d4 == "Mixed Sexes - Number of participant firms" ~ "Mixed Sexes - Number of Participants"
                   , ic == "EG.3.2-26" & ms_d4 == "Mixed Sexes - Volume of sales (in MT)"     ~ "Mixed Sexes - Volume of Sales"
                   , ic == "EG.3.2-26" & ms_d4 == "Disaggregates Not Available (for sex of producer) - Value of sales (in $USD)" ~ "Disaggregates Not Available - Value of Sales"
                   , ic == "EG.3.2-26" & ms_d4 == "Disaggregates Not Available (for sex of producer) - Number of participant producers" ~ "Disaggregates Not Available - Number of Participants"
                   , ic == "EG.3.2-26" & ms_d4 == "Disaggregates Not Available (for sex of producer) - Number of participant firms" ~ "Disaggregates Not Available - Number of Participants"
                   , ic == "EG.3.2-26" & ms_d4 == "Disaggregates Not Available (for sex of producer) - Volume of sales (in MT)" ~ "Disaggregates Not Available - Volume of Sales"
                   , ic == "EG.3.2-26" & ms_d4 == "Disaggregates Not Available (for sex of proprietor) - Value of sales (in $USD)" ~ "Disaggregates Not Available - Value of Sales"
                   , ic == "EG.3.2-26" & ms_d4 == "Disaggregates Not Available (for sex of proprietor) - Number of participant producers" ~ "Disaggregates Not Available - Number of Participants"
                   , ic == "EG.3.2-26" & ms_d4 == "Disaggregates Not Available (for sex of proprietor) - Number of participant firms" ~ "Disaggregates Not Available - Number of Participants"
                   , ic == "EG.3.2-26" & ms_d4 == "Disaggregates Not Available (for sex of proprietor) - Volume of sales (in MT)" ~ "Disaggregates Not Available - Volume of Sales"
                   , ic == "EG.3.2-26" & ms_d4 == "Age 15-29 years - Value of sales (in $USD)" ~ "15-29 Years - Value of Sales"
                   , ic == "EG.3.2-26" & ms_d4 == "Age 15-29 years - Number of participant producers" ~"15-29 Years - Number of Participants"
                   , ic == "EG.3.2-26" & ms_d4 == "Age 15-29 years - Number of participant firms" ~ "15-29 Years - Number of Participants"
                   , ic == "EG.3.2-26" & ms_d4 == "Age 15-29 years - Volume of sales (in MT)"  ~ "15-29 Years - Volume of Sales"
                   , ic == "EG.3.2-26" & ms_d4 == "Age 30+ years - Value of sales (in $USD)"   ~ "30+ Years - Value of Sales"
                   , ic == "EG.3.2-26" & ms_d4 == "Age 30+ years - Number of participant producers" ~ "30+ Years - Number of Participants"
                   , ic == "EG.3.2-26" & ms_d4 == "Age 30+ years - Number of participant firms" ~ "30+ Years - Number of Participants"
                   , ic == "EG.3.2-26" & ms_d4 == "Age 30+ years - Volume of sales (in MT)"    ~ "30+ Years - Volume of Sales"
                   , ic == "EG.3.2-26" & ms_d4 == "Mixed Ages - Value of sales (in $USD)"      ~ "Mixed Ages - Value of Sales"
                   , ic == "EG.3.2-26" & ms_d4 == "Mixed Ages - Number of participant producers" ~ "Mixed Ages - Number of Participants"
                   , ic == "EG.3.2-26" & ms_d4 == "Mixed Ages - Number of participant firms"   ~ "Mixed Ages - Number of Participants"
                   , ic == "EG.3.2-26" & ms_d4 == "Mixed Ages - Volume of sales (in MT)"       ~ "Mixed Ages - Volume of Sales"
                   , ic == "EG.3.2-26" & ms_d4 == "Disaggregates Not Available (for age of producer) - Value of sales (in $USD)" ~ "Disaggregates Not Available - Value of Sales"
                   , ic == "EG.3.2-26" & ms_d4 == "Disaggregates Not Available (for age of producer) - Number of participant producers" ~ "Disaggregates Not Available - Number of Participants"
                   , ic == "EG.3.2-26" & ms_d4 == "Disaggregates Not Available (for age of producer) - Volume of sales (in MT)" ~ "Disaggregates Not Available - Volume of Sales"
                   , ic == "EG.3.2-26" & ms_d4 == "Disaggregates Not Available (for age of proprietor) - Value of sales (in $USD)" ~ "Disaggregates Not Available - Value of Sales"
                   , ic == "EG.3.2-26" & ms_d4 == "Disaggregates Not Available (for age of proprietor) - Number of participant producers" ~ "Disaggregates Not Available - Number of Participants"
                   , ic == "EG.3.2-26" & ms_d4 == "Disaggregates Not Available (for age of proprietor) - Number of participant firms" ~ "Disaggregates Not Available - Number of Participants"
                   , ic == "EG.3.2-26" & ms_d4 == "Disaggregates Not Available (for age of proprietor) - Volume of sales (in MT)" ~ "Disaggregates Not Available - Volume of Sales"

                   ## EG.3.2-27 ####
                   , ic == "EG.3.2-27" & ms_d4 == "Individuals / Microenterprises" ~ "Individuals / Microenterprises"
                   , ic == "EG.3.2-27" & ms_d4 == "Small and Medium Enterprises" ~ "Small and Medium Enterprises"
                   , ic == "EG.3.2-27" & ms_d4 == "Large Enterprises and Corporations" ~ "Large Enterprises and Corporations"
                   , ic == "EG.3.2-27" & ms_d4 == "Disaggregates Not Available" ~ "Disaggregates Not Available"
                   , ic == "EG.3.2-27" & ms_d4 == "Male" ~ "Male"
                   , ic == "EG.3.2-27" & ms_d4 == "Female" ~ "Female"
                   , ic == "EG.3.2-27" & ms_d4 == "Mixed (for enterprises)" ~ "Mixed (for enterprises)"
                   , ic == "EG.3.2-27" & ms_d4 == "Mixed (for enterprises)" ~ "Mixed Ages (for enterprises)"
                   , ic == "EG.3.2-27" & ms_d4 == "15-29" ~ "15-29"
                   , ic == "EG.3.2-27" & ms_d4 == "30+" ~ "30+"

                   ## EG.3.2-x19 ####
                   , ic == "EG.3.2-x19" & ms_d2 == "Number of direct beneficiaries" ~ "Number of Participants"
                   , ic == "EG.3.2-x19" & ms_d2 == "Reporting year sales" ~ "Value of Sales"
                   , ic == "EG.3.2-x19" & ! ms_d2 %in% c("Number of direct beneficiaries"
                                                         , "Reporting year sales") ~ ms_d2
                   , .default = ms_d4
  )
}

## Third order####
#' Standardize FTFMS disaggregates
#'
#' These four functions are used in a mutate statement in `rbind_dis_to_ftfms.R`. They use case_when to
#' create standardized disaggregates for d1, d2, d3, d4 to match the
#' DIS disaggregates.
#' @param ic a vector (column) of indicator codes
#' @param ms_d1 a vector (column) of DisaggregationName1 from the FTFMS database
#' @param ms_d2 a vector (column) of DisaggregationName2 from the FTFMS database
#' @param ms_d3 a vector (column) of DisaggregationName3 from the FTFMS database
#' @param ms_d4 a vector (column) of DisaggregationName4 from the FTFMS database
#'
#' @return a vector (column) of strings with disaggregates
#' @examples
#' if(exists(ms)) ms %>% mutate(d3 = third_order())
#' @export third_order
third_order <- function(ic, ms_d1, ms_d2, ms_d3, ms_d4){
  dplyr::case_when(ic == "EG.3.2-7" & ms_d4 == "Phase I: Under research as a result of USG assistance" ~ "Phase I: Under Research as a Result of USG Assistance"
                   , ic == "EG.3.2-7" & ms_d4 == "Phase II: Under field testing as a result of USG assistance" ~ "Phase II: Under Field Testing as a Result of USG Assistance"
                   , ic == "EG.3.2-7" & ms_d4 == "Phase III: Made available for uptake as a result of USG assistance" ~ "Phase III: Made Available for Uptake as a Result of USG Assistance"
                   , ic == "EG.3.2-7" & ms_d4 == "Phase IV: Demonstrated uptake by the public and/or private sector with USG assistance" ~ "Phase IV: Demonstrated Uptake by the Public and/or Private Sector with USG Assistance"

                   ##EG.3.2-24 #####
                   , ic == "EG.3.2-24" & ms_d2 %in% c("Sex (no double-counting)"
                                                      , "Age category (no double-counting)"
                                                      , "Management Practice or Tech Type (double-counting allowed)"
                                                      , "Technology type", "Commodity") ~ ms_d3
                   ##EG.3.2-25 ####
                   , ic == "EG.3.2-25" & ms_d3 == "Female" ~ "Female"
                   , ic == "EG.3.2-25" & ms_d3 == "Association-applied" ~ "Association-Applied"
                   , ic == "EG.3.2-25" & ms_d3 == "Disaggregates Not Available" ~ "Disaggregates Not Available"
                   , ic == "EG.3.2-25" & ms_d3 == "15-29" ~ "15-29"
                   , ic == "EG.3.2-25" & ms_d3 == "30+" ~ "30+"
                   , ic == "EG.3.2-25" & ms_d3 == "Association-applied" ~ "Association-Applied"
                   , ic == "EG.3.2-25" & ms_d3 == "Disaggregates Not Available" ~ "Disaggregates Not Available"
                   , ic == "EG.3.2-25" & ms_d3 == "Crop genetics" ~ "Crop Genetics"
                   , ic == "EG.3.2-25" & ms_d3 == "Cultural practices" ~ "Cultural Practices"
                   , ic == "EG.3.2-25" & ms_d3 == "Livestock management" ~ "Livestock Management"
                   , ic == "EG.3.2-25" & ms_d3 == "Wild-caught fisheries management" ~ "Wild-Caught Fisheries Management"
                   , ic == "EG.3.2-25" & ms_d3 == "Aquaculture management" ~ "Aquaculture Management"
                   , ic == "EG.3.2-25" & ms_d3 == "Natural resource or ecosystem management" ~"Natural Resource or Ecosystem Management"
                   , ic == "EG.3.2-25" & ms_d3 == "Pest and disease management" ~ "Pest and Disease Management"
                   , ic == "EG.3.2-25" & ms_d3 == "Soil-related fertility and conservation" ~ "Soil-Related Fertility and Conservation"
                   , ic == "EG.3.2-25" & ms_d3 == "Irrigation" ~ "Irrigation"
                   , ic == "EG.3.2-25" & ms_d3 == "Agriculture water management-non-irrigation based" ~ "Agriculture Water Management-Non-Irrigation Based"
                   , ic == "EG.3.2-25" & ms_d3 == "Climate mitigation" ~ "Climate Mitigation"
                   , ic == "EG.3.2-25" & ms_d3 == "Climate adaptation/climate risk management" ~ "Climate Adaptation/Climate Risk Management"
                   , ic == "EG.3.2-25" & ms_d3 == "Other" ~ "Other"
                   , ic == "EG.3.2-25" & ms_d2 == "Commodity" ~ ms_d3

                   ##EG.3.2-26 #####
                   , ic == "EG.3.2-26" & ms_d3 == "Sex of Producer" ~ "Sex"
                   , ic == "EG.3.2-26" & ms_d3 == "Sex of proprietor(s)" ~ "Sex"
                   , ic == "EG.3.2-26" & ms_d3 == "Age of Producer" ~ "Age"
                   , ic == "EG.3.2-26" & ms_d3 == "Age of proprietor(s)" ~ "Age"

                   ##EG.3.2-27 ####
                   , ic == "EG.3.2-27" & ms_d3 == "Individuals / Microenterprises" ~ "Individuals / Microenterprises"
                   , ic == "EG.3.2-27" & ms_d3 == "Small and Medium Enterprises" ~ "Small and Medium Enterprises"
                   , ic == "EG.3.2-27" & ms_d3 == "Large Enterprises and Corporations" ~ "Large Enterprises and Corporations"
                   , ic == "EG.3.2-27" & ms_d3 == "Disaggregates Not Available" ~ "Disaggregates Not Available"
                   , ic == "EG.3.2-27" & ms_d3 == "Male" ~ "Male"
                   , ic == "EG.3.2-27" & ms_d3 == "Female" ~ "Female"
                   , ic == "EG.3.2-27" & ms_d3 == "Mixed (for enterprises)" ~ "Mixed (for enterprises)"
                   , ic == "EG.3.2-27" & ms_d3 == "Disaggregates Not Available" ~ "Disaggregates Not Available"
                   , ic == "EG.3.2-27" & ms_d3 == "15-29" ~ "15-29"
                   , ic == "EG.3.2-27" & ms_d3 == "30+" ~ "30+"
                   , ic == "EG.3.2-27" & ms_d3 == "Mixed Ages (for enterprises)" ~ "Mixed Ages (for enterprises)"
                   , ic == "EG.3.2-27" & ms_d3 == "Disaggregates Not Available" ~ "Disaggregates Not Available"
                   , ic == "EG.3.2-27" & ms_d3 == "Size of recipient(s)" ~ "Size of recipient(s)"
                   , ic == "EG.3.2-27" & ms_d3 == "Sex of recipient(s)" ~ "Sex of recipient(s)"
                   , ic == "EG.3.2-27" & ms_d3 == "Age of recipient(s)" ~ "Age of recipient(s)"

                   ##EG.3.2-x17 ####
                   , ic == "EG.3.2-x17" & ms_d2 %in% c("Sex", "Commodity", "Duration") ~ ms_d3
                   # there could be a problem with capicalization if changes are made
                   # , ic == "EG.3.2-x17" & ms_d3 == "animal genetics" ~ "Animal Genetics"
                   # , ic == "EG.3.2-x17" & ms_d3 == "climate adaptation" ~ "Climate Adaptation"
                   # , ic == "EG.3.2-x17" & ms_d3 == "climate mitigation" ~ "Climate Mitigation"
                   # , ic == "EG.3.2-x17" & ms_d3 == "climate mitigation or adaptation" ~ "Climate Mitigation or Adaptation"
                   # , ic == "EG.3.2-x17" & ms_d3 == "crop genetics" ~ "Crop Genetics"
                   # , ic == "EG.3.2-x17" & ms_d3 == "cultural practices" ~ "Cultural Practices"
                   # , ic == "EG.3.2-x17" & ms_d3 == "disease management" ~ "Disease Management"
                   # , ic == "EG.3.2-x17" & ms_d3 == "wild fishing technique/gear" ~ "Wild Fishing Gear/Technique"
                   # , ic == "EG.3.2-x17" & ms_d3 == "irrigation" ~ "Irrigation"
                   # , ic == "EG.3.2-x17" & ms_d3 == "other" ~ "Other"
                   # , ic == "EG.3.2-x17" & ms_d3 == "pest management" ~ "Pest Management"
                   # , ic == "EG.3.2-x17" & ms_d3 == "post-harvest - handling and storage" ~ "Post-Harvest Handling and Storage"
                   # , ic == "EG.3.2-x17" & ms_d3 == "processing" ~ "Processing"
                   # , ic == "EG.3.2-x17" & ms_d3 == "soil-related fertility and conservation" ~ "Soil-Related Fertility and Conservation"
                   # , ic == "EG.3.2-x17" & ms_d3 == "total w/one or more improved technology" ~ "Total With One or More Improved Technology"
                   # , ic == "EG.3.2-x17" & ms_d3 == "water management (non-irrigation)" ~ "Water Management (Non-Irrigation)"
                   # , ic == "EG.3.2-x17" & ms_d3 == "Association-applied" ~ "Association-Applied"
                   # , ic == "EG.3.2-x17" & ms_d3 == "aquaculture management" ~ "Aquaculture Management"
                   # , ic == "EG.3.2-x17" & ms_d3 == "Livestock management" ~ "Livestock Management"
                   # , ic == "EG.3.2-x17" & ms_d3 == "marketing and distribution" ~ "Marketing and Distribution"
                   # , ic == "EG.3.2-x17" & ms_d3 == "value-added processing" ~ "Value-Added Processing"
                   # , ic == "EG.3.2-x17" & ms_d3 == "Disaggregates Not Available" ~ "Disaggregates Not Available"

                   ##EG.3.2-x18 (Hectares)####
                   , ic == "EG.3.2-x18" & ms_d2 == "animal genetics" ~ "Animal Genetics"
                   , ic == "EG.3.2-x18" & ms_d2 == "climate adaptation" ~ "Climate Adaptation"
                   , ic == "EG.3.2-x18" & ms_d2 == "climate mitigation" ~ "Climate Mitigation"
                   , ic == "EG.3.2-x18" & ms_d2 == "climate mitigation or adaptation" ~ "Climate Mitigation or Adaptation"
                   , ic == "EG.3.2-x18" & ms_d2 == "crop genetics" ~ "Crop Genetics"
                   , ic == "EG.3.2-x18" & ms_d2 == "cultural practices" ~ "Cultural Practices"
                   , ic == "EG.3.2-x18" & ms_d2 == "disease management" ~ "Disease Management"
                   , ic == "EG.3.2-x18" & ms_d2 == "fishing gear/technique" ~ "Fishing Gear/Technique"
                   , ic == "EG.3.2-x18" & ms_d2 == "irrigation" ~ "Irrigation"
                   , ic == "EG.3.2-x18" & ms_d2 == "other" ~ "Other"
                   , ic == "EG.3.2-x18" & ms_d2 == "pest management" ~ "Pest Management"
                   , ic == "EG.3.2-x18" & ms_d2 == "post-harvest handling and storage" ~ "Post-Harvest Handling and Storage"
                   , ic == "EG.3.2-x18" & ms_d2 == "processing" ~ "Processing"
                   , ic == "EG.3.2-x18" & ms_d2 == "soil-related fertility and conservation" ~ "Soil-Related Fertility and Conservation"
                   , ic == "EG.3.2-x18" & ms_d2 == "total w/one or more improved technology" ~ "Total With One or More Improved Technology"
                   , ic == "EG.3.2-x18" & ms_d2 == "water management (non-irrigation)" ~ "Water Management (Non-Irrigation)"
                   , ic == "EG.3.2-x18" & ms_d2 == "Association-applied" ~ "Association-Applied"
                   , ic == "EG.3.2-x18" &
                     ! ms_d2 %in% c("animal genetics", "climate adaptation", "climate mitigation"
                                    , "climate mitigation", "crop genetics", "cultural practices", "disease management"
                                    , "fishing gear/technique", "irrigation", "other", "pest management"
                                    , "post-harvest handling and storage", "processing", "soil-related fertility and conservation"
                                    , "total w/one or more improved technology", "water management (non-irrigation)",
                                    "Association-applied") ~ ms_d2

                   ##EG.3.2-x19 (Sales)####
                   , ic == "EG.10.4-7" & ms_d3 == "Female" ~ "Female"
                   , ic == "EG.10.4-7" & ms_d3 == "Male" ~ "Male"
                   , ic == "EG.10.4-7" & ms_d3 == "Disaggregates Not Available" ~ "Disaggregates Not Available"
                   , ic == "EG.10.4-7" & ms_d3 == "Individual/Household" ~ "Individual/Household"
                   , ic == "EG.10.4-7" & ms_d3 == "Community/Group" ~ "Community/Group"
                   , ic == "EG.10.4-7" & ms_d3 == "Business/Commercial" ~ "Business/Commercial"
                   , ic == "EG.10.4-7" & ms_d3 == "Other legal entity (e.g. churches, NGOs)" ~ "Other Legal Entity (e.g. Churches, NGOs)"
                   , ic == "EG.10.4-7" & ms_d3 == "Disaggregates Not Available" ~ "Disaggregates Not Available"
                   , ic == "EG.10.4-7" & ms_d3 == "Rural" ~ "Rural"
                   , ic == "EG.10.4-7" & ms_d3 == "Marine water" ~ "Marine Water"
                   , ic == "EG.10.4-7" & ms_d3 == "Urban" ~ "Urban"
                   , ic == "EG.10.4-7" & ms_d3 == "Freshwater" ~ "Freshwater"
                   , ic == "EG.10.4-7" & ms_d3 == "Disaggregates Not Available" ~ "Disaggregates Not Available"

                   ##EG.10.4-8 (Participants by Resource Type)##
                   , ic == "EG.10.4-8" & ms_d3 == "Male" ~ "Male"
                   , ic == "EG.10.4-8" & ms_d3 == "Female" ~ "Female"
                   , ic == "EG.10.4-8" & ms_d3 == "Disaggregates Not Available" ~ "Disaggregates Not Available"
                   , ic == "EG.10.4-8" & ms_d3 == "Customary" ~ "Customary"
                   , ic == "EG.10.4-8" & ms_d3 == "Freehold" ~ "Freehold"
                   , ic == "EG.10.4-8" & ms_d3 == "Leasehold" ~ "Leasehold"
                   , ic == "EG.10.4-8" & ms_d3 == "State" ~ "State"
                   , ic == "EG.10.4-8" & ms_d3 == "Community/Group Rights" ~ "Community/Group Rights"
                   , ic == "EG.10.4-8" & ms_d3 == "Cooperatives" ~ "Cooperatives"
                   , ic == "EG.10.4-8" & ms_d3 == "Other (Specify prior to data collection and report in an indicator comment)" ~ "Other"
                   , ic == "EG.10.4-8" & ms_d3 == "Disaggregates Not Available" ~ "Disaggregates Not Available"
                   , .default = ms_d3
  )
}

## Second order ####
#' Standardize FTFMS disaggregates
#'
#' These four functions are used in a mutate statement in `rbind_dis_to_ftfms.R`. They use case_when to
#' create standardized disaggregates for d1, d2, d3, d4 to match the
#' DIS disaggregates.
#' @param ic a vector (column) of indicator codes
#' @param ms_d1 a vector (column) of DisaggregationName1 from the FTFMS database
#' @param ms_d2 a vector (column) of DisaggregationName2 from the FTFMS database
#' @param ms_d3 a vector (column) of DisaggregationName3 from the FTFMS database
#' @param ms_d4 a vector (column) of DisaggregationName4 from the FTFMS database
#'
#' @return a vector (column) of strings with disaggregates
#' @examples
#' if(exists(ms)) ms %>% mutate(d2 = second_order())
second_order <-  function(ic, ms_d1, ms_d2, ms_d3, ms_d4){
  dplyr::case_when(
    ic == "CBLD-9" & ms_d3 == "Numerator = number of organizations of this type with improved performance" ~ "Number of Organizations with Improved Performance"
    , ic == "CBLD-9" & ms_d3 == "Denominator = number of USG-assisted organizations of this type receiving organizational capacity development support" ~ "Total Number of USG-Assisted Organizations Receiveing Organizational Capacity Development Support"
    , ic == "EG.3-x1" & ms_d2 == "Household type:  Adult Female no Adult Male (FNM)" ~ "Household Type: Adult Female No Adult Male (FNM)"
    , ic == "EG.3-x1" & ms_d2 == "Household type:  Adult Male no Adult Female (MNF)" ~ "Household Type:  Adult Male No Adult Female (MNF)"
    , ic == "EG.3-x1" & ms_d2 == "Household type:  Child No Adults (CNA)" ~ "Household Type:  Child No Adults (CNA)"
    , ic == "EG.3-x1" & ms_d2 == "Household type:  Disaggregates Not Available" ~ "Household Type:  Disaggregates Not Available"
    , ic == "EG.3-x1" & ms_d2 == "Household type:  Male and Female Adults (M&F)" ~ "Household Type:  Male and Female Adults (M&F)"
    ## Location ##
    , ic == "EG.3-x1" & ms_d2 == "Rural" ~ "Rural"
    , ic == "EG.3-x1" & ms_d2 == "Urban/Peri-urban" ~ "Urban/Peri-Urban"
    ##Duration##
    , ic == "EG.3-x1" & ms_d2 == "New" ~ "New"
    , ic == "EG.3-x1" & ms_d2 == "Continuing" ~ "Continuing"
    ##DNA##
    , ic == "EG.3-x1" & ms_d2 == "Disaggregates Not Available" ~ "Disaggregates Not Available"

    ##EG.3-2 (Participants)####
    , ic == "EG.3-2" & ms_d2 == "Female" ~ "Female"
    , ic == "EG.3-2" & ms_d2 == "Male" ~ "Male"
    ##Sex and Age##
    , ic == "EG.3-2" & ms_d2 == "Not Applicable (for household members counted from HH-level interventions)" ~ "Not Applicable"
    , ic == "EG.3-2" & ms_d2 == "Disaggregates Not Available" ~ "Disaggregates Not Available"
    ##Age##
    , ic == "EG.3-2" & ms_d2 == "School-aged children (only for total reached by USG school feeding programs, regardless of actual age)" ~ "School-Aged Children"
    , ic == "EG.3-2" & ms_d2 == "15-29" ~ "15-29"
    , ic == "EG.3-2" & ms_d2 == "30+" ~ "30+"
    ##Type of Individuals##
    , ic == "EG.3-2" & ms_d2 == "Parents/caregivers" ~ "Parents/Caregivers"
    , ic == "EG.3-2" & ms_d2 == "Household members (household-level interventions only)" ~ "Household Members"
    , ic == "EG.3-2" & ms_d2 == "School-aged children (only for USG school feeding programs)" ~ "School-Aged Children"
    , ic == "EG.3-2" & ms_d2 == "People in government" ~ "People in Government"
    , ic == "EG.3-2" & ms_d2 == "People in USG-assisted private sector firms" ~ "People in USG-Assisted Private Sector Firms"
    , ic == "EG.3-2" & ms_d2 == "People in civil society" ~ "People in Civil Society"
    , ic == "EG.3-2" & ms_d2 == "Laborers (Non-producer diversified livelihoods participants)" ~ "Laborers (Non-Producer Diversified Livelihoods Participants)"
    , ic == "EG.3-2" & ms_d2 == "Producer: Smallholder farmer" ~ "Producer: Smallholder Farmer"
    , ic == "EG.3-2" & ms_d2 == "Producer: Non-smallholder farmer" ~ "Producer: Non-Smallholder Farmer"
    , ic == "EG.3-2" & ms_d2 == "Producer: Aquaculture" ~ "Producer: Aquaculture"
    , ic == "EG.3-2" & ms_d2 == "Producer: size Disaggregate Not Available" ~ "Producer: Size Disaggregate Not Available"
    , ic == "EG.3-2" & ms_d2 == "Type of individual Not Applicable"~ "Type of Individual Not Applicable"
    , ic == "EG.3-2" & ms_d2 == "Type of Individual Disaggregates Not Available" ~ "Type of Individual Disaggregates Not Available"

    ##EG.3-x6, -7, -8 ####
    , ic == "EG.3-x6, -7, -8" & ms_d3 == "Association-applied" ~ "Association-Applied"
    , ic == "EG.3-x6, -7, -8" & ms_d3 == "Disaggregates Not Available" ~ "Disaggregates Not Available"
    , ic == "EG.3-x6, -7, -8" & ms_d3 == "Female" ~ "Female"
    , ic == "EG.3-x6, -7, -8" & ms_d3 == "Male" ~ "Male"
    , ic == "EG.3-x6, -7, -8" & ms_d3 == "Joint" ~ "Joint"

    ##EG.3-x9####
    ##Duration##
    , ic == "EG.3-x9" & ms_d2 == "New" ~ "New"
    , ic == "EG.3-x9" & ms_d2 == "Continuing" ~ "Continuing"
    ##Location##
    , ic == "EG.3-x9" & ms_d2 == "Rural" ~ "Rural"
    , ic == "EG.3-x9" & ms_d2 == "Urban/peri-urban" ~ "Urban/Peri-Urban"
    ##Sex of Job-Holder##
    , ic == "EG.3-x9" & ms_d2 == "Female" ~ "Female"
    , ic == "EG.3-x9" & ms_d2 == "Male" ~ "Male"
    , ic == "EG.3-x9" & ms_d2 == "Disaggregates Not Available" ~ "Disaggregates Not Available"

    ##EG.3-10-11-12 ####
    ##Participants##
    , ic == "EG.3.1-x12" & ms_d2 == "Agricultural input policy (e.g. seed, fertilizer)" ~ "Agricultural Input Policy (e.g. Seed, Fertilizer)"
    , ic == "EG.3.1-x12" & ms_d2 == "Agricultural trade policy" ~ "Agricultural Trade Policy"
    , ic == "EG.3.1-x12" & ms_d2 == "Enabling environment for private sector investment" ~ "Enabling Environment for Private Sector Investment"
    , ic == "EG.3.1-x12" & ms_d2 == "Institutional architecture for improved policy formulation" ~ "Institutional Architecture for Improved Policy Formulation"
    , ic == "EG.3.1-x12" & ms_d2 == "Land and natural resources tenure, rights, and policy" ~ "Land and Natural Resources Tenure, Rights, and Policy"
    , ic == "EG.3.1-x12" & ms_d2 == "Nutrition (e.g. fortification, food safety)" ~ "Nutrition (e.g. Fortification, Food Safety)"
    , ic == "EG.3.1-x12" & ms_d2 == "Other" ~ "Other"
    , ic == "EG.3.1-x12" & ms_d2 == "Resilience and agricultural risk management policy" ~ "Resilience and Agricultural Risk Management Policy"
    ##Process/Step##
    , ic == "EG.3.1-x12" & ms_d2 == "Analysis" ~ "Analysis"
    , ic == "EG.3.1-x12" & ms_d2 == "Approval (legislative or regulatory)" ~ "Approval (Legislative or Regulatory)"
    , ic == "EG.3.1-x12" & ms_d2 == "Drafting or revision" ~ "Drafting or Revision"
    , ic == "EG.3.1-x12" & ms_d2 == "Full and effective implementation" ~ "Full and Effective Implementation"
    , ic == "EG.3.1-x12" & ms_d2 == "Stakeholder consultation/public debate" ~ "Stakeholder Consultation/Public Debate"
    , ic == "EG.3.1-x12" & ms_d2 == "Total policies passing through one of more processes/steps of policy change" ~ "Total Policies Passing Through One of More Processes/Steps of Policy Change"
    , ic == "EG.3.1-x12" & ms_d2 == "Disaggregates Not Available" ~ "Disaggregates Not Available"

    ##EG.3.2-x1####
    , ic == "EG.3.2-x1" & ms_d1 == "Type of Individual" ~  ms_d3
    , ic == "EG.3.2-x1" & ms_d1 == "Sex" ~ ms_d2

    ##EG.3.2-2 (Participants by Sex)####
    ##Female##
    , ic == "EG.3.2-2" & ms_d2 == "Female" ~ "Female"
    ##Male##
    , ic == "EG.3.2-2" & ms_d2 == "Male" ~ "Male"
    ##DNA##
    , ic == "EG.3.2-2" & ms_d2 == "Disaggregates Not Available" ~ "Disaggregates Not Available"
    ##New##
    , ic == "EG.3.2-2" & ms_d2 == "New" ~ "New"
    ##Continuing##
    , ic == "EG.3.2-2" & ms_d2 == "Continuing" ~ "Continuing"
    ##DNA##
    , ic == "EG.3.2-2" & ms_d2 == "Disaggregates Not Available" ~ "Disaggregates Not Available"
    ##EG.3.2-x3##
    , ic == "EG.3.2-x3" & ms_d2 == "n/a" ~ "Not Applicable"
    , ic == "EG.3.2-x3" & ms_d2 == "Micro (1-10 employees)" ~ "Micro (1-10 Employees)"
    , ic == "EG.3.2-x3" & ms_d2 == "Small (11-50 employees)" ~ "Small (11-50 Employees)"
    , ic == "EG.3.2-x3" & ms_d2 == "Medium (51-100 employees)" ~ "Medium (51-100 Employees)"
    , ic == "EG.3.2-x3" &
      ! ms_d2 %in% c("n/a", "Micro (1-10 employees)", "Small (11-50 employees)"
                     , "Medium (51-100 employees)") ~ ms_d2

    ##EG.3.2-x4####
    , ic == "EG.3.2-x4" & ms_d2 == "Community-based organizations (CBOs)" ~ "Community-Based Organizations (CBOs)"
    , ic == "EG.3.2-x4" & ms_d2 == "For-profit private enterprises" ~ "For-Profit Private Enterprises"
    , ic == "EG.3.2-x4" & ms_d2 == "Producers organizations" ~ "Producers Organizations"
    , ic == "EG.3.2-x4" & ms_d2 == "Trade and business associations" ~ "Trade and Business Associations"
    , ic == "EG.3.2-x4" & ms_d2 == "Water users associations" ~ "Water Users Associations"
    , ic == "EG.3.2-x4" & ms_d2 == "Women's groups" ~ "Women's Groups"
    , ic == "EG.3.2-x4" ~ ms_d2

    ##EG.3.2-x6 (USD)##
    , ic == "EG.3.2-x6" & ms_d2 == "n/a" ~ "Not Applicable"
    , ic == "EG.3.2-x6" & ms_d2 == "Local traders/assemblers" ~ "Local Traders/Assemblers"
    , ic == "EG.3.2-x6" & ms_d2 == "Wholesalers/processors" ~ "Wholesalers/Processors"
    , ic == "EG.3.2-x6" ~ ms_d2

    ##EG.3.2-7 (Number of Technologies, Practices, and Approaches)####
    ##Phase of Research (Double-Counting Allowed)##
    , ic == "EG.3.2-7" & ms_d3 ==  "Phase of Research (double-counting allowed)" ~ "Phase of Research (Double-Counting Allowed)"
    ##Total Number of Unique Technologies/Practices (No Double-Counting)##
    , ic == "EG.3.2-7" & ms_d1 =="Total number of unique technologies / practices / approaches from all categories (no double-counting)" ~ "Total Number of Unique Technologies/Practices"

    ##EG.3.2-x14 (HHs)####
    , ic == "EG.3.2-x14" & ms_d2 == "Household type:  Adult Female no Adult Male (FNM)" ~ "Household Type:  Adult Female No Adult Male (FNM)"
    , ic == "EG.3.2-x14" & ms_d2 == "Household type:  Adult Male no Adult Female (MNF)" ~ "Household Type:  Adult Male No Adult Female (MNF)"
    , ic == "EG.3.2-x14" & ms_d2 == "Household type:  Child No Adults (CNA)" ~ "Household Type:  Child No Adults (CNA)"
    , ic == "EG.3.2-x14" & ms_d2 == "Household type:  Disaggregates Not Available" ~ "Household Type:  Disaggregates Not Available"
    , ic == "EG.3.2-x14" & ms_d2 == "Household type:  Male and Female Adults (M&F)" ~ "Household Type:  Male and Female Adults (M&F)"
    , ic == "EG.3.2-x14" ~ ms_d2

    ##EG.3.2-24 (Value Chain Actor Type Sex)####
    ##Sex##
    , ic == "EG.3.2-24" & ms_d2 == "Sex (no double-counting)" ~ "Sex"
    ##Age##
    , ic == "EG.3.2-24" & ms_d2 == "Age category (no double-counting)" ~ "Age"
    ##Management Practice or Tech Type##
    , ic == "EG.3.2-24" & ms_d2 == "Management Practice or Tech Type (double-counting allowed)" ~ "Management Practice or Tech Type"
    ##Commodity##
    , ic == "EG.3.2-24" & ms_d2 == "Commodity" ~ "Commodity"

    ##EG.3.2-25 (Type of Hectare by Sex)####
    ##Sex##
    , ic == "EG.3.2-25" & ms_d2 == "Sex of Participant (no double-counting)" ~ "Sex"
    ###Age####
    , ic == "EG.3.2-25" & ms_d2 == "Age of Participant (no double-counting)" ~ "Age"
    ###Management Practice or Tech Type####
    , ic == "EG.3.2-25" & ms_d2 == "Management Practice or Tech Type (double-counting allowed)" ~ "Management Practice or Tech Type"
    ###Commodity####
    , ic == "EG.3.2-25" & ms_d2 == "Commodity" ~ "Commodity"

    ##EG.3.2-26 (Value Type of Product, Service, or Commodity Sex)####
    ##(Type of Product) Inputs: Seeds and Planting Material##
    , ic == "EG.3.2-26" & ms_d2 == "Producer - smallholder" ~ "Producer - Smallholder"
    , ic == "EG.3.2-26" & ms_d2 == "Producer - non-smallholder" ~ "Producer - Non-Smallholder"
    , ic == "EG.3.2-26" & ms_d2 == "Firm - microenterprise" ~ "Firm - Microenterprise"
    , ic == "EG.3.2-26" & ms_d2 == "Firm - Small and medium enterprise" ~ "Firm - Small and Medium Enterprise"
    , ic == "EG.3.2-26" & ms_d2 == "Firm - Small & Medium enterprise" ~ "Firm - Small and Medium Enterprise"
    , ic == "EG.3.2-26" & ms_d2 == "Firm - Large enterprise or corporation" ~ "Firm - Large Enterprise or Corporation"

    ##EG.3.2-27####
    ##Size of recipient(s)##
    , ic == "EG.3.2-27" & ms_d2 == "Size of recipient(s)" ~ "Size of recipient(s)"
    ##Sex of recipient(s)##
    , ic == "EG.3.2-27"& ms_d2 == "Sex of recipient(s)" ~ "Sex of recipient(s)"
    ##Age of recipient(s)##
    , ic == "EG.3.2-27" & ms_d2 == "Age of recipient(s)" ~ "Age of recipient(s)"
    ##VALUE of financing received by recipient Size, Sex, and Age##
    , ic == "EG.3.2-27" & ms_d2 == "VALUE of financing received by recipient Size, Sex, and Age:" ~ "Value of financing received"
    ##NUMBER of financing recipients by Size, Sex, and Age##
    , ic == "EG.3.2-27" & ms_d2 == "NUMBER of financing recipients by Size, Sex, and Age:" ~ "Number of financing recipients"

    ##EG.3.2-x17 (Number of Individuals Applying Promoted Practices)####
    , ic == "EG.3.2-x17" & ms_d2 == "Commodity" ~ "Commodity"
    , ic == "EG.3.2-x17" & ms_d2 == "Sex" ~ "Sex"
    , ic == "EG.3.2-x17" & ms_d2 == "Technology type" ~ "Management Practice or Tech Type (double-counting allowed)"
    , ic == "EG.3.2-x17" & ms_d2 == "Duration" ~ "Duration"

    ##EG.3.2-x18 (Hectares Under Improved Management)####
    , ic == "EG.3.2-x18" ~ ms_d2

    ##EG.3.2-x19 (Value)####
    , ic == "EG.3.2-x19" ~ "Producer - Smallholder"

    ##EG.3.3-10 (Percentage and Number by Age)####
    ##Age Less than 19 Years##
    , ic == "EG.3.3-10" & ms_d2 == "Age <19 years" ~ "Age Less Than 19 Years"
    ##Age 19+ Years##
    , ic == "EG.3.3-10"& ms_d2 == "Age 19+ years" ~ "Age 19+ Years"
    ##DNA##
    , ic == "EG.3.3-10" & ms_d2 == "Disaggregate Not Available (for age category of female participants)" ~ "Disaggregates Not Available"

    ##EG.4.2-7 (Participants by Sex)####
    ##Females##
    , ic == "EG.4.2-7" & ms_d2 == "Sex" ~ "Sex"
    ##Type of Documentation##
    , ic == "EG.10.4-7" & ms_d2 == "Type of Documentation" ~ "Type of Documentation"
    ##Location##
    , ic == "EG.10.4-7" & ms_d2 == "Location" ~ "Location"

    ##EG.10.4-8 (Participants by Resource Type)####
    ##Sex##
    , ic == "EG.10.4-8" & ms_d2 == "Sex" ~ "Sex"
    ##Tenure Type##
    , ic == "EG.10.4-8" & ms_d2 == "Tenure Type" ~ "Tenure Type"
    ##Location##
    , ic == "EG.10.4-8" & ms_d2 == "Location" ~ "Location"

    ##ES.5-1 (Participants by Sex)####
    ##Females##
    , ic == "HL.8.2-5" & ms_d3 %in% c( "Rural - number of households where both water and soap are found at the commonly used handwashing station"
                                       , "Urban - number of households where both water and soap are found at the commonly used handwashing station"
                                       , "Disaggregates Not Available (for Residence Location) - number of households where both water and soap are found at the commonly used handwashing station") ~ "Number of Households Where Both Water and Soap are Found at the Commonly Used Handwashing Station"

    ##Total Number of Households Covered by the Handwashing Behavior change Intervention##
    , ic == "HL.8.2-5" & ms_d3 %in% c( "Rural - total number of households covered by the handwashing behavior change intervention"
                                       , "Urban - total number of households covered by the handwashing behavior change intervention"
                                       , "Disaggregates Not Available (for Residence Location)  - total number of households covered by the handwashing behavior change intervention") ~ "Number of Households Where Both Water and Soap are Found at the Commonly Used Handwashing Station"

    ##HL.9-1 (Participants by Sex)####
    ##Females##
    , ic == "HL.9-1" & ms_d1 == "Sex (no double-counting allowed)" ~ ms_d2
    ##Number of children under five whose parents/caretakers received behavior change communication interventions that promote essential infant and young child feeding behaviors##
    , ic == "HL.9-1" & ms_d2 == "Number of children under five whose parents/caretakers received behavior change communication interventions that promote essential infant and young child feeding behaviors" ~ "Number of Children Under Five Whose Parents/Caretakers Received Behavior Change Communication Interventions that Promote Essential Infant and Young Child Feeding Behaviors"
    ##Number of children 6-59 months who received vitamin A supplementation in the past 6 months##
    , ic == "HL.9-1" & ms_d2 == "Number of children 6-59 months  who received vitamin A supplementation in the past 6 months" ~ "Number of Children 6-59 Months Who Received Vitamin A Supplementation in the Past 6 Months"
    ##Number of children under five who received zinc supplementation during episode of diarrhea##
    , ic == "HL.9-1" & ms_d2 == "Number of children under five who received zinc supplementation during episode of diarrhea" ~ "Number of Children Under Five Who Received Zinc Supplementation During Episode of Diarrhea"
    ##Number of children under five who received Multiple Micronutrient Powder (MNP) supplementation##
    , ic == "HL.9-1" & ms_d2 == "Number of children under five who received Multiple Micronutrient Powder (MNP) supplementation" ~ "Number of Children Under Five Who Received Multiple Micronutrient Powder (MNP) Supplementation"
    ##Number of children under five who were admitted for treatment of severe acute malnutrition##
    , ic == "HL.9-1" & ms_d2 == "Number of children under five who were admitted for treatment of severe acute malnutrition" ~ "Number of Children Under Five Who Were Admitted for Treatment of Severe Acute Malnutrition"
    ##Number of children under five who were admitted for treatment of moderate acute malnutrition##
    , ic == "HL.9-1" & ms_d2 == "Number of children under five who were admitted for treatment of moderate acute malnutrition" ~ "Number of Children Under Five Who Were Admitted for Treatment of Moderate Acute Malnutrition"
    ##Number of children under five who received direct food assistance of fortified/specialized food products##
    , ic == "HL.9-1" & ms_d2 == "Number of children under five who received direct food assistance of fortified/specialized food products" ~ "Number of Children Under Five Who Received Direct Food Assistance of Fortified/Specialized Food Products"
    ##HL.9-3 (Participants)##
    ##Women < 19##
    , ic == "HL.9-3" & ms_d2 == "Number of women < 19 years of age" ~ "Women < 19"
    ##Women >= 19##
    , ic == "HL.9-3" & ms_d2 == "Number of women > or == 19 years of age" ~ "Women >= 19"
    ##DNA##
    , ic == "HL.9-3" & ms_d2 == "Disaggregates Not Available" ~ "Disaggregates Not Available"
    ##Intervention##
    ##Number of Women Receiving Iron and Folic Acid Supplementation##
    , ic == "HL.9-3" & ms_d2 == "Number of women receiving iron and folic acid supplementation" ~ "Number of Women Receiving Iron and Folic Acid Supplementation"
    ##Number of Women Receiving Counseling on Maternal and/or Child Nutrition##
    , ic == "HL.9-3" & ms_d2 == "Number of women receiving counseling on maternal and/or child nutrition" ~ "Number of Women Receiving Counseling on Maternal and/or Child Nutrition"
    ##Number of Women Receiving Calcium Supplementation##
    , ic == "HL.9-3" & ms_d2 == "Number of women receiving calcium supplementation" ~ "Number of Women Receiving Calcium Supplementation"
    ##Number of Women Receiving Multiple Micronutrient Supplementation##
    , ic == "HL.9-3" & ms_d2 == "Number of women receiving multiple micronutrient supplementation" ~ "Number of Women Receiving Multiple Micronutrient Supplementation"
    ##Number of Women Receiving Direct Food Assistance of Fortified/Specialized Food Products##
    , ic == "HL.9-3" & ms_d2 == "Number of women receiving direct food assistance of fortified/specialized food products" ~ "Number of Women Receiving Direct Food Assistance of Fortified/Specialized Food Products"

    ##HL.9-4 (Participants by Sex)####
    ##Females##
    , ic == "HL.9-4" & ms_d1 == "Sex" ~ ms_d2
    ##Non-degree seeking trainees##
    , ic == "HL.9-4" & ms_d2 == "Non-degree seeking trainees" ~ "Non-Degree Seeking Trainees"
    ##Degree seeking trainees: New##
    , ic == "HL.9-4" & ms_d2 == "Degree seeking trainees: New" ~ "Degree Seeking Trainees: New"
    ##Degree seeking trainees: Continuing##
    , ic == "HL.9-4" & ms_d2 == "Degree seeking trainees: Continuing" ~ "Degree Seeking Trainees: Continuing"
    ##Degree seeking trainees: Disaggregates Not Available##
    , ic == "HL.9-4" & ms_d2 == "Degree seeking trainees: Disaggregates Not Available" ~ "Degree Seeking Trainees: Disaggregates Not Available"
    ##Disaggregates Not Available##
    , ic == "HL.9-4" & ms_d2 == "Disaggregates Not Available" ~ "Disaggregates Not Available"

    ##RESIL-1 (Number of Plans by Type -  Double Counting)####
    ##Total Number of Plans##
    , ic == "RESIL-1" & ms_d2 %in% c("Community - number of plans", "Government - number of plans") ~ "Total Number of Plans"
    ##Proposed##
    , ic == "RESIL-1" & ms_d4 == "Proposed" ~ "Proposed"
    ##Adopted##
    , ic == "RESIL-1" & ms_d4 == "Adopted" ~ "Adopted"
    ##Implemented##
    , ic == "RESIL-1" & ms_d4 == "Implemented" ~ "Implemented"
    ##Institutionalized##
    , ic == "RESIL-1" & ms_d4 == "Institutionalized" ~ "Institutionalized"
    ##FTFMS Only##
    , ic %in% c("EG.3.2-x20", "EG.3.2-x25", "EG.3.2-x27", "EG.3.2-x29"
                , "EG.3.2-x34", "EG.3.2-x37", "EG.3.2-x41", "EG.5.2-x1", "FTF-x01") ~ ms_d2
    ##Commodity##
    , ic %in% c("EG.3.2-x23", "EG.3.3-x11") ~ ms_d1
    , .default = ms_d2
  )
}

## First order ####
#' Standardize FTFMS disaggregates
#'
#' These four functions are used in a mutate statement in `rbind_dis_to_ftfms.R`. They use case_when to
#' create standardized disaggregates for d1, d2, d3, d4 to match the
#' DIS disaggregates.
#' @param ic a vector (column) of indicator codes
#' @param ms_d1 a vector (column) of DisaggregationName1 from the FTFMS database
#' @param ms_d2 a vector (column) of DisaggregationName2 from the FTFMS database
#' @param ms_d3 a vector (column) of DisaggregationName3 from the FTFMS database
#' @param ms_d4 a vector (column) of DisaggregationName4 from the FTFMS database
#'
#' @return a vector (column) of strings with disaggregates
#' @examples
#' if(exists(ms)) ms %>% mutate(d1 = first_order())
first_order <- function(ic, ms_d1, ms_d2, ms_d3, ms_d4){
  dplyr::case_when(
    ##CBLD-9####
    ic == "CBLD-9" & ms_d2 == "Education (higher education, secondary, primary)" ~ "Education (Higher Education, Secondary, Primary)"
    , ic == "CBLD-9" & ms_d2 == "Research institutions (non-degree granting)" ~ "Research Institution (Non-Degree Granting)"
    , ic == "CBLD-9" & ms_d2 == "Cooperative (formal and registered private sector firm that serves members)" ~ "Cooperative (Formal and Registered Private Sector Firm That Serves Members)"
    , ic == "CBLD-9" & ms_d2 == "Producer Group (informal, unregistered group of producers)" ~ "Producer Group (Informal, Unregistered Group of Producers)"
    , ic == "CBLD-9" & ms_d2 == "Faith-based organizations" ~"Faith-based organizations"
    , ic == "CBLD-9" & ms_d2 == "Governmental agencies (at national or sub-national levels)" ~ "Governmental Agencies (at National or Sub-National Levels)"
    , ic == "CBLD-9" & ms_d2 == "Health service delivery sites (hospital, clinic, community, pharmacies)" ~ "Health Service Delivery Sites (Hospital, Clinic, Community, Pharmacies)"
    , ic == "CBLD-9" & ms_d2 == "Private sector firms" ~ "Private Sector Firms"
    , ic == "CBLD-9" & ms_d2 == "Non-governmental and not-for profit organizations" ~ "Non-Governmental and Not-For Profit Organizations"
    , ic == "CBLD-9" & ms_d2 == "Other" ~ "Other"

    ##EG.3-x1####
    ##Gendered Household Type##
    , ic == "EG.3-x1" ~ ms_d1 # "Gendered Household Type" , "Location",  "Duration"

    ##EG.3-2####
    ##Sex##
    , ic == "EG.3-2" & ms_d1 == "Sex of individuals participating (no double counting)" ~ "Sex of Individuals Participating"
    ##Age##
    , ic == "EG.3-2" & ms_d1 == "Age Category of individuals participating (no double counting)" ~ "Age Category of Individuals Participating"
    ##Type of Individuals Participating##
    , ic == "EG.3-2" & ms_d2 == "Type of individuals participating (double-counting allowed)" ~ "Type of Individuals Participating"

    ##EG.3-x6, -7, -8####
    , ic == "EG.3-x6, -7, -8" & ms_d2 == "Number of Direct Beneficiaries" ~ "Number of Direct Beneficiaries"
    , ic == "EG.3-x6, -7, -8" & ms_d2 == "Purchased input costs (USD)" ~ "Purchased Input Costs (USD)"
    , ic == "EG.3-x6, -7, -8" & ms_d2 == "Quantity of Sales" ~ "Quantity of Sales"
    , ic == "EG.3-x6, -7, -8" & ms_d2 == "Total Production" ~ "Total Production"
    , ic == "EG.3-x6, -7, -8" & ms_d2 == "Units of Production: Hectares planted (for crops); Number of animals (for milk, eggs); or Area (ha) of ponds or Number of crates (for fish)" ~ "Units of Production"
    , ic == "EG.3-x6, -7, -8" & ms_d2 == "Value of Sales (USD)" ~ "Value of Sales (USD)"

    ##EG.3-x9####
    # , ic == "EG.3-x9" & ms_d1 == "Duration" ~ "Duration"
    # , ic == "EG.3-x9" & ms_d1 == "Location" ~ "Location"
    # , ic == "EG.3-x9" & ms_d1 == "Sex of job-holder" ~ "Sex of Job-Holder"

    ##EG.3.1-1 (Kms)####
    ##Improved##
    , ic == "EG.3.1-1" & ms_d1 == "Improved" ~ "Improved"
    , ic == "EG.3.1-1" & ms_d1 == "Constructed (new)" ~ "Constructed (New)"
    , ic == "EG.3.1-1" & ms_d1 == "Disaggregates Not Available" ~ "Disaggregates Not Available"

    ##EG.3.1-x12 (Policies)####
    ##Policy Area##
    , ic == "EG.3.1-x12" & ms_d1 == "Policy Area" ~ "Policy Area"
    ##Process/Step##
    , ic == "EG.3.1-x12" & ms_d1 == "Process/Step" ~ "Process/Step"

    ##EG.3.1-x13####
    , ic == "EG.3.1-x13" & ms_d1 == "Female" ~ "Female"
    , ic == "EG.3.1-x13" & ms_d1 == "Male" ~ "Male"
    , ic == "EG.3.1-x13" & ms_d1 == "Joint" ~ "Joint"
    , ic == "EG.3.1-x13" & ms_d1 == "Communal" ~ "Communal"
    , ic == "EG.3.1-x13" & ms_d1 == "Disaggregates Not Available" ~ "Disaggregates Not Available"

    ##EG.3.1-14####
    ##USG Commitment Amount##
    , ic == "EG.3.1-14" & ms_d2 == "USG commitment amount ($USD)" ~ "USG Commitment Amount"
    ##Private Sector Partner Leveraged Amount##
    , ic == "EG.3.1-14" & ms_d2 == "Private sector partner leveraged amount ($USD)" ~ "Private Sector Partner Leveraged Amount"

    ##EG.3.1-x22 (Hectares)####
    , ic == "EG.3.1-x22" ~ ms_d1

    ##EG.3.2-x1####
    , ic == "EG.3.2-x1" & ms_d1 == "Type of Individual" ~ ms_d2
    , ic == "EG.3.2-x1" & ms_d1 == "Sex" ~ ms_d1

    ##EG.3.2-x3####
    , ic == "EG.3.2-x3" & ms_d1 == "Sex of owner / producer" ~ "Sex of Owner/Producer"
    , ic == "EG.3.2-x3" ~ ms_d1

    ##EG.3.2-x4####
    , ic == "EG.3.2-x4" & ms_d1 == "Duration" ~ "Duration"
    , ic == "EG.3.2-x4" & ms_d1 == "Type of organization" ~ "Type of Organization"

    ##EG.3.2-x5####
    , ic == "EG.3.2-x5" & ms_d2 == "Agricultural post harvest transformation" ~ "Agricultural Post Harvest Transformation"
    , ic == "EG.3.2-x5" & ms_d2 == "Agricultural production" ~ "Agricultural Production"
    , ic == "EG.3.2-x5" &
      ! ms_d2 %in% c("Agricultural production" ~ "Agricultural Production"
                     , "Agricultural post harvest transformation") ~ ms_d2

    ##EG.3.2-x6####
    , ic == "EG.3.2-x6" & ms_d1 == "Sex of recipient" ~ "Sex of Recipient(s)"
    , ic == "EG.3.2-x6" & ms_d1 == "Type of loan recipient" ~ "Type of Loan Recipient(s)"

    ##EG.3.2-x14####
    , ic == "EG.3.2-x14" ~ ms_d1

    ##EG.3.2-x17####
    , ic == "EG.3.2-x17" & ms_d1 == "Producers" ~ "Producers"
    , ic == "EG.3.2-x17" & ms_d1 == "Others" ~ "Others"

    ##EG.3.2-x18 (Hectares Under Improved Management)####
    , ic == "EG.3.2-x18" & ms_d1 == "Technology type" ~ "Management Practice or Tech Type"
    , ic == "EG.3.2-x18" & ms_d1 != "Technology type" ~ ms_d1

    ##EG.3.2-x19 (Value by Type and Sex####
    , ic == "EG.3.2-x19" ~ "Commodity"

    ##EG.3.2-x22 ####
    , ic == "EG.3.2-x22" ~ "Value of New Private Sector Capital Investment"

    ##EG.3.2-2####
    ##Sex##
    , ic == "EG.3.2-2" & ms_d1 == "Sex" ~ "Sex"
    ##Duration##
    , ic == "EG.3.2-2" & ms_d1 == "Duration" ~ "Duration"

    ##EG.3.2-7####
    ##Plant and Animal Improvement Research##
    , ic == "EG.3.2-7" &
      ms_d2 %in% c("Plant and Animal Improvement Research"
                   , "Plant and Animal Improvement Research - Unique number of technologies / practices") ~ "Plant and Animal Improvement Research"
    ##Production Systems Research##
    , ic == "EG.3.2-7" & ms_d2 %in% c("Production Systems Research"
                                      , "Production Systems Research - Unique number of technologies / practices") ~ "Production Systems Research"
    ##Social Science Research##
    , ic == "EG.3.2-7" & ms_d2 %in% c("Social Science Research"
                                      , "Social Science Research - Unique number of technologies / practices") ~ "Social Science Research"
    ##DNA##
    , ic == "EG.3.2-7" &
      ms_d2 %in% c("Disaggregates Not Available (research category not listed or unknown)"
                   , "Disaggregates Not Available (research category not listed or unknown) - Unique number of technologies / practices") ~ "Disaggregates Not Available"

    ##EG.3.2-24####
    ##Smallholder Producers##
    , ic == "EG.3.2-24" & ms_d1 == "Value Chain Actor Type: Smallholder producers" ~ "Smallholder Producers"
    ##Non-Smallholder Producers##
    , ic == "EG.3.2-24" & ms_d1 == "Value Chain Actor Type: Non-smallholder producers" ~ "Non-Smallholder Producers"
    ##People in Government##
    , ic == "EG.3.2-24" & ms_d1 == "Value Chain Actor Type: People in government" ~ "People in Government"
    ##People in Private Sector Firms##
    , ic == "EG.3.2-24" & ms_d1 == "Value Chain Actor Type: People in private Sector Firms" ~ "People in Private Sector Firms"
    ##People in Civil Society##
    , ic == "EG.3.2-24" & ms_d2 == "Value Chain Actor Type: People in civil society" ~ "People in Civil Society"
    ##Others##
    , ic == "EG.3.2-24" & ms_d1 == "Value Chain Actor Type: Others" ~ "Others"
    ##DNA##
    , ic == "EG.3.2-24" & ms_d1 == "Value Chain Actor Type: Disaggregates Not Available" ~ "Disaggregates Not Available"

    ##EG.3.2-25####
    ##Crop Land##
    , ic == "EG.3.2-25" & ms_d1 == "Type of Hectare: Crop land" ~ "Crop Land"
    ##Cultivated Pasture##
    , ic == "EG.3.2-25" & ms_d1 == "Type of Hectare: Cultivated pasture" ~ "Cultivated Pasture"
    ##Rangeland##
    , ic == "EG.3.2-25" & ms_d1 == "Type of Hectare: Rangeland" ~ "Rangeland"
    ##Conservation/Protected Area##
    , ic == "EG.3.2-25" & ms_d1 == "Type of Hectare: Conservation/protected area" ~ "Conservation/Protected Area"
    ##Freshwater or Marine Ecosystems##
    , ic == "EG.3.2-25" & ms_d1 == "Type of Hectare: Freshwater or marine ecosystems" ~ "Freshwater or Marine Ecosystems"
    ##Aquaculture##
    , ic == "EG.3.2-25" & ms_d1 == "Type of Hectare: Aquaculture" ~ "Aquaculture"
    ##Other##
    , ic == "EG.3.2-25" & ms_d1 == "Type of Hectare: Other" ~ "Other"

    ##EG.3.2-26####
    ##(Type of Product) Inputs: Seeds and Planting Material##
    , ic == "EG.3.2-26" & ms_d1 == "(Type of Product): Inputs: Seeds and planting material" ~ "(Type of Product) Inputs: Seeds and Planting Material"
    ##(Type of Product): Inputs: Other Non-Durable Inputs, Such as Fertilizers and Pesticides##
    , ic == "EG.3.2-26" & ms_d1 == "(Type of Product): Inputs: Other non-durable inputs, such as fertilizer and pesticides" ~ "(Type of Product): Inputs: Other Non-Durable Inputs, Such as Fertilizers and Pesticides"
    ##(Type of Product): Inputs: Durable Equipment and Machinery##
    , ic == "EG.3.2-26" & ms_d1 == "(Type of Product): Inputs: Durable equipment and machinery" ~ "(Type of Product): Inputs: Durable Equipment and Machinery"
    ##(Type of Product): Processed Products/Value Added Products (Post Harvest)##
    , ic == "EG.3.2-26" & ms_d1 == "(Type of Product): Processed products/value added products (post-harvest)" ~ "(Type of Product): Processed Products/Value Added Products (Post Harvest)"
    ##(Type of Product): Post-Harvest Storage and Processing Equipment##
    , ic == "EG.3.2-26" & ms_d1 == "(Type of Product): Post-harvest storage and processing equipment" ~ "(Type of Product): Post-Harvest Storage and Processing Equipment"
    ##(Type of Product): Disaggregates Not Available##
    , ic == "EG.3.2-26" & ms_d1 == "(Type of Product): Disaggregates Not Available" ~ "(Type of Product): Disaggregates Not Available"
    ##(Type of Service): Business Services, Including Financial, Entrepreneurial, Legal, etc.##
    , ic == "EG.3.2-26" & ms_d1 == "(Type of Service): Business services, including financial, entrepreneurial, legal, etc." ~ "(Type of Service): Business Services, Including Financial, Entrepreneurial, Legal, etc."
    ##(Type of Service): Information Services:  SMS, Radio, TV, Print, etc.##
    , ic == "EG.3.2-26" & ms_d1 == "(Type of Service): Information services: SMS, Radio, TV, print, etc." ~ "(Type of Service): Information Services:  SMS, Radio, TV, Print, etc."
    ##(Type of Service): Production Support Services##
    , ic == "EG.3.2-26" & ms_d1 == "(Type of Service): Production support services (see full list in Handbook)" ~ "(Type of Service): Production Support Services"
    ##(Type of Service): Disaggregates Not Available##
    , ic == "EG.3.2-26" & ms_d1 == "(Type of Service): Disaggregates Not Available" ~ "(Type of Service): Disaggregates Not Available"
    ##Commodity##
    , ic == "EG.3.2-26" & ms_d1 %in% c(
      "Apples", "Avocado", "Bananas (NRVCC)", "Beans (biofortified) (NRVCC)"
      , "Beans (non-biofortified) (NRVCC)", "Beans and pulses (NRVCC)"
      , "Cabbage (NRVCC)", "Camel (live) (NRVCC)", "Carrots (NRVCC)"
      , "Cashews (NRVCC)", "Cassava", "Cattle (Beef) (NRVCC)"
      , "Cattle (live) (NRVCC)", "Cauliflower (NRVCC)"
      , "Chickens (poultry) (NRVCC)", "Chickpea (NRVCC)"
      , "Chilies (NRVCC)", "Citrus (NRVCC)", "Coffee", "Commodity"
      , "Cowpeas (NRVCC)", "Cucumber", "Dairy (non-milk products, e.g. yogurt) (NRVCC)"
      , "Disaggregates Not Available or Other", "Eggplant", "Eggs (NRVCC)"
      , "Fava Beans (NRVCC)", "Fish (ponds) (NRVCC)", "Forage/Fodder"
      , "Fruits", "Goat (live) (NRVCC)", "Green Beans", "Groundnuts/peanuts (NRVCC)"
      , "Honey", "Horticulture", "Lentil (NRVCC)", "Lettuce", "Maize"
      , "Maize grain", "Mango (NRVCC)", "Milk (Cow) (NRVCC)"
      , "Milk (general, not animal-specific) (NRVCC)", "Millet"
      , "Mung Bean (NRVCC)", "Not Applicable", "Okra (NRVCC)", "Onions/Shallots"
      , "Papaya (NRVCC)", "Paprika", "Passion fruit (NRVCC)", "Peas, green (NRVCC)"
      , "Pigeon peas (NRVCC)", "Pineapples (NRVCC)", "Potatoes", "Pulses (NRVCC)"
      , "Rice", "Rice grain", "Rice-irrigated", "Sesame Seed (NRVCC)"
      , "Sesame Seed (oil)", "Sheep (live) (NRVCC)", "Sorghum", "Soybean Rain-fed (NRVCC)"
      , "Soybeans (NRVCC)", "Soybeans (oil)", "Sunflower (oil)", "Sunflower seed (NRVCC)"
      , "Sweet Potatoes", "Sweet Potatoes - Orange/Dark Yellow - biofortified (NRVCC)"
      , "Sweet Potatoes - Orange/Dark Yellow - non biofortified (NRVCC)"
      , "Sweet Potatoes - White/Pale Yellow", "Tomatoes", "Vegetables"
      , "Watermelon", "Wheat") ~ "Commodity"

    ##EG.3.2-27####
    ##Total Value of Financing Received by Recipient Size, Sex, and Age##
    , ic == "EG.3.2-27" & ms_d1 == "TOTAL VALUE of financing received by recipient Size, Sex, and Age:" ~ "Total Value of Financing Received"
    ##Total Number of Financing Recipients by Size, Sex, and Age##
    , ic == "EG.3.2-27" & ms_d1 == "TOTAL NUMBER of financing recipients by Size, Sex, and Age:" ~ "Total Number of Financing Recipients"
    ##Total Value of Debt Financing Received by Recipient Size, Sex, and Age##
    , ic == "EG.3.2-27" & ms_d1 == "TOTAL VALUE of DEBT financing received by recipient Size, Sex, and Age:" ~ "Total Value of Debt Financing Received"
    ##Total Number of Debt Financing Recipients by Size, Sex, and Age##
    , ic == "EG.3.2-27" & ms_d1 == "TOTAL NUMBER of DEBT financing recipients by Size, Sex, and Age:" ~ "Total Number of Debt Financing Recipients"
    ##Type of Financing Accessed: Cash Debt##
    , ic == "EG.3.2-27" & ms_d1 == "Type of financing accessed: Cash Debt" ~ "Type of Financing Accessed: Cash Debt"
    ##Type of Financing Accessed: In-Kind Debt##
    , ic == "EG.3.2-27" & ms_d1 == "Type of financing accessed: In-Kind Debt" ~ "Type of Financing Accessed: In-Kind Debt"
    ##Type of Financing Accessed: Non-Debt##
    , ic == "EG.3.2-27" & ms_d1 == "Type of financing accessed: Non-Debt" ~ "Type of Financing Accessed: Non-Debt"

    ##EG.3.2-28####
    , ic == "EG.3.2-28" ~ "Number of Hectares Under Improved Management Practices or Technologies That Promote Improved Climate Risk Reduction and/or Natural Resources Management"

    ##EG.3.3-10####
    ##Percentage of Female Participants Consuming a Diet of Minimum Diversity##
    , ic == "EG.3.3-10" & ms_d3 == "Percentage of female participants consuming a diet of minimum diversity" ~ "Percentage of Female Participants Consuming a Diet of Minimum Diversity"
    ##Number of Female Participants of the Nutrition-Sensitive Agriculture Activity##
    , ic == "EG.3.3-10" & ms_d3 == "Number of female participants of the nutrition-sensitive agriculture activity" ~ "Number of Female Participants of the Nutrition-Sensitive Agriculture Activity"

    ##EG.10.4-7####
    ##Resource Type: Land##
    , ic == "EG.10.4-7" & ms_d1 == "Resource Type: Land" ~ "Resource Type: Land"
    ##Resource Type: Marine##
    , ic == "EG.10.4-7" & ms_d1 == "Resource Type: Marine" ~ "Resource Type: Marine"

    ##EG.10.4-8####
    ##Resource Type: Land##
    , ic == "EG.10.4-8" & ms_d1 == "Resource Type: Land" ~ "Resource Type: Land"
    ##Resource Type: Marine##
    , ic == "EG.10.4-8" & ms_d1 == "Resource Type: Marine" ~ "Resource Type: Marine"

    ##GNDR-2 (Number)####
    ##Number of Female Program Participants##
    , ic == "GNDR-2" & ms_d1 == "Numerator: Number of female program participants" ~ "Number of Female Program Participants"
    ##Number of Total Program Participants##
    , ic == "GNDR-2" & ms_d1 == "Denominator: Total number of male and female participants in the program" ~ "Number of Total Program Participants"

    ## HL.8.2-2####
    ##Sex##
    , ic == "HL.8.2-2" & ms_d2 == "Urban - percentage of households with soap and water at a handwashing station" ~ "Urban"
    ##Rural##
    , ic == "HL.8.2-2" & ms_d2 == "Rural - percentage of households with soap and water at a handwashing station" ~ "Rural"
    ##DNA##
    , ic == "HL.8.2-2" & ms_d2 == "Disaggregates Not Available (for Residence Location) - percentage of households with soap and water at a handwashing station" ~ "Disaggregates Not Available"

    ##HL.9-1####
    ##Sex##
    , ic == "HL.9-1" & ms_d1 == "Sex (no double-counting allowed)" ~ "Sex"
    ##Intervention##
    , ic == "HL.9-1" & ms_d1 == "Intervention (double-counting allowed)" ~ "Intervention (Double-Counting Allowed)"

    ##HL.9-2 (Participants by Sex)####
    ##Female##
    , ic == "HL.9-2" ~ ms_d1

    ##HL.9-3####
    ##Women < 19##
    , ic == "HL.9-3" & ms_d1 == "Age (no double counting)" ~ "Age"

    ##Intervention##
    ##Number of Women Receiving Iron and Folic Acid Supplementation##
    , ic == "HL.9-3" & ms_d1 == "Intervention (double-counting allowed)" ~ "Intervention (Double-Counting Allowed)"

    ##HL.9-4####
    ##Sex##
    , ic == "HL.9-4" & ms_d1 == "Sex" ~ "Sex"
    ##Type of Training##
    , ic == "HL.9-4" & ms_d1 == "Type of Training" ~ "Type of Training"

    ##RESIL-1 (Number of Plans by Type -  Double Counting)####
    ##Government##
    , ic == "RESIL-1" & ms_d2 == "Government - number of plans" ~ "Government - Number of Plans"
    , ic == "RESIL-1" & ms_d3 == "Phase of development (double-counting allowed)" ~ "Phase of Development"
    ##Community##
    , ic == "RESIL-1" & ms_d2 == "Community - number of plans" ~ "Community - Number of Plans"
    , ic == "RESIL-1" & ms_d3 == "Phase of development (double-counting allowed)" ~ "Phase of Development"

    ##Youth-3 (Number by Age)##
    ##Number of Youth Program Participants##
    , ic == "Youth-3" & ms_d1 == "Numerator: Number of youth program participants" ~ "Number of Youth Program Participants"
    ##Number of Total Program Participants##
    , ic == "Youth-3" &  ms_d1 == "Denominator: Number of total participants in the program" ~ "Number of Total Program Participants"
    , ic %in% c("EG.3.2-x20", "EG.3.2-x21", "EG.3.2-x25", "EG.3.2-x27"
                , "EG.3.2-x29", "EG.3.2-x32", "EG.3.2-x34", "EG.3.2-x37"
                , "EG.3.2-x41", "EG.5.2-x1", "EG.11-x6", "FTF-x01", "HL.9-x1"
                , "HL.9-x15") ~ ms_d1
    ##Commodity Indicators##
    , ic %in% c("EG.3.2-x23", "EG.3.3-x11") ~ ms_d2
    , .default = ms_d1
  )
}


# DIS ####
#' @export make_disaggregate_crosswalk
make_disaggregate_crosswalk <- function(){


  # TODO: Remove FTFMS from the DIS data. The UDNs cause multiples of the FTFMS disaggregates
  # Fourth order ####
d4 <-  dplyr::bind_rows(
  ##EG.3-10-11-12 (TP and UP by Sex)##
  ## Females
  tibble(ic = "EG.3-10-11-12"
         , udn = c("3.1.1.1.8.1.2", "3.1.1.1.9.1.2", "3.1.1.1.10.1.2"
                   , "3.1.1.2.8.1.2", "3.1.1.2.9.1.2", "3.1.1.2.10.1.2"
                   , "3.1.1.3.8.1.2", "3.1.1.3.9.1.2", "3.1.1.3.10.1.2"
                   , "3.1.1.4.8.1.2", "3.1.1.4.9.1.2", "3.1.1.4.10.1.2")
         , name = "Female"
  ),
  ##Males##
  tibble(ic = "EG.3-10-11-12"
         , udn = c("3.1.1.1.8.1.1", "3.1.1.1.9.1.1", "3.1.1.1.10.1.1"
                   , "3.1.1.2.8.1.1", "3.1.1.2.9.1.1", "3.1.1.2.10.1.1"
                   , "3.1.1.3.8.1.1", "3.1.1.3.9.1.1", "3.1.1.3.10.1.1"
                   , "3.1.1.4.8.1.1", "3.1.1.4.9.1.1", "3.1.1.4.10.1.1")
         , name = "Male"
  ),
  ##DNA##
  tibble(ic = "EG.3-10-11-12"
         , udn = c("3.1.1.1.8.1.3", "3.1.1.1.9.1.3", "3.1.1.1.10.1.3"
                   , "3.1.1.2.8.1.3", "3.1.1.2.9.1.3", "3.1.1.2.10.1.3"
                   , "3.1.1.3.8.1.3", "3.1.1.3.9.1.3", "3.1.1.3.10.1.3"
                   , "3.1.1.4.8.1.3", "3.1.1.4.9.1.3", "3.1.1.4.10.1.3")
         , name = "Disaggregates Not Available"
  ),
  ##15-29##
  tibble(ic = "EG.3-10-11-12"
         , udn = c("3.1.1.1.8.2.1", "3.1.1.1.9.2.1", "3.1.1.1.10.2.1"
                   , "3.1.1.2.8.2.1", "3.1.1.2.9.2.1", "3.1.1.2.10.2.1"
                   , "3.1.1.3.8.2.1", "3.1.1.3.9.2.1", "3.1.1.3.10.2.1"
                   , "3.1.1.4.8.2.1", "3.1.1.4.9.2.1", "3.1.1.4.10.2.1")
         , name = "15-29"
  ),
  ##30+##
  tibble(ic = "EG.3-10-11-12",
         udn = c("3.1.1.1.8.2.2", "3.1.1.1.9.2.2", "3.1.1.1.10.2.2"
                 , "3.1.1.2.8.2.2", "3.1.1.2.9.2.2", "3.1.1.2.10.2.2"
                 , "3.1.1.3.8.2.2", "3.1.1.3.9.2.2", "3.1.1.3.10.2.2"
                 , "3.1.1.4.8.2.2", "3.1.1.4.9.2.2", "3.1.1.4.10.2.2")
         , name = "30+"
  ),
  ##DNA##
  tibble(ic = "EG.3-10-11-12"
         , udn = c("3.1.1.1.8.2.3", "3.1.1.1.9.2.3", "3.1.1.1.10.2.3"
                   , "3.1.1.2.8.2.3", "3.1.1.2.9.2.3", "3.1.1.2.10.2.3"
                   , "3.1.1.3.8.2.3", "3.1.1.3.9.2.3", "3.1.1.3.10.2.3"
                   , "3.1.1.4.8.2.3", "3.1.1.4.9.2.3", "3.1.1.4.10.2.3")
         , name = "Disaggregates Not Available"
  ),
  #### EG.3.2-26 (USD by Sex) ####
  ##Female##
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.3.1", "3.1.1.2.3.1", "3.1.1.3.3.1", "3.1.1.4.3.1"
                   , "3.1.1.5.3.1", "3.1.2.1.3.1", "3.1.2.2.3.1", "3.1.2.3.3.1"
                   , "3.1.2.4.3.1", "3.1.2.5.3.1", "3.1.3.1.3.1", "3.1.3.2.3.1"
                   , "3.1.3.3.3.1", "3.1.3.4.3.1", "3.1.3.5.3.1", "3.1.4.1.3.1"
                   , "3.1.4.2.3.1", "3.1.4.3.3.1", "3.1.4.4.3.1", "3.1.4.5.3.1"
                   , "3.1.5.1.3.1", "3.1.5.2.3.1", "3.1.5.3.3.1", "3.1.5.4.3.1"
                   , "3.1.5.5.3.1", "3.1.6.1.3.1", "3.1.6.2.3.1", "3.1.6.3.3.1"
                   , "3.1.6.4.3.1", "3.1.6.5.3.1", "3.2.1.1.3.1", "3.2.1.2.3.1"
                   , "3.2.1.3.3.1", "3.2.2.1.3.1", "3.2.2.2.3.1", "3.2.2.3.3.1"
                   , "3.2.3.1.3.1", "3.2.3.2.3.1", "3.2.3.3.3.1", "3.2.4.1.3.1"
                   , "3.2.4.2.3.1", "3.2.4.3.3.1", "3.3.1.4.1", "3.3.2.4.1"
                   , "3.3.3.4.1", "3.3.4.4.1", "3.3.5.4.1")
         , name = "Female - Value of Sales"
  ),
  ##Male##
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.3.3", "3.1.1.2.3.3", "3.1.1.3.3.3", "3.1.1.4.3.3"
                   , "3.1.1.5.3.3", "3.1.2.1.3.3", "3.1.2.2.3.3", "3.1.2.3.3.3"
                   , "3.1.2.4.3.3", "3.1.2.5.3.3", "3.1.3.1.3.3", "3.1.3.2.3.3"
                   , "3.1.3.3.3.3", "3.1.3.4.3.3", "3.1.3.5.3.3", "3.1.4.1.3.3"
                   , "3.1.4.2.3.3", "3.1.4.3.3.3", "3.1.4.4.3.3", "3.1.4.5.3.3"
                   , "3.1.5.1.3.3", "3.1.5.2.3.3", "3.1.5.3.3.3", "3.1.5.4.3.3"
                   , "3.1.5.5.3.3", "3.1.6.1.3.3", "3.1.6.2.3.3", "3.1.6.3.3.3"
                   , "3.1.6.4.3.3", "3.1.6.5.3.3", "3.2.1.1.3.3", "3.2.1.2.3.3"
                   , "3.2.1.3.3.3", "3.2.2.1.3.3", "3.2.2.2.3.3", "3.2.2.3.3.3"
                   , "3.2.3.1.3.3", "3.2.3.2.3.3", "3.2.3.3.3.3", "3.2.4.1.3.3"
                   , "3.2.4.2.3.3", "3.2.4.3.3.3", "3.3.1.4.4", "3.3.2.4.4"
                   , "3.3.3.4.4", "3.3.4.4.4", "3.3.5.4.4")
         , name = "Male - Value of Sales"
  ),
  ##Mixed##
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.3.5", "3.1.1.2.3.5", "3.1.1.3.3.5", "3.1.1.4.3.5"
                   , "3.1.1.5.3.5", "3.1.2.1.3.5", "3.1.2.2.3.5", "3.1.2.3.3.5"
                   , "3.1.2.4.3.5", "3.1.2.5.3.5", "3.1.3.1.3.5", "3.1.3.2.3.5"
                   , "3.1.3.3.3.5", "3.1.3.4.3.5", "3.1.3.5.3.5", "3.1.4.1.3.5"
                   , "3.1.4.2.3.5", "3.1.4.3.3.5", "3.1.4.4.3.5", "3.1.4.5.3.5"
                   , "3.1.5.1.3.5", "3.1.5.2.3.5", "3.1.5.3.3.5", "3.1.5.4.3.5"
                   , "3.1.5.5.3.5", "3.1.6.1.3.5", "3.1.6.2.3.5", "3.1.6.3.3.5"
                   , "3.1.6.4.3.5", "3.1.6.5.3.5", "3.2.1.1.3.5", "3.2.1.2.3.5"
                   , "3.2.1.3.3.5", "3.2.2.1.3.5", "3.2.2.2.3.5", "3.2.2.3.3.5"
                   , "3.2.3.1.3.5", "3.2.3.2.3.5", "3.2.3.3.3.5", "3.2.4.1.3.5"
                   , "3.2.4.2.3.5", "3.2.4.3.3.5", "3.3.1.4.7", "3.3.2.4.7"
                   , "3.3.3.4.7", "3.3.4.4.7", "3.3.5.4.7")
         , name = "Mixed Sexes - Value of Sales"
  ),
  ##DNA##
  tibble(ic = "EG.3.2-26",
         udn = c("3.1.1.1.3.7", "3.1.1.2.3.7", "3.1.1.3.3.7", "3.1.1.4.3.7"
                 , "3.1.1.5.3.7", "3.1.2.1.3.7", "3.1.2.2.3.7", "3.1.2.3.3.7"
                 , "3.1.2.4.3.7", "3.1.2.5.3.7", "3.1.3.1.3.7", "3.1.3.2.3.7"
                 , "3.1.3.3.3.7", "3.1.3.4.3.7", "3.1.3.5.3.7", "3.1.4.1.3.7"
                 , "3.1.4.2.3.7", "3.1.4.3.3.7", "3.1.4.4.3.7", "3.1.4.5.3.7"
                 , "3.1.5.1.3.7", "3.1.5.2.3.7", "3.1.5.3.3.7", "3.1.5.4.3.7"
                 , "3.1.5.5.3.7", "3.1.6.1.3.7", "3.1.6.2.3.7", "3.1.6.3.3.7"
                 , "3.1.6.4.3.7", "3.1.6.5.3.7", "3.2.1.1.3.7", "3.2.1.2.3.7"
                 , "3.2.1.3.3.7", "3.2.2.1.3.7", "3.2.2.2.3.7", "3.2.2.3.3.7"
                 , "3.2.3.1.3.7", "3.2.3.2.3.7", "3.2.3.3.3.7", "3.2.4.1.3.7"
                 , "3.2.4.2.3.7", "3.2.4.3.3.7", "3.3.1.4.10", "3.3.2.4.10"
                 , "3.3.3.4.10", "3.3.4.4.10", "3.3.5.4.10")
         , name = "Disaggregates Not Available - Value of Sales"
  ),
  #### EG.3.2-26 (Volume by Sex) ####
  ##Female##
  tibble(ic = "EG.3.2-26",
         udn = c("3.3.1.4.2", "3.3.2.4.2", "3.3.3.4.2", "3.3.4.4.2", "3.3.5.4.2")
         , name = "Female – Volume of Sales"
  ),
  ##Male##
  tibble(ic = "EG.3.2-26",
         udn = c("3.3.1.4.5", "3.3.2.4.5", "3.3.3.4.5", "3.3.4.4.5", "3.3.5.4.5")
         , name = "Male - Volume of Sales"
  ),
  ##Mixed##
  tibble(ic = "EG.3.2-26",
         udn = c("3.3.1.4.8", "3.3.2.4.8", "3.3.3.4.8", "3.3.4.4.8", "3.3.5.4.8")
         , name = "Mixed Sexes - Volume of Sales"
  ),
  ##DNA##
  tibble(ic = "EG.3.2-26",
         udn = c("3.3.1.4.11", "3.3.2.4.11", "3.3.3.4.11", "3.3.4.4.11", "3.3.5.4.11")
         , name = "Disaggregates Not Available - Volume of Sales"
  ),
  #### EG.3.2-26 (Participants by Sex) ####
  ##Female##
  tibble(ic = "EG.3.2-26",
         udn = c("3.1.1.1.3.2", "3.1.1.2.3.2", "3.1.1.3.3.2", "3.1.1.4.3.2"
                 , "3.1.1.5.3.2", "3.1.2.1.3.2", "3.1.2.2.3.2", "3.1.2.3.3.2"
                 , "3.1.2.4.3.2", "3.1.2.5.3.2", "3.1.3.1.3.2", "3.1.3.2.3.2"
                 , "3.1.3.3.3.2", "3.1.3.4.3.2", "3.1.3.5.3.2", "3.1.4.1.3.2"
                 , "3.1.4.2.3.2", "3.1.4.3.3.2", "3.1.4.4.3.2", "3.1.4.5.3.2"
                 , "3.1.5.1.3.2", "3.1.5.2.3.2", "3.1.5.3.3.2", "3.1.5.4.3.2"
                 , "3.1.5.5.3.2", "3.1.6.1.3.2", "3.1.6.2.3.2", "3.1.6.3.3.2"
                 , "3.1.6.4.3.2", "3.1.6.5.3.2", "3.2.1.1.3.2", "3.2.1.2.3.2"
                 , "3.2.1.3.3.2", "3.2.2.1.3.2", "3.2.2.2.3.2", "3.2.2.3.3.2"
                 , "3.2.3.1.3.2", "3.2.3.2.3.2", "3.2.3.3.3.2", "3.2.4.1.3.2"
                 , "3.2.4.2.3.2", "3.2.4.3.3.2", "3.3.1.4.3", "3.3.2.4.3"
                 , "3.3.3.4.3", "3.3.4.4.3", "3.3.5.4.3")
         , name = "Female - Number of Participants"
  ),
  ##Male##
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.3.4", "3.1.1.2.3.4", "3.1.1.3.3.4", "3.1.1.4.3.4"
                   , "3.1.1.5.3.4", "3.1.2.1.3.4", "3.1.2.2.3.4", "3.1.2.3.3.4"
                   , "3.1.2.4.3.4", "3.1.2.5.3.4", "3.1.3.1.3.4", "3.1.3.2.3.4"
                   , "3.1.3.3.3.4", "3.1.3.4.3.4", "3.1.3.5.3.4", "3.1.4.1.3.4"
                   , "3.1.4.2.3.4", "3.1.4.3.3.4", "3.1.4.4.3.4", "3.1.4.5.3.4"
                   , "3.1.5.1.3.4", "3.1.5.2.3.4", "3.1.5.3.3.4", "3.1.5.4.3.4"
                   , "3.1.5.5.3.4", "3.1.6.1.3.4", "3.1.6.2.3.4", "3.1.6.3.3.4"
                   , "3.1.6.4.3.4", "3.1.6.5.3.4", "3.2.1.1.3.4", "3.2.1.2.3.4"
                   , "3.2.1.3.3.4", "3.2.2.1.3.4", "3.2.2.2.3.4", "3.2.2.3.3.4"
                   , "3.2.3.1.3.4", "3.2.3.2.3.4", "3.2.3.3.3.4", "3.2.4.1.3.4"
                   , "3.2.4.2.3.4", "3.2.4.3.3.4", "3.3.1.4.6", "3.3.2.4.6"
                   , "3.3.3.4.6", "3.3.4.4.6", "3.3.5.4.6")
         , name = "Male - Number of Participants"
  ),
  ##Mixed##
  tibble(ic = "EG.3.2-26",
         udn = c("3.1.1.1.3.6", "3.1.1.2.3.6", "3.1.1.3.3.6", "3.1.1.4.3.6"
                 , "3.1.1.5.3.6", "3.1.2.1.3.6", "3.1.2.2.3.6", "3.1.2.3.3.6"
                 , "3.1.2.4.3.6", "3.1.2.5.3.6", "3.1.3.1.3.6", "3.1.3.2.3.6"
                 , "3.1.3.3.3.6", "3.1.3.4.3.6", "3.1.3.5.3.6", "3.1.4.1.3.6"
                 , "3.1.4.2.3.6", "3.1.4.3.3.6", "3.1.4.4.3.6", "3.1.4.5.3.6"
                 , "3.1.5.1.3.6", "3.1.5.2.3.6", "3.1.5.3.3.6", "3.1.5.4.3.6"
                 , "3.1.5.5.3.6", "3.1.6.1.3.6", "3.1.6.2.3.6", "3.1.6.3.3.6"
                 , "3.1.6.4.3.6", "3.1.6.5.3.6", "3.2.1.1.3.6", "3.2.1.2.3.6"
                 , "3.2.1.3.3.6", "3.2.2.1.3.6", "3.2.2.2.3.6", "3.2.2.3.3.6"
                 , "3.2.3.1.3.6", "3.2.3.2.3.6", "3.2.3.3.3.6", "3.2.4.1.3.6"
                 , "3.2.4.2.3.6", "3.2.4.3.3.6", "3.3.1.4.9", "3.3.2.4.9"
                 , "3.3.3.4.9", "3.3.4.4.9", "3.3.5.4.9")
         , name = "Mixed Sexes - Number of Participants"
  ),
  ##DNA##
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.3.8", "3.1.1.2.3.8", "3.1.1.3.3.8", "3.1.1.4.3.8"
                   , "3.1.1.5.3.8", "3.1.2.1.3.8", "3.1.2.2.3.8", "3.1.2.3.3.8"
                   , "3.1.2.4.3.8", "3.1.2.5.3.8", "3.1.3.1.3.8", "3.1.3.2.3.8"
                   , "3.1.3.3.3.8", "3.1.3.4.3.8", "3.1.3.5.3.8", "3.1.4.1.3.8"
                   , "3.1.4.2.3.8", "3.1.4.3.3.8", "3.1.4.4.3.8", "3.1.4.5.3.8"
                   , "3.1.5.1.3.8", "3.1.5.2.3.8", "3.1.5.3.3.8", "3.1.5.4.3.8"
                   , "3.1.5.5.3.8", "3.1.6.1.3.8", "3.1.6.2.3.8", "3.1.6.3.3.8"
                   , "3.1.6.4.3.8", "3.1.6.5.3.8", "3.2.1.1.3.8", "3.2.1.2.3.8"
                   , "3.2.1.3.3.8", "3.2.2.1.3.8", "3.2.2.2.3.8", "3.2.2.3.3.8"
                   , "3.2.3.1.3.8", "3.2.3.2.3.8", "3.2.3.3.3.8", "3.2.4.1.3.8"
                   , "3.2.4.2.3.8", "3.2.4.3.3.8", "3.3.1.4.12", "3.3.2.4.12"
                   , "3.3.3.4.12", "3.3.4.4.12", "3.3.5.4.12")
         , name = "Disaggregates Not Available - Number of Participants"
  ),
  #### EG.3.2-26 (USD by Age) ####
  ##15-29 years##
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.4.1", "3.1.1.2.4.1", "3.1.1.3.4.1", "3.1.1.4.4.1"
                   , "3.1.1.5.4.1", "3.1.2.1.4.1", "3.1.2.2.4.1", "3.1.2.3.4.1"
                   , "3.1.2.4.4.1", "3.1.2.5.4.1", "3.1.3.1.4.1", "3.1.3.2.4.1"
                   , "3.1.3.3.4.1", "3.1.3.4.4.1", "3.1.3.5.4.1", "3.1.4.1.4.1"
                   , "3.1.4.2.4.1", "3.1.4.3.4.1", "3.1.4.4.4.1", "3.1.4.5.4.1"
                   , "3.1.5.1.4.1", "3.1.5.2.4.1", "3.1.5.3.4.1", "3.1.5.4.4.1"
                   , "3.1.5.5.4.1", "3.1.6.1.4.1", "3.1.6.2.4.1", "3.1.6.3.4.1"
                   , "3.1.6.4.4.1", "3.1.6.5.4.1", "3.2.1.1.4.1", "3.2.1.2.4.1"
                   , "3.2.1.3.4.1", "3.2.2.1.4.1", "3.2.2.2.4.1", "3.2.2.3.4.1"
                   , "3.2.3.1.4.1", "3.2.3.2.4.1", "3.2.3.3.4.1", "3.2.4.1.4.1"
                   , "3.2.4.2.4.1", "3.2.4.3.4.1", "3.3.1.5.1", "3.3.2.5.1"
                   , "3.3.3.5.1", "3.3.4.5.1", "3.3.5.5.1")
         , name = "15-29 Years - Value of Sales"
  ),
  ##30+ years##
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.4.3", "3.1.1.2.4.3", "3.1.1.3.4.3", "3.1.1.4.4.3"
                   , "3.1.1.5.4.3", "3.1.2.1.4.3", "3.1.2.2.4.3", "3.1.2.3.4.3"
                   , "3.1.2.4.4.3", "3.1.2.5.4.3", "3.1.3.1.4.3", "3.1.3.2.4.3"
                   , "3.1.3.3.4.3", "3.1.3.4.4.3", "3.1.3.5.4.3", "3.1.4.1.4.3"
                   , "3.1.4.2.4.3", "3.1.4.3.4.3", "3.1.4.4.4.3", "3.1.4.5.4.3"
                   , "3.1.5.1.4.3", "3.1.5.2.4.3", "3.1.5.3.4.3", "3.1.5.4.4.3"
                   , "3.1.5.5.4.3", "3.1.6.1.4.3", "3.1.6.2.4.3", "3.1.6.3.4.3"
                   , "3.1.6.4.4.3", "3.1.6.5.4.3", "3.2.1.1.4.3", "3.2.1.2.4.3"
                   , "3.2.1.3.4.3", "3.2.2.1.4.3", "3.2.2.2.4.3", "3.2.2.3.4.3"
                   , "3.2.3.1.4.3", "3.2.3.2.4.3", "3.2.3.3.4.3", "3.2.4.1.4.3"
                   , "3.2.4.2.4.3", "3.2.4.3.4.3", "3.3.1.5.4", "3.3.2.5.4"
                   , "3.3.3.5.4", "3.3.4.5.4", "3.3.5.5.4")
         , name = "30+ Years - Value of Sales"
  ),
  ##Mixed Ages##
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.4.5", "3.1.1.2.4.5", "3.1.1.3.4.5", "3.1.1.4.4.5"
                   , "3.1.1.5.4.5", "3.1.2.1.4.5", "3.1.2.2.4.5", "3.1.2.3.4.5"
                   , "3.1.2.4.4.5", "3.1.2.5.4.5", "3.1.3.1.4.5", "3.1.3.2.4.5"
                   , "3.1.3.3.4.5", "3.1.3.4.4.5", "3.1.3.5.4.5", "3.1.4.1.4.5"
                   , "3.1.4.2.4.5", "3.1.4.3.4.5", "3.1.4.4.4.5", "3.1.4.5.4.5"
                   , "3.1.5.1.4.5", "3.1.5.2.4.5", "3.1.5.3.4.5", "3.1.5.4.4.5"
                   , "3.1.5.5.4.5", "3.1.6.1.4.5", "3.1.6.2.4.5", "3.1.6.3.4.5"
                   , "3.1.6.4.4.5", "3.1.6.5.4.5", "3.2.1.1.4.5", "3.2.1.2.4.5"
                   , "3.2.1.3.4.5", "3.2.2.1.4.5", "3.2.2.2.4.5", "3.2.2.3.4.5"
                   , "3.2.3.1.4.5", "3.2.3.2.4.5", "3.2.3.3.4.5", "3.2.4.1.4.5"
                   , "3.2.4.2.4.5", "3.2.4.3.4.5", "3.3.1.5.7", "3.3.2.5.7"
                   , "3.3.3.5.7", "3.3.4.5.7", "3.3.5.5.7")
         , name = "Mixed Ages - Value of Sales"
  ),
  ##Disaggregates Not Available##
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.4.7", "3.1.1.2.4.7", "3.1.1.3.4.7", "3.1.1.4.4.7"
                   , "3.1.1.5.4.7", "3.1.2.1.4.7", "3.1.2.2.4.7", "3.1.2.3.4.7"
                   , "3.1.2.4.4.7", "3.1.2.5.4.7", "3.1.3.1.4.7", "3.1.3.2.4.7"
                   , "3.1.3.3.4.7", "3.1.3.4.4.7", "3.1.3.5.4.7", "3.1.4.1.4.7"
                   , "3.1.4.2.4.7", "3.1.4.3.4.7", "3.1.4.4.4.7", "3.1.4.5.4.7"
                   , "3.1.5.1.4.7", "3.1.5.2.4.7", "3.1.5.3.4.7", "3.1.5.4.4.7"
                   , "3.1.5.5.4.7", "3.1.6.1.4.7", "3.1.6.2.4.7", "3.1.6.3.4.7"
                   , "3.1.6.4.4.7", "3.1.6.5.4.7", "3.2.1.1.4.7", "3.2.1.2.4.7"
                   , "3.2.1.3.4.7", "3.2.2.1.4.7", "3.2.2.2.4.7", "3.2.2.3.4.7"
                   , "3.2.3.1.4.7", "3.2.3.2.4.7", "3.2.3.3.4.7", "3.2.4.1.4.7"
                   , "3.2.4.2.4.7", "3.2.4.3.4.7", "3.3.1.5.10", "3.3.2.5.10"
                   , "3.3.3.5.10", "3.3.4.5.10", "3.3.5.5.10")
         , name = "Disaggregates Not Available - Value of sales"
  ),
  #### EG.3.2-26 (Volume by Age) ####
  ##15-29 years##
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.5.2", "3.3.2.5.2", "3.3.3.5.2", "3.3.4.5.2", "3.3.5.5.2"
                   , "3.3.1.5.5", "3.3.2.5.5", "3.3.3.5.5", "3.3.4.5.5", "3.3.5.5.5")
         , name = "30+ Years - Volume of Sales"
  ),
  ##Mixed Ages##
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.5.8", "3.3.2.5.8", "3.3.3.5.8", "3.3.4.5.8", "3.3.5.5.8")
         , name = "Mixed Ages - Volume of Sales"
  ),
  ##Disaggregates Not Available##
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.5.11", "3.3.2.5.11", "3.3.3.5.11", "3.3.4.5.11", "3.3.5.5.11")
         , name = "Disaggregates Not Available - Volume of sales"
  ),
  #### EG.3.2-26 (Participants by Age) ####
  ##15-29 Years##
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.4.2", "3.1.1.2.4.2", "3.1.1.3.4.2", "3.1.1.4.4.2"
                   , "3.1.1.5.4.2", "3.1.2.1.4.2", "3.1.2.2.4.2", "3.1.2.3.4.2"
                   , "3.1.2.4.4.2", "3.1.2.5.4.2", "3.1.3.1.4.2", "3.1.3.2.4.2"
                   , "3.1.3.3.4.2", "3.1.3.4.4.2", "3.1.3.5.4.2", "3.1.4.1.4.2"
                   , "3.1.4.2.4.2", "3.1.4.3.4.2", "3.1.4.4.4.2", "3.1.4.5.4.2"
                   , "3.1.5.1.4.2", "3.1.5.2.4.2", "3.1.5.3.4.2", "3.1.5.4.4.2"
                   , "3.1.5.5.4.2", "3.1.6.1.4.2", "3.1.6.2.4.2", "3.1.6.3.4.2"
                   , "3.1.6.4.4.2", "3.1.6.5.4.2", "3.2.1.1.4.2", "3.2.1.2.4.2"
                   , "3.2.1.3.4.2", "3.2.2.1.4.2", "3.2.2.2.4.2", "3.2.2.3.4.2"
                   , "3.2.3.1.4.2", "3.2.3.2.4.2", "3.2.3.3.4.2", "3.2.4.1.4.2"
                   , "3.2.4.2.4.2", "3.2.4.3.4.2", "3.3.1.5.3", "3.3.2.5.3"
                   , "3.3.3.5.3", "3.3.4.5.3", "3.3.5.5.3")
         , name = "15-29 Years - Number of Participants"
  ),
  ##30+ Years##
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.4.4", "3.1.1.2.4.4", "3.1.1.3.4.4", "3.1.1.4.4.4"
                   , "3.1.1.5.4.4", "3.1.2.1.4.4", "3.1.2.2.4.4", "3.1.2.3.4.4"
                   , "3.1.2.4.4.4", "3.1.2.5.4.4", "3.1.3.1.4.4", "3.1.3.2.4.4"
                   , "3.1.3.3.4.4", "3.1.3.4.4.4", "3.1.3.5.4.4", "3.1.4.1.4.4"
                   , "3.1.4.2.4.4", "3.1.4.3.4.4", "3.1.4.4.4.4", "3.1.4.5.4.4"
                   , "3.1.5.1.4.4", "3.1.5.2.4.4", "3.1.5.3.4.4", "3.1.5.4.4.4"
                   , "3.1.5.5.4.4", "3.1.6.1.4.4", "3.1.6.2.4.4", "3.1.6.3.4.4"
                   , "3.1.6.4.4.4", "3.1.6.5.4.4", "3.2.1.1.4.4", "3.2.1.2.4.4"
                   , "3.2.1.3.4.4", "3.2.2.1.4.4", "3.2.2.2.4.4", "3.2.2.3.4.4"
                   , "3.2.3.1.4.4", "3.2.3.2.4.4", "3.2.3.3.4.4", "3.2.4.1.4.4"
                   , "3.2.4.2.4.4", "3.2.4.3.4.4", "3.3.1.5.6", "3.3.2.5.6"
                   , "3.3.3.5.6", "3.3.4.5.6", "3.3.5.5.6")
         , name = "30+ Years - Number of Participants"
  ),
  ##Mixed Ages##
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.4.6", "3.1.1.2.4.6", "3.1.1.3.4.6", "3.1.1.4.4.6"
                   , "3.1.1.5.4.6", "3.1.2.1.4.6", "3.1.2.2.4.6", "3.1.2.3.4.6"
                   , "3.1.2.4.4.6", "3.1.2.5.4.6", "3.1.3.1.4.6", "3.1.3.2.4.6"
                   , "3.1.3.3.4.6", "3.1.3.4.4.6", "3.1.3.5.4.6", "3.1.4.1.4.6"
                   , "3.1.4.2.4.6", "3.1.4.3.4.6", "3.1.4.4.4.6", "3.1.4.5.4.6"
                   , "3.1.5.1.4.6", "3.1.5.2.4.6", "3.1.5.3.4.6", "3.1.5.4.4.6"
                   , "3.1.5.5.4.6", "3.1.6.1.4.6", "3.1.6.2.4.6", "3.1.6.3.4.6"
                   , "3.1.6.4.4.6", "3.1.6.5.4.6", "3.2.1.1.4.6", "3.2.1.2.4.6"
                   , "3.2.1.3.4.6", "3.2.2.1.4.6", "3.2.2.2.4.6", "3.2.2.3.4.6"
                   , "3.2.3.1.4.6", "3.2.3.2.4.6", "3.2.3.3.4.6", "3.2.4.1.4.6"
                   , "3.2.4.2.4.6", "3.2.4.3.4.6", "3.3.1.5.9", "3.3.2.5.9"
                   , "3.3.3.5.9", "3.3.4.5.9", "3.3.5.5.9")
         , name = "Mixed Ages - Number of Participants"
  ),
  ##Disaggregates Not Available##
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.4.8", "3.1.1.2.4.8", "3.1.1.3.4.8", "3.1.1.4.4.8"
                   , "3.1.1.5.4.8", "3.1.2.1.4.8", "3.1.2.2.4.8", "3.1.2.3.4.8"
                   , "3.1.2.4.4.8", "3.1.2.5.4.8", "3.1.3.1.4.8", "3.1.3.2.4.8"
                   , "3.1.3.3.4.8", "3.1.3.4.4.8", "3.1.3.5.4.8", "3.1.4.1.4.8"
                   , "3.1.4.2.4.8", "3.1.4.3.4.8", "3.1.4.4.4.8", "3.1.4.5.4.8"
                   , "3.1.5.1.4.8", "3.1.5.2.4.8", "3.1.5.3.4.8", "3.1.5.4.4.8"
                   , "3.1.5.5.4.8", "3.1.6.1.4.8", "3.1.6.2.4.8", "3.1.6.3.4.8"
                   , "3.1.6.4.4.8", "3.1.6.5.4.8", "3.2.1.1.4.8", "3.2.1.2.4.8"
                   , "3.2.1.3.4.8", "3.2.2.1.4.8", "3.2.2.2.4.8", "3.2.2.3.4.8"
                   , "3.2.3.1.4.8", "3.2.3.2.4.8", "3.2.3.3.4.8", "3.2.4.1.4.8"
                   , "3.2.4.2.4.8", "3.2.4.3.4.8", "3.3.1.5.12", "3.3.2.5.12"
                   , "3.3.3.5.12", "3.3.4.5.12", "3.3.5.5.12")
         , name = "Disaggregates Not Available - Number of Participants"
  ),
  ## FTFMS ####
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Females - Value of sales (in $USD)"
         , name = "Female - Value of Sales"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Females - Number of participant producers"
         , name = "Female - Number of Participants"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Females - Number of participant firms"
         , name = "Female - Number of Participants"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Females - Volume of sales (in MT)"
         , name = "Female - Volume of Sales"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Males - Value of sales (in $USD)"
         , name = "Male - Value of Sales"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Males - Number of participant producers"
         , name = "Male - Number of Participants"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Males - Number of participant firms"
         , name = "Male - Number of Participants"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Males - Volume of sales (in MT)"
         , name = "Male - Volume of Sales"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Mixed Sexes - Value of sales (in $USD)"
         , name = "Mixed Sexes - Value of Sales"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Mixed Sexes - Number of participant producers"
         , name = "Mixed Sexes - Number of Participants"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Mixed Sexes - Number of participant firms"
         , name = "Mixed Sexes - Number of Participants"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Mixed Sexes - Volume of sales (in MT)"
         , name = "Mixed Sexes - Volume of Sales"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Disaggregates Not Available (for sex of producer) - Value of sales (in $USD)"
         , name = "Disaggregates Not Available - Value of Sales"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Disaggregates Not Available (for sex of producer) - Number of participant producers"
         , name = "Disaggregates Not Available - Number of Participants"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Disaggregates Not Available (for sex of producer) - Number of participant firms"
         , name = "Disaggregates Not Available - Number of Participants"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Disaggregates Not Available (for sex of producer) - Volume of sales (in MT)"
         , name = "Disaggregates Not Available - Volume of Sales"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Disaggregates Not Available (for sex of proprietor) - Value of sales (in $USD)"
         , name = "Disaggregates Not Available - Value of Sales"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Disaggregates Not Available (for sex of proprietor) - Number of participant producers"
         , name = "Disaggregates Not Available - Number of Participants"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Disaggregates Not Available (for sex of proprietor) - Number of participant firms"
         , name = "Disaggregates Not Available - Number of Participants"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Disaggregates Not Available (for sex of proprietor) - Volume of sales (in MT)"
         , name = "Disaggregates Not Available - Volume of Sales"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Age 15-29 years - Value of sales (in $USD)"
         , name = "15-29 Years - Value of Sales"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Age 15-29 years - Number of participant producers"
         , name = "15-29 Years - Number of Participants"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Age 15-29 years - Number of participant firms"
         , name = "15-29 Years - Number of Participants"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Age 15-29 years - Volume of sales (in MT)"
         , name = "15-29 Years - Volume of Sales"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Age 30+ years - Value of sales (in $USD)"
         , name = "30+ Years - Value of Sales"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Age 30+ years - Number of participant producers"
         , name = "30+ Years - Number of Participants"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Age 30+ years - Number of participant firms"
         , name = "30+ Years - Number of Participants"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Age 30+ years - Volume of sales (in MT)"
         , name = "30+ Years - Volume of Sales"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Mixed Ages - Value of sales (in $USD)"
         , name = "Mixed Ages - Value of Sales"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Mixed Ages - Number of participant producers"
         , name = "Mixed Ages - Number of Participants"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Mixed Ages - Number of participant firms"
         , name = "Mixed Ages - Number of Participants"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Mixed Ages - Volume of sales (in MT)"
         , name = "Mixed Ages - Volume of Sales"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Disaggregates Not Available (for age of producer) - Value of sales (in $USD)"
         , name = "Disaggregates Not Available - Value of Sales"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Disaggregates Not Available (for age of producer) - Number of participant producers"
         , name = "Disaggregates Not Available - Number of Participants"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Disaggregates Not Available (for age of producer) - Volume of sales (in MT)"
         , name = "Disaggregates Not Available - Volume of Sales"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Disaggregates Not Available (for age of proprietor) - Value of sales (in $USD)"
         , name = "Disaggregates Not Available - Value of Sales"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Disaggregates Not Available (for age of proprietor) - Number of participant producers"
         , name = "Disaggregates Not Available - Number of Participants"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Disaggregates Not Available (for age of proprietor) - Number of participant firms"
         , name = "Disaggregates Not Available - Number of Participants"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d4 = "Disaggregates Not Available (for age of proprietor) - Volume of sales (in MT)"
         , name = "Disaggregates Not Available - Volume of Sales"
  ),

  #### EG.3.2-27 (Value Sex) ####
  ##Individuals / Microenterprises##
  tibble(ic = "EG.3.2-27"
         , udn = c("3.5.1.1.1", "3.5.2.1.1", "3.6.1.1.1", "3.6.2.1.1", "3.7.1.1.1"
                   , "3.7.2.1.1"), name = "Individuals / Microenterprises"
  ),
  tibble(ic = "EG.3.2-27", ms_d4 = "Individuals / Microenterprises"
         , name = "Individuals / Microenterprises"
  ),
  ##Small and Medium Enterprises##
  tibble(ic = "EG.3.2-27"
         , udn = c("3.5.1.1.2", "3.5.2.1.2", "3.6.1.1.2", "3.6.2.1.2"
                   , "3.7.1.1.2", "3.7.2.1.2")
         , name = "Small and Medium Enterprises"
  ),
  tibble(ic = "EG.3.2-27"
         , ms_d4 = "Small and Medium Enterprises"
         , name = "Small and Medium Enterprises"
  ),
  ##Large Enterprises and Corporations##
  tibble(ic = "EG.3.2-27"
         , udn = c("3.5.1.1.3", "3.5.2.1.3", "3.6.1.1.3", "3.6.2.1.3"
                   , "3.7.1.1.3", "3.7.2.1.3"),
         name = "Large Enterprises and Corporations"
  ),
  tibble(ic = "EG.3.2-27"
         , ms_d4 = "Large Enterprises and Corporations"
         , name = "Large Enterprises and Corporations"
  ),
  ##Disaggregates Not Available##
  tibble(ic = "EG.3.2-27"
         , udn = c("3.5.1.1.4", "3.5.2.1.4", "3.6.1.1.4", "3.6.2.1.4"
                   , "3.7.1.1.4", "3.7.2.1.4", "3.5.1.2.4", "3.5.2.2.4"
                   , "3.6.1.2.4", "3.6.2.2.4", "3.7.1.2.4", "3.7.2.2.4"
                   ,"3.5.1.3.4", "3.5.2.3.4", "3.6.1.3.4", "3.6.2.3.4"
                   , "3.7.1.3.4", "3.7.2.3.4")
         , name = "Disaggregates Not Available"
  ),
  tibble(ic = "EG.3.2-27"
         , ms_d4 = "Disaggregates Not Available"
         , name = "Disaggregates Not Available"
  ),
  ##Male##
  tibble(ic = "EG.3.2-27"
         , udn = c("3.5.1.2.1", "3.5.2.2.1", "3.6.1.2.1", "3.6.2.2.1"
                   , "3.7.1.2.1", "3.7.2.2.1")
         , name = "Male"
  ),
  tibble(ic = "EG.3.2-27"
         , ms_d4 = "Male"
         , name = "Male"
  ),
  ##Female##
  tibble(ic = "EG.3.2-27"
         , udn = c("3.5.1.2.2", "3.5.2.2.2", "3.6.1.2.2", "3.6.2.2.2", "3.7.1.2.2"
                   , "3.7.2.2.2")
         , name = "Female"
  ),
  tibble(ic = "EG.3.2-27"
         , ms_d4 = "Female"
         , name = "Female"
  ),
  ##Mixed (for enterprises)##
  tibble(ic = "EG.3.2-27"
         , udn = c("3.5.1.2.3", "3.5.2.2.3", "3.6.1.2.3", "3.6.2.2.3"
                   , "3.7.1.2.3", "3.7.2.2.3", "3.5.1.3.3", "3.5.2.3.3"
                   , "3.6.1.3.3", "3.6.2.3.3", "3.7.1.3.3", "3.7.2.3.3")
         , name = "Mixed (for enterprises)"
  ),
  tibble(ic = "EG.3.2-27"
         , ms_d4 = c("Mixed (for enterprises)", "Mixed Ages (for enterprises)")
         , name = "Mixed (for enterprises)"
  ),
  ##15-29##
  tibble(ic = "EG.3.2-27"
         , udn = c("3.5.1.3.1", "3.5.2.3.1", "3.6.1.3.1", "3.6.2.3.1"
                   , "3.7.1.3.1", "3.7.2.3.1")
         , name = "15-29"
  ),
  tibble(ic = "EG.3.2-27", ms_d4 = "15-29", name = "15-29"
  ),
  ##30+##
  tibble(ic = "EG.3.2-27"
         , udn = c("3.5.1.3.2", "3.5.2.3.2", "3.6.1.3.2", "3.6.2.3.2"
                   , "3.7.1.3.2", "3.7.2.3.2")
         , name = "30+"
  ),
  tibble(ic = "EG.3.2-27", ms_d4 = "30+", name = "30+"
  ),

  #### EG.3.2-x19 (Sales) ####
  tibble(ic = "EG.3.2-x19", ms_d2 = "Number of direct beneficiaries"
         , name = "Number of Participants"
  ),
  tibble(ic = "EG.3.2-x19", ms_d2 = "Reporting year sales"
         , name = "Value of Sales"
  ),
  tibble(ic = "EG.3.2-x19", name = "process"
  )
)


# Third order####
d3 <-  dplyr::bind_rows(
  ##EG-c (Percent of Households)##
  ##Household type: Male and Female Adults (M&F)##
  ##Percent of People Living on Less Than $1.90/Day in this Gendered Household Type##
  tibble(ic = "EG-c"
         , udn = c("3.3.1.1", "3.4.1.1", "3.5.1.1", "3.6.1.1", "3.7.1.1")
         , name = "In All ZOIs/Areas Combined (%)"
  ),
  tibble(ic = "EG-c"
         , udn = c("3.3.1.2", "3.4.1.2", "3.5.1.2", "3.6.1.2", "3.7.1.2")
         , name = "In Target/Aligned Country ZOI (%)"
  ),
  tibble(ic = "EG-c"
         , udn = c("3.3.1.3", "3.4.1.3", "3.5.1.3", "3.6.1.3", "3.7.1.3")
         , name = "In FFP Development Program Area (%)"
  ),
  tibble(ic = "EG-c"
         , udn = c("3.3.1.4", "3.4.1.4", "3.5.1.4", "3.6.1.4", "3.7.1.4")
         , name = "In Resilience to Recurrent Crisis Area (%)"
         ##Number of People in this Gendered Household Type##
  ),
  tibble(ic = "EG-c"
         , udn = c("3.3.2.1", "3.4.2.1", "3.5.2.1", "3.6.2.1", "3.7.2.1")
         , name = "In All ZOIs/Areas Combined"
  ),
  tibble(ic = "EG-c"
         , udn = c("3.3.2.2", "3.4.2.2", "3.5.2.2", "3.6.2.2", "3.7.2.2")
         , name = "In Target/Aligned Country ZOI"
  ),
  tibble(ic = "EG-c"
         , udn = c("3.3.2.3", "3.4.2.3", "3.5.2.3", "3.6.2.3", "3.7.2.3")
         , name = "In FFP Development Program Area"
  ),
  tibble(ic = "EG-c"
         , udn = c("3.3.2.4", "3.4.2.4", "3.5.2.4", "3.6.2.4", "3.7.2.4")
         , name = "In Resilience to Recurrent Crisis Area"
         ##EG-g (Percent of Households)##
         ##Household type: Male and Female Adults (M&F)##
         ##Percent of Households in this Gendered Household Type Falling Below the Fixed Threshold for the Poorest Quintile of the Comparative Wealth Index##
  ),
  tibble(ic = "EG-g"
         , udn = c("3.3.1.1", "3.4.1.1", "3.5.1.1", "3.6.1.1", "3.7.1.1")
         , name = "In All ZOIs/Areas Combined (%)"
  ),
  tibble(ic = "EG-g"
         , udn = c("3.3.1.2", "3.4.1.2", "3.5.1.2", "3.6.1.2", "3.7.1.2")
         , name = "In Target/Aligned Country ZOI (%)"
  ),
  tibble(ic = "EG-g"
         , udn = c("3.3.1.3", "3.4.1.3", "3.5.1.3", "3.6.1.3", "3.7.1.3")
         , name = "In FFP Development Program Area (%)"
  ),
  tibble(ic = "EG-g"
         , udn = c("3.3.1.4", "3.4.1.4", "3.5.1.4", "3.6.1.4", "3.7.1.4")
         , name = "In Resilience to Recurrent Crisis Area (%)"
         ##Number of Households in this Gendered Household Type##
  ),
  tibble(ic = "EG-g"
         , udn = c("3.3.2.1", "3.4.2.1", "3.5.2.1", "3.6.2.1", "3.7.2.1")
         , name = "In All ZOIs/Areas Combined"
  ),
  tibble(ic = "EG-g"
         , udn = c("3.3.2.2", "3.4.2.2", "3.5.2.2", "3.6.2.2", "3.7.2.2")
         , name = "In Target/Aligned Country ZOI"
  ),
  tibble(ic = "EG-g"
         , udn = c("3.3.2.3", "3.4.2.3", "3.5.2.3", "3.6.2.3", "3.7.2.3")
         , name = "In FFP Development Program Area"
  ),
  tibble(ic = "EG-g"
         , udn = c("3.3.2.4", "3.4.2.4", "3.5.2.4", "3.6.2.4", "3.7.2.4")
         , name = "In Resilience to Recurrent Crisis Area"
         ##EG-h (Percent of People)##
         ##Household type: Male and Female Adults (M&F)##
         ##Mean Percent Shortfall of the Poor for People in this Gendered Household Type##
  ),
  tibble(ic = "EG-h"
         , udn = c("3.3.1.1", "3.4.1.1", "3.5.1.1", "3.6.1.1", "3.7.1.1")
         , name = "In All ZOIs/Areas Combined (%)"
  ),
  tibble(ic = "EG-h"
         , udn = c("3.3.1.2", "3.4.1.2", "3.5.1.2", "3.6.1.2", "3.7.1.2")
         , name = "In Target/Aligned Country ZOI (%)"
  ),
  tibble(ic = "EG-h"
         , udn = c("3.3.1.3", "3.4.1.3", "3.5.1.3", "3.6.1.3", "3.7.1.3")
         , name = "In FFP Development Program Area (%)"
  ),
  tibble(ic = "EG-h"
         , udn = c("3.3.1.4", "3.4.1.4", "3.5.1.4", "3.6.1.4", "3.7.1.4")
         , name = "In Resilience to Recurrent Crisis Area (%)"
         ##Number of People in this Gendered Household Type##
  ),
  tibble(ic = "EG-h"
         , udn = c("3.3.2.1", "3.4.2.1", "3.5.2.1", "3.6.2.1", "3.7.2.1")
         , name = "In All ZOIs/Areas Combined"
  ),
  tibble(ic = "EG-h"
         , udn = c("3.3.2.2", "3.4.2.2", "3.5.2.2", "3.6.2.2", "3.7.2.2")
         , name = "In Target/Aligned Country ZOI"
  ),
  tibble(ic = "EG-h"
         , udn = c("3.3.2.3", "3.4.2.3", "3.5.2.3", "3.6.2.3", "3.7.2.3")
         , name = "In FFP Development Program Area"
  ),
  tibble(ic = "EG-h"
         , udn = c("3.3.2.4", "3.4.2.4", "3.5.2.4", "3.6.2.4", "3.7.2.4")
         , name = "In Resilience to Recurrent Crisis Area"
         ##EG.3-10-11-12 (TP and UP by Sex)##
         ##Females##
  ),
  ##EG.3.1-c
  ##Value of exports##
  tibble(ic = "EG.3.1-c", udn = "3.1.1", name = "Value of Exports (USD)"
  ),
  ##Volume of exports##
  tibble(ic = "EG.3.1-c", udn = "3.1.2", name = "Volume of Exports (MT)"
  ),
  tibble(ic = "EG.3-10-11-12"
         , udn = c("3.1.1.1.8.1.2", "3.1.1.1.9.1.2", "3.1.1.1.10.1.2"
                   , "3.1.1.2.8.1.2", "3.1.1.2.9.1.2", "3.1.1.2.10.1.2"
                   , "3.1.1.3.8.1.2", "3.1.1.3.9.1.2", "3.1.1.3.10.1.2"
                   , "3.1.1.4.8.1.2", "3.1.1.4.9.1.2", "3.1.1.4.10.1.2")
         , name = "Sex"
         ##Males##
  ),
  tibble(ic = "EG.3-10-11-12"
         , udn = c("3.1.1.1.8.1.1", "3.1.1.1.9.1.1", "3.1.1.1.10.1.1"
                   , "3.1.1.2.8.1.1", "3.1.1.2.9.1.1", "3.1.1.2.10.1.1"
                   , "3.1.1.3.8.1.1", "3.1.1.3.9.1.1", "3.1.1.3.10.1.1"
                   , "3.1.1.4.8.1.1", "3.1.1.4.9.1.1", "3.1.1.4.10.1.1")
         , name = "Sex"
         ##DNA##
  ),
  tibble(ic = "EG.3-10-11-12"
         , udn = c("3.1.1.1.8.1.3", "3.1.1.1.9.1.3", "3.1.1.1.10.1.3"
                   , "3.1.1.2.8.1.3", "3.1.1.2.9.1.3", "3.1.1.2.10.1.3"
                   , "3.1.1.3.8.1.3", "3.1.1.3.9.1.3", "3.1.1.3.10.1.3"
                   , "3.1.1.4.8.1.3", "3.1.1.4.9.1.3", "3.1.1.4.10.1.3")
         , name = "Sex"
         ##15-29##
  ),
  tibble(ic = "EG.3-10-11-12"
         , udn = c("3.1.1.1.8.2.2", "3.1.1.1.9.2.2", "3.1.1.1.10.2.2"
                   , "3.1.1.2.8.2.2", "3.1.1.2.9.2.2", "3.1.1.2.10.2.2"
                   , "3.1.1.3.8.2.2", "3.1.1.3.9.2.2", "3.1.1.3.10.2.2"
                   , "3.1.1.4.8.2.2", "3.1.1.4.9.2.2", "3.1.1.4.10.2.2")
         , name = "Age"
         ##30+##
  ),
  tibble(ic = "EG.3-10-11-12"
         , udn = c("3.1.1.1.8.2.1", "3.1.1.1.9.2.1", "3.1.1.1.10.2.1"
                   , "3.1.1.2.8.2.1", "3.1.1.2.9.2.1", "3.1.1.2.10.2.1"
                   , "3.1.1.3.8.2.1", "3.1.1.3.9.2.1", "3.1.1.3.10.2.1"
                   , "3.1.1.4.8.2.1", "3.1.1.4.9.2.1", "3.1.1.4.10.2.1")
         , name = "Age"
         ##DNA##
  ),
  tibble(ic = "EG.3-10-11-12"
         , udn = c("3.1.1.1.8.2.3", "3.1.1.1.9.2.3", "3.1.1.1.10.2.3"
                   , "3.1.1.2.8.2.3", "3.1.1.2.9.2.3", "3.1.1.2.10.2.3"
                   , "3.1.1.3.8.2.3", "3.1.1.3.9.2.3", "3.1.1.3.10.2.3"
                   , "3.1.1.4.8.2.3", "3.1.1.4.9.2.3", "3.1.1.4.10.2.3")
         , name = "Age"
         ##EG.3.2-7 (Number of Technologies, Practices, and Approaches)##
         ##Phase I: Under Research as a Result of USG Assistance##
  ),
  tibble(ic = "EG.3.2-7"
         , udn = c("3.1.5.1.1.1", "3.1.5.2.1.1", "3.1.5.3.1.1", "3.1.5.4.1.1")
         , name = "Phase I: Under Research as a Result of USG Assistance"
  ),
  tibble(ic = "EG.3.2-7"
         , ms_d4 = "Phase I: Under research as a result of USG assistance"
         , name = "Phase I: Under Research as a Result of USG Assistance"
         ##Phase II: Under Field Testing as a Result of USG Assistance##
  ),
  tibble(ic = "EG.3.2-7"
         , udn = c("3.1.5.1.1.2", "3.1.5.2.1.2", "3.1.5.3.1.2", "3.1.5.4.1.2")
         , name = "Phase II: Under Field Testing as a Result of USG Assistance"
  ),
  tibble(ic = "EG.3.2-7"
         , ms_d4 = "Phase II: Under field testing as a result of USG assistance"
         , name = "Phase II: Under Field Testing as a Result of USG Assistance"
         ##Phase III: Made Available for Uptake as a Result of USG Assistance##
  ),
  tibble(ic = "EG.3.2-7"
         , udn = c("3.1.5.1.1.3", "3.1.5.2.1.3", "3.1.5.3.1.3", "3.1.5.4.1.3")
         , name = "Phase III: Made Available for Uptake as a Result of USG Assistance"
  ),
  tibble(ic = "EG.3.2-7"
         , ms_d4 = "Phase III: Made available for uptake as a result of USG assistance"
         , name = "Phase III: Made Available for Uptake as a Result of USG Assistance"
         ##Phase IV: Demonstrated Uptake by the Public and/or Private Sector with USG Assistance##
  ),
  tibble(ic = "EG.3.2-7"
         , udn = c("3.1.5.1.1.4", "3.1.5.2.1.4", "3.1.5.3.1.4", "3.1.5.4.1.4")
         , name = "Phase IV: Demonstrated Uptake by the Public and/or Private Sector with USG Assistance"
  ),
  tibble(ic = "EG.3.2-7"
         , ms_d4 = "Phase IV: Demonstrated uptake by the public and/or private sector with USG assistance"
         , name = "Phase IV: Demonstrated Uptake by the Public and/or Private Sector with USG Assistance"
         ##EG.3.2-24 (Participants by Sex)##
         ##Females##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.1.2", "3.2.1.2", "3.3.1.2", "3.4.1.2", "3.5.1.2"
                   , "3.6.1.2", "3.7.1.2")
         , name = "Female"
         ##Males##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.1.1", "3.2.1.1", "3.3.1.1", "3.4.1.1", "3.5.1.1"
                   , "3.6.1.1", "3.7.1.1")
         , name = "Male"
         ##DNA##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.1.3", "3.2.1.3", "3.3.1.3", "3.4.1.3", "3.5.1.3"
                   , "3.6.1.3", "3.7.1.3")
         , name = "Disaggregates Not Available"
         ##15-29##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.2.1", "3.2.2.1", "3.3.2.1", "3.4.2.1", "3.5.2.1"
                   , "3.6.2.1", "3.7.2.1")
         , name = "15-29"
         ##30+##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.2.2", "3.2.2.2", "3.3.2.2", "3.4.2.2", "3.5.2.2"
                   , "3.6.2.2", "3.7.2.2")
         , name = "30+"
         ##DNA##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.2.3", "3.2.2.3", "3.3.2.3", "3.4.2.3", "3.5.2.3"
                   , "3.6.2.3", "3.7.2.3")
         , name = "Disaggregates Not Available"
         ##Crop Genetics##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.3.1", "3.2.3.1", "3.3.3.1", "3.4.3.1", "3.5.3.1"
                   , "3.6.3.1", "3.7.3.1")
         , name = "Crop Genetics"
         ##Cultural Practices##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.3.2", "3.2.3.2", "3.3.3.2", "3.4.3.2", "3.5.3.2"
                   , "3.6.3.2", "3.7.3.2")
         , name = "Cultural Practices"
         ##Livestock Management##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.3.3", "3.2.3.3", "3.3.3.3", "3.4.3.3", "3.5.3.3"
                   , "3.6.3.3", "3.7.3.3")
         , name = "Livestock Management"
         ##Wild-Caught Fisheries Management##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.3.4", "3.2.3.4", "3.3.3.4", "3.4.3.4", "3.5.3.4"
                   , "3.6.3.4", "3.7.3.4")
         , name = "Wild-Caught Fisheries Management"
         ##Aquaculture Management##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.3.5", "3.2.3.5", "3.3.3.5", "3.4.3.5", "3.5.3.5"
                   , "3.6.3.5", "3.7.3.5")
         , name = "Aquaculture Management"
         ##Natural Resource or Ecosystem Management##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.3.6", "3.2.3.6", "3.3.3.6", "3.4.3.6", "3.5.3.6"
                   , "3.6.3.6", "3.7.3.6")
         , name = "Natural Resource or Ecosystem Management"
         ##Pest and Disease Management##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.3.7", "3.2.3.7", "3.3.3.7", "3.4.3.7", "3.5.3.7"
                   , "3.6.3.7", "3.7.3.7")
         , name = "Pest and Disease Management"
         ##Soil-Related Fertility and Conservation##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.3.8", "3.2.3.8", "3.3.3.8", "3.4.3.8", "3.5.3.8"
                   , "3.6.3.8", "3.7.3.8")
         , name = "Soil-Related Fertility and Conservation"
         ##Irrigation##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.3.9", "3.2.3.9", "3.3.3.9", "3.4.3.9", "3.5.3.9"
                   , "3.6.3.9", "3.7.3.9")
         , name = "Irrigation"
         ##Agriculture Water Management-Non-Irrigation Based##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.3.10", "3.2.3.10", "3.3.3.10", "3.4.3.10", "3.5.3.10"
                   , "3.6.3.10", "3.7.3.10")
         , name = "Agriculture Water Management-Non-Irrigation Based"
         ##Climate Mitigation##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.3.11", "3.2.3.11", "3.3.3.11", "3.4.3.11", "3.5.3.11"
                   , "3.6.3.11", "3.7.3.11")
         , name = "Climate Mitigation"
         ##Climate Adaptation/Climate Risk Management##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.3.12", "3.2.3.12", "3.3.3.12", "3.4.3.12", "3.5.3.12"
                   , "3.6.3.12", "3.7.3.12")
         , name = "Climate Adaptation/Climate Risk Management"
         ##Marketing and Distribution##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.3.13", "3.2.3.13", "3.3.3.13", "3.4.3.13", "3.5.3.13"
                   , "3.6.3.13", "3.7.3.13")
         , name = "Marketing and Distribution"
         ##Post-Harvest Handling and Storage##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.3.14", "3.2.3.14", "3.3.3.14", "3.4.3.14", "3.5.3.14"
                   , "3.6.3.14", "3.7.3.14")
         , name = "Post-Harvest Handling and Storage"
         ##Value-Added Processing##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.3.15", "3.2.3.15", "3.3.3.15", "3.4.3.15", "3.5.3.15"
                   , "3.6.3.15", "3.7.3.15")
         , name = "Value-Added Processing"
         ##Other##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.3.16", "3.2.3.16", "3.3.3.16", "3.4.3.16", "3.5.3.16"
                   , "3.6.3.16", "3.7.3.16")
         , name = "Other"
         ##Commodity##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.4", "3.2.4", "3.3.4", "3.4.4", "3.5.4", "3.6.4", "3.7.4")
         , name = "[Commodity (From DIS)]"
         ##FTFMS##
  ),
  tibble(ic = "EG.3.2-24", ms_d2 = c("Sex (no double-counting)"
                                     , "Age category (no double-counting)"
                                     , "Management Practice or Tech Type (double-counting allowed)"
                                     , "Technology type", "Commodity")
         , name = "ms_d3"
         ##EG.3.2-25 (Type of Hectare by Sex)##
         ##Sex##
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.1.1", "3.2.1.1", "3.3.1.1", "3.4.1.1", "3.5.1.1"
                   , "3.6.1.1", "3.7.1.1")
         , name = "Male"
  ),
  tibble(ic = "EG.3.2-25", ms_d3 = "Male"
         , name = "Male"
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.1.2", "3.2.1.2", "3.3.1.2", "3.4.1.2", "3.5.1.2"
                   , "3.6.1.2", "3.7.1.2")
         , name = "Female"
  ),
  tibble(ic = "EG.3.2-25"
         , ms_d3 = "Female"
         , name = "Female"
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.1.3", "3.2.1.3", "3.3.1.3", "3.4.1.3", "3.5.1.3"
                   , "3.6.1.3", "3.7.1.3")
         , name = "Association-Applied"
  ),
  tibble(ic = "EG.3.2-25"
         , ms_d3 = "Association-applied"
         , name = "Association-Applied"
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.1.4", "3.2.1.4", "3.3.1.4", "3.4.1.4", "3.5.1.4"
                   , "3.6.1.4", "3.7.1.4")
         , name = "Disaggregates Not Available"
  ),
  tibble(ic = "EG.3.2-25"
         , ms_d3 = "Disaggregates Not Available"
         , name = "Disaggregates Not Available"
         ##Age##
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.2.1", "3.2.2.1", "3.3.2.1", "3.4.2.1", "3.5.2.1"
                   , "3.6.2.1", "3.7.2.1")
         , name = "15-29"
  ),
  tibble(ic = "EG.3.2-25"
         , ms_d3 = "15-29"
         , name = "15-29"
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.2.2", "3.2.2.2", "3.3.2.2", "3.4.2.2", "3.5.2.2"
                   , "3.6.2.2", "3.7.2.2")
         , name = "30+"
  ),
  tibble(ic = "EG.3.2-25"
         , ms_d3 = "30+"
         , name = "30+"
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.2.3", "3.2.2.3", "3.3.2.3", "3.4.2.3", "3.5.2.3"
                   , "3.6.2.3", "3.7.2.3")
         , name = "Association-Applied"
  ),
  tibble(ic = "EG.3.2-25"
         , ms_d3 = "Association-applied"
         , name = "Association-Applied"
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.2.4", "3.2.2.4", "3.3.2.4", "3.4.2.4", "3.5.2.4"
                   , "3.6.2.4", "3.7.2.4")
         , name = "Disaggregates Not Available"
  ),
  tibble(ic = "EG.3.2-25"
         , ms_d3 = "Disaggregates Not Available"
         , name = "Disaggregates Not Available"
         ##Management Practice or Tech Type##
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.3.1", "3.2.3.1", "3.3.3.1", "3.4.3.1", "3.5.3.1"
                   , "3.6.3.1", "3.7.3.1")
         , name = "Crop Genetics"
  ),
  tibble(ic = "EG.3.2-25"
         , ms_d3 = "Crop genetics"
         , name = "Crop Genetics"
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.3.2", "3.2.3.2", "3.3.3.2", "3.4.3.2", "3.5.3.2"
                   , "3.6.3.2", "3.7.3.2")
         , name = "Cultural Practices"
  ),
  tibble(ic = "EG.3.2-25"
         , ms_d3 = "Cultural practices"
         , name = "Cultural Practices"
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.3.3", "3.2.3.3", "3.3.3.3", "3.4.3.3", "3.5.3.3"
                   , "3.6.3.3", "3.7.3.3")
         , name = "Livestock Management"
  ),
  tibble(ic = "EG.3.2-25"
         , ms_d3 = "Livestock management"
         , name = "Livestock Management"
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.3.4", "3.2.3.4", "3.3.3.4", "3.4.3.4", "3.5.3.4"
                   , "3.6.3.4", "3.7.3.4")
         , name = "Wild-Caught Fisheries Management"
  ),
  tibble(ic = "EG.3.2-25"
         , ms_d3 = "Wild-caught fisheries management"
         , name = "Wild-Caught Fisheries Management"
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.3.5", "3.2.3.5", "3.3.3.5", "3.4.3.5", "3.5.3.5"
                   , "3.6.3.5", "3.7.3.5")
         , name = "Aquaculture Management"
  ),
  tibble(ic = "EG.3.2-25"
         , ms_d3 = "Aquaculture management"
         , name = "Aquaculture Management"
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.3.6", "3.2.3.6", "3.3.3.6", "3.4.3.6", "3.5.3.6"
                   , "3.6.3.6", "3.7.3.6")
         , name = "Natural Resource or Ecosystem Management"
  ),
  tibble(ic = "EG.3.2-25"
         , ms_d3 = "Natural resource or ecosystem management"
         , name = "Natural Resource or Ecosystem Management"
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.3.7", "3.2.3.7", "3.3.3.7", "3.4.3.7", "3.5.3.7"
                   , "3.6.3.7", "3.7.3.7")
         , name = "Pest and Disease Management"
  ),
  tibble(ic = "EG.3.2-25"
         , ms_d3 = "Pest and disease management"
         , name = "Pest and Disease Management"
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.3.8", "3.2.3.8", "3.3.3.8", "3.4.3.8", "3.5.3.8"
                   , "3.6.3.8", "3.7.3.8")
         , name = "Soil-Related Fertility and Conservation"
  ),
  tibble(ic = "EG.3.2-25"
         , ms_d3 = "Soil-related fertility and conservation"
         , name = "Soil-Related Fertility and Conservation"
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.3.9", "3.2.3.9", "3.3.3.9", "3.4.3.9", "3.5.3.9"
                   , "3.6.3.9", "3.7.3.9")
         , name = "Irrigation"
  ),
  tibble(ic = "EG.3.2-25"
         , ms_d3 = "Irrigation"
         , name = "Irrigation"
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.3.10", "3.2.3.10", "3.3.3.10", "3.4.3.10", "3.5.3.10"
                   , "3.6.3.10", "3.7.3.10")
         , name = "Agriculture Water Management-Non-Irrigation Based"
  ),
  tibble(ic = "EG.3.2-25"
         , ms_d3 = "Agriculture water management-non-irrigation based"
         , name = "Agriculture Water Management-Non-Irrigation Based"
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.3.11", "3.2.3.11", "3.3.3.11", "3.4.3.11", "3.5.3.11"
                   , "3.6.3.11", "3.7.3.11")
         , name = "Climate Mitigation"
  ),
  tibble(ic = "EG.3.2-25"
         , ms_d3 = "Climate mitigation"
         , name = "Climate Mitigation"
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.3.12", "3.2.3.12", "3.3.3.12", "3.4.3.12", "3.5.3.12"
                   , "3.6.3.12", "3.7.3.12")
         , name = "Climate Adaptation/Climate Risk Management"
  ),
  tibble(ic = "EG.3.2-25"
         , ms_d3 = "Climate adaptation/climate risk management"
         , name = "Climate Adaptation/Climate Risk Management"
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.3.13", "3.2.3.13", "3.3.3.13", "3.4.3.13", "3.5.3.13"
                   , "3.6.3.13", "3.7.3.13")
         , name = "Other"
  ),
  tibble(ic = "EG.3.2-25"
         , ms_d3 = "Other"
         , name = "Other"
         ##Commodity##
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.4", "3.2.4", "3.3.4", "3.4.4", "3.5.4", "3.6.4"
                   , "3.7.4")
         , name = "[Commodity (From DIS)]"
  ),
  tibble(ic = "EG.3.2-25", ms_d2 ="Commodity"
         , name = "ms_d3"
         ##EG.3.2-26 (USD by Sex)##
         ##Female##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.3.1", "3.1.1.2.3.1", "3.1.1.3.3.1", "3.1.1.4.3.1"
                   , "3.1.1.5.3.1", "3.1.2.1.3.1", "3.1.2.2.3.1", "3.1.2.3.3.1"
                   , "3.1.2.4.3.1", "3.1.2.5.3.1", "3.1.3.1.3.1", "3.1.3.2.3.1"
                   , "3.1.3.3.3.1", "3.1.3.4.3.1", "3.1.3.5.3.1", "3.1.4.1.3.1"
                   , "3.1.4.2.3.1", "3.1.4.3.3.1", "3.1.4.4.3.1", "3.1.4.5.3.1"
                   , "3.1.5.1.3.1", "3.1.5.2.3.1", "3.1.5.3.3.1", "3.1.5.4.3.1"
                   , "3.1.5.5.3.1", "3.1.6.1.3.1", "3.1.6.2.3.1", "3.1.6.3.3.1"
                   , "3.1.6.4.3.1", "3.1.6.5.3.1")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.1.3.1", "3.2.1.2.3.1", "3.2.1.3.3.1")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.1.3.1", "3.2.2.2.3.1", "3.2.2.3.3.1")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.1.3.1", "3.2.3.2.3.1", "3.2.3.3.3.1")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.1.3.1", "3.2.4.2.3.1", "3.2.4.3.3.1")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.4.1", "3.3.2.4.1", "3.3.3.4.1", "3.3.4.4.1"
                   , "3.3.5.4.1")
         , name = "Sex"
         ##Male##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.3.3", "3.1.1.2.3.3", "3.1.1.3.3.3", "3.1.1.4.3.3"
                   , "3.1.1.5.3.3", "3.1.2.1.3.3", "3.1.2.2.3.3", "3.1.2.3.3.3"
                   , "3.1.2.4.3.3", "3.1.2.5.3.3", "3.1.3.1.3.3", "3.1.3.2.3.3"
                   , "3.1.3.3.3.3", "3.1.3.4.3.3", "3.1.3.5.3.3", "3.1.4.1.3.3"
                   , "3.1.4.2.3.3", "3.1.4.3.3.3", "3.1.4.4.3.3", "3.1.4.5.3.3"
                   , "3.1.5.1.3.3", "3.1.5.2.3.3", "3.1.5.3.3.3", "3.1.5.4.3.3"
                   , "3.1.5.5.3.3", "3.1.6.1.3.3", "3.1.6.2.3.3", "3.1.6.3.3.3"
                   , "3.1.6.4.3.3", "3.1.6.5.3.3")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.1.3.3", "3.2.1.2.3.3", "3.2.1.3.3.3")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.1.3.3", "3.2.2.2.3.3", "3.2.2.3.3.3")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.1.3.3", "3.2.3.2.3.3", "3.2.3.3.3.3")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.1.3.3", "3.2.4.2.3.3", "3.2.4.3.3.3")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.4.4", "3.3.2.4.4", "3.3.3.4.4", "3.3.4.4.4"
                   , "3.3.5.4.4")
         , name = "Sex"
         ##Mixed##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.3.5", "3.1.1.2.3.5", "3.1.1.3.3.5", "3.1.1.4.3.5"
                   , "3.1.1.5.3.5", "3.1.2.1.3.5", "3.1.2.2.3.5", "3.1.2.3.3.5"
                   , "3.1.2.4.3.5", "3.1.2.5.3.5", "3.1.3.1.3.5", "3.1.3.2.3.5"
                   , "3.1.3.3.3.5", "3.1.3.4.3.5", "3.1.3.5.3.5", "3.1.4.1.3.5"
                   , "3.1.4.2.3.5", "3.1.4.3.3.5", "3.1.4.4.3.5", "3.1.4.5.3.5"
                   , "3.1.5.1.3.5", "3.1.5.2.3.5", "3.1.5.3.3.5", "3.1.5.4.3.5"
                   , "3.1.5.5.3.5", "3.1.6.1.3.5", "3.1.6.2.3.5", "3.1.6.3.3.5"
                   , "3.1.6.4.3.5", "3.1.6.5.3.5")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.1.3.5", "3.2.1.2.3.5", "3.2.1.3.3.5")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.1.3.5", "3.2.2.2.3.5", "3.2.2.3.3.5")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.1.3.5", "3.2.3.2.3.5", "3.2.3.3.3.5")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.1.3.5", "3.2.4.2.3.5", "3.2.4.3.3.5")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.4.7", "3.3.2.4.7", "3.3.3.4.7", "3.3.4.4.7"
                   , "3.3.5.4.7")
         , name = "Sex"
         ##DNA##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.3.7", "3.1.1.2.3.7", "3.1.1.3.3.7", "3.1.1.4.3.7"
                   , "3.1.1.5.3.7", "3.1.2.1.3.7", "3.1.2.2.3.7", "3.1.2.3.3.7"
                   , "3.1.2.4.3.7", "3.1.2.5.3.7", "3.1.3.1.3.7", "3.1.3.2.3.7"
                   , "3.1.3.3.3.7", "3.1.3.4.3.7", "3.1.3.5.3.7", "3.1.4.1.3.7"
                   , "3.1.4.2.3.7", "3.1.4.3.3.7", "3.1.4.4.3.7", "3.1.4.5.3.7"
                   , "3.1.5.1.3.7", "3.1.5.2.3.7", "3.1.5.3.3.7", "3.1.5.4.3.7"
                   , "3.1.5.5.3.7", "3.1.6.1.3.7", "3.1.6.2.3.7", "3.1.6.3.3.7"
                   , "3.1.6.4.3.7", "3.1.6.5.3.7")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.1.3.7", "3.2.1.2.3.7", "3.2.1.3.3.7")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.1.3.7", "3.2.2.2.3.7", "3.2.2.3.3.7")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.1.3.7", "3.2.3.2.3.7", "3.2.3.3.3.7")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.1.3.7", "3.2.4.2.3.7", "3.2.4.3.3.7")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.4.10", "3.3.2.4.10", "3.3.3.4.10", "3.3.4.4.10"
                   , "3.3.5.4.10")
         , name = "Sex"
         ##EG.3.2-26 (Volume by Sex)##
         ##Female##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.4.2", "3.3.2.4.2", "3.3.3.4.2", "3.3.4.4.2"
                   , "3.3.5.4.2")
         , name = "Sex"
         ##Male##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.4.5", "3.3.2.4.5", "3.3.3.4.5", "3.3.4.4.5"
                   , "3.3.5.4.5")
         , name = "Sex"
         ##Mixed##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.4.8", "3.3.2.4.8", "3.3.3.4.8", "3.3.4.4.8"
                   , "3.3.5.4.8")
         , name = "Sex"
         ##DNA##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.4.11", "3.3.2.4.11", "3.3.3.4.11", "3.3.4.4.11"
                   , "3.3.5.4.11")
         , name = "Sex"
         ##EG.3.2-26 (Participants by Sex)##
         ##Female##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.3.2", "3.1.1.2.3.2", "3.1.1.3.3.2", "3.1.1.4.3.2"
                   , "3.1.1.5.3.2", "3.1.2.1.3.2", "3.1.2.2.3.2", "3.1.2.3.3.2"
                   , "3.1.2.4.3.2", "3.1.2.5.3.2", "3.1.3.1.3.2", "3.1.3.2.3.2"
                   , "3.1.3.3.3.2", "3.1.3.4.3.2", "3.1.3.5.3.2", "3.1.4.1.3.2"
                   , "3.1.4.2.3.2", "3.1.4.3.3.2", "3.1.4.4.3.2", "3.1.4.5.3.2"
                   , "3.1.5.1.3.2", "3.1.5.2.3.2", "3.1.5.3.3.2", "3.1.5.4.3.2"
                   , "3.1.5.5.3.2", "3.1.6.1.3.2", "3.1.6.2.3.2", "3.1.6.3.3.2"
                   , "3.1.6.4.3.2", "3.1.6.5.3.2")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.1.3.2", "3.2.1.2.3.2", "3.2.1.3.3.2")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.1.3.2", "3.2.2.2.3.2", "3.2.2.3.3.2")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.1.3.2", "3.2.3.2.3.2", "3.2.3.3.3.2")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.1.3.2", "3.2.4.2.3.2", "3.2.4.3.3.2")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.4.3", "3.3.2.4.3", "3.3.3.4.3", "3.3.4.4.3"
                   , "3.3.5.4.3")
         , name = "Sex"
         ##Male##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.3.4", "3.1.1.2.3.4", "3.1.1.3.3.4", "3.1.1.4.3.4"
                   , "3.1.1.5.3.4", "3.1.2.1.3.4", "3.1.2.2.3.4", "3.1.2.3.3.4"
                   , "3.1.2.4.3.4", "3.1.2.5.3.4", "3.1.3.1.3.4", "3.1.3.2.3.4"
                   , "3.1.3.3.3.4", "3.1.3.4.3.4", "3.1.3.5.3.4", "3.1.4.1.3.4"
                   , "3.1.4.2.3.4", "3.1.4.3.3.4", "3.1.4.4.3.4", "3.1.4.5.3.4"
                   , "3.1.5.1.3.4", "3.1.5.2.3.4", "3.1.5.3.3.4", "3.1.5.4.3.4"
                   , "3.1.5.5.3.4", "3.1.6.1.3.4", "3.1.6.2.3.4", "3.1.6.3.3.4"
                   , "3.1.6.4.3.4", "3.1.6.5.3.4")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.1.3.4", "3.2.1.2.3.4", "3.2.1.3.3.4")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.1.3.4", "3.2.2.2.3.4", "3.2.2.3.3.4")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.1.3.4", "3.2.3.2.3.4", "3.2.3.3.3.4")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.1.3.4", "3.2.4.2.3.4", "3.2.4.3.3.4")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.4.6", "3.3.2.4.6", "3.3.3.4.6", "3.3.4.4.6"
                   , "3.3.5.4.6")
         , name = "Sex"
         ##Mixed##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.3.6", "3.1.1.2.3.6", "3.1.1.3.3.6", "3.1.1.4.3.6"
                   , "3.1.1.5.3.6", "3.1.2.1.3.6", "3.1.2.2.3.6", "3.1.2.3.3.6"
                   , "3.1.2.4.3.6", "3.1.2.5.3.6", "3.1.3.1.3.6", "3.1.3.2.3.6"
                   , "3.1.3.3.3.6", "3.1.3.4.3.6", "3.1.3.5.3.6", "3.1.4.1.3.6"
                   , "3.1.4.2.3.6", "3.1.4.3.3.6", "3.1.4.4.3.6", "3.1.4.5.3.6"
                   , "3.1.5.1.3.6", "3.1.5.2.3.6", "3.1.5.3.3.6", "3.1.5.4.3.6"
                   , "3.1.5.5.3.6", "3.1.6.1.3.6", "3.1.6.2.3.6", "3.1.6.3.3.6"
                   , "3.1.6.4.3.6", "3.1.6.5.3.6")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.1.3.6", "3.2.1.2.3.6", "3.2.1.3.3.6")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.1.3.6", "3.2.2.2.3.6", "3.2.2.3.3.6")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.1.3.6", "3.2.3.2.3.6", "3.2.3.3.3.6")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.1.3.6", "3.2.4.2.3.6", "3.2.4.3.3.6")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.4.9", "3.3.2.4.9", "3.3.3.4.9", "3.3.4.4.9"
                   , "3.3.5.4.9")
         , name = "Sex"
         ##DNA##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.3.8", "3.1.1.2.3.8", "3.1.1.3.3.8", "3.1.1.4.3.8"
                   , "3.1.1.5.3.8", "3.1.2.1.3.8", "3.1.2.2.3.8", "3.1.2.3.3.8"
                   , "3.1.2.4.3.8", "3.1.2.5.3.8", "3.1.3.1.3.8", "3.1.3.2.3.8"
                   , "3.1.3.3.3.8", "3.1.3.4.3.8", "3.1.3.5.3.8", "3.1.4.1.3.8"
                   , "3.1.4.2.3.8", "3.1.4.3.3.8", "3.1.4.4.3.8", "3.1.4.5.3.8"
                   , "3.1.5.1.3.8", "3.1.5.2.3.8", "3.1.5.3.3.8", "3.1.5.4.3.8"
                   , "3.1.5.5.3.8", "3.1.6.1.3.8", "3.1.6.2.3.8", "3.1.6.3.3.8"
                   , "3.1.6.4.3.8", "3.1.6.5.3.8")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.1.3.8", "3.2.1.2.3.8", "3.2.1.3.3.8")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.1.3.8", "3.2.2.2.3.8", "3.2.2.3.3.8")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.1.3.8", "3.2.3.2.3.8", "3.2.3.3.3.8")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.1.3.8", "3.2.4.2.3.8", "3.2.4.3.3.8")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.4.12", "3.3.2.4.12", "3.3.3.4.12", "3.3.4.4.12"
                   , "3.3.5.4.12")
         , name = "Sex"
         ##EG.3.2-26 (USD by Age)##
         ##15-29 Years##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.4.1", "3.1.1.2.4.1", "3.1.1.3.4.1", "3.1.1.4.4.1"
                   , "3.1.1.5.4.1", "3.1.2.1.4.1", "3.1.2.2.4.1", "3.1.2.3.4.1"
                   , "3.1.2.4.4.1", "3.1.2.5.4.1", "3.1.3.1.4.1", "3.1.3.2.4.1"
                   , "3.1.3.3.4.1", "3.1.3.4.4.1", "3.1.3.5.4.1", "3.1.4.1.4.1"
                   , "3.1.4.2.4.1", "3.1.4.3.4.1", "3.1.4.4.4.1", "3.1.4.5.4.1"
                   , "3.1.5.1.4.1", "3.1.5.2.4.1", "3.1.5.3.4.1", "3.1.5.4.4.1"
                   , "3.1.5.5.4.1", "3.1.6.1.4.1", "3.1.6.2.4.1", "3.1.6.3.4.1"
                   , "3.1.6.4.4.1", "3.1.6.5.4.1")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.1.4.1", "3.2.1.2.4.1", "3.2.1.3.4.1")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.1.4.1", "3.2.2.2.4.1", "3.2.2.3.4.1")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.1.4.1", "3.2.3.2.4.1", "3.2.3.3.4.1")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.1.4.1", "3.2.4.2.4.1", "3.2.4.3.4.1")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.5.1", "3.3.2.5.1", "3.3.3.5.1", "3.3.4.5.1"
                   , "3.3.5.5.1")
         , name = "Age"
         ##30+ Years##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.4.3", "3.1.1.2.4.3", "3.1.1.3.4.3", "3.1.1.4.4.3"
                   , "3.1.1.5.4.3", "3.1.2.1.4.3", "3.1.2.2.4.3", "3.1.2.3.4.3"
                   , "3.1.2.4.4.3", "3.1.2.5.4.3", "3.1.3.1.4.3", "3.1.3.2.4.3"
                   , "3.1.3.3.4.3", "3.1.3.4.4.3", "3.1.3.5.4.3", "3.1.4.1.4.3"
                   , "3.1.4.2.4.3", "3.1.4.3.4.3", "3.1.4.4.4.3", "3.1.4.5.4.3"
                   , "3.1.5.1.4.3", "3.1.5.2.4.3", "3.1.5.3.4.3", "3.1.5.4.4.3"
                   , "3.1.5.5.4.3", "3.1.6.1.4.3", "3.1.6.2.4.3", "3.1.6.3.4.3"
                   , "3.1.6.4.4.3", "3.1.6.5.4.3")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.1.4.3", "3.2.1.2.4.3", "3.2.1.3.4.3")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.1.4.3", "3.2.2.2.4.3", "3.2.2.3.4.3")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.1.4.3", "3.2.3.2.4.3", "3.2.3.3.4.3")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.1.4.3", "3.2.4.2.4.3", "3.2.4.3.4.3")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.5.4", "3.3.2.5.4", "3.3.3.5.4", "3.3.4.5.4"
                   , "3.3.5.5.4")
         , name = "Age"
         ##Mixed Ages##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.4.5", "3.1.1.2.4.5", "3.1.1.3.4.5", "3.1.1.4.4.5"
                   , "3.1.1.5.4.5", "3.1.2.1.4.5", "3.1.2.2.4.5", "3.1.2.3.4.5"
                   , "3.1.2.4.4.5", "3.1.2.5.4.5", "3.1.3.1.4.5", "3.1.3.2.4.5"
                   , "3.1.3.3.4.5", "3.1.3.4.4.5", "3.1.3.5.4.5", "3.1.4.1.4.5"
                   , "3.1.4.2.4.5", "3.1.4.3.4.5", "3.1.4.4.4.5", "3.1.4.5.4.5"
                   , "3.1.5.1.4.5", "3.1.5.2.4.5", "3.1.5.3.4.5", "3.1.5.4.4.5"
                   , "3.1.5.5.4.5", "3.1.6.1.4.5", "3.1.6.2.4.5", "3.1.6.3.4.5"
                   , "3.1.6.4.4.5", "3.1.6.5.4.5")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.1.4.5", "3.2.1.2.4.5", "3.2.1.3.4.5")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.1.4.5", "3.2.2.2.4.5", "3.2.2.3.4.5")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.1.4.5", "3.2.3.2.4.5", "3.2.3.3.4.5")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.1.4.5", "3.2.4.2.4.5", "3.2.4.3.4.5")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.5.7", "3.3.2.5.7", "3.3.3.5.7", "3.3.4.5.7"
                   , "3.3.5.5.7")
         , name = "Age"
         ##DNA##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.4.7", "3.1.1.2.4.7", "3.1.1.3.4.7", "3.1.1.4.4.7"
                   , "3.1.1.5.4.7", "3.1.2.1.4.7", "3.1.2.2.4.7", "3.1.2.3.4.7"
                   , "3.1.2.4.4.7", "3.1.2.5.4.7", "3.1.3.1.4.7", "3.1.3.2.4.7"
                   , "3.1.3.3.4.7", "3.1.3.4.4.7", "3.1.3.5.4.7", "3.1.4.1.4.7"
                   , "3.1.4.2.4.7", "3.1.4.3.4.7", "3.1.4.4.4.7", "3.1.4.5.4.7"
                   , "3.1.5.1.4.7", "3.1.5.2.4.7", "3.1.5.3.4.7", "3.1.5.4.4.7"
                   , "3.1.5.5.4.7", "3.1.6.1.4.7", "3.1.6.2.4.7", "3.1.6.3.4.7"
                   , "3.1.6.4.4.7", "3.1.6.5.4.7")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.1.4.7", "3.2.1.2.4.7", "3.2.1.3.4.7")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.1.4.7", "3.2.2.2.4.7", "3.2.2.3.4.7")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.1.4.7", "3.2.3.2.4.7", "3.2.3.3.4.7")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.1.4.7", "3.2.4.2.4.7", "3.2.4.3.4.7")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.5.10", "3.3.2.5.10", "3.3.3.5.10", "3.3.4.5.10"
                   , "3.3.5.5.10")
         , name = "Age"
         ##EG.3.2-26 (USD by Age)##
         ##15-29 Years##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.5.2", "3.3.2.5.2", "3.3.3.5.2", "3.3.4.5.2"
                   , "3.3.5.5.2")
         , name = "Age"
         ##30+ Years##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.5.5", "3.3.2.5.5", "3.3.3.5.5", "3.3.4.5.5"
                   , "3.3.5.5.5")
         , name = "Age"
         ##Mixed Ages##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.5.8", "3.3.2.5.8", "3.3.3.5.8", "3.3.4.5.8"
                   , "3.3.5.5.8")
         , name = "Age"
         ##DNA##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.5.11", "3.3.2.5.11", "3.3.3.5.11", "3.3.4.5.11"
                   , "3.3.5.5.11")
         , name = "Age"
         ##EG.3.2-26 (Participants by Age)##
         ##15-29 Years##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.4.2", "3.1.1.2.4.2", "3.1.1.3.4.2", "3.1.1.4.4.2"
                   , "3.1.1.5.4.2", "3.1.2.1.4.2", "3.1.2.2.4.2", "3.1.2.3.4.2"
                   , "3.1.2.4.4.2", "3.1.2.5.4.2", "3.1.3.1.4.2", "3.1.3.2.4.2"
                   , "3.1.3.3.4.2", "3.1.3.4.4.2", "3.1.3.5.4.2", "3.1.4.1.4.2"
                   , "3.1.4.2.4.2", "3.1.4.3.4.2", "3.1.4.4.4.2", "3.1.4.5.4.2"
                   , "3.1.5.1.4.2", "3.1.5.2.4.2", "3.1.5.3.4.2", "3.1.5.4.4.2"
                   , "3.1.5.5.4.2", "3.1.6.1.4.2", "3.1.6.2.4.2", "3.1.6.3.4.2"
                   , "3.1.6.4.4.2", "3.1.6.5.4.2")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.1.4.2", "3.2.1.2.4.2", "3.2.1.3.4.2")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.1.4.2", "3.2.2.2.4.2", "3.2.2.3.4.2")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.1.4.2", "3.2.3.2.4.2", "3.2.3.3.4.2")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.1.4.2", "3.2.4.2.4.2", "3.2.4.3.4.2")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.5.3", "3.3.2.5.3", "3.3.3.5.3", "3.3.4.5.3"
                   , "3.3.5.5.3")
         , name = "Age"
         ##30+ Years##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.4.4", "3.1.1.2.4.4", "3.1.1.3.4.4", "3.1.1.4.4.4"
                   , "3.1.1.5.4.4", "3.1.2.1.4.4", "3.1.2.2.4.4", "3.1.2.3.4.4"
                   , "3.1.2.4.4.4", "3.1.2.5.4.4", "3.1.3.1.4.4", "3.1.3.2.4.4"
                   , "3.1.3.3.4.4", "3.1.3.4.4.4", "3.1.3.5.4.4", "3.1.4.1.4.4"
                   , "3.1.4.2.4.4", "3.1.4.3.4.4", "3.1.4.4.4.4", "3.1.4.5.4.4"
                   , "3.1.5.1.4.4", "3.1.5.2.4.4", "3.1.5.3.4.4", "3.1.5.4.4.4"
                   , "3.1.5.5.4.4", "3.1.6.1.4.4", "3.1.6.2.4.4", "3.1.6.3.4.4"
                   , "3.1.6.4.4.4", "3.1.6.5.4.4")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.1.4.4", "3.2.1.2.4.4", "3.2.1.3.4.4")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.1.4.4", "3.2.2.2.4.4", "3.2.2.3.4.4")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.1.4.4", "3.2.3.2.4.4", "3.2.3.3.4.4")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.1.4.4", "3.2.4.2.4.4", "3.2.4.3.4.4")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.5.6", "3.3.2.5.6", "3.3.3.5.6", "3.3.4.5.6"
                   , "3.3.5.5.6")
         , name = "Age"
         ##Mixed Ages##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.4.6", "3.1.1.2.4.6", "3.1.1.3.4.6", "3.1.1.4.4.6"
                   , "3.1.1.5.4.6", "3.1.2.1.4.6", "3.1.2.2.4.6", "3.1.2.3.4.6"
                   , "3.1.2.4.4.6", "3.1.2.5.4.6", "3.1.3.1.4.6", "3.1.3.2.4.6"
                   , "3.1.3.3.4.6", "3.1.3.4.4.6", "3.1.3.5.4.6", "3.1.4.1.4.6"
                   , "3.1.4.2.4.6", "3.1.4.3.4.6", "3.1.4.4.4.6", "3.1.4.5.4.6"
                   , "3.1.5.1.4.6", "3.1.5.2.4.6", "3.1.5.3.4.6", "3.1.5.4.4.6"
                   , "3.1.5.5.4.6", "3.1.6.1.4.6", "3.1.6.2.4.6", "3.1.6.3.4.6"
                   , "3.1.6.4.4.6", "3.1.6.5.4.6")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.1.4.6", "3.2.1.2.4.6", "3.2.1.3.4.6")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.1.4.6", "3.2.2.2.4.6", "3.2.2.3.4.6")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.1.4.6", "3.2.3.2.4.6", "3.2.3.3.4.6")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.1.4.6", "3.2.4.2.4.6", "3.2.4.3.4.6")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.5.9", "3.3.2.5.9", "3.3.3.5.9", "3.3.4.5.9"
                   , "3.3.5.5.9")
         , name = "Age"
         ##DNA##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.4.8", "3.1.1.2.4.8", "3.1.1.3.4.8", "3.1.1.4.4.8"
                   , "3.1.1.5.4.8", "3.1.2.1.4.8", "3.1.2.2.4.8", "3.1.2.3.4.8"
                   , "3.1.2.4.4.8", "3.1.2.5.4.8", "3.1.3.1.4.8", "3.1.3.2.4.8"
                   , "3.1.3.3.4.8", "3.1.3.4.4.8", "3.1.3.5.4.8", "3.1.4.1.4.8"
                   , "3.1.4.2.4.8", "3.1.4.3.4.8", "3.1.4.4.4.8", "3.1.4.5.4.8"
                   , "3.1.5.1.4.8", "3.1.5.2.4.8", "3.1.5.3.4.8", "3.1.5.4.4.8"
                   , "3.1.5.5.4.8", "3.1.6.1.4.8", "3.1.6.2.4.8", "3.1.6.3.4.8"
                   , "3.1.6.4.4.8", "3.1.6.5.4.8")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.1.4.8", "3.2.1.2.4.8", "3.2.1.3.4.8")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.1.4.8", "3.2.2.2.4.8", "3.2.2.3.4.8")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.1.4.8", "3.2.3.2.4.8", "3.2.3.3.4.8")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.1.4.8", "3.2.4.2.4.8", "3.2.4.3.4.8")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.5.12", "3.3.2.5.12", "3.3.3.5.12", "3.3.4.5.12"
                   , "3.3.5.5.12")
         , name = "Age"
         ##FTFMS##
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d3 = "Sex of Producer"
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d3 = "Sex of proprietor(s)"
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d3 = "Age of Producer"
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d3 = "Age of proprietor(s)"
         , name = "Age"
         ##EG.3.2-27 (Value Sex)##
         ##Individuals / Microenterprises##
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.1.1.1", "3.2.1.1", "3.3.1.1", "3.4.1.1")
         , name = "Individuals / Microenterprises"
  ),
  tibble(ic = "EG.3.2-27"
         , ms_d3 = "Individuals / Microenterprises"
         , name = "Individuals / Microenterprises"
         ##Small and Medium Enterprises##
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.1.1.2", "3.2.1.2", "3.3.1.2", "3.4.1.2")
         , name = "Small and Medium Enterprises"
  ),
  tibble(ic = "EG.3.2-27"
         , ms_d3 = "Small and Medium Enterprises"
         , name = "Small and Medium Enterprises"
         ##Large Enterprises and Corporations##
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.1.1.3", "3.2.1.3", "3.3.1.3", "3.4.1.3")
         , name = "Large Enterprises and Corporations"
  ),
  tibble(ic = "EG.3.2-27"
         , ms_d3 = "Large Enterprises and Corporations"
         , name = "Large Enterprises and Corporations"
         ##Disaggregates Not Available##
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.1.1.4", "3.2.1.4", "3.3.1.4", "3.4.1.4")
         , name = "Disaggregates Not Available"
  ),
  tibble(ic = "EG.3.2-27"
         , ms_d3 = "Disaggregates Not Available"
         , name = "Disaggregates Not Available"
         ##Male##
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.1.2.1", "3.2.2.1", "3.3.2.1", "3.4.2.1")
         , name = "Male"
  ),
  tibble(ic = "EG.3.2-27"
         , ms_d3 = "Male"
         , name = "Male"
         ##Female##
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.1.2.2", "3.2.2.2", "3.3.2.2", "3.4.2.2")
         , name = "Female"
  ),
  tibble(ic = "EG.3.2-27"
         , ms_d3 = "Female"
         , name = "Female"
         ##Mixed (for enterprises)##
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.1.2.3", "3.2.2.3", "3.3.2.3", "3.4.2.3")
         , name = "Mixed (for enterprises)"
  ),
  tibble(ic = "EG.3.2-27"
         , ms_d3 = "Mixed (for enterprises)"
         , name = "Mixed (for enterprises)"
         ##Disaggregates Not Available##
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.1.2.4", "3.2.2.4", "3.3.2.4", "3.4.2.4")
         , name = "Disaggregates Not Available"
  ),
  tibble(ic = "EG.3.2-27"
         , ms_d3 = "Disaggregates Not Available"
         , name = "Disaggregates Not Available"
         ##15-29##
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.1.3.1", "3.2.3.1", "3.3.3.1", "3.4.3.1")
         , name = "15-29"
  ),
  tibble(ic = "EG.3.2-27"
         , ms_d3 = "15-29"
         , name = "15-29"
         ##30+##
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.1.3.2", "3.2.3.2", "3.3.3.2", "3.4.3.2")
         , name = "30+"
  ),
  tibble(ic = "EG.3.2-27"
         , ms_d3 = "30+"
         , name = "30+"
         ##Mixed Ages (for enterprises)##
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.1.3.3", "3.2.3.3", "3.3.3.3", "3.4.3.3")
         , name = "Mixed Ages (for enterprises)"
  ),
  tibble(ic = "EG.3.2-27"
         , ms_d3 = "Mixed Ages (for enterprises)"
         , name = "Mixed Ages (for enterprises)"
         ##Disaggregates Not Available##
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.1.3.4", "3.2.3.4", "3.3.3.4", "3.4.3.4")
         , name = "Disaggregates Not Available"
  ),
  tibble(ic = "EG.3.2-27"
         , ms_d3 = "Disaggregates Not Available"
         , name = "Disaggregates Not Available"
         ##Size of recipient(s)##
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.5.1.1.1", "3.5.1.1.2", "3.5.1.1.3", "3.5.1.1.4")
         , name = "Size of recipient(s)"
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.5.2.1.1", "3.5.2.1.2", "3.5.2.1.3", "3.5.2.1.4")
         , name = "Size of recipient(s)"
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.6.1.1.1", "3.6.1.1.2", "3.6.1.1.3", "3.6.1.1.4")
         , name = "Size of recipient(s)"
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.6.2.1.1", "3.6.2.1.2", "3.6.2.1.3", "3.6.2.1.4")
         , name = "Size of recipient(s)"
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.7.1.1.1", "3.7.1.1.2", "3.7.1.1.3", "3.7.1.1.4")
         , name = "Size of recipient(s)"
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.7.2.1.1", "3.7.2.1.2", "3.7.2.1.3", "3.7.2.1.4")
         , name = "Size of recipient(s)"
  ),
  tibble(ic = "EG.3.2-27"
         , ms_d3 = "Size of recipient(s)"
         , name = "Size of recipient(s)"
         ##Sex of recipient(s)##
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.5.1.2.1", "3.5.1.2.2", "3.5.1.2.3", "3.5.1.2.4")
         , name = "Sex of recipient(s)"
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.5.2.2.1", "3.5.2.2.2", "3.5.2.2.3", "3.5.2.2.4")
         , name = "Sex of recipient(s)"
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.6.1.2.1", "3.6.1.2.2", "3.6.1.2.3", "3.6.1.2.4")
         , name = "Sex of recipient(s)"
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.6.2.2.1", "3.6.2.2.2", "3.6.2.2.3", "3.6.2.2.4")
         , name = "Sex of recipient(s)"
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.7.1.2.1", "3.7.1.2.2", "3.7.1.2.3", "3.7.1.2.4")
         , name = "Sex of recipient(s)"
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.7.2.2.1", "3.7.2.2.2", "3.7.2.2.3", "3.7.2.2.4")
         , name = "Sex of recipient(s)"
  ),
  tibble(ic = "EG.3.2-27"
         , ms_d3 = "Sex of recipient(s)"
         , name = "Sex of recipient(s)"
         ##Age of recipient(s)##
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.5.1.3.1", "3.5.1.3.2", "3.5.1.3.3", "3.5.1.3.4")
         , name = "Age of recipient(s)"
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.5.2.3.1", "3.5.2.3.2", "3.5.2.3.3", "3.5.2.3.4")
         , name = "Age of recipient(s)"
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.6.1.3.1", "3.6.1.3.2", "3.6.1.3.3", "3.6.1.3.4")
         , name = "Age of recipient(s)"
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.6.2.3.1", "3.6.2.3.2", "3.6.2.3.3", "3.6.2.3.4")
         , name = "Age of recipient(s)"
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.7.1.3.1", "3.7.1.3.2", "3.7.1.3.3", "3.7.1.3.4")
         , name = "Age of recipient(s)"
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.7.2.3.1", "3.7.2.3.2", "3.7.2.3.3", "3.7.2.3.4")
         , name = "Age of recipient(s)"
  ),
  tibble(ic = "EG.3.2-27"
         , ms_d3 = "Age of recipient(s)"
         , name = "Age of recipient(s)"
         ##EG.3.2-x17 (Producers)##
  ),
  tibble(ic = "EG.3.2-x17", ms_d2 = c("Sex", "Commodity", "Duration")
         , name = "ms_d3"
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d3 = "animal genetics"
         , name = "Animal Genetics"
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d3 = "climate adaptation"
         , name = "Climate Adaptation"
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d3 = "climate mitigation"
         , name = "Climate Mitigation"
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d3 = "climate mitigation or adaptation"
         , name = "Climate Mitigation or Adaptation"
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d3 = "crop genetics"
         , name = "Crop Genetics"
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d3 = "cultural practices"
         , name = "Cultural Practices"
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d3 = "disease management"
         , name = "Disease Management"
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d3 = "wild fishing technique/gear"
         , name = "Wild Fishing Gear/Technique"
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d3 = "irrigation"
         , name = "Irrigation"
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d3 = "other"
         , name = "Other"
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d3 = "pest management"
         , name = "Pest Management"
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d3 = "post-harvest - handling and storage"
         , name = "Post-Harvest Handling and Storage"
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d3 = "processing"
         , name = "Processing"
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d3 = "soil-related fertility and conservation"
         , name = "Soil-Related Fertility and Conservation"
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d3 = "total w/one or more improved technology"
         , name = "Total With One or More Improved Technology"
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d3 = "water management (non-irrigation)"
         , name = "Water Management (Non-Irrigation)"
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d3 = "Association-applied"
         , name = "Association-Applied"

  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d3 = "aquaculture management"
         , name = "Aquaculture Management"
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d3 = "Livestock management"
         , name = "Livestock Management"
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d3 = "marketing and distribution"
         , name = "Marketing and Distribution"
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d3 = "value-added processing"
         , name = "Value-Added Processing"
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d3 = "Disaggregates Not Available"
         , name = "Disaggregates Not Available"
         ##EG.3.2-x18 (Hectares)##
  ),
  tibble(ic = "EG.3.2-x18", ms_d2 = c("animal genetics")
         , name = "Animal Genetics"
  ),
  tibble(ic = "EG.3.2-x18", ms_d2 ="climate adaptation"
         , name = "Climate Adaptation"
  ),
  tibble(ic = "EG.3.2-x18", ms_d2 ="climate mitigation"
         , name = "Climate Mitigation"
  ),
  tibble(ic = "EG.3.2-x18", ms_d2 ="climate mitigation or adaptation"
         , name = "Climate Mitigation or Adaptation"
  ),
  tibble(ic = "EG.3.2-x18", ms_d2 ="crop genetics"
         , name = "Crop Genetics"
  ),
  tibble(ic = "EG.3.2-x18", ms_d2 ="cultural practices"
         , name = "Cultural Practices"
  ),
  tibble(ic = "EG.3.2-x18", ms_d2 ="disease management"
         , name = "Disease Management"
  ),
  tibble(ic = "EG.3.2-x18", ms_d2 ="fishing gear/technique"
         , name = "Fishing Gear/Technique"
  ),
  tibble(ic = "EG.3.2-x18", ms_d2 ="irrigation"
         , name = "Irrigation"
  ),
  tibble(ic = "EG.3.2-x18", ms_d2 ="other"
         , name = "Other"
  ),
  tibble(ic = "EG.3.2-x18", ms_d2 ="pest management"
         , name = "Pest Management"
  ),
  tibble(ic = "EG.3.2-x18", ms_d2 ="post-harvest handling and storage"
         , name = "Post-Harvest Handling and Storage"
  ),
  tibble(ic = "EG.3.2-x18", ms_d2 ="processing"
         , name = "Processing"
  ),
  tibble(ic = "EG.3.2-x18", ms_d2 ="soil-related fertility and conservation"
         , name = "Soil-Related Fertility and Conservation"
  ),
  tibble(ic = "EG.3.2-x18", ms_d2 ="total w/one or more improved technology"
         , name = "Total With One or More Improved Technology"
  ),
  tibble(ic = "EG.3.2-x18", ms_d2 ="water management (non-irrigation)"
         , name = "Water Management (Non-Irrigation)"
  ),
  tibble(ic = "EG.3.2-x18", ms_d2 ="Association-applied"
         , name = "Association-Applied"
  ),
  tibble(ic = "EG.3.2-x18"
         , name = "ms_d2"
         ##EG.3.2-x19 (Sales)##
  ),
  tibble(ic = "EG.3.2-x19"
         , name = "No Age/Sex Information Collected"
         ##EG.10.4-7 (Resource Type)##
         ##Sex##
  ),
  tibble(ic = "EG.10.4-7"
         , udn = c("3.1.1.2", "3.2.1.2")
         , name = "Female"
  ),
  tibble(ic = "EG.10.4-7"
         , ms_d3 = "Female"
         , name = "Female"
  ),
  tibble(ic = "EG.10.4-7"
         , udn = c("3.1.1.1", "3.2.1.1")
         , name = "Male"
  ),
  tibble(ic = "EG.10.4-7"
         , ms_d3 = "Male"
         , name = "Male"
  ),
  tibble(ic = "EG.10.4-7"
         , udn = c("3.1.1.3", "3.2.1.3")
         , name = "Disaggregates Not Available"
  ),
  tibble(ic = "EG.10.4-7"
         , ms_d3 = "Disaggregates Not Available"
         , name = "Disaggregates Not Available"
         ##Type of Documentation##
  ),
  tibble(ic = "EG.10.4-7"
         , udn = c("3.1.2.1", "3.2.2.1")
         , name = "Individual/Household"
  ),
  tibble(ic = "EG.10.4-7"
         , ms_d3 = "Individual/Household"
         , name = "Individual/Household"
  ),
  tibble(ic = "EG.10.4-7"
         , udn = c("3.1.2.2", "3.2.2.2")
         , name = "Community/Group"
  ),
  tibble(ic = "EG.10.4-7"
         , ms_d3 = "Community/Group"
         , name = "Community/Group"
  ),
  tibble(ic = "EG.10.4-7"
         , udn = c("3.1.2.3", "3.2.2.3")
         , name = "Business/Commercial"
  ),
  tibble(ic = "EG.10.4-7"
         , ms_d3 = "Business/Commercial"
         , name = "Business/Commercial"
  ),
  tibble(ic = "EG.10.4-7"
         , udn = c("3.1.2.4", "3.2.2.4")
         , name = "Other Legal Entity (e.g. Churches, NGOs)"
  ),
  tibble(ic = "EG.10.4-7"
         , ms_d3 = "Other legal entity (e.g. churches, NGOs)"
         , name = "Other Legal Entity (e.g. Churches, NGOs)"
  ),
  tibble(ic = "EG.10.4-7"
         , udn = c("3.1.2.5", "3.2.2.5")
         , name = "Disaggregates Not Available"
  ),
  tibble(ic = "EG.10.4-7"
         , ms_d3 = "Disaggregates Not Available"
         , name = "Disaggregates Not Available"
         ##Location##
  ),
  tibble(ic = "EG.10.4-7"
         , udn ="3.1.3.1"
         , name = "Rural"
  ),
  tibble(ic = "EG.10.4-7"
         , ms_d3 = "Rural"
         , name = "Rural"
  ),
  tibble(ic = "EG.10.4-7"
         , udn ="3.2.3.1"
         , name = "Marine Water"
  ),
  tibble(ic = "EG.10.4-7"
         , ms_d3 = "Marine water"
         , name = "Marine Water"
  ),
  tibble(ic = "EG.10.4-7"
         , udn ="3.1.3.2"
         , name = "Urban"
  ),
  tibble(ic = "EG.10.4-7"
         , ms_d3 = "Urban"
         , name = "Urban"
  ),
  tibble(ic = "EG.10.4-7"
         , udn ="3.2.3.2"
         , name = "Freshwater"
  ),
  tibble(ic = "EG.10.4-7"
         , ms_d3 = "Freshwater"
         , name = "Freshwater"
  ),
  tibble(ic = "EG.10.4-7"
         , udn = c("3.1.3.3", "3.2.3.3")
         , name = "Disaggregates Not Available"
  ),
  tibble(ic = "EG.10.4-7"
         , ms_d3 = "Disaggregates Not Available"
         , name = "Disaggregates Not Available"
         ##EG.10.4-8 (Participants by Resource Type)##
         ##Sex##
  ),
  tibble(ic = "EG.10.4-8"
         , udn = c("3.1.1.1", "3.2.1.1")
         , name = "Male"
  ),
  tibble(ic = "EG.10.4-8"
         , ms_d3 = "Male"
         , name = "Male"
  ),
  tibble(ic = "EG.10.4-8"
         , udn = c("3.1.1.2", "3.2.1.2")
         , name = "Female"
  ),
  tibble(ic = "EG.10.4-8"
         , ms_d3 = "Female"
         , name = "Female"
  ),
  tibble(ic = "EG.10.4-8"
         , udn = c("3.1.1.3", "3.2.1.3")
         , name = "Disaggregates Not Available"
  ),
  tibble(ic = "EG.10.4-8"
         , ms_d3 = "Disaggregates Not Available"
         , name = "Disaggregates Not Available"
         ##Tenure Type##
  ),
  tibble(ic = "EG.10.4-8"
         , udn = c("3.1.2.1", "3.2.2.1")
         , name = "Customary"
  ),
  tibble(ic = "EG.10.4-8"
         , ms_d3 = "Customary"
         , name = "Customary"
  ),
  tibble(ic = "EG.10.4-8"
         , udn = c("3.1.2.2", "3.2.2.2")
         , name = "Freehold"
  ),
  tibble(ic = "EG.10.4-8"
         , ms_d3 = "Freehold"
         , name = "Freehold"
  ),
  tibble(ic = "EG.10.4-8"
         , udn = c("3.1.2.3", "3.2.2.3")
         , name = "Leasehold"
  ),
  tibble(ic = "EG.10.4-8"
         , ms_d3 = "Leasehold"
         , name = "Leasehold"
  ),
  tibble(ic = "EG.10.4-8"
         , udn = c("3.1.2.4", "3.2.2.4")
         , name = "State"
  ),
  tibble(ic = "EG.10.4-8"
         , ms_d3 = "State"
         , name = "State"
  ),
  tibble(ic = "EG.10.4-8"
         , udn = c("3.1.2.5", "3.2.2.5")
         , name = "Community/Group Rights"
  ),
  tibble(ic = "EG.10.4-8"
         , ms_d3 = "Community/Group Rights"
         , name = "Community/Group Rights"
  ),
  tibble(ic = "EG.10.4-8"
         , udn = c("3.1.2.6", "3.2.2.6")
         , name = "Cooperatives"
  ),
  tibble(ic = "EG.10.4-8"
         , ms_d3 = "Cooperatives"
         , name = "Cooperatives"
  ),
  tibble(ic = "EG.10.4-8"
         , udn = c("3.1.2.7", "3.2.2.7")
         , name = "Other"
  ),
  tibble(ic = "EG.10.4-8"
         , ms_d3 = "Other (Specify prior to data collection and report in an indicator comment)"
         , name = "Other"
  ),
  tibble(ic = "EG.10.4-8"
         , udn = c("3.1.2.8", "3.2.2.8")
         , name = "Disaggregates Not Available"
  ),
  tibble(ic = "EG.10.4-8"
         , ms_d3 = "Disaggregates Not Available"
         , name = "Disaggregates Not Available"
         ##Location##
  ),
  tibble(ic = "EG.10.4-8"
         , udn ="3.1.3.1"
         , name = "Rural"
  ),
  tibble(ic = "EG.10.4-8"
         , ms_d3 = "Rural"
         , name = "Rural"
  ),
  tibble(ic = "EG.10.4-8"
         , udn ="3.2.3.1"
         , name = "Freshwater"
  ),
  tibble(ic = "EG.10.4-8"
         , ms_d3 = "Freshwater"
         , name = "Freshwater"
  ),
  tibble(ic = "EG.10.4-8"
         , udn ="3.1.3.2"
         , name = "Urban"
  ),
  tibble(ic = "EG.10.4-8"
         , ms_d3 = "Urban"
         , name = "Urban"
  ),
  tibble(ic = "EG.10.4-8"
         , udn ="3.2.3.2"
         , name = "Marine Water"
  ),
  tibble(ic = "EG.10.4-8"
         , ms_d3 = "Marine water"
         , name = "Marine Water"
  ),
  tibble(ic = "EG.10.4-8"
         , udn = c("3.1.3.3", "3.2.3.3")
         , name = "Disaggregates Not Available"
  ),
  tibble(ic = "EG.10.4-8"
         , ms_d3 = "Disaggregates Not Available"
         , name = "Disaggregates Not Available"
  )

)


# Second order ####
d2 <-   dplyr::bind_rows(
  tibble(ic = "CBLD-9"
         , udn = c("3.3.1.1", "3.3.2.1", "3.3.3.1", "3.3.4.1", "3.3.5.1", "3.3.6.1", "3.3.7.1", "3.3.8.1", "3.3.9.1", "3.3.10.1")
         , name = "Total Number of Organizations with Improved Performance"
  ),
  tibble(ic = "CBLD-9"
         , ms_d3 = "Numerator = number of organizations of this type with improved performance"
         , name = "Total Number of Organizations with Improved Performance"
         ##Total Number of USG-Assisted Organizations Receiveing Organizational Capacity Development Support##
  ),
  tibble(ic = "CBLD-9"
         , udn = c("3.3.1.2", "3.3.2.2", "3.3.3.2", "3.3.4.2", "3.3.5.2", "3.3.6.2", "3.3.7.2", "3.3.8.2", "3.3.9.2", "3.3.10.2")
         , name = "Total Number of USG-Assisted Organizations Receiveing Organizational Capacity Development Support"
  ),
  tibble(ic = "CBLD-9"
         , ms_d3 = "Denominator = number of USG-assisted organizations of this type receiving organizational capacity development support"
         , name = "Total Number of USG-Assisted Organizations Receiveing Organizational Capacity Development Support"
         ##EG-c (Percent of People)##
         ##Percent of people living on less than $1.90/day (all household types together)##
  ),
  tibble(ic = "EG-c", udn ="3.1.1"
         , name = "In All ZOIs/Areas Combined (%)"
  ),
  tibble(ic = "EG-c", udn ="3.1.2"
         , name = "In Target/Aligned Country ZOI (%)"
  ),
  tibble(ic = "EG-c", udn ="3.1.3"
         , name = "In FFP Development Program Area (%)"
  ),
  tibble(ic = "EG-c", udn ="3.1.4"
         , name = "In Resilience to Recurrent Crisis Area (%)"
         ##Number of people (all household types together)##
  ),
  tibble(ic = "EG-c", udn ="3.2.1"
         , name = "In All ZOIs/Areas Combined"
  ),
  tibble(ic = "EG-c", udn ="3.2.2"
         , name = "In Target/Aligned Country ZOI"
  ),
  tibble(ic = "EG-c", udn ="3.2.3"
         , name = "In FFP Development Program Area"
  ),
  tibble(ic = "EG-c", udn ="3.2.4"
         , name = "In Resilience to Recurrent Crisis Area"
         ##Household type: Male and Female Adults (M&F)##
         ##Percent of people living on less than $1.90/day in this gendered household type##
  ),
  tibble(ic = "EG-c"
         , udn = c("3.3.1.1", "3.3.1.2", "3.3.1.3", "3.3.1.4")
         , name = "Percent of People Living on Less than $1.90/Day in this Gendered Household Type"
         ##Number  of People in this Gendered Household Type##
  ),
  tibble(ic = "EG-c"
         , udn = c("3.3.2.1", "3.3.2.2", "3.3.2.3", "3.3.2.4")
         , name = "Number of People in this Gendered Household Type"
         ##Household type: Adult Female no Adult Male (FNM)##
         ##Percent of people living on less than $1.90/day in this gendered household type##
  ),
  tibble(ic = "EG-c"
         , udn = c("3.4.1.1", "3.4.1.2", "3.4.1.3", "3.4.1.4")
         , name = "Percent of People Living on Less than $1.90/Day in this Gendered Household Type"
         ##Number  of People in this Gendered Household Type##
  ),
  tibble(ic = "EG-c"
         , udn = c("3.4.2.1", "3.4.2.2", "3.4.2.3", "3.4.2.4")
         , name = "Number of People in this Gendered Household Type"
         ##Household type: Adult Male no Adult Female (MNF)##
         ##Percent of people living on less than $1.90/day in this gendered household type##
  ),
  tibble(ic = "EG-c"
         , udn = c("3.5.1.1", "3.5.1.2", "3.5.1.3", "3.5.1.4")
         , name = "Percent of People Living on Less than $1.90/Day in this Gendered Household Type"
         ##Number  of People in this Gendered Household Type##
  ),
  tibble(ic = "EG-c"
         , udn = c("3.5.2.1", "3.5.2.2", "3.5.2.3", "3.5.2.4")
         , name = "Number of People in this Gendered Household Type"
         ##Household type: Child No Adults (CNA)##
         ##Percent of people living on less than $1.90/day in this gendered household type##
  ),
  tibble(ic = "EG-c"
         , udn = c("3.6.1.1", "3.6.1.2", "3.6.1.3", "3.6.1.4")
         , name = "Percent of People Living on Less than $1.90/Day in this Gendered Household Type"
         ##Number  of People in this Gendered Household Type##
  ),
  tibble(ic = "EG-c"
         , udn = c("3.6.2.1", "3.6.2.2", "3.6.2.3", "3.6.2.4")
         , name = "Number of People in this Gendered Household Type"
         ##Household type: Disaggregates Not Available##
         ##Percent of people living on less than $1.90/day in this gendered household type##
  ),
  tibble(ic = "EG-c"
         , udn = c("3.7.1.1", "3.7.1.2", "3.7.1.3", "3.7.1.4")
         , name = "Percent of People Living on Less than $1.90/Day in this Gendered Household Type"
         ##Number  of People in this Gendered Household Type##
  ),
  tibble(ic = "EG-c"
         , udn = c("3.7.2.1", "3.7.2.2", "3.7.2.3", "3.7.2.4")
         , name = "Number of People in this Gendered Household Type"
         ##EG-d (Percent of People)##
         ##Household type: Male and Female Adults (M&F)##
         ##Percent of people living on less than $1.90/day in this gendered household type##
  ),
  tibble(ic = "EG-d", udn ="3.2.1.1"
         , name = "Percent of People Living on Less than $1.90/Day in this Gendered Household Type"
         ##Number of people in this gendered household type##
  ),
  tibble(ic = "EG-d", udn ="3.2.1.2"
         , name = "Number of People in this Gendered Household Type"
         ##Household type: Adult Female no Adult Male (FNM)##
         ##Percent of people living on less than $1.90/day in this gendered household type##
  ),
  tibble(ic = "EG-d", udn ="3.2.2.1"
         , name = "Percent of People Living on Less than $1.90/Day in this Gendered Household Type"
         ##Number of people in this gendered household type##
  ),
  tibble(ic = "EG-d", udn ="3.2.2.2"
         , name = "Number of People in this Gendered Household Type"
         ##Household type: Adult Male no Adult Female (MNF)##
         ##Percent of people living on less than $1.90/day in this gendered household type##
  ),
  tibble(ic = "EG-d", udn ="3.2.3.1"
         , name = "Percent of People Living on Less than $1.90/Day in this Gendered Household Type"
         ##Number of people in this gendered household type##
  ),
  tibble(ic = "EG-d", udn ="3.2.3.2"
         , name = "Number of People in this Gendered Household Type"
         ##Household type: Child No Adults (CNA)##
         ##Percent of people living on less than $1.90/day in this gendered household type##
  ),
  tibble(ic = "EG-d", udn ="3.2.4.1"
         , name = "Percent of People Living on Less than $1.90/Day in this Gendered Household Type"
         ##Number of people in this gendered household type##
  ),
  tibble(ic = "EG-d", udn ="3.2.4.2"
         , name = "Number of People in this Gendered Household Type"
         ##Household type: Disaggregates Not Available##
         ##Percent of people living on less than $1.90/day in this gendered household type##
  ),
  tibble(ic = "EG-d", udn ="3.2.5.1"
         , name = "Percent of People Living on Less than $1.90/Day in this Gendered Household Type"
         ##Number of people in this gendered household type##
  ),
  tibble(ic = "EG-d", udn ="3.2.5.2"
         , name = "Number of People in this Gendered Household Type"
         ##EG-e (Prevalence)##
         ##In All ZOIs/Areas Combined (%)##
  ),
  tibble(ic = "EG-e"
         , udn = c("3.1.1", "3.2.1", "3.3.1", "3.5.1.1", "3.6.1.1", "3.7.1.1", "3.8.1.1", "3.9.1.1")
         , name = "In All ZOIs/Areas Combined (%)"
         ##In Target/Aligned Country ZOI (%)##
  ),
  tibble(ic = "EG-e"
         , udn = c("3.1.2", "3.2.2", "3.3.2", "3.5.1.2", "3.6.1.2", "3.7.1.2", "3.8.1.2", "3.9.1.2")
         , name = "In Target/Aligned Country ZOI (%)"
         ##In FFP Development Program Area (%)##
  ),
  tibble(ic = "EG-e"
         , udn = c("3.1.3", "3.2.3", "3.3.3", "3.5.1.3", "3.6.1.3", "3.7.1.3", "3.8.1.3", "3.9.1.3")
         , name = "In FFP Development Program Area (%)"
         ##In Resilience to Recurrent Crisis Area (%)##
  ),
  tibble(ic = "EG-e"
         , udn = c("3.1.4", "3.2.4", "3.3.4", "3.5.1.4", "3.6.1.4", "3.7.1.4", "3.8.1.4", "3.9.1.4")
         , name = "In Resilience to Recurrent Crisis Area (%)"
         ##In All ZOIs/Areas Combined##
  ),
  tibble(ic = "EG-e"
         , udn = c("3.4.1", "3.5.2.1", "3.6.2.1", "3.7.2.1", "3.8.2.1", "3.9.2.1")
         , name = "In All ZOIs/Areas Combined"
         ##In Target/Aligned Country ZOI##
  ),
  tibble(ic = "EG-e"
         , udn = c("3.4.2", "3.5.2.2", "3.6.2.2", "3.7.2.2", "3.8.2.2", "3.9.2.2")
         , name = "In Target/Aligned Country ZOI"
         ##In FFP Development Program Area##
  ),
  tibble(ic = "EG-e"
         , udn = c("3.4.3", "3.5.2.3", "3.6.2.3", "3.7.2.3", "3.8.2.3", "3.9.2.3")
         , name = "In FFP Development Program Area"
         ##In Resilience to Recurrent Crisis Area##
  ),
  tibble(ic = "EG-e"
         , udn = c("3.4.4", "3.5.2.4", "3.6.2.4", "3.7.2.4", "3.8.2.4", "3.9.2.4")
         , name = "In Resilience to Recurrent Crisis Area"
         ##EG-f (Prevalence)##
         ##Percent of People with Moderate and Severe Food Insecurity in this Gendered Household Type in the Country (%)##
  ),
  tibble(ic = "EG-f"
         , udn = c("3.5.1", "3.6.1", "3.7.1", "3.8.1", "3.9.1")
         , name = "Percent of People with Moderate and Severe Food Insecurity in this Gendered Household Type in the Country (%)"
         ##Number of Households for this Gendered Household Type in the Country##
  ),
  tibble(ic = "EG-f"
         , udn = c("3.5.2", "3.6.2", "3.7.2", "3.8.2", "3.9.2")
         , name = "Number of Households for this Gendered Household Type in the Country"
         ##EG-g (Percent of People)##
         ##Percent of Households Falling Below the Fixed Threshold for the Poorest Quintile of the Comparative Wealth Index (All Household Types Together)##
  ),
  tibble(ic = "EG-g", udn ="3.1.1"
         , name = "In All ZOIs/Areas Combined (%)"
  ),
  tibble(ic = "EG-g", udn ="3.1.2"
         , name = "In Target/Aligned Country ZOI (%)"
  ),
  tibble(ic = "EG-g", udn ="3.1.3"
         , name = "In FFP Development Program Area (%)"
  ),
  tibble(ic = "EG-g", udn ="3.1.4"
         , name = "In Resilience to Recurrent Crisis Area (%)"
         ##Number of Households (All Household Types Together)##
  ),
  tibble(ic = "EG-g", udn ="3.2.1"
         , name = "In All ZOIs/Areas Combined"
  ),
  tibble(ic = "EG-g", udn ="3.2.2"
         , name = "In Target/Aligned Country ZOI"
  ),
  tibble(ic = "EG-g", udn ="3.2.3"
         , name = "In FFP Development Program Area"
  ),
  tibble(ic = "EG-g", udn ="3.2.4"
         , name = "In Resilience to Recurrent Crisis Area"
         ##Household type: Male and Female Adults (M&F)##
         ##Percent of Households in This Gendered Household Type Falling Below the Fixed Threshold for the Poorest Quintile of the Comparative Wealth Index##
  ),
  tibble(ic = "EG-g"
         , udn = c("3.3.1.1", "3.3.1.2", "3.3.1.3", "3.3.1.4")
         , name = "Mean Percent Shortfall of the Poor for People in this Gendered Household Type"
         ##Number of Households in this Gendered Household Type##
  ),
  tibble(ic = "EG-g"
         , udn = c("3.3.2.1", "3.3.2.2", "3.3.2.3", "3.3.2.4")
         , name = "Number of People in this Gendered Household Type"
         ##Household type: Adult Female no Adult Male (FNM)##
         ##Percent of Households in This Gendered Household Type Falling Below the Fixed Threshold for the Poorest Quintile of the Comparative Wealth Index##
  ),
  tibble(ic = "EG-g"
         , udn = c("3.4.1.1", "3.4.1.2", "3.4.1.3", "3.4.1.4")
         , name = "Mean Percent Shortfall of the Poor for People in this Gendered Household Type"
         ##Number of Households in this Gendered Household Type##
  ),
  tibble(ic = "EG-g"
         , udn = c("3.4.2.1", "3.4.2.2", "3.4.2.3", "3.4.2.4")
         , name = "Number of People in this Gendered Household Type"
         ##Household type: Adult Male no Adult Female (MNF)##
         ##Percent of Households in This Gendered Household Type Falling Below the Fixed Threshold for the Poorest Quintile of the Comparative Wealth Index##
  ),
  tibble(ic = "EG-g"
         , udn = c("3.5.1.1", "3.5.1.2", "3.5.1.3", "3.5.1.4")
         , name = "Mean Percent Shortfall of the Poor for People in this Gendered Household Type"
         ##Number of Households in this Gendered Household Type##
  ),
  tibble(ic = "EG-g"
         , udn = c("3.5.2.1", "3.5.2.2", "3.5.2.3", "3.5.2.4")
         , name = "Number of People in this Gendered Household Type"
         ##Household type: Child No Adults (CNA)##
         ##Percent of Households in This Gendered Household Type Falling Below the Fixed Threshold for the Poorest Quintile of the Comparative Wealth Index##
  ),
  tibble(ic = "EG-g"
         , udn = c("3.6.1.1", "3.6.1.2", "3.6.1.3", "3.6.1.4")
         , name = "Mean Percent Shortfall of the Poor for People in this Gendered Household Type"
         ##Number of Households in this Gendered Household Type##
  ),
  tibble(ic = "EG-g"
         , udn = c("3.6.2.1", "3.6.2.2", "3.6.2.3", "3.6.2.4")
         , name = "Number of People in this Gendered Household Type"
         ##Household type: Disaggregates Not Available##
         ##Percent of Households in This Gendered Household Type Falling Below the Fixed Threshold for the Poorest Quintile of the Comparative Wealth Index##
  ),
  tibble(ic = "EG-g"
         , udn = c("3.7.1.1", "3.7.1.2", "3.7.1.3", "3.7.1.4")
         , name = "Mean Percent Shortfall of the Poor for People in this Gendered Household Type"
         ##Number of Households in this Gendered Household Type##
  ),
  tibble(ic = "EG-g"
         , udn = c("3.7.2.1", "3.7.2.2", "3.7.2.3", "3.7.2.4")
         , name = "Number of People in this Gendered Household Type"
         ##EG-h (Percent of People)##
         ##Mean Percent Shortfall of the Poor (All Household Types Together)##
  ),
  tibble(ic = "EG-h", udn ="3.1.1"
         , name = "In All ZOIs/Areas Combined (%)"
  ),
  tibble(ic = "EG-h", udn ="3.1.2"
         , name = "In Target/Aligned Country ZOI (%)"
  ),
  tibble(ic = "EG-h", udn ="3.1.3"
         , name = "In FFP Development Program Area (%)"
  ),
  tibble(ic = "EG-h", udn ="3.1.4"
         , name = "In Resilience to Recurrent Crisis Area (%)"
         ##Number of People (All Household Types Together)##
  ),
  tibble(ic = "EG-h", udn ="3.2.1"
         , name = "In All ZOIs/Areas Combined"
  ),
  tibble(ic = "EG-h", udn ="3.2.2"
         , name = "In Target/Aligned Country ZOI"
  ),
  tibble(ic = "EG-h", udn ="3.2.3"
         , name = "In FFP Development Program Area"
  ),
  tibble(ic = "EG-h", udn ="3.2.4"
         , name = "In Resilience to Recurrent Crisis Area"
         ##Household type: Male and Female Adults (M&F)##
         ##Mean Percent Shortfall of the Poor for People in this Gendered Household Type##
  ),
  tibble(ic = "EG-h"
         , udn = c("3.3.1.1", "3.3.1.2", "3.3.1.3", "3.3.1.4")
         , name = "Mean Percent Shortfall of the Poor for People in this Gendered Household Type"
         ##Number of People in this Gendered Household Type##
  ),
  tibble(ic = "EG-h"
         , udn = c("3.3.2.1", "3.3.2.2", "3.3.2.3", "3.3.2.4")
         , name = "Number of People in this Gendered Household Type"
         ##Household type: Adult Female no Adult Male (FNM)##
         ##Mean Percent Shortfall of the Poor for People in this Gendered Household Type##
  ),
  tibble(ic = "EG-h"
         , udn = c("3.4.1.1", "3.4.1.2", "3.4.1.3", "3.4.1.4")
         , name = "Mean Percent Shortfall of the Poor for People in this Gendered Household Type"
         ##Number of People in this Gendered Household Type##
  ),
  tibble(ic = "EG-h"
         , udn = c("3.4.2.1", "3.4.2.2", "3.4.2.3", "3.4.2.4")
         , name = "Number of People in this Gendered Household Type"
         ##Household type: Adult Male no Adult Female (MNF)##
         ##Mean Percent Shortfall of the Poor for People in this Gendered Household Type##
  ),
  tibble(ic = "EG-h"
         , udn = c("3.5.1.1", "3.5.1.2", "3.5.1.3", "3.5.1.4")
         , name = "Mean Percent Shortfall of the Poor for People in this Gendered Household Type"
         ##Number of People in this Gendered Household Type##
  ),
  tibble(ic = "EG-h"
         , udn = c("3.5.2.1", "3.5.2.2", "3.5.2.3", "3.5.2.4")
         , name = "Number of People in this Gendered Household Type"
         ##Household type: Child No Adults (CNA)##
         ##Mean Percent Shortfall of the Poor for People in this Gendered Household Type##
  ),
  tibble(ic = "EG-h"
         , udn = c("3.6.1.1", "3.6.1.2", "3.6.1.3", "3.6.1.4")
         , name = "Mean Percent Shortfall of the Poor for People in this Gendered Household Type"
         ##Number of People in this Gendered Household Type##
  ),
  tibble(ic = "EG-h"
         , udn = c("3.6.2.1", "3.6.2.2", "3.6.2.3", "3.6.2.4")
         , name = "Number of People in this Gendered Household Type"
         ##Household type: Disaggregates Not Available##
         ##Mean Percent Shortfall of the Poor for People in this Gendered Household Type##
  ),
  tibble(ic = "EG-h"
         , udn = c("3.7.1.1", "3.7.1.2", "3.7.1.3", "3.7.1.4")
         , name = "Mean Percent Shortfall of the Poor for People in this Gendered Household Type"
         ##Number of People in this Gendered Household Type##
  ),
  tibble(ic = "EG-h"
         , udn = c("3.7.2.1", "3.7.2.2", "3.7.2.3", "3.7.2.4")
         , name = "Number of People in this Gendered Household Type"
         ##EG.3-e (Percent Change)##
         ##Total Value Added in the Agri-food Sector in USD##
  ),
  tibble(ic = "EG.3-e", udn ="3.1.1.1"
         , name = "Agriculture"
  ),
  tibble(ic = "EG.3-e", udn ="3.1.1.2"
         , name = "Agro-Processing"
  ),
  tibble(ic = "EG.3-e", udn ="3.1.1.3"
         , name = "Trade and Transport"
  ),
  tibble(ic = "EG.3-e", udn ="3.1.1.4"
         , name = "Input Supply"
  ),
  tibble(ic = "EG.3-e", udn ="3.1.1.5"
         , name = "Hotel and Food Services"
         ##EG.3-f (A-WEAI)##
         ##In All ZOI/Areas Combined##
  ),
  tibble(ic = "EG.3-f"
         , udn = c("3.1.1", "3.2.1", "3.3.1", "3.4.1", "3.5.1", "3.6.1", "3.7.1", "3.8.1", "3.9.1", "3.10.1", "3.11.1")
         , name = "In All ZOI/Areas Combined"
         ##In Target/Aligned Country FTF ZOI##
  ),
  tibble(ic = "EG.3-f"
         , udn = c("3.1.2", "3.2.2", "3.3.2", "3.4.2", "3.5.2", "3.6.2", "3.7.2", "3.8.2", "3.9.2", "3.10.2", "3.11.2")
         , name = "In Target/Aligned Country FTF ZOI"
         ##In FFP Development Program Area##
  ),
  tibble(ic = "EG.3-f"
         , udn = c("3.1.3", "3.2.3", "3.3.3", "3.4.3", "3.5.3", "3.6.3", "3.7.3", "3.8.3", "3.9.3", "3.10.3", "3.11.3")
         , name = "In FFP Development Program Area"
         ##In Resilience to Recurrent Crisis Areas##
  ),
  tibble(ic = "EG.3-f"
         , udn = c("3.1.4", "3.2.4", "3.3.4", "3.4.4", "3.5.4", "3.6.4", "3.7.4", "3.8.4", "3.9.4", "3.10.4", "3.11.4")
         , name = "In Resilience to Recurrent Crisis Areas"
         ##EG.3-g (Number of People)##
         ##Number of People Working in the Agri-food Sector Components##
  ),
  tibble(ic = "EG.3-g", udn ="3.1.1"
         , name = "Agriculture"
  ),
  tibble(ic = "EG.3-g", udn ="3.1.2"
         , name = "Agro-Processing"
  ),
  tibble(ic = "EG.3-g", udn ="3.1.3"
         , name = "Trade and Transport"
  ),
  tibble(ic = "EG.3-g", udn ="3.1.4"
         , name = "Input Supply"
  ),
  tibble(ic = "EG.3-g", udn ="3.1.5"
         , name = "Hotel and Food Services"
         ##EG.3-x1 (Number of Households)##
         ##Gendered Household Type##
  ),
  tibble(ic = "EG.3-x1"
         , ms_d2 = "Household type:  Adult Female no Adult Male (FNM)"
         , name = "Household Type: Adult Female No Adult Male (FNM)"
  ),
  tibble(ic = "EG.3-x1"
         , ms_d2 = "Household type:  Adult Male no Adult Female (MNF)"
         , name = "Household Type:  Adult Male No Adult Female (MNF)"
  ),
  tibble(ic = "EG.3-x1"
         , ms_d2 = "Household type:  Child No Adults (CNA)"
         , name = "Household Type:  Child No Adults (CNA)"
  ),
  tibble(ic = "EG.3-x1"
         , ms_d2 = "Household type:  Disaggregates Not Available"
         , name = "Household Type:  Disaggregates Not Available"
  ),
  tibble(ic = "EG.3-x1"
         , ms_d2 = "Household type:  Male and Female Adults (M&F)"
         , name = "Household Type:  Male and Female Adults (M&F)"
         ##Location##
  ),
  tibble(ic = "EG.3-x1"
         , ms_d2 = "Rural"
         , name = "Rural"
  ),
  tibble(ic = "EG.3-x1"
         , ms_d2 = "Urban/Peri-urban"
         , name = "Urban/Peri-Urban"
         ##Duration##
  ),
  tibble(ic = "EG.3-x1"
         , ms_d2 = "New"
         , name = "New"
  ),
  tibble(ic = "EG.3-x1"
         , ms_d2 = "Continuing"
         , name = "Continuing"
         ##DNA##
  ),
  tibble(ic = "EG.3-x1"
         , ms_d2 = "Disaggregates Not Available"
         , name = "Disaggregates Not Available"
         ##EG.3-2 (Participants)##
         ##Sex##
  ),
  tibble(ic = "EG.3-2"
         , udn = "3.1.2"
         , ms_d2 = "Female"
         , name = "Female"
  ),
  tibble(ic = "EG.3-2"
         , udn = "3.1.1"
         , ms_d2 = "Male"
         , name = "Male"
         ##Sex and Age##
  ),
  tibble(ic = "EG.3-2"
         , udn = c("3.1.3", "3.2.4")
         , ms_d2 = "Not Applicable (for household members counted from HH-level interventions)"
         , name = "Not Applicable"
  ),
  tibble(ic = "EG.3-2"
         , udn = c("3.1.4", "3.2.5")
         , ms_d2 = "Disaggregates Not Available"
         , name = "Disaggregates Not Available"
         ##Age##
  ),
  tibble(ic = "EG.3-2"
         , udn = c("3.2.1")
         , ms_d2 = "School-aged children (only for total reached by USG school feeding programs, regardless of actual age)"
         , name = "School-Aged Children"
  ),
  tibble(ic = "EG.3-2"
         , udn = c("3.2.2")
         , ms_d2 = "15-29"
         , name = "15-29"
  ),
  tibble(ic = "EG.3-2"
         , udn = c("3.2.3")
         , ms_d2 = "30+"
         , name = "30+"
         ##Type of Individuals##
  ),
  tibble(ic = "EG.3-2"
         , udn = c("3.3.1")
         , ms_d2 = "Parents/caregivers"
         , name = "Parents/Caregivers"
  ),
  tibble(ic = "EG.3-2"
         , udn = c("3.3.2")
         , ms_d2 = "Household members (household-level interventions only)"
         , name = "Household Members"
  ),
  tibble(ic = "EG.3-2"
         , udn = c("3.3.3"), ms_d2 = "School-aged children (only for USG school feeding programs)"
         , name = "School-Aged Children"
  ),
  tibble(ic = "EG.3-2"
         , udn = c("3.3.4") , ms_d2 = "People in government"
         , name = "People in Government"
  ),
  tibble(ic = "EG.3-2"
         , udn = c("3.3.5") , ms_d2 = "People in USG-assisted private sector firms"
         , name = "People in USG-Assisted Private Sector Firms"
  ),
  tibble(ic = "EG.3-2"
         , udn = c("3.3.6") , ms_d2 = "People in civil society"
         , name = "People in Civil Society"
  ),
  tibble(ic = "EG.3-2"
         , udn = c("3.3.7") , ms_d2 = "Laborers (Non-producer diversified livelihoods participants)"
         , name = "Laborers (Non-Producer Diversified Livelihoods Participants)"
  ),
  tibble(ic = "EG.3-2"
         , udn = c("3.3.8") , ms_d2 = "Producer: Smallholder farmer"
         , name = "Producer: Smallholder Farmer"
  ),
  tibble(ic = "EG.3-2"
         , udn = c("3.3.9") , ms_d2 = "Producer: Non-smallholder farmer"
         , name = "Producer: Non-Smallholder Farmer"
  ),
  tibble(ic = "EG.3-2"
         , udn = c("3.3.10") , ms_d2 = "Producer: Aquaculture"
         , name = "Producer: Aquaculture"
  ),
  tibble(ic = "EG.3-2"
         , udn = c("3.3.11") , ms_d2 = "Producer: size Disaggregate Not Available"
         , name = "Producer: Size Disaggregate Not Available"
  ),
  tibble(ic = "EG.3-2"
         , udn = c("3.3.12") , ms_d2 = "Type of individual Not Applicable"
         , name = "Type of Individual Not Applicable"
  ),
  tibble(ic = "EG.3-2"
         , udn = c("3.3.13") , ms_d2 = "Type of Individual Disaggregates Not Available"
         , name = "Type of Individual Disaggregates Not Available"
         ##EG.3-2_OULevel (Participants)##
         ##Sex##
  ),
  tibble(ic = "EG.3-2_OULevel", udn ="3.1.2"
         , name = "Female"
  ),
  tibble(ic = "EG.3-2_OULevel", udn ="3.1.1"
         , name = "Male"
         ##Sex and Age##
  ),
  tibble(ic = "EG.3-2_OULevel"
         , udn = c("3.1.3", "3.2.4")
         , name = "Not Applicable"
  ),
  tibble(ic = "EG.3-2_OULevel"
         , udn = c("3.1.4", "3.2.5")
         , name = "Disaggregates Not Available"
         ##Age##
  ),
  tibble(ic = "EG.3-2_OULevel", udn ="3.2.1"
         , name = "School-Aged Children"
  ),
  tibble(ic = "EG.3-2_OULevel", udn ="3.2.2"
         , name = "15-29"
  ),
  tibble(ic = "EG.3-2_OULevel", udn ="3.2.3"
         , name = "30+"
         ##Type of Individuals##
  ),
  tibble(ic = "EG.3-2_OULevel", udn ="3.3.1"
         , name = "Parents/Caregivers"
  ),
  tibble(ic = "EG.3-2_OULevel", udn ="3.3.2"
         , name = "Household Members"
  ),
  tibble(ic = "EG.3-2_OULevel", udn ="3.3.3"
         , name = "School-Aged Children"
  ),
  tibble(ic = "EG.3-2_OULevel", udn ="3.3.4"
         , name = "People in Government"
  ),
  tibble(ic = "EG.3-2_OULevel", udn ="3.3.5"
         , name = "People in USG-Assisted Private Sector Firms"
  ),
  tibble(ic = "EG.3-2_OULevel", udn ="3.3.6"
         , name = "People in Civil Society"
  ),
  tibble(ic = "EG.3-2_OULevel", udn ="3.3.7"
         , name = "Laborers (Non-Producer Diversified Livelihoods Participants)"
  ),
  tibble(ic = "EG.3-2_OULevel", udn ="3.3.8"
         , name = "Producer: Smallholder Farmer"
  ),
  tibble(ic = "EG.3-2_OULevel", udn ="3.3.9"
         , name = "Producer: Non-Smallholder Farmer"
  ),
  tibble(ic = "EG.3-2_OULevel", udn ="3.3.10"
         , name = "Producer: Aquaculture"
  ),
  tibble(ic = "EG.3-2_OULevel", udn ="3.3.11"
         , name = "Producer: Size Disaggregate Not Available"
  ),
  tibble(ic = "EG.3-2_OULevel", udn ="3.3.12"
         , name = "Type of Individual Not Applicable"
  ),
  tibble(ic = "EG.3-2_OULevel", udn ="3.3.13"
         , name = "Type of Individual Disaggregates Not Available"
         ##EG.3-x6, -7, -8##
  ),
  tibble(ic="EG.3-x6, -7, -8", ms_d3 =  "Association-applied"
         , name = "Association-Applied"
  ),
  tibble(ic="EG.3-x6, -7, -8", ms_d3 =  "Disaggregates Not Available"
         , name = "Disaggregates Not Available"
  ),
  tibble(ic="EG.3-x6, -7, -8", ms_d3 =  "Female"
         , name = "Female"
  ),
  tibble(ic="EG.3-x6, -7, -8", ms_d3 =  "Male"
         , name = "Male"
  ),
  tibble(ic="EG.3-x6, -7, -8", ms_d3 =  "Joint"
         , name = "Joint"
         ##EG.3-x9##
         ##Duration##
  ),
  tibble(ic="EG.3-x9"
         , ms_d2 = "New"
         , name = "New"
  ),
  tibble(ic="EG.3-x9"
         , ms_d2 = "Continuing"
         , name = "Continuing"
         ##Location##
  ),
  tibble(ic="EG.3-x9"
         , ms_d2 = "Rural"
         , name = "Rural"
  ),
  tibble(ic="EG.3-x9"
         , ms_d2 = "Urban/peri-urban"
         , name = "Urban/Peri-Urban"
         ##Sex of Job-Holder##
  ),
  tibble(ic="EG.3-x9"
         , ms_d2 = "Female"
         , name = "Female"
  ),
  tibble(ic="EG.3-x9"
         , ms_d2 = "Male"
         , name = "Male"
  ),
  tibble(ic="EG.3-x9"
         , ms_d2 = "Disaggregates Not Available"
         , name = "Disaggregates Not Available"
         ##EG.3-10-11-12 (TP and UP Sex)##
         ##Participants##
  ),
  tibble(ic = "EG.3-10-11-12"
         , udn = c("3.1.1.1.8.1.2", "3.1.1.1.8.1.1", "3.1.1.1.8.1.3", "3.1.1.2.8.1.2", "3.1.1.2.8.1.1", "3.1.1.2.8.1.3", "3.1.1.3.8.1.2", "3.1.1.3.8.1.1", "3.1.1.3.8.1.3", "3.1.1.4.8.1.2", "3.1.1.4.8.1.1", "3.1.1.4.8.1.3")
         , name = "Number of Participants"
         ##TP##
  ),
  tibble(ic = "EG.3-10-11-12"
         , udn = c("3.1.1.1.9.1.2", "3.1.1.1.9.1.1", "3.1.1.1.9.1.3", "3.1.1.2.9.1.2", "3.1.1.2.9.1.1", "3.1.1.2.9.1.3", "3.1.1.3.9.1.2", "3.1.1.3.9.1.1", "3.1.1.3.9.1.3", "3.1.1.4.9.1.2", "3.1.1.4.9.1.1", "3.1.1.4.9.1.3")
         , name = "Total Production"
         ##UP##
  ),
  tibble(ic = "EG.3-10-11-12"
         , udn = c("3.1.1.1.10.1.2", "3.1.1.1.10.1.1", "3.1.1.1.10.1.3", "3.1.1.2.10.1.2", "3.1.1.2.10.1.1", "3.1.1.2.10.1.3", "3.1.1.3.10.1.2", "3.1.1.3.10.1.1", "3.1.1.3.10.1.3", "3.1.1.4.10.1.2", "3.1.1.4.10.1.1", "3.1.1.4.10.1.3")
         , name = "Units of Production"
         ##Participants##
  ),
  tibble(ic = "EG.3-10-11-12"
         , udn = c("3.1.1.1.8.2.2", "3.1.1.1.8.2.1", "3.1.1.1.8.2.3", "3.1.1.2.8.2.2", "3.1.1.2.8.2.1", "3.1.1.2.8.2.3", "3.1.1.3.8.2.2", "3.1.1.3.8.2.1", "3.1.1.3.8.2.3", "3.1.1.4.8.2.2", "3.1.1.4.8.2.1", "3.1.1.4.8.2.3")
         , name = "Number of Participants"
         ##TP##
  ),
  tibble(ic = "EG.3-10-11-12"
         , udn = c("3.1.1.1.9.2.2", "3.1.1.1.9.2.1", "3.1.1.1.9.2.3", "3.1.1.2.9.2.2", "3.1.1.2.9.2.1", "3.1.1.2.9.2.3", "3.1.1.3.9.2.2", "3.1.1.3.9.2.1", "3.1.1.3.9.2.3", "3.1.1.4.9.2.2", "3.1.1.4.9.2.1", "3.1.1.4.9.2.3")
         , name = "Total Production"
         ##UP##
  ),
  tibble(ic = "EG.3-10-11-12"
         , udn = c("3.1.1.1.10.2.2", "3.1.1.1.10.2.1", "3.1.1.1.10.2.3", "3.1.1.2.10.2.2", "3.1.1.2.10.2.1", "3.1.1.2.10.2.3", "3.1.1.3.10.2.2", "3.1.1.3.10.2.1", "3.1.1.3.10.2.3", "3.1.1.4.10.2.2", "3.1.1.4.10.2.1", "3.1.1.4.10.2.3")
         , name = "Units of Production"

         ##EG.3.1-c (Value)##
         ##Value of exports##
  ),
  ##Value of exports##
  ##EG.3.1-c##
  ##Value of exports##
  tibble(ic = "EG.3.1-c",  udn = c("3.1", "3.1.1", "3.1.2"), name = "[Commodity (From DIS)]"
  ),
         ##EG.3.1-x12 (Policies)##
         ##Policy Area##
  tibble(ic = "EG.3.1-x12"
         , ms_d2 = "Agricultural input policy (e.g. seed, fertilizer)"
         , name = "Agricultural Input Policy (e.g. Seed, Fertilizer)"
  ),
  tibble(ic = "EG.3.1-x12"
         , ms_d2 = "Agricultural trade policy"
         , name = "Agricultural Trade Policy"
  ),
  tibble(ic = "EG.3.1-x12"
         , ms_d2 = "Enabling environment for private sector investment"
         , name = "Enabling Environment for Private Sector Investment"
  ),
  tibble(ic = "EG.3.1-x12"
         , ms_d2 = "Institutional architecture for improved policy formulation"
         , name = "Institutional Architecture for Improved Policy Formulation"
  ),
  tibble(ic = "EG.3.1-x12"
         , ms_d2 = "Land and natural resources tenure, rights, and policy"
         , name = "Land and Natural Resources Tenure, Rights, and Policy"
  ),
  tibble(ic = "EG.3.1-x12"
         , ms_d2 = "Nutrition (e.g. fortification, food safety)"
         , name = "Nutrition (e.g. Fortification, Food Safety)"
  ),
  tibble(ic = "EG.3.1-x12"
         , ms_d2 = "Other"
         , name = "Other"
  ),
  tibble(ic = "EG.3.1-x12"
         , ms_d2 = "Resilience and agricultural risk management policy"
         , name = "Resilience and Agricultural Risk Management Policy"
         ##Process/Step##
  ),
  tibble(ic = "EG.3.1-x12"
         , ms_d2 = "Analysis"
         , name = "Analysis"
  ),
  tibble(ic = "EG.3.1-x12"
         , ms_d2 = "Approval (legislative or regulatory)"
         , name = "Approval (Legislative or Regulatory)"
  ),
  tibble(ic = "EG.3.1-x12"
         , ms_d2 = "Drafting or revision"
         , name = "Drafting or Revision"
  ),
  tibble(ic = "EG.3.1-x12"
         , ms_d2 = "Full and effective implementation"
         , name = "Full and Effective Implementation"
  ),
  tibble(ic = "EG.3.1-x12"
         , ms_d2 = "Stakeholder consultation/public debate"
         , name = "Stakeholder Consultation/Public Debate"
  ),
  tibble(ic = "EG.3.1-x12"
         , ms_d2 = "Total policies passing through one of more processes/steps of policy change"
         , name = "Total Policies Passing Through One of More Processes/Steps of Policy Change"
  ),
  tibble(ic = "EG.3.1-x12"
         , ms_d2 = "Disaggregates Not Available"
         , name = "Disaggregates Not Available"
         ##EG.3.2-x1##
  ),
  tibble(ic = "EG.3.2-x1"
         , ms_d1 ="Type of Individual"
         , name = "ms_d3"
  ),
  tibble(ic = "EG.3.2-x1"
         , ms_d1 ="Sex"
         , name = "ms_d2"
         ##EG.3.2-2 (Participants by Sex)##
         ##Female##
  ),
  tibble(ic = "EG.3.2-2"
         , udn = c("3.1.2") , ms_d2 = "Female"
         , name = "Female"
         ##Male##
  ),
  tibble(ic = "EG.3.2-2"
         , udn = c("3.1.1") , ms_d2 = "Male"
         , name = "Male"
         ##DNA##
  ),
  tibble(ic = "EG.3.2-2"
         , udn = c("3.1.3") , ms_d2 = "Disaggregates Not Available"
         , name = "Disaggregates Not Available"
         ##New##
  ),
  tibble(ic = "EG.3.2-2"
         , udn = c("3.2.1") , ms_d2 = "New"
         , name = "New"
         ##Continuing##
  ),
  tibble(ic = "EG.3.2-2"
         , udn = c("3.2.2") , ms_d2 = "Continuing"
         , name = "Continuing"
         ##DNA##
  ),
  tibble(ic = "EG.3.2-2"
         , udn = c("3.2.3") , ms_d2 = "Disaggregates Not Available"
         , name = "Disaggregates Not Available"
         ##EG.3.2-x3##
  ),
  tibble(ic = "EG.3.2-x3"
         , ms_d2 = "n/a"
         , name = "Not Applicable"
  ),
  tibble(ic = "EG.3.2-x3"
         , ms_d2 = "Micro (1-10 employees)"
         , name = "Micro (1-10 Employees)"
  ),
  tibble(ic = "EG.3.2-x3"
         , ms_d2 = "Small (11-50 employees)"
         , name = "Small (11-50 Employees)"
  ),
  tibble(ic = "EG.3.2-x3"
         , ms_d2 = "Medium (51-100 employees)"
         , name = "Medium (51-100 Employees)"
  ),
  tibble(ic = "EG.3.2-x3"
         , name = "ms_d2"
         ##EG.3.2-x4##
  ),
  tibble(ic = "EG.3.2-x4"
         , ms_d2 = "Community-based organizations (CBOs)"
         , name = "Community-Based Organizations (CBOs)"
  ),
  tibble(ic = "EG.3.2-x4"
         , ms_d2 = "For-profit private enterprises"
         , name = "For-Profit Private Enterprises"
  ),
  tibble(ic = "EG.3.2-x4"
         , ms_d2 = "Producers organizations"
         , name = "Producers Organizations"
  ),
  tibble(ic = "EG.3.2-x4"
         , ms_d2 = "Trade and business associations"
         , name = "Trade and Business Associations"
  ),
  tibble(ic = "EG.3.2-x4"
         , ms_d2 = "Water users associations"
         , name = "Water Users Associations"
  ),
  tibble(ic = "EG.3.2-x4"
         , ms_d2 = "Women's groups"
         , name = "Women's Groups"
  ),
  tibble(ic = "EG.3.2-x4"
         , name = "ms_d2"
         ##EG.3.2-x6 (USD)##
  ),
  tibble(ic = "EG.3.2-x6"
         , ms_d2 = "n/a"
         , name = "Not Applicable"
  ),
  tibble(ic = "EG.3.2-x6"
         , ms_d2 = "Local traders/assemblers"
         , name = "Local Traders/Assemblers"
  ),
  tibble(ic = "EG.3.2-x6"
         , ms_d2 = "Wholesalers/processors"
         , name = "Wholesalers/Processors"
  ),
  tibble(ic = "EG.3.2-x6"
         , name = "ms_d2"
         ##EG.3.2-7 (Number of Technologies, Practices, and Approaches)##
         ##Phase of Research (Double-Counting Allowed)##
  ),
  tibble(ic = "EG.3.2-7"
         , udn = c("3.1.5.1.1.1", "3.1.5.1.1.2", "3.1.5.1.1.3", "3.1.5.1.1.4")
         , name = "Phase of Research (Double-Counting Allowed)"
  ),
  tibble(ic = "EG.3.2-7"
         , udn = c("3.1.5.2.1.1", "3.1.5.2.1.2", "3.1.5.2.1.3", "3.1.5.2.1.4")
         , name = "Phase of Research (Double-Counting Allowed)"
  ),
  tibble(ic = "EG.3.2-7"
         , udn = c("3.1.5.3.1.1", "3.1.5.3.1.2", "3.1.5.3.1.3", "3.1.5.3.1.4")
         , name = "Phase of Research (Double-Counting Allowed)"
  ),
  tibble(ic = "EG.3.2-7"
         , udn = c("3.1.5.4.1.1", "3.1.5.4.1.2", "3.1.5.4.1.3", "3.1.5.4.1.4")
         , name = "Phase of Research (Double-Counting Allowed)"
  ),
  tibble(ic = "EG.3.2-7", ms_d3 =  "Phase of Research (double-counting allowed)"
         , name = "Phase of Research (Double-Counting Allowed)"
         ##Total Number of Unique Technologies/Practices (No Double-Counting)##
  ),
  tibble(ic = "EG.3.2-7"
         , udn = c("3.1.1", "3.1.2", "3.1.3", "3.1.4")
         , name = "Total Number of Unique Technologies/Practices (No Double-Counting)"
  ),
  tibble(ic = "EG.3.2-7"
         , ms_d1 ="Total number of unique technologies / practices / approaches from all categories (no double-counting)"
         , name = "Total Number of Unique Technologies/Practices (No Double-Counting)"
         ##EG.3.2-x14 (HHs)##
  ),
  tibble(ic = "EG.3.2-x14"
         , ms_d2 = "Household type:  Adult Female no Adult Male (FNM)"
         , name = "Household Type:  Adult Female No Adult Male (FNM)"
  ),
  tibble(ic = "EG.3.2-x14"
         , ms_d2 = "Household type:  Adult Male no Adult Female (MNF)"
         , name = "Household Type:  Adult Male No Adult Female (MNF)"
  ),
  tibble(ic = "EG.3.2-x14"
         , ms_d2 = "Household type:  Child No Adults (CNA)"
         , name = "Household Type:  Child No Adults (CNA)"
  ),
  tibble(ic = "EG.3.2-x14"
         , ms_d2 = "Household type:  Disaggregates Not Available"
         , name = "Household Type:  Disaggregates Not Available"
  ),
  tibble(ic = "EG.3.2-x14"
         , ms_d2 = "Household type:  Male and Female Adults (M&F)"
         , name = "Household Type:  Male and Female Adults (M&F)"
  ),
  tibble(ic = "EG.3.2-x14"
         , name = "ms_d2"
         ##EG.3.2-24 (Value Chain Actor Type Sex)##
         ##Sex##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.1.1", "3.1.1.2", "3.1.1.3")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.2.1.1", "3.2.1.2", "3.2.1.3")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.3.1.1", "3.3.1.2", "3.3.1.3")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.4.1.1", "3.4.1.2", "3.4.1.3")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.5.1.1", "3.5.1.2", "3.5.1.3")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.6.1.1", "3.6.1.2", "3.6.1.3")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.7.1.1", "3.7.1.2", "3.7.1.3")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-24"
         , ms_d2 = "Sex (no double-counting)"
         , name = "Sex"
         ##Age##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.2.1", "3.1.2.2", "3.1.2.3")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.2.2.1", "3.2.2.2", "3.2.2.3")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.3.2.1", "3.3.2.2", "3.3.2.3")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.4.2.1", "3.4.2.2", "3.4.2.3")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.5.2.1", "3.5.2.2", "3.5.2.3")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.6.2.1", "3.6.2.2", "3.6.2.3")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.7.2.1", "3.7.2.2", "3.7.2.3")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-24"
         , ms_d2 = "Age category (no double-counting)"
         , name = "Age"
         ##Management Practice or Tech Type##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.3.1", "3.1.3.2", "3.1.3.3", "3.1.3.4", "3.1.3.5"
                   , "3.1.3.6", "3.1.3.7", "3.1.3.8", "3.1.3.9", "3.1.3.10"
                   , "3.1.3.11", "3.1.3.12", "3.1.3.13", "3.1.3.14", "3.1.3.15"
                   , "3.1.3.16")
         , name = "Management Practice or Tech Type"
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.2.3.1", "3.2.3.2", "3.2.3.3", "3.2.3.4", "3.2.3.5"
                   , "3.2.3.6", "3.2.3.7", "3.2.3.8", "3.2.3.9", "3.2.3.10"
                   , "3.2.3.11", "3.2.3.12", "3.2.3.13", "3.2.3.14", "3.2.3.15"
                   , "3.2.3.16")
         , name = "Management Practice or Tech Type (Double-Counting Allowed)"
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.3.3.1", "3.3.3.2", "3.3.3.3", "3.3.3.4", "3.3.3.5"
                   , "3.3.3.6", "3.3.3.7", "3.3.3.8", "3.3.3.9", "3.3.3.10"
                   , "3.3.3.11", "3.3.3.12", "3.3.3.13", "3.3.3.14", "3.3.3.15"
                   , "3.3.3.16")
         , name = "Management Practice or Tech Type"
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.4.3.1", "3.4.3.2", "3.4.3.3", "3.4.3.4", "3.4.3.5"
                   , "3.4.3.6", "3.4.3.7", "3.4.3.8", "3.4.3.9", "3.4.3.10"
                   , "3.4.3.11", "3.4.3.12", "3.4.3.13", "3.4.3.14", "3.4.3.15"
                   , "3.4.3.16")
         , name = "Management Practice or Tech Type"
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.5.3.1", "3.5.3.2", "3.5.3.3", "3.5.3.4", "3.5.3.5"
                   , "3.5.3.6", "3.5.3.7", "3.5.3.8", "3.5.3.9", "3.5.3.10"
                   , "3.5.3.11", "3.5.3.12", "3.5.3.13", "3.5.3.14", "3.5.3.15"
                   , "3.5.3.16")
         , name = "Management Practice or Tech Type"
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.6.3.1", "3.6.3.2", "3.6.3.3", "3.6.3.4", "3.6.3.5"
                   , "3.6.3.6", "3.6.3.7", "3.6.3.8", "3.6.3.9", "3.6.3.10"
                   , "3.6.3.11", "3.6.3.12", "3.6.3.13", "3.6.3.14", "3.6.3.15"
                   , "3.6.3.16")
         , name = "Management Practice or Tech Type"
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.7.3.1", "3.7.3.2", "3.7.3.3", "3.7.3.4", "3.7.3.5"
                   , "3.7.3.6", "3.7.3.7", "3.7.3.8", "3.7.3.9", "3.7.3.10"
                   , "3.7.3.11", "3.7.3.12", "3.7.3.13", "3.7.3.14", "3.7.3.15"
                   , "3.7.3.16")
         , name = "Management Practice or Tech Type"
  ),
  tibble(ic = "EG.3.2-24"
         , ms_d2 = "Management Practice or Tech Type (double-counting allowed)"
         , name = "Management Practice or Tech Type"
         ##Commodity##
  ),
  tibble(ic = "EG.3.2-24"
         , udn = c("3.1.4", "3.2.4", "3.3.4", "3.4.4", "3.5.4", "3.6.4", "3.7.4")
         , name = "Commodity"
  ),
  tibble(ic = "EG.3.2-24"
         , ms_d2 = "Commodity"
         , name = "Commodity"
         ##EG.3.2-25 (Type of Hectare by Sex)##
         ##Sex##
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.1.1", "3.1.1.2", "3.1.1.3", "3.1.1.4", "3.2.1.1"
                   , "3.2.1.2", "3.2.1.3", "3.2.1.4", "3.3.1.1", "3.3.1.2"
                   , "3.3.1.3", "3.3.1.4", "3.4.1.1", "3.4.1.2", "3.4.1.3"
                   , "3.4.1.4", "3.5.1.1", "3.5.1.2", "3.5.1.3", "3.5.1.4"
                   , "3.6.1.1", "3.6.1.2", "3.6.1.3", "3.6.1.4", "3.7.1.1"
                   , "3.7.1.2", "3.7.1.3", "3.7.1.4")
         , name = "Sex"
  ),
  tibble(ic = "EG.3.2-25"
         , ms_d2 = "Sex of Participant (no double-counting)"
         , name = "Sex"
         ##Age##
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.2.1", "3.1.2.2", "3.1.2.3", "3.1.2.4", "3.2.2.1"
                   , "3.2.2.2", "3.2.2.3", "3.2.2.4", "3.3.2.1", "3.3.2.2"
                   , "3.3.2.3", "3.3.2.4", "3.4.2.1", "3.4.2.2", "3.4.2.3"
                   , "3.4.2.4", "3.5.2.1", "3.5.2.2", "3.5.2.3", "3.5.2.4"
                   , "3.6.2.1", "3.6.2.2", "3.6.2.3", "3.6.2.4", "3.7.2.1"
                   , "3.7.2.2", "3.7.2.3", "3.7.2.4")
         , name = "Age"
  ),
  tibble(ic = "EG.3.2-25"
         , ms_d2 = "Age of Participant (no double-counting)"
         , name = "Age"
         ##Management Practice or Tech Type##
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.3.1", "3.1.3.2", "3.1.3.3", "3.1.3.4", "3.1.3.5"
                   , "3.1.3.6", "3.1.3.7", "3.1.3.8", "3.1.3.9", "3.1.3.10"
                   , "3.1.3.11", "3.1.3.12", "3.1.3.13", "3.2.3.1", "3.2.3.2"
                   , "3.2.3.3", "3.2.3.4", "3.2.3.5", "3.2.3.6", "3.2.3.7"
                   , "3.2.3.8", "3.2.3.9", "3.2.3.10", "3.2.3.11", "3.2.3.12"
                   , "3.2.3.13", "3.3.3.1", "3.3.3.2", "3.3.3.3", "3.3.3.4"
                   , "3.3.3.5", "3.3.3.6", "3.3.3.7", "3.3.3.8", "3.3.3.9"
                   , "3.3.3.10", "3.3.3.11", "3.3.3.12", "3.3.3.13", "3.4.3.1"
                   , "3.4.3.2", "3.4.3.3", "3.4.3.4", "3.4.3.5", "3.4.3.6"
                   , "3.4.3.7", "3.4.3.8", "3.4.3.9", "3.4.3.10", "3.4.3.11"
                   , "3.4.3.12", "3.4.3.13", "3.5.3.1", "3.5.3.2", "3.5.3.3"
                   , "3.5.3.4", "3.5.3.5", "3.5.3.6", "3.5.3.7", "3.5.3.8"
                   , "3.5.3.9", "3.5.3.10", "3.5.3.11", "3.5.3.12", "3.5.3.13"
                   , "3.6.3.1", "3.6.3.2", "3.6.3.3", "3.6.3.4", "3.6.3.5"
                   , "3.6.3.6", "3.6.3.7", "3.6.3.8", "3.6.3.9", "3.6.3.10"
                   , "3.6.3.11", "3.6.3.12", "3.6.3.13", "3.7.3.1", "3.7.3.2"
                   , "3.7.3.3", "3.7.3.4", "3.7.3.5", "3.7.3.6", "3.7.3.7"
                   , "3.7.3.8", "3.7.3.9", "3.7.3.10", "3.7.3.11", "3.7.3.12"
                   , "3.7.3.13")
         , name = "Management Practice or Tech Type"
  ),
  tibble(ic = "EG.3.2-25"
         , ms_d2 = "Management Practice or Tech Type (double-counting allowed)"
         , name = "Management Practice or Tech Type"
         ##Commodity##
  ),
  tibble(ic = "EG.3.2-25"
         , udn = c("3.1.4", "3.2.4", "3.3.4", "3.4.4", "3.5.4", "3.6.4", "3.7.4")
         , name = "Commodity"
  ),
  tibble(ic = "EG.3.2-25"
         , ms_d2 = "Commodity"
         , name = "Commodity"
         ##EG.3.2-26 (Value Type of Product, Service, or Commodity Sex)##
         ##(Type of Product) Inputs: Seeds and Planting Material##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.3.1", "3.1.1.1.3.3", "3.1.1.1.3.5", "3.1.1.1.3.7")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.2.3.1", "3.1.1.2.3.3", "3.1.1.2.3.5", "3.1.1.2.3.7")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.3.3.1", "3.1.1.3.3.3", "3.1.1.3.3.5", "3.1.1.3.3.7")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.4.3.1", "3.1.1.4.3.3", "3.1.1.4.3.5", "3.1.1.4.3.7")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.5.3.1", "3.1.1.5.3.3", "3.1.1.5.3.5", "3.1.1.5.3.7")
         , name = "Firm - Large Enterprise or Corporation"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.3.2", "3.1.1.1.3.4", "3.1.1.1.3.6", "3.1.1.1.3.8")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.2.3.2", "3.1.1.2.3.4", "3.1.1.2.3.6", "3.1.1.2.3.8")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.3.3.2", "3.1.1.3.3.4", "3.1.1.3.3.6", "3.1.1.3.3.8")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.4.3.2", "3.1.1.4.3.4", "3.1.1.4.3.6", "3.1.1.4.3.8")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.5.3.2", "3.1.1.5.3.4", "3.1.1.5.3.6", "3.1.1.5.3.8")
         , name = "Firm - Large Enterprise or Corporation"
         ##(Type of Product): Inputs: Other Non-Durable Inputs, Such as Fertilizers and Pesticides##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.2.1.3.1", "3.1.2.1.3.3", "3.1.2.1.3.5", "3.1.2.1.3.7")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.2.2.3.1", "3.1.2.2.3.3", "3.1.2.2.3.5", "3.1.2.2.3.7")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.2.3.3.1", "3.1.2.3.3.3", "3.1.2.3.3.5", "3.1.2.3.3.7")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.2.4.3.1", "3.1.2.4.3.3", "3.1.2.4.3.5", "3.1.2.4.3.7")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.2.5.3.1", "3.1.2.5.3.3", "3.1.2.5.3.5", "3.1.2.5.3.7")
         , name = "Firm - Large Enterprise or Corporation"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.2.1.3.2", "3.1.2.1.3.4", "3.1.2.1.3.6", "3.1.2.1.3.8")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.2.2.3.2", "3.1.2.2.3.4", "3.1.2.2.3.6", "3.1.2.2.3.8")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.2.3.3.2", "3.1.2.3.3.4", "3.1.2.3.3.6", "3.1.2.3.3.8")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.2.4.3.2", "3.1.2.4.3.4", "3.1.2.4.3.6", "3.1.2.4.3.8")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.2.5.3.2", "3.1.2.5.3.4", "3.1.2.5.3.6", "3.1.2.5.3.8")
         , name = "Firm - Large Enterprise or Corporation"
         ##(Type of Product): Inputs: Durable Equipment and Machinery##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.3.1.3.1", "3.1.3.1.3.3", "3.1.3.1.3.5", "3.1.3.1.3.7")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.3.2.3.1", "3.1.3.2.3.3", "3.1.3.2.3.5", "3.1.3.2.3.7")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.3.3.3.1", "3.1.3.3.3.3", "3.1.3.3.3.5", "3.1.3.3.3.7")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.3.4.3.1", "3.1.3.4.3.3", "3.1.3.4.3.5", "3.1.3.4.3.7")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.3.5.3.1", "3.1.3.5.3.3", "3.1.3.5.3.5", "3.1.3.5.3.7")
         , name = "Firm - Large Enterprise or Corporation"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.3.1.3.2", "3.1.3.1.3.4", "3.1.3.1.3.6", "3.1.3.1.3.8")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.3.2.3.2", "3.1.3.2.3.4", "3.1.3.2.3.6", "3.1.3.2.3.8")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.3.3.3.2", "3.1.3.3.3.4", "3.1.3.3.3.6", "3.1.3.3.3.8")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.3.4.3.2", "3.1.3.4.3.4", "3.1.3.4.3.6", "3.1.3.4.3.8")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.3.5.3.2", "3.1.3.5.3.4", "3.1.3.5.3.6", "3.1.3.5.3.8")
         , name = "Firm - Large Enterprise or Corporation"
         ##(Type of Product): Processed Products/Value Added Products (Post Harvest)##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.4.1.3.1", "3.1.4.1.3.3", "3.1.4.1.3.5", "3.1.4.1.3.7")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.4.2.3.1", "3.1.4.2.3.3", "3.1.4.2.3.5", "3.1.4.2.3.7")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.4.3.3.1", "3.1.4.3.3.3", "3.1.4.3.3.5", "3.1.4.3.3.7")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.4.4.3.1", "3.1.4.4.3.3", "3.1.4.4.3.5", "3.1.4.4.3.7")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.4.5.3.1", "3.1.4.5.3.3", "3.1.4.5.3.5", "3.1.4.5.3.7")
         , name = "Firm - Large Enterprise or Corporation"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.4.1.3.2", "3.1.4.1.3.4", "3.1.4.1.3.6", "3.1.4.1.3.8")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.4.2.3.2", "3.1.4.2.3.4", "3.1.4.2.3.6", "3.1.4.2.3.8")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.4.3.3.2", "3.1.4.3.3.4", "3.1.4.3.3.6", "3.1.4.3.3.8")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.4.4.3.2", "3.1.4.4.3.4", "3.1.4.4.3.6", "3.1.4.4.3.8")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.4.5.3.2", "3.1.4.5.3.4", "3.1.4.5.3.6", "3.1.4.5.3.8")
         , name = "Firm - Large Enterprise or Corporation"
         ##(Type of Product): Post-Harvest Storage and Processing Equipment##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.5.1.3.1", "3.1.5.1.3.3", "3.1.5.1.3.5", "3.1.5.1.3.7")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.5.2.3.1", "3.1.5.2.3.3", "3.1.5.2.3.5", "3.1.5.2.3.7")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.5.3.3.1", "3.1.5.3.3.3", "3.1.5.3.3.5", "3.1.5.3.3.7")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.5.4.3.1", "3.1.5.4.3.3", "3.1.5.4.3.5", "3.1.5.4.3.7")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.5.5.3.1", "3.1.5.5.3.3", "3.1.5.5.3.5", "3.1.5.5.3.7")
         , name = "Firm - Large Enterprise or Corporation"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.5.1.3.2", "3.1.5.1.3.4", "3.1.5.1.3.6", "3.1.5.1.3.8")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.5.2.3.2", "3.1.5.2.3.4", "3.1.5.2.3.6", "3.1.5.2.3.8")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.5.3.3.2", "3.1.5.3.3.4", "3.1.5.3.3.6", "3.1.5.3.3.8")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.5.4.3.2", "3.1.5.4.3.4", "3.1.5.4.3.6", "3.1.5.4.3.8")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.5.5.3.2", "3.1.5.5.3.4", "3.1.5.5.3.6", "3.1.5.5.3.8")
         , name = "Firm - Large Enterprise or Corporation"
         ##(Type of Product): Disaggregates Not Available##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.6.1.3.1", "3.1.6.1.3.3", "3.1.6.1.3.5", "3.1.6.1.3.7")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.6.2.3.1", "3.1.6.2.3.3", "3.1.6.2.3.5", "3.1.6.2.3.7")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.6.3.3.1", "3.1.6.3.3.3", "3.1.6.3.3.5", "3.1.6.3.3.7")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.6.4.3.1", "3.1.6.4.3.3", "3.1.6.4.3.5", "3.1.6.4.3.7")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.6.5.3.1", "3.1.6.5.3.3", "3.1.6.5.3.5", "3.1.6.5.3.7")
         , name = "Firm - Large Enterprise or Corporation"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.6.1.3.2", "3.1.6.1.3.4", "3.1.6.1.3.6", "3.1.6.1.3.8")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.6.2.3.2", "3.1.6.2.3.4", "3.1.6.2.3.6", "3.1.6.2.3.8")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.6.3.3.2", "3.1.6.3.3.4", "3.1.6.3.3.6", "3.1.6.3.3.8")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.6.4.3.2", "3.1.6.4.3.4", "3.1.6.4.3.6", "3.1.6.4.3.8")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.6.5.3.2", "3.1.6.5.3.4", "3.1.6.5.3.6", "3.1.6.5.3.8")
         , name = "Firm - Large Enterprise or Corporation"
         ##(Type of Service): Business Services, Including Financial, Entrepreneurial, Legal, etc.##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.1.3.1", "3.2.1.1.3.3", "3.2.1.1.3.5", "3.2.1.1.3.7")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.2.3.1", "3.2.1.2.3.3", "3.2.1.2.3.5", "3.2.1.2.3.7")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.3.3.1", "3.2.1.3.3.3", "3.2.1.3.3.5", "3.2.1.3.3.7")
         , name = "Firm - Large Enterprise or Corporation"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.1.3.2", "3.2.1.1.3.4", "3.2.1.1.3.6", "3.2.1.1.3.8")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.2.3.2", "3.2.1.2.3.4", "3.2.1.2.3.6", "3.2.1.2.3.8")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.3.3.2", "3.2.1.3.3.4", "3.2.1.3.3.6", "3.2.1.3.3.8")
         , name = "Firm - Large Enterprise or Corporation"
         ##(Type of Service): Information Services:  SMS, Radio, TV, Print, etc.##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.1.3.1", "3.2.2.1.3.3", "3.2.2.1.3.5", "3.2.2.1.3.7")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.2.3.1", "3.2.2.2.3.3", "3.2.2.2.3.5", "3.2.2.2.3.7")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.3.3.1", "3.2.2.3.3.3", "3.2.2.3.3.5", "3.2.2.3.3.7")
         , name = "Firm - Large Enterprise or Corporation"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.1.3.2", "3.2.2.1.3.4", "3.2.2.1.3.6", "3.2.2.1.3.8")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.2.3.2", "3.2.2.2.3.4", "3.2.2.2.3.6", "3.2.2.2.3.8")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.3.3.2", "3.2.2.3.3.4", "3.2.2.3.3.6", "3.2.2.3.3.8")
         , name = "Firm - Large Enterprise or Corporation"
         ##(Type of Service): Production Support Services##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.1.3.1", "3.2.3.1.3.3", "3.2.3.1.3.5", "3.2.3.1.3.7")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.2.3.1", "3.2.3.2.3.3", "3.2.3.2.3.5", "3.2.3.2.3.7")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.3.3.1", "3.2.3.3.3.3", "3.2.3.3.3.5", "3.2.3.3.3.7")
         , name = "Firm - Large Enterprise or Corporation"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.1.3.2", "3.2.3.1.3.4", "3.2.3.1.3.6", "3.2.3.1.3.8")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.2.3.2", "3.2.3.2.3.4", "3.2.3.2.3.6", "3.2.3.2.3.8")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.3.3.2", "3.2.3.3.3.4", "3.2.3.3.3.6", "3.2.3.3.3.8")
         , name = "Firm - Large Enterprise or Corporation"
         ##(Type of Service): Disaggregates Not Available##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.1.3.1", "3.2.4.1.3.3", "3.2.4.1.3.5", "3.2.4.1.3.7")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.2.3.1", "3.2.4.2.3.3", "3.2.4.2.3.5", "3.2.4.2.3.7")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.3.3.1", "3.2.4.3.3.3", "3.2.4.3.3.5", "3.2.4.3.3.7")
         , name = "Firm - Large Enterprise or Corporation"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.1.3.2", "3.2.4.1.3.4", "3.2.4.1.3.6", "3.2.4.1.3.8")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.2.3.2", "3.2.4.2.3.4", "3.2.4.2.3.6", "3.2.4.2.3.8")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.3.3.2", "3.2.4.3.3.4", "3.2.4.3.3.6", "3.2.4.3.3.8")
         , name = "Firm - Large Enterprise or Corporation"
         ##Commodity##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.4.1", "3.3.1.4.4", "3.3.1.4.7", "3.3.1.4.10")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.2.4.1", "3.3.2.4.4", "3.3.2.4.7", "3.3.2.4.10")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.3.4.1", "3.3.3.4.4", "3.3.3.4.7", "3.3.3.4.10")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.4.4.1", "3.3.4.4.4", "3.3.4.4.7", "3.3.4.4.10")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.5.4.1", "3.3.5.4.4", "3.3.5.4.7", "3.3.5.4.10")
         , name = "Firm - Large Enterprise or Corporation"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.4.2", "3.3.1.4.5", "3.3.1.4.8", "3.3.1.4.11")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.2.4.2", "3.3.2.4.5", "3.3.2.4.8", "3.3.2.4.11")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.3.4.2", "3.3.3.4.5", "3.3.3.4.8", "3.3.3.4.11")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.4.4.2", "3.3.4.4.5", "3.3.4.4.8", "3.3.4.4.11")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.5.4.2", "3.3.5.4.5", "3.3.5.4.8", "3.3.5.4.11")
         , name = "Firm - Large Enterprise or Corporation"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.4.3", "3.3.1.4.6", "3.3.1.4.9", "3.3.1.4.12")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.2.4.3", "3.3.2.4.6", "3.3.2.4.9", "3.3.2.4.12")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.3.4.3", "3.3.3.4.6", "3.3.3.4.9", "3.3.3.4.12")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.4.4.3", "3.3.4.4.6", "3.3.4.4.9", "3.3.4.4.12")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.5.4.3", "3.3.5.4.6", "3.3.5.4.9", "3.3.5.4.12")
         , name = "Firm - Large Enterprise or Corporation"
         ##EG.3.2-26 (Value Type of Product, Service, or Commodity Age)##
         ##(Type of Product) Inputs: Seeds and Planting Material##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.4.1", "3.1.1.1.4.3", "3.1.1.1.4.5", "3.1.1.1.4.7")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.2.4.1", "3.1.1.2.4.3", "3.1.1.2.4.5", "3.1.1.2.4.7")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.3.4.1", "3.1.1.3.4.3", "3.1.1.3.4.5", "3.1.1.3.4.7")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.4.4.1", "3.1.1.4.4.3", "3.1.1.4.4.5", "3.1.1.4.4.7")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.5.4.1", "3.1.1.5.4.3", "3.1.1.5.4.5", "3.1.1.5.4.7")
         , name = "Firm - Large Enterprise or Corporation"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.1.4.2", "3.1.1.1.4.4", "3.1.1.1.4.6", "3.1.1.1.4.8")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.2.4.2", "3.1.1.2.4.4", "3.1.1.2.4.6", "3.1.1.2.4.8")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.3.4.2", "3.1.1.3.4.4", "3.1.1.3.4.6", "3.1.1.3.4.8")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.4.4.2", "3.1.1.4.4.4", "3.1.1.4.4.6", "3.1.1.4.4.8")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.1.5.4.2", "3.1.1.5.4.4", "3.1.1.5.4.6", "3.1.1.5.4.8")
         , name = "Firm - Large Enterprise or Corporation"
         ##(Type of Product): Inputs: Other Non-Durable Inputs, Such as Fertilizers and Pesticides##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.2.1.4.1", "3.1.2.1.4.3", "3.1.2.1.4.5", "3.1.2.1.4.7")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.2.2.4.1", "3.1.2.2.4.3", "3.1.2.2.4.5", "3.1.2.2.4.7")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.2.3.4.1", "3.1.2.3.4.3", "3.1.2.3.4.5", "3.1.2.3.4.7")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.2.4.4.1", "3.1.2.4.4.3", "3.1.2.4.4.5", "3.1.2.4.4.7")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.2.5.4.1", "3.1.2.5.4.3", "3.1.2.5.4.5", "3.1.2.5.4.7")
         , name = "Firm - Large Enterprise or Corporation"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.2.1.4.2", "3.1.2.1.4.4", "3.1.2.1.4.6", "3.1.2.1.4.8")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.2.2.4.2", "3.1.2.2.4.4", "3.1.2.2.4.6", "3.1.2.2.4.8")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.2.3.4.2", "3.1.2.3.4.4", "3.1.2.3.4.6", "3.1.2.3.4.8")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.2.4.4.2", "3.1.2.4.4.4", "3.1.2.4.4.6", "3.1.2.4.4.8")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.2.5.4.2", "3.1.2.5.4.4", "3.1.2.5.4.6", "3.1.2.5.4.8")
         , name = "Firm - Large Enterprise or Corporation"
         ##(Type of Product): Inputs: Durable Equipment and Machinery##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.3.1.4.1", "3.1.3.1.4.3", "3.1.3.1.4.5", "3.1.3.1.4.7")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.3.2.4.1", "3.1.3.2.4.3", "3.1.3.2.4.5", "3.1.3.2.4.7")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.3.3.4.1", "3.1.3.3.4.3", "3.1.3.3.4.5", "3.1.3.3.4.7")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.3.4.4.1", "3.1.3.4.4.3", "3.1.3.4.4.5", "3.1.3.4.4.7")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.3.5.4.1", "3.1.3.5.4.3", "3.1.3.5.4.5", "3.1.3.5.4.7")
         , name = "Firm - Large Enterprise or Corporation"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.3.1.4.2", "3.1.3.1.4.4", "3.1.3.1.4.6", "3.1.3.1.4.8")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.3.2.4.2", "3.1.3.2.4.4", "3.1.3.2.4.6", "3.1.3.2.4.8")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.3.3.4.2", "3.1.3.3.4.4", "3.1.3.3.4.6", "3.1.3.3.4.8")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.3.4.4.2", "3.1.3.4.4.4", "3.1.3.4.4.6", "3.1.3.4.4.8")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.3.5.4.2", "3.1.3.5.4.4", "3.1.3.5.4.6", "3.1.3.5.4.8")
         , name = "Firm - Large Enterprise or Corporation"
         ##(Type of Product): Processed Products/Value Added Products (Post Harvest)##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.4.1.4.1", "3.1.4.1.4.3", "3.1.4.1.4.5", "3.1.4.1.4.7")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.4.2.4.1", "3.1.4.2.4.3", "3.1.4.2.4.5", "3.1.4.2.4.7")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.4.3.4.1", "3.1.4.3.4.3", "3.1.4.3.4.5", "3.1.4.3.4.7")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.4.4.4.1", "3.1.4.4.4.3", "3.1.4.4.4.5", "3.1.4.4.4.7")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.4.5.4.1", "3.1.4.5.4.3", "3.1.4.5.4.5", "3.1.4.5.4.7")
         , name = "Firm - Large Enterprise or Corporation"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.4.1.4.2", "3.1.4.1.4.4", "3.1.4.1.4.6", "3.1.4.1.4.8")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.4.2.4.2", "3.1.4.2.4.4", "3.1.4.2.4.6", "3.1.4.2.4.8")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.4.3.4.2", "3.1.4.3.4.4", "3.1.4.3.4.6", "3.1.4.3.4.8")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.4.4.4.2", "3.1.4.4.4.4", "3.1.4.4.4.6", "3.1.4.4.4.8")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.4.5.4.2", "3.1.4.5.4.4", "3.1.4.5.4.6", "3.1.4.5.4.8")
         , name = "Firm - Large Enterprise or Corporation"
         ##(Type of Product): Post-Harvest Storage and Processing Equipment##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.5.1.4.1", "3.1.5.1.4.3", "3.1.5.1.4.5", "3.1.5.1.4.7")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.5.2.4.1", "3.1.5.2.4.3", "3.1.5.2.4.5", "3.1.5.2.4.7")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.5.3.4.1", "3.1.5.3.4.3", "3.1.5.3.4.5", "3.1.5.3.4.7")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.5.4.4.1", "3.1.5.4.4.3", "3.1.5.4.4.5", "3.1.5.4.4.7")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.5.5.4.1", "3.1.5.5.4.3", "3.1.5.5.4.5", "3.1.5.5.4.7")
         , name = "Firm - Large Enterprise or Corporation"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.5.1.4.2", "3.1.5.1.4.4", "3.1.5.1.4.6", "3.1.5.1.4.8")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.5.2.4.2", "3.1.5.2.4.4", "3.1.5.2.4.6", "3.1.5.2.4.8")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.5.3.4.2", "3.1.5.3.4.4", "3.1.5.3.4.6", "3.1.5.3.4.8")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.5.4.4.2", "3.1.5.4.4.4", "3.1.5.4.4.6", "3.1.5.4.4.8")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.5.5.4.2", "3.1.5.5.4.4", "3.1.5.5.4.6", "3.1.5.5.4.8")
         , name = "Firm - Large Enterprise or Corporation"
         ##(Type of Product): Disaggregates Not Available##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.6.1.4.1", "3.1.6.1.4.3", "3.1.6.1.4.5", "3.1.6.1.4.7")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.6.2.4.1", "3.1.6.2.4.3", "3.1.6.2.4.5", "3.1.6.2.4.7")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.6.3.4.1", "3.1.6.3.4.3", "3.1.6.3.4.5", "3.1.6.3.4.7")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.6.4.4.1", "3.1.6.4.4.3", "3.1.6.4.4.5", "3.1.6.4.4.7")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.6.5.4.1", "3.1.6.5.4.3", "3.1.6.5.4.5", "3.1.6.5.4.7")
         , name = "Firm - Large Enterprise or Corporation"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.6.1.4.2", "3.1.6.1.4.4", "3.1.6.1.4.6", "3.1.6.1.4.8")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.6.2.4.2", "3.1.6.2.4.4", "3.1.6.2.4.6", "3.1.6.2.4.8")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.6.3.4.2", "3.1.6.3.4.4", "3.1.6.3.4.6", "3.1.6.3.4.8")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.6.4.4.2", "3.1.6.4.4.4", "3.1.6.4.4.6", "3.1.6.4.4.8")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.1.6.5.4.2", "3.1.6.5.4.4", "3.1.6.5.4.6", "3.1.6.5.4.8")
         , name = "Firm - Large Enterprise or Corporation"
         ##(Type of Service): Business Services, Including Financial, Entrepreneurial, Legal, etc.##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.1.4.1", "3.2.1.1.4.3", "3.2.1.1.4.5", "3.2.1.1.4.7")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.2.4.1", "3.2.1.2.4.3", "3.2.1.2.4.5", "3.2.1.2.4.7")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.3.4.1", "3.2.1.3.4.3", "3.2.1.3.4.5", "3.2.1.3.4.7")
         , name = "Firm - Large Enterprise or Corporation"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.1.4.2", "3.2.1.1.4.4", "3.2.1.1.4.6", "3.2.1.1.4.8")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.2.4.2", "3.2.1.2.4.4", "3.2.1.2.4.6", "3.2.1.2.4.8")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.1.3.4.2", "3.2.1.3.4.4", "3.2.1.3.4.6", "3.2.1.3.4.8")
         , name = "Firm - Large Enterprise or Corporation"
         ##(Type of Service): Information Services:  SMS, Radio, TV, Print, etc.##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.1.4.1", "3.2.2.1.4.3", "3.2.2.1.4.5", "3.2.2.1.4.7")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.2.4.1", "3.2.2.2.4.3", "3.2.2.2.4.5", "3.2.2.2.4.7")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.3.4.1", "3.2.2.3.4.3", "3.2.2.3.4.5", "3.2.2.3.4.7")
         , name = "Firm - Large Enterprise or Corporation"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.1.4.2", "3.2.2.1.4.4", "3.2.2.1.4.6", "3.2.2.1.4.8")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.2.4.2", "3.2.2.2.4.4", "3.2.2.2.4.6", "3.2.2.2.4.8")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.2.3.4.2", "3.2.2.3.4.4", "3.2.2.3.4.6", "3.2.2.3.4.8")
         , name = "Firm - Large Enterprise or Corporation"
         ##(Type of Service): Production Support Services##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.1.4.1", "3.2.3.1.4.3", "3.2.3.1.4.5", "3.2.3.1.4.7")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.2.4.1", "3.2.3.2.4.3", "3.2.3.2.4.5", "3.2.3.2.4.7")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.3.4.1", "3.2.3.3.4.3", "3.2.3.3.4.5", "3.2.3.3.4.7")
         , name = "Firm - Large Enterprise or Corporation"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.1.4.2", "3.2.3.1.4.4", "3.2.3.1.4.6", "3.2.3.1.4.8")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.2.4.2", "3.2.3.2.4.4", "3.2.3.2.4.6", "3.2.3.2.4.8")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.3.3.4.2", "3.2.3.3.4.4", "3.2.3.3.4.6", "3.2.3.3.4.8")
         , name = "Firm - Large Enterprise or Corporation"
         ##(Type of Service): Disaggregates Not Available##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.1.4.1", "3.2.4.1.4.3", "3.2.4.1.4.5", "3.2.4.1.4.7")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.2.4.1", "3.2.4.2.4.3", "3.2.4.2.4.5", "3.2.4.2.4.7")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.3.4.1", "3.2.4.3.4.3", "3.2.4.3.4.5", "3.2.4.3.4.7")
         , name = "Firm - Large Enterprise or Corporation"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.1.4.2", "3.2.4.1.4.4", "3.2.4.1.4.6", "3.2.4.1.4.8")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.2.4.2", "3.2.4.2.4.4", "3.2.4.2.4.6", "3.2.4.2.4.8")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.2.4.3.4.2", "3.2.4.3.4.4", "3.2.4.3.4.6", "3.2.4.3.4.8")
         , name = "Firm - Large Enterprise or Corporation"
         ##Commodity##
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.5.1", "3.3.1.5.4", "3.3.1.5.7", "3.3.1.5.10")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.2.5.1", "3.3.2.5.4", "3.3.2.5.7", "3.3.2.5.10")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.3.5.1", "3.3.3.5.4", "3.3.3.5.7", "3.3.3.5.10")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.4.5.1", "3.3.4.5.4", "3.3.4.5.7", "3.3.4.5.10")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.5.5.1", "3.3.5.5.4", "3.3.5.5.7", "3.3.5.5.10")
         , name = "Firm - Large Enterprise or Corporation"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.5.2", "3.3.1.5.5", "3.3.1.5.8", "3.3.1.5.11")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.2.5.2", "3.3.2.5.5", "3.3.2.5.8", "3.3.2.5.11")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.3.5.2", "3.3.3.5.5", "3.3.3.5.8", "3.3.3.5.11")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.4.5.2", "3.3.4.5.5", "3.3.4.5.8", "3.3.4.5.11")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.5.5.2", "3.3.5.5.5", "3.3.5.5.8", "3.3.5.5.11")
         , name = "Firm - Large Enterprise or Corporation"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.1.5.3", "3.3.1.5.6", "3.3.1.5.9", "3.3.1.5.12")
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.2.5.3", "3.3.2.5.6", "3.3.2.5.9", "3.3.2.5.12")
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.3.5.3", "3.3.3.5.6", "3.3.3.5.9", "3.3.3.5.12")
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.4.5.3", "3.3.4.5.6", "3.3.4.5.9", "3.3.4.5.12")
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , udn = c("3.3.5.5.3", "3.3.5.5.6", "3.3.5.5.9", "3.3.5.5.12")
         , name = "Firm - Large Enterprise or Corporation"
         ##FTFMS##
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d2 = "Producer - smallholder"
         , name = "Producer - Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d2 = "Producer - non-smallholder"
         , name = "Producer - Non-Smallholder"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d2 = "Firm - microenterprise"
         , name = "Firm - Microenterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d2 = "Firm - Small and medium enterprise"
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d2 = "Firm - Small & Medium enterprise"
         , name = "Firm - Small and Medium Enterprise"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d2 = "Firm - Large enterprise or corporation"
         , name = "Firm - Large Enterprise or Corporation"
  ),
  ##EG.3.2-27##
  ##Size of recipient(s)##

  tibble(ic = "EG.3.2-27"
         , udn = c("3.1.1.1", "3.1.1.2", "3.1.1.3", "3.1.1.4"
                   , "3.2.1.1", "3.2.1.2", "3.2.1.3", "3.2.1.4"
                   , "3.3.1.1", "3.3.1.2", "3.3.1.3", "3.3.1.4"
                   , "3.4.1.1", "3.4.1.2", "3.4.1.3", "3.4.1.4")
         , ms_d2 = "Size of recipient(s)"
         , name = "Size of recipient(s)"
         ##Sex of recipient(s)##
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.1.2.1", "3.1.2.2", "3.1.2.3", "3.1.2.4"
                   , "3.2.2.1", "3.2.2.2", "3.2.2.3", "3.2.2.4"
                   , "3.3.2.1", "3.3.2.2", "3.3.2.3", "3.3.2.4"
                   , "3.4.2.1", "3.4.2.2", "3.4.2.3", "3.4.2.4")
         , ms_d2 = "Sex of recipient(s)"
         , name = "Sex of recipient(s)"
         ##Age of recipient(s)##
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.1.3.1", "3.1.3.2", "3.1.3.3", "3.1.3.4"
                   , "3.2.3.1", "3.2.3.2", "3.2.3.3", "3.2.3.4"
                   , "3.3.3.1", "3.3.3.2", "3.3.3.3", "3.3.3.4"
                   , "3.4.3.1", "3.4.3.2", "3.4.3.3", "3.4.3.4")
         , ms_d2 = "Age of recipient(s)"
         , name = "Age of recipient(s)"
         ##VALUE of financing received by recipient Size, Sex, and Age##
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.5.1.1.1", "3.5.1.1.2", "3.5.1.1.3", "3.5.1.1.4"
                   , "3.5.1.2.1", "3.5.1.2.2", "3.5.1.2.3", "3.5.1.2.4"
                   , "3.5.1.3.1", "3.5.1.3.2", "3.5.1.3.3", "3.5.1.3.4"
                   , "3.6.1.1.1", "3.6.1.1.2", "3.6.1.1.3", "3.6.1.1.4"
                   , "3.6.1.2.1", "3.6.1.2.2", "3.6.1.2.3", "3.6.1.2.4"
                   , "3.6.1.3.1", "3.6.1.3.2", "3.6.1.3.3", "3.6.1.3.4"
                   , "3.7.1.1.1", "3.7.1.1.2", "3.7.1.1.3", "3.7.1.1.4"
                   , "3.7.1.2.1", "3.7.1.2.2", "3.7.1.2.3", "3.7.1.2.4"
                   , "3.7.1.3.1", "3.7.1.3.2", "3.7.1.3.3", "3.7.1.3.4")
         , ms_d2 = "VALUE of financing received by recipient Size, Sex, and Age:"
         , name = "Value of financing received"
         ##NUMBER of financing recipients by Size, Sex, and Age##
  ),
  tibble(ic = "EG.3.2-27"
         , udn = c("3.5.2.1.1", "3.5.2.1.2", "3.5.2.1.3", "3.5.2.1.4"
                   , "3.5.2.2.1", "3.5.2.2.2", "3.5.2.2.3", "3.5.2.2.4"
                   , "3.5.2.3.1", "3.5.2.3.2", "3.5.2.3.3", "3.5.2.3.4"
                   , "3.6.2.1.1", "3.6.2.1.2", "3.6.2.1.3", "3.6.2.1.4"
                   , "3.6.2.2.1", "3.6.2.2.2", "3.6.2.2.3", "3.6.2.2.4"
                   , "3.6.2.3.1", "3.6.2.3.2", "3.6.2.3.3", "3.6.2.3.4"
                   , "3.7.2.1.1", "3.7.2.1.2", "3.7.2.1.3", "3.7.2.1.4"
                   , "3.7.2.2.1", "3.7.2.2.2", "3.7.2.2.3", "3.7.2.2.4"
                   , "3.7.2.3.1", "3.7.2.3.2", "3.7.2.3.3", "3.7.2.3.4")
         , ms_d2 = "NUMBER of financing recipients by Size, Sex, and Age:"
         , name = "Number of financing recipients"
         ##EG.3.2-x17 (Number of Individuals Applying Promoted Practices)##
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d2 = "Commodity"
         , name = "Commodity"
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d2 = "Sex"
         , name = "Sex (No Double-Counting)"
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d2 = "Technology type"
         , name = "Management Practice or Tech Type (double-counting allowed)"
  ),
  tibble(ic = "EG.3.2-x17"
         , ms_d2 = "Duration"
         , name = "Duration"
         ##EG.3.2-x18 (Hectares Under Improved Management)##
  ),
  tibble(ic = "EG.3.2-x18"
         , ms_d1 ="Technology type"
         , name = "Management Practice or Tech Type"
  ),
  tibble(ic = "EG.3.2-x18"
         , name = "ms_d1"
         ##EG.3.2-x19 (Value)##
  ),
  tibble(ic = "EG.3.2-x19"
         , name = "Producer - Smallholder (Row Label Implied)"
         ##EG.3.3-10 (Percentage and Number by Age)##
         ##Age Less than 19 Years##
  ),
  tibble(ic = "EG.3.3-10"
         , udn = c("3.1.1.1", "3.1.1.2")
         , ms_d2 = "Age <19 years"
         , name = "Age Less Than 19 Years"
         ##Age 19+ Years##
  ),
  tibble(ic = "EG.3.3-10"
         , udn = c("3.1.2.1", "3.1.2.2")
         , ms_d2 = "Age 19+ years"
         , name = "Age 19+ Years"
         ##DNA##
  ),
  tibble(ic = "EG.3.3-10"
         , udn = c("3.1.3.1", "3.1.3.2")
         , ms_d2 = "Disaggregate Not Available (for age category of female participants)"
         , name = "Disaggregates Not Available"
         ##EG.4.2-7 (Participants by Sex)##
         ##Females##
  ),
  tibble(ic = "EG.4.2-7"
         , udn ="3.1.2"
         , name = "Female"
         ##Males##
  ),
  tibble(ic = "EG.4.2-7", udn ="3.1.1"
         , name = "Male"
         ##DNA##
  ),
  tibble(ic = "EG.4.2-7", udn ="3.1.3"
         , name = "Disaggregates Not Available"
         ##15-29##
  ),
  tibble(ic = "EG.4.2-7", udn ="3.2.1"
         , name = "15-29"
         ##30+##
  ),
  tibble(ic = "EG.4.2-7", udn ="3.2.2"
         , name = "30+"
         ##DNA##
  ),
  tibble(ic = "EG.4.2-7", udn ="3.2.3"
         , name = "Disaggregates Not Available"
         ##Savings##
  ),
  tibble(ic = "EG.4.2-7", udn ="3.3.1"
         , name = "Savings"
         ##Credit##
  ),
  tibble(ic = "EG.4.2-7", udn ="3.3.2"
         , name = "Credit"
         ##DNA##
  ),
  tibble(ic = "EG.4.2-7", udn ="3.3.3"
         , name = "Disaggregates Not Available "
         ##New##
  ),
  tibble(ic = "EG.4.2-7", udn ="3.4.1"
         , name = "New"
         ##Continuing##
  ),
  tibble(ic = "EG.4.2-7", udn ="3.4.2"
         , name = "Continuing"
         ##DNA##
  ),
  tibble(ic = "EG.4.2-7", udn ="3.4.3"
         , name = "Disaggregates Not Available "
         ##EG.10.4-7 (Resource Type)##
         ##Sex##
  ),
  tibble(ic = "EG.10.4-7"
         , udn = c("3.1.1.2", "3.1.1.1", "3.1.1.3", "3.2.1.2", "3.2.1.1", "3.2.1.3")
         , name = "Sex"
  ),
  tibble(ic = "EG.10.4-7"
         , ms_d2 = "Sex"
         , name = "Sex"
         ##Type of Documentation##
  ),
  tibble(ic = "EG.10.4-7"
         , udn = c("3.1.2.1", "3.1.2.2", "3.1.2.3", "3.1.2.4", "3.1.2.5"
                   , "3.2.2.1", "3.2.2.2", "3.2.2.3", "3.2.2.4", "3.2.2.5")
         , name = "Type of Documentation"
  ),
  tibble(ic = "EG.10.4-7"
         , ms_d2 = "Type of Documentation"
         , name = "Type of Documentation"
         ##Location##
  ),
  tibble(ic = "EG.10.4-7"
         , udn = c("3.1.3.1", "3.1.3.2", "3.1.3.3", "3.2.3.1", "3.2.3.2", "3.2.3.3")
         , name = "Location"
  ),
  tibble(ic = "EG.10.4-7"
         , ms_d2 = "Location"
         , name = "Location"
         ##EG.10.4-8 (Participants by Resource Type)##
         ##Sex##
  ),
  tibble(ic = "EG.10.4-8"
         , udn = c("3.1.1.1", "3.1.1.2", "3.1.1.3", "3.2.1.1", "3.2.1.2", "3.2.1.3")
         , name = "Sex"
  ),
  tibble(ic = "EG.10.4-8"
         , ms_d2 = "Sex"
         , name = "Sex"
         ##Tenure Type##
  ),
  tibble(ic = "EG.10.4-8"
         , udn = c("3.1.2.1", "3.1.2.2", "3.1.2.3", "3.1.2.4", "3.1.2.5"
                   , "3.1.2.6", "3.1.2.7", "3.1.2.8", "3.2.2.1", "3.2.2.2"
                   , "3.2.2.3", "3.2.2.4", "3.2.2.5", "3.2.2.6", "3.2.2.7"
                   , "3.2.2.8")
         , name = "Tenure Type"
  ),
  tibble(ic = "EG.10.4-8"
         , ms_d2 = "Tenure Type"
         , name = "Tenure Type"
         ##Location##
  ),
  tibble(ic = "EG.10.4-8"
         , udn = c("3.1.3.1", "3.1.3.2", "3.1.3.3", "3.2.3.1", "3.2.3.2", "3.2.3.3")
         , name = "Location"
  ),
  tibble(ic = "EG.10.4-8"
         , ms_d2 = "Location"
         , name = "Location"
         ##ES.5-1 (Participants by Sex)##
         ##Females##
  ),
  tibble(ic = "ES.5-1", udn ="3.1.2"
         , name = "Female"
         ##Males##
  ),
  tibble(ic = "ES.5-1", udn ="3.1.1"
         , name = "Male"
         ##DNA##
  ),
  tibble(ic = "ES.5-1", udn ="3.1.3"
         , name = "Disaggregates Not Available"
         ##15-29##
  ),
  tibble(ic = "ES.5-1", udn ="3.2.1"
         , name = "15-29"
         ##30+##
  ),
  tibble(ic = "ES.5-1", udn ="3.2.2"
         , name = "30+"
         ##DNA##
  ),
  tibble(ic = "ES.5-1", udn ="3.2.3"
         , name = "Disaggregates Not Available"
         ##New##
  ),
  tibble(ic = "ES.5-1", udn ="3.3.1"
         , name = "New"
         ##Continuing##
  ),
  tibble(ic = "ES.5-1", udn ="3.3.2"
         , name = "Continuing"
         ##DNA##
  ),
  tibble(ic = "ES.5-1", udn ="3.3.3"
         , name = "Disaggregates Not Available"
         ##Community Asset##
  ),
  tibble(ic = "ES.5-1", udn ="3.4.1"
         , name = "Community Asset"
         ##Human Assets/Capital##
  ),
  tibble(ic = "ES.5-1", udn ="3.4.2"
         , name = "Human Assets/Capital"
         ##Household Assets##
  ),
  tibble(ic = "ES.5-1", udn ="3.4.3"
         , name = "Household Assets"
         ##DNA##
  ),
  tibble(ic = "ES.5-1", udn ="3.4.4"
         , name = "Disaggregation Not Available"
         ##HL.8.2-2 (Participants by Sex)##
         ##Females##
  ),
  tibble(ic = "HL.8.2-2", udn ="3.1.2"
         , name = "Female"
         ##Males##
  ),
  tibble(ic = "HL.8.2-2", udn ="3.1.1"
         , name = "Male"
         ##DNA##
  ),
  tibble(ic = "HL.8.2-2", udn ="3.1.3"
         , name = "Disaggregates Not Available"
         ##Urban##
  ),
  tibble(ic = "HL.8.2-2", udn ="3.2.1"
         , name = "Urban"
         ##Rural##
  ),
  tibble(ic = "HL.8.2-2", udn ="3.2.2"
         , name = "Rural"
         ##DNA##
  ),
  tibble(ic = "HL.8.2-2", udn ="3.2.3"
         , name = "Disaggregates Not Available"
         ##1st Quintile##
  ),
  tibble(ic = "HL.8.2-2", udn ="3.3.1"
         , name = "1st Wealth Quintile"
         ##2nd Quintile##
  ),
  tibble(ic = "HL.8.2-2", udn ="3.3.2"
         , name = "2nd Wealth Quintile"
         ##3rd Quintile##
  ),
  tibble(ic = "HL.8.2-2", udn ="3.3.3"
         , name = "3rd Wealth Quintile"
         ##4th Quintile##
  ),
  tibble(ic = "HL.8.2-2", udn ="3.3.4"
         , name = "4th Wealth Quintile"
         ##5th Quintile##
  ),
  tibble(ic = "HL.8.2-2", udn ="3.3.5"
         , name = "5th Wealth Quintile"
         ##DNA##
  ),
  tibble(ic = "HL.8.2-2", udn ="3.3.6"
         , name = "Disaggregates Not Available"
         ##HL.8.2-5 (Percent by Residence Location)##
         ##Number of Households Where Both Water and Soap are Found at the Commonly Used Handwashing Station##
  ),
  tibble(ic = "HL.8.2-5"
         , udn = c("3.2.1.1", "3.2.2.1", "3.2.3.1")
         , name = "Number of Households Where Both Water and Soap are Found at the Commonly Used Handwashing Station"
  ),
  tibble(ic = "HL.8.2-5"
         , ms_d3 = c( "Rural - number of households where both water and soap are found at the commonly used handwashing station"
                      , "Urban - number of households where both water and soap are found at the commonly used handwashing station"
                      , "Disaggregates Not Available (for Residence Location) - number of households where both water and soap are found at the commonly used handwashing station")
         , name = "Number of Households Where Both Water and Soap are Found at the Commonly Used Handwashing Station"
         ##Total Number of Households Covered by the Handwashing Behavior change Intervention##
  ),
  tibble(ic = "HL.8.2-5"
         , udn = c("3.2.1.2", "3.2.2.2", "3.2.3.2")
         , name = "Total Number of Households Covered by the Handwashing Behavior Shange Intervention"
  ),
  tibble(ic = "HL.8.2-5"
         , ms_d3 = c( "Rural - total number of households covered by the handwashing behavior change intervention", "Urban - total number of households covered by the handwashing behavior change intervention", "Disaggregates Not Available (for Residence Location)  - total number of households covered by the handwashing behavior change intervention")
         , name = "Number of Households Where Both Water and Soap are Found at the Commonly Used Handwashing Station"
         ##HL.9-1 (Participants by Sex)##
         ##Females##
  ),
  tibble(ic = "HL.9-1", udn ="3.1.2"
         , name = "Female"
         ##Males##
  ),
  tibble(ic = "HL.9-1", udn ="3.1.1"
         , name = "Male"
         ##DNA##
  ),
  tibble(ic = "HL.9-1", udn ="3.1.3"
         , name = "Disaggregates Not Available"
         ##FTFMS##
  ),
  tibble(ic = "HL.9-1"
         , ms_d1 ="Sex (no double-counting allowed)"
         , name = "ms_d2"
         ##Number of children under five whose parents/caretakers received behavior change communication interventions that promote essential infant and young child feeding behaviors##
  ),
  tibble(ic = "HL.9-1", udn ="3.2.1"
         , name = "Number of Children Under Five Whose Parents/Caretakers Received Behavior Change Communication Interventions that Promote Essential Infant and Young Child Feeding Behaviors"
  ),
  tibble(ic = "HL.9-1"
         , ms_d2 = "Number of children under five whose parents/caretakers received behavior change communication interventions that promote essential infant and young child feeding behaviors"
         , name = "Number of Children Under Five Whose Parents/Caretakers Received Behavior Change Communication Interventions that Promote Essential Infant and Young Child Feeding Behaviors"
         ##Number of children 6-59 months who received vitamin A supplementation in the past 6 months##
  ),
  tibble(ic = "HL.9-1", udn ="3.2.2"
         , name = "Number of Children 6-59 Months Who Received Vitamin A Supplementation in the Past 6 Months"
  ),
  tibble(ic = "HL.9-1"
         , ms_d2 = "Number of children 6-59 months  who received vitamin A supplementation in the past 6 months"
         , name = "Number of Children 6-59 Months Who Received Vitamin A Supplementation in the Past 6 Months"
         ##Number of children under five who received zinc supplementation during episode of diarrhea##
  ),
  tibble(ic = "HL.9-1", udn ="3.2.3"
         , name = "Number of Children Under Five Who Received Zinc Supplementation During Episode of Diarrhea"
  ),
  tibble(ic = "HL.9-1"
         , ms_d2 = "Number of children under five who received zinc supplementation during episode of diarrhea"
         , name = "Number of Children Under Five Who Received Zinc Supplementation During Episode of Diarrhea"
         ##Number of children under five who received Multiple Micronutrient Powder (MNP) supplementation##
  ),
  tibble(ic = "HL.9-1", udn ="3.2.4"
         , name = "Number of Children Under Five Who Received Multiple Micronutrient Powder (MNP) Supplementation"
  ),
  tibble(ic = "HL.9-1"
         , ms_d2 = "Number of children under five who received Multiple Micronutrient Powder (MNP) supplementation"
         , name = "Number of Children Under Five Who Received Multiple Micronutrient Powder (MNP) Supplementation"
         ##Number of children under five who were admitted for treatment of severe acute malnutrition##
  ),
  tibble(ic = "HL.9-1", udn ="3.2.5"
         , name = "Number of Children Under Five Who Were Admitted for Treatment of Severe Acute Malnutrition"
  ),
  tibble(ic = "HL.9-1"
         , ms_d2 = "Number of children under five who were admitted for treatment of severe acute malnutrition"
         , name = "Number of Children Under Five Who Were Admitted for Treatment of Severe Acute Malnutrition"
         ##Number of children under five who were admitted for treatment of moderate acute malnutrition##
  ),
  tibble(ic = "HL.9-1", udn ="3.2.6"
         , name = "Number of Children Under Five Who Were Admitted for Treatment of Moderate Acute Malnutrition"
  ),
  tibble(ic = "HL.9-1"
         , ms_d2 = "Number of children under five who were admitted for treatment of moderate acute malnutrition"
         , name = "Number of Children Under Five Who Were Admitted for Treatment of Moderate Acute Malnutrition"
         ##Number of children under five who received direct food assistance of fortified/specialized food products##
  ),
  tibble(ic = "HL.9-1", udn ="3.2.7"
         , name = "Number of Children Under Five Who Received Direct Food Assistance of Fortified/Specialized Food Products"
  ),
  tibble(ic = "HL.9-1"
         , ms_d2 = "Number of children under five who received direct food assistance of fortified/specialized food products"
         , name = "Number of Children Under Five Who Received Direct Food Assistance of Fortified/Specialized Food Products"
         ##HL.9-3 (Participants)##
         ##Women < 19##
  ),
  tibble(ic = "HL.9-3", udn ="3.1.1"
         , name = "Women < 19"
  ),
  tibble(ic = "HL.9-3"
         , ms_d2 = "Number of women < 19 years of age"
         , name = "Women < 19"
         ##Women >= 19##
  ),
  tibble(ic = "HL.9-3", udn ="3.1.2"
         , name = "Women >= 19"
  ),
  tibble(ic = "HL.9-3"
         , ms_d2 = "Number of women > or = 19 years of age"
         , name = "Women >= 19"
         ##DNA##
  ),
  tibble(ic = "HL.9-3", udn ="3.1.3"
         , name = "Disaggregates Not Available"
  ),
  tibble(ic = "HL.9-3"
         , ms_d2 = "Disaggregates Not Available"
         , name = "Disaggregates Not Available"
         ##Intervention##
         ##Number of Women Receiving Iron and Folic Acid Supplementation##
  ),
  tibble(ic = "HL.9-3", udn ="3.2.1"
         , name = "Number of Women Receiving Iron and Folic Acid Supplementation"
  ),
  tibble(ic = "HL.9-3"
         , ms_d2 = "Number of women receiving iron and folic acid supplementation"
         , name = "Number of Women Receiving Iron and Folic Acid Supplementation"
         ##Number of Women Receiving Counseling on Maternal and/or Child Nutrition##
  ),
  tibble(ic = "HL.9-3", udn ="3.2.2"
         , name = "Number of Women Receiving Counseling on Maternal and/or Child Nutrition"
  ),
  tibble(ic = "HL.9-3"
         , ms_d2 = "Number of women receiving counseling on maternal and/or child nutrition"
         , name = "Number of Women Receiving Counseling on Maternal and/or Child Nutrition"
         ##Number of Women Receiving Calcium Supplementation##
  ),
  tibble(ic = "HL.9-3", udn ="3.2.3"
         , name = "Number of Women Receiving Calcium Supplementation"
  ),
  tibble(ic = "HL.9-3"
         , ms_d2 = "Number of women receiving calcium supplementation"
         , name = "Number of Women Receiving Calcium Supplementation"
         ##Number of Women Receiving Multiple Micronutrient Supplementation##
  ),
  tibble(ic = "HL.9-3", udn ="3.2.4"
         , name = "Number of Women Receiving Multiple Micronutrient Supplementation"
  ),
  tibble(ic = "HL.9-3"
         , ms_d2 = "Number of women receiving multiple micronutrient supplementation"
         , name = "Number of Women Receiving Multiple Micronutrient Supplementation"
         ##Number of Women Receiving Direct Food Assistance of Fortified/Specialized Food Products##
  ),
  tibble(ic = "HL.9-3", udn ="3.2.5"
         , name = "Number of Women Receiving Direct Food Assistance of Fortified/Specialized Food Products"
  ),
  tibble(ic = "HL.9-3"
         , ms_d2 = "Number of women receiving direct food assistance of fortified/specialized food products"
         , name = "Number of Women Receiving Direct Food Assistance of Fortified/Specialized Food Products"
         ##HL.9-4 (Participants by Sex)##
         ##Females##
  ),
  tibble(ic = "HL.9-4", udn ="3.1.2"
         , name = "Female"
         ##Males##
  ),
  tibble(ic = "HL.9-4", udn ="3.1.1"
         , name = "Male"
         ##DNA##
  ),
  tibble(ic = "HL.9-4", udn ="3.1.3"
         , name = "Disaggregates Not Available"
         ##FTFMS##
  ),
  tibble(ic = "HL.9-4"
         , ms_d1 ="Sex"
         , name = "ms_d2"
         ##Non-degree seeking trainees##
  ),
  tibble(ic = "HL.9-4", udn ="3.2.1"
         , name = "Non-Degree Seeking Trainees"
  ),
  tibble(ic = "HL.9-4"
         , ms_d2 = "Non-degree seeking trainees"
         , name = "Non-Degree Seeking Trainees"
         ##Degree seeking trainees: New##
  ),
  tibble(ic = "HL.9-4", udn ="3.2.2"
         , name = "Degree Seeking Trainees: New"
  ),
  tibble(ic = "HL.9-4"
         , ms_d2 = "Degree seeking trainees: New"
         , name = "Degree Seeking Trainees: New"
         ##Degree seeking trainees: Continuing##
  ),
  tibble(ic = "HL.9-4", udn ="3.2.3"
         , name = "Degree Seeking Trainees: Continuing"
  ),
  tibble(ic = "HL.9-4"
         , ms_d2 = "Degree seeking trainees: Continuing"
         , name = "Degree Seeking Trainees: Continuing"
         ##Degree seeking trainees: Disaggregates Not Available##
  ),
  tibble(ic = "HL.9-4", udn ="3.2.4"
         , name = "Degree Seeking Trainees: Disaggregates Not Available"
  ),
  tibble(ic = "HL.9-4"
         , ms_d2 = "Degree seeking trainees: Disaggregates Not Available"
         , name = "Degree Seeking Trainees: Disaggregates Not Available"
         ##Disaggregates Not Available##
  ),
  tibble(ic = "HL.9-4", udn ="3.2.5"
         , name = "Disaggregates Not Available"
  ),
  tibble(ic = "HL.9-4"
         , ms_d2 = "Disaggregates Not Available"
         , name = "Disaggregates Not Available"
         ##RESIL-1 (Number of Plans by Type -  Double Counting)##
         ##Total Number of Plans##
  ),
  tibble(ic = "RESIL-1"
         , udn = c("3.1.1", "3.1.2")
         , name = "Total Number of Plans"
  ),
  tibble(system = "ftfms"
         , ic = "RESIL-1"
         , ms_d2 = c("Community - number of plans", "Government - number of plans")
         , name = "Total Number of Plans"
         ##Proposed##
  ),
  tibble(ic = "RESIL-1"
         , udn = c("3.1.1.1.1", "3.1.2.1.1")
         , name = "Proposed"
  ),
  tibble(system = "ftfms"
         , ic = "RESIL-1"
         ,ms_d4 = "Proposed"
         , name = "Proposed"
         ##Adopted##
  ),
  tibble(ic = "RESIL-1"
         , udn = c("3.1.1.1.2", "3.1.2.1.2")
         , name = "Adopted"
  ),
  tibble(system = "ftfms"
         , ic = "RESIL-1"
         , ms_d4 = "Adopted"
         , name = "Adopted"
         ##Implemented##
  ),
  tibble(ic = "RESIL-1"
         , udn = c("3.1.1.1.3", "3.1.2.1.3")
         , name = "Implemented"
  ),
  tibble(system = "ftfms", ic = "RESIL-1"
         , ms_d4 = "Implemented"
         , name = "Implemented"
         ##Institutionalized##
  ),
  tibble(ic = "RESIL-1"
         , udn = c("3.1.1.1.4", "3.1.2.1.4")
         , name = "Institutionalized"
  ),
  tibble(system = "ftfms", ic = "RESIL-1"
         , ms_d4 = "Institutionalized"
         , name = "Institutionalized"
         ##FTFMS Only##
  ),
  tibble(ic = c("EG.3.2-x20", "EG.3.2-x25", "EG.3.2-x27", "EG.3.2-x29"
                , "EG.3.2-x34", "EG.3.2-x37", "EG.3.2-x41", "EG.5.2-x1", "FTF-x01")
         , name = "ms_d2"
         ##Commodity##
  ),
  tibble(ic = c("EG.3.2-x23", "EG.3.3-x11")
         , name = "ms_d1"
  )
)

# First order ####
d1 <-  dplyr::bind_rows(
  ##CBLD-9##
  ##Education (Higher Education, Secondary, Primary)##
  tibble(ic = "CBLD-9"
         , udn =  c("3.3.1.1", "3.3.1.2")
         , ms_d2 = "Education (higher education, secondary, primary)"
         , name = "Education (Higher Education, Secondary, Primary)"
  ),
  ##Research Institution (Non-Degree Granting)##
  tibble(ic = "CBLD-9"
         , udn =  c("3.3.2.1", "3.3.2.2")
         , ms_d2 = "Research institutions (non-degree granting)"
         , name = "Research Institution (Non-Degree Granting)"
  ),
  ##Cooperative (Formal and Registered Private Sector Firm That Serves Members)##
  tibble(ic = "CBLD-9"
         , udn =  c("3.3.3.1", "3.3.3.2")
         , ms_d2 = "Cooperative (formal and registered private sector firm that serves members)"
         , name = "Cooperative (Formal and Registered Private Sector Firm That Serves Members)"
  ),
  ##Producer Group (Informal, Unregistered Group of Producers)##
  tibble(ic = "CBLD-9"
         , udn =  c("3.3.4.1", "3.3.4.2")
         , ms_d2 = "Producer Group (informal, unregistered group of producers)"
         , name = "Producer Group (Informal, Unregistered Group of Producers)"
  ),
  ##Faith-Based Organizations##
  tibble(ic = "CBLD-9"
         , udn =  c("3.3.5.1", "3.3.5.2")
         , ms_d2 = "Faith-based organizations"
         , name = "Faith-Based Organizations"
  ),
  ##Governmental Agencies (at National or Sub-National Levels)##
  tibble(ic = "CBLD-9"
         , udn =  c("3.3.6.1", "3.3.6.2")
         , ms_d2 = "Governmental agencies (at national or sub-national levels)"
         , name = "Governmental Agencies (at National or Sub-National Levels)"
  ),
  ##Health Service Delivery Sites (Hospital, Clinic, Community, Pharmacies)##
  tibble(ic = "CBLD-9"
         , udn =  c("3.3.7.1", "3.3.7.2")
         , ms_d2 = "Health service delivery sites (hospital, clinic, community, pharmacies)"
         , name = "Health Service Delivery Sites (Hospital, Clinic, Community, Pharmacies)"
  ),
  ##Private Sector Firms##
  tibble(ic = "CBLD-9"
         , udn =  c("3.3.8.1", "3.3.8.2")
         , ms_d2 = "Private sector firms"
         , name = "Private Sector Firms"
  ),
  ##Non-Governmental and Not-For Profit Organizations##
  tibble(ic = "CBLD-9"
         , udn =  c("3.3.9.1", "3.3.9.2")
         , ms_d2 = "Non-governmental and not-for profit organizations"
         , name = "Non-Governmental and Not-For Profit Organizations"
  ),
  ##Other##
  tibble(ic = "CBLD-9"
         , udn =  c("3.3.10.1", "3.3.10.2"), ms_d2 = "Other", name = "Other"
  ),
  ##EG-c##
  ##Percent of people living on less than $1.90/day (all household types together)##
  tibble(ic = "EG-c"
         , udn =  c("3.1.1", "3.1.2", "3.1.3", "3.1.4")
         , name = "Percent of People Living on Less Than $1.90/Day (All Household Types Together)"
  ),
  ##Number of people (all household types together)##
  tibble(ic = "EG-c"
         , udn =  c("3.2.1", "3.2.2", "3.2.3", "3.2.4")
         , name = "Number of People (All Household Types Together)"
  ),
  ##Household type: Male and Female Adults (M&F)##
  tibble(ic = "EG-c"
         , udn =  c("3.3.1.1", "3.3.1.2", "3.3.1.3", "3.3.1.4", "3.3.2.1"
                    , "3.3.2.2", "3.3.2.3", "3.3.2.4")
         , name = "Household Type: Male and Female Adults (M&F)"
  ),
  ##Household type: Adult Female no Adult Male (FNM)##
  tibble(ic = "EG-c"
         , udn =  c("3.4.1.1", "3.4.1.2", "3.4.1.3", "3.4.1.4", "3.4.2.1"
                    , "3.4.2.2", "3.4.2.3", "3.4.2.4")
         , name = "Household Type: Adult Female No Adult Male (FNM)"
  ),
  ##Household type: Adult Male no Adult Female (MNF)##
  tibble(ic = "EG-c"
         , udn =  c("3.5.1.1", "3.5.1.2", "3.5.1.3", "3.5.1.4", "3.5.2.1"
                    , "3.5.2.2", "3.5.2.3", "3.5.2.4")
         , name = "Household Type: Adult Male No Adult Female (MNF)"
  ),
  ##Household type: Child No Adults (CNA)##
  tibble(ic = "EG-c"
         , udn =  c("3.6.1.1", "3.6.1.2", "3.6.1.3", "3.6.1.4", "3.6.2.1"
                    , "3.6.2.2", "3.6.2.3", "3.6.2.4")
         , name = "Household Type: Child No Adults (CNA)"
  ),
  ##Household type: Disaggregates Not Available##
  tibble(ic = "EG-c"
         , udn =  c("3.7.1.1", "3.7.1.2", "3.7.1.3", "3.7.1.4", "3.7.2.1"
                    , "3.7.2.2", "3.7.2.3", "3.7.2.4")
         , name = "Household Type: Disaggregates Not Available"
  ),
  ##EG-d##
  ##Percent of People Living on <$1.90/Day in the Country (%)##
  tibble(ic = "EG-d", udn = "3.1"
         , name = "Percent of People Living on <$1.90/Day in the Country (%)"
  ),
  ##Total Number of People in the Country##
  tibble(ic = "EG-d"
         , udn = "3.2"
         , name = "Total Number of People in the Country"
  ),
  ##Household type: Male and Female Adults (M&F)##
  tibble(ic = "EG-d"
         , udn =  c("3.2.1.1", "3.2.1.2")
         , name = "Household Type: Male and Female Adults (M&F)"
  ),
  ##Household type: Adult Female no Adult Male (FNM)##
  tibble(ic = "EG-d"
         , udn =  c("3.2.2.1", "3.2.2.2")
         , name = "Household Type: Adult Female No Adult Male (FNM)"
  ),
  ##Household type: Adult Male no Adult Female (MNF)##
  tibble(ic = "EG-d"
         , udn =  c("3.2.3.1", "3.2.3.2")
         , name = "Household Type: Adult Male No Adult Female (MNF)"
  ),
  ##Household type: Child No Adults (CNA)##
  tibble(ic = "EG-d"
         , udn =  c("3.2.4.1", "3.2.4.2")
         , name = "Household Type: Child No Adults (CNA)"
  ),
  ##Household type: Disaggregates Not Available##
  tibble(ic = "EG-d"
         , udn =  c("3.2.5.1", "3.2.5.2")
         , name = "Household Type: Disaggregates Not Available"
  ),
  ##EG-e##
  ##Percent of moderate and severe food insecurity (all household types together)##
  tibble(ic = "EG-e"
         , udn =  c("3.1.1", "3.1.2", "3.1.3", "3.1.4")
         , name = "Percent of Moderate and Severe Food Insecurity (All Household Types Together)"
  ),
  ##Percent of moderate food insecurity (all household types together)##
  tibble(ic = "EG-e"
         , udn =  c("3.2.1", "3.2.2", "3.2.3", "3.2.4")
         , name = "Percent of Moderate Food Insecurity (All Household Types Together)"
  ),
  ##Percent of severe food insecurity (all household types together)##
  tibble(ic = "EG-e"
         , udn =  c("3.3.1", "3.3.2", "3.3.3", "3.3.4")
         , name = "Percent of Severe Food Insecurity (All Household Types Together)"
  ),
  ##Number of households (all household types together)##
  tibble(ic = "EG-e"
         , udn =  c("3.4.1", "3.4.2", "3.4.3", "3.4.4")
         , name = "Number of Households (All Household Types Together)"
  ),
  ##Household type: Male and Female Adults (M&F)##
  tibble(ic = "EG-e"
         , udn =  c("3.5.1.1", "3.5.1.2", "3.5.1.3", "3.5.1.4", "3.5.2.1"
                    , "3.5.2.2", "3.5.2.3", "3.5.2.4")
         , name = "Household Type: Male and Female Adults (M&F)"
  ),
  ##Household Type: Adult Female No Adult Male (FNM)##
  tibble(ic = "EG-e"
         , udn =  c("3.6.1.1", "3.6.1.2", "3.6.1.3", "3.6.1.4", "3.6.2.1"
                    , "3.6.2.2", "3.6.2.3", "3.6.2.4")
         , name = "Household Type: Adult Female No Adult Male (FNM)"
  ),
  ##Household Type: Adult Male No Adult Female (MNF)##
  tibble(ic = "EG-e"
         , udn =  c("3.7.1.1", "3.7.1.2", "3.7.1.3", "3.7.1.4", "3.7.2.1"
                    , "3.7.2.2", "3.7.2.3", "3.7.2.4")
         , name = "Household Type: Adult Male No Adult Female (MNF)"
  ),
  ##Household Type: Child No Adults (CNA)##
  tibble(ic = "EG-e"
         , udn =  c("3.8.1.1", "3.8.1.2", "3.8.1.3", "3.8.1.4", "3.8.2.1"
                    , "3.8.2.2", "3.8.2.3", "3.8.2.4")
         , name = "Household Type: Child No Adults (CNA)"
  ),
  ##Household Type: Disaggregates Not Available##
  tibble(ic = "EG-e"
         , udn =  c("3.9.1.1", "3.9.1.2", "3.9.1.3", "3.9.1.4", "3.9.2.1"
                    , "3.9.2.2", "3.9.2.3", "3.9.2.4")
         , name = "Household Type: Disaggregates Not Available"
  ),
  ##EG-f##
  ##Percent of People with Moderate and Severe Food Insecurity (All Household Types Together) in the Country (%)##
  tibble(ic = "EG-f"
         , udn =  "3.1"
         , name = "Percent of People with Moderate and Severe Food Insecurity (All Household Types Together) in the Country (%)"
  ),
  ##Percent of People with Moderate Food Insecurity (All Household Types Together) in the Country (%)##
  tibble(ic = "EG-f"
         , udn = "3.2"
         , name = "Percent of People with Moderate Food Insecurity (All Household Types Together) in the Country (%)"
  ),
  ##Percent of People with Severe Food Insecurity (All Household Types Together) in the Country (%)##
  tibble(ic = "EG-f"
         , udn =  "3.3"
         , name = "Percent of People with Severe Food Insecurity (All Household Types Together) in the Country (%)"
  ),
  ##Total Number of People (All Household Types Together) in the Country##
  tibble(ic = "EG-f"
         , udn =  "3.4"
         , name = "Total Number of People (All Household Types Together) in the Country"
  ),
  ##Gendered Household Type##
  tibble(ic = "EG-f"
         , udn =  c("3.5.1", "3.5.2")
         , name = "Household Type: Male and Female Adults (M&F)"
  ),
  tibble(ic = "EG-f"
         , udn =  c("3.6.1", "3.6.2")
         , name = "Household Type: Adult Female No Adult Male (FNM)"
  ),
  tibble(ic = "EG-f"
         , udn =  c("3.7.1", "3.7.2")
         , name = "Household Type: Adult Male No Adult Female (MNF)"
  ),
  tibble(ic = "EG-f"
         , udn =  c("3.8.1", "3.8.2")
         , name = "Household Type: Child No Adults (CNA)"
  ),
  tibble(ic = "EG-f"
         , udn =  c("3.9.1", "3.9.2")
         , name = "Household Type: Disaggregates Not Available"
  ),
  ##EG-g##
  ##Percent of Households Falling Below the Fixed Threshold for the Poorest Quintile of the Comparative Wealth Index (All Household Types Together)##
  tibble(ic = "EG-g"
         , udn =  c("3.1.1", "3.1.2", "3.1.3", "3.1.4")
         , name = "Percent of Households Falling Below the Fixed Threshold for the Poorest Quintile of the Comparative Wealth Index (All Household Types Together)"
  ),
  ##Number of Households (All Household Types Together)##
  tibble(ic = "EG-g"
         , udn =  c("3.2.1", "3.2.2", "3.2.3", "3.2.4")
         , name = "Number of Households (All Household Types Together)"
  ),
  ##Percent of Households in this Gendered Household Type Falling Below the Fixed Threshold for the Poorest Quintile of the Comparative Wealth Index##
  tibble(ic = "EG-g"
         , udn =  c("3.3.1.1", "3.3.1.2", "3.3.1.3", "3.3.1.4", "3.3.2.1"
                    , "3.3.2.2", "3.3.2.3", "3.3.2.4")
         , name = "Household Type: Male and Female Adults (M&F)"
  ),
  tibble(ic = "EG-g"
         , udn =  c("3.4.1.1", "3.4.1.2", "3.4.1.3", "3.4.1.4", "3.4.2.1"
                    , "3.4.2.2", "3.4.2.3", "3.4.2.4")
         , name = "Household Type: Adult Female No Adult Male (FNM)"
  ),
  tibble(ic = "EG-g"
         , udn =  c("3.5.1.1", "3.5.1.2", "3.5.1.3", "3.5.1.4", "3.5.2.1"
                    , "3.5.2.2", "3.5.2.3", "3.5.2.4")
         , name = "Household Type: Adult Male No Adult Female (MNF)"
  ),
  tibble(ic = "EG-g"
         , udn =  c("3.6.1.1", "3.6.1.2", "3.6.1.3", "3.6.1.4", "3.6.2.1"
                    , "3.6.2.2", "3.6.2.3", "3.6.2.4")
         , name = "Household Type: Child No Adults (CNA)"
  ),
  tibble(ic = "EG-g"
         , udn =  c("3.7.1.1", "3.7.1.2", "3.7.1.3", "3.7.1.4", "3.7.2.1"
                    , "3.7.2.2", "3.7.2.3", "3.7.2.4")
         , name = "Household Type: Disaggregates Not Available"
  ),
  ##EG-h##
  ##Mean Percent Shortfall of the Poor (All Household Types Together)##
  tibble(ic = "EG-h"
         , udn =  c("3.1.1", "3.1.2", "3.1.3", "3.1.4")
         , name = "Mean Percent Shortfall of the Poor (All Household Types Together)"
  ),
  ##Number of People (All Household Types Together)##
  tibble(ic = "EG-h"
         , udn =  c("3.2.1", "3.2.2", "3.2.3", "3.2.4")
         , name = "Number of People (All Household Types Together)"
  ),
  ##Mean Percent Shortfall of the Poor for People in this Gendered Household Type##
  tibble(ic = "EG-h"
         , udn =  c("3.3.1.1", "3.3.1.2", "3.3.1.3", "3.3.1.4", "3.3.2.1"
                    , "3.3.2.2", "3.3.2.3", "3.3.2.4")
         , name = "Household Type: Male and Female Adults (M&F)"
  ),
  tibble(ic = "EG-h"
         , udn =  c("3.4.1.1", "3.4.1.2", "3.4.1.3", "3.4.1.4", "3.4.2.1"
                    , "3.4.2.2", "3.4.2.3", "3.4.2.4")
         , name = "Household Type: Adult Female No Adult Male (FNM)"
  ),
  tibble(ic = "EG-h"
         , udn =  c("3.5.1.1", "3.5.1.2", "3.5.1.3", "3.5.1.4", "3.5.2.1"
                    , "3.5.2.2", "3.5.2.3", "3.5.2.4")
         , name = "Household Type: Adult Male No Adult Female (MNF)"
  ),
  tibble(ic = "EG-h"
         , udn =  c("3.6.1.1", "3.6.1.2", "3.6.1.3", "3.6.1.4", "3.6.2.1"
                    , "3.6.2.2", "3.6.2.3", "3.6.2.4")
         , name = "Household Type: Child No Adults (CNA)"
  ),
  tibble(ic = "EG-h"
         , udn =  c("3.7.1.1", "3.7.1.2", "3.7.1.3", "3.7.1.4", "3.7.2.1"
                    , "3.7.2.2", "3.7.2.3", "3.7.2.4")
         , name = "Household Type: Disaggregates Not Available"
  ),
  ##EG.3-e (Percent Change)##
  ##Total Value Added in the Agri-food Sector in USD##
  tibble(ic = "EG.3-e"
         , udn =  c("3.1.1.1", "3.1.1.2", "3.1.1.3", "3.1.1.4", "3.1.1.5")
         , name = "Total Value Added in the Agri-food Sector in USD"
  ),
  ##EG.3-f (A-WEAI)##
  ##Sample-Weighted A-WEAI Score##
  tibble(ic = "EG.3-f"
         , udn =  c("3.1.1", "3.1.2", "3.1.3", "3.1.4")
         , name = "Sample-Weighted A-WEAI Score"
  ),
  ##Number of Primary Female Adult Decision Makers Active in Household Agricultural Activities##
  tibble(ic = "EG.3-f"
         , udn =  c("3.2.1", "3.2.2", "3.2.3", "3.2.4")
         , name = "Number of Primary Female Adult Decision Makers Active in Household Agricultural Activities"
  ),
  ##Number of Primary Male Adult Decision Makers Active in Household Agricultural Activities##
  tibble(ic = "EG.3-f"
         , udn =  c("3.3.1", "3.3.2", "3.3.3", "3.3.4")
         , name = "Number of Primary Male Adult Decision Makers Active in Household Agricultural Activities"
  ),
  ##Sample-Weighted 5DE Score##
  tibble(ic = "EG.3-f"
         , udn =  c("3.4.1", "3.4.2", "3.4.3", "3.4.4")
         , name = "Sample-Weighted 5DE Score"
  ),
  ##Sample-Weighted GPI Score##
  tibble(ic = "EG.3-f"
         , udn =  c("3.5.1", "3.5.2", "3.5.3", "3.5.4")
         , name = "Sample-Weighted GPI Score"
  ),
  ##Sample-Weighted A-WEAI Score for Women 18-29 Years Old##
  tibble(ic = "EG.3-f"
         , udn =  c("3.6.1", "3.6.2", "3.6.3", "3.6.4")
         , name = "Sample-Weighted A-WEAI Score for Women 18-29 Years Old"
  ),
  ##Number of Primary Female 18-29 Year Old Decision Makers Active in Household Agricultural Activities##
  tibble(ic = "EG.3-f"
         , udn =  c("3.7.1", "3.7.2", "3.7.3", "3.7.4")
         , name = "Number of Primary Female 18-29 Year Old Decision Makers Active in Household Agricultural Activities"
  ),
  ##Number of Primary Male 18-29 Year Old Decision Makers Active in Household Agricultural Activities##
  tibble(ic = "EG.3-f"
         , udn =  c("3.8.1", "3.8.2", "3.8.3", "3.8.4")
         , name = "Number of Primary Male 18-29 Year Old Decision Makers Active in Household Agricultural Activities"
  ),
  ##Sample-Weighted A-WEAI Score for Women 30+ Years Old##
  tibble(ic = "EG.3-f"
         , udn =  c("3.9.1", "3.9.2", "3.9.3", "3.9.4")
         , name = "Sample-Weighted A-WEAI Score for Women 30+ Years Old"
  ),
  ##Number of Primary Female 30+ Year Old Decision Makers Active in Household Agricultural Activities##
  tibble(ic = "EG.3-f"
         , udn =  c("3.10.1", "3.10.2", "3.10.3", "3.10.4")
         , name = "Number of Primary Female 30+ Year Old Decision Makers Active in Household Agricultural Activities"
  ),
  ##Number of Primary Male 30+ Year Old Decision Makers Active in Household Agricultural Activities##
  tibble(ic = "EG.3-f"
         , udn =  c("3.11.1", "3.11.2", "3.11.3", "3.11.4")
         , name = "Number of Primary Male 30+ Year Old Decision Makers Active in Household Agricultural Activities"
  ),
  ##EG.3-g##
  ##Number of People Working in the Agri-food Sector Components##
  tibble(ic = "EG.3-g"
         , udn =  c("3.1.1", "3.1.2", "3.1.3", "3.1.4", "3.1.5")
         , name = "Number of People Working in the Agri-food Sector Components"
  ),
  ##EG.3-x1##
  ##Gendered Household Type##
  tibble(ic = "EG.3-x1"
         , ms_d1 = "Gendered Household Type"
         , name = "Gendered Household Type"
  ),
  ##Location##
  tibble(ic = "EG.3-x1", ms_d1 = "Location", name = "Location"
  ),
  ##Duration##
  tibble(ic = "EG.3-x1", ms_d1 = "Duration", name = "Duration"
  ),
  ##EG.3-2##
  ##Sex##
  tibble(ic = "EG.3-2"
         , udn =  c("3.1.2", "3.1.1", "3.1.3", "3.1.4")
         , ms_d1 = "Sex of individuals participating (no double counting)"
         , name = "Sex of Individuals Participating"
  ),
  ##Age##
  tibble(ic = "EG.3-2"
         , udn =  c("3.2.2", "3.2.1", "3.2.3", "3.2.4", "3.2.5")
         , ms_d1 = "Age Category of individuals participating (no double counting)"
         , name = "Age Category of Individuals Participating"
  ),
  ##Type of Individuals Participating##
  tibble(ic = "EG.3-2"
         , udn =  c("3.3.2", "3.3.1", "3.3.3", "3.3.4", "3.3.5", "3.3.6"
                    , "3.3.7", "3.3.8", "3.3.9", "3.3.10", "3.3.11", "3.3.12"
                    , "3.3.13")
         , ms_d2 = "Type of individuals participating (double-counting allowed)"
         , name = "Type of Individuals Participating"
  ),
  ##EG.3-2_OULevel##
  ##Sex##
  tibble(ic = "EG.3-2_OULevel"
         , udn =  c("3.1.2", "3.1.1", "3.1.3", "3.1.4")
         , name = "Sex of Individuals Participating"
  ),
  ##Age##
  tibble(ic = "EG.3-2_OULevel"
         , udn =  c("3.2.2", "3.2.1", "3.2.3", "3.2.4", "3.2.5")
         , name = "Age Category of Individuals Participating"
  ),
  ##Type of Individuals Participating##
  tibble(ic = "EG.3-2_OULevel"
         , udn =  c("3.3.2", "3.3.1", "3.3.3", "3.3.4", "3.3.5", "3.3.6"
                    , "3.3.7", "3.3.8", "3.3.9", "3.3.10", "3.3.11", "3.3.12"
                    , "3.3.13")
         , name = "Type of Individuals Participating"
  ),
  ##EG.3-x6, -7, -8##
  tibble(ic = "EG.3-x6, -7, -8"
         , ms_d2 = "Number of Direct Beneficiaries"
         , name = "Number of Direct Beneficiaries"
  ),
  tibble(ic = "EG.3-x6, -7, -8"
         , ms_d2 = "Purchased input costs (USD)"
         , name = "Purchased Input Costs (USD)"
  ),
  tibble(ic = "EG.3-x6, -7, -8"
         , ms_d2 = "Quantity of Sales"
         , name = "Quantity of Sales"
  ),
  tibble(ic = "EG.3-x6, -7, -8"
         , ms_d2 = "Total Production"
         , name = "Total Production"
  ),
  tibble(ic = "EG.3-x6, -7, -8"
         , ms_d2 = "Units of Production: Hectares planted (for crops); Number of animals (for milk, eggs); or Area (ha) of ponds or Number of crates (for fish)"
         , name = "Units of Production"
  ),
  tibble(ic = "EG.3-x6, -7, -8", ms_d2 = "Value of Sales (USD)", name = "Value of Sales (USD)"
  ),
  ##EG.3-x9##
  tibble(ic = "EG.3-x9", ms_d1 = "Duration", name = "Duration"
  ),
  tibble(ic = "EG.3-x9", ms_d1 = "Location", name = "Location"
  ),
  tibble(ic = "EG.3-x9", ms_d1 = "Sex of job-holder", name = "Sex of Job-Holder"
  ),
  ##EG.3-10-11-12##
  ##1##
  tibble(ic = "EG.3-10-11-12_CROP"
         , udn =  c("3.1.1.1.8.1.2", "3.1.1.1.9.1.2", "3.1.1.1.10.1.2"
                    , "3.1.1.1.8.1.1", "3.1.1.1.9.1.1", "3.1.1.1.10.1.1"
                    , "3.1.1.1.8.1.3", "3.1.1.1.9.1.3", "3.1.1.1.10.1.3"
                    , "3.1.1.1.8.2.2", "3.1.1.1.9.2.2", "3.1.1.1.10.2.2"
                    , "3.1.1.1.8.2.1", "3.1.1.1.9.2.1", "3.1.1.1.10.2.1"
                    , "3.1.1.1.8.2.3", "3.1.1.1.9.2.3", "3.1.1.1.10.2.3")
         , name = "Farm Size: Smallholder Producer"
  ),
  tibble(ic = "EG.3-10-11-12_DAIRY"
         , udn =  c("3.1.1.1.8.1.2", "3.1.1.1.9.1.2", "3.1.1.1.10.1.2"
                    , "3.1.1.1.8.1.1", "3.1.1.1.9.1.1", "3.1.1.1.10.1.1"
                    , "3.1.1.1.8.1.3", "3.1.1.1.9.1.3", "3.1.1.1.10.1.3"
                    , "3.1.1.1.8.2.2", "3.1.1.1.9.2.2", "3.1.1.1.10.2.2"
                    , "3.1.1.1.8.2.1", "3.1.1.1.9.2.1", "3.1.1.1.10.2.1"
                    , "3.1.1.1.8.2.3", "3.1.1.1.9.2.3", "3.1.1.1.10.2.3")
         , name = "Production System: Rangeland"
  ),
  tibble(ic = "EG.3-10-11-12_EGGS"
         , udn =  c("3.1.1.1.8.1.2", "3.1.1.1.9.1.2", "3.1.1.1.10.1.2"
                    , "3.1.1.1.8.1.1", "3.1.1.1.9.1.1", "3.1.1.1.10.1.1"
                    , "3.1.1.1.8.1.3", "3.1.1.1.9.1.3", "3.1.1.1.10.1.3"
                    , "3.1.1.1.8.2.2", "3.1.1.1.9.2.2", "3.1.1.1.10.2.2"
                    , "3.1.1.1.8.2.1", "3.1.1.1.9.2.1", "3.1.1.1.10.2.1"
                    , "3.1.1.1.8.2.3", "3.1.1.1.9.2.3", "3.1.1.1.10.2.3")
         , name = "Production System: Rangeland"
  ),
  tibble(ic = "EG.3-10-11-12_LIVSTK"
         , udn =  c("3.1.1.1.8.1.2", "3.1.1.1.9.1.2", "3.1.1.1.10.1.2"
                    , "3.1.1.1.8.1.1", "3.1.1.1.9.1.1", "3.1.1.1.10.1.1"
                    , "3.1.1.1.8.1.3", "3.1.1.1.9.1.3", "3.1.1.1.10.1.3"
                    , "3.1.1.1.8.2.2", "3.1.1.1.9.2.2", "3.1.1.1.10.2.2"
                    , "3.1.1.1.8.2.1", "3.1.1.1.9.2.1", "3.1.1.1.10.2.1"
                    , "3.1.1.1.8.2.3", "3.1.1.1.9.2.3", "3.1.1.1.10.2.3")
         , name = "Production System: Rangeland"
  ),
  tibble(ic = "EG.3-10-11-12_OTHER"
         , udn =  c("3.1.1.1.8.1.2", "3.1.1.1.9.1.2", "3.1.1.1.10.1.2"
                    , "3.1.1.1.8.1.1", "3.1.1.1.9.1.1", "3.1.1.1.10.1.1"
                    , "3.1.1.1.8.1.3", "3.1.1.1.9.1.3", "3.1.1.1.10.1.3"
                    , "3.1.1.1.8.2.2", "3.1.1.1.9.2.2", "3.1.1.1.10.2.2"
                    , "3.1.1.1.8.2.1", "3.1.1.1.9.2.1", "3.1.1.1.10.2.1"
                    , "3.1.1.1.8.2.3", "3.1.1.1.9.2.3", "3.1.1.1.10.2.3")
         , name = "Farm Size: Smallholder Producer"
  ),
  tibble(ic = "EG.3-10-11-12_PONDAQ"
         , udn =  c("3.1.1.1.8.1.2", "3.1.1.1.9.1.2", "3.1.1.1.10.1.2"
                    , "3.1.1.1.8.1.1", "3.1.1.1.9.1.1", "3.1.1.1.10.1.1"
                    , "3.1.1.1.8.1.3", "3.1.1.1.9.1.3", "3.1.1.1.10.1.3"
                    , "3.1.1.1.8.2.2", "3.1.1.1.9.2.2", "3.1.1.1.10.2.2"
                    , "3.1.1.1.8.2.1", "3.1.1.1.9.2.1", "3.1.1.1.10.2.1"
                    , "3.1.1.1.8.2.3", "3.1.1.1.9.2.3", "3.1.1.1.10.2.3")
         , name = "Farm Size: Pond Aquaculture"
  ),
  ##2##
  tibble(ic = "EG.3-10-11-12_CROP"
         , udn =  c("3.1.1.2.8.1.2", "3.1.1.2.9.1.2", "3.1.1.2.10.1.2"
                    , "3.1.1.2.8.1.1", "3.1.1.2.9.1.1", "3.1.1.2.10.1.1"
                    , "3.1.1.2.8.1.3", "3.1.1.2.9.1.3", "3.1.1.2.10.1.3"
                    , "3.1.1.2.8.2.2", "3.1.1.2.9.2.2", "3.1.1.2.10.2.2"
                    , "3.1.1.2.8.2.1", "3.1.1.2.9.2.1", "3.1.1.2.10.2.1"
                    , "3.1.1.2.8.2.3", "3.1.1.2.9.2.3", "3.1.1.2.10.2.3")
         , name = "Farm Size: Non-Smallholder Producer"
  ),
  tibble(ic = "EG.3-10-11-12_DAIRY"
         , udn =  c("3.1.1.2.8.1.2", "3.1.1.2.9.1.2", "3.1.1.2.10.1.2"
                    , "3.1.1.2.8.1.1", "3.1.1.2.9.1.1", "3.1.1.2.10.1.1"
                    , "3.1.1.2.8.1.3", "3.1.1.2.9.1.3", "3.1.1.2.10.1.3"
                    , "3.1.1.2.8.2.2", "3.1.1.2.9.2.2", "3.1.1.2.10.2.2"
                    , "3.1.1.2.8.2.1", "3.1.1.2.9.2.1", "3.1.1.2.10.2.1"
                    ,"3.1.1.2.8.2.3", "3.1.1.2.9.2.3", "3.1.1.2.10.2.3")
         , name = "Production System: Mixed Crop-Livestock"
  ),
  tibble(ic = "EG.3-10-11-12_EGGS"
         , udn =  c("3.1.1.2.8.1.2", "3.1.1.2.9.1.2", "3.1.1.2.10.1.2"
                    , "3.1.1.2.8.1.1", "3.1.1.2.9.1.1", "3.1.1.2.10.1.1"
                    , "3.1.1.2.8.1.3", "3.1.1.2.9.1.3", "3.1.1.2.10.1.3"
                    , "3.1.1.2.8.2.2", "3.1.1.2.9.2.2", "3.1.1.2.10.2.2"
                    , "3.1.1.2.8.2.1", "3.1.1.2.9.2.1", "3.1.1.2.10.2.1"
                    , "3.1.1.2.8.2.3", "3.1.1.2.9.2.3", "3.1.1.2.10.2.3")
         , name = "Production System: Mixed Crop-Livestock"
  ),
  tibble(ic = "EG.3-10-11-12_LIVSTK"
         , udn =  c("3.1.1.2.8.1.2", "3.1.1.2.9.1.2", "3.1.1.2.10.1.2"
                    , "3.1.1.2.8.1.1", "3.1.1.2.9.1.1", "3.1.1.2.10.1.1"
                    , "3.1.1.2.8.1.3", "3.1.1.2.9.1.3", "3.1.1.2.10.1.3"
                    , "3.1.1.2.8.2.2", "3.1.1.2.9.2.2", "3.1.1.2.10.2.2"
                    , "3.1.1.2.8.2.1", "3.1.1.2.9.2.1", "3.1.1.2.10.2.1"
                    , "3.1.1.2.8.2.3", "3.1.1.2.9.2.3", "3.1.1.2.10.2.3")
         , name = "Production System: Mixed Crop-Livestock"
  ),
  tibble(ic = "EG.3-10-11-12_OTHER"
         , udn =  c("3.1.1.2.8.1.2", "3.1.1.2.9.1.2", "3.1.1.2.10.1.2"
                    , "3.1.1.2.8.1.1", "3.1.1.2.9.1.1", "3.1.1.2.10.1.1"
                    , "3.1.1.2.8.1.3", "3.1.1.2.9.1.3", "3.1.1.2.10.1.3"
                    , "3.1.1.2.8.2.2", "3.1.1.2.9.2.2", "3.1.1.2.10.2.2"
                    , "3.1.1.2.8.2.1", "3.1.1.2.9.2.1", "3.1.1.2.10.2.1"
                    , "3.1.1.2.8.2.3", "3.1.1.2.9.2.3", "3.1.1.2.10.2.3")
         , name = "Farm Size: Non-Smallholder Producer"
  ),
  ##3##
  tibble(ic = "EG.3-10-11-12_DAIRY"
         , udn =  c("3.1.1.3.8.1.2", "3.1.1.3.9.1.2", "3.1.1.3.10.1.2"
                    , "3.1.1.3.8.1.1", "3.1.1.3.9.1.1", "3.1.1.3.10.1.1"
                    , "3.1.1.3.8.1.3", "3.1.1.3.9.1.3", "3.1.1.3.10.1.3"
                    , "3.1.1.3.8.2.2", "3.1.1.3.9.2.2", "3.1.1.3.10.2.2"
                    , "3.1.1.3.8.2.1", "3.1.1.3.9.2.1", "3.1.1.3.10.2.1"
                    , "3.1.1.3.8.2.3", "3.1.1.3.9.2.3", "3.1.1.3.10.2.3")
         , name = "Production System: Urban/Peri-Urban"
  ),
  tibble(ic = "EG.3-10-11-12_EGGS"
         , udn =  c("3.1.1.3.8.1.2", "3.1.1.3.9.1.2", "3.1.1.3.10.1.2"
                    , "3.1.1.3.8.1.1", "3.1.1.3.9.1.1", "3.1.1.3.10.1.1"
                    , "3.1.1.3.8.1.3", "3.1.1.3.9.1.3", "3.1.1.3.10.1.3"
                    , "3.1.1.3.8.2.2", "3.1.1.3.9.2.2", "3.1.1.3.10.2.2"
                    , "3.1.1.3.8.2.1", "3.1.1.3.9.2.1", "3.1.1.3.10.2.1"
                    , "3.1.1.3.8.2.3", "3.1.1.3.9.2.3", "3.1.1.3.10.2.3")
         , name = "Production System: Urban/Peri-Urban"
  ),
  tibble(ic = "EG.3-10-11-12_LIVSTK"
         , udn =  c("3.1.1.3.8.1.2", "3.1.1.3.9.1.2", "3.1.1.3.10.1.2"
                    , "3.1.1.3.8.1.1", "3.1.1.3.9.1.1", "3.1.1.3.10.1.1"
                    , "3.1.1.3.8.1.3", "3.1.1.3.9.1.3", "3.1.1.3.10.1.3"
                    , "3.1.1.3.8.2.2", "3.1.1.3.9.2.2", "3.1.1.3.10.2.2"
                    , "3.1.1.3.8.2.1", "3.1.1.3.9.2.1", "3.1.1.3.10.2.1"
                    , "3.1.1.3.8.2.3", "3.1.1.3.9.2.3", "3.1.1.3.10.2.3")
         , name = "Production System: Urban/Peri-Urban"
  ),
  ##4##
  tibble(ic = "EG.3-10-11-12_DAIRY"
         , udn =  c("3.1.1.4.8.1.2", "3.1.1.4.9.1.2", "3.1.1.4.10.1.2"
                    , "3.1.1.4.8.1.1", "3.1.1.4.9.1.1", "3.1.1.4.10.1.1"
                    , "3.1.1.4.8.1.3", "3.1.1.4.9.1.3", "3.1.1.4.10.1.3"
                    , "3.1.1.4.8.2.2", "3.1.1.4.9.2.2", "3.1.1.4.10.2.2"
                    , "3.1.1.4.8.2.1", "3.1.1.4.9.2.1", "3.1.1.4.10.2.1"
                    , "3.1.1.4.8.2.3", "3.1.1.4.9.2.3", "3.1.1.4.10.2.3")
         , name = "Production System: Intensive/Commercial"
  ),
  tibble(ic = "EG.3-10-11-12_EGGS"
         , udn =  c("3.1.1.4.8.1.2", "3.1.1.4.9.1.2", "3.1.1.4.10.1.2"
                    , "3.1.1.4.8.1.1", "3.1.1.4.9.1.1", "3.1.1.4.10.1.1"
                    , "3.1.1.4.8.1.3", "3.1.1.4.9.1.3", "3.1.1.4.10.1.3"
                    , "3.1.1.4.8.2.2", "3.1.1.4.9.2.2", "3.1.1.4.10.2.2"
                    , "3.1.1.4.8.2.1", "3.1.1.4.9.2.1", "3.1.1.4.10.2.1"
                    , "3.1.1.4.8.2.3", "3.1.1.4.9.2.3", "3.1.1.4.10.2.3")
         , name = "Production System: Intensive/Commercial"
  ),
  tibble(ic = "EG.3-10-11-12_LIVSTK"
         , udn =  c("3.1.1.4.8.1.2", "3.1.1.4.9.1.2", "3.1.1.4.10.1.2"
                    , "3.1.1.4.8.1.1", "3.1.1.4.9.1.1", "3.1.1.4.10.1.1"
                    , "3.1.1.4.8.1.3", "3.1.1.4.9.1.3", "3.1.1.4.10.1.3"
                    , "3.1.1.4.8.2.2", "3.1.1.4.9.2.2", "3.1.1.4.10.2.2"
                    , "3.1.1.4.8.2.1", "3.1.1.4.9.2.1", "3.1.1.4.10.2.1"
                    , "3.1.1.4.8.2.3", "3.1.1.4.9.2.3", "3.1.1.4.10.2.3")
         , name = "Production System: Intensive/Commercial"
  ),
  ##EG.3.1-c##
  ##Value of exports##
  tibble(ic = "EG.3.1-c", udn = c("3.1.1", "3.1.2"),  name = "Commodity"
  ),
  tibble(ic = "EG.3.1-c", udn = "3.1",  name = "Value of Exports (USD)"
  ),
  ##EG.3.1-d##
  ##Milestone##
  tibble(ic = "EG.3.1-d", udn = "3.1.1", name = "Milestones Uploaded"
  ),
  ##EG.3.1-1 (Kms)##
  ##Improved##
  tibble(ic = "EG.3.1-1", udn = "3.1", ms_d1 = "Improved", name = "Improved"
  ),
  ##Constructed (New)##
  tibble(ic = "EG.3.1-1", udn =  "3.2", ms_d1 = "Constructed (new)", name = "Constructed (New)"
  ),
  ##DNA##
  tibble(ic = "EG.3.1-1", udn =  "3.3", ms_d1 = "Disaggregates Not Available"
         , name = "Disaggregates Not Available"
  ),
  ##EG.3.1-x2##
  tibble(ic = "EG.3.1-x2", name = "Number of Hectares"
  ),
  ##EG.3.1-x12 (Policies)##
  ##Policy Area##
  tibble(ic = "EG.3.1-x12", ms_d1 = "Policy Area", name = "Policy Area"
  ),
  ##Process/Step##
  tibble(ic = "EG.3.1-x12", ms_d1 = "Process/Step", name = "Process/Step"
  ),
  ##EG.3.1-x13##
  tibble(ic = "EG.3.1-x13", ms_d1 = "Female", name = "Female"
  ),
  tibble(ic = "EG.3.1-x13", ms_d1 = "Male", name = "Male"
  ),
  tibble(ic = "EG.3.1-x13", ms_d1 = "Joint", name = "Joint"
  ),
  tibble(ic = "EG.3.1-x13", ms_d1 = "Communal", name = "Communal"
  ),
  tibble(ic = "EG.3.1-x13", ms_d1 = "Disaggregates Not Available"
         , name = "Disaggregates Not Available"
  ),
  ##EG.3.1-14##
  ##USG Commitment Amount##
  tibble(ic = "EG.3.1-14", udn =  "3.1.1", ms_d2 = "USG commitment amount ($USD)"
         , name = "USG Commitment Amount"
  ),
  ##Private Sector Partner Leveraged Amount##
  tibble(ic = "EG.3.1-14", udn =  "3.1.2"
         , ms_d2 = "Private sector partner leveraged amount ($USD)"
         , name = "Private Sector Partner Leveraged Amount"
  ),
  ##EG.3.1-15##
  ##Private Sector Partner Leveraged Amount##
  tibble(ic = "EG.3.1-15", udn = "3.1", name = "Private Sector Partner Leveraged Amount"
  ),
  ##EG.3.1-x21##
  tibble(ic = "EG.3.1-x21", name = "Number of Climate Vulnerability Assessments"
  ),
  ##EG.3.1-x22 (Hectares)##
  tibble(ic = "EG.3.1-x22", name = "ms_d1"
  ),
  ##EG.3.2-x1##
  tibble(ic = "EG.3.2-x1", ms_d1 = "Type of Individual"
         , name = "ms_d2"
  ),
  tibble(ic = "EG.3.2-x1", ms_d1 = "Sex", name = "ms_d1"
  ),
  ##EG.3.2-x3##
  tibble(ic = "EG.3.2-x3", ms_d1 = "Sex of owner / producer"
         , name = "Sex of Owner/Producer"
  ),
  tibble(ic = "EG.3.2-x3", name = "ms_d1"
  ),
  ##EG.3.2-x4##
  tibble(ic = "EG.3.2-x4", ms_d1 = "Duration", name = "Duration"
  ),
  tibble(ic = "EG.3.2-x4"
         , ms_d1 = "Type of organization"
         , name = "Type of Organization"
  ),
  ##EG.3.2-x5##
  tibble(ic = "EG.3.2-x5"
         , ms_d2 = "Agricultural post harvest transformation"
         , name = "Agricultural Post Harvest Transformation"
  ),
  tibble(ic = "EG.3.2-x5", ms_d2 = "Agricultural production"
         , name = "Agricultural Production"
  ),
  tibble(ic = "EG.3.2-x5", name = "ms_d2"
  ),
  ##EG.3.2-x6##
  tibble(ic = "EG.3.2-x6", ms_d1 = "Sex of recipient"
         , name = "Sex of Recipient(s)"
  ),
  tibble(ic = "EG.3.2-x6", ms_d1 = "Type of loan recipient"
         , name = "Type of Loan Recipient(s)"
  ),
  ##EG.3.2-x14##
  tibble(ic = "EG.3.2-x14"
         , name = "ms_d1"
  ),
  ##EG.3.2-x17##
  tibble(ic = "EG.3.2-x17", ms_d1 = "Producers", name = "Producers"
  ),
  tibble(ic = "EG.3.2-x17", ms_d1 = "Others", name = "Others"
  ),
  ##EG.3.2-x18 (Hectares Under Improved Management)##
  tibble(ic = "EG.3.2-x18", name = "Cultivated Land"
  ),
  ##EG.3.2-x19 (Value by Type and Sex##
  tibble(ic = "EG.3.2-x19", name = "Commodity"
  ),
  ##EG.3.2-x22 (PSI Value)##
  tibble(ic = "EG.3.2-x22", name = "Value of New Private Sector Capital Investment"
  ),
  ##EG.3.2-2##
  ##Sex##
  tibble(ic = "EG.3.2-2", udn =  c("3.1.2", "3.1.1", "3.1.3")
         , ms_d1 = "Sex", name = "Sex"
  ),
  ##Duration##
  tibble(ic = "EG.3.2-2", udn =  c("3.2.2", "3.2.1", "3.2.3")
         , ms_d1 = "Duration", name = "Duration"
  ),
  ##EG.3.2-7##
  ##Plant and Animal Improvement Research##
  tibble(ic = "EG.3.2-7"
         , udn =  c("3.1.1", "3.1.5.1.1.1", "3.1.5.1.1.2", "3.1.5.1.1.3"
                    , "3.1.5.1.1.4")
         , name = "Plant and Animal Improvement Research"
  ),
  tibble(ic = "EG.3.2-7"
         , ms_d2 = c("Plant and Animal Improvement Research"
                     , "Plant and Animal Improvement Research - Unique number of technologies / practices")
         , name = "Plant and Animal Improvement Research"
  ),
  ##Production Systems Research##
  tibble(ic = "EG.3.2-7"
         , udn =  c("3.1.2", "3.1.5.2.1.1", "3.1.5.2.1.2", "3.1.5.2.1.3"
                    , "3.1.5.2.1.4")
         , name = "Production Systems Research"
  ),
  tibble(ic = "EG.3.2-7"

         , ms_d2 = c("Production Systems Research"
                     , "Production Systems Research - Unique number of technologies / practices")
         , name = "Production Systems Research"
  ),
  ##Social Science Research##
  tibble(ic = "EG.3.2-7"
         , udn =  c("3.1.3", "3.1.5.3.1.1", "3.1.5.3.1.2", "3.1.5.3.1.3", "3.1.5.3.1.4")
         , name = "Social Science Research"
  ),
  tibble(ic = "EG.3.2-7"
         , ms_d2 = c("Social Science Research"
                     , "Social Science Research - Unique number of technologies / practices")
         , name = "Social Science Research"
  ),
  ##DNA##
  tibble(ic = "EG.3.2-7"
         , udn =  c("3.1.4", "3.1.5.4.1.1", "3.1.5.4.1.2", "3.1.5.4.1.3", "3.1.5.4.1.4")
         , name = "Disaggregates Not Available"
  ),
  tibble(ic = "EG.3.2-7"
         , ms_d2 = c("Disaggregates Not Available (research category not listed or unknown)"
                     , "Disaggregates Not Available (research category not listed or unknown) - Unique number of technologies / practices")
         , name = "Disaggregates Not Available"
  ),
  ##EG.3.2-24##
  ##Smallholder Producers##
  tibble(ic = "EG.3.2-24"
         , udn =  c("3.1.4", "3.1.1.1", "3.1.1.2", "3.1.1.3", "3.1.2.1"
                    , "3.1.2.2", "3.1.2.3", "3.1.3.1", "3.1.3.2", "3.1.3.3"
                    , "3.1.3.4", "3.1.3.5", "3.1.3.6", "3.1.3.7", "3.1.3.8"
                    , "3.1.3.9", "3.1.3.10", "3.1.3.11", "3.1.3.12", "3.1.3.13"
                    , "3.1.3.14", "3.1.3.15", "3.1.3.16")
         , ms_d1 = "Value Chain Actor Type: Smallholder producers"
         , name = "Smallholder Producers"
  ),
  ##Non-Smallholder Producers##
  tibble(ic = "EG.3.2-24"
         , udn =  c("3.2.4", "3.2.1.1", "3.2.1.2", "3.2.1.3", "3.2.2.1"
                    , "3.2.2.2", "3.2.2.3", "3.2.3.1", "3.2.3.2", "3.2.3.3"
                    , "3.2.3.4", "3.2.3.5", "3.2.3.6", "3.2.3.7", "3.2.3.8"
                    , "3.2.3.9", "3.2.3.10", "3.2.3.11", "3.2.3.12", "3.2.3.13"
                    , "3.2.3.14", "3.2.3.15", "3.2.3.16")
         , ms_d1 = "Value Chain Actor Type: Non-smallholder producers"
         , name = "Non-Smallholder Producers"
  ),
  ##People in Government##
  tibble(ic = "EG.3.2-24"
         , udn =  c("3.3.4", "3.3.1.1", "3.3.1.2", "3.3.1.3", "3.3.2.1"
                    , "3.3.2.2", "3.3.2.3", "3.3.3.1", "3.3.3.2", "3.3.3.3"
                    , "3.3.3.4", "3.3.3.5", "3.3.3.6", "3.3.3.7", "3.3.3.8"
                    , "3.3.3.9", "3.3.3.10", "3.3.3.11", "3.3.3.12", "3.3.3.13"
                    , "3.3.3.14", "3.3.3.15", "3.3.3.16")
         , ms_d1 = "Value Chain Actor Type: People in government"
         , name = "People in Government"
  ),
  ##People in Private Sector Firms##
  tibble(ic = "EG.3.2-24"
         , udn =  c("3.4.4", "3.4.1.1", "3.4.1.2", "3.4.1.3", "3.4.2.1"
                    , "3.4.2.2", "3.4.2.3", "3.4.3.1", "3.4.3.2", "3.4.3.3"
                    , "3.4.3.4", "3.4.3.5", "3.4.3.6", "3.4.3.7", "3.4.3.8"
                    , "3.4.3.9", "3.4.3.10", "3.4.3.11", "3.4.3.12", "3.4.3.13"
                    , "3.4.3.14", "3.4.3.15", "3.4.3.16")
         , ms_d1 = "Value Chain Actor Type: People in private Sector Firms"
         , name = "People in Private Sector Firms"
  ),
  ##People in Civil Society##
  tibble(ic = "EG.3.2-24"
         , udn =  c("3.5.4", "3.5.1.1", "3.5.1.2", "3.5.1.3", "3.5.2.1"
                    , "3.5.2.2", "3.5.2.3", "3.5.3.1", "3.5.3.2", "3.5.3.3"
                    , "3.5.3.4", "3.5.3.5", "3.5.3.6", "3.5.3.7", "3.5.3.8"
                    , "3.5.3.9", "3.5.3.10", "3.5.3.11", "3.5.3.12", "3.5.3.13"
                    , "3.5.3.14", "3.5.3.15", "3.5.3.16")
         , ms_d2 = "Value Chain Actor Type: People in civil society"
         , name = "People in Civil Society"
  ),
  ##Others##
  tibble(ic = "EG.3.2-24"
         , udn =  c("3.6.4", "3.6.1.1", "3.6.1.2", "3.6.1.3", "3.6.2.1"
                    , "3.6.2.2", "3.6.2.3", "3.6.3.1", "3.6.3.2", "3.6.3.3"
                    , "3.6.3.4", "3.6.3.5", "3.6.3.6", "3.6.3.7", "3.6.3.8"
                    , "3.6.3.9", "3.6.3.10", "3.6.3.11", "3.6.3.12", "3.6.3.13"
                    , "3.6.3.14", "3.6.3.15", "3.6.3.16")
         , ms_d1 = "Value Chain Actor Type: Others"
         , name = "Others"
  ),
  ##DNA##
  tibble(ic = "EG.3.2-24"
         , udn =  c("3.7.4", "3.7.1.1", "3.7.1.2", "3.7.1.3", "3.7.2.1"
                    , "3.7.2.2", "3.7.2.3", "3.7.3.1", "3.7.3.2", "3.7.3.3"
                    , "3.7.3.4", "3.7.3.5", "3.7.3.6", "3.7.3.7", "3.7.3.8"
                    , "3.7.3.9", "3.7.3.10", "3.7.3.11", "3.7.3.12", "3.7.3.13"
                    , "3.7.3.14", "3.7.3.15", "3.7.3.16")
         , ms_d1 = "Value Chain Actor Type: Disaggregates Not Available"
         , name = "Disaggregates Not Available"
  ),
  ##EG.3.2-25##
  ##Crop Land##
  tibble(ic = "EG.3.2-25"
         , udn =  c("3.1.1.1", "3.1.1.2", "3.1.1.3", "3.1.1.4", "3.1.2.1"
                    , "3.1.2.2", "3.1.2.3", "3.1.2.4", "3.1.3.1", "3.1.3.2"
                    , "3.1.3.3", "3.1.3.4", "3.1.3.5", "3.1.3.6", "3.1.3.7"
                    , "3.1.3.8", "3.1.3.9", "3.1.3.10", "3.1.3.11", "3.1.3.12"
                    , "3.1.3.13", "3.1.4")
         , ms_d1 = "Type of Hectare: Crop land"
         , name = "Crop Land"
  ),
  ##Cultivated Pasture##
  tibble(ic = "EG.3.2-25"
         , udn =  c("3.2.1.1", "3.2.1.2", "3.2.1.3", "3.2.1.4", "3.2.2.1"
                    , "3.2.2.2", "3.2.2.3", "3.2.2.4", "3.2.3.1", "3.2.3.2"
                    , "3.2.3.3", "3.2.3.4", "3.2.3.5", "3.2.3.6", "3.2.3.7"
                    , "3.2.3.8", "3.2.3.9", "3.2.3.10", "3.2.3.11", "3.2.3.12"
                    , "3.2.3.13", "3.2.4")
         , ms_d1 = "Type of Hectare: Cultivated pasture"
         , name = "Cultivated Pasture"
  ),
  ##Rangeland##
  tibble(ic = "EG.3.2-25"
         , udn =  c("3.3.1.1", "3.3.1.2", "3.3.1.3", "3.3.1.4", "3.3.2.1"
                    , "3.3.2.2", "3.3.2.3", "3.3.2.4", "3.3.3.1", "3.3.3.2"
                    , "3.3.3.3", "3.3.3.4", "3.3.3.5", "3.3.3.6", "3.3.3.7"
                    , "3.3.3.8", "3.3.3.9", "3.3.3.10", "3.3.3.11", "3.3.3.12"
                    , "3.3.3.13", "3.3.4")
         , ms_d1 = "Type of Hectare: Rangeland"
         , name = "Rangeland"
  ),
  ##Conservation/Protected Area##
  tibble(ic = "EG.3.2-25"
         , udn =  c("3.4.1.1", "3.4.1.2", "3.4.1.3", "3.4.1.4", "3.4.2.1"
                    , "3.4.2.2", "3.4.2.3", "3.4.2.4", "3.4.3.1", "3.4.3.2"
                    , "3.4.3.3", "3.4.3.4", "3.4.3.5", "3.4.3.6", "3.4.3.7"
                    , "3.4.3.8", "3.4.3.9", "3.4.3.10", "3.4.3.11", "3.4.3.12"
                    , "3.4.3.13", "3.4.4")
         , ms_d1 = "Type of Hectare: Conservation/protected area"
         , name = "Conservation/Protected Area"
  ),
  ##Freshwater or Marine Ecosystems##
  tibble(ic = "EG.3.2-25"
         , udn =  c("3.5.1.1", "3.5.1.2", "3.5.1.3", "3.5.1.4", "3.5.2.1"
                    , "3.5.2.2", "3.5.2.3", "3.5.2.4", "3.5.3.1", "3.5.3.2"
                    , "3.5.3.3", "3.5.3.4", "3.5.3.5", "3.5.3.6", "3.5.3.7"
                    , "3.5.3.8", "3.5.3.9", "3.5.3.10", "3.5.3.11", "3.5.3.12"
                    , "3.5.3.13", "3.5.4")
         , ms_d1 = "Type of Hectare: Freshwater or marine ecosystems"
         , name = "Freshwater or Marine Ecosystems"
  ),
  ##Aquaculture##
  tibble(ic = "EG.3.2-25"
         , udn =  c("3.6.1.1", "3.6.1.2", "3.6.1.3", "3.6.1.4", "3.6.2.1"
                    , "3.6.2.2", "3.6.2.3", "3.6.2.4", "3.6.3.1", "3.6.3.2"
                    , "3.6.3.3", "3.6.3.4", "3.6.3.5", "3.6.3.6", "3.6.3.7"
                    , "3.6.3.8", "3.6.3.9", "3.6.3.10", "3.6.3.11", "3.6.3.12"
                    , "3.6.3.13", "3.6.4")
         , ms_d1 = "Type of Hectare: Aquaculture"
         , name = "Aquaculture"
  ),
  ##Other##
  tibble(ic = "EG.3.2-25"
         , udn =  c("3.7.1.1", "3.7.1.2", "3.7.1.3", "3.7.1.4", "3.7.2.1"
                    , "3.7.2.2", "3.7.2.3", "3.7.2.4", "3.7.3.1", "3.7.3.2"
                    , "3.7.3.3", "3.7.3.4", "3.7.3.5", "3.7.3.6", "3.7.3.7"
                    , "3.7.3.8", "3.7.3.9", "3.7.3.10", "3.7.3.11", "3.7.3.12"
                    , "3.7.3.13", "3.7.4")
         , ms_d1 = "Type of Hectare: Other"
         , name = "Other"
  ),
  ##EG.3.2-26##
  ##(Type of Product) Inputs: Seeds and Planting Material##
  tibble(ic = "EG.3.2-26"
         , udn =  c("3.1.1.1.3.1", "3.1.1.1.3.3", "3.1.1.1.3.5", "3.1.1.1.3.7"
                    , "3.1.1.2.3.1", "3.1.1.2.3.3", "3.1.1.2.3.5", "3.1.1.2.3.7"
                    , "3.1.1.3.3.1", "3.1.1.3.3.3", "3.1.1.3.3.5", "3.1.1.3.3.7"
                    , "3.1.1.4.3.1", "3.1.1.4.3.3", "3.1.1.4.3.5", "3.1.1.4.3.7"
                    , "3.1.1.5.3.1", "3.1.1.5.3.3", "3.1.1.5.3.5", "3.1.1.5.3.7"
                    , "3.1.1.1.3.2", "3.1.1.1.3.4", "3.1.1.1.3.6", "3.1.1.1.3.8"
                    , "3.1.1.2.3.2", "3.1.1.2.3.4", "3.1.1.2.3.6", "3.1.1.2.3.8"
                    , "3.1.1.3.3.2", "3.1.1.3.3.4", "3.1.1.3.3.6", "3.1.1.3.3.8"
                    , "3.1.1.4.3.2", "3.1.1.4.3.4", "3.1.1.4.3.6", "3.1.1.4.3.8"
                    , "3.1.1.5.3.2", "3.1.1.5.3.4", "3.1.1.5.3.6", "3.1.1.5.3.8"
                    , "3.1.1.1.4.1", "3.1.1.1.4.3", "3.1.1.1.4.5", "3.1.1.1.4.7"
                    , "3.1.1.2.4.1", "3.1.1.2.4.3", "3.1.1.2.4.5", "3.1.1.2.4.7"
                    , "3.1.1.3.4.1", "3.1.1.3.4.3", "3.1.1.3.4.5", "3.1.1.3.4.7"
                    , "3.1.1.4.4.1", "3.1.1.4.4.3", "3.1.1.4.4.5", "3.1.1.4.4.7"
                    , "3.1.1.5.4.1", "3.1.1.5.4.3", "3.1.1.5.4.5", "3.1.1.5.4.7"
                    , "3.1.1.1.4.2", "3.1.1.1.4.4", "3.1.1.1.4.6", "3.1.1.1.4.8"
                    , "3.1.1.2.4.2", "3.1.1.2.4.4", "3.1.1.2.4.6", "3.1.1.2.4.8"
                    , "3.1.1.3.4.2", "3.1.1.3.4.4", "3.1.1.3.4.6", "3.1.1.3.4.8"
                    , "3.1.1.4.4.2", "3.1.1.4.4.4", "3.1.1.4.4.6", "3.1.1.4.4.8"
                    , "3.1.1.5.4.2", "3.1.1.5.4.4", "3.1.1.5.4.6", "3.1.1.5.4.8")
         , ms_d1 = "(Type of Product): Inputs: Seeds and planting material"
         , name = "(Type of Product) Inputs: Seeds and Planting Material"
  ),
  ##(Type of Product): Inputs: Other Non-Durable Inputs, Such as Fertilizers and Pesticides##
  tibble(ic = "EG.3.2-26"
         , udn =  c("3.1.2.1.3.1", "3.1.2.1.3.3", "3.1.2.1.3.5", "3.1.2.1.3.7"
                    , "3.1.2.2.3.1", "3.1.2.2.3.3", "3.1.2.2.3.5", "3.1.2.2.3.7"
                    , "3.1.2.3.3.1", "3.1.2.3.3.3", "3.1.2.3.3.5", "3.1.2.3.3.7"
                    , "3.1.2.4.3.1", "3.1.2.4.3.3", "3.1.2.4.3.5", "3.1.2.4.3.7"
                    , "3.1.2.5.3.1", "3.1.2.5.3.3", "3.1.2.5.3.5", "3.1.2.5.3.7"
                    , "3.1.2.1.3.2", "3.1.2.1.3.4", "3.1.2.1.3.6", "3.1.2.1.3.8"
                    , "3.1.2.2.3.2", "3.1.2.2.3.4", "3.1.2.2.3.6", "3.1.2.2.3.8"
                    , "3.1.2.3.3.2", "3.1.2.3.3.4", "3.1.2.3.3.6", "3.1.2.3.3.8"
                    , "3.1.2.4.3.2", "3.1.2.4.3.4", "3.1.2.4.3.6", "3.1.2.4.3.8"
                    , "3.1.2.5.3.2", "3.1.2.5.3.4", "3.1.2.5.3.6", "3.1.2.5.3.8"
                    , "3.1.2.1.4.1", "3.1.2.1.4.3", "3.1.2.1.4.5", "3.1.2.1.4.7"
                    , "3.1.2.2.4.1", "3.1.2.2.4.3", "3.1.2.2.4.5", "3.1.2.2.4.7"
                    , "3.1.2.3.4.1", "3.1.2.3.4.3", "3.1.2.3.4.5", "3.1.2.3.4.7"
                    , "3.1.2.4.4.1", "3.1.2.4.4.3", "3.1.2.4.4.5", "3.1.2.4.4.7"
                    , "3.1.2.5.4.1", "3.1.2.5.4.3", "3.1.2.5.4.5", "3.1.2.5.4.7"
                    , "3.1.2.1.4.2", "3.1.2.1.4.4", "3.1.2.1.4.6", "3.1.2.1.4.8"
                    , "3.1.2.2.4.2", "3.1.2.2.4.4", "3.1.2.2.4.6", "3.1.2.2.4.8"
                    , "3.1.2.3.4.2", "3.1.2.3.4.4", "3.1.2.3.4.6", "3.1.2.3.4.8"
                    , "3.1.2.4.4.2", "3.1.2.4.4.4", "3.1.2.4.4.6", "3.1.2.4.4.8"
                    , "3.1.2.5.4.2", "3.1.2.5.4.4", "3.1.2.5.4.6", "3.1.2.5.4.8")
         , ms_d1 = "(Type of Product): Inputs: Other non-durable inputs, such as fertilizer and pesticides"
         , name = "(Type of Product): Inputs: Other Non-Durable Inputs, Such as Fertilizers and Pesticides"
  ),
  ##(Type of Product): Inputs: Durable Equipment and Machinery##
  tibble(ic = "EG.3.2-26"
         , udn =  c("3.1.3.1.3.1", "3.1.3.1.3.3", "3.1.3.1.3.5", "3.1.3.1.3.7"
                    , "3.1.3.2.3.1", "3.1.3.2.3.3", "3.1.3.2.3.5", "3.1.3.2.3.7"
                    , "3.1.3.3.3.1", "3.1.3.3.3.3", "3.1.3.3.3.5", "3.1.3.3.3.7"
                    , "3.1.3.4.3.1", "3.1.3.4.3.3", "3.1.3.4.3.5", "3.1.3.4.3.7"
                    , "3.1.3.5.3.1", "3.1.3.5.3.3", "3.1.3.5.3.5", "3.1.3.5.3.7"
                    , "3.1.3.1.3.2", "3.1.3.1.3.4", "3.1.3.1.3.6", "3.1.3.1.3.8"
                    , "3.1.3.2.3.2", "3.1.3.2.3.4", "3.1.3.2.3.6", "3.1.3.2.3.8"
                    , "3.1.3.3.3.2", "3.1.3.3.3.4", "3.1.3.3.3.6", "3.1.3.3.3.8"
                    , "3.1.3.4.3.2", "3.1.3.4.3.4", "3.1.3.4.3.6", "3.1.3.4.3.8"
                    , "3.1.3.5.3.2", "3.1.3.5.3.4", "3.1.3.5.3.6", "3.1.3.5.3.8"
                    , "3.1.3.1.4.1", "3.1.3.1.4.3", "3.1.3.1.4.5", "3.1.3.1.4.7"
                    , "3.1.3.2.4.1", "3.1.3.2.4.3", "3.1.3.2.4.5", "3.1.3.2.4.7"
                    , "3.1.3.3.4.1", "3.1.3.3.4.3", "3.1.3.3.4.5", "3.1.3.3.4.7"
                    , "3.1.3.4.4.1", "3.1.3.4.4.3", "3.1.3.4.4.5", "3.1.3.4.4.7"
                    , "3.1.3.5.4.1", "3.1.3.5.4.3", "3.1.3.5.4.5", "3.1.3.5.4.7"
                    , "3.1.3.1.4.2", "3.1.3.1.4.4", "3.1.3.1.4.6", "3.1.3.1.4.8"
                    , "3.1.3.2.4.2", "3.1.3.2.4.4", "3.1.3.2.4.6", "3.1.3.2.4.8"
                    , "3.1.3.3.4.2", "3.1.3.3.4.4", "3.1.3.3.4.6", "3.1.3.3.4.8"
                    , "3.1.3.4.4.2", "3.1.3.4.4.4", "3.1.3.4.4.6", "3.1.3.4.4.8"
                    , "3.1.3.5.4.2", "3.1.3.5.4.4", "3.1.3.5.4.6", "3.1.3.5.4.8")
         , ms_d1 = "(Type of Product): Inputs: Durable equipment and machinery"
         , name = "(Type of Product): Inputs: Durable Equipment and Machinery"
  ),
  ##(Type of Product): Processed Products/Value Added Products (Post Harvest)##
  tibble(ic = "EG.3.2-26"
         , udn =  c("3.1.4.1.3.1", "3.1.4.1.3.3", "3.1.4.1.3.5", "3.1.4.1.3.7"
                    , "3.1.4.2.3.1", "3.1.4.2.3.3", "3.1.4.2.3.5", "3.1.4.2.3.7"
                    , "3.1.4.3.3.1", "3.1.4.3.3.3", "3.1.4.3.3.5", "3.1.4.3.3.7"
                    , "3.1.4.4.3.1", "3.1.4.4.3.3", "3.1.4.4.3.5", "3.1.4.4.3.7"
                    , "3.1.4.5.3.1", "3.1.4.5.3.3", "3.1.4.5.3.5", "3.1.4.5.3.7"
                    , "3.1.4.1.3.2", "3.1.4.1.3.4", "3.1.4.1.3.6", "3.1.4.1.3.8"
                    , "3.1.4.2.3.2", "3.1.4.2.3.4", "3.1.4.2.3.6", "3.1.4.2.3.8"
                    , "3.1.4.3.3.2", "3.1.4.3.3.4", "3.1.4.3.3.6", "3.1.4.3.3.8"
                    , "3.1.4.4.3.2", "3.1.4.4.3.4", "3.1.4.4.3.6", "3.1.4.4.3.8"
                    , "3.1.4.5.3.2", "3.1.4.5.3.4", "3.1.4.5.3.6", "3.1.4.5.3.8"
                    , "3.1.4.1.4.1", "3.1.4.1.4.3", "3.1.4.1.4.5", "3.1.4.1.4.7"
                    , "3.1.4.2.4.1", "3.1.4.2.4.3", "3.1.4.2.4.5", "3.1.4.2.4.7"
                    , "3.1.4.3.4.1", "3.1.4.3.4.3", "3.1.4.3.4.5", "3.1.4.3.4.7"
                    , "3.1.4.4.4.1", "3.1.4.4.4.3", "3.1.4.4.4.5", "3.1.4.4.4.7"
                    , "3.1.4.5.4.1", "3.1.4.5.4.3", "3.1.4.5.4.5", "3.1.4.5.4.7"
                    , "3.1.4.1.4.2", "3.1.4.1.4.4", "3.1.4.1.4.6", "3.1.4.1.4.8"
                    , "3.1.4.2.4.2", "3.1.4.2.4.4", "3.1.4.2.4.6", "3.1.4.2.4.8"
                    , "3.1.4.3.4.2", "3.1.4.3.4.4", "3.1.4.3.4.6", "3.1.4.3.4.8"
                    , "3.1.4.4.4.2", "3.1.4.4.4.4", "3.1.4.4.4.6", "3.1.4.4.4.8"
                    , "3.1.4.5.4.2", "3.1.4.5.4.4", "3.1.4.5.4.6", "3.1.4.5.4.8")
         , ms_d1 = "(Type of Product): Processed products/value added products (post-harvest)"
         , name = "(Type of Product): Processed Products/Value Added Products (Post Harvest)"
  ),
  ##(Type of Product): Post-Harvest Storage and Processing Equipment##
  tibble(ic = "EG.3.2-26"
         , udn =  c("3.1.5.1.3.1", "3.1.5.1.3.3", "3.1.5.1.3.5", "3.1.5.1.3.7"
                    , "3.1.5.2.3.1", "3.1.5.2.3.3", "3.1.5.2.3.5", "3.1.5.2.3.7"
                    , "3.1.5.3.3.1", "3.1.5.3.3.3", "3.1.5.3.3.5", "3.1.5.3.3.7"
                    , "3.1.5.4.3.1", "3.1.5.4.3.3", "3.1.5.4.3.5", "3.1.5.4.3.7"
                    , "3.1.5.5.3.1", "3.1.5.5.3.3", "3.1.5.5.3.5", "3.1.5.5.3.7"
                    , "3.1.5.1.3.2", "3.1.5.1.3.4", "3.1.5.1.3.6", "3.1.5.1.3.8"
                    , "3.1.5.2.3.2", "3.1.5.2.3.4", "3.1.5.2.3.6", "3.1.5.2.3.8"
                    , "3.1.5.3.3.2", "3.1.5.3.3.4", "3.1.5.3.3.6", "3.1.5.3.3.8"
                    , "3.1.5.4.3.2", "3.1.5.4.3.4", "3.1.5.4.3.6", "3.1.5.4.3.8"
                    , "3.1.5.5.3.2", "3.1.5.5.3.4", "3.1.5.5.3.6", "3.1.5.5.3.8"
                    , "3.1.5.1.4.1", "3.1.5.1.4.3", "3.1.5.1.4.5", "3.1.5.1.4.7"
                    , "3.1.5.2.4.1", "3.1.5.2.4.3", "3.1.5.2.4.5", "3.1.5.2.4.7"
                    , "3.1.5.3.4.1", "3.1.5.3.4.3", "3.1.5.3.4.5", "3.1.5.3.4.7"
                    , "3.1.5.4.4.1", "3.1.5.4.4.3", "3.1.5.4.4.5", "3.1.5.4.4.7"
                    , "3.1.5.5.4.1", "3.1.5.5.4.3", "3.1.5.5.4.5", "3.1.5.5.4.7"
                    , "3.1.5.1.4.2", "3.1.5.1.4.4", "3.1.5.1.4.6", "3.1.5.1.4.8"
                    , "3.1.5.2.4.2", "3.1.5.2.4.4", "3.1.5.2.4.6", "3.1.5.2.4.8"
                    , "3.1.5.3.4.2", "3.1.5.3.4.4", "3.1.5.3.4.6", "3.1.5.3.4.8"
                    , "3.1.5.4.4.2", "3.1.5.4.4.4", "3.1.5.4.4.6", "3.1.5.4.4.8"
                    , "3.1.5.5.4.2", "3.1.5.5.4.4", "3.1.5.5.4.6", "3.1.5.5.4.8")
         , ms_d1 = "(Type of Product): Post-harvest storage and processing equipment"
         , name = "(Type of Product): Post-Harvest Storage and Processing Equipment"
  ),
  ##(Type of Product): Disaggregates Not Available##
  tibble(ic = "EG.3.2-26"
         , udn =  c("3.1.6.1.3.1", "3.1.6.1.3.3", "3.1.6.1.3.5", "3.1.6.1.3.7"
                    , "3.1.6.2.3.1", "3.1.6.2.3.3", "3.1.6.2.3.5", "3.1.6.2.3.7"
                    , "3.1.6.3.3.1", "3.1.6.3.3.3", "3.1.6.3.3.5", "3.1.6.3.3.7"
                    , "3.1.6.4.3.1", "3.1.6.4.3.3", "3.1.6.4.3.5", "3.1.6.4.3.7"
                    , "3.1.6.5.3.1", "3.1.6.5.3.3", "3.1.6.5.3.5", "3.1.6.5.3.7"
                    , "3.1.6.1.3.2", "3.1.6.1.3.4", "3.1.6.1.3.6", "3.1.6.1.3.8"
                    , "3.1.6.2.3.2", "3.1.6.2.3.4", "3.1.6.2.3.6", "3.1.6.2.3.8"
                    , "3.1.6.3.3.2", "3.1.6.3.3.4", "3.1.6.3.3.6", "3.1.6.3.3.8"
                    , "3.1.6.4.3.2", "3.1.6.4.3.4", "3.1.6.4.3.6", "3.1.6.4.3.8"
                    , "3.1.6.5.3.2", "3.1.6.5.3.4", "3.1.6.5.3.6", "3.1.6.5.3.8"
                    , "3.1.6.1.4.1", "3.1.6.1.4.3", "3.1.6.1.4.5", "3.1.6.1.4.7"
                    , "3.1.6.2.4.1", "3.1.6.2.4.3", "3.1.6.2.4.5", "3.1.6.2.4.7"
                    , "3.1.6.3.4.1", "3.1.6.3.4.3", "3.1.6.3.4.5", "3.1.6.3.4.7"
                    , "3.1.6.4.4.1", "3.1.6.4.4.3", "3.1.6.4.4.5", "3.1.6.4.4.7"
                    , "3.1.6.5.4.1", "3.1.6.5.4.3", "3.1.6.5.4.5", "3.1.6.5.4.7"
                    , "3.1.6.1.4.2", "3.1.6.1.4.4", "3.1.6.1.4.6", "3.1.6.1.4.8"
                    , "3.1.6.2.4.2", "3.1.6.2.4.4", "3.1.6.2.4.6", "3.1.6.2.4.8"
                    , "3.1.6.3.4.2", "3.1.6.3.4.4", "3.1.6.3.4.6", "3.1.6.3.4.8"
                    , "3.1.6.4.4.2", "3.1.6.4.4.4", "3.1.6.4.4.6", "3.1.6.4.4.8"
                    , "3.1.6.5.4.2", "3.1.6.5.4.4", "3.1.6.5.4.6", "3.1.6.5.4.8")
         , ms_d1 = "(Type of Product): Disaggregates Not Available"
         , name = "(Type of Product): Disaggregates Not Available"
  ),
  ##(Type of Service): Business Services, Including Financial, Entrepreneurial, Legal, etc.##
  tibble(ic = "EG.3.2-26"
         , udn =  c("3.2.1.1.3.1", "3.2.1.1.3.3", "3.2.1.1.3.5", "3.2.1.1.3.7"
                    , "3.2.1.2.3.1", "3.2.1.2.3.3", "3.2.1.2.3.5", "3.2.1.2.3.7"
                    , "3.2.1.3.3.1", "3.2.1.3.3.3", "3.2.1.3.3.5", "3.2.1.3.3.7"
                    , "3.2.1.1.3.2", "3.2.1.1.3.4", "3.2.1.1.3.6", "3.2.1.1.3.8"
                    , "3.2.1.2.3.2", "3.2.1.2.3.4", "3.2.1.2.3.6", "3.2.1.2.3.8"
                    , "3.2.1.3.3.2", "3.2.1.3.3.4", "3.2.1.3.3.6", "3.2.1.3.3.8"
                    , "3.2.1.1.4.1", "3.2.1.1.4.3", "3.2.1.1.4.5", "3.2.1.1.4.7"
                    , "3.2.1.2.4.1", "3.2.1.2.4.3", "3.2.1.2.4.5", "3.2.1.2.4.7"
                    , "3.2.1.3.4.1", "3.2.1.3.4.3", "3.2.1.3.4.5", "3.2.1.3.4.7"
                    , "3.2.1.1.4.2", "3.2.1.1.4.4", "3.2.1.1.4.6", "3.2.1.1.4.8"
                    , "3.2.1.2.4.2", "3.2.1.2.4.4", "3.2.1.2.4.6", "3.2.1.2.4.8"
                    , "3.2.1.3.4.2", "3.2.1.3.4.4", "3.2.1.3.4.6", "3.2.1.3.4.8")
         , ms_d1 = "(Type of Service): Business services, including financial, entrepreneurial, legal, etc."
         , name = "(Type of Service): Business Services, Including Financial, Entrepreneurial, Legal, etc."
  ),
  ##(Type of Service): Information Services:  SMS, Radio, TV, Print, etc.##
  tibble(ic = "EG.3.2-26"
         , udn =  c("3.2.2.1.3.1", "3.2.2.1.3.3", "3.2.2.1.3.5", "3.2.2.1.3.7"
                    , "3.2.2.2.3.1", "3.2.2.2.3.3", "3.2.2.2.3.5", "3.2.2.2.3.7"
                    , "3.2.2.3.3.1", "3.2.2.3.3.3", "3.2.2.3.3.5", "3.2.2.3.3.7"
                    , "3.2.2.1.3.2", "3.2.2.1.3.4", "3.2.2.1.3.6", "3.2.2.1.3.8"
                    , "3.2.2.2.3.2", "3.2.2.2.3.4", "3.2.2.2.3.6", "3.2.2.2.3.8"
                    , "3.2.2.3.3.2", "3.2.2.3.3.4", "3.2.2.3.3.6", "3.2.2.3.3.8"
                    , "3.2.2.1.4.1", "3.2.2.1.4.3", "3.2.2.1.4.5", "3.2.2.1.4.7"
                    , "3.2.2.2.4.1", "3.2.2.2.4.3", "3.2.2.2.4.5", "3.2.2.2.4.7"
                    , "3.2.2.3.4.1", "3.2.2.3.4.3", "3.2.2.3.4.5", "3.2.2.3.4.7"
                    , "3.2.2.1.4.2", "3.2.2.1.4.4", "3.2.2.1.4.6", "3.2.2.1.4.8"
                    , "3.2.2.2.4.2", "3.2.2.2.4.4", "3.2.2.2.4.6", "3.2.2.2.4.8"
                    , "3.2.2.3.4.2", "3.2.2.3.4.4", "3.2.2.3.4.6", "3.2.2.3.4.8")
         , ms_d1 = "(Type of Service): Information services: SMS, Radio, TV, print, etc."
         , name = "(Type of Service): Information Services:  SMS, Radio, TV, Print, etc."
  ),
  ##(Type of Service): Production Support Services##
  tibble(ic = "EG.3.2-26"
         , udn =  c("3.2.3.1.3.1", "3.2.3.1.3.3", "3.2.3.1.3.5", "3.2.3.1.3.7"
                    , "3.2.3.2.3.1", "3.2.3.2.3.3", "3.2.3.2.3.5", "3.2.3.2.3.7"
                    , "3.2.3.3.3.1", "3.2.3.3.3.3", "3.2.3.3.3.5", "3.2.3.3.3.7"
                    , "3.2.3.1.3.2", "3.2.3.1.3.4", "3.2.3.1.3.6", "3.2.3.1.3.8"
                    , "3.2.3.2.3.2", "3.2.3.2.3.4", "3.2.3.2.3.6", "3.2.3.2.3.8"
                    , "3.2.3.3.3.2", "3.2.3.3.3.4", "3.2.3.3.3.6", "3.2.3.3.3.8"
                    , "3.2.3.1.4.1", "3.2.3.1.4.3", "3.2.3.1.4.5", "3.2.3.1.4.7"
                    , "3.2.3.2.4.1", "3.2.3.2.4.3", "3.2.3.2.4.5", "3.2.3.2.4.7"
                    , "3.2.3.3.4.1", "3.2.3.3.4.3", "3.2.3.3.4.5", "3.2.3.3.4.7"
                    , "3.2.3.1.4.2", "3.2.3.1.4.4", "3.2.3.1.4.6", "3.2.3.1.4.8"
                    , "3.2.3.2.4.2", "3.2.3.2.4.4", "3.2.3.2.4.6", "3.2.3.2.4.8"
                    , "3.2.3.3.4.2", "3.2.3.3.4.4", "3.2.3.3.4.6", "3.2.3.3.4.8")
         , ms_d1 = "(Type of Service): Production support services (see full list in Handbook)"
         , name = "(Type of Service): Production Support Services"
  ),
  ##(Type of Service): Disaggregates Not Available##
  tibble(ic = "EG.3.2-26"
         , udn =  c("3.2.4.1.3.1", "3.2.4.1.3.3", "3.2.4.1.3.5", "3.2.4.1.3.7"
                    , "3.2.4.2.3.1", "3.2.4.2.3.3", "3.2.4.2.3.5", "3.2.4.2.3.7"
                    , "3.2.4.3.3.1", "3.2.4.3.3.3", "3.2.4.3.3.5", "3.2.4.3.3.7"
                    , "3.2.4.1.3.2", "3.2.4.1.3.4", "3.2.4.1.3.6", "3.2.4.1.3.8"
                    , "3.2.4.2.3.2", "3.2.4.2.3.4", "3.2.4.2.3.6", "3.2.4.2.3.8"
                    , "3.2.4.3.3.2", "3.2.4.3.3.4", "3.2.4.3.3.6", "3.2.4.3.3.8"
                    , "3.2.4.1.4.1", "3.2.4.1.4.3", "3.2.4.1.4.5", "3.2.4.1.4.7"
                    , "3.2.4.2.4.1", "3.2.4.2.4.3", "3.2.4.2.4.5", "3.2.4.2.4.7"
                    , "3.2.4.3.4.1", "3.2.4.3.4.3", "3.2.4.3.4.5", "3.2.4.3.4.7"
                    , "3.2.4.1.4.2", "3.2.4.1.4.4", "3.2.4.1.4.6", "3.2.4.1.4.8"
                    , "3.2.4.2.4.2", "3.2.4.2.4.4", "3.2.4.2.4.6", "3.2.4.2.4.8"
                    , "3.2.4.3.4.2", "3.2.4.3.4.4", "3.2.4.3.4.6", "3.2.4.3.4.8")
         , ms_d1 = "(Type of Service): Disaggregates Not Available"
         , name = "(Type of Service): Disaggregates Not Available"
  ),
  ##Commodity##
  tibble(ic = "EG.3.2-26"
         , udn =  c("3.3.1.4.1", "3.3.1.4.4", "3.3.1.4.7", "3.3.1.4.10"
                    , "3.3.2.4.1", "3.3.2.4.4", "3.3.2.4.7", "3.3.2.4.10"
                    , "3.3.3.4.1", "3.3.3.4.4", "3.3.3.4.7", "3.3.3.4.10"
                    , "3.3.4.4.1", "3.3.4.4.4", "3.3.4.4.7", "3.3.4.4.10"
                    , "3.3.5.4.1", "3.3.5.4.4", "3.3.5.4.7", "3.3.5.4.10"
                    , "3.3.1.4.3", "3.3.1.4.6", "3.3.1.4.9", "3.3.1.4.12"
                    , "3.3.2.4.3", "3.3.2.4.6", "3.3.2.4.9", "3.3.2.4.12"
                    , "3.3.3.4.3", "3.3.3.4.6", "3.3.3.4.9", "3.3.3.4.12"
                    , "3.3.4.4.3", "3.3.4.4.6", "3.3.4.4.9", "3.3.4.4.12"
                    , "3.3.5.4.3", "3.3.5.4.6", "3.3.5.4.9", "3.3.5.4.12"
                    , "3.3.1.4.2", "3.3.1.4.5", "3.3.1.4.8", "3.3.1.4.11"
                    , "3.3.2.4.2", "3.3.2.4.5", "3.3.2.4.8", "3.3.2.4.11"
                    , "3.3.3.4.2", "3.3.3.4.5", "3.3.3.4.8", "3.3.3.4.11"
                    , "3.3.4.4.2", "3.3.4.4.5", "3.3.4.4.8", "3.3.4.4.11"
                    , "3.3.5.4.2", "3.3.5.4.5", "3.3.5.4.8", "3.3.5.4.11"
                    , "3.3.1.5.1", "3.3.1.5.4", "3.3.1.5.7", "3.3.1.5.10"
                    , "3.3.2.5.1", "3.3.2.5.4", "3.3.2.5.7", "3.3.2.5.10"
                    , "3.3.3.5.1", "3.3.3.5.4", "3.3.3.5.7", "3.3.3.5.10"
                    , "3.3.4.5.1", "3.3.4.5.4", "3.3.4.5.7", "3.3.4.5.10"
                    , "3.3.5.5.1", "3.3.5.5.4", "3.3.5.5.7", "3.3.5.5.10"
                    , "3.3.1.5.3", "3.3.1.5.6", "3.3.1.5.9", "3.3.1.5.12"
                    , "3.3.2.5.3", "3.3.2.5.6", "3.3.2.5.9", "3.3.2.5.12"
                    , "3.3.3.5.3", "3.3.3.5.6", "3.3.3.5.9", "3.3.3.5.12"
                    , "3.3.4.5.3", "3.3.4.5.6", "3.3.4.5.9", "3.3.4.5.12"
                    , "3.3.5.5.3", "3.3.5.5.6", "3.3.5.5.9", "3.3.5.5.12"
                    , "3.3.1.5.2", "3.3.1.5.5", "3.3.1.5.8", "3.3.1.5.11"
                    , "3.3.2.5.2", "3.3.2.5.5", "3.3.2.5.8", "3.3.2.5.11"
                    , "3.3.3.5.2", "3.3.3.5.5", "3.3.3.5.8", "3.3.3.5.11"
                    , "3.3.4.5.2", "3.3.4.5.5", "3.3.4.5.8", "3.3.4.5.11"
                    , "3.3.5.5.2", "3.3.5.5.5", "3.3.5.5.8", "3.3.5.5.11")
         , name = "Commodity"
  ),
  tibble(ic = "EG.3.2-26"
         , ms_d1 = c(
           "Apples", "Avocado", "Bananas (NRVCC)", "Beans (biofortified) (NRVCC)"
           , "Beans (non-biofortified) (NRVCC)", "Beans and pulses (NRVCC)"
           , "Cabbage (NRVCC)", "Camel (live) (NRVCC)", "Carrots (NRVCC)"
           , "Cashews (NRVCC)", "Cassava", "Cattle (Beef) (NRVCC)"
           , "Cattle (live) (NRVCC)", "Cauliflower (NRVCC)"
           , "Chickens (poultry) (NRVCC)", "Chickpea (NRVCC)"
           , "Chilies (NRVCC)", "Citrus (NRVCC)", "Coffee", "Commodity"
           , "Cowpeas (NRVCC)", "Cucumber", "Dairy (non-milk products, e.g. yogurt) (NRVCC)"
           , "Disaggregates Not Available or Other", "Eggplant", "Eggs (NRVCC)"
           , "Fava Beans (NRVCC)", "Fish (ponds) (NRVCC)", "Forage/Fodder"
           , "Fruits", "Goat (live) (NRVCC)", "Green Beans", "Groundnuts/peanuts (NRVCC)"
           , "Honey", "Horticulture", "Lentil (NRVCC)", "Lettuce", "Maize"
           , "Maize grain", "Mango (NRVCC)", "Milk (Cow) (NRVCC)"
           , "Milk (general, not animal-specific) (NRVCC)", "Millet"
           , "Mung Bean (NRVCC)", "Not Applicable", "Okra (NRVCC)", "Onions/Shallots"
           , "Papaya (NRVCC)", "Paprika", "Passion fruit (NRVCC)", "Peas, green (NRVCC)"
           , "Pigeon peas (NRVCC)", "Pineapples (NRVCC)", "Potatoes", "Pulses (NRVCC)"
           , "Rice", "Rice grain", "Rice-irrigated", "Sesame Seed (NRVCC)"
           , "Sesame Seed (oil)", "Sheep (live) (NRVCC)", "Sorghum", "Soybean Rain-fed (NRVCC)"
           , "Soybeans (NRVCC)", "Soybeans (oil)", "Sunflower (oil)", "Sunflower seed (NRVCC)"
           , "Sweet Potatoes", "Sweet Potatoes - Orange/Dark Yellow - biofortified (NRVCC)"
           , "Sweet Potatoes - Orange/Dark Yellow - non biofortified (NRVCC)"
           , "Sweet Potatoes - White/Pale Yellow", "Tomatoes", "Vegetables"
           , "Watermelon", "Wheat")
         , name = "Commodity"
  ),
  ##EG.3.2-27##
  ##Total Value of Financing Received by Recipient Size, Sex, and Age##
  tibble(ic = "EG.3.2-27"
         , udn =  c("3.1.1.1", "3.1.1.2", "3.1.1.3", "3.1.1.4", "3.1.2.1"
                    , "3.1.2.2", "3.1.2.3", "3.1.2.4", "3.1.3.1", "3.1.3.2"
                    , "3.1.3.3", "3.1.3.4")
         , ms_d1 = "TOTAL VALUE of financing received by recipient Size, Sex, and Age:"
         , name = "Total Value of Financing Received"
  ),
  ##Total Number of Financing Recipients by Size, Sex, and Age##
  tibble(ic = "EG.3.2-27"
         , udn =  c("3.2.1.1", "3.2.1.2", "3.2.1.3", "3.2.1.4", "3.2.2.1"
                    , "3.2.2.2", "3.2.2.3", "3.2.2.4", "3.2.3.1", "3.2.3.2"
                    , "3.2.3.3", "3.2.3.4")
         , ms_d1 = "TOTAL NUMBER of financing recipients by Size, Sex, and Age:"
         , name = "Total Number of Financing Recipients"
  ),
  ##Total Value of Debt Financing Received by Recipient Size, Sex, and Age##
  tibble(ic = "EG.3.2-27"
         , udn =  c("3.3.1.1", "3.3.1.2", "3.3.1.3", "3.3.1.4", "3.3.2.1"
                    , "3.3.2.2", "3.3.2.3", "3.3.2.4", "3.3.3.1", "3.3.3.2"
                    , "3.3.3.3", "3.3.3.4")
         , ms_d1 = "TOTAL VALUE of DEBT financing received by recipient Size, Sex, and Age:"
         , name = "Total Value of Debt Financing Received"
  ),
  ##Total Number of Debt Financing Recipients by Size, Sex, and Age##

  tibble(ic = "EG.3.2-27"
         , udn =  c("3.4.1.1", "3.4.1.2", "3.4.1.3", "3.4.1.4", "3.4.2.1"
                    , "3.4.2.2", "3.4.2.3", "3.4.2.4", "3.4.3.1", "3.4.3.2"
                    , "3.4.3.3", "3.4.3.4")
         , ms_d1 = "TOTAL NUMBER of DEBT financing recipients by Size, Sex, and Age:"
         , name = "Total Number of Debt Financing Recipients"
  ),
  ##Type of Financing Accessed: Cash Debt##
  tibble(ic = "EG.3.2-27"
         , udn =  c("3.5.1.1.1", "3.5.1.1.2", "3.5.1.1.3", "3.5.1.1.4"
                    , "3.5.1.2.1", "3.5.1.2.2", "3.5.1.2.3", "3.5.1.2.4"
                    , "3.5.1.3.1", "3.5.1.3.2", "3.5.1.3.3", "3.5.1.3.4"
                    , "3.5.2.1.1", "3.5.2.1.2", "3.5.2.1.3", "3.5.2.1.4"
                    , "3.5.2.2.1", "3.5.2.2.2", "3.5.2.2.3", "3.5.2.2.4"
                    , "3.5.2.3.1", "3.5.2.3.2", "3.5.2.3.3", "3.5.2.3.4")
         , ms_d1 = "Type of financing accessed: Cash Debt"
         , name = "Type of Financing Accessed: Cash Debt"
  ),
  ##Type of Financing Accessed: In-Kind Debt##
  tibble(ic = "EG.3.2-27"
         , udn =  c("3.6.1.1.1", "3.6.1.1.2", "3.6.1.1.3", "3.6.1.1.4"
                    , "3.6.1.2.1", "3.6.1.2.2", "3.6.1.2.3", "3.6.1.2.4"
                    , "3.6.1.3.1", "3.6.1.3.2", "3.6.1.3.3", "3.6.1.3.4"
                    , "3.6.2.1.1", "3.6.2.1.2", "3.6.2.1.3", "3.6.2.1.4"
                    , "3.6.2.2.1", "3.6.2.2.2", "3.6.2.2.3", "3.6.2.2.4"
                    , "3.6.2.3.1", "3.6.2.3.2", "3.6.2.3.3", "3.6.2.3.4")
         , ms_d1 = "Type of financing accessed: In-Kind Debt"
         , name = "Type of Financing Accessed: In-Kind Debt"
  ),
  ##Type of Financing Accessed: Non-Debt##
  tibble(ic = "EG.3.2-27"
         , udn =  c("3.7.1.1.1", "3.7.1.1.2", "3.7.1.1.3", "3.7.1.1.4"
                    , "3.7.1.2.1", "3.7.1.2.2", "3.7.1.2.3", "3.7.1.2.4"
                    , "3.7.1.3.1", "3.7.1.3.2", "3.7.1.3.3", "3.7.1.3.4"
                    , "3.7.2.1.1", "3.7.2.1.2", "3.7.2.1.3", "3.7.2.1.4"
                    , "3.7.2.2.1", "3.7.2.2.2", "3.7.2.2.3", "3.7.2.2.4"
                    , "3.7.2.3.1", "3.7.2.3.2", "3.7.2.3.3", "3.7.2.3.4")
         , ms_d1 = "Type of financing accessed: Non-Debt"
         , name = "Type of Financing Accessed: Non-Debt"
  ),
  ##EG.3.2-28##
  tibble(ic = "EG.3.2-28"
         , name = "Number of Hectares Under Improved Management Practices or Technologies That Promote Improved Climate Risk Reduction and/or Natural Resources Management"
  ),
  ##EG.3.3-10##
  ##Percentage of Female Participants Consuming a Diet of Minimum Diversity##
  tibble(ic = "EG.3.3-10"
         , udn =  c("3.1.1.1", "3.1.2.1", "3.1.3.1")
         , ms_d3 = "Percentage of female participants consuming a diet of minimum diversity"
         , name = "Percentage of Female Participants Consuming a Diet of Minimum Diversity"
  ),
  ##Number of Female Participants of the Nutrition-Sensitive Agriculture Activity##
  tibble(ic = "EG.3.3-10"
         , udn =  c("3.1.1.2", "3.1.2.2", "3.1.3.2")
         , ms_d3 = "Number of female participants of the nutrition-sensitive agriculture activity"
         , name = "Number of Female Participants of the Nutrition-Sensitive Agriculture Activity"
  ),
  #EG.4.2-7##
  ##Sex##
  tibble(ic = "EG.4.2-7"
         , udn =  c("3.1.2", "3.1.1", "3.1.3")
         , name = "Sex of participant (no double-counting)"
  ),
  ##Age##
  tibble(ic = "EG.4.2-7"
         , udn =  c("3.2.1", "3.2.2", "3.2.3")
         , name = "Age of participant (no double-counting)"
  ),
  ##Product Type##
  tibble(ic = "EG.4.2-7"
         , udn =  c("3.3.1", "3.3.2", "3.3.3")
         , name = "Product Type (double-counting allowed)"
  ),
  ##Duration##
  tibble(ic = "EG.4.2-7"
         , udn =  c("3.4.1", "3.4.2", "3.4.3")
         , name = "Duration  (no double-counting)"
  ),
  ##EG.10.4-7##
  ##Resource Type: Land##
  tibble(ic = "EG.10.4-7"
         , udn =  c("3.1.1.2", "3.1.1.1", "3.1.1.3", "3.1.2.1", "3.1.2.2"
                    , "3.1.2.3", "3.1.2.4", "3.1.2.5", "3.1.3.1", "3.1.3.2"
                    , "3.1.3.3")
         , ms_d1 = "Resource Type: Land"
         , name = "Resource Type: Land"
  ),
  ##Resource Type: Marine##
  tibble(ic = "EG.10.4-7"
         , udn =  c("3.2.1.2", "3.2.1.1", "3.2.1.3", "3.2.2.1", "3.2.2.2"
                    , "3.2.2.3", "3.2.2.4", "3.2.2.5", "3.2.3.1", "3.2.3.2"
                    , "3.2.3.3")
         , ms_d1 = "Resource Type: Marine"
         , name = "Resource Type: Marine"
  ),
  ##EG.10.4-8##
  ##Resource Type: Land##
  tibble(ic = "EG.10.4-8"
         , udn =  c("3.1.1.1", "3.1.1.2", "3.1.1.3", "3.1.2.1", "3.1.2.2"
                    , "3.1.2.3", "3.1.2.4", "3.1.2.5", "3.1.2.6", "3.1.2.7"
                    , "3.1.2.8", "3.1.3.1", "3.1.3.2", "3.1.3.3")
         , ms_d1 = "Resource Type: Land", name = "Resource Type: Land"
  ),
  ##Resource Type: Marine##
  tibble(ic = "EG.10.4-8"
         , udn =  c("3.2.1.1", "3.2.1.2", "3.2.1.3", "3.2.2.1", "3.2.2.2"
                    , "3.2.2.3", "3.2.2.4", "3.2.2.5", "3.2.2.6", "3.2.2.7"
                    , "3.2.2.8", "3.2.3.1", "3.2.3.2", "3.2.3.3")
         , ms_d1 = "Resource Type: Marine", name = "Resource Type: Marine"
  ),
  ##ES.5-1##
  ##Sex##
  tibble(ic = "ES.5-1", udn =  c("3.1.2", "3.1.1", "3.1.3"), name = "Sex"
  ),
  ##Age##
  tibble(ic = "ES.5-1", udn =  c("3.2.1", "3.2.2", "3.2.3"), name = "Age Category"
  ),
  ##Duration##
  tibble(ic = "ES.5-1"
         , udn =  c("3.3.1", "3.3.2", "3.3.3")
         , name = "Duration"
  ),
  ##Type of Asset##
  tibble(ic = "ES.5-1", udn =  c("3.4.1", "3.4.2", "3.4.3", "3.4.4")
         , name = "Type of Asset Strengthened"
  ),
  ##GNDR-2 (Number)##
  ##Number of Female Program Participants##
  tibble(ic = "GNDR-2", udn =  "3.1"
         , ms_d1 = "Numerator: Number of female program participants"
         , name = "Number of Female Program Participants"
  ),
  ##Number of Total Program Participants##
  tibble(ic = "GNDR-2", udn =  "3.2"
         , ms_d1 = "Denominator: Total number of male and female participants in the program"
         , name = "Number of Total Program Participants"
  ),
  ## HL.8.2-2##
  ##Sex##
  tibble(ic = "HL.8.2-2", udn =  c("3.1.2", "3.1.1", "3.1.3"), name = "Sex"
  ),
  ##Location##
  tibble(ic = "HL.8.2-2", udn =  c("3.2.1", "3.2.2", "3.2.3"), name = "Location"
  ),
  ##Wealth Quintile##
  tibble(ic = "HL.8.2-2"
         , udn =  c("3.3.1", "3.3.2", "3.3.3", "3.3.4", "3.3.5", "3.3.6")
         , name = "Wealth Quintile"
  ),
  ##HL.8.2-5##
  ##Urban##
  tibble(ic = "HL.8.2-5", udn =  c("3.2.1.1", "3.2.1.2")
         , ms_d2 = "Urban - percentage of households with soap and water at a handwashing station"
         , name = "Urban"
  ),
  ##Rural##
  tibble(ic = "HL.8.2-5", udn =  c("3.2.2.1", "3.2.2.2")
         , ms_d2 = "Rural - percentage of households with soap and water at a handwashing station"
         , name = "Rural"
  ),
  ##DNA##
  tibble(ic = "HL.8.2-5", udn =  c("3.2.3.1", "3.2.3.2")
         , ms_d2 = "Disaggregates Not Available (for Residence Location) - percentage of households with soap and water at a handwashing station"
         , name = "Disaggregates Not Available"
  ),
  ##HL.9-1##
  ##Sex##
  tibble(ic = "HL.9-1", udn =  c("3.1.2", "3.1.1", "3.1.3")
         , ms_d1 = "Sex (no double-counting allowed)", name = "Sex"
  ),
  ##Intervention##
  tibble(ic = "HL.9-1"
         , udn =  c("3.2.1", "3.2.2", "3.2.3", "3.2.4", "3.2.5", "3.2.6", "3.2.7")
         , ms_d1 = "Intervention (double-counting allowed)"
         , name = "Intervention (Double-Counting Allowed)"
  ),
  ##HL.9-2 (Participants by Sex)##
  ##Female##
  tibble(ic = "HL.9-2", udn = "3.2", name = "Female"
  ),
  ##Males##
  tibble(ic = "HL.9-2", udn = "3.1", name = "Male"
  ),
  ##DNA##
  tibble(ic = "HL.9-2", udn = "3.3", name = "Disaggregates Not Available"
  ),
  ##FTFMS##
  tibble(ic = "HL.9-2", name = "ms_d1"
  ),
  ##HL.9-3##
  ##Women < 19##
  tibble(ic = "HL.9-3", udn =  c("3.1.1", "3.1.2", "3.1.3")
         , ms_d1 = "Age (no double counting)", name = "Age"
  ),
  ##Intervention##
  ##Number of Women Receiving Iron and Folic Acid Supplementation##
  tibble(ic = "HL.9-3", udn =  c("3.2.1", "3.2.2", "3.2.3", "3.2.4", "3.2.5")
         , ms_d1 = "Intervention (double-counting allowed)"
         , name = "Intervention (Double-Counting Allowed)"
  ),
  ##HL.9-4##
  ##Sex##
  tibble(ic = "HL.9-4", udn =  c("3.1.2", "3.1.1", "3.1.3"), ms_d1 = "Sex"
         , name = "Sex"
  ),
  ##Type of Training##
  tibble(ic = "HL.9-4", udn =  c("3.2.1", "3.2.2", "3.2.3", "3.2.4", "3.2.5")
         , ms_d1 = "Type of Training", name = "Type of Training"
  ),
  ##HL.9-a##
  ##Children 0-59##
  tibble(ic = "HL.9-a"
         , udn =  c("3.1.1", "3.1.2", "3.1.3", "3.1.4", "3.2.1", "3.2.2"
                    , "3.2.3", "3.2.4", "3.7.1.1", "3.7.1.2", "3.7.1.3"
                    , "3.7.1.4", "3.7.2.1", "3.7.2.2", "3.7.2.3", "3.7.2.4"
                    , "3.8.1.1", "3.8.1.2", "3.8.1.3", "3.8.1.4", "3.8.2.1"
                    , "3.8.2.2", "3.8.2.3", "3.8.2.4", "3.9.1.1", "3.9.1.2"
                    , "3.9.1.3", "3.9.1.4", "3.9.2.1", "3.9.2.2", "3.9.2.3"
                    , "3.9.2.4")
         , name = "Children 0-59 Months of Age"
  ),
  ##Children 0-23##
  tibble(ic = "HL.9-a"
         , udn =  c("3.3.1", "3.3.2", "3.3.3", "3.3.4", "3.4.1", "3.4.2"
                    , "3.4.3", "3.4.4")
         , name = "Children 0-23 Months of Age"
  ),
  ##Children 24-59##
  tibble(ic = "HL.9-a"
         , udn =  c("3.5.1", "3.5.2", "3.5.3", "3.5.4", "3.6.1", "3.6.2"
                    , "3.6.3", "3.6.4")
         , name = "Children 24-59 Months of Age"
  ),
  ##HL.9-b##
  ##Children 0-59##
  tibble(ic = "HL.9-b"
         , udn =  c("3.1.1", "3.1.2", "3.1.3", "3.1.4", "3.2.1", "3.2.2"
                    , "3.2.3", "3.2.4", "3.7.1.1", "3.7.1.2", "3.7.1.3"
                    , "3.7.1.4", "3.7.2.1", "3.7.2.2", "3.7.2.3", "3.7.2.4"
                    , "3.8.1.1", "3.8.1.2", "3.8.1.3", "3.8.1.4", "3.8.2.1"
                    , "3.8.2.2", "3.8.2.3", "3.8.2.4", "3.9.1.1", "3.9.1.2"
                    , "3.9.1.3", "3.9.1.4", "3.9.2.1", "3.9.2.2", "3.9.2.3"
                    , "3.9.2.4")
         , name = "Children 0-59 Months of Age"
  ),
  ##Children 0-23##
  tibble(ic = "HL.9-b"
         , udn =  c("3.3.1", "3.3.2", "3.3.3", "3.3.4", "3.4.1", "3.4.2"
                    , "3.4.3", "3.4.4")
         , name = "Children 0-23 Months of Age"
  ),
  ##Children 24-59##
  tibble(ic = "HL.9-b"
         , udn =  c("3.5.1", "3.5.2", "3.5.3", "3.5.4", "3.6.1", "3.6.2"
                    , "3.6.3", "3.6.4")
         , name = "Children 24-59 Months of Age"
  ),
  ##HL.9-d##
  ##Percent of Non-Pregnant Women 15-49 Years of Age That Is Underweight##
  tibble(ic = "HL.9-d"
         , udn =  c("3.1.1", "3.1.2", "3.1.3", "3.1.4")
         , name = "Percent of Non-Pregnant Women 15-49 Years of Age That Is Underweight"
  ),
  ##Number of Non-Pregnant Women 15-49 Years of Age##
  tibble(ic = "HL.9-d"
         , udn =  c("3.2.1", "3.2.2", "3.2.3", "3.2.4")
         , name = "Number of Non-Pregnant Women 15-49 Years of Age"
  ),
  ##Percent of Non-Pregnant Women 15-18 Years of Age That Is Underweight##
  tibble(ic = "HL.9-d", udn =  c("3.3.1", "3.3.2", "3.3.3", "3.3.4")
         , name = "Percent of Non-Pregnant Women 15-18 Years of Age That Is Underweight"
  ),
  ##Number of Non-Pregnant Women 15-18 Years of Age##
  tibble(ic = "HL.9-d", udn =  c("3.4.1", "3.4.2", "3.4.3", "3.4.4")
         , name = "Number of Non-Pregnant Women 15-18 Years of Age"
  ),
  ##Percent of Non-Pregnant Women 19-49 Years of Age That Is Underweight##
  tibble(ic = "HL.9-d", udn =  c("3.5.1", "3.5.2", "3.5.3", "3.5.4")
         , name = "Percent of Non-Pregnant Women 19-49 Years of Age That Is Underweight"
  ),
  ##Number of Non-Pregnant Women 19-49 Years of Age##
  tibble(ic = "HL.9-d", udn =  c("3.6.1", "3.6.2", "3.6.3", "3.6.4")
         , name = "Number of Non-Pregnant Women 19-49 Years of Age"
  ),
  ##HL.9-h##
  ##Age Group##
  tibble(ic = "HL.9-h"
         , udn =  c("3.1.1", "3.1.2", "3.1.3", "3.1.4", "3.1.5", "3.1.6"
                    , "3.1.7", "3.1.8")
         , name = "Age Group"
  ) ,
  ##Sex##
  tibble(ic = "HL.9-h"
         , udn =  c("3.2.1", "3.2.2", "3.2.3", "3.2.4", "3.2.5", "3.2.6")
         , name = "Sex"
  ),
  ##HL.9.1-a##,
  ##Percent of Children 6-23 Months of Age Receiving a Minimum Acceptable Diet##
  tibble(ic = "HL.9.1-a", udn =  c("3.1.1", "3.1.2", "3.1.3", "3.1.4")
         , name = "Percent of Children 6-23 Months of Age Receiving a Minimum Acceptable Diet"
  ),
  ##Number of Children 6-23 Months of Age##
  tibble(ic = "HL.9.1-a", udn =  c("3.2.1", "3.2.2", "3.2.3", "3.2.4")
         , name = "Number of Children 6-23 Months of Age"
  ),
  ##Percent of Males 6-23 months of Age Receiving a Minimum Acceptable Diet##
  tibble(ic = "HL.9.1-a", udn =  c("3.3.1.1", "3.3.1.2", "3.3.1.3", "3.3.1.4")
         , name = "Percent of Males 6-23 months of Age Receiving a Minimum Acceptable Diet"
  ),
  ##Number of Male Children 6-23 Months of Age##
  tibble(ic = "HL.9.1-a", udn =  c("3.3.2.1", "3.3.2.2", "3.3.2.3", "3.3.2.4")
         , name = "Number of Male Children 0-59 Months of Age"
  ),
  ##Percent of Females 0-59 Months of Age Receiving a Minimum Acceptable Diet##
  tibble(ic = "HL.9.1-a", udn =  c("3.4.1.1", "3.4.1.2", "3.4.1.3", "3.4.1.4")
         , name = "Percent of Females 0-59 Months of Age Receiving a Minimum Acceptable Diet"
  ),
  ##Number of Females 6-23 Months of Age##
  tibble(ic = "HL.9.1-a", udn =  c("3.4.2.1", "3.4.2.2", "3.4.2.3", "3.4.2.4")
         , name = "Number of Females 6-23 Months of Age"
  ),
  ##Percent of Children Whose Sex Disaggregation is Not Available 6-23 Months of Age Receiving a Minimum Acceptable Diet##
  tibble(ic = "HL.9.1-a", udn =  c("3.5.1.1", "3.5.1.2", "3.5.1.3", "3.5.1.4")
         , name = "Percent of Children Whose Sex Disaggregation is Not Available 6-23 Months of Age Receiving a Minimum Acceptable Diet"
  ),
  ##Number of Children Whose Sex Disaggregation is Not Available 6-23 Months of Age##
  tibble(ic = "HL.9.1-a", udn =  c("3.5.2.1", "3.5.2.2", "3.5.2.3", "3.5.2.4")
         , name = "Number of Children Whose Sex Disaggregation is Not Available 6-23 Months of Age"
  ),
  ##HL.9.1-b##
  ##Percent of Children 0-5 Months of Age in Sample Who Are Exclusively Breast Fed##
  tibble(ic = "HL.9.1-b", udn =  c("3.1.1", "3.1.2", "3.1.3", "3.1.4")
         , name = "Percent of Children 0-5 Months of Age in Sample Who Are Exclusively Breast Fed"
  ),
  ##Number of Children 0-5 Months of Age##
  tibble(ic = "HL.9.1-b", udn =  c("3.2.1", "3.2.2", "3.2.3", "3.2.4")
         , name = "Number of Children 0-5 Months of Age"
  ),
  ##Percent of Males 6-23 months of Age Who Are Exclusively Breast Fed##
  tibble(ic = "HL.9.1-b", udn =  c("3.3.1.1", "3.3.1.2", "3.3.1.3", "3.3.1.4")
         , name = "Percent of Males 0-5 Months of Age Who Are Exclusively Breast Fed"
  ),
  ##Number of Male Children 0-5 Months of Age##
  tibble(ic = "HL.9.1-b", udn =  c("3.3.2.1", "3.3.2.2", "3.3.2.3", "3.3.2.4")
         , name = "Number of Male Children 0-5 Months of Age"
  ),
  ##Percent of Females 0-5 Months of Age Who Are Exclusively Breast Fed##
  tibble(ic = "HL.9.1-b", udn =  c("3.4.1.1", "3.4.1.2", "3.4.1.3", "3.4.1.4")
         , name = "Percent of Females 0-5 Months of Age Who Are Exclusively Breast Fed"
  ),
  ##Number of Females 0-5 Months of Age##
  tibble(ic = "HL.9.1-b", udn =  c("3.4.2.1", "3.4.2.2", "3.4.2.3", "3.4.2.4")
         , name = "Number of Females 0-5 Months of Age"
  ),
  ##Percent of Children Whose Sex Disaggregation is Not Available 0-5 Months of Age Who Are Exclusively Breast Fed##
  tibble(ic = "HL.9.1-b", udn =  c("3.5.1.1", "3.5.1.2", "3.5.1.3", "3.5.1.4")
         , name = "Percent of Children Whose Sex Disaggregation is Not Available 0-5 Months of Age Who Are Exclusively Breast Fed"
  ),
  ##Number of Children Whose Sex Disaggregation is Not Available 0-5 Months of Age##
  tibble(ic = "HL.9.1-b", udn =  c("3.5.2.1", "3.5.2.2", "3.5.2.3", "3.5.2.4")
         , name = "Number of Children Whose Sex Disaggregation is Not Available 0-5 Months of Age"
  ),
  ##HL.9.1-d##
  ##Percent of Women 15-49 Years Who Consumed a Diet of Minimum Diversity in the Previous 24 Hours##
  tibble(ic = "HL.9.1-d", udn =  c("3.1.1", "3.1.2", "3.1.3", "3.1.4")
         , name = "Percent of Women 15-49 Years Who Consumed a Diet of Minimum Diversity in the Previous 24 Hours"
  ),
  ##Number of Women 15-49 Years of Age##
  tibble(ic = "HL.9.1-d", udn =  c("3.2.1", "3.2.2", "3.2.3", "3.2.4")
         , name = "Number of Women 15-49 Years of Age"
  ),
  ##Percent of Women 15-18 Years Who Consumed a Diet of Minimum Diversity in the Previous 24 Hours##
  tibble(ic = "HL.9.1-d", udn =  c("3.3.1", "3.3.2", "3.3.3", "3.3.4")
         , name = "Percent of Women 15-18 Years Who Consumed a Diet of Minimum Diversity in the Previous 24 Hours"
  ),
  ##Number of Women 15-18 Years of Age##
  tibble(ic = "HL.9.1-d", udn =  c("3.4.1", "3.4.2", "3.4.3", "3.4.4")
         , name = "Number of Women 15-18 Years of Age"
  ),
  ##Percent of Women 19-49 Years Who Consumed a Diet of Minimum Diversity in the Previous 24 Hours##
  tibble(ic = "HL.9.1-d", udn =  c("3.5.1", "3.5.2", "3.5.3", "3.5.4")
         , name = "Percent of Women 19-49 Years Who Consumed a Diet of Minimum Diversity in the Previous 24 Hours"
  ),
  ##Number of Women 19-49 Years of Age##
  tibble(ic = "HL.9.1-d", udn =  c("3.6.1", "3.6.2", "3.6.3", "3.6.4")
         , name = "Number of Women 19-49 Years of Age"
  ),
  ##RESIL-1 (Number of Plans by Type -  Double Counting)##
  ##Government##
  tibble(ic = "RESIL-1", udn = "3.1.1", name = "Government - Number of Plans"
  ),
  tibble(ic = "RESIL-1", ms_d2 = "Government - number of plans"
         , name = "Government - Number of Plans"
  ),
  tibble(ic = "RESIL-1", udn =  c("3.1.1.1.1", "3.1.1.1.2", "3.1.1.1.3", "3.1.1.1.4")
         , name = "Government - Phase of Development"
  ),
  tibble(ic = "RESIL-1"
         , ms_d3 = "Phase of development (double-counting allowed)"
         , name = "Phase of Development"
  ),
  ##Community##
  tibble(ic = "RESIL-1", udn = "3.1.2", name = "Community - Number of Plans"
  ),
  tibble(ic = "RESIL-1", ms_d2 = "Community - number of plans"
         , name = "Community - Number of Plans"
  ),
  tibble(ic = "RESIL-1", udn =  c("3.1.2.1.1", "3.1.2.1.2", "3.1.2.1.3", "3.1.2.1.4")
         , name = "Community - Phase of Development"
  ),
  tibble(ic = "RESIL-1"
         , ms_d3 = "Phase of development (double-counting allowed)"
         , name = "Phase of Development"
  ),
  ##RESIL-a##
  ##ARSSI Score##
  tibble(ic = "RESIL-a", udn =  c("3.1.1", "3.1.2", "3.1.3", "3.1.4")
         , name = "ARSSI Score"
  ),
  ##Number of Households (All Household Types Together)##
  tibble(ic = "RESIL-a", udn =  c("3.2.1", "3.2.2", "3.2.3", "3.2.4")
         , name = "Number of Households (All Household Types Together)"
  ),
  ##Household type: Male and Female Adults (M&F)##
  tibble(ic = "RESIL-a"
         , udn =  c("3.3.1.1", "3.3.1.2", "3.3.1.3", "3.3.1.4", "3.3.2.1"
                    , "3.3.2.2", "3.3.2.3", "3.3.2.4")
         , name = "Household Type: Male and Female Adults (M&F)"
  ),
  ##Household type: Adult Female no Adult Male (FNM)##
  tibble(ic = "RESIL-a"
         , udn =  c("3.4.1.1", "3.4.1.2", "3.4.1.3", "3.4.1.4", "3.4.2.1"
                    , "3.4.2.2", "3.4.2.3", "3.4.2.4")
         , name = "Household Type: Adult Female No Adult Male (FNM)"
  ),
  ##Household type: Adult Male no Adult Female (MNF)##
  tibble(ic = "RESIL-a"
         , udn =  c("3.5.1.1", "3.5.1.2", "3.5.1.3", "3.5.1.4", "3.5.2.1"
                    , "3.5.2.2", "3.5.2.3", "3.5.2.4")
         , name = "Household Type: Adult Male No Adult Female (MNF)"
  ),
  ##Household type: Child No Adults (CNA)##
  tibble(ic = "RESIL-a"
         , udn =  c("3.6.1.1", "3.6.1.2", "3.6.1.3", "3.6.1.4", "3.6.2.1"
                    , "3.6.2.2", "3.6.2.3", "3.6.2.4")
         , name = "Household Type: Child No Adults (CNA)"
  ),
  ##Household type: Disaggregates Not Available##
  tibble(ic = "RESIL-a"
         , udn =  c("3.7.1.1", "3.7.1.2", "3.7.1.3", "3.7.1.4", "3.7.2.1"
                    , "3.7.2.2", "3.7.2.3", "3.7.2.4")
         , name = "Household Type: Disaggregates Not Available"
  ),
  ##RESIL-b##
  ##Overall Index of Social Capital at the Household Level##
  tibble(ic = "RESIL-b", udn =  c("3.1.1", "3.1.2", "3.1.3", "3.1.4")
         , name = "Overall Index of Social Capital at the Household Level"
  ),
  ##Bonding Sub-Index##
  tibble(ic = "RESIL-b", udn =  c("3.2.1", "3.2.2", "3.2.3", "3.2.4")
         , name = "Bonding Sub-Index"
  ),
  ##Bridging Sub-Index##
  tibble(ic = "RESIL-b", udn =  c("3.3.1", "3.3.2", "3.3.3", "3.3.4")
         , name = "Bridging Sub-Index"
  ),
  ##Total Number of Households##
  tibble(ic = "RESIL-b", udn =  c("3.4.1", "3.4.2", "3.4.3", "3.4.4")
         , name = "Total Number of Households"
  ),
  ##Household Type: Male and Female Adults (M&F)##
  tibble(ic = "RESIL-b"
         , udn =  c("3.5.1.1", "3.5.1.2", "3.5.1.3", "3.5.1.4", "3.5.2.1"
                    , "3.5.2.2", "3.5.2.3", "3.5.2.4", "3.5.3.1", "3.5.3.2"
                    , "3.5.3.3", "3.5.3.4")
         , name = "Household Type: Male and Female Adults (M&F)"
  ),
  ##Household Type: Adult Female No Adult Male (FNM)##
  tibble(ic = "RESIL-b"
         , udn =  c("3.6.1.1", "3.6.1.2", "3.6.1.3", "3.6.1.4", "3.6.2.1"
                    , "3.6.2.2", "3.6.2.3", "3.6.2.4", "3.6.3.1", "3.6.3.2"
                    , "3.6.3.3", "3.6.3.4")
         , name = "Household Type: Adult Female No Adult Male (FNM)"
  ),
  ##Household Type: Adult Male No Adult Female (MNF)##
  tibble(ic = "RESIL-b"
         , udn =  c("3.7.1.1", "3.7.1.2", "3.7.1.3", "3.7.1.4", "3.7.2.1"
                    , "3.7.2.2", "3.7.2.3", "3.7.2.4", "3.7.3.1", "3.7.3.2"
                    , "3.7.3.3", "3.7.3.4")
         , name = "Household Type: Adult Male No Adult Female (MNF)"
  ),
  ##Household Type: Child No Adults (CNA)##
  tibble(ic = "RESIL-b"
         , udn =  c("3.8.1.1", "3.8.1.2", "3.8.1.3", "3.8.1.4", "3.8.2.1"
                    , "3.8.2.2", "3.8.2.3", "3.8.2.4", "3.8.3.1", "3.8.3.2"
                    , "3.8.3.3", "3.8.3.4")
         , name = "Household Type: Child No Adults (CNA)"
  ),
  ##STIR-12 (Number of Publications)##
  tibble(ic = "STIR-12", udn =  c("3.1.1", "3.1.2")
         , name = "Number of Peer-Reviewed Scientific Publications Resulting From USG Support to Research and Implementation Programs"
  ),
  ##Youth-3 (Number by Age)##
  ##Number of Youth Program Participants##
  tibble(ic = "Youth-3", udn =  "3.1"
         , ms_d1 = "Numerator: Number of youth program participants"
         , name = "Number of Youth Program Participants"
  ),
  ##Number of Total Program Participants##
  tibble(ic = "Youth-3", udn =  "3.2"
         , ms_d1 = "Denominator: Number of total participants in the program"
         , name = "Number of Total Program Participants"
  )

)


# Combine ####
## DIS ####
disaggregate_crosswalk <-  dplyr::bind_rows(
  dplyr::mutate(d1, order = "first_order")
  ,  dplyr::mutate(d2, order = "second_order")
  ,  dplyr::mutate(d3, order = "third_order")
  ,  dplyr::mutate(d4, order = "fourth_order")
  ) %>%
  dplyr::select(ic, udn, name, order) %>%
  dplyr::filter(!is.na(udn))%>%
  # dplyr::group_by(ic, udn, order) %>%
  # dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  # dplyr::filter(n > 1L)
  tidyr::pivot_wider(id_cols = c(ic, udn)
                     , names_from = order
                     , values_from = name)

return(disaggregate_crosswalk)

}

