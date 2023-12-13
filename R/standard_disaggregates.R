# this is a list of all the sex disaggregates for sex grouped by the label field
s1 <- tibble(category = c("sex"),
             disaggregate = c("female", "male", "disaggregates not available"
                              , "association-applied", "joint", "neither"))
s2 <- tibble(category = "sex of owner / producer"
             , disaggregate = c("female", "male", "joint", "n/a"
                                , "disaggregates not available", "neither"))
s3 <- tibble(category = "sex of recipient(s)"
             , disaggregate = c("female", "male", "joint","n/a"
                                , "disaggregates not available", "neither"
                                , "mixed (for enterprises)"))
s4 <- tibble(category = c("sex of participant (no double-counting)")
             , disaggregate = c("female", "male"
                                , "disaggregates not available", "neither"))

s5 <- tibble(category=c("sex of account owner or policy holder")
             , disaggregate = c("female", "male"
                                , "disaggregates not available", "neither"
                                , "jointly-held"))

s6 <- tibble(category = "sex (no double-counting)"
             , disaggregate = c("female", "male"
                                , "disaggregates not available", "neither"))

s7 <- tibble(category = "sex of individuals participating"
             , disaggregate = c("female", "male"
                                , "disaggregates not available"
                                ,"not applicable", "neither"))
# this one is not really a sex disaggregate->
# tibble(category = "no age/sex information collected"
#              , disaggregate = c("value of sales", "baseline sales"
#              , "number of participants", "volume of sales (mt)"
#              , "type of sales"))

sex_lookup <- bind_rows(s1, s2, s3, s4, s5, s6, s7)
# list of all the distinct sex disaggregate categories to replace in the
# replace_all function below.


# Age
## D1
a1 <- tibble(category = c("age category of individuals participating"),
             disaggregate = c("disaggregates not available", "30+"
                              , "school-aged children", "15-29", "not applicable"
                              , "mixed ages (for enterprises)"))

a2 <- tibble(category = c("age"),
             disaggregate = c("women < 19", "women >= 19",
                              "disaggregates not available", "15-29", "30+"
                              , "disaggregates not available - value of sales"
                              , "15-29 years - value of sales"
                              , "15-29 years - number of participants"
                              , "30+ years - value of sales"
                              , "30+ years - number of participants"
                              , "mixed ages - value of sales"
                              , "mixed ages - number of participants"
                              , "disaggregates not available - number of participants"
                              , "disaggregates not available - number of participant producers"
                              , "30+ years - volume of sales"
                              , "15-29 years - volume of sales"
                              , "disaggregates not available - volume of sales"))

a3 <- tibble(category = c("age category"),
             disaggregate = c("15-29", "30+",
                              "disaggregates not available"))

a4 <- tibble(category = c("age of participant (no double-counting)"),
             disaggregate = c("15-29", "30+",
                              "disaggregates not available"))

# a5 <- tibble(category = c(
#   rep("number of female participants of the nutrition-sensitive agriculture activity", 2),
#   rep("percentage of female participants consuming a diet of minimum diversity", 2)),
#              disaggregate = rep(c("age less than 19 years", "age 19+ years"), 2))

a6 <- tibble(category = c("age of recipient(s)"),
             disaggregate = c("15-29", "30+", "mixed ages (for enterprises)"
                              , "disaggregates not available"
                              , "mixed (for enterprises)"))

age_lookup <- bind_rows(a1, a2, a3, a4, a6)
# list of all the distinct sex disaggregate categories to replace in the
# replace_all function below.

disags_replace_all <- c(
  "sex of owner / producer" = "sex"
  , "sex of recipient\\(s\\)" = "sex"
  , "sex of participant \\(no double-counting\\)" = "sex"
  , "sex of account owner or policy holder" = "sex"
  , "sex \\(no double-counting\\)" = "sex"
  , "management practice or tech type \\(double-counting allowed\\)" =
    "management practice or tech type"
  , "sex of individuals participating" = "sex"
  , "age category of individuals participating" = "age"
  , "age category" = "age"
  , "age of participant (no double-counting)" = "age"
  , "age of recipient(s)" = "age"
  , "age of participant (no double-counting)" = "age" )
