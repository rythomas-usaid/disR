
#disag1 values:
#
# sex
# age
# type of individuals participating
# mgmt_practice management practice or tech type
# commodity
# size of recipient(s)



# Sex ####
# this is a list of all the sex disaggregates for sex grouped by the label field
s1 <- tibble(category = c("sex"),
             disaggregate = c("female", "male", "disaggregates not available"
                              , "association-applied", "joint", "neither"
                              , "mixed sexes"))
s2 <- tibble(category = "sex of owner / producer"
             , disaggregate = c("female", "male", "joint", "n/a"
                                , "disaggregates not available", "neither"))
s3 <- tibble(category = "sex of recipient(s)"
             , disaggregate = c("female", "male", "joint","n/a", "not applicable"
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
s8 <- tibble(category = "sex of job-holder"
             , disaggregate = c("female", "male"
                                , "disaggregates not available"))

# this one is not really a sex disaggregate->
# tibble(category = "no age/sex information collected"
#              , disaggregate = c("value of sales", "baseline sales"
#              , "number of participants", "volume of sales (mt)"
#              , "type of sales"))

sex_lookup <- bind_rows(s1, s2, s3, s4, s5, s6, s7, s8)
# list of all the distinct sex disaggregate categories to replace in the
# replace_all function below.


# Age ####
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



# Mgmt Prac ####
mgmt_prac_lookup <- tibble(
  category = "management practice or tech type"
  , disaggregate = c(
    "irrigation","agriculture water management-non-irrigation"
    , "cultural practices", "livestock management", "marketing and distribution"
    ,"pest and disease management", "post-harvest handling and storage"
    , "soil-related fertility and conservation", "other"
    , "climate adaptation/climate risk management", "climate mitigation"
    , "crop genetics", "value-added processing", "aquaculture management"
    , "natural resource or ecosystem management"
    , "wild-caught fisheries management", "disaggregates not available"
    , "total with one or more improved technology", "climate adaptation"
    , "disease management", "pest management"
    , "water management (non-irrigation)", "wild fishing gear/technique"
    , "climate mitigation or adaptation", "animal genetics"
    , "fishing gear/technique", "processing")
)

commodities_lookup <- tibble(
  category = "commodity"
  , disaggregate =
    c("disaggregates not available or other", "jute", "lentil (nrvcc)",
      "maize", "not applicable", "onions/shallots", "rice", "wheat",
      "horticulture", "carp (ponds) (nrvcc)", "tilapia (ponds) (nrvcc)",
      "garlic", "mung bean (nrvcc)", "cattle (live) (nrvcc)", "forage/fodder",
      "milk (cow) (nrvcc)", "groundnuts/peanuts (nrvcc)", "sesame seed (oil)",
      "sunflower (oil)", "chickens (poultry) (nrvcc)", "gourd, bitter (nrvcc)",
      "gourd, bottle (nrvcc)", "watermelon", "chilies (nrvcc)", "goat (live) (nrvcc)",
      "gourd, sweet (nrvcc)", "vegetables", "bananas (nrvcc)", "cashews (nrvcc)",
      "coconut (flesh, milk)", "coffee, green beans", "flowers", "fruits",
      "mango (nrvcc)", "potatoes", "papaya (nrvcc)", "fish (ponds) (nrvcc)",
      "chickpea (nrvcc)", "maize grain", "dark green leafy vegetables (nrvcc)",
      "african leafy vegetables group (nrvcc)", "eggplant", "cattle (beef) (nrvcc)",
      "dairy (non-milk products, e.g. yogurt) (nrvcc)", "eggs (nrvcc)",
      "sorghum", "pork (meat) (nrvcc)", "beans and pulses (nrvcc)",
      "sheep (live) (nrvcc)", "cocoa", "coffee", "cassava", "pulses (nrvcc)",
      "soybeans (nrvcc)", "beans (biofortified) (nrvcc)", "beans (non-biofortified) (nrvcc)",
      "cabbage (nrvcc)", "cowpeas (nrvcc)", "millet", "okra (nrvcc)",
      "pigeon peas (nrvcc)", "tomatoes", "sesame seed (nrvcc)", "basil",
      "fennel", "green beans", "pomegranate", "grapes", "honey", "peppers, chile (nrvcc)",
      "animal feed", "goat (meat) (nrvcc)", "sheep (lamb/mutton) (nrvcc)",
      "avocado", "hazelnuts (nrvcc)", "soybean rain-fed (nrvcc)", "apples",
      "carrots (nrvcc)", "cauliflower (nrvcc)", "cucumber", "fava beans (nrvcc)",
      "lettuce", "paprika", "passion fruit (nrvcc)", "peas, green (nrvcc)",
      "sweet potatoes - orange/dark yellow - biofortified (nrvcc)",
      "camel (live) (nrvcc)", "milk (general, not animal-specific) (nrvcc)",
      "moringa (nrvcc)", "pineapples (nrvcc)", "sweet potatoes", "soybeans (oil)",
      "cotton", "peanuts (oil)", "rice grain", "sorghum/millet", "tomatoes, fresh",
      "sweet potatoes - white/pale yellow", "sweet potatoes - orange/dark yellow - non biofortified (nrvcc)",
      "fish (open-water cages) (nrvcc)", "rice-irrigated", "rice-rainfed",
      "kale (nrvcc)", "sunflower seed (nrvcc)", "chia", "quinoa", "maize, fresh (green mealies)",
      "peppers, various types or type unknown (nrvcc)", "sacha inchi",
      "ginger", "handicrafts", "camel (meat) (nrvcc)", "milk (camel) (nrvcc)",
      "milk (goat) (nrvcc)", "rice-lowland", "maize flour", "maize, orange (nrvcc)",
      "butternut squash (nrvcc)"))

# duration ####
duration_lookup <- tibble(
  category = "duration  (no double-counting)"
  , disaggregate = c("new", "continuing"))
disag_lookup <- bind_rows(sex_lookup, age_lookup, mgmt_prac_lookup
                          , commodities_lookup, duration_lookup)


disags_replace_all <- c(
  "sex of owner / producer" = "sex"
  , "sex of recipient\\(s\\)" = "sex"
  , "sex of participant \\(no double-counting\\)" = "sex"
  , "sex of account owner or policy holder" = "sex"
  , "sex \\(no double-counting\\)" = "sex"
  , "sex of individuals participating" = "sex"
  , "sex of job-holder" = "sex"

  , "management practice or tech type \\(double-counting allowed\\)" =
    "management practice or tech type"

  , "age category of individuals participating" = "age"
  , "age category" = "age"
  , "age of participant \\(no double-counting\\)" = "age"
  , "age of recipient\\(s\\)" = "age"
  , "age of participant \\(no double-counting\\)" = "age"
  , "mixed ages \\(for enterprises\\)" = "mixed", "mixed ages" = "mixed"
  , "mixed \\(for enterprises\\)" = "mixed"
  , "age less than 19 years" = "< 19", "age 19+ years" = "19+"
  , "women < 19" = "< 19", "women >= 19" = ">= 19"

  , "size of recipient\\(s\\)" = "size"
  , "size of msme" = "size"
  , "smallholder \\(row label implied\\)" = "smallholder")
