pacman::p_load(magrittr, dplyr, stringr)

## Loading in the data

dat_desc <- readr::read_csv("Willi_Variablenübersicht.csv")

dat <- read.csv(
  file = "WilliFINAL.csv",
  sep = ",",
  fileEncoding = "UTF-16LE"
)
head(dat)
## Translating column names for easier handling

colnames(dat_desc) <- c(
  "q_id",
  "meaning",
  "answer_type",
  "answer_format",
  "question",
  "notes"
)

## Recoding variables invertedly. NOTE: if it throws an error
## (must be 5 or 1 not 6), then just simply add this modified key
## to the line in recode key spitting the error. Thats becase missing values
## are unexpected and are not present in the mini data set.

## recode_key_na <- c(`1` = 5L, `2` = 4L, `4` = 2L, `5` = 1L, `3` = 3L, `-9` = NA)

recode_key <- c(
  `1` = 5L, # The L makes sure the value remains it's type as integer.
  `2` = 4L,
  `4` = 2L,
  `5` = 1L,
  `3` = 3L
)

## making variable names human readable

dat <- rename(dat,
  gender = SD01,
  age = SD02_01,
  income = SD03,
  education = SD04,
  gini_sum = GI01_SM,
  gini_1 = GI01_01,
  gini_2 = GI01_02,
  gini_3 = GI01_03,
  gini_4 = GI01_04,
  gini_5 = GI01_05,
  attitude_income_difference = PR03_01,
  attitude_gvt_responsible = PR03_02,
  attitude_gvt_living_standard = PR03_03,
  attitude_gvt_social_services = PR03_04,
  social_media_usage_response_type = ST04,
  os_assess_fb = ST18_01,
  os_assess_insta = ST19_01,
  self_assess_fb = ST15_01,
  self_assess_insta = ST16_01,
  insta_scale = ST11_01,
  fb_scale = ST12_01,
  treated = FI02,
  consent = FI04 # Still needs figuring out.
)
## Recode lines

dat <- mutate(dat,
  attitude_gvt_social_services = recode(
    attitude_gvt_social_services,
    recode_key
  )
)

## Dropping unused columns. NOTE: Assumes that desired columns are lower case.

dat <- dat[, !(str_detect(colnames(dat), "(?=.*[A-Z])"))]

## creating the gini coefficient

dat <- mutate(dat,
  gini_coef = (((2 * (1 * gini_5 + 2 * gini_4 + 3 * gini_3 + 4 * gini_2 + 5 * gini_1)) / 500) - (6 / 5)),
  redist_sum_non_na_answers = 4 - (is.na(attitude_income_difference) +
    is.na(attitude_gvt_responsible) +
    is.na(attitude_gvt_living_standard) +
    is.na(attitude_gvt_social_services)),
  redistribution_attitude = (sum(
    attitude_income_difference,
    attitude_gvt_responsible,
    attitude_gvt_living_standard,
    attitude_gvt_social_services
  )), # If at least 3/4 redistribution questions have been answered, take average. otherwise NA.
  pref_for_redistribution = ifelse(redist_sum_non_na_answers > 2,
    redistribution_attitude / redist_sum_non_na_answers,
    NA
  )
)



## NOTE: This is an alternative function implementation to calculate the Gini
## coef for n cases
## GiniCalc <- function(x, n) {
##   upper <- 0
##   lower <- 0
##   for(i in (1:dim(x)[1])) {
##     upper <- upper + (i * x[i])
##     lower <- lower + x[i]}
##   Gini = (2 * upper) / (dim(x)[1] * lower)
##   return(Gini)
## }
