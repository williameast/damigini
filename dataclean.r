pacman::p_load(magrittr, dplyr, stringr, readr, data.table, tidyverse)

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

## recode_key_na <- c(`1` = 5L,
## `2` = 4L,
## `4` = 2L,
## `5` = 1L,
## `3` = 3L,
## `-9` = NA)

## recode_key <- c(
##   `1` = 5L, # The L makes sure the value remains it's type as integer.
##   `2` = 4L,
##   `4` = 2L,
##   `5` = 1L,
##   `3` = 3L
## )

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
  attitude_income_difference = PR03_01, # a (Bewertung der Vermögensunterschiede)
  attitude_gvt_responsible = PR03_02, # b (Support for Redistribution)
  attitude_gvt_living_standard = PR03_03, # c
  attitude_gvt_social_services = PR03_04, # d
  social_media_usage_response_type = ST04,
  os_assess_fb = ST18_01,
  os_assess_insta = ST19_01,
  self_assess_fb = ST15_01,
  self_assess_insta = ST16_01,
  insta_user = ST11_01,
  fb_user = ST12_01,
  treated = FI02,
  type_of_occupation = SD05,
  lives_in_germany = SD06,
  social_ladder = SD07, # Do we need to invert this?
  max_tax_rate = PR04_01
)

## recoding the a variable, needed to be inverted.
dat <- mutate(dat,
  attitude_gvt_social_services = (6 - attitude_gvt_social_services)
)

dat$social_ladder <- (11 - dat$social_ladder)

## Creating binary variable to capture self-assessed or app assessed SNS time.
dat <- mutate(dat,
  self_assess = ifelse(!is.na(self_assess_fb | self_assess_insta),
    1,
    0
  ),
  os_assess = ifelse(!is.na(os_assess_fb | os_assess_insta),
    1,
    0
  )
)

## Number of facebook users, insta users, inclusive and exclusive.

dat <- mutate(dat,
  fb_user = ifelse(fb_user == 2, 1, 0),
  insta_user = ifelse(insta_user == 2, 1, 0),
  sns_user = ifelse(fb_user^insta_user, 1, 0),
  only_fb_user = ifelse((fb_user == 1)^(insta_user == 0), 1, 0),
  only_insta_user = ifelse((insta_user == 1)^(fb_user == 0), 1, 0)
)


## Age range calculation
dat$age_ordinal <- cut(dat$age, c(10, 19, 29, 39, 49, 59, 69, 79),
  labels = c(
    "10-19",
    "20-29",
    "30-39",
    "40-49",
    "50-59",
    "60-69",
    "70-79"
  ),
  include.lowest = TRUE
)

## ggplot(dat, aes(x = age_ordinal)) + geom_bar(stat = "count")




## Dropping unused columns. NOTE: Assumes that desired columns are lower case.
dat <- dat[, !(str_detect(colnames(dat), "(?=.*[A-Z])"))]
## creating the gini coefficient.
## Redistribution is calculated as the average of all redistribution answers.

dat <- mutate(dat,
  gini_coef = (((2 *
    (1 * gini_5 +
      2 * gini_4 +
      3 * gini_3 +
      4 * gini_2 +
      5 * gini_1))
  / 500) - (6 / 5)),
  redist_sum_non_na_answers = 4 - (is.na(attitude_income_difference) +
    is.na(attitude_gvt_responsible) +
    is.na(attitude_gvt_living_standard) +
    is.na(attitude_gvt_social_services)),
  redistribution_attitude = (
    attitude_income_difference +
      attitude_gvt_responsible +
      attitude_gvt_living_standard +
      attitude_gvt_social_services), # If at least 3/4
  # redistribution questions have been answered, take average. otherwise NA.
  pref_for_redistribution = ifelse(redist_sum_non_na_answers > 2,
    redistribution_attitude / redist_sum_non_na_answers,
    NA
  )
)

## make a variable for social media use, and a bool for if self or OS

dat <- mutate(dat,
  ## insta_use = ifelse(social_media_usage_response_type == 1,
  ##   os_assess_insta,
  ##   self_assess_insta
  ## ),
  ## facebook_use = ifelse(social_media_usage_response_type == 1,
  ##   os_assess_fb,
  ##   self_assess_fb
  ## ),
  ## os_assess = ifelse(os_assess_fb | os_assess_insta == 1, 1, 0),
  ## os_assess = ifelse(social_media_usage_response_type == -9,
  ##   NA,
  ##   social_media_usage_response_type
  ## ),
  treated = ifelse(treated == 2, 1, 0) # Bool for treated status
)

dat <- mutate(dat,
  gender = ifelse(gender == 3, "Non-binary",
    (ifelse(gender == 2, "Male", "Female"))
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

## Recoding max tax rate

dat$max_tax_rate <- (ifelse(dat$max_tax_rate == -9, NA, dat$max_tax_rate - 1))


## Dami kann das machen
## recodekey_income = c(`1` = "<19999",
##                      `2` = "20000-29999",
##                      ``)

## Dami macht auch bildungsgrad


saveRDS(dat, "Data_clean.rds")

# TO DO

# ANOVAS
# 1 gi  Variable: gini_coef???
# 2 sp (a) Bewertung Vermögensun  Variable: attitude_income_difference = PR03_01
# 3 sp  (b) Der Staat soll verteilen  Variable: attitude_gvt_responsible = PR03_02
# 4 sp (combined) alle 5
# 5  Steuer Frage  Variable: max_tax_rate = PR04_01
# 6 SSS  Variable: social_ladder = SD07

# !!!! additional  ANOVA Statistics!!!!!
# Anova Tables
# http://users.minet.uni-jena.de/~jschum/biostat/ANOVA.pdf (gute Quelle mit allen Test Uni-Jena)
# http://www.sthda.com/english/wiki/one-way-anova-test-in-r
# Boxplots
# http://www.sthda.com/english/wiki/one-way-anova-test-in-r
# Eta-Quadrat
# https://www.statology.org/eta-squared-in-r/

# !!!!Welche Test?
# http://users.minet.uni-jena.de/~jschum/biostat/ANOVA.pdf (Uni Jena schlägt eine grafische Überprüfung vor. Willi zeigen!!!)
# Normalverteilung?


# INFOS
# !!!!Alter!!!! age = SD02_01,
# !!!!Geschlecht!!!! gender = SD01,
# !!!!Bildungsgrad!!!! education = SD04,
# !!!!Einkommen income = SD03,



## Number of facebook users, insta users, inclusive and exclusive.

# !!!! Wie viel Prozent von denen, die SNS time beantwortet haben, haben objektiv geantwortet.!!!!

# !!!!Durchschnittliche Nutzungsdauer [overall, objektiv, subjektiv] !!!!!

# !!!!ZEILE 172 sieht nicht gut aus GENDER, so kriege ich doch nur 2 Geschlechter Diskriminierungsalarm!!!

# !!!! Wieviele gewertete Datensetze N=???? !!!! (müsste ANOVA anzeigen)
# !!!! Wie viele in Treatment/Control Group ????? !!!!müsste ANOVA anzeigen)

# !!!!Daten-/Grafik-Transfer zu Latex!!!!!
# https://cran.r-project.org/web/packages/texreg/vignettes/texreg.pdf
# https://www.r-bloggers.com/2011/04/export-a-table-created-by-r-to-a-tex-file/