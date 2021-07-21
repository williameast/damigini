pacman::p_load(tidyverse, stargazer, xtable)

dat <- readRDS("Data_clean.rds")

colnames(dat)

## calulating averages for descriptive statistics
dat$gender <- as.factor(dat$gender)

Summary_Stats <- select(dat, age, gender, education, income) %>% summary()

print(xtable(Summary_Stats, type = "latex"))

attitude_income_difference.aov <- summary(aov(attitude_income_difference ~ treated,
                                              data = dat,
                                              projections = FALSE,
                                              na.omit = TRUE))


print(xtable(attitude_income_difference.aov, type = "latex"))


