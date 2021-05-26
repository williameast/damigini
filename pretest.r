dat <- read.csv("WilliFINAL.csv",
  sep = ",",
  fileEncoding = "UTF-16LE"
)
head(dat)

dat_clean <- select(dat)
