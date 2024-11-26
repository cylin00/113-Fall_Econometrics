rm(list = ls())
library(haven)
Card1995 <- read_dta("Econometrics_Data/Card1995/Card1995.dta") 
head(Card1995)

library(dplyr)
data <- Card1995 %>% filter(!is.na(lwage76))
dim(data) # Assure the size of data
