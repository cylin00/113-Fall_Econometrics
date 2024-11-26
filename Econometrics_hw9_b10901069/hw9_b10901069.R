rm(list = ls())
library(haven)
library(AER) 
library(stargazer)
library(dplyr)
library(broom)
library(knitr)
Card1995 <- read_dta("Econometrics_Data/Card1995/Card1995.dta") 
head(Card1995)

data <- Card1995 %>% filter(!is.na(lwage76))
dim(data)

log_wage <- data$lwage76
education <- data$ed76
experience <- data$age76 - data$ed76 - 6
experience2 <- experience^2 / 100
black <- data$black
south <- data$reg76r
urban <- data$smsa76r
college <- data$nearc4
public <- data$nearc4a
private <- data$nearc4b
age <- data$age76
age2 <- age^2 / 100

data_model <- data.frame(log_wage, education, experience, experience2, black, south, urban, college, public, private, age, age2)

### 1.
LogWage <- lm(log_wage ~ experience + experience2 + black + south + urban + college, data = data_model)
Education <- lm(education ~ experience + experience2 + black + south + urban + college, data = data_model)
Education2 <- lm(education ~ black + south + urban + college + age + age2, data = data_model)
Experience <- lm(experience ~ black + south + urban + college + age + age2, data = data_model)
Experience2 <- lm(experience2 ~ black + south + urban + college + age + age2, data = data_model)
Education3 <- lm(education ~ experience + experience2 + black + south + urban + public + private, data = data_model)


stargazer(LogWage, Education, Education2, Experience, Experience2,
          type = "text",
          title = "Regression Results",
          omit.stat = c("f"))
stargazer(Education3,
          type = "text",
          omit.stat = c("f"))


### 2. 
Ols <- lm(log_wage ~ education + experience + experience2 + black + south + urban, data = data_model)
Iva <- ivreg(log_wage ~ education + experience + experience2 + black + south + urban | 
               college + experience + experience2 + black + south + urban, data = data_model)
Ivb <- ivreg(log_wage ~ education + experience + experience2 + black + south + urban | 
               college + age + age2 + black + south + urban, data = data_model)
Twoslsa <- ivreg(log_wage ~ education + experience + experience2 + black + south + urban | 
                   public + private + experience + experience2 + black + south + urban, data = data_model)
Twoslsb <- ivreg(log_wage ~ education + experience + experience2 + black + south + urban | 
                   public + private + age + age2 + black + south + urban, data = data_model)

stargazer(Ols, Iva, Ivb, Twoslsa, Twoslsb, type = "text", title = "Instrumental Variable Wage Regressions")

