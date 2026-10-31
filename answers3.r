library(haven)
library(flextable)
library(officer)
library(dplyr)
library(ggplot2)
library(fixest)

tenure_data <- read_dta("data/tenure_choice.dta")


# 3.1

tenure_data <- mutate(tenure_data,
  birth_year = 2010 - age,
  greatest_silent = (age >= 65),
  boomer = (age >= 46 & age <= 64),
  genx = (age >= 30 & age <= 45),
  millenial = (age <= 29),
  married = (marst == 1),
  separated = (marst == 2),
  unmarried = (marst == 3),
  native_citizen = (citizen == 0),
  abroad_citizen = (citizen == 1),
  naturalized_citizen = (citizen == 2),
  noncitizen = (citizen == 3),
  log_income = log(hhincome),
  income_quintile = ntile(hhincome, 5),
  income_quintile_1 = income_quintile == 1,
  income_quintile_2 = income_quintile == 2,
  income_quintile_3 = income_quintile == 3,
  income_quintile_4 = income_quintile == 4,
  income_quintile_5 = income_quintile == 5,
  homeowner = (own == 1),
  renter = (own == 0)
)

sum(tenure_data$trantime == 0 & tenure_data$empstat == 1, na.rm = TRUE)
