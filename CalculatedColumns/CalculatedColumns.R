library(tidyverse)

dt <- read_csv("CalculatedColumns/data/EmployeeData.csv")

dt <- dt %>% mutate(
  AnnualBaseSalary = ifelse(frequency=="HOURLY",base*40*52,base*5*52),
  AnnualTotalCompensation = ifelse(frequency=="HOURLY",total*40*52,total*5*52),
  AnnualBonus = AnnualTotalCompensation - AnnualBaseSalary
) 
