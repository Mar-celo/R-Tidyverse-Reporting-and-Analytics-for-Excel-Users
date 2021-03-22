library(tidyverse)
dt <- read_csv("CalculatedColumns/rawData/EmployeeDataProcessed.csv")

dt %>% filter(year==2017 & county %in% c("CASCADE","POWELL") | AnnualBase >= 20000)

dt %>% filter(year %in% 2014:2017 &!county %in% c("CASCADE","POWELL"))

2014:2017

2014.1 %in% 2014:2017
2014.1 >= 2014 & 2014.1 <= 2017
