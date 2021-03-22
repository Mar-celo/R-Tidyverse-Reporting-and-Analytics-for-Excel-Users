library(tidyverse)

dt <- read_csv("rawData/EmployeeDataProcessed.csv")

dt %>% 
  # filter(county == "LEWIS AND CLARK") %>%
  group_by(department,county,city) %>% 
  summarise_if(is.numeric,sum) %>% 
  write_csv("EmployeeSummarised.csv")

