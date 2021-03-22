library(tidyverse)

dt <- read_csv("rawData/EmployeeDataProcessed.csv")

glimpse(dt)

dt %>% group_by(county) %>% 
  count() %>% 
  arrange(desc(n))

dt %>% 
  filter(county == "LEWIS AND CLARK") %>% 
  group_by(department) %>% 
  summarise(
    AvgBase = mean(AnnualBase),
    MedianBase = median(AnnualBase),
    TotalBase = sum(AnnualBase)
  )

dt %>% 
  filter(county == "LEWIS AND CLARK") %>% 
  group_by(department) %>% 
  summarise_if(is.numeric,mean) 

dt %>% 
  filter(county == "LEWIS AND CLARK") %>% 
  group_by(department,year) %>% 
  summarise(TotalBase = sum(AnnualBase)) %>% 
  spread(year,TotalBase)

