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

# Wide data with legacy function spread()
dt %>% 
  filter(county == "LEWIS AND CLARK") %>% 
  group_by(department,year) %>% 
  summarise(TotalBase = sum(AnnualBase)) %>% 
  spread(year,TotalBase)

# Wide data with new function pivot_wider()
WideData <- dt %>% 
  filter(county == "LEWIS AND CLARK") %>% 
  group_by(department,year) %>% 
  summarise(TotalBase = sum(AnnualBase)) %>% 
  pivot_wider(names_from = year,values_from = TotalBase)

WideData

# Unpivot your data with pivot_longer()
WideData %>% 
  pivot_longer(-department,names_to = "yr",values_to = "Base Salary")