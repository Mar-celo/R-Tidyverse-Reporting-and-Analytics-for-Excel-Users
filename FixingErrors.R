mtcars %>% 
  group_by(cyl) %>% 
  count()

library(Tidyverse)

Employees <- tibble(
  EmployeeID = c("01001", "01002", "01007"), 
  Name = c("Tony","Bruce","Nick")
)

EmployeeData <- tibble(
  EmployeeID = c("01001", "01002", "01008"), 
  BaseSalary = c(90000,20000,50000),
  Department = c("Technology","Research","Finance")
)

Employees <- left_join(Employees,EmployeeData,by="EmployeeID")
Employees <- left_join(Employees,EmployeeData,by="EmployeeID")

