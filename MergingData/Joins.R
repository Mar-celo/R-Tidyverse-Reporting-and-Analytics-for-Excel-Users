library(tidyverse)

A <- read_csv("data/Employees.csv")
B <- read_csv("data/EmployeeData.csv")

#Automatically Joins on all fields with the same name
left_join(A,B)

#Looks for the column EmployeeID to join on both tables
left_join(A,B,by="EmployeeID")

#When the column names are different you can specify it like this
left_join(A,B,by=c("EmployeeID"="EmployeeID"))

right_join(A,B)
anti_join(A,B)
full_join(A,B)
inner_join(A,B)
semi_join(A,B)

DepartmentHistory <- read_csv("data/DepartmentHistory.csv")

#Department history includes multiple records for a single employee
left_join(A,DepartmentHistory)

#Extract the latest department only
latestDepartment <- DepartmentHistory %>% 
  arrange(desc(Year)) %>% 
  distinct(EmployeeID,.keep_all = T)

left_join(A,latestDepartment)
