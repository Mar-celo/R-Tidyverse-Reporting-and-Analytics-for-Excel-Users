library(tidyverse)
SampleImport <- read_csv("LoadingData/data/SampleImport.csv", 
                         col_types = cols(HireDate = col_date(format = "%m/%d/%Y")))
View(SampleImport)

# read_csv() - Use this version from the tidyverse readr library instead of 
# read.csv() - This is from base R is slower and is not as good at choosing data types


# To load data from Excel you'll need to load the readxl library. 
# Avoid importing from Excel where possible
library(readxl)
SampleImport <- read_excel("~/R/rTraining/LoadingData/data/SampleImport.xlsx", 
                           col_types = c("text", "text", "numeric"))
View(SampleImport)

getwd()
