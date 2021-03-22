library(tidyverse)
library(DT)

mpg$cty %>% summary()
ctyPercentiles <- mpg$cty %>% quantile(c(.25,.75))

mpg %>% 
  datatable(rownames = F) %>% 
  formatCurrency("displ",currency = "£",digits = 2) %>% 
  formatStyle("cty",
              backgroundColor = styleInterval(ctyPercentiles,c("green","yellow","red")),
              color = styleInterval(ctyPercentiles,c("white","blue","white"))) %>% 
  formatStyle("hwy",background = styleColorBar(mpg$hwy,"steelblue"))
