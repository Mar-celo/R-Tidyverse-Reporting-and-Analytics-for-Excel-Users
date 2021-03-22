library(tidyverse)
library(plotly)

mpg
g <- ggplot(mpg) + 
  geom_point(aes(x=displ,y=hwy,color=class))

ggplotly(g)
