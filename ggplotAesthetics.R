library(tidyverse)

mpg

ggplot(mpg) +
  geom_point(aes(x=displ,y=hwy,color=class,shape=drv,size=year,alpha=model)) 

ggplot(mpg) +
  geom_point(aes(x=displ,y=hwy,size=class)) 

ggplot(mpg) +
  geom_point(aes(x=displ,y=hwy,shape=class)) 

ggplot(mpg) +
  geom_point(aes(x=displ,y=hwy,alpha=class)) 