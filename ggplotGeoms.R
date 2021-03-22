library(tidyverse)

ggplot(mpg) +
  geom_bar(aes(x=displ)) 

ggplot(mpg) +
  geom_col(aes(x=displ,y=hwy)) 

ggplot(mpg) +
  geom_col(aes(x=displ,y=hwy,fill=class)) 

ggplot(mpg) +
  geom_boxplot(aes(x=class,y=hwy)) 

ggplot(mpg) +
  geom_text(aes(x=displ,y=hwy,label=class),check_overlap = T) 

ggplot(mpg) +
  geom_point(aes(x=displ,y=hwy)) 

ggplot(mpg) +
  geom_jitter(aes(x=displ,y=hwy)) 

ggplot(mpg) +
  geom_smooth(aes(x=displ,y=hwy)) 

ggplot(mpg) +
  geom_smooth(aes(x=displ,y=hwy),method = "lm") 