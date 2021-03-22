library(tidyverse)

ggplot(mpg) +
  geom_jitter(aes(x=displ,y=hwy,color=class)) +
  facet_wrap(~class)

ggplot(mpg) +
  geom_jitter(aes(x=displ,y=hwy,color=class)) +
  facet_grid(year~class)

