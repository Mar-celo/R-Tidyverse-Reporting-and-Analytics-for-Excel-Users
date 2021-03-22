library(tidyverse)

ggplot(mpg) +
  geom_jitter(aes(x=displ,y=hwy,color=class)) +
  geom_smooth(aes(x=displ,y=hwy)) +
  annotate("rect",xmin=1.5,xmax=2,ymin=40,ymax=45,alpha=0.3,color="red") +
  annotate("text",x=3.5,y=43,label="Let's highlight \n these outliers") +
  ggtitle("Lets label our chart","Add subtitles here")
