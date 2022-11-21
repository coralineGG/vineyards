# working directory
getwd()
setwd("D:/études/SUPAGRO/2A/D4/github/vineyards")

# library 
library(tidyverse)

# get the data
canopeo_cc_ini = read.table("Data_collection_canopeo_cc_only.csv", header = TRUE, sep = ";", dec = ",")
view(canopeo_cc_ini)
# we wanted to turn the column named "zone_X" into one 
#in order for the data to be easier to analyse
canopeo_cc <- canopeo_cc_ini%>%
  select (Line, Z1, Z2, Z3)%>%
  pivot_longer(cols = Z1:Z3, names_to = "zone",values_to = "cover_rate" )
view(canopeo_cc)
#Graph 
library(ggplot2)
canopeo_cc%>%
  group_by(Line)%>%
  ggplot (aes(x = Line, y = cover_rate, ymin=0, ymax=100, xmin=1, xmax=14)) + 
  geom_point()

canopeo_cc%>%
  group_by(Line)%>%
  summarise(mymean=mean(cover_rate, na.rm=T),
            mysd=sd(cover_rate, na.rm=T))%>%
  mutate(ymax=mymean+mysd, ymin=mymean-mysd)%>%
  ggplot (aes(x = reorder(Line, mymean), y = mymean, ymin=ymin, ymax=ymax)) + 
  geom_point()+
  geom_errorbar()+
  labs(x="Line", y="Cover rate", title = "Percentage of coverance of the soil for different cover crops")
 
#Anova
anova.canopeo<-lm(cover_rate~Line, canopeo_cc)
anova(anova.canopeo)

## Our P-value is > 0,05 so we cannot do a tuckey test

