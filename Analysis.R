# working directory
getwd()
setwd("D:/études/SUPAGRO/2A/D4/github/vineyards")

# library 
library(tidyverse)

# get the data
canopeo_cc_ini = read.table("data_canopeo-cc.csv", header = TRUE, sep = ";", dec = ",")
view(canopeo_cc_ini)
# we wanted to turn the column named "zone_X" into one 
#in order for the data to be easier to analyse
canopeo_cc <- canopeo_cc_ini%>%
  select (date, Line, Zone.1, Zone.2, Zone.3, NEW.Z1, NEW.Z3)%>%
  slice(1:84)%>%
  filter(date == "07/11/2022")%>%
  #filter(cover_rate =!is.na()) pour essayer d'enlever les NA
  #na.omit()%>% # si on ajoute cette ligne, R ne garde que les Line 12... de fait, nous ne savons pas comment enlever les NA de notre tableau
  pivot_longer(cols = Zone.1:NEW.Z3, names_to = "zone",values_to = "cover_rate" )
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
  ggplot (aes(x = Line, y = mymean, ymin=ymin, ymax=ymax)) + 
  geom_point()+
  geom_errorbar()+
  labs(x="Line", y="Cover rate", title = "Percentage of coverance of the soil for different cover crops")
  #geom_pointrange(ymin=0, ymax=100, xmin=1, xmax=14) #we are not sure it will be added to the geom_dotplot()graph but that's what we want
#we couldn't test these lines since the begining of our code doesn't work

#Anova
anova.canopeo<-lm(cover_rate~Line, canopeo_cc)
anova(anova.canopeo)
