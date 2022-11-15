# working directory
getwd()
setwd("D:/études/SUPAGRO/2A/D4/github/vineyards")

# library 
library(tidyverse)

# get the data for the cover rate
canopeo_weed_ini = read.table("data_canopeo_weeds.csv", header = TRUE, sep = ";", dec = ",")
view(canopeo_weed_ini)
# we wanted to turn the column named "zone_X" into one 
#in order for the data to be easier to analyse
canopeo_weed <- canopeo_weed_ini%>%
  select (date, line, z1, z2, z3,nature)%>%
  pivot_longer(cols = z1:z3, names_to = "zone",values_to = "weed_cover_rate" )
view(canopeo_weed)

#Graph 
library(ggplot2)
canopeo_weed%>%
  group_by(line)%>%
  ggplot (aes(x = line, y = weed_cover_rate, ymin=0, ymax=100, xmin=1, xmax=14)) + 
  geom_point()

canopeo_weed%>%
  group_by(line)%>%
  summarise(mymean=mean(weed_cover_rate, na.rm=T),
            mysd=sd(weed_cover_rate, na.rm=T))%>%
  mutate(ymax=mymean+mysd, ymin=mymean-mysd)%>%
  ggplot (aes(x = line, y = mymean, ymin=ymin, ymax=ymax)) + 
  geom_point()+
  geom_errorbar()+
  labs(x="Line", y="Weed cover rate", title = "Percentage of weed soil coverance when different cover crops were grown")

#Anova
anova.canopeo<-lm(weed_cover_rate~line, canopeo_weed)
anova(anova.canopeo)
 
# p-value > 0,05 so no tuckey test

#t.test : comparing the control to all the treatments
control_cover_rate<-canopeo_weed%>%
  filter(line==1)
treatment_cover_rate<-canopeo_weed%>%
  filter(line!=1)

t.test(control_cover_rate$weed_cover_rate, treatment_cover_rate$weed_cover_rate)

#t.test : comparing the mono to the mix
mono_cover_rate<-canopeo_weed%>%
  filter(nature=="mono")
view(mono_cover_rate)
mix_cover_rate<-canopeo_weed%>%
  filter(nature=="mix")

t.test(mono_cover_rate$weed_cover_rate, mix_cover_rate$weed_cover_rate)

###########Weed biomass################

#get the data for the biomass
biomass_weed_ini = read.table("3_weed_biomass.csv", header = TRUE, sep = ";", dec = ",")
view(biomass_weed_ini)
# we wanted to turn the column named "zone_X" into one 
#in order for the data to be easier to analyse
biomass_weed <- biomass_weed_ini%>%
  pivot_longer(cols = Z1:Z3, names_to = "zone",values_to = "weed_biomass" )
view(biomass_weed)

#Graph 
library(ggplot2)
biomass_weed%>%
  group_by(line)%>%
  ggplot (aes(x = line, y = weed_biomass, ymin=0, ymax=30, xmin=1, xmax=14)) + 
  geom_point()

biomass_weed%>%
  group_by(line)%>%
  summarise(mymean=mean(weed_biomass, na.rm=T),
            mysd=sd(weed_biomass, na.rm=T))%>%
  mutate(ymax=mymean+mysd, ymin=mymean-mysd)%>%
  ggplot (aes(x = line, y = mymean, ymin=ymin, ymax=ymax)) + 
  geom_point()+
  geom_errorbar()+
  labs(x="Line", y="Weed biomass", title = "Biomass of the weeds per line")

#Anova
anova.biomass<-lm(weed_biomass~line, biomass_weed)
anova(anova.biomass)

# p-value > 0,05 so no tuckey test

#t.test : comparing the control to all the treatments
control_biomass<-biomass_weed%>%
  filter(line==1)
treatment_biomass<-biomass_weed%>%
  filter(line!=1)

t.test(control_biomass$weed_biomass, treatment_biomass$weed_biomass)

#t.test : comparing the mono to the mix
mono_biomass<-biomass_weed%>%
  filter(nature=="mono")
mix_biomass<-biomass_weed%>%
  filter(nature=="mix")

t.test(mono_biomass$weed_biomass, mix_biomass$weed_biomass)

########Weed density###########

#get the data for the weed density
density_ini = read.table("emergence_rate.csv", header = TRUE, sep = ";", dec = ",")
view(density_ini)
# we wanted to turn the column named "zone_X" into one 
#in order for the data to be easier to analyse
density_w_cc <-density_ini%>%
  pivot_longer(cols = Zone.1:Zone.3, names_to = "zone",values_to = "weed_density" )
view(density_w_cc)

weed_density<-density_w_cc%>%
  filter(Specie=="Weed")
view(weed_density)

#Graph 
library(ggplot2)
weed_density%>%
  group_by(Line)%>%
  ggplot (aes(x = Line, y = weed_density, ymin=0, ymax=80, xmin=1, xmax=14)) + 
  geom_point()

weed_density%>%
  group_by(Line)%>%
  summarise(mymean=mean(weed_density, na.rm=T),
            mysd=sd(weed_density, na.rm=T))%>%
  mutate(ymax=mymean+mysd, ymin=mymean-mysd)%>%
  ggplot (aes(x = Line, y = mymean, ymin=ymin, ymax=ymax)) + 
  geom_point()+
  geom_errorbar()+
  labs(x="Line", y="Weed density", title = "Weed density per line 19 days after the sowing date")

#Anova
anova.density<-lm(weed_density~Line, weed_density)
anova(anova.density)

# no tuckey test

#t.test : comparing the control to all the treatments
control_density<-weed_density%>%
  filter(Line==1)
treatment_density<-weed_density%>%
  filter(Line!=1)

t.test(control_density$weed_density, treatment_density$weed_density)

#t.test : comparing the mono to the mix
mono_density<-weed_density%>%
  filter(nature=="mono")
mix_density<-weed_density%>%
  filter(nature=="mix")

t.test(mono_density$weed_density, mix_density$weed_density)
