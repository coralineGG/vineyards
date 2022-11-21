# WEED BIOMASS RATIO (WBR)
getwd()
setwd("C:/Users/lizet/OneDrive/Datos adjuntos/Documentos/GitHub/vineyards")

# library 
library(tidyverse)
library(agricolae)

# get the data for the cover rate
#canopeo_weed_ini = weed_biomass
#canopeo_weed = weed_wbr
#weed_cover_rate = weed_biomass_ratio
#anova.canopeo = anova.wbr

weed_biomass = read.table("WBR_Weed.csv", header = TRUE, sep = ",", dec = ".")
view(weed_biomass)
# we wanted to turn the column named "zone_X" into one 
#in order for the data to be easier to analyse
weed_wbr <- weed_biomass %>%
  
  select (line,z1,z2,z3)%>%
  mutate(line = factor(line)) %>% 
  
  pivot_longer(cols = z1:z3, names_to = "zone",values_to = "weed_biomass_ratio" )
view(weed_wbr)

#Graph 
library(ggplot2)
weed_wbr%>%
  group_by(line)%>%
  ggplot (aes(x = line, y = weed_biomass_ratio, ymin=0, ymax=10, xmin=1, xmax=14)) + 
  geom_point()

#Anova
anova.wbr<-lm(weed_biomass_ratio~line, weed_wbr)
anova(anova.wbr)

par(mfrow = c(2,2))
plot(anova.wbr)
par(mfrow = c(1,1))
#canopeo_weed_ini = weed_biomass
#canopeo_weed = weed_wbr
#weed_cover_rate = weed_biomass_ratio
#anova.canopeo = anova.wbr

# Kruskal
kruskal <- kruskal(weed_wbr$weed_biomass_ratio, weed_wbr$line, console = T)
posthock <- kruskal$groups %>% 
  rownames_to_column("line") %>% 
  select(line, groups)
 #HASTA AQUI VA MI TRABAJO## ---------------------------------
weed_wbr%>%
  group_by(line)%>%
  summarise(mymean=mean(weed_biomass_ratio, na.rm=T),
            mysd=sd(weed_biomass_ratio, na.rm=T))%>%
  mutate(ymax=mymean+mysd, ymin=mymean-mysd) %>% 
  full_join(posthock) %>% 
  ggplot (aes(x = reorder(line, mymean), y = mymean, label = groups,
              ymin=ymin, ymax=ymax)) + 
  geom_point()+
  geom_text(aes(y = 10)) +
  geom_errorbar()+
  labs(x="Line", y="Weed biomass ratio", title = "Add title")


# posthoc <- HSD.test(anova.wbr, "zone", group = T, console = T)
# posthoc2 <- posthoc$group %>%
#   rownames_to_column("zone")
# 
# weed_wbr <- full_join(weed_wbr, 
#                           select(posthoc2, c(zone, groups)))
# 
# ggplot ( weed_wbr, aes(x = zone, y = weed_biomass_ratio, label = groups)) + 
#   geom_boxplot() +
#   geom_text(aes(y = max(weed_biomass_ratio)+ 0.2*max(weed_biomass_ratio))) +
#   labs(x="Zone", y="Weed biomass ratio", title = "Add title")
# 

# p-value > 0,05 so no tuckey test

#t.test : comparing the control to all the treatments
# control_wbr<-weed_wbr%>%
#   filter(line==1)
# treatment_wbr<-weed_wbr%>%
#   filter(line!=1)
# 
# t.test(control_wbr$weed_biomass_ratio, treatment_wbr$weed_biomass_ratio)

#t.test : comparing the mono to the mix
#mono_wbr<-weed_wbr%>%
  #filter(nature=="mono")
#mix_wbr<-weed_wbr%>%
  #filter(nature=="mix")

#t.test(mono_wbr$weed_biomass_ratio, mix_wbr$weed_biomass_ratio)

#lines <- c("6","7","8","11","14")
#testfiltre <- weed_wbr %>% 
 # filter(line %in% lines)



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
