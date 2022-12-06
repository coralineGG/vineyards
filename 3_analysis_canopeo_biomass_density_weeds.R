# working directory
# getwd()
# setwd("D:/études/SUPAGRO/2A/D4/github/vineyards")

# library 
library(tidyverse)
library(agricolae)


# get the data for the cover rate
canopeo_weed_ini = read.table("data_canopeo_weeds.csv", header = TRUE, sep = ";", dec = ",")
view(canopeo_weed_ini)
# we wanted to turn the column named "zone_X" into one 
#in order for the data to be easier to analyse
canopeo_weed <- canopeo_weed_ini%>%
  select (date, line, z1, z2, z3,nature,treatment)%>%
  mutate(line = as.factor(line)) %>% 
  pivot_longer(cols = z1:z3, names_to = "zone",values_to = "weed_cover_rate" )
view(canopeo_weed)

#Graph 
library(ggplot2)

#Anova line+zone
anova.canopeo <-lm(weed_cover_rate~line+zone, canopeo_weed)
anova(anova.canopeo)
par(mfrow = c(2,2))
plot(anova.canopeo)

summary(anova.canopeo)

# Kruskal
kruskal <- kruskal(canopeo_weed$weed_cover_rate, canopeo_weed$line, console = T)
table.letters.covers <- kruskal$groups %>%
  rownames_to_column("line") %>%
  select(line, groups)
view(table.letters.covers)
#pas sur de ce à quoi ça sert...
#est-ce que c'est pour grouper les lignes associée par le kruskal entre elles ?

canopeo_weed%>%
  group_by(line)%>%
  summarise(mymean=mean(weed_cover_rate, na.rm=T),
            mysd=sd(weed_cover_rate, na.rm=T))%>%
  mutate(ymax=mymean+mysd, ymin=mymean-mysd) %>% 
  full_join(table.letters.covers) %>% 
  ggplot (aes(x = reorder(line, mymean), y = mymean, label = groups,
              ymin=ymin, ymax=ymax)) + 
  geom_point()+
  geom_text(aes(y = 75)) +
  geom_errorbar()+
  labs(x="Line", y="Weed cover rate", title = "Percentage of weed soil coverance when different cover crops were grown")

#essaie boxplot
canopeo_weed%>%
  ggplot()+
  aes(x=line, y=weed_cover_rate,fill=line)+
  geom_boxplot()

# tukey.weed.3 <- HSD.test(anova.canopeo.l, "line", group = T, console = T)
# print (tukey.weed.3)
# table.letters.weed.3<- tukey.weed.3$group %>%
#   rownames_to_column("line")%>%
#   select(line,groups)
# # je ne sais pas non plus à quoi ça sert mais on l'utilise après
# 
# canopeo_weed <- full_join(canopeo_weed,
#                           select(tukey, c(zone, groups)))

## c'est tous les même d'après tukey

# ggplot ( canopeo_weed, aes(x = zone, y = weed_cover_rate, label = groups)) +
#   geom_boxplot() +
#   geom_text(aes(y = max(weed_cover_rate)+ 0.2*max(weed_cover_rate))) +
#   labs(x="Zone", y="Weed cover rate", title = "Percentage of weed soil coverance when different cover crops were grown")

#pas de graph parce que pas de diff d'près tukey

#Anova pour l'étude des zones

canopeo_weed_1 <- canopeo_weed%>%
  mutate(weed_cover_rate=weed_cover_rate/100)
 
anova.canopeo.z <- glm(weed_cover_rate~zone, canopeo_weed, family = "binomial") 
anova(anova.canopeo.z)
par(mfrow = c(1,1))
plot(anova.canopeo.z)
summary(anova.canopeo.z)

# kruskal
kruskal.weed.zone <- kruskal(canopeo_weed$weed_cover_rate, canopeo_weed$zone, console = T)
table.letters.covers <- kruskal$groups %>%
  rownames_to_column("line") %>%
  select(line, groups)
view(table.letters.covers)


#n don't show anything
tuckey.z<- HSD.test(anova.canopeo.z, "zone", alpha = 0.05, group=TRUE, main = NULL, 
         console=FALSE)
print(tuckey.z)

#put the letters 
canopeo_weed%>%
  group_by(zone)%>%
  summarise(mymean=mean(weed_cover_rate, na.rm=T),
            mysd=sd(weed_cover_rate, na.rm=T))%>%
  mutate(ymax=mymean+mysd, ymin=mymean-mysd) %>% 
  ggplot (aes(x = reorder(zone, mymean), y = mymean, label = zone,
              ymin=ymin, ymax=ymax)) + 
  geom_point()+
  geom_text(aes(y = 75)) +
  geom_errorbar()+
  labs(x="Zone", y="Weed cover rate", title = "Percentage of weed soil coverance when different cover crops were grown")


#t.test : comparing the control to all the treatments
control_cover_rate<-canopeo_weed%>%
  filter(line==1)
treatment_cover_rate<-canopeo_weed%>%
  filter(line!=1)

t.test(control_cover_rate$weed_cover_rate, treatment_cover_rate$weed_cover_rate)

canopeo_weed%>%
  
#test graph 
canopeo_weed%>%
  ggplot()+
  aes(x=(, line!=1), y=weed_cover_rate,fill=line)+
  geom_boxplot()

#t.test : comparing the mono to the mix
mono_cover_rate<-canopeo_weed%>%
  filter(nature=="mono")
mix_cover_rate<-canopeo_weed%>%
  filter(nature=="mix")

t.test(mono_cover_rate$weed_cover_rate, mix_cover_rate$weed_cover_rate)

#test graph 
canopeo_weed%>%
  group_by(nature)%>%
  ggplot()+
  aes(x= nature, y=weed_cover_rate, fill=line)+
  geom_boxplot()

# lines <- c("6","7","8","11","14")
# testfiltre <- canopeo_weed %>% 
#   filter(line %in% lines)
#je crois que c'est pour une autre question



###########Weed biomass################

#get the data for the biomass

biomass_weed_ini = read.table("3_weed_biomass.csv", header = TRUE, sep = ";", dec = ",")
view(biomass_weed_ini)
# we wanted to turn the column named "zone_X" into one 
#in order for the data to be easier to analyse
biomass_weed <- biomass_weed_ini%>%
  pivot_longer(cols = Z1:Z3, names_to = "zone",values_to = "weed_biomass" )%>%
  mutate(line=as.factor(line))
view(biomass_weed)

#Graph 
library(ggplot2)
biomass_weed%>%
  group_by(line)%>%
  summarise(mymean=mean(weed_biomass, na.rm=T),
            mysd=sd(weed_biomass, na.rm=T))%>%
  mutate(ymax=mymean+mysd, ymin=mymean-mysd)%>%
  ggplot (aes(x = reorder(line, mymean), y = mymean, ymin=ymin, ymax=ymax)) + 
  geom_point()+
  geom_errorbar()+
  labs(x="Line", y="Weed biomass (g)", title = "Biomass of the weeds per line")

#Anova line
anova.biomass.l<-lm(weed_biomass~line+zone, biomass_weed)
anova(anova.biomass.l)
summary(anova.biomass.l)

#pouvait-on faire l'anova ?
par(mfrow = c(1,1))
plot(anova.biomass.l)
#oui, on peut donc dire que l'expérience ne permet pas de différencier les traitements 
# on ne fait pas de tukey"
# p-value > 0,05 so no tuckey test

#tukey test
tukey.z<- HSD.test(anova.biomass.z, "zone", alpha = 0.05, group=TRUE, main = NULL,
                    console=FALSE)
print(tuckey.z)
table.letters.biomass <- tukey.z$groups %>%
  rownames_to_column("zone") %>%
  select(zone, groups)
view(table.letters.biomass)

# biomass_weed%>%
#   group_by(zone)%>%
#   summarise(mymean=mean(weed_biomass, na.rm=T),
#             mysd=sd(weed_biomass, na.rm=T))%>%
#   mutate(ymax=mymean+mysd, ymin=mymean-mysd)%>%
#   full_join(table.letters.biomass) %>% # ne finctionne pas
#   ggplot (aes(x = reorder(zone, mymean), y = mymean, ymin=ymin, ymax=ymax, label= groups)) + 
#   geom_point()+
#   geom_text(aes(y=20))+
#   geom_errorbar()+
#   labs(x="Zone", y="Weed biomass", title = "Biomass of the weeds per line")


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
  pivot_longer(cols = Z1:Z3, names_to = "zone",values_to = "weed_density" )%>%
  mutate(Line=as.factor(Line))
view(density_w_cc)

weed_density<-density_w_cc%>%
  filter(Specie=="Weed")
view(weed_density)

#Graph 
library(ggplot2)
weed_density%>%
  group_by(Line)%>%
  summarise(mymean=mean(weed_density, na.rm=T),
            mysd=sd(weed_density, na.rm=T))%>%
  mutate(ymax=mymean+mysd, ymin=mymean-mysd)%>%
  ggplot (aes(x = reorder(Line, mymean), y = mymean, ymin=ymin, ymax=ymax)) + 
  geom_point()+
  geom_errorbar()+
  labs(x="Line", y="Weed density", title = "Weed density per line 19 days after the sowing date")

#boxplot 
weed_density%>%
  ggplot()+
  aes(x=Line, y=weed_density, fill=Line, group=Line)+
  geom_boxplot()

#Anova
anova.density.l<-glm(weed_density~Line+zone, weed_density, family='poisson')
anova(anova.density.l)
summary(anova.density.l)
par(mfrow = c(1,1))
plot(anova.biomass)

# no tuckey test

# # anova zone
# anova.density.z<-lm(weed_density~zone, weed_density)
# anova(anova.density.z)
# par(mfrow = c(1,1))
# plot(anova.biomass)
# 
# # tukey
# tukey.density.z<- HSD.test(anova.density.z, "zone", alpha = 0.05, group=TRUE, main = NULL, 
#                    console=FALSE)
# print(tukey.density.z)
# table.letters.density <- tukey.z$groups %>%
#   rownames_to_column("zone") %>%
#   select(zone, groups)
# view(table.letters.density)
# 
# weed_density%>%
#   group_by(zone)%>%
#   summarise(mymean=mean(weed_density, na.rm=T),
#             mysd=sd(weed_density, na.rm=T))%>%
#   mutate(ymax=mymean+mysd, ymin=mymean-mysd)%>%
#   full_join(table.letters.biomass) %>% 
#   ggplot (aes(x = reorder(zone, mymean), y = mymean, ymin=ymin, ymax=ymax, label= groups)) + 
#   geom_point()+
#   geom_text(aes(y=50))+
#   geom_errorbar()+
#   labs(x="Zone", y="Weed density", title = "density of the weeds per line")

#t.test : comparing the control to all the treatments
control_density<-weed_density%>%
  filter(Line==1)
treatment_density<-weed_density%>%
  filter(Line!=1)

t.test(control_density$weed_density, treatment_density$weed_density)
#faire le grphique en groupant tout les treatment pour voir la variance 

#t.test : comparing the mono to the mix
mono_density<-weed_density%>%
  filter(nature=="mono")
mix_density<-weed_density%>%
  filter(nature=="mix")

t.test(mono_density$weed_density, mix_density$weed_density)
