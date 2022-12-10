# working directory
getwd()
setwd("D:/études/SUPAGRO/2A/D4/github/vineyards")

# library 
library(tidyverse)
library(agricolae)
library(ggplot2)
library(car)


# get the data for the cover rate
canopeo_weed_ini = read.table("data_canopeo_weeds.csv", header = TRUE, sep = ";", dec = ",")
view(canopeo_weed_ini)

# we wanted to turn the column named "zone_X" into one : pivot  
#in order for the data to be easier to analyse
canopeo_weed <- canopeo_weed_ini%>%
  select (date, line, z1, z2, z3,nature,treatment)%>%
  mutate(line = as.factor(line)) %>% 
  pivot_longer(cols = z1:z3, names_to = "zone",values_to = "weed_cover_rate" )
view(canopeo_weed)

# get the content of each line 
line_content = read.table("vineyard_analysis/Line_content.csv", header = TRUE, sep = ";", dec = ",")

view(line_content)

line_content%>%
  mutate(Line=as.factor(Line))

#Graph 

# ajouter les vérifications anova quand lisa trouve !!##
#shapiro for normality 
shapiro.test(canopeo_weed$weed_cover_rate)
#normality of the values 

#levene for homogeneity of variance
leveneTest(weed_cover_rate~line, canopeo_weed)
#ok for the lines, the variances are homogene 
leveneTest(weed_cover_rate~zone, canopeo_weed)
#ok for the zones, the variances are homogene
## we can do an anova

#Anova line+zone
anova.canopeo <-lm(weed_cover_rate~line+zone, canopeo_weed)
anova(anova.canopeo)
par(mfrow = c(2,2))
plot(anova.canopeo)

summary(anova.canopeo)

# je ne suis pas sure qu'il faille faire un kruskal si ertaines modalités sont significatives à l'anova 
# Kruskal
# kruskal <- kruskal(canopeo_weed$weed_cover_rate, canopeo_weed$line, console = T)
# table.letters.covers <- kruskal$groups %>%
#   rownames_to_column("line") %>%
#   select(line, groups)%>%
#   mutate(line=as.integer(line))
# view(table.letters.covers)

#tukey parce que anova significative pour certains treatments 
tukey.weed.line <- HSD.test(anova.canopeo, "line", group = T, console = T)
# pas de différence entre les treatments 

#tukey car anova significative pour les differentes zones 
tukey.weed.zone <- HSD.test(anova.canopeo, "zone", group = T, console = T)

#dunn.test après un k test

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

#boxplot
canopeo_weed.1<-canopeo_weed%>%
  with(reorder(line, weed_cover_rate, median, na.rm=T))%>%
  full_join(table.letters.covers)
view(canopeo_weed)
view(table.letters.covers)
canopeo_weed.1%>%
  ggplot()+
  aes(x=line, y=weed_cover_rate,fill=line, label = groups,)+
  geom_text(aes(y = 75)) +
  geom_boxplot()+
  labs(x="Treatment", y="weed cover rate in percentage", title = "Percentage of soil covered by weeds per treatment")

canopeo_weed%>%
  ggplot()+
  aes(x=reorder(line, weed_cover_rate, FUN=median, label = groups()), y=weed_cover_rate, fill=line)+
  geom_boxplot()+
  geom_text(aes(x=line, y=75, label=table.letters.covers$groups))+
  labs(x="Treatment", y="weed cover rate in percentage", title = "Percentage of soil covered by weeds per treatment")

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
  
#t.test : comparing the mono to the mix
mono_cover_rate<-canopeo_weed%>%
  filter(nature=="monoculture")
mix_cover_rate<-canopeo_weed%>%
  filter(nature=="mix")

t.test(mono_cover_rate$weed_cover_rate, mix_cover_rate$weed_cover_rate)

#graph 
canopeo_weed$line<-with(canopeo_weed, reorder(line,weed_cover_rate, median, na.rm=T))
canopeo_weed%>%
  group_by(nature)%>%
  ggplot()+
  aes(x= nature, y=weed_cover_rate, fill=nature)+
  geom_boxplot()+
  labs(x="Nature", y="weed cover rate in percentage", title = "Percentage of coverance of the soil for cover crops in monoculture and in mixes")

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

#Anova line
anova.biomass.l<-lm(weed_biomass~line+zone, biomass_weed)
anova(anova.biomass.l)
summary(anova.biomass.l)
par(mfrow = c(1,1))
plot(anova.biomass.l)
#oui, on peut donc dire que l'expérience ne permet pas de différencier les traitements 
# on ne fait pas de tukey"
# p-value > 0,05 so no tuckey test

#graph
biomass_weed$line<-with(biomass_weed, reorder(line, weed_biomass, median, na.rm=T))
biomass_weed%>%
  ggplot()+
  aes(x=line, y=weed_biomass, fill=line)+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle=45))+
  labs(x="Treatment", y="Weed biomass (g)", title = "Weed biomass measured for different cover crops")

#tukey test
tukey.z<- HSD.test(anova.biomass.l, "zone", alpha = 0.05, group=TRUE, main = NULL,
                    console=FALSE)
print(tukey.z)
table.letters.biomass <- tukey.z$groups %>%
  rownames_to_column("zone") %>%
  select(zone, groups)
view(table.letters.biomass)

#t.test : comparing the mono to the mix
mono_biomass<-biomass_weed%>%
  filter(nature=="monoculture")
mix_biomass<-biomass_weed%>%
  filter(nature=="mix")

t.test(mono_biomass$weed_biomass, mix_biomass$weed_biomass)

#graph t-test
biomass_weed$line<-with(biomass_weed, reorder(line,weed_biomass, median, na.rm=T))
biomass_weed%>%
  group_by(nature)%>%
  ggplot()+
  aes(x= nature, y=weed_biomass, fill=nature)+
  geom_boxplot()

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
#boxplot 
weed_density$Line<-with(weed_density,reorder(Line, weed_density, median, na.rm=T))
weed_density%>%
  ggplot()+
  aes(x=Line, y=weed_density, fill=Line, group=Line)+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle=45))+
  labs(x="Treatment", y="Weed density (plant/250cm2)", title = "Weed density measured for different cover crops")

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

#t.test : comparing the mono to the mix
mono_density<-weed_density%>%
  filter(nature=="monoculture")
mix_density<-weed_density%>%
  filter(nature=="mix")

t.test(mono_density$weed_density, mix_density$weed_density)

#graph t-test
weed_density$Line<-with(weed_density, reorder(Line,weed_density, median, na.rm=T))
weed_density%>%
  group_by(nature)%>%
  ggplot()+
  aes(x= nature, y=weed_density, fill=nature)+
  geom_boxplot()+
  labs(x="Treatment", y="Weed density (plant/250cm2)", title = "Comparision of the weed density measured for different cover crops in monoculture and in association")