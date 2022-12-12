# working directory
getwd()
setwd("D:/études/SUPAGRO/2A/D4/github/vineyards")

# library 
library(tidyverse)
library(agricolae)
library(ggplot2)
library(car)
library(dplyr)


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
  mutate(Line=as.factor(Line))%>%
  rename('line'='Line') -> line_content

#shapiro for normality 
shapiro.test(canopeo_weed$weed_cover_rate)
# not normality of the values 

#levene for homogeneity of variance
leveneTest(weed_cover_rate~line, canopeo_weed)
#ok for the lines, the variances are homogene 
leveneTest(weed_cover_rate~zone, canopeo_weed)
#ok for the zones, the variances are homogene
## we can't do an anova


# Kruskal
kruskal <- kruskal(canopeo_weed$weed_cover_rate, canopeo_weed$line, console = T)
table.letters.covers <- kruskal$groups %>%
  rownames_to_column("line") %>%
  select(line, groups)%>%
  mutate(line=as.factor(line))


#dunn.test après un k test

canopeo_weed%>%
  full_join(line_content) -> canopeo_weed

#boxplot " graph 
canopeo_weed%>%
  full_join(table.letters.covers)-> canopeo_weed

canopeo_weed.1<-canopeo_weed%>%
  with(reorder(cover_crop, weed_cover_rate, median, na.rm=T))

canopeo_weed%>%
  ggplot()+
  aes(x=reorder(cover_crop, weed_cover_rate, na.rm=TRUE), y=weed_cover_rate, fill=cover_crop, label = groups,)+
  geom_text(aes(y = 75)) +
  geom_boxplot()+
  theme(axis.text.x = element_text(angle=45), legend.position = 'none')+
  labs(x="Treatment", y="weed cover rate in percentage", title = "Percentage of soil covered by weeds per treatment")

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

# we wanted to turn the column named "zone_X" into one 
#in order for the data to be easier to analyse
biomass_weed <- biomass_weed_ini%>%
  pivot_longer(cols = Z1:Z3, names_to = "zone",values_to = "weed_biomass" )%>%
  mutate(line=as.factor(line))
view(biomass_weed)


#Anova line

shapiro.test(biomass_weed$weed_biomass)
#p value = 0,06>0,05 ==> normality of the values 
leveneTest(weed_biomass~line, biomass_weed)
#pvalue >0.05 ==> homogeneity of variance 
# ==> can do an anova 

anova.biomass.l<-lm(weed_biomass~line, biomass_weed)
anova(anova.biomass.l)
# anova not significative
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
  theme(axis.text.x = element_text(angle=45), legend.position = 'none')+
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

# we wanted to turn the column named "zone_X" into one 
#in order for the data to be easier to analyse
density_w_cc <-density_ini%>%
  pivot_longer(cols = Z1:Z3, names_to = "zone",values_to = "weed_density" )%>%
  mutate(Line=as.factor(Line))

weed_density<-density_w_cc%>%
  filter(Specie=="Weed")

#Anova

shapiro.test(weed_density$weed_density)
#pvalue=0.08 normality ok
leveneTest(weed_density~Line, weed_density)
#pvalue=0.8 homogeneity of variance okay
leveneTest(weed_density~zone, weed_density)
#pvalue=0.8 homogeneity of variance okay

anova.density.l<-glm(weed_density~Line+zone, weed_density, family='poisson')
anova(anova.density.l)
summary(anova.density.l)
par(mfrow = c(1,1))
plot(anova.density.l)

# tuket because a lot of line have significant difference 


# tukey
tukey.density.l<- HSD.test(anova.density.l, "Line", alpha = 0.05, group=TRUE, main = NULL,
                   console=FALSE)

table.letters.density <- tukey.density.l$groups %>%
  rownames_to_column("Line") %>%
  select(Line, groups)
view(table.letters.density)

#Graph 
#boxplot 

weed_density$Line<-with(weed_density,reorder(Line, weed_density, median, na.rm=T))
weed_density%>%
  full_join(table.letters.density)%>%
  ggplot()+
  aes(x=reorder(Line,weed_density, na.rm=TRUE) , y=weed_density, fill=Line, group=Line, label=groups)+
  geom_boxplot()+
  geom_text(aes(y=-1))+
  theme(axis.text.x = element_text(angle=45), legend.position = 'none')+
  labs(x="Treatment", y="Weed density (plant/250cm2)", title = "Weed density measured for different cover crops")


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