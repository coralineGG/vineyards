# library
library(tidyverse)
library(ggplot2)

getwd()
setwd(paste0(getwd(),"/vineyard_analysis"))

### help to build the dataset ####### 

# get the data of the biomass
biomass_cc_w_ini = read.table("4_biomass_cc_w.csv", header = TRUE,sep = ";",
                              dec = ",")
view(biomass_cc_w_ini)

# pivot table 
biomass_cc_w <- biomass_cc_w_ini %>%
  pivot_longer(cols = Z1:Z3, 
               names_to = "zone", values_to = "biomass" )
view(biomass_cc_w)

# get the data of emergence_rate
emergence_cc_w_ini = read.table("4_emergence_rate.csv", header = TRUE,sep = ";",
                              dec = ",")
view(emergence_cc_w_ini)

#pivot table
emergence_cc_w <- emergence_cc_w_ini %>%
  pivot_longer(cols = Z1:Z3, 
               names_to = "zone", values_to = "emergence_rate" )
view(emergence_cc_w)

# join the tables 

biomass_emergence <- biomass_cc_w %>%
  full_join(emergence_cc_w)%>%
  mutate(Line = factor(Line))
view(biomass_emergence)

#do the LER colomns 

# with the emergence rate at the beginning 
LER<-biomass_emergence %>%
  mutate( pre_LERp = biomass / emergence_rate )
  
view(LER)

### importation du bon tableur #####
LER= read.table("4_LER.csv", header = TRUE,sep = ";", dec = ",")
view(LER)

### analysis ####  

##### weed regulation and competition in the cover #### 

# graph of the values of LER in function of the treatment 
LER%>%
  filter( mono_mix == "mix") %>%
  filter (Specie != "Weed") %>%
  mutate(Line=as.factor(Line))%>%
  group_by(Line)%>%
  summarise(mymean=mean(LER_cc, na.rm=T),
            mysd=sd(LER_cc, na.rm=T))%>%
  mutate(ymax=mymean+mysd, ymin=mymean-mysd) %>%
  ggplot (aes(x = reorder(Line, mymean), y = mymean,
              ymin=ymin, ymax=ymax, na.rm=T)) + 
  geom_point()+
  geom_errorbar()+
  labs(x="Line", y="LER value", title = "LER value in function of the treatment")

#Test anova sur les LER
anova.LER <-lm(LER_cc~Line, LER)
anova(anova.LER)
summary(anova.LER)

par(mfrow = c(2,2))
plot(anova.LER)
# ça passe on pourrait enlever les observations 5 et 86

#sans la ligne 5 et 86
LER_best <- LER %>%
  rownames_to_column("observation")%>%
  filter (observation != 86)%>%
  filter( observation != 5)

#graph sans les lignes : 
LER_best%>%
  filter( mono_mix == "mix") %>%
  filter (Specie != "Weed") %>%
  group_by(Line)%>%
  summarise(mymean=mean(LER_cc, na.rm=T),
            mysd=sd(LER_cc, na.rm=T))%>%
  mutate(ymax=mymean+mysd, ymin=mymean-mysd) %>%
  ggplot (aes(x = reorder(Line, mymean), y = mymean,
              ymin=ymin, ymax=ymax, na.rm=T)) + 
  geom_point()+
  geom_errorbar()+
  labs(x="Line", y="LER value", title = "LER value in function of the treatment")

#Test anova 
anova.LER.best <-lm(LER_cc~Line, LER_best)
anova(anova.LER)

par(mfrow = c(2,2))
plot(anova.LER.best)

view(LER)

#### pour chaque espèce, comparaison 

### faire avec la densité de semis le calcul de 
#   la LER pour voir ce que ça rend 


