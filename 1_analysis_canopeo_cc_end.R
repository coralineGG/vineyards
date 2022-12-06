# working directory
getwd()
setwd("D:/études/SUPAGRO/2A/D4/github/vineyards")

# library 
library(tidyverse)
library(agricolae)

# get the data
canopeo_cc_ini = read.table("Data_collection_canopeo_cc_only.csv", header = TRUE, sep = ";", dec = ",")
view(canopeo_cc_ini)
# we wanted to turn the column named "zone_X" into one 
#in order for the data to be easier to analyse
canopeo_cc <- canopeo_cc_ini%>%
  select (Line, Z1, Z2, Z3)%>%
  pivot_longer(cols = Z1:Z3, names_to = "zone",values_to = "cover_rate" )
view(canopeo_cc)

canopeo_cc%>%
  mutate(Line=as.factor(Line))->canopeo_cc
#Graph 
library(ggplot2)
canopeo_cc%>%
  group_by(Line)%>%
  summarise(mymean=mean(cover_rate, na.rm=T),
            mysd=sd(cover_rate, na.rm=T))%>%
  mutate(ymax=mymean+mysd, ymin=mymean-mysd)%>%
  ggplot ()+
  aes(x = reorder(Line, mymean), y = mymean, fill=Line) + 
  geom_boxplot()+
  #geom_errorbar()+
  labs(x="Line", y="Cover rate", title = "Percentage of coverance of the soil for different cover crops")

canopeo_cc%>%
  group_by(Line)%>%
  summarise(mymean=mean(cover_rate, na.rm=T),
            mysd=sd(cover_rate, na.rm=T))%>%
  mutate(ymax=mymean+mysd, ymin=mymean-mysd)%>%
  ggplot()+
  aes(x=Line, y=cover_rate,fill=Line)+
  geom_boxplot()

canopeo_cc%>%
  ggplot()+
  aes(x=Line, y=cover_rate,fill=Line)+
  geom_boxplot()

#Anova line+zone
anova.canopeo1<-lm(cover_rate~as.factor(Line)+zone, canopeo_cc)
anova(anova.canopeo1)
summary(anova.canopeo1)
par(mfrow = c(1,1))
plot(anova.canopeo1.l)

## Our P-value is > 0,05 so we cannot do a tukey test

# #anova zone
# anova.canopeo1.z<-lm(cover_rate~zone, canopeo_cc)
# anova(anova.canopeo1.z)
# par(mfrow = c(1,1))
# plot(anova.canopeo1.z)


tukey.canopeo.1<- HSD.test(anova.canopeo1, "zone", alpha = 0.05, group=TRUE, main = NULL, console=FALSE)
print(tukey.canopeo.1)
table.letters.canopeo.1 <- tukey.canopeo.1$groups %>%
  rownames_to_column("zone") %>%
  select(zone, groups)
view(table.letters.canopeo.1)

canopeo_cc%>%
  group_by(zone)%>%
  summarise(mymean=mean(cover_rate, na.rm=T),
            mysd=sd(cover_rate, na.rm=T))%>%
  mutate(ymax=mymean+mysd, ymin=mymean-mysd)%>%
  full_join(table.letters.canopeo.1)%>%
  ggplot (aes(x = reorder(zone, mymean), y = mymean, ymin=ymin, ymax=ymax, label = groups)) + 
  geom_text(aes(y = 80)) +
  geom_point()+
  geom_errorbar()+
  labs(x="Zone", y="Cover rate", title = "Percentage of coverance of the soil for different cover crops")
summary(anova.canopeo1.z)
