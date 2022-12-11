# working directory
getwd()
setwd("D:/études/SUPAGRO/2A/D4/github/vineyards")

# library 
library(tidyverse)
library(agricolae)
library(car)
library(ggplot2)

# get the data
canopeo_cc_ini = read.table("Data_collection_canopeo_cc_only.csv", header = TRUE, sep = ";", dec = ",")
view(canopeo_cc_ini)
# we wanted to turn the column named "zone_X" into one 
#in order for the data to be easier to analyse
canopeo_cc <- canopeo_cc_ini%>%
  select (Line, Z1, Z2, Z3)%>%
  pivot_longer(cols = Z1:Z3, names_to = "zone",values_to = "cover_rate" )
view(canopeo_cc)

line_content = read.table("vineyard_analysis/Line_content.csv", header = TRUE, sep = ";", dec = ",")

line_content%>%
  mutate(Line=as.factor(Line))-> line_content

canopeo_cc%>%
  mutate(Line=as.factor(Line))%>%
  full_join(line_content)->canopeo_cc

view(canopeo_cc)




#fonctionne pas encore
shapiro.test(canopeo_cc$cover_rate) # normality of values 
leveneTest(cover_rate~Line, data=canopeo_cc) # homogenity of varances 

#Anova line+zone
anova.canopeo1<-lm(cover_rate~Line+zone, canopeo_cc)
anova(anova.canopeo1)
summary(anova.canopeo1)
par(mfrow = c(1,1))
plot(anova.canopeo1)

## Our P-value is < 0,05 so we can do a tukey test

# #anova zone
# anova.canopeo1.z<-lm(cover_rate~zone, canopeo_cc)
# anova(anova.canopeo1.z)
# par(mfrow = c(1,1))
# plot(anova.canopeo1.z)

# Attention tuckey pour les zones et pas pour les lignes !!
tukey.canopeo.1<- HSD.test(anova.canopeo1, "zone", alpha = 0.05, group=TRUE, main = NULL, console=FALSE)
print(tukey.canopeo.1)
table.letters.canopeo.1 <- tukey.canopeo.1$groups %>%
  rownames_to_column("zone") %>%
  select(zone, groups)
view(table.letters.canopeo.1)

# tukey for the lines 

tukey.canopeo.line<- HSD.test(anova.canopeo1, "Line", alpha = 0.05, group=TRUE, main = NULL, console=FALSE)
print(tukey.canopeo.line)
table.letters.canopeo.line <- tukey.canopeo.line$groups %>%
  rownames_to_column("Line") %>%
  select(Line, groups)
view(table.letters.canopeo.line)

#graph 

canopeo_cc$cover_crop<-with(canopeo_cc, reorder(cover_crop, cover_rate, median, na.rm=T))
canopeo_cc%>%
  full_join(table.letters.canopeo.line)%>%
  ggplot()+
  aes(x=cover_crop, y=cover_rate, fill=cover_crop, label=groups)+
  geom_text(aes(y=-10))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle=45), legend.position='none')+
  labs(x="Treatment", y="Cover rate in percentage", title = "Cover rate for different cover crops treatments")



