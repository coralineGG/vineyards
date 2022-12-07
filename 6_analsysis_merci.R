# working directory
getwd()
setwd("D:/études/SUPAGRO/2A/D4/github/vineyards")

# library 
library(tidyverse)
library(agricolae)

# get the data
merci_cc_ini = read.table("6_merci.csv", header = TRUE, sep = ";", dec = ",")
view(merci_cc_ini)

merci_cc <- merci_cc_ini%>%
  select (Line, Z1, Z2, Z3)%>%
  pivot_longer(cols = Z1:Z3, names_to = "zone",values_to = "merci_value" )
view(merci_cc)
#Graph 
library(ggplot2)
merci_cc%>%
  group_by(Line)%>%
  summarise(mymean=mean(merci_value, na.rm=T),
            mysd=sd(merci_value, na.rm=T))%>%
  mutate(ymax=mymean+mysd, ymin=mymean-mysd)%>%
  ggplot (aes(x = reorder(Line, mymean), y = mymean, ymin=ymin, ymax=ymax)) + 
  geom_point()+
  geom_errorbar()+
  labs(x="Line", y="N quantity potentially returned to the soil", title = "Potential release of N in the soil per treatment in Kg/ha")

#boxplot ## ne fonctionne 
merci_cc$Line<-with(merci_cc, reorder(Line, merci_value, median, na.rm=T))
merci_cc%>%
  ggplot(aes(x=Line, y=merci_value, fill=Line))+
  geom_boxplot()+
  labs(x="Treatment", y="Quantity of N potentially released into the soil (Kg/ha)", title = "N quantity potentially released by the cover into the soil")

#Anova line
anova.merci<-lm(merci_value~Line+zone, merci_cc)
anova(anova.merci)
par(mfrow = c(1,1))
plot(anova.merci)

kruskal.merci <- kruskal(merci_cc$merci_value, merci_cc$Line, console = T)
table.letters.merci <- kruskal.merci$groups %>%
  rownames_to_column("line") %>%
  select(line, groups)
view(table.letters.merci)
