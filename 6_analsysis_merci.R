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
  labs(x="Line", y="Cover rate", title = "Percentage of coverance of the soil for different cover crops")

#Anova line
anova.merci<-lm(merci_value~Line, merci_cc)
anova(anova.merci)
par(mfrow = c(1,1))
plot(anova.merci)

kruskal.merci <- kruskal(merci_cc$merci_value, merci_cc$Line, console = T)
table.letters.merci <- kruskal.merci$groups %>%
  rownames_to_column("line") %>%
  select(line, groups)
view(table.letters.merci)

