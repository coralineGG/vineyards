# working directory
getwd()
setwd("D:/études/SUPAGRO/2A/D4/github/vineyards")

# library 
library(tidyverse)
library(agricolae)
library(ggplot2)

# get the data
merci_cc_ini = read.table("6_merci_cc_and_weeds.csv", header = TRUE, sep = ";", dec = ",")
view(merci_cc_ini)

merci_cc <- merci_cc_ini%>%
  select (Line, Z1, Z2, Z3)%>%
  pivot_longer(cols = Z1:Z3, names_to = "zone",values_to = "merci_value" )%>%
  mutate(Line=as.factor(Line))

#Graph 

#Anova line

shapiro.test(merci_cc$merci_value) 
#not normality of the values 
# can't do an anova 

kruskal.merci <- kruskal(merci_cc$merci_value, merci_cc$Line, console = T)
table.letters.merci <- kruskal.merci$groups %>%
  rownames_to_column("Line") %>%
  select(Line, groups)
view(table.letters.merci)

#graph

line_content = read.table("vineyard_analysis/Line_content.csv", header = TRUE, sep = ";", dec = ",")

line_content%>%
  mutate(Line=as.factor(Line)) -> line_content

merci_cc%>%
  full_join(table.letters.merci)%>%
  full_join(line_content)%>%
  ggplot()+
  aes(x=reorder(cover_crop, merci_value, na.rm=TRUE) , y=merci_value, fill=cover_crop, label=groups)+
  geom_boxplot()+
  geom_text(aes(y=-1))+
  theme(axis.text.x = element_text(angle=45), legend.position = 'none')+
  labs(x="Treatment", y="Quantity of N potentially released into the soil (Kg/ha)", title = "N quantity potentially released by the cover into the soil")

