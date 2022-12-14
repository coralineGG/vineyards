# WEED BIOMASS RATIO (WBR)
getwd()
setwd("D:/études/SUPAGRO/2A/D4/github/vineyards")

# library 
library(tidyverse)
library(agricolae)

# get the data for the cover rate

weed_biomass = read.table("5_WBR_Weed.csv", header = TRUE, sep = ",", dec = ".")
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

#verification before the anova 
shapiro.test(weed_wbr$weed_biomass_ratio)
#p-value = 4.543e-09
# can't do an anova because not normality of values 

# Kruskal
kruskal <- kruskal(weed_wbr$weed_biomass_ratio, weed_wbr$line, console = T)
posthock <- kruskal$groups %>% 
  rownames_to_column("line") %>% 
  select(line, groups)

weed_wbr%>%
  group_by(line)%>%
  summarise(mymean=mean(weed_biomass_ratio, na.rm=T),
            mysd=sd(weed_biomass_ratio, na.rm=T))%>%
  mutate(ymax=mymean+mysd, ymin=mymean-mysd) %>% 
  full_join(posthock) %>% 
  ggplot (aes(x = reorder(line, mymean), y = mymean, label = groups,
              ymin=ymin, ymax=ymax)) + 
  geom_point()+
  geom_text(aes(y = 9)) +
  geom_errorbar()+
  labs(x="Traitments", y="Weed biomass ratio (WBR)", title = "Weed regulation with cover crops")



