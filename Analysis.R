# working directory
getwd()
setwd("D:/études/SUPAGRO/2A/D4/github/vineyards")

# library 
library(tidyverse)

# get the data
canopeo_cc_ini = read.table("canopeo_cc.csv", header = TRUE, sep = ";", dec = ",")

# we wanted to turn the column named "zone_X" into one 
#in order for the data to be easier to analy
canopeo_cc <- canopeo_cc_ini%>%
  select (date, Line, Zone.1, Zone.2, Zone.3, NEW.Z1, NEW.Z3)%>%
  na.omit()%>%
  pivot_longer(canopeo_cc_ini, cols = Zone.1:NEW.Z3, names_to = "zone",values_to = "cover_rate" )
# we don't understand the error message it returns :
# Error in gsub(vec_paste0("^", names_prefix), "", cols) : 
#   assertion 'tree->num_tags == num_tags' failed in executing regexp: file 'tre-compile.c', line 634
# In addition: Warning message:
#   In gsub(vec_paste0("^", names_prefix), "", cols) :
#   argument 'pattern' has length > 1 and only the first element will be used

#Graph 
library(ggplot2)
canopeo_cc%>%
  group_by(Line)%>%
  ggplot (aes(x = Line, y = cover_rate)) + 
  geom_dotplot()+
  geom_pointrange() #we are not sure it will be added to the geom_dotplot()graph but that's what we want
#we couldn't test these lines since the begining of our code doesn't work

#Anova
anova.canopeo<-lm(cover_rate~Line)
