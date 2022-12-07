# working directory
getwd()
setwd("D:/études/SUPAGRO/2A/D4/github/vineyards")

# library 
library(tidyverse)
library(agricolae)
library(lubridate)

# get the data
canopeo_cc_ini = read.table("2_canopeo_time.csv", header = TRUE, sep = ";", dec = ",")
view(canopeo_cc_ini)
# we wanted to turn the column named "zone_X" into one 
#in order for the data to be easier to analyse
canopeo_cc <- canopeo_cc_ini%>%
  select (date, Line, Zone.1, Zone.2, Zone.3)%>%
  pivot_longer(cols = Zone.1:Zone.3, names_to = "zone",values_to = "cover_rate" )
view(canopeo_cc)

canopeo_cc%>%
  mutate(date=dmy(date),
         Line=as.factor(Line))%>%
  ggplot()+
  aes(x=date,y=cover_rate,color=Line)+
  geom_point()+
  geom_smooth()

canopeo_cc_test<-canopeo_cc%>%
  filter(date>03/10/2022)%>%
  filter(date<07/11/2022)
view(canopeo_cc_test)
  
filter (date<"07/11/2022")

canopeo_cc%>%
  mutate(Line=as.factor(Line))->canopeo_cc
