# library
library(tidyverse)
library(ggplot2)

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
  
###### first analysis without LER ######
# I have no idea 

###### Second analysis with LER #####

#do the LER colomns 

# with the emergence rate at the beginning 
biomass_emergence <- biomass_cc_w %>%
  
  

# with density of sowing 
biomass_LER <- biomass_cc_w %>%
  
  



