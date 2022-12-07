# working directory
getwd()
#setwd("/home/lucile/Bureau/Mpl Sup Agro/2A/project/vineyards")
setwd("C:/Users/vallet/Documents/Doctorat/Enseignement/Supagro_stat/Cover_crop/")
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

# Sigmoid fitting ==============================================================
canopeo_cc%>%
  mutate(date=dmy(date),
         Line=as.factor(Line))->canopeo_cc #reclassify variables

List_model <-
  list() #Creation of an empty list, will carry model infos for each line
for (line in 1:14) {
  #Select variables for fitting ------------------------------------------------
  canopeo_cc %>%
    filter(Line == line) -> canopeo_line #Select only line of interest
  date <- yday(canopeo_line$date)
  cover_rate <- canopeo_line$cover_rate
  
  # Fit a sigmoid model --------------------------------------------------------
  #K = 100, b= slope at Midpoint, c= translation (refer to time of the temporal distance with DOY 0)
  M1 <-
    nls(cover_rate ~ 100 / (1 + exp(-b * (date + c))), start = list(b = 0.3, c =
                                                                      -300))
  #Make prediction--------------------------------------------------------------
  x_seq <- c(0:365)
  prediction <- predict(M1, newdata = data.frame(date = x_seq))
  plot(date, cover_rate)
  lines(x_seq, prediction)
  
  #Select ouptuts---------------------------------------------------------------
  output <- list(
    "line" = line,
    "date" = date,
    "cover_rate" = cover_rate,
    "Model_SlopeAtMidpoint" = M1$m$getPars()[1],
    "Model_TranslationFrom0"= M1$m$getPars()[2],
    "x_seq" = x_seq,
    "prediction" = prediction
  )
  List_model[[line]] <- output
}

#Extract actual data
data <-
  map(List_model,
      ~ cbind(
        "line" = .$line,
        "date" = .$date,
        "cover_rate" = .$cover_rate
      ))
data <- as.data.frame(reduce(data, rbind))

#Extract predctions
predicted <-
  map(List_model,
      ~ cbind(
        "line" = .$line,
        "x_seq" = .$x_seq,
        "prediction" = .$prediction
      ))
predicted <- as.data.frame(reduce(predicted, rbind))

#Extract model parameters
param<- map(List_model,
            ~ c("line" = .$line,
                "Model_SlopeAtMidpoint"=.$Model_SlopeAtMidpoint,
                "Model_TranslationFrom0"=.$Model_TranslationFrom0))
param<-bind_rows(param)

#Draw it on actual values
ggplot() +
  geom_point(data = data,
             aes(x = date,
                 y = cover_rate,
                 color = as.factor(line))) +
  geom_line(data = predicted,
            aes(x = x_seq,
                y = prediction,
                color = as.factor(line))) +
  theme_minimal()+
  xlim(276,311)

#Draw it on end of year
ggplot() +
  geom_point(data = data,
             aes(x = date,
                 y = cover_rate,
                 color = as.factor(line))) +
  geom_line(data = predicted,
            aes(x = x_seq,
                y = prediction,
                color = as.factor(line))) +
  theme_minimal()+
  xlim(c(250,365))

