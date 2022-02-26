library(tidyverse)
library(rnoaa)
library(climate)
library(dplyr)

jap <- read.csv("data/japan1.csv")    

# [daily] "id" - data available 1: ghcnd (rnoaa package) [daily data]
# [hourly or daily - covariates like those we collected manually from the japanese meterological agency]:
    # "wmo_id" (which most japan sites appear to have) - data available 2: ogimet (climate package) [hourly or daily]
    # can be done like in the liestal_data file
(stations <- ghcnd_stations())

data <- list()
for (i in 1:nrow(jap)){
  station1 <- stations %>% filter(name == jap$location[[i]])
  data <- append(data, list(station1))
  names(data)[i] <- jap$location[i]
  # to find locations with no station through rnoaa:
  #if (nrow(data[[i]])==0){ 
  #print(jap$location[[i]])
  #}
}
#data

# reference: https://stackoverflow.com/questions/17499013/how-do-i-make-a-list-of-data-frames/24376207
# 10 of the 111 locations have no station in ghcnd: "SAKATA", "FUKUSHIMA", "TOYOOKA", "MAIZURU", "NAZEFUNCHATOGE"
# "YONAGUNIJIMA", "IRIOMOTEJIMA", "ISHIGAKIJIMA", "MIYAKOJIMA", "MINAMIDAITOJIMA"