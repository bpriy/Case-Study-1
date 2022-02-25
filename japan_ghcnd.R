library(tidyverse)
library(rnoaa)
library(dplyr)

# public data on pbd from 1921-2021
jap <- read.csv("data/japan1.csv") 

(stations <- ghcnd_stations())

japstat <- list()
for (i in 1:nrow(jap)){
  station1 <- stations %>% filter(name == jap$location[[i]])
  japstat <- append(japstat, list(station1))
  names(japstat)[i] <- jap$location[i]
  # to find locations with no station in ghcnd:
  #if (nrow(japstat[[i]])==0){ 
  #print(jap$location[[i]])
  #}
}
#japstat

# reference: https://stackoverflow.com/questions/17499013/how-do-i-make-a-list-of-data-frames/24376207
# 10 of the 111 locations have no station in ghcnd: "SAKATA", "FUKUSHIMA", "TOYOOKA", "MAIZURU", "NAZEFUNCHATOGE"
# "YONAGUNIJIMA", "IRIOMOTEJIMA", "ISHIGAKIJIMA", "MIYAKOJIMA", "MINAMIDAITOJIMA"





