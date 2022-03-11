library(tidyverse)
library(rnoaa)
library(climate)
library(dplyr)

jap <- read.csv("data/skorea1.csv") 

(stations <- ghcnd_stations())

data <- list()
for (i in 1:nrow(jap)){
  station1 <- stations %>% filter(name == jap$location[[i]])
  data <- append(data, list(station1))
  names(data)[i] <- jap$location[i]
  # to find locations with station through rnoaa:
  if (nrow(data[[i]])!=0){ 
  print(data[i])
    }
}
data

# reference: https://stackoverflow.com/questions/17499013/how-do-i-make-a-list-of-data-frames/24376207

# 9 of the 59 locations have a station in ghcnd: "CHUNCHEON" (47101), "GANGNEUNG" (47105 - wmo_id), 
# "INCHEON" (47112), "ULLEUNGDO" (47115), "POHANG" (47138), # "BUSAN" (47159), "MOKPO" (47165), 
# "YEOSU"(47168), "JEJU" (47184) - however, data is not available on all covariates (primarily only
# temperature and precipitation)

CHUNCHEON <- meteo_ogimet(interval = "daily",   date = c("2018-05-01", "2018-07-01"), station = 47101)
GANGNEUNG <- meteo_ogimet(interval = "daily",   date = c("2018-05-01", "2018-07-01"), station = 47105)
INCHEON <- meteo_ogimet(interval = "daily",   date = c("2018-05-01", "2018-07-01"), station = 47112)
ULLEUNGDO <- meteo_ogimet(interval = "daily",   date = c("2018-05-01", "2018-07-01"), station = 47115)
POHANG <- meteo_ogimet(interval = "daily",  date = c("2018-05-01", "2018-07-01"), station = 47138)
BUSAN <- meteo_ogimet(interval = "daily",   date = c("2018-05-01", "2018-07-01"), station = 47159)
MOKPO <- meteo_ogimet(interval = "daily",   date = c("2018-05-01", "2018-07-01"), station = 47165)
YEOSU <- meteo_ogimet(interval = "daily",   date = c("2018-05-01", "2018-07-01"), station = 47168)
JEJU <- meteo_ogimet(interval = "daily",   date = c("2018-05-01", "2018-07-01"), station = 47184)

# https://cran.r-project.org/web/packages/climate/climate.pdf (pg 15) - in GitHub repository: "climate.pdf"
# can use meteo_ogimet() to curate data from https://www.ogimet.com/index.phtml.



