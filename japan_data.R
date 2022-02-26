library(tidyverse)
library(rnoaa)
library(climate)
library(dplyr)

# public data on pbd from 1921-2021
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



# search nearest stations to cities in Japan with no station
ja_cities <- c("SAKATA", "FUKUSHIMA", "TOYOOKA", "MAIZURU", "NAZE", "YONAGUNIJIMA", "IRIOMOTEJIMA", "ISHIGAKIJIMA", "MIYAKOJIMA", "MINAMIDAITOJIMA")
add_ja_cities <- read.csv("data/japan.csv", header=TRUE)
add_ja_cities$country <- str_split_fixed(add_ja_cities$location, "/", 2)[,1]
add_ja_cities$city <- str_split_fixed(add_ja_cities$location, "/", 2)[,2]

ncol(add_ja_cities)
df_ja1 <- data.frame(c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0))
df_ja <- data.frame(c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0), c(0))
for(q in 1:length(ja_cities)){
  df_ja1 <- add_ja_cities[add_ja_cities$city==str_to_title(ja_cities)[q], ]
  # https://stackoverflow.com/questions/8145670/how-do-i-select-the-first-row-in-an-r-data-frame-that-meets-certain-criteria
  df_ja[q,] <- subset(df_ja1, subset = !duplicated(df_ja1["city"]))
}

ls_ja <- list(NULL)
ja_5 <- list(NULL)
for(r in 1:length(ja_cities)){
  ls_ja[[r]] <- data.frame(id=str_to_title(ja_cities)[r], latitude=round(df_ja[r,2], 4), longitude=round(df_ja[r,3], 4))
  df <- unlist(ls_ja[[r]])
  df <- data.frame(id=df[1], latitude=df[2], longitude=df[3])
  ja_5[[r]] <- meteo_nearby_stations(lat_lon_df=df, limit=1)
}

# closest station for each city in Japan that did not have a station
ja_5
