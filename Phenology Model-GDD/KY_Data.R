library(tidyverse)
library(rnoaa)
library(purrr)
library(dplyr)
library(Metrics)
library(climate)
library(zoo)

# public data on pbd 
cherry <- read.csv("data/kyoto.csv") 
publicpbd <- cherry %>% subset(select = c(year, bloom_doy))
publicpbd

# set-up temperatures' data set to work with: part 1 - tmin and tmax

### station one: kyoto, ja
temps <- ghcnd_search(stationid = "JA000047759", var=c("tmin", "tmax"), ) 
temps <- temps %>% reduce(full_join, by='date') %>% subset(select = c(date, tmin, tmax))
temps <- temps %>% mutate(year = as.integer(format(date, "%Y")),
                          month = as.integer(strftime(date, '%m')) %% 12, # make December "0"
                          day = as.integer(strftime(date, '%d')),
                          season = cut(month, breaks = c(0, 2, 5, 8, 11),
                                       include.lowest = TRUE,
                                       labels = c("Winter", "Spring", "Summer", "Fall")),
                          year = if_else(month >= 10 | month == 0, year + 1L, year))

temps$tmin <- temps$tmin/10
temps$tmax <- temps$tmax/10

tavg <- ghcnd_search(stationid = "JA000047759", var=c("tavg"))
temps <- left_join(temps, tavg$tavg, by ="date") 
temps <- temps %>% subset(select = c(date, tmin, tmax, tavg, year, month, season, day))
temps$tavg <- temps$tavg/10

# NA values: (code reference - https://stackoverflow.com/questions/53195961/count-total-missing-values-by-group)
nas1 <- temps %>%  group_by(year) %>% summarise_each(funs(sum(is.na(.))))

# Fill in Missing Values based on Hourly Data
for (i in 19602:25446){
  idate <- temps$date[[i]]
  if (!is.na(temps$tmin[[i]]) & !is.na(temps$tmax[[i]])) {
    next
  }
  hdata <- 0
  try(hdata <- meteo_ogimet(interval = "daily", date = c(idate), station = 47759))
  try(temps$tmax[[i]] <- ifelse(is.na(temps$tmin[[i]]), hdata$TemperatureCMax, temps$tmax[i]))
  try(temps$tmin[[i]] <- ifelse(is.na(temps$tmin[[i]]), hdata$TemperatureCMin, temps$tmin[i]))
  print(i)
}

hdata <- meteo_ogimet(interval = "daily", date = c("2004-10-01","2006-09-30"), station = 47759)
#write.csv(hdata,"data\\kyo2005_data.csv", row.names = FALSE)
df1 <- read.csv("data/kyo20046_data.csv")
temps <- read.csv("data/kyoto_data.csv")
temps$date <- as.Date(temps$date, origin = "1899-12-30")
df1$date <- as.Date(df1$date, origin = "1899-12-30")
# reference: https://stackoverflow.com/questions/43230470/how-to-convert-excel-date-format-to-proper-date-in-r
data <- full_join(temps, df1, by = c("date"))
data['tmin'] <- ifelse(!is.na(data$tmin.y), data$tmin.y, data$tmin.x)
data['tmax'] <- ifelse(!is.na(data$tmax.y), data$tmin.y, data$tmax.x)
data['tavg'] <- ifelse(!is.na(data$tavg.y), data$tavg.y, data$tavg.x)
write.csv(data,"data\\kydata.csv", row.names = FALSE)

# Fill Missing Values
temps$tavg <- ifelse(!is.na(temps$tmin) & !is.na(temps$tmax), temps$tmin+temps$tmax/2 , temps$tavg)
temps$tavg <- na.approx(temps$tavg, na.rm=FALSE)
temps$tmax <- na.approx(temps$tmax, na.rm=FALSE)
temps$tmin <- ifelse(2*temps$tavg-temps$tmax <= temps$tmax, 2*temps$tavg-temps$tmax, temps$tmin)
df <- temps %>% group_by(month, day) %>% summarise(diff = mean(tmax - tmin, na.rm=TRUE),.groups = 'keep')

temps <- left_join(temps, df, by = c("month", "day"))
temps$tmin <- ifelse(is.na(temps$tmin) & 2*temps$tavg-temps$tmax > temps$tmax, temps$tmax-temps$diff, temps$tmin)

write.csv(temps,"data\\kyoto_data.csv", row.names = FALSE)

# Reference: https://www.tutorialspoint.com/how-to-replace-missing-values-with-linear-interpolation-method-in-an-r-vector