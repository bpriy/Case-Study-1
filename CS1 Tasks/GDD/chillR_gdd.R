##########################################
## Load Libraries      
##########################################
library(rnoaa)
library(chillR)
library(tidyverse)
library(stringr)
library(lubridate)


# DATA FOR WASHINGTON, DC (HOURLY)
##########################
c <- c('character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character', 'character' )

dc_hourly <- read.csv("data/dc_hourly_36_42.csv", colClasses=c(c)) %>% 
  bind_rows(read.csv("data/dc_hourly_43_52.csv", colClasses=c(c))) %>% 
  bind_rows(read.csv("data/dc_hourly_53_62.csv", colClasses=c(c))) %>%  
  bind_rows(read.csv("data/dc_hourly_63_72.csv", colClasses=c(c))) %>%  
  bind_rows(read.csv("data/dc_hourly_73_82.csv", colClasses=c(c))) %>% 
  bind_rows(read.csv("data/dc_hourly_83_92.csv", colClasses=c(c))) %>% 
  bind_rows(read.csv("data/dc_hourly_93_02.csv", colClasses=c(c))) %>%
  bind_rows(read.csv("data/dc_hourly_03_12.csv", colClasses=c(c))) %>%
  bind_rows(read.csv("data/dc_hourly_13_22.csv", colClasses=c(c))) 

colnames(dc_hourly)

date <- gsub("T", " ", dc_hourly$DATE)
dc_hourly$DATE <- as.POSIXct(strptime(date, format = "%Y-%m-%d %H:%M:%S"))
dc_hourly$HourlyDryBulbTemperature <- as.numeric(dc_hourly$HourlyDryBulbTemperature)

dc_hourly[81600:81650,c(2,44)]

# GDD
gdd_temps <- GDD(dc_hourly[dc_hourly$DATE >= "1946-11-01 00:00:00" & 
                             dc_hourly$DATE <= "1947-04-14 23:00:00", 44], summ=TRUE, Tbase = 4)


GDD.calc <- function(data, basetemp, y1, m1, d1, y2, m2, d2, n){
  gdd <- list(NULL)
  for(p in 1:n) {
  gdd[[p]] <- GDD(data[data$DATE >= paste(y1, "-", m1, "-", d1, " 00:00:00", sep="") & 
               data$DATE <= paste(y2, "-", m2, "-", d2, " 23:00:00", sep=""), 44], 
        summ=TRUE, Tbase = basetemp)
  }
  gdd
}

sample_gdd_cal <- GDD.calc(dc_hourly, 4, 1946, 11, 01, 1947, 04, 15, 3)

