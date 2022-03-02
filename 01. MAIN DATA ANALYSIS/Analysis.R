####            Cherry Blossom Peak Bloom Prediction Project              #####
################################################################################
################################################################################

# Jonathan Abbamonte
# Priyanjali Bukke

# STAT 634
# Case Study 1

# Mar. 1, 2022

################################################################################

##########################################
## Load Libraries      
##########################################
library(rnoaa)
library(chillR)
library(nlme)
library(doBy)
library(forecast)
library(stringr)
library(xlsx)
library(openxlsx)
library(lubridate)
library(climate)
library(measurements)
library(FactoMineR)
library(leaps)
library(car)
library(pls)
library(astsa)
library(mgcv)
library(itsadug)
library(tidyverse)


select <- dplyr::select
##########################################
## Import Response Variable      
##########################################
cherry_bloom_data <- read.csv("data/washingtondc.csv") %>% 
  bind_rows(read.csv("data/liestal.csv")) %>% 
  bind_rows(read.csv("data/kyoto.csv"))

write.csv(cherry_bloom_data, "cherryrawdata.csv", row.names=FALSE)
head(cherry_bloom_data)

summaryBy(year ~ location, data=cherry_bloom_data, 
          FUN = function(x) range(x))

# subset dataset to data since 1900
cherry <- cherry_bloom_data %>% 
  filter(year >= 1900)

# create separate dataframes for each location
cherry.ja <- cherry[cherry$location=="kyoto",]
cherry.dc <- cherry[cherry$location=="washingtondc",]
cherry.sw <- cherry[cherry$location=="liestal",]

# create vector for years
ja.yr <- data.frame(year = c(seq(from=1900, to=2021, by=1)) )
dc.yr <- data.frame(year = c(seq(from=1921, to=2021, by=1)) )

cherry.ja <- merge(cherry.ja, ja.yr, by="year", all=TRUE)
cherry.sw <- merge(cherry.sw, ja.yr, by="year", all=TRUE)
cherry.dc <- merge(cherry.dc, dc.yr, by="year", all=TRUE)

bd.ja1 <- cherry.ja$bloom_doy
bd.dc1 <- cherry.dc$bloom_doy
bd.sw1 <- cherry.sw$bloom_doy
blooms <- data.frame(year=ja.yr, kyoto_doy=bd.ja1, liestal_doy=bd.sw1,  
                     kyoto=cherry.ja$bloom_date, liestal=cherry.sw$bloom_date)
blooms <- merge(blooms, cherry.dc, by="year", all=T) 
blooms %>% 
  select(year, kyoto_doy, liestal_doy, bloom_doy, kyoto, liestal, bloom_date) %>% 
  rename(DC_doy = bloom_doy, DC = bloom_date)

# create time series for bloom_doy
bloom_doy_ja <- ts(cherry.ja$bloom_doy, start=1900, end=2021, frequency=1)
bloom_doy_dc <- ts(cherry.dc$bloom_doy, start=1921, end=2021, frequency=1)
bloom_doy_sw <- ts(cherry.sw$bloom_doy, start=1900, end=2021, frequency=1)

# Weight Responses
## baseline peak bloom date (PBD) is when 70% of blossoms are open
pbd.ja <- data.frame(year = cherry.ja$year, bloom.date = cherry.ja$bloom_date, bloom_doy = cherry.ja$bloom_doy, 
                     B_doy.adj = c(cherry.ja$bloom_doy + (1-(0.8*(1/0.7)))*7), Adj.bloom.date = as.Date(cherry.ja$bloom_date) - 1 )
pbd.sw <- data.frame(year = cherry.sw$year, bloom.date = cherry.sw$bloom_date, bloom_doy = cherry.sw$bloom_doy, 
                     B_doy.adj = c(cherry.sw$bloom_doy + (1-(0.25*(1/0.7)))*7 - 0.5), Adj.bloom.date = as.Date(cherry.ja$bloom_date) + 4 )
pbd.dc <- data.frame(year = cherry.dc$year, bloom.date = cherry.dc$bloom_date, bloom_doy = cherry.dc$bloom_doy, 
                     B_doy.adj = c(cherry.dc$bloom_doy + (1-(0.7*(1/0.7)))*7), Adj.bloom.date =cherry.dc$bloom_date)

7*(1-(0.8*(1/0.7)))
7*(1-(0.25*(1/0.7)))

cherry.ja$PBD <- pbd.ja[,4]
cherry.dc$PBD <- pbd.dc[,3]
cherry.sw$PBD <- pbd.sw[,4]

cherry.ja$Adj.bloom.date <- pbd.ja[,"Adj.bloom.date"]
cherry.dc$Adj.bloom.date <- cherry.dc$bloom_date
cherry.sw$Adj.bloom.date <- pbd.sw[,"Adj.bloom.date"]

bd.ja <- cherry.ja$PBD
bd.dc <- cherry.dc$PBD
bd.sw <- cherry.sw$PBD


# additional data
add_data_bloom <- read.csv("data/japan.csv") %>% # Japan
  bind_rows(read.csv("data/meteoswiss.csv")) %>% # Switzerland
  bind_rows(read.csv("data/south_korea.csv"))    # South Korea

summaryBy(year ~ location, data=add_data_bloom, 
          FUN = function(x) range(x))

add_data_bloom$country <- str_split_fixed(add_data_bloom$location, "/", 2)[,1]
add_data_bloom$city <- str_split_fixed(add_data_bloom$location, "/", 2)[,2]



##########################################
## Import Covariates      
##########################################

# Plant Hardiness Zones
hardiness_zones <- read.csv("data/plant_hardiness_zones_japan.csv") %>% 
  bind_rows(read.csv("data/plant_hardiness_zones_south_korea.csv")) %>% 
  bind_rows(read.csv("data/plant_hardiness_zones_switzerland.csv")) 
hardiness_zones <- rename(hardiness_zones, city = Location)

bloom.add <- merge(add_data_bloom, hardiness_zones, by="city", all=TRUE, sort=FALSE)

sum(!complete.cases(bloom.add$city))
sum(!complete.cases(bloom.add$lat))
nrow(bloom.add)
nrow(add_data_bloom)

hardiness_zones2 <- read.csv("data/plant_hardiness_zones.csv", header=T)
cherry.ja$hardiness.zone <- hardiness_zones2[hardiness_zones2$location=="kyoto",2]
cherry.dc$hardiness.zone <- hardiness_zones2[hardiness_zones2$location=="washingtondc",2]
cherry.sw$hardiness.zone <- hardiness_zones2[hardiness_zones2$location=="liestal",2]


# Hours of Sunlight
sun_dc <- read.csv("data/sunlight_dc.csv") %>% 
  mutate(city = "washingtondc") %>%
  rename(sunlight_duration = Sunlight.Duration..minutes.) %>% 
  separate(col=Date, into=c("month", "day", "year"), remove=FALSE)
sun_dc$year <- as.integer(sun_dc$year)
sun_dc$date <- as.Date(paste(sun_dc$year, sun_dc$month, sun_dc$day, sep="-"), format="%Y-%m-%d")
  
sun_ja <- read.csv("data/sunlight_ja.csv") %>% 
  mutate(city = "kyoto") %>% 
  rename(sunlight_duration = Sunlight.Duration..minutes.) %>% 
  separate(col=Date, into=c("month", "day", "year"), remove=FALSE)
sun_ja$year <- as.integer(sun_ja$year)
sun_ja$date <- as.Date(paste(sun_ja$year, sun_ja$month, sun_ja$day, sep="-"), format="%Y-%m-%d")
  
sun_sw <- read.csv("data/sunlight_sw.csv") %>% 
  mutate(city = "liestal") %>% 
  rename(sunlight_duration = Sunlight.Duration..minutes.) %>% 
  separate(col=Date, into=c("month", "day", "year"), remove=FALSE)
sun_sw$year <- as.integer(sun_sw$year)
sun_sw$date <- as.Date(paste(sun_sw$year, sun_sw$month, sun_sw$day, sep="-"), format="%Y-%m-%d")

sun_bc <- read.csv("data/sunlight_bc.csv") %>% 
  mutate(city = "vancouver")%>% 
  rename(sunlight_duration = Sunlight.Duration..minutes.) %>% 
  separate(col=Date, into=c("month", "day", "year"), remove=FALSE)
sun_bc$year <- as.integer(sun_bc$year)
sun_bc$date <- as.Date(paste(sun_bc$year, sun_bc$month, sun_bc$day, sep="-"), format="%Y-%m-%d")


# Sunlight hours: Jan-PBD
# number of sunlight hours to doy - Kyoto
sunhr.ja <- NULL
sunlist.ja <- list(NULL)
yr <- seq(from=1900, to=2021, by=1)
for(k in 1:122) {
  sunlist.ja[[k]] <- sun_ja %>% 
    filter(year == yr[k]) %>% 
    select(sunlight_duration) 

   ifelse(is.na(sunlist.ja[[k]][bd.ja[k],]), 
          sunhr.ja[k] <- NA, 
          {sunhr.ja[k] <- sum(sunlist.ja[[k]][1:bd.ja[k],])
          sunhr.ja[k] <- sunhr.ja[k] / (60 * bd.ja[k]) } )
}
cherry.ja$sun <- sunhr.ja


# number of sunlight hours to doy - DC
sunhr.dc <- NULL
sunlist.dc <- list(NULL)
yr1 <- seq(from=1921, to=2021, by=1)
for(k in 1:length(yr1)) {
  sunlist.dc[[k]] <- sun_dc %>% 
    filter(year == yr1[k]) %>% 
    select(sunlight_duration) 
  
  ifelse(is.na(sunlist.dc[[k]][bd.dc[k],]), 
         sunhr.dc[k] <- NA, 
         {sunhr.dc[k] <- sum(sunlist.dc[[k]][1:bd.dc[k],])
         sunhr.dc[k] <- sunhr.dc[k] / (60 * bd.dc[k]) } )
}
cherry.dc$sun <- sunhr.dc


# number of sunlight hours to doy - Liestal
sunhr.sw <- NULL
sunlist.sw <- list(NULL)
for(k in 1:122) {
  sunlist.sw[[k]] <- sun_sw %>% 
    filter(year == yr[k]) %>% 
    select(sunlight_duration) 
  
  ifelse(is.na(sunlist.sw[[k]][bd.sw[k],]), 
         sunhr.sw[k] <- NA, 
         {sunhr.sw[k] <- sum(sunlist.sw[[k]][1:bd.sw[k],])
          sunhr.sw[k] <- sunhr.sw[k] / (60 * bd.sw[k]) } )
  
}
cherry.sw$sun <- sunhr.sw

# Visualizations of Number of Hours of Sunlight vs. Time
library(ggplot2)
library(dplyr)
ggplot(cherry.dc, aes(x=year, y=sun)) + geom_line() + geom_point() +
  xlab("Year") + ylab("Average Number of Sunlight Hours from January 1 to PBD") + 
  theme_bw()
# reference: https://felixfan.github.io/ggplot2-remove-grid-background-margin/

ggplot(cherry.sw, aes(x=year, y=sun)) + geom_line() + geom_point() +
  xlab("Year") + ylab("Average Number of Sunlight Hours from January 1 to PBD") + 
  theme_bw()
# reference: https://felixfan.github.io/ggplot2-remove-grid-background-margin/

ggplot(cherry.ja, aes(x=year, y=sun)) + geom_line() + geom_point() +
  xlab("Year") + ylab("Average Number of Sunlight Hours from January 1 to PBD") + 
  theme_bw()
# reference: https://felixfan.github.io/ggplot2-remove-grid-background-margin/


# Sunlight hours: May-Sep
# Kyoto
sunhr.ja <- NA
sunyear_start <- seq(from=as.Date("1900-05-01"), to=as.Date("2020-05-01"), by="year")
sunyear_end <- seq(from=as.Date("1900-09-30"), to=as.Date("2020-09-30"), by="year")
for(j in 1:length(sunyear_start)) {
  sunlist.ja <- sun_ja %>% 
    filter(date >= sunyear_start[j] & date <= sunyear_end[j]) %>% 
    select(sunlight_duration) 
  
  sunhr.ja[j+1] <- sum(sunlist.ja) 
}
cherry.ja$sun.maysep <- sunhr.ja
cherry.ja$sun.maysep <- cherry.ja$sun.maysep / 153 / 60

# DC
sunhr.dc <- NULL
sunyear_start <- seq(from=as.Date("1920-05-01"), to=as.Date("2020-05-01"), by="year")
sunyear_end <- seq(from=as.Date("1920-09-30"), to=as.Date("2020-09-30"), by="year")
for(j in 1:length(sunyear_start)) {
  sunlist.dc <- sun_dc %>% 
    filter(date >= sunyear_start[j] & date <= sunyear_end[j]) %>% 
    select(sunlight_duration) 
  
  sunhr.dc[j] <- sum(sunlist.dc) 
}
cherry.dc$sun.maysep <- sunhr.dc
cherry.dc$sun.maysep <- cherry.dc$sun.maysep / 153 / 60

# Liestal
sunhr.sw <- NA
sunyear_start <- seq(from=as.Date("1900-05-01"), to=as.Date("2020-05-01"), by="year")
sunyear_end <- seq(from=as.Date("1900-09-30"), to=as.Date("2020-09-30"), by="year")
for(j in 1:length(sunyear_start)) {
  sunlist.sw <- sun_sw %>% 
    filter(date >= sunyear_start[j] & date <= sunyear_end[j]) %>% 
    select(sunlight_duration) 
  
  sunhr.sw[j+1] <- sum(sunlist.sw) 
}
cherry.sw$sun.maysep <- sunhr.sw
cherry.sw$sun.maysep <- cherry.sw$sun.maysep / 153 / 60



# DATA FOR KYOTO (MONTHLY)
###########################
sn <- getSheetNames("data/Kyoto_Data.xlsx")
list1 <- list(NULL)
for(k in 1:length(sn)) {
  list1[[k]] <- read.xlsx2("data/Kyoto_Data.xlsx", sheetName = sn[k])
}

for(k in 1:14) {
  for(i in 1:nrow(list1[[k]])) for(j in 1:14)  {
  ifelse(str_detect(list1[[k]][i,j], "]"), list1[[k]][i,j] <- "", list1[[k]][i,j] <- list1[[k]][i,j])
  ifelse(str_detect(list1[[k]][i,j], "#"), list1[[k]][i,j] <- "", list1[[k]][i,j] <- list1[[k]][i,j])
  ifelse(str_detect(list1[[k]][i,j], "×"), list1[[k]][i,j] <- "", list1[[k]][i,j] <- list1[[k]][i,j])
  ifelse(str_detect(list1[[k]][i,j], fixed(")")), list1[[k]][i,j] <- gsub(" )", "", list1[[k]][i,j]), list1[[k]][i,j] <- list1[[k]][i,j] )
  ifelse(str_detect(list1[[k]][i,j], "--"), list1[[k]][i,j] <- "", list1[[k]][i,j] <- list1[[k]][i,j])
  }
}

# https://stackoverflow.com/questions/34832171/using-a-loop-to-create-multiple-data-frames-in-r
for(g in 1:length(sn)) {
  assign(paste("x", g, sep=""), list1[[g]])
}
sn

x1 <- gather(x1, "key", "value", -Year, factor_key = T) %>% group_by(Year, key) %>% summarize(mdtemp= value) %>% as.data.frame() 
x2 <- gather(x2, "key", "value", -Year, factor_key = T) %>% group_by(Year, key) %>% summarize(md.mintemp= value) %>% as.data.frame() 
x3 <- gather(x3, "key", "value", -Year, factor_key = T) %>% group_by(Year, key) %>% summarize(md.maxtemp= value) %>% as.data.frame() 
x4 <- gather(x4, "key", "value", -Year, factor_key = T) %>% group_by(Year, key) %>% summarize(mwind= value) %>% as.data.frame() 
x5 <- gather(x5, "key", "value", -Year, factor_key = T) %>% group_by(Year, key) %>% summarize(msealevel.ap= value) %>% as.data.frame() 
x6 <- gather(x6, "key", "value", -Year, factor_key = T) %>% group_by(Year, key) %>% summarize(stationlevel.ap= value) %>% as.data.frame() 
x7 <- gather(x7, "key", "value", -Year, factor_key = T) %>% group_by(Year, key) %>% summarize(mr.humidity= value) %>% as.data.frame() 
x8 <- gather(x8, "key", "value", -Year, factor_key = T) %>% group_by(Year, key) %>% summarize(m.cloud= value) %>% as.data.frame() 
x9 <- gather(x9, "key", "value", -Year, factor_key = T) %>% group_by(Year, key) %>% summarize(mpct.pos.sun= value) %>% as.data.frame() 
x10 <- gather(x10, "key", "value", -Year, factor_key = T) %>% group_by(Year, key) %>% summarize(t.sun.time= value) %>% as.data.frame() 
x11 <- gather(x11, "key", "value", -Year, factor_key = T) %>% group_by(Year, key) %>% summarize(t.precip= value) %>% as.data.frame() 
x12 <- gather(x12, "key", "value", -Year, factor_key = T) %>% group_by(Year, key) %>% summarize(t.snwdp = value) %>% as.data.frame() 
x13 <- gather(x13, "key", "value", -Year, factor_key = T) %>% group_by(Year, key) %>% summarize(m.solar.rad= value) %>% as.data.frame() 
x14 <- gather(x14, "key", "value", -Year, factor_key = T) %>% group_by(Year, key) %>% summarize(m.vp= value) %>% as.data.frame() 

x1[,3] <- as.numeric(as.character((x1[,3]))) ; x2[,3] <- as.numeric(as.character(x2[,3])) 
x3[,3] <- as.numeric(as.character(x3[,3])) ; x4[,3] <- as.numeric(as.character(x4[,3]))
x5[,3] <- as.numeric(as.character(x5[,3])) ; x6[,3] <- as.numeric(as.character(x6[,3]))
x7[,3] <- as.numeric(as.character(x7[,3])) ; x8[,3] <- as.numeric(as.character(x8[,3]))
x9[,3] <- as.numeric(as.character(x9[,3])) ; x10[,3] <- as.numeric(as.character(x10[,3]))
x11[,3] <- as.numeric(as.character(x11[,3])) ; x12[,3] <- as.numeric(as.character(x12[,3]))
x13[,3] <- as.numeric(as.character(x13[,3])) ; x14[,3] <- as.numeric(as.character(x14[,3]))

detach("package:crunch", unload=TRUE)
# technique borrowed from: https://www.musgraveanalytics.com/blog/2018/2/12/how-to-merge-multiple-data-frames-using-base-r
kyoto <- Reduce(function(x,y) merge(x = x, y = y, by = c("Year", "key"), all.x=TRUE), 
                list(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14)) 
kyoto <- kyoto[-(1:13),]
kyoto <- kyoto %>% 
  filter(Year >= 1900, key != "Annual") %>% 
  rename(Month = key)
kyoto$hardiness.zone <- hardiness_zones2[hardiness_zones2$location=="kyoto",2]


# DATA FOR KYOTO (DAILY)
########################
kyoto.day <- read.csv("data/kyoto_ghcn.csv", header=TRUE)

kyoto.ghcn <- read.csv("data/kyoto_ghcn.csv", header=TRUE) %>% 
  filter(NAME == "KYOTO, JA")
kyoto.ghcn$DATE <- as.Date(kyoto.ghcn$DATE)
kyoto.ghcn <- kyoto.ghcn %>% separate(col=DATE, into=c("year", "month", "day"), remove=FALSE)
kyoto.ghcn$year <- as.integer(kyoto.ghcn$year)
kyoto.ghcn$month <- as.integer(kyoto.ghcn$month)
kyoto.ghcn$day <- as.integer(kyoto.ghcn$day)


kyoto.gsod <- read.csv("data/kyoto_GSOD.csv", header=T)
kyoto.gsod$DATE <- as.Date(kyoto.gsod$DATE)
kyoto.gsod <- kyoto.gsod %>% separate(col=DATE, into=c("year", "month", "day"), remove=FALSE)
kyoto.gsod$year <- as.integer(kyoto.gsod$year)

summary(kyoto.gsod)
for (h in 1:nrow(kyoto.gsod)) {
  ifelse(kyoto.gsod[h,"DEWP"] == 9999.9, kyoto.gsod[h,"DEWP"] <- NA, kyoto.gsod[h,"DEWP"] <- kyoto.gsod[h,"DEWP"])
  ifelse(kyoto.gsod[h,"GUST"] == 999.9, kyoto.gsod[h,"GUST"] <- NA, kyoto.gsod[h,"GUST"] <- kyoto.gsod[h,"GUST"])
  ifelse(kyoto.gsod[h,"MAX"] == 9999.90, kyoto.gsod[h,"MAX"] <- NA, kyoto.gsod[h,"MAX"] <- kyoto.gsod[h,"MAX"])
  ifelse(kyoto.gsod[h,"MIN"] == 9999.90, kyoto.gsod[h,"MIN"] <- NA, kyoto.gsod[h,"MIN"] <- kyoto.gsod[h,"MIN"])
  ifelse(kyoto.gsod[h,"MXSPD"] == 999.9, kyoto.gsod[h,"MXSPD"] <- NA, kyoto.gsod[h,"MXSPD"] <- kyoto.gsod[h,"MXSPD"])
  ifelse(kyoto.gsod[h,"PRCP"] == 99.99, kyoto.gsod[h,"PRCP"] <- 0, kyoto.gsod[h,"PRCP"] <- kyoto.gsod[h,"PRCP"])
  ifelse(kyoto.gsod[h,"SLP"] == 9999.9, kyoto.gsod[h,"SLP"] <- NA, kyoto.gsod[h,"SLP"] <- kyoto.gsod[h,"SLP"])
  ifelse(kyoto.gsod[h,"SNDP"] == 999.9, kyoto.gsod[h,"SNDP"] <- 0, kyoto.gsod[h,"SNDP"] <- kyoto.gsod[h,"SNDP"])
  ifelse(kyoto.gsod[h,"STP"] == 9999.9, kyoto.gsod[h,"STP"] <- NA, kyoto.gsod[h,"STP"] <- kyoto.gsod[h,"STP"])
  ifelse(kyoto.gsod[h,"STP"] == 999.9, kyoto.gsod[h,"STP"] <- NA, kyoto.gsod[h,"STP"] <- kyoto.gsod[h,"STP"])
  ifelse(kyoto.gsod[h,"VISIB"] == 999.9, kyoto.gsod[h,"VISIB"] <- NA, kyoto.gsod[h,"VISIB"] <- kyoto.gsod[h,"VISIB"])
  ifelse(kyoto.gsod[h,"WDSP"] == 999.9, kyoto.gsod[h,"WDSP"] <- NA, kyoto.gsod[h,"WDSP"] <- kyoto.gsod[h,"WDSP"])
}



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

hr.dt.seq1 <- data.frame(DATE = seq(as.POSIXct("1936-09-01 00:00:00"), 
                             as.POSIXct("2004-12-31 23:00:00"), by="hour"))
hr.dt.seq2 <- data.frame(DATE = seq(as.POSIXct("2005-01-01 00:51:00"), 
                                    as.POSIXct("2005-12-31 23:51:00"), by="hour"))
hr.dt.seq3 <- data.frame(DATE = seq(as.POSIXct("2006-01-01 00:52:00"), 
                                    as.POSIXct("2022-02-20 23:52:00"), by="hour"))
ob1 <- dc_hourly %>% filter(DATE <= "2004-12-31 23:00:00")
ob2 <- dc_hourly %>% filter(DATE <= "2005-12-31 23:00:00" & DATE >= "2005-01-01 00:51:00")
ob3 <- dc_hourly %>% filter(DATE >= "2006-01-01 00:52:00")

dc_hourly1 <- merge(ob1, hr.dt.seq1, by="DATE", all=TRUE)
dc_hourly2 <- merge(ob2, hr.dt.seq2, by="DATE", all=TRUE)
dc_hourly3 <- merge(ob3, hr.dt.seq3, by="DATE", all=TRUE)

dc_hourly <- rbind(dc_hourly1, dc_hourly2)
dc_hourly <- rbind(dc_hourly, dc_hourly3)


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

GDH(stack_hourly_temps(dc_hourly$HourlyDryBulbTemperature[1:365], latitude = 38.8853), summ=TRUE)


# DATA FOR WASHINGTON, DC (DAILY)
##############################
# GHCN Daily
dc.ghcn <- read.csv("data/dc_ghcn_daily.csv", header=TRUE)
dc.ghcn$DATE <- as.Date(dc.ghcn$DATE)
dc.ghcn <- dc.ghcn %>% separate(col=DATE, into=c("year", "month", "day"), remove=FALSE)
dc.ghcn$year <- as.integer(dc.ghcn$year)
dc.ghcn$month <- as.integer(dc.ghcn$month)
dc.ghcn$day <- as.integer(dc.ghcn$day)

# GSOD
###################################################################################
dc.gsod <- read.csv("data/dc_GSOD.csv", header=T)
dc.gsod$DATE <- as.Date(dc.gsod$DATE, format="%m/%d/%Y")
dc.gsod <- dc.gsod %>% separate(col=DATE, into=c("year", "month", "day"), remove=FALSE)
dc.gsod$year <- as.integer(dc.gsod$year)
###################################################################################

summary(dc.gsod)
for (h in 1:nrow(dc.gsod)) {
  ifelse(dc.gsod[h,"DEWP"] == 9999.9, dc.gsod[h,"DEWP"] <- NA, dc.gsod[h,"DEWP"] <- dc.gsod[h,"DEWP"])
  ifelse(dc.gsod[h,"GUST"] == 999.9, dc.gsod[h,"GUST"] <- NA, dc.gsod[h,"GUST"] <- dc.gsod[h,"GUST"])
  ifelse(dc.gsod[h,"MAX"] == 9999.90, dc.gsod[h,"MAX"] <- NA, dc.gsod[h,"MAX"] <- dc.gsod[h,"MAX"])
  ifelse(dc.gsod[h,"MIN"] == 9999.90, dc.gsod[h,"MIN"] <- NA, dc.gsod[h,"MIN"] <- dc.gsod[h,"MIN"])
  ifelse(dc.gsod[h,"MXSPD"] == 999.9, dc.gsod[h,"MXSPD"] <- NA, dc.gsod[h,"MXSPD"] <- dc.gsod[h,"MXSPD"])
  ifelse(dc.gsod[h,"SLP"] == 9999.9, dc.gsod[h,"SLP"] <- NA, dc.gsod[h,"SLP"] <- dc.gsod[h,"SLP"])
  ifelse(dc.gsod[h,"SNDP"] == 999.9, dc.gsod[h,"SNDP"] <- 0, dc.gsod[h,"SNDP"] <- dc.gsod[h,"SNDP"])
  ifelse(dc.gsod[h,"STP"] == 9999.9, dc.gsod[h,"STP"] <- NA, dc.gsod[h,"STP"] <- dc.gsod[h,"STP"])
}


for (h in 11532:nrow(dc.gsod)) {
  ifelse(dc.gsod[h,"PRCP"] == 99.99, dc.gsod[h,"PRCP"] <- 0, dc.gsod[h,"PRCP"] <- dc.gsod[h,"PRCP"])
}


for (h in 1:11531) {
  dc.gsod[h,"PRCP"] <- NA
}



# DATA FOR Liestal (DAILY)
##############################
basel1 <- read.csv("data/sw, basel_ghcn.csv", header=TRUE)
basel2 <- read.csv("data/sw, basel_GSOD.csv", header=TRUE)

basel1$DATE <- as.Date(basel1$DATE)
basel2$DATE <- as.Date(basel2$DATE)

basel1 <- basel1 %>% separate(col=DATE, into=c("year", "month", "day"), remove=FALSE)
basel1$year <- as.integer(basel1$year)
basel1$month <- as.integer(basel1$month)
basel1$day <- as.integer(basel1$day)
basel2 <- basel2 %>% separate(col=DATE, into=c("year", "month", "day"), remove=FALSE)
basel2$year <- as.integer(basel2$year)

colnames(basel1)
basel1[,10] <- as.numeric(as.character(basel1[,10]))
basel1[,12] <- as.numeric(as.character(basel1[,12]))
basel1[,14] <- as.numeric(as.character(basel1[,14]))
basel1[,16] <- as.numeric(as.character(basel1[,16]))
basel1[,18] <- as.numeric(as.character(basel1[,18]))

summary(basel2)
for (h in 1:nrow(basel2)) {
  ifelse(basel2[h,"GUST"] == 999.9, basel2[h,"GUST"] <- NA, basel2[h,"GUST"] <- basel2[h,"GUST"])
  ifelse(basel2[h,"MAX"] == 9999.90, basel2[h,"MAX"] <- NA, basel2[h,"MAX"] <- basel2[h,"MAX"])
  ifelse(basel2[h,"MIN"] == 9999.90, basel2[h,"MIN"] <- NA, basel2[h,"MIN"] <- basel2[h,"MIN"])
  ifelse(basel2[h,"MXSPD"] == 999.9, basel2[h,"MXSPD"] <- NA, basel2[h,"MXSPD"] <- basel2[h,"MXSPD"])
  ifelse(basel2[h,"PRCP"] == 99.99, basel2[h,"PRCP"] <- NA, basel2[h,"PRCP"] <- basel2[h,"PRCP"])
  ifelse(basel2[h,"SNDP"] == 999.9, basel2[h,"SNDP"] <- 0, basel2[h,"SNDP"] <- basel2[h,"SNDP"])
  ifelse(basel2[h,"VISIB"] == 999.9, basel2[h,"VISIB"] <- NA, basel2[h,"VISIB"] <- basel2[h,"VISIB"])
  ifelse(basel2[h,"WDSP"] == 999.9, basel2[h,"WDSP"] <- NA, basel2[h,"WDSP"] <- basel2[h,"WDSP"])
  ifelse(basel2[h,"STP"] == 9999.9, basel2[h,"STP"] <- NA, basel2[h,"STP"] <- basel2[h,"STP"])
}

basel2$PRCP <- conv_unit(basel2$PRCP, from="inch", to="mm")
basel2$MAX <- conv_unit(basel2$MAX, from="F", to="C")
basel2$MIN <- conv_unit(basel2$MIN, from="F", to="C")
basel2$TEMP <- conv_unit(basel2$TEMP, from="F", to="C")

basel1$TMIN <- basel1$TMIN / 10
basel1$TMAX <- basel1$TMAX / 10
basel1$TAVG <- basel1$TAVG / 10

summaryBy(PRCP + SNWD ~ year, data=basel1, FUN = function(x) sum(is.na(x)))
summaryBy(PRCP ~ year, data=basel2, FUN = function(x) sum(is.na(x)))
summaryBy(TMIN + TMAX + TAVG ~ year, data=basel1, FUN = function(x) sum(is.na(x)))

for (g in 40990:nrow(basel1)) {
  ifelse(is.na(basel1$PRCP[g]), 
         basel1$PRCP[g] <- basel2[basel2$DATE == basel1$DATE[g], 22], 
         basel1$PRCP[g] <- basel1$PRCP[g] )
  ifelse(is.na(basel1$PRCP[g]), 
         basel1$PRCP[g] <- basel2[basel2$DATE == basel1$DATE[g], 22], 
         basel1$PRCP[g] <- basel1$PRCP[g] )
}

liestal.gsod <- basel2
liestal.ghcn <- basel1


# GSOD
# WDSP, SLP
#####################
summary(kyoto.gsod)
summary(dc.gsod)
summary(liestal.gsod)

library(zoo)

# Kyoto
yr <- seq(from=1900, to=2022, by=1)
gsod.ja <- data.frame(yr, c(rep(NA, 123)), c(rep(NA, 123)), c(rep(NA, 123)), c(rep(NA, 123)))
stor <- list(NULL)

for(k in 1:69) for(m in 3:5){
  stor[[k]] <- kyoto.gsod %>% 
    filter(year == yr[k+53]) %>% 
    select(year, WDSP, STP, SLP, DATE)
  yvec <- data.frame(DATE = seq.Date(from=as.Date(paste(yr[k+53],"01-01",sep="-")), to=as.Date(paste(yr[k+53],"12-31",sep="-")), by="day"))
  stor[[k]] <- merge(stor[[k]], yvec, by="DATE", all=T)
  n <- nrow(stor[[k]])
  
  ifelse(nrow(stor[[k]]) > 1,
    ifelse(sum(is.na(stor[[k]][1:bd.ja[k+53],m])) > 0.25*bd.ja[k+53],  
           gsod.ja[k+53,1] <- NA,
           ifelse(is.na(stor[[k]][n,m]) | is.na(stor[[k]][1,m]), 
                     {stor[[k]][n,m] <- mean(stor[[k]][354:364,m], na.rm=T)
                     stor[[k]][1,m] <- mean(stor[[k]][1:10,m], na.rm=T)
                     stor[[k]][,m] <- na.approx(stor[[k]][,m]) 
                     gsod.ja[k+53,m] <- sum(stor[[k]][1:bd.ja[k+53],m])}, 
                     {stor[[k]][,m] <- na.approx(stor[[k]][,m]) 
                      gsod.ja[k+53,m] <- sum(stor[[k]][1:bd.ja[k+53],m])})  ),
  {gsod.ja[k+53,3] <- NA
   gsod.ja[k+53,4] <- NA
   gsod.ja[k+53,5] <- NA})
}
cherry.ja$WDSP <- gsod.ja[1:122,3] / bd.ja
cherry.ja$SLP <- gsod.ja[1:122,5] / bd.ja /10


# Washington, DC
yr1 <- seq(from=1921, to=2021, by=1)
gsod.dc <- data.frame(yr1, c(rep(NA, 101)), c(rep(NA, 101)), c(rep(NA, 101)), c(rep(NA, 101)))
stor <- list(NULL)

for(k in 1:66) for(m in 3:5){
  stor[[k]] <- dc.gsod %>% 
    filter(year == yr1[k+35]) %>% 
    select(year, WDSP, STP, SLP, DATE)
  yvec <- data.frame(DATE = seq.Date(from=as.Date(paste(yr1[k+35],"01-01",sep="-")), to=as.Date(paste(yr1[k+35],"12-31",sep="-")), by="day"))
  stor[[k]] <- merge(stor[[k]], yvec, by="DATE", all=T)
  n <- nrow(stor[[k]])
  
  ifelse(nrow(stor[[k]]) > 1,
         ifelse(sum(is.na(stor[[k]][1:bd.dc[k+35],m])) > 0.25*bd.dc[k+35],  
                gsod.dc[k+35,m] <- NA,
                ifelse(is.na(stor[[k]][n,m]) | is.na(stor[[k]][1,m]), 
                          ifelse(sum(is.na(stor[[k]][335:365,m])) > 28,
                                 gsod.dc[k+35,m] <- NA,
                                 {stor[[k]][n,m] <- mean(stor[[k]][344:364,m], na.rm=T)
                                 stor[[k]][1,m] <- mean(stor[[k]][1:10,m], na.rm=T)
                                 stor[[k]][,m] <- na.approx(stor[[k]][,m]) 
                                 gsod.dc[k+35,m] <- sum(stor[[k]][1:bd.dc[k+35],m])}),
                           {stor[[k]][,m] <- na.approx(stor[[k]][,m]) 
                           gsod.dc[k+35,m] <- sum(stor[[k]][1:bd.dc[k+35],m])})  ),
         {gsod.dc[k+35,3] <- NA
          gsod.dc[k+35,4] <- NA
          gsod.dc[k+35,5] <- NA})
}
cherry.dc$WDSP <- gsod.dc[,3] / bd.dc
cherry.dc$SLP <- gsod.dc[,5] / bd.dc /10


# Liestal
yr <- seq(from=1900, to=2022, by=1)
gsod.sw <- data.frame(yr, c(rep(NA, 123)), c(rep(NA, 123)), c(rep(NA, 123)), c(rep(NA, 123)))
stor <- list(NULL)

for(k in 1:69) for(m in 3:5){
  stor[[k]] <- liestal.gsod %>% 
    filter(year == yr[k+53]) %>% 
    select(year, WDSP, STP, SLP, DATE)
  yvec <- data.frame(DATE = seq.Date(from=as.Date(paste(yr[k+53],"01-01",sep="-")), to=as.Date(paste(yr[k+53],"12-31",sep="-")), by="day"))
  stor[[k]] <- merge(stor[[k]], yvec, by="DATE", all=T)
  n <- nrow(stor[[k]])
  
  ifelse(nrow(stor[[k]]) > 1,
         ifelse(sum(is.na(stor[[k]][1:bd.sw[k+53],m])) > 0.25*bd.sw[k+53],  
                gsod.sw[k+53,m] <- NA,
                ifelse(is.na(stor[[k]][n,m] | is.na(stor[[k]][1,m])), 
                       {stor[[k]][n,m] <- mean(stor[[k]][344:364,m], na.rm=T)
                       stor[[k]][1,m] <- mean(stor[[k]][1:10,m], na.rm=T)
                        stor[[k]][,m] <- na.approx(stor[[k]][,m]) 
                        gsod.sw[k+53,m] <- sum(stor[[k]][1:bd.sw[k+53],m])}, 
                        {stor[[k]][,m] <- na.approx(stor[[k]][,m]) 
                          gsod.sw[k+53,m] <- sum(stor[[k]][1:bd.sw[k+53],m])})  ),
         {gsod.sw[k+53,3] <- NA
          gsod.sw[k+53,4] <- NA
          gsod.sw[k+53,5] <- NA})
}
cherry.sw$WDSP <- gsod.sw[1:122,3] / bd.sw
cherry.sw$SLP <- gsod.sw[1:122,5] / bd.sw / 10




# GHCN
# PRCP, SNWD, TMIN, TMAX, TAVG
# For DC also: PSUN
##########################################
summary(kyoto.ghcn)
summary(dc.ghcn)
summary(liestal.ghcn)


# Kyoto
yr <- seq(from=1900, to=2021, by=1)
ghcn.ja <- data.frame(yr, c(rep(NA, 122)), c(rep(NA, 122)), c(rep(NA, 122)),
                      c(rep(NA, 122)), c(rep(NA, 122)), c(rep(NA, 122)) )
stor <- list(NULL)

for(i in 1:72) for(m in 3:7) {
  stor[[i]] <- kyoto.ghcn %>% 
    filter(year == yr[i+50]) %>% 
    select(year, PRCP, SNWD, TMIN, TMAX, TAVG, DATE) 
  yvec <- data.frame(DATE = seq.Date(from=as.Date(paste(yr[i+50],"01-01",sep="-")), to=as.Date(paste(yr[i+50],"12-31",sep="-")), by="day"))
  stor[[i]] <- merge(stor[[i]], yvec, by="DATE", all=T)
  
  ifelse(nrow(stor[[i]]) > 1,
         ifelse(sum(is.na(stor[[i]][1:bd.ja[i+50],m])) > 0.25*bd.ja[i+50],  
                ghcn.ja[i+50,m] <- NA,
                ifelse(is.na(stor[[i]][nrow(stor[[i]]),m]) | is.na(stor[[i]][1,m]), 
                            {stor[[i]][nrow(stor[[i]]),m] <- mean(stor[[i]][354:364,m], na.rm=T)
                            stor[[i]][1,m] <- mean(stor[[i]][2:12,m], na.rm=T)
                            stor[[i]][,m] <- na.approx(stor[[i]][,m]) 
                            ghcn.ja[i+50,m] <- sum(stor[[i]][1:bd.ja[i+50],m])}, 
                            {stor[[i]][,m] <- na.approx(stor[[i]][,m]) 
                             ghcn.ja[i+50,m] <- sum(stor[[i]][1:bd.ja[i+50],m])})  ),
         {ghcn.ja[i+50,2] <- NA
           ghcn.ja[i+50,3] <- NA
           ghcn.ja[i+50,4] <- NA
           ghcn.ja[i+50,5] <- NA
           ghcn.ja[i+50,6] <- NA})}

cherry.ja$PRCP <- ghcn.ja[,3]
cherry.ja$SNWD <- ghcn.ja[,4]
cherry.ja$TMIN <- ghcn.ja[,5]
cherry.ja$TMAX <- ghcn.ja[,6]
cherry.ja$TAVG <- ghcn.ja[,7]


yr <- seq(from=1900, to=2021, by=1)
month.ja <- data.frame(yr, c(rep(NA, 122)), c(rep(NA, 122)), c(rep(NA, 122)),
                      c(rep(NA, 122)), c(rep(NA, 122)), c(rep(NA, 122)), c(rep(NA, 122)) )
stor <- list(NULL)
for(i in 1:122) for(m in 2:8) {
  stor[[i]] <- kyoto %>% 
    filter(Year == yr[i]) %>% 
    select(Year, mpct.pos.sun, msealevel.ap, t.precip, t.snwdp, mdtemp, md.mintemp, md.maxtemp) 
  
  ifelse(nrow(stor[[i]]) > 1,
         ifelse(sum(is.na(stor[[i]][1:bd.ja[i],m])) > 0.25*bd.ja[i],  
                month.ja[i,m] <- NA,
                ifelse(is.na(stor[[i]][nrow(stor[[i]]),m]) | is.na(stor[[i]][1,m]), 
                       {stor[[i]][nrow(stor[[i]]),m] <- mean(stor[[i]][354:364,m], na.rm=T)
                       stor[[i]][1,m] <- mean(stor[[i]][2:12,m], na.rm=T)
                       stor[[i]][,m] <- na.approx(stor[[i]][,m]) 
                       month.ja[i,m] <- sum(stor[[i]][1:bd.ja[i],m])}, 
                       {stor[[i]][,m] <- na.approx(stor[[i]][,m]) 
                       month.ja[i,m] <- sum(stor[[i]][1:bd.ja[i],m])})  ),
         {month.ja[i,2] <- NA
         month.ja[i,3] <- NA
         month.ja[i,4] <- NA
         month.ja[i,5] <- NA
         month.ja[i,6] <- NA
         month.ja[i,7] <- NA
         month.ja[i,8] <- NA})}



# Temp Diff
cherry.ja$TempDiffAbs <- abs(cherry.ja[,"TMAX"] - cherry.ja[,"TMIN"]) / 10
cherry.ja$TempDiffSq <- (cherry.ja[,"TMAX"] - cherry.ja[,"TMIN"])^2 / 1000
cherry.ja$Tavg <- (cherry.ja[,"TMAX"] + cherry.ja[,"TMIN"]) / 2





# Washington, DC
dc.ghcn$Tavg <- (dc.ghcn$TMAX + dc.ghcn$TMIN) / 2

yr1 <- seq(from=1921, to=2021, by=1)
ghcn.dc <- data.frame(yr1, c(rep(NA, 101)), c(rep(NA, 101)), c(rep(NA, 101)), c(rep(NA, 101)),
                      c(rep(NA, 101)), c(rep(NA, 101)), c(rep(NA, 101)), c(rep(NA, 101)) )
stor <- list(NULL)

for(i in 1:66) for(m in 3:9) {
  stor[[i]] <- dc.ghcn %>% 
    filter(year == yr1[i+35]) %>% 
    select(year, PRCP, SNWD, TMIN, TMAX, TAVG, PSUN, Tavg, DATE) 
  yvec <- data.frame(DATE = seq.Date(from=as.Date(paste(yr1[i+35],"01-01",sep="-")), to=as.Date(paste(yr1[i+35],"12-31",sep="-")), by="day"))
  stor[[i]] <- merge(stor[[i]], yvec, by="DATE", all=T)
  
  ifelse(nrow(stor[[i]]) > 1,
         ifelse(sum(is.na(stor[[i]][1:bd.dc[i+35],m])) > 0.25*bd.dc[i+35],  
                ghcn.dc[i+35,m] <- NA,
                ifelse(is.na(stor[[i]][nrow(stor[[i]]),m]) | is.na(stor[[i]][1,m]),
                       ifelse(sum(is.na(stor[[i]][335:365,m])) > 28,
                              ghcn.dc[i+35,m] <- NA,
                             {stor[[i]][nrow(stor[[i]]),m] <- mean(stor[[i]][354:364,m], na.rm=T)
                             stor[[i]][1,m] <- mean(stor[[i]][2:12,m], na.rm=T)
                             stor[[i]][,m] <- na.approx(stor[[i]][,m]) 
                             ghcn.dc[i+35,m] <- sum(stor[[i]][1:bd.dc[i+35],m])}), 
                       {stor[[i]][,m] <- na.approx(stor[[i]][,m]) 
                       ghcn.dc[i+35,m] <- sum(stor[[i]][1:bd.dc[i+35],m])})  ),
         {ghcn.dc[i+35,2] <- NA
         ghcn.dc[i+35,3] <- NA
         ghcn.dc[i+35,4] <- NA
         ghcn.dc[i+35,5] <- NA
         ghcn.dc[i+35,6] <- NA
         ghcn.dc[i+35,7] <- NA
         ghcn.dc[i+35,8] <- NA
         ghcn.dc[i+35,9] <- NA})}

cherry.dc$PRCP <- ghcn.dc[,3]
cherry.dc$SNWD <- ghcn.dc[,4]
cherry.dc$TMIN <- ghcn.dc[,5]
cherry.dc$TMAX <- ghcn.dc[,6]
cherry.dc$TAVG <- ghcn.dc[,7]
cherry.dc$PSUN <- ghcn.dc[,8] / bd.dc
cherry.dc$Tavg <- ghcn.dc[,9] / bd.dc


# Temp Diff
cherry.dc$TempDiffAbs <- abs(cherry.dc[,"TMAX"] - cherry.dc[,"TMIN"]) / 10
cherry.dc$TempDiffSq <- (cherry.dc[,"TMAX"] - cherry.dc[,"TMIN"])^2 / 1000
cherry.dc$Tavg <- (cherry.dc[,"TMAX"] + cherry.dc[,"TMIN"]) / 2




# Liestal
yr <- seq(from=1900, to=2021, by=1)
ghcn.sw <- data.frame(yr, c(rep(NA, 122)), c(rep(NA, 122)), c(rep(NA, 122)),
                      c(rep(NA, 122)), c(rep(NA, 122)) )
stor <- list(NULL)

for(i in 1:122) for(m in 2:6) {
  stor[[i]] <- liestal.ghcn %>% 
    filter(year == yr[i]) %>% 
    select(year, PRCP, SNWD, TMIN, TMAX, TAVG) 
  
  ifelse(nrow(stor[[i]]) > 1,
         ifelse(sum(is.na(stor[[i]][1:bd.sw[i],m])) > 0.25*bd.sw[i],  
                ghcn.sw[i,m] <- NA,
                ifelse(is.na(stor[[i]][nrow(stor[[i]]),m]) | is.na(stor[[i]][1,m]), 
                       {stor[[i]][nrow(stor[[i]]),m] <- mean(stor[[i]][354:364,m], na.rm=T)
                       stor[[i]][1,m] <- mean(stor[[i]][2:12,m], na.rm=T)
                       stor[[i]][,m] <- na.approx(stor[[i]][,m]) 
                       ghcn.sw[i,m] <- sum(stor[[i]][1:bd.sw[i],m])}, 
                       {stor[[i]][,m] <- na.approx(stor[[i]][,m]) 
                       ghcn.sw[i,m] <- sum(stor[[i]][1:bd.sw[i],m])})  ),
         {ghcn.sw[i,2] <- NA
         ghcn.sw[i,3] <- NA
         ghcn.sw[i,4] <- NA
         ghcn.sw[i,5] <- NA
         ghcn.sw[i,6] <- NA})}

cherry.sw$PRCP <- ghcn.sw[,2]
cherry.sw$SNWD <- ghcn.sw[,3]
cherry.sw$TMIN <- ghcn.sw[,4]
cherry.sw$TMAX <- ghcn.sw[,5]
cherry.sw$TAVG <- ghcn.sw[,6]


# Temp Diff
cherry.sw$TempDiffAbs <- abs(cherry.sw[,"TMAX"] - cherry.sw[,"TMIN"]) / 10
cherry.sw$TempDiffSq <- (cherry.sw[,"TMAX"] - cherry.sw[,"TMIN"])^2 / 1000
cherry.sw$Tavg <- (cherry.sw[,"TMAX"] + cherry.sw[,"TMIN"]) / 2



##################################################################################
##################################################################################
# Growing Degree Days (GDD)
##################################################################################
##################################################################################

library(tidyverse)
library(rnoaa)
library(purrr)
library(dplyr)
library(Metrics)


# WASHINGTON, DC
##################################################################################
##################################################################################
# public data on pbd from 1921-2021
cherry <- read.csv("data/washingtondc.csv") 
publicpbd <- cherry %>% subset(select = c(year, bloom_doy))
publicpbd

# set-up temperatures' data set to work with: part 1 - tmin and tmax
temps <- ghcnd_search(stationid = "USW00013743", var=c("tmin", "tmax"), ) 
temps <- temps %>% reduce(full_join, by='date') %>% subset(select = c(date, tmin, tmax))
temps <- temps %>% mutate(year = as.integer(format(date, "%Y")),
                          month = as.integer(strftime(date, '%m')) %% 12, # make December "0"
                          season = cut(month, breaks = c(0, 2, 5, 8, 11),
                                       include.lowest = TRUE,
                                       labels = c("Winter", "Spring", "Summer", "Fall")),
                          year = if_else(month >= 10 | month == 0, year + 1L, year))

# check on tmin and tmax missing values: (code reference - https://stackoverflow.com/questions/53195961/count-total-missing-values-by-group)
nas1 <- temps %>%  group_by(year) %>% summarise_each(funs(sum(is.na(.))))

# set-up temperatures' data set to work with: part 2 - tavg
tavg <- ghcnd_search(stationid = "USW00013743", var=c("tavg"))
data <- left_join(temps, tavg$tavg, by ="date") 
data <- data %>% subset(select = c(date, tmin, tmax, tavg, year, month, season))

# convert temp data units to degrees celcius
data$tmin <- data$tmin/10
data$tmax <- data$tmax/10
data$tavg <- data$tavg/10

summary(data)

# replace daily missing tavg values by mean of corresponding tmin,tmax
data$tavg <- ifelse(!is.na(data$tmin) & !is.na(data$tmax), data$tmin+data$tmax/2 , data$tavg)
summary(data)

# NA values: (code reference - https://stackoverflow.com/questions/53195961/count-total-missing-values-by-group)
nas2 <- data %>%  group_by(year) %>% summarise_each(funs(sum(is.na(.))))

# subset and split data by year
# data <- subset(data, year >= 1945 & year <= 2021)

datal = split(data, data$year)

#################

# specify years
totyears = 1945:2021
y = length(totyears)

# The Basic Two-Step Phenology Model (With October 1st As Dormancy Initiation Date and Parameters: TC = 5, RC = -140.1, Rh = 264)
TC = 5 # threshold temperature below which chill days (Cd) accumulate
Rc = -140.1 # chill requirement
Rh = 264 # heat requirement
ppd = 0 # predicted peak bloom date 
year = 0 
chill_days = 0
heat_days = 0
for (i in 1:y){ # for each year from 1945 to 2021
  #### Dormancy Period
  # calculate daily "chill day" values starting from october 1st
  Cd <- with(datal[[i]], ifelse(0 <= TC & TC <= tmin & tmin <= tmax, 0, 
                                ifelse(0 <= tmin & tmin <= TC & TC < tmax,((tavg - tmin)-((tmax - TC)/2)),
                                       ifelse(0 <= tmin & tmin <= tmax & tmax <= TC, (tavg - tmin),
                                              ifelse(tmin < 0 & 0 < tmax & tmax <= TC, ((tmax)/(tmax-tmin))*(tmax/2),
                                                     ifelse(tmin < 0 & 0 < TC & TC < tmax, (((tmax)/(tmax-tmin))*(tmax/2)-((tmax-TC)/2)), 0))))))
  Cd <- with(datal[[i]], ifelse(Cd < 0, Cd, Cd*(-1))) # chill day value should be negative (if it is not zero)
  tab <- cbind(Cd,datal[[i]])
  tab[,"cm_cd"] <- cumsum(tab$Cd) # calculated "chill days accumulated so far" for each day
  chill <- min(which(tab$cm_cd <= Rc))  # number of days until dormancy released = number of days to first reach at least the Rc value (since October 1st)
  if (!is.finite(chill)) { 
    ppd[i] = "NA"
    year[i] = datal[[i]][1,5]
    next
  }
  hs = chill + 1
  if (hs > length(tab$Cd)) {
    ppd[i] = "NA"
    year[i] = datal[[i]][1,5]
    next
  }  
  tab$Cd[hs:length(tab$Cd)] <- 0 # once dormancy released, no more chill days accrued
  tab$cm_cd[hs:length(tab$cm_cd)] <- cumsum(tab$Cd) # update cumulative chill days column to align with the line above
  #### Floral Development Period
  # calculate daily "heat day" (or anti-chill) values starting from day after chill requirement was met
  tab[,"Ca"] <- with(datal[[i]], ifelse(0 <= TC & TC <= tmin & tmin <= tmax, tavg-TC, 
                                        ifelse(0 <= tmin & tmin <= TC & TC < tmax,(tmax - TC)/2,
                                               ifelse(0 <= tmin & tmin <= tmax & tmax <= TC, 0,
                                                      ifelse(tmin < 0 & 0 < tmax & tmax <= TC, 0,
                                                             ifelse(tmin < 0 & 0 < TC & TC < tmax, (tmax - TC)/2, 0))))))
  tab$Ca <- with(tab, ifelse(Ca > 0, Ca, Ca*(-1))) # heat day value should be positive
  tab$Ca[1:chill] <- 0 # before dormancy released, no heat days accrued
  tab[,"cm_ca"] <- cumsum(tab$Ca) # calculated "chill days accumulated so far" for each day
  
  totdays <- min(which(tab$cm_ca >= Rh)) # total number of days to reach peak bloom (since October 1st)
  if (!is.finite(totdays)) {
    ppd[i] = "NA"
    year[i] = datal[[i]][1,5]
    next
  }
  heat <- totdays - chill
  
  ppd[i] = totdays - 92 # convert the totdays value to "day of year" (starting from January 1st) & store the "ppd" value obtained in each for-loop iteration (year)
  year[i] = datal[[i]][1,5] # helps keep track of the year that corresponds to each ppd value
  chill_days[i] <- chill
  heat_days[i] <- heat
}
year <- unlist(year) 
predpbd <- data.frame(year,ppd, heat_days, chill_days) # store ppd and corresponding year values in a table
predpbd <- subset(predpbd, ppd!="NA") # reference: https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
predpbd$ppd <- as.numeric(predpbd$ppd) 
restab <- left_join(predpbd, publicpbd, by ="year") # create a table with the year, predicted pbd, and public pbd
rmse(restab$ppd, restab$bloom_doy) # calculate root mean square error between observed and predicted pbd

dc10_gddsummary <- data.frame(restab$year, restab$chill_days, restab$heat_days)
write.csv(dc10_gddsummary,"data\\dc10_gddsummary.csv", row.names = FALSE)

fit = lm(restab$ppd ~ restab$bloom_doy) 
summary(fit)

plot(restab$bloom_doy, restab$ppd, xlab = "Public PBD", ylab = "Predicted PBD", main = "Predicted vs. Observed PBD from 1946 to 2021")
lines(restab$bloom_doy, coef(fit)[1]+coef(fit)[2]*(restab$bloom_doy))

plot(restab$year, restab$ppd, type='l', col="blue", xlab = "Year", ylab = "Peak Bloom Dates (Day of Year)", main = "Predicted vs. Observed PBD from 1946 to 2021") 
lines(restab$year, restab$bloom_doy, col="black")
legend(x="topright", inset = c(-0.37,0), legend=c("Predicted", "Observed"), 
       col=c("blue", "black"), pch=c(20), xpd=TRUE)


yr1 <- data.frame(year = seq(from=1921, to=2021, by=1))
tab <- merge(restab, yr1, by="year", all=TRUE)
cherry.dc$ppd <- tab[,2]
cherry.dc$heat.days <- tab[,3]
cherry.dc$chill.days <- tab[,4]






# Liestal, Switzerland
##################################################################################
##################################################################################
# public data on pbd from 1921-2021
cherry <- read.csv("data/liestal.csv") 
publicpbd <- cherry %>% subset(select = c(year, bloom_doy))
publicpbd

# set-up temperatures' data set to work with: part 1 - tmin and tmax
temps <- ghcnd_search(stationid = "GME00127786", var=c("tmin", "tmax"), ) 
temps <- temps %>% reduce(full_join, by='date') %>% subset(select = c(date, tmin, tmax))
temps <- temps %>% mutate(year = as.integer(format(date, "%Y")),
                          month = as.integer(strftime(date, '%m')) %% 12, # make December "0"
                          season = cut(month, breaks = c(0, 2, 5, 8, 11),
                                       include.lowest = TRUE,
                                       labels = c("Winter", "Spring", "Summer", "Fall")),
                          year = if_else(month >= 10 | month == 0, year + 1L, year))

# check on tmin and tmax missing values: (code reference - https://stackoverflow.com/questions/53195961/count-total-missing-values-by-group)
nas1 <- temps %>%  group_by(year) %>% summarise_each(funs(sum(is.na(.))))

# set-up temperatures' data set to work with: part 2 - tavg
data <- temps %>% rowwise() %>% mutate(tavg = mean(c(tmax,tmin)))

# convert temp data units to degrees celcius
data$tmin <- data$tmin/10
data$tmax <- data$tmax/10
data$tavg <- data$tavg/10

summary(data)

# NA values: (code reference - https://stackoverflow.com/questions/53195961/count-total-missing-values-by-group)
nas2 <- data %>%  group_by(year) %>% summarise_each(funs(sum(is.na(.))))

# subset and split data by year
# data <- subset(data, year >= 1945 & year <= 2021)

datal = split(data, data$year)

# specify years
totyears = 1953:2022
y = length(totyears)


# The Basic Two-Step Phenology Model (With October 1st As Dormancy Initiation Date and Parameters)
TC = 5 # threshold temperature below which chill days (Cd) accumulate
Rc = -68 # chill requirement
Rh = 210.5 # heat requirement
ppd = 0 # predicted peak bloom date 
year = 0 
chill_days = 0
heat_days = 0
for (i in 1:y){ # for each year from 1953 to 2021
  #### Dormancy Period
  # calculate daily "chill day" values starting from october 1st
  Cd <- with(datal[[i]], ifelse(0 <= TC & TC <= tmin & tmin <= tmax, 0, 
                                ifelse(0 <= tmin & tmin <= TC & TC < tmax,((tavg - tmin)-((tmax - TC)/2)),
                                       ifelse(0 <= tmin & tmin <= tmax & tmax <= TC, (tavg - tmin),
                                              ifelse(tmin < 0 & 0 < tmax & tmax <= TC, ((tmax)/(tmax-tmin))*(tmax/2),
                                                     ifelse(tmin < 0 & 0 < TC & TC < tmax, (((tmax)/(tmax-tmin))*(tmax/2)-((tmax-TC)/2)), 0))))))
  Cd <- with(datal[[i]], ifelse(Cd < 0, Cd, Cd*(-1))) # chill day value should be negative (if it is not zero)
  tab <- cbind(Cd,datal[[i]])
  tab[,"cm_cd"] <- cumsum(tab$Cd) # calculated "chill days accumulated so far" for each day
  chill <- min(which(tab$cm_cd <= Rc))  # number of days until dormancy released = number of days to first reach at least the Rc value (since October 1st)
  if (!is.finite(chill)) { 
    ppd[i] = "NA"
    chill_days[i] <- "NA"
    heat_days[i] <- "NA"
    year[i] = datal[[i]][1,4]
    next
  }
  hs = chill + 1
  if (hs > length(tab$Cd)) {
    ppd[i] = "NA"
    chill_days[i] <- "NA"
    heat_days[i] <- "NA"
    year[i] = datal[[i]][1,4]
    next
  }  
  tab$Cd[hs:length(tab$Cd)] <- 0 # once dormancy released, no more chill days accrued
  tab$cm_cd[hs:length(tab$cm_cd)] <- cumsum(tab$Cd) # update cumulative chill days column to align with the line above
  #### Floral Development Period
  # calculate daily "heat day" (or anti-chill) values starting from day after chill requirement was met
  tab[,"Ca"] <- with(datal[[i]], ifelse(0 <= TC & TC <= tmin & tmin <= tmax, tavg-TC, 
                                        ifelse(0 <= tmin & tmin <= TC & TC < tmax,(tmax - TC)/2,
                                               ifelse(0 <= tmin & tmin <= tmax & tmax <= TC, 0,
                                                      ifelse(tmin < 0 & 0 < tmax & tmax <= TC, 0,
                                                             ifelse(tmin < 0 & 0 < TC & TC < tmax, (tmax - TC)/2, 0))))))
  tab$Ca <- with(tab, ifelse(Ca > 0, Ca, Ca*(-1))) # heat day value should be positive
  tab$Ca[1:chill] <- 0 # before dormancy released, no heat days accrued
  tab[,"cm_ca"] <- cumsum(tab$Ca) # calculated "chill days accumulated so far" for each day
  
  totdays <- min(which(tab$cm_ca >= Rh)) # total number of days to reach peak bloom (since October 1st)
  if (!is.finite(totdays)) {
    ppd[i] = "NA"
    chill_days[i] <- "NA"
    heat_days[i] <- "NA"
    year[i] = datal[[i]][1,4]
    next
  }
  heat <- totdays - chill
  
  ppd[i] = totdays - 92 # convert the totdays value to "day of year" (starting from January 1st) & store the "ppd" value obtained in each for-loop iteration (year)
  year[i] = datal[[i]][1,4] # helps keep track of the year that corresponds to each ppd value
  chill_days[i] <- chill
  heat_days[i] <- heat
}
year <- unlist(year) 
predpbd <- data.frame(year,ppd, chill_days, heat_days) # store ppd and corresponding year values in a table
predpbd <- subset(predpbd, ppd!="NA") # reference: https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
predpbd$ppd <- as.numeric(predpbd$ppd) 
restab <- left_join(predpbd, publicpbd, by ="year") # create a table with the year, predicted pbd, and public pbd
rmse(restab$ppd, restab$bloom_doy) # calculate root mean square error between observed and predicted pbd

lies10_gddsummary <- data.frame(restab$year, restab$chill_days, restab$heat_days)
write.csv(lies10_gddsummary,"data\\lies10_gddsummary.csv", row.names = FALSE)

fit = lm(restab$ppd ~ restab$bloom_doy) 
summary(fit)

plot(restab$bloom_doy, restab$ppd, xlab = "Public PBD", ylab = "Predicted PBD", main = "Predicted vs. Observed PBD from 1954 to 2021 (no 2015)")
lines(restab$bloom_doy, coef(fit)[1]+coef(fit)[2]*(restab$bloom_doy))

plot(restab$year, restab$ppd, type='l', col="blue", xlab = "Year", ylab = "Peak Bloom Dates (Day of Year)", main = "Predicted vs. Observed PBD from 1954 to 2021 (no 2015)") 
lines(restab$year, restab$bloom_doy, col="black")
legend(x="topright", inset = c(-0.37,0), legend=c("Predicted", "Observed"), 
       col=c("blue", "black"), pch=c(20), xpd=TRUE)




yr <- data.frame(year = seq(from=1900, to=2021, by=1))
tab <- merge(restab, yr, by="year", all=TRUE)
cherry.sw$ppd <- tab[,2]
cherry.sw$heat.days <- tab[,3]
cherry.sw$chill.days <- tab[,4]
cherry.sw$heat.days <- as.numeric(cherry.sw$heat.days)
cherry.sw$chill.days <- as.numeric(cherry.sw$chill.days)


# Kyoto, Japan
##################################################################################
##################################################################################
library(tidyverse)
library(rnoaa)
library(purrr)
library(dplyr)
library(Metrics)

# public data on pbd 
cherry <- read.csv("data/kyoto.csv") 
publicpbd <- cherry %>% subset(select = c(year, bloom_doy))
publicpbd

data <- read.csv("data/kydata.csv")
data$date <- as.Date(data$date)
data <- data %>% mutate(year = as.integer(format(date, "%Y")),
                        month = as.integer(strftime(date, '%m')) %% 12, # make December "0"
                        day = as.integer(strftime(date, '%d')),
                        season = cut(month, breaks = c(0, 2, 5, 8, 11),
                                     include.lowest = TRUE,
                                     labels = c("Winter", "Spring", "Summer", "Fall")),
                        year = if_else(month >= 10 | month == 0, year + 1L, year))
data <- data[order(data$date),]
# split data by year
datal = split(data, data$year)

#################

# specify years
totyears = 1951:2021
y = length(totyears)

# The Basic Two-Step Phenology Model (With October 1st As Dormancy Initiation Date and Parameters)
TC = 6 # threshold temperature below which chill days (Cd) accumulate
Rc = -125 # chill requirement
Rh = 235 # heat requirement
ppd = 0 # predicted peak bloom date 
year = 0 
chill_days = 0
heat_days = 0
for (i in 1:y){ # for each year from 1951 to 2021
  #### Dormancy Period
  # calculate daily "chill day" values starting from october 1st
  Cd <- with(datal[[i]], ifelse(0 <= TC & TC <= tmin & tmin <= tmax, 0, 
                                ifelse(0 <= tmin & tmin <= TC & TC < tmax,((tavg - tmin)-((tmax - TC)/2)),
                                       ifelse(0 <= tmin & tmin <= tmax & tmax <= TC, (tavg - tmin),
                                              ifelse(tmin < 0 & 0 < tmax & tmax <= TC, ((tmax)/(tmax-tmin))*(tmax/2),
                                                     ifelse(tmin < 0 & 0 < TC & TC < tmax, (((tmax)/(tmax-tmin))*(tmax/2)-((tmax-TC)/2)), 0))))))
  Cd <- with(datal[[i]], ifelse(Cd < 0, Cd, Cd*(-1))) # chill day value should be negative (if it is not zero)
  tab <- cbind(Cd,datal[[i]])
  tab[,"cm_cd"] <- cumsum(tab$Cd) # calculated "chill days accumulated so far" for each day
  chill <- min(which(tab$cm_cd <= Rc))  # number of days until dormancy released = number of days to first reach at least the Rc value (since October 1st)
  if (!is.finite(chill)) { 
    ppd[i] = "NA"
    chill_days[i] <- "NA"
    heat_days[i] <- "NA"
    year[i] = datal[[i]][1,5]
    next
  }
  hs = chill + 1
  if (hs > length(tab$Cd)) {
    ppd[i] = "NA"
    chill_days[i] <- "NA"
    heat_days[i] <- "NA"
    year[i] = datal[[i]][1,5]
    next
  }  
  tab$Cd[hs:length(tab$Cd)] <- 0 # once dormancy released, no more chill days accrued
  tab$cm_cd[hs:length(tab$cm_cd)] <- cumsum(tab$Cd) # update cumulative chill days column to align with the line above
  #### Floral Development Period
  # calculate daily "heat day" (or anti-chill) values starting from day after chill requirement was met
  tab[,"Ca"] <- with(datal[[i]], ifelse(0 <= TC & TC <= tmin & tmin <= tmax, tavg-TC, 
                                        ifelse(0 <= tmin & tmin <= TC & TC < tmax,(tmax - TC)/2,
                                               ifelse(0 <= tmin & tmin <= tmax & tmax <= TC, 0,
                                                      ifelse(tmin < 0 & 0 < tmax & tmax <= TC, 0,
                                                             ifelse(tmin < 0 & 0 < TC & TC < tmax, (tmax - TC)/2, 0))))))
  tab$Ca <- with(tab, ifelse(Ca > 0, Ca, Ca*(-1))) # heat day value should be positive
  tab$Ca[1:chill] <- 0 # before dormancy released, no heat days accrued
  tab[,"cm_ca"] <- cumsum(tab$Ca) # calculated "chill days accumulated so far" for each day
  
  totdays <- min(which(tab$cm_ca >= Rh)) # total number of days to reach peak bloom (since October 1st)
  if (!is.finite(totdays)) {
    ppd[i] = "NA"
    chill_days[i] <- "NA"
    heat_days[i] <- "NA"
    year[i] = datal[[i]][1,5]
    next
  }
  heat <- totdays - chill
  
  ppd[i] = totdays - 92 # convert the totdays value to "day of year" (starting from January 1st) & store the "ppd" value obtained in each for-loop iteration (year)
  year[i] = datal[[i]][1,5] # helps keep track of the year that corresponds to each ppd value
  chill_days[i] <- chill
  heat_days[i] <- heat
}
year <- unlist(year) 
predpbd <- data.frame(year,ppd, chill_days, heat_days) # store ppd and corresponding year values in a table
predpbd <- subset(predpbd, ppd!="NA") # reference: https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
predpbd$ppd <- as.numeric(predpbd$ppd) 
restab <- left_join(predpbd, publicpbd, by ="year") # create a table with the year, predicted pbd, and public pbd
rmse(restab$ppd, restab$bloom_doy) # calculate root mean square error between observed and predicted pbd

kyo10_gddsummary <- data.frame(restab$year, restab$chill_days, restab$heat_days)
write.csv(kyo10_gddsummary,"data\\kyo10_gddsummary.csv", row.names = FALSE)

fit = lm(restab$ppd ~ restab$bloom_doy) 
summary(fit)

plot(restab$bloom_doy, restab$ppd, xlab = "Public PBD", ylab = "Predicted PBD", main = "Predicted vs. Observed PBD from 1952 to 2021")
lines(restab$bloom_doy, coef(fit)[1]+coef(fit)[2]*(restab$bloom_doy))

plot(restab$year, restab$ppd, type='l', col="blue", xlab = "Year", ylab = "Peak Bloom Dates (Day of Year)", main = "Predicted vs. Observed PBD from 1952 to 2021") 
lines(restab$year, restab$bloom_doy, col="black")
legend(x="topright", inset = c(-0.37,0), legend=c("Predicted", "Observed"), 
       col=c("blue", "black"), pch=c(20), xpd=TRUE)






yr <- data.frame(year = seq(from=1900, to=2021, by=1))
tab <- merge(restab, yr, by="year", all=TRUE)
cherry.ja$ppd <- tab[,2]
cherry.ja$heat.days <- as.numeric(tab[,3])
cherry.ja$chill.days <- as.numeric(tab[,4])



# ALT Kyoto, Japan
##################################################################################
##################################################################################
yr <- seq(from=1900, to=2021, by=1)
stor <- list(NULL)

for(j in 1:71) {
  stor[[j]] <- kyoto.ghcn %>% 
    filter(year == yr[j+51]) %>% 
    select(year, TMIN, TMAX, TAVG, DATE)
  yvec <- data.frame(DATE = seq.Date(from=as.Date(paste(yr[j+51],"01-01",sep="-")), length.out=365, by="day"))
  stor[[j]] <- merge(stor[[j]], yvec, by="DATE", all=T)
  
  ifelse(is.na(stor[[j]][1,"TMIN"]), stor[[j]][1,"TMIN"] <- mean(stor[[j]][2:12,"TMIN"], na.rm=T), stor[[j]][1,"TMIN"]<-stor[[j]][1,"TMIN"] )
  
  ifelse(is.na(stor[[j]][365,"TMIN"]), stor[[j]][365,"TMIN"] <- mean(stor[[j]][354:364,"TMIN"], na.rm=T), stor[[j]][365,"TMIN"]<-stor[[j]][365,"TMIN"] )
  
  ifelse(is.na(stor[[j]][1,"TMAX"]), stor[[j]][1,"TMAX"] <- mean(stor[[j]][2:12,"TMAX"], na.rm=T), stor[[j]][1,"TMAX"]<-stor[[j]][1,"TMAX"] )
  
  ifelse(is.na(stor[[j]][365,"TMAX"]), stor[[j]][365,"TMAX"] <- mean(stor[[j]][354:364,"TMAX"], na.rm=T), stor[[j]][365,"TMAX"]<-stor[[j]][365,"TMAX"] )
  
  for(q in 1:nrow(stor[[j]])) {
  ifelse(stor[[j]][20,"year"] != 2005 & stor[[j]][20,"year"] >= 1990,
         stor[[j]][q,"TMIN"] <- 2*(stor[[j]][q,"TAVG"]) - stor[[j]][q,"TMAX"],
         stor[[j]][q,"TMIN"] <- stor[[j]][q,"TMIN"] )
  }
  
  ifelse(stor[[j]][20,"year"] != 2005,
         {stor[[j]][,"TMIN"] <- interpolate_gaps(stor[[j]][,"TMIN"])$interp
         stor[[j]][,"TMAX"] <- interpolate_gaps(stor[[j]][,"TMAX"])$interp},
         emptyvec <- TRUE )
  
  ifelse(stor[[j]][20,"year"] == 2005,
         {for(q in 1:nrow(stor[[j]])){
           stor[[j]][q,"TMIN"] <- weighted.mean(x=c(stor[[j-1]][q,"TMIN"], stor[[j-2]][q,"TMIN"], stor[[j-3]][q,"TMIN"], stor[[j-4]][q,"TMIN"], stor[[j-5]][q,"TMIN"]), w=c(bd.ja[j-1], bd.ja[j-2], bd.ja[j-3], bd.ja[j-4], bd.ja[j-5]))
         stor[[j]][q,"TMAX"] <- weighted.mean(x=c(stor[[j-1]][q,"TMAX"], stor[[j-2]][q,"TMAX"], stor[[j-3]][q,"TMAX"], stor[[j-4]][q,"TMAX"], stor[[j-5]][q,"TMAX"]), w=c(bd.ja[j-1], bd.ja[j-2], bd.ja[j-3], bd.ja[j-4], bd.ja[j-5]))}},
         emptyvec2 <- TRUE )
  
  stor[[j]] <- stor[[j]][1:bd.ja[j+51],]
}

year_file <- list(NULL)
hr_temp.ja <- list(NULL)
hr_t.ja <- NULL
for (k in 1:71) {
  year_file[[k]] <- stor[[k]][, c("DATE", "TMIN", "TMAX")] %>% 
    separate(col=DATE, into=c("Year", "Month", "Day"), remove=TRUE)
  year_file[[k]] <- as.data.frame(year_file[[k]])
  year_file[[k]] <- year_file[[k]][,c(4,5,1,2,3)]
  
  subset <- year_file[[k]][,3:5]
  subset2 <- make_JDay(subset)
  
  year_file[[k]] <- data.frame(year_file[[k]][,1:2], subset2)
}
names.file <- c("Tmin", "Tmax", "Year", "Month", "Day", "JDay")
year_file <- lapply(year_file, setNames, names.file)
for (k in 1:54) {
  hr_temp.ja[[k]] <- make_hourly_temps(latitude = 35.0120, year_file = year_file[[k]])
  hr_t.ja[k] <- hr_temp.ja[[k]][length(hr_temp.ja[[k]])]
}
for (k in 56:71) {
  hr_temp.ja[[k]] <- make_hourly_temps(latitude = 35.0120, year_file = year_file[[k]])
  hr_t.ja[k] <- hr_temp.ja[[k]][length(hr_temp.ja[[k]])]
}

gdd.ja <- data.frame(yr, c(rep(NA, 122))) 
for (h in 1:54) {
  gdd.ja[h+51,2] <- tail(GDD(hr_t.ja[[h]], summ=TRUE, Tbase=5), n=1)
}
for (h in 56:71) {
  gdd.ja[h+51,2] <- tail(GDD(hr_t.ja[[h]], summ=TRUE, Tbase=5), n=1)
}

cherry.ja$GDD <- gdd.ja[,2]




# ALT Washington, DC
##################################################################################
##################################################################################
yr <- seq(from=1921, to=2021, by=1)
stor <- list(NULL)

for(j in 1:76) {
  stor[[j]] <- dc.ghcn %>% 
    filter(year == yr[j+25]) %>% 
    select(year, TMIN, TMAX, TAVG, DATE)
  yvec <- data.frame(DATE = seq.Date(from=as.Date(paste(yr[j+25],"01-01",sep="-")), to=as.Date(paste(yr[j+25],"12-31",sep="-")), by="day"))
  stor[[j]] <- merge(stor[[j]], yvec, by="DATE", all=T)
  n <- nrow(stor[[j]])
  
  ifelse(is.na(stor[[j]][1,"TMIN"]), stor[[j]][1,"TMIN"] <- mean(stor[[j]][2:12,"TMIN"], na.rm=T), stor[[j]][1,"TMIN"]<-stor[[j]][1,"TMIN"] )
  
  ifelse(is.na(stor[[j]][n,"TMIN"]), stor[[j]][n,"TMIN"] <- mean(stor[[j]][354:364,"TMIN"], na.rm=T), stor[[j]][n,"TMIN"]<-stor[[j]][n,"TMIN"] )
  
  ifelse(is.na(stor[[j]][1,"TMAX"]), stor[[j]][1,"TMAX"] <- mean(stor[[j]][2:12,"TMAX"], na.rm=T), stor[[j]][1,"TMAX"]<-stor[[j]][1,"TMAX"] )
  
  ifelse(is.na(stor[[j]][n,"TMAX"]), stor[[j]][n,"TMAX"] <- mean(stor[[j]][354:364,"TMAX"], na.rm=T), stor[[j]][n,"TMAX"]<-stor[[j]][n,"TMAX"] )
  
  stor[[j]][,"TMIN"] <- interpolate_gaps(stor[[j]][,"TMIN"])$interp
  stor[[j]][,"TMAX"] <- interpolate_gaps(stor[[j]][,"TMAX"])$interp
  
  stor[[j]] <- stor[[j]][1:bd.dc[j+25],]
}

year_file <- list(NULL)
hr_temp.dc <- list(NULL)
hr_t.dc <- NULL
for (k in 1:76) {
  year_file[[k]] <- stor[[k]][, c("DATE", "TMIN", "TMAX")] %>% 
    separate(col=DATE, into=c("Year", "Month", "Day"), remove=TRUE)
  year_file[[k]] <- as.data.frame(year_file[[k]])
  year_file[[k]] <- year_file[[k]][,c(4,5,1,2,3)]
  
  subset <- year_file[[k]][,3:5]
  subset2 <- make_JDay(subset)
  
  year_file[[k]] <- data.frame(year_file[[k]][,1:2], subset2)
}

names.file <- c("Tmin", "Tmax", "Year", "Month", "Day", "JDay")
year_file <- lapply(year_file, setNames, names.file)

for (k in 1:76) {
  hr_temp.dc[[k]] <- make_hourly_temps(latitude = 38.8853, year_file = year_file[[k]])
  hr_t.dc[k] <- hr_temp.dc[[k]][length(hr_temp.dc[[k]])]
}


gdd.dc <- data.frame(yr, c(rep(NA, 101))) 
for (h in 1:76) {
  gdd.dc[h+25,2] <- tail(GDD(hr_t.dc[[h]], summ=TRUE, Tbase=5), n=1)
}


cherry.dc$GDD <- gdd.dc[,2]




# ALT Liestal, Switzerland
##################################################################################
##################################################################################
yr <- seq(from=1900, to=2021, by=1)
stor <- list(NULL)

for(j in 1:121) {
  stor[[j]] <- liestal.ghcn %>% 
    filter(year == yr[j+1]) %>% 
    select(year, TMIN, TMAX, TAVG, DATE)
  yvec <- data.frame(DATE = seq.Date(from=as.Date(paste(yr[j+1],"01-01",sep="-")), to=as.Date(paste(yr[j+1],"12-31",sep="-")), by="day"))
  stor[[j]] <- merge(stor[[j]], yvec, by="DATE", all=T)
  n <- nrow(stor[[j]])
  
  ifelse(is.na(stor[[j]][1,"TMIN"]), stor[[j]][1,"TMIN"] <- mean(stor[[j]][2:12,"TMIN"], na.rm=T), stor[[j]][1,"TMIN"]<-stor[[j]][1,"TMIN"] )
  
  ifelse(is.na(stor[[j]][n,"TMIN"]), stor[[j]][n,"TMIN"] <- mean(stor[[j]][354:364,"TMIN"], na.rm=T), stor[[j]][n,"TMIN"]<-stor[[j]][n,"TMIN"] )
  
  ifelse(is.na(stor[[j]][1,"TMAX"]), stor[[j]][1,"TMAX"] <- mean(stor[[j]][2:12,"TMAX"], na.rm=T), stor[[j]][1,"TMAX"]<-stor[[j]][1,"TMAX"] )
  
  ifelse(is.na(stor[[j]][n,"TMAX"]), stor[[j]][n,"TMAX"] <- mean(stor[[j]][354:364,"TMAX"], na.rm=T), stor[[j]][n,"TMAX"]<-stor[[j]][n,"TMAX"] )
  
  stor[[j]][,"TMIN"] <- interpolate_gaps(stor[[j]][,"TMIN"])$interp
  stor[[j]][,"TMAX"] <- interpolate_gaps(stor[[j]][,"TMAX"])$interp
  
  stor[[j]] <- stor[[j]][1:bd.sw[j+1],]
}

year_file <- list(NULL)
hr_temp.sw <- list(NULL)
hr_t.sw <- NULL
for (k in 1:121) {
  year_file[[k]] <- stor[[k]][, c("DATE", "TMIN", "TMAX")] %>% 
    separate(col=DATE, into=c("Year", "Month", "Day"), remove=TRUE)
  year_file[[k]] <- as.data.frame(year_file[[k]])
  year_file[[k]] <- year_file[[k]][,c(4,5,1,2,3)]
  
  subset <- year_file[[k]][,3:5]
  subset2 <- make_JDay(subset)
  
  year_file[[k]] <- data.frame(year_file[[k]][,1:2], subset2)
}

names.file <- c("Tmin", "Tmax", "Year", "Month", "Day", "JDay")
year_file <- lapply(year_file, setNames, names.file)

for (k in 1:121) {
  hr_temp.sw[[k]] <- make_hourly_temps(latitude = 47.4814, year_file = year_file[[k]])
  hr_t.sw[k] <- hr_temp.sw[[k]][length(hr_temp.sw[[k]])]
}


gdd.sw <- data.frame(yr, c(rep(NA, 122))) 
for (h in 1:121) {
  gdd.sw[h+1,2] <- tail(GDD(hr_t.sw[[h]], summ=TRUE, Tbase=5), n=1)
}


cherry.sw$GDD <- gdd.sw[,2]



##################################################################################
##################################################################################

# Exploratory Analysis

##################################################################################
##################################################################################

# DC
view(cherry.dc)
cherry.dc.subset <- cherry.dc[cherry.dc$year >= 1973,]
cherry.dc.subset2 <- cherry.dc[cherry.dc$year >= 1965 & cherry.dc$year <= 1983,]
cherry.dc.subset3 <- cherry.dc[cherry.dc$year >= 1956,]
cherry.dc.subset4 <- cherry.dc[cherry.dc$year >= 2000,]
colnames(cherry.dc)
summary(cherry.dc)

lm.dc1 <- lm(bloom_doy ~ 
               heat.days +
               chill.days +
               sun +
               sun.maysep +
               WDSP +
               SLP +
               PRCP +
               SNWD +
               GDD +
               heat.days*chill.days +
               heat.days*sun +
               chill.days*sun
               , data=cherry.dc.subset)
summary(lm.dc1)


summary(lm(bloom_doy ~ GDD, data=cherry.dc.subset4))





# Kyoto
view(cherry.ja)
cherry.ja.subset <- cherry.ja[cherry.ja$year >= 1973,]
colnames(cherry.ja)
summary(cherry.ja)

lm.ja1 <- lm(bloom_doy ~ 
               GDD +
               sun +
               sun.maysep +
               WDSP +
               SLP
               , data=cherry.ja.subset)
summary(lm.ja1)

summary(lm(bloom_doy ~ GDD , data=cherry.ja))


# Liestal
view(cherry.sw)
cherry.sw.subset <- cherry.sw[cherry.sw$year >= 1973,]
colnames(cherry.sw)
summary(cherry.sw)

lm.sw1 <- lm(bloom_doy ~ 
               heat.days +
               chill.days +
               sun +
               PRCP +
               sun.maysep, data=cherry.sw)
summary(lm.sw1)

summary(lm(bloom_doy ~ GDD , data=cherry.sw))




# Sunlight hours: Jan-Apr 1
# number of sunlight hours to doy - Kyoto
sunhr.ja <- NULL
sunlist.ja <- list(NULL)
ja.sun.fut <- NULL
yr <- seq(from=2022, to=2032, by=1)
for(k in 1:11) {
  sunlist.ja[[k]] <- sun_ja %>% 
    filter(year == yr[k]) %>% 
    select(sunlight_duration) 
  
  ifelse(is.na(sunlist.ja[[k]][bd.ja[k],]), 
         sunhr.ja[k] <- NA, 
         sunhr.ja[k] <- sum(sunlist.ja[[k]][1:bd.ja[k],]) )
  
  d <- as.integer(as.Date(paste(yr[[k]],"04-01",sep="-")) - as.Date(paste(yr[[k]],"01-01",sep="-")))
  ja.sun.fut[k] <- sunhr.ja[k] / (60 * d )
}



# number of sunlight hours to doy - DC
sunhr.dc <- NULL
sunlist.dc <- list(NULL)
dc.sun.fut <- NULL
yr1 <- seq(from=2022, to=2032, by=1)
for(k in 1:length(yr1)) {
  sunlist.dc[[k]] <- sun_dc %>% 
    filter(year == yr1[k]) %>% 
    select(sunlight_duration) 
  
  ifelse(is.na(sunlist.dc[[k]][bd.dc[k],]), 
         sunhr.dc[k] <- NA, 
         sunhr.dc[k] <- sum(sunlist.dc[[k]][1:bd.dc[k],]) )
  d <- as.integer(as.Date(paste(yr[[k]],"04-01",sep="-")) - as.Date(paste(yr[[k]],"01-01",sep="-")))
  dc.sun.fut[k] <- sunhr.dc[k] / (60 * d)
}


# number of sunlight hours to doy - Liestal
sunhr.sw <- NULL
sunlist.sw <- list(NULL)
sw.sun.fut <- NULL
yr <- seq(from=2022, to=2032, by=1)
for(k in 1:11) {
  sunlist.sw[[k]] <- sun_sw %>% 
    filter(year == yr[k]) %>% 
    select(sunlight_duration) 
  
  ifelse(is.na(sunlist.sw[[k]][bd.sw[k],]), 
         sunhr.sw[k] <- NA, 
         sunhr.sw[k] <- sum(sunlist.sw[[k]][1:bd.sw[k],]) )
  d <- as.integer(as.Date(paste(yr[[k]],"04-01",sep="-")) - as.Date(paste(yr[[k]],"01-01",sep="-")))
  sw.sun.fut[k] <- sunhr.sw[k] / (60 * d)
}




