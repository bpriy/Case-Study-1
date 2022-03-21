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
library(modelr)
library(Metrics)
library(FSA)
library(seasonal)
library(fpp2)
library(zoo)
library(imputeTS)

as.Date <- base::as.Date
view <- tibble::view
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
blooms <- blooms %>% 
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
(1-(0.25*(1/0.7)))*7 - 0.5

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


# Vancouver : May-Sep
sunhr.bc <- NA
sunyear_start <- seq(from=as.Date("1936-05-01"), to=as.Date("2020-05-01"), by="year")
sunyear_end <- seq(from=as.Date("1936-09-30"), to=as.Date("2020-09-30"), by="year")
for(j in 1:length(sunyear_start)) {
  sunlist.bc <- sun_bc %>% 
    filter(date >= sunyear_start[j] & date <= sunyear_end[j]) %>% 
    select(sunlight_duration) 
  
  sunhr.bc[j] <- sum(sunlist.bc) 
}
cherry.bc$sun.maysep <- sunhr.bc
cherry.bc$sun.maysep <- cherry.bc$sun.maysep / 153 / 60



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

nara.ghcn <- read.csv("data/kyoto_ghcn.csv", header=TRUE) %>% 
  filter(NAME == "NARA, JA")
nara.ghcn$DATE <- as.Date(nara.ghcn$DATE)
nara.ghcn <- nara.ghcn %>% separate(col=DATE, into=c("year", "month", "day"), remove=FALSE)
nara.ghcn$year <- as.integer(nara.ghcn$year)
nara.ghcn$month <- as.integer(nara.ghcn$month)
nara.ghcn$day <- as.integer(nara.ghcn$day)

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



# Vancouver
######################################################################################
######################################################################################
# Station: CA001108473 (1937-2013)
van.ghcn <- read.csv("data/vancouver_airport, ghcn.csv", header=TRUE)
van.ghcn$DATE <- as.Date(van.ghcn$DATE, format="%m/%d/%Y")
van.ghcn <- van.ghcn %>% separate(col=DATE, into=c("year", "month", "day"), remove=FALSE)
van.ghcn$year <- as.integer(van.ghcn$year)
van.ghcn$month <- as.integer(van.ghcn$month)
van.ghcn$day <- as.integer(van.ghcn$day)

van.ghcn$PRCP <- as.numeric(van.ghcn$PRCP)
van.ghcn$SNOW <- as.numeric(van.ghcn$SNOW)
van.ghcn$SNWD <- as.numeric(van.ghcn$SNWD)
van.ghcn$TMAX <- as.numeric(van.ghcn$TMAX)
van.ghcn$TMIN <- as.numeric(van.ghcn$TMIN)
van.ghcn$MDPR <- as.numeric(van.ghcn$MDPR)

van.ghcn$TMIN <- van.ghcn$TMIN / 10
van.ghcn$TMAX <- van.ghcn$TMAX / 10


# Station: CA001108395 (2014-2015)
van2.ghcn <- read.csv("data/vancouver_airport2, ghcn.csv", header=TRUE)
van2.ghcn$DATE <- as.Date(van2.ghcn$DATE)
van2.ghcn <- van2.ghcn %>% separate(col=DATE, into=c("year", "month", "day"), remove=FALSE)
van2.ghcn$year <- as.integer(van2.ghcn$year)
van2.ghcn$month <- as.integer(van2.ghcn$month)
van2.ghcn$day <- as.integer(van2.ghcn$day)

van2.ghcn$PRCP <- as.numeric(van2.ghcn$PRCP)
van2.ghcn$SNOW <- as.numeric(van2.ghcn$SNOW)
van2.ghcn$SNWD <- as.numeric(van2.ghcn$SNWD)
van2.ghcn$TMAX <- as.numeric(van2.ghcn$TMAX)
van2.ghcn$TMIN <- as.numeric(van2.ghcn$TMIN)
van2.ghcn$TAVG <- as.numeric(van2.ghcn$TAVG)
van2.ghcn$WDFG <- as.numeric(van2.ghcn$WDFG)
van2.ghcn$WSFG <- as.numeric(van2.ghcn$WSFG)

van2.ghcn$TMIN <- van2.ghcn$TMIN / 10
van2.ghcn$TMAX <- van2.ghcn$TMAX / 10
van2.ghcn$TAVG <- van2.ghcn$TAVG / 10


#Station: CA001108380 (2016-2021)
van3.ghcn <- read.csv("data/vancouver_sea_island, ghcn.csv", header=TRUE)
van3.ghcn$DATE <- as.Date(van3.ghcn$DATE)
van3.ghcn <- van3.ghcn %>% separate(col=DATE, into=c("year", "month", "day"), remove=FALSE)
van3.ghcn$year <- as.integer(van3.ghcn$year)
van3.ghcn$month <- as.integer(van3.ghcn$month)
van3.ghcn$day <- as.integer(van3.ghcn$day)

van3.ghcn$PRCP <- as.numeric(van3.ghcn$PRCP)
van3.ghcn$TMAX <- as.numeric(van3.ghcn$TMAX)
van3.ghcn$TMIN <- as.numeric(van3.ghcn$TMIN)
van3.ghcn$TAVG <- as.numeric(van3.ghcn$TAVG)
van3.ghcn$WSFG <- as.numeric(van3.ghcn$WSFG)

van3.ghcn$TMIN <- van3.ghcn$TMIN / 10
van3.ghcn$TMAX <- van3.ghcn$TMAX / 10
van3.ghcn$TAVG <- van3.ghcn$TAVG / 10


summary(van.ghcn)
summary(van2.ghcn)
summary(van3.ghcn)
summary(van2.ghcn[van2.ghcn$DATE >= as.Date("2013-06-12"), ])

van.ghcn$TAVG <- NA
van.ghcn$TAVG_ATTRIBUTES <- NA
van.ghcn$WDFG <- NA
van.ghcn$WDFG_ATTRIBUTES <- NA
van.ghcn$WSFG <- NA
van.ghcn$WSFG_ATTRIBUTES <- NA

van2.ghcn$MDPR <- NA
van2.ghcn$MDPR_ATTRIBUTES <- NA

van.ghcn <- van.ghcn[,c("STATION", "DATE", "year", "month", "day", "LATITUDE", "LONGITUDE", "ELEVATION", "NAME", 
                        "PRCP", "PRCP_ATTRIBUTES", "SNOW", "SNOW_ATTRIBUTES", "SNWD", "SNWD_ATTRIBUTES", "TMAX", 
                        "TMAX_ATTRIBUTES", "TMIN", "TMIN_ATTRIBUTES", "TAVG", "TAVG_ATTRIBUTES", "WDFG", 
                        "WDFG_ATTRIBUTES", "WSFG", "WSFG_ATTRIBUTES", "MDPR", "MDPR_ATTRIBUTES")]


vanc.ghcn <- van.ghcn %>% 
  rbind(van2.ghcn[van2.ghcn$DATE > as.Date("2013-06-12"), ])


yvec <- data.frame(DATE = seq.Date(from=as.Date("1937-01-01"), to=as.Date("2022-03-01"), by="day"))
vanc.ghcn <- merge(vanc.ghcn, yvec, by="DATE", all=TRUE)
van3.ghcn <- merge(van3.ghcn, yvec, by="DATE", all=TRUE)

vanc.ghcn <- vanc.ghcn %>% separate(col=DATE, into=c("year", "month", "day"), remove=FALSE)
vanc.ghcn$year <- as.integer(vanc.ghcn$year)
vanc.ghcn$month <- as.integer(vanc.ghcn$month)
vanc.ghcn$day <- as.integer(vanc.ghcn$day)

summary(vanc.ghcn[vanc.ghcn$DATE > as.Date("2013-06-12"),])
summary(van3.ghcn[van3.ghcn$DATE > as.Date("2013-06-12"),])


cherry.bc <- data.frame(year = c(seq(from=1937, to=2021, by=1)),
                        location = c(rep("vancouver", 85)),
                        lat = c(rep(49.2237, 85)),
                        long = c(rep(-123.1636, 85)),
                        alt = c(rep(24, 85)) )



# GSOD
# WDSP, SLP
#####################
summary(kyoto.gsod)
summary(dc.gsod)
summary(liestal.gsod)



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
summary(van.ghcn)


# Kyoto
# TMIN, TMAX, TAVG
yr <- seq(from=1900, to=2021, by=1)
ghcn.ja <- data.frame(yr, c(rep(NA, 122)), c(rep(NA, 122)), c(rep(NA, 122)), c(rep(NA, 122)) )
stor <- list(NULL)

for(i in 1:72) for(m in 3:5) {
  stor[[i]] <- kyoto.ghcn %>% 
    filter(year == yr[i+50]) %>% 
    select(year, TMIN, TMAX, TAVG, DATE) 
  yvec <- data.frame(DATE = seq.Date(from=as.Date(paste(yr[i+50],"01-01",sep="-")), 
                                     to=as.Date(paste(yr[i+50],"12-31",sep="-")), by="day"))
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

cherry.ja$TMIN <- ghcn.ja[,3]
cherry.ja$TMAX <- ghcn.ja[,4]
cherry.ja$TAVG <- ghcn.ja[,5]



# tavg.m : JAN-APR
yr <- seq(from=1900, to=2021, by=1)
ghcn.ja <- data.frame(yr, c(rep(NA, 122)), c(rep(NA, 122)), c(rep(NA, 122)), c(rep(NA, 122)), c(rep(NA, 122)) )
stor <- list(NULL)

for(i in 1:72) for(m in 3:6) {
  stor[[i]] <- kyoto.ghcn %>% 
    filter(DATE >= as.Date(paste(yr[i+50], "01-01", sep="-")) & DATE <= as.Date(paste(yr[i+50], "04-15", sep="-")) ) %>% 
    select(year, TMIN, TMAX, TAVG, PRCP, DATE) 
  yvec <- data.frame(DATE = seq.Date(from=as.Date(paste(yr[i+50],"01-01",sep="-")), 
                                     to=as.Date(paste(yr[i+50],"04-15",sep="-")), by="day"))
  stor[[i]] <- merge(stor[[i]], yvec, by="DATE", all=T)
  
  ifelse(nrow(stor[[i]]) > 1,
         ifelse(sum(is.na(stor[[i]][,m])) > 0.10*nrow(stor[[i]]),  
                ghcn.ja[i+50,m] <- NA,
                ifelse(is.na(stor[[i]][nrow(stor[[i]]),m]) | is.na(stor[[i]][1,m]), 
                       {stor[[i]][nrow(stor[[i]]),m] <- mean(stor[[i]][94:104,m], na.rm=T)
                       stor[[i]][1,m] <- mean(stor[[i]][2:12,m], na.rm=T)
                       stor[[i]][,m] <- na.approx(stor[[i]][,m]) 
                       ghcn.ja[i+50,m] <- sum(stor[[i]][,m])}, 
                       {stor[[i]][,m] <- na.approx(stor[[i]][,m]) 
                       ghcn.ja[i+50,m] <- sum(stor[[i]][,m])})  ),
         {ghcn.ja[i+50,2] <- NA
         ghcn.ja[i+50,3] <- NA
         ghcn.ja[i+50,4] <- NA
         ghcn.ja[i+50,5] <- NA
         ghcn.ja[i+50,6] <- NA})
  
  ifelse(!is.na(ghcn.ja[i+50,5]),
         ghcn.ja[i+50,5] <- ghcn.ja[i+50,5],
         {ifelse(!is.na(ghcn.ja[i+50,3]) & !is.na(ghcn.ja[i+50,4]),
                ghcn.ja[i+50,5] <- ((ghcn.ja[i+50,3] + ghcn.ja[i+50,4]) / 2),
                ghcn.ja[i+50,5] <- NA )}  )
}

ghcn.ja[106,5] <- as.vector(predict(auto.arima(ts(ghcn.ja[52:106,5]), max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                                               seasonal=TRUE, method="ML"), n.ahead=1)$pred)

cherry.ja$tmin.m <- ghcn.ja[,3]
cherry.ja$tmax.m <- ghcn.ja[,4]
cherry.ja$tavg.m <- ghcn.ja[,5]



# PRCP, SNWD
yr <- seq(from=1900, to=2021, by=1)
ghcn.ja <- data.frame(yr, c(rep(NA, 122)), c(rep(NA, 122)), c(rep(NA, 122)) )
stor <- list(NULL)
as.Date <- base::as.Date

for(i in 1:72) {
  stor[[i]] <- kyoto.ghcn %>% 
    filter(year == yr[i+50]) %>% 
    select(year, PRCP, SNWD, DATE) 
  yvec <- data.frame(DATE = seq.Date(from=as.Date(paste(yr[i+50],"01-01",sep="-")), 
                                     to=as.Date(paste(yr[i+50],"12-31",sep="-")), by="day"))
  stor[[i]] <- merge(stor[[i]], yvec, by="DATE", all=T)
} 
for(i in 1:72) for(j in 1:365)  {
  ifelse(all(is.na(stor[[i]][,3])), ghcn.ja[i,3] <- NA,
    {ifelse(is.na(stor[[i]][j,3]), 
           stor[[i]][j,3] <- 0, 
           stor[[i]][j,3] <- stor[[i]][j,3])} )
  
  ifelse(all(is.na(stor[[i]][,4])), ghcn.ja[i,4] <- NA,
         {ifelse(is.na(stor[[i]][j,4]), 
                 stor[[i]][j,4] <- 0, 
                 stor[[i]][j,4] <- stor[[i]][j,4])} )
}  
for (i in 1:72) for(k in 3:4) {ghcn.ja[i+50,k] <- sum(stor[[i]][1:bd.ja[i+50],k])}

cherry.ja$PRCP <- ghcn.ja[,3]
cherry.ja$SNWD <- ghcn.ja[,4]



# Kyoto monthly data
yr <- seq(from=1900, to=2021, by=1)
month.ja <- data.frame(yr, mpct.pos.sun=c(rep(NA, 122)), t.sun.time=c(rep(NA, 122)), 
                       msealevel.ap=c(rep(NA, 122)), t.snwdp=c(rep(NA, 122)), t.precip=c(rep(NA, 122)) )
stor <- list(NULL)
for(i in 1:122)  {
  stor[[i]] <- kyoto %>% 
    filter(Year == yr[i]) %>% 
    select(Year, Month, mpct.pos.sun, t.sun.time, msealevel.ap, t.precip, t.snwdp, mdtemp, 
           md.mintemp, md.maxtemp) 
}  
for(i in 1:122) {
  #mpct.pos.sun
  ifelse(all(!is.na(stor[[i]][1:3,"mpct.pos.sun"])),
         month.ja[i,2] <- mean(stor[[i]][1:3,"mpct.pos.sun"], na.rm=TRUE),
         month.ja[i,2] <- NA)
  
  #t.sun.time
  ifelse(all(!is.na(stor[[i]][1:3,"t.sun.time"])),
         month.ja[i,3] <- sum(stor[[i]][1:3,"t.sun.time"], na.rm=TRUE),
         month.ja[i,3] <- NA)
  
  #msealevel.ap
  ifelse(all(!is.na(stor[[i]][1:3,"msealevel.ap"])),
         {if (!is.na(stor[[i]][1,"msealevel.ap"])) 
          stor[[i]][1,"msealevel.ap"] <-conv_unit(stor[[i]][1,"msealevel.ap"], from="hPa", to="mbar")*0.1
         if (!is.na(stor[[i]][2,"msealevel.ap"])) 
          stor[[i]][2,"msealevel.ap"] <-conv_unit(stor[[i]][2,"msealevel.ap"], from="hPa", to="mbar")*0.1
         if (!is.na(stor[[i]][3,"msealevel.ap"])) 
          stor[[i]][3,"msealevel.ap"] <-conv_unit(stor[[i]][3,"msealevel.ap"], from="hPa", to="mbar")*0.1
         month.ja[i,4] <- mean(stor[[i]][1:3,"msealevel.ap"], na.rm=TRUE)},
         month.ja[i,4] <- NA)
  
   #t.snwdp
  ifelse(all(!is.na(stor[[i]][1:3,"t.snwdp"])),
         month.ja[i,5] <- sum(stor[[i]][1:3,"t.snwdp"], na.rm=TRUE),
         month.ja[i,5] <- NA)
  
  #t.precip
  ifelse(all(!is.na(stor[[i]][1:3,"t.precip"])),
         month.ja[i,6] <- sum(stor[[i]][1:3,"t.precip"], na.rm=TRUE),
         month.ja[i,6] <- NA)
}
cherry.ja$pos.sun <- month.ja[,"mpct.pos.sun"]
cherry.ja$sun.time <- month.ja[,"t.sun.time"]
cherry.ja$slap <- month.ja[,"msealevel.ap"]
cherry.ja$precip <- month.ja[,"t.precip"]
cherry.ja$prcp.m <- cherry.ja$precip


# Temp Diff
cherry.ja$TempDiffAbs <- abs(cherry.ja[,"TMAX"] - cherry.ja[,"TMIN"]) / 10
cherry.ja$TempDiffSq <- (cherry.ja[,"TMAX"] - cherry.ja[,"TMIN"])^2 / 1000
cherry.ja$Tavg <- (cherry.ja[,"TMAX"] + cherry.ja[,"TMIN"]) / 2





# Washington, DC
yr1 <- seq(from=1921, to=2021, by=1)
ghcn.dc <- data.frame(yr1, c(rep(NA, 101)), c(rep(NA, 101)), c(rep(NA, 101)), c(rep(NA, 101)),
                      c(rep(NA, 101)), c(rep(NA, 101)), c(rep(NA, 101)) )
stor <- list(NULL)

for(i in 1:66) for(m in 3:8) {
  stor[[i]] <- dc.ghcn %>% 
    filter(year == yr1[i+35]) %>% 
    select(year, PRCP, SNWD, TMIN, TMAX, TAVG, PSUN, DATE) 
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


# Temp Diff
cherry.dc$TempDiffAbs <- abs(cherry.dc[,"TMAX"] - cherry.dc[,"TMIN"]) / 10
cherry.dc$TempDiffSq <- (cherry.dc[,"TMAX"] - cherry.dc[,"TMIN"])^2 / 1000
cherry.dc$Tavg <- (cherry.dc[,"TMAX"] + cherry.dc[,"TMIN"]) / 2



# tavg.m : JAN-APR
yr <- seq(from=1921, to=2021, by=1)
ghcn.dc <- data.frame(yr, c(rep(NA, 101)), c(rep(NA, 101)), c(rep(NA, 101)), c(rep(NA, 101)), c(rep(NA, 101)) )
stor <- list(NULL)

for(i in 1:66) for(m in 3:6) {
  stor[[i]] <- dc.ghcn %>% 
    filter(DATE >= as.Date(paste(yr[i+35], "01-01", sep="-")) & DATE <= as.Date(paste(yr[i+35], "04-15", sep="-")) ) %>% 
    select(year, TMIN, TMAX, TAVG, PRCP, DATE) 
  yvec <- data.frame(DATE = seq.Date(from=as.Date(paste(yr[i+35],"01-01",sep="-")), 
                                     to=as.Date(paste(yr[i+35],"04-15",sep="-")), by="day"))
  stor[[i]] <- merge(stor[[i]], yvec, by="DATE", all=T)
  
  ifelse(nrow(stor[[i]]) > 1,
         ifelse(sum(is.na(stor[[i]][,m])) > 0.10*nrow(stor[[i]]),  
                ghcn.dc[i+35,m] <- NA,
                ifelse(is.na(stor[[i]][nrow(stor[[i]]),m]) | is.na(stor[[i]][1,m]), 
                       {stor[[i]][nrow(stor[[i]]),m] <- mean(stor[[i]][94:104,m], na.rm=T)
                       stor[[i]][1,m] <- mean(stor[[i]][2:12,m], na.rm=T)
                       stor[[i]][,m] <- na.approx(stor[[i]][,m]) 
                       ghcn.dc[i+35,m] <- sum(stor[[i]][,m])}, 
                       {stor[[i]][,m] <- na.approx(stor[[i]][,m]) 
                       ghcn.dc[i+35,m] <- sum(stor[[i]][,m])})  ),
         {ghcn.dc[i+35,2] <- NA
         ghcn.dc[i+35,3] <- NA
         ghcn.dc[i+35,4] <- NA
         ghcn.dc[i+35,5] <- NA})
  
  ifelse(!is.na(ghcn.dc[i+35,5]),
         ghcn.dc[i+35,5] <- ghcn.dc[i+35,5],
         {ifelse(!is.na(ghcn.dc[i+35,3]) & !is.na(ghcn.dc[i+35,4]),
                 ghcn.dc[i+35,5] <- ((ghcn.dc[i+35,3] + ghcn.dc[i+35,4]) / 2),
                 ghcn.dc[i+35,5] <- NA )}  )
}

cherry.dc$tmin.m <- ghcn.dc[,3]
cherry.dc$tmax.m <- ghcn.dc[,4]
cherry.dc$tavg.m <- ghcn.dc[,5]
cherry.dc$prcp.m <- ghcn.dc[,6]



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


# tavg.m : JAN-APR
yr <- seq(from=1900, to=2021, by=1)
ghcn.sw <- data.frame(yr, c(rep(NA, 122)), c(rep(NA, 122)), c(rep(NA, 122)), c(rep(NA, 122)), c(rep(NA, 122)) )
stor <- list(NULL)

for(i in 1:72) for(m in 3:6) {
  stor[[i]] <- liestal.ghcn %>% 
    filter(DATE >= as.Date(paste(yr[i+50], "01-01", sep="-")) & DATE <= as.Date(paste(yr[i+50], "04-15", sep="-")) ) %>% 
    select(year, TMIN, TMAX, TAVG, PRCP, DATE) 
  yvec <- data.frame(DATE = seq.Date(from=as.Date(paste(yr[i+50],"01-01",sep="-")), 
                                     to=as.Date(paste(yr[i+50],"04-15",sep="-")), by="day"))
  stor[[i]] <- merge(stor[[i]], yvec, by="DATE", all=T)
  
  ifelse(nrow(stor[[i]]) > 1,
         ifelse(sum(is.na(stor[[i]][,m])) > 0.10*nrow(stor[[i]]),  
                ghcn.sw[i+50,m] <- NA,
                ifelse(is.na(stor[[i]][nrow(stor[[i]]),m]) | is.na(stor[[i]][1,m]), 
                       {stor[[i]][nrow(stor[[i]]),m] <- mean(stor[[i]][94:104,m], na.rm=T)
                       stor[[i]][1,m] <- mean(stor[[i]][2:12,m], na.rm=T)
                       stor[[i]][,m] <- na.approx(stor[[i]][,m]) 
                       ghcn.sw[i+50,m] <- sum(stor[[i]][,m])}, 
                       {stor[[i]][,m] <- na.approx(stor[[i]][,m]) 
                       ghcn.sw[i+50,m] <- sum(stor[[i]][,m])})  ),
         {ghcn.sw[i+50,2] <- NA
         ghcn.sw[i+50,3] <- NA
         ghcn.sw[i+50,4] <- NA
         ghcn.sw[i+50,5] <- NA
         ghcn.sw[i+50,6] <- NA})
  
  ifelse(!is.na(ghcn.sw[i+50,5]),
         ghcn.sw[i+50,5] <- ghcn.sw[i+50,5],
         {ifelse(!is.na(ghcn.sw[i+50,3]) & !is.na(ghcn.sw[i+50,4]),
                 ghcn.sw[i+50,5] <- ((ghcn.sw[i+50,3] + ghcn.sw[i+50,4]) / 2),
                 ghcn.sw[i+50,5] <- NA )}  )
}

ghcn.sw[118,3] <- (2*ghcn.sw[118,5]) - ghcn.sw[118,4]

cherry.sw$tmin.m <- ghcn.sw[,3]
cherry.sw$tmax.m <- ghcn.sw[,4]
cherry.sw$tavg.m <- ghcn.sw[,5]
cherry.sw$prcp.m <- ghcn.sw[,6]




# Vancouver
summary(vanc.ghcn[vanc.ghcn$year>2013,])
# TMIN, TMAX, tavg.mar
yr <- seq(from=1937, to=2021, by=1)
ghcn.bc <- data.frame(yr, c(rep(NA, 85)), c(rep(NA, 85)), c(rep(NA, 85)) )
stor <- list(NULL)

for(i in 1:85) for(m in 3:4) {
  stor[[i]] <- vanc.ghcn %>% 
    filter(DATE >= as.Date(paste(yr[i], "01-01", sep="-")) & DATE <= as.Date(paste(yr[i], "04-15", sep="-")) ) %>% 
    select(year, TMIN, TMAX, month, DATE) 
  yvec <- data.frame(DATE = seq.Date(from=as.Date(paste(yr[i],"01-01",sep="-")), 
                                     to=as.Date(paste(yr[i],"04-15",sep="-")), by="day"))
  stor[[i]] <- merge(stor[[i]], yvec, by="DATE", all=T)
}
jan <- list(NULL)
feb <- list(NULL)
mar <- list(NULL)
apr <- list(NULL)
mena1 <- NULL ; mena2 <- NULL ; mena3 <- NULL ; mena4 <- NULL
mena5 <- NULL ; mena6 <- NULL ; mena7 <- NULL ; mena8 <- NULL
for(i in 1:85) {
  
  w1 <- as.data.frame(stor[[i]][1:31,3])
  w2 <- as.data.frame(stor[[i]][32:59,3])
  w3 <- as.data.frame(stor[[i]][60:90,3])
  w4 <- as.data.frame(stor[[i]][91:nrow(stor[[i]]),3])
  
  names(w1) <- c("X0")
  names(w2) <- c("X0")
  names(w3) <- c("X0")
  names(w4) <- c("X0")
  
  mena1[i] <- mean(w1$X0, na.rm=T)
  mena2[i] <- mean(w2$X0, na.rm=T)
  mena3[i] <- mean(w3$X0, na.rm=T)
  mena4[i] <- mean(w4$X0, na.rm=T)
  
  q1 <- as.data.frame(stor[[i]][1:31,4])
  q2 <- as.data.frame(stor[[i]][32:59,4])
  q3 <- as.data.frame(stor[[i]][60:90,4])
  q4 <- as.data.frame(stor[[i]][91:nrow(stor[[i]]),4])
  
  names(q1) <- c("X1")
  names(q2) <- c("X1")
  names(q3) <- c("X1")
  names(q4) <- c("X1")
  
  mena5[i] <- mean(q1$X1, na.rm=T)
  mena6[i] <- mean(q2$X1, na.rm=T)
  mena7[i] <- mean(q3$X1, na.rm=T)
  mena8[i] <- mean(q4$X1, na.rm=T)
  
  r1 <- cbind(w1, q1)
  r2 <- cbind(w2, q2)
  r3 <- cbind(w3, q3)
  r4 <- cbind(w4, q4)
  
  jan[[1]] <- rbind(as.data.frame(jan[[1]]), r1)
  feb[[1]] <- rbind(as.data.frame(feb[[1]]), r2)
  mar[[1]] <- rbind(as.data.frame(mar[[1]]), r3)
  apr[[1]] <- rbind(as.data.frame(apr[[1]]), r4)
}

jan <- as.data.frame(jan)
feb <- as.data.frame(feb)
mar <- as.data.frame(mar)
apr <- as.data.frame(apr)

tyme <- seq(from=1937, to=2021, by=1)
plot(tyme, mena1, type="l")
plot(tyme, mena2, type="l")
plot(tyme, mena3, type="l")
plot(tyme, mena4, type="l")
plot(tyme, mena5, type="l")
plot(tyme, mena6, type="l")
plot(tyme, mena7, type="l")
plot(tyme, mena8, type="l")

j1 <- sd(jan[,1], na.rm=T) ; j2 <- sd(jan[,2], na.rm=T) ; j3 <- sd(feb[,1], na.rm=T) ; j4 <- sd(feb[,2], na.rm=T)
j5 <- sd(mar[,1], na.rm=T) ; j6 <- sd(mar[,2], na.rm=T) ; j7 <- sd(apr[,1], na.rm=T) ; j8 <- sd(apr[,2], na.rm=T)

menaj <- data.frame(NA,NA, mean(mena1), mean(mena5))
menaf <- data.frame(NA,NA, mean(mena2), mean(mena6))
menam <- data.frame(NA,NA, mean(mena3), mean(mena7))
menaa <- data.frame(NA,NA, mean(mena4), mean(mena8))

jj <- data.frame(NA,NA, j1, j5)
jf <- data.frame(NA,NA, j2, j6)
jm <- data.frame(NA,NA, j3, j7)
ja <- data.frame(NA,NA, j4, j8)

set.seed(7)
for(i in 1:85)  {
  hold.df <- stor[[i]]
  
  jans <- hold.df[hold.df$month==1, ] 
  for(j in 1:nrow(jans)) for(k in 1:nrow(jans)) for(m in 3:4) {
    ifelse(is.na(jans[k,m]),
           jans[k,m] <- rnorm(1, mean=menaj[,m], sd=jj[,m]),
           jans[k,m] <- jans[k,m] ) }
    
    febs <- hold.df[hold.df$month==2, ] 
    for(j in (nrow(jans)+1):nrow(febs) ) for(k in 1:nrow(febs)) for(m in 3:4) {
      ifelse(is.na(febs[k,m]),
             febs[k,m] <- rnorm(1, mean=menaf[,m], sd=jf[,m]),
             febs[k,m] <- febs[k,m] ) }
    
    mars <- hold.df[hold.df$month==3, ] 
    for(j in (nrow(febs)+1):nrow(mars) ) for(k in 1:nrow(mars)) for(m in 3:4) {
      ifelse(is.na(mars[k,m]),
             mars[k,m] <- rnorm(1, mean=menam[,m], sd=jm[,m]),
             mars[k,m] <- mars[k,m] ) }
    
    aprs <- hold.df[hold.df$month==4, ] 
    for(j in (nrow(mars)+1):nrow(aprs) ) for(k in 1:nrow(aprs)) for(m in 3:4) {
      ifelse(is.na(aprs[k,m]),
             aprs[k,m] <- rnorm(1, mean=menaa[,m], sd=ja[,m]),
             aprs[k,m] <- aprs[k,m] ) }
    
    stor[[i]] <- jans %>% rbind(febs) %>% rbind(mars) %>% rbind(aprs)
    for(m in 3:4){ ghcn.bc[i,m] <- sum(stor[[i]][,m]) }
    ghcn.bc[i,5] <- sum((stor[[i]][,3] + stor[[i]][,4]) / 2)
}    
    
cherry.bc$TMIN <- ghcn.bc[,3]
cherry.bc$TMAX <- ghcn.bc[,4]
cherry.bc$tavg.m <- ghcn.bc[,5]



# prcp.m
yr <- seq(from=1937, to=2021, by=1)
ghcn.bc <- data.frame(yr, c(rep(NA, 85)), c(rep(NA, 85)) )
stor <- list(NULL)

for(i in 1:85)  {
  stor[[i]] <- vanc.ghcn %>% 
    filter(DATE >= as.Date(paste(yr[i], "01-01", sep="-")) & DATE <= as.Date(paste(yr[i], "04-15", sep="-")) ) %>% 
    select(year, PRCP, DATE) 
  yvec <- data.frame(DATE = seq.Date(from=as.Date(paste(yr[i],"01-01",sep="-")), 
                                     to=as.Date(paste(yr[i],"04-15",sep="-")), by="day"))
  stor[[i]] <- merge(stor[[i]], yvec, by="DATE", all=T)
  
  for(k in 1:nrow(stor[[i]])) {
  ifelse(is.na(stor[[i]][k,"PRCP"]),
         stor[[i]][k,"PRCP"] <- 0,
         stor[[i]][k,"PRCP"] <- stor[[i]][k,"PRCP"])}
         
  ghcn.bc[i,3] <- sum(stor[[i]][,"PRCP"])
}

cherry.bc$prcp.m <- ghcn.bc[,3]






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
mor <- list(NULL)

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

  rang <- as.Date(paste(yr[j+51],"04-16",sep="-")) - as.Date(paste(yr[j+51],"01-01",sep="-"))
  mor[[j]] <- stor[[j]][1:rang,]
  
  stor[[j]] <- stor[[j]][1:bd.ja[j+51],]
}

year_file <- list(NULL)
year_file2 <- list(NULL)
hr_temp.ja <- list(NULL)
hr_temp.ja2 <- list(NULL)
hr_t.ja <- NULL
hr_t.ja2 <- NULL
for (k in 1:71) {
  year_file[[k]] <- stor[[k]][, c("DATE", "TMIN", "TMAX")] %>% 
    separate(col=DATE, into=c("Year", "Month", "Day"), remove=TRUE)
  year_file[[k]] <- as.data.frame(year_file[[k]])
  year_file[[k]] <- year_file[[k]][,c(4,5,1,2,3)]
  
  subset <- year_file[[k]][,3:5]
  subset2 <- make_JDay(subset)
  
  year_file[[k]] <- data.frame(year_file[[k]][,1:2], subset2)
  
  # JAN-APR
  year_file2[[k]] <- mor[[k]][, c("DATE", "TMIN", "TMAX")] %>% 
    separate(col=DATE, into=c("Year", "Month", "Day"), remove=TRUE)
  year_file2[[k]] <- as.data.frame(year_file2[[k]])
  year_file2[[k]] <- year_file2[[k]][,c(4,5,1,2,3)]
  
  subset3 <- year_file2[[k]][,3:5]
  subset4 <- make_JDay(subset3)
  
  year_file2[[k]] <- data.frame(year_file2[[k]][,1:2], subset4)
}

names.file <- c("Tmin", "Tmax", "Year", "Month", "Day", "JDay")
year_file <- lapply(year_file, setNames, names.file)
year_file2 <- lapply(year_file2, setNames, names.file)


for (k in 1:54) {
  hr_temp.ja[[k]] <- make_hourly_temps(latitude = 35.0120, year_file = year_file[[k]])
  hr_t.ja[k] <- hr_temp.ja[[k]][length(hr_temp.ja[[k]])]
  
  hr_temp.ja2[[k]] <- make_hourly_temps(latitude = 35.0120, year_file = year_file2[[k]])
  hr_t.ja2[k] <- hr_temp.ja2[[k]][length(hr_temp.ja2[[k]])]
}
for (k in 56:71) {
  hr_temp.ja[[k]] <- make_hourly_temps(latitude = 35.0120, year_file = year_file[[k]])
  hr_t.ja[k] <- hr_temp.ja[[k]][length(hr_temp.ja[[k]])]
  
  hr_temp.ja2[[k]] <- make_hourly_temps(latitude = 35.0120, year_file = year_file2[[k]])
  hr_t.ja2[k] <- hr_temp.ja2[[k]][length(hr_temp.ja2[[k]])]
}

gdd.ja <- data.frame(yr, c(rep(NA, 122))) 
gdd.ja2 <- data.frame(yr, c(rep(NA, 122))) 
for (h in 1:54) {
  gdd.ja[h+51,2] <- tail(GDD(hr_t.ja[[h]], summ=TRUE, Tbase=5), n=1)
  gdd.ja2[h+51,2] <- tail(GDD(hr_t.ja2[[h]], summ=TRUE, Tbase=5), n=1)
}
for (h in 56:71) {
  gdd.ja[h+51,2] <- tail(GDD(hr_t.ja[[h]], summ=TRUE, Tbase=5), n=1)
  gdd.ja2[h+51,2] <- tail(GDD(hr_t.ja2[[h]], summ=TRUE, Tbase=5), n=1)
}

cherry.ja$GDD <- gdd.ja[,2]
cherry.ja$gdd.m <- gdd.ja2[,2]



# ALT Washington, DC
##################################################################################
##################################################################################
yr <- seq(from=1921, to=2021, by=1)
stor <- list(NULL)
mor <- list(NULL)

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
  
  rang <- as.Date(paste(yr[j+25],"04-16",sep="-")) - as.Date(paste(yr[j+25],"01-01",sep="-"))
  mor[[j]] <- stor[[j]][1:rang,]
  
  stor[[j]] <- stor[[j]][1:bd.dc[j+25],]
}

year_file <- list(NULL)
year_file2 <- list(NULL)
hr_temp.dc <- list(NULL)
hr_temp.dc2 <- list(NULL)
hr_t.dc <- NULL
hr_t.dc2 <- NULL
for (k in 1:76) {
  year_file[[k]] <- stor[[k]][, c("DATE", "TMIN", "TMAX")] %>% 
    separate(col=DATE, into=c("Year", "Month", "Day"), remove=TRUE)
  year_file[[k]] <- as.data.frame(year_file[[k]])
  year_file[[k]] <- year_file[[k]][,c(4,5,1,2,3)]
  
  subset <- year_file[[k]][,3:5]
  subset2 <- make_JDay(subset)
  
  year_file[[k]] <- data.frame(year_file[[k]][,1:2], subset2)
  
  # JAN-APR
  year_file2[[k]] <- mor[[k]][, c("DATE", "TMIN", "TMAX")] %>% 
    separate(col=DATE, into=c("Year", "Month", "Day"), remove=TRUE)
  year_file2[[k]] <- as.data.frame(year_file2[[k]])
  year_file2[[k]] <- year_file2[[k]][,c(4,5,1,2,3)]
  
  subset3 <- year_file2[[k]][,3:5]
  subset4 <- make_JDay(subset3)
  
  year_file2[[k]] <- data.frame(year_file2[[k]][,1:2], subset4)
}

names.file <- c("Tmin", "Tmax", "Year", "Month", "Day", "JDay")
year_file <- lapply(year_file, setNames, names.file)
year_file2 <- lapply(year_file2, setNames, names.file)


for (k in 1:76) {
  hr_temp.dc[[k]] <- make_hourly_temps(latitude = 38.8853, year_file = year_file[[k]])
  hr_t.dc[k] <- hr_temp.dc[[k]][length(hr_temp.dc[[k]])]
  
  hr_temp.dc2[[k]] <- make_hourly_temps(latitude = 38.8853, year_file = year_file2[[k]])
  hr_t.dc2[k] <- hr_temp.dc2[[k]][length(hr_temp.dc2[[k]])]
}


gdd.dc <- data.frame(yr, c(rep(NA, 101))) 
gdd.dc2 <- data.frame(yr, c(rep(NA, 101))) 
for (h in 1:76) {
  gdd.dc[h+25,2] <- tail(GDD(hr_t.dc[[h]], summ=TRUE, Tbase=5), n=1)
  gdd.dc2[h+25,2] <- tail(GDD(hr_t.dc2[[h]], summ=TRUE, Tbase=5), n=1)
}


cherry.dc$GDD <- gdd.dc[,2]
cherry.dc$gdd.m <- gdd.dc2[,2]





# ALT Liestal, Switzerland
##################################################################################
##################################################################################
yr <- seq(from=1900, to=2021, by=1)
stor <- list(NULL)
mor <- list(NULL)

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
  
  rang <- as.Date(paste(yr[j+1],"04-16",sep="-")) - as.Date(paste(yr[j+1],"01-01",sep="-"))
  mor[[j]] <- stor[[j]][1:rang,]
  
  stor[[j]] <- stor[[j]][1:bd.sw[j+1],]
}

year_file <- list(NULL)
year_file2 <- list(NULL)
hr_temp.sw <- list(NULL)
hr_temp.sw2 <- list(NULL)
hr_t.sw <- NULL
hr_t.sw2 <- NULL
for (k in 1:121) {
  year_file[[k]] <- stor[[k]][, c("DATE", "TMIN", "TMAX")] %>% 
    separate(col=DATE, into=c("Year", "Month", "Day"), remove=TRUE)
  year_file[[k]] <- as.data.frame(year_file[[k]])
  year_file[[k]] <- year_file[[k]][,c(4,5,1,2,3)]
  
  subset <- year_file[[k]][,3:5]
  subset2 <- make_JDay(subset)
  
  year_file[[k]] <- data.frame(year_file[[k]][,1:2], subset2)
  
  # JAN-APR
  year_file2[[k]] <- mor[[k]][, c("DATE", "TMIN", "TMAX")] %>% 
    separate(col=DATE, into=c("Year", "Month", "Day"), remove=TRUE)
  year_file2[[k]] <- as.data.frame(year_file2[[k]])
  year_file2[[k]] <- year_file2[[k]][,c(4,5,1,2,3)]
  
  subset3 <- year_file2[[k]][,3:5]
  subset4 <- make_JDay(subset3)
  
  year_file2[[k]] <- data.frame(year_file2[[k]][,1:2], subset4)
}

names.file <- c("Tmin", "Tmax", "Year", "Month", "Day", "JDay")
year_file <- lapply(year_file, setNames, names.file)
year_file2 <- lapply(year_file2, setNames, names.file)

for (k in 1:121) {
  hr_temp.sw[[k]] <- make_hourly_temps(latitude = 47.4814, year_file = year_file[[k]])
  hr_t.sw[k] <- hr_temp.sw[[k]][length(hr_temp.sw[[k]])]
  
  hr_temp.sw2[[k]] <- make_hourly_temps(latitude = 47.4814, year_file = year_file2[[k]])
  hr_t.sw2[k] <- hr_temp.sw2[[k]][length(hr_temp.sw2[[k]])]
}


gdd.sw <- data.frame(yr, c(rep(NA, 122))) 
gdd.sw2 <- data.frame(yr, c(rep(NA, 122))) 
for (h in 1:121) {
  gdd.sw[h+1,2] <- tail(GDD(hr_t.sw[[h]], summ=TRUE, Tbase=5), n=1)
  gdd.sw2[h+1,2] <- tail(GDD(hr_t.sw2[[h]], summ=TRUE, Tbase=5), n=1)
}


cherry.sw$GDD <- gdd.sw[,2]
cherry.sw$gdd.m <- gdd.sw2[,2]



# Vancouver
##################################################################################
##################################################################################
yr <- seq(from=1937, to=2021, by=1)
stor <- list(NULL)
mor <- list(NULL)

for(j in 1:85) {
  stor[[j]] <- vanc.ghcn %>% 
    filter(year == yr[j]) %>% 
    select(year, TMIN, TMAX, TAVG, DATE)
  yvec <- data.frame(DATE = seq.Date(from=as.Date(paste(yr[j],"01-01",sep="-")), to=as.Date(paste(yr[j],"12-31",sep="-")), by="day"))
  stor[[j]] <- merge(stor[[j]], yvec, by="DATE", all=T)
  n <- nrow(stor[[j]])
  
  ifelse(is.na(stor[[j]][1,"TMIN"]), stor[[j]][1,"TMIN"] <- mean(stor[[j]][2:12,"TMIN"], na.rm=T), stor[[j]][1,"TMIN"]<-stor[[j]][1,"TMIN"] )
  
  ifelse(is.na(stor[[j]][n,"TMIN"]), stor[[j]][n,"TMIN"] <- mean(stor[[j]][354:364,"TMIN"], na.rm=T), stor[[j]][n,"TMIN"]<-stor[[j]][n,"TMIN"] )
  
  ifelse(is.na(stor[[j]][1,"TMAX"]), stor[[j]][1,"TMAX"] <- mean(stor[[j]][2:12,"TMAX"], na.rm=T), stor[[j]][1,"TMAX"]<-stor[[j]][1,"TMAX"] )
  
  ifelse(is.na(stor[[j]][n,"TMAX"]), stor[[j]][n,"TMAX"] <- mean(stor[[j]][354:364,"TMAX"], na.rm=T), stor[[j]][n,"TMAX"]<-stor[[j]][n,"TMAX"] )
  
  stor[[j]][,"TMIN"] <- interpolate_gaps(stor[[j]][,"TMIN"])$interp
  stor[[j]][,"TMAX"] <- interpolate_gaps(stor[[j]][,"TMAX"])$interp
  
  rang <- as.Date(paste(yr[j],"04-16",sep="-")) - as.Date(paste(yr[j],"01-01",sep="-"))
  mor[[j]] <- stor[[j]][1:rang,]
}

year_file2 <- list(NULL)
hr_temp.bc2 <- list(NULL)
hr_t.bc2 <- NULL
for (k in 1:85) {
  # JAN-APR
  year_file2[[k]] <- mor[[k]][, c("DATE", "TMIN", "TMAX")] %>% 
    separate(col=DATE, into=c("Year", "Month", "Day"), remove=TRUE)
  year_file2[[k]] <- as.data.frame(year_file2[[k]])
  year_file2[[k]] <- year_file2[[k]][,c(4,5,1,2,3)]
  
  subset3 <- year_file2[[k]][,3:5]
  subset4 <- make_JDay(subset3)
  
  year_file2[[k]] <- data.frame(year_file2[[k]][,1:2], subset4)
}

names.file <- c("Tmin", "Tmax", "Year", "Month", "Day", "JDay")
year_file2 <- lapply(year_file2, setNames, names.file)

for (k in 1:85) {
  hr_temp.bc2[[k]] <- make_hourly_temps(latitude = 38.8853, year_file = year_file2[[k]])
  hr_t.bc2[k] <- hr_temp.bc2[[k]][length(hr_temp.bc2[[k]])]
}


gdd.bc2 <- data.frame(yr, c(rep(NA, 85))) 
for (h in 1:85) {
  gdd.bc2[h,2] <- tail(GDD(hr_t.bc2[[h]], summ=TRUE, Tbase=5), n=1)
}


cherry.bc$gdd.m <- gdd.bc2[,2]



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
# number of sunlight hours to Apr 1 - Kyoto
sunhr.ja <- NULL
sunlist.ja <- list(NULL)
ja.sun <- NULL
yr <- seq(from=1900, to=2021, by=1)
for(k in 1:length(yr)) {
  sunlist.ja[[k]] <- sun_ja %>% 
    filter(date >= as.Date(paste(yr[k], "01-01", sep="-")) & 
             date <= as.Date(paste(yr[k], "04-01", sep="-")) ) %>% 
    select(sunlight_duration) 
  
  sunhr.ja[k] <- sum(sunlist.ja[[k]])
  
  d <- as.integer(as.Date(paste(yr[[k]],"04-02",sep="-")) - as.Date(paste(yr[[k]],"01-01",sep="-")))
  ja.sun[k] <- sunhr.ja[k] / (60 * d )
}
cherry.ja$sun.mar <- ja.sun


# number of sunlight hours to Apr 1 - DC
sunhr.dc <- NULL
sunlist.dc <- list(NULL)
dc.sun <- NULL
yr1 <- seq(from=1921, to=2021, by=1)
for(k in 1:length(yr1)) {
  sunlist.dc[[k]] <- sun_dc %>% 
    filter(date >= as.Date(paste(yr[k], "01-01", sep="-")) & 
             date <= as.Date(paste(yr[k], "04-01", sep="-")) ) %>% 
    select(sunlight_duration) 
  
  sunhr.dc[k] <- sum(sunlist.dc[[k]]) 
  
  d <- as.integer(as.Date(paste(yr[[k]],"04-02",sep="-")) - as.Date(paste(yr[[k]],"01-01",sep="-")))
  dc.sun[k] <- sunhr.dc[k] / (60 * d )
}
cherry.dc$sun.mar <- dc.sun


# number of sunlight hours to Apr 1 - Liestal
sunhr.sw <- NULL
sunlist.sw <- list(NULL)
sw.sun <- NULL
yr <- seq(from=1900, to=2021, by=1)
for(k in 1:length(yr)) {
  sunlist.sw[[k]] <- sun_sw %>% 
    filter(date >= as.Date(paste(yr[k], "01-01", sep="-")) & date <= as.Date(paste(yr[k], "04-01", sep="-")) ) %>% 
    select(sunlight_duration) 
  
   sunhr.sw[k] <- sum(sunlist.sw[[k]]) 
   
   d <- as.integer(as.Date(paste(yr[[k]],"04-02",sep="-")) - as.Date(paste(yr[[k]],"01-01",sep="-")))
   sw.sun[k] <- sunhr.sw[k] / (60 * d )
}
cherry.sw$sun.mar <- sw.sun




# Sunlight hours, 2022-2032: Jan-Apr 1
# average sunlight hours - Kyoto
sunhr.ja <- NULL
sunlist.ja <- list(NULL)
ja.sun.fut <- NULL
yr <- seq(from=2022, to=2032, by=1)
for(k in 1:11) {
  sunlist.ja[[k]] <- sun_ja %>% 
    filter(date >= as.Date(paste(yr[k], "01-01", sep="-")) & date <= as.Date(paste(yr[k], "04-01", sep="-")) ) %>% 
    select(sunlight_duration) 
  
   sunhr.ja[k] <- sum(sunlist.ja[[k]])
  
  d <- as.integer(as.Date(paste(yr[[k]],"04-02",sep="-")) - as.Date(paste(yr[[k]],"01-01",sep="-")))
  ja.sun.fut[k] <- sunhr.ja[k] / (60 * d )
}


# average sunlight hours - DC
sunhr.dc <- NULL
sunlist.dc <- list(NULL)
dc.sun.fut <- NULL
yr1 <- seq(from=2022, to=2032, by=1)
for(k in 1:length(yr1)) {
  sunlist.dc[[k]] <- sun_dc %>% 
    filter(date >= as.Date(paste(yr[k], "01-01", sep="-")) & date <= as.Date(paste(yr[k], "04-01", sep="-")) ) %>% 
    select(sunlight_duration) 
  
  sunhr.dc[k] <- sum(sunlist.dc[[k]]) 
  
  d <- as.integer(as.Date(paste(yr[[k]],"04-02",sep="-")) - as.Date(paste(yr[[k]],"01-01",sep="-")))
  dc.sun.fut[k] <- sunhr.dc[k] / (60 * d)
}


# average sunlight hours - Liestal
sunhr.sw <- NULL
sunlist.sw <- list(NULL)
sw.sun.fut <- NULL
yr <- seq(from=2022, to=2032, by=1)
for(k in 1:11) {
  sunlist.sw[[k]] <- sun_sw %>% 
    filter(date >= as.Date(paste(yr[k], "01-01", sep="-")) & date <= as.Date(paste(yr[k], "04-01", sep="-")) ) %>% 
    select(sunlight_duration) 
  
  sunhr.sw[k] <- sum(sunlist.sw[[k]])
  
  d <- as.integer(as.Date(paste(yr[[k]],"04-02",sep="-")) - as.Date(paste(yr[[k]],"01-01",sep="-")))
  sw.sun.fut[k] <- sunhr.sw[k] / (60 * d)
}


# average sunlight hours - Vancouver
sunhr.bc <- NULL
sunlist.bc <- list(NULL)
bc.sun.fut <- NULL
yr <- seq(from=2022, to=2032, by=1)
for(k in 1:11) {
  sunlist.bc[[k]] <- sun_bc %>% 
    filter(date >= as.Date(paste(yr[k], "01-01", sep="-")) & date <= as.Date(paste(yr[k], "04-01", sep="-")) ) %>% 
    select(sunlight_duration) 
  
  sunhr.sw[k] <- sum(sunlist.sw[[k]])
  
  d <- as.integer(as.Date(paste(yr[[k]],"04-02",sep="-")) - as.Date(paste(yr[[k]],"01-01",sep="-")))
  sw.sun.fut[k] <- sunhr.sw[k] / (60 * d)
}




# Sunlight hours, 2022-2032: May-Sep
# average sunlight hours - Kyoto
sunhr.ja <- NULL
sunlist.ja <- list(NULL)
ja.sunms.fut <- NULL
yr <- seq(from=2021, to=2031, by=1)
for(k in 1:length(yr)) {
  sunlist.ja[[k]] <- sun_ja %>% 
    filter(date >= as.Date(paste(yr[k], "05-01", sep="-")) & 
             date <= as.Date(paste(yr[k], "09-30", sep="-")) ) %>% 
    select(sunlight_duration) 
  
  sunhr.ja[k] <- sum(sunlist.ja[[k]])
  
  d <- as.integer(as.Date(paste(yr[[k]],"10-01",sep="-")) - as.Date(paste(yr[[k]],"05-01",sep="-")))
  ja.sunms.fut[k] <- sunhr.ja[k] / (60 * d )
}


# average sunlight hours - DC
sunhr.dc <- NULL
sunlist.dc <- list(NULL)
dc.sunms.fut <- NULL
yr1 <- seq(from=2021, to=2031, by=1)
for(k in 1:length(yr1)) {
  sunlist.dc[[k]] <- sun_dc %>% 
    filter(date >= as.Date(paste(yr[k], "05-01", sep="-")) & 
             date <= as.Date(paste(yr[k], "09-30", sep="-")) ) %>% 
    select(sunlight_duration) 
  
  sunhr.dc[k] <- sum(sunlist.dc[[k]]) 
  
  d <- as.integer(as.Date(paste(yr[[k]],"10-01",sep="-")) - as.Date(paste(yr[[k]],"05-01",sep="-")))
  dc.sunms.fut[k] <- sunhr.dc[k] / (60 * d)
}


# average sunlight hours - Liestal
sunhr.sw <- NULL
sunlist.sw <- list(NULL)
sw.sunms.fut <- NULL
yr <- seq(from=2021, to=2031, by=1)
for(k in 1:length(yr)) {
  sunlist.sw[[k]] <- sun_sw %>% 
    filter(date >= as.Date(paste(yr[k], "05-01", sep="-")) & 
             date <= as.Date(paste(yr[k], "09-30", sep="-")) ) %>% 
    select(sunlight_duration) 
  
  sunhr.sw[k] <- sum(sunlist.sw[[k]])
  
  d <- as.integer(as.Date(paste(yr[[k]],"10-01",sep="-")) - as.Date(paste(yr[[k]],"05-01",sep="-")))
  sw.sunms.fut[k] <- sunhr.sw[k] / (60 * d)
}


# average sunlight hours - Vancouver
sunhr.bc <- NULL
sunlist.bc <- list(NULL)
bc.sunms.fut <- NULL
yr <- seq(from=2021, to=2031, by=1)
for(k in 1:length(yr)) {
  sunlist.bc[[k]] <- sun_bc %>% 
    filter(date >= as.Date(paste(yr[k], "05-01", sep="-")) & 
             date <= as.Date(paste(yr[k], "09-30", sep="-")) ) %>% 
    select(sunlight_duration) 
  
  sunhr.bc[k] <- sum(sunlist.bc[[k]])
  
  d <- as.integer(as.Date(paste(yr[[k]],"10-01",sep="-")) - as.Date(paste(yr[[k]],"05-01",sep="-")))
  bc.sunms.fut[k] <- sunhr.bc[k] / (60 * d)
}



# plant hardiness zone for Vancouver
cherry.bc$hardiness.zone <- "8b"



view(cherry.ja)
view(cherry.dc)
view(cherry.sw)
view(cherry.bc)








########################################################################################################
########################################################################################################
########################################################################################################
###
###  DATA ANALYSIS AND MODELING
###
########################################################################################################
########################################################################################################
########################################################################################################




# Linear Models
#########################################################################
# Washington, DC
colnames(cherry.dc)

plot(cherry.dc$SLP, type="l")
numsim <- 10000
slp.smp <- NULL
set.seed(7)
for (k in 1:numsim){
  slp.smp[k] <- sample(cherry.dc[c(36:45,53:70), "SLP"], size=1)
}
plot(density(slp.smp))
set.seed(7)
v1 <- sample(slp.smp, size = 7)
cherry.dc$SLP[46:52] <- v1

cherry.dc.sub <- cherry.dc[cherry.dc$year >= 1973, 
                           c("sun", "sun.mar", "sun.maysep", "WDSP", "SLP", "PRCP", "SNWD", "Tavg", 
                             "TempDiffAbs", "heat.days", "chill.days", "ppd", "GDD", "bloom_doy")]

scatterplotMatrix(cherry.dc.sub)
cor(cherry.dc.sub)


# Stepwise Regression
lm1 <- lm(bloom_doy ~ sun.mar + sun.maysep + WDSP + SLP + PRCP + SNWD + Tavg + 
            TempDiffAbs + heat.days + chill.days + ppd + GDD,
          data=cherry.dc.sub, na.action = na.omit)
step.lm1 <- step(lm1, direction = "backward")

# Best subset selection
cherry.dc.pred <- cherry.dc.sub %>% filter(complete.cases(.))
cherry.dc.y <- cherry.dc.pred[,"bloom_doy"]
cherry.dc.pred <- within(cherry.dc.pred, rm("bloom_doy", "sun", "ppd"))

bestlm <- RegBest(y = cherry.dc.y, x  = cherry.dc.pred, 
                  na.action = na.omit, method="Cp", nbest=3)
bestlm$best
summary(regsubsets(y = cherry.dc.y, x = cherry.dc.pred, nbest=1 ))


# models
lm.b1 <- lm(bloom_doy ~ SNWD + WDSP + PRCP + SLP + TempDiffAbs + chill.days + heat.days + GDD, 
            data = cherry.dc.sub)
summary(lm.b1)

lm.b2 <- lm(bloom_doy ~ WDSP + SLP + PRCP + SNWD + TempDiffAbs + heat.days + chill.days + GDD,
            data=cherry.dc.sub)
summary(lm.b2)

lm.b3 <- lm(bloom_doy ~ SNWD + WDSP + PRCP + SLP + TempDiffAbs + chill.days + heat.days + GDD + sun.maysep,
            data=cherry.dc.sub)
summary(lm.b3)

lm.b4 <- lm(bloom_doy ~ SNWD + WDSP + PRCP + SLP + TempDiffAbs + chill.days + heat.days + GDD + sun.mar,
            data=cherry.dc.sub)
summary(lm.b4)


AIC(lm.b1, lm.b2, lm.b3, lm.b4)
# lm.b4 was provisionally selected as the best balanced model

# Diagnostics
vif(lm.b4)  # heat.days has high VIF
scatterplotMatrix(cherry.dc.sub)
cor(cherry.dc.sub)

# Backward stepwise model selection with heat.days removed
cherry.dc.subrm <- cherry.dc.sub %>% filter(complete.cases(.))
lm2 <- lm(bloom_doy ~ sun.mar + sun.maysep + WDSP + SLP + PRCP + SNWD + Tavg + 
            TempDiffAbs + chill.days + GDD,
          data=cherry.dc.subrm)
step(lm2, direction = "backward")

cherry.dc.pred <- within(cherry.dc.pred, rm("heat.days"))
bestlm <- RegBest(y = cherry.dc.y, x  = cherry.dc.pred, 
                  na.action = na.omit, method="adjr2", nbest=3)
bestlm$best


# Revised model with heat.days removed
lm.r1 <- lm(bloom_doy ~ SLP + PRCP + WDSP + Tavg + TempDiffAbs + chill.days + GDD,
            data=cherry.dc.sub)
summary(lm.r1)

lm.r2 <- lm(bloom_doy ~ SLP + PRCP + WDSP + Tavg + TempDiffAbs + chill.days + GDD + sun.mar + sun.maysep,
            data=cherry.dc.sub)
summary(lm.r2)

AIC(lm.r1, lm.r2)
anova(lm.r1, lm.r2)
# since lm.r1 and lm.r2 are not significantly different from each other and lm.r2 has higher adj r-squared, we go with lm.r2

# Diagnostics
vif(lm.r2) # VIF ok now for all variables
qqPlot(lm.r2)
plot(lm.r2)

# test for serial correlation
acf2(resid(lm.r2))
acf(resid(lm.r1))
pacf(resid(lm.r1))
acf2(resid(lm.r2)^2)
durbinWatsonTest(lm.r2)

attach(cherry.dc)
par(mfrow=c(1,1))
tyme <- seq(from=1921, to=2021, by=1)
plot(tyme, PBD, type="l")
plot(tyme, GDD, type="l")
plot(tyme, cherry.dc$sun.mar, type="l")
plot(tyme, cherry.dc$WDSP, type="l")
plot(tyme, chill.days, type="l")
plot(tyme, TempDiffAbs, type="l")
plot(tyme, Tavg, type="l")
plot(tyme, SLP, type="l")
plot(tyme, PRCP, type="l")
detach(cherry.dc)


# Time series analysis
# set store variables as time series for model
dc.bloom.ts <- ts(cherry.dc$bloom_doy[53:101])
gdd <- ts(cherry.dc$GDD[53:101])
sun.mar <- ts(cherry.dc$sun.mar[53:101])
wdsp <- ts(cherry.dc$WDSP[53:101])
chill <- ts(cherry.dc$chill.days[53:101])
tempdiff <- ts(cherry.dc$TempDiffAbs[53:101])
t_avg <- ts(cherry.dc$Tavg[53:101])
x_slp <- ts(cherry.dc$SLP[53:101])
prcp <- ts(cherry.dc$PRCP[53:101])
time.dc <- time(dc.bloom.ts)
heating <- ts(cherry.dc$heat.days[53:101])
year.ts <- ts(cherry.dc$year[53:101])
snow <-ts(cherry.dc$SNWD[53:101])
sun_ms <- ts(cherry.dc$sun.maysep[53:101])


dc.arima <- auto.arima(dc.bloom.ts, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                       seasonal=TRUE, method="ML",
                       xreg=cbind(x_slp, prcp, wdsp, t_avg, tempdiff, chill, gdd, sun.mar, sun_ms) )
summary(dc.arima)
dc.arima$arma

# ARIMA model diagnostics
# code and techniques borrowed from https://otexts.com/fpp2/regarima.html
cbind("Regression Errors" = residuals(dc.arima, type="regression"),
      "ARIMA errors" = residuals(dc.arima, type="innovation")) %>%
  autoplot(facets=TRUE)

checkresiduals(dc.arima)
# ARIMA residuals approximate white noise, are normally distributed, but the acf residual plot does not look good

dc.arima2 <- Arima(dc.bloom.ts, order = c(6, 0, 4), seasonal = list(order=c(6, 0, 3), period=5), 
                   method="ML", include.drift=TRUE,
                   xreg=cbind(x_slp, prcp, wdsp, t_avg, tempdiff, chill, gdd, sun.mar, sun_ms) )
dc.arima2$arma
checkresiduals(dc.arima2)
summary(dc.arima2)

dc.arima$aic
dc.arima2$aic
# final arima model is
summary(dc.arima2)
dc.arima2$arma

# predictions from ARIMA model
dc.arima.pred2 <- as.data.frame(forecast(dc.arima2, xreg=cbind(x_slp, prcp, wdsp, t_avg, tempdiff, 
                                                               chill, gdd, sun.mar, sun_ms)))[1:11,1]
dc.arima.pred <- as.data.frame(forecast(dc.arima, xreg=cbind(x_slp, prcp, wdsp, t_avg, tempdiff, 
                                                             chill, gdd, sun.mar, sun_ms)))[1:11,1]


# predict covariates
###################################################################################
cherry.dc.sub2 <- cherry.dc[cherry.dc$year >= 1973, 
                            c("year", "location", "sun", "sun.mar", "sun.maysep", "WDSP", "SLP", "PRCP", 
                              "SNWD", "Tavg", "TempDiffAbs", "heat.days", "chill.days", "ppd", "GDD",
                              "bloom_doy")]


# find most significant predictors for each covariate in the final model:
# sun.mar + WDSP + chill.days + Tavg + SLP + PRCP + TempDiffAbs + GDD
# then fit linear regression model with arima structure on each covariate and 
# forecast out for 11 years

scatterplotMatrix(cherry.dc.sub)

# Tavg
##########
lm.c1 <- lm(Tavg ~ year + sun.maysep + sun.mar + PRCP + SLP + WDSP + chill.days + TempDiffAbs + GDD + heat.days, data=cherry.dc.sub2)
summary(lm.c1)
plot(lm.c1)
acf2(resid(lm.c1))
acf2(resid(lm.c1)^2)
durbinWatsonTest(lm.c1)
durbinWatsonTest(lm.c1)

lm.t1 <- auto.arima(t_avg, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, seasonal=TRUE, method="ML", xreg=cbind(sun.mar, sun_ms, wdsp, x_slp, prcp, tempdiff, chill, heating, gdd) )
summary(lm.t1)
lm.t1$arma
checkresiduals(lm.t1)
Tavg.fut <- as.data.frame(forecast(lm.t1, 
                                   xreg=cbind(sun.mar, sun_ms, wdsp, x_slp, prcp, tempdiff, chill, heating, gdd )))[1:11, 1]


# TempDiffAbs
###############
lm.c2 <- lm(TempDiffAbs ~ year + sun.maysep + sun.mar + PRCP + SLP + WDSP + Tavg + chill.days + heat.days + GDD, data=cherry.dc.sub2)
summary(lm.c2)
plot(lm.c2)
acf2(resid(lm.c2))
acf2(resid(lm.c2)^2)
durbinWatsonTest(lm.c2)
durbinWatsonTest(lm.c2)

lm.t2 <- auto.arima(tempdiff, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, seasonal=TRUE, method="ML",
                    xreg=cbind(sun.mar, sun_ms, wdsp, x_slp, prcp, t_avg, chill, heating, gdd) )
summary(lm.t2)
lm.t2$arma
checkresiduals(lm.t2)
TempDiffAbs.fut <- as.data.frame(forecast(lm.t2, 
                                          xreg=cbind(sun.mar, sun_ms, wdsp, x_slp, prcp, t_avg, chill, heating, gdd)))[1:11, 1]


# PRCP
############
lm.c3 <- lm(PRCP ~ year + sun.maysep + sun.mar + TempDiffAbs + SLP + WDSP + chill.days + Tavg + GDD + heat.days, data=cherry.dc.sub2)
summary(lm.c3)
plot(lm.c3)
acf2(resid(lm.c3))
acf2(resid(lm.c3)^2)
durbinWatsonTest(lm.c3)
durbinWatsonTest(lm.c3)

lm.t3 <- auto.arima(prcp, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, seasonal=TRUE, method="ML", 
                    xreg=cbind(sun.mar, sun_ms, wdsp, x_slp, tempdiff, t_avg, chill, heating, gdd) )

summary(lm.t3)
lm.t3$arma
checkresiduals(lm.t3)
PRCP.fut <- as.data.frame(forecast(lm.t3, 
                                   xreg=cbind(sun.mar, sun_ms, wdsp, x_slp, tempdiff, t_avg, chill, heating, gdd)))[1:11, 1]


# SLP
#############
lm.c4 <- lm(SLP ~ year + sun.maysep + sun.mar + TempDiffAbs + Tavg + PRCP + wdsp + chill.days + heat.days + GDD, data=cherry.dc.sub2)
summary(lm.c4)
plot(lm.c4)
acf2(resid(lm.c4))
acf2(resid(lm.c4)^2)
durbinWatsonTest(lm.c4)
durbinWatsonTest(lm.c4)

lm.t4 <- auto.arima(x_slp, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                    seasonal=TRUE, method="ML",
                    xreg=cbind(sun.mar, sun_ms, wdsp, prcp, tempdiff, t_avg, chill, heating, gdd) )
summary(lm.t4)
lm.t4$arma
checkresiduals(lm.t4)
SLP.fut <- as.data.frame(forecast(lm.t4, 
                                  xreg=cbind(sun.mar, sun_ms, wdsp, prcp, tempdiff, t_avg, chill, heating, gdd)))[1:11, 1]


# WDSP
############
lm.c5 <- lm(wdsp ~ year + sun.maysep + sun.mar + TempDiffAbs + Tavg + PRCP + SLP + chill.days + heat.days + GDD, data=cherry.dc.sub2)
summary(lm.c5)
plot(lm.c5)
acf2(resid(lm.c5))
acf2(resid(lm.c5)^2)
durbinWatsonTest(lm.c5)
durbinWatsonTest(lm.c5)

lm.t5 <- auto.arima(wdsp, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                    seasonal=TRUE, method="ML",
                    xreg=cbind(sun.mar, sun_ms, x_slp, prcp, tempdiff, t_avg, chill, heating, gdd) )
summary(lm.t5)
lm.t5$arma
checkresiduals(lm.t5)
WDSP.fut <- as.data.frame(forecast(lm.t5, 
                                   xreg=cbind(sun.mar, sun_ms, x_slp, prcp, tempdiff, t_avg, chill, heating, gdd)))[1:11, 1]


# chill.days
#################
lm.c6 <- lm(log(chill.days) ~ year + sun.maysep + sun.mar + TempDiffAbs + Tavg + PRCP + SLP + WDSP + heat.days + GDD, data=cherry.dc.sub2)
summary(lm.c6)
plot(lm.c6)
acf2(resid(lm.c6))
acf2(resid(lm.c6)^2)
durbinWatsonTest(lm.c6)
durbinWatsonTest(lm.c6)

lm.t6 <- auto.arima(chill, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                    seasonal=TRUE, method="ML",
                    xreg=cbind(sun.mar, sun_ms, x_slp, prcp, tempdiff, t_avg, wdsp, heating, gdd) )
summary(lm.t6)
lm.t6$arma
checkresiduals(lm.t6)
chill.days.fut <- as.data.frame(forecast(lm.t6, 
                                         xreg=cbind(sun.mar, sun_ms, x_slp, prcp, tempdiff, t_avg, wdsp, heating, gdd)))[1:11, 1]


# GDD
###############
lm.c7 <- lm(GDD ~ year + sun.maysep + sun.mar + TempDiffAbs + Tavg + PRCP + SLP + WDSP + heat.days + chill.days, data=cherry.dc.sub2)
summary(lm.c7)
plot(lm.c7)
acf2(resid(lm.c7))
acf2(resid(lm.c7)^2)
durbinWatsonTest(lm.c7)
durbinWatsonTest(lm.c7)

lm.t7 <- auto.arima(gdd, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                    seasonal=TRUE, method="ML",
                    xreg=cbind(sun.mar, sun_ms, x_slp, prcp, tempdiff, t_avg, wdsp, heating, chill) )
summary(lm.t7)
lm.t7$arma
checkresiduals(lm.t7)
GDD.fut <- as.data.frame(forecast(lm.t7, 
                                  xreg=cbind(sun.mar, sun_ms, x_slp, prcp, tempdiff, t_avg, wdsp, heating, chill)))[1:11, 1]


# SNWD
############
lm.c8 <- lm(log(SNWD+0.01) ~ year + sun.maysep + sun.mar + TempDiffAbs + Tavg + PRCP + SLP + chill.days + heat.days + GDD + WDSP, data=cherry.dc.sub2)
summary(lm.c8)
plot(lm.c8)
acf2(resid(lm.c8))
acf2(resid(lm.c8)^2)
durbinWatsonTest(lm.c8)
durbinWatsonTest(lm.c8)

data1 <- cherry.dc.sub2[-c(26),]
plot(data1$year, log(data1$SNWD+.01))
plot(data1$sun.maysep, log(data1$SNWD+.01))
plot(data1$sun.mar, log(data1$SNWD+.01))
plot(data1$TempDiffAbs, log(data1$SNWD+.01))
plot(data1$Tavg, log(data1$SNWD+.01))
plot(data1$PRCP, log(data1$SNWD+.01))
plot(data1$SLP, log(data1$SNWD+.01))
plot(data1$chill.days, log(data1$SNWD+.01))
plot(data1$heat.days, log(data1$SNWD+.01))
plot(data1$GDD, log(data1$SNWD+.01))

lm.t8 <- auto.arima(snow, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                    seasonal=TRUE, method="ML",
                    xreg=cbind(sun.mar, sun_ms, x_slp, prcp, wdsp, tempdiff, t_avg, chill, heating, gdd) )
summary(lm.t8)
lm.t8$arma
checkresiduals(lm.t8)
SNWD.fut <- as.data.frame(forecast(lm.t8, 
                                   xreg=cbind(sun.mar, sun_ms, x_slp, prcp, wdsp, tempdiff, t_avg, chill, 
                                              heating, gdd)))[1:11, 1]



# Forecasts: Washington, DC
########################################################################################
summary(lm.r1)
summary(lm.r2)
fut.data <- data.frame(SLP=SLP.fut, PRCP=PRCP.fut, WDSP=WDSP.fut, TempDiffAbs=TempDiffAbs.fut, 
                       Tavg=Tavg.fut, chill.days=chill.days.fut, GDD=GDD.fut)
fut.data2 <- data.frame(SLP=SLP.fut, PRCP=PRCP.fut, WDSP=WDSP.fut, TempDiffAbs=TempDiffAbs.fut, 
                        Tavg=Tavg.fut, chill.days=chill.days.fut, GDD=GDD.fut, sun.mar=dc.sun.fut, 
                        sun.maysep=dc.sunms.fut)

# forecast bloom DOYs from linear models
dc.pred.lmr1 <- predict(lm.r1, newdata=fut.data)
dc.pred.lmr2 <- predict(lm.r2, newdata=fut.data2)
# predictions from linear models do not look plausible

# forecast bloom DOYs from ARIMA models
dc.arima.pred2 <- as.data.frame(forecast(dc.arima2, xreg=cbind(x_slp, prcp, wdsp, t_avg, tempdiff, 
                                                               chill, gdd, sun.mar, sun_ms)))[1:11,1]
dc.arima.pred <- as.data.frame(forecast(dc.arima, xreg=cbind(x_slp, prcp, wdsp, t_avg, tempdiff, 
                                                             chill, gdd, sun.mar, sun_ms)))[1:11,1]

# Root Mean Square Errors for each model
rmse(cherry.dc.sub$bloom_doy, fitted(lm.r1))  # linear lm.r1
rmse(cherry.dc.sub$bloom_doy, fitted(lm.r2))  # linear lm.r2
rmse(cherry.dc$bloom_doy[53:101], as.vector(dc.arima$fitted))  # ARIMA1 (0 0 0; 0 0 0), fr=1
rmse(cherry.dc$bloom_doy[53:101], as.vector(dc.arima2$fitted)) # ARIMA2 (6 0 3; 5 0 2), fr=1

# ARIMA2 has the lowest RMSE, ARIMA2 is our final model.
summary(dc.arima2)
dc.arima2$arma

# residuals plots
df3 <- data.frame(fitted=fitted(dc.arima2), residuals=residuals(dc.arima2))
ggplot(df3, aes(x=fitted, y=residuals)) +
  geom_point() 
qqPlot(residuals(dc.arima2))



# Forecast
#######################################################################
#Forecasted PBDs from Linear Model lm.r2
yr.f <- seq(from=2022, to=2032, by=1)
lm.r2.tbl <- data.frame(year=yr.f, dc.pred.lmr2)


# Final forecasted DOYs for 2022-2032 are:
dc.final.pred <- dc.arima.pred2
yr.f <- seq(from=2022, to=2032, by=1)
dc.tbl <- data.frame(year=yr.f, dc.final.pred)

# prediction for 2022: 
as.Date(dc.final.pred[1], origin="2022-01-01")





# Generalized Additive Model
##########################################
dc.gam <- gam(bloom_doy ~ s(WDSP) + s(PRCP) + s(Tavg) + s(GDD) + s(TempDiffAbs), 
              data=cherry.dc.sub, family=gaussian())
summary(dc.gam)
pacf(resid(dc.gam))



AIC(dc.gam)
acf(resid(dc.gam))
pacf(resid(dc.gam))
plot(dc.gam)
gam.check(dc.gam)
concurvity(dc.gam)

# PBD predictions from generalized additive model
dc.gam.pred <- predict(dc.gam, newdata=fut.data2)
dc.gam.pred.tbl <- data.frame(year=yr.f, GAM_forecast=dc.gam.pred)
dc.gam.pred.tbl

# RMSE for GAM model
rmse(cherry.dc.sub$bloom_doy, dc.gam$fitted.values)



dc.gam4 <- gamm(bloom_doy ~ s(sun) + s(sun.maysep) + s(Tavg), 
                data=cherry.dc.sub2, family=gaussian, 
                correlation = corAR1(form= ~1))
acf2(resid(dc.gam4))


r1 <- start_value_rho(dc.gam2, plot=TRUE)
nrow(cherry.dc.sub2)
cherry.dc.sub2$ar <- c(TRUE, rep(FALSE, 62))

dc.gam3 <- bam(bloom_doy ~ s(sun) + s(sun.maysep) + s(Tavg), 
               data=cherry.dc.sub2, rho=r1, AR.start=cherry.dc.sub2$ar)
acf2(resid(dc.gam3))



ggplot(cherry.dc.sub2, aes(sun, bloom_doy)) +
  geom_point()+
  stat_smooth(method = gam, formula = y ~ s(x))
ggplot(cherry.dc.sub2, aes(sun.maysep, bloom_doy)) +
  geom_point()+
  stat_smooth(method = gam, formula = y ~ s(x))
ggplot(cherry.dc.sub2, aes(Tavg, bloom_doy)) +
  geom_point()+
  stat_smooth(method = gam, formula = y ~ s(x))


cherry.sw.sub2 <- cherry.sw[60:122, c("sun", "sun.maysep", "Tavg", "PBD")]
sw.pre <- within(cherry.sw.sub2, rm("PBD"))
predict(dc.gam2, newdata = sw.pre)

predict(dc.gam2)




# Principle Component Regression Model
###########################################################
set.seed(7)
pcr.dc <- pcr(bloom_doy ~ Tavg + sun.maysep + GDD + I(sun.maysep^2) + sun + 
                I(sun^2) + I(sun^3), 
              data=cherry.dc.sub2, center = TRUE, scale = TRUE, validation = "LOO", jackknife = TRUE)
summary(pcr.dc)
pcr.dc$loadings
pcr.dc$coefficients
pcr.dc$Yloadings
validationplot(pcr.dc)
set.seed(7)
jack.test(pcr.dc)

set.seed(7)
pcr.dc2 <- pcr(bloom_doy ~ Tavg + sun.maysep + I(sun.maysep^2) + sun + 
                 I(sun^2) + I(sun^3), 
               data=cherry.dc.sub2, center = TRUE, scale = TRUE, validation = "LOO", jackknife = TRUE)
summary(pcr.dc2)
pcr.dc2$loadings
pcr.dc2$Yloadings
validationplot(pcr.dc2)
set.seed(7)
pcr.dc.b <- jack.test(pcr.dc2)


library(clusterSim)
cherry.sw.sub2 <- cherry.sw[60:122, c("sun", "sun.maysep", "Tavg", "PBD")]
bloom.norm2 <- data.Normalization(cherry.sw.sub2[,"PBD"], type="n1", normalization="column")
sw.pre <- within(cherry.sw.sub2, rm("PBD"))
colnames(sw.pre)
colnames(cherry.dc.sub2)
sw.pre$sun.maysepSq <- (sw.pre$sun.maysep)^2
sw.pre$sunlightSq <- (sw.pre$sun)^2
sw.pre$sunlightCu <- (sw.pre$sun)^3

predict(pcr.dc2, newdata = sw.pre )


bloom.norm <- data.Normalization(cherry.dc.sub2[,"bloom_doy"], type="n1", normalization="column")
cherry.pca <- within(cherry.dc.sub2, rm("bloom_doy"))
cherry.pca$sun.maysepSq <- (cherry.pca$sun.maysep)^2
cherry.pca$sunlightSq <- (cherry.pca$sun)^2
cherry.pca$sunlightCu <- (cherry.pca$sun)^3

detach("package:clusterSim", unload=TRUE)

pca.dc <- prcomp(cherry.pca, center=TRUE, scale=TRUE)
summary(pca.dc)
screeplot(pca.dc, type="line", main="Screeplot")
pca.dc$rotation

dc_load <- as.data.frame(pca.dc$x)
dc.pca.data <- cbind(bloom.norm, dc_load)
pca.dc.lm <- lm(bloom.norm ~ ., data = dc.pca.data)
summary(pca.dc.lm)

Z <- as.matrix(pca.dc.lm$coefficients[2:8])
R <- as.matrix(pca.dc$rotation)
beta.dc <- R %*% Z
beta.dc







#############################################################################################
## Kyoto, Japan
#############################################################################################
#############################################################################################
cherry.ja.sub <- cherry.ja[cherry.ja$year >= 1953, 
                           c("year", "sun.mar", "sun.maysep", "WDSP", "SLP", "PRCP", "TAVG", 
                             "Tavg", "heat.days", "chill.days", "GDD", "PBD", "pos.sun", 
                             "sun.time", "slap", "precip")]

tyme2 <- seq(from=1953, to=2021, by=1)
plot(tyme2, cherry.ja.sub$WDSP, type="l")
plot(tyme2, cherry.ja.sub$SLP, type="l")
plot(tyme2, cherry.ja.sub$PRCP, type="l")

bwid <- 3.49*sd(!is.na(cherry.ja.sub$SLP))*(sum(!is.na(cherry.ja.sub$SLP))^(-1/3)) / 10
hist(~SLP, data=cherry.ja.sub, w=.034)
hist(~SLP, data=cherry.ja.sub[1:42,], w=.04)

bwid <- 3.49*sd(!is.na(cherry.ja.sub$PRCP))*(sum(!is.na(cherry.ja.sub$PRCP))^(-1/3))
hist(~PRCP, data=cherry.ja.sub, w=20)

bwid <-3.49*sd(!is.na(cherry.ja.sub$WDSP))*(sum(!is.na(cherry.ja.sub$WDSP))^(-1/3))
hist(~WDSP, data=cherry.ja.sub, w=0.2)

# impute missing value for PRCP using arima modeled time series 
ts.prcp <- ts(cherry.ja$PRCP[52:122])
summary(auto.arima(ts.prcp, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                   seasonal=TRUE,method="ML"))
prcp.m1 <- Arima(ts.prcp, order = c(2, 1, 3), seasonal = list(order=c(2, 1, 2), period=3), 
                 method="ML")
prcp.m1$arma
checkresiduals(prcp.m1)
summary(prcp.m1)
# high RMSE

sd(cherry.ja$PRCP[52:122], na.rm=TRUE)
# sd even larger so we go with arima model anyway

prcp.m2 <- Arima(ts.prcp[1:54], order = c(2, 1, 3), seasonal = list(order=c(2, 1, 2), period=3), 
                 method="ML")
summary(prcp.m2)
cherry.ja[cherry.ja$year==2005,"PRCP"] <- as.vector(predict(prcp.m2, n.ahead=1)$pred)
cherry.ja.sub[cherry.ja.sub$year==2005,"PRCP"] <- as.vector(predict(prcp.m2, n.ahead=1)$pred)


# since SLP is approximately normally distributed white noise until 1994, sample missing values until 1994 from N(mu, sigma^2) distribution
st1 <- mean(cherry.ja.sub[cherry.ja.sub$year>=1953 & cherry.ja.sub$year<=1994, "SLP"], na.rm=TRUE)
st2 <- sd(cherry.ja.sub[cherry.ja.sub$year>=1953 & cherry.ja.sub$year<=1994, "SLP"], na.rm=TRUE)
sum(is.na(cherry.ja.sub[cherry.ja.sub$year>=1953 & cherry.ja.sub$year<=1994, "SLP"]))

set.seed(7)
st3 <- rnorm(11, mean=st1, sd=st2)
cherry.ja[is.na(cherry.ja$SLP), "year"]

cherry.ja[cherry.ja$year>=1965 & cherry.ja$year<=1972, "SLP"] <- st3[1:8]
cherry.ja.sub[cherry.ja.sub$year>=1965 & cherry.ja.sub$year<=1972, "SLP"] <- st3[1:8]
cherry.ja[cherry.ja$year==1975, "SLP"] <- st3[9]
cherry.ja.sub[cherry.ja.sub$year==1975, "SLP"] <- st3[9]
cherry.ja[cherry.ja$year>=1988 & cherry.ja$year<=1989, "SLP"] <- st3[10:11]
cherry.ja.sub[cherry.ja.sub$year>=1988 & cherry.ja.sub$year<=1989, "SLP"] <- st3[10:11]

# less variability and clear moving average trend after 1994. So fit MA() model to predict missing values after 1994
ts.slp <- ts(cherry.ja.sub[cherry.ja.sub$year>=1995 & cherry.ja.sub$year<=2021, "SLP"])
slp.m1 <- Arima(ts.slp, order = c(0, 0, 2), seasonal = list(order=c(0, 0, 0), period=1), 
                method="ML")
checkresiduals(slp.m1)
summary(slp.m1)

slp.m2 <- Arima(ts.slp[1:10], order = c(0, 0, 2), seasonal = list(order=c(0, 0, 0), period=1), 
                method="ML")
summary(slp.m2)
cherry.ja[cherry.ja$year==2005, "SLP"] <- as.vector(predict(slp.m2, n.ahead=1)$pred)
cherry.ja.sub[cherry.ja.sub$year==2005, "SLP"] <- as.vector(predict(slp.m2, n.ahead=1)$pred)


# model missing values for WDSP since 1973 as time series with drift
ts.wdsp <- ts(cherry.ja.sub[cherry.ja.sub$year>=1973, "WDSP"])

wdsp.m1 <- Arima(ts.wdsp, order = c(0, 0, 2), seasonal = list(order=c(0, 0, 1), period=4), 
                 method="ML", include.drift=TRUE)
checkresiduals(wdsp.m1)
summary(wdsp.m1)

cherry.ja.sub[is.na(cherry.ja.sub$WDSP), "year"] # missing value years

wdsp.m2 <- Arima(ts.wdsp[1:35], order = c(0, 0, 2), seasonal = list(order=c(0, 0, 1), period=4), 
                 method="ML", include.drift=TRUE)
summary(wdsp.m2)

wdsp.m3 <- Arima(ts.wdsp[1:52], order = c(0, 0, 2), seasonal = list(order=c(0, 0, 1), period=4), 
                 method="ML", include.drift=TRUE)
summary(wdsp.m3)

# predicted values
cherry.ja.sub[cherry.ja.sub$year==1988, "WDSP"] <- cherry.ja[cherry.ja$year==1988, "WDSP"] <-
  predict(wdsp.m2, n.ahead=2)$pred[1,1] + predict(wdsp.m2, n.ahead=2)$pred[1,2]

cherry.ja.sub[cherry.ja.sub$year==1989, "WDSP"] <- cherry.ja[cherry.ja$year==1989, "WDSP"] <-
  predict(wdsp.m2, n.ahead=2)$pred[2,1] + predict(wdsp.m2, n.ahead=2)$pred[2,2]

cherry.ja.sub[cherry.ja.sub$year==2005, "WDSP"] <- cherry.ja[cherry.ja$year==2005, "WDSP"] <-
  predict(wdsp.m3, n.ahead=1)$pred[1] + predict(wdsp.m3, n.ahead=1)$pred[2]

cherry.ja.sub[cherry.ja.sub$year==1975, "WDSP"] <- cherry.ja[cherry.ja$year==1975, "WDSP"] <-
  mean(wdsp.m1$fitted, na.rm=TRUE)


# TAVG
for (k in 1:nrow(cherry.ja)) {
  ifelse(!is.na(cherry.ja$TMIN[k]) & !is.na(cherry.ja$TMAX[k]),
         {ifelse(is.na(cherry.ja$TAVG[k]), 
                 cherry.ja$TAVG[k] <- (cherry.ja$TMAX[k] + cherry.ja$TMIN[k]) / 2,
                 cherry.ja$TAVG[k] <- cherry.ja$TAVG[k])},
         cherry.ja$TAVG[k] <- cherry.ja$TAVG[k])
}
cherry.ja.sub$TAVG <- cherry.ja[cherry.ja$year>=1953, "TAVG"]

plot(tyme2, cherry.ja.sub$TAVG, type="l")

# use time series to predict missing value for 2005
ts.tavg <- ts(cherry.ja.sub$TAVG)

tavg.m1 <- Arima(ts.tavg, order = c(0, 1, 1), seasonal = list(order=c(0, 1, 1), period=8), 
                 method="ML")
checkresiduals(tavg.m1)
summary(tavg.m1)

tavg.m2 <- Arima(ts.tavg[1:52], order = c(0, 1, 1), seasonal = list(order=c(0, 1, 1), period=8), 
                 method="ML")
summary(tavg.m2)

# predicted value
cherry.ja[cherry.ja$year==2005, "TAVG"] <- cherry.ja.sub[cherry.ja.sub$year==2005, "TAVG"] <- 
  predict(tavg.m2, n.ahead=1)$pred[1]


# GDD
# use time series to predict missing value for 2005
plot(tyme2, cherry.ja.sub$GDD, type="l")
ts.gdd <- ts(cherry.ja.sub$GDD)

gdd.m1 <- auto.arima(ts.gdd)

gdd.m1 <- Arima(ts.gdd, order = c(2, 0, 2), seasonal = list(order=c(0, 0, 2), period=8), 
                method="ML", include.drift=TRUE)
checkresiduals(gdd.m1)
summary(gdd.m1)

gdd.m2 <- Arima(ts.gdd[1:52], order = c(2, 0, 2), seasonal = list(order=c(0, 0, 2), period=8), 
                method="ML", include.drift=TRUE)
summary(gdd.m2)

# predicted value
cherry.ja[cherry.ja$year==2005, "GDD"] <- cherry.ja.sub[cherry.ja.sub$year==2005, "GDD"] <- 
  predict(gdd.m2, n.ahead=1)$pred[1] + predict(gdd.m2, n.ahead=1)$pred[2]




# Model Selection
############################
# Stepwise Regression
lm2 <- lm(PBD ~ sun.mar + sun.maysep + SLP + PRCP + TAVG + heat.days + chill.days + GDD + 
            pos.sun + sun.time + slap + precip,
          data=cherry.ja.sub, na.action = na.omit)
summary(lm2)
step.lm2 <- step(lm2, direction = "backward")

lm3 <- lm(PBD ~ sun.mar + sun.maysep + SLP + PRCP + TAVG + heat.days + chill.days + GDD + 
            pos.sun + sun.time + slap + precip + WDSP,
          data=cherry.ja.sub[cherry.ja.sub$year>=1973,], na.action = na.omit)
summary(lm3)
step.lm3 <- step(lm3, direction = "backward")

# Best subset selection
cherry.ja.pred <- cherry.ja.sub
cherry.ja.y <- cherry.ja.pred[,"PBD"]
cherry.ja.pred <- within(cherry.ja.pred, rm("PBD", "year", "WDSP"))

bestlm2 <- RegBest(y = cherry.ja.y, x = cherry.ja.pred, 
                   na.action = na.omit, method="adjr2", nbest=3)
bestlm2$best
summary(regsubsets(y = cherry.ja.y, x = cherry.ja.pred, nbest=1))


scatterplotMatrix(cherry.ja.sub)
cor(cherry.ja.sub)

ja.lm1 <- lm(PBD ~ TAVG + heat.days + chill.days + GDD, data=cherry.ja.sub)
summary(ja.lm1)
vif(ja.lm1) # high VIF for heat.days and chill.days. Take heat.days out

lm4 <- lm(PBD ~ sun.mar + sun.maysep + SLP + PRCP + TAVG + chill.days + GDD + 
            pos.sun + sun.time + slap + precip,
          data=cherry.ja.sub, na.action = na.omit)
summary(lm4)
step.lm4 <- step(lm4, direction = "backward")


# revised models with heat.days taken out
ja.lm2 <- lm(PBD ~ TAVG + chill.days + GDD + sun.time, data=cherry.ja.sub)
summary(ja.lm2)
vif(ja.lm2) # VIF ok now for all variables

ja.lm3 <- lm(PBD ~ TAVG + chill.days + GDD + sun.time + slap, data=cherry.ja.sub)
summary(ja.lm3)

ja.lm4 <- lm(PBD ~ TAVG + chill.days + GDD + sun.time + slap + PRCP, data=cherry.ja.sub)
summary(ja.lm4)

ja.lm5 <- lm(PBD ~ TAVG + chill.days + GDD + sun.time + slap + PRCP + sun.time + pos.sun, 
             data=cherry.ja.sub)
summary(ja.lm5)

ja.lm6 <- lm(PBD ~ TAVG + chill.days + GDD + sun.time + slap + PRCP + sun.time + pos.sun + sun.maysep, 
             data=cherry.ja.sub)
summary(ja.lm6)

anova(ja.lm2, ja.lm3)
anova(ja.lm2, ja.lm4)
anova(ja.lm2, ja.lm5)
anova(ja.lm2, ja.lm6)
anova(ja.lm2, lm4)

AIC(ja.lm2, ja.lm3, ja.lm4, ja.lm5, ja.lm6)
# ja.lm3 is the best balanced model
# ja.lm3 is not significantly different from ja.lm2 or the saturated model
# ja.lm3 has the highest adj. R-squared
# ja.lm3 has an AIC very close to ja.lm2

AIC(ja.lm3)
plot(ja.lm3)
acf2(resid(ja.lm3))
acf2(resid(ja.lm3)^2)
durbinWatsonTest(ja.lm3)
durbinWatsonTest(ja.lm3)



# Linear model with ARIMA adjusted errors
#################################################
cherry.ja.sub <- cherry.ja[cherry.ja$year >= 1953, 
                           c("year", "sun.mar", "sun.maysep", "WDSP", "SLP", "PRCP", "TAVG", 
                             "TempDiffAbs", "heat.days", "chill.days", "GDD", "PBD", "pos.sun", 
                             "sun.time", "slap", "precip")]
{attach(cherry.ja)
  par(mfrow=c(1,1))
  tyme <- seq(from=1900, to=2021, by=1)
  plot(tyme, PBD, type="l")
  plot(tyme, GDD, type="l")
  plot(tyme, TAVG, type="l")
  plot(tyme, chill.days, type="l")
  plot(tyme, cherry.ja$sun.mar, type="l")
  plot(tyme, cherry.ja$sun.maysep, type="l")
  plot(tyme, cherry.ja$WDSP, type="l")
  plot(tyme, SLP, type="l")
  plot(tyme, PRCP, type="l")
  plot(tyme, pos.sun, type="l")
  plot(tyme, sun.time, type="l")
  plot(tyme, slap, type="l")
  plot(tyme, precip, type="l")
  detach(cherry.ja)}

# store variables as time series for model
ja.pbd.ts <- ts(cherry.ja$PBD[54:122])
gdd <- ts(cherry.ja$GDD[54:122])
t_avg <- ts(cherry.ja$TAVG[54:122])
x_slp <- ts(cherry.ja$SLP[54:122])
ts.slp <- ts(cherry.ja$slap[54:122])
prcp <- ts(cherry.ja$PRCP[54:122])
prec <- ts(cherry.ja$precip[54:122])
wdsp <- ts(cherry.ja$WDSP[54:122])
heating <- ts(cherry.ja$heat.days[54:122])
chill <- ts(cherry.ja$chill.days[54:122])
year.ts <- ts(cherry.ja$year[54:122])
snow <-ts(cherry.ja$SNWD[54:122])
sun.mar <- ts(cherry.ja$sun.mar[54:122])
sun_ms <- ts(cherry.ja$sun.maysep[54:122])
p_sun <- ts(cherry.ja$pos.sun[54:122])
sun_t <- ts(cherry.ja$sun.time[54:122])


ja.lm3 <- lm(PBD ~ TAVG + chill.days + GDD + sun.time + slap, data=cherry.ja.sub)
summary(ja.lm3)

# auto arima
ja.arima.auto <- auto.arima(ja.pbd.ts, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                            seasonal=TRUE, method="ML",
                            xreg=cbind(t_avg, chill, gdd, sun_t, ts.slp) )
summary(ja.arima.auto)
ja.arima.auto$arma

# AR(2) model diagnostics
# code and techniques borrowed from https://otexts.com/fpp2/regarima.html
cbind("Regression Errors" = residuals(ja.arima.auto, type="regression"),
      "ARIMA errors" = residuals(ja.arima.auto, type="innovation")) %>%
  autoplot(facets=TRUE)

checkresiduals(ja.arima.auto)
# AR(2) model residuals don't quite approximate white noise, are approximately normally distributed, and the acf residual plot does not look great


############################################################
############################################################
# Run Code Here from optimize_arima.R to Replicate Results
############################################################
############################################################

checkresiduals(bestmdl)
summary(bestmdl)


ja.arima <- Arima(ja.pbd.ts, order = c(5, 0, 4), seasonal = list(order=c(1, 0, 2), period=5), 
                  method="ML", include.drift=TRUE,
                  xreg=cbind(t_avg, chill, gdd, sun_t, ts.slp) )
ja.arima$arma
checkresiduals(ja.arima)
summary(ja.arima)

ja.arima.auto$aic
bestmdl$aic
ja.arima$aic

# residuals plots
df1 <- data.frame(fitted=fitted(ja.arima), residuals=residuals(ja.arima))
ggplot(df1, aes(x=fitted, y=residuals)) +
  geom_point() 
qqPlot(residuals(ja.arima))

# final SARIMA model is
summary(ja.arima)
ja.arima$arma
checkresiduals(ja.arima)


# Forecast
##############################################################
# predictions from SARIMA model
ja.arima.pred <- as.data.frame(forecast(ja.arima, xreg=cbind(t_avg, chill, gdd, sun_t, ts.slp)))[1:11,1]

fitjanomis <- as.vector(na_remove(ja.arima$fitted))
rmse(cherry.ja$PBD[c(54:73,75:90,92:122)],fitjanomis) #RMSE

# Final unadjusted forecasted DOYs for 2022-2032 are:
ja.final.pred <- ja.arima.pred - 7*(1-(0.8*(1/0.7)))
ja.pred.tbl <- data.frame(year=yr.f, ja.final.pred)

# forecast date for 2022: 
as.Date(ja.final.pred[1], origin="2022-01-01")





#############################################################################################
## Liestal, Switzerland
#############################################################################################
#############################################################################################
colnames(cherry.sw)
cherry.sw.sub <- cherry.sw[cherry.sw$year >= 1931, 
                           c("sun.mar", "sun.maysep", "PRCP", "SNWD", "Tavg", "TempDiffAbs", 
                             "heat.days", "chill.days", "GDD", "PBD")] 
cherry.sw.sub2 <- cherry.sw[cherry.sw$year >= 1954, 
                            c("sun.mar", "sun.maysep", "PRCP", "SNWD", "Tavg", "TempDiffAbs", 
                              "heat.days", "chill.days", "GDD", "PBD")] 


lm5 <- lm(PBD ~ sun.mar + sun.maysep + PRCP + SNWD + Tavg + TempDiffAbs + heat.days + chill.days + GDD,
          data=cherry.sw.sub, na.action = na.omit)
summary(lm5)
# Stepwise Regression
step.lm5 <- step(lm5, direction = "backward")

# Best subset selection
cherry.sw.pred <- cherry.sw[cherry.sw$year >= 1931, 
                            c("sun.mar", "sun.maysep", "PRCP", "SNWD", "Tavg", "TempDiffAbs", 
                              "heat.days", "chill.days", "GDD", "PBD")] %>% filter(complete.cases(.))
cherry.sw.y <- cherry.sw.pred[,"PBD"]
cherry.sw.pred <- within(cherry.sw.pred, rm("PBD"))

bestlm3 <- RegBest(y = cherry.sw.y, x = cherry.sw.pred, 
                   na.action = na.omit, method="adjr2", nbest=3)
bestlm3$best
summary(regsubsets(y = cherry.sw.y, x = cherry.sw.pred, nbest=1 ))

scatterplotMatrix(cherry.sw.sub)
cor(cherry.sw.sub)

# linear models
sw.lm1 <- lm(PBD ~ sun.maysep + TempDiffAbs + heat.days + chill.days + GDD, 
             data = cherry.sw.sub)
summary(sw.lm1)

sw.lm2 <- lm(PBD ~ sun.maysep + TempDiffAbs + heat.days + chill.days + GDD + SNWD, 
             data = cherry.sw.sub)
summary(sw.lm2)

vif(sw.lm1) # high VIF for heat.days. Remove heat.days

# Refit most covariates model without heat.days
lm6 <- lm(PBD ~ sun.mar + sun.maysep + PRCP + SNWD + Tavg + TempDiffAbs + chill.days + GDD,
          data=cherry.sw.sub, na.action = na.omit)
summary(lm6)

# Stepwise Regression
step.lm6 <- step(lm6, direction = "backward")

# Best subset selection
cherry.sw.pred <- cherry.sw[cherry.sw$year >= 1931, 
                            c("sun.mar", "sun.maysep", "PRCP", "SNWD", "Tavg", "TempDiffAbs", 
                              "chill.days", "GDD", "PBD")] %>% filter(complete.cases(.))
cherry.sw.y <- cherry.sw.pred[,"PBD"]
cherry.sw.pred <- within(cherry.sw.pred, rm("PBD"))

bestlm3 <- RegBest(y = cherry.sw.y, x = cherry.sw.pred, 
                   na.action = na.omit, method="adjr2", nbest=3)
bestlm3$best

bestlm4 <- RegBest(y = cherry.sw.y, x = cherry.sw.pred, 
                   na.action = na.omit, method="Cp", nbest=3)
bestlm4$best

cherry.sw.pred <- cherry.sw[cherry.sw$year >= 1954, 
                            c("sun.mar", "sun.maysep", "PRCP", "SNWD", "Tavg", "TempDiffAbs", 
                              "chill.days", "GDD", "PBD")] %>% filter(complete.cases(.))
cherry.sw.y <- cherry.sw.pred[,"PBD"]
cherry.sw.pred <- within(cherry.sw.pred, rm("PBD", "chill.days"))

bestlm5 <- RegBest(y = cherry.sw.y, x = cherry.sw.pred, 
                   na.action = na.omit, method="adjr2", nbest=3)
bestlm5$best

# Fit model with lowest AIC
sw.r1 <- lm(PBD ~ sun.maysep + TempDiffAbs + GDD, 
            data = cherry.sw.sub, na.action=na.omit)
summary(sw.r1)

sw.r2 <- lm(PBD ~ sun.maysep + TempDiffAbs + GDD + Tavg, 
            data = cherry.sw.sub, na.action=na.omit)
summary(sw.r2)

# Fit model with highest adj. R-squared
sw.r3 <- lm(PBD ~ sun.maysep + TempDiffAbs + GDD + Tavg + chill.days, 
            data = cherry.sw.sub, na.action=na.omit)
summary(sw.r3)

sw.r4 <- lm(PBD ~ sun.maysep + TempDiffAbs + GDD + Tavg + chill.days + sun.mar + PRCP, 
            data = cherry.sw.sub, na.action=na.omit)
summary(sw.r4)

AIC(sw.r1, sw.r2) # sw.r2 has the lowest AIC for 1931-2021 data
AIC(sw.r3, sw.r4) # sw.r3 has the lowest AIC for 1954-2021 data

# Compare models sw.r2 and sw.r3
rmse(cherry.sw.sub$PBD, sw.r2$fitted.values)
rmse(cherry.sw.sub[!is.na(cherry.sw.sub$chill.days),"PBD"], sw.r3$fitted.values)
# sw.r3 has the lower RMSE so we proceed with model sw.r3

# sw.r3 consistent with most complex model
anova(sw.r3, sw.r4)
anova(sw.r3, lm6)

# Diagnostics
vif(sw.r3)
plot(sw.r3)
acf2(resid(sw.r3))
acf2(resid(sw.r3)^2)
durbinWatsonTest(sw.r3)
durbinWatsonTest(sw.r3)

{attach(cherry.sw)
  par(mfrow=c(1,1))
  tyme <- seq(from=1900, to=2021, by=1)
  plot(tyme, PBD, type="l")
  plot(tyme, GDD, type="l")
  plot(tyme, Tavg, type="l")
  plot(tyme, TempDiffAbs, type="l")
  plot(tyme, cherry.sw$sun.maysep, type="l")
  plot(tyme, chill.days, type="l")
  plot(tyme, PRCP, type="l")
  plot(tyme, SNWD, type="l")
  plot(tyme, cherry.sw$sun.mar, type="l")
  plot(tyme, cherry.sw$WDSP, type="l")
  plot(tyme, SLP, type="l")
  detach(cherry.sw)}

# store variables as time series for model
sw.pbd.ts <- ts(cherry.sw$PBD[55:122])
gdd <- ts(cherry.sw$GDD[55:122])
t_avg <- ts(cherry.sw$Tavg[55:122])
tempdif <- ts(cherry.sw$TempDiffAbs[55:122])
ts.slp <- ts(cherry.sw$SLP[55:122])
prcp <- ts(cherry.sw$PRCP[55:122])
snow <-ts(cherry.sw$SNWD[55:122])
wdsp <- ts(cherry.sw$WDSP[55:122])
heating <- ts(cherry.sw$heat.days[55:122])
chill <- ts(cherry.sw$chill.days[55:122])
year.ts <- ts(cherry.sw$year[55:122])
sun.mar <- ts(cherry.sw$sun.mar[55:122])
sun_ms <- ts(cherry.sw$sun.maysep[55:122])

# original linear model
sw.r3 <- lm(PBD ~ sun.maysep + TempDiffAbs + GDD + Tavg + chill.days, 
            data = cherry.sw.sub, na.action=na.omit)

# auto.arima
sw.auto.arima <- auto.arima(sw.pbd.ts, max.p=5, max.d=3, max.q=5, max.P=5, max.D=3, max.Q=5, 
                            seasonal=TRUE, method="ML",
                            xreg=cbind(sun_ms, tempdif, gdd, t_avg, chill) )
checkresiduals(sw.auto.arima)
sw.auto.arima$arma
summary(sw.auto.arima)



############################################################
############################################################
# Run Code Here from optimize_arima.R to Replicate Results
############################################################
############################################################
# loop through models to find best possible model
checkresiduals(bestmdl)
summary(bestmdl)
bestmdl$arma

# manually adjust bestmdl
sw.arima <- Arima(sw.pbd.ts, order = c(1, 0, 0), seasonal = list(order=c(2, 0, 5), period=1), 
                  method="ML", include.drift=FALSE,
                  xreg=cbind(sun_ms, tempdif, gdd, t_avg, chill) )
sw.arima$arma
checkresiduals(sw.arima)
summary(sw.arima)

# analysis just on the time series PBD
sw.auto.arima2 <- auto.arima(sw.pbd.ts, max.p=5, max.d=3, max.q=5, max.P=5, max.D=3, max.Q=5, 
                             seasonal=TRUE, method="ML" )
checkresiduals(sw.auto.arima2)
sw.auto.arima2$arma
summary(sw.auto.arima2)


############################################################
############################################################
# Run Code Here from optimize_arima.R to Replicate Results
############################################################
############################################################
checkresiduals(bestmdl2)
summary(bestmdl2)
bestmdl2$arma

sw.arima2 <- Arima(sw.pbd.ts, order = c(0, 2, 0), seasonal = list(order=c(6, 2, 2), period=8), 
                   method="ML", include.drift=FALSE)
sw.arima2$arma
checkresiduals(sw.arima2)
summary(sw.arima2)


# Predict Covariates
###################################################
# Tavg
##########
lm.c1 <- lm(Tavg ~ year + sun.maysep + sun.mar + PRCP + SNWD + TempDiffAbs + chill.days + heat.days + GDD
            , data=cherry.sw.sub2)
summary(lm.c1)

lm.s1 <- auto.arima(t_avg, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                    seasonal=TRUE, method="ML",
                    xreg=cbind(sun.mar, sun_ms, snow, prcp, tempdif, chill, heating, gdd) )
lm.s1$arma
checkresiduals(lm.s1)
summary(lm.s1)

Tavg.futs <- as.data.frame(forecast(lm.s1, xreg=cbind(sun.mar, sun_ms, snow, prcp, tempdif, chill, 
                                                      heating, gdd )))[1:11, 1]

# TempDiffAbs
###############
lm.c2 <- lm(TempDiffAbs ~ year + sun.maysep + sun.mar + PRCP + SNWD + Tavg + chill.days + heat.days + GDD
            , data=cherry.sw.sub2)
summary(lm.c2)

lm.s2 <- auto.arima(tempdif, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                    seasonal=TRUE, method="ML",
                    xreg=cbind(sun.mar, sun_ms, snow, prcp, t_avg, chill, heating, gdd) )
summary(lm.s2)
lm.s2$arma
checkresiduals(lm.s2)
TempDiffAbs.futs <- as.data.frame(forecast(lm.s2, 
                                           xreg=cbind(sun.mar, sun_ms, snow, prcp, t_avg, chill, heating, gdd)))[1:11, 1]

# chill.days
#################
lm.c6 <- lm(chill.days ~ year + sun.maysep + sun.mar + PRCP + SNWD + TempDiffAbs + Tavg + heat.days + GDD
            , data=cherry.sw.sub2)
summary(lm.c6)
acf2(resid(lm.c6))

lm.s6 <- auto.arima(chill, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                    seasonal=TRUE, method="ML",
                    xreg=cbind(sun.mar, sun_ms, snow, prcp, t_avg, tempdif, heating, gdd) )
lm.s6$arma
checkresiduals(lm.s6)
summary(lm.s6)

chill.days.futs <- as.data.frame(forecast(lm.s6, xreg=cbind(sun.mar, sun_ms, snow, prcp, t_avg, tempdif, 
                                                            heating, gdd)))[1:11, 1]

# GDD
###############
lm.c7 <- lm(GDD ~ year + sun.maysep + sun.mar + PRCP + SNWD + TempDiffAbs + Tavg + heat.days + chill.days
            , data=cherry.sw.sub2)
summary(lm.c7)
acf2(resid(lm.c7))

lm.s7 <- auto.arima(gdd, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                    seasonal=TRUE, method="ML",
                    xreg=cbind(sun.mar, sun_ms, snow, prcp, t_avg, tempdif, heating, chill) )
lm.s7$arma
checkresiduals(lm.s7)
summary(lm.s7)

GDD.futs <- as.data.frame(forecast(lm.s7, xreg=cbind(sun.mar, sun_ms, snow, prcp, t_avg, tempdif, 
                                                     heating, chill)))[1:11, 1]

# store variables as time series for model
sw.pbd.ts2 <- ts(cherry.sw$PBD[32:122])
gdd2 <- ts(cherry.sw$GDD[32:122])
t_avg2 <- ts(cherry.sw$Tavg[32:122])
tempdif2 <- ts(cherry.sw$TempDiffAbs[32:122])
sun_ms2 <- ts(cherry.sw$sun.maysep[32:122])
# auto.arima
sw.auto.arima3 <- auto.arima(sw.pbd.ts2, max.p=5, max.d=3, max.q=5, max.P=5, max.D=3, max.Q=5, 
                             seasonal=TRUE, method="ML",
                             xreg=cbind(sun_ms2, tempdif2, gdd2, t_avg2) )
checkresiduals(sw.auto.arima)
sw.auto.arima$arma
summary(sw.auto.arima)


# comparison of SARIMA models (since 1954 data)
sw.auto.arima$aic
bestmdl$aic
sw.arima$aic

# sw.r3 model covariate projections
sw.r3
yr.futs <- seq(from=2022, to=2032, by=1)
futs.data <- data.frame(year=yr.futs, sun.maysep=sw.sunms.fut, TempDiffAbs=TempDiffAbs.futs, 
                        Tavg=Tavg.futs, chill.days=chill.days.futs, GDD=GDD.futs)
# projections from sw.r3
sw.r3.pred <- predict(sw.r3, newdata=futs.data)

fed1 <- anova(sw.r3)
tail(fed1$`Sum Sq`, n=1)
fed1$`Mean Sq`
fed2 <- summary(sw.r3)
fed2$r.squared


SSE <- tail(anova(sw.r3)$`Sum Sq`, n=1)
R2 <- summary(sw.r3)$r.squared

SST = SSE / (1 - R2)


sw.pbd.ts3 <- ts(sw.pbd.ts, frequency=5)
sw.r3n <- tslm(sw.pbd.ts3 ~ trend + season)
summary(sw.r3n)
plot(forecast(sw.r3n))

# predictions from PBD time series without covariates
sw.arima2.pred <- predict(sw.arima2, n.ahead=11)
forecast(sw.arima2, h=11)

# predictions from time series since 1931
forecast(sw.auto.arima3, xreg=cbind(sun_ms2, tempdif2, gdd2, t_avg2))

# RMSE
rmse(cherry.sw[cherry.sw$year>=1954, "PBD"][-c(62:63)], sw.auto.arima$fitted[-c(62:63)]) 
rmse(cherry.sw[cherry.sw$year>=1954, "PBD"][-c(62:63)], bestmdl$fitted[-c(62:63)]) 
rmse(cherry.sw[cherry.sw$year>=1954, "PBD"][-c(62:63)], sw.arima$fitted[-c(62:63)])
rmse(cherry.sw[cherry.sw$year>=1954 & !is.na(cherry.sw$chill.days), "PBD"], sw.r3$fitted.values)
rmse(cherry.sw[cherry.sw$year>=1954, "PBD"], sw.arima2$fitted)
rmse(cherry.sw[cherry.sw$year>=1931, "PBD"], sw.auto.arima3$fitted)
# sw.arima2 has the lowest RMSE but does not yield sensible forecasts and high variablity in errors at end of time series. sw.arima is the best balanced model

# final SARIMA model is
summary(sw.arima)
sw.arima$arma
checkresiduals(sw.arima)
sw.arima$call




# Forecast
##############################################################
# Forecasts from Linear Model sw.r3
sw.r3.pred.adj <- sw.r3.pred - ((1-(0.25*(1/0.7)))*7 - 0.5)
sw.pred.lm.tbl <- data.frame(year=yr.f, liestal=sw.r3.pred.adj)

# predictions from SARIMA model
sw.arima.pred <- as.data.frame(forecast(sw.arima, 
                                        xreg=cbind(sun_ms, tempdif, gdd, t_avg, chill)))[1:11,1]

# Final unadjusted forecasted DOYs for 2022-2032 are:
sw.final.pred <- sw.arima.pred - ((1-(0.25*(1/0.7)))*7 - 0.5)
sw.pred.tbl <- data.frame(year=yr.f, liestal=sw.final.pred)

# forecast date for 2022: 
as.Date(sw.final.pred[1], origin="2022-01-01")

# forecast does not appear sensible
plot(tyme, cherry.sw$PBD, type="l")






############################################################################################
# Vancouver, Canada
############################################################################################
############################################################################################
cherry.df <- cherry.dc[, c("year", "location", "lat", "long", "alt", "bloom_date", "bloom_doy", "PBD", "Adj.bloom.date", "hardiness.zone", "sun", "sun.maysep", "sun.mar", "WDSP", "SLP", "PRCP", "SNWD", "TMIN", "TMAX", "TAVG", "Tavg", "TempDiffAbs", "TempDiffSq", "ppd", "heat.days", "chill.days", "GDD", "tavg.m", "tmin.m", "tmax.m", "prcp.m", "gdd.m")] %>% 
  rbind(cherry.ja[, c("year", "location", "lat", "long", "alt", "bloom_date", "bloom_doy", "PBD", "Adj.bloom.date", "hardiness.zone", "sun", "sun.maysep", "sun.mar", "WDSP", "SLP", "PRCP", "SNWD", "TMIN", "TMAX", "TAVG", "Tavg", "TempDiffAbs", "TempDiffSq", "ppd", "heat.days", "chill.days", "GDD", "tavg.m", "tmin.m", "tmax.m", "prcp.m", "gdd.m")]) %>% 
  rbind(cherry.sw[, c("year", "location", "lat", "long", "alt", "bloom_date", "bloom_doy", "PBD", "Adj.bloom.date", "hardiness.zone", "sun", "sun.maysep", "sun.mar", "WDSP", "SLP", "PRCP", "SNWD", "TMIN", "TMAX", "TAVG", "Tavg", "TempDiffAbs", "TempDiffSq", "ppd", "heat.days", "chill.days", "GDD", "tavg.m", "tmin.m", "tmax.m", "prcp.m", "gdd.m")]) 

cherry.df$hardiness.zone <- factor(cherry.df$hardiness.zone,
                                   levels=c("7a", "7b", "9b"))
levels(cherry.df$hardiness.zone)<- c("7a", "7b", "8a", "8b", "9a", "9b")
cherry.df$hardiness.zone <- factor(cherry.df$hardiness.zone, ordered=TRUE,
                                   levels=c("7a", "7b", "8a", "8b", "9a", "9b"))

cherry.bc$hardiness.zone <- factor(cherry.bc$hardiness.zone,
                                   levels=c("8b"))
levels(cherry.bc$hardiness.zone)<- c("7a", "7b", "8a", "8b", "9a", "9b")
cherry.bc$hardiness.zone <- factor(cherry.bc$hardiness.zone, ordered=TRUE,
                                   levels=c("7a", "7b", "8a", "8b", "9a", "9b"))
cherry.bc$hardiness.zone <- factor("8b", ordered=TRUE,
                                   levels=c("7a", "7b", "8a", "8b", "9a", "9b"))

# select observations since 1956
cherry.df.s <- cherry.df[cherry.df$year >= 1956,]

summary(cherry.ja)
summary(cherry.dc)
summary(cherry.sw)


# Most complex model
all.m1 <- lm(PBD ~ lat + alt + hardiness.zone + location + sun.mar + sun.maysep + prcp.m + tavg.m + gdd.m, 
             data=cherry.df, na.action=na.omit) 
summary(all.m1)

# Stepwise Regression
step.lm7 <- step(all.m1, direction = "backward")

all.m2 <- lm(PBD ~ gdd.m + alt + sun.mar + sun.maysep + prcp.m + lat + tavg.m, data=cherry.df)
summary(all.m2)

all.m3 <- lm(PBD ~ sun.maysep + prcp.m + lat + tavg.m, data=cherry.df)
summary(all.m3)

vif(all.m2) 

all.m4 <- lm(PBD ~ gdd.m + alt + sun.mar + sun.maysep + prcp.m + tavg.m, data=cherry.df)
summary(all.m4) #remove lat
all.m5 <- lm(PBD ~ gdd.m + alt + prcp.m + lat + tavg.m, data=cherry.df)
summary(all.m5) # remove sun.mar and sun.maysep
AIC(all.m4, all.m5)
# remove lat
# go with all.m4


# Stepwise Regression
step.lm7 <- step(all.m4, direction = "backward")

all.m6 <- lm(PBD ~ gdd.m + alt + sun.mar + sun.maysep + prcp.m + tavg.m, data=cherry.df)
summary(all.m6)
vif(all.m6) # high VIF; remove sun.mar


all.m7 <- lm(PBD ~ gdd.m + alt + sun.maysep + prcp.m + tavg.m, data=cherry.df)
summary(all.m7)

# Stepwise Regression
step.lm8 <- step(all.m7, direction = "backward")

all.m8 <- lm(PBD ~ prcp.m + alt + sun.maysep +  tavg.m, data=cherry.df)
summary(all.m8)
vif(all.m8) # VIF high; remove alt

all.m9 <- lm(PBD ~ prcp.m + alt + sun.maysep + tavg.m + gdd.m, data=cherry.df)
summary(all.m9)
vif(all.m9)


all.m10 <- lm(PBD ~ prcp.m + sun.maysep + tavg.m + gdd.m, data=cherry.df)
summary(all.m10)

# Stepwise Regression
step.lm9 <- step(all.m10, direction = "backward")


all.lm <- lm(PBD ~ prcp.m + sun.maysep + tavg.m + gdd.m, data=cherry.df)
summary(all.lm)

all.lm1 <- lm(PBD ~ prcp.m + tavg.m + gdd.m , data=cherry.df)
summary(all.lm1)
vif(all.lm1)

AIC(all.lm, all.lm1)
anova(all.lm, all.lm1)

# Diagnostics
vif(all.lm)
plot(all.lm)
acf2(resid(all.lm))
acf2(resid(all.lm)^2)
durbinWatsonTest(all.lm)
durbinWatsonTest(all.lm)


# not working
all.lm <- gls(PBD ~ prcp.m + sun.maysep + tavg.m + gdd.m, data=cherry.df,
              correlation=corAR1(1, form= ~1 | location, p=2, q=1))
summary(all.lm)
acf2(resid(all.lm))



cherry.df.sub <- cherry.df[, c("location", "PBD", "prcp.m", "tavg.m", "gdd.m", "hardiness.zone")] %>% 
  filter(complete.cases(.))

nrow(cherry.df.sub)
nrow(cherry.df.sub[cherry.df.sub$location == "washingtondc", ])
nrow(cherry.df.sub[cherry.df.sub$location == "kyoto", ])
nrow(cherry.df.sub[cherry.df.sub$location == "liestal", ])

# RMSE of all.lm1
rmse(cherry.df.sub[cherry.df.sub$location == "washingtondc", "PBD"], all.lm$fitted.values[1:66])
rmse(cherry.df.sub[cherry.df.sub$location == "kyoto", "PBD"], all.lm$fitted.values[67:136])
rmse(cherry.df.sub[cherry.df.sub$location == "liestal", "PBD"], all.lm$fitted.values[137:208])


# Final Linear Model
####################
all.lm <- lm(PBD ~ prcp.m + sun.maysep + tavg.m + gdd.m, data=cherry.df)
summary(all.lm)



# Data for Vancouver
vancouver.data <- cherry.bc[ ,c("year", "prcp.m", "tavg.m", "gdd.m", "sun.maysep")]

# create time series
bc.prcp <- ts(cherry.bc$prcp.m)
bc.tavg <- ts(cherry.bc$tavg.m)
bc.gdd <- ts(cherry.bc$gdd.m)
bc.sms <- ts(cherry.bc$sun.maysep)
bc.hz <- ts(cherry.bc$hardiness.zone)


# predict prcp.m
bc.prcp.fut <- auto.arima(bc.prcp, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                          seasonal=TRUE, method="ML",
                          xreg=cbind(bc.tavg, bc.gdd) )
summary(bc.prcp.fut)
bc.prcp.fut$arma
bc.prcp.pred <- as.data.frame(forecast(bc.prcp.fut, xreg=cbind(bc.tavg, bc.gdd)))[1:11,1]


# predict tavg.m
bc.tavg.fut <- auto.arima(bc.tavg, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                          seasonal=TRUE, method="ML",
                          xreg=cbind(bc.prcp, bc.gdd) )
summary(bc.tavg.fut)
bc.tavg.fut$arma
bc.tavg.pred <- as.data.frame(forecast(bc.tavg.fut, xreg=cbind(bc.prcp, bc.gdd)))[1:11,1]


# predict gdd.m
bc.gdd.fut <- auto.arima(bc.gdd, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                         seasonal=TRUE, method="ML",
                         xreg=cbind(bc.prcp, bc.tavg) )
summary(bc.gdd.fut)
bc.gdd.fut$arma
bc.gdd.pred <- as.data.frame(forecast(bc.gdd.fut, xreg=cbind(bc.prcp, bc.tavg)))[1:11,1]


# forecasted covariates dataframe
yr.f <- seq(from=2022, to=2032, by=1)
pred.covs <- data.frame(year=yr.f, prcp.m=bc.tavg.pred, tavg.m=bc.tavg.pred, gdd.m=bc.gdd.pred, sun.maysep=bc.sunms.fut)
vancouver.data <- rbind(vancouver.data, pred.covs)



# Vancouver forecast for 2022-2032:
############################################################
predict.bc <- predict(all.lm, newdata = pred.covs)
predict.tbl <- data.frame(year=yr.f, vancouver_doy=predict.bc)
predict.tbl




# VAR Model
#############################################################

#################
# Load Libraries
#################
library(tseries)
library(vars)
library(fpp2)
library(MTS)


# make time series for PBDs
ts.pbd.ja <- ts(cherry.df.s[cherry.df.s$location=="kyoto", "PBD"])
ts.pbd.dc <- ts(cherry.df.s[cherry.df.s$location=="washingtondc", "PBD"])
ts.pbd.sw <- ts(cherry.df.s[cherry.df.s$location=="liestal", "PBD"])

cor.df <- data.frame(ts.pbd.ja, ts.pbd.dc, ts.pbd.sw)
cor(cor.df)

# test for cointegration
po.test(cbind(ts.pbd.ja, ts.pbd.dc, ts.pbd.sw))
# strong evidence that the series are cointegrated so series are appropriate for modelling with VAR

# test for stationarity
adf.test(ts.pbd.ja)
adf.test(ts.pbd.dc)
adf.test(ts.pbd.sw)
pp.test(ts.pbd.ja)
pp.test(ts.pbd.dc)
pp.test(ts.pbd.sw)
# we can reject the null hypothesis that the series have unit roots.
# PBDs are stationary series.
# Use VAR in levels.

zt <- data.frame(kyoto=cherry.ja[cherry.ja$year>=1954, "PBD"],
                 washingtondc=cherry.dc[cherry.dc$year>=1954, "sPBD"],
                 liestal=cherry.sw[cherry.sw$year>=1954, "PBD"])

cherry.var <- cherry.df[cherry.df$year >= 1954, ] %>% 
  select(location, year, PBD, Tavg, GDD, hardiness.zone, sun.maysep)

xt <- data.frame(GDD.ja=cherry.var[cherry.var$location=="kyoto", "GDD"],
                 GDD.dc=cherry.var[cherry.var$location=="washingtondc", "GDD"],
                 GDD.sw=cherry.var[cherry.var$location=="liestal", "GDD"])

VARselect(as.matrix(zt, nrow=68))
all.var <- VAR(as.matrix(zt, nrow=68), p=1)
all.var$bic



# Forecast with VAR Model
###########################################################################
VARpred(all.var, h=11)

# Forecasted dates for DC, Kyoto, and Liestal from VAR model
table1 <- as.data.frame(VARpred(all.var, h=11)[1])
cities <- data.frame(year=yr.f, table1)
cities


# Forecasted dates for Vancouver from VAR model
vancouver.var.preds <- data.frame(year=yr.f, vancouver_doy=rowMeans(table1))
vancouver.var.preds





VARX(zt=zt, p=1, include.mean=T)





boxplot(blooms$DC_doy, blooms$liestal_doy,
        xlab = c("Washington", "Liestal"))


for (i in 1:nrow(cherry.df)) {
  ifelse(is.na(cherry.df$location[i]), 
         cherry.df$location[i] <- "kyoto", 
         cherry.df$location[i] <- cherry.df$location[i])
}
for (i in 1:nrow(cherry.df)) {
  ifelse(cherry.df$location[i] == "kyoto", cherry.df$location[i] <- "Kyoto",
         ifelse(cherry.df$location[i] == "washingtondc", cherry.df$location[i] <- "Washington",
                ifelse(cherry.df$location[i] == "liestal", cherry.df$location[i] <- "Liestal", u<-2)))
}


cbPalette <- c("#000000", "#E69F00", "#56B4E9")
ggplot(cherry.df, aes(x=year, y=bloom_doy, group=location)) + 
  geom_line(aes(color=location), size=.75) + 
  scale_colour_manual(values=cbPalette) +
  labs(title="Fig. 1 - Historic Number of Days to Peak Bloom by Location",
       x="Year", y="Number of days to peak bloom")

cbPalette2 <- c("#009E73", "#E69F00", "#56B4E9")
ggplot(cherry.df, aes(x=location, y=bloom_doy)) + 
  geom_boxplot(fill=cbPalette2) + 
  labs(title="Fig. 2 - Boxplots of Number of Days to Peak Bloom by Location",
       x="Location", y="Number of days to peak bloom")

summary(cherry.df[cherry.df$location=="Washington", "bloom_doy"])



# Predictive R-squared
# @author Thomas Hopper
# code and methods for computing predicted R-squared are borrowed from: https://rpubs.com/RatherBit/102428
# and https://tomhopper.me/2014/05/16/can-we-do-better-than-r-squared/

# predicted residual sums of squares (PRESS)
PRESS <- function(linear.model) {
  PRESS <- sum( (residuals(linear.model)/(1-lm.influence(linear.model)$hat))^2 )
  return(PRESS)
}

# predicted R-squared
pred_r_squared <- function(linear.model) {
  TSS <- sum(anova(linear.model)$'Sum Sq')
  pred.r.squared <- 1-PRESS(linear.model)/(TSS)
  return(pred.r.squared)
}



# Washington, DC
#################
pred_r_squared(lm.r2)
summary(lm.r2)$r.squared

# Liestal
#############
pred_r_squared(sw.r3)
summary(sw.r3)$r.squared

# Vancouver
##############
pred_r_squared(all.lm)
summary(all.lm)$r.squared




