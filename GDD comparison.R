
##########################################
## Load Libraries      
##########################################
library(rnoaa)
library(chillR)
library(tidyverse)


##########################################
## Import Response Variable      
##########################################
cherry_bloom_data <- read.csv("data/washingtondc.csv") %>% 
  bind_rows(read.csv("data/liestal.csv")) %>% 
  bind_rows(read.csv("data/kyoto.csv"))


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

bd.ja <- cherry.ja$bloom_doy
bd.dc <- cherry.dc$bloom_doy
bd.sw <- cherry.sw$bloom_doy


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



# DATA FOR WASHINGTON, DC (DAILY)
##############################
# GHCN Daily
dc.ghcn <- read.csv("data/dc_ghcn_daily.csv", header=TRUE)
dc.ghcn$DATE <- as.Date(dc.ghcn$DATE)
dc.ghcn <- dc.ghcn %>% separate(col=DATE, into=c("year", "month", "day"), remove=FALSE)
dc.ghcn$year <- as.integer(dc.ghcn$year)
dc.ghcn$month <- as.integer(dc.ghcn$month)
dc.ghcn$day <- as.integer(dc.ghcn$day)


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

basel1$TMIN <- basel1$TMIN / 10
basel1$TMAX <- basel1$TMAX / 10
basel1$TAVG <- basel1$TAVG / 10

for (g in 40990:nrow(basel1)) {
  ifelse(is.na(basel1$PRCP[g]), 
         basel1$PRCP[g] <- basel2[basel2$DATE == basel1$DATE[g], 22], 
         basel1$PRCP[g] <- basel1$PRCP[g] )
  ifelse(is.na(basel1$PRCP[g]), 
         basel1$PRCP[g] <- basel2[basel2$DATE == basel1$DATE[g], 22], 
         basel1$PRCP[g] <- basel1$PRCP[g] )
}

liestal.ghcn <- basel1


### Run your code for 10_GDD


# PPD DC
yr1 <- data.frame(year = seq(from=1921, to=2021, by=1))
tab <- merge(restab, yr1, by="year", all=TRUE)
cherry.dc$ppd <- tab[,2]
cherry.dc$heat.days <- tab[,3]
cherry.dc$chill.days <- tab[,4]

# PPD Liestal
yr <- data.frame(year = seq(from=1900, to=2021, by=1))
tab <- merge(restab, yr, by="year", all=TRUE)
cherry.sw$ppd <- tab[,2]
cherry.sw$heat.days <- tab[,3]
cherry.sw$chill.days <- tab[,4]
cherry.sw$heat.days <- as.numeric(cherry.sw$heat.days)
cherry.sw$chill.days <- as.numeric(cherry.sw$chill.days)


# PPD Kyoto
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
colnames(cherry.dc)
summary(cherry.dc)

lm.dc1 <- lm(bloom_doy ~ 
               heat.days +
               chill.days +
               sunlight.duration +
               sun.maysep +
               WDSP +
               SLP +
               PRCP +
               SNWD +
               GDD
             , data=cherry.dc.subset)
summary(lm.dc1)


# GDD is best predictor for response
m1 <- lm(bloom_doy ~ ppd, data=cherry.dc)
m2 <- lm(bloom_doy ~ heat.days + chill.days, data=cherry.dc)
m3 <- lm(bloom_doy ~ heat.days * chill.days, data=cherry.dc)
m4 <- lm(bloom_doy ~ GDD, data=cherry.dc)
summary(m1)
summary(m2)
summary(m3)
summary(m4)

AIC(m1, m2, m3, m4)

# Even if we subset the day to more recent years, GDD is the better predictor
cherry.dc.subset4 <- cherry.dc[cherry.dc$year >= 2000,]

m1 <- lm(bloom_doy ~ ppd, data=cherry.dc.subset4)
m2 <- lm(bloom_doy ~ heat.days + chill.days, data=cherry.dc.subset4)
m3 <- lm(bloom_doy ~ heat.days * chill.days, data=cherry.dc.subset4)
m4 <- lm(bloom_doy ~ GDD, data=cherry.dc.subset4)
summary(m1)
summary(m2)
summary(m3)
summary(m4)

AIC(m1, m2, m3, m4)


# GDD is the best predictor for Liestal and Kyoto too
# Kyoto
cherry.ja.subset <- cherry.ja[cherry.ja$year >= 1973,]

m1 <- lm(bloom_doy ~ ppd, data=cherry.ja.subset)
m2 <- lm(bloom_doy ~ heat.days + chill.days, data=cherry.ja.subset)
m3 <- lm(bloom_doy ~ heat.days * chill.days, data=cherry.ja.subset)
m4 <- lm(bloom_doy ~ GDD, data=cherry.ja.subset)
summary(m1)
summary(m2)
summary(m3)
summary(m4)

AIC(m1, m2, m3, m4)


# Liestal
cherry.sw.subset <- cherry.sw[cherry.sw$year >= 1956 & cherry.sw$year <= 2014,]

m1 <- lm(bloom_doy ~ ppd, data=cherry.sw.subset)
m2 <- lm(bloom_doy ~ heat.days + chill.days, data=cherry.sw.subset)
m3 <- lm(bloom_doy ~ heat.days * chill.days, data=cherry.sw.subset)
m4 <- lm(bloom_doy ~ GDD, data=cherry.sw.subset)
summary(m1)
summary(m2)
summary(m3)
summary(m4)

AIC(m1, m2, m3, m4)

