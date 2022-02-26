library(tidyverse)
library(rnoaa)
library(purrr)
library(dplyr)
library(Metrics)

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
                          year = if_else(month >= 9 | month == 0, year + 1L, year))

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

# parameter optimization: TC, RC, RH

# PART 1 (result : TC = 5, RC = -135, Rh = 275, MinRMSE = 5.280301)
RMSE = 0
Tc = 0
Rc = 0
Rh = 0
Yr = 0
x = 1

for (RH in seq(195,275,10)){
  for (RC in seq(-75,-175,-10)){
    for (TC in seq(0,10)){
      ppd = 0
      year = 0
      for (i in 1:y){
        Cd <- with(datal[[i]], ifelse(0 <= TC & TC <= tmin & tmin <= tmax, 0, 
                                      ifelse(0 <= tmin & tmin <= TC & TC < tmax,((tavg - tmin)-((tmax - TC)/2)),
                                             ifelse(0 <= tmin & tmin <= tmax & tmax <= TC, (tavg - tmin),
                                                    ifelse(tmin < 0 & 0 < tmax & tmax <= TC, ((tmax)/(tmax-tmin))*(tmax/2),
                                                           ifelse(tmin < 0 & 0 < TC & TC < tmax, (((tmax)/(tmax-tmin))*(tmax/2)-((tmax-TC)/2)), 0))))))
        Cd <- with(datal[[i]], ifelse(Cd < 0, Cd, Cd*(-1))) # chill day value should be negative
        tab <- cbind(Cd,datal[[i]])
        tab[,"cm_cd"] <- cumsum(tab$Cd)
        chill <- min(which(tab$cm_cd <= RC))  # number of chill days
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
        tab$Cd[hs:length(tab$Cd)] <- 0 
        tab$cm_cd[hs:length(tab$cm_cd)] <- 0 
        
        tab[,"Ca"] <- with(datal[[i]], ifelse(0 <= TC & TC <= tmin & tmin <= tmax, tavg-TC, 
                                              ifelse(0 <= tmin & tmin <= TC & TC < tmax,(tmax - TC)/2,
                                                     ifelse(0 <= tmin & tmin <= tmax & tmax <= TC, 0,
                                                            ifelse(tmin < 0 & 0 < tmax & tmax <= TC, 0,
                                                                   ifelse(tmin < 0 & 0 < TC & TC < tmax, (tmax - TC)/2, 0))))))
        tab$Ca <- with(tab, ifelse(Ca > 0, Ca, Ca*(-1))) # anti-chill day value should be positive
        tab$Ca[1:chill] <- 0 
        tab[,"cm_ca"] <- cumsum(tab$Ca)
        
        totdays <- min(which(tab$cm_ca >= RH)) # number of heat days
        if (!is.finite(totdays)) {
          ppd[i] = "NA"
          year[i] = datal[[i]][1,5]
          next
        }
        ppd[i] = totdays - 122
        year[i] = datal[[i]][1,5]
        
        print(i)
        print(TC)
        print(RC)
        print(RH)
      }
      year <- unlist(year)
      predpbd <- data.frame(year,ppd)
      predpbd <- subset(predpbd, ppd!="NA") # reference: https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
      predpbd$ppd <- as.numeric(predpbd$ppd)
      restab <- left_join(predpbd, publicpbd, by ="year") 
      
      RMSE[x] <- rmse(restab$ppd, restab$bloom_doy)
      Tc[x] = TC
      Rc[x] = RC
      Rh[x] = RH
      Yr[x] = nrow(restab)
      x = x+1
    }
  }
}
DF <- data.frame(RMSE, Tc, Rc, Rh, Yr)
DF <- subset(DF, Yr == y)
yi <- which.min((RMSE))
TCmin1 = Tc[[yi]]
RCmin1 = Rc[[yi]]
RHmin1 = Rh[[yi]]
RMSEmin1 = RMSE[[yi]]

TCmin1
RCmin1
RHmin1
RMSEmin1

# PART 2 (result : TC = 5, RC = -138, Rh = 280, MinRMSE = 5.211324)
RMSE = 0
Tc = 0
Rc = 0
Rh = 0
Yr = 0
x = 1

for (RH in seq(270,280)){
  for (RC in seq(-130,-140)){
    for (TC in seq(4,6)){
      ppd = 0
      year = 0
      for (i in 1:y){
        Cd <- with(datal[[i]], ifelse(0 <= TC & TC <= tmin & tmin <= tmax, 0, 
                                      ifelse(0 <= tmin & tmin <= TC & TC < tmax,((tavg - tmin)-((tmax - TC)/2)),
                                             ifelse(0 <= tmin & tmin <= tmax & tmax <= TC, (tavg - tmin),
                                                    ifelse(tmin < 0 & 0 < tmax & tmax <= TC, ((tmax)/(tmax-tmin))*(tmax/2),
                                                           ifelse(tmin < 0 & 0 < TC & TC < tmax, (((tmax)/(tmax-tmin))*(tmax/2)-((tmax-TC)/2)), 0))))))
        Cd <- with(datal[[i]], ifelse(Cd < 0, Cd, Cd*(-1))) # chill day value should be negative
        tab <- cbind(Cd,datal[[i]])
        tab[,"cm_cd"] <- cumsum(tab$Cd)
        chill <- min(which(tab$cm_cd <= RC))  # number of chill days
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
        tab$Cd[hs:length(tab$Cd)] <- 0 
        tab$cm_cd[hs:length(tab$cm_cd)] <- 0 
        
        tab[,"Ca"] <- with(datal[[i]], ifelse(0 <= TC & TC <= tmin & tmin <= tmax, tavg-TC, 
                                              ifelse(0 <= tmin & tmin <= TC & TC < tmax,(tmax - TC)/2,
                                                     ifelse(0 <= tmin & tmin <= tmax & tmax <= TC, 0,
                                                            ifelse(tmin < 0 & 0 < tmax & tmax <= TC, 0,
                                                                   ifelse(tmin < 0 & 0 < TC & TC < tmax, (tmax - TC)/2, 0))))))
        tab$Ca <- with(tab, ifelse(Ca > 0, Ca, Ca*(-1))) # anti-chill day value should be positive
        tab$Ca[1:chill] <- 0 
        tab[,"cm_ca"] <- cumsum(tab$Ca)
        
        totdays <- min(which(tab$cm_ca >= RH)) # number of heat days
        if (!is.finite(totdays)) {
          ppd[i] = "NA"
          year[i] = datal[[i]][1,5]
          next
        }
        ppd[i] = totdays - 122
        year[i] = datal[[i]][1,5]
        
        print(i)
        print(TC)
        print(RC)
        print(RH)
      }
      year <- unlist(year)
      predpbd <- data.frame(year,ppd)
      predpbd <- subset(predpbd, ppd!="NA") # reference: https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
      predpbd$ppd <- as.numeric(predpbd$ppd)
      restab <- left_join(predpbd, publicpbd, by ="year") 
      
      RMSE[x] <- rmse(restab$ppd, restab$bloom_doy)
      Tc[x] = TC
      Rc[x] = RC
      Rh[x] = RH
      Yr[x] = nrow(restab)
      x = x+1
    }
  }
}
DF <- data.frame(RMSE, Tc, Rc, Rh, Yr)
yi <- which.min((RMSE))
TCmin1 = Tc[[yi]]
RCmin1 = Rc[[yi]]
RHmin1 = Rh[[yi]]
RMSEmin1 = RMSE[[yi]]

TCmin1
RCmin1
RHmin1
RMSEmin1

# PART 3 (result : TC = 5, RC = -138, Rh = 280.2, MinRMSE = 5.203744)
RMSE = 0
Tc = 0
Rc = 0
Rh = 0
Yr = 0
x = 1

for (RH in seq(279.5,280.5, 0.1)){
  for (RC in seq(-137.5,-138.5, -0.1)){
    for (TC in seq(4.5,5.5,0.1)){
      ppd = 0
      year = 0
      for (i in 1:y){
        Cd <- with(datal[[i]], ifelse(0 <= TC & TC <= tmin & tmin <= tmax, 0, 
                                      ifelse(0 <= tmin & tmin <= TC & TC < tmax,((tavg - tmin)-((tmax - TC)/2)),
                                             ifelse(0 <= tmin & tmin <= tmax & tmax <= TC, (tavg - tmin),
                                                    ifelse(tmin < 0 & 0 < tmax & tmax <= TC, ((tmax)/(tmax-tmin))*(tmax/2),
                                                           ifelse(tmin < 0 & 0 < TC & TC < tmax, (((tmax)/(tmax-tmin))*(tmax/2)-((tmax-TC)/2)), 0))))))
        Cd <- with(datal[[i]], ifelse(Cd < 0, Cd, Cd*(-1))) # chill day value should be negative
        tab <- cbind(Cd,datal[[i]])
        tab[,"cm_cd"] <- cumsum(tab$Cd)
        chill <- min(which(tab$cm_cd <= RC))  # number of chill days
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
        tab$Cd[hs:length(tab$Cd)] <- 0 
        tab$cm_cd[hs:length(tab$cm_cd)] <- 0 
        
        tab[,"Ca"] <- with(datal[[i]], ifelse(0 <= TC & TC <= tmin & tmin <= tmax, tavg-TC, 
                                              ifelse(0 <= tmin & tmin <= TC & TC < tmax,(tmax - TC)/2,
                                                     ifelse(0 <= tmin & tmin <= tmax & tmax <= TC, 0,
                                                            ifelse(tmin < 0 & 0 < tmax & tmax <= TC, 0,
                                                                   ifelse(tmin < 0 & 0 < TC & TC < tmax, (tmax - TC)/2, 0))))))
        tab$Ca <- with(tab, ifelse(Ca > 0, Ca, Ca*(-1))) # anti-chill day value should be positive
        tab$Ca[1:chill] <- 0 
        tab[,"cm_ca"] <- cumsum(tab$Ca)
        
        totdays <- min(which(tab$cm_ca >= RH)) # number of heat days
        if (!is.finite(totdays)) {
          ppd[i] = "NA"
          year[i] = datal[[i]][1,5]
          next
        }
        ppd[i] = totdays - 122
        year[i] = datal[[i]][1,5]
        
        print(i)
        print(TC)
        print(RC)
        print(RH)
      }
      year <- unlist(year)
      predpbd <- data.frame(year,ppd)
      predpbd <- subset(predpbd, ppd!="NA") # reference: https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
      predpbd$ppd <- as.numeric(predpbd$ppd)
      restab <- left_join(predpbd, publicpbd, by ="year") 
      
      RMSE[x] <- rmse(restab$ppd, restab$bloom_doy)
      Tc[x] = TC
      Rc[x] = RC
      Rh[x] = RH
      Yr[x] = nrow(restab)
      x = x+1
    }
  }
}
DF <- data.frame(RMSE, Tc, Rc, Rh, Yr)
yi <- which.min((RMSE))
TCmin1 = Tc[[yi]]
RCmin1 = Rc[[yi]]
RHmin1 = Rh[[yi]]
RMSEmin1 = RMSE[[yi]]

TCmin1
RCmin1
RHmin1
RMSEmin1