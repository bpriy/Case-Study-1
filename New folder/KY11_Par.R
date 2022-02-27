library(tidyverse)
library(rnoaa)
library(purrr)
library(dplyr)
library(Metrics)

# public data on pbd 
cherry <- read.csv("data/kyoto.csv") 
publicpbd <- cherry %>% subset(select = c(year, bloom_doy))
publicpbd

data <- read.csv("data/kyoto_data.csv")

# split data by year
datal = split(data, data$year)

#################

# specify years
totyears = 1951:2021
y = length(totyears)

# parameter optimization: TC, RC, RH

# PART 1 (result : TC = 5, RC = -75, Rh = 195; MinRMSE = 21.31364)
RMSE = 0
Tc = 0
Rc = 0
Rh = 0
Yr = 0
x = 1

for (RH in seq(195,275, 10)){
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
        ppd[i] = totdays - 61
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
DF <- subset(DF, Yr > y-2)
yi <- which.min((RMSE))
TCmin1 = Tc[[yi]]
RCmin1 = Rc[[yi]]
RHmin1 = Rh[[yi]]
RMSEmin1 = RMSE[[yi]]

TCmin1
RCmin1
RHmin1
RMSEmin1

# PART 2 (result : TC = 5, RC = -70, Rh = 190; MinRMSE = 20.15724)
RMSE = 0
Tc = 0
Rc = 0
Rh = 0
Yr = 0
x = 1

for (RH in seq(190,200)){
  for (RC in seq(-70,-80)){
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
        ppd[i] = totdays - 61
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
DF <- subset(DF, Yr > y-2)
yi <- which.min((RMSE))
TCmin1 = Tc[[yi]]
RCmin1 = Rc[[yi]]
RHmin1 = Rh[[yi]]
RMSEmin1 = RMSE[[yi]]

TCmin1
RCmin1
RHmin1
RMSEmin1
