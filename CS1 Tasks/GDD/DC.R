library(tidyverse)
library(rnoaa)
library(purrr)
library(dplyr)

cherry <- read.csv("data/washingtondc.csv") %>% 
  bind_rows(read.csv("data/liestal.csv")) %>% 
  bind_rows(read.csv("data/kyoto.csv"))
cherry
ch_obs <- cherry %>% subset(select = c(location, year, bloom_doy))
ch_obs

# parameter optimization: washington dc
ch_obs <- ch_obs[ch_obs$location %in% c("washingtondc"),]
publicpbd <- ch_obs %>% subset(select = c(year, bloom_doy))

dc_tm <- ghcnd_search(stationid = "USW00013743", var=c("tmin", "tmax"), ) 
dc_tm <- dc_tm %>% reduce(full_join, by='date') %>% subset(select = c(date, tmin, tmax))
dc_tm <- dc_tm %>% mutate(year = as.integer(format(date, "%Y")),
                                month = as.integer(strftime(date, '%m')) %% 12, # make December "0"
                                season = cut(month, breaks = c(0, 2, 5, 8, 11),
                                             include.lowest = TRUE,
                                             labels = c("Winter", "Spring", "Summer", "Fall")),
                                year = if_else(month >= 10 | month == 0, year + 1L, year))
# NA values: (code reference - https://stackoverflow.com/questions/53195961/count-total-missing-values-by-group)
dc_nas1 <- dc_tm %>%  group_by(year) %>% summarise_each(funs(sum(is.na(.))))


dc_ta <- ghcnd_search(stationid = "USW00013743", var=c("tavg"))
dc_list <- list(dc_tm, dc_ta$tavg)
dc_ts <- left_join(dc_tm, dc_ta$tavg, by ="date") 
dc_ts <- dc_ts %>% subset(select = c(date, tmin, tmax, tavg, year, month, season))

dc_ts$tmin <- dc_ts$tmin/10
dc_ts$tmax <- dc_ts$tmax/10
dc_ts$tavg <- dc_ts$tavg/10

summary(dc_ts)

# replace daily missing tmin/tmax by mean temp in corresponding year-month
dc_ts$tavg <- ifelse(!is.na(dc_ts$tmin) & !is.na(dc_ts$tmax), dc_ts$tmin+dc_ts$tmax/2 , dc_ts$tavg)
summary(dc_ts)

# NA values: (code reference - https://stackoverflow.com/questions/53195961/count-total-missing-values-by-group)
dc_nas2 <- dc_ts %>%  group_by(year) %>% summarise_each(funs(sum(is.na(.))))

ddat_list = split(dc_ts, dc_ts$year)

totyears = 1945:2021
y = length(totyears)

# part 1
RMSE = 0 
Tc = 0 #0-15
Cr = 0 #0-(-300)
x = 1

for (CR in seq(-20,-200)){
  for (TC in seq(0,15)){
    z = 0
    ppd = 0
    year = 0
    for (i in 1:y){
      Cd <- with(ddat_list[[i]], ifelse(0 <= TC & TC <= tmin & tmin <= tmax, 0, 
                                        ifelse(0 <= tmin & tmin <= TC & TC <= tmax,((tavg - tmin)-((tmax - TC)/2)),
                                               ifelse(0 <= tmin & tmin <= tmax & tmax <= TC, (tavg - tmin),
                                                      ifelse(tmin <= 0 & 0 <= tmax & tmax <= TC, ((tmax)/(tmax-tmin))*(tmax/2),
                                                             (((tmax)/(tmax-tmin))*(tmax/2)-((tmax-TC)/2)))))))
      Cd <- with(ddat_list[[i]], ifelse(Cd <=0, Cd, Cd*(-1)))
      nky <- cbind(Cd,ddat_list[[i]])
      nky[,"cm_cd"] <- cumsum(nky$Cd)
      nky
      chill <- min(which(nky$cm_cd <= CR))  # number of chill days
      if (!is.finite(chill)) {
        ppd[i] = "NA"
        year[i] = ddat_list[[i]][1,5]
        next
      }
      hs = chill + 1
      if (hs > length(nky$Cd)) {
        ppd[i] = "NA"
        year[i] = ddat_list[[i]][1,5]
        next
      }  
      nky$Cd[hs:length(nky$Cd)] <- 0 
      nky$cm_cd[hs:length(nky$cm_cd)] <- 0 
      
      nky[,"Ca"] <- with(ddat_list[[i]], ifelse(0 <= TC & TC <= tmin & tmin <= tmax, tavg-TC, 
                                                ifelse(0 <= tmin & tmin <= TC & TC <= tmax,(tmax - TC)/2,
                                                       ifelse(0 <= tmin & tmin <= tmax & tmax <= TC, 0,
                                                              ifelse(tmin <= 0 & 0 <= tmax & tmax <= TC, 0,
                                                                     (tmax - TC)/2))))) 
      
      nky$Ca[1:chill] <- 0 
      nky[,"cm_ca"] <- cumsum(nky$Ca)
      
      heat <- min(which(CR + nky$cm_ca  >= 0))  # number of heat days
      if (!is.finite(heat)) {
        ppd[i] = "NA"
        year[i] = ddat_list[[i]][1,5]
        next
      }
      totdays <- chill + heat
      
      ppd[i] = totdays
      year[i] = ddat_list[[i]][1,5]
      z = z + 1     
      
      print(i)
      print(TC)
      print(CR)
    }
    year <- unlist(year)
    predpbd <- data.frame(year,ppd)
    predpbd <- subset(predpbd, ppd!="NA") # reference: https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
    predpbd$ppd <- as.numeric(predpbd$ppd)
    dc_chpb <- left_join(predpbd, publicpbd, by ="year") %>% rowwise() %>% mutate(diff2 = (bloom_doy-ppd)^2)
    
    RMSE[x] <- sqrt(sum(dc_chpb$diff2)/(z))
    Tc[x] = TC
    Cr[x] = CR
    x = x+1
  }
}
DF <- data.frame(RMSE, Tc, Cr)
yi <- which.min((RMSE))
TCmin1 = Tc[[yi]]
CRmin1 = Cr[[yi]]
RMSEmin1 = RMSE[[yi]]

# part 2
RMSE = 0 
Tc = 0 #0-15
Cr = 0 #0-(-300)
x = 1

for (CR in seq(-29,-31,-0.1)){
  for (TC in seq(5,7,0.1)){
    z = 0
    ppd = 0
    year = 0
    for (i in 1:y){
      Cd <- with(ddat_list[[i]], ifelse(0 <= TC & TC <= tmin & tmin <= tmax, 0, 
                                        ifelse(0 <= tmin & tmin <= TC & TC <= tmax,((tavg - tmin)-((tmax - TC)/2)),
                                               ifelse(0 <= tmin & tmin <= tmax & tmax <= TC, (tavg - tmin),
                                                      ifelse(tmin <= 0 & 0 <= tmax & tmax <= TC, ((tmax)/(tmax-tmin))*(tmax/2),
                                                             (((tmax)/(tmax-tmin))*(tmax/2)-((tmax-TC)/2)))))))
      Cd <- with(ddat_list[[i]], ifelse(Cd <=0, Cd, Cd*(-1)))
      nky <- cbind(Cd,ddat_list[[i]])
      nky[,"cm_cd"] <- cumsum(nky$Cd)
      nky
      chill <- min(which(nky$cm_cd <= CR))  # number of chill days
      if (!is.finite(chill)) {
        ppd[i] = "NA"
        year[i] = ddat_list[[i]][1,5]
        next
      }
      hs = chill + 1
      if (hs > length(nky$Cd)) {
        ppd[i] = "NA"
        year[i] = ddat_list[[i]][1,5]
        next
      }  
      nky$Cd[hs:length(nky$Cd)] <- 0 
      nky$cm_cd[hs:length(nky$cm_cd)] <- 0 
      
      nky[,"Ca"] <- with(ddat_list[[i]], ifelse(0 <= TC & TC <= tmin & tmin <= tmax, tavg-TC, 
                                                ifelse(0 <= tmin & tmin <= TC & TC <= tmax,(tmax - TC)/2,
                                                       ifelse(0 <= tmin & tmin <= tmax & tmax <= TC, 0,
                                                              ifelse(tmin <= 0 & 0 <= tmax & tmax <= TC, 0,
                                                                     (tmax - TC)/2))))) 
      
      nky$Ca[1:chill] <- 0 
      nky[,"cm_ca"] <- cumsum(nky$Ca)
      
      heat <- min(which(CR + nky$cm_ca  >= 0))  # number of heat days
      if (!is.finite(heat)) {
        ppd[i] = "NA"
        year[i] = ddat_list[[i]][1,5]
        next
      }
      totdays <- chill + heat
      
      ppd[i] = totdays
      year[i] = ddat_list[[i]][1,5]
      z = z + 1     
      
      print(i)
      print(TC)
      print(CR)
    }
    year <- unlist(year)
    predpbd <- data.frame(year,ppd)
    predpbd <- subset(predpbd, ppd!="NA") # reference: https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
    predpbd$ppd <- as.numeric(predpbd$ppd)
    dc_chpb <- left_join(predpbd, publicpbd, by ="year") %>% rowwise() %>% mutate(diff2 = (bloom_doy-ppd)^2)
    
    RMSE[x] <- sqrt(sum(dc_chpb$diff2)/(z))
    Tc[x] = TC
    Cr[x] = CR
    x = x+1
  }
}
DF <- data.frame(RMSE, Tc, Cr)
yi <- which.min((RMSE))
TCmin1 = Tc[[yi]]
CRmin1 = Cr[[yi]]
RMSEmin1 = RMSE[[yi]]

TCmin1
CRmin1
RMSEmin1


# part 3
RMSE = RMSEmin1 
# TC = TCmin1
# CR = CRmin1

TC = 6
CR = -29.1
x = 1

z = 0
ppd = 0
year = 0

  z = 0
  ppd = 0
  year = 0
  for (i in 1:y){
    Cd <- with(ddat_list[[i]], ifelse(0 <= TC & TC <= tmin & tmin <= tmax, 0, 
                                      ifelse(0 <= tmin & tmin <= TC & TC <= tmax,((tavg - tmin)-((tmax - TC)/2)),
                                             ifelse(0 <= tmin & tmin <= tmax & tmax <= TC, (tavg - tmin),
                                                    ifelse(tmin <= 0 & 0 <= tmax & tmax <= TC, ((tmax)/(tmax-tmin))*(tmax/2),
                                                           (((tmax)/(tmax-tmin))*(tmax/2)-((tmax-TC)/2)))))))
    Cd <- with(ddat_list[[i]], ifelse(Cd <=0, Cd, Cd*(-1)))
    nky <- cbind(Cd,ddat_list[[i]])
    nky[,"cm_cd"] <- cumsum(nky$Cd)
    nky
    chill <- min(which(nky$cm_cd <= CR))  # number of chill days
    if (!is.finite(chill)) {
      ppd[i] = "NA"
      year[i] = ddat_list[[i]][1,5]
      next
    }
    hs = chill + 1
    if (hs > length(nky$Cd)) {
      ppd[i] = "NA"
      year[i] = ddat_list[[i]][1,5]
      next
    }  
    nky$Cd[hs:length(nky$Cd)] <- 0 
    nky$cm_cd[hs:length(nky$cm_cd)] <- 0 
    
    nky[,"Ca"] <- with(ddat_list[[i]], ifelse(0 <= TC & TC <= tmin & tmin <= tmax, tavg-TC, 
                                              ifelse(0 <= tmin & tmin <= TC & TC <= tmax,(tmax - TC)/2,
                                                     ifelse(0 <= tmin & tmin <= tmax & tmax <= TC, 0,
                                                            ifelse(tmin <= 0 & 0 <= tmax & tmax <= TC, 0,
                                                                   (tmax - TC)/2))))) 
    
    nky$Ca[1:chill] <- 0 
    nky[,"cm_ca"] <- cumsum(nky$Ca)
    
    heat <- min(which(CR + nky$cm_ca  >= 0))  # number of heat days
    if (!is.finite(heat)) {
      ppd[i] = "NA"
      year[i] = ddat_list[[i]][1,5]
      next
    }
    totdays <- chill + heat
    
    ppd[i] = totdays
    year[i] = ddat_list[[i]][1,5]
    z = z + 1     
    
    print(i)
    print(TC)
    print(CR)
}
  year <- unlist(year)
  predpbd <- data.frame(year,ppd)
  predpbd <- subset(predpbd, ppd!="NA") # reference: https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
  predpbd$ppd <- as.numeric(predpbd$ppd)
  dc_chpb <- left_join(predpbd, publicpbd, by ="year") %>% rowwise() %>% mutate(diff2 = (bloom_doy-ppd)^2)

# Plot 1: Predicted vs. Observed
fit = lm(dc_chpb$ppd ~ dc_chpb$bloom_doy)
summary(fit)

plot(dc_chpb$bloom_doy, dc_chpb$ppd, xlab = "Public PBD", ylab = "Predicted PBD", main = "PBD from 1945 to 2021 (Predicted and Observed)")
lines(dc_chpb$bloom_doy, 111.0581-0.2193*dc_chpb$bloom_doy)


# Plot 2
par(mar=c(6,6,6,6))

plot(dc_chpb$year, dc_chpb$ppd, type='l', col="blue", xlab = "Year", ylab = "Peak Bloom Dates (Day of Year)", main = "PBD from 1945 to 2021 (Predicted and Observed)") 
lines(dc_chpb$year, dc_chpb$bloom_doy, col="black")
legend(x="topright", inset = c(-0.37,0), legend=c("Predicted", "Observed"), 
       col=c("blue", "black"), pch=c(20), xpd=TRUE)

