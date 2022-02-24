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
data <- subset(data, year >= 1991 & year <= 2010)
datal = split(data, data$year)

#################

# specify years
totyears = 1991:2010
y = length(totyears)

# parameter optimization: TC, RC, RH
TC = 4 
Rc = -110
Rh = 250
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
                 chill <- min(which(tab$cm_cd <= Rc))  # number of chill days
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
                 
                 totdays <- min(which(tab$cm_ca >= Rh)) # number of heat days
                 if (!is.finite(totdays)) {
                   ppd[i] = "NA"
                   year[i] = datal[[i]][1,5]
                   next
                 }
                 ppd[i] = totdays - 92
                 year[i] = datal[[i]][1,5]

    }
    year <- unlist(year)
    predpbd <- data.frame(year,ppd)
    predpbd <- subset(predpbd, ppd!="NA") # reference: https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
    predpbd$ppd <- as.numeric(predpbd$ppd)
    restab <- left_join(predpbd, publicpbd, by ="year") 
    rmse(restab$ppd, restab$bloom_doy)
    
    fit = lm(restab$ppd ~ restab$bloom_doy)
    summary(fit)
    
    plot(restab$bloom_doy, restab$ppd, xlab = "Public PBD", ylab = "Predicted PBD", main = "PBD from 1991 to 2010 (Predicted and Observed)")
    lines(restab$bloom_doy, 9.9524+0.8671*restab$bloom_doy)
    
    plot(restab$year, restab$ppd, type='l', col="blue", xlab = "Year", ylab = "Peak Bloom Dates (Day of Year)", main = "PBD from 1991 to 2010 (Predicted and Observed)") 
    lines(restab$year, restab$bloom_doy, col="black")
    legend(x="topright", inset = c(-0.37,0), legend=c("Predicted", "Observed"), 
           col=c("blue", "black"), pch=c(20), xpd=TRUE)
    
    plot(restab$bloom_doy, restab$ppd, xlab = "Public PBD", ylab = "Predicted PBD", main = "PBD from 1991 to 2010 (Predicted and Observed)")
    lines(restab$bloom_doy, coef(fit)[1]+coef(fit)[2]*(restab$bloom_doy))