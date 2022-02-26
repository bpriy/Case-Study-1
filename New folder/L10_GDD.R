library(tidyverse)
library(rnoaa)
library(purrr)
library(dplyr)
library(Metrics)

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
