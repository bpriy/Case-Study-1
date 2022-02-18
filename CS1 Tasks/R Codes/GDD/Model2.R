library(tidyverse)
library(rnoaa)
library(purrr)

cherry <- read.csv("data/washingtondc.csv") %>% 
  bind_rows(read.csv("data/liestal.csv")) %>% 
  bind_rows(read.csv("data/kyoto.csv"))
cherry
ch_obs <- cherry %>% subset(select = c(location, year, bloom_doy))
ch_obs

# parameter optimization: kyoto
ch_obs <- ch_obs[ch_obs$location %in% c("kyoto"),]
publicpbd <- ch_obs %>% subset(select = c(year, bloom_doy))

kyoto_tm <- ghcnd_search(stationid = "JA000047759", var=c("tmin", "tmax")) 
kyoto_tm <- kyoto_tm %>% reduce(full_join, by='date') %>% subset(select = c(date, tmin, tmax))
kyoto_tm <- kyoto_tm %>% mutate(year = as.integer(format(date, "%Y")),
                                month = as.integer(strftime(date, '%m')) %% 12, # make December "0"
                                season = cut(month, breaks = c(0, 2, 5, 8, 11),
                                             include.lowest = TRUE,
                                             labels = c("Winter", "Spring", "Summer", "Fall")),
                                year = if_else(month >= 10 | month == 0, year + 1L, year))
# NA values: (code reference - https://stackoverflow.com/questions/53195961/count-total-missing-values-by-group)
kyoto_nas <- kyoto_tm %>%  group_by(year) %>% summarise_each(funs(sum(is.na(.))))
# subset if we only want to do certain years 
# kyoto_tm <- subset(kyoto_tm, year < 1989)

kyoto_ta <- ghcnd_search(stationid = "JA000047759", var=c("tavg"))
kts_list <- list(kyoto_tm, kyoto_ta$tavg)
kyoto_ts <- left_join(kyoto_tm, kyoto_ta$tavg, by ="date") 
kyoto_ts <- kyoto_ts %>% subset(select = c(date, tmin, tmax, tavg, year, month, season))
kyoto_ts$tavg <- ifelse(is.na(kyoto_ts$tavg), kyoto_ts$tmin+kyoto_ts$tmax/2 , kyoto_ts$tavg)
summary(kyoto_ts)
kyoto_ts
clipr::write_clip(kyoto_ts)
kdat_list = split(kyoto_ts, kyoto_ts$year)

x = 1
totyears = 1951:2022
y = length(totyears)

# part 1
CR = -100
  for (TC in seq(7,12)){
    z = 1
    for (i in 1:y){
      Cd <- with(kdat_list[[i]], ifelse(0 <= TC & TC <= tmin & tmin <= tmax, 0, 
                                        ifelse(0 <= tmin & tmin <= TC & TC <= tmax,((tavg - tmin)-((tmax - TC)/2)),
                                               ifelse(0 <= tmin & tmin <= tmax & tmax <= TC, (tavg - tmin),
                                                      ifelse(tmin <= 0 & 0 <= tmax & tmax <= TC, ((tmax)/(tmax-tmin))*(tmax/2),
                                                             (((tmax)/(tmax-tmin))*(tmax/2)-((tmax-TC)/2)))))))
      Cd <- with(kdat_list[[i]], ifelse(Cd <=0, Cd, Cd*(-1)))
      nky <- cbind(Cd,kdat_list[[i]])
      nky[,"cm_cd"] <- cumsum(nky$Cd)
      nky
      chill <- min(which(nky$cm_cd <= CR))  # number of chill days
      if (!is.finite(chill)) {
        next
      }
      hs = chill + 1
      nky$Cd[hs:length(nky$Cd)] <- 0 
      nky$cm_cd[hs:length(nky$cm_cd)] <- 0 
      
      nky[,"Ca"] <- with(kdat_list[[i]], ifelse(0 <= TC & TC <= tmin & tmin <= tmax, tavg-TC, 
                                                ifelse(0 <= tmin & tmin <= TC & TC <= tmax,(tmax - TC)/2,
                                                       ifelse(0 <= tmin & tmin <= tmax & tmax <= TC, 0,
                                                              ifelse(tmin <= 0 & 0 <= tmax & tmax <= TC, 0,
                                                                     (tmax - TC)/2))))) 
      
      nky$Ca[1:chill] <- 0 
      nky[,"cm_ca"] <- cumsum(nky$Ca)
      
      heat <- min(which(CR + nky$cm_ca  >= 0))  # number of heat days
      totdays <- chill + heat
      
      ppd[z] = c(totdays)
      year[z] = kdat_list[[i]][1,5]
      
      print(i)
      print(TC)
      print(CR)
    }

  }
##################################################

predpbd <- data.frame(year,ppd)
kyoto_chpb <- left_join(predpbd, publicpbd, by ="year") %>% 
  rowwise() %>% mutate(diff2 = (bloom_doy-ppd)^2)
z = z + 1


for (CR in -100: -200){
  for (TC in seq(7,12)){
for (i in 1:y){
Cd <- with(kdat_list[[i]], ifelse(0 <= TC & TC <= tmin & tmin <= tmax, 0, 
                            ifelse(0 <= tmin & tmin <= TC & TC <= tmax,((tavg - tmin)-((tmax - TC)/2)),
                                   ifelse(0 <= tmin & tmin <= tmax & tmax <= TC, (tavg - tmin),
                                          ifelse(tmin <= 0 & 0 <= tmax & tmax <= TC, ((tmax)/(tmax-tmin))*(tmax/2),
                                                 (((tmax)/(tmax-tmin))*(tmax/2)-((tmax-TC)/2)))))))
Cd <- with(kdat_list[[i]], ifelse(Cd <=0, Cd, Cd*(-1)))
nky <- cbind(Cd,kdat_list[[i]])
nky[,"cm_cd"] <- cumsum(nky$Cd)
nky
chill <- min(which(nky$cm_cd <= CR))  # number of chill days
if (!is.finite(chill)) {
  next
}
hs = chill + 1
nky$Cd[hs:length(nky$Cd)] <- 0 
nky$cm_cd[hs:length(nky$cm_cd)] <- 0 

nky[,"Ca"] <- with(kdat_list[[i]], ifelse(0 <= TC & TC <= tmin & tmin <= tmax, tavg-TC, 
                                    ifelse(0 <= tmin & tmin <= TC & TC <= tmax,(tmax - TC)/2,
                                           ifelse(0 <= tmin & tmin <= tmax & tmax <= TC, 0,
                                                  ifelse(tmin <= 0 & 0 <= tmax & tmax <= TC, 0,
                                                       (tmax - TC)/2))))) 

nky$Ca[1:chill] <- 0 
nky[,"cm_ca"] <- cumsum(nky$Ca)

heat <- min(which(CR + nky$cm_ca  >= 0))  # number of heat days
totdays <- chill + heat

ppd[z] = totdays
year[z] = kdat_list[[i]][1,5]

predpbd <- data.frame(year,ppd)
kyoto_chpb <- left_join(predpbd, publicpbd, by ="year") %>% 
  rowwise() %>% mutate(diff2 = (bloom_doy-ppd)^2)
z = z + 1
print(i)
print(TC)
print(CR)
}
  }
  }


RMSD[x] <- sqrt(sum(kyoto_chpb$diff2)/(y))
Tc[x] = TC
Cr[x] = CR
x = x+1
yi <- min(which(RMSD))
year[yi]
