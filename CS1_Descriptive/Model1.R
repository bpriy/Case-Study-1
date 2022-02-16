library(tidyverse)
library(rnoaa)
library(purrr)

cherry <- read.csv("data/washingtondc.csv") %>% 
  bind_rows(read.csv("data/liestal.csv")) %>% 
  bind_rows(read.csv("data/kyoto.csv"))
cherry
ch <- cherry %>% subset(select = c(location, year, bloom_doy))

v<-c("kyoto")
ch <- ch[ch$location %in% v,]
kypb <- ch %>% subset(select = c(year, bloom_doy))

ky1 <- ghcnd_search(stationid = "JA000047759", var=c("tmax")) 
ky2 <- ghcnd_search(stationid = "JA000047759", var=c("tmin"))
kytem_list <- list(ky1$tmax, ky2$tmin)
kytem <- kytem_list %>% 
  reduce(full_join, by='date') %>% 
  subset(select = c(date, tmin, tmax)) %>%
  rowwise() %>% mutate(tavg = mean(c(tmax,tmin))) # estimated average temperature 

kytem <- kytem %>% mutate(year = as.integer(format(date, "%Y")),
                          month = as.integer(strftime(date, '%m')) %% 12, # make December "0"
                          season = cut(month, breaks = c(0, 2, 5, 8, 11),
                                       include.lowest = TRUE,
                                       labels = c("Winter", "Spring", "Summer", "Fall")),
                          year = if_else(month >= 10 | month == 0, year + 1L, year))
kytem <- na.omit(kytem)
kytem

kdat_list = (split(kytem, kytem$year))
kdat_list

func <- function(data){
Cd <- with(data, ifelse(0 <= TC & TC <= tmin & tmin <= tmax, 0, 
                                    ifelse(0 <= tmin & tmin <= TC & TC <= tmax,((tavg - tmin)-((tmax - TC)/2)),
                                           ifelse(0 <= tmin & tmin <= tmax & tmax <= TC, (tavg - tmin),
                                                  ifelse(tmin <= 0 & 0 <= tmax & tmax <= TC, ((tmax)/(tmax-tmin))*(tmax/2),
                                                         (((tmax)/(tmax-tmin))*(tmax/2)-((tmax-TC)/2)))))))
Cd <- with(data, ifelse(Cd <=0, Cd, Cd*(-1)))
nky <- cbind(Cd,data)
nky[,"cm_cd"] <- cumsum(nky$Cd)
chill <- min(which(nky$cm_cd <= CR))  # number of chill days
hs = chill + 1
nky$Cd[hs:length(nky$Cd)] <- 0 
nky$cm_cd[hs:length(nky$cm_cd)] <- 0 

nky[,"Ca"] <- with(data, ifelse(0 <= TC & TC <= tmin & tmin <= tmax, tavg-TC, 
                                            ifelse(0 <= tmin & tmin <= TC & TC <= tmax,(tmax - TC)/2,
                                                   ifelse(0 <= tmin & tmin <= tmax & tmax <= TC, 0,
                                                          ifelse(tmin <= 0 & 0 <= tmax & tmax <= TC, 0,
                                                                 (tmax - TC)/2))))) 

nky$Ca[1:chill] <- 0 
nky[,"cm_ca"] <- cumsum(nky$Ca)

heat <- min(which(CR + nky$cm_ca  >= 0))  # number of heat days
ppd = chill + heat

ppd
}

CR = -100
TC = 7

func(kdat_list$`2020`)
for (CR in -100: -200){
  for (TC in seq(7,12,by = 0.1)){
    map_dfr(kdat_list, func, .year = "year")
  }}


