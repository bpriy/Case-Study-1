library(rnoaa)
library(tidyverse)

dc_ghcnd <- ghcnd_search(stationid = "USW00013743", var=c("tmin", "tmax", "tavg", "tsun", "awnd", "prcp", "acsh")) 
dc_ghcnd <- dc_ghcnd %>% reduce(full_join, by='date') %>% subset(select = c(date, tmin, tmax, tavg, tsun, awnd, prcp, acsh))

kyoto_ghcnd <- ghcnd_search(stationid = "JA000047759", var=c("tmin", "tmax", "tavg", "prcp")) 
kyoto_ghcnd <- kyoto_ghcnd %>% reduce(full_join, by='date') %>% subset(select = c(date, tmin, tmax, tavg, prcp))

liestal_ghcnd <- ghcnd_search(stationid = "GME00127786", var=c("tmin", "tmax", "prcp")) 
liestal_ghcnd <- liestal_ghcnd %>% reduce(full_join, by='date') %>% subset(select = c(date, tmin, tmax, prcp)) %>% rowwise() %>% mutate(tavg = mean(c(tmax,tmin)))

vancouver_ghcnd <- ghcnd_search(stationid = "CA001108395", var=c("tmin", "tmax", "tavg", "prcp", "wsfg")) 
vancouver_ghcnd <- vancouver_ghcnd %>% reduce(full_join, by='date') %>% subset(select = c(date, tmin, tmax, tavg, prcp, wsfg))

# STATION      LATITUDE (decimal degrees) LONGITUDE (decimal degrees) ELEVATION (meters) STATION NAME            YEARS COLLECTED
# USW00013743  38.8483                    -77.0342                    3.0                WASHINGTON_REAGAN_AP    1945 (1984 for awnd)-2022, 1965-1998 for acsh    
# JA000047759  35.0170                    135.7330                    46.0               KYOTO                   1951 (1945 for tavg) -2022       
# CA001108395  49.2000                   -123.1833                    4.0                VANCOUVER_INTL_A        1957-2022         
# GME00127786  47.6                       7.79                        287                RHEINFELDEN             1953-2021         

(stations <- ghcnd_stations())
stations %>% filter(id == "USW00013743")