library(rnoaa)
library(tidyverse)

# API Acess Token: fwcZDIbCZYPCqmEWEhDBVHyNRsWyvFQB

# Station IDs
######################################################
# Washington D.C. - USW00013743 
# Kyoto           - JA000047759 
# Liestal         - GME00127786 
# Vancouver       - CA001108395
######################################################

ghcnd_search(stationid = "JA000047759")

stations %>%
  filter(id=="USW00013743" | id=="JA000047759" | id=="GME00127786" | id=="CA001108395") %>% 
  print(n=Inf)

# Washington, DC
loc1 <- ncdc_stations(
  stationid = "GHCND:USW00013743",
  token = "fwcZDIbCZYPCqmEWEhDBVHyNRsWyvFQB")
ncdc_datasets(stationid="GHCND:USW00013743", 
              token="fwcZDIbCZYPCqmEWEhDBVHyNRsWyvFQB")
# Kyoto, Japan
loc2 <- ncdc_stations(
  stationid = "GHCND:JA000047759",
  token = "fwcZDIbCZYPCqmEWEhDBVHyNRsWyvFQB")
ncdc_datasets(stationid="GHCND:JA000047759", 
              token="fwcZDIbCZYPCqmEWEhDBVHyNRsWyvFQB")
# Liestal, Switzerland
loc3 <- ncdc_stations(
  stationid = "GHCND:GME00127786",
  token = "fwcZDIbCZYPCqmEWEhDBVHyNRsWyvFQB")
# Vancouver, BC
loc4 <- ncdc_stations(
  stationid = "GHCND:CA001108395",
  token = "fwcZDIbCZYPCqmEWEhDBVHyNRsWyvFQB")

# Station Data
loc1$data
loc2$data
loc3$data
loc4$data


##############################################
## K Nearest Neighbor ##
##############################################

library(RANN)
# Coordinates for 4 locations in competition
cor_dc <- data.frame("latitude"=38.8853, "longitude"=-77.0386)
cor_kyoto <- data.frame("latitude"=35.0120, "longitude"=135.6761)
cor_liestal <- data.frame("latitude"=47.4814, "longitude"=7.730519)
cor_bc <- data.frame("latitude"=49.2237, "longitude"=-123.1636)

df_dc <- data.frame(id = "dc", latitude = 38.8853, longitude = -77.0386)
df_kyoto <- data.frame(id = "kyoto", latitude = 35.0120, longitude = 135.6761)
df_liestal <- data.frame(id = "liestal", latitude = 47.4814, longitude = 7.730519)
df_bc <- data.frame(id = "vancouver", latitude = 49.2237, longitude = -123.1636)

# create list of coordinates for all GHCND stations
stations2 <- stations[,1:6] 
head(stations2)
stations2 <- distinct(stations2, id, .keep_all=TRUE)
cor_stations <- as.data.frame(stations2[,2:3])

# Washington, DC
nn_25_dc <- nn2(cor_stations, cor_dc, k=25)
dc_25 <- as.vector(nn_25_dc$nn.idx)
View(stations2[dc_25, ])
dc_25_2 <- meteo_nearby_stations(lat_lon_df=df_dc, limit=25)
print(as.tibble(dc_25_2), n=25)

# Kyoto, Japan
nn_25_kyoto <- nn2(cor_stations, cor_kyoto, k=25)
kyoto_25 <- as.vector(nn_25_kyoto$nn.idx)
View(stations2[kyoto_25, ])
kyoto_25_2 <- meteo_nearby_stations(lat_lon_df=df_kyoto, limit=25)
print(as.tibble(kyoto_25_2), n=25)

# Liestal, Switzerland
nn_25_liestal <- nn2(cor_stations, cor_liestal, k=25)
liestal_25 <- as.vector(nn_25_liestal$nn.idx)
View(stations2[liestal_25, ])
liestal_25_2 <- meteo_nearby_stations(lat_lon_df=df_liestal, limit=25)
print(as.tibble(liestal_25_2), n=25)

# Vancouver, BC
nn_25_bc <- nn2(cor_stations, cor_bc, k=25)
bc_25 <- as.vector(nn_25_bc$nn.idx)
View(stations2[bc_25, ])
bc_25_2 <- meteo_nearby_stations(lat_lon_df=df_bc, limit=25)
print(as.tibble(bc_25_2), n=25)


stations %>% 
  filter(id=="USW00013743" | id=="USW00093725" | id=="USW00013751") %>% 
  print(n=Inf)


stations %>% 
  filter(id=="CA001108448" | id=="CA001108470" | id=="CA001108473" | id=="CA001108435" | id=="CA001108447") %>% 
  print(n=Inf)




##############################################
##  Data Download   ##
##############################################
# Kyoto, Japan
kyoto_mo <- ghcnd(stationid = "JA000047759", refresh = TRUE)
kyoto_day <- ncdc(datasetid = "GHCND", stationid = "JA000047759",
                  token = "fwcZDIbCZYPCqmEWEhDBVHyNRsWyvFQB",
                  startdate = 2021-10-01,
                  enddate = 2021-12-31)

# Washington, DC
dc_daily <- ncdc(datasetid = "GHCND", stationid = "GHCND:USW00013743",
                 token = "fwcZDIbCZYPCqmEWEhDBVHyNRsWyvFQB",
                 startdate = 2021-10-01,
                 enddate = 2021-12-31)

lcd(station="72405013743", year=2014)
