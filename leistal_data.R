library(climate)

# code to obtain data, with sample dates per documentation for starters 
feldberg <- meteo_ogimet(interval = "hourly",   date = c("2018-05-01", "2018-07-01"), station = 10908)
feldberg <- meteo_ogimet(interval = "daily",   date = c("2018-05-01", "2018-07-01"), station = 10908)

frieburg <- meteo_ogimet(interval = "hourly",   date = c("2018-05-01", "2018-07-01"), station = 10803)
frieburg <- meteo_ogimet(interval = "daily",   date = c("2018-05-01", "2018-07-01"), station = 10803)

# From the K- Nearest Neighbor Search, the following two neighbors have an wmo_id and ogimet data
# FELDBERG/SCHWARZWALD: GME00120934 (wmo_id= 10908)
# FREIBURG (CIV/FAFB) : GMM00010803 (wmo_id= 10803)

# FELDBERG appears to have more data available based on the ghcnd station search information

# https://cran.r-project.org/web/packages/climate/climate.pdf (pg 15) - in GitHub repository: "climate.pdf"
# can use meteo_ogimet() to curate data from https://www.ogimet.com/index.phtml.