library(dplyr)
library(rnoaa)
library(climate)

(stations <- ghcnd_stations())

# The start date and end dates are based on available data (got as much as able)
# may contain some repeat entries for dates

##### HARDINESS ZONE 7B (WASHINGTON D.C.)
# 1, South Korea
stations %>% filter(name == "INCHEON")
incheon <- meteo_ogimet(interval = "daily",   date = c("2007-10-01", "2021-09-30"), station = 47112)
# write.csv(incheon,"data\\7B_incheon.csv", row.names = FALSE)

# 2. Japan
stations %>% filter(name == "SAPPORO")
sapporo <- meteo_ogimet(interval = "daily",   date = c("2007-10-01", "2021-09-30"), station = 47412)
# write.csv(sapporo,"data\\7B_sapporo.csv", row.names = FALSE)

# 3, South Korea
andong <- meteo_ogimet(interval = "daily",   date = c("2007-10-01", "2021-09-30"), station = 47136)
# write.csv(andong,"data\\7B_andong.csv", row.names = FALSE)

# 4, South Korea
seoul <- meteo_ogimet(interval = "daily",   date = c("2007-10-01", "2021-09-30"), station = 47108)
# write.csv(seoul,"data\\7B_seoul.csv", row.names = FALSE)

##### HARDINESS ZONE 7A (LIESTAL)
# 1, South Korea
wonju <- meteo_ogimet(interval = "daily",   date = c("2007-10-01", "2021-09-30"), station = 47108)
# write.csv(wonju,"data\\7A_wonju.csv", row.names = FALSE)

# 2, South Korea
yeoju <- meteo_ogimet(interval = "daily",   date = c("2007-10-01", "2021-09-30"), station = 47083)
 write.csv(yeoju,"data\\7A_yeoju.csv", row.names = FALSE)

# 3, South Korea
suwon <- meteo_ogimet(interval = "daily",   date = c("2007-10-01", "2021-09-30"), station = 47120)
# write.csv(suwon,"data\\7A_suwon.csv", row.names = FALSE)

##### HARDINESS ZONE 9B (KYOTO)
# 1, South Korea
stations %>% filter(name == "JEJU")
jeju <- meteo_ogimet(interval = "daily",   date = c("2007-10-01", "2021-09-30"), station = 47114)
# write.csv(jeju,"data\\9B_jeju.csv", row.names = FALSE)

# 2, Japan
stations %>% filter(name == "HAMAMATSU")
hamamatsu <- meteo_ogimet(interval = "daily",   date = c("2007-10-01", "2021-09-30"), station = 47582)
# write.csv(hamamatsu,"data\\9B_hamamatsu.csv", row.names = FALSE)

# 3, Japan
miyazaki <- meteo_ogimet(interval = "daily",   date = c("2007-10-01", "2021-09-30"), station = 47830)
# write.csv(miyazaki,"data\\9B_miyazaki.csv", row.names = FALSE)

# 4, Japan
nara <- meteo_ogimet(interval = "daily",   date = c("2007-10-01", "2021-09-30"), station = 47780)
 # write.csv(nara,"data\\9B_nara.csv", row.names = FALSE)

##### HARDINESS ZONE 8B (VANCOUVER)
# 1, South Korea
stations %>% filter(name == "BUSAN")
busan <- meteo_ogimet(interval = "daily",   date = c("2007-10-01", "2021-09-30"), station = 47159)
# write.csv(busan,"data\\8B_busan.csv", row.names = FALSE)

# 2, Japan
stations %>% filter(name == "NAGANO")
nagano <- meteo_ogimet(interval = "daily",   date = c("2007-10-01", "2021-09-30"), station = 47159)
# write.csv(nagano,"data\\8B_nagano.csv", row.names = FALSE)

# 2, Japan
utsunomiya <- meteo_ogimet(interval = "daily",   date = c("2007-10-01", "2021-09-30"), station = 47615)
write.csv(utsunomiya,"data\\8B_utsunomiya.csv", row.names = FALSE)

# 4, Japan
akita <- meteo_ogimet(interval = "daily",   date = c("2007-10-01", "2021-09-30"), station = 47545)
# write.csv(akita,"data\\8B_akita.csv", row.names = FALSE)