

# Kyoto
bestaic <- 100000
bestmdl <- NULL
for(a in 0:5) for(b in 0:5) for(c in 0:5) for(d in 0:5) for(e in 1:10) for(f in 0:2) for(g in 0:2){
  
  ja.arima <- Arima(ja.pbd.ts, order = c(a, f, b), seasonal = list(order=c(c, g, d), period=e), 
                    method="ML", include.drift=FALSE,
                    xreg=cbind(t_avg, chill, gdd, sun_t, ts.slp) )
  
  if (ja.arima$aic < bestaic) 
  {bestmdl <- ja.arima
  bestaic <- ja.arima$aic}
}


# Liestal 1
bestaic <- 100000
bestmdl <- NULL
for(a in 0:6) for(b in 0:5) for(c in 0:6) for(d in 0:5) for(e in 1:8) for(f in 0:2) for(g in 0:2){
  
  sw.arima <- Arima(sw.pbd.ts, order = c(a, f, b), seasonal = list(order=c(c, g, d), period=e), 
                    method="ML", include.drift=FALSE,
                    xreg=cbind(sun_ms, tempdif, gdd, t_avg, chill) )
  
  if (sw.arima$aic < bestaic) 
  {bestmdl <- sw.arima
  bestaic <- sw.arima$aic}
}


# Liestal 2
bestaic2 <- 100000
bestmdl2 <- NULL
for(a in 0:6) for(b in 0:5) for(c in 0:6) for(d in 0:5) for(e in 1:8) for(f in 0:2) for(g in 0:2){
  
  sw.arima3 <- Arima(sw.pbd.ts, order = c(a, f, b), seasonal = list(order=c(c, g, d), period=e), 
                     method="ML", include.drift=FALSE )
  
  if (sw.arima3$aic < bestaic2) 
  {bestmdl2 <- sw.arima
  bestaic2 <- sw.arima$aic}
}

