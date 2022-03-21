
# Linear Models
#########################################################################
# Washington, DC
colnames(cherry.dc)

plot(cherry.dc$SLP, type="l")
numsim <- 10000
slp.smp <- NULL
set.seed(7)
for (k in 1:numsim){
  slp.smp[k] <- sample(cherry.dc[c(36:45,53:70), "SLP"], size=1)
}
plot(density(slp.smp))
set.seed(7)
v1 <- sample(slp.smp, size = 7)
cherry.dc$SLP[46:52] <- v1

cherry.dc.sub <- cherry.dc[cherry.dc$year >= 1973, 
                           c("sun", "sun.mar", "sun.maysep", "WDSP", "SLP", "PRCP", "SNWD", "Tavg", 
                             "TempDiffAbs", "heat.days", "chill.days", "ppd", "GDD", "bloom_doy")]

scatterplotMatrix(cherry.dc.sub)
cor(cherry.dc.sub)


# Stepwise Regression
lm1 <- lm(bloom_doy ~ sun.mar + sun.maysep + WDSP + SLP + PRCP + SNWD + Tavg + 
            TempDiffAbs + heat.days + chill.days + ppd + GDD,
          data=cherry.dc.sub, na.action = na.omit)
step.lm1 <- step(lm1, direction = "backward")

# Best subset selection
cherry.dc.pred <- cherry.dc.sub %>% filter(complete.cases(.))
cherry.dc.y <- cherry.dc.pred[,"bloom_doy"]
cherry.dc.pred <- within(cherry.dc.pred, rm("bloom_doy", "sun", "ppd"))

bestlm <- RegBest(y = cherry.dc.y, x  = cherry.dc.pred, 
                  na.action = na.omit, method="Cp", nbest=3)
bestlm$best
summary(regsubsets(y = cherry.dc.y, x = cherry.dc.pred, nbest=1 ))


# models
lm.b1 <- lm(bloom_doy ~ SNWD + WDSP + PRCP + SLP + TempDiffAbs + chill.days + heat.days + GDD, 
            data = cherry.dc.sub)
summary(lm.b1)

lm.b2 <- lm(bloom_doy ~ WDSP + SLP + PRCP + SNWD + TempDiffAbs + heat.days + chill.days + GDD,
            data=cherry.dc.sub)
summary(lm.b2)

lm.b3 <- lm(bloom_doy ~ SNWD + WDSP + PRCP + SLP + TempDiffAbs + chill.days + heat.days + GDD + sun.maysep,
            data=cherry.dc.sub)
summary(lm.b3)

lm.b4 <- lm(bloom_doy ~ SNWD + WDSP + PRCP + SLP + TempDiffAbs + chill.days + heat.days + GDD + sun.mar,
            data=cherry.dc.sub)
summary(lm.b4)


AIC(lm.b1, lm.b2, lm.b3, lm.b4)
# lm.b4 was provisionally selected as the best balanced model

# Diagnostics
vif(lm.b4)  # heat.days has high VIF
scatterplotMatrix(cherry.dc.sub)
cor(cherry.dc.sub)

# Backward stepwise model selection with heat.days removed
cherry.dc.subrm <- cherry.dc.sub %>% filter(complete.cases(.))
lm2 <- lm(bloom_doy ~ sun.mar + sun.maysep + WDSP + SLP + PRCP + SNWD + Tavg + 
           TempDiffAbs + chill.days + GDD,
         data=cherry.dc.subrm)
step(lm2, direction = "backward")

cherry.dc.pred <- within(cherry.dc.pred, rm("heat.days"))
bestlm <- RegBest(y = cherry.dc.y, x  = cherry.dc.pred, 
                  na.action = na.omit, method="adjr2", nbest=3)
bestlm$best


# Revised model with heat.days removed
lm.r1 <- lm(bloom_doy ~ SLP + PRCP + WDSP + Tavg + TempDiffAbs + chill.days + GDD,
            data=cherry.dc.sub)
summary(lm.r1)

lm.r2 <- lm(bloom_doy ~ SLP + PRCP + WDSP + Tavg + TempDiffAbs + chill.days + GDD + sun.mar + sun.maysep,
            data=cherry.dc.sub)
summary(lm.r2)

AIC(lm.r1, lm.r2)
anova(lm.r1, lm.r2)
# since lm.r1 and lm.r2 are not significantly different from each other and lm.r2 has higher adj r-squared, we go with lm.r2

# Diagnostics
vif(lm.r2) # VIF ok now for all variables
qqPlot(lm.r2)
plot(lm.r2)

# test for serial correlation
acf2(resid(lm.r2))
acf(resid(lm.r1))
pacf(resid(lm.r1))
acf2(resid(lm.r2)^2)
durbinWatsonTest(lm.r2)

attach(cherry.dc)
par(mfrow=c(1,1))
tyme <- seq(from=1921, to=2021, by=1)
plot(tyme, PBD, type="l")
plot(tyme, GDD, type="l")
plot(tyme, cherry.dc$sun.mar, type="l")
plot(tyme, cherry.dc$WDSP, type="l")
plot(tyme, chill.days, type="l")
plot(tyme, TempDiffAbs, type="l")
plot(tyme, Tavg, type="l")
plot(tyme, SLP, type="l")
plot(tyme, PRCP, type="l")
detach(cherry.dc)


# Time series analysis
# set store variables as time series for model
dc.bloom.ts <- ts(cherry.dc$bloom_doy[53:101])
gdd <- ts(cherry.dc$GDD[53:101])
sun.mar <- ts(cherry.dc$sun.mar[53:101])
wdsp <- ts(cherry.dc$WDSP[53:101])
chill <- ts(cherry.dc$chill.days[53:101])
tempdiff <- ts(cherry.dc$TempDiffAbs[53:101])
t_avg <- ts(cherry.dc$Tavg[53:101])
x_slp <- ts(cherry.dc$SLP[53:101])
prcp <- ts(cherry.dc$PRCP[53:101])
time.dc <- time(dc.bloom.ts)
heating <- ts(cherry.dc$heat.days[53:101])
year.ts <- ts(cherry.dc$year[53:101])
snow <-ts(cherry.dc$SNWD[53:101])
sun_ms <- ts(cherry.dc$sun.maysep[53:101])


dc.arima <- auto.arima(dc.bloom.ts, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                       seasonal=TRUE, method="ML",
                       xreg=cbind(x_slp, prcp, wdsp, t_avg, tempdiff, chill, gdd, sun.mar, sun_ms) )
summary(dc.arima)
dc.arima$arma

# ARIMA model diagnostics
# code and techniques borrowed from https://otexts.com/fpp2/regarima.html
cbind("Regression Errors" = residuals(dc.arima, type="regression"),
      "ARIMA errors" = residuals(dc.arima, type="innovation")) %>%
  autoplot(facets=TRUE)

checkresiduals(dc.arima)
# ARIMA residuals approximate white noise, are normally distributed, but the acf residual plot does not look good

dc.arima2 <- Arima(dc.bloom.ts, order = c(6, 0, 4), seasonal = list(order=c(6, 0, 3), period=5), 
                   method="ML", include.drift=TRUE,
                   xreg=cbind(x_slp, prcp, wdsp, t_avg, tempdiff, chill, gdd, sun.mar, sun_ms) )
dc.arima2$arma
checkresiduals(dc.arima2)
summary(dc.arima2)

dc.arima$aic
dc.arima2$aic
# final arima model is
summary(dc.arima2)
dc.arima2$arma

# predictions from ARIMA model
dc.arima.pred2 <- as.data.frame(forecast(dc.arima2, xreg=cbind(x_slp, prcp, wdsp, t_avg, tempdiff, 
                                                             chill, gdd, sun.mar, sun_ms)))[1:11,1]
dc.arima.pred <- as.data.frame(forecast(dc.arima, xreg=cbind(x_slp, prcp, wdsp, t_avg, tempdiff, 
                                                             chill, gdd, sun.mar, sun_ms)))[1:11,1]


# predict covariates
###################################################################################
cherry.dc.sub2 <- cherry.dc[cherry.dc$year >= 1973, 
                           c("year", "location", "sun", "sun.mar", "sun.maysep", "WDSP", "SLP", "PRCP", 
                             "SNWD", "Tavg", "TempDiffAbs", "heat.days", "chill.days", "ppd", "GDD",
                             "bloom_doy")]


# find most significant predictors for each covariate in the final model:
# sun.mar + WDSP + chill.days + Tavg + SLP + PRCP + TempDiffAbs + GDD
# then fit linear regression model with arima structure on each covariate and 
# forecast out for 11 years

scatterplotMatrix(cherry.dc.sub)

# Tavg
##########
lm.c1 <- lm(Tavg ~ year + sun.maysep + sun.mar + PRCP + SLP + WDSP + chill.days + TempDiffAbs + GDD + heat.days, data=cherry.dc.sub2)
summary(lm.c1)
plot(lm.c1)
acf2(resid(lm.c1))
acf2(resid(lm.c1)^2)
durbinWatsonTest(lm.c1)
durbinWatsonTest(lm.c1)

lm.t1 <- auto.arima(t_avg, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, seasonal=TRUE, method="ML", xreg=cbind(sun.mar, sun_ms, wdsp, x_slp, prcp, tempdiff, chill, heating, gdd) )
summary(lm.t1)
lm.t1$arma
checkresiduals(lm.t1)
Tavg.fut <- as.data.frame(forecast(lm.t1, 
        xreg=cbind(sun.mar, sun_ms, wdsp, x_slp, prcp, tempdiff, chill, heating, gdd )))[1:11, 1]


# TempDiffAbs
###############
lm.c2 <- lm(TempDiffAbs ~ year + sun.maysep + sun.mar + PRCP + SLP + WDSP + Tavg + chill.days + heat.days + GDD, data=cherry.dc.sub2)
summary(lm.c2)
plot(lm.c2)
acf2(resid(lm.c2))
acf2(resid(lm.c2)^2)
durbinWatsonTest(lm.c2)
durbinWatsonTest(lm.c2)

lm.t2 <- auto.arima(tempdiff, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, seasonal=TRUE, method="ML",
                    xreg=cbind(sun.mar, sun_ms, wdsp, x_slp, prcp, t_avg, chill, heating, gdd) )
summary(lm.t2)
lm.t2$arma
checkresiduals(lm.t2)
TempDiffAbs.fut <- as.data.frame(forecast(lm.t2, 
              xreg=cbind(sun.mar, sun_ms, wdsp, x_slp, prcp, t_avg, chill, heating, gdd)))[1:11, 1]


# PRCP
############
lm.c3 <- lm(PRCP ~ year + sun.maysep + sun.mar + TempDiffAbs + SLP + WDSP + chill.days + Tavg + GDD + heat.days, data=cherry.dc.sub2)
summary(lm.c3)
plot(lm.c3)
acf2(resid(lm.c3))
acf2(resid(lm.c3)^2)
durbinWatsonTest(lm.c3)
durbinWatsonTest(lm.c3)

lm.t3 <- auto.arima(prcp, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, seasonal=TRUE, method="ML", 
                    xreg=cbind(sun.mar, sun_ms, wdsp, x_slp, tempdiff, t_avg, chill, heating, gdd) )

summary(lm.t3)
lm.t3$arma
checkresiduals(lm.t3)
PRCP.fut <- as.data.frame(forecast(lm.t3, 
          xreg=cbind(sun.mar, sun_ms, wdsp, x_slp, tempdiff, t_avg, chill, heating, gdd)))[1:11, 1]


# SLP
#############
lm.c4 <- lm(SLP ~ year + sun.maysep + sun.mar + TempDiffAbs + Tavg + PRCP + wdsp + chill.days + heat.days + GDD, data=cherry.dc.sub2)
summary(lm.c4)
plot(lm.c4)
acf2(resid(lm.c4))
acf2(resid(lm.c4)^2)
durbinWatsonTest(lm.c4)
durbinWatsonTest(lm.c4)

lm.t4 <- auto.arima(x_slp, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                    seasonal=TRUE, method="ML",
                    xreg=cbind(sun.mar, sun_ms, wdsp, prcp, tempdiff, t_avg, chill, heating, gdd) )
summary(lm.t4)
lm.t4$arma
checkresiduals(lm.t4)
SLP.fut <- as.data.frame(forecast(lm.t4, 
            xreg=cbind(sun.mar, sun_ms, wdsp, prcp, tempdiff, t_avg, chill, heating, gdd)))[1:11, 1]


# WDSP
############
lm.c5 <- lm(wdsp ~ year + sun.maysep + sun.mar + TempDiffAbs + Tavg + PRCP + SLP + chill.days + heat.days + GDD, data=cherry.dc.sub2)
summary(lm.c5)
plot(lm.c5)
acf2(resid(lm.c5))
acf2(resid(lm.c5)^2)
durbinWatsonTest(lm.c5)
durbinWatsonTest(lm.c5)

lm.t5 <- auto.arima(wdsp, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                    seasonal=TRUE, method="ML",
                    xreg=cbind(sun.mar, sun_ms, x_slp, prcp, tempdiff, t_avg, chill, heating, gdd) )
summary(lm.t5)
lm.t5$arma
checkresiduals(lm.t5)
WDSP.fut <- as.data.frame(forecast(lm.t5, 
          xreg=cbind(sun.mar, sun_ms, x_slp, prcp, tempdiff, t_avg, chill, heating, gdd)))[1:11, 1]


# chill.days
#################
lm.c6 <- lm(log(chill.days) ~ year + sun.maysep + sun.mar + TempDiffAbs + Tavg + PRCP + SLP + WDSP + heat.days + GDD, data=cherry.dc.sub2)
summary(lm.c6)
plot(lm.c6)
acf2(resid(lm.c6))
acf2(resid(lm.c6)^2)
durbinWatsonTest(lm.c6)
durbinWatsonTest(lm.c6)

lm.t6 <- auto.arima(chill, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                    seasonal=TRUE, method="ML",
                    xreg=cbind(sun.mar, sun_ms, x_slp, prcp, tempdiff, t_avg, wdsp, heating, gdd) )
summary(lm.t6)
lm.t6$arma
checkresiduals(lm.t6)
chill.days.fut <- as.data.frame(forecast(lm.t6, 
            xreg=cbind(sun.mar, sun_ms, x_slp, prcp, tempdiff, t_avg, wdsp, heating, gdd)))[1:11, 1]


# GDD
###############
lm.c7 <- lm(GDD ~ year + sun.maysep + sun.mar + TempDiffAbs + Tavg + PRCP + SLP + WDSP + heat.days + chill.days, data=cherry.dc.sub2)
summary(lm.c7)
plot(lm.c7)
acf2(resid(lm.c7))
acf2(resid(lm.c7)^2)
durbinWatsonTest(lm.c7)
durbinWatsonTest(lm.c7)

lm.t7 <- auto.arima(gdd, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                  seasonal=TRUE, method="ML",
                  xreg=cbind(sun.mar, sun_ms, x_slp, prcp, tempdiff, t_avg, wdsp, heating, chill) )
summary(lm.t7)
lm.t7$arma
checkresiduals(lm.t7)
GDD.fut <- as.data.frame(forecast(lm.t7, 
          xreg=cbind(sun.mar, sun_ms, x_slp, prcp, tempdiff, t_avg, wdsp, heating, chill)))[1:11, 1]


# SNWD
############
lm.c8 <- lm(log(SNWD+0.01) ~ year + sun.maysep + sun.mar + TempDiffAbs + Tavg + PRCP + SLP + chill.days + heat.days + GDD + WDSP, data=cherry.dc.sub2)
summary(lm.c8)
plot(lm.c8)
acf2(resid(lm.c8))
acf2(resid(lm.c8)^2)
durbinWatsonTest(lm.c8)
durbinWatsonTest(lm.c8)

data1 <- cherry.dc.sub2[-c(26),]
plot(data1$year, log(data1$SNWD+.01))
plot(data1$sun.maysep, log(data1$SNWD+.01))
plot(data1$sun.mar, log(data1$SNWD+.01))
plot(data1$TempDiffAbs, log(data1$SNWD+.01))
plot(data1$Tavg, log(data1$SNWD+.01))
plot(data1$PRCP, log(data1$SNWD+.01))
plot(data1$SLP, log(data1$SNWD+.01))
plot(data1$chill.days, log(data1$SNWD+.01))
plot(data1$heat.days, log(data1$SNWD+.01))
plot(data1$GDD, log(data1$SNWD+.01))

lm.t8 <- auto.arima(snow, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                    seasonal=TRUE, method="ML",
                    xreg=cbind(sun.mar, sun_ms, x_slp, prcp, wdsp, tempdiff, t_avg, chill, heating, gdd) )
summary(lm.t8)
lm.t8$arma
checkresiduals(lm.t8)
SNWD.fut <- as.data.frame(forecast(lm.t8, 
                                   xreg=cbind(sun.mar, sun_ms, x_slp, prcp, wdsp, tempdiff, t_avg, chill, 
                                              heating, gdd)))[1:11, 1]



# Forecasts: Washington, DC
########################################################################################
summary(lm.r1)
summary(lm.r2)
fut.data <- data.frame(SLP=SLP.fut, PRCP=PRCP.fut, WDSP=WDSP.fut, TempDiffAbs=TempDiffAbs.fut, 
                       Tavg=Tavg.fut, chill.days=chill.days.fut, GDD=GDD.fut)
fut.data2 <- data.frame(SLP=SLP.fut, PRCP=PRCP.fut, WDSP=WDSP.fut, TempDiffAbs=TempDiffAbs.fut, 
                       Tavg=Tavg.fut, chill.days=chill.days.fut, GDD=GDD.fut, sun.mar=dc.sun.fut, 
                       sun.maysep=dc.sunms.fut)

# forecast bloom DOYs from linear models
dc.pred.lmr1 <- predict(lm.r1, newdata=fut.data)
dc.pred.lmr2 <- predict(lm.r2, newdata=fut.data2)
# predictions from linear models do not look plausible

# forecast bloom DOYs from ARIMA models
dc.arima.pred2 <- as.data.frame(forecast(dc.arima2, xreg=cbind(x_slp, prcp, wdsp, t_avg, tempdiff, 
                                                              chill, gdd, sun.mar, sun_ms)))[1:11,1]
dc.arima.pred <- as.data.frame(forecast(dc.arima, xreg=cbind(x_slp, prcp, wdsp, t_avg, tempdiff, 
                                                             chill, gdd, sun.mar, sun_ms)))[1:11,1]

# Root Mean Square Errors for each model
rmse(cherry.dc.sub$bloom_doy, fitted(lm.r1))  # linear lm.r1
rmse(cherry.dc.sub$bloom_doy, fitted(lm.r2))  # linear lm.r2
rmse(cherry.dc$bloom_doy[53:101], as.vector(dc.arima$fitted))  # ARIMA1 (0 0 0; 0 0 0), fr=1
rmse(cherry.dc$bloom_doy[53:101], as.vector(dc.arima2$fitted)) # ARIMA2 (6 0 3; 5 0 2), fr=1

# ARIMA2 has the lowest RMSE, ARIMA2 is our final model.
summary(dc.arima2)
dc.arima2$arma

# residuals plots
df3 <- data.frame(fitted=fitted(dc.arima2), residuals=residuals(dc.arima2))
ggplot(df3, aes(x=fitted, y=residuals)) +
  geom_point() 
qqPlot(residuals(dc.arima2))



# Forecast
#######################################################################
#Forecasted PBDs from Linear Model lm.r2
yr.f <- seq(from=2022, to=2032, by=1)
lm.r2.tbl <- data.frame(year=yr.f, dc.pred.lmr2)


# Final forecasted DOYs for 2022-2032 are:
dc.final.pred <- dc.arima.pred2
yr.f <- seq(from=2022, to=2032, by=1)
dc.tbl <- data.frame(year=yr.f, dc.final.pred)

# prediction for 2022: 
as.Date(dc.final.pred[1], origin="2022-01-01")





# Generalized Additive Model
##########################################
dc.gam <- gam(bloom_doy ~ s(WDSP) + s(PRCP) + s(Tavg) + s(GDD) + s(TempDiffAbs), 
              data=cherry.dc.sub, family=gaussian())
summary(dc.gam)
pacf(resid(dc.gam))



AIC(dc.gam)
acf(resid(dc.gam))
pacf(resid(dc.gam))
plot(dc.gam)
gam.check(dc.gam)
concurvity(dc.gam)

# PBD predictions from generalized additive model
dc.gam.pred <- predict(dc.gam, newdata=fut.data2)
dc.gam.pred.tbl <- data.frame(year=yr.f, GAM_forecast=dc.gam.pred)
dc.gam.pred.tbl

# RMSE for GAM model
rmse(cherry.dc.sub$bloom_doy, dc.gam$fitted.values)



dc.gam4 <- gamm(bloom_doy ~ s(sun) + s(sun.maysep) + s(Tavg), 
                data=cherry.dc.sub2, family=gaussian, 
                correlation = corAR1(form= ~1))
acf2(resid(dc.gam4))


r1 <- start_value_rho(dc.gam2, plot=TRUE)
nrow(cherry.dc.sub2)
cherry.dc.sub2$ar <- c(TRUE, rep(FALSE, 62))

dc.gam3 <- bam(bloom_doy ~ s(sun) + s(sun.maysep) + s(Tavg), 
               data=cherry.dc.sub2, rho=r1, AR.start=cherry.dc.sub2$ar)
acf2(resid(dc.gam3))



ggplot(cherry.dc.sub2, aes(sun, bloom_doy)) +
         geom_point()+
         stat_smooth(method = gam, formula = y ~ s(x))
ggplot(cherry.dc.sub2, aes(sun.maysep, bloom_doy)) +
  geom_point()+
  stat_smooth(method = gam, formula = y ~ s(x))
ggplot(cherry.dc.sub2, aes(Tavg, bloom_doy)) +
  geom_point()+
  stat_smooth(method = gam, formula = y ~ s(x))


cherry.sw.sub2 <- cherry.sw[60:122, c("sun", "sun.maysep", "Tavg", "PBD")]
sw.pre <- within(cherry.sw.sub2, rm("PBD"))
predict(dc.gam2, newdata = sw.pre)

predict(dc.gam2)




# Principle Component Regression Model
###########################################################
set.seed(7)
pcr.dc <- pcr(bloom_doy ~ Tavg + sun.maysep + GDD + I(sun.maysep^2) + sun + 
                I(sun^2) + I(sun^3), 
    data=cherry.dc.sub2, center = TRUE, scale = TRUE, validation = "LOO", jackknife = TRUE)
summary(pcr.dc)
pcr.dc$loadings
pcr.dc$coefficients
pcr.dc$Yloadings
validationplot(pcr.dc)
set.seed(7)
jack.test(pcr.dc)

set.seed(7)
pcr.dc2 <- pcr(bloom_doy ~ Tavg + sun.maysep + I(sun.maysep^2) + sun + 
                 I(sun^2) + I(sun^3), 
              data=cherry.dc.sub2, center = TRUE, scale = TRUE, validation = "LOO", jackknife = TRUE)
summary(pcr.dc2)
pcr.dc2$loadings
pcr.dc2$Yloadings
validationplot(pcr.dc2)
set.seed(7)
pcr.dc.b <- jack.test(pcr.dc2)


library(clusterSim)
cherry.sw.sub2 <- cherry.sw[60:122, c("sun", "sun.maysep", "Tavg", "PBD")]
bloom.norm2 <- data.Normalization(cherry.sw.sub2[,"PBD"], type="n1", normalization="column")
sw.pre <- within(cherry.sw.sub2, rm("PBD"))
colnames(sw.pre)
colnames(cherry.dc.sub2)
sw.pre$sun.maysepSq <- (sw.pre$sun.maysep)^2
sw.pre$sunlightSq <- (sw.pre$sun)^2
sw.pre$sunlightCu <- (sw.pre$sun)^3

predict(pcr.dc2, newdata = sw.pre )


bloom.norm <- data.Normalization(cherry.dc.sub2[,"bloom_doy"], type="n1", normalization="column")
cherry.pca <- within(cherry.dc.sub2, rm("bloom_doy"))
cherry.pca$sun.maysepSq <- (cherry.pca$sun.maysep)^2
cherry.pca$sunlightSq <- (cherry.pca$sun)^2
cherry.pca$sunlightCu <- (cherry.pca$sun)^3

detach("package:clusterSim", unload=TRUE)

pca.dc <- prcomp(cherry.pca, center=TRUE, scale=TRUE)
summary(pca.dc)
screeplot(pca.dc, type="line", main="Screeplot")
pca.dc$rotation

dc_load <- as.data.frame(pca.dc$x)
dc.pca.data <- cbind(bloom.norm, dc_load)
pca.dc.lm <- lm(bloom.norm ~ ., data = dc.pca.data)
summary(pca.dc.lm)

Z <- as.matrix(pca.dc.lm$coefficients[2:8])
R <- as.matrix(pca.dc$rotation)
beta.dc <- R %*% Z
beta.dc







#############################################################################################
## Kyoto, Japan
#############################################################################################
#############################################################################################
cherry.ja.sub <- cherry.ja[cherry.ja$year >= 1953, 
                           c("year", "sun.mar", "sun.maysep", "WDSP", "SLP", "PRCP", "TAVG", 
                             "Tavg", "heat.days", "chill.days", "GDD", "PBD", "pos.sun", 
                             "sun.time", "slap", "precip")]

tyme2 <- seq(from=1953, to=2021, by=1)
plot(tyme2, cherry.ja.sub$WDSP, type="l")
plot(tyme2, cherry.ja.sub$SLP, type="l")
plot(tyme2, cherry.ja.sub$PRCP, type="l")

bwid <- 3.49*sd(!is.na(cherry.ja.sub$SLP))*(sum(!is.na(cherry.ja.sub$SLP))^(-1/3)) / 10
hist(~SLP, data=cherry.ja.sub, w=.034)
hist(~SLP, data=cherry.ja.sub[1:42,], w=.04)

bwid <- 3.49*sd(!is.na(cherry.ja.sub$PRCP))*(sum(!is.na(cherry.ja.sub$PRCP))^(-1/3))
hist(~PRCP, data=cherry.ja.sub, w=20)

bwid <-3.49*sd(!is.na(cherry.ja.sub$WDSP))*(sum(!is.na(cherry.ja.sub$WDSP))^(-1/3))
hist(~WDSP, data=cherry.ja.sub, w=0.2)

# impute missing value for PRCP using arima modeled time series 
ts.prcp <- ts(cherry.ja$PRCP[52:122])
summary(auto.arima(ts.prcp, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                   seasonal=TRUE,method="ML"))
prcp.m1 <- Arima(ts.prcp, order = c(2, 1, 3), seasonal = list(order=c(2, 1, 2), period=3), 
                 method="ML")
prcp.m1$arma
checkresiduals(prcp.m1)
summary(prcp.m1)
# high RMSE

sd(cherry.ja$PRCP[52:122], na.rm=TRUE)
# sd even larger so we go with arima model anyway

prcp.m2 <- Arima(ts.prcp[1:54], order = c(2, 1, 3), seasonal = list(order=c(2, 1, 2), period=3), 
                 method="ML")
summary(prcp.m2)
cherry.ja[cherry.ja$year==2005,"PRCP"] <- as.vector(predict(prcp.m2, n.ahead=1)$pred)
cherry.ja.sub[cherry.ja.sub$year==2005,"PRCP"] <- as.vector(predict(prcp.m2, n.ahead=1)$pred)


# since SLP is approximately normally distributed white noise until 1994, sample missing values until 1994 from N(mu, sigma^2) distribution
st1 <- mean(cherry.ja.sub[cherry.ja.sub$year>=1953 & cherry.ja.sub$year<=1994, "SLP"], na.rm=TRUE)
st2 <- sd(cherry.ja.sub[cherry.ja.sub$year>=1953 & cherry.ja.sub$year<=1994, "SLP"], na.rm=TRUE)
sum(is.na(cherry.ja.sub[cherry.ja.sub$year>=1953 & cherry.ja.sub$year<=1994, "SLP"]))

set.seed(7)
st3 <- rnorm(11, mean=st1, sd=st2)
cherry.ja[is.na(cherry.ja$SLP), "year"]

cherry.ja[cherry.ja$year>=1965 & cherry.ja$year<=1972, "SLP"] <- st3[1:8]
cherry.ja.sub[cherry.ja.sub$year>=1965 & cherry.ja.sub$year<=1972, "SLP"] <- st3[1:8]
cherry.ja[cherry.ja$year==1975, "SLP"] <- st3[9]
cherry.ja.sub[cherry.ja.sub$year==1975, "SLP"] <- st3[9]
cherry.ja[cherry.ja$year>=1988 & cherry.ja$year<=1989, "SLP"] <- st3[10:11]
cherry.ja.sub[cherry.ja.sub$year>=1988 & cherry.ja.sub$year<=1989, "SLP"] <- st3[10:11]

# less variability and clear moving average trend after 1994. So fit MA() model to predict missing values after 1994
ts.slp <- ts(cherry.ja.sub[cherry.ja.sub$year>=1995 & cherry.ja.sub$year<=2021, "SLP"])
slp.m1 <- Arima(ts.slp, order = c(0, 0, 2), seasonal = list(order=c(0, 0, 0), period=1), 
                method="ML")
checkresiduals(slp.m1)
summary(slp.m1)

slp.m2 <- Arima(ts.slp[1:10], order = c(0, 0, 2), seasonal = list(order=c(0, 0, 0), period=1), 
                method="ML")
summary(slp.m2)
cherry.ja[cherry.ja$year==2005, "SLP"] <- as.vector(predict(slp.m2, n.ahead=1)$pred)
cherry.ja.sub[cherry.ja.sub$year==2005, "SLP"] <- as.vector(predict(slp.m2, n.ahead=1)$pred)


# model missing values for WDSP since 1973 as time series with drift
ts.wdsp <- ts(cherry.ja.sub[cherry.ja.sub$year>=1973, "WDSP"])

wdsp.m1 <- Arima(ts.wdsp, order = c(0, 0, 2), seasonal = list(order=c(0, 0, 1), period=4), 
                 method="ML", include.drift=TRUE)
checkresiduals(wdsp.m1)
summary(wdsp.m1)

cherry.ja.sub[is.na(cherry.ja.sub$WDSP), "year"] # missing value years

wdsp.m2 <- Arima(ts.wdsp[1:35], order = c(0, 0, 2), seasonal = list(order=c(0, 0, 1), period=4), 
                 method="ML", include.drift=TRUE)
summary(wdsp.m2)

wdsp.m3 <- Arima(ts.wdsp[1:52], order = c(0, 0, 2), seasonal = list(order=c(0, 0, 1), period=4), 
                 method="ML", include.drift=TRUE)
summary(wdsp.m3)

# predicted values
cherry.ja.sub[cherry.ja.sub$year==1988, "WDSP"] <- cherry.ja[cherry.ja$year==1988, "WDSP"] <-
  predict(wdsp.m2, n.ahead=2)$pred[1,1] + predict(wdsp.m2, n.ahead=2)$pred[1,2]
   
cherry.ja.sub[cherry.ja.sub$year==1989, "WDSP"] <- cherry.ja[cherry.ja$year==1989, "WDSP"] <-
  predict(wdsp.m2, n.ahead=2)$pred[2,1] + predict(wdsp.m2, n.ahead=2)$pred[2,2]

cherry.ja.sub[cherry.ja.sub$year==2005, "WDSP"] <- cherry.ja[cherry.ja$year==2005, "WDSP"] <-
  predict(wdsp.m3, n.ahead=1)$pred[1] + predict(wdsp.m3, n.ahead=1)$pred[2]

cherry.ja.sub[cherry.ja.sub$year==1975, "WDSP"] <- cherry.ja[cherry.ja$year==1975, "WDSP"] <-
  mean(wdsp.m1$fitted, na.rm=TRUE)


# TAVG
for (k in 1:nrow(cherry.ja)) {
ifelse(!is.na(cherry.ja$TMIN[k]) & !is.na(cherry.ja$TMAX[k]),
       {ifelse(is.na(cherry.ja$TAVG[k]), 
               cherry.ja$TAVG[k] <- (cherry.ja$TMAX[k] + cherry.ja$TMIN[k]) / 2,
               cherry.ja$TAVG[k] <- cherry.ja$TAVG[k])},
       cherry.ja$TAVG[k] <- cherry.ja$TAVG[k])
}
cherry.ja.sub$TAVG <- cherry.ja[cherry.ja$year>=1953, "TAVG"]

plot(tyme2, cherry.ja.sub$TAVG, type="l")

# use time series to predict missing value for 2005
ts.tavg <- ts(cherry.ja.sub$TAVG)

tavg.m1 <- Arima(ts.tavg, order = c(0, 1, 1), seasonal = list(order=c(0, 1, 1), period=8), 
                 method="ML")
checkresiduals(tavg.m1)
summary(tavg.m1)

tavg.m2 <- Arima(ts.tavg[1:52], order = c(0, 1, 1), seasonal = list(order=c(0, 1, 1), period=8), 
                 method="ML")
summary(tavg.m2)

# predicted value
cherry.ja[cherry.ja$year==2005, "TAVG"] <- cherry.ja.sub[cherry.ja.sub$year==2005, "TAVG"] <- 
  predict(tavg.m2, n.ahead=1)$pred[1]


# GDD
# use time series to predict missing value for 2005
plot(tyme2, cherry.ja.sub$GDD, type="l")
ts.gdd <- ts(cherry.ja.sub$GDD)

gdd.m1 <- auto.arima(ts.gdd)

gdd.m1 <- Arima(ts.gdd, order = c(2, 0, 2), seasonal = list(order=c(0, 0, 2), period=8), 
                 method="ML", include.drift=TRUE)
checkresiduals(gdd.m1)
summary(gdd.m1)

gdd.m2 <- Arima(ts.gdd[1:52], order = c(2, 0, 2), seasonal = list(order=c(0, 0, 2), period=8), 
                method="ML", include.drift=TRUE)
summary(gdd.m2)

# predicted value
cherry.ja[cherry.ja$year==2005, "GDD"] <- cherry.ja.sub[cherry.ja.sub$year==2005, "GDD"] <- 
  predict(gdd.m2, n.ahead=1)$pred[1] + predict(gdd.m2, n.ahead=1)$pred[2]




# Model Selection
############################
# Stepwise Regression
lm2 <- lm(PBD ~ sun.mar + sun.maysep + SLP + PRCP + TAVG + heat.days + chill.days + GDD + 
            pos.sun + sun.time + slap + precip,
          data=cherry.ja.sub, na.action = na.omit)
summary(lm2)
step.lm2 <- step(lm2, direction = "backward")

lm3 <- lm(PBD ~ sun.mar + sun.maysep + SLP + PRCP + TAVG + heat.days + chill.days + GDD + 
            pos.sun + sun.time + slap + precip + WDSP,
          data=cherry.ja.sub[cherry.ja.sub$year>=1973,], na.action = na.omit)
summary(lm3)
step.lm3 <- step(lm3, direction = "backward")

# Best subset selection
cherry.ja.pred <- cherry.ja.sub
cherry.ja.y <- cherry.ja.pred[,"PBD"]
cherry.ja.pred <- within(cherry.ja.pred, rm("PBD", "year", "WDSP"))

bestlm2 <- RegBest(y = cherry.ja.y, x = cherry.ja.pred, 
                  na.action = na.omit, method="adjr2", nbest=3)
bestlm2$best
summary(regsubsets(y = cherry.ja.y, x = cherry.ja.pred, nbest=1))


scatterplotMatrix(cherry.ja.sub)
cor(cherry.ja.sub)

ja.lm1 <- lm(PBD ~ TAVG + heat.days + chill.days + GDD, data=cherry.ja.sub)
summary(ja.lm1)
vif(ja.lm1) # high VIF for heat.days and chill.days. Take heat.days out

lm4 <- lm(PBD ~ sun.mar + sun.maysep + SLP + PRCP + TAVG + chill.days + GDD + 
            pos.sun + sun.time + slap + precip,
          data=cherry.ja.sub, na.action = na.omit)
summary(lm4)
step.lm4 <- step(lm4, direction = "backward")


# revised models with heat.days taken out
ja.lm2 <- lm(PBD ~ TAVG + chill.days + GDD + sun.time, data=cherry.ja.sub)
summary(ja.lm2)
vif(ja.lm2) # VIF ok now for all variables

ja.lm3 <- lm(PBD ~ TAVG + chill.days + GDD + sun.time + slap, data=cherry.ja.sub)
summary(ja.lm3)

ja.lm4 <- lm(PBD ~ TAVG + chill.days + GDD + sun.time + slap + PRCP, data=cherry.ja.sub)
summary(ja.lm4)

ja.lm5 <- lm(PBD ~ TAVG + chill.days + GDD + sun.time + slap + PRCP + sun.time + pos.sun, 
             data=cherry.ja.sub)
summary(ja.lm5)

ja.lm6 <- lm(PBD ~ TAVG + chill.days + GDD + sun.time + slap + PRCP + sun.time + pos.sun + sun.maysep, 
             data=cherry.ja.sub)
summary(ja.lm6)

anova(ja.lm2, ja.lm3)
anova(ja.lm2, ja.lm4)
anova(ja.lm2, ja.lm5)
anova(ja.lm2, ja.lm6)
anova(ja.lm2, lm4)

AIC(ja.lm2, ja.lm3, ja.lm4, ja.lm5, ja.lm6)
# ja.lm3 is the best balanced model
# ja.lm3 is not significantly different from ja.lm2 or the saturated model
# ja.lm3 has the highest adj. R-squared
# ja.lm3 has an AIC very close to ja.lm2

AIC(ja.lm3)
plot(ja.lm3)
acf2(resid(ja.lm3))
acf2(resid(ja.lm3)^2)
durbinWatsonTest(ja.lm3)
durbinWatsonTest(ja.lm3)



# Linear model with ARIMA adjusted errors
#################################################
cherry.ja.sub <- cherry.ja[cherry.ja$year >= 1953, 
                           c("year", "sun.mar", "sun.maysep", "WDSP", "SLP", "PRCP", "TAVG", 
                             "TempDiffAbs", "heat.days", "chill.days", "GDD", "PBD", "pos.sun", 
                             "sun.time", "slap", "precip")]
{attach(cherry.ja)
par(mfrow=c(1,1))
tyme <- seq(from=1900, to=2021, by=1)
plot(tyme, PBD, type="l")
plot(tyme, GDD, type="l")
plot(tyme, TAVG, type="l")
plot(tyme, chill.days, type="l")
plot(tyme, cherry.ja$sun.mar, type="l")
plot(tyme, cherry.ja$sun.maysep, type="l")
plot(tyme, cherry.ja$WDSP, type="l")
plot(tyme, SLP, type="l")
plot(tyme, PRCP, type="l")
plot(tyme, pos.sun, type="l")
plot(tyme, sun.time, type="l")
plot(tyme, slap, type="l")
plot(tyme, precip, type="l")
detach(cherry.ja)}

# store variables as time series for model
ja.pbd.ts <- ts(cherry.ja$PBD[54:122])
gdd <- ts(cherry.ja$GDD[54:122])
t_avg <- ts(cherry.ja$TAVG[54:122])
x_slp <- ts(cherry.ja$SLP[54:122])
ts.slp <- ts(cherry.ja$slap[54:122])
prcp <- ts(cherry.ja$PRCP[54:122])
prec <- ts(cherry.ja$precip[54:122])
wdsp <- ts(cherry.ja$WDSP[54:122])
heating <- ts(cherry.ja$heat.days[54:122])
chill <- ts(cherry.ja$chill.days[54:122])
year.ts <- ts(cherry.ja$year[54:122])
snow <-ts(cherry.ja$SNWD[54:122])
sun.mar <- ts(cherry.ja$sun.mar[54:122])
sun_ms <- ts(cherry.ja$sun.maysep[54:122])
p_sun <- ts(cherry.ja$pos.sun[54:122])
sun_t <- ts(cherry.ja$sun.time[54:122])


ja.lm3 <- lm(PBD ~ TAVG + chill.days + GDD + sun.time + slap, data=cherry.ja.sub)
summary(ja.lm3)

# auto arima
ja.arima.auto <- auto.arima(ja.pbd.ts, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                       seasonal=TRUE, method="ML",
                       xreg=cbind(t_avg, chill, gdd, sun_t, ts.slp) )
summary(ja.arima.auto)
ja.arima.auto$arma

# AR(2) model diagnostics
# code and techniques borrowed from https://otexts.com/fpp2/regarima.html
cbind("Regression Errors" = residuals(ja.arima.auto, type="regression"),
      "ARIMA errors" = residuals(ja.arima.auto, type="innovation")) %>%
  autoplot(facets=TRUE)

checkresiduals(ja.arima.auto)
# AR(2) model residuals don't quite approximate white noise, are approximately normally distributed, and the acf residual plot does not look great


############################################################
############################################################
# Run Code Here from optimize_arima.R to Replicate Results
############################################################
############################################################

checkresiduals(bestmdl)
summary(bestmdl)


ja.arima <- Arima(ja.pbd.ts, order = c(5, 0, 4), seasonal = list(order=c(1, 0, 2), period=5), 
                  method="ML", include.drift=TRUE,
                  xreg=cbind(t_avg, chill, gdd, sun_t, ts.slp) )
ja.arima$arma
checkresiduals(ja.arima)
summary(ja.arima)

ja.arima.auto$aic
bestmdl$aic
ja.arima$aic

# residuals plots
df1 <- data.frame(fitted=fitted(ja.arima), residuals=residuals(ja.arima))
ggplot(df1, aes(x=fitted, y=residuals)) +
  geom_point() 
qqPlot(residuals(ja.arima))

# final SARIMA model is
summary(ja.arima)
ja.arima$arma
checkresiduals(ja.arima)


# Forecast
##############################################################
# predictions from SARIMA model
ja.arima.pred <- as.data.frame(forecast(ja.arima, xreg=cbind(t_avg, chill, gdd, sun_t, ts.slp)))[1:11,1]

fitjanomis <- as.vector(na_remove(ja.arima$fitted))
rmse(cherry.ja$PBD[c(54:73,75:90,92:122)],fitjanomis) #RMSE

# Final unadjusted forecasted DOYs for 2022-2032 are:
ja.final.pred <- ja.arima.pred - 7*(1-(0.8*(1/0.7)))
ja.pred.tbl <- data.frame(year=yr.f, ja.final.pred)

# forecast date for 2022: 
as.Date(ja.final.pred[1], origin="2022-01-01")





#############################################################################################
## Liestal, Switzerland
#############################################################################################
#############################################################################################
colnames(cherry.sw)
cherry.sw.sub <- cherry.sw[cherry.sw$year >= 1931, 
                           c("sun.mar", "sun.maysep", "PRCP", "SNWD", "Tavg", "TempDiffAbs", 
                             "heat.days", "chill.days", "GDD", "PBD")] 
cherry.sw.sub2 <- cherry.sw[cherry.sw$year >= 1954, 
                           c("sun.mar", "sun.maysep", "PRCP", "SNWD", "Tavg", "TempDiffAbs", 
                             "heat.days", "chill.days", "GDD", "PBD")] 


lm5 <- lm(PBD ~ sun.mar + sun.maysep + PRCP + SNWD + Tavg + TempDiffAbs + heat.days + chill.days + GDD,
          data=cherry.sw.sub, na.action = na.omit)
summary(lm5)
# Stepwise Regression
step.lm5 <- step(lm5, direction = "backward")

# Best subset selection
cherry.sw.pred <- cherry.sw[cherry.sw$year >= 1931, 
                            c("sun.mar", "sun.maysep", "PRCP", "SNWD", "Tavg", "TempDiffAbs", 
                              "heat.days", "chill.days", "GDD", "PBD")] %>% filter(complete.cases(.))
cherry.sw.y <- cherry.sw.pred[,"PBD"]
cherry.sw.pred <- within(cherry.sw.pred, rm("PBD"))

bestlm3 <- RegBest(y = cherry.sw.y, x = cherry.sw.pred, 
                  na.action = na.omit, method="adjr2", nbest=3)
bestlm3$best
summary(regsubsets(y = cherry.sw.y, x = cherry.sw.pred, nbest=1 ))

scatterplotMatrix(cherry.sw.sub)
cor(cherry.sw.sub)

# linear models
sw.lm1 <- lm(PBD ~ sun.maysep + TempDiffAbs + heat.days + chill.days + GDD, 
             data = cherry.sw.sub)
summary(sw.lm1)

sw.lm2 <- lm(PBD ~ sun.maysep + TempDiffAbs + heat.days + chill.days + GDD + SNWD, 
             data = cherry.sw.sub)
summary(sw.lm2)

vif(sw.lm1) # high VIF for heat.days. Remove heat.days

# Refit most covariates model without heat.days
lm6 <- lm(PBD ~ sun.mar + sun.maysep + PRCP + SNWD + Tavg + TempDiffAbs + chill.days + GDD,
          data=cherry.sw.sub, na.action = na.omit)
summary(lm6)

# Stepwise Regression
step.lm6 <- step(lm6, direction = "backward")

# Best subset selection
cherry.sw.pred <- cherry.sw[cherry.sw$year >= 1931, 
                            c("sun.mar", "sun.maysep", "PRCP", "SNWD", "Tavg", "TempDiffAbs", 
                              "chill.days", "GDD", "PBD")] %>% filter(complete.cases(.))
cherry.sw.y <- cherry.sw.pred[,"PBD"]
cherry.sw.pred <- within(cherry.sw.pred, rm("PBD"))

bestlm3 <- RegBest(y = cherry.sw.y, x = cherry.sw.pred, 
                   na.action = na.omit, method="adjr2", nbest=3)
bestlm3$best

bestlm4 <- RegBest(y = cherry.sw.y, x = cherry.sw.pred, 
                   na.action = na.omit, method="Cp", nbest=3)
bestlm4$best

cherry.sw.pred <- cherry.sw[cherry.sw$year >= 1954, 
                            c("sun.mar", "sun.maysep", "PRCP", "SNWD", "Tavg", "TempDiffAbs", 
                              "chill.days", "GDD", "PBD")] %>% filter(complete.cases(.))
cherry.sw.y <- cherry.sw.pred[,"PBD"]
cherry.sw.pred <- within(cherry.sw.pred, rm("PBD", "chill.days"))

bestlm5 <- RegBest(y = cherry.sw.y, x = cherry.sw.pred, 
                   na.action = na.omit, method="adjr2", nbest=3)
bestlm5$best

# Fit model with lowest AIC
sw.r1 <- lm(PBD ~ sun.maysep + TempDiffAbs + GDD, 
             data = cherry.sw.sub, na.action=na.omit)
summary(sw.r1)

sw.r2 <- lm(PBD ~ sun.maysep + TempDiffAbs + GDD + Tavg, 
            data = cherry.sw.sub, na.action=na.omit)
summary(sw.r2)

# Fit model with highest adj. R-squared
sw.r3 <- lm(PBD ~ sun.maysep + TempDiffAbs + GDD + Tavg + chill.days, 
            data = cherry.sw.sub, na.action=na.omit)
summary(sw.r3)

sw.r4 <- lm(PBD ~ sun.maysep + TempDiffAbs + GDD + Tavg + chill.days + sun.mar + PRCP, 
            data = cherry.sw.sub, na.action=na.omit)
summary(sw.r4)

AIC(sw.r1, sw.r2) # sw.r2 has the lowest AIC for 1931-2021 data
AIC(sw.r3, sw.r4) # sw.r3 has the lowest AIC for 1954-2021 data

# Compare models sw.r2 and sw.r3
rmse(cherry.sw.sub$PBD, sw.r2$fitted.values)
rmse(cherry.sw.sub[!is.na(cherry.sw.sub$chill.days),"PBD"], sw.r3$fitted.values)
# sw.r3 has the lower RMSE so we proceed with model sw.r3

# sw.r3 consistent with most complex model
anova(sw.r3, sw.r4)
anova(sw.r3, lm6)

# Diagnostics
vif(sw.r3)
plot(sw.r3)
acf2(resid(sw.r3))
acf2(resid(sw.r3)^2)
durbinWatsonTest(sw.r3)
durbinWatsonTest(sw.r3)

{attach(cherry.sw)
  par(mfrow=c(1,1))
  tyme <- seq(from=1900, to=2021, by=1)
  plot(tyme, PBD, type="l")
  plot(tyme, GDD, type="l")
  plot(tyme, Tavg, type="l")
  plot(tyme, TempDiffAbs, type="l")
  plot(tyme, cherry.sw$sun.maysep, type="l")
  plot(tyme, chill.days, type="l")
  plot(tyme, PRCP, type="l")
  plot(tyme, SNWD, type="l")
  plot(tyme, cherry.sw$sun.mar, type="l")
  plot(tyme, cherry.sw$WDSP, type="l")
  plot(tyme, SLP, type="l")
  detach(cherry.sw)}

# store variables as time series for model
sw.pbd.ts <- ts(cherry.sw$PBD[55:122])
gdd <- ts(cherry.sw$GDD[55:122])
t_avg <- ts(cherry.sw$Tavg[55:122])
tempdif <- ts(cherry.sw$TempDiffAbs[55:122])
ts.slp <- ts(cherry.sw$SLP[55:122])
prcp <- ts(cherry.sw$PRCP[55:122])
snow <-ts(cherry.sw$SNWD[55:122])
wdsp <- ts(cherry.sw$WDSP[55:122])
heating <- ts(cherry.sw$heat.days[55:122])
chill <- ts(cherry.sw$chill.days[55:122])
year.ts <- ts(cherry.sw$year[55:122])
sun.mar <- ts(cherry.sw$sun.mar[55:122])
sun_ms <- ts(cherry.sw$sun.maysep[55:122])

# original linear model
sw.r3 <- lm(PBD ~ sun.maysep + TempDiffAbs + GDD + Tavg + chill.days, 
            data = cherry.sw.sub, na.action=na.omit)

# auto.arima
sw.auto.arima <- auto.arima(sw.pbd.ts, max.p=5, max.d=3, max.q=5, max.P=5, max.D=3, max.Q=5, 
                            seasonal=TRUE, method="ML",
                       xreg=cbind(sun_ms, tempdif, gdd, t_avg, chill) )
checkresiduals(sw.auto.arima)
sw.auto.arima$arma
summary(sw.auto.arima)



############################################################
############################################################
# Run Code Here from optimize_arima.R to Replicate Results
############################################################
############################################################
# loop through models to find best possible model
checkresiduals(bestmdl)
summary(bestmdl)
bestmdl$arma

# manually adjust bestmdl
sw.arima <- Arima(sw.pbd.ts, order = c(1, 0, 0), seasonal = list(order=c(2, 0, 5), period=1), 
                  method="ML", include.drift=FALSE,
                  xreg=cbind(sun_ms, tempdif, gdd, t_avg, chill) )
sw.arima$arma
checkresiduals(sw.arima)
summary(sw.arima)

# analysis just on the time series PBD
sw.auto.arima2 <- auto.arima(sw.pbd.ts, max.p=5, max.d=3, max.q=5, max.P=5, max.D=3, max.Q=5, 
                        seasonal=TRUE, method="ML" )
checkresiduals(sw.auto.arima2)
sw.auto.arima2$arma
summary(sw.auto.arima2)


############################################################
############################################################
# Run Code Here from optimize_arima.R to Replicate Results
############################################################
############################################################
checkresiduals(bestmdl2)
summary(bestmdl2)
bestmdl2$arma

sw.arima2 <- Arima(sw.pbd.ts, order = c(0, 2, 0), seasonal = list(order=c(6, 2, 2), period=8), 
                  method="ML", include.drift=FALSE)
sw.arima2$arma
checkresiduals(sw.arima2)
summary(sw.arima2)


# Predict Covariates
###################################################
# Tavg
##########
lm.c1 <- lm(Tavg ~ year + sun.maysep + sun.mar + PRCP + SNWD + TempDiffAbs + chill.days + heat.days + GDD
            , data=cherry.sw.sub2)
summary(lm.c1)

lm.s1 <- auto.arima(t_avg, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                    seasonal=TRUE, method="ML",
                    xreg=cbind(sun.mar, sun_ms, snow, prcp, tempdif, chill, heating, gdd) )
lm.s1$arma
checkresiduals(lm.s1)
summary(lm.s1)

Tavg.futs <- as.data.frame(forecast(lm.s1, xreg=cbind(sun.mar, sun_ms, snow, prcp, tempdif, chill, 
                                                      heating, gdd )))[1:11, 1]

# TempDiffAbs
###############
lm.c2 <- lm(TempDiffAbs ~ year + sun.maysep + sun.mar + PRCP + SNWD + Tavg + chill.days + heat.days + GDD
            , data=cherry.sw.sub2)
summary(lm.c2)

lm.s2 <- auto.arima(tempdif, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                    seasonal=TRUE, method="ML",
                    xreg=cbind(sun.mar, sun_ms, snow, prcp, t_avg, chill, heating, gdd) )
summary(lm.s2)
lm.s2$arma
checkresiduals(lm.s2)
TempDiffAbs.futs <- as.data.frame(forecast(lm.s2, 
                                          xreg=cbind(sun.mar, sun_ms, snow, prcp, t_avg, chill, heating, gdd)))[1:11, 1]

# chill.days
#################
lm.c6 <- lm(chill.days ~ year + sun.maysep + sun.mar + PRCP + SNWD + TempDiffAbs + Tavg + heat.days + GDD
            , data=cherry.sw.sub2)
summary(lm.c6)
acf2(resid(lm.c6))

lm.s6 <- auto.arima(chill, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                    seasonal=TRUE, method="ML",
                    xreg=cbind(sun.mar, sun_ms, snow, prcp, t_avg, tempdif, heating, gdd) )
lm.s6$arma
checkresiduals(lm.s6)
summary(lm.s6)

chill.days.futs <- as.data.frame(forecast(lm.s6, xreg=cbind(sun.mar, sun_ms, snow, prcp, t_avg, tempdif, 
                                                           heating, gdd)))[1:11, 1]

# GDD
###############
lm.c7 <- lm(GDD ~ year + sun.maysep + sun.mar + PRCP + SNWD + TempDiffAbs + Tavg + heat.days + chill.days
            , data=cherry.sw.sub2)
summary(lm.c7)
acf2(resid(lm.c7))

lm.s7 <- auto.arima(gdd, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                    seasonal=TRUE, method="ML",
                    xreg=cbind(sun.mar, sun_ms, snow, prcp, t_avg, tempdif, heating, chill) )
lm.s7$arma
checkresiduals(lm.s7)
summary(lm.s7)

GDD.futs <- as.data.frame(forecast(lm.s7, xreg=cbind(sun.mar, sun_ms, snow, prcp, t_avg, tempdif, 
                                                     heating, chill)))[1:11, 1]

# store variables as time series for model
sw.pbd.ts2 <- ts(cherry.sw$PBD[32:122])
gdd2 <- ts(cherry.sw$GDD[32:122])
t_avg2 <- ts(cherry.sw$Tavg[32:122])
tempdif2 <- ts(cherry.sw$TempDiffAbs[32:122])
sun_ms2 <- ts(cherry.sw$sun.maysep[32:122])
# auto.arima
sw.auto.arima3 <- auto.arima(sw.pbd.ts2, max.p=5, max.d=3, max.q=5, max.P=5, max.D=3, max.Q=5, 
                            seasonal=TRUE, method="ML",
                            xreg=cbind(sun_ms2, tempdif2, gdd2, t_avg2) )
checkresiduals(sw.auto.arima)
sw.auto.arima$arma
summary(sw.auto.arima)


# comparison of SARIMA models (since 1954 data)
sw.auto.arima$aic
bestmdl$aic
sw.arima$aic

# sw.r3 model covariate projections
sw.r3
yr.futs <- seq(from=2022, to=2032, by=1)
futs.data <- data.frame(year=yr.futs, sun.maysep=sw.sunms.fut, TempDiffAbs=TempDiffAbs.futs, 
                        Tavg=Tavg.futs, chill.days=chill.days.futs, GDD=GDD.futs)
# projections from sw.r3
sw.r3.pred <- predict(sw.r3, newdata=futs.data)

fed1 <- anova(sw.r3)
tail(fed1$`Sum Sq`, n=1)
fed1$`Mean Sq`
fed2 <- summary(sw.r3)
fed2$r.squared


SSE <- tail(anova(sw.r3)$`Sum Sq`, n=1)
R2 <- summary(sw.r3)$r.squared

SST = SSE / (1 - R2)


sw.pbd.ts3 <- ts(sw.pbd.ts, frequency=5)
sw.r3n <- tslm(sw.pbd.ts3 ~ trend + season)
summary(sw.r3n)
plot(forecast(sw.r3n))

# predictions from PBD time series without covariates
sw.arima2.pred <- predict(sw.arima2, n.ahead=11)
forecast(sw.arima2, h=11)

# predictions from time series since 1931
forecast(sw.auto.arima3, xreg=cbind(sun_ms2, tempdif2, gdd2, t_avg2))

# RMSE
rmse(cherry.sw[cherry.sw$year>=1954, "PBD"][-c(62:63)], sw.auto.arima$fitted[-c(62:63)]) 
rmse(cherry.sw[cherry.sw$year>=1954, "PBD"][-c(62:63)], bestmdl$fitted[-c(62:63)]) 
rmse(cherry.sw[cherry.sw$year>=1954, "PBD"][-c(62:63)], sw.arima$fitted[-c(62:63)])
rmse(cherry.sw[cherry.sw$year>=1954 & !is.na(cherry.sw$chill.days), "PBD"], sw.r3$fitted.values)
rmse(cherry.sw[cherry.sw$year>=1954, "PBD"], sw.arima2$fitted)
rmse(cherry.sw[cherry.sw$year>=1931, "PBD"], sw.auto.arima3$fitted)
# sw.arima2 has the lowest RMSE but does not yield sensible forecasts and high variablity in errors at end of time series. sw.arima is the best balanced model

# final SARIMA model is
summary(sw.arima)
sw.arima$arma
checkresiduals(sw.arima)
sw.arima$call




# Forecast
##############################################################
# Forecasts from Linear Model sw.r3
sw.r3.pred.adj <- sw.r3.pred - ((1-(0.25*(1/0.7)))*7 - 0.5)
sw.pred.lm.tbl <- data.frame(year=yr.f, liestal=sw.r3.pred.adj)

# predictions from SARIMA model
sw.arima.pred <- as.data.frame(forecast(sw.arima, 
                                        xreg=cbind(sun_ms, tempdif, gdd, t_avg, chill)))[1:11,1]

# Final unadjusted forecasted DOYs for 2022-2032 are:
sw.final.pred <- sw.arima.pred - ((1-(0.25*(1/0.7)))*7 - 0.5)
sw.pred.tbl <- data.frame(year=yr.f, liestal=sw.final.pred)

# forecast date for 2022: 
as.Date(sw.final.pred[1], origin="2022-01-01")

# forecast does not appear sensible
plot(tyme, cherry.sw$PBD, type="l")






############################################################################################
# Vancouver, Canada
############################################################################################
############################################################################################
cherry.df <- cherry.dc[, c("year", "location", "lat", "long", "alt", "bloom_date", "bloom_doy", "PBD", "Adj.bloom.date", "hardiness.zone", "sun", "sun.maysep", "sun.mar", "WDSP", "SLP", "PRCP", "SNWD", "TMIN", "TMAX", "TAVG", "Tavg", "TempDiffAbs", "TempDiffSq", "ppd", "heat.days", "chill.days", "GDD", "tavg.m", "tmin.m", "tmax.m", "prcp.m", "gdd.m")] %>% 
  rbind(cherry.ja[, c("year", "location", "lat", "long", "alt", "bloom_date", "bloom_doy", "PBD", "Adj.bloom.date", "hardiness.zone", "sun", "sun.maysep", "sun.mar", "WDSP", "SLP", "PRCP", "SNWD", "TMIN", "TMAX", "TAVG", "Tavg", "TempDiffAbs", "TempDiffSq", "ppd", "heat.days", "chill.days", "GDD", "tavg.m", "tmin.m", "tmax.m", "prcp.m", "gdd.m")]) %>% 
  rbind(cherry.sw[, c("year", "location", "lat", "long", "alt", "bloom_date", "bloom_doy", "PBD", "Adj.bloom.date", "hardiness.zone", "sun", "sun.maysep", "sun.mar", "WDSP", "SLP", "PRCP", "SNWD", "TMIN", "TMAX", "TAVG", "Tavg", "TempDiffAbs", "TempDiffSq", "ppd", "heat.days", "chill.days", "GDD", "tavg.m", "tmin.m", "tmax.m", "prcp.m", "gdd.m")]) 

cherry.df$hardiness.zone <- factor(cherry.df$hardiness.zone,
                                   levels=c("7a", "7b", "9b"))
levels(cherry.df$hardiness.zone)<- c("7a", "7b", "8a", "8b", "9a", "9b")
cherry.df$hardiness.zone <- factor(cherry.df$hardiness.zone, ordered=TRUE,
                                   levels=c("7a", "7b", "8a", "8b", "9a", "9b"))

cherry.bc$hardiness.zone <- factor(cherry.bc$hardiness.zone,
                                   levels=c("8b"))
levels(cherry.bc$hardiness.zone)<- c("7a", "7b", "8a", "8b", "9a", "9b")
cherry.bc$hardiness.zone <- factor(cherry.bc$hardiness.zone, ordered=TRUE,
                                   levels=c("7a", "7b", "8a", "8b", "9a", "9b"))
cherry.bc$hardiness.zone <- factor("8b", ordered=TRUE,
                                   levels=c("7a", "7b", "8a", "8b", "9a", "9b"))

# select observations since 1956
cherry.df.s <- cherry.df[cherry.df$year >= 1956,]

summary(cherry.ja)
summary(cherry.dc)
summary(cherry.sw)


# Most complex model
all.m1 <- lm(PBD ~ lat + alt + hardiness.zone + location + sun.mar + sun.maysep + prcp.m + tavg.m + gdd.m, 
             data=cherry.df, na.action=na.omit) 
summary(all.m1)

# Stepwise Regression
step.lm7 <- step(all.m1, direction = "backward")

all.m2 <- lm(PBD ~ gdd.m + alt + sun.mar + sun.maysep + prcp.m + lat + tavg.m, data=cherry.df)
summary(all.m2)

all.m3 <- lm(PBD ~ sun.maysep + prcp.m + lat + tavg.m, data=cherry.df)
summary(all.m3)

vif(all.m2) 

all.m4 <- lm(PBD ~ gdd.m + alt + sun.mar + sun.maysep + prcp.m + tavg.m, data=cherry.df)
summary(all.m4) #remove lat
all.m5 <- lm(PBD ~ gdd.m + alt + prcp.m + lat + tavg.m, data=cherry.df)
summary(all.m5) # remove sun.mar and sun.maysep
AIC(all.m4, all.m5)
# remove lat
# go with all.m4


# Stepwise Regression
step.lm7 <- step(all.m4, direction = "backward")

all.m6 <- lm(PBD ~ gdd.m + alt + sun.mar + sun.maysep + prcp.m + tavg.m, data=cherry.df)
summary(all.m6)
vif(all.m6) # high VIF; remove sun.mar


all.m7 <- lm(PBD ~ gdd.m + alt + sun.maysep + prcp.m + tavg.m, data=cherry.df)
summary(all.m7)

# Stepwise Regression
step.lm8 <- step(all.m7, direction = "backward")

all.m8 <- lm(PBD ~ prcp.m + alt + sun.maysep +  tavg.m, data=cherry.df)
summary(all.m8)
vif(all.m8) # VIF high; remove alt

all.m9 <- lm(PBD ~ prcp.m + alt + sun.maysep + tavg.m + gdd.m, data=cherry.df)
summary(all.m9)
vif(all.m9)


all.m10 <- lm(PBD ~ prcp.m + sun.maysep + tavg.m + gdd.m, data=cherry.df)
summary(all.m10)

# Stepwise Regression
step.lm9 <- step(all.m10, direction = "backward")


all.lm <- lm(PBD ~ prcp.m + sun.maysep + tavg.m + gdd.m, data=cherry.df)
summary(all.lm)

all.lm1 <- lm(PBD ~ prcp.m + tavg.m + gdd.m , data=cherry.df)
summary(all.lm1)
vif(all.lm1)

AIC(all.lm, all.lm1)
anova(all.lm, all.lm1)

# Diagnostics
vif(all.lm)
plot(all.lm)
acf2(resid(all.lm))
acf2(resid(all.lm)^2)
durbinWatsonTest(all.lm)
durbinWatsonTest(all.lm)


# not working
all.lm <- gls(PBD ~ prcp.m + sun.maysep + tavg.m + gdd.m, data=cherry.df,
             correlation=corAR1(1, form= ~1 | location, p=2, q=1))
summary(all.lm)
acf2(resid(all.lm))



cherry.df.sub <- cherry.df[, c("location", "PBD", "prcp.m", "tavg.m", "gdd.m", "hardiness.zone")] %>% 
  filter(complete.cases(.))

nrow(cherry.df.sub)
nrow(cherry.df.sub[cherry.df.sub$location == "washingtondc", ])
nrow(cherry.df.sub[cherry.df.sub$location == "kyoto", ])
nrow(cherry.df.sub[cherry.df.sub$location == "liestal", ])

# RMSE of all.lm1
rmse(cherry.df.sub[cherry.df.sub$location == "washingtondc", "PBD"], all.lm$fitted.values[1:66])
rmse(cherry.df.sub[cherry.df.sub$location == "kyoto", "PBD"], all.lm$fitted.values[67:136])
rmse(cherry.df.sub[cherry.df.sub$location == "liestal", "PBD"], all.lm$fitted.values[137:208])


# Final Linear Model
####################
all.lm <- lm(PBD ~ prcp.m + sun.maysep + tavg.m + gdd.m, data=cherry.df)
summary(all.lm)



# Data for Vancouver
vancouver.data <- cherry.bc[ ,c("year", "prcp.m", "tavg.m", "gdd.m", "sun.maysep")]

# create time series
bc.prcp <- ts(cherry.bc$prcp.m)
bc.tavg <- ts(cherry.bc$tavg.m)
bc.gdd <- ts(cherry.bc$gdd.m)
bc.sms <- ts(cherry.bc$sun.maysep)
bc.hz <- ts(cherry.bc$hardiness.zone)


# predict prcp.m
bc.prcp.fut <- auto.arima(bc.prcp, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                          seasonal=TRUE, method="ML",
                          xreg=cbind(bc.tavg, bc.gdd) )
summary(bc.prcp.fut)
bc.prcp.fut$arma
bc.prcp.pred <- as.data.frame(forecast(bc.prcp.fut, xreg=cbind(bc.tavg, bc.gdd)))[1:11,1]


# predict tavg.m
bc.tavg.fut <- auto.arima(bc.tavg, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                          seasonal=TRUE, method="ML",
                          xreg=cbind(bc.prcp, bc.gdd) )
summary(bc.tavg.fut)
bc.tavg.fut$arma
bc.tavg.pred <- as.data.frame(forecast(bc.tavg.fut, xreg=cbind(bc.prcp, bc.gdd)))[1:11,1]


# predict gdd.m
bc.gdd.fut <- auto.arima(bc.gdd, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                          seasonal=TRUE, method="ML",
                          xreg=cbind(bc.prcp, bc.tavg) )
summary(bc.gdd.fut)
bc.gdd.fut$arma
bc.gdd.pred <- as.data.frame(forecast(bc.gdd.fut, xreg=cbind(bc.prcp, bc.tavg)))[1:11,1]


# forecasted covariates dataframe
yr.f <- seq(from=2022, to=2032, by=1)
pred.covs <- data.frame(year=yr.f, prcp.m=bc.tavg.pred, tavg.m=bc.tavg.pred, gdd.m=bc.gdd.pred, sun.maysep=bc.sunms.fut)
vancouver.data <- rbind(vancouver.data, pred.covs)



# Vancouver forecast for 2022-2032:
############################################################
predict.bc <- predict(all.lm, newdata = pred.covs)
predict.tbl <- data.frame(year=yr.f, vancouver_doy=predict.bc)
predict.tbl




# VAR Model
#############################################################

#################
# Load Libraries
#################
library(tseries)
library(vars)
library(fpp2)
library(MTS)


# make time series for PBDs
ts.pbd.ja <- ts(cherry.df.s[cherry.df.s$location=="kyoto", "PBD"])
ts.pbd.dc <- ts(cherry.df.s[cherry.df.s$location=="washingtondc", "PBD"])
ts.pbd.sw <- ts(cherry.df.s[cherry.df.s$location=="liestal", "PBD"])

cor.df <- data.frame(ts.pbd.ja, ts.pbd.dc, ts.pbd.sw)
cor(cor.df)

# test for cointegration
po.test(cbind(ts.pbd.ja, ts.pbd.dc, ts.pbd.sw))
# strong evidence that the series are cointegrated so series are appropriate for modelling with VAR

# test for stationarity
adf.test(ts.pbd.ja)
adf.test(ts.pbd.dc)
adf.test(ts.pbd.sw)
pp.test(ts.pbd.ja)
pp.test(ts.pbd.dc)
pp.test(ts.pbd.sw)
# we can reject the null hypothesis that the series have unit roots.
# PBDs are stationary series.
# Use VAR in levels.

zt <- data.frame(kyoto=cherry.ja[cherry.ja$year>=1954, "PBD"],
                 washingtondc=cherry.dc[cherry.dc$year>=1954, "sPBD"],
                 liestal=cherry.sw[cherry.sw$year>=1954, "PBD"])

cherry.var <- cherry.df[cherry.df$year >= 1954, ] %>% 
  select(location, year, PBD, Tavg, GDD, hardiness.zone, sun.maysep)

xt <- data.frame(GDD.ja=cherry.var[cherry.var$location=="kyoto", "GDD"],
                 GDD.dc=cherry.var[cherry.var$location=="washingtondc", "GDD"],
                 GDD.sw=cherry.var[cherry.var$location=="liestal", "GDD"])

VARselect(as.matrix(zt, nrow=68))
all.var <- VAR(as.matrix(zt, nrow=68), p=1)
all.var$bic



# Forecast with VAR Model
###########################################################################
VARpred(all.var, h=11)

# Forecasted dates for DC, Kyoto, and Liestal from VAR model
table1 <- as.data.frame(VARpred(all.var, h=11)[1])
cities <- data.frame(year=yr.f, table1)
cities


# Forecasted dates for Vancouver from VAR model
vancouver.var.preds <- data.frame(year=yr.f, vancouver_doy=rowMeans(table1))
vancouver.var.preds





VARX(zt=zt, p=1, include.mean=T)





boxplot(blooms$DC_doy, blooms$liestal_doy,
        xlab = c("Washington", "Liestal"))


for (i in 1:nrow(cherry.df)) {
  ifelse(is.na(cherry.df$location[i]), 
         cherry.df$location[i] <- "kyoto", 
         cherry.df$location[i] <- cherry.df$location[i])
}
for (i in 1:nrow(cherry.df)) {
  ifelse(cherry.df$location[i] == "kyoto", cherry.df$location[i] <- "Kyoto",
         ifelse(cherry.df$location[i] == "washingtondc", cherry.df$location[i] <- "Washington",
                ifelse(cherry.df$location[i] == "liestal", cherry.df$location[i] <- "Liestal", u<-2)))
}


cbPalette <- c("#000000", "#E69F00", "#56B4E9")
ggplot(cherry.df, aes(x=year, y=bloom_doy, group=location)) + 
  geom_line(aes(color=location), size=.75) + 
  scale_colour_manual(values=cbPalette) +
  labs(title="Fig. 1 - Historic Number of Days to Peak Bloom by Location",
       x="Year", y="Number of days to peak bloom")

cbPalette2 <- c("#009E73", "#E69F00", "#56B4E9")
ggplot(cherry.df, aes(x=location, y=bloom_doy)) + 
  geom_boxplot(fill=cbPalette2) + 
  labs(title="Fig. 2 - Boxplots of Number of Days to Peak Bloom by Location",
       x="Location", y="Number of days to peak bloom")

summary(cherry.df[cherry.df$location=="Washington", "bloom_doy"])



# Predictive R-squared
# @author Thomas Hopper
# code and methods for computing predicted R-squared are borrowed from: https://rpubs.com/RatherBit/102428
# and https://tomhopper.me/2014/05/16/can-we-do-better-than-r-squared/

# predicted residual sums of squares (PRESS)
PRESS <- function(linear.model) {
  PRESS <- sum( (residuals(linear.model)/(1-lm.influence(linear.model)$hat))^2 )
  return(PRESS)
}

# predicted R-squared
pred_r_squared <- function(linear.model) {
  TSS <- sum(anova(linear.model)$'Sum Sq')
  pred.r.squared <- 1-PRESS(linear.model)/(TSS)
  return(pred.r.squared)
}



# Washington, DC
#################
pred_r_squared(lm.r2)
summary(lm.r2)$r.squared

# Liestal
#############
pred_r_squared(sw.r3)
summary(sw.r3)$r.squared

# Vancouver
##############
pred_r_squared(all.lm)
summary(all.lm)$r.squared








###########################################################################################



# create time series
ts.pbd.ja <- ts(cherry.df.s[cherry.df.s$location=="kyoto", "PBD"])
ts.pbd.dc <- ts(cherry.df.s[cherry.df.s$location=="washingtondc", "PBD"])
ts.pbd.sw <- ts(cherry.df.s[cherry.df.s$location=="liestal", "PBD"])

ts.prcp.ja <- ts(cherry.df.s[cherry.df.s$location=="kyoto", "prcp.m"])
ts.prcp.dc <- ts(cherry.df.s[cherry.df.s$location=="washingtondc", "prcp.m"])
ts.prcp.sw <- ts(cherry.df.s[cherry.df.s$location=="liestal", "prcp.m"])

ts.tavg.ja <- ts(cherry.df.s[cherry.df.s$location=="kyoto", "tavg.m"])
ts.tavg.dc <- ts(cherry.df.s[cherry.df.s$location=="washingtondc", "tavg.m"])
ts.tavg.sw <- ts(cherry.df.s[cherry.df.s$location=="liestal", "tavg.m"])

ts.sun.ja <- ts(cherry.df.s[cherry.df.s$location=="kyoto", "sun"])
ts.sun.dc <- ts(cherry.df.s[cherry.df.s$location=="washingtondc", "sun"])
ts.sun.dc <- ts(cherry.df.s[cherry.df.s$location=="liestal", "sun"])



all.lm1 <- lm(PBD ~ Tavg + GDD + hardiness.zone, data=cherry.df)
summary(all.lm1)

all.lm2 <- lm(PBD ~ Tavg + GDD + hardiness.zone + sun.maysep, data=cherry.df)
summary(all.lm2)

all.lm3 <- lm(PBD ~ Tavg + GDD + hardiness.zone + sun.maysep + chill.days, data=cherry.df)
summary(all.lm3)

AIC(all.lm, all.lm1, all.lm2)
# all.lm2 has the lowest AIC and has a higher R-squared than all.lm3
# all.lm3 has fewer observations.
# Thus, all.lm2 is our provisional linear model










acf2(resid(all.lm))
vif(all.lm)
plot(all.lm)







var.df <- cherry.df.s[,c("Tavg", "sun", "sun.maysep")]
y.df <- data.frame(JA = c(ts.pbd.ja), DC = c(ts.pbd.dc), SW = c(ts.pbd.sw))


all.var <- ar(cbind(ts.pbd.ja, ts.pbd.dc, ts.pbd.sw))
all.var$ar
acf(all.var$resid[-1,])

#VARselect(, lag.max = 1,
          #type = “const”, exogen = cbind())

all.var2 <- VAR(cbind(ts.pbd.ja, ts.pbd.dc, ts.pbd.sw), p=1 )
coef(all.var2)
predict(all.var2, n.ahead=10)





