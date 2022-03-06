
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
lm.b1 <- lm(bloom_doy ~ SNWD + WDSP + PRCP + SLP + TempDiffABs + chill.days + heat.days + GDD, 
            data = cherry.dc.sub)
summary(lm.b1)

lm.b2 <- lm(bloom_doy ~ WDSP + SLP + PRCP + SNWD + TempDiffAbs + heat.days + chill.days + GDD,
            data=cherry.dc.sub)
summary(lm.b2)

lm.b3 <- lm(bloom_doy ~ SNWD + WDSP + PRCP + SLP + TempDiffABs + chill.days + heat.days + GDD + sun.maysep,
            data=cherry.dc.sub)
summary(lm.b3)

lm.b4 <- lm(bloom_doy ~ SNWD + WDSP + PRCP + SLP + TempDiffABs + chill.days + heat.days + GDD + sun.mar,
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

dc.arima2 <- Arima(dc.bloom.ts, order = c(6, 0, 3), seasonal = list(order=c(5, 0, 2), period=1), 
                   method="ML",
                   xreg=cbind(x_slp, prcp, wdsp, t_avg, tempdiff, chill, gdd, sun.mar, sun_ms) )
dc.arima2$arma
checkresiduals(dc.arima2)
summary(dc.arima2)

dc.arima$aic
dc.arima2$aic
# final arima model is
summary(dc.arima2)

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

lm.t1 <- auto.arima(t_avg, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
            seasonal=TRUE, method="ML",
            xreg=cbind(sun.mar, sun_ms, wdsp, x_slp, prcp, tempdiff, chill, heating, gdd) )
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

lm.t2 <- auto.arima(tempdiff, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                    seasonal=TRUE, method="ML",
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

lm.t3 <- auto.arima(prcp, max.p=5, max.d=5, max.q=5, max.P=5, max.D=5, max.Q=5, 
                    seasonal=TRUE, method="ML",
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
                                                             chill, gdd, sun.mar, snow, sun_ms)))[1:11,1]

# Root Mean Square Errors for each model
rmse(cherry.dc.sub$bloom_doy, fitted(lm.r1))  # linear lm.r1
rmse(cherry.dc.sub$bloom_doy, fitted(lm.r2))  # linear lm.r2
rmse(cherry.dc$bloom_doy[53:101], as.vector(dc.arima$fitted))  # ARIMA1 (0 0 0; 0 0 0), fr=1
rmse(cherry.dc$bloom_doy[53:101], as.vector(dc.arima2$fitted)) # ARIMA2 (6 0 3; 5 0 2), fr=1

# ARIMA2 has the lowest RMSE, ARIMA2 is our final model.
# Final forecasted DOYs for 2022-2032 is:
dc.arima.pred2





# General Additive Model
##########################################
dc.gam <- gam(dc.bloom.ts ~ s(dc.x1.t) + s(dc.x2.t) + s(dc.x3.t))
summary(dc.gam)
pacf(resid(dc.gam))

dc.gam2 <- gam(bloom_doy ~ s(sun) + s(sun.maysep) + s(Tavg), data=cherry.dc.sub)
summary(dc.gam2)


AIC(dc.gam2)
acf(resid(dc.gam2))
pacf(resid(dc.gam2))
plot(dc.gam2)
gam.check(dc.gam2)
concurvity(dc.gam2)


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
                           c("sun.mar", "sun.maysep", "WDSP", "SLP", "PRCP", "Tavg", "TempDiffAbs", 
                             "heat.days", "chill.days", "GDD", "PBD")]

tyme2 <- seq(from=1953, to=2021, by=1)
plot(tyme2, cherry.ja.sub$WDSP, type="l")
plot(tyme2, cherry.ja.sub$SLP, type="l")
plot(tyme2, cherry.ja.sub$PRCP, type="l")

bwid <- 3.49*sd(!is.na(cherry.ja.sub$SLP))*(sum(!is.na(cherry.ja.sub$SLP))^(-1/3)) / 10
hist(~SLP, data=cherry.ja.sub, w=.034)

bwid <- 3.49*sd(!is.na(cherry.ja.sub$PRCP))*(sum(!is.na(cherry.ja.sub$PRCP))^(-1/3))
hist(~PRCP, data=cherry.ja.sub, w=20)

# create time series for PRCP and impute missing values
ts.prcp <- ts(cherry.ja$PRCP[52:122])
prcp.m1 <- Arima(ts.prcp, order = c(6, 1, 3), seasonal = list(order=c(3, 1, 2), period=4), 
                 method="ML",)
prcp.m1$arma
checkresiduals(prcp.m1)
summary(prcp.m1)

library(imputeTS)
test1 <- na_kalman(as.vector(prcp.m1$fitted), smooth=TRUE)
tyme3 <- seq(from=1953, to=2021, by=1)
plot(tyme3, test2, type="l")
plot(tyme3, cherry.ja.sub$PRCP, type="l")
mean(cherry.ja.sub$PRCP, na.rm=TRUE)
test2 <- na_kalman(cherry.ja.sub$PRCP, smooth=TRUE, model="auto.arima")

cherry.ja %>% select(year, PRCP)


numsim <- 10000
slp.smp <- NULL
prc.smp <- NULL
set.seed(7)
for (k in 1:numsim){
  slp.smp[k] <- sample(cherry.ja[c(54:65,74:75,77:88,91:105,107:122), "SLP"], size=1)
  prc.smp[k] <- sample(cherry.ja[c(52:90,100,105,107:122), "PRCP"], size=1)
  
}
plot(density(slp.smp))
plot(density(prc.smp))

set.seed(7)
v1 <- sample(slp.smp, size = 14)
v2 <- sample(prc.smp, size = 14)
cherry.ja$SLP[c(52:53,66:73,76,89:90,106)] <- v1
cherry.ja$PRCP[c(91:99,101:104,106)] <- v2


# Stepwise Regression
lm2 <- lm(PBD ~ sun + sun.maysep + WDSP + SLP + PRCP + Tavg + 
            TempDiffAbs + heat.days + chill.days + GDD,
          data=cherry.ja.sub, na.action = na.omit)

step.lm2 <- step(lm2, direction = "backward")


# Best subset selection
cherry.ja.pred <- cherry.ja.sub
cherry.ja.y <- cherry.ja.pred[,"PBD"]
cherry.ja.pred <- within(cherry.ja.pred, rm("PBD"))

bestlm2 <- RegBest(y = cherry.ja.y, x = cherry.ja.pred, 
                  na.action = na.omit, method=c("adjr2", "Cp"), nbest=3)
bestlm$best
summary(regsubsets(y = cherry.ja.y, x = cherry.ja.pred, nbest=1 ))

scatterplotMatrix(cherry.ja.sub)

ja.lm1 <- lm(PBD ~ sun + sun.maysep + PRCP + chill.days + TempDiffAbs + SLP, data=cherry.ja.sub)

summary(ja.lm1)
vif(ja.lm1)
AIC(ja.lm1)
acf2(resid(ja.lm1))
acf2(resid(ja.lm1)^2)
durbinWatsonTest(ja.lm1)
plot(ja.lm1)







## Liestal
colnames(cherry.sw)
cherry.sw.sub <- cherry.sw[year >= 1931, 
                           c("sun", "sun.maysep", "PRCP", "SNWD", "Tavg", "TempDiffAbs", 
                             "heat.days", "chill.days", "GDD", "PBD")] %>% filter(complete.cases(.))
lm3 <- lm(PBD ~ sun + sun.maysep + PRCP + SNWD + Tavg + TempDiffAbs + heat.days + chill.days + GDD,
          data=cherry.sw.sub, na.action = na.omit)
# Stepwise Regression
step.lm3 <- step(lm3, direction = "backward")


# Best subset selection
cherry.sw.pred <- cherry.sw.sub
cherry.sw.y <- cherry.sw.pred[,"PBD"]
cherry.sw.pred <- within(cherry.sw.pred, rm("PBD"))

bestlm3 <- RegBest(y = cherry.sw.y, x = cherry.sw.pred, 
                  na.action = na.omit, method=c("adjr2", "Cp"), nbest=3)
bestlm3$best
summary(regsubsets(y = cherry.sw.y, x = cherry.sw.pred, nbest=1 ))

scatterplotMatrix((cherry.sw.sub))


sw.lm1 <- lm(PBD ~ sun + sun.maysep + TempDiffAbs + heat.days + chill.days + I(sun^2) + I(sun^3), 
             data = cherry.sw.sub)
sw.lm2 <- lm(PBD ~ PRCP + GDD + sun.maysep + sun + I(sun^2) + I(sun^3), 
             data = cherry.sw.sub)

summary(sw.lm1)
vif(sw.lm1)
AIC(sw.lm1, sw.lm2)
plot(sw.lm1)
acf2(resid(sw.lm1))

sw.PBD <- ts(cherry.sw$PBD[32:122])
sw.x1 <- ts(cherry.sw$sun[32:122])
sw.x2 <- ts(cherry.sw$sun.maysep[32:122])
sw.x3 <- ts(cherry.sw$TempDiffAbs[32:122])
sw.x4 <- ts(cherry.sw$Tavg[32:122])
sw.x5 <- ts(cherry.sw$heat.days[32:122])
sw.x6 <- ts(cherry.sw$chill.days[32:122])
sw.x7 <- ts(cherry.sw$GDD[32:122])
time.sw <- time(sw.PBD)


sw.arima <- auto.arima(sw.PBD, max.p=5, max.d=3, max.q=5, max.P=5, max.D=3, max.Q=5, seasonal=TRUE, method="CSS",
                       xreg=cbind(sw.x1, sw.x2, sw.x3, sw.x5, sw.x6, (sw.x1^2), (sw.x1^3)) )
sw.arima2 <- auto.arima(sw.PBD,max.p=5, max.d=3, max.q=5, max.P=5, max.D=3, max.Q=5, seasonal=TRUE, method="CSS",
                        xreg=cbind(sw.x1, sw.x2, sw.x3, sw.x5, (sw.x1^2), (sw.x1^3)) )
sw.arima3 <- auto.arima(sw.PBD,max.p=5, max.d=3, max.q=5, max.P=5, max.D=3, max.Q=5, seasonal=TRUE, method="CSS",
                        xreg=cbind(sw.x1, sw.x2, sw.x3, (sw.x1^2), (sw.x1^3)) )

AIC(sw.arima, sw.arima2, sw.arima3)

summary(sw.arima)
summary(sw.arima2) # smallest RMSE, 
summary(sw.arima3) # by far best AIC

acf2(resid(sw.arima3))
acf2(resid(sw.arima3)^2) #acf for all models disappointing
sw.arima$arma
sw.arima$loglik
sw.arima$aic


sw.forecasts <- as.data.frame(forecast(sw.arima2, xreg=cbind(sw.x1, sw.x2, sw.x3, sw.x5, (sw.x1^2), (sw.x1^3)) ))
pred.yrs <- c(seq(from=2022, to=2032, by=1))
sw.predictions <- data.frame(year = pred.yrs, prediction = sw.forecasts[1:11,])







# Vancouver

cherry.df <- cherry.dc[, c("year", "location", "lat", "long", "alt", "bloom_date", "bloom_doy", "PBD", "Adj.bloom.date", "hardiness.zone", "sun", "sun.maysep", "sun.mar", "WDSP", "SLP", "PRCP", "SNWD", "TMIN", "TMAX", "TAVG", "Tavg", "TempDiffAbs", "TempDiffSq", "ppd", "heat.days", "chill.days", "GDD")] %>% 
  rbind(cherry.ja[, c("year", "location", "lat", "long", "alt", "bloom_date", "bloom_doy", "PBD", "Adj.bloom.date", "hardiness.zone", "sun", "sun.maysep", "sun.mar", "WDSP", "SLP", "PRCP", "SNWD", "TMIN", "TMAX", "TAVG", "Tavg", "TempDiffAbs", "TempDiffSq", "ppd", "heat.days", "chill.days", "GDD")]) %>% 
  rbind(cherry.sw[, c("year", "location", "lat", "long", "alt", "bloom_date", "bloom_doy", "PBD", "Adj.bloom.date", "hardiness.zone", "sun", "sun.maysep", "sun.mar", "WDSP", "SLP", "PRCP", "SNWD", "TMIN", "TMAX", "TAVG", "Tavg", "TempDiffAbs", "TempDiffSq", "ppd", "heat.days", "chill.days", "GDD")]) 

cherry.df.sub <- cherry.df %>% 
  select("location", "year", "sun.maysep", "sun.mar", "Tavg", "TempDiffAbs")


cherry.df.s <- cherry.df[cherry.df$year >= 1956,]

all.lm1 <- lm(PBD ~ lat + sun + sun.maysep + WDSP + SLP + PRCP + SNWD + Tavg + TempDiffSq + ppd + heat.days + chill.days + GDD, data=cherry.df ) 
summary(all.lm1)

# Stepwise Regression
step.lm4 <- step(lm1, direction = "backward")

all.lm <- lm(PBD ~ Tavg + sun.maysep + sun + I(sun^2), data=cherry.df)
summary(all.lm)

acf2(resid(all.lm))
vif(all.lm)
plot(all.lm)

ts.pbd.ja <- ts(cherry.df.s[cherry.df.s$location=="kyoto", "PBD"])
ts.pbd.dc <- ts(cherry.df.s[cherry.df.s$location=="washingtondc", "PBD"])
ts.pbd.sw <- ts(cherry.df.s[cherry.df.s$location=="liestal", "PBD"])

ts.tavg.ja <- ts(cherry.df.s[cherry.df.s$location=="kyoto", "Tavg"])
ts.tavg.dc <- ts(cherry.df.s[cherry.df.s$location=="washingtondc", "Tavg"])
ts.tavg.sw <- ts(cherry.df.s[cherry.df.s$location=="liestal", "Tavg"])

ts.suns.ja <- ts(cherry.df.s[cherry.df.s$location=="kyoto", "sun.maysep"])
ts.suns.dc <- ts(cherry.df.s[cherry.df.s$location=="washingtondc", "sun.maysep"])
ts.suns.sw <- ts(cherry.df.s[cherry.df.s$location=="liestal", "sun.maysep"])

ts.sun.ja <- ts(cherry.df.s[cherry.df.s$location=="kyoto", "sun"])
ts.sun.dc <- ts(cherry.df.s[cherry.df.s$location=="washingtondc", "sun"])
ts.sun.dc <- ts(cherry.df.s[cherry.df.s$location=="liestal", "sun"])


library(vars)


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





