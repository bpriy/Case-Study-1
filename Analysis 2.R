
# Linear Models
#########################################################################
# Washington, DC
colnames(cherry.dc)

cherry.dc.sub <- cherry.dc[year >= 1956, 
                           c("sun", "sun.maysep", "WDSP", "SLP", "PRCP", "SNWD", "Tavg", "TempDiffAbs", 
                             "heat.days", "chill.days", "GDD", "bloom_doy")] %>% filter(complete.cases(.))
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
cherry.dc.sub$SLP[46:52] <- v1
cherry.dc$SLP[46:52] <- v1


# Stepwise Regression
lm1 <- lm(bloom_doy ~ sun + sun.maysep + WDSP + SLP + PRCP + SNWD + Tavg + 
            TempDiffAbs + heat.days + chill.days + GDD,
          data=cherry.dc.sub, na.action = na.omit)
step.lm1 <- step(lm1, direction = "backward")

# Best subset selection
cherry.dc.pred <- cherry.dc.sub[year >= 1956, 
                            c("sun", "sun.maysep", "WDSP", "SLP", "PRCP", "SNWD", "Tavg", "TempDiffAbs", 
                              "heat.days", "chill.days", "GDD", "bloom_doy")] %>% 
  filter(complete.cases(.))
cherry.dc.y <- cherry.dc.pred[,"bloom_doy"]
cherry.dc.pred <- within(cherry.dc.pred, rm("bloom_doy"))

bestlm <- RegBest(y = cherry.dc.y, x = cherry.dc.pred, 
                  na.action = na.omit, method=c("adjr2", "Cp"), nbest=3)
bestlm$best
summary(regsubsets(y = cherry.dc.y, x = cherry.dc.pred, nbest=1 ))


# models
lm.b1 <- lm(bloom_doy ~ sun + sun.maysep + TempDiffAbs, 
            data = cherry.dc.sub)
summary(lm.b1)

lm.b2 <- lm(bloom_doy ~ sun + sun.maysep + SLP + TempDiffAbs, data=cherry.dc.sub)
summary(lm.b2)
AIC(lm.b2, lm.b1)

# Diagnostics
vif(lm.b1)
vif(lm.b2)
scatterplotMatrix(cherry.dc.sub)
cor(cherry.dc.sub2)

attach(cherry.dc)
par(mfrow=c(1,1))
tyme <- seq(from=1900, to=2021, by=1)
plot(seq(1, length(PBD), by=1), PBD, type="l")
plot(seq(1, length(PBD), by=1), sun, type="l")
plot(seq(1, length(PBD), by=1), Tavg, type="l")
plot(seq(1, length(PBD), by=1), GDD, type="l")
detach(cherry.dc)

acf2(resid(lm.b2))
pacf(resid(lm.b2))
durbinWatsonTest(lm.b2)
qqPlot(lm.b2)
plot(lm.b2)


t.lm1 <- lm(bloom_doy ~ sun + sun.maysep + SLP + TempDiffAbs + GDD +
                I(sun^2) + I(sun^3) + I(sun.maysep^2) 
              , data=cherry.dc.sub)
t.lm2 <- lm(bloom_doy ~ sun + sun.maysep + SLP + heat.days + 
               I(sun.maysep^2) + I(sun^2) + I(sun^3)
            , data=cherry.dc.sub)
t.lm3 <- lm(bloom_doy ~ Tavg + SLP + sun + sun.maysep + I(sun.maysep^2) + I(sun^2) + I(sun^3), data=cherry.dc.sub)

AIC(t.lm1, t.lm2, t.lm3)
anova(t.lm3, t.lm1)

summary(t.lm3)
plot(t.lm3)

vif(t.lm3)
acf2(resid(t.lm3))
pacf(resid(t.lm3))
durbinWatsonTest(t.lm3)

# set store variables as time series for model
dc.bloom.ts <- ts(cherry.dc$bloom_doy[36:101])
dc.x1 <- ts(cherry.dc$Tavg[36:101])
dc.x2 <- ts(cherry.dc$sun[36:101])
dc.x3 <- ts(cherry.dc$sun.maysep[36:101])
dc.x4 <- ts(cherry.dc$SLP[36:101])
time.dc <- time(dc.bloom.ts)

# plot time vs. covariates
scatterplot(time.dc, dc.x1)
scatterplot(time.dc, dc.x2)
scatterplot(time.dc, dc.x3)
scatterplot(time.dc, dc.x4)
scatterplot(time.dc, dc.x2^2)
scatterplot(time.dc, dc.x2^3)
scatterplot(time.dc, dc.x3^2)

# multiple linear regression 
tm.dc.lm1 <- lm(dc.bloom.ts ~ time.dc + dc.x1 + dc.x2 + I(dc.x2^2) + I(dc.x2^3) + dc.x3 + I(dc.x3^2) + dc.x4)
summary(tm.dc.lm1)

# detrend data
dc.x1.t <- resid(lm(dc.x1 ~ time.dc))
dc.x2.t <- resid(lm(dc.x2 ~ time.dc))
dc.x3.t <- resid(lm(dc.x3 ~ time.dc))
dc.x4.t <- resid(lm(dc.x4 ~ time.dc))
dc.x2.t2 <- resid(lm(I(dc.x2^2) ~ time.dc))
dc.x2.t3 <- resid(lm(I(dc.x2^3) ~ time.dc))
dc.x3.t2 <- resid(lm(I(dc.x3^2) ~ time.dc))


# fit model with detrended data
tm.dc.lm2 <- lm(dc.bloom.ts ~ dc.x1.t + dc.x2.t + dc.x3.t + dc.x4.t + dc.x2.t2 + dc.x2.t3 + dc.x3.t2 + time.dc )
summary(tm.dc.lm2)
acf2(resid(tm.dc.lm2))
acf2(resid(tm.dc.lm2)^2)

# Diagnostics
vif(tm.dc.lm2)
plot(tm.dc.lm2)

ar.dc <- arima(dc.bloom.ts, order = c(1, 0, 0), method="CSS", xreg = cbind(time.dc, dc.x1.t, dc.x2.t, dc.x3.t, 
                                                             (dc.x2.t^2), (dc.x2.t^3), (dc.x3.t^2)) )
summary(ar.dc)
coef(tm.dc.lm2)

logLik(tm.dc.lm2)
ar.dc$loglik

AIC(tm.dc.lm2)

dc.arima2 <- auto.arima(dc.bloom.ts, max.p=5, max.d=3, max.q=5, max.P=5, max.D=3, max.Q=5, seasonal=TRUE, method="CSS",
           xreg=cbind(time.dc, dc.x1.t, dc.x2.t, dc.x3.t, dc.x4.t, dc.x2.t2, dc.x3.t2) )
forecast(dc.arima2, xreg=cbind(time.dc, dc.x1.t, dc.x2.t, dc.x3.t, dc.x4.t, dc.x2.t2, dc.x3.t2) )

acf2(resid(dc.arima2))
scatterplot(cherry.dc$PBD, resid(dc.arima2))

tm.dc.lm3 <- lm(dc.bloom.ts ~ dc.x1.t + dc.x2.t  + I(dc.x2.t^2) + time.dc )
plot(tm.dc.lm3)

tm.dc.lm4 <- lm(dc.bloom.ts ~ dc.x1.t + dc.x2.t  + I(dc.x2.t^2) + I(dc.x2.t^3) +  time.dc )
anova(tm.dc.lm3, tm.dc.lm4)

dc.slp <- ts(cherry.dc$SLP[36:101])
dc.slp <- resid(lm(dc.slp ~ time.dc))
dc.gdd <- ts(cherry.dc$GDD[36:101])
dc.gdd <- resid(lm(dc.gdd ~ time.dc))

tm.dc.lm5 <- lm(dc.bloom.ts ~ dc.x1.t + dc.x2.t + I(dc.x2.t^2) + dc.slp + time.dc )
AIC(tm.dc.lm3, tm.dc.lm4, tm.dc.lm5)



summary(tm.dc.lm3)
vif(tm.dc.lm3)
AIC(tm.dc.lm3)
acf2(resid(tm.dc.lm3))

yr <- as.data.frame(year = seq(from=1956, to=2032, by=1))
dc.pred2 <- as.data.frame(yr) %>% 
  bind_cols(predicted_dob = predict(tm.dc.lm3, newdata =.))



###################################################################################
cherry.df <- cherry.dc[, c("year", "location", "lat", "long", "alt", "bloom_date", "bloom_doy", "PBD", "Adj.bloom.date", "hardiness.zone", "sun", "sun.maysep", "WDSP", "SLP", "PRCP", "SNWD", "TMIN", "TMAX", "TAVG", "Tavg", "TempDiffAbs", "TempDiffSq", "ppd", "heat.days", "chill.days", "GDD")] %>% 
  rbind(cherry.ja[, c("year", "location", "lat", "long", "alt", "bloom_date", "bloom_doy", "PBD", "Adj.bloom.date", "hardiness.zone", "sun", "sun.maysep", "WDSP", "SLP", "PRCP", "SNWD", "TMIN", "TMAX", "TAVG", "Tavg", "TempDiffAbs", "TempDiffSq", "ppd", "heat.days", "chill.days", "GDD")]) %>% 
  rbind(cherry.sw[, c("year", "location", "lat", "long", "alt", "bloom_date", "bloom_doy", "PBD", "Adj.bloom.date", "hardiness.zone", "sun", "sun.maysep", "WDSP", "SLP", "PRCP", "SNWD", "TMIN", "TMAX", "TAVG", "Tavg", "TempDiffAbs", "TempDiffSq", "ppd", "heat.days", "chill.days", "GDD")]) 

tavg_temps <- lm(Tavg ~ year + location + sun,
                 data = cherry.df)
summary(tavg_temps)


fut.yr <- c(rep(seq(from=2022, to=2032, by=1), 3) )
fut.suns <- c(ja.sun.fut, dc.sun.fut, sw.sun.fut)
fut.loc <- c(rep("kyoto", 11), rep("washingtondc", 11), rep("liestal", 11))
fut.data <- data.frame(location = fut.loc, year = fut.yr, sun = fut.suns)

pred.temps <- predict(tavg_temps, newdata=fut.data)
fut.data$Tavg <- pred.temps
###################################################################################


dc.x1.fut <- ts(fut.data[fut.data$location=="washingtondc", 4])
dc.x2.fut <- ts(dc.sun.fut)
time.dc.f <- time(dc.x1.fut)

# plot time vs. covariates
scatterplot(time.dc.f, dc.x1.fut)
scatterplot(time.dc.f, dc.x2.fut)
scatterplot(time.dc.f, dc.x2.fut^2)


# multiple linear regression 
#tm.dc.lm1 <- lm(dc.bloom.ts ~ time.dc + dc.x1 + dc.x2 + I(dc.x2^2) + I(dc.x2^3) + dc.x3 + I(dc.x3^2))
#summary(tm.dc.lm1)

# detrend data
dc.x1.futt <- resid(lm(dc.x1.fut ~ time.dc))
dc.x2.futt <- resid(lm(dc.x2.fut ~ time.dc))
dc.x3.futt <- resid(lm(dc.x3.fut ~ time.dc))
dc.x4.futt <- resid(lm(dc.x4.fut ~ time.dc))
dc.x2.t2futt <- resid(lm(I(dc.x2.fut^2) ~ time.dc))


tm.dc.lm3 <- lm(dc.bloom.ts ~ dc.x1.t + dc.x2.t  + I(dc.x2.t^2) + time.dc )
plot(tm.dc.lm3)

sqre <- dc.x2.futt^2
dc.fut <- data.frame(dc.x1.t = dc.x1.futt, dc.x2.t = dc.x2.futt, time.dc = c(time.dc.f))
dc.dat <- data.frame(dc.x1.t = dc.x1.t, dc.x2.t = dc.x2.t, time.dc = c(time.dc) )
dc.futr <- rbind(dc.dat, dc.fut)

predict(tm.dc.lm3, newdata = dc.fut)
tm.dc.lm3$fitted.values


# ARIMA Model
dc.arima <- auto.arima(dc.bloom.ts, max.p=5, max.d=3, max.q=5, max.P=5, max.D=3, max.Q=5, seasonal=TRUE, method="CSS",
           xreg=cbind(time.dc, dc.x1.t, dc.x2.t, dc.x3.t, (dc.x2.t^2)) )
summary(dc.arima)
dc.arima$arma
dc.arima$model

acf2(resid(dc.arima))
acf2(resid(dc.arima)^2)
dc.arima$loglik
logLik(tm.dc.lm3)
dc.arima$aic


dc.test <- auto.arima(dc.bloom.ts[1:56], max.p=5, max.d=3, max.q=5, max.P=5, max.D=3, max.Q=5, seasonal=TRUE, method="CSS",
                       xreg=cbind(time.dc[1:56], dc.x1.t[1:56], dc.x2.t[1:56], dc.x3.t[1:56], (dc.x2.t^2)[1:56]) )
summary(dc.test)
pred.dc <- forecast(dc.test, xreg=cbind(time.dc[1:56], dc.x1.t[1:56], dc.x2.t[1:56], dc.x3.t[1:56], (dc.x2.t^2)[1:56]))
pred.dc <- as.data.frame(pred.dc)
test.yrs <- c(seq(from=2011, to=2021, by=1))
rmse(cherry$bloom_doy[91:101], pred.dc[1:11,1])
pred.dc.test <- data.frame(test.yrs, pred.dc[1:11,])


dc.forecasts <- as.data.frame(forecast(dc.arima, xreg=cbind(time.dc, dc.x1.t, dc.x2.t, dc.x3.t, (dc.x2.t^2)) ))
pred.yrs <- c(seq(from=2022, to=2032, by=1))
dc.predictions <- data.frame(year = pred.yrs, prediction = dc.forecasts[1:11,])



# General Additive Model
dc.gam <- gam(dc.bloom.ts ~ s(dc.x1.t) + s(dc.x2.t) + s(dc.x3.t))
summary(dc.gam)
pacf(resid(dc.gam))

dc.gam2 <- gam(bloom_doy ~ s(sun) + s(sun.maysep) + s(Tavg), data=cherry.dc.sub2)
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



auto.arima(dc.bloom.ts, max.p=5, max.d=3, max.q=5, max.P=5, max.D=3, max.Q=5, seasonal=TRUE, method="CSS",
           )

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



## Kyoto
cherry.ja.sub <- cherry.ja[year >= 1951, 
                           c("sun", "sun.maysep", "WDSP", "SLP", "PRCP", "Tavg", "TempDiffAbs", 
                             "heat.days", "chill.days", "GDD", "PBD")] %>% filter(complete.cases(.))

plot(cherry.ja$SLP, type="l")
plot(cherry.ja$WDSP, type="l")
plot(cherry.ja$PRCP, type="l")

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


sw.lm1 <- lm(PBD ~ PRCP + heat.days + chill.days + GDD + sun.maysep + sun, 
             data = cherry.sw.sub)
sw.lm2 <- lm(PBD ~ PRCP + GDD + sun.maysep + sun + I(sun^2) + I(sun^3), 
             data = cherry.sw.sub)

summary(sw.lm2)
vif(sw.lm2)
AIC(sw.lm1, sw.lm2)
plot(sw.lm2)
acf2(resid(sw.lm2))

sw.PBD <- ts(cherry.sw$PBD[32:122])
sw.x1 <- ts(cherry.sw$PRCP[32:122])
sw.x2 <- ts(cherry.sw$GDD[32:122])
sw.x3 <- ts(cherry.sw$sun.maysep[32:122])
sw.x4 <- ts(cherry.sw$sun[32:122])
time.sw <- time(sw.PBD)


sw.arima <- auto.arima(sw.PBD,max.p=5, max.d=3, max.q=5, max.P=5, max.D=3, max.Q=5, seasonal=TRUE, method="CSS",
                       xreg=cbind(sw.x1, sw.x2, sw.x3, sw.x4, (sw.x4^2), (sw.x4^3)) )

summary(sw.arima)
acf2(resid(sw.arima))
acf2(resid(sw.arima)^2)
sw.arima$arma
sw.arima$loglik
sw.arima$aic


sw.forecasts <- as.data.frame(forecast(sw.arima, xreg=cbind(sw.x1, sw.x2, sw.x3, sw.x4, (sw.x4^2), (sw.x4^3)) ))
pred.yrs <- c(seq(from=2022, to=2032, by=1))
sw.predictions <- data.frame(year = pred.yrs, prediction = sw.forecasts[1:11,])







# Vancouver

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

acf2()

all.var <- ar(cbind(ts.pbd.ja, ts.pbd.dc, ts.pbd.sw))
all.var$ar
acf(all.var$resid[-1,])

#VARselect(, lag.max = 1,
          #type = “const”, exogen = cbind())

all.var2 <- VAR(cbind(ts.pbd.ja, ts.pbd.dc, ts.pbd.sw), p=1 )
coef(all.var2)
predict(all.var2, n.ahead=10)





