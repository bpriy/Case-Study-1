
# Linear Models
#########################################################################
# Washington, DC
colnames(cherry.dc)

# Stepwise Regression
cherry.dc.sub <- cherry.dc[year >= 1956, 
                           c("sunlight.duration", "sun.maysep", "WDSP", "SLP", "PRCP", "SNWD", "Tavg", "TempDiffAbs", 
                             "heat.days", "chill.days", "GDD", "bloom_doy")] %>% filter(complete.cases(.))
lm1 <- lm(bloom_doy ~ sunlight.duration + sun.maysep + WDSP + SLP + PRCP + SNWD + Tavg + 
            TempDiffAbs + heat.days + chill.days + GDD,
          data=cherry.dc.sub, na.action = na.omit)
step.lm1 <- step(lm1, direction = "backward")

# Best subset selection
cherry.dc.pred <- cherry.dc[year >= 1956, 
                            c("sunlight.duration", "sun.maysep", "WDSP", "SLP", "PRCP", "SNWD", "Tavg", "TempDiffAbs", 
                              "heat.days", "chill.days", "GDD", "bloom_doy")] %>% 
  filter(complete.cases(.))
cherry.dc.y <- cherry.dc.pred[,"bloom_doy"]
cherry.dc.pred <- within(cherry.dc.pred, rm("bloom_doy"))

bestlm <- RegBest(y = cherry.dc.y, x = cherry.dc.pred, 
                  na.action = na.omit, method=c("adjr2", "Cp"), nbest=3)
bestlm$best
summary(regsubsets(y = cherry.dc.y, x = cherry.dc.pred, nbest=1 ))



cherry.dc.sub2 <- cherry.dc[year >= 1956,
                            c("sunlight.duration", "sun.maysep", "Tavg", "GDD", "bloom_doy")] %>% filter(complete.cases(.))

lm.b1 <- lm(bloom_doy ~ sunlight.duration + sun.maysep + Tavg + GDD, 
            data = cherry.dc.sub2)
summary(lm.b1)

lm.b2 <- lm(bloom_doy ~ sunlight.duration + sun.maysep + Tavg, data=cherry.dc.sub2)
AIC(lm.b2, lm.b1)

# Diagnostics
vif(lm.b1)
vif(lm.b2)
scatterplotMatrix(cherry.dc.sub)
cor(cherry.dc.sub2)


acf(resid(lm.b2))
pacf(resid(lm.b2))
durbinWatsonTest(lm.b2)
qqPlot(lm.b2)
plot(lm.b2)


t.lm1 <- lm(bloom_doy ~ sunlight.duration + sun.maysep + Tavg + 
                I(sunlight.duration^2) + I(sunlight.duration^3) + I(sun.maysep^2) 
              , data=cherry.dc.sub2)
t.lm2 <- lm(bloom_doy ~ sunlight.duration + sun.maysep + Tavg + 
              I(sunlight.duration^2) + I(sun.maysep^2) 
            , data=cherry.dc.sub2)
t.lm3 <- lm(bloom_doy ~ sunlight.duration  + Tavg + sun.maysep + GDD + I(sun.maysep^2) + I(sunlight.duration^2) + I(sunlight.duration^3), data=cherry.dc.sub2)

AIC(t.lm1, t.lm2, t.lm3)
anova(t.lm1, t.lm2)

summary(t.lm3)
plot(t.lm1)

vif(t.lm3)
acf(resid(t.lm1))
pacf(resid(t.lm1))
durbinWatsonTest(t.lm1)

# set store variables as time series for model
dc.bloom.ts <- ts(cherry.dc$bloom_doy[36:101])
dc.x1 <- ts(cherry.dc$Tavg[36:101])
dc.x2 <- ts(cherry.dc$sunlight.duration[36:101])
dc.x3 <- ts(cherry.dc$sun.maysep[36:101])
time.dc <- time(dc.bloom.ts)

# plot time vs. covariates
scatterplot(time.dc, dc.x1)
scatterplot(time.dc, dc.x2)
scatterplot(time.dc, dc.x3)
scatterplot(time.dc, dc.x2^2)
scatterplot(time.dc, dc.x2^3)
scatterplot(time.dc, dc.x3^2)

# multiple linear regression 
tm.dc.lm1 <- lm(dc.bloom.ts ~ time.dc + dc.x1 + dc.x2 + I(dc.x2^2) + I(dc.x2^3) + dc.x3 + I(dc.x3^2))
summary(tm.dc.lm1)

# detrend data
dc.x1.t <- resid(lm(dc.x1 ~ time.dc))
dc.x2.t <- resid(lm(dc.x2 ~ time.dc))
dc.x3.t <- resid(lm(dc.x3 ~ time.dc))

# fit model with detrended data
tm.dc.lm2 <- lm(dc.bloom.ts ~ dc.x1.t + dc.x2.t + dc.x3.t + I(dc.x2.t^2) + I(dc.x2.t^3) + I(dc.x3.t^2) + time.dc )
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
           xreg=cbind(time.dc, dc.x1.t, dc.x2.t, dc.x3.t, (dc.x2.t^2), (dc.x2.t^3), (dc.x3.t^2)) )
predict(dc.arima2, n.ahead = 10)

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


cherry.df <- cherry.dc[, c("year", "location", "lat", "alt", "bloom_doy", "bloom_date", "hardiness.zone", "sunlight.duration", "Tavg", "GDD", "heat.days", "chill.days")] %>% 
  rbind(cherry.ja[, c("year", "location", "lat", "alt", "bloom_doy", "bloom_date", "hardiness.zone", "sunlight.duration", "Tavg", "GDD", "heat.days", "chill.days")]) %>% 
  rbind(cherry.sw[, c("year", "location", "lat", "alt", "bloom_doy", "bloom_date", "hardiness.zone", "sunlight.duration", "Tavg", "GDD", "heat.days", "chill.days")]) 

tavg_temps <- lm(Tavg ~ year + location + sunlight.duration,
                 data = cherry.df)
summary(tavg_temps)


yr <- as.data.frame(c(seq(from=1956, to=2032, by=1)) )
colnames(yr) <- c("year")
new1 <- cherry.df[cherry.df$location=="washingtondc", c("year", "Tavg", "sunlight.duration")]
new2 <- merge(yr, new1, by="year", all=TRUE)

forecast(tm.dc.lm3, newdata = new2)
tm.dc.lm3$fitted.values


# ARIMA Model
dc.arima <- auto.arima(dc.bloom.ts, max.p=5, max.d=3, max.q=5, max.P=5, max.D=3, max.Q=5, seasonal=TRUE, method="CSS",
           xreg=cbind(time.dc, dc.x1.t, dc.x2.t, dc.x3.t, (dc.x2.t^2)) )
summary(dc.arima)
dc.arima$arma
dc.arima$model

acf2(resid(dc.arima))
dc.arima$loglik
logLik(tm.dc.lm3)


dc.test <- auto.arima(dc.bloom.ts[1:56], max.p=5, max.d=3, max.q=5, max.P=5, max.D=3, max.Q=5, seasonal=TRUE, method="CSS",
                       xreg=cbind(time.dc[1:56], dc.x1.t[1:56], dc.x2.t[1:56], dc.x3.t[1:56], (dc.x2.t^2)[1:56]) )
summary(dc.test)
pred.dc <- forecast(dc.test, xreg=cbind(time.dc[1:56], dc.x1.t[1:56], dc.x2.t[1:56], dc.x3.t[1:56], (dc.x2.t^2)[1:56]))
pred.dc <- as.data.frame(pred.dc)
rmse(cherry$bloom_doy[91:101], pred.dc[1:11,1])
pred.yrs <- c(seq(from=2022, to=2032, by=1))
dc.predictions <- data.frame(pred.yrs, pred.dc[1:11,])




# General Additive Model
dc.gam <- gam(dc.bloom.ts ~ s(dc.x1.t) + s(dc.x2.t) + s(dc.x3.t))
summary(dc.gam)
pacf(resid(dc.gam))

dc.gam2 <- gam(bloom_doy ~ s(sunlight.duration) + s(sun.maysep) + s(Tavg), data=cherry.dc.sub2)
summary(dc.gam2)


AIC(dc.gam2)
acf(resid(dc.gam2))
pacf(resid(dc.gam2))
plot(dc.gam2)
gam.check(dc.gam2)
concurvity(dc.gam2)


dc.gam4 <- gamm(bloom_doy ~ s(sunlight.duration) + s(sun.maysep) + s(Tavg), 
                data=cherry.dc.sub2, family=gaussian, 
                correlation = corAR1(form= ~1))
acf2(resid(dc.gam4))


r1 <- start_value_rho(dc.gam2, plot=TRUE)
nrow(cherry.dc.sub2)
cherry.dc.sub2$ar <- c(TRUE, rep(FALSE, 62))

dc.gam3 <- bam(bloom_doy ~ s(sunlight.duration) + s(sun.maysep) + s(Tavg), 
               data=cherry.dc.sub2, rho=r1, AR.start=cherry.dc.sub2$ar)
acf2(resid(dc.gam3))



auto.arima(dc.bloom.ts, max.p=5, max.d=3, max.q=5, max.P=5, max.D=3, max.Q=5, seasonal=TRUE, method="CSS",
           )

ggplot(cherry.dc.sub2, aes(sunlight.duration, bloom_doy)) +
         geom_point()+
         stat_smooth(method = gam, formula = y ~ s(x))
ggplot(cherry.dc.sub2, aes(sun.maysep, bloom_doy)) +
  geom_point()+
  stat_smooth(method = gam, formula = y ~ s(x))
ggplot(cherry.dc.sub2, aes(Tavg, bloom_doy)) +
  geom_point()+
  stat_smooth(method = gam, formula = y ~ s(x))


cherry.sw.sub2 <- cherry.sw[60:122, c("sunlight.duration", "sun.maysep", "Tavg", "PBD")]
sw.pre <- within(cherry.sw.sub2, rm("PBD"))
predict(dc.gam2, newdata = sw.pre)

predict(dc.gam2)


set.seed(7)
pcr.dc <- pcr(bloom_doy ~ Tavg + sun.maysep + GDD + I(sun.maysep^2) + sunlight.duration + 
                I(sunlight.duration^2) + I(sunlight.duration^3), 
    data=cherry.dc.sub2, center = TRUE, scale = TRUE, validation = "LOO", jackknife = TRUE)
summary(pcr.dc)
pcr.dc$loadings
pcr.dc$coefficients
pcr.dc$Yloadings
validationplot(pcr.dc)
set.seed(7)
jack.test(pcr.dc)

set.seed(7)
pcr.dc2 <- pcr(bloom_doy ~ Tavg + sun.maysep + I(sun.maysep^2) + sunlight.duration + 
                 I(sunlight.duration^2) + I(sunlight.duration^3), 
              data=cherry.dc.sub2, center = TRUE, scale = TRUE, validation = "LOO", jackknife = TRUE)
summary(pcr.dc2)
pcr.dc2$loadings
pcr.dc2$Yloadings
validationplot(pcr.dc2)
set.seed(7)
pcr.dc.b <- jack.test(pcr.dc2)


library(clusterSim)
cherry.sw.sub2 <- cherry.sw[60:122, c("sunlight.duration", "sun.maysep", "Tavg", "PBD")]
bloom.norm2 <- data.Normalization(cherry.sw.sub2[,"PBD"], type="n1", normalization="column")
sw.pre <- within(cherry.sw.sub2, rm("PBD"))
colnames(sw.pre)
colnames(cherry.dc.sub2)
sw.pre$sun.maysepSq <- (sw.pre$sun.maysep)^2
sw.pre$sunlightSq <- (sw.pre$sunlight.duration)^2
sw.pre$sunlightCu <- (sw.pre$sunlight.duration)^3

predict(pcr.dc2, newdata = sw.pre )


bloom.norm <- data.Normalization(cherry.dc.sub2[,"bloom_doy"], type="n1", normalization="column")
cherry.pca <- within(cherry.dc.sub2, rm("bloom_doy"))
cherry.pca$sun.maysepSq <- (cherry.pca$sun.maysep)^2
cherry.pca$sunlightSq <- (cherry.pca$sunlight.duration)^2
cherry.pca$sunlightCu <- (cherry.pca$sunlight.duration)^3

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
                           c("sunlight.duration", "sun.maysep", "WDSP", "SLP", "PRCP", "Tavg", "TempDiffAbs", 
                             "heat.days", "chill.days", "GDD", "PBD")] %>% filter(complete.cases(.))
lm2 <- lm(PBD ~ sunlight.duration + sun.maysep + WDSP + SLP + PRCP + Tavg + 
            TempDiffAbs + heat.days + chill.days + GDD,
          data=cherry.ja.sub, na.action = na.omit)
# Stepwise Regression
step.lm2 <- step(lm2, direction = "both")


# Best subset selection
cherry.ja.pred <- cherry.ja.sub
cherry.ja.y <- cherry.ja.pred[,"PBD"]
cherry.ja.pred <- within(cherry.ja.pred, rm("PBD"))

bestlm2 <- RegBest(y = cherry.ja.y, x = cherry.ja.pred, 
                  na.action = na.omit, method=c("adjr2", "Cp"), nbest=3)
bestlm$best
summary(regsubsets(y = cherry.ja.y, x = cherry.ja.pred, nbest=1 ))

scatterplotMatrix(cherry.ja.sub)

ja.lm1 <- lm(PBD ~ sunlight.duration + sun.maysep + PRCP + heat.days + chill.days + TempDiffAbs, data=cherry.ja.sub)

summary(ja.lm1)
vif(ja.lm1)
AIC(ja.lm1)
acf2(resid(ja.lm1))
plot(ja.lm1)



# ARIMA Model
ja.arima <- auto.arima




# ARIMA Model
dc.arima <- auto.arima(dc.bloom.ts, max.p=5, max.d=3, max.q=5, max.P=5, max.D=3, max.Q=5, seasonal=TRUE, method="CSS",
                       xreg=cbind(time.dc, dc.x1.t, dc.x2.t, dc.x3.t, (dc.x2.t^2)) )
summary(dc.arima)
dc.arima$arma
dc.arima$model

acf2(resid(dc.arima))
dc.arima$loglik
logLik(tm.dc.lm3)

rmse(cherry.dc$bloom_doy[36:101], )

dc.test <- auto.arima(dc.bloom.ts[1:56], max.p=5, max.d=3, max.q=5, max.P=5, max.D=3, max.Q=5, seasonal=TRUE, method="CSS",
                      xreg=cbind(time.dc[1:56], dc.x1.t[1:56], dc.x2.t[1:56], dc.x3.t[1:56], (dc.x2.t^2)[1:56]) )
summary(dc.test)
pred.dc <- forecast(dc.test, xreg=cbind(time.dc[1:56], dc.x1.t[1:56], dc.x2.t[1:56], dc.x3.t[1:56], (dc.x2.t^2)[1:56]))
pred.dc <- as.data.frame(pred.dc)
rmse(cherry$bloom_doy[91:101], pred.dc[1:11,1])






## Liestal
colnames(cherry.sw)
cherry.sw.sub <- cherry.sw[year >= 1931, 
                           c("sunlight.duration", "sun.maysep", "PRCP", "SNWD", "Tavg", "TempDiffAbs", 
                             "heat.days", "chill.days", "GDD", "PBD")] %>% filter(complete.cases(.))
lm3 <- lm(PBD ~ sunlight.duration + sun.maysep + PRCP + SNWD + Tavg + TempDiffAbs + heat.days + chill.days + GDD,
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



sw.lm1 <- lm(PBD ~ sunlight.duration)







view(cherry.dc)

