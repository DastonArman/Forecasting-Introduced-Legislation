#Import the data however you want

#Set up time series object
T = length(congress$congress) #Data length
s = 22 #Data frequency
years = T/s #Cycles
styear = 93  #Starting cycle
st = c(styear, 1) #Starting position

data = ts(congress$introduced, start = st, frequency = s)

#Summary statistics
summary(data)
sd(data)

#Simple plot
plot(data, xlab = 'Congress', ylab = 'Introduced Legislation', 
     main = 'Introduced Legislation Over Time by Congress (93.1 - 115.22)', xaxt = 'n')
axis(1, at = c(93:115))

#Monthly averages
month_aggregate = aggregate(congress, list(congress$month), mean)
plot(month_aggregate$introduced, type = 'l', xlab = 'Congressional Month', ylab = 'Average Introduced Legislation', 
     main = 'Average Introduced Legislation by Congressional Month', xaxt = 'n')
axis(1, at = c(1:22))

#Log dataset
lny = log(data)
plot(lny)

#Decomposition
dataTSE = decompose(lny, type = "additive")
plot(dataTSE)
title('Additive Decomposition of Log Introduced Legislation')

#Seasonality model
library(forecast)
M <- seasonaldummy(lny)

library(stargazer)
seasonal_model <- lm(lny ~ M)
summary(seasonal_model)

seasonal = ts(fitted(seasonal_model), start = st, frequency = s)
residuals = lny - seasonal

par(mar = c(5,5,5,5))
plot(lny, ylim = c(-6, 10), main = "Seasonality Model Predictions and Residuals", ylab = 'Log Introduced Legislation',
     xlab = 'Congress', xaxt = 'n')
lines(trend_seasonal, col = "blue")
axis(1, at = c(93:115))
par(new = T)
plot(residuals, ylim = c(-3, 8), ylab = '', axes = F, xlab = '')
axis(4, pretty(c(-3, 3)))
abline(h = 0, col='grey')
mtext("Residuals", side = 4, line = 2.5, at = 0)

#ACT/PACF from seasonal model in case of unit roots
par(mfrow = c(2, 1))
acf(residuals, main = 'Seasonality Model Residuals ACF')
pacf(residuals, main = 'Seasonality Model Residuals PACF')
par(mfrow = c(1, 1))

#Dickey-Fuller test for unit root
library(urca)
residuals_ts = ts(residuals, start = st, frequency = s)
summary(ur.df(residuals_ts, type = c("trend"), lags = 10, selectlags = "AIC"))

#Trend and seasonality model
t = (1:T)
t2 = t^2
M <- seasonaldummy(lny)

trend_seasonal_model <- lm(lny ~ t + t2 + M)
summary(trend_seasonal_model)

trend_seasonal = ts(fitted(trend_seasonal_model), start = st, frequency = s)
residuals = lny - trend_seasonal

par(mar = c(5,5,5,5))
plot(lny, ylim = c(-6, 10), main = "Trend and Seasonality Model Predictions and Residuals", ylab = 'Log Introduced Legislation',
     xlab = 'Congress', xaxt = 'n')
lines(trend_seasonal, col = "blue")
axis(1, at = c(93:115))
par(new = T)
plot(residuals, ylim = c(-3, 8), ylab = '', axes = F, xlab = '')
axis(4, pretty(c(-3, 3)))
abline(h = 0, col='grey')
mtext("Residuals", side = 4, line = 2.5, at = 0)

#Serial correlation in residuals?
library(lmtest)
dwtest(trend_seasonal_model)

#ACT/PACF from trend-seasonal model
par(mfrow = c(2, 1))
acf(residuals, main = 'Trend and Seasonality Model Residuals ACF')
pacf(residuals, main = 'Trend and Seasonality Model Residuals PACF')
par(mfrow = c(1, 1))

#Fit an ARMA(p, q) model 
S2 = rep(c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), T/s)
S3 = rep(c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), T/s)
S4 = rep(c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), T/s)
S5 = rep(c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), T/s)
S6 = rep(c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), T/s)
S7 = rep(c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), T/s)
S8 = rep(c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), T/s)
S9 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), T/s)
S10 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), T/s)
S11 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), T/s)
S12 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), T/s)
S13 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0), T/s)
S14 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0), T/s)
S15 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0), T/s)
S16 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0), T/s)
S17 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0), T/s)
S18 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0), T/s)
S19 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0), T/s)
S20 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0), T/s)
S21 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0), T/s)
S22 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), T/s)

TrSeas = model.matrix(~ t + t2 + S2 + S3 + S4 + S5 + S6 + S7 + S8 + S9 + S10 + S11 + S12 + 
                        S13 + S14 + S15 + S16 + S17 + S18 + S19 + S20 + S21 + S22)

best.order <- c(0, 0, 0)
best.aic <- Inf
for (p in 0:4) for (q in 0:4) {
  fit.arima = arima(lny, order = c(p, 0, q), include.mean = FALSE, xreg = TrSeas, optim.control = list(maxit = 1000))
  fit.aic <- AIC(fit.arima)
  if (fit.aic < best.aic) {
    best.order <- c(p, 0, q)
    best.arima <- fit.arima
    best.aic <- fit.aic
  }
}

best.order
best.aic
summary(best.arima)
exp(sqrt(0.27))

#Durbin-Watson statistic
dw = sum((best.arima$residuals - lag(best.arima$residuals))^2, na.rm = TRUE)/sum(best.arima$residuals^2, na.rm = TRUE)
dw

#Plot the model results
lntrend = ts(lny - best.arima$residuals, start = st, frequency = s)
residuals = lny - lntrend

par(mar = c(5,5,5,5))
plot(lny, ylim = c(-6, 10), main = "Final Model Predictions and Residuals", ylab = 'Log Introduced Legislation',
     xlab = 'Congress', xaxt = 'n')
lines(lntrend, col = "blue")
axis(1, at = c(93:115))
par(new = T)
plot(residuals, ylim = c(-3, 8), ylab = '', axes = F, xlab = '')
axis(4, pretty(c(-3, 3)))
abline(h = 0, col='grey')
mtext("Residuals", side = 4, line = 2.5, at = 0)

#ACF/PACF
par(mfrow = c(2, 1))
acf(residuals, main = 'Full Model Residuals ACF')
pacf(residuals, main = 'Full Model Residuals PACF')
par(mfrow = c(1, 1))

#We construct a histogram and hope that it looks like a bell-shaped normal 
#distribution:
hist(residuals, breaks = 80, xlim = c(-3, 3), col = 'light blue', xlab = 'Residuals', main = 'Histogram of Residuals')
ks.test(x = residuals, y = 'pnorm')

low_error = sort(residuals)[length(residuals)*.025]
high_error = sort(residuals)[length(residuals)*.975]

#I take 44 data points out of sample, which I will forecast later.
h = 22
lny_short = lny[1:(T-h)]
Ts = length(lny_short) #set up the length of your data

lny_short = ts(lny_short, frequency = s, start = st)

ts = (1:Ts)
ts2 = ts^2
S2 = rep(c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Ts/s)
S3 = rep(c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Ts/s)
S4 = rep(c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Ts/s)
S5 = rep(c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Ts/s)
S6 = rep(c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Ts/s)
S7 = rep(c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Ts/s)
S8 = rep(c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Ts/s)
S9 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Ts/s)
S10 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Ts/s)
S11 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Ts/s)
S12 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Ts/s)
S13 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0), Ts/s)
S14 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0), Ts/s)
S15 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0), Ts/s)
S16 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0), Ts/s)
S17 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0), Ts/s)
S18 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0), Ts/s)
S19 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0), Ts/s)
S20 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0), Ts/s)
S21 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0), Ts/s)
S22 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), Ts/s)

TrSeas = model.matrix(~ ts + ts2 + S2 + S3 + S4 + S5 + S6 + S7 + S8 + S9 + S10 + S11 + S12 + 
                        S13 + S14 + S15 + S16 + S17 + S18 + S19 + S20 + S21 + S22)
best.model.s = arima(lny_short, order = best.order, include.mean = FALSE, xreg = TrSeas, method = 'ML', optim.control = list(maxit = 1000))

fs = ((Ts+1):(Ts+h))
fs2 = fs^2
S2 = rep(c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), h/s)
S3 = rep(c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), h/s)
S4 = rep(c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), h/s)
S5 = rep(c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), h/s)
S6 = rep(c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), h/s)
S7 = rep(c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), h/s)
S8 = rep(c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), h/s)
S9 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), h/s)
S10 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), h/s)
S11 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), h/s)
S12 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), h/s)
S13 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0), h/s)
S14 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0), h/s)
S15 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0), h/s)
S16 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0), h/s)
S17 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0), h/s)
S18 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0), h/s)
S19 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0), h/s)
S20 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0), h/s)
S21 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0), h/s)
S22 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), h/s)

TrSeas = model.matrix(~ fs + fs2 + S2 + S3 + S4 + S5 + S6 + S7 + S8 + S9 + S10 + S11 + S12 + 
                        S13 + S14 + S15 + S16 + S17 + S18 + S19 + S20 + S21 + S22)

#Recall that we model logarithmic transformation of liquor sales, while we need to 
#forecast sales in levels
prediction = predict(best.model.s, h, newxreg = TrSeas)
Forecast = prediction$pred
UF = Forecast + high_error
LF = Forecast + low_error

Forecast = exp(Forecast)
UF = exp(UF)
LF = exp(LF)

plot(data, xlim = c(112, 115), ylim = c(0, 2500), ylab = "Introduced Legislation", main = "Full Model In-Sample 22-Month Forecast",
     xlab = 'Congress')
lines(Forecast, col = "blue")
lines(UF, col = "red")
lines(LF, col = "red")

#Out-of-sample forecast
to = ((T+1):(T+44))
t2o = to^2
S2 = rep(c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 2)
S3 = rep(c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 2)
S4 = rep(c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 2)
S5 = rep(c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 2)
S6 = rep(c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 2)
S7 = rep(c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 2)
S8 = rep(c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 2)
S9 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 2)
S10 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 2)
S11 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 2)
S12 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 2)
S13 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0), 2)
S14 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0), 2)
S15 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0), 2)
S16 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0), 2)
S17 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0), 2)
S18 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0), 2)
S19 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0), 2)
S20 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0), 2)
S21 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0), 2)
S22 = rep(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), 2)

TrSeas = model.matrix(~ to + t2o + S2 + S3 + S4 + S5 + S6 + S7 + S8 + S9 + S10 + S11 + S12 + 
                        S13 + S14 + S15 + S16 + S17 + S18 + S19 + S20 + S21 + S22)


prediction = predict(best.arima, 44, newxreg = TrSeas)
Forecast = prediction$pred
UF = Forecast + high_error
LF = Forecast + low_error

Forecast = exp(Forecast)
UF = exp(UF)
LF = exp(LF)

plot(data, xlim = c(112, 117), ylim = c(0, 3500), ylab = "Introduced Legislation", main = "Full Model Out-Of-Sample 44-Month Forecast",
     xlab = 'Congress')
lines(Forecast, col = "blue")
lines(UF, col = "red")
lines(LF, col = "red")
