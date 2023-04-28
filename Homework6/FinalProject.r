library("forecast")
library("urca")
library("vars")

data <- read.csv("C:/Users/steve/Documents/GitHub/Econometrics/Homework6/Oil.csv")
WTI <- ts(data$WTI, frequency = 12, start = c(1987, 5))
Brent <- ts(data$Brent, frequency = 12, start = c(1987, 5))

####################
# Check Stationary
####################

# Split Data
t0 <- c(1987, 5)
t1 <- c(2004, 12)
t2 <- c(2005, 1)

# Create Sets
WTI_train <- window(WTI, start = t0, end = t1)
WTI_test <- window(WTI, start = t2)

Brent_train <- window(Brent, start = t0, end = t1)
Brent_test <- window(Brent, start = t2)

# First Test
WTI_ADF_none <- ur.df(WTI_train, type = "none", lags = 12, selectlags = c("AIC"))
print(summary(WTI_ADF_none))

# Second Test
WTI_ADF_drift <- ur.df(WTI_train, type = "drift", lags = 12, selectlags = c("AIC"))
print(summary(WTI_ADF_drift))

# Third Test
WTI_ADF_trend <- ur.df(WTI_train, type = "trend", lags = 12, selectlags = c("AIC"))
print(summary(WTI_ADF_trend))

# First Difference Sequence
dwti <- diff(WTI_train)

# First Test
WTI_ADF_none <- ur.df(dwti, type = "none", lags = 12, selectlags = c("AIC"))
print(summary(WTI_ADF_none))

# Second Test
WTI_ADF_drift <- ur.df(dwti, type = "drift", lags = 12, selectlags = c("AIC"))
print(summary(WTI_ADF_drift))

# Third Test
WTI_ADF_trend <- ur.df(dwti, type = "trend", lags = 12, selectlags = c("AIC"))
print(summary(WTI_ADF_trend))

####################
# ARMA Model Selection
####################

WTI_ACF <- Acf(WTI_test)
WTI_PACF <- Pacf(WTI_test)

# AIC Model
WTI_ARMA_AIC <- auto.arima(WTI_train, d = 0, max.p = 5, max.q = 5, max.order = 10, seasonal = FALSE, stepwise = FALSE, ic = "aic")
print("********** ARIMA AIC **********")
print(summary(WTI_ARMA_AIC))

# BIC Model
WTI_ARMA_BIC <- auto.arima(WTI_train, d = 0, max.p = 5, max.q = 5, max.order = 10, seasonal = FALSE, stepwise = FALSE, ic = "bic")
print("********** ARIMA BIC **********")
print(summary(WTI_ARMA_BIC))

####################
# Co integration Test and VECM
####################

df_level <- data.frame(WTI_train, Brent_train)

pairs_test <- ca.jo(df_level, type = "eigen", ecdet = "const", K = 2, spec = "transitory")
print(summary(pairs_test))

vecm <- cajorls(pairs_test, r = 1)
error <- vecm$rlm$model["ect1"]

vecm_ADF <- ur.df(error$ect1, type = "none", lags = 12, selectlags = c("AIC"))
print(summary(WTI_ADF_none))

plot(error$ect1)

cajorls(pairs_test)
coef(summary(cajorls(pairs_test)$rlm))

####################
# Forecast
####################

var.model = vec2var(pairs_test)
H = 12
fc <- predict(var.model, n.ahead = H)

WTI_forecast <- ts(fc$fcst$WTI_train[1:H,1], frequency=12, start=t2)
Brent_forecast <- ts(fc$fcst$Brent_train[1:H,1], frequency=12, start=t2)

plot(WTI_forecast)
plot(Brent_forecast)
plot(fc)

WTI_RMSE = 0
WTI_MAE = 0
for (i in 1:length(WTI_forecast)) {
  
  WTI_RMSE <- WTI_RMSE + (WTI_forecast[i] - WTI_test[i])^2
  WTI_MAE <- WTI_MAE + abs(WTI_forecast[i] - WTI_test[i])
  
}
WTI_RMSE <- sqrt(WTI_RMSE / length(WTI_forecast))
WTI_MAE <- WTI_MAE / length(WTI_forecast)

BRENT_RMSE = 0
BRENT_MAE = 0
for (i in 1:length(Brent_forecast)) {
  
  BRENT_RMSE <- BRENT_RMSE + (Brent_forecast[i] - Brent_test[i])^2
  BRENT_MAE <- BRENT_MAE + abs(Brent_forecast[i] - Brent_test[i])
  
}
BRENT_RMSE <- sqrt(BRENT_RMSE / length(BRENT_forecast))
BRENT_MAE <- BRENT_MAE / length(BRENT_forecast)






