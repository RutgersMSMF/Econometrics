library("forecast")
library("urca")

# Import Data from CSV File
data <- read.csv("C:/Users/steve/Documents/GitHub/Econometrics/Homework4/608_4th.csv")

y1 <- ts(data$y1, freq = 12, start = c(2000, 1))
y2 <- ts(data$y2, freq = 12, start = c(2000, 1))

##########

# Y1 Start

##########

# Y1 Season Plot
seasonplot(y1, col = rainbow(12))

# Y1 ACF and PACF
plot(y1, type = 'l')
y1ACF <- Acf(y1)
y1PACF <- Pacf(y1)

# Regress Y1 on a Linear Trend 
time <- seq(1, length(y1), by = 1)
y1LM <- lm(y1 ~ time)

# Linear Model Residuals and ACF and PACF
plot(y1LM$residuals, type = 'l')
y1LMACF <- Acf(y1LM$residuals)
y1LMPACF <- Pacf(y1LM$residuals)

# ADF Test
y1ADF <- ur.df(y1, type = "none", lags = 5, selectlags = "BIC")
print(summary(y1ADF))
y1ADF <- ur.df(y1, type = "drift", lags = 5, selectlags = "BIC")
print(summary(y1ADF))
y1ADF <- ur.df(y1, type = "trend", lags = 5, selectlags = "BIC")
print(summary(y1ADF))

# Fit ARIMA Model
y1Arima <- Arima(y1, order = c(0, 0, 2), xreg = 1:length(y1))
print(summary(y1Arima))

plot(y1Arima$residuals)
y1ArimaACF <- Acf(y1Arima$residuals)
y1ArimaPACF <- Pacf(y1Arima$residuals)

##########

# Y2 Start

##########

# Y2 Season Plot
seasonplot(y2, col = rainbow(12))

# Y2 ACF and PACF
plot(y2, type = 'l')
y2ACF <- Acf(y2)
y2PACF <- Pacf(y2)

# Regress Y1 on a Linear Trend 
time <- seq(1, length(y2), by = 1)
y2LM <- lm(y2 ~ time)

# Linear Model Residuals and ACF and PACF
plot(y2LM$residuals, type = 'l')
y2LMACF <- Acf(y2LM$residuals)
y2LMPACF <- Pacf(y2LM$residuals)

# ADF Test
y2ADF <- ur.df(y2, type = "none", lags = 5, selectlags = "BIC")
print(summary(y2ADF))
y2ADF <- ur.df(y2, type = "drift", lags = 5, selectlags = "BIC")
print(summary(y2ADF))
y2ADF <- ur.df(y2, type = "trend", lags = 5, selectlags = "BIC")
print(summary(y2ADF))

# Fit ARIMA Model
y2Arima <- Arima(y2, order = c(1, 1, 0), xreg = 1:length(y2))
print(summary(y2Arima))

plot(y2Arima$residuals)
y2ArimaACF <- Acf(y2Arima$residuals)
y2ArimaPACF <- Pacf(y2Arima$residuals)




