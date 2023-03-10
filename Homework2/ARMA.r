library("forecast")

# Import Data from CSV File
data <- read.csv("C:/Users/steve/Documents/GitHub/Econometrics/Homework2/A2_Data.csv")

# T Statistic Function
compute_t_statistic <- function(beta_hat, standard_err) {
  return(beta_hat / standard_err)
}

# Time Series 1
y1 <- data$y1
plot(y1)

y1_acf <- Acf(y1)
y1_pacf <- Pacf(y1)

y1_arima <- auto.arima(y1, d = 0, max.p = 5, max.q = 5, max.order = 10, seasonal = FALSE, stepwise = FALSE, ic = 'aic')
print("Time Series One")
print(y1_arima)

t1 <- compute_t_statistic(0.7837, 0.0196)
print(paste0("T Statistic: ", abs(t1)))

t1 <- compute_t_statistic(-0.0719, 0.0461)
print(paste0("T Statistic: ", abs(t1)))

plot(y1_arima$residuals)
y1_arima_acf <- Acf(y1_arima$residuals)
y1_arima_pacf <- Pacf(y1_arima$residuals)

# Time Series 2
y2 <- data$y2
plot(y2)

y2_acf <- Acf(y2)
y2_pacf <- Pacf(y2)

y2_arima <- auto.arima(y2, d = 0, max.p = 5, max.q = 5, max.order = 10, seasonal = FALSE, stepwise = FALSE, ic = 'aic')
print("Time Series Two")
print(y2_arima)

t2 <- compute_t_statistic(0.7213, 0.0259)
print(paste0("T Statistic: ", abs(t2)))

t2 <- compute_t_statistic(0.5928, 0.0247)
print(paste0("T Statistic: ", abs(t2)))

plot(y2_arima$residuals)
y2_arima_acf <- Acf(y2_arima$residuals)
y2_arima_pacf <- Pacf(y2_arima$residuals)

# Time Series 3
y3 <- data$y3
plot(y3)

y3_acf <- Acf(y3)
y3_pacf <- Pacf(y3)

y3_arima <- auto.arima(y3, d = 0, max.p = 5, max.q = 5, max.order = 10, seasonal = FALSE, stepwise = FALSE, ic = 'aic')
print("Time Series Three")
print(y3_arima)

t3 <- compute_t_statistic(0.8426, 0.0240)
print(paste0("T Statistic: ", abs(t3)))

t3 <- compute_t_statistic(-0.4302, 0.0372)
print(paste0("T Statistic: ", abs(t3)))

t3 <- compute_t_statistic(0.2021, 0.0344)
print(paste0("T Statistic: ", abs(t3)))

plot(y3_arima$residuals)
y3_arima_acf <- Acf(y3_arima$residuals)
y3_arima_pacf <- Pacf(y3_arima$residuals)


