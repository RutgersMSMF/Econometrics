library("forecast")
library("TSA")
library("MCS")

# Import Data from CSV File
data <- read.csv("C:/Users/steve/Documents/GitHub/Econometrics/Homework3/A3.csv")

y <- data$y
yTrain <- y[1:900]

# 1. 
# Fit AR Process to Training Set
ARProcess <- arima(yTrain, order = c(1, 0, 0))

# Report ACF and PACF of Residuals
ARProcessACF <- Acf(ARProcess$residuals)
ARProcessPACF <- Pacf(ARProcess$residuals)

# Apply LB.Test()
print(paste0("AR(1) Process: ", LB.test(ARProcess, lag = 12)))

# 2. 
# Fit MA Process to Training Set 
MAProcess <- arima(yTrain, order = c(0, 0, 1))

# Report ACF and PACF of Residuals
MAProcessACF <- Acf(MAProcess$residuals)
MAProcessPACF <- Pacf(MAProcess$residuals)

# Apply LB.Test()
print(paste0("MA(1) Process: ", LB.test(MAProcess, lag = 12)))

# 3. 
# Solve for ARIMA Process
yFalse <- auto.arima(yTrain, d = 0, max.p = 10, max.q = 10, max.order = 15, seasonal = FALSE, stepwise = FALSE, ic = 'aic')

# Report ACF and PACF of Residuals
yFalseACF <- Acf(yFalse$residuals)
yFalsePACF <- Pacf(yFalse$residuals)

# Apply LB.Test()
LB.test(yFalse, lag = 12)

# 4. 
# Solve For ARIMA Process
yTrue <- auto.arima(yTrain, d = 0, max.p = 10, max.q = 10, max.order = 15, seasonal = FALSE, stepwise = TRUE, ic = 'aic')

# Report ACF and PACF of Residuals
yTrueACF <- Acf(yTrue$residuals)
yTruePACF <- Pacf(yTrue$residuals)

# Apply LB.Test()
LB.test(yTrue, lag = 12)

# 5. 
# Report AIC and BIC 
print(paste0("Y False AIC: ", yFalse$aic))
print(paste0("Y True AIC: ", yTrue$aic))

print(paste0("Y False BIC: ", yFalse$bic))
print(paste0("Y True BIC: ", yTrue$bic))

# 6. 
# Forecasting 
Loss <- matrix(nrow = 100, ncol = 4)

for (t in 1:100) {
  
  
  
}

optimizationRoutineOne <- MCSprocedure(Loss, alpha = 0.05, B = 5000)
optimizationRoutineTwo <- MCSprocedure(Loss, alpha = 0.15, B = 5000)







