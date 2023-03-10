#####################
##
## R basics
##
#####################

# create a numeric vector
a <- c(1, 2, 3, 4)
a[3]
# create a vector of logical values
b <- c(TRUE, FALSE, TRUE, FALSE, FALSE)
# create a vector of character strings
c <- c("aa", "bb", "cc", "dd", "ee")

# create a numeric matrix
A <- matrix( c(2, 4, 3, 1, 5, 7), nrow=3, ncol=2) 
# transpose of a matrix
t(A)
# combine two matrices by column
B = matrix(c(7, 4, 2), nrow=3, ncol=1) 
cbind(A, B)
# combine two matrices by row
C = matrix( c(6, 2), nrow=1, ncol=2)
rbind(A, C)

# Deconstruction
c(A)

#####################
##
## R simulation
##
#####################


# Normal: rnorm(n, mean = 0, sd = 1)
rnorm(6)
rnorm(4, 0, 20)

# Continuous uniform: runif(n, min = 0, max = 1)
runif(10)
runif(5, 0, 2)

# Poissoin: rpois(n, lambda)
rpois(10, 6)

# Exponential: rexp(n, rate = 1)
rexp(4, 1.5)

# Binomial: rbinom(n, size, prob)
rbinom(10, 3, 0.5)

#####################
##
## R For Loop
##
#####################

# A simple example
for (i in 1:10) {
  print(i^3)
}

# Create a vector filled with random normal values
Z <- rnorm(100)

# Initialization
Y <- rep(0, length(Z))

for(i in 1:length(Z)) {
  Y[i] <- sum(Z[1:i]^2)
}

# A different way
Y[1] <- Z[1]^2
for(i in 2:length(Z)) {
  Y[i] <- Y[i-1] + Z[i]^2
}

print(i)
print(sum(Z))

# A simple way
sum(Z^2)
# Vector/Matrix multiplication
t(Z) %*% Z

# Sample mean and standard error
Zmean <- sum(Z)/length(Z)
Zsd <- sqrt( sum((Z - Zmean)^2) / (length(Z)-1) )
print(c(Zmean, Zsd))

#####################
##
## Data visualization
##
#####################

# Scatter plot
scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")

# BoxPlot --- Check for outliers
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", 
                                            boxplot.stats(cars$speed)$out))  # box plot for 'speed'
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", 
                                              boxplot.stats(cars$dist)$out))  # box plot for 'distance'

# Density plot
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency") 
polygon(density(cars$speed), col="red")
plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency")  
polygon(density(cars$dist), col="blue")

# Linear regression
linearMod <- lm(dist ~ speed, data=cars)  
summary(linearMod)


#####################
##
## R Sequential Simulation
##
#####################

T <- 1000
eps <- rnorm(T)

y <- rep(0, T)
phi <- 0

for (t in 2:T) {
  y[t] <- phi*y[t-1] + eps[t]
}

plot(y, type='l', col='blue', ylab='y', xlab='Time', 
     main=paste("Simulated Process with phi =", phi))


#####################
##
## R OLS Simulation
##
#####################
library(MASS) # We need this library to simulate multivariate normals

mu <- c(2, 10)
Sigma <- matrix( c(1, 0.5, 0.5, 4), nrow=2, ncol=2 )
beta_true <- c(10-2*0.5/1, 0.5/1)

N_sim <- 5000
beta_hat <- matrix(0, N_sim, length(beta_true))
beta_Var <- matrix(0, N_sim, length(beta_true))
CLT <- matrix(0, N_sim, length(beta_true))

start_time <- Sys.time()

for (k in 2:4) {
  N_sample <- 10^k
  for (i in 1:N_sim) {
    Z <- mvrnorm(N_sample, mu, Sigma)
    X <- Z[,1]
    Y <- Z[,2]
    Xdata <- cbind(1, X)
    beta_hat[i,] <- solve( t(Xdata) %*% Xdata ) %*% t(Xdata) %*% Y
    epsilon <- Y - Xdata %*% beta_hat[i,]
    beta_Var[i,] <- t(epsilon) %*% epsilon/N_sample * diag(solve( t(Xdata) %*% Xdata ))
    CLT[i,] <- (beta_hat[i,] - beta_true) / sqrt(beta_Var[i,])
  }
  
  if (N_sample == 100) {
    LLN_S <- beta_hat
    CLT_S <- CLT
  }  else if (N_sample == 1000) {
    LLN_M <- beta_hat
    CLT_M <- CLT
  } else if (N_sample == 10000) {
    LLN_L <- beta_hat
    CLT_L <- CLT
  }
}

end_time <- Sys.time()
mes <- sprintf("The for loop took %0.2f seconds", difftime(end_time,start_time,unit=("secs")))
message(mes)


j <- 2

par(mfrow=c(1, 2))  # divide graph area into 2 columns
plot(density(LLN_S[,j]), main="Law of Large Numbers", ylab="Frequency", 
     xlim=c(min(LLN_S[,j]), max(LLN_S[,j])), ylim=c(0, max(density(LLN_L[,j])$y)), col="blue", lwd=2) 
lines(density(LLN_M[,j]), col="darkviolet", lwd=2)
lines(density(LLN_L[,j]), col="black", lwd=2)
abline(v=beta_true[j], col='red', lwd=2)
legend("topleft", lty=1, col=c("blue","darkviolet","black","red"), 
       c("Small","Medium","Large","True"), lwd=2)
plot(density(CLT_L[,j]), main="Central Limit Theorem", ylab="Frequency", 
     xlim=c(-4, 4), ylim=c(0,0.41), col="blue", lwd=2) 
lines(density(CLT_M[,j]), col="darkviolet", lwd=2)
lines(density(CLT_S[,j]), col="black", lwd=2)
curve(dnorm(x, mean=0, sd=1), -4, 4, col="red", lwd=2, add=TRUE)
legend("topleft", lty=1, col=c("blue","darkviolet","black","red"), 
       c("Small","Medium","Large","True"), lwd=2)


# 95% confidence interval
critical_value <- qnorm(1-0.05/2,0,1)
standard_error_j <- sqrt(beta_Var[,j])
CIj <- cbind( beta_hat[,j] - critical_value*standard_error_j, 
              beta_hat[,j] + critical_value*standard_error_j )
# Sample coverage rate
sum( (beta_true[j] > CIj[,1]) * (beta_true[j] < CIj[,2]) ) / N_sim

# (1-alpha) confidence interval
alpha <- seq(0,1,0.01)
cover_rate <- rep(0, length(alpha))
for (k in (1:length(alpha))) {
  critical_value <- qnorm(1-alpha[k]/2,0,1)
  standard_error_j <- sqrt(beta_Var[,j])
  CIj <- cbind( beta_hat[,j] - critical_value*standard_error_j, 
                beta_hat[,j] + critical_value*standard_error_j )
  # Sample coverage rate
  cover_rate[k] <- sum( (beta_true[j] > CIj[,1]) * (beta_true[j] < CIj[,2]) ) / N_sim
  
}

plot(1-alpha, cover_rate, type='b', col="blue")
abline(a=0, b=1, col="red")







