T <- 200
eps <- rnorm(T)

y1 <- rep(0, T)
y2 <- rep(0, T)
y3 <- rep(0, T)
phi <- 0.95

y1[1] = 10
for (t in 2:T) {
  y1[t] <- phi * y1[t-1] + eps[t]
}

y2[1] = 0  
for (t in 2:T) {
  y2[t] <- phi * y2[t-1] + eps[t]
}

y3[1] = -10
for (t in 2:T) {
  y3[t] <- phi * y3[t-1] + eps[t]
}

plot(y1, type = 'l', col = 'green', ylab = 'y', xlab = 'Time', 
     main = paste("Simulated Process with phi = ", phi), 
     ylim = c(min(y1, y2, y3), max(y1, y2, y3)))

lines(y2, col = 'blue')
lines(y3, col = 'red')





