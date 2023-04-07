# Create Helper Methods
indicatorFunction <- function(arr, c) {
  
  avg <- 0
  for(n in arr) {
    
    if (n > c) {
      avg <- avg + 1
    }
    
  }
  
  return (avg / length(arr))
  
}

importanceSampling <- function(arr, c) {
  
  avg = 0
  for(n in arr) {
    
    if (n > c) {
      avg <- avg + dnorm(n) / dnorm(n, mean = 5, sd = 1)
    }
    
  }

  return (avg / length(arr))
    
}

####################
# Initial Data
####################

# Set Sample Size
N = 10^6

####################
# A Simple Task
####################

# P(X > 5) = E[I(X > 5)]
simpleTask = 1 - pnorm(5)

# Directly Generate Random Numbers
c = 5
RV = rnorm(N)

# Calculate Sample Average of Indicator Function
NaiveAvg <- indicatorFunction(RV, c)

# Sample From Different Distribution
RV = rnorm(N, mean = 5, sd = 1)

# Importance Sampling Procedure
ImportanceSamplingAverage <- importanceSampling(RV, c)

####################
# A Slightly Harder Task
####################

harderTask = pnorm(1) - pnorm(-1)

# Generate Uniform Random Variables 
U = runif(N, min = -1, max = 1)

avg <- 0
for(n in U) {
  
  if (n > -1 & n < 1) {
    avg <- avg + dnorm(n) / dunif(n, min = -1, max = 1)
  }
  
}

USamples <- avg / length(U)

# Generate Normal Random Variables
N = rnorm(N, mean = 0, sd = 1)

avg <- 0
for(n in N) {
  
  if (n > -1 & n < 1) {
    avg <- avg + 1
  }
  
}

NSamples <- avg / length(N)

# Generate Cauchy Random Variables
C = rt(N, df = 1)

avg <- 0
for(n in C) {
  
  if (n > -1 & n < 1) {
    avg <- avg + dnorm(n) / dt(0, df = 1)
  }
  
}

CSamples <- avg / length(C)










