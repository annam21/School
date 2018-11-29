# Homework 8
# Anna Moeller
# 11/29/2018

# Function to draw 2 numbers, calculate Z, find mean and sd of Z
fn <- function(n, nsim = 100000){
  Z <- rep(NA, nsim)
  for(i in 1:nsim){
    XandY <- sample(1:n, 2, replace = F)
    Z[i] <- abs(diff(XandY))
  }
  return(Z)
}

# n = 4
n <- 4
Z4 <- fn(n)

mean(Z4)
(n+1)/3

sd(Z4)
sqrt((n-2)*(n+1)/18)

# n = 10
n <- 10
Z4 <- fn(n)

mean(Z4)
(n+1)/3

sd(Z4)
sqrt((n-2)*(n+1)/18)

# n = 20
n <- 20
Z4 <- fn(n)

mean(Z4)
(n+1)/3

sd(Z4)
sqrt((n-2)*(n+1)/18)