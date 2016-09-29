# Anna Moeller
# Model for exponential time-to-event
# 3/9/16

# Load packages
library(MASS)

###########################################################################
# Version 4. Design matrix. Function that takes expo MLE when
#   lambda = log(BX) (1 or more covariates)   

# Simulate data
#    This lambda gives two photo rates: rate[1] for Feb and rate[2] for April
#    Generate half of my data with the first lambda and half with the second
  lambda.fn <- function(B, month){
    exp(B[1] + B[2]*month)
  }
  B <- c(1, 3)
  truelambda <- lambda.fn(B, month = c(0, 1)) # 0 for February, 1 for April
  n <- 1000
  
  # A vector of times to first event (response variable)
  data <- c(rexp(n/2, rate = truelambda[1]), rexp(n/2, rate = truelambda[2]))
  
  # Design matrix
  design <- matrix(c(rep(1, n), rep(0, n/2), rep(1, n/2)), ncol = 2)

# Write a function that takes the exponential MLE when lambda = log(BX)
expo.linear.fn <- function(data, design, guess){

  # Conditions to make this run correctly
  stopifnot(is.matrix(design))
  stopifnot(is.numeric(guess))
  stopifnot(ncol(design) == length(guess))
  
  # Exponential Likelihood: L(B|data)
  logL <- function(data, design, B){
    lambda <- exp(design %*% B)
    sum(log(lambda)) - (data %*% lambda)
  }
  
  # Optimize to find the MLE estimates of B
  tmp <- optim(par = guess, fn = logL, data = data, design = design,
               control = list(fnscale = -1), hessian = T)
  
  # Calculate estimate's SE, CIs
  B_hat <- tmp$par
  var_B_hat <- diag(-ginv(tmp$hessian))
  SE_B_hat <- sqrt(var_B_hat)
  LCI_B_hat <- B_hat - 1.96*SE_B_hat
  UCI_B_hat <- B_hat + 1.96*SE_B_hat
  
  # An abbreviated design matrix for back-transforming
  desg2 <- unique(design)
  
  # Back transform to lambda
  lambda_hat <- as.numeric(exp(desg2 %*% B_hat))

   # Do delta method to get LCI/UCI for lambda_hat
  
#   part_deriv <- matrix(exp(desg2 %*% B), ncol = 2)
#   var_lambda_hat <- part_deriv %*% -ginv(tmp$hessian) %*% t(part_deriv)
#   SE_lambda_hat <- sqrt(var_lambda_hat)
#   LCI_lambda_hat <- lambda_hat - 1.96*SE_lambda_hat
#   UCI_lambda_hat <- lambda_hat + 1.96*SE_lambda_hat
  
  ### Delta method, the other way, to get a full matrix out. 
  part_deriv <- exp(desg2 %*% B_hat) # This works because the derivative is the same as the function, and is the same for both B0 and B1
  mat_part_deriv <- cbind(part_deriv, part_deriv) # This probably only works for this example
  vc_lambda_hat <- mat_part_deriv %*% -ginv(tmp$hessian) %*% t(mat_part_deriv)
  SE_lambda_hat <- sqrt(diag(vc_lambda_hat))
  LCI_lambda_hat <- lambda_hat - 1.96*SE_lambda_hat
  UCI_lambda_hat <- lambda_hat + 1.96*SE_lambda_hat

#   vc_lambda_hat <- rbind(matrix(rep(exp(c(1,0) %*% B_hat), 2), ncol = 2),
#                          matrix(rep(exp(c(1,1) %*% B_hat), 2), ncol = 2)) %*%
#                    -ginv(tmp$hessian) %*% 
#                    t(rbind(matrix(rep(exp(c(1,0) %*% B_hat), 2), ncol = 2), 
#                            matrix(rep(exp(c(1,1) %*% B_hat), 2), ncol = 2)))
    
  # output things
  out <- list(B_hat = B_hat,
              SE_B_hat = SE_B_hat,
              LCI_B_hat = LCI_B_hat,
              UCI_B_hat = UCI_B_hat,
              lambda_hat = lambda_hat,
              SE_lambda_hat = SE_lambda_hat,
              LCI_lambda_hat = LCI_lambda_hat,
              UCI_lambda_hat = UCI_lambda_hat)
}

# # Call the function
# ests <- expo.linear.fn(data, design, guess = c(2, 50))
# ests

###########################################################################
# # Version 3. Function that takes expo MLE when lambda is a multivariate fn   
# 
# # Simulate data
# #   This lambda gives two photo rates: rate[1] for Feb and rate[2] for April
# #   Generate half of my data with the first lambda and half with the second
# lambda.fn <- function(B, month){
#   log(B[1] + B[2]*month)
# }
# B <- c(2, 50)
# month <- c(0, 1)            # 0 for February, 1 for April
# truelambda <- lambda.fn(B, month)
# n <- 1000
# data <- data.frame(time = c(rexp(n/2, rate = truelambda[1]), rexp(n/2, rate = truelambda[2])),
#                    month = c(rep(0, n/2), rep(1, n/2)))
# 
# # Initialize things
# guess <- c(2, 50)
# 
# # Write a function that takes the exponential MLE when lambda
# #   is a multivariate function
# expo.multi.fn <- function(data, lambda.fn, guess){
#   
#   # Exponential Likelihood: L(B|data)
#   logL <- function(data, lambda.fn, B){
#     sum(log(lambda.fn(B, data$month))) - sum(data$time * lambda.fn(B, data$month))
#   }
#   
#   # Optimize to find the MLE estimates of B
#   tmp <- optim(par = guess, fn = logL, data = data, lambda.fn = lambda.fn,
#                control = list(fnscale = -1), hessian = T)
#   
#   # Calculate estimate's SE, CIs
#   B_hat <- tmp$par
#   var_B_hat <- diag(-ginv(tmp$hessian))
#   SE_B_hat <- sqrt(var_B_hat)
#   LCI_B_hat <- B_hat - 1.96*SE_B_hat
#   UCI_B_hat <- B_hat + 1.96*SE_B_hat
#   
#   # Back transform to lambda
#   ### Do I want/need this? What's interesting is probably B
#   lambda_hat <- lambda.fn(B_hat, month = c(0,1))
#   LCI_lambda_hat <- lambda.fn(LCI_B_hat, month = c(0,1))
#   UCI_lambda_hat <- lambda.fn(UCI_B_hat, month = c(0,1))
#   
#   # output things
#   out <- list(B_hat = B_hat,
#               SE_B_hat = SE_B_hat,
#               LCI_B_hat = LCI_B_hat,
#               UCI_B_hat = UCI_B_hat,
#               lambda_hat = lambda_hat,
#               LCI_lambda_hat = LCI_lambda_hat,
#               UCI_lambda_hat = UCI_lambda_hat)
# }
# 
# # Call the function
# ests <- expo.multi.fn(data, lambda.fn, guess)
# ests
# 
# ############################################################################
# Version 2. Function that takes expo MLE when lambda is a univariate fn  
# Initialize stuff
lambda.fn <- function(B0){
  log(B0)
}
n <- 100
B0 <- 10
data <- rexp(n, rate = lambda.fn(B0))
guess <- 10

# Write a function that takes the exponential MLE when 
#   lambda is a univariate function
expo.uni.fn <- function(data, lambda.fn, guess){
  
  # Exponential Likelihood: L(B0|data)
  logL <- function(data, lambda.fn, B0){
    length(data)*log(lambda.fn(B0)) - sum(lambda.fn(B0)*data)
  }
  
  # Optimize to find the MLE estimate of B0
  tmp <- optim(par = guess, fn = logL, data = data, lambda.fn = lambda.fn,
               control = list(fnscale = -1), hessian = T)
  
  # Calculate estimate's SE, CIs
  B0_hat <- tmp$par
  var_B0_hat <-  -ginv(tmp$hessian)
  SE_B0_hat <- as.numeric(sqrt(var_B0_hat)) ## This may be a problem for bivariate
  LCI_B0_hat <- B0_hat - 1.96*SE_B0_hat
  UCI_B0_hat <- B0_hat + 1.96*SE_B0_hat
  
  # Back transform to lambda
  lambda_hat <- lambda.fn(B0_hat)
  LCI_lambda_hat <- lambda.fn(LCI_B0_hat)
  UCI_lambda_hat <- lambda.fn(UCI_B0_hat)
  
  # output things
  out <- list(B0_hat = B0_hat,
              SE_B0_hat = SE_B0_hat,
              LCI_B0_hat = LCI_B0_hat,
              UCI_B0_hat = UCI_B0_hat,
              lambda_hat = lambda_hat,
              LCI_lambda_hat = LCI_lambda_hat,
              UCI_lambda_hat = UCI_lambda_hat)
}


############################################################################
# Version 1. Build a function that will directly estimate lambda when lambda is a number

# Initialize things
lambda <- 10
n <- 1000
data <- rexp(n, rate = lambda)
guess <- 5

expo.const.fn <- function(data, guess){
  
  # Exponential Likelihood: L(lambda|data)
  # It's okay that this takes lambda and the general function doesn't, because we only 
  #   use this function in optim, with lambda = guess
  logL <- function(data, lambda){
    length(data)*log(lambda) - sum(lambda*data)
  }
  
  # Optimize to find the MLE estimate of lambda-hat
  tmp <- optim(par = guess, fn = logL, data = data, 
               control = list(fnscale = -1), hessian = T)
  
  # Calculate lambda_hat and its SE, CIs
  lambda_hat <- tmp$par
  var_lambda_hat <-  -ginv(tmp$hessian)
  SE_lambda_hat <- as.numeric(sqrt(var_lambda_hat)) ## This may be a problem for bivariate
  LCI <- lambda_hat - 1.96*SE_lambda_hat
  UCI <- lambda_hat + 1.96*SE_lambda_hat
  
  # output things
  out <- list(lambda_hat = lambda_hat,
              SE_lambda_hat = SE_lambda_hat,
              LCI = LCI,
              UCI = UCI)
}

################################################################################ 
# Call the functions on my randomwalk data
# These are the data from 100 simulations of populations of 50 animals
load("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Simulation/randomwalk.RData")
timetohole <- randomwalk$timetohole

load("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Simulation/timetohole100.RData")
load("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/Simulation/timetohole50.RData")

### timetohole50 has censors that I need to figure out how to deal with
timetohole50 <-timetohole50[!is.na(timetohole50)]

# Estimate lambda_hat with expo.const and plot the exponential curve
ests50 <- expo.const.fn(timetohole50, guess = 1)
ests100 <- expo.const.fn(timetohole100, guess = 1)

par(mfrow = c(1,2),
    oma = c(0,0,2,0))
prob100 <- hist(timetohole100, prob = T, xlab = "Time to First Capture", 
               main = "N = 100", xlim = c(0, 400))
x <- 1:max(prob100$breaks) - 1
y <- dexp(x, ests100$lambda_hat)
lines(y~x, col = "red", lwd = 2)

prob50 <- hist(timetohole50, prob = T, xlab = "Time to First Capture", 
                main = "N = 50", xlim = c(0, 400))
x <- 1:max(prob50$breaks) - 1
y <- dexp(x, ests50$lambda_hat)
lines(y~x, col = "red", lwd = 2)
title(main = "Simulated Time to First Capture", outer = T)


x <- 1:max(prob50$breaks) - 1
y <- dexp(x, ests$lambda_hat)
lines(y~x, col = "red", lwd = 2)

# Estimate lambda_hat with expo.uni.fn and plot the exponential curve
ests2 <- expo.uni.fn(timetohole, lambda.fn, guess = 5)
y2 <- dexp(x, ests2$lambda_hat)
lines(y2~x, col = "blue", lty = 2, lwd = 2)

#######################################################
## Call the functions on my 2014 data

load("C:/Users/anna.moeller/Documents/GitHub/Camera-trap-study/2014 data/2014timetoevent_per3.RData")
# The object is called time2014
time2014 <- mutate(time2014,
                   time = as.numeric(time))

# Use data from Deer Canyon
# Split the data in half: first half = month 0, second half = month 1
tmp <- filter(time2014,
              site == "DC",
              period != min(period) + (max(period) - min(period))/2) %>%
  mutate(month = ifelse(period < min(period) + (max(period) - min(period))/2, 0, 1))

# My response vector is:
resp <- tmp$time

# My design matrix is:
B <- matrix(c(rep(1, length(resp)), 
              tmp$month), ncol = 2)

# Estimate lambda_hat with expo.linear
ests <- expo.linear.fn(resp, B, guess = c(2, 2))

# Plot data
par(mfrow = c(1, 2))

# First half of data
prob <- hist(resp[B[, 2] == 0], prob = T, xlab = "Time to First Capture", 
             main = "Time to First Capture")
x <- 1:max(prob$breaks) - 1
y <- dexp(x, ests$lambda_hat[1])
lines(y~x, col = "red", lwd = 2)

# Second half of data
prob2 <- hist(resp[B[, 2] == 1], prob = T, xlab = "Time to First Capture", 
             main = "Time to First Capture")
x <- 1:max(prob2$breaks) - 1
y <- dexp(x, ests$lambda_hat[2])
lines(y~x, col = "red", lwd = 2)

# Yikes. Not exponential. 


