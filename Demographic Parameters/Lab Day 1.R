  # Estimating Demographic Parameters
  # Lab Day 1
  # Anna Moeller
  # 9/1/2016

  # Load packages
  library(MASS)

# 1. Write a function for the binomial probability
  binpr <- function(y, n, p) {
    # Calculate binomial probability
    # Arguments: y = # successes
    #            n = # of trials
    #            p = probability of success
    # Returns probability
    
    choose(n, y) * p^y * (1-p)^(n-y)
  }
  
  # Check with dbinom
  binpr(10, 15, 0.5)
  dbinom(10, 15, 0.5)
  
# 2. Plot likelihood functions
  x <- (1:1000)/1000
  tst1 <- binpr(y = 6, n = 10, p = x)
  tst2 <- binpr(y = 60, n = 100, p = x)
  
  par(mfrow = c(1, 2))
  plot(x, tst1, type = "l", xlab = "p-hat", ylab = "Likelihood", main = "6, 10")
  plot(x, tst2, type = "l", xlab = "p-hat", ylab = "Likelihood", main = "60, 100")
  
# 3. Plot log-likelihood functions
  logL1 <- log(tst1)
  logL2 <- log(tst2)
  
  plot(x, logL1, type = "l", xlab = "p-hat", ylab = "log Likelihood", main = "6, 10")
  lines(x, dbinom(6, 10, x, log = T), col = "red") # To check that I did it right
  plot(x, logL2, type = "l", xlab = "p-hat", ylab = "log Likelihood", main = "60, 100",
       ylim = c(-35, 0))
  
# 4. Use optim and calculate SE(phat)
  # This is the maximum of the likelihood, not the log likelihood, but they should be the same
  opt <- optim(par = 0.5, fn = binpr, y = 23, n = 71, control = list(fnscale = -1), hessian = T)
  
  # Plot it for kicks
  par(mfrow = c(1, 1))
  plot(x, binpr(y = 23, n = 71, p = x), type = "l", xlab = "p-hat", ylab = "Likelihood")
  abline(v = opt$par, col = "red") # Truth
  
  # Calculate SE of p-hat
  sqrt(opt$par*(1-opt$par)/71)
  # -ginv(opt$hessian)  # This is wrong on the probability scale
  # -1/opt$hessian  # This is wrong on the probability scale
  
# Extra credit: Write the log likelihood
  logL <- function(y, n, p) {
    log(choose(n, y)) + y*log(p) + (n - y)*log(1 - p)
  }
  
  # Test it
  logL(10, 15, 0.5)
  dbinom(10, 15, 0.5, log = T)
  
  # See if this fixes my variance estimate
  opt2 <- optim(par = 0.5, fn = logL, y = 23, n = 71, control = list(fnscale = -1), hessian = T)
  
  # Calculate SE of p-hat
  sqrt(opt2$par*(1-opt2$par)/71)
  sqrt(-ginv(opt2$hessian))
  sqrt(-1/opt2$hessian)
  # Sweet! to calculate variance with the hessian, you have to be on the logL scale
  