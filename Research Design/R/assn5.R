# Anna Moeller
# Assignment 5
# 9/28/2015

# function of logL of normal distribution (fn to be optimized)
n.likelihood <- function(p, data){
  ll <- sum(dnorm(data, p[1], p[2], log = T))
  return(ll)
}

# 1. 
  # Create RVs that are the average of two RVs from unif(0,1)
  newdata <- (runif(100, 0, 1) + runif(100, 0, 1)) / 2  
  
  # Select initial values
  b <- c(1, 1)
  
  # optimize mean and sd
  res <- optim(b, n.likelihood, control = list(fnscale = -1), hessian = T, data = newdata)
  
  # output from optimizing logL:
  # estimate of mean = 0.5486868, sd = 0.1909085

  # SE(mean) = 0.2663244
  sqrt(-ginv(res$hessian)[1, 1])
  
  # SE(SD) = 0.1884756
  sqrt(-ginv(res$hessian)[2, 2])

  # compare output with mean() and sd()
  mean(newdata) # 0.5486932
  sd(newdata) # 0.1918524

  # histogram
  hist(newdata)

# 2.
  # Generate data
  data <- c(rnorm(250, 10, 3), rnorm(750, 20, 2))

  # Take a look at them
  hist(data)

  # Use normal likelihood to estimate
  # Select initial values
  b <- c(1, 1)
  
  # optimize mean and sd
  res <- optim(b, n.likelihood, control = list(fnscale = -1), hessian = T, data = data)

  # Compare
  mean(data)
  sd(data)

# 3.
  
  # write a representation of the logL equation
  n.likelihood <- function(p, data){
    ll <- sum(log(p[1] * dnorm(data, p[2], p[3]) + 
                 (1 - p[1]) * dnorm(data, p[4], p[5])))
    return(ll)
  }

  # try to run this
  # Select initial values
  b <- c(.5, 10, 3, 20, 2) # True values
  c <- c(.5, 5, 3, 5, 2) # Means less than 10
  d <- c(.5, 25, 3, 25, 2) # Means greater than 20

  # optimize mean and sd
  res <- optim(b, n.likelihood, control = list(fnscale = -1), hessian = T, data = data)
  res2 <- optim(c, n.likelihood, control = list(fnscale = -1), hessian = T, data = data)
  res3 <- optim(d, n.likelihood, control = list(fnscale = -1), hessian = T, data = data)

  # Get the SE(mean) and SE(SD)
  # starting value = 5
  sqrt(-ginv(res2$hessian)[2, 2]) # mean1
  sqrt(-ginv(res2$hessian)[3, 3]) # SD1
  sqrt(-ginv(res2$hessian)[4, 4]) # mean2
  sqrt(-ginv(res2$hessian)[5, 5]) # SD2
  # starting value = 25
  sqrt(-ginv(res3$hessian)[2, 2]) # mean1
  sqrt(-ginv(res3$hessian)[3, 3]) # SD1
  sqrt(-ginv(res3$hessian)[4, 4]) # mean2
  sqrt(-ginv(res3$hessian)[5, 5]) # SD2
