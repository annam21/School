# Anna Moeller
# 9/28/2015
# MLE lab

# generate data
ndata <- rnorm(100, 10, 3)

# we are trying to optimize: logL(mu, sigma | data) = sum()

# optimize parameter mean (p[1]) and sd (p[2])
n.likelihood <- function(p, data){
  ll <- sum(dnorm(data, p[1], p[2], log = T))
  return(ll)
}

# b is the initial value vector for each of the parameters we're trying to estimate
# SD must be positive
# These just need to be close
b <- c(1, 1)

# actually optimize function
# default: minimizes function, so use fnscale = -1
# hessian returns matrix of second partial derivatives, which gives us our variance
res <- optim(b, n.likelihood, control = list(fnscale = -1), hessian = T, data = ndata)

# explain optimum output
res
# par is the parameter estimates
# value is the value of the logL at its max
# counts is the number of times it had to do a computation to get this
# convergence - we want 0, anything else is an error
# hessian gives us the shape of the top of the logL curve

-ginv(res$hessian) 
# gives variance matrix (with package MASS)
#     covariance for norm should be identical and close to 0 (because in norm, they're independent)
# Not the most precise function for calculating inverse matrix



