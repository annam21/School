# Anna Moeller
# Lab 6
# 10/5/2015

library(MASS)

# another MLE example
trueyes <- 0.2     # true value we're trying to estimate
randyes <- 0.5     # prob of responding yes no matter what the truth is

# What is the probability of observing a res? 
rr.likelihoodknown <- function(p, y, randyes){
  pp <- randyes + p - randyes*p
  ll <- sum(dbinom(y, 1, pp, log = T))
  return(ll)
}

# Generate data
N <- 1000   # number of respondents
y <- rbinom(N, 1, randyes + trueyes - randyes*trueyes) # generate survey responses
b <- 0.5   # initial value of the parameter

# optimize
res <- optim(b, rr.likelihoodknown, control = list(fnscale = -1), hessian = T, 
             y = y, randyes = randyes)



############
# joint likelihood

# How many samples of the coin flip do we need to know the probability of heads?
probh <- matrix(NA, 1000, 2)
for(z in 1:1000){
  x <- rbinom(z, 1, randyes)
  res <- optim(b, joint.likelihood, control = list(fnscale = -1), hessian = T, ??????????      )
  probh[z,1] = res$par[1]
  probh[z,2] = ????????:???
}