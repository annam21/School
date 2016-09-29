# Anna Moeller
# 2/10/2016
# Build CIs by 3 different methods

# Simulate 100 datasets with 100 datapoints each, with p = 0.5, p = 0.9, p = 0.98
# Solve for p-hat with MLE. Estimate SE(p-hat), LCI, UCI, coverage
# Solve for p-hat by using logit(theta). Estimate SE(p-hat), LCI, UCI, coverage
# Solve for p-hat with profile likelihood. estimate SE(p-hat), LCI, UCI, coverage
# Store all these answers together to compare.
# See how many of the 100 datasets have a p-hat that falls within the 95% CI

# Simulate 100 datasets. Let number of trials = 10, just because.
size <- rep(10, 100)
aa <- lapply(size, rbinom, n = 100, prob = 0.5)
bb <- lapply(size, rbinom, n = 100, prob = 0.9)
cc <- lapply(size, rbinom, n = 100, prob = 0.98)

# I'm very confused. The binomial already is a dataset. It's a single 
#  number you get that represents the number of successes from multiple trials.
#  If you pull rbinom with size = 100, that's 100 datasets and you would get a
#  p-hat for each one
#  I guess I'll pretend for now that's what I'm supposed to be doing. 
#  If that doesn't work, I'll start with the exponential instead.

# 100 binomial data sets. Each with 100 trials - this is the number of successes we got
numtrials <- 100
numdatasets <- 100
prob <- 0.5
data <- rbinom(n = numdatasets, size = numtrials, prob = prob)

# MLE
  # Estimate p-hat, using MLE
  p_hat <- data/numtrials
  
  # Calculate SE(p-hat) (estimate SD)
  SE_p_hat <- sqrt((p_hat*(1-p_hat))/numtrials)
  
  # calculate LCI, UCI
  LCI <- p_hat - 1.96*SE_p_hat
  UCI <- p_hat + 1.96*SE_p_hat
  
  # calculate coverage (the number of datasets where the CI actually overlapped p)
  coverage <- sum(LCI < prob & prob < UCI)/numdatasets

# p = logistic(theta)
  # logL of binomial, as a function of theta = log(p/(1-p))
  logL <- function(data, numdatasets, theta){
    # Gives the value of the logL for any one lambda (we want the max value)
    # Takes a vector x and a single value for lambda
    data*log(1/(1 + exp(-theta))) + (numdatasets - data)*log(1-1/(1+exp(-theta)))
  }
  
  # optimize over all the datasets
  theta_hat <- numeric(length = numdatasets)
  for(i in 1:numdatasets){
    tmp <- optim(par = 1, fn = logL, data = data[i], numdatasets = numdatasets, 
                 control = list(fnscale = -1))
    theta_hat[i] <- tmp$par
  }

  #calculate SE(theta_hat)
  SE_theta_hat <- sqrt((theta_hat*(1-theta_hat))/numtrials)
# this doesn't work yet.



  # Untransform theta back to p
  p_hat <- 1/(1+exp(-theta_hat)) 
