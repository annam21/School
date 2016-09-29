# Anna Moeller
# 2/10/2016
# Build CIs by 3 different methods

# Simulate 100 datasets with 100 datapoints each, with p = 0.5, p = 0.9, p = 0.98
# Solve for p-hat with MLE. Estimate SE(p-hat), LCI, UCI, coverage
# Solve for p-hat by using logit(theta). Estimate SE(p-hat), LCI, UCI, coverage
# Solve for p-hat with profile likelihood. estimate SE(p-hat), LCI, UCI, coverage
# Store all these answers together to compare.
# See how many of the 100 datasets have a p-hat that falls within the 95% CI

# Version 1: MLE
mle.fn <- function(data, prob, numtrials, numdatasets){
  # Estimate p-hat, using MLE
  p_hat <- data/numtrials
  
  # Calculate SE(p-hat) (estimate SD)
  SE_p_hat <- sqrt((p_hat*(1-p_hat))/numtrials)
  
  # calculate LCI, UCI
  LCI <- p_hat - 1.96*SE_p_hat
  UCI <- p_hat + 1.96*SE_p_hat
  
  # calculate coverage (the number of datasets where the CI actually overlapped p)
  coverage <- sum(LCI < prob & prob < UCI)/numdatasets
  
  # Store everything in a list
  out <- list(p_hat = p_hat, 
              SE_p_hat = SE_p_hat, 
              LCI = LCI, 
              UCI = UCI, 
              coverage = coverage)
}

# Version 2: Logistic transformation for MLE, with Delta Method
transf.fn <- function(data, prob, numtrials, numdatasets){

  # p = logistic(theta)) = 1/(1 + exp(-theta))
  # L[p|y, n] = p^y * (1-p)^(n-y)  
  # L[theta|y, n] = (1/(1 + exp(-theta)))^y * (1-(1/(1 + exp(-theta))))^(n-y)
  
  # logL[theta|y, n]
  logL <- function(data, numtrials, theta){
    data*log(1/(1 + exp(-theta))) + (numtrials - data)*log(1-(1/(1 + exp(-theta))))
  }
  
  # optimize logL 
  tmp <- lapply(data, function(x){
    optim(par = .5, fn = logL, data = x, 
          numtrials = numtrials, control = list(fnscale = -1), hessian = T)
  })

  theta_hat <- sapply(tmp, function(x){
    x$par
  })

  # Get back to p
  p_hat <- 1/(1 + exp(-theta_hat))
  
  # Calculate var(theta_hat)
  var_theta_hat <- sapply(tmp, function(x){
    -1/x$hessian
  })
  # more generally, for larger than 1x1 matrix: -ginv(tmp$hessian) # MASS()
  ### For prob = 0.98, the hessian is 0, so var_theta_hat = 1/0 = DNE

  # Calculate SE(p_hat) from Delta Method
  SE_p_hat <- exp(theta_hat)/((exp(theta_hat) + 1)^2) * sqrt(var_theta_hat)

  # Calculate CIs from SE(p_hat)
  LCI <- p_hat - 1.96*SE_p_hat
  UCI <- p_hat + 1.96*SE_p_hat
  LCI[LCI == -Inf] <- NA
  UCI[UCI == Inf] <- NA
  
  # calculate coverage (the number of datasets where the CI actually overlapped p)
  coverage <- sum(LCI < prob & prob < UCI, na.rm = T)/numdatasets
  
  # Store everything in a list
  out <- list(p_hat = p_hat, 
              SE_p_hat = SE_p_hat, 
              LCI = LCI, 
              UCI = UCI, 
              coverage = coverage)
}

# Version 3: Profile likelihood
pi.fn <- function(data, prob, numtrials, numdatasets){
  
  # logL[p|y, n]
  logL <- function(data, numtrials, p){
    data*log(p) + (numtrials - data)*log(1-p)
  }
  
  # optimize logL 
  tmp <- lapply(data, function(x){
    optim(par = .5, fn = logL, data = x, 
          numtrials = numtrials, control = list(fnscale = -1), hessian = F)
  })
  # Will not work for prob <- 0.98 with hessian = T
  
  p_hat <- sapply(tmp, function(x){
    x$par
  })
  
  # value of logL(p_hat)
  logLval <- sapply(tmp, function(x){
    x$value
  })
  
  # Solve for the p that satisfies this equation = 0
  tosolve <- function(data, numtrials, logLval, p){
    -2*(data*log(p) + (numtrials - data)*log(1-p)) + 2*logLval - 1.92
  }
  
#   curve(expr = -2*(data[1]*log(x) + (numtrials - data[1])*log(1-x)) + 
#             2*logLval[1] - 1.92, from = 0, to = 1, n = 201)
  
# # troubleshooting
# # Calculate the deviance value of p_hat for each dataset
#   chk <- NA
#   for(i in 1:length(data)){
#     chk[i] <- tosolve(data[i], numtrials, logLval[i], p_hat[i])
#   }
#   # Okay, good these are all negative numbers v close to -1.92
#   
# # Calculate the deviance value of 0.99999 for each dataset
#   chk2 <- NA
#   for(i in 1:length(data)){
#     chk2[i] <- tosolve(data[i], numtrials, logLval[i], 0.99999)
#   }
#   # These should all be positive. However, some are exactly -1.918002.
#   # data[chk2 < 0]
#   # It's happening where data == 100, so p_hat is 1, so when we use uniroot
#   #   the interval is actually backward
    
  # Find roots using uniroot to find LPI and UPI
  # I'm not sure what to do with the p_hat = 1, so we will assign UPI = 1
  #   for those
  root1 <- NA
  root2 <- NA
  for(i in 1:length(data)){
    root1[i] <- uniroot(f = tosolve, interval = c(0.0001, p_hat[i]), 
                        data = data[i], numtrials = numtrials, logLval = logLval[i])
  }
  for(i in 1:length(data)){
    if(data[i] == 100){
      root2[i] <- 1
    } else {
      root2[i] <- uniroot(f = tosolve, interval = c(p_hat[i], 0.9999), 
                          data = data[i], numtrials = numtrials, logLval = logLval[i])
    }
  }
  LPI <- unlist(root1)
  UPI <- unlist(root2)

  # calculate coverage (the number of datasets where the CI actually overlapped p)
  coverage <- sum(LPI < prob & prob < UPI)/numdatasets
  
  # Store everything in a list
  out <- list(p_hat = p_hat, 
              LPI = LPI, 
              UPI = UPI, 
              coverage = coverage)
}

# Version 4: Like version 2, but without delta method
transf2.fn <- function(data, prob, numtrials, numdatasets){
  
  # p = logistic(theta)) = 1/(1 + exp(-theta))
  # L[p|y, n] = p^y * (1-p)^(n-y)  
  # L[theta|y, n] = (1/(1 + exp(-theta)))^y * (1-(1/(1 + exp(-theta))))^(n-y)
  
  # logL[theta|y, n]
  logL <- function(data, numtrials, theta){
    data*log(1/(1 + exp(-theta))) + (numtrials - data)*log(1-(1/(1 + exp(-theta))))
  }
  
  # optimize logL 
  tmp <- lapply(data, function(x){
    optim(par = .5, fn = logL, data = x, 
          numtrials = numtrials, control = list(fnscale = -1), hessian = T)
  })
  
  theta_hat <- sapply(tmp, function(x){
    x$par
  })
  
  # Calculate var(theta_hat)
  var_theta_hat <- sapply(tmp, function(x){
    -1/x$hessian
  })
  # more generally, for larger than 1x1 matrix: -ginv(tmp$hessian) # MASS()
  ### For prob = 0.98, the hessian is 0, so var_theta_hat = 1/0 = DNE
  
  SE_theta_hat <- sqrt(var_theta_hat)
  
  # Calculate CIs from SE(theta_hat)
  LCI_theta <- theta_hat - 1.96*SE_theta_hat
  UCI_theta <- theta_hat + 1.96*SE_theta_hat
  
  # Get back to p
  p_hat <- 1/(1 + exp(-theta_hat))
  LCI <- 1/(1 + exp(-LCI_theta))
  UCI <- 1/(1 + exp(-UCI_theta))
  
  # calculate coverage (the number of datasets where the CI actually overlapped p)
  coverage <- sum(LCI < prob & prob < UCI, na.rm = T)/numdatasets
  
  # Store everything in a list
  out <- list(p_hat = p_hat, 
              SE_p_hat = NA, 
              LCI = LCI, 
              UCI = UCI, 
              coverage = coverage)
}


###########################################################################

# 100 binomial data sets. Each with 100 trials - this is the number of successes we got
numtrials <- 100
numdatasets <- 100
data0.5 <- rbinom(n = numdatasets, size = numtrials, prob = 0.5)
data0.9 <- rbinom(n = numdatasets, size = numtrials, prob = 0.9)
data0.98 <- rbinom(n = numdatasets, size = numtrials, prob = 0.98)

# Call them all
mle0.5 <- mle.fn(data0.5, prob = 0.5, numtrials, numdatasets)
mle0.9 <- mle.fn(data0.9, prob = 0.9, numtrials, numdatasets)
mle0.98 <- mle.fn(data0.98, prob = 0.98, numtrials, numdatasets)
# This also has UCIs > 1. I haven't done anything to change these...

transf0.5 <- transf.fn(data0.5, prob = 0.5, numtrials, numdatasets)
transf0.9 <- transf.fn(data0.9, prob = 0.9, numtrials, numdatasets)
transf0.98 <- transf.fn(data0.98, prob = 0.98, numtrials, numdatasets)
### prob = 0.98 has a lot of SE_p_hat = Inf
### I assigned these to have NA confidence intervals. What's the real way? 
### CIs can't overlap 1, right? A lot of these do. Do I need to fix that? 

pi0.5 <- pi.fn(data0.5, prob = 0.5, numtrials, numdatasets)
pi0.9 <- pi.fn(data0.9, prob = 0.9, numtrials, numdatasets)
pi0.98 <- pi.fn(data0.98, prob = 0.98, numtrials, numdatasets)
# prob = 0.98 will not work with hessian = T (but we don't actually need it)
# I had to assign the UPI = 1 when data = 100 (else uniroot won't work)
### Is this an appropriate fix?

transf2_0.5 <- transf2.fn(data0.5, prob = 0.5, numtrials, numdatasets)
transf2_0.9 <- transf2.fn(data0.9, prob = 0.9, numtrials, numdatasets)
transf2_0.98 <- transf2.fn(data0.98, prob = 0.98, numtrials, numdatasets)
# This has very high coverage because a lot of CIs are 0-1

# Plot p_hat and its confidence interval
library(Hmisc)

# pi.fn outputs LPI and UPI so it won't work the same for plotting
output <- transf2_0.98
# output$LCI <- output$LPI
# output$UCI <- output$UPI
prob <- 0.98

x1 <- which(output$LCI < prob & prob < output$UCI)
x2 <- which(!(output$LCI < prob & prob < output$UCI))
errbar(x = x1, y = output$p_hat[x1], yplus = output$UCI[x1], yminus = output$LCI[x1],
       xlab = "", ylab = "p-hat")
errbar(x = x2, y = output$p_hat[x2], yplus = output$UCI[x2], yminus = output$LCI[x2],
       col = "red", errbar.col = "red", add = T)
abline(a = prob, b = 0, col = "red")



############################### Other stuff from v2 ##################################
  # p = logit(theta) = log(theta/(1-theta))
  # L[p|y, n] = p^y * (1-p)^(n-y)
  # L[theta|y, n] = (log(theta/(1-theta)))^y * (1-(log(theta/(1-theta))))^(n-y)
  # logL[theta|y, n]  


#   logL <- function(data, numtrials, theta){
#     data*log((log(theta/(1-theta)))) + (numtrials-data)*log(1-(log(theta/(1-theta))))
#   }
# 
#   # optimize logL 
#   data <- rbinom(n = 1, size = numtrials, prob = prob)
# 
#   optim(.5, logL, x = data, control = list(fnscale = -1))
# 
# theta_hat <- numeric(length = numdatasets)
#   for(i in 1:numdatasets){
#     tmp <- optim(par = 1, fn = logL, data = data[i], numdatasets = numdatasets, 
#                  control = list(fnscale = -1))
#     theta_hat[i] <- tmp$par
#   }
# 

#   # Calculate SE(theta_hat) (estimate SD)
#   SE_theta_hat <- sqrt((theta_hat*(1-theta_hat))/numtrials)
#   Nope. 


# errbar(x = c(x1, x2), 
#        y = c(mle$p_hat[x1], mle$p_hat[x2]), 
#        yplus = c(mle$UCI[x1], mle$UCI[x2]),
#        yminus = c(mle$LCI[x1], mle$LCI[x2]),
#        col = c("red", "blue"))


#   # Calculate SE(p_hat|theta_hat) = SE(1/(1+exp(-theta_hat)))
#   SE_p_hat <- sqrt(((1/(1+exp(-theta_hat)))*(1-(1/(1+exp(-theta_hat)))))/numtrials)

# ### Is this supposed to be the second derivative??
#   SE_p_hat <- sqrt((exp(theta_hat)*(1-exp(theta_hat)))/(exp(theta_hat)+1)^3 * 
#                      var_theta_hat)
### First derivative, squared? 
