  # Bayesian STE/TTE
  # Anna Moeller
  # 7/25/2017
  
  library(R2jags)
  library(mcmcplots)
  
  ##########################################
  # Space-to-Event
  
  # simulate data
  ncam <- 10
  nocc <- 10
  N <- 100
  A <- 1000 * 1000 * 10 # 10 km2, written in m2
  a <- 50 # square meters
  censor <- ncam * a
  lambda <- N/A 
  set.seed(101)
  toevent <- matrix(rexp(nocc, rate = lambda), ncol = nocc )
  toevent[toevent > censor] <- NA
  
  is.censored <- matrix(0, nrow = nrow(toevent), ncol = ncol(toevent))
  is.censored[is.na(toevent)] <- 1  
  
  # Build model
  
  # Put together data and run model 
  
  
  
  ##################################################
  # Time to Event
  
 