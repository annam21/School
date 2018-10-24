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
  D <- N/A
  lambda <- D * a
  toevent <- matrix(rexp(nocc, rate = lambda), ncol = nocc )
  toevent[toevent > censor] <- NA
  
  is.censored <- matrix(rep(0, length(toevent)), nrow = 1)
  is.censored[is.na(toevent)] <- 1   
  dat.ste <- list(toevent = toevent, 
                  is.censored = is.censored,
                  nrows = nrow(toevent),
                  ncols = ncol(toevent),
                  censor = censor,
                  A = A,
                  a = a)
  
  
  # Initial values for JAGS
  inits <- function(){
    list(beta0 = runif(1, -3, 3))
  }
  
  # Parameters to monitor in JAGS
  params <- c("beta0", "lambda", "N")
  
  # MCMC specifications
  ni <- 10000
  nt <- 1
  nb <- 5000
  nc <- 3
  
  # Call JAGS
  res <- jags( dat.ste, 
               inits,
               params,
               "",  ############################################################################
               n.chains=nc, 
               n.iter=ni, 
               n.burnin=nb,
               n.thin=nt
  )
  
  # Look at results
  res
  mcmcplot(res)
  
  estN <- res$BUGSoutput$mean$N
  
  # Variance
  sd <- res$BUGSoutput$sd$N
  CRI <- quantile(res$BUGSoutput$sims.list$N, probs = c(.025, .975))
  
  
  ##########################################
  # Time-to-Event
  ncam <- 4
  nocc <- 5
  nper <- 4 # (number of sampling periods per occasion = max observable TTE)
  dat.tte <- list(toevent = matrix(c(NA, NA, 1, NA, NA, NA, NA, 4, NA, 2, 1, NA,
                                     NA, NA, NA, NA, NA, 3, NA, 2), ncol = nocc),
                  censor = nper + 1,
                  P = P,
                  rows = ncam, 
                  cols = nocc) 
  dat.tte$is.censored <- matrix(rep(0, length(dat.tte$toevent)), ncol = nocc)
  dat.tte$is.censored[is.na(dat.tte$toevent)] <- 1
  
  # Call jags (left everything same as above)
  res.tte <- jags( dat.tte, 
                   inits,
                   params,
                   "GitHub/Time2Event_Abundance/Bayesian exponential likelihood.txt",
                   n.chains=nc, 
                   n.iter=ni, 
                   n.burnin=nb,
                   n.thin=nt
  )
  
  # Look at results
  res.tte
