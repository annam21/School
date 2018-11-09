  # Bayesian STE/TTE
  # Anna Moeller
  # 7/25/2017
  
  library(R2jags)
  library(mcmcplots)
  
  ##########################################
  # Space-to-Event
  
  # simulate data
  ncam <- 100
  nocc <- 1000
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
  dat.ste <- list(
    toevent = toevent,
    is.censored = is.censored,
    nr = nrow(toevent),
    nc = ncol(toevent),
    censor = censor,
    A = A
  )
  
  # Initial values
  inits <- function(){
    list(beta0 = runif(1, -3, 3))
  }
  
  # Parameters to monitor
  params <- c("beta0", "N", "lambda")
  
  # Specs
  ni <- 30000
  nc <- 3
  nb <- 10000
  nt <- 1
  
  # Jags call 
  est <- jags(
    dat.ste, 
    inits,
    params,
    "Demographic Parameters/Lecture STE IS/Class exponential model.txt",  
    n.chains=nc, 
    n.iter=ni, 
    n.burnin=nb,
    n.thin=nt
  )
  
  est
  mcmcplot(est)

 