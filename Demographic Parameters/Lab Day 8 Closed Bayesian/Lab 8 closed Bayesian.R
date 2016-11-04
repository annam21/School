  # Lab Day 8 trying more Bayesian closed models
  # Anna Moeller
  # 11/3/2016
  
  # Linear models to test
  # pdot = cdot ... constant and equal
  p[i, t] <- mean.p
  
  # pdot + c ... Behavioral response
  p[i, t] <- mean.p + b.c * (t > f[i])
  
  # ptime = ctime ... time-varying, no behavioral
  p[i, t] <- mean.p[t]
  
  # ptime + c ... time-varying, behavioral response
  p[i, t] <- mean.p[t] + b.c*(t > f[i])
  
  # Load packages
  library(mcmcplots)
  library(R2jags)
 
  # Set working directory
  setwd("C:/Users/anna.moeller/Documents/GitHub/School/Demographic Parameters/Lab Day 8 Closed Bayesian")
  
  # Read in data
  dat <- read.csv("closedData.csv")
  EH <- as.matrix(dat)
  
  # More data stuff
  nOcc <- ncol(EH)
  nInd <- nrow(EH)
  
  # Generate augmented data
  M <- matrix(0, I(4*nInd), nOcc)
  EHaug <- rbind(EH, M)
  
  # In some models we need to know whether it's a capture or recapture
  get.first <- function(x) {
    # a function to identify the time period of the first capture from an encounter history matrix
    return(min(which(x != 0)))
  }
  
#########################
# pdot = cdot model
  
  # data to send to JAGS
  N.data <- list( y = EHaug, 
                  nSite = nrow(EHaug), 
                  nOcc = nOcc)
  
  # initial values for JAGS
  N.inits <- function(){
    list( z = rep( 1, nrow(EHaug) ),
          b0.psi = runif( 1, -3, 3 ),
          b0.p = runif( 1, -3, 3 )
    )
  }
  
  # set parameters to track in JAGS
  N.parms <- c( "b0.psi", "b0.p", "mean.psi", "mean.p", "N")
  
  # set up for MCMC run
  ni <- 5000
  nt <- 1
  nb <- 500
  nc <- 3
  
  # run the MCMC chain in JAGS
  N.result <- jags( N.data, 
                    N.inits,
                    N.parms,
                    "abund_pdot.txt",
                    n.chains=nc, 
                    n.iter=ni, 
                    n.burnin=nb,
                    n.thin=nt
  )
  
  N.result
  mcmcplot( N.result )
  
##########################
  # pdot + c (behavioral)
  
  fall <- apply(EHaug, 1, get.first)
  
  # data to send to JAGS (same)
  N.data <- list( y = EHaug, 
                  nSite = nrow(EHaug), 
                  nOcc = nOcc, 
                  f = fall)
  
  # initial values for JAGS
  N.inits <- function(){
    list( z = rep( 1, nrow(EHaug) ),
          b0.psi = runif( 1, -3, 3 ),
          b0.p = runif( 1, -3, 3 ),
          b0.c = runif(1, -3, 3)
    )
  }
  
  # set parameters to track in JAGS
  N.parms <- c( "b0.psi", "b0.p", "b0.c", "mean.psi", "mean.p", "N")
  
  # set up for MCMC run
  ni <- 5000
  nt <- 1
  nb <- 500
  nc <- 3
  
  # run the MCMC chain in JAGS
  N.result <- jags( N.data, 
                    N.inits,
                    N.parms,
                    "abund_pdotPlusc.txt",
                    n.chains=nc, 
                    n.iter=ni, 
                    n.burnin=nb,
                    n.thin=nt
  )
  
  N.result
  mcmcplot( N.result )
  
########################
  # ptime = ctime
  
  # data to send to JAGS (same)
  N.data <- list( y = EHaug, 
                  nSite = nrow(EHaug), 
                  nOcc = nOcc)
  
  # initial values for JAGS
  N.inits <- function(){
    list( z = rep( 1, nrow(EHaug) ),
          b0.psi = runif( 1, -3, 3 ),
          b0.p = runif( nOcc, -3, 3 )
    )
  }
  
  # set parameters to track in JAGS
  N.parms <- c( "b0.psi", "b0.p", "mean.psi", "mean.p", "N")
  
  # set up for MCMC run
  ni <- 5000
  nt <- 1
  nb <- 500
  nc <- 3
  
  # run the MCMC chain in JAGS
  N.result <- jags( N.data, 
                    N.inits,
                    N.parms,
                    "abund_ptime.txt",
                    n.chains=nc, 
                    n.iter=ni, 
                    n.burnin=nb,
                    n.thin=nt
  )
  
  N.result
  mcmcplot( N.result )
  
#####################
# ptime + c
 
  fall <- apply(EHaug, 1, get.first)
  
  # data to send to JAGS (same)
  N.data <- list( y = EHaug, 
                  nSite = nrow(EHaug), 
                  nOcc = nOcc, 
                  f = fall)
  
  # initial values for JAGS
  N.inits <- function(){
    list( z = rep( 1, nrow(EHaug) ),
          b0.psi = runif( 1, -3, 3 ),
          b0.p = runif( nOcc, -3, 3 ),
          b0.c = runif(1, -3, 3)
    )
  }
  
  # set parameters to track in JAGS
  N.parms <- c( "b0.psi", "b0.p", "b0.c", "mean.psi", "mean.p", "N")
  
  # set up for MCMC run
  ni <- 5000
  nt <- 1
  nb <- 500
  nc <- 3
  
  # run the MCMC chain in JAGS
  N.result <- jags( N.data, 
                    N.inits,
                    N.parms,
                    "abund_ptimePlusc.txt",
                    n.chains=nc, 
                    n.iter=ni, 
                    n.burnin=nb,
                    n.thin=nt
  )
  
  N.result
  mcmcplot( N.result )
  
  
#####################################
# 