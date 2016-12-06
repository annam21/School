  ################################################################################
  #
  # Occupancy simulation and analysis
  #
  ################################################################################
  
  library(mcmcplots)
  library(R2jags)
  setwd("C:/Users/anna.moeller/Documents/GitHub/School/Demographic Parameters/Lab Day 7 Occupancy and Abundance")
  
  
  
  ################################################################################
  # Simulation with individual heterogeneity on p
  ################################################################################
  nOcc <- 6                        # number of occasions
  nInd <- 200                      # number of sites
  bodySize <- rnorm(nInd, 5, 2)
  trueB0 <- -2
  trueB1 <- 0.1
  truep <- matrix(0, nInd, nOcc)
  for(i in 1:nInd){
    for(j in 1:nOcc) {
      truep[i, j] <- plogis(trueB0 + trueB1*bodySize[i])
    }
  }
 
  EH <- matrix( 0, nInd, nOcc )    # Encounter history
  # Simulating N
  for (i in 1:nInd){
    for (j in 1:nOcc){
      if(runif(1) < truep[i, j]){
        EH[i, j] <- 1
      }
    }
  }
  # Chop off all the ones with no detetions
  nd <- which(apply(EH, 1, sum) > 0) # those with at least 1 detection
  EH <- EH[nd, ]
  bodySize <- bodySize[nd]
  
  #################################################################################
  # Analysis in JAGS
  #################################################################################
  
  ### Here is the only change for abundance
  nOcc <- ncol(EH)
  nInd <- nrow(EH)
  
  # Generate augmented data
  M <- matrix(0, I(4*nInd), nOcc)
  EHaug <- rbind(EH, M)
  
  # data to send to JAGS
  N.data <- list( y=EHaug, nSite=nrow(EHaug), nOcc=nOcc )
  
  # initial values for JAGS
  N.inits <- function(){
    list( #z = rep( 1, nrow(EHaug) ),
          z = c(rep(1, nInd), rep(0, nrow(M))), # Change init for z for better convergence
          b0.psi = runif( 1, -3, 3 ),
          b0.p = runif( 1, -3, 3 ),
          taue = runif(1, 0, 1)
    )
  }
  
  # set parameters to track in JAGS
  N.parms <- c( "b0.psi", "b0.p","mean.psi", "mean.p", "N", "taue")
  
  # set up for MCMC run
  ni <- 5000
  nt <- 1
  nb <- 500
  nc <- 3
  
  # run the MCMC chain in JAGS
  N.result <- jags( N.data, 
                    N.inits,
                    N.parms,
                    "abund_prandom.txt",
                    n.chains=nc, 
                    n.iter=ni, 
                    n.burnin=nb,
                    n.thin=nt
  )
  N.result
  mcmcplot( N.result )
  