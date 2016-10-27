  ################################################################################
  #
  # Occupancy simulation and analysis
  #
  ################################################################################
  
  library(mcmcplots)
  library(R2jags)
  setwd("C:/Users/anna.moeller/Documents/GitHub/School/Demographic Parameters/Lab Day 7 Occupancy and Abundance")
  
  
  
  ################################################################################
  # Simulation
  ################################################################################
  
  EH <- as.matrix(read.csv("closedCR.csv"))
  
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
    list( z = rep( 1, nrow(EHaug) ),
          b0.psi = runif( 1, -3, 3 ),
          b0.p = runif( 1, -3, 3 )
    )
  }
  
  # set parameters to track in JAGS
  N.parms <- c( "b0.psi", "b0.p","mean.psi", "mean.p", "N")
  
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
  