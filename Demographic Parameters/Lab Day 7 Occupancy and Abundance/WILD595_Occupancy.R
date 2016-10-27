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
  
  nOcc <- 4                         # number of occasions
  nSite <- 100                      # number of sites
  truePsi <- 0.6                    # true value of occupancy
  truep <- runif( 4, 0.2, 0.3 )     # true value of detection probability
  
  EH <- matrix( 0, nSite, nOcc )    # Encounter history
  
  for( i in 1:nSite ){              # loop over sites  
    if( runif(1) < truePsi ){       # test for occupied
      for( j in 1:nOcc ){           # loop over occasions
        if( runif(1) < truep[j] ){  # test for detection
          EH[i,j] <- 1              # update encounter history
        }
      }
    }
  }
  
  #################################################################################
  # Analysis in JAGS
  #################################################################################
  
  
  # data to send to JAGS
  occ.data <- list( y=EH, nSite=nSite, nOcc=nOcc )
  
  # initial values for JAGS
  occ.inits <- function(){
    list( z = rep( 1, nSite ),
          b0.psi = runif( 1, -3, 3 ),
          b0.p = runif( 1, -3, 3 )
        )
  }
  
  # set parameters to track in JAGS
  occ.parms <- c( "b0.psi", "b0.p","mean.psi", "mean.p" )
  
  # set up for MCMC run
  ni <- 5000
  nt <- 1
  nb <- 500
  nc <- 3
  
  # run the MCMC chain in JAGS
  occ.result <- jags( occ.data, 
                      occ.inits,
                      occ.parms,
                      "occ_pdot.txt",
                      n.chains=nc, 
                      n.iter=ni, 
                      n.burnin=nb,
                      n.thin=nt
                    )
  occ.result
  mcmcplot( occ.result )