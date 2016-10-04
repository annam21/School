  # Clam analysis Bayesian
  # Anna Moeller
  # 9/29/2016
  
  # Try that whole dang thing in Bayesian
  # Load packages
  library(mcmcplots)
  library(R2jags)
  
################
  # Prepare data
  
  # read data file 
  clam <- read.csv("GitHub/School/Demographic Parameters/Lab Day 5/clam_data.csv")

  # Subset data to make encounter history matrix
  EH <- as.matrix(clam[, 1:9])
  
  # Define your dimensions
  #   Everything later will be a matrix with nAnimal rows and nYears columns
  nYears <- ncol( EH )
  nAnimal <- nrow( EH )
  
  # Make a vector of the first capture occasions.
  # This is used to:
  #   1. Create initial values for z
  #   2. Go into the model, so you can assign a 1 to the first capture and then 
  #      keep working from there (but not before)

  first <- apply(EH, 1, function(x){
    min(which(x != 0))
  })
  
  # We would remove animals captured in the last year but there aren't any
  any(first == nYears)
  
  # Write a function to generate initial values for z
  # z has to agree with y (NA before the animal was captured and 1 after that)
  z.init.fn <- function(ch, f){
    for( i in 1:nrow(ch) ){
      ch[i, 1:(f[i])] <- NA
      ch[i, (f[i] + 1):ncol(ch)] <- 1
    }
    return(ch)
  }
 
########################################
  # Run constant model (phidot.pdot)
  # 2 parameters: phi and p
  
  # Define a list of data to be passed to JAGS
  cjs.data <- list(y = EH, f = first, nind = nAnimal, nocc = nYears)
  
  # Specify initial values to feed to JAGS
  cjs.inits <- function(){
    list(
      z = z.init.fn(EH, first),
      b0.phi = runif(1, -3, 3),
      b0.p = runif(1, -3, 3)
    )
  }
  
  # Parameters to monitor in JAGS
  cjs.params <- c("b0.phi", "b0.p", "mean.phi", "mean.p")
  
  # MCMC specifications
  ni <- 500
  nt <- 1
  nb <- 50
  nc <- 1
  
  # run constant model in JAGS
  # logit(phi) = B0
  phidot_pdot_clam <- jags(cjs.data, 
                           cjs.inits,
                           cjs.params,
                           "GitHub/School/Demographic Parameters/Lab Day 4 Bayesian CJS/cjs_phidot_pdot.txt",
                           n.chains=nc, 
                           n.iter=ni, 
                           n.burnin=nb,
                           n.thin=nt
  )
  
  # Look at results
  phidot_pdot_clam
  mcmcplot(phidot_pdot_clam)
  
  # Save it because it took awhile to run
  save(phidot_pdot_clam, file = "GitHub/School/Demographic Parameters/Lab Day 5/Model Output/phidot_pdot_clam.RData")
  load("GitHub/School/Demographic Parameters/Lab Day 5/Model Output/phidot_pdot_clam.RData")
  
  
################################
# Run phidot.ptime
  
  # Define a list of data to be passed to JAGS
  cjs.data <- list(y = EH, f = first, nind = nAnimal, nocc = nYears)
  
  # Specify initial values to feed to JAGS
  cjs.inits <- function(){
    list(
      z = z.init.fn(EH, first),
      b0.phi = runif(1, -3, 3),
      b0.p = runif((nYears-1), -3, 3)
    )
  }
  
  # Parameters to monitor in JAGS
  cjs.params <- c("b0.phi", "b0.p", "mean.phi", "mean.p")
  
  # MCMC specifications
  ni <- 500
  nt <- 1
  nb <- 50
  nc <- 1
  
  # run constant model in JAGS
  # logit(phi) = B0
  phidot_ptime_clam <- jags(cjs.data, 
                           cjs.inits,
                           cjs.params,
                           "GitHub/School/Demographic Parameters/Lab Day 5/JAGS Models/cjs_phidot_ptime.txt",
                           n.chains=nc, 
                           n.iter=ni, 
                           n.burnin=nb,
                           n.thin=nt
  )
  
  # Look at results
  phidot_ptime_clam

#########################
# Run phidot.pTime
  
  # Define a list of data to be passed to JAGS
  cjs.data <- list(y = EH, f = first, nind = nAnimal, nocc = nYears)
  
  # Specify initial values to feed to JAGS
  cjs.inits <- function(){
    list(
      z = z.init.fn(EH, first),
      b0.phi = runif(1, -3, 3),
      b0.p = runif(1, -3, 3),
      b1.slope.p = runif(1, -3, 3)
    )
  }
  
  # Parameters to monitor in JAGS
  cjs.params <- c("b0.phi", "mean.phi", "b0.p", "b1.slope.p")
  
  # MCMC specifications
  ni <- 500
  nt <- 1
  nb <- 50
  nc <- 1
  
  # run constant model in JAGS
  # logit(phi) = B0
  phidot_pTime_clam <- jags(cjs.data, 
                            cjs.inits,
                            cjs.params,
                            "GitHub/School/Demographic Parameters/Lab Day 5/JAGS Models/cjs_phidot_plinTime.txt",
                            n.chains=nc, 
                            n.iter=ni, 
                            n.burnin=nb,
                            n.thin=nt
  )
  
  # Look at results
  phidot_pTime_clam
  
################################
# Run phiacute.pdot
  
  # Define a list of data to be passed to JAGS
  cjs.data <- list(y = EH, f = first, nind = nAnimal, nocc = nYears, trt = clam$trt)
  
  # Specify initial values to feed to JAGS
  cjs.inits <- function(){
    list(
      z = z.init.fn(EH, first),
      b0.phi = runif(1, -3, 3),
      b0.p = runif(1, -3, 3),
      b.acute.phi = runif(1, -3, 3)
    )
  }
  
  # Parameters to monitor in JAGS
  ### Remove mean.phi because there is now one for every individual and time
  cjs.params <- c("b0.phi", "b0.p", "mean.p", "b.acute.phi")
  
  # MCMC specifications
  ni <- 500
  nt <- 1
  nb <- 50
  nc <- 1
  
  # run constant model in JAGS
  # logit(phi) = B0
  phiacute_pdot_clam <- jags(cjs.data, 
                           cjs.inits,
                           cjs.params,
                           "GitHub/School/Demographic Parameters/Lab Day 5/JAGS Models/cjs_phiacute_pdot.txt",
                           n.chains=nc, 
                           n.iter=ni, 
                           n.burnin=nb,
                           n.thin=nt
  )
  
  # Look at results
  phiacute_pdot_clam
  
################################
# Run phiacute.ptime
  
  # Define a list of data to be passed to JAGS
  cjs.data <- list(y = EH, f = first, nind = nAnimal, nocc = nYears, trt = clam$trt)
  
  # Specify initial values to feed to JAGS
  cjs.inits <- function(){
    list(
      z = z.init.fn(EH, first),
      b0.phi = runif(1, -3, 3),
      b0.p = runif((nYears - 1), -3, 3),
      b.acute.phi = runif(1, -3, 3)
    )
  }
  
  # Parameters to monitor in JAGS
  ### Remove mean.phi because there is now one for every individual and time
  cjs.params <- c("b0.phi", "b0.p", "b.acute.phi")
  
  # MCMC specifications
  ni <- 500
  nt <- 1
  nb <- 50
  nc <- 1
  
  # run constant model in JAGS
  # logit(phi) = B0
  phiacute_ptime_clam <- jags(cjs.data, 
                             cjs.inits,
                             cjs.params,
                             "GitHub/School/Demographic Parameters/Lab Day 5/JAGS Models/cjs_phiacute_ptime.txt",
                             n.chains=nc, 
                             n.iter=ni, 
                             n.burnin=nb,
                             n.thin=nt
  )
  
  # Look at results
  phiacute_ptime_clam
  
################################
# Run phiacute.pTime
  
  # Define a list of data to be passed to JAGS
  cjs.data <- list(y = EH, f = first, nind = nAnimal, nocc = nYears, trt = clam$trt)
  
  # Specify initial values to feed to JAGS
  cjs.inits <- function(){
    list(
      z = z.init.fn(EH, first),
      b0.phi = runif(1, -3, 3),
      b.acute.phi = runif(1, -3, 3),
      b0.p = runif(1, -3, 3),
      b1.slope.p = runif(1, -3, 3)
    )
  }
  
  # Parameters to monitor in JAGS
  ### Remove mean.phi because there is now one for every individual and time
  cjs.params <- c("b0.phi", "b.acute.phi", "b0.p", "b1.slope.p")
  
  # MCMC specifications
  ni <- 500
  nt <- 1
  nb <- 50
  nc <- 1
  
  # run constant model in JAGS
  # logit(phi) = B0
  phiacute_pTime_clam <- jags(cjs.data, 
                             cjs.inits,
                             cjs.params,
                             "GitHub/School/Demographic Parameters/Lab Day 5/JAGS Models/cjs_phiacute_plinTime.txt",
                             n.chains=nc, 
                             n.iter=ni, 
                             n.burnin=nb,
                             n.thin=nt
  )
  
  # Look at results
  phiacute_pTime_clam
  
################################
# Run phichronic.pdot
  
  # Define a list of data to be passed to JAGS
  cjs.data <- list(y = EH, f = first, nind = nAnimal, nocc = nYears, trt = clam$trt)
  
  # Specify initial values to feed to JAGS
  cjs.inits <- function(){
    list(
      z = z.init.fn(EH, first),
      b0.phi = runif(1, -3, 3),
      b0.p = runif(1, -3, 3),
      b.chronic.phi = runif(1, -3, 3)
    )
  }
  
  # Parameters to monitor in JAGS
  ### Remove mean.phi because there is now one for every individual and time
  cjs.params <- c("b0.phi", "b0.p", "mean.p", "b.chronic.phi")
  
  # MCMC specifications
  ni <- 500
  nt <- 1
  nb <- 50
  nc <- 1
  
  # run constant model in JAGS
  # logit(phi) = B0
  phichronic_pdot_clam <- jags(cjs.data, 
                             cjs.inits,
                             cjs.params,
                             "GitHub/School/Demographic Parameters/Lab Day 5/JAGS Models/cjs_phichronic_pdot.txt",
                             n.chains=nc, 
                             n.iter=ni, 
                             n.burnin=nb,
                             n.thin=nt
  )
  
  # Look at results
  phichronic_pdot_clam
  
################################
# Run phichronic.ptime
  
  # Define a list of data to be passed to JAGS
  cjs.data <- list(y = EH, f = first, nind = nAnimal, nocc = nYears, trt = clam$trt)
  
  # Specify initial values to feed to JAGS
  cjs.inits <- function(){
    list(
      z = z.init.fn(EH, first),
      b0.phi = runif(1, -3, 3),
      b0.p = runif((nYears - 1), -3, 3),
      b.chronic.phi = runif(1, -3, 3)
    )
  }
  
  # Parameters to monitor in JAGS
  ### Remove mean.phi because there is now one for every individual and time
  cjs.params <- c("b0.phi", "b0.p", "b.chronic.phi")
  
  # MCMC specifications
  ni <- 500
  nt <- 1
  nb <- 50
  nc <- 1
  
  # run constant model in JAGS
  # logit(phi) = B0
  phichronic_ptime_clam <- jags(cjs.data, 
                              cjs.inits,
                              cjs.params,
                              "GitHub/School/Demographic Parameters/Lab Day 5/JAGS Models/cjs_phichronic_ptime.txt",
                              n.chains=nc, 
                              n.iter=ni, 
                              n.burnin=nb,
                              n.thin=nt
  )
  
  # Look at results
  phichronic_ptime_clam
  
################################
# Run phichronic.pTime
  
  # Define a list of data to be passed to JAGS
  cjs.data <- list(y = EH, f = first, nind = nAnimal, nocc = nYears, trt = clam$trt)
  
  # Specify initial values to feed to JAGS
  cjs.inits <- function(){
    list(
      z = z.init.fn(EH, first),
      b0.phi = runif(1, -3, 3),
      b.chronic.phi = runif(1, -3, 3),
      b0.p = runif(1, -3, 3),
      b1.slope.p = runif(1, -3, 3)
    )
  }
  
  # Parameters to monitor in JAGS
  ### Remove mean.phi because there is now one for every individual and time
  cjs.params <- c("b0.phi", "b.chronic.phi", "b0.p", "b1.slope.p")
  
  # MCMC specifications
  ni <- 500
  nt <- 1
  nb <- 50
  nc <- 1
  
  # run constant model in JAGS
  # logit(phi) = B0
  phichronic_pTime_clam <- jags(cjs.data, 
                              cjs.inits,
                              cjs.params,
                              "GitHub/School/Demographic Parameters/Lab Day 5/JAGS Models/cjs_phichronic_plinTime.txt",
                              n.chains=nc, 
                              n.iter=ni, 
                              n.burnin=nb,
                              n.thin=nt
  )
  
  # Look at results
  phichronic_pTime_clam
  
#################################
  # Compare models
  # models <- c("phidot_pdot_clam", "phidot_ptime_clam", "phidot_pTime_clam",
  #             "phiacute_pdot_clam", "phiacute_ptime_clam", "phiacute_pTime_clam",
  #             "phichronic_pdot_clam", "phichronic_ptime_clam", "phichronic_pTime_clam")
  # 
  source("GitHub/School/Demographic Parameters/Lab Day 5/jags_comp_fn.R")
  jags_comp_fn()
  
  # compute CRIs, etc. 
  