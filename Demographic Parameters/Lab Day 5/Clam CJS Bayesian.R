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
  clam <- read.csv("GitHub/School/Demographic Parameters/Lab Day 5/clam_data.csv" )
  
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
  