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

  #####
  # Put together data 
  dat.ste <- list(toevent = toevent, 
                  is.censored = is.censored,
                  nrows = nrow(toevent),
                  ncols = ncol(toevent),
                  censor = censor,
                  A = A)
  
  # Initial values for JAGS
  inits <- function(){
    list(beta0 = runif(1, -3, 3))
  }
  
  # Parameters to monitor in JAGS
  params <- c("beta0", "lambda", "N")
  
  # MCMC specifications
  ni <- 10000
  nt <- 1
  nb <- 2000
  nc <- 3
  
  # Call JAGS
  res <- jags( dat.ste, 
               inits,
               params,
               "Demographic Parameters/Lecture STE IS/Bayesian exponential likelihood2.txt",  
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
  
  ######################################################
  # Instantaneous Sampling 

  # Create IS encounter history and run model
  is_estN_fn <- function(cam_occ_EH, steps_btw_samples, A){
    
    # For IS, count all the animals at given times
    is_eh <- cam_occ_EH %>%
      filter(step %% steps_btw_samples == 0) 
    
    is_est <- is_eh %>%
      mutate(dens_ij = nanimals/a) %>%
      summarise(D = mean(dens_ij) ) %>%
      mutate(N = D * A)
    
    return(is_est)
  }
  
  # IS bootstrap
  is_boot_fn <- function(cam_occ_EH, steps_btw_samples, A, nboot){
    
    bootN <- rep(NA, nboot)
    for(i in 1:nboot){
      # Sample cams with replacement (thus spread then gather): make a new eh
      bootdf <- cam_occ_EH %>% 
        spread(step, nanimals) %>%
        sample_n(size = nrow(.), replace = T) %>%
        gather(step, nanimals, -cam, -a) %>%
        mutate(step = as.numeric(step))
      
      # Estimate abundance on it
      bootN[i] <- is_estN_fn(bootdf, steps_btw_samples, A)$N
    }
    
    SE_is_estN <- sd(bootN)
    CI_is_estN <- quantile(bootN, c(0.025, 0.975))
    
    return(list(SE_is_estN = SE_is_estN, 
                CI_is_estN = CI_is_estN) )
  }
  
  # Proposed variance estimator by reviewer (Fewster 2009)
  # Create IS encounter history and run model
  is_estN_Fewster_var_fn <- function(cam_occ_EH, steps_btw_samples, A){
    
    # For IS, count all the animals at given times
    is_eh <- cam_occ_EH %>%
      filter(step %% steps_btw_samples == 0) # Sampling every len_occ steps
    
    Jai_ni <- is_eh %>%
      group_by(cam) %>%
      summarise(Jai = sum(a),
                ni = sum(nanimals))
    
    n_L <- Jai_ni %>%
      summarise(n = sum(ni),
                L = sum(Jai))
    
    M <- length(unique(Jai_ni$cam))
    L <- n_L$L
    n <- n_L$n
    Jai <- Jai_ni$Jai
    ni <- Jai_ni$ni
    
    varD <- M / L^2 / (M-1) * sum(Jai^2  * (ni/Jai - n/L)^2)
    form <- sprintf("~ %f * x1", A)
    
    is_est <- is_eh %>%
      mutate(dens_ij = nanimals/a) %>%
      summarise(D = mean(dens_ij) ) %>%
      mutate(N = D * A,
             varD = varD,
             SE_N = deltamethod(as.formula(form), D, varD))
    
    return(is_est)
  }
  
  # Call the functions
  library(tidyverse)
  library(msm)
  dat.is <- readRDS("Demographic Parameters/Lecture STE IS/is_EH.rds")
  
  is_estN_fn(dat.is, steps_btw_samples = 10, A = 900)
  is_boot_fn(dat.is, steps_btw_samples = 10, A = 900, nboot = 100)
  is_estN_Fewster_var_fn(dat.is, steps_btw_samples = 10, A = 900)
  
  
  ##########################################
  # Time-to-Event
  dat.tte <- readRDS("Demographic Parameters/Lecture STE IS/tte_EH.rds")
  
  
  dat.tte$is.censored <- matrix(0, nrow = nrow(dat.tte$toevent), ncol = ncol(dat.tte$toevent))
  dat.tte$is.censored[is.na(dat.tte$toevent)] <- 1
  dat.tte$nrows <- nrow(dat.tte$toevent)
  dat.tte$ncols <- ncol(dat.tte$toevent)
  
  # Call jags (left everything same as above)
  res.tte <- jags( dat.tte, 
                   inits,
                   params,
                   "Demographic Parameters/Lecture STE IS/Bayesian exponential likelihood2.txt",  
                   n.chains=nc, 
                   n.iter=ni, 
                   n.burnin=nb,
                   n.thin=nt
  )
  
  # Look at results
  res.tte
