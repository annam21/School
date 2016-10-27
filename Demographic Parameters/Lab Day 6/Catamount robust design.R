  # Lab Day 6
  # Anna Moeller
  # 10/13/2016
  
  # Load packages
  library(RMark)
  library(dplyr)
  
# Robust design - Catamount
  
  # Read in inp and convert to data.frame
  # No groups to define
  pike <- convert.inp("GitHub/School/Demographic Parameters/Lab Day 6/catamount.inp", 
                      group.df = NULL, covariates = "len", use.comments = F)
 
  # Process data (turn into a list)
  pike.proc <- process.data(pike, model = "RDHuggins", 
                            time.intervals = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 0))
  
  # Make design data for RMark
  pike.ddl <- make.design.data(pike.proc)
  
  # Open PIM chart and PIM matrices
### ?????
  
  # Add a covariate for the removal treatment in S
  pike.ddl$S <- mutate(pike.ddl$S, rem = ifelse(time == 4, 1, 0))

  # Define model formulae
  # p and c
  pcdot <- list(formula = ~1, share = T) # both constant and equal
  pctime <- list(formula = ~time, share = T) # both equal and time-varying
  pPlusc <- list(formula = ~1 + c, share = T) # both constant but unequal
  
### I really have no idea if these ones are right
  pctimePluslen <- list(formula = ~time + len, share = T)  
  pPlusclen <- list(formula = ~len + c, share = T)
  
  # S
  Sdot <- list(formula = ~1)
  Stime <- list(formula = ~time)
  Sremoval <- list(formula = ~rem)
  
  # Gammas
  gammasdot <- list(formula = ~1, share = T)
  gammaplusgamma <- list(formala = ~1 + GammaDoublePrime, share = T)
  
  # Define Mark specs
  run.pike <- function(p, S, gammas) { 
    mark(
      pike.proc, 
      pike.ddl,
      model.parameters = list(p = p,
                              S = S,
                              GammaPrime = gammas),     
      model = "RDHuggins",
      output = F,    
      delete = T
    )
  }
  
  # Define models to run
  pike.mod <- function() {
    pcdot.Sdot.gammasdot <- run.pike(pcdot, Sdot, gammasdot)
    pctime.Sdot.gammasdot <- run.pike(pctime, Sdot, gammasdot)
    pPlusc.Sdot.gammasdot <- run.pike(pPlusc, Sdot, gammasdot)
    pctimePluslen.Sdot.gammasdot <- run.pike(pctimePluslen, Sdot, gammasdot)
    pPlusclen.Sdot.gammasdot <- run.pike(pPlusclen, Sdot, gammasdot)

    pcdot.Stime.gammasdot <- run.pike(pcdot, Stime, gammasdot)
    pctime.Stime.gammasdot <- run.pike(pctime, Stime, gammasdot)
    pPlusc.Stime.gammasdot <- run.pike(pPlusc, Stime, gammasdot)
    pctimePluslen.Stime.gammasdot <- run.pike(pctimePluslen, Stime, gammasdot)
    pPlusclen.Stime.gammasdot <- run.pike(pPlusclen, Stime, gammasdot)
    
    pcdot <- run.pike(pcdot, Srem, gammasdot)
    pctime.Srem.gammasdot <- run.pike(pctime, Srem, gammasdot)
    pPlusc.Srem.gammasdot <- run.pike(pPlusc, Srem, gammasdot)
    pctimePluslen.Srem.gammasdot <- run.pike(pctimePluslen, Srem, gammasdot)
    pPlusclen.Srem.gammasdot <- run.pike(pPlusclen, Srem, gammasdot)
    
    # ... And all the different gammas. But these don't work. 
### NOPE. Don't work. 
    
    return(collect.models())
  }
  
  # Results
  pike.res <- pike.mod()
  pike.res
  
  # Top model
  summary(pike.res$dot)
  pike.res$dot$results$derived$N
  
  # Run example of what happens if you don't constrain p
  # Example of what happens if you don't constrain p
  ct <- list(formula = ~1)
  pt <- list(formula = ~time)
  
  pcbad <- mark(pike.proc, pike.ddl, model.parameters = list(p = pt, c= ct),     
    model = "Huggins", output = T, delete = T
  )
  summary(pcbad)
  pcbad$results$derived
  names(pcbad$results)
  pcbad$results$real
  