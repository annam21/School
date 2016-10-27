  # Lab Day 6
  # Anna Moeller
  # 10/13/2016
  
  # Load packages
  library(RMark)
  
# Closed population model - Crawford
  
  # Read in inp and convert to data.frame
  # No groups to define
  pike <- convert.inp("GitHub/School/Demographic Parameters/Lab Day 6/Crawford.inp", 
                      group.df = NULL, covariates = "len", use.comments = F)
  
  # Process data (turn into a list)
  pike.proc <- process.data(pike, model = "Huggins")
  
  # Make design data for RMark
  pike.ddl <- make.design.data(pike.proc)
  
  # Define model formulae
  # Share = T, can share parameters across both p and c
  pcdot <- list(formula = ~1, share = T) # both constant and equal
  pctime <- list(formula = ~time, share = T) # both equal and time-varying
  pclen <- list(formula = ~len, share = T) # both equal but individual-varying
  pPlusc <- list(formula = ~1 + c, share = T) # both constant but unequal
  # In the 2-occasion case, pctime = pPlusc so these are redundant
 
   # Define Mark specs
  run.pike <- function(p) { 
    mark(
      pike.proc, 
      pike.ddl,
      model.parameters = list(p = p),     
      model = "Huggins",
      output = F,    
      delete = T
    )
  }
  
  # Define models to run
  pike.mod <- function() {
    dot <- run.pike(pcdot)
    # time <- run.pike(pctime) # This one is redundant in the 2-occasion case
    len <- run.pike(pclen)
    pPlusc <- run.pike(pPlusc)

    return(collect.models())
  }
  
  # Results
  pike.res <- pike.mod()
  pike.res
  
  # Top model
  summary(pike.res$dot)
  
  # Get an estimate of N
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
  