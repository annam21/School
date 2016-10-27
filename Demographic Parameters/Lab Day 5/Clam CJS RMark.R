  # Lab Day 5
  # Anna Moeller
  # 9/29/2016
  
  # Start with RMark
  library(RMark)
  
  # You have to specify a working directory the the stinking tmp files to go in
  setwd("GitHub/School/Demographic Parameters/Lab Day 5")
  
  # Define what the groups are
  groups <- data.frame(treatment = c("treatment", "control")) 
  
  # Read in inp and convert to data.frame
  clam <- convert.inp("clams.inp", group.df = groups, covariates = NULL, use.comments = F)
  
  # Process data (turn into a list)
  clam.proc <- process.data(clam, model = "CJS", groups = "treatment")
  
  # Make design data for RMark
  # This is what we actually use from here on out
  clam.ddl <- make.design.data(clam.proc)
  
  # Add covariates to design data for Phi for effect and lag effect
  clam.ddl$Phi$acute <- 0
  clam.ddl$Phi$acute[clam.ddl$Phi$time %in% c(2, 6) & clam.ddl$Phi$treatment == "treatment"] <- 1
  
  clam.ddl$Phi$chronic <- 0
  clam.ddl$Phi$chronic[clam.ddl$Phi$time %in% c(2, 3, 6, 7) & clam.ddl$Phi$treatment == "treatment"] <- 1
  
  # Define model formulae
  #  Phi
  phidot <- list(formula = ~1)
  phiacute <- list(formula = ~acute)
  phichronic <- list(formula = ~chronic)
  
  # p
  pdot <- list(formula = ~1)
  pTime <- list(formula = ~Time) # linear trend
  ptime <- list(formula = ~time) # random time-varying
  
  # Define models to run
  run.clam <- function() {
    clam.phidot.pdot <- mark(clam.proc, clam.ddl,
                             model.parameters = list(Phi = phidot, p = pdot),
                             delete = F, output = T)
    clam.phiacute.pdot <- mark(clam.proc, clam.ddl,
                               model.parameters = list(Phi = phiacute, p = pdot),
                               delete = T, output = F)
    clam.phichronic.pdot <- mark(clam.proc, clam.ddl,
                               model.parameters = list(Phi = phichronic, p = pdot),
                               delete = T, output = F)
    clam.phidot.pTime <- mark(clam.proc, clam.ddl,
                             model.parameters = list(Phi = phidot, p = pTime),
                             delete = T, output = F)
    clam.phiacute.pTime <- mark(clam.proc, clam.ddl,
                               model.parameters = list(Phi = phiacute, p = pTime),
                               delete = T, output = F)
    clam.phichronic.pTime <- mark(clam.proc, clam.ddl,
                                 model.parameters = list(Phi = phichronic, p = pTime),
                                 delete = T, output = F)
    clam.phidot.ptime <- mark(clam.proc, clam.ddl,
                              model.parameters = list(Phi = phidot, p = ptime),
                              delete = T, output = F)
    clam.phiacute.ptime <- mark(clam.proc, clam.ddl,
                                model.parameters = list(Phi = phiacute, p = ptime),
                                delete = T, output = F)
    clam.phichronic.ptime <- mark(clam.proc, clam.ddl,
                                  model.parameters = list(Phi = phichronic, p = ptime),
                                  delete = T, output = F)
    
    # Return the table that puts AIC, model weight, etc. together 
    return(collect.models() )
  }
  
  # Run models
  clam.mod <- run.clam()
  clam.mod$model.table
  
  # GOF
  release.gof(clam.proc)
  
  # You can only look at the output if delete = F
  
  