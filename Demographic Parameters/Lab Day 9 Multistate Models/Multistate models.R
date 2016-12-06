  # Lab Day 9 Multistate Models
  # Anna Moeller 
  # 12/1/2016
  
  # Start with RMark
  library(RMark)
  
  # You have to specify a working directory the the stinking tmp files to go in
  setwd("C:/Users/anna.moeller/Documents/GitHub/School/Demographic Parameters/Lab Day 9 Multistate Models")
  
  # Read in inp and convert to data.frame
  dat <- convert.inp("MSSURV.inp", group.df = NULL , covariates = NULL, use.comments = F)
  
  # Process data (turn into a list)
  dat.proc <- process.data(dat, model = "Multistrata")
  
  # Make design data
  dat.ddl <- make.design.data(dat.proc)
  
  # For funsies, add a covariate that says A->B= B->A
### This doesn't seem right. 
  dat.ddl$Psi$AB <- 0
  dat.ddl$Psi$AB[dat.ddl$Psi$stratum == "A" & dat.ddl$Psi$tostratum == "B"] <- 1
  dat.ddl$Psi$AB[dat.ddl$Psi$stratum == "B" & dat.ddl$Psi$tostratum == "A"] <- 1
  
  # Define model formulae
  # S
  Sdot <- list(formula = ~1, link = "sin")
  Stime <- list(formula = ~time)
  
  # p
  pdot <- list(formula = ~1, link = "sin")
  ptime <- list(formula = ~time) 
  
  # Psi 
  ### USE mlogit link on all Psi (because it's multinomial)
  Psidot <- list(formula = ~1, link = "mlogit")
  Psitime <- list(formula = ~time, link = "mlogit") 
  PsiAB <- list(formula = ~AB, link = "mlogit")
  Psistrat <- list(formula = ~stratum, link = "mlogit") # Rate is same for all transitions from A, from B...
    # This can also be coded as formula = ~ -1+stratum 
  
  # Define RMark specs
  run.mod <- function(S, p, Psi) {
    mark(
      dat.proc, 
      dat.ddl,
      model = "Multistrata", 
      model.parameters = list(S = S, p = p, Psi = Psi), 
      output = F,    
      delete = T
      )
  }
    
  # Define models to run
  call.mods <- function() {
    Sdot.pdot.Psidot <- run.mod(Sdot, pdot, Psidot)
    Sdot.pdot.Psitime <- run.mod(Sdot, pdot, Psitime)
    Sdot.ptime.Psidot <- run.mod(Sdot, ptime, Psidot)
    Sdot.ptime.Psitime <- run.mod(Sdot, ptime, Psitime)
    
    Stime.pdot.Psidot <- run.mod(Stime, pdot, Psidot)
    Stime.pdot.Psitime <- run.mod(Stime, pdot, Psitime)
    Stime.ptime.Psidot <- run.mod(Stime, ptime, Psidot)
    Stime.ptime.Psitime <- run.mod(Stime, ptime, Psitime)
    
    return( collect.models() )
  }
  
  # Call the models
  res <- call.mods()
  