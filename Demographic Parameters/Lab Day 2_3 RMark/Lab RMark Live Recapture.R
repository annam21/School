  # Lab Day 2 - Part B: Live Recapture Models
  # Anna Moeller
  # 9/8/16
  
  # Load packages  
  library(RMark)
  
  # Import data and convert to a format for RMark
  # Even though it looks like ".INP" you have to call it ".inp"
  groups <- data.frame(sex = c("males", "females"))
  data <- convert.inp("School/Demographic Parameters/Lab Day 2 RMark/DIPPER.inp", 
                      group.df = groups, covariates = NULL, use.comments = F)
  
  # Process the data for RMark
  proc <- process.data(data, groups = "sex", model = "CJS")
  
  # Make design data for RMark
  dsn <- make.design.data(proc)

  # Add flood to the design data
  dsn$Phi$flood <- 0
  dsn$Phi$flood[dsn$Phi$Time >= 3] <- 1 ### How do you do this? Is this right??
### Is this right??
  dsn$p$flood <- 0
  dsn$p$flood[dsn$p$Time >= 3] <- 1 ### How do you do this? Is this right??
  
  # Biological hypotheses
    # Apparent survival hypotheses
    phi.dot <- list(formula = ~1)
    phi.time <- list(formula = ~time)
    phi.sex <- list(formula = ~sex)
    phi.flood <- list(formula = ~flood)
    phi.sex.time <- list(formula = ~sex*time)
    
    # Recapture probability hypotheses
    p.dot <- list(formula = ~1)
    p.time <- list(formula = ~time)
    p.sex <- list(formula = ~sex)
    p.flood <- list(formula = ~flood)
    p.sex.time <- list(formula = ~sex*time)
  
  # Run models in MARK
  dipper.phi.dot.p.dot <- mark(proc, dsn, model.parameters = list(Phi = phi.dot, p = p.dot))
  dipper.phi.time.p.time <- mark(proc, dsn, model.parameters = list(Phi = phi.time, p = p.time))
  dipper.phi.sex.p.sex <- mark(proc, dsn, model.parameters = list(Phi = phi.sex, p = p.sex))
  dipper.phi.sex.time.p.sex.time <- mark(proc, dsn, model.parameters = list(Phi = phi.sex.time, p = p.sex.time))
  dipper.phi.flood.p.flood <- mark(proc, dsn, model.parameters = list(Phi = phi.flood, p = p.flood))
  
  