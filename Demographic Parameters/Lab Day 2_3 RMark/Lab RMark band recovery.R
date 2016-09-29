  # WILD 595 Band Recovery Model lab
  # Intro to RMark
  
  # Call the RMark package - you'll need to have Mark installed on your computer as well.
  library(RMark)

# Part A
  # Give groups in the order that they are in data file
  groups <- data.frame(age = c("Adults", "Subadults")) 
  
  # Convert MARK files to something that works with RMark
  # This creates a long data file
  npMales <- convert.inp("School/Demographic Parameters/Lab Day 2 RMark/NPmalesEH.inp", group.df = groups, covariates = NULL, use.comments = FALSE)
  
  # Process the data - build into something RMark can use
  # model = "Recovery" for Seber, model="Brownie" for Brownie parameterization
  npMales.processed <- process.data(npMales, groups = "age", model = "Recovery")
  
  # Make design data (takes processed data)
  # Helps you build linear models later on
  npMales.ddl <- make.design.data(npMales.processed)
  
  # Add more to the design data for our model
  # Define which animals are subadults (only for one year)
  # In both S (survival design data)
  npMales.ddl$S$young <- 0
  npMales.ddl$S$young[npMales.ddl$S$initial.age.class == "Subadults" & npMales.ddl$S$age == 0] <- 1
  # And r (recovery design data)
  npMales.ddl$r$young <- 0
  npMales.ddl$r$young[npMales.ddl$r$initial.age.class == "Subadults" & npMales.ddl$r$age == 0] <- 1
  
  # Biological hypotheses to test
  # Survival submodels
  Sdot <- list(formula = ~1) # Constant (intercept-only)
  Stime <- list(formula = ~time) # Time-varying
  Sage <- list(formula = ~young) # Different between subadult/adult
  Sagetime <- list(formula = ~young*time)
  
  # recovery submodels
  rdot <- list(formula = ~1)
  rtime <- list(formula = ~time)
  rage <- list(formula = ~young)
  ragetime <- list(formula = ~young*time)
  
  # Run analysis in MARK
  npMales.Sdot.rdot <- mark(npMales.processed, npMales.ddl, 
                            model.parameters = list(S = Sdot, r = rdot))
  npMales.Sage.rage <- mark(npMales.processed, npMales.ddl, 
                            model.parameters = list(S = Sage, r = rage))
  npMales.Stime.rtime <- mark(npMales.processed, npMales.ddl, 
                              model.parameters = list(S = Stime, r = rtime))
  npMales.Stime.rdot <- mark(npMales.processed, npMales.ddl, 
                             model.parameters = list(S = Stime, r = rdot))
  npMales.Sdot.rtime <- mark(npMales.processed, npMales.ddl, 
                             model.parameters = list(S = Sdot, r = rtime))
  npMales.Sagetime.ragetime <- mark(npMales.processed, npMales.ddl, 
                                    model.parameters = list(S = Sagetime, r = ragetime))
  
  # Look at AIC
  npMales.Sdot.rdot$results$AICc
  npMales.Sage.rage$results$AICc
  npMales.Stime.rtime$results$AICc
  npMales.Stime.rdot$results$AICc
  npMales.Sdot.rtime$results$AICc
  npMales.Sagetime.ragetime$results$AICc
  
  # Look at help files for prettier ways to output results