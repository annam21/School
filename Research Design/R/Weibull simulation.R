  # Anna Moeller
  # Research Design simulation
  # 11/2/2015
  
  # Load packages 
  library(vcd)
  library(MASS)
  
  # Set working directory
  setwd("C:/Users/anna.moeller/Documents/School/Research Design/R")
  
  # Select Weibull shape values, sample sizes, reps in each n in each k
  k <- seq(1, 10, by = 0.5)
  n <- 1:50
  reps <- 1000
  
  # initialize a final storage vector
  final <- list()
  
  # Loop over Weibull
  for(h in 1:length(k)){
    
    # Initialize intermediate storage vector
    probrej <- NULL
    
    # Loop over sample size
    for(i in 1:length(n)){
      
      # Initialize an intermediate storage vector
      rej <- NULL
      
      # Loop over reps
      for(j in 1:reps){
        
        # Simulate data
        draw <- rweibull(n[i], shape = k[h], scale = 1)
        
        # Kolmogorov-Smirnov Goodness-of-fit test
        # Estimate lambda from these data, assuming they're exponential
        fit1 <- fitdistr(draw, "exponential") 
        # Goodness of fit test (are the data actually exponential?)
        comp <- ks.test(draw, "pexp", fit1$estimate)
        
        # Reject H0 or not? (H0: the data are exponential)
        rej[j] <- comp$p.value < 0.05
      }
      
      # For this Weibull and sample size, calculate the probability of rejecting H0
      probrej[i] <- sum(rej)/length(rej)
    }
    
    # Store a prob of rejection by sample size curve for each Weibull
    final[[h]] <- probrej
  }
  
  plot(x = n, y = final[[which(k == 2)]],  xlab = "sample size", 
       ylab = "Probability of Rejecting H0", 
       main = "Probability of Rejecting the Exponential Distribution")
  points(x = n, y = final[[which(k == 3)]], col = "red", lty = 1)
  points(x = n, y = final[[which(k == 5)]], col = "orange")
  points(x = n, y = final[[which(k == 7)]], col = "green")
  points(x = n, y = smooth(final[[which(k == 9)]]), col = "blue")
  legend(x = "topright", legend = c("k = 2", "k = 3", "k = 5", "k = 7", "k = 9"),
         col = c("black", "red", "orange", "green", "blue"), pch = 1)
  
  # Save this output
  save(final, file = "simulation.RData")
  for future: 
  load("simulation.RData")
  final
