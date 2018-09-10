  # Anna Moeller
  # Homework 1
  # 9/10/18

  library(tidyverse)
  
  # Sec 1.6
  # Problem 6
  # Toss 3 coins and estimate probability that all are heads or all tails
  nsim <- 1000000
  set.seed(1003)
  x <- rerun(nsim, sample(c(0,1), 3, replace = T)) %>% 
    map_dbl(sum) 
  length(which(x %in% c(0,3))) / length(x)
  
  # Simulation estimates 0.250271 probability, which is similar to the calculated
  # probability of 0.25.
  
  # Sec 1.7
  # Problem 6
  # Roll 6 dice. Estimate probability that all 6 are different
  nsim <- 1000000
  out <- rep(NA, nsim)
  set.seed(23049)
  for(i in 1:nsim){
    x <- sample(1:6, 6, replace = T)
    out[i] <- length(unique(x)) == 6
  }
  
  sum(out)/nsim
    
  # Simulation estimates probability 0.015317, which is close to calculated 
  #  probability of 0.0154321