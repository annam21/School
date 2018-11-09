  # Homework 6
  # Anna Moeller 
  # 10/28/2018

  # Additional problem #2
  n <- 1000000
  N <- sample(0:3, n, prob = c(.15, .2,.5, .15), replace = T)
  X <- rbinom(n, size = N, prob = 0.5)
  
  # part a) marginal of X
  length(which(X == 0))/n
  length(which(X == 1))/n
  length(which(X == 2))/n
  length(which(X == 3))/n
  
  # Part b) conditional distribution of N iven X = 0
  condit <- N[X==0]
  length(which(condit == 0))/length(condit)
  length(which(condit == 1))/length(condit)
  length(which(condit == 2))/length(condit)
  length(which(condit == 3))/length(condit)
  
  # Problem 3
  # pseudo-random X
  Y <- runif(n)
  X <- sqrt(4*Y)
  hist(X, freq = F, breaks = 100)
  curve(x/2, col = "red", lwd = 2,add = T)  