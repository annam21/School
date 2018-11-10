  # Homework 7
  # Anna Moeller 
  # 11/2/2018

  # Additional problems

  # Number 1
  

  # Number 3
  n <- 1000000
  X <- runif(n)
  Y <- runif(n)
  Z <- X/(X-Y)
  hist(Z, breaks = 1000000, xlim = c(-50, 50), freq = F)
  