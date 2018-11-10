  # Homework 7
  # Anna Moeller 
  # 11/2/2018

  # Additional problems

  # Number 1
  n <- 1000000
  X <- rexp(n, 1)
  Y <- rexp(n, 1)
  U <- X/(X + Y)
  hist(U, freq = F)
  abline(h = 1, col = "red")

  # Number 3
  n <- 1000000
  X <- runif(n)
  Y <- runif(n)
  Z <- X/(X-Y)
  hist(Z, breaks = 1000000, xlim = c(-30, 30), freq = F)
  curve(1/(2*x^2), add = T, col = "red")
  
  