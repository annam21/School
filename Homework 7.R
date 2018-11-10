  # Homework 7
  # Anna Moeller 
  # 11/9/2018

  # Additional problems 
  # 1
  n <- 1000000
  X <- rexp(n, 1)
  Y <- rexp(n, 1)
  U <- X/(X + Y)
  hist(U, freq = F)
  abline(h = 1, col = "red")
  