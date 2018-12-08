   # Roll a 1, 2, or 3. If you get a 1, it's 2 hours to safety. A 2 is 3 hours 
  #  back to middle. A 3 is 7 hours back to middle. 
  
  nsim <- 100000
  out <- rep(NA, nsim)
  for(i in 1:nsim){
    success <- F
    x <- 0
    while(success == F){
      c <- sample(1:3, 1)
      if(c == 1){
        x <- x + 2
        success <- T
      } else if(c == 2) {
        x <- x + 3
      } else if(c ==3){
        x <- x + 7
      }
    }
    out[i] <- x
  }
  mean(out)
  sd(out)
