  # Homework 4
  # Anna Moeller 
  # 10/4/2018

# Additional problem # 1
  # Locations are: 1) Forest, 2) Mountains, 3) Plains
  # initialize vector of where we search
  search <- rep(NA, 5)
  
  # Prior probabilities that the plane went down in each region
  pr_region <- matrix(c(.5, .3, .2, rep(NA, 15)) , nrow = 3)
  row.names(pr_region) <- c("Forest", "Mountains", "Plains")
  
  # Conditional probabilities of a successful search given the plane is there
  pr_success_g_its_there <- c(.3, .5, .8)
  
  # probability of finding plane on first search
  pr_success <- pr_success_g_its_there * pr_region
  row.names(pr_success) <- c("Forest", "Mountains", "Plains")
   
  for(i in 1:5){ 
    search[i] <- which(pr_success[, i] == max(pr_success[, i]))
   
    # We know the search failed. So calculate posterior probability that it's in each region
    pr_region[, i+1] <- pr_region[, i] * (1-pr_success_g_its_there) /
      sum(pr_region[, i] * (1-pr_success_g_its_there))
     
    # What is the new probability of success (given updated prob of each region)? 
    pr_success[, i+1] <- pr_region[, i+1] * pr_success_g_its_there
  }
  
  #####################
  # Display results
  
  # Probability of plane being in each region
  pr_region
  
  # Probability of finding the plane in each region
  pr_success
  
  # Given that, where did we search? 
  c("Forest", "Mountains", "Plains")[search]
  
################################################################################
  # Grad problem # 2 
  sim <- function(nsim, n){
    out <- rep(NA, nsim)
    
    for(i in 1:nsim){
      # Draw three integers from 1:n without replacement
      draw <- sample(1:n, 3)
      
      # Are any two consecutive? 
      out[i] <- 1 %in% diff(draw)
    }
    
    # Estimated probability 
    mean(out)
  }
  
  # Part a
  sim(nsim = 100000, n = 9)
  
  # Part b
  sim(nsim = 100000, n = 5)
  sim(nsim = 100000, n = 20)

  # Compare to calculated probability with n = 9
  8/9 * 1/8 + 
    7/9 * 1/8 * 1/7 + 
    1/9 * 7/8 * 1/7 + 
    1/9 * 6/8 * 1/7 + 
    6/9 * 5/8 * 1/7 + 
    1/9 * 6/8 * 1/7
