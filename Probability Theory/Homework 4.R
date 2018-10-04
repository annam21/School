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
    # Where do we search? 
    # If two probabilities are the same, randomly pick one. 
    test <- which(pr_success[, i] == max(pr_success[, i]))
    if(length(test) > 1){
      search[i] <- sample(test, 1)
    } else if(length(test) == 1){
      search[i] <- test
    }
    
    # Probability that the search failed
    pr_failed <- 1 - pr_success[search[i], i]
    
    # Posterior probability of being in region given failed search
    for(j in 1:3){
      if(j == search[i]){
        pr_region[j, i + 1] <- pr_region[j, i] * (1 - pr_success_g_its_there[j]) * 
          pr_region[j, i] / pr_failed
      } else {
        pr_region[j, i+1] <- pr_region[j, i]/pr_failed
      }
    }
    
    # Probability of finding plane
    pr_success <- pr_success_g_its_there * pr_region
  }
  
  # Display 
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
  sim(nsim = 10000, n = 5)
  sim(nsim = 10000, n = 20)

  # Calculated probability with n = 9
  8/9 * 1/8 + 
    7/9 * 1/8 * 1/7 + 
    1/9 * 7/8 * 1/7 + 
    1/9 * 6/8 * 1/7 + 
    6/9 * 5/8 * 1/7 + 
    1/9 * 6/8 * 1/7
  