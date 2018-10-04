  # Homework 4
  # Anna Moeller 
  # 10/4/2018

  # Prior probabilities that the plane went down in each region
  # Locations are: 1) Forest, 2) Mountains, 3) Plains
  pr_region <- matrix(c(.5, .3, .2, rep(NA, 15)) , nrow = 3)
  
  # Conditional probabilities of a successful search given the plane is there
  pr_success_g_its_there <- matrix(c(.3, .5, .8, rep(NA, 15)), nrow = 3)
  
  # probability of finding plane on first search
  pr_success <- pr_success_g_its_there * pr_region
  
  # Which place do we search? 
  search <- which(pr_success[, i] == max(pr_success[, i]))
  
  # Probability that the search failed
  pr_failed <- 1 - pr_success[search, i]
  
  # Posterior probability of being in region given failed search
  for(j in 1:3){
    if(j == search){
      pr_success[j, i + 1] <- pr_region[j, i] * (1 - pr_success_g_its_there[j, i]) * 
        pr_region[j, i] / pr_failed
    } else {
      pr_success[j, i+1] <- pr_region[j, i]/pr_failed
    }
  }

 