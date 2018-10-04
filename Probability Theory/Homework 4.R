  # Homework 4
  # Anna Moeller 
  # 10/4/2018

  # Prior probabilities that the plane went down in each region
  # Locations are: 1) Forest, 2) Mountains, 3) Plains
  pr_region <- matrix(c(.5, .3, .2) )
  pr_F <- 0.5
  pr_M <- 0.3
  pr_P <- 0.2
  
  # Conditional probabilities of a successful search given the plane is there
  pr_SFgF <- 0.3
  pr_SMgM <- 0.5
  pr_SPgP <- 0.8
  
  # probability of finding plane on first search
  pr_SF <- pr_SFgF * pr_F
  pr_SM <- pr_SMgM * pr_M
  pr_SP <- pr_SPgP * pr_P
  
  # Which place do we search? 
  search <- which(c(pr_SF, pr_SM, pr_SP) == max(pr_SF, pr_SM, pr_SP))
  
  # Probability that the search failed
  pr_failed <- 1 - c(pr_SF, pr_SM, pr_SP)[search]
  
  # Posterior probability of being in region given failed search
  if(search == 1){
    pr_F <- pr_F * (1 - pr_SFgF) * pr_F/ pr_failed
  } else{
    pr_F <- 1 * pr_F/pr_failed
  }
  if(search == 2){
    pr_M <- pr_M * (1 - pr_SMgM) * pr_M/ pr_failed
  } else {
    pr_M <- 1 * pr_M/pr_failed
  }
  if(search == 3){
    pr_P <- pr_P * (1 - pr_SPgP) * pr_P/ pr_failed
  } else {
    pr_P <- 1 * pr_P/pr_failed
  }