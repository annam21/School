# Anna Moeller
# Week 9 notes
# Research Design

# Count data
elk<-c(9,14,15,142,15,11,7,6,15,10,11,10,7,21,18,16,20,9,12,14,0,0,0,0,0)

###############
# Bootstrap
###############
# Select reps
reps <- 100

# Initialize an end vector
meanCV <- NULL

# Loop over sample sizes
for(j in 1:8){
  n <- j * 50
  
  # Initialize an intermediate storage vector
  CV <- NULL
  
  # Loop over reps
  for(i in 1:reps){
    # Sample with replacement from the pilot data
    samp <- sample(elk, size = n, replace = T)
    
    # Calculate the CV for this sample
    CV[i] <- (sd(samp)/sqrt(n))/mean(samp)
  }
  
  # Calculate the mean CV for this sample size 
  meanCV[j] <- mean(CV)
}

# Plot CV by sample size
plot(x = 50 * (1:length(meanCV)), y = meanCV)


###################
# Distribution 
###################
# Select reps and sample size
reps <- 1000
n <- 1:40

# Initialize end vector
CV <- NULL

# Loop over sample size from 1 to 100
for(i in n){
  # Simulate survival data
  samp <- rbinom(n = reps, size = i, prob = 0.75)
  
  # Calculate the CV of probability of survival for this sample size
  p <- samp/i
  CV[i] <- (sd(p)/sqrt(length(p)))/mean(p)
}

# Plot CV by sample size
plot(x = n, y = CV)

# This looks wrong again

#######################
# Process simulation
#######################
# Loop over animals 
# Loop over years
# Initialize detection vector
det <- NULL

# Is this animal alive in this year?
alive <- rbinom(1, 1, 0.75)

# If the animal was alive, was it detected? 
draw <- rbinom(1, 1, 0.6)
if(alive == 1 & draw == 1){
  det <- 1
} else {
  det <- 0
}