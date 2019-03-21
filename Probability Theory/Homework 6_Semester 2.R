# Additional Problems

### Problem 1
n <- c(10, 50, 500)
sumX <- c(6.1, 27.3, 262.4)

# Frequentist CI
c1 <- qgamma(.975, n, 1)
c2 <- qgamma(.025, n, 1)
LCI <- c2/sumX
UCI <- c1/sumX

# Bayesian CRI 

# Plot them
plot(1, type="n", xlab="", ylab="", xlim=c(0, 4), ylim=c(0, 4), xaxt = "n")
segments(c(1,2,3), LCI, c(1,2,3), UCI)


### Problem 3
#### Part h
n <- 1:20
theta <- 1

# Expressions for MSE
# Theta^
theta_hat <- (n*theta^2)/( (n+2)*(n+1)^2 ) + theta^2/(n-1)^2
# Theta^U
theta_hat_U <- theta^2/n/(n+2)
# Theta~
theta_tilde <- theta^2/3/n

# Plot the MSEs
curve(th)
plot(n, theta_hat, type = "l", lty = 1, ylab = "MSE")
lines(n, theta_hat_U, type = "l", lty = 2)
lines(n, theta_tilde, type = "l", lty = 3)
legend("topright", legend = c("theta_hat", "theta_hat_U", "theta_tilde"), lty = c(1,2,3))

#### Part i
theta <- 1
n <- 5
nsim <- 10
theta_hat <- theta_hat_U <- theta_tilde <- matrix(NA, nrow = length(n), ncol = nsim)

for (i in 1:length(n)) {
  for(j in 1:nsim){
    X <- runif(n[i], 0, theta)
    theta_hat[i, j] <- (n[i]*theta^2)/( (n[i]+2)*(n[i]+1)^2 ) + theta^2/(n[i]-1)^2
    theta_hat_U[i, j] <- theta^2/n[i]/(n[i]+2)
    theta_tilde[i, j] <- theta^2/3/n[i]
  }
}

