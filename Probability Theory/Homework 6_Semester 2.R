# Additional Problems

### Problem 1
n <- c(10, 50, 500)
sumX <- c(6.1, 27.3, 262.4)

# Bayesian CRI
LCRI <- qgamma(0.025, n+5, 1+sumX)
UCRI <- qgamma(0.975, n+5, 1+sumX)

# Frequentist CI
c1 <- qgamma(.975, n, 1)
c2 <- qgamma(.025, n, 1)
LCI <- c2/sumX
UCI <- c1/sumX

# Plot them
plot(1, type="n", xlab="n", ylab="", xlim=c(0, 4), ylim=c(0, 5), xaxt = "n")
axis(1, at = seq(1.1, 3.1), labels = n)
segments(c(1,2,3), LCI, c(1,2,3), UCI)
segments(c(1.2,2.2,3.2), LCRI, c(1.2,2.2,3.2), UCRI, col = "red")
legend("topright", 
       legend = c("CI", "CRI"),
       lty = 1,
       col = c("black", "red")
       ) 

## Problem 3
### Part h
n <- 1:20
theta <- 1

# Expressions for MSE
# Theta^
theta_hat_MSE <- 2*theta^2/(n+1)/(n+2)
# Theta^U
theta_hat_U_MSE <- theta^2/n/(n+2)
# Theta~
theta_tilde_MSE <- theta^2/3/n

# Plot the MSEs
curve(th)
plot(n, theta_hat_MSE, type = "l", lty = 1, ylab = "MSE")
lines(n, theta_hat_U_MSE, type = "l", lty = 2)
lines(n, theta_tilde_MSE, type = "l", lty = 3)
legend("topright", legend = c("theta_hat", "theta_hat_U", "theta_tilde"), lty = c(1,2,3))

#### Part i
theta <- 1
n <- c(5, 10, 20)
nsim <- 1e6
 
theta_hat <- theta_hat_U <- theta_tilde <- matrix(NA, nrow = length(n), ncol = nsim)
for(i in 1:length(n)){
  for(j in 1:nsim){
    X <- runif(n[i], 0, theta)
    theta_hat[i, j] <- max(X)
    theta_hat_U[i, j] <- max(X) * (n[i]+1)/n[i]
    theta_tilde[i, j] <- mean(X) * 2 
  }
}

# calculate MSE from the sample
sampMSE <- function(estimator, truth){
  mean((estimator-truth)^2)
}

# Theta hat
theta_hat_true_MSE <- 2*theta^2/(n+1)/(n+2) # true
theta_hat_samp_MSE <- apply(theta_hat, 1, sampMSE, theta) # from sample

# Theta hat unbiased
theta_hat_U_true_MSE <- theta^2/n/(n+2)
theta_hat_U_samp_MSE <- apply(theta_hat_U, 1, sampMSE, theta)

# Theta tilde
theta_tilde_true_MSE <- theta^2/3/n
theta_tilde_samp_MSE <- apply(theta_tilde, 1, sampMSE, theta)

t <- as.data.frame(
  rbind(theta_hat_true_MSE, 
        theta_hat_samp_MSE, 
        theta_hat_U_true_MSE,
        theta_hat_U_samp_MSE,
        theta_tilde_true_MSE,
        theta_tilde_samp_MSE
        )
)
names(t) <- n
t