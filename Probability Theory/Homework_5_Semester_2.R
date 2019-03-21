X <- c(0.8, 0.35, 0.59, 0.69, 0.06, 0.05, 0.12, 0.84, 0.14, 0.74)
mn <- mean(X)
s2 <- sum((X-mn)^2)/length(X)

a <- (mn^2 - mn^3 - mn*s2)/s2
b <- (a - a*mn)/mn

# Problem 2
fit <- MASS::fitdistr(X, "beta", start = list(shape1 = a, shape2 = b))
curve(dbeta(x, fit$estimate[1], fit$estimate[2]))
