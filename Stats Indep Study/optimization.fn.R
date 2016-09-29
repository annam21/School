# Anna Moeller
# 2/3/2016
# Optimization function for exponential distribution

# Draw Random Variables from a given exponential distribution
data <- rexp(100, rate = 5)

# Draw Random Variables from an exponential distribution
expo.fn <- function(x, lambda){
  pdf <- lambda*exp(-lambda*x)
  # now i can plug in any x and lambda and get its probability density value
  # so i want you to pick random x values based on 
}

data <- expo.fn(___)




# logL for expo.fn
f <- function(x, lambda){
  # Gives the value of the logL for any one lambda (we want the max value)
  length(x)*log(lambda) - sum(lambda*x) 
}

# first derivative of expo logL
df.dx <- function(x, lambda){
  length(x)/lambda - sum(x)
}  

# second derivative of expo logL
d2f.dx2 <- function(x, lambda){
  -length(x)/lambda^2
}

# Write a Newton function to maximize likelihood and return likeliest value of lambda
newton <- function(data, df.dx, d2f.dx2, guess, tolerance, maxiter){
  est <- guess
  out <- matrix(NA, nrow = maxiter + 1, ncol = 2)
  out[1, ] <- c(guess, df.dx(data, guess))
  iter <- 1
  continue <- T
  
  while (continue) {
    iter <- iter + 1
    est.old <- est
    est <- est - df.dx(data, est)/d2f.dx2(data, est) # Newton
    out[iter, ] <- c(est, df.dx(data, est))
    continue <- (abs(est - est.old) > tolerance) && (iter <= maxiter)
  }
  if (iter > maxiter) { 
    warning("Maximum number of iterations reached")
    return(out)
  }
  out <- out[!is.na(out[, 1]), ]
  out
}

# Call it...
newton(data, df.dx, d2f.dx2, guess = 1, tolerance = 0.0001, maxiter = 100)
# Error in while (continue) { : missing value where TRUE/FALSE needed
# means your initial guess was bad



# Draw Random Variables from an exponential distribution
expo.fn <- function(n, lambda){
  prx <- lambda * exp(-x * lambda) 
  return(prx)
}

# Generate a curve
x <- seq(0, 50, by = 1)
data <- sapply(x, expo.fn, lambda = 1/5)
plot(x, data)
# I could generate "better" data by selecting x from a uniform
x2 <- runif(100, min = 0, max = 50)



# first derivative of expo logL
df.dx <- function(f, x, lambda, h){
  (f(x + h, lambda) - f(x, lambda))/h
}  

# second derivative of expo logL
d2f.dx2 <- function(f, df.dx, x, lambda, h){
  (df.dx(f, x + h, lambda, h) - df.dx(f, x, lambda, h))/h
}


# Test out f to make sure it's right
s <- seq(0.1, 3, by = 0.1)
xx <- sapply(s, f, x = data)
plot(s, xx)

xx2 <- sapply(s, df.dx, x = data)
plot(s, xx2)

xx3 <- sapply(s, d2f.dx2, x = data)
plot(s, xx3)
