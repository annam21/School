#
# WILD 240 Assignment 5 
# Confidence intervals
#

# Question 1
X=matrix(rnorm(2400, 10, 2), nrow=24, ncol=100)
# X contains 100 data sets with sample size 24
# columns of X represent each data set (V1 to V100)
# rows represent data points within each set.
# the true mean of X is 10


# Question 2
xbar = apply( X, 2, mean )
xvar = apply( X, 2, var )
xse = sqrt(xvar/nrow(X) )
xUCIz = xbar + 1.96*xse
xLCIz = xbar - 1.96*xse

# Question 3
z.coverage = ifelse( xUCIz > 10 & xLCIz < 10, 1, 0 )
sum(z.coverage)/100
z.width=xUCIz-xLCIz

