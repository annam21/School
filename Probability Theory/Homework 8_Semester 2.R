# Problem 2
library(Sleuth3)
library(boot)
head(case2202)

# Mean salamanders per plot
mn <- function(d, i) mean(d$Salamanders[i])
b_mn <- boot(case2202, mn, 1e5)
boot.ci(b_mn)
plot(b_mn)

# Correlation between forest age and salamanders
correl <- function(d, i) cor(d$ForestAge[i], d$Salamanders[i])
b_correl <- boot(case2202, correl, 1e5)
boot.ci(b_correl)
plot(b_correl)
