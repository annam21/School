# Anna Moeller
# Ptarmigan Assignement 1
# 9/14/2015

# Load packages
library(dplyr)

# Read in data
pt <- read.csv("C:/Users/anna.moeller/Documents/School/Research Design/ptarm.csv")

# Data manipulation
# 1
filter(pt, Site == "Kluane")
# 2
arrange(pt, Sex)
# 3
select(pt, Sex, Site)
# 4 
mutate(pt, diff = P9-P8)
# 5
filt <- filter(pt, Site == "Kluane") %>%
  arrange(Sex) %>%
  mutate(diff = P9-P8)
  
# Data exploration
qqnorm(filt$P8)
qqline(filt$P8, distribution = qnorm)

par(mfrow = c(2,1))
by(pt$P8, pt$Site, hist)

# Summary statistics
mean(filt$P8)
median(filt$P8)
var(filt$P8)
sd(filt$P8)
# Standard error
se <- function(x) sd(x)/sqrt(length(x))
se(filt$P8)
