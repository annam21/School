# Anna Moeller
# Assignment 4
# 9/21/2015

# Load packages
library(dplyr)

# Read in data
pt <- read.csv("C:/Users/anna.moeller/Documents/School/Research Design/R/ptarm.csv")

# Make year a factor
pt <- mutate(pt, Year = as.factor(Year))

# One factor: site
  # Site as ANOVA
  res1 <- aov(P8 ~ Site, data = pt)
  summary(res1)
  
  # Site as linear model
  res2 <- lm(P8 ~ Site, data = pt)
  summary(res2)

# Two factors: site and sex
  # Additive 
  res3 <- aov(P8 ~ Site + Sex, data = pt)
  res4 <- lm(P8 ~ Site + Sex, data = pt)

  # Interaction
  res5 <- lm(P8 ~ Site*Sex, data = pt)

# Three factors: site and sex and age
  res6 <- lm(P8 ~ Site*Sex*Ageclass, data = pt)

# Make a boxplot
par(mfrow = c(2,1))
boxplot(pt$P8 ~ pt$Sex + pt$Site)
boxplot(pt$P8 ~ pt$Sex*pt$Site) # These are the same 

par(mfrow = c(1,1))
boxplot(pt$P8 ~ pt$Sex + pt$Site + pt$Ageclass)
  