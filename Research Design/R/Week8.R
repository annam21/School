# Anna Moeller
# GLMM 

# Load data and clean it up
elk <- read.csv("C:/Users/anna.moeller/Documents/School/Research Design/R/elk.csv")
elk$State <- as.factor(elk$State)
elk$Name <- as.factor(elk$Name)

# Load packages
library(lme4) 
library(lattice)
library(ggplot2)
library(gamm4)

# Run some models
(res11 <- lmer( CalfCowRatio ~ WinterNDVI + (1|State/Name), data=elk, REML=FALSE ))
(res12 <- lmer( CalfCowRatio ~ Eprecipb + (1|State/Name), data=elk, REML=FALSE ))
(res13 <- lmer( CalfCowRatio ~ SummerNDVI + (1|State/Name), data=elk, REML=FALSE ))
(res14 <- lmer( CalfCowRatio ~ WinterNDVI + Eprecipb + (1|State/Name), data=elk, REML=FALSE ))
(res15 <- lmer( CalfCowRatio ~ WinterNDVI + Eprecipb + SummerNDVI + (1|State/Name), data=elk, REML=FALSE ))
(res16 <- lmer( CalfCowRatio ~ Eprecipb + SummerNDVI + (1|State/Name), data=elk, REML=FALSE ))
(res17 <- lmer( CalfCowRatio ~ WinterNDVI + SummerNDVI + (1|State/Name), data=elk, REML=FALSE ))

(res21 <- lmer( CalfCowRatio ~ WinterNDVI:Ecotype + (1|State/Name), data=elk, REML=FALSE ))
(res22 <- lmer( CalfCowRatio ~ Eprecipb:Ecotype + (1|State/Name), data=elk, REML=FALSE ))
(res23 <- lmer( CalfCowRatio ~ SummerNDVI:Ecotype + (1|State/Name), data=elk, REML=FALSE ))

# Pull out the fixed effects to plot them 
parm <- fixef(res23)
coefs <- data.frame( a=rep(parm[1],3), b=parm[2:4])

# Plot 1
p <- qplot(SummerNDVI, CalfCowRatio, data = elk)
p + geom_smooth(aes(group = Ecotype), method = "lm") 

# Plot it again but differently
p <- qplot(SummerNDVI, CalfCowRatio, data = elk)
p + geom_abline(data = coefs, aes(intercept = a, slope = b))

# Pull out just the random effects
ints <- ranef(res23)

# Plot histogram of each ????
hist(ints$"Name:State"[, 1])

# Something ???
sqrt(unlist(VarCorr(res23))
     
# Plot something else ??? 
qqmath(ranef(res23, postVar = TRUE), strip = FALSE)$Name 

# ??? 

################################################################################
# Non-linear effects
library(gamm4)
res<-gamm(CalfCowRatio ~ s(Eprecipb) + s(SummerNDVI) , random=list(State=~1, Name=~1), data=elk, method="ML" )

plot(res$gam)

################################################################################

# Exercise

# Models with a single covariate
res1 <- glm(CalfCowRatio ~ WinterNDVI, data = elk)
res2 <- glm(CalfCowRatio ~ SummerNDVI, data = elk)
res3 <- glm(CalfCowRatio ~ Eprecipb, data = elk)

# Which is best supported? Look at lowest AIC/BIC, and for a small SD compared to Estimate
# Winter AIC 9408, SD:est bad
# Summer AIC 9382, SD:est better, p-value significant, t-value < -5
# Precip AIC 9412, SD:est bad
# SummerNDVI is most supported. It has a negative relationship with cow calf ratios

# try a combo with winter and summer. does this work?
res4 <- glm(CalfCowRatio ~ SummerNDVI + WinterNDVI, data = elk)
# This gives different results

# Add a random effect to summer
res5 <- lmer(CalfCowRatio ~ SummerNDVI + (1|State/Name), data = elk, REML=FALSE)
# AIC 9102 - better than any single covariate
# unit accounts for 35 of the variance, state accounts for 39 of the variance
# Together they account for less than half the total variance

# With interaction
res21 <- lmer( CalfCowRatio ~ WinterNDVI:Ecotype + (1|State/Name), data=elk, REML=FALSE )
res22 <- lmer( CalfCowRatio ~ Eprecipb:Ecotype + (1|State/Name), data=elk, REML=FALSE )
res23 <- lmer( CalfCowRatio ~ SummerNDVI:Ecotype + (1|State/Name), data=elk, REML=FALSE )
# Winter AIC 9095
# Precip AIC 9098
# Summer AIC 9095
