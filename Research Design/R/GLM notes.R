# Anna Moeller
# Lab 7
# GLMs
# 10/12/15

# read in GLM data
data <- read.csv("C:/Users/anna.moeller/Documents/School/Research Design/R/glmData.csv")

res.x1 <- lm(y ~ x1, data = data)
summary(res.x1)
# distribution of residuals
# we defined residuals as norm(0, sigma2) so IQ1 to IQ2 should = IQ2 to IQ3
# coefficients:
# H0 for intercept is that intercept = 0 (generally stupid, 
#    because we wouldn't use this covariate if intercept is 0)
# H0 that slope is 0 and there is no relationship
# multiple R-squared (0, 1) tells us how much variance is explained by this model 
# R-squared will be smaller if there is huge variance
# adjusted R-squared penalizes R-squared for the number of parameters used
# F-statistic tells us how significant the relationship is (how steep the slope is)
### My thoughts: variance = sum of squares of distance from mean

plot(res.x1)
# residuals vs. fitted: fitted = prediction from model
#    always has mean = 0, 
#    if residuals are identically distr, 0 should be in middle of y-axis, have = #s above/below line
#    red line should be straight and close to 0
#    we also want equal points across values of y (the x-axis) - this would make red line angle up/down
#    common: fan-shape: increased variance with increased values of cov 
#    (with more animals we're measuring, there is more variance)
# qq-plot
# scale-location: should be flat. this folds plot1 so everything is positive
# residuals vs. leverage
#    leverage: how much each data pt contributes to the slope of the line
#    cook's distance: data pts outside this line are contributing too much to the slope

res.x1x2 <- lm(y ~ x1 + x2, data = data)
summary(res.x1x2)
plot(res.x1x2)

res.x1Ix2 <- lm(y ~ x1 + x2 + x1*x2, data = data)
summary(res.x1Ix2)

# true model from how data were simulated
res.true <- lm(y ~ x1 + x3 + x4 + I(x4*x4), data = data)
plot(res.true)
# in plots, we have more points on the right side of the plot
#    this means that we have more data points at those values
#    and inference is weaker at values of y with fewer data points (on the left)

res.true2 <- glm(y ~ x1 + x3 + x4 + I(x4*x4), data = data)
summary(res.true2)
