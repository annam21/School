

### Week 6 summary statistics and plotting

# a key philosophy in R, and in data analysis in general, is always to 
# look at your data early in the process. That is, don't start by
# fitting models and looking at anova tables; start by looking at the raw
# data in different ways.

############################# SCATTERPLOTS with fitted lines

sb <- data.frame(Seatbelts)
names(sb)

# Make a quick scatter plot of the DriversKilled and PetrolPrice
# Two ways to do this depending on how you specify the columns
plot(sb$PetrolPrice, sb$DriversKilled)

plot(DriversKilled ~ PetrolPrice, data=sb)

# Look at ?plot and graphical parameters

# Change the axis labels - using ylab and xlab

# Using \n add a line break to x/y label
# "Gas Price\n(per gal)"
plot(sb$DriversKilled ~ sb$PetrolPrice, ylab = "Deaths", xlab = "Gas Price (per gal)" )

# Rotate the Y axis labels using las
# A better way present valu
plot(sb$DriversKilled ~ sb$PetrolPrice, ylab = "Deaths", xlab = "Gas Price (per gal)", las = 1)

# change the data points--they're too big and too overlapping in the middle.
# will do two things--fill them in and make them much smaller.
#pch = shape, cex = size, col = color
plot(sb$DriversKilled ~ sb$PetrolPrice,
     ylab = "Deaths",
     xlab = "Gas Price (per gal)",
     las = 1,
     pch = 3,
     cex = 0.6,
     col = "blue")

# There appears to be a relationship.. so let's examine that more
# Fit simple linear regression model using lm() and Save the model output as fit.lm
# Formula is: y ~ x 

 fit.lm <- lm(DriversKilled ~ PetrolPrice, data = sb)

# Look at the summary of the model output using summary()
 summary(fit.lm)
# what is this showing us?
 
 
# Now add regression line using the abline function lwd = line width
 # abline(model.object, col=, lwd=)
 abline(fit.lm, col = "limegreen", lwd = 2)

# Switch over to dashed lines using the linetype: lty = type
  plot(sb$DriversKilled ~ sb$PetrolPrice, 
       ylab = "Deaths", xlab = "Gas Price (per gal)", las = 1,
    pch = 3,
    cex = 0.6,
    col= "blue")
  
abline(fit.lm, col = "purple", lwd = 5, lty = 2)

# Add text describing the line by specifiying x, y location of the text you want to include
# text(x, y, "text)

text(0.13,110 , "LM fit", col = "purple", pos = 3, cex = 1)

# To close/clear a plot window
dev.off()


######## Other kinds of plots

######## HISTOGRAM

  hist(sb$DriversKilled)
  
  #Adjust the bins of the data using breaks or nclass
  
  hist(sb$DriversKilled, breaks = 12)
  hist(sb$DriversKilled, breaks = 50)
  
# Use same customization as regular plot function
  hist(sb$DriversKilled, breaks = 12, main = "My Histogram", col = "limegreen")
  
# Check out the difference between Freq = T or F  
  hist(sb$DriversKilled, breaks = 12, main = "My Histogram", col = "limegreen", freq = F)
  
# Can examine the characteristics fo the histogram by saving it as an object and then viewing said object
  x <- hist(sb$DriversKilled, breaks = 12, main = "My Histogram", col = "limegreen")
  x
  
# Maybe you want a smoothed distribution rather than a blocked-histogram
  # Use density() to fit a line
  d <- density(sb$DriversKilled)
  plot(d, col = "red", lwd = 3)
  
  # We can add that to our histogram using lines()
  hist(sb$DriversKilled, 
       breaks = 20, 
       main = "My Histogram", 
       col = "lightgray", 
       border = "lightgray",
       freq = F)
  lines(d, col = "red", lwd = 2)
  # Be sure to use freq = F!

# Alternatively, use a polygon 
  hist(sb$DriversKilled, 
       breaks = 20, 
       main = "My Histogram", 
       col = "lightgray", 
       border = "lightgray",
       freq = F)
  polygon(d, col = "red", border = "red")
  # You can make it transparent by using col = adjustcolor("red", alpha.f=0.5)
  

###################### BOXPLOTS
# Boxplots are useful to examine difference in categorical values
  # use iris data

#use boxplot() to compare Sepal.Length by Species
boxplot(Sepal.Length ~ Species, data = iris)

# Note outlier

# Look into the boxplot output to find outlier
print(boxplot(iris$Sepal.Length ~ iris$Species))

#or you can save the boxplot as an object and view it


# Now slightly more complex analyses involving multiple covariates
# Adding more than one covariate to regression model
# Data set on cars and different specs
head(mtcars)

# Let's examine whether miles per gallon (mpg) is predicted by weight (wt) and / or hp.
car.m <- lm(mpg ~ wt + hp, data = mtcars)

# Create some subsets of car.m model with single covariates to compare 
car.m2 <- lm(mpg ~ wt, data = mtcars)
car.m3 <- lm(mpg ~ hp, data = mtcars)
car.null <- lm(mpg ~ 1, data = mtcars)

#can extract AIC values
AIC(car.m, car.m2, car.m3, car.null)

# top model summary
  summary(car.m)

  
################ Diagnostic plots
# Diagnostic plots - plot top model object using plot
  # http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/
  plot(car.m)
  # Hit return

# coefficient values - can extract to use in plotting
coefs <- car.m$coefficients

#fitted values - also can think of as the predicted values given your data
fits <- car.m$fitted.values
fits2 <- data.frame(FitMpg = fits, KnownMpg = mtcars$mpg)

# What happens if we remove outliers you may create more in the data
# Three cars (rows) having a large effect in model
# Remove the Toy. Corolla, Chry. Imperial, and Mas. Bora.
rownames(mtcars) #see row names

  ncar <- mtcars[-c(20,17,31),]

#re-run original model with new data
  new.m <- lm(mpg ~ wt + hp, data = ncar)
  summary(new.m)
  summary(car.m)
  coefficients(new.m)
  coefficients(car.m)
  plot(new.m)


### Ways to visualize groups of data at once
  
  plot(iris$Petal.Length, iris$Petal.Width)  
  
  # color or change the shape to represent species ID
  plot(iris$Petal.Length, iris$Petal.Width, col = iris$Species)  
  legend("topright",
         legend = c("setosa", "versicolor", "virginica"),
         pch = 1,
         col = c("black", "red", "green"))
  
plot(iris$Petal.Length, iris$Petal.Width, pch = as.numeric(iris$Species))

# The plot function plots each row in order so just providing 3 colors or 3 shapes doesn't work ex: c(23,24,25)
# To get a full vector to use in plotting:
  c(23,24,25)[unclass(iris$Species)]

  plot(iris$Petal.Length, iris$Petal.Width, pch = c(23,24,25)[unclass(iris$Species)])

# col vs bg
# shape outline color vs fill
  plot(iris$Petal.Length, iris$Petal.Width, pch = 21, bg = c("red","limegreen","purple")[unclass(iris$Species)])
  plot(iris$Petal.Length, iris$Petal.Width, pch = 21, col = c("red","limegreen","purple")[unclass(iris$Species)])

# Add a legend
  legend("topleft", 
         pch = 21, 
         legend = levels(iris$Species),
         col = c("red", "limegreen", "purple"))
  
  
# Using pairs() to plot all combinations of continuous data
  pairs(iris[,1:4])  # only plotting first for columns

# add in the species colors for contrast
  pairs(iris[,1:4], pch = 21, bg = c("red","limegreen","purple")[unclass(iris$Species)])

# do some more refining - look at lower.panel and labels
 ?pairs()
  
  pairs(iris[,1:4], pch = 21, bg = c("red","limegreen","purple")[unclass(iris$Species)],lower.panel=NULL, labels=c("SL","SW","PL","PW"), font.labels=2, cex.labels=4.5)


##################### another question: is my data really normal? does this matter?

# qqnorm and qqline
qqnorm(sb$DriversKilled)
qqline(sb$DriversKilled)  # qqline adds the 1/1 line

# let's see what non-normal data looks like on a qq plot
Z = rexp(1000)
hist(Z)

qqnorm(Z)
qqline(Z)

# Formal test Shapiro-Wilk for normality using shapiro.test()
# Null: The data are not different from a normal (i.e., they are normal)

shapiro.test(sb$DriversKilled)

# Now shapiro the Z data from above
hist(Z)
shapiro.test(Z)


###### Exercise ######

## test to see if petal length for just setosa in the iris data set is normal - hint: hist, qqnorm, shapiro.test





