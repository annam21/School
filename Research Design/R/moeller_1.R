# Anna Moeller
# Assignment 2
# 9/14/2015

hey <- function(x, y, filename){
  # Takes vectors x and y and output filename, returns a list with column sums 
  # and unique elements across both, outputs jpeg of their histograms
  
  # Alert if character vector
  if(class(x) == "character" | class(y) == "character"){
    print("Hey! This is a character vector!")
  } else {
    print("Everything looks OK")
  }
  
  # Make everything numeric
  x <- as.numeric(x)
  y <- as.numeric(y)

  # Make a function that sums up all values of a vector and ignores NAs
  mysum <- function(x){
    sumx <- 0
    for(i in 1:length(x)){
      if(!is.na(x[i])){
        sumx <- sumx + x[i]
      }
    }
    return(sumx)
  }
  
  # Create a list where:
    # First element of the list is sum across x and sum across y
    # Second element of the list is unique elements across xy
  l <- list(sums = c(mysum(x), mysum(y)), 
            unique = unique(c(x, y)))
  
  # Plot histograms of x and y, write them to jpeg
  jpeg(paste(filename, ".jpg", sep = ""))
  
  par(mfrow = c(2,1))
  hist(x, prob = T)
  curve(dnorm(x, mean = mean(x, na.rm = T), sd = sd(x, na.rm = T)), add = T, col = "red")
 
  hist(y, prob = T)
  curve(dnorm(x, mean = mean(y, na.rm = T), sd = sd(y, na.rm = T)), add = T, col = "red")
  
  dev.off()
  
  # Return l
  return(l)
}
