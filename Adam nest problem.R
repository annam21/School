  # Add a summarised covariate to your dataframe when each entry is a range of 
  #    dates
  
  # Original dataframe
  n <- data.frame(ID = c(1, 2),
                  hatch = as.Date(c("2016-01-01", "2016-01-03")),
                  fledge = as.Date(c("2016-01-02", "2016-01-04"))
  )
  
  # Dataframe of weather covariates to reference and summarise
  w <- data.frame(date = as.Date(c("2016-01-01", "2016-01-02", "2016-01-03", "2016-01-04")),
                  rain = c(12, 4, 16, 10)
  )
  
  foo <- function(weatherdata, weathercol = "rain", fn = mean, s, e){
    # Returns a summarised vector corresponding to a range of dates
    # Takes: weather dataframe (must include date column and some covariate column)
    #    weathercol = the covariate column in weatherdata you want to summarise
    #    fn = what summarise function you want to do (eg. mean, max...) (not in quotes)
    #    s & e = the range of dates you want to summarise, should be of class Date
    out <- weatherdata %>%
      filter(between(date, s, e)) %>%
      # Apply fn to weathercol
      summarise(new = fn(.[, grep(weathercol, names(.))]) ) %>%
      # This returns the column as a vector
      .$new
    return(out)
  }
  
  # Example call
  yay <- n %>%
    rowwise() %>%
    # Create a new column with your specifications
    # All you need to change are column name, weathercol, and fn
    mutate(max_rain = foo(weatherdata = w, weathercol = "rain", fn = max, 
                          s = hatch, e = fledge) ) %>%
    ungroup()
  
############################
  # A less attractive way that nonetheless has some cool tricks
  #   Mostly combining objects and names together in dplyr (grep, .dots, fun_, setNames)
  foo <- function(weatherdata, weathercol = "rain", fn = mean, newcol = "newcol", s, e){
    # Returns a summarised dataframe corresponding to a range of dates
    # Takes: weather dataframe (must include date column and some covariate column)
    #    weathercol = the column in the weather dataframe you want to summarise
    #    fn = what summarise function you want to do (eg. mean, max...) (not in quotes)
    #    newcol = the name of the output column you want to make
    #    s & e = the range of dates you want to summarise, should be of class Date
    out <- weatherdata %>%
      filter(between(date, s, e)) %>%
      # Give back the first date in the range and
      #   the new summarised column (apply fn to weathercol)
      summarise(date = s,
                new = fn(.[, grep(weathercol, names(.))]) ) %>%
      # Now rename the new summarised column to newcol
      rename_(., .dots = setNames("new", newcol))
    return(out)
  }
  
  # Example call
  yay <- n %>%
    rowwise() %>%
    # Make a summarised weather dataframe for each row of n
    do(foo(weatherdata = w, weathercol = "rain", fn = max, newcol = "max_rain", 
           s = .$hatch, e = .$fledge)) %>%
    # Join it back with n
    left_join(n, ., by = c("hatch" = "date") )
  

    