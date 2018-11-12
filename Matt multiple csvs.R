# Multiple csv's to Matt
# Anna Moeller
# 10/25/2018

# Load packages
library(tidyverse)

# All the file paths (full path)
filepaths <- list.files(, full.names = T) # fill in here

# Pull in all the csvs into a tibble
  dat <- tibble(File = filepaths) %>%
    
    # This is an example of how to extract certain things from the filepath into new columns
    extract(File, "Site", "/(.* County)/", remove = FALSE) %>% 
    
    # Read in the files into a single column (this will look like a list)
    mutate(Data = lapply(File, read_csv) ) %>% 
    
    # Make it into a prettier dataframe (no list column)
    unnest(Data)