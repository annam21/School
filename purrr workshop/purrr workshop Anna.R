  # purrr workshop webinar - Charlotte Wickham
  # Anna Moeller
  # 11/1/2017
  
  library(tidyverse)
  load("data/swapi.rda")
  
  # What is the difference between people[[1]] and people[1]?
  # [[1]] return the stuff in the first element
  # [1] return a list with 1 element, whose contents are the stuff in the first element
  
  # map(.x, .f)
  # for each element of .x, do .f
  
  # ex. 1: How many starships has each person been in?
  luke <- people[[1]]
  length(luke$starships)
  
  # Use ~ for the formula, and use .x as the placeholder meaning "for each element"
  map(people, ~length(.x$starships))
  map(people, ~length(.x[["starships"]])) 
  
  # Ex. 2. Find the home planet of each character, using planet_lookup
  load("data/planet_lookup.rda")
  # For Luke
  planet_lookup[luke$homeworld == names(planet_lookup)] # My way
  planet_lookup[luke$homeworld] # Prettier way since it's a named vector
  # For all
  map(people, ~planet_lookup[.x$homeworld])
  
  # map always returns a list
  # map_lgl, map_dbl, map_chr, map_int return vectors
  # walk() used when you don't want anything returned, but want to plot/save, etc. 
  
  # Name each element of people
  people <- people %>%
    set_names(map_chr(., "name"))
  
  # ex. 3
  # How many starships has each character been in?
  map_int(people, ~ length(.x[["starships"]])) %>% # integer because counting
    hist() # cool piping
  # What color is each character's hair?
  map_chr(people, ~ .x[["hair_color"]])
  # Is the character male?
  map_lgl(people, ~ .x[["gender"]] == "male")
  # How heavy is each character? should be int but there are "unknown"s
  map_chr(people, ~ .x[["mass"]]) %>%
    readr::parse_number(na = "unknown")
  
  # .f can be a string
  # All equivalent
  map(people, ~.x$starships) 
  map(people, ~.x[["starships"]]) 
  map(people, "starships") # No tilde
  
  # .f can be an integer
  map(people, ~.x[[5]])
  map(people, 5) # Gives the 5th element of each person's list

  # .f can be a function (just like lapply)
  map_int(people, length, ...) # If there were additional arguments to length()
  map_int(people, length(.x, ...)) 
  
  # Stopped at 65:06
  
  # Second day. See if I remember anything
  
  # Which film has the most characters?
  # 1.
  numppl <- map_int(films, ~length(.x$characters))
  # 2. 
  numppl <- map(films, ~.x$characters) %>%
    map_int(length)
  films[[which(numppl == max(numppl))]]$title
  # Go further
  map(films, "characters") %>%
    map_int(length) %>%
    set_names(map(films, "title")) %>%
    sort(decreasing = T)

  # Stopped again at 76:15

  