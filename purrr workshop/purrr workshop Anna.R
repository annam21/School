  # purrr workshop webinar - Charlotte Wickham
  # Anna Moeller
  # 11/1/2017
  
  library(tidyverse)
  load("data/swapi.rda")
  load("data/planet_lookup.rda")
  
  # What IS the difference between people[[1]] and people[1]?
  # [[1]] return the stuff in the first element
  # [1] return a list with 1 element, whose contents are the stuff in the first element
  
  # map(.x, .f)
  # for each element of .x, do .f
  
  # ex. 1: How many startships has each person been in?
  luke <- people[[1]]
  length(luke$starships)
  
  # For my own information... Also can use position
  head(map(people, ~.x[[1]]))
  head(map(people, 1))
  
  # Use ~ for the formula, and use .x as the placeholder meaning "for each element"
  map(people, ~length(.x$starships))
  map(people, ~length(.x[["starships"]])) 
  
  # Ex. 2. Find the home planet of each character, using planet_lookup
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
  
  ##### On other computer, Until 76:00
  ##### Back to Mac
  # Create planet_lookup vector
  pl <- planets %>%
    map_chr("name") %>%
    set_names(map_chr(planets, "url"))
  
  # Which species has the most possible eye colors?
  ec <- species %>%
    map_chr("eye_colors") %>%
    strsplit(split = ", ") %>%
    set_names(map_chr(species, ~.x$name)) %>%
    map_int(length) # Sort of lazy, includes NA and unknowns
  
  # Putting a few vars in a tibble
  film_lookup <- map_chr(films, "title") %>% set_names(map_chr(films, "url"))
  film_number_lookup <- stringr::str_split_fixed(names(film_lookup), "/", 7)[, 6]  %>%
    as.numeric() %>%
    set_names(map_chr(films, "url"))
  
  people_tbl <- tibble(
    name    = people %>% map_chr("name"), 
    films   = people %>% map("films"),
    height  = people %>% map_chr("height") %>% 
      readr::parse_number(na = "unknown"),
    species = people %>% map_chr("species", .null = NA_character_) # .null: what to return
                                                  # For characters that don't have that element
  ) %>%
    mutate(n_films = map_int(films, length),
           film_names = map(films, ~ film_lookup[.x]),
           film_numbers = map(films, ~ film_number_lookup[.x])) # inside mutate, can use films without "" to get column films
  
  # Practice: collapse film_numbers into a single string for each person
  # Try for just Luke first
  people_tbl$film_numbers[[1]]
  paste( people_tbl$film_numbers[[1]], sep = ", ") # sep makes them all into a vector
  paste( people_tbl$film_numbers[[1]], collapse = ", ") # collapse makes them all into a single string
  prac <- people_tbl %>%
    mutate(fnums = map_chr(film_numbers, ~paste(.x, collapse = ", ")) ) 

  # map2
  # map with both .x and .y (same length)(match first elements, 2nd elements, ...)

  # look up purrr:rerun for running a fn repeatedly (e.g. simulating rnorm over multiple 
  #   values of mu and sd)
  # invoke_map to apply multiple functions to one .x
  # look up safely() and transpose() for errors
  