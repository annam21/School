# Example monitoring database
library(tidyverse)
xx <- tibble::tibble(
  ID = 1:3,
  start = as.Date(c("2016-04-01", "2017-04-06", "2016-12-31")),
  end = as.Date(c("2018-02-11", "2018-09-10", NA)), 
  age_at_capture = c("Y", "A", "A")
) 

this_month <- as.Date(lubridate::floor_date(Sys.Date()))
xx %>% 
  mutate(date = lubridate::floor_date(start, unit = "months"),
         end = lubridate::floor_date(end, unit = "months")) %>% 
  group_by(ID, age_at_capture) %>% 
  complete(
    date = seq(start, this_month, by = "month")
  ) %>% 
  print(n = Inf)