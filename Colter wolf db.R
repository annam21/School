require(tidyverse)

df <- data.frame(
  cam = rep(c(1,2), each = 4),
  date = rep(rep(c(Sys.time(), Sys.time()+1), each = 2), 2),
  Sp1 = c("wolf", "wolf", "none", "none", "none", "none", "none", "wolf"),
  Male = c(1, 1, 0, 0, 0, 0, 0, 0),
  Female = c(1,0,0,0,0,0,0,1),
  Sp2 = c("none", "none", "deer", "elk", "none", "none", "none", "deer"),
  Male2 = c(0,0,1,0,0,0,0,0),
  Female2 = c(0,0,0,1,0,0,0,1),
  Sp3 = c(rep("none", 7), "bobcat"),
  Male3 = c(rep(0, 7), 1),
  Female3 = rep(0, 8),
  stringsAsFactors = F
)


# Append all the columns
df1 <- df %>% 
  select(cam, date, Sp1, Male, Female) %>% 
  rename(Species = Sp1) 
df2 <- df %>%
  select(cam, date, Sp2, Male2, Female2) %>% 
  rename(Species = Sp2,
         Male = Male2, 
         Female = Female2)
df3 <- df %>%
  select(cam, date, Sp3, Male3, Female3) %>% 
  rename(Species = Sp3,
         Male = Male3, 
         Female = Female3)
new <- bind_rows(df1, df2, df3) %>% 
  distinct() # Get rid of multiple rows of "none"
# Done. This is what we want. 

# For analysis
new %>% 
  # Choose study species
  filter(Species == "wolf") %>% 
  # Get rid of multiple photos at the same time 
  group_by(cam, date, Species) %>% 
  summarise(Male = max(Male),
            Female = max(Female)) %>% 
  # Now get a total count
  group_by(cam, date, Species) %>% 
  summarise(count = sum(Male, Female))


