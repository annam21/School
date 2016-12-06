  # Final project
  # Anna Moeller
  # 11/30/2016

  # Packages
  source("C:/Users/anna.moeller/Documents/GitHub/packages.R")
  
  # Set working directory
  setwd("C:/Users/anna.moeller/Documents/GitHub")
 
  # Load data
  load("CameraTrapStudy/2015 data/pics.wide20160804.RData")
  
  # Separate out mule deer and white tails
  source("CameraTrapStudy/Image Analysis/deerpresent_fn.R")
  mddata <- deerpresent_fn(pics) %>%
    
    # Get rid of all that other crap
    select(site, plot, cam, timeLST, dateLST, mdpresent, MDbuck, MDantlerless, MDfawn, 
           MDunkn, opstate, uniquemark, easting, northing, plot.start, plot.end) %>%
    
    # Only look at February
    filter(month(dateLST) == 2)
  
  # See if mule deer are at every site
  mddata %>%
    group_by(plot) %>%
    summarise(deer = any(mdpresent == T))

  
  
#########################
  # Create occupancy encounter history
### Without effort included so far. 
  
  # Load access database
  load("C:/Users/anna.moeller/Documents/GitHub/CameraTrapStudy/2015 data/access.sum.RData")
  
  # Make a dataframe for camera number 1-9
  camplot <- select(access.sum, camID, plot, camnum) %>%
    # Take out the weird cameras that don't really exist
    filter(camnum != 0, 
           !(camID %in% c("AM999", "AM888", "AM777"))) %>% 
    select(-camnum) %>%
    rename(cam = camID) %>%
    mutate(camID = 1:159, # Unique ID for each camera
           plotnum = as.numeric(as.factor(plot)) ) 

  # Make an encounter history (combine all cameras, temporal replicates of 1 day)
  dat <- mddata %>%
    rename(present = mdpresent) %>%
    left_join(., camplot, by = c("plot" = "plot")) %>%
    # Encounter history by day, group all cameras together
    group_by(plot, dateLST) %>%
    summarise(pres = any(present == T),
              plotnum = first(plotnum)) %>%
    # This part stays the same
    mutate(eh = ifelse(pres == T, 1, 0),
           occasion = as.numeric(as.factor(dateLST)),
           beav = ifelse(grepl("BH", plot), 1, 0)) # Is the camera in the Beaverhead?

############################################
  # pdot
  # Data
  occ.data <- list(y = dat$eh,
                   nObs = length(dat$dateLST),
                   nSite = length(unique(dat$plotnum)),
                   site = dat$plotnum,
                   occ = dat$occasion
                   )
    
  # Initial values
  # initial values for JAGS
  occ.inits <- function(){
    list( z = rep( 1, length(unique(dat$plotnum))) ,
          b0.psi = runif( 1, -3, 3 ),
          b0.p = runif( 1, -3, 3 )
    )
  }
  
  # set parameters to track in JAGS
  occ.parms <- c( "b0.psi", "b0.p", "mean.psi", "mean.p" )
  
  # set up for MCMC run
  ni <- 5000
  nt <- 1
  nb <- 500
  nc <- 3
  
  # run the MCMC chain in JAGS
  occ.result <- jags( occ.data, 
                      occ.inits,
                      occ.parms,
                      "School/Demographic Parameters/Final Project/pdot_occ.txt",
                      n.chains=nc, 
                      n.iter=ni, 
                      n.burnin=nb,
                      n.thin=nt
  )
  
  # View result
  occ.result
  mcmcplot( occ.result )
 
############################################
  # psizone
  zone <- dat %>%
    group_by(plotnum) %>%
    summarise(beav = first(beav))
  
  # Data
  occ.data <- list(y = dat$eh,
                   nObs = length(dat$dateLST),
                   site = dat$plotnum,
                   nSite = length(zone$beav),
                   occ = dat$occasion,
                   zone = zone$beav
  )
  
  # Initial values
  # initial values for JAGS
  occ.inits <- function(){
    list( z = rep( 1, length(zone$beav) ),
          b0.psi = runif( 1, -3, 3 ),
          b0.p = runif( 1, -3, 3 ),
          b1.psi = runif( 1, -3, 3 )
    )
  }
  
  # set parameters to track in JAGS
  occ.parms <- c( "b0.psi", "b1.psi", "b0.p", "mean.p" )
  
  # set up for MCMC run
  ni <- 5000
  nt <- 1
  nb <- 500
  nc <- 3
  
  # run the MCMC chain in JAGS
  occ.result <- jags( occ.data, 
                      occ.inits,
                      occ.parms,
                      "School/Demographic Parameters/Final Project/psizone_occ.txt",
                      n.chains=nc, 
                      n.iter=ni, 
                      n.burnin=nb,
                      n.thin=nt
  )
  
  # View result
  occ.result
  mcmcplot( occ.result )
  
  # occupancy 
  plogis(occ.result$BUGSoutput$mean$b0.psi) # St. Joe
  plogis(occ.result$BUGSoutput$mean$b0.psi + occ.result$BUGSoutput$mean$b1.psi) # Beaverhead
  
###################################################
  # Include camera effort
  
  
  
  
  
  
  
  
  
  
  
  
  
  
###################################
  # Create an effort encounter history by week
  # Start by making an eh by day
  source("C:/Users/anna.moeller/Documents/GitHub/CameraTrapStudy/Image Analysis/eh_fn.R")
  cam.eh <- eh_fn(pics, starthour = "12:00:00", endhour = "12:00:00", 
                  by_t = "day", datelim = NULL, animal.eh = F)
  
  # Look at the week I'm using and if at least 4 of the days are open, call it open
  effort <- filter(cam.eh, ideal.date >= as.Date("2016-02-01") & 
                     ideal.date <= as.Date("2016-02-08")) %>%
    group_by(cam) %>%
    summarise(test = length(which(open == T))) %>%
    mutate(open = ifelse(test >= 4, T, F)) %>%
    select(-test)
  
  # Create an occupancy encounter history for a single week
  # This gives NAs where there is no "elkpresent" column for the entire week
  #   That includes photos that have not been gone through yet and
  #   photos that don't exist (got deleted/stolen, etc.)
  # This also accounts for censored photos using effort, above
  occ <- filter(pics, dateLST >= as.Date("2016-02-01") & dateLST <= as.Date("2016-02-08")) %>%
    group_by(cam) %>%
    summarise(occupied = any(elkpresent == T),
              plot = min(plot)) %>%
    right_join(., camnum, by = c("cam" = "camID")) %>% # This is to change cam from an ID to a number 1-9
    left_join(., effort, by = c("cam" = "cam")) %>% # This adds the "open" column
    mutate(occupied = replace(occupied, open == F, NA), # If it's closed, occupied is an NA
           occupied = ifelse(is.na(occupied), ".", ifelse(occupied == T, "1", "0"))) %>% 
    select(-plot.x, -cam, -open) %>% 
    rename(plot = plot.y) %>%
    spread(camnum, occupied) %>%
    unite(ch, 2:10, sep = "") # tidyr
  
  # For JAGS
  occ <- filter(pics, dateLST >= as.Date("2016-02-01") & dateLST <= as.Date("2016-02-08")) %>%
    group_by(cam) %>%
    summarise(occupied = any(elkpresent == T),
              plot = min(plot)) %>%
    right_join(., camnum, by = c("cam" = "camID")) %>% # This is to change cam from an ID to a number 1-9
    left_join(., effort, by = c("cam" = "cam")) %>% # This adds the "open" column
    mutate(occupied = replace(occupied, open == F, NA), # If it's closed, occupied is an NA
           occupied = ifelse(occupied == T, 1, ifelse(occupied == F, 0, NA))) %>% 
    select(-plot.x, -cam, -open) %>% 
    rename(plot = plot.y) %>%
    spread(camnum, occupied) 
  
  # #############################
  # # Okay, scrap that. Try bunnies 
  # lagos <- pics %>%
  #   select(site, plot, cam, timeLST, dateLST, otherpresent, lagomorph, opstate, 
  #          easting, northing, plot.start, plot.end)
  # lagos$lagomorph[is.na(lagos$lagomorph)] <- 0
  # lagos %>%
  #   group_by(plot) %>%
  #   summarise(lag = any(lagomorph > 0))
  # ### Even worse. 
  # 
  # # wolves are just as bad
  # pics %>% 
  #   group_by(plot) %>%
  #   summarise(new = any((!is.na(wolf.pup) & wolf.pup > 0 )| 
  #                         (!is.na(wolf.adult) & wolf.adult > 0)) )
  
  # meese?
  # dat <- pics 
  # dat$MooseBull[is.na(dat$MooseBull)] <- 0
  # dat$MooseCalf[is.na(dat$MooseCalf)] <- 0
  # dat$MooseAntlerless[is.na(dat$MooseAntlerless)] <- 0
  # dat$MooseUnkn[is.na(dat$MooseUnkn)] <- 0
  # pics %>%
  #   group_by(plot)    %>%
  #   summarise(l = any(MooseBull > 0 | MooseAntlerless > 0| MooseCalf > 0| MooseUnkn > 0))
  # 
  
  # To find column names
  names(pics)[grep("lion", names(pics))]
  
  ##############################
  # Lions (Best option)
  lions <- pics %>%
    select(site, plot, cam, timeLST, dateLST, otherpresent, lion.kitten, lion.adult, opstate, 
           easting, northing, plot.start, plot.end) %>%
    filter(month(dateLST) == 2)
  lions$lion.kitten[is.na(lions$lion.kitten)] <- 0
  lions$lion.adult[is.na(lions$lion.adult)] <- 0
  lions %>%
    group_by(plot) %>%
    summarise(l = any(lion.adult > 0 | lion.kitten > 0)) 
  
  ### Make encounter history
  # Divide up into chunks of time? 
  
  
  
  # # pdot nested
  # # Data
  # occ.data <- list(y = dat$eh,
  #                  nObs = length(dat$dateLST),
  #                  site = dat$plotnum,
  #                  occ = dat$occasion
  # )
  # 
  # # Initial values
  # # initial values for JAGS
  # occ.inits <- function(){
  #   list( z = rep( 1, length(dat$dateLST)) ,
  #         b0.psi = runif( 1, -3, 3 ),
  #         b0.p = runif( 1, -3, 3 )
  #   )
  # }
  # 
  # # set parameters to track in JAGS
  # occ.parms <- c( "b0.psi", "b0.p","mean.psi", "mean.p" )
  # 
  # # set up for MCMC run
  # ni <- 5000
  # nt <- 1
  # nb <- 500
  # nc <- 3
  # 
  # # run the MCMC chain in JAGS
  # occ.result <- jags( occ.data, 
  #                     occ.inits,
  #                     occ.parms,
  #                     "School/Demographic Parameters/Final Project/morenested.txt",
  #                     n.chains=nc, 
  #                     n.iter=ni, 
  #                     n.burnin=nb,
  #                     n.thin=nt
  # )
  # 
  # # View result
  # occ.result
  # mcmcplot( occ.result )
  
  