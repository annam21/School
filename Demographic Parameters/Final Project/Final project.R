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
  
  # Make it general for functions below
  data <- mddata %>%
    rename(present = mdpresent)
    
  # # See if mule deer are at every site
  # mddata %>%
  #   group_by(plot) %>%
  #   summarise(deer = any(mdpresent == T))

  # Load access database
  load("C:/Users/anna.moeller/Documents/GitHub/CameraTrapStudy/2015 data/access.sum.RData")
  
  # Associate each cam and plot with numbers
    # camID is unique for every camera
    # camnum is 1-9 for cameras within a plot (used in spatial replicate model)
    # plotnum is 1-18
  camplot <- select(access.sum, camID, plot, camnum) %>%
    # Take out the weird cameras that don't really exist
    filter(camnum != 0, 
           !(camID %in% c("AM999", "AM888", "AM777"))) %>% 
    rename(cam = camID) %>%
    mutate(camID = 1:159, # Unique ID for each camera
           plotnum = as.numeric(as.factor(plot)) ) 
  
  # Create indicator of which zone the plot is in (length = # sites)
  zone <- camplot %>%
    group_by(plot) %>%
    summarise(plotnum = first(plotnum)) %>%
    mutate(beav = ifelse(grepl("BH", plot), 1, 0)) %>% # Is the camera in the Beaverhead?
    ungroup()
  
  
####################################################
  # Temporal replicates (combine data from all cameras per site) 
 
  # Effort is irrelevant because all days have at least one camera open 
  # # Start by making an eh by day
  # source("C:/Users/anna.moeller/Documents/GitHub/CameraTrapStudy/Image Analysis/eh_fn.R")
  # cam.eh <- eh_fn(pics, starthour = "12:00:00", endhour = "12:00:00", 
  #                 by_t = "day", datelim = NULL, animal.eh = F)
  # 
  # # Add effort to data, if the camera was closed, make occupancy NA
  # effort <- left_join(cam.eh, camplot, by = c("cam" = "cam")) %>%
  #   filter(ideal.date >= as.Date("2016-02-01"),
  #          ideal.date <= as.Date("2016-02-29")) %>%
  #   group_by(plot, ideal.date) %>%
  #   summarise(openever = any(open == T))
  #
  # any(tst$openever == F) #  All days have at least one camera open

  # Make an encounter history (combine all cameras, temporal replicates of 1 day)
  dat.t <- data %>%
    left_join(., camplot, by = c("plot" = "plot")) %>%
    # Encounter history by day, group all cameras together
    group_by(plot, dateLST) %>%
    summarise(pres = any(present == T),
              plotnum = first(plotnum)) %>%
    # This part stays the same
    mutate(eh = ifelse(pres == T, 1, 0),
           occasion = as.numeric(as.factor(dateLST)) ) 
 
############################################
  # pdot temporal 
  # Data
  occ.data <- list(y = dat.t$eh,
                   nObs = length(dat.t$eh),
                   nSite = length(unique(dat.t$plotnum)),
                   site = dat.t$plotnum,
                   occ = dat.t$occasion
                   )
    
  # Initial values
  # initial values for JAGS
  occ.inits <- function(){
    list( z = rep( 1, length(unique(dat.t$plotnum))) ,
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
  occ.res.t <- jags( occ.data, 
                      occ.inits,
                      occ.parms,
                      "School/Demographic Parameters/Final Project/pdot_occ.txt",
                      n.chains = nc, 
                      n.iter = ni, 
                      n.burnin = nb,
                      n.thin = nt
  )
  
  # View result
  occ.res.t
  mcmcplot( occ.res.t )
  
  
 
############################################
  # Psi zone temporal
  # JAGS Data
  occ.data <- list(y = dat.t$eh,
                   nObs = length(dat.t$dateLST),
                   site = dat.t$plotnum,
                   nSite = length(zone$beav),
                   occ = dat.t$occasion,
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
                      n.chains = nc, 
                      n.iter = ni, 
                      n.burnin = nb,
                      n.thin = nt
  )
  
  # View result
  occ.result
  mcmcplot( occ.result )
  
  # occupancy 
  plogis(occ.result$BUGSoutput$mean$b0.psi) # St. Joe
  plogis(occ.result$BUGSoutput$mean$b0.psi + occ.result$BUGSoutput$mean$b1.psi) # Beaverhead
  
######################################
  # Spatial replicates
  
  # Effort is moot because all cameras have at least 1 day open during the month
### I think... But doesn't this sort of change the data???

  # Make an encounter history (combine all days (Feb only), spatial replicates of 1 camera)
  # Now, occasion is 1-9 (the camera replicates)
  dat.sp <- data %>%
    left_join(., camplot, by = c("plot" = "plot", "cam" = "cam")) %>%
    # Encounter history by day, group all cameras together
    group_by(cam) %>%
    summarise(pres = any(present == T),
              plotnum = first(plotnum),
              camID = first(camID),
              occasion = first(camnum)) %>%
    # This part stays the same
    mutate(eh = ifelse(pres == T, 1, 0))
  
############################################
  # pdot spatial
  # Data
  occ.data <- list(y = dat.sp$eh,
                   nObs = length(dat.sp$eh),
                   nSite = length(unique(dat.sp$plotnum)),
                   site = dat.sp$plotnum,
                   occ = dat.sp$occasion
  )
  
  # Initial values
  # initial values for JAGS
  occ.inits <- function(){
    list( z = rep( 1, length(unique(dat.sp$plotnum))) ,
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
  occ.res.sp <- jags( occ.data, 
                      occ.inits,
                      occ.parms,
                      "School/Demographic Parameters/Final Project/pdot_occ.txt",
                      n.chains = nc, 
                      n.iter = ni, 
                      n.burnin = nb,
                      n.thin = nt
  )
  
  # View result
  occ.res.sp
  mcmcplot( occ.res.sp )
  
### Not every camera has 9 replicates - is the model putting these as NAs in p[i,t]
  # and making up values for them?
  
###########################################
  # n = 9
##############################################
  # pdot temporal n = 9
  # Data
  dat.t.9 <- data %>%
    filter(dateLST >= as.Date("2016-02-09"),
           dateLST <= as.Date("2016-02-17")) %>%
    left_join(., camplot, by = c("plot" = "plot")) %>%
    # Encounter history by day, group all cameras together
    group_by(plot, dateLST) %>%
    summarise(pres = any(present == T),
              plotnum = first(plotnum)) %>%
    # This part stays the same
    mutate(eh = ifelse(pres == T, 1, 0),
           occasion = as.numeric(as.factor(dateLST)) ) 

  occ.data <- list(y = dat.t.9$eh,
                   nObs = length(dat.t.9$eh),
                   nSite = length(unique(dat.t.9$plotnum)),
                   site = dat.t.9$plotnum,
                   occ = dat.t.9$occasion
  )
  
  # Initial values
  # initial values for JAGS
  occ.inits <- function(){
    list( z = rep( 1, length(unique(dat.t.9$plotnum))) ,
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
  occ.res.t.9 <- jags( occ.data, 
                     occ.inits,
                     occ.parms,
                     "School/Demographic Parameters/Final Project/pdot_occ.txt",
                     n.chains = nc, 
                     n.iter = ni, 
                     n.burnin = nb,
                     n.thin = nt
  )
  
  # View result
  occ.res.t.9
  mcmcplot( occ.res.t.9 )
  
  
############################################
  # pdot spatial n = 9
  # Data
  dat.sp.9 <- data %>%
    filter(dateLST >= as.Date("2016-02-09"),
           dateLST <= as.Date("2016-02-17")) %>%
    left_join(., camplot, by = c("plot" = "plot", "cam" = "cam")) %>%
    # Encounter history by day, group all cameras together
    group_by(cam) %>%
    summarise(pres = any(present == T),
              plotnum = first(plotnum),
              camID = first(camID),
              occasion = first(camnum)) %>%
    # This part stays the same
    mutate(eh = ifelse(pres == T, 1, 0))
  
  occ.data <- list(y = dat.sp.9$eh,
                   nObs = length(dat.sp.9$eh),
                   nSite = length(unique(dat.sp.9$plotnum)),
                   site = dat.sp.9$plotnum,
                   occ = dat.sp.9$occasion
  )
  
  # Initial values
  # initial values for JAGS
  occ.inits <- function(){
    list( z = rep( 1, length(unique(dat.sp.9$plotnum))) ,
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
  occ.res.sp.9 <- jags( occ.data, 
                      occ.inits,
                      occ.parms,
                      "School/Demographic Parameters/Final Project/pdot_occ.txt",
                      n.chains = nc, 
                      n.iter = ni, 
                      n.burnin = nb,
                      n.thin = nt
  )
  
  # View result
  occ.res.sp.9
  mcmcplot( occ.res.sp.9 )
  
  ### Not every camera has 9 replicates - is the model putting these as NAs in p[i,t]
  # and making up values for them?
  
###################################################
  # Temporal and spatial replicates
  # Make encounter history (both spatial and temporal replicates)
  dat.sptp <- data %>%
    left_join(., camplot, by = c("plot" = "plot", "cam" = "cam")) %>%
    # Encounter history by day and camera
    group_by(plot, camnum, dateLST) %>%
    summarise(pres = any(present == T),
              plotnum = first(plotnum)) %>%
    # This part stays the same (ish)
    mutate(eh = ifelse(pres == T, 1, 0),
           time = as.numeric(as.factor(dateLST)),
           space = camnum)

#######################################################
  # pdot spatial and temporal
  # Data
  occ.data <- list(y = dat.sptp$eh,
                   nObs = length(dat.sptp$eh),
                   nSite = length(unique(dat.sptp$plotnum)),
                   site = dat.sptp$plotnum,
                   nSpace = max(dat.sptp$space), # No. cams per plot
                   space = dat.sptp$space,
                   nTime = max(dat.sptp$time), # No. temporal replicates
                   time = dat.sptp$time
                   
  )
  
  # Initial values
  # initial values for JAGS
  occ.inits <- function(){
    list( z = rep( 1, length(unique(dat.sptp$plotnum))) ,
          zz = matrix( 1, nrow = length(unique(dat.sptp$plotnum)), 
                       ncol = max(dat.sptp$camnum) ),
          b0.psi = runif( 1, -3, 3 ),
          b0.p = runif( 1, -3, 3 ),
          b0.theta = runif(1, -3, 3)
    )
  }
  
  # set parameters to track in JAGS
  occ.parms <- c( "b0.psi", "b0.p", "b0.theta", "mean.psi", "mean.p", "mean.theta")
  
  # set up for MCMC run
  ni <- 5000
  nt <- 1
  nb <- 500
  nc <- 3
  
  # run the MCMC chain in JAGS
  occ.res.sptp <- jags( occ.data, 
                      occ.inits,
                      occ.parms,
                      "School/Demographic Parameters/Final Project/spat_temp_occ.txt",
                      n.chains = nc, 
                      n.iter = ni, 
                      n.burnin = nb,
                      n.thin = nt
  )
  
  # View result
  occ.res.sptp
  mcmcplot( occ.res.sptp )
  
  # plot the deviance myself
  hist(occ.res.sptp$BUGSoutput$sims.list$deviance, breaks = 100)
### hmmm...
    
#####################################################
  # Also include camera effort in spatiotemporal one
  # Start by making an eh by day
  source("C:/Users/anna.moeller/Documents/GitHub/CameraTrapStudy/Image Analysis/eh_fn.R")
  cam.eh <- eh_fn(pics, starthour = "00:00:00", endhour = "00:00:00", 
                  by_t = "day", datelim = NULL, animal.eh = F)
  
  # Add effort to data, if the camera was closed, make occupancy NA
  effort <- left_join(dat.sptp, camplot, by = c("plot" = "plot", "camnum" = "camnum",
                                                "plotnum" = "plotnum")) %>%
    left_join(., cam.eh, by = c("cam" = "cam", "dateLST" = "ideal.date")) %>%
    mutate(eh = replace(eh, open == F, NA))
    
  # There aren't very many of these because mddata only includes the pictures that were
  # taken in February (if a camera didn't take any pictures on a day, it got filtered out)
  # These are just the camera that are iced over, spit on, etc. 
    
  # Data
  occ.data <- list(y = effort$eh,
                   nObs = length(effort$eh),
                   nSite = length(unique(effort$plotnum)),
                   site = effort$plotnum,
                   nSpace = max(effort$space), # No. cams per plot
                   space = effort$space,
                   nTime = max(effort$time), # No. temporal replicates
                   time = effort$time
                   
  )
  
  # Initial values
  # initial values for JAGS
  occ.inits <- function(){
    list( z = rep( 1, length(unique(effort$plotnum))) ,
          zz = matrix( 1, nrow = length(unique(effort$plotnum)), 
                       ncol = max(effort$camnum) ),
          b0.psi = runif( 1, -3, 3 ),
          b0.p = runif( 1, -3, 3 ),
          b0.theta = runif(1, -3, 3)
    )
  }
  
  # set parameters to track in JAGS
  occ.parms <- c( "b0.psi", "b0.p", "b0.theta", "mean.psi", "mean.p", "mean.theta")
  
  # set up for MCMC run
  ni <- 5000
  nt <- 1
  nb <- 500
  nc <- 3
  
  # run the MCMC chain in JAGS
  occ.res.sptp.ef <- jags( occ.data, 
                        occ.inits,
                        occ.parms,
                        "School/Demographic Parameters/Final Project/spat_temp_occ.txt",
                        n.chains = nc, 
                        n.iter = ni, 
                        n.burnin = nb,
                        n.thin = nt
  )
  
  # View result
  occ.res.sptp.ef
  mcmcplot( occ.res.sptp.ef )
  
  # This helped decrease DIC a little.  
  
#################################
  # Results table
  df <- data.frame(
    model = c("temporal", "spatial", "spatiotemporal", "spatiotemporal with effort"),
    psi = c(occ.res.t$BUGSoutput$mean$mean.psi, 
            occ.res.sp$BUGSoutput$mean$mean.psi, 
            occ.res.sptp$BUGSoutput$mean$mean.psi,
            occ.res.sptp.ef$BUGSoutput$mean$mean.psi),
    SD.psi = c(occ.res.t$BUGSoutput$sd$mean.psi, 
           occ.res.sp$BUGSoutput$sd$mean.psi, 
           occ.res.sptp$BUGSoutput$sd$mean.psi,
           occ.res.sptp.ef$BUGSoutput$sd$mean.psi),
    p = c(occ.res.t$BUGSoutput$mean$mean.p, 
          occ.res.sp$BUGSoutput$mean$mean.p, 
          occ.res.sptp$BUGSoutput$mean$mean.p,
          occ.res.sptp.ef$BUGSoutput$mean$mean.p),
    SD.p = c(occ.res.t$BUGSoutput$sd$mean.p, 
           occ.res.sp$BUGSoutput$sd$mean.p, 
           occ.res.sptp$BUGSoutput$sd$mean.p,
           occ.res.sptp.ef$BUGSoutput$sd$mean.p),
    theta = c(NA, 
              NA, 
              occ.res.sptp$BUGSoutput$mean$mean.theta,
              occ.res.sptp.ef$BUGSoutput$mean$mean.theta),
    SD.theta = c(NA, 
                 NA, 
                 occ.res.sptp$BUGSoutput$sd$mean.theta,
                 occ.res.sptp.ef$BUGSoutput$sd$mean.theta)
    )
  
  # Making graph of p
  tst <- data.frame(x = occ.res.sp$BUGSoutput$sims.list$mean.p)
  ggplot(tst, aes(x)) + 
    geom_freqpoly(bins = 50) +
    xlim(0.25, 0.75)
  tst2 <- data.frame(x = occ.res.t$BUGSoutput$sims.list$mean.p)
  ggplot(tst2, aes(x)) + 
    geom_freqpoly(bins = 100) + 
    xlim(0.25, 0.75)
  
  # Comparing when n = 9
  df9 <- data.frame(
    model = c("temporal n = 9", "spatial n = 9"),
    psi = c(occ.res.t.9$BUGSoutput$mean$mean.psi, 
            occ.res.sp.9$BUGSoutput$mean$mean.psi),
    SD.psi = c(occ.res.t.9$BUGSoutput$sd$mean.psi, 
               occ.res.sp.9$BUGSoutput$sd$mean.psi),
    p = c(occ.res.t.9$BUGSoutput$mean$mean.p, 
          occ.res.sp.9$BUGSoutput$mean$mean.p),
    SD.p = c(occ.res.t.9$BUGSoutput$sd$mean.p, 
            occ.res.sp.9$BUGSoutput$sd$mean.p)
  )
  
  
################################################
  # Bunch o' crap I'm not using 
  
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
  # lions %>%
  #   group_by(plot) %>%
  #   summarise(l = any(lion.adult > 0 | lion.kitten > 0))
 # data <- lions %>%
 #   rename(present = otherpresent)