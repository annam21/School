  # Final project
  # Anna Moeller
  # 11/30/2016

  # Packages
  source("C:/Users/anna.moeller/Documents/GitHub/packages.R")
  # I think this only needs dplyr
  
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
 
####################################################
  # Temporal replicates 
############################################

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
  ni <- 25000
  nt <- 1
  nb <- 15000
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
  
######################################
  # Spatial replicates
######################################

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
  ni <- 25000
  nt <- 1
  nb <- 15000
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

########################################
  # Temporal and spatial replicates
########################################
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
  ni <- 25000
  nt <- 1
  nb <- 15000
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
  
#################################
  # Results table
  df <- data.frame(
    model = c("temporal", "spatial", "spatiotemporal"),
    psi = c(occ.res.t$BUGSoutput$mean$mean.psi, 
            occ.res.sp$BUGSoutput$mean$mean.psi, 
            occ.res.sptp$BUGSoutput$mean$mean.psi),
    SD.psi = c(occ.res.t$BUGSoutput$sd$mean.psi, 
           occ.res.sp$BUGSoutput$sd$mean.psi, 
           occ.res.sptp$BUGSoutput$sd$mean.psi),
    p = c(occ.res.t$BUGSoutput$mean$mean.p, 
          occ.res.sp$BUGSoutput$mean$mean.p, 
          occ.res.sptp$BUGSoutput$mean$mean.p),
    SD.p = c(occ.res.t$BUGSoutput$sd$mean.p, 
           occ.res.sp$BUGSoutput$sd$mean.p, 
           occ.res.sptp$BUGSoutput$sd$mean.p),
    theta = c(NA, 
              NA, 
              occ.res.sptp$BUGSoutput$mean$mean.theta),
    SD.theta = c(NA, 
                 NA, 
                 occ.res.sptp$BUGSoutput$sd$mean.theta)
    )
 