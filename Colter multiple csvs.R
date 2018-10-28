  # Loading CWD files and creating plots of prevalence and spread relative to culling
  # Colter Chitwood and Anna Moeller
  # 3/23/2018 (edited 4/5/18 and 4/9/18)

  # Load packages
  library(tidyverse)

  # # Load 1 data file
  # dat_raw <- read_csv("Data/Chariton County/Culling/no cull_100 runs/subregion_tharvestChariton County.csv",
  #                     skip = 2)
  
  # List all the csv files
  files_tharv <- list.files("Data", recursive = T, pattern = "subregion_tharvest")
  filepath_tharv <- file.path("Data", files_tharv)
  
  files_cwdinfdy <- list.files("Data", recursive = T, pattern = "cwdinfdy")
  files_cwdinfdy <- files_cwdinfdy[grep("Culling", files_cwdinfdy)]
  filepath_cwdinfdy <- file.path("Data", files_cwdinfdy)

  # Write a function that reads in the csvs and gets rid of all the crap
  decrapper_tharv_fn <- function(df){  
    df %>%
      # Get rid of short rows (e.g., seed-infection, 10)
      .[complete.cases(.),] %>% 
      
      # Add run number and year number
      mutate(first = SubregionArea == "SubregionArea", 
            run = cumsum(first)) %>%
      group_by(run) %>%

      # Get rid of extraneous column names
      mutate(index = cumsum(!first)) %>% 
      filter(index != 0) %>%
      ungroup()
  }
  
  # Write a function that reads in the csvs and gets rid of all the crap
  decrapper_cwdinfdy_fn <- function(df){  
    df %>%
      # Get rid of short rows (e.g., seed-infection, 10)
      .[complete.cases(.),] %>% 
      
      # Add run number and year number
      mutate(first = AdultMale == "AdultMale",
             run = cumsum(first)) %>%
      group_by(run) %>%
      
      # Get rid of extraneous column names
      mutate(index = cumsum(!first)) %>% 
      filter(index != 0) %>%
      ungroup()
  }
  
  # Bring in all the csv files together, run decrapper
  dat_tharv <- tibble(File = filepath_tharv) %>%
    extract(File, "Site", "/(.* County)/", remove = FALSE) %>%
    extract(File, "cullname", "Culling/(.* cull)_100 runs/", remove = FALSE) %>%
    mutate(Data = lapply(File, read_csv, skip = 2),
           Data = lapply(Data, decrapper_tharv_fn)) %>%
    
    # Make it into a prettier dataframe (no list column)
    unnest(Data) %>%
    mutate(CullRate = MaleFawnCullingRate) %>%
    select(cullname, Site, run, index, CullRate, grep("Culled", names(.)), CWDarea) %>%
    mutate_at(vars(CullRate:CWDarea), funs(as.numeric(.)))
  
  dat_cwdinfdy <- tibble(File = filepath_cwdinfdy) %>%
    extract(File, "Site", "/(.* County)/", remove = FALSE) %>%
    extract(File, "cullname", "Culling/(.* cull)_100 runs/", remove = FALSE) %>%
    mutate(Data = lapply(File, read_csv, skip = 2),
           Data = lapply(Data, decrapper_cwdinfdy_fn)) %>%
    
    # Make it into a prettier dataframe (no list column)
    unnest(Data) %>%
    rename(TotalCWDpos = `TotalCWD+`) %>%
    select(cullname, Site, run, index, TotalPreHarvestPop, TotalCWDpos, CWDArea) %>%
    mutate_at(vars(TotalPreHarvestPop:CWDArea), funs(as.numeric(.)))
    
  # Put the two together
  dat_org <- full_join(dat_tharv, dat_cwdinfdy, 
                       by = c("cullname" = "cullname",
                              "Site" = "Site", 
                              "run" = "run",
                              "index" = "index", 
                              "CWDarea" = "CWDArea"))
  
  # # View it
  # dat_org %>%
  #   select(Site, run, index, CullRate) %>%
  #   head(10)
  
  #########################
  # Troubleshooting
  #########################
  # 
  # # Find files where some runs do not have 5 rows 
  # dat_org %>%
  #   group_by(Site, MaleFawnCullingRate, run) %>%
  #   summarise(n = length(unique(index))) %>%
  #   filter(n != 5) %>%
  #   mutate(approxrow = 8 * (n-1) + 6)

  #########################
  # Actually doing analyses
  #########################
  
  # Make calculations
  dat_proc <- dat_org %>%
    mutate(preval = TotalCWDpos / TotalPreHarvestPop,
           TotalCull = AdultMaleCulled + YearlingMaleCulled + FawnMaleCulled + 
             AdultFemaleCulled + YearlingFemaleCulled + FawnFemaleCulled) %>%
    select(Site, run, index, CullRate, preval, TotalCull, CWDarea)
  
  # Means 
  dat_means <- dat_proc %>%
    group_by(Site, CullRate, index) %>%
    summarise(mean_prev = mean(preval),
              mean_num_culled = mean(TotalCull),
              mean_cwd_area = mean(CWDarea),
              n_runs = n(),
              se_prev = sd(preval),
              sd_num_culled = sd(TotalCull)) %>%
    mutate(ci_prev = 1.96*se_prev)
  
  ###########################
  # Make plots
  ###########################
  # Mean prevalence over the simulations (95% CIs)
  ggplot(dat_means, aes(x = index, 
                        y = mean_prev, 
                        color = factor(CullRate))) +
    geom_point(size = 3) +
    # geom_errorbar(aes(ymin = mean_prev - ci_prev, 
    #                   ymax = mean_prev + ci_prev), 
    #               width = 0.1, 
    #               position = position_dodge(0.1)) +
    geom_line() + 
    facet_wrap(~Site) + 
    #Pretty 
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) + 
    labs(color = "Culling Rate") + 
    xlab("Year") + 
    ylab("Prevalence") +
    ggtitle("Mean Prevalence") + 
    theme(plot.title = element_text(hjust = 0.5))
  
  # Mean number of deer culled per year with STANDARD DEVIATION
  ggplot(dat_means, aes(x = index, 
                        y = mean_num_culled, 
                        color = factor(CullRate))) +
    geom_point(size = 3, shape = 17) +
    # geom_errorbar(aes(ymin = mean_num_culled - sd_num_culled,
    #                  ymax = mean_num_culled + sd_num_culled),
    #              width = 0.7,
    #              position = position_dodge(0)) +
    geom_line() + 
    facet_wrap(~Site) + 
    # Pretty
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) + 
    labs(color = "Culling Rate") + 
    xlab("Year") + 
    ylab("Number Culled") + 
    ggtitle("Mean Number of Deer Culled") + 
    theme(plot.title = element_text(hjust = 0.5))
  
  # Plot CWD area
  ggplot(dat_means, aes(x = index, 
                        y = mean_cwd_area, 
                        color = factor(CullRate))) +
    geom_point(size = 3, shape = 15) +
    # geom_errorbar(aes(ymin = mean_num_culled - sd_num_culled,
    #                   ymax = mean_num_culled + sd_num_culled),
    #               width = 0.7,
    #               position = position_dodge(0)) +
    geom_line() + 
    facet_wrap(~Site) + 
    # Make it pretty
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank()) + 
    labs(color = "Culling Rate") + 
    xlab("Year") + 
    ylab("Square Miles") + 
    ggtitle("Mean CWD Positive Area") + 
    theme(plot.title = element_text(hjust = 0.5))
