## Site-level climate-AGBI relationships using linear models

rm(list = ls())

# Load AGBI
# Choose detrended or trended
load('out/site_detrended_AGBI.RData')
#load('out/site_detrended_AGBI.RData')

# Rename trended dataframe
if(exists('site_agbi')) site_save_comb <- site_agbi
# Rename response column
if(exists('site_agbi')) site_save_comb <- dplyr::rename(site_save_comb, residual_AGBI = mean)

# Average over taxa and plots
save_comb <- site_save_comb 

# Indexing for loops
site <- c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA', 'HARVARD Model RW', 'HARVARD Model RW + Census')

# Load climate data
load('climate/prism_clim.RData')

# Duplicate Harvard climate
prism_harv <- dplyr::filter(prism_long, loc == 'HARVARD')
prism_long <- dplyr::mutate(prism_long, loc = dplyr::if_else(loc == 'HARVARD', 'HARVARD Model RW', loc))
prism_long <- rbind(prism_long, prism_harv)
prism_long <- dplyr::mutate(prism_long, loc = dplyr::if_else(loc == 'HARVARD', 'HARVARD Model RW + Census', loc))

# Format
prism_growing <- prism_long |> 
  dplyr::mutate(year = as.numeric(year)) |>
  dplyr::mutate(growing_year = dplyr::if_else(month %in% c('01', '02', '03', '04', 
                                                           '05', '06', '07', '08'),
                                              year, year + 1)) |>
  dplyr::group_by(growing_year, loc) |>
  dplyr::summarize(PPT = mean(PPT2),
                   Tmean = mean(Tmean2),
                   sd_PPT = sd(PPT2),
                   sd_Tmean = sd(Tmean2),
                   Tmin = min(Tmin2),
                   Tmax = max(Tmax2),
                   Vpdmin = min(Vpdmin2),
                   Vpdmax = max(Vpdmax2)) |>
  dplyr::rename(year = growing_year,
                site = loc)

# Storage
coeff_save_site <- matrix(, nrow = length(site), ncol = 11)

##############################################################################
# for each site, run all detrended trees through climwin
##############################################################################

# format dates for prism.long
prism_long$Date <- paste0("23/", prism_long$month,"/", prism_long$year) # needs to be in dd/mm/yyyy

# format dates for the tree AGBI 
site_save_comb$Date <- paste0("31/10/", site_save_comb$year)


site.nm <- "ROOSTER"

# this function selects AGBI from each site and runs climwin, generates plots and saves the outputs
# I just selected moving window for the last 12 months, but changing the range to the slidingwin
# function to c(24,0) would run the moving window correlations for the last two years
runclimwin.site <- function(site.nm){
  prism_site <- prism_long %>% filter(loc %in% site.nm)
  comb_site <- site_save_comb %>% filter(site %in% site.nm )
  
  ROwin <- slidingwin(xvar = list(ppt = prism_site$PPT2, 
                                  tave = prism_site$Tmean2, 
                                  tmin = prism_site$Tmin2, 
                                  tmax = prism_site$Tmax2, 
                                  vpdmax = prism_site$Vpdmax2), 
                      cdate = prism_site$Date, 
                      bdate = comb_site$Date, 
                      baseline = lm(residual_AGBI ~ 1  , data = comb_site),
                      cinterval = "month", 
                      #cmissing = "method1",
                      range = c(18,0), # go back to previous march
                      type = "absolute", 
                      refday = c(31, 10), # october 31st is the reference date 
                      stat = "mean", 
                      func = "lin") 
  PPTOutput <- ROwin[[1]]$Dataset
  TaveOutput <- ROwin[[2]]$Dataset
  TminOutput <- ROwin[[3]]$Dataset
  TmaxOutput <- ROwin[[4]]$Dataset
  VPDmaxOutput <- ROwin[[5]]$Dataset
  
  # plot up the heat maps--note that x and y are months before October
  Ppt.heat <- plotdelta(dataset = PPTOutput)+ylab("Window open \n (Months before October)")+
    xlab("Window close \n (Months before October)")+
    ggtitle(paste0("Delta AIC for Precipitation, \n", site.nm, ", ",  taxon.id))
  
  Tave.heat <- plotdelta(dataset = TaveOutput)+ylab("Window open \n (Months before October)")+
    xlab("Window close \n (Months before October)")+
    ggtitle(paste0("Delta AIC for Avg. Temperature, \n", site.nm, ", ",  taxon.id ))
  
  
  Tmin.heat <- plotdelta(dataset = TminOutput)+ylab("Window open \n (Months before October)")+
    xlab("Window close \n (Months before October)")+
    ggtitle(paste0("Delta AIC for Min. Temperature,\n", site.nm, ", ",  taxon.id ))
  
  Tmax.heat <- plotdelta(dataset = TmaxOutput)+ylab("Window open \n (Months before October)")+
    xlab("Window close \n (Months before October)")+
    ggtitle(paste0("Delta AIC for Max. Temperature,\n", site.nm, ", ",  taxon.id))
  
  
  VPDmax.heat <- plotdelta(dataset = VPDmaxOutput)+ylab("Window open \n (Months before October)")+
    xlab("Window close \n (Months before October)")+
    ggtitle(paste0("Delta AIC for Max. VPD, \n", site.nm, ", ",  taxon.id ))
  
  cowplot::plot_grid(Ppt.heat, Tave.heat, 
                     Tmin.heat, Tmax.heat, 
                     VPDmax.heat, align = "hv", ncol = 3)
  ggsave(height = 8.5, width = 12, units = "in", paste0("out/climwin/AGBI_site/AGBI_site_climwin_",site.nm,".png"))
  
  # save all the outputs for model selection in systematic way:
  save(PPTOutput, 
       TaveOutput, 
       TminOutput, 
       TmaxOutput, 
       VPDmaxOutput,
       file = paste0("out/climwin/AGBI_site/deltaAICc_AGBI_site_clim_",site.nm,".Rdata"))
}

# run this function for each taxon at each site
for(s in 1:length(unique(site_save_comb$site))){
  taxon_site_comb <- site_save_comb %>% filter(site %in% unique(site_save_comb$site)[s])
 
  

    runclimwin.site(site.nm = unique(site_save_comb$site)[s])
    
    
  
  
}

########################################################################################
# read in the output and summarise the top models for each site
########################################################################################
# note that this only picks the single best fit model, not necessarily all possible windows that fit well
# but this file might be useful to compare to Alyssa's climate correlations to converge on ideal climate ranges for each site
best.fit.dAICc.taxon <- function(site.nm){
  load(paste0("out/climwin/AGBI_site/deltaAICc_AGBI_site_clim_", site.nm, ".Rdata"))
  
  # get the Ppt best filtresults
  
  window.best.fit <- data.frame(Site = site.nm, 
                                #taxon = taxon.id,
                                Climate = c("Ppt", "Tave", "Tmax", "Tmin", "VPDmax"), 
                                WindowOpen = c(PPTOutput[1,]$WindowOpen, TaveOutput[1,]$WindowOpen, 
                                               TmaxOutput[1,]$WindowOpen, TminOutput[1,]$WindowOpen, VPDmaxOutput[1,]$WindowOpen), 
                                WindowClose = c(PPTOutput[1,]$WindowClose, TaveOutput[1,]$WindowClose, 
                                                TmaxOutput[1,]$WindowClose, TminOutput[1,]$WindowClose, VPDmaxOutput[1,]$WindowClose))
  window.best.fit <- window.best.fit %>% mutate(Month.start = 19 - WindowOpen, 
                                                Month.end = 19 - WindowClose)
  
  
  window.best.fit
}

# run over the sites and taxon here to get a big summary:
taxon.climate.vars <- site.climate.vars <- list()
for(s in 1:length(unique(site_save_comb$site))){
  taxon_site_comb <- site_save_comb %>% filter(site %in% unique(site_save_comb$site)[s])
  site.climate.vars[[s]]  <-  best.fit.dAICc.taxon(site.nm = unique(site_save_comb$site)[s])

  
}

site.climates <- do.call(rbind, site.climate.vars)
write.csv(site.climates, "out/climwin/climwin_site_summary.csv", row.names = FALSE)
