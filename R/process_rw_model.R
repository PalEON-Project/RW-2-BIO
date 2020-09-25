## Step 3: Process model output into biomass estimates 

## This script processes the output of the stat model into a format that is generally useful in PalEON and for the PEcAn workflow
## It will automatically process the data with and without the sampling correction depending on the site configuration file.

process_rw_model <- function(census_site, mvers, dvers, site, nest,
                             finalyr = NULL, keep = 250, plot_radius = NULL){
  
  ###############################################################
  ################ 1. Prepare workspace and data ################
  ###############################################################
  
  # load needed libraries
  library(ggplot2)
  library(reshape2)
  library(abind)
  library(dplyr)
  
  # create save folders for data 
  site_dir <- file.path('sites',site)

  # decide how many models you need
  if (census_site){
    fnames = paste0(c('ring_model_t_pdbh_sigd_STAN', 'ring_model_t_pdbh_STAN'), '_', 
                    site, '_',mvers,'_',dvers,'.RDS')
    models = c('Model RW', 'Model RW + Census')
  }else{
    fnames = paste0(c('ring_model_t_pdbh_sigd_STAN'), '_', site, '_',mvers,'_',dvers,'.RDS')
    models = c('Model RW')
  }

  # extract stat model output 
  output_dir = file.path(site_dir,'runs',paste0(mvers,'_',dvers),'output')
  nmodels = length(fnames)
  
  # we only need "D" for diameter and we only need the iterations that we are planning to keep
  post = list()
  for (i in 1:length(fnames)) {
    fname_model = fnames[i]
    out = readRDS(paste0(output_dir,'/', fname_model))
    post[[i]] = out$D[(dim(out$D)[1]-keep+1):(dim(out$D)[1]),]
  }  
  rm(out)
  
  # load built data for site 
  dat = readRDS(file.path(site_dir,'runs',paste0(mvers,'_',dvers),'input',paste0('tree_data_', site ,'_STAN_',mvers,'_', dvers, '.RDS')))
  N_years = dat$N_years
  N_Tr = dat$N_Tr
  X2Tr = dat$X2Tr
  X2year = dat$X2year
  Tr = dat$Tr %>% arrange(stat_id)
  taxon = Tr$taxon
  plot = Tr$plot 
  years = dat$years
  distance = Tr$distance
  
  if (is.null(finalyr)) finalyr = max(years)
  
  if (census_site){
    N_C = dat$N_C
    X2C = dat$X2C
    X2year_C = dat$X2year_C
    
    allTrees = dat$allTrees %>% arrange(stat_id)
    taxon_C = allTrees$taxon
    plot_C = allTrees$plot
    distance = allTrees$distance
  }
  
  # match species acronyms to level3a available species/pfts 
  choj = read.csv('data/acronym_to_chojnacky_v0.1.csv', stringsAsFactors = FALSE)
  if (!census_site){
    choj = choj %>% filter(acronym %in% unique(taxon))
  }else{
    choj = choj %>% filter(acronym %in% unique(taxon_C))
  }
  
  #####################################################
  ################ 2. Estimate biomass ################
  #####################################################
  
  # first for RW only model (no census)
  dbh_array = array(NA, dim = c(N_Tr, N_years, keep))
  agb_array = array(NA, dim = c(N_Tr, N_years, keep))
  
  for (t in 1:N_Tr){
    
    # determine which estimates correspond to this tree
    inds = which(X2Tr == t)
    yrinds = X2year[inds]
    
    # extract diameter data
    dbh_array[t,yrinds,] = t(post[[1]][,inds])
    
    # get equation coefficients based on taxon
    beta0 = choj$beta0[which(choj$acronym == taxon[t])]
    beta1 = choj$beta1[which(choj$acronym == taxon[t])]
    
    # use biomass equation to estimate biomass from diameter
    agb_array[t,,] = exp(beta0 + beta1 * log(dbh_array[t,,]))
  }
  
  # next for RW + Census Model (if applicable)
  if (census_site){
    
    dbh_array_C = array(NA, dim = c(N_C, N_years, keep))
    agb_array_C = array(NA, dim = c(N_C, N_years, keep))
    
    for (t in 1:N_C){
      
      # determine which estimates correspond to this tree
      inds = which(X2C == t)
      yrinds = X2year_C[inds]
      
      # extract diameter data
      dbh_array_C[t,yrinds,] = t(post[[2]][,inds])
      
      # get equation coefficients based on taxon
      beta0 = choj$beta0[which(choj$acronym == taxon_C[t])]
      beta1 = choj$beta1[which(choj$acronym == taxon_C[t])]
      
      # use biomass equation to estimate biomass from diameter
      agb_array_C[t,,] = exp(beta0 + beta1 * log(dbh_array_C[t,,]))
    }
  }
  
  rm(post)

  ####################################################################
  ################ 3. Remove biomass from small trees ################
  ####################################################################

  # first for RW only model
  for (tree in 1:N_Tr){
    for (year in 1:N_years){
      
      # determine mean DBH for this year and tree
      dbh_mean = mean(dbh_array[tree, year, ], na.rm=TRUE)
  
      # if smaller than 5 cm., eliminate the data 
      if (is.na(dbh_mean) | dbh_mean >= 5) next
      dbh_array[tree, year, ] = rep(NA, keep)
      agb_array[tree, year, ] = rep(NA, keep)
    }
  }
  
  # second for RW + Census model
  if (census_site){
    
    for (tree in 1:N_C){
      for (year in 1:N_years){
        
        # determine mean DBH for this year and tree
        dbh_mean = mean(dbh_array_C[tree, year, ], na.rm=TRUE)
        
        # if smaller than 5 cm., eliminate the data 
        if (is.na(dbh_mean) | dbh_mean >= 5) next
        dbh_array_C[tree, year, ] = rep(NA, keep)
        agb_array_C[tree, year, ] = rep(NA, keep)
      }
    }
  }
  
  ###############################################################################
  ################ 4. Apply census smoothing (RW + CENSUS MODEL) ################
  ###############################################################################
  
  # If the census is the final record of a tree and the tree was not alive for all of the censuses, 
  # we need to determine the year in which the tree died stochastically since it could have been anytime 
  # between the censuses.
  
  if (census_site){
    
    # determine which trees we need to consider for smoothing (all those marked as "dead" in finalCond)
    smoothIDs = unique(dat$Dobs$stat_id[which(dat$Dobs$finalCond == 'dead')])
    
    # loop through all of the trees 
    for (id in smoothIDs){
      
      # get last census year
      cYr = max(dat$Dobs$year[which(dat$Dobs$stat_id == id)]) 
      if (cYr == -Inf) next
      
      # get last data year 
      dYr = X2year_C[dat$idx_C$lastidx[which(dat$idx_C$stat_id == id)]]
      
      timeRange = seq(cYr, dYr)
      
      # loop through all iterations and stochastically choose the last year the tree lived  
      for (k in 1:keep){
       lYr = sample(timeRange, 1)
       if (lYr != dYr) agb_array_C[id,((lYr+1):dYr),k] = rep(NA, length(c((lYr+1):dYr)))
      }
    }
  }
  
  ################################################################
  ################ 5. Calculate biomass increment ################
  ################################################################
  
  # determine biomass increment
  
  # first for RW only model
  abi = apply(agb_array, c(1,3), function(x) diff(x))
  abi = aperm(abi, c(2, 1, 3))
  abi_melt = melt(abi)
  colnames(abi_melt) = c('tree', 'year', 'iter', 'value')
  abi_melt = abi_melt %>% filter(!is.na(value))
  abi_melt$year = years[abi_melt$year]
  abi_melt$plot = plot[abi_melt$tree]
  abi_melt$taxon = taxon[abi_melt$tree]
  abi_melt$model = rep("Model RW", nrow(abi_melt))
  abi_melt$type = rep('abi',nrow(abi_melt))
  rm(abi)
  
  # then for RW + census model 
  if (census_site){
    abi_C = apply(agb_array_C, c(1,3), function(x) diff(x))
    abi_C = aperm(abi_C, c(2, 1, 3))
    abi_melt_C = melt(abi_C)
    colnames(abi_melt_C) = c('tree', 'year', 'iter', 'value')
    abi_melt_C = abi_melt_C %>% filter(!is.na(value))
    abi_melt_C$year = years[abi_melt_C$year]
    abi_melt_C$plot = plot_C[abi_melt_C$tree]
    abi_melt_C$taxon = taxon_C[abi_melt_C$tree]
    abi_melt_C$model = rep("Model RW + Census", nrow(abi_melt_C))
    abi_melt_C$type = rep('abi',nrow(abi_melt_C))
    abi_melt = rbind(abi_melt, abi_melt_C)
    rm(abi_C,abi_melt_C)
  }
  
  ###################################################################
  ################ 6. Organize data into data frames ################
  ###################################################################

  # melt down dbh_array to data frame
  dbh_melt = melt(dbh_array)
  colnames(dbh_melt) = c('tree','year','iter','value')
  dbh_melt = dbh_melt %>% filter(!is.na(value))
  dbh_melt$year = years[dbh_melt$year]
  dbh_melt$plot = plot[dbh_melt$tree]
  dbh_melt$taxon = taxon[dbh_melt$tree]
  dbh_melt$model = rep("Model RW", nrow(dbh_melt))
  dbh_melt$type = rep('dbh',nrow(dbh_melt))
  rm(dbh_array)
  
  # melt down agb_array to data frame
  agb_melt = melt(agb_array)
  colnames(agb_melt) = c('tree','year','iter','value')
  agb_melt = agb_melt %>% filter(!is.na(value))
  agb_melt$year = years[agb_melt$year]
  agb_melt$plot = plot[agb_melt$tree]
  agb_melt$taxon = taxon[agb_melt$tree]
  agb_melt$model = rep("Model RW", nrow(agb_melt))
  agb_melt$type = rep('ab',nrow(agb_melt))
  
  if (census_site){
    
    # melt down dbh_array_C to data frame
    dbh_melt_C = melt(dbh_array_C)
    colnames(dbh_melt_C) = c('tree','year','iter','value')
    dbh_melt_C = dbh_melt_C %>% filter(!is.na(value))
    dbh_melt_C$year = years[dbh_melt_C$year]
    dbh_melt_C$plot = plot_C[dbh_melt_C$tree]
    dbh_melt_C$taxon = taxon_C[dbh_melt_C$tree]
    dbh_melt_C$model = rep("Model RW + Census", nrow(dbh_melt_C))
    dbh_melt_C$type = rep('dbh',nrow(dbh_melt_C))
    dbh_melt = rbind(dbh_melt, dbh_melt_C)
    rm(dbh_array_C,dbh_melt_C)
    
    # melt down agb_array_C to data frame
    agb_melt_C = melt(agb_array_C)
    colnames(agb_melt_C) = c('tree','year','iter','value')
    agb_melt_C = agb_melt_C %>% filter(!is.na(value))
    agb_melt_C$year = years[agb_melt_C$year]
    agb_melt_C$plot = plot_C[agb_melt_C$tree]
    agb_melt_C$taxon = taxon_C[agb_melt_C$tree]
    agb_melt_C$model = rep("Model RW + Census", nrow(agb_melt_C))
    agb_melt_C$type = rep('ab',nrow(agb_melt_C))
    agb_melt = rbind(agb_melt, agb_melt_C)
    rm(agb_melt_C)
  }
  
  # remove incomplete rw/census years if applicable 
  agb_melt = agb_melt %>% filter(year <= finalyr)
  abi_melt = abi_melt %>% filter(year <= finalyr)
  dbh_melt = dbh_melt %>% filter(year <= finalyr)
  
  ####################################################################
  ################ 7. Save individual-level RDS files ################
  ####################################################################
  
  # save AGB RDS and CSV files
  filename = file.path(output_dir,paste0('AGB_STAN_',site,'_',mvers,'_',dvers))
  saveRDS(agb_melt, paste0(filename,'.RDS'))
  
  # save AGBI RDS and CSV files
  filename2 = file.path(output_dir,paste0('AGBI_STAN_',site,'_',mvers,'_',dvers))
  saveRDS(abi_melt, paste0(filename2,'.RDS'))
  
  # save DBH RDS and CSV files
  filename3 = file.path(output_dir,paste0('DBH_STAN_',site,'_',mvers,'_',dvers))
  saveRDS(dbh_melt, paste0(filename3,'.RDS'))
  
  ################################################################################
  ################ 8. Perform sampling correction (RW ONLY MODEL) ################
  ################################################################################
  
  # sampling correction adjusts biomass values to account for the PalEON sampling method 
  
  # determine the measured diameter of trees at time of coring
  pdbh = exp(dat$logTr)
  
  # option to not apply the sampling correction
  if (nest == 'nofix'){
    agb_array = agb_array * (1/(pi*plot_radius^2)) * (1/0.0001) * (1/1000)
  }
  
  # double-nested plots 
  if (nest == 'double'){
    idx_small  = which(pdbh<20)
    idx_large = which(pdbh>=20)
    
    # we need to adjust the biomass units from kg/plot to Mg/ha
    inner_factor = (1 / (pi*13^2)) * (1/0.0001) * (1/1000)
    outer_factor = (1 / (pi*20^2)) * (1/0.0001) * (1/1000)
    agb_array[idx_small,,] = agb_array[idx_small,,] * inner_factor
    agb_array[idx_large,,] = agb_array[idx_large,,] * outer_factor
  }
  
  # triple-nested plots
  if (nest == 'triple'){
    idx_small = which(pdbh<20)
    idx_med = which((pdbh>=20) & (pdbh<30))
    idx_large = which(pdbh>=30)
    
    # we need to adjust the biomass units from kg/plot to Mg/ha
    inner_factor = (1 / (pi*13^2)) * (1/0.0001) * (1/1000)
    mid_factor = (1 / (pi*20^2)) * (1/0.0001) * (1/1000)
    outer_factor = (1 / (pi*30^2)) * (1/0.0001) * (1/1000)
    agb_array[idx_small,,] = agb_array[idx_small,,] * inner_factor
    agb_array[idx_med,,] = agb_array[idx_med,,] * mid_factor
    agb_array[idx_large,,] = agb_array[idx_large,,] * outer_factor
  }
  
  if (census_site){
    if (nest == 'double') {
      agb_array_C = agb_array_C * (1 / (pi*20^2)) * (1/0.0001) * (1/1000)
    }
    if (nest == 'triple') {
      agb_array_C = agb_array_C * (1 / (pi*30^2)) * (1/0.0001) * (1/1000)
    }
    if (nest == 'nofix') {
      agb_array_C = agb_array_C * (1 / (pi*plot_radius^2)) * (1/0.0001) * (1/1000)
    }
  }
  
  # recreate agb data frame using corrected array 
  agb_melt = melt(agb_array)
  colnames(agb_melt) = c('tree','year','iter','value')
  agb_melt = agb_melt %>% filter(!is.na(value))
  agb_melt$year = years[agb_melt$year]
  agb_melt$plot = plot[agb_melt$tree]
  agb_melt$taxon = taxon[agb_melt$tree]
  agb_melt$model = rep("Model RW", nrow(agb_melt))
  agb_melt$type = rep('ab',nrow(agb_melt))
  #rm(agb_array)
  
  if (census_site){
    # melt down agb_array_C to data frame
    agb_melt_C = melt(agb_array_C)
    colnames(agb_melt_C) = c('tree','year','iter','value')
    agb_melt_C = agb_melt_C %>% filter(!is.na(value))
    agb_melt_C$year = years[agb_melt_C$year]
    agb_melt_C$plot = plot_C[agb_melt_C$tree]
    agb_melt_C$taxon = taxon_C[agb_melt_C$tree]
    agb_melt_C$model = rep("Model RW + Census", nrow(agb_melt_C))
    agb_melt_C$type = rep('ab',nrow(agb_melt_C))
    agb_melt = rbind(agb_melt, agb_melt_C)
    #rm(agb_melt_C, agb_array_C)
  }
  
  ##############################################################################
  ################ 9. Save taxon-level total aboveground biomass ################
  ##############################################################################

  # sum annual biomass across taxa for each year, iteration, and plot 
  # also remove incomplete final years if applicable
  agb_taxa = agb_melt %>%
    filter(year <= finalyr) %>%
    group_by(taxon, year, plot, iter, model) %>% 
    summarize(ab = sum(value))
  
  # save file
  filename4 = file.path(output_dir,paste0('AGB_TAXA_STAN_',site,'_',mvers,'_',dvers))
  saveRDS(agb_taxa, paste0(filename4,'.RDS'))
  
  #####################################################################################################
  ################ 10. Calculate approximate diameters from measured DBH and increments ################
  #####################################################################################################
  
  # this section calculates aboveground biomass based on the RW and the measured DBH values 
  dbh_data = matrix(NA, N_Tr, N_years)
  agb_data = matrix(NA, N_Tr, N_years)
  
  for (i in 1:N_Tr){
    print(i)
    
    # get data for this tree
    data_now = dat$Xobs %>% filter(stat_id == i)
    dbh_now = pdbh[i]
    taxon_now = taxon[i]
    yrs = sort(unique(data_now$year), decreasing = TRUE)
    
    # set diameter at time of coring
    dbh_data[i,yrs[1]] = dbh_now
    dbh_last = dbh_now
    
    # loop through years with available data
    for (j in 2:length(yrs)){
      yr = yrs[j]
      incr = min((data_now %>% filter(year == yr))$incr, na.rm = TRUE)
      dbh_temp = dbh_last - (2 * incr/10)
      
      # make sure diameter is not less than 5 cm
      if (dbh_temp < 5){
        dbh_last = 0 
      }else{
        dbh_data[i,yr] = dbh_temp 
        dbh_last = dbh_temp
      }
    }
    
    # then determine biomass based on chojnacky equations for this species
    beta0 = choj$beta0[which(choj$acronym == taxon_now)]
    beta1 = choj$beta1[which(choj$acronym == taxon_now)]
    agb_data[i,] = exp(beta0 + beta1 * log(dbh_data[i,]))
  }
  
  # rescale for sampling correction
  
  # if we do not want to apply sampling correction
  if (nest == 'nofix'){
    agb_data = agb_data * (1/(pi*30^2)) * (1/0.0001) * (1/1000)
  }
  
  # double-nested plots 
  if (nest == 'double'){
    agb_data[idx_small,] = agb_data[idx_small,] * inner_factor
    agb_data[idx_large,] = agb_data[idx_large,] * outer_factor
  }
  
  # triple-nested plots (most other plots)
  if (nest == 'triple'){
    agb_data[idx_small,] = agb_data[idx_small,] * inner_factor
    agb_data[idx_med,] = agb_data[idx_med,] * mid_factor
    agb_data[idx_large,] = agb_data[idx_large,] * outer_factor
  }

  # melt to data frames
  data_melt = melt(agb_data)
  colnames(data_melt) = c("tree", "year", "value")
  data_melt = data_melt %>% filter(!is.na(value))
  data_melt$year = years[data_melt$year]
  data_melt$plot = plot[data_melt$tree]
  data_melt$taxon = taxon[data_melt$tree]
  data_melt$type = rep('ab',nrow(data_melt))
  rm(agb_data, dbh_data, data_now)
  
  # remove incomplete years 
  data_melt = data_melt %>% filter(year <= finalyr)
  
  #############################################
  ################ 11. Figures ################
  #############################################
  
  # determine quantiles for graphing 
  agb_plot = agb_taxa %>%
    group_by(model, plot, taxon, year) %>%
    summarize(ab025 = quantile(ab, 0.025),
              ab50 = quantile(ab, 0.5),
              ab975 = quantile(ab, 0.975))
  
  sum_plot = agb_taxa %>%
    group_by(model, plot, year, iter) %>%
    summarize(ab = sum(ab)) %>%
    ungroup() %>% 
    group_by(model, plot, year) %>%
    summarize(ab025 = quantile(ab, 0.025),
              ab50 = quantile(ab, 0.5),
              ab975 = quantile(ab, 0.975))
  
  data_pft_plot = data_melt %>% 
    group_by(year, plot, taxon) %>% 
    summarize(ab = sum(value))
  
  data_plot = data_melt %>%
    group_by(year, plot) %>%
    summarize(ab = sum(value))
  
  # figure to compare biomass by PFT for each plot 
  pl1 = ggplot() + 
    geom_line(data = agb_plot, aes(x = year, y = ab50, group = taxon, color = taxon)) + 
    geom_ribbon(data = agb_plot, aes(x = year, ymin = ab025, ymax = ab975, 
                                     fill = taxon, group = taxon, color = taxon), alpha = 0.4) +
    #geom_line(data = data_pft_plot, aes(x = year, y = ab, group = taxon)) +
    facet_grid(~plot~as.factor(model)) + 
    theme_bw() + 
    labs(x = 'Year', y = 'Biomass (Mg/ha)', color = 'Species', fill = 'Species',
         title = 'Aboveground Biomass by PFT')
  ggsave(pl1, filename = file.path(site_dir,'runs',paste0(mvers,'_',dvers),'figures','processed_pft_AGB.jpg'))
  
  # figure to compare total biomass for each plot 
  pl2 = ggplot() + 
    geom_line(data = sum_plot, aes(x = year, y = ab50, 
                                   group = model, color = model)) + 
    geom_ribbon(data = sum_plot, aes(x = year, ymin = ab025, ymax = ab975, 
                                     group = model, color = model, fill = model), alpha = 0.4) +
    geom_line(data = data_plot, aes(x = year, y = ab)) + 
    facet_wrap(~plot) + 
    theme_bw() +
    labs(x = 'Year', y = 'Biomass (Mg/ha)', color = 'Model', fill = 'Model', 
         title = "Total Aboveground Biomass by Plot") 
  ggsave(pl2, filename = file.path(site_dir,'runs',paste0(mvers,'_',dvers),'figures','processed_total_plot_AGB.jpg'))
  
  # figure to compare total site biomass 
  sum_site = agb_taxa %>%
    group_by(model, plot, year, iter) %>%
    summarize(ab = sum(ab)) %>% 
    ungroup() %>% 
    group_by(model, iter,  year) %>%
    summarize(ab = mean(ab)) %>% 
    ungroup() %>% 
    group_by(model, year) %>% 
    summarize(ab025 = quantile(ab, 0.025),
              ab50 = quantile(ab, 0.5),
              ab975 = quantile(ab, 0.975))
  
  data_site = data_plot %>% 
    group_by(year) %>%
    summarize(ab = mean(ab))

  pl3 = ggplot(sum_site) +
    geom_line(data = sum_site, aes(x = year, y = ab50, 
                                   group = model, color = model)) + 
    geom_ribbon(data = sum_site, aes(x = year, ymin = ab025, ymax = ab975, 
                                     group = model, color = model, fill = model), alpha = 0.4) +
    geom_line(data = data_site, aes(x = year, y = ab)) + 
    theme_bw() +
    labs(x = 'Year', y = 'Biomass (Mg/ha)', color = 'Model', fill = 'Model', 
         title = "Total Aboveground Biomass") 
  ggsave(pl3, filename = file.path(site_dir,'runs',paste0(mvers,'_',dvers),'figures','processed_total_site_AGB.jpg'))
  
  # figure to show cumulative biomass contribution across species and demonstrate which species are most important to site 
  prior_mat <- agb_taxa %>% 
    group_by(year, model, plot, taxon) %>%
    summarize(ab = mean(ab)) %>%
    ungroup() %>% 
    group_by(year, model, taxon) %>% 
    summarize(ab = mean(ab)) %>%
    ungroup() %>%
    group_by(model, taxon) %>%
    summarize(contr = sum(ab)) %>%
    arrange(model, desc(contr))
  
  # first,  let's look at RW model 
  prior_mat1 = prior_mat %>% filter(model == 'Model RW')
  prior_mat1$perc = prior_mat1$contr/sum(prior_mat1$contr,na.rm=T)
  prior_mat1$cumsum = cumsum(prior_mat1$perc)
  prior_mat1$taxon = factor(x = prior_mat1$taxon, levels = prior_mat1$taxon)
  pl4 = ggplot(prior_mat1) +
    geom_point(aes(x=taxon, y=cumsum)) + 
    geom_hline(yintercept = 0.98, col = 'red') + 
    labs(title = 'overall cumulative proportion of biomass by species - Model RW', 
         y = 'cumulative proportion of biomass', 
         x = 'species')
  ggsave(pl4, filename = file.path(site_dir,'runs',paste0(mvers,'_',dvers),'figures','98_biomass_modelRW.jpg'))
  
  # then RW + Census model if applicable 
  if (census_site){
    prior_mat2 = prior_mat %>% filter(model == 'Model RW + Census')
    prior_mat2$perc = prior_mat2$contr/sum(prior_mat2$contr,na.rm=T)
    prior_mat2$cumsum = cumsum(prior_mat2$perc)
    prior_mat2$taxon = factor(x = prior_mat2$taxon, levels = prior_mat2$taxon)
    pl5 = ggplot(prior_mat2) +
      geom_point(aes(x=taxon, y=cumsum)) + 
      geom_hline(yintercept = 0.98, col = 'red') + 
      labs(title = 'overall cumulative proportion of biomass by species - Model RW + Census', 
           y = 'cumulative proportion of biomass', 
           x = 'species')
    ggsave(pl5, filename = file.path(site_dir,'runs',paste0(mvers,'_',dvers),'figures','98_biomass_modelRW_census.jpg'))
  }
}
