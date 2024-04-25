## Step 3: Process model output into biomass estimates 

## This script processes the output of the stat model into a format that is generally useful in PalEON and for the PEcAn workflow
## It will automatically process the data with and without the sampling correction depending on the site configuration file.

# Need to add plot_radius if no sampling correction 

validate_rw_model <- function(census_site, mvers, dvers, site, nest,
                              keep = 250, pool = 250, nchains = 3){
  
  ###############################################################
  ################ 1. Prepare workspace and data ################
  ###############################################################
  
  # create save folders for data 
  site_dir <- file.path('sites',site)
  
  # decide how many models you need
  if (census_site){
    fnames = paste0(c('ring_model_t_pdbh_sigd_STAN', 'ring_model_t_pdbh_STAN'), '_', 
                    site, '_',mvers,'_',dvers,'.RDS')
    models = c('Model RW', 'Model RW + Census')
  }else{
    # fnames = paste0(c('ring_model_t_pdbh_sigd_STAN'), '_', site, '_',mvers,'_',dvers,'.RDS')
    fnames = paste0(c('ring_model_t_pdbh_sigd_species_STAN'), '_', site, '_',mvers,'_',dvers,'.RDS')
    models = c('Model RW')
  }
  
  # extract stat model output 
  output_dir = file.path(site_dir,'runs',paste0(mvers,'_',dvers),'output')
  nmodels = length(fnames)
  
  # we only need "D" for diameter and we only need the iterations that we are planning to keep
  post = list()
  post_rw = list()
  post_bt = list()
  for (i in 1:length(fnames)) {
    fname_model = fnames[i]
    out = readRDS(paste0(output_dir,'/', fname_model))
    
    # get all array slices for diameters 
    variables = names(out[1,1,])
    allDs = grep('D\\[',variables)
    allRWs = grep('X\\[',variables)
    allBTs = grep('beta_t\\[',variables)
    
    # we need to put into matrix for use in processing, some compile info from all chains
    out.temp = out[seq(dim(out)[1]-pool+1, dim(out)[1], pool/(keep/nchains)),,allDs]
    out.temp.rw = out[seq(dim(out)[1]-pool+1, dim(out)[1], pool/(keep/nchains)),,allRWs]
    out.temp.bt = out[seq(dim(out)[1]-pool+1, dim(out)[1], pool/(keep/nchains)),,allBTs]
    
    
    if(nchains>1){
      out = out.temp[,1,]
      out.rw = out.temp.rw[,1,]
      out.bt = out.temp.bt[,1,]
      for (j in 2:ncol(out.temp)){
        out = rbind(out, out.temp[,j,])
        out.rw = rbind(out.rw, out.temp.rw[,j,])
      }
      post[[i]] = out
      post_rw[[i]] = out.rw
    } else {
      post[[i]] = out.temp
      post_rw[[i]] = out.temp.rw
    }
  }  
  rm(out, out.temp, allDs, variables)
  
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
  year_lo = min(years)
  year_hi = max(years)
  
  list2env(dat, envir = globalenv())
  
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
  
  #####################################################
  ################ 1a. Plot model and data ############
  #####################################################
  
  pdf(file.path(site_dir,'runs',paste0(mvers,'_',dvers),'figures','tree_growth_model_data.pdf'), width=10, height=6)
  # pdf(paste0('figures/dbh_vs_year_estimated_', model, '.pdf'), width=10, height=6)
  
  # first for RW only model (no census)
  # dbh_array = array(NA, dim = c(N_Tr, N_years, keep))
  
  for (tree in 1:N_Tr){
    
    print(tree)
    
    # determine which estimates correspond to this tree
    inds = which(X2Tr == tree)
    yrinds = X2year[inds]
    
    # extract diameter data
    # dbh_array[t,yrinds,] = t(post[[1]][,inds])
    
    dbh_iter = t(post[[1]][,inds])
    dbh_iter = data.frame(dbh_iter)
    dbh_iter = data.frame(year=years[yrinds], dbh_iter)
    
    dbh_mean = apply(dbh_iter[,2:ncol(dbh_iter)], 1, mean, na.rm=TRUE)
    dbh_quant = t(apply(dbh_iter[,2:ncol(dbh_iter)], 1, 
                        function(x) quantile(x, c(0.025, 0.5, 0.975), na.rm=TRUE)))
    
    dbh_tree = data.frame(d_mean = dbh_mean, 
                          d_median = dbh_quant[,2], 
                          d_lo = dbh_quant[,1], 
                          d_hi = dbh_quant[,3], 
                          year = years[yrinds])
    
    idx_d_obs = which(Tr$stat_id == tree)
    
    dbh_obs = data.frame(d_obs = Tr$dbh[idx_d_obs],
                         year = years[Tr$year[idx_d_obs]])
    
    stem_id = Tr$id[idx_d_obs[1]]
    
    # Create a text
    grob <- grobTree(textGrob(paste0('Tree ', tree, '; Stem ID ', stem_id, '; Species ', taxon[tree] ), x=0.05,  y=0.9, hjust=0,
                              gp=gpar(col="black", fontsize=22)))
    
    p1 <- ggplot() +
      # geom_line(data=dbh_tree, aes(x=year, y=d_mean)) +
      geom_ribbon(data=dbh_tree, aes(x=year, ymin=d_lo, ymax=d_hi), fill='lightgrey') +
      geom_line(data=dbh_tree, aes(x=year, y=d_median)) +
      geom_point(data=dbh_obs, aes(x=year, y=d_obs), size=2) +
      # geom_dog(data=dbh_obs, aes(x=year, y=d_obs, dog='glasses'), size=2) +
      # ylim(c(0,500)) +
      xlab('year') +
      ylab('dbh (cm)') +
      xlim(c(year_lo, year_hi)) +
      theme_bw(16)  +
      # ggtitle(paste0('Tree ', i)) +
      annotation_custom(grob)
    
    # print(p1)
    
    # stem_id = core2stemids[i]
    # species_id = species_ids[core2species[i]]
    
    inds = which(X2Tr == tree)
    yrinds = X2year[inds]
    
    # extract diameter data
    # dbh_array[t,yrinds,] = t(post[[1]][,inds])
    
    rw_iter = t(post_rw[[1]][,inds])
    rw_iter = data.frame(rw_iter)
    rw_iter = data.frame(year=years[yrinds], rw_iter)
    
    rw_mean = apply(rw_iter[,2:ncol(rw_iter)], 1, mean, na.rm=TRUE)
    rw_quant = t(apply(rw_iter[,2:ncol(rw_iter)], 1, 
                       function(x) quantile(x, c(0.025, 0.5, 0.975), na.rm=TRUE)))
    
    rw_tree = data.frame(x_mean = rw_mean, 
                         x_median = rw_quant[,2], 
                         x_lo = rw_quant[,1], 
                         x_hi = rw_quant[,3], 
                         year = years[yrinds])
    
    idx_rw_obs = which(Xobs$stat_id == tree)
    
    rw_obs = data.frame(x_obs = Xobs$incr[idx_rw_obs],
                        year = years[Xobs$year[idx_rw_obs]])
    
    # Create a text
    grob <- grobTree(textGrob(paste0('Tree ', tree, '; Stem ID ', stem_id, '; Species ', taxon[tree] ), x=0.05,  y=0.9, hjust=0,
                              gp=gpar(col="black", fontsize=22)))
    
    p2 <- ggplot() +
      # geom_line(data=dbh_tree, aes(x=year, y=d_mean)) +
      geom_ribbon(data=rw_tree, aes(x=year, ymin=x_lo, ymax=x_hi), fill='lightgrey') +
      geom_line(data=rw_tree, aes(x=year, y=x_median)) +
      geom_point(data=rw_obs, aes(x=year, y=x_obs), size=2) +
      # geom_dog(data=rw_obs, aes(x=year, y=x_obs, dog='glasses'), size=2) +
      # ylim(c(0,500)) +
      xlab('year') +
      ylab('rw (mm)') +
      xlim(c(year_lo, year_hi)) +
      theme_bw(16)  #+
    # ggtitle(paste0('Tree ', i)) +
    # annotation_custom(grob)
    
    # print(p2)
    
    grid.arrange(p1, p2, nrow = 2)
    
    
  }
  dev.off()
  
  
  #####################################################
  ################ 1a. Plot model and data ############
  #####################################################
  dbh_validate = data.frame(stat_id = numeric(0),
                            stem_id = character(0),
                            species_id = character(0),
                            year = numeric(0),
                            d_model_mean = numeric(0),
                            d_model_median = numeric(0),
                            d_model_lo = numeric(0),
                            d_model_hi = numeric(0),
                            d_obs = numeric(0))
  
  # create data frame of DBH model and data values
  rw_validate = data.frame(stat_id = numeric(0),
                           stem_id = character(0),
                           species_id = character(0),
                           year = numeric(0),
                           rw_model_mean = numeric(0),
                           rw_model_median = numeric(0),
                           rw_model_lo = numeric(0),
                           rw_model_hi = numeric(0),
                           rw_obs = numeric(0))
  
  for (tree in 1:N_Tr){
    
    print(tree)
    
    # determine which estimates correspond to this tree
    inds = which(X2Tr == tree)
    yrinds = X2year[inds]
    
    # extract diameter data
    # dbh_array[t,yrinds,] = t(post[[1]][,inds])
    
    dbh_iter = t(post[[1]][,inds])
    dbh_iter = data.frame(dbh_iter)
    dbh_iter = data.frame(year=years[yrinds], dbh_iter)
    
    d_mean = apply(dbh_iter[,2:ncol(dbh_iter)], 1, mean, na.rm=TRUE)
    d_quant = t(apply(dbh_iter[,2:ncol(dbh_iter)], 1, 
                      function(x) quantile(x, c(0.025, 0.5, 0.975), na.rm=TRUE)))
    
    idx_d_obs = which(Tr$stat_id == tree)
    
    dbh_obs = data.frame(d_obs = Tr$dbh[idx_d_obs],
                         year = years[Tr$year[idx_d_obs]])
    
    stem_id = Tr$id[idx_d_obs[1]]
    species_id = taxon[tree]
    
    
    dbh_model = data.frame(d_model_mean = d_mean, 
                           d_model_median = d_quant[,2], 
                           d_model_lo = d_quant[,1], 
                           d_model_hi = d_quant[,3], 
                           year = years[yrinds])
    dbh_model = subset(dbh_model, year %in% dbh_obs$year)
    
    dbh_merged = merge(dbh_model, dbh_obs)
    dbh_merged = data.frame(stat_id = rep(i),
                            stem_id = rep(stem_id),
                            species_id = rep(species_id),
                            dbh_merged)
    
    dbh_validate = rbind(dbh_validate,
                         dbh_merged)
    # 
    # rw_mean = apply(rw_iter[,2:ncol(rw_iter)], 1, mean, na.rm=TRUE)
    # rw_quant = t(apply(rw_iter[,2:ncol(rw_iter)], 1, 
    #                    function(x) quantile(x, c(0.025, 0.5, 0.975), na.rm=TRUE)))
    # 
    # rw_tree = data.frame(x_mean = rw_mean, 
    #                      x_median = rw_quant[,2], 
    #                      x_lo = rw_quant[,1], 
    #                      x_hi = rw_quant[,3], 
    #                      year = years[yrinds])
    # 
    # idx_rw_obs = which(Xobs$stat_id == tree)
    # 
    # rw_obs = data.frame(x_obs = Xobs$incr[idx_rw_obs],
    #                     year = years[Xobs$year[idx_rw_obs]])
    
    
    
    inds = which(X2Tr == tree)
    yrinds = X2year[inds]
    
    # extract diameter data
    # dbh_array[t,yrinds,] = t(post[[1]][,inds])
    
    rw_iter = t(post_rw[[1]][,inds])
    rw_iter = data.frame(rw_iter)
    rw_iter = data.frame(year=years[yrinds], rw_iter)
    
    rw_mean = apply(rw_iter[,2:ncol(rw_iter)], 1, mean, na.rm=TRUE)
    rw_quant = t(apply(rw_iter[,2:ncol(rw_iter)], 1,
                       function(x) quantile(x, c(0.025, 0.5, 0.975), na.rm=TRUE)))
    
    # rw_tree = data.frame(x_mean = rw_mean, 
    #                      x_median = rw_quant[,2], 
    #                      x_lo = rw_quant[,1], 
    #                      x_hi = rw_quant[,3], 
    #                      year = years[yrinds])
    
    
    
    idx_rw_obs = which(Xobs$stat_id == tree)
    
    rw_obs = data.frame(rw_obs = Xobs$incr[idx_rw_obs],
                        year = years[Xobs$year[idx_rw_obs]])
    
    rw_obs_avg = rw_obs %>% 
      group_by(year) %>%
      summarize(rw_obs = mean(rw_obs, na.rm=TRUE))
    
    
    rw_model = data.frame(rw_model_mean = rw_mean, 
                          rw_model_median = rw_quant[,2], 
                          rw_model_lo = rw_quant[,1], 
                          rw_model_hi = rw_quant[,3], 
                          year = years[yrinds])
    rw_model = subset(rw_model, year %in% rw_obs_avg$year)
    
    rw_merged = data.frame(stat_id = rep(i),
                           stem_id = rep(stem_id),
                           species_id = rep(species_id),
                           merge(rw_model, rw_obs_avg))
    
    rw_validate = rbind(rw_validate,
                        rw_merged)
  }
  
  # save file
  filename1a = file.path(output_dir,paste0('DBH_VALIDATE_',site,'_',mvers,'_',dvers))
  saveRDS(dbh_validate, paste0(filename1a,'.RDS'))
  
  filename1b = file.path(output_dir,paste0('RW_VALIDATE_',site,'_',mvers,'_',dvers))
  saveRDS(rw_validate, paste0(filename1b,'.RDS'))
  
  # pdf(paste0(figure_dir, '/dbh_model_vs_data_scatter_update_', model_name, '.pdf'), width=10, height=6)
  p <- ggplot(data=dbh_validate) +
    geom_abline(intercept=0, slope=1, lty=2, colour='red') +
    # geom_smooth(method='lm', aes(x=d_obs, y=d_model_median), fullrange=TRUE) +
    # geom_line(data=dbh_tree, aes(x=year, y=d_mean)) +
    geom_linerange(aes(x=d_obs, ymin=d_model_lo, ymax=d_model_hi), colour='black', alpha=0.3) +
    geom_point(aes(x=d_obs, y=d_model_median), colour='black', size=2, alpha=0.3) +
    geom_line(stat='smooth', method='lm', formula = y ~ x, aes(x=d_obs, y=d_model_median), colour = 'blue', alpha=0.5, fullrange=TRUE) +
    xlab('dbh obs (cm)') +
    ylab('dbh model (cm)') +
    theme_bw(16)  + 
    coord_fixed() +
    xlim(c(15, 90)) +
    ylim(c(15, 90))
  print(p)
  # dev.off()
  ggsave(file.path(site_dir,'runs',paste0(mvers,'_',dvers),'figures','dbh_model_vs_data_scatter_update.pdf'), width=10, height=6)
  ggsave(file.path(site_dir,'runs',paste0(mvers,'_',dvers),'figures','dbh_model_vs_data_scatter_update.png'), width=10, height=6)
  
  rw_max = max(rw_validate[,c('rw_model_mean', 'rw_model_median', 'rw_model_lo', 'rw_model_hi', 'rw_obs')])
  rw_min = min(rw_validate[,c('rw_model_mean', 'rw_model_median', 'rw_model_lo', 'rw_model_hi', 'rw_obs')])
  
  rw_max = ceiling(rw_max)
  rw_min = floor(rw_min)
  
  # pdf(paste0(figure_dir, '/rw_model_vs_data_scatter_update_', model_name, '.pdf'), width=10, height=6)
  p <- ggplot(data=rw_validate) +
    geom_abline(intercept=0, slope=1, lty=2, colour='red') +
    geom_smooth(method='lm', aes(x=rw_obs, y=rw_model_median), fullrange=TRUE) +
    geom_linerange(aes(x=rw_obs, ymin=rw_model_lo, ymax=rw_model_hi), colour='black', alpha=0.3) +
    geom_point(aes(x=rw_obs, y=rw_model_median), colour='black', size=2, alpha=0.3) +
    # geom_dog(data=rw_obs, aes(x=year, y=x_obs, dog='glasses'), size=2) +
    # ylim(c(0,500)) +
    xlab('rw obs (mm)') +
    ylab('rw model (mm)') +
    theme_bw(16) + 
    coord_equal() +
    xlim(c(rw_min, rw_max)) +
    ylim(c(rw_min, rw_max))
  print(p)
  # dev.off()
  ggsave(file.path(site_dir,'runs',paste0(mvers,'_',dvers),'figures','rw_model_vs_data_scatter_update.pdf'), width=10, height=6)
  ggsave(file.path(site_dir,'runs',paste0(mvers,'_',dvers),'figures','rw_model_vs_data_scatter_update.png'), width=10, height=6)
  
  logrw_validate = rw_validate
  logrw_validate[,5:ncol(logrw_validate)] = apply(logrw_validate[,5:ncol(logrw_validate)], c(1,2), function(x) if (x==0){x = NA} else {x})
  logrw_validate[,5:ncol(logrw_validate)] = log(logrw_validate[,5:ncol(rw_validate)])
  
  
  logrw_max = max(logrw_validate[,c('rw_model_mean', 'rw_model_median', 'rw_model_lo', 'rw_model_hi', 'rw_obs')], na.rm=TRUE)
  logrw_min = min(logrw_validate[,c('rw_model_mean', 'rw_model_median', 'rw_model_lo', 'rw_model_hi', 'rw_obs')], na.rm=TRUE)
  
  logrw_max = ceiling(logrw_max)
  logrw_min = floor(logrw_min)
  
  # pdf(paste0(figure_dir, '/logrw_model_vs_data_scatter_update_', model_name, '.pdf'), width=10, height=6)
  p <- ggplot(data=logrw_validate) +
    geom_abline(intercept=0, slope=1, lty=2, colour='red') +
    geom_smooth(method='lm', aes(x=rw_obs, y=rw_model_median), fullrange=TRUE) +
    geom_linerange(aes(x=rw_obs, ymin=rw_model_lo, ymax=rw_model_hi), colour='black', alpha=0.3) +
    geom_point(aes(x=rw_obs, y=rw_model_median), colour='black', size=2, alpha=0.3) +
    # geom_dog(data=rw_obs, aes(x=year, y=x_obs, dog='glasses'), size=2) +
    # ylim(c(0,500)) +
    xlab('log rw obs (log(mm))') +
    ylab('log rw model  (log(mm))') +
    theme_bw(16) + 
    coord_equal() +
    xlim(c(logrw_min, logrw_max)) +
    ylim(c(logrw_min, logrw_max))
  print(p)
  # dev.off()
  ggsave(file.path(site_dir,'runs',paste0(mvers,'_',dvers),'figures','logrw_model_vs_data_scatter_update.pdf'), width=10, height=6)
  ggsave(file.path(site_dir,'runs',paste0(mvers,'_',dvers),'figures','logrw_model_vs_data_scatter_update.png'), width=10, height=6)
  
  # rw_validate_avg = rw_validate %>% 
  #   group_by(year, stem_id, species_id, rw_model_mean, rw_model_median, rw_model_lo, rw_model_hi) %>% 
  #   summarize(rw_obs = mean(rw_obs, na.rm=TRUE))
  # 
  # p <- ggplot(data=rw_validate_avg) +
  #   geom_abline(intercept=0, slope=1, lty=2, colour='red') +
  #   geom_smooth(method='lm', aes(x=rw_obs, y=rw_model_median), fullrange=TRUE) +
  #   geom_linerange(aes(x=rw_obs, ymin=rw_model_lo, ymax=rw_model_hi), colour='black', alpha=0.3) +
  #   geom_point(aes(x=rw_obs, y=rw_model_median), colour='black', size=2, alpha=0.3) +
  #   # geom_dog(data=rw_obs, aes(x=year, y=x_obs, dog='glasses'), size=2) +
  #   # ylim(c(0,500)) +
  #   xlab('rw obs (mm)') +
  #   ylab('rw model (mm)') +
  #   theme_bw(16) + 
  #   coord_equal() +
  #   xlim(c(0, 6.5)) +
  #   ylim(c(0, 6.5)) 
  # print(p)
  # # dev.off()
  # ggsave(file.path(site_dir,'runs',paste0(mvers,'_',dvers),'figures','rw_avg_model_vs_data_scatter_update.pdf'), width=10, height=6)
  # ggsave(file.path(site_dir,'runs',paste0(mvers,'_',dvers),'figures','rw_avg_model_vs_data_scatter_update.png'), width=10, height=6)
  # 
  
}