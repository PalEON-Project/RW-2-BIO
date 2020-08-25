## Step 2: Run model 

## This script takes the built data from the last step and fits the STAN model, giving annual DBH estimates for each individual. 

run_rw_model <- function(census_site, site, mvers, 
                         dvers, keep = 250){
  
  ##############################################
  ################ 1. Load data ################
  ##############################################
  
  library(rstan)
  
  # Create directories if needed
  site_dir <- file.path('sites',site)
  if (!file.exists(file.path(site_dir,'output')))   dir.create(file.path(site_dir,'output'))
  if (!file.exists(file.path(site_dir,'figures')))   dir.create(file.path(site_dir,'figures'))

  # load built dataset 
  dat = readRDS(file.path('sites',site,'data','built',paste0('tree_data_', site ,'_STAN_',mvers,'_', dvers, '.RDS')))
  
  ########################################################################
  ################ 2. Run RW + census model if applicable ################
  ########################################################################
  
  if (census_site){
    
    # compile STAN model
    compiled <- stan_model(file = paste0('models/ring_model_t_pdbh_STAN.stan'))
    
    
    # fit and extract values
    fit <- sampling(compiled, 
                    data = dat, 
                    iter = 5000, 
                    chains = 1,
                    verbose=TRUE)
    rm(compiled)
    post=rstan::extract(fit)
    rm(fit)
    
    # save as RDS file 
    saveRDS(post, file = paste0('sites/', site, '/output/ring_model_t_pdbh_STAN_', site, '_', mvers, '_', dvers, '.RDS'))
  
    # generate a quick figure to roughly check model output  
    X2taxon = sapply(dat$X2C, function(id){dat$allTrees$taxon[which(dat$allTrees$stat_id == id)]})
    output = data.frame(D = apply(post$D[(dim(post$D)[1]-keep+1):dim(post$D)[1],], 2, mean), 
                        year = dat$X2year_C, 
                        tree = dat$X2C, 
                        taxon = X2taxon)
    col_names = names(post)
    sig_d_obs = mean(post[[which(col_names=="sig_d_obs")]][(dim(post$D)[1]-keep+1):dim(post$D)[1]])
    rm(post)
    
    pl = ggplot(output) + 
      geom_line(aes(x = year, y = D, group = tree, color = tree)) +
      facet_wrap(~taxon) + 
      labs(x = 'Year', y = 'Diameter (cm)', title = 'Estimated Diameter over Time') + 
      theme(legend.position = 'none')
    ggsave(pl, filename = file.path('sites',site,'figures','estimated_species_growth_census.jpg'))
    
    rm(output)
  }

  ######################################################
  ################ 3. Run RW only model ################
  ######################################################
  
  # if we do not have census data for this site, we have to obtain mean measurement error for DBH 
  # from a site that has both of these datasets (i.e. Harvard Forest)
  # TO DO: this needs to be automated I think so that we can adjust which site we want to use
  if (!census_site){
    out = readRDS('sites/HARVARD/output/ring_model_t_pdbh_STAN_HARVARD_v2.0_082020.RDS')
    
    # extract measurement error from dataset and find mean 
    dat$sig_d_obs = mean(out$sig_d_obs[(length(out$sig_d_obs)-keep+1):length(out$sig_d_obs)])
    rm(out)
    
  # otherwise we use the value found in the model above 
  }else{
    dat$sig_d_obs = sig_d_obs
  }

  # compile STAN model
  compiled <- stan_model(file = paste0('models/ring_model_t_pdbh_sigd_STAN.stan'))
  
  # fit and extract values
  fit <- sampling(compiled, 
                  data = dat, 
                  iter = 5000, 
                  chains = 1,
                  verbose=TRUE)
  rm(compiled)
  post=rstan::extract(fit)
  rm(fit)
  
  # save as RDS file 
  saveRDS(post, file = paste0('sites/', site, '/output/ring_model_t_pdbh_sigd_STAN_', site, '_', mvers,'_', dvers, '.RDS'))
  
  # generate a quick figure to roughly check model output 
  output = data.frame(D = apply(post$D[(dim(post$D)[1]-keep+1):dim(post$D)[1],], 2, mean), year = dat$X2year, tree = dat$X2Tr, taxon = dat$Tr$taxon[dat$X2Tr])
  rm(post) 
  
  pl = ggplot(output) + 
    geom_line(aes(x = year, y = D, group = tree, color = tree)) +
    facet_wrap(~taxon) + 
    labs(x = 'Year', y = 'Diameter (cm)', title = 'Estimated Diameter over Time') + 
    theme(legend.position = 'none')
  ggsave(pl, filename = file.path('sites',site,'figures','estimated_species_growth.jpg'))
}
