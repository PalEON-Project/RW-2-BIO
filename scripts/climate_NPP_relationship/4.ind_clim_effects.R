## Linear models between annual growth of each tree over time and
## climate drivers at annual scale
## Previously attempted to use monthly data and this was too much
## for a linear regression model

rm(list = ls())

# Load AGBI
# Uncomment trended or detrended
load('out/tree_trended_AGBI.RData')
#load('out/tree_detrended_AGBI.RData')

# Rename dataframe if using trended
if(exists('tree_agbi')) save_comb <- tree_agbi
# Rename response column if using trended
if(exists('tree_agbi')) save_comb <- dplyr::rename(save_comb, residual_AGBI = mean)

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

# Load competition information
load('data/competition_metrics.RData')

ntrees <- c(length(unique(save_comb$tree[which(save_comb$site == 'GOOSE')])),
            length(unique(save_comb$tree[which(save_comb$site == 'NRP')])),
            length(unique(save_comb$tree[which(save_comb$site == 'ROOSTER')])),
            length(unique(save_comb$tree[which(save_comb$site == 'SYLVANIA')])),
            length(unique(save_comb$tree[which(save_comb$site == 'HARVARD Model RW')])),
            length(unique(save_comb$tree[which(save_comb$site == 'HARVARD Model RW + Census')])))

# Storage
coeff_save <- matrix(, nrow = sum(ntrees), ncol = 11)

row_ind <- 0

# For each site, let's iteratively fit a simple linear model with
# average temperature and precipitation as predictors of each tree's annual growth
for(i in 1:length(site)){
  # Tree number index, unique to each site
  tree <- unique(save_comb$tree[which(save_comb$site == site[i])])
  # Save site name
  site_name <- site[i]
  # Loop through each tree at a given site
  for(j in tree){
    # Increment counter
    row_ind <- row_ind + 1
    # Subset full data for one tree
    sub <- dplyr::filter(save_comb, site == site_name &
                           tree == j)
    # Combine tree data with climate
    joined <- sub |>
      # Join with annual climate drivers
      dplyr::left_join(y = prism_growing, by = c('site', 'year')) |>
      # Join with tree-level competition metrics
      dplyr::left_join(y = tree_dbh, by = c('tree', 'year', 'plot', 'taxon', 'site')) |>
      # only keep individual tree basal area (ba)
      dplyr::select(-dbh)
    
    # Fit linear model
    # annual increment of individual tree is a function of 
    # mean annual precipitation, mean annual temperature,
    # annual temperature seasonality
    # maximum annual VPD,
    # individual tree basal area
    if(nrow(joined) < 4){
      coeff_save[row_ind,1] <- i
      coeff_save[row_ind,2] <- unique(sub$plot)
      coeff_save[row_ind,3] <- unique(sub$taxon)
      coeff_save[row_ind,4] <- j
      coeff_save[row_ind,5:10] <- NA
      coeff_save[row_ind,11] <- NA
    }else{
      mod <- lm(formula = residual_AGBI ~ PPT + Tmean + 
                  sd_Tmean + Vpdmax + 
                  ba,
                data = joined)   
      # Save site name, tree number, coefficients, and r2 in matrix
      coeff_save[row_ind,1] <- i
      coeff_save[row_ind,2] <- unique(sub$plot)
      coeff_save[row_ind,3] <- unique(sub$taxon)
      coeff_save[row_ind,4] <- j
      coeff_save[row_ind,5:10] <- coefficients(mod)
      coeff_save[row_ind,11] <- summary(mod)$adj.r.squared
    }
    print(j)
  }
  print(paste0('---------------------',i,'----------------'))
}

# Column names
colnames(coeff_save) <- c('Site', 'Plot', 'Taxon', 'Tree', 'Intercept',
                          'Precipitation', 'Temperature', 
                          'SD_Temperature',
                          'Maximum_VPD', 
                          'BA', 'R2')

# Format
coeff_save <- as.data.frame(coeff_save)

# Replace site numbers with names
coeff_save <- coeff_save |>
  dplyr::mutate(Site = as.character(Site)) |>
  dplyr::mutate(Site = dplyr::if_else(Site == 1, 'GOOSE', Site),
                Site = dplyr::if_else(Site == 2, 'NRP', Site),
                Site = dplyr::if_else(Site == 3, 'ROOSTER', Site),
                Site = dplyr::if_else(Site == 4, 'SYLVANIA', Site),
                Site = dplyr::if_else(Site == 5, 'HARVARD Model RW', Site),
                Site = dplyr::if_else(Site == 6, 'HARVARD Model RW + Census', Site)) |>
  # Format
  dplyr::mutate(Intercept = as.numeric(Intercept),
                Precipitation = as.numeric(Precipitation),
                Temperature = as.numeric(Temperature),
                SD_Temperature = as.numeric(SD_Temperature),
                Maximum_VPD = as.numeric(Maximum_VPD),
                BA = as.numeric(BA),
                R2 = as.numeric(R2))

# Distribution of R2 for each site with individual models
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = R2)) +
  ggplot2::geom_density() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::xlab(expression(R^2)) + ggplot2::ylab('Density') +
  ggplot2::theme_minimal()

# Violin plot of R2
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = R2)) +
  ggplot2::geom_violin(fill = NA) +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed') +
  ggplot2::theme_minimal()

# Violin of precipitation coefficient
ci <- coeff_save |>
  dplyr::summarize(lower = quantile(Precipitation, probs = 0.025, na.rm = TRUE),
                   upper = quantile(Precipitation, probs = 0.975, na.rm = TRUE))

coeff_save |>
  dplyr::filter(Precipitation > ci$lower & Precipitation < ci$upper) |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = Precipitation)) +
  ggplot2::geom_violin(fill = NA) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for precipitation') +
  ggplot2::theme_minimal()

# Violin of temperature coefficient
ci <- coeff_save |>
  dplyr::summarize(lower = quantile(Temperature, probs = 0.025, na.rm = TRUE),
                   upper = quantile(Temperature, probs = 0.975, na.rm = TRUE))

coeff_save |>
  dplyr::filter(Temperature > ci$lower & Temperature < ci$upper) |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = Temperature)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for temperature') +
  ggplot2::theme_minimal()

# Violin of precipitation seasonality coefficient
#coeff_save |>
#  ggplot2::ggplot(ggplot2::aes(x = Site, y = SD_Precipitation)) +
#  ggplot2::geom_violin() +
#  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
#  ggplot2::xlab('') + ggplot2::ylab('Coefficient for precipitation seasonality') +
#  ggplot2::theme_minimal()

# Violin for temperature seasonality coefficient
ci <- coeff_save |>
  dplyr::summarize(lower = quantile(SD_Temperature, probs = 0.025, na.rm = TRUE),
                   upper = quantile(SD_Temperature, probs = 0.975, na.rm = TRUE))

coeff_save |>
  dplyr::filter(SD_Temperature > ci$lower & SD_Temperature < ci$upper) |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = SD_Temperature)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for temperature seasonality') +
  ggplot2::theme_minimal()

# Violin of minimum temperature coefficient
#coeff_save |>
#  ggplot2::ggplot(ggplot2::aes(x = Site, y = Minimum_temperature)) +
#  ggplot2::geom_violin() +
#  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
#  ggplot2::xlab('') + ggplot2::ylab('Coefficient for minimum temperature') +
#  ggplot2::theme_minimal()

# Violin of maximum temperature coefficient
#coeff_save |>
#  ggplot2::ggplot(ggplot2::aes(x = Site, y = Maximum_temperature)) +
#  ggplot2::geom_violin() +
#  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
#  ggplot2::xlab('') + ggplot2::ylab('Coefficient for maximum temperature') +
#  ggplot2::theme_minimal()

# Violin of minimum VPD
#coeff_save |>
#  ggplot2::ggplot(ggplot2::aes(x = Site, y = Minimum_VPD)) +
#  ggplot2::geom_violin() +
#  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
#  ggplot2::xlab('') + ggplot2::ylab('Coefficient for minimum VPD') +
#  ggplot2::theme_minimal()

# Violin of maximum VPD
ci <- coeff_save |>
  dplyr::summarize(lower = quantile(Maximum_VPD, probs = 0.025, na.rm = TRUE),
                   upper = quantile(Maximum_VPD, probs = 0.975, na.rm = TRUE))

coeff_save |>
  dplyr::filter(Maximum_VPD > ci$lower & Maximum_VPD < ci$upper) |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = Maximum_VPD)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for maximum VPD') +
  ggplot2::theme_minimal()

# Violin of BA
ci <- coeff_save |>
  dplyr::summarize(lower = quantile(BA, probs = 0.025, na.rm = TRUE),
                   upper = quantile(BA, probs = 0.975, na.rm = TRUE))

coeff_save |>
  dplyr::filter(BA > ci$lower & BA < ci$upper) |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = BA)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for tree basal area') +
  ggplot2::theme_minimal()

if(exists('tree_agbi')){
  save(coeff_save, file = 'out/ind_lm_coeff_save_trended.RData')
}else{
  save(coeff_save, file = 'out/ind_lm_coeff_save.RData')
}
