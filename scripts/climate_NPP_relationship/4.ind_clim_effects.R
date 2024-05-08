## Linear models between annual growth of each tree over time and
## climate drivers at annual scale
## Previously attempted to use monthly data and this was too much
## for a linear regression model
rm(list = ls())

# Load detrended AGBI
load('out/tree_detrended_AGBI.RData')

# Indexing for loops
site <- c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA')

# Load climate data
load('climate/prism_clim.RData')

# Format
prism_long <- dplyr::rename(prism_long, site = loc) |>
  dplyr::mutate(year = as.numeric(year))

# Pivot wider
prism_annual <- prism_long |>
  dplyr::group_by(year, site) |>
  # Average over months
  dplyr::summarize(mean_PPT = mean(PPT2),
                   mean_Tmean = mean(Tmean2),
                   sd_PPT = sd(PPT2),
                   sd_Tmean = sd(Tmean2),
                   mean_Tmin = min(Tmin2),
                   mean_Tmax = max(Tmax2),
                   mean_Vpdmin = min(Vpdmin2),
                   mean_Vpdmax = max(Vpdmax2)) 

# Load competition information
load('data/competition_metrics.RData')

ntrees <- c(length(unique(save_comb$tree[which(save_comb$site == 'GOOSE')])),
            length(unique(save_comb$tree[which(save_comb$site == 'NRP')])),
            length(unique(save_comb$tree[which(save_comb$site == 'ROOSTER')])),
            length(unique(save_comb$tree[which(save_comb$site == 'SYLVANIA')])))

# Storage
coeff_save <- matrix(, nrow = sum(ntrees), ncol = 15)

row_ind <- 0
# For each site, let's iteratively fit a simple linear model with
# average temperature and precipitation as predictors of each tree's annual growth
for(i in 1:4){
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
      dplyr::left_join(y = prism_annual, by = c('site', 'year')) |>
      # Join with tree-level competition metrics
      dplyr::left_join(y = tree_dbh, by = c('tree', 'year', 'plot', 'taxon', 'site')) |>
      # only keep individual tree basal area (ba)
      dplyr::select(-dbh)
    # Fit linear model
    # annual increment of individual tree is a function of 
    # mean annual precipitation, mean annual temperature,
    # annual precipitation seasonality, annual temperature seasonality
    # minimum annual temperature, maximum annual temperature,
    # minimum annual VPD, maximum annual VPD,
    # individual tree basal area, basal area greater than,
    # fraction of basal area for given taxon,
    # total plot basal area
    if(nrow(joined) == 1){
      coeff_save[row_ind,1] <- i
      coeff_save[row_ind,2] <- unique(sub$plot)
      coeff_save[row_ind,3] <- unique(sub$taxon)
      coeff_save[row_ind,4] <- j
      coeff_save[row_ind,5:14] <- NA
      coeff_save[row_ind,15] <- NA
    }else{
      mod <- lm(formula = residual_AGBI ~ mean_PPT + mean_Tmean + 
                  sd_PPT + sd_Tmean +
                  mean_Tmin + mean_Tmax + 
                  mean_Vpdmin + mean_Vpdmax + 
                  ba,
                data = joined)   
      # Save site name, tree number, coefficients, and r2 in matrix
      coeff_save[row_ind,1] <- i
      coeff_save[row_ind,2] <- unique(sub$plot)
      coeff_save[row_ind,3] <- unique(sub$taxon)
      coeff_save[row_ind,4] <- j
      coeff_save[row_ind,5:14] <- coefficients(mod)
      coeff_save[row_ind,15] <- summary(mod)$adj.r.squared
    }
    print(j)
  }
  print(paste0('---------------------',i,'----------------'))
}

# Column names
colnames(coeff_save) <- c('Site', 'Plot', 'Taxon', 'Tree', 'Intercept',
                          'Precipitation', 'Temperature', 
                          'SD_Precipitation', 'SD_Temperature',
                          'Minimum_temperature', 'Maximum_temperature',
                          'Minimum_VPD', 'Maximum_VPD', 
                          'BA', 'R2')
# Format
coeff_save <- as.data.frame(coeff_save)

# Replace site numbers with names
coeff_save <- coeff_save |>
  dplyr::mutate(Site = as.character(Site)) |>
  dplyr::mutate(Site = dplyr::if_else(Site == 1, 'GOOSE', Site),
                Site = dplyr::if_else(Site == 2, 'NRP', Site),
                Site = dplyr::if_else(Site == 3, 'ROOSTER', Site),
                Site = dplyr::if_else(Site == 4, 'SYLVANIA', Site)) |>
  # Format
  dplyr::mutate(Intercept = as.numeric(Intercept),
                Precipitation = as.numeric(Precipitation),
                Temperature = as.numeric(Temperature),
                SD_Precipitation = as.numeric(SD_Precipitation),
                SD_Temperature = as.numeric(SD_Temperature),
                Minimum_temperature = as.numeric(Minimum_temperature),
                Maximum_temperature = as.numeric(Maximum_temperature),
                Minimum_VPD = as.numeric(Minimum_VPD),
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
  ggplot2::geom_violin() +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2)) +
  ggplot2::theme_minimal()

# Violin of precipitation coefficient
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = Precipitation)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for precipitation') +
  ggplot2::theme_minimal()

# Violin of temperature coefficient
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = Temperature)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for temperature') +
  ggplot2::theme_minimal()

# Violin of precipitation seasonality coefficient
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = SD_Precipitation)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for precipitation seasonality') +
  ggplot2::theme_minimal()

# Violin for temperature seasonality coefficient
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = SD_Temperature)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for temperature seasonality') +
  ggplot2::theme_minimal()

# Violin of minimum temperature coefficient
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = Minimum_temperature)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for minimum temperature') +
  ggplot2::theme_minimal()

# Violin of maximum temperature coefficient
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = Maximum_temperature)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for maximum temperature') +
  ggplot2::theme_minimal()

# Violin of minimum VPD
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = Minimum_VPD)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for minimum VPD') +
  ggplot2::theme_minimal()

# Violin of maximum VPD
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = Maximum_VPD)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for maximum VPD') +
  ggplot2::theme_minimal()

# Violin of BA
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = BA)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for tree basal area') +
  ggplot2::theme_minimal()

save(coeff_save, file = 'out/ind_lm_coeff_save.RData')
