rm(list = ls())

# Load total increment
goose_total_agbi <- readRDS('sites/GOOSE/runs/v2.0_012021/output/AGBI_STAN_GOOSE_v2.0_012021.RDS')
nrp_total_agbi <- readRDS('sites/NORTHROUND/runs/v2.0_082020/output/AGBI_STAN_NORTHROUND_v2.0_082020.RDS')
rooster_total_agbi <- readRDS('sites/ROOSTER/runs/v2.0_082020/output/AGBI_STAN_ROOSTER_v2.0_082020.RDS')
sylv_total_agbi <- readRDS('sites/SYLVANIA/runs/v2.0_082020/output/AGBI_STAN_SYLVANIA_v2.0_082020.RDS')

goose_total_agbi <- goose_total_agbi |>
  dplyr::mutate(site = 'GOOSE') |>
  dplyr::filter(year > 1959)
nrp_total_agbi <- nrp_total_agbi |>
  dplyr::mutate(site = 'NRP') |>
  dplyr::filter(year > 1959)
rooster_total_agbi <- rooster_total_agbi |>
  dplyr::mutate(site = 'ROOSTER') |>
  dplyr::filter(year > 1959)
sylv_total_agbi <- sylv_total_agbi |>
  dplyr::mutate(site = 'SYLVANIA') |>
  dplyr::filter(year > 1959)

# Combine sites
total_agbi <- rbind(goose_total_agbi, nrp_total_agbi,
                    rooster_total_agbi, sylv_total_agbi)

# Plot NPP for each tree and site
total_agbi |>
  dplyr::group_by(tree, year, plot, taxon, site) |>
  dplyr::summarize(mean = mean(value)) |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = mean, color = as.factor(tree))) +
  ggplot2::geom_line(show.legend = F) +
  ggplot2::facet_wrap(~site) +
  ggplot2::xlab('') + ggplot2::ylab('AGBI') +
  ggplot2::theme_minimal()

# Save mean over iterations in dataframe
total_agbi <- total_agbi |>
  dplyr::group_by(tree, year, plot, taxon, site) |>
  dplyr::summarize(mean = mean(value))

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
  dplyr::summarize(mean_PPT = mean(PPT2),
                   mean_Tmean = mean(Tmean2),
                   mean_Tmin = mean(Tmin2),
                   mean_Tmax = mean(Tmax2),
                   mean_Vpdmin = mean(Vpdmin2),
                   mean_Vpdmax = mean(Vpdmax2)) 
prism_month <- tidyr::pivot_wider(prism_long, names_from = 'month', values_from = c('PPT2', 'Tmean2', 'Tmin2', 'Tmax2', 'Vpdmin2', 'Vpdmax2'))

# Load competition information
load('data/competition_metrics.RData')

ntrees <- c(length(unique(goose_total_agbi$tree)),
            length(unique(nrp_total_agbi$tree)),
            length(unique(rooster_total_agbi$tree)),
            length(unique(sylv_total_agbi$tree)))
# Storage
coeff_save <- matrix(, nrow = sum(ntrees), ncol = 14)

row_ind <- 0
# For each site, let's iteratively fit a simple linear model with
# average temperature and precipitation as predictors of each tree's annual growth
for(i in 1:4){
  # Tree number index, unique to each site
  tree <- unique(total_agbi$tree[which(total_agbi$site == site[i])])
  # Save site name
  site_name <- site[i]
  # Loop through each tree at a given site
  for(j in tree){
    # Increment counter
    row_ind <- row_ind + 1
    # Subset full data for one tree
    sub <- dplyr::filter(total_agbi, site == site_name &
                           tree == j)
    # Combine tree data with climate
    joined <- sub |>
      dplyr::left_join(y = prism_annual, by = c('site', 'year')) |>
      dplyr::left_join(y = ba_by_tree, by = c('tree', 'year', 'plot', 'taxon', 'site')) |>
      # only keep ba, bagt
      dplyr::select(-frac, -total_ba, -dbh) |>
      dplyr::left_join(y = ba_by_taxon, by = c('plot', 'site', 'year', 'taxon')) |>
      # only keep ba, bagt (from before), frac
      dplyr::select(-total_ba.x, -total_ba.y) |>
      dplyr::left_join(y = total_ba, by = c('plot', 'site', 'year'))
    # Fit linear model
    mod <- lm(formula = mean ~ mean_PPT + mean_Tmean + mean_Tmin + 
                mean_Tmax + mean_Vpdmin + mean_Vpdmax + 
                ba + bagt + frac + total_ba,
              data = joined)   
    # Save site name, tree number, coefficients, and r2 in matrix
    coeff_save[row_ind,1] <- i
    coeff_save[row_ind,2] <- j
    coeff_save[row_ind,3:13] <- coefficients(mod)
    coeff_save[row_ind,14] <- summary(mod)$adj.r.squared
    print(j)
  }
  print(paste0('---------------------',i,'----------------'))
}

# Column names
colnames(coeff_save) <- c('Site', 'Tree', 'Intercept',
                          'Precipitation', 'Temperature', 
                          'Minimum_temperature', 'Maximum_temperature',
                          'Minimum_VPD', 'Maximum_VPD', 
                          'BA', 'BAGT', 'frac_ba', 'total_ba', 'R2')
# Format
coeff_save <- as.data.frame(coeff_save)

# Replace site numbers with names
coeff_save <- coeff_save |>
  dplyr::mutate(Site = as.character(Site)) |>
  dplyr::mutate(Site = dplyr::if_else(Site == 1, 'GOOSE', Site),
                Site = dplyr::if_else(Site == 2, 'NRP', Site),
                Site = dplyr::if_else(Site == 3, 'ROOSTER', Site),
                Site = dplyr::if_else(Site == 4, 'SYLVANIA', Site))

# Distribution of R2 for each site with individual models
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = R2)) +
  ggplot2::geom_density() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = 0), linetype = 'dashed') +
  ggplot2::xlab(expression(R^2)) + ggplot2::ylab('Density') +
  ggplot2::theme_minimal()

# Violin plot of R2
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = R2)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
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

# Violin of BAGT
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = BAGT)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for basal area greater than') +
  ggplot2::theme_minimal()

# Violin of fraction of plot-level basal area belonging to the taxon
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = frac_ba)) +
  ggplot2::geom_violin() + 
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for fraction of basal area of each species') +
  ggplot2::theme_minimal()

# Violin of total plot level basal area
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = total_ba)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for total basal area') +
  ggplot2::theme_minimal()

# Run linear models with monthly climate
## Currently does not work because of singularities

# Storage
coeff_save_month <- matrix(, nrow = sum(ntrees), ncol = 76)

row_ind <- 0
# For each site, let's iteratively fit a simple linear model with
# average temperature and precipitation as predictors of each tree's annual growth
for(i in 1:4){
  # Tree number index, unique to each site
  tree <-  unique(total_agbi$tree[which(total_agbi$site == site[i])])
  # Save site name
  site_name <- site[i]
  # Loop through each tree at a given site
  for(j in tree){
    # Increment counter
    row_ind <- row_ind + 1
    # Subset full data for one tree
    sub <- dplyr::filter(total_agbi, site == site_name &
                           tree == j)
    # Combine tree data with climate
    joined <- dplyr::left_join(x = sub, y = prism_month, by = c('site', 'year')) |> 
      dplyr::ungroup() |>
      dplyr::select(-tree, -year, -plot, -taxon, -site)
    # Fit linear model
    mod <- lm(formula = mean ~ .,
              data = joined)   
    # Save site name, tree number, coefficients, and r2 in matrix
    coeff_save_month[row_ind,1] <- i
    coeff_save_month[row_ind,2] <- j
    coeff_save_month[row_ind,3:75] <- coefficients(mod)
    coeff_save_month[row_ind,76] <- summary(mod)$adj.r.squared
    print(j)
  }
  print(paste0('---------------------',i,'----------------'))
}

# Column names
coeff_save_month <- as.data.frame(coeff_save_month)
colnames(coeff_save_month)[1:3] <- c('Site', 'Tree', 'Intercept')
colnames(coeff_save_month)[4:75] <- colnames(joined)[2:73]
colnames(coeff_save_month)[76] <- c('R2')

# Replace site numbers with names
coeff_save_month <- coeff_save_month |>
  dplyr::mutate(Site = as.character(Site)) |>
  dplyr::mutate(Site = dplyr::if_else(Site == 1, 'GOOSE', Site),
                Site = dplyr::if_else(Site == 2, 'NRP', Site),
                Site = dplyr::if_else(Site == 3, 'ROOSTER', Site),
                Site = dplyr::if_else(Site == 4, 'SYLVANIA', Site))

# Distribution of R2 for each site with individual models
coeff_save_month |>
  ggplot2::ggplot(ggplot2::aes(x = R2)) +
  ggplot2::geom_density() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = 0), linetype = 'dashed') +
  ggplot2::xlab(expression(R^2)) + ggplot2::ylab('Density') +
  ggplot2::theme_minimal()

coeff_save_month |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = R2)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2)) +
  ggplot2::theme_minimal()
