#rm(list = ls())

# For taxon level: add canopy position if possible
# For plot level: add species relative abundance from basal area as a proxy for competition
## for competition: basal area larger than (proportion of trees larger than individual tree's basal area), plot-level basal area
# For individual level: add basal area larger than, relative abundance of that species

# Second question: are species at different sites responding similarly to climate or other predictors??

# Load total biomass
goose_total_agb <- readRDS('sites/GOOSE/runs/v2.0_012021/output/AGB_STAN_GOOSE_v2.0_012021.RDS')
nrp_total_agb <- readRDS('sites/NORTHROUND/runs/v2.0_082020/output/AGB_STAN_NORTHROUND_v2.0_082020.RDS')
rooster_total_agb <- readRDS('sites/ROOSTER/runs/v2.0_082020/output/AGB_STAN_ROOSTER_v2.0_082020.RDS')
sylv_total_agb <- readRDS('sites/SYLVANIA/runs/v2.0_082020/output/AGB_STAN_SYLVANIA_v2.0_082020.RDS')

# Load total increment
goose_total_agbi <- readRDS('sites/GOOSE/runs/v2.0_012021/output/AGBI_STAN_GOOSE_v2.0_012021.RDS')
nrp_total_agbi <- readRDS('sites/NORTHROUND/runs/v2.0_082020/output/AGBI_STAN_NORTHROUND_v2.0_082020.RDS')
rooster_total_agbi <- readRDS('sites/ROOSTER/runs/v2.0_082020/output/AGBI_STAN_ROOSTER_v2.0_082020.RDS')
sylv_total_agbi <- readRDS('sites/SYLVANIA/runs/v2.0_082020/output/AGBI_STAN_SYLVANIA_v2.0_082020.RDS')

# Add site names column
goose_total_agb <- dplyr::mutate(goose_total_agb, site = 'GOOSE')
nrp_total_agb <- dplyr::mutate(nrp_total_agb, site = 'NRP')
rooster_total_agb <- dplyr::mutate(rooster_total_agb, site = 'ROOSTER')
sylv_total_agb <- dplyr::mutate(sylv_total_agb, site = 'SYLVANIA')

goose_total_agbi <- dplyr::mutate(goose_total_agbi, site = 'GOOSE')
nrp_total_agbi <- dplyr::mutate(nrp_total_agbi, site = 'NRP')
rooster_total_agbi <- dplyr::mutate(rooster_total_agbi, site = 'ROOSTER')
sylv_total_agbi <- dplyr::mutate(sylv_total_agbi, site = 'SYLVANIA')

# Combine sites
total_agb <- rbind(goose_total_agb, nrp_total_agb,
                   rooster_total_agb, sylv_total_agb)
total_agbi <- rbind(goose_total_agbi, nrp_total_agbi,
                    rooster_total_agbi, sylv_total_agbi)

# Plot NPP for each species by plot and site
total_agbi |>
  dplyr::group_by(year, plot, taxon, site) |>
  dplyr::summarize(mean = mean(value)) |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = mean, color = taxon)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(plot~site) +
  ggplot2::xlab('') + ggplot2::ylab('AGBI') +
  ggplot2::theme_minimal()

# Save mean over iterations in dataframe
total_agbi <- total_agbi |>
  dplyr::group_by(year, plot, taxon, site) |>
  dplyr::summarize(mean = mean(value))

# Indexing for loops
site <- c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA')
taxa <- c()
taxa[1] <- length(unique(total_agbi$taxon[which(total_agbi$site == 'GOOSE')]))
taxa[2] <- length(unique(total_agbi$taxon[which(total_agbi$site == 'NRP')]))
taxa[3] <- length(unique(total_agbi$taxon[which(total_agbi$site == 'ROOSTER')]))
taxa[4] <- length(unique(total_agbi$taxon[which(total_agbi$site == 'SYLVANIA')]))

# Load climate data
load('climate/prism_clim.RData')

# Format
prism_long <- dplyr::rename(prism_long, site = loc) |>
  dplyr::mutate(year = as.numeric(year))

# Pivot wider
prism_annual <- prism_long |>
  dplyr::group_by(year, site) |>
  dplyr::summarize(mean_PPT = mean(PPT2),
                   mean_Tmean = mean(Tmean2)) 
prism_month <- tidyr::pivot_wider(prism_long, names_from = 'month', values_from = c('PPT2', 'Tmean2'))

# Storage
coeff_save_taxon <- matrix(, nrow = sum(taxa), ncol = 7)

row_ind <- 0
# For each site, let's iteratively fit a simple linear model with
# average temperature and precipitation as predictors of each tree's annual growth
for(i in 1:4){
  # Tree number index, unique to each site
  taxon <- seq(from = 1, to = taxa[i], by = 1)
  # Save site name
  site_name <- site[i]
  # Loop through each tree at a given site
  for(j in taxon){
    # Increment counter
    row_ind <- row_ind + 1
    # Save tree number
    taxon_name <- unique(total_agbi$taxon[which(total_agbi$site == site_name)])[j]
    # Subset full data for one tree
    sub <- dplyr::filter(total_agbi, site == site_name &
                           taxon == taxon_name)
    # Combine tree data with climate
    joined <- dplyr::left_join(x = sub, y = prism_annual, by = c('site', 'year'))
    # Fit linear model
    mod <- lm(formula = mean ~ mean_PPT + mean_Tmean + mean_PPT * mean_Tmean,
              data = joined)   
    # Save site name, tree number, coefficients, and r2 in matrix
    coeff_save_taxon[row_ind,1] <- i
    coeff_save_taxon[row_ind,2] <- taxon_name
    coeff_save_taxon[row_ind,3:6] <- coefficients(mod)
    coeff_save_taxon[row_ind,7] <- summary(mod)$adj.r.squared
    print(j)
  }
  print(paste0('---------------------',i,'----------------'))
}

# Column names
colnames(coeff_save_taxon) <- c('Site', 'Taxon', 'Intercept',
                          'Precipitation', 'Temperature', 
                          'Interaction', 'R2')
# Format
coeff_save_taxon <- as.data.frame(coeff_save_taxon)

# Replace site numbers with names
coeff_save_taxon <- coeff_save_taxon |>
  dplyr::mutate(Site = as.character(Site)) |>
  dplyr::mutate(Site = dplyr::if_else(Site == 1, 'GOOSE', Site),
                Site = dplyr::if_else(Site == 2, 'NRP', Site),
                Site = dplyr::if_else(Site == 3, 'ROOSTER', Site),
                Site = dplyr::if_else(Site == 4, 'SYLVANIA', Site)) |>
  # Format columns
  dplyr::mutate(Intercept = as.numeric(Intercept),
                Precipitation = as.numeric(Precipitation),
                Temperature = as.numeric(Temperature),
                Interaction = as.numeric(Interaction),
                R2 = as.numeric(R2))

# Distribution of R2 for each site with individual models
coeff_save_taxon |>
  ggplot2::ggplot(ggplot2::aes(x = R2)) +
  ggplot2::geom_density() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = 0), linetype = 'dashed') +
  ggplot2::xlab(expression(R^2)) + ggplot2::ylab('Density') +
  ggplot2::theme_minimal()

# Violin
coeff_save_taxon |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = R2)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2)) +
  ggplot2::theme_minimal()

# Comparison with individual models
ggplot2::ggplot() +
  ggplot2::geom_density(data = coeff_save, ggplot2::aes(x = R2, color = 'Individual')) +
  ggplot2::geom_density(data = coeff_save_taxon, ggplot2::aes(x = R2, color = 'Taxon')) +
  ggplot2::geom_vline(ggplot2::aes(xintercept = 0), linetype = 'dashed') +
  ggplot2::xlab(expression(R^2)) + ggplot2::ylab('Density') +
  ggplot2::theme_minimal() +
  ggplot2::labs(color = 'Model')

ggplot2::ggplot() +
  ggplot2::geom_violin(data = coeff_save, ggplot2::aes(x = Site, y = R2, color = 'Individual')) +
  ggplot2::geom_violin(data = coeff_save_taxon, ggplot2::aes(x = Site, y = R2, color = 'Taxon')) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2)) +
  ggplot2::theme_minimal()

# Run linear models with monthly climate

# Storage
coeff_save_taxon_month <- matrix(, nrow = sum(taxa), ncol = 52)

row_ind <- 0
# For each site, let's iteratively fit a simple linear model with
# average temperature and precipitation as predictors of each tree's annual growth
for(i in 1:4){
  # Tree number index, unique to each site
  taxon <- seq(from = 1, to = taxa[i], by = 1)
  # Save site name
  site_name <- site[i]
  # Loop through each tree at a given site
  for(j in taxon){
    # Increment counter
    row_ind <- row_ind + 1
    # Save tree number
    taxon_name <- unique(total_agbi$taxon[which(total_agbi$site == site_name)])[j]
    # Subset full data for one tree
    sub <- dplyr::filter(total_agbi, site == site_name &
                           taxon == taxon_name)
    # Combine tree data with climate
    joined <- dplyr::left_join(x = sub, y = prism_month, by = c('site', 'year'))
    # Fit linear model
    mod <- lm(formula = mean ~ PPT2_01 + PPT2_02 + PPT2_03 + PPT2_04 + PPT2_05 + PPT2_06 +
                PPT2_07 + PPT2_08 + PPT2_09 + PPT2_10 + PPT2_11 + PPT2_12 +
                Tmean2_01 + Tmean2_02 + Tmean2_03 + Tmean2_04 + Tmean2_05 + Tmean2_06 +
                Tmean2_07 + Tmean2_08 + Tmean2_09 + Tmean2_10 + Tmean2_11 + Tmean2_12,
              data = joined)   
    # Save site name, tree number, coefficients, and r2 in matrix
    coeff_save_taxon_month[row_ind,1] <- i
    coeff_save_taxon_month[row_ind,2] <- tree_number
    coeff_save_taxon_month[row_ind,3:27] <- coefficients(mod)
    coeff_save_taxon_month[row_ind,28] <- summary(mod)$adj.r.squared
    coeff_save_taxon_month[row_ind,29:52] <- car::vif(mod, type = 'predictor')
    print(j)
  }
  print(paste0('---------------------',i,'----------------'))
}

# Column names
colnames(coeff_save_taxon_month) <- c('Site', 'Taxon', 'Intercept',
                                'PPT_01', 'PPT_02', 'PPT_03', 'PPT_04', 'PPT_05', 'PPT_06',
                                'PPT_07', 'PPT_08', 'PPT_09', 'PPT_10', 'PPT_11', 'PPT_12',
                                'Tmean_01', 'Tmean_02', 'Tmean_03', 'Tmean_04', 'Tmean_05', 'Tmean_06',
                                'Tmean_07', 'Tmean_08', 'Tmean_09', 'Tmean_10', 'Tmean_11', 'Tmean_12',
                                'R2', 'VIF1', 'VIF2', 'VIF3', 'VIF4', 'VIF5', 'VIF6', 'VIF7', 'VIF8',
                                'VIF9', 'VIF10', 'VIF11', 'VIF12', 'VIF13', 'VIF14', 'VIF15', 'VIF16',
                                'VIF17', 'VIF18', 'VIF19', 'VIF20', 'VIF21', 'VIF22', 'VIF23', 'VIF24')
# Format
coeff_save_taxon_month <- as.data.frame(coeff_save_taxon_month)

# Replace site numbers with names
coeff_save_taxon_month <- coeff_save_taxon_month |>
  dplyr::mutate(Site = as.character(Site)) |>
  dplyr::mutate(Site = dplyr::if_else(Site == 1, 'GOOSE', Site),
                Site = dplyr::if_else(Site == 2, 'NRP', Site),
                Site = dplyr::if_else(Site == 3, 'ROOSTER', Site),
                Site = dplyr::if_else(Site == 4, 'SYLVANIA', Site))

# Distribution of R2 for each site with individual models
coeff_save_taxon_month |>
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

# Compare with individual models
ggplot2::ggplot() +
  ggplot2::geom_density(data = coeff_save_month, ggplot2::aes(x = R2, color = 'Individual')) +
  ggplot2::geom_density(data = coeff_save_taxon_month, ggplot2::aes(x = R2, color = 'Taxon')) +
  ggplot2::facet_wrap(~Site) +
  ggplot2::xlab(expression(R^2)) + ggplot2::ylab('Density') +
  ggplot2::theme_minimal() +
  ggplot2::labs(color = 'Model')

ggplot2::ggplot() +
  ggplot2::geom_violin(data = coeff_save_month, ggplot2::aes(x = Site, y = R2, color = 'Individual')) +
  ggplot2::geom_violin(data = coeff_save_taxon_month, ggplot2::aes(x = Site, y = R2, color = 'Taxon')) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2)) +
  ggplot2::theme_minimal() +
  ggplot2::labs(color = 'Model')
