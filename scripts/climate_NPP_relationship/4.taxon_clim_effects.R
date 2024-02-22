rm(list = ls())

# Load total increment
goose_total_agbi <- readRDS('sites/GOOSE/runs/v2.0_012021/output/AGBI_STAN_GOOSE_v2.0_012021.RDS')
nrp_total_agbi <- readRDS('sites/NORTHROUND/runs/v2.0_082020/output/AGBI_STAN_NORTHROUND_v2.0_082020.RDS')
rooster_total_agbi <- readRDS('sites/ROOSTER/runs/v2.0_082020/output/AGBI_STAN_ROOSTER_v2.0_082020.RDS')
sylv_total_agbi <- readRDS('sites/SYLVANIA/runs/v2.0_082020/output/AGBI_STAN_SYLVANIA_v2.0_082020.RDS')

# Subset for 1960 and beyond to reduce problem of fading record
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

# Storage
coeff_save_taxon <- matrix(, nrow = sum(taxa), ncol = 14)

row_ind <- 0
# For each site, let's iteratively fit a simple linear model with
# average temperature and precipitation as predictors of each tree's annual growth
for(i in 1:4){
  # Taxon number index, unique to each site
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
    joined <- sub |>
      # Join with annual climate drivers
      dplyr::left_join(y = prism_annual, by = c('site', 'year')) |>
      # Join with taxon-level competition metrics
      dplyr::left_join(y = ba_by_taxon, by = c('plot', 'site', 'year', 'taxon')) |>
      # only keep fraction of total basal area from each taxon (frac)
      dplyr::select(-total_ba.x, -total_ba.y) |>
      # Join with plot-level competition metrics
      dplyr::left_join(y = total_ba, by = c('plot', 'site', 'year'))
    
    # Fit linear model
    # annual increment of individual tree is a function of
    # mean annual precipitation, mean annual temperature,
    # annual precipitation seasonality, annual temperature seasonality,
    # minimum annual temperature, maximum annual temperature,
    # minimum annual VPD, maximum annual VPD,
    # fraction of basal area for given taxon,
    # total plot basal area
    mod <- lm(formula = mean ~ mean_PPT + mean_Tmean + 
                sd_PPT + sd_Tmean +
                mean_Tmin + mean_Tmax +
                mean_Vpdmin + mean_Vpdmax +
                frac + total_ba,
              data = joined)   
    # Save site name, tree number, coefficients, and r2 in matrix
    coeff_save_taxon[row_ind,1] <- i
    coeff_save_taxon[row_ind,2] <- taxon_name
    coeff_save_taxon[row_ind,3:13] <- coefficients(mod)
    coeff_save_taxon[row_ind,14] <- summary(mod)$adj.r.squared
    print(j)
  }
  print(paste0('---------------------',i,'----------------'))
}

# Column names
colnames(coeff_save_taxon) <- c('Site', 'Taxon', 'Intercept',
                                'Precipitation', 'Temperature',
                                'SD_Precipitation', 'SD_Temperature',
                                'Minimum_temperature', 'Maximum_temperature',
                                'Minimum_VPD', 'Maximum_VPD',
                                'frac_ba', 'total_ba', 'R2')
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
                SD_Precipitation = as.numeric(SD_Precipitation),
                SD_Temperature = as.numeric(SD_Temperature),
                Minimum_temperature = as.numeric(Minimum_temperature),
                Maximum_temperature = as.numeric(Maximum_temperature),
                Minimum_VPD = as.numeric(Minimum_VPD),
                Maximum_VPD = as.numeric(Maximum_VPD),
                frac_ba = as.numeric(frac_ba),
                total_ba = as.numeric(total_ba),
                R2 = as.numeric(R2))

# Distribution of R2 for each site with individual models
coeff_save_taxon |>
  ggplot2::ggplot(ggplot2::aes(x = R2)) +
  ggplot2::geom_density() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::xlab(expression(R^2)) + ggplot2::ylab('Density') +
  ggplot2::theme_minimal()

# Violin
coeff_save_taxon |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = R2)) +
  ggplot2::geom_violin() +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2)) +
  ggplot2::theme_minimal()

# Save 
save(coeff_save_taxon, file = 'out/taxon_lm_coeff_save.RData')

## Comparison with individual models
# Load individual model output
load('out/ind_lm_coeff_save.RData')

# Format to combine dataframes
coeff_save <- coeff_save |>
  dplyr::mutate(type = 'Individual') |>
  tidyr::pivot_longer(cols = Intercept:R2, names_to = 'var', values_to = 'val')
coeff_save_taxon <- coeff_save_taxon |>
  dplyr::mutate(type = 'Taxon') |>
  tidyr::pivot_longer(cols = Intercept:R2, names_to = 'var', values_to = 'val')
coeff_save_combined <- coeff_save |>
  dplyr::full_join(y = coeff_save_taxon, 
                   by = c('Site', 'Taxon', 'var', 'type')) |>
  dplyr::mutate(val = dplyr::if_else(!is.na(val.x), val.x, val.y))

coeff_save_combined |>
  dplyr::filter(var == 'R2') |>
  ggplot2::ggplot(ggplot2::aes(x = val, color = type)) +
  ggplot2::geom_density() +
  ggplot2::xlab(expression(R^2)) + ggplot2::ylab('Density') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank())

coeff_save_combined |>
  dplyr::filter(var == 'R2') |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_violin() +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of precipitation coefficients
coeff_save_combined |>
  dplyr::filter(var == 'Precipitation') |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for precipitation') +
  ggplot2::theme_minimal() +
  ggplot2::ylim(c(-0.2, 0.3)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of temperature coefficient
coeff_save_combined |>
  dplyr::filter(var == 'Temperature') |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for temperature') +
  ggplot2::theme_minimal() +
  ggplot2::ylim(c(-5, 5)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of precipitation seasonality coefficient
coeff_save_combined |>
  dplyr::filter(var == 'SD_Precipitation') |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for precipitation seasonality') +
  ggplot2::theme_minimal() +
  ggplot2::ylim(c(-0.25, 0.25)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of temperature seasonality coefficient
coeff_save_combined |>
  dplyr::filter(var == 'SD_Temperature') |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for temperature seasonality') +
  ggplot2::theme_minimal() +
  ggplot2::ylim(c(-5, 5)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of minimum temperature coefficient
coeff_save_combined |>
  dplyr::filter(var == 'Minimum_temperature') |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for minimum temperature') +
  ggplot2::theme_minimal() +
  ggplot2::ylim(c(-1, 1)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of maximum temperature coefficient
coeff_save_combined |>
  dplyr::filter(var == 'Maximum_temperature') |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for maximum temperature') +
  ggplot2::theme_minimal() +
  ggplot2::ylim(c(-2, 3)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of minimum VPD coefficient
coeff_save_combined |>
  dplyr::filter(var == 'Minimum_VPD') |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for minimum VPD') +
  ggplot2::theme_minimal() +
  ggplot2::ylim(c(-10, 10)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of maximum VPD coefficient
coeff_save_combined |>
  dplyr::filter(var == 'Maximum_VPD') |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for maximum VPD') +
  ggplot2::theme_minimal() +
  ggplot2::ylim(c(-1, 1)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of fraction basal area/species coefficient
coeff_save_combined |>
  dplyr::filter(var == 'frac_ba') |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for taxon fraction basal area') +
  ggplot2::theme_minimal() +
  ggplot2::ylim(c(-500, 750)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of total plot basal area
coeff_save_combined |>
  dplyr::filter(var == 'total_ba') |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for plot basal area') +
  ggplot2::theme_minimal() +
  ggplot2::ylim(c(-0.005, 0.005)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())