## Taxon-level climate correlations

rm(list = ls())

# Load detrended AGBI
load('out/taxon_detrended_AGBI.RData')

# Indexing for loops
site <- c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA', 'HARVARD')
taxa <- c()
taxa[1] <- length(unique(taxon_save_comb$taxon[which(taxon_save_comb$site == 'GOOSE')]))
taxa[2] <- length(unique(taxon_save_comb$taxon[which(taxon_save_comb$site == 'NRP')]))
taxa[3] <- length(unique(taxon_save_comb$taxon[which(taxon_save_comb$site == 'ROOSTER')]))
taxa[4] <- length(unique(taxon_save_comb$taxon[which(taxon_save_comb$site == 'SYLVANIA')]))
taxa[5] <- length(unique(taxon_save_comb$taxon[which(taxon_save_comb$site == 'HARVARD')]))

# Load climate data
load('climate/prism_clim.RData')

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
coeff_save_taxon <- matrix(, nrow = sum(taxa), ncol = 12)

row_ind <- 0
# For each site, let's iteratively fit a simple linear model with
# average temperature and precipitation as predictors of each tree's annual growth
for(i in 1:length(site)){
  # Taxon number index, unique to each site
  taxon <- unique(taxon_save_comb$taxon[which(taxon_save_comb$site == site[i])])
  # Save site name
  site_name <- site[i]
  # Loop through each taxon at a given site
  for(j in taxon){
    # Increment counter
    row_ind <- row_ind + 1
    # Subset full data for one taxon
    sub <- dplyr::filter(taxon_save_comb, site == site_name &
                           taxon == j)
    # Combine tree data with climate
    joined <- sub |>
      # Join with annual climate drivers
      dplyr::left_join(y = prism_growing, by = c('site', 'year'))

    # Fit linear model
    # annual increment of each taxon is a function of
    # mean annual precipitation, mean annual temperature,
    # annual precipitation seasonality, annual temperature seasonality,
    # minimum annual temperature, maximum annual temperature,
    # minimum annual VPD, maximum annual VPD,
    # fraction of basal area for given taxon,
    # total plot basal area
    mod <- lm(formula = residual_AGBI ~ PPT + Tmean + 
                sd_PPT + sd_Tmean +
                Tmin + Tmax +
                Vpdmin + Vpdmax,
              data = joined)   
    # Save site name, tree number, coefficients, and r2 in matrix
    coeff_save_taxon[row_ind,1] <- i
    coeff_save_taxon[row_ind,2] <- j
    coeff_save_taxon[row_ind,3:11] <- coefficients(mod)
    coeff_save_taxon[row_ind,12] <- summary(mod)$adj.r.squared
    print(j)
  }
  print(paste0('---------------------',i,'----------------'))
}

# Column names
colnames(coeff_save_taxon) <- c('Site', 'Taxon', 'Intercept',
                                'Precipitation', 'Temperature',
                                'SD_Precipitation', 'SD_Temperature',
                                'Minimum_temperature', 'Maximum_temperature',
                                'Minimum_VPD', 'Maximum_VPD', 'R2')
# Format
coeff_save_taxon <- as.data.frame(coeff_save_taxon)

# Replace site numbers with names
coeff_save_taxon <- coeff_save_taxon |>
  dplyr::mutate(Site = as.character(Site)) |>
  dplyr::mutate(Site = dplyr::if_else(Site == 1, 'GOOSE', Site),
                Site = dplyr::if_else(Site == 2, 'NRP', Site),
                Site = dplyr::if_else(Site == 3, 'ROOSTER', Site),
                Site = dplyr::if_else(Site == 4, 'SYLVANIA', Site),
                Site = dplyr::if_else(Site == 5, 'HARVARD', Site)) |>
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
  ggplot2::geom_violin(fill = NA) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed') +
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
  ggplot2::ggplot(ggplot2::aes(x = Site, y = val, color = type)) +
  ggplot2::geom_violin(fill = NA) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of precipitation coefficients
ci <- coeff_save_combined |>
  dplyr::filter(type == 'Taxon') |>
  tidyr::pivot_wider(names_from = 'var', values_from = 'val') |>
  dplyr::summarize(lower = quantile(Precipitation, probs = 0.025, na.rm = TRUE),
                   upper = quantile(Precipitation, probs = 0.975, na.rm = TRUE))

coeff_save_combined |>
  dplyr::filter(var == 'Precipitation') |>
  dplyr::filter(val > ci$lower & val < ci$upper) |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for precipitation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of temperature coefficient
ci <- coeff_save_combined |>
  dplyr::filter(type == 'Taxon') |>
  tidyr::pivot_wider(names_from = 'var', values_from = 'val') |>
  dplyr::summarize(lower = quantile(Temperature, probs = 0.025, na.rm = TRUE),
                   upper = quantile(Temperature, probs = 0.975, na.rm = TRUE))

coeff_save_combined |>
  dplyr::filter(var == 'Temperature') |>
  dplyr::filter(val > ci$lower & val < ci$upper) |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for temperature') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of precipitation seasonality coefficient
ci <- coeff_save_combined |>
  dplyr::filter(type == 'Taxon') |>
  tidyr::pivot_wider(names_from = 'var', values_from = 'val') |>
  dplyr::summarize(lower = quantile(SD_Precipitation, probs = 0.025, na.rm = TRUE),
                   upper = quantile(SD_Precipitation, probs = 0.975, na.rm = TRUE))

coeff_save_combined |>
  dplyr::filter(var == 'SD_Precipitation') |>
  dplyr::filter(val > ci$lower & val < ci$upper) |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for precipitation seasonality') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of temperature seasonality coefficient
ci <- coeff_save_combined |>
  dplyr::filter(type == 'Taxon') |>
  tidyr::pivot_wider(names_from = 'var', values_from = 'val') |>
  dplyr::summarize(lower = quantile(SD_Temperature, probs = 0.025, na.rm = TRUE),
                   upper = quantile(SD_Temperature, probs = 0.975, na.rm = TRUE))

coeff_save_combined |>
  dplyr::filter(var == 'SD_Temperature') |>
  dplyr::filter(val > ci$lower & val < ci$upper) |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for temperature seasonality') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of minimum temperature coefficient
ci <- coeff_save_combined |>
  dplyr::filter(type == 'Taxon') |>
  tidyr::pivot_wider(names_from = 'var', values_from = 'val') |>
  dplyr::summarize(lower = quantile(Minimum_temperature, probs = 0.025, na.rm = TRUE),
                   upper = quantile(Minimum_temperature, probs = 0.975, na.rm = TRUE))

coeff_save_combined |>
  dplyr::filter(var == 'Minimum_temperature') |>
  dplyr::filter(val > ci$lower & val < ci$upper) |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for minimum temperature') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of maximum temperature coefficient
ci <- coeff_save_combined |>
  dplyr::filter(type == 'Taxon') |>
  tidyr::pivot_wider(names_from = 'var', values_from = 'val') |>
  dplyr::summarize(lower = quantile(Minimum_temperature, probs = 0.025, na.rm = TRUE),
                   upper = quantile(Minimum_temperature, probs = 0.975, na.rm = TRUE))

coeff_save_combined |>
  dplyr::filter(var == 'Maximum_temperature') |>
  dplyr::filter(val > ci$lower & val < ci$upper) |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for maximum temperature') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of minimum VPD coefficient
ci <- coeff_save_combined |>
  dplyr::filter(type == 'Taxon') |>
  tidyr::pivot_wider(names_from = 'var', values_from = 'val') |>
  dplyr::summarize(lower = quantile(Minimum_VPD, probs = 0.025, na.rm = TRUE),
                   upper = quantile(Minimum_VPD, probs = 0.975, na.rm = TRUE))

coeff_save_combined |>
  dplyr::filter(var == 'Minimum_VPD') |>
  dplyr::filter(val > ci$lower & val < ci$upper) |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for minimum VPD') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of maximum VPD coefficient
ci <- coeff_save_combined |>
  dplyr::filter(type == 'Taxon') |>
  tidyr::pivot_wider(names_from = 'var', values_from = 'val') |>
  dplyr::summarize(lower = quantile(Maximum_VPD, probs = 0.025, na.rm = TRUE),
                   upper = quantile(Maximum_VPD, probs = 0.975, na.rm = TRUE))

coeff_save_combined |>
  dplyr::filter(var == 'Maximum_VPD') |>
  dplyr::filter(val > ci$lower & val < ci$upper) |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for maximum VPD') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank())
