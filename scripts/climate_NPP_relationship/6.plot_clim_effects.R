## Plot-level climate-AGBI relationships using linear models

rm(list = ls())

# Load detrended AGBI
load('out/taxon_detrended_AGBI.RData')

# Average over taxa
save_comb <- save_comb |>
  dplyr::group_by(year, plot, site) |>
  dplyr::summarize(residual_AGBI = mean(residual_AGBI))

# Indexing for loops
site <- c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA')
plot <- c()
plot[1] <- length(unique(save_comb$plot[which(save_comb$site == 'GOOSE')]))
plot[2] <- length(unique(save_comb$plot[which(save_comb$site == 'NRP')]))
plot[3] <- length(unique(save_comb$plot[which(save_comb$site == 'ROOSTER')]))
plot[4] <- length(unique(save_comb$plot[which(save_comb$site == 'SYLVANIA')]))

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

# Storage
coeff_save_plot <- matrix(, nrow = sum(plot), ncol = 12)

row_ind <- 0
# For each site, let's iteratively fit a simple linear model with
# average temperature and precipitation as predictors of each tree's annual growth
for(i in 1:4){
  # Plot number index, unique to each site
  plot_j <- seq(from = 1, to = plot[i], by = 1)
  # Save site name
  site_name <- site[i]
  # Loop through each plot at a given site
  for(j in plot_j){
    # Increment counter
    row_ind <- row_ind + 1
    # Save plot number
    plot_name <- plot_j[j]
    # Subset full data for one plot
    sub <- dplyr::filter(save_comb, site == site_name &
                           plot == plot_name)
    # Combine tree data with climate
    joined <- sub |>
      # Join with annual climate drivers
      dplyr::left_join(y = prism_annual, by = c('site', 'year'))
    
    # Fit linear model
    # annual increment of plot is a function of
    # mean annual precipitation, mean annual temperature,
    # annual precipitation seasonality, annual temperature seasonality,
    # minimum annual temperature, maximum annual temperature
    # minimum annual VPD, maximum annual VPD
    # total plot basal area
    mod <- lm(formula = residual_AGBI ~ mean_PPT + mean_Tmean +
                sd_PPT + sd_Tmean +
                mean_Tmin + mean_Tmax +
                mean_Vpdmin + mean_Vpdmax,
              data = joined)
    # Save site name, tree number, coefficients, and r2 in matrix
    coeff_save_plot[row_ind,1] <- i
    coeff_save_plot[row_ind,2] <- plot_name
    coeff_save_plot[row_ind,3:11] <- coefficients(mod)
    coeff_save_plot[row_ind,12] <- summary(mod)$adj.r.squared
    print(j)
  }
  print(paste0('-----------------------',i,'-----------------------'))
}

# Column names
colnames(coeff_save_plot) <- c('Site', 'Plot', 'Intercept',
                               'Precipitation', 'Temperature',
                               'SD_Precipitation', 'SD_Temperature',
                               'Minimum_temperature', 'Maximum_temperature',
                               'Minimum_VPD', 'Maximum_VPD','R2')

# Format
coeff_save_plot <- as.data.frame(coeff_save_plot)

# Replace site numbers with names
coeff_save_plot <- coeff_save_plot |>
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
                R2 = as.numeric(R2))

# Distribution of R2 for each site with individual models
coeff_save_plot |>
  ggplot2::ggplot(ggplot2::aes(x = Plot, y = R2)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::facet_wrap(~Site) +
  ggplot2::ylab(expression(R^2)) + ggplot2::xlab('') +
  ggplot2::theme_minimal()

# Save
save(coeff_save_plot, file = 'out/plot_lm_coeff_save.RData')

## Comparison with other levels of organization
# Load individual model output
load('out/ind_lm_coeff_save.RData')
load('out/taxon_lm_coeff_save.RData')

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
  ggplot2::ggplot() +
  ggplot2::geom_density(ggplot2::aes(x = val, color = type)) +
  ggplot2::geom_density(data = coeff_save_plot, ggplot2::aes(x = R2, color = 'Plot')) +
  ggplot2::xlab(expression(R^2)) + ggplot2::ylab('Density') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank())

coeff_save_combined |>
  dplyr::filter(var == 'R2') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_point(data = coeff_save_plot, ggplot2::aes(x = Site, y = R2, color = 'Plot'), shape = 1) +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2)) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_manual(values = 'black') +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of precipitation coefficients
coeff_save_combined |>
  dplyr::filter(var == 'Precipitation') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_point(data = coeff_save_plot, ggplot2::aes(x = Site, y = Precipitation, color = 'Plot'), shape = 1) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for precipitation') +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_manual(values = 'black') +
  ggplot2::ylim(c(-0.05, 0.05)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of temperature coefficient
coeff_save_combined |>
  dplyr::filter(var == 'Temperature') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_point(data = coeff_save_plot, ggplot2::aes(x = Site, y = Temperature, color = 'Plot'), shape = 1) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for temperature') +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_manual(values = 'black') +
  ggplot2::ylim(c(-0.15, 0.15)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of precipitation seasonality coefficient
coeff_save_combined |>
  dplyr::filter(var == 'SD_Precipitation') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_point(data = coeff_save_plot, ggplot2::aes(x = Site, y = SD_Precipitation, color = 'Plot'), shape = 1) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for precipitation seasonality') +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_manual(values = 'black') +
  ggplot2::ylim(c(-0.01, 0.01)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of temperature seasonality coefficient
coeff_save_combined |>
  dplyr::filter(var == 'SD_Temperature') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_point(data = coeff_save_plot, ggplot2::aes(x = Site, y = SD_Temperature, color = 'Plot'), shape = 1) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for temperature seasonality') +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_manual(values = 'black') +
  ggplot2::ylim(c(-0.25, 0.25)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of minimum temperature coefficient
coeff_save_combined |>
  dplyr::filter(var == 'Minimum_temperature') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_point(data = coeff_save_plot, ggplot2::aes(x = Site, y = Minimum_temperature, color = 'Plot'), shape = 1) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for minimum temperature') +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_manual(values = 'black') +
  ggplot2::ylim(c(-0.05, 0.05)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of maximum temperature coefficient
coeff_save_combined |>
  dplyr::filter(var == 'Maximum_temperature') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_point(data = coeff_save_plot, ggplot2::aes(x = Site, y = Maximum_temperature, color = 'Plot'), shape = 1) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for maximum temperature') +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_manual(values = 'black') +
  ggplot2::ylim(c(-0.15, 0.15)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of minimum VPD coefficient
coeff_save_combined |>
  dplyr::filter(var == 'Minimum_VPD') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_point(data = coeff_save_plot, ggplot2::aes(x = Site, y = Minimum_VPD, color = 'Plot'), shape = 1) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for minimum VPD') +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_manual(values = 'black') +
  ggplot2::ylim(c(-0.5, 0.5)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of maximum VPD coefficient
coeff_save_combined |>
  dplyr::filter(var == 'Maximum_VPD') |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = val, fill = type)) +
  ggplot2::geom_point(data = coeff_save_plot, ggplot2::aes(x = Site, y = Maximum_VPD, color = 'Plot'), shape = 1) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for maximum VPD') +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_manual(values = 'black') +
  ggplot2::ylim(c(-0.05, 0.05)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

