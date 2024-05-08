## Site-level climate-AGBI relationships using linear models

rm(list = ls())

# Load detrended AGBI
load('out/taxon_detrended_AGBI.RData')

# Average over taxa and plots
save_comb <- save_comb |>
  dplyr::group_by(year, site) |>
  dplyr::summarize(residual_AGBI = mean(residual_AGBI))

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

# Storage
coeff_save_site <- matrix(, nrow = length(site), ncol = 11)

for(i in 1:4){
  site_name <- site[i]
  sub <- dplyr::filter(save_comb, site == site_name)
  joined <- sub |>
    dplyr::left_join(y = prism_annual, by = c('site', 'year'))
  
  # Fit linear model
  mod <- lm(formula = residual_AGBI ~ mean_PPT + mean_Tmean +
              sd_PPT + sd_Tmean +
              mean_Tmin + mean_Tmax +
              mean_Vpdmin + mean_Vpdmax,
            data = joined)
  # Save site name, coefficients, and r2 in matrix
  coeff_save_site[i,1] <- site_name
  coeff_save_site[i,2:10] <- coefficients(mod)
  coeff_save_site[i,11] <- summary(mod)$adj.r.squared
  
  print(paste0('-----------------------',i,'-----------------------'))
}

# Column names
colnames(coeff_save_site) <- c('Site', 'Intercept',
                               'Precipitation', 'Temperature',
                               'SD_Precipitation', 'SD_Temperature',
                               'Minimum_temperature', 'Maximum_temperature',
                               'Minimum_VPD', 'Maximum_VPD', 'R2')

# Format
coeff_save_site <- as.data.frame(coeff_save_site)

# Format columns
coeff_save_site <- coeff_save_site |>
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

# Distribution of R2 for each site 
coeff_save_site |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = R2)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::ylab(expression(R^2)) + ggplot2::ylab('') +
  ggplot2::theme_minimal()

# Save
save(coeff_save_site, file = 'out/site_lm_coeff_save.RData')

## Comparison with other levels of organization
# Load individual model output
load('out/ind_lm_coeff_save.RData')
load('out/taxon_lm_coeff_save.RData')
load('out/plot_lm_coeff_save.RData')

ggplot2::ggplot() +
  ggplot2::geom_density(data = coeff_save,
                        ggplot2::aes(x = R2, color = 'Individual')) +
  ggplot2::geom_density(data = coeff_save_taxon,
                        ggplot2::aes(x = R2, color = 'Taxon')) +
  ggplot2::geom_density(data = coeff_save_plot,
                        ggplot2::aes(x = R2, color = 'Plot')) +
  ggplot2::geom_density(data = coeff_save_site,
                        ggplot2::aes(x = R2, color = 'Site')) +
  ggplot2::xlab(expression(R^2)) + ggplot2::ylab('Density') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank())

ggplot2::ggplot() +
  ggplot2::geom_violin(data = coeff_save,
                       ggplot2::aes(x = Site, y = R2, color = 'Individual'), fill = NA) +
  ggplot2::geom_violin(data = coeff_save_taxon,
                       ggplot2::aes(x = Site, y = R2, color = 'Taxon'), fill = NA) +
  ggplot2::geom_point(data = coeff_save_plot,
                      ggplot2::aes(x = Site, y = R2, shape = 'Plot'), color = 'black') +
  ggplot2::geom_point(data = coeff_save_site,
                      ggplot2::aes(x = Site, y = R2, shape = 'Site'), color = 'black') +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2)) +
  ggplot2::theme_minimal() +
  ggplot2::scale_shape_manual(values = c(1, 16)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of precipitation coefficients
ggplot2::ggplot() +
  ggplot2::geom_violin(data = coeff_save,
                       ggplot2::aes(x = Site, y = Precipitation, color = 'Individual'), fill = NA) +
  ggplot2::geom_violin(data = coeff_save_taxon,
                       ggplot2::aes(x = Site, y = Precipitation, color = 'Taxon'), fill = NA) +
  ggplot2::geom_point(data = coeff_save_plot,
                      ggplot2::aes(x = Site, y = Precipitation, shape = 'Plot'), color = 'black') +
  ggplot2::geom_point(data = coeff_save_site,
                      ggplot2::aes(x = Site, y = Precipitation, shape = 'Site'), color = 'black') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for precipitation') +
  ggplot2::theme_minimal() +
  ggplot2::scale_shape_manual(values = c(1, 16)) +
  ggplot2::ylim(c(-0.05, 0.05)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of temperature coefficients
ggplot2::ggplot() +
  ggplot2::geom_violin(data = coeff_save,
                       ggplot2::aes(x = Site, y = Temperature, color = 'Individual'), fill = NA) +
  ggplot2::geom_violin(data = coeff_save_taxon,
                       ggplot2::aes(x = Site, y = Temperature, color = 'Taxon'), fill = NA) +
  ggplot2::geom_point(data = coeff_save_plot,
                      ggplot2::aes(x = Site, y = Temperature, shape = 'Plot'), color = 'black') +
  ggplot2::geom_point(data = coeff_save_site,
                      ggplot2::aes(x = Site, y = Temperature, shape = 'Site'), color = 'black') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for temperature') +
  ggplot2::theme_minimal() +
  ggplot2::scale_shape_manual(values = c(1, 16)) +
  ggplot2::ylim(c(-0.15, 0.15)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of precipitation seasonality coefficients
ggplot2::ggplot() +
  ggplot2::geom_violin(data = coeff_save,
                       ggplot2::aes(x = Site, y = SD_Precipitation, color = 'Individual'), fill = NA) +
  ggplot2::geom_violin(data = coeff_save_taxon,
                       ggplot2::aes(x = Site, y = SD_Precipitation, color = 'Taxon'), fill = NA) +
  ggplot2::geom_point(data = coeff_save_plot,
                      ggplot2::aes(x = Site, y = SD_Precipitation, shape = 'Plot'), color = 'black') +
  ggplot2::geom_point(data = coeff_save_site,
                      ggplot2::aes(x = Site, y = SD_Precipitation, shape = 'Site'), color = 'black') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for precipitation seasonality') +
  ggplot2::theme_minimal() +
  ggplot2::scale_shape_manual(values = c(1, 16)) +
  ggplot2::ylim(c(-0.01, 0.01)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of temperature seasonality coefficients
ggplot2::ggplot() +
  ggplot2::geom_violin(data = coeff_save,
                       ggplot2::aes(x = Site, y = SD_Temperature, color = 'Individual'), fill = NA) +
  ggplot2::geom_violin(data = coeff_save_taxon,
                       ggplot2::aes(x = Site, y = SD_Temperature, color = 'Taxon'), fill = NA) +
  ggplot2::geom_point(data = coeff_save_plot,
                      ggplot2::aes(x = Site, y = SD_Temperature, shape = 'Plot'), color = 'black') +
  ggplot2::geom_point(data = coeff_save_site,
                      ggplot2::aes(x = Site, y = SD_Temperature, shape = 'Site'), color = 'black') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for temperature seasonality') +
  ggplot2::theme_minimal() +
  ggplot2::scale_shape_manual(values = c(1, 16)) +
  ggplot2::ylim(c(-0.25, 0.25)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of minimum temperature coefficients
ggplot2::ggplot() +
  ggplot2::geom_violin(data = coeff_save,
                       ggplot2::aes(x = Site, y = Minimum_temperature, color = 'Individual'), fill = NA) +
  ggplot2::geom_violin(data = coeff_save_taxon,
                       ggplot2::aes(x = Site, y = Minimum_temperature, color = 'Taxon'), fill = NA) +
  ggplot2::geom_point(data = coeff_save_plot,
                      ggplot2::aes(x = Site, y = Minimum_temperature, shape = 'Plot'), color = 'black') +
  ggplot2::geom_point(data = coeff_save_site,
                      ggplot2::aes(x = Site, y = Minimum_temperature, shape = 'Site'), color = 'black') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for minimum temperature') +
  ggplot2::theme_minimal() +
  ggplot2::scale_shape_manual(values = c(1, 16)) +
  ggplot2::ylim(c(-0.05, 0.05)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of maximum temperature coefficients
ggplot2::ggplot() +
  ggplot2::geom_violin(data = coeff_save,
                       ggplot2::aes(x = Site, y = Maximum_temperature, color = 'Individual'), fill = NA) +
  ggplot2::geom_violin(data = coeff_save_taxon,
                       ggplot2::aes(x = Site, y = Maximum_temperature, color = 'Taxon'), fill = NA) +
  ggplot2::geom_point(data = coeff_save_plot,
                      ggplot2::aes(x = Site, y = Maximum_temperature, shape = 'Plot'), color = 'black') +
  ggplot2::geom_point(data = coeff_save_site,
                      ggplot2::aes(x = Site, y = Maximum_temperature, shape = 'Site'), color = 'black') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for maximum temperature') +
  ggplot2::theme_minimal() +
  ggplot2::scale_shape_manual(values = c(1, 16)) +
  ggplot2::ylim(c(-0.15, 0.15)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of minimum VPD coefficients
ggplot2::ggplot() +
  ggplot2::geom_violin(data = coeff_save,
                       ggplot2::aes(x = Site, y = Minimum_VPD, color = 'Individual'), fill = NA) +
  ggplot2::geom_violin(data = coeff_save_taxon,
                       ggplot2::aes(x = Site, y = Minimum_VPD, color = 'Taxon'), fill = NA) +
  ggplot2::geom_point(data = coeff_save_plot,
                      ggplot2::aes(x = Site, y = Minimum_VPD, shape = 'Plot'), color = 'black') +
  ggplot2::geom_point(data = coeff_save_site,
                      ggplot2::aes(x = Site, y = Minimum_VPD, shape = 'Site'), color = 'black') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for minimum VPD') +
  ggplot2::theme_minimal() +
  ggplot2::scale_shape_manual(values = c(1, 16)) +
  ggplot2::ylim(c(-0.5, 0.5)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of maximum VPD coefficients
ggplot2::ggplot() +
  ggplot2::geom_violin(data = coeff_save,
                       ggplot2::aes(x = Site, y = Maximum_VPD, color = 'Individual'), fill = NA) +
  ggplot2::geom_violin(data = coeff_save_taxon,
                       ggplot2::aes(x = Site, y = Maximum_VPD, color = 'Taxon'), fill = NA) +
  ggplot2::geom_point(data = coeff_save_plot,
                      ggplot2::aes(x = Site, y = Maximum_VPD, shape = 'Plot'), color = 'black') +
  ggplot2::geom_point(data = coeff_save_site,
                      ggplot2::aes(x = Site, y = Maximum_VPD, shape = 'Site'), color = 'black') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for maximum VPD') +
  ggplot2::theme_minimal() +
  ggplot2::scale_shape_manual(values = c(1, 16)) +
  ggplot2::ylim(c(-0.05, 0.05)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())
