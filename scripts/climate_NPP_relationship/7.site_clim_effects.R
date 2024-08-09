## Site-level climate-AGBI relationships using linear models

rm(list = ls())

# Load detrended AGBI
load('out/taxon_detrended_AGBI.RData')

# Average over taxa and plots
save_comb <- taxon_save_comb |>
  dplyr::group_by(year, site) |>
  dplyr::summarize(residual_AGBI = mean(residual_AGBI))

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

# Storage
coeff_save_site <- matrix(, nrow = length(site), ncol = 11)

for(i in 1:length(site)){
  site_name <- site[i]
  sub <- dplyr::filter(save_comb, site == site_name)
  joined <- sub |>
    dplyr::left_join(y = prism_growing, by = c('site', 'year'))
  
  # Fit linear model
  mod <- lm(formula = residual_AGBI ~ PPT + Tmean +
              sd_PPT + sd_Tmean +
              Tmin + Tmax +
              Vpdmin + Vpdmax,
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
                      ggplot2::aes(x = Site, y = R2, color = 'Plot'), shape = 1) +
  ggplot2::geom_point(data = coeff_save_site,
                      ggplot2::aes(x = Site, y = R2, color = 'Site'), shape = 16) +
  
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2)) +
  ggplot2::theme_minimal() +
  ggplot2::scale_shape_manual(values = c(1, 16)) +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of precipitation coefficients
ci <- coeff_save_taxon |>
  dplyr::summarize(lower = quantile(Precipitation, probs = 0.025, na.rm = TRUE),
                   upper = quantile(Precipitation, probs = 0.975, na.rm = TRUE))

ggplot2::ggplot() +
  ggplot2::geom_violin(data = dplyr::filter(coeff_save, Precipitation > ci$lower & Precipitation < ci$upper),
                       ggplot2::aes(x = Site, y = Precipitation, color = 'Individual'), 
                       fill = NA) +
  ggplot2::geom_violin(data = dplyr::filter(coeff_save_taxon, Precipitation > ci$lower & Precipitation < ci$upper),
                       ggplot2::aes(x = Site, y = Precipitation, color = 'Taxon'), 
                       fill = NA) +
  ggplot2::geom_point(data = dplyr::filter(coeff_save_plot, Precipitation > ci$lower & Precipitation < ci$upper),
                      ggplot2::aes(x = Site, y = Precipitation, color = 'Plot'), shape = 1) +
  ggplot2::geom_point(data = dplyr::filter(coeff_save_site, Precipitation > ci$lower & Precipitation < ci$upper),
                      ggplot2::aes(x = Site, y = Precipitation, color = 'Site'), shape = 16) +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for precipitation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of temperature coefficients
ci <- coeff_save_taxon |>
  dplyr::summarize(lower = quantile(Temperature, probs = 0.025, na.rm = TRUE),
                   upper = quantile(Temperature, probs = 0.975, na.rm = TRUE))

ggplot2::ggplot() +
  ggplot2::geom_violin(data = dplyr::filter(coeff_save, Temperature > ci$lower & Temperature < ci$upper),
                       ggplot2::aes(x = Site, y = Temperature, color = 'Individual'), fill = NA) +
  ggplot2::geom_violin(data = dplyr::filter(coeff_save_taxon, Temperature > ci$lower & Temperature < ci$upper),
                       ggplot2::aes(x = Site, y = Temperature, color = 'Taxon'), fill = NA) +
  ggplot2::geom_point(data = dplyr::filter(coeff_save_plot, Temperature > ci$lower & Temperature < ci$upper),
                      ggplot2::aes(x = Site, y = Temperature, color = 'Plot'), shape = 1) +
  ggplot2::geom_point(data = dplyr::filter(coeff_save_site, Temperature > ci$lower & Temperature < ci$upper),
                      ggplot2::aes(x = Site, y = Temperature, color = 'Site'), shape = 16) +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for temperature') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of precipitation seasonality coefficients
ci <- coeff_save_taxon |>
  dplyr::summarize(lower = quantile(SD_Precipitation, probs = 0.025, na.rm = TRUE),
                   upper = quantile(SD_Precipitation, probs = 0.975, na.rm = TRUE))

ggplot2::ggplot() +
  #ggplot2::geom_violin(data = dplyr::filter(coeff_save, SD_Precipitation > ci$lower & SD_Precipitation < ci$upper),
  #                     ggplot2::aes(x = Site, y = SD_Precipitation, color = 'Individual'), fill = NA) +
  ggplot2::geom_violin(data = dplyr::filter(coeff_save_taxon, SD_Precipitation > ci$lower & SD_Precipitation < ci$upper),
                       ggplot2::aes(x = Site, y = SD_Precipitation, color = 'Taxon'), fill = NA) +
  ggplot2::geom_point(data = dplyr::filter(coeff_save_plot, SD_Precipitation > ci$lower & SD_Precipitation < ci$upper),
                      ggplot2::aes(x = Site, y = SD_Precipitation, color = 'Plot'), shape = 1) +
  ggplot2::geom_point(data = dplyr::filter(coeff_save_site, SD_Precipitation > ci$lower & SD_Precipitation < ci$upper),
                      ggplot2::aes(x = Site, y = SD_Precipitation, color = 'Site'), shape = 16) +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for precipitation seasonality') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of temperature seasonality coefficients
ci <- coeff_save_taxon |>
  dplyr::summarize(lower = quantile(SD_Temperature, probs = 0.025, na.rm = TRUE),
                   upper = quantile(SD_Temperature, probs = 0.975, na.rm = TRUE))

ggplot2::ggplot() +
  ggplot2::geom_violin(data = dplyr::filter(coeff_save, SD_Temperature > ci$lower & SD_Temperature < ci$upper),
                       ggplot2::aes(x = Site, y = SD_Temperature, color = 'Individual'), fill = NA) +
  ggplot2::geom_violin(data = dplyr::filter(coeff_save_taxon, SD_Temperature > ci$lower & SD_Temperature < ci$upper),
                       ggplot2::aes(x = Site, y = SD_Temperature, color = 'Taxon'), fill = NA) +
  ggplot2::geom_point(data = dplyr::filter(coeff_save_plot, SD_Temperature > ci$lower & SD_Temperature < ci$upper),
                      ggplot2::aes(x = Site, y = SD_Temperature, color = 'Plot'), shape = 1) +
  ggplot2::geom_point(data = dplyr::filter(coeff_save_site, SD_Temperature > ci$lower & SD_Temperature < ci$upper),
                      ggplot2::aes(x = Site, y = SD_Temperature, color = 'Site'), shape = 16) +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for temperature seasonality') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of minimum temperature coefficients
ci <- coeff_save_taxon |>
  dplyr::summarize(lower = quantile(Minimum_temperature, probs = 0.025, na.rm = TRUE),
                   upper = quantile(Minimum_temperature, probs = 0.975, na.rm = TRUE))

ggplot2::ggplot() +
  #ggplot2::geom_violin(data = dplyr::filter(coeff_save, Minimum_temperature > ci$lower & Minimum_temperature < ci$upper),
  #                     ggplot2::aes(x = Site, y = Minimum_temperature, color = 'Individual'), fill = NA) +
  ggplot2::geom_violin(data = dplyr::filter(coeff_save_taxon, Minimum_temperature > ci$lower & Minimum_temperature < ci$upper),
                       ggplot2::aes(x = Site, y = Minimum_temperature, color = 'Taxon'), fill = NA) +
  ggplot2::geom_point(data = dplyr::filter(coeff_save_plot, Minimum_temperature > ci$lower & Minimum_temperature < ci$upper),
                      ggplot2::aes(x = Site, y = Minimum_temperature, color = 'Plot'), shape = 1) +
  ggplot2::geom_point(data = dplyr::filter(coeff_save_site, Minimum_temperature > ci$lower & Minimum_temperature < ci$upper),
                      ggplot2::aes(x = Site, y = Minimum_temperature, color = 'Site'), shape = 16) +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for minimum temperature') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of maximum temperature coefficients
ci <- coeff_save_taxon |>
  dplyr::summarize(lower = quantile(Maximum_temperature, probs = 0.025, na.rm = TRUE),
                   upper = quantile(Maximum_temperature, probs = 0.975, na.rm = TRUE))

ggplot2::ggplot() +
  #ggplot2::geom_violin(data = dplyr::filter(coeff_save, Maximum_temperature > ci$lower & Maximum_temperature < ci$upper),
  #                     ggplot2::aes(x = Site, y = Maximum_temperature, color = 'Individual'), fill = NA) +
  ggplot2::geom_violin(data = dplyr::filter(coeff_save_taxon, Maximum_temperature > ci$lower & Maximum_temperature < ci$upper),
                       ggplot2::aes(x = Site, y = Maximum_temperature, color = 'Taxon'), fill = NA) +
  ggplot2::geom_point(data = dplyr::filter(coeff_save_plot, Maximum_temperature > ci$lower & Maximum_temperature < ci$upper),
                      ggplot2::aes(x = Site, y = Maximum_temperature, color = 'Plot'), shape = 1) +
  ggplot2::geom_point(data = dplyr::filter(coeff_save_site, Maximum_temperature > ci$lower & Maximum_temperature < ci$upper),
                      ggplot2::aes(x = Site, y = Maximum_temperature, color = 'Site'), shape = 16) +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for maximum temperature') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of minimum VPD coefficients
ci <- coeff_save_taxon |>
  dplyr::summarize(lower = quantile(Minimum_VPD, probs = 0.025, na.rm = TRUE),
                   upper = quantile(Minimum_VPD, probs = 0.975, na.rm = TRUE))

ggplot2::ggplot() +
  #ggplot2::geom_violin(data = dplyr::filter(coeff_save, Minimum_VPD > ci$lower & Minimum_VPD < ci$upper),
  #                     ggplot2::aes(x = Site, y = Minimum_VPD, color = 'Individual'), fill = NA) +
  ggplot2::geom_violin(data = dplyr::filter(coeff_save_taxon, Minimum_VPD > ci$lower & Minimum_VPD < ci$upper),
                       ggplot2::aes(x = Site, y = Minimum_VPD, color = 'Taxon'), fill = NA) +
  ggplot2::geom_point(data = dplyr::filter(coeff_save_plot, Minimum_VPD > ci$lower & Minimum_VPD < ci$upper),
                      ggplot2::aes(x = Site, y = Minimum_VPD, color = 'Plot'), shape = 1) +
  ggplot2::geom_point(data = dplyr::filter(coeff_save_site, Minimum_VPD > ci$lower & Minimum_VPD < ci$upper),
                      ggplot2::aes(x = Site, y = Minimum_VPD, color = 'Site'), shape = 16) +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for minimum VPD') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank())

# Violin of maximum VPD coefficients
ci <- coeff_save_taxon |>
  dplyr::summarize(lower = quantile(Maximum_VPD, probs = 0.025, na.rm = TRUE),
                   upper = quantile(Maximum_VPD, probs = 0.975, na.rm = TRUE))

ggplot2::ggplot() +
  ggplot2::geom_violin(data = dplyr::filter(coeff_save, Maximum_VPD > ci$lower & Maximum_VPD < ci$upper),
                       ggplot2::aes(x = Site, y = Maximum_VPD, color = 'Individual'), fill = NA) +
  ggplot2::geom_violin(data = dplyr::filter(coeff_save_taxon, Maximum_VPD > ci$lower & Maximum_VPD < ci$upper),
                       ggplot2::aes(x = Site, y = Maximum_VPD, color = 'Taxon'), fill = NA) +
  ggplot2::geom_point(data = dplyr::filter(coeff_save_plot, Maximum_VPD > ci$lower & Maximum_VPD < ci$upper),
                      ggplot2::aes(x = Site, y = Maximum_VPD, color = 'Plot'), shape = 1) +
  ggplot2::geom_point(data = dplyr::filter(coeff_save_site, Maximum_VPD > ci$lower & Maximum_VPD < ci$upper),
                      ggplot2::aes(x = Site, y = Maximum_VPD, color = 'Site'), shape = 16) +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for maximum VPD') +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank())

