rm(list = ls())

load('out/ind_lm_coeff_save.RData')

# Plot individual-level lms by: tree size, species, plot

#### By species ####
sp_col <- c("#35618f", '#35618f', "#c9dd87", "#b00091", '#b00091', '#b00091', "#9bea30", "#c9dd87", "#c9dd87", "#a143f9", "#709f0f", "#c9dd87", "#8b6fed", "#8b6fed", "#fcd107", "#4233a6", "#62ecb6")

# R2
coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = R2, color = Taxon)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = R2), fill = NA) +
  ggplot2::theme_minimal() +
  #ggplot2::scale_color_manual(values = sp_col) +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2))

coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Taxon, y = R2, fill = Taxon)) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed') +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~Site) +
  #ggplot2::scale_fill_manual(values = sp_col) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2))

# Precipitation
coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Precipitation, color = Taxon)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Precipitation), fill = NA) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed') +
  ggplot2::theme_minimal() +
  #ggplot2::scale_color_manual(values = sp_col) +
  ggplot2::ylim(c(-0.2, 0.3)) +
  ggplot2::xlab('') + ggplot2::ylab('Precipitation coefficient')

coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Taxon, y = Precipitation, fill = Taxon)) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::scale_fill_manual(values = sp_col) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('') + ggplot2::ylab('Precipitation coefficient') +
  ggplot2::ylim(c(-0.05, 0.2))

# Temperature
coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Temperature, color = Taxon)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_manual(values = sp_col) +
 ggplot2::ylim(c(-6,5)) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature coefficient')

coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Taxon, y = Temperature, fill = Taxon)) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::scale_fill_manual(values = sp_col) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature coefficient') +
  ggplot2::ylim(c(-3, 1))

# SD Precipitation
coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Precipitation, color = Taxon)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_manual(values = sp_col) +
  ggplot2::ylim(c(-0.25,0.25)) +
  ggplot2::xlab('') + ggplot2::ylab('Precipitation variation coefficient')

coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Taxon, y = SD_Precipitation, fill = Taxon)) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::scale_fill_manual(values = sp_col) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('') + ggplot2::ylab('Precipitation variation coefficient') +
  ggplot2::ylim(c(-0.1, 0.1))

# SD Temperature
coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Temperature, color = Taxon)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_manual(values = sp_col) +
 ggplot2::ylim(c(-10,6)) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature variation coefficient')

coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Taxon, y = SD_Temperature, fill = Taxon)) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::scale_fill_manual(values = sp_col) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature variation coefficient') +
  ggplot2::ylim(c(-5, 5))

# Minimum temperature
coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_temperature, color = Taxon)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_manual(values = sp_col) +
  ggplot2::ylim(c(-2,2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum temperature coefficient')

coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Taxon, y = Minimum_temperature, fill = Taxon)) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::scale_fill_manual(values = sp_col) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum temperature coefficient') +
  ggplot2::ylim(c(-1, 1))

# Maximum temperature
coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_temperature, color = Taxon)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_manual(values = sp_col) +
  ggplot2::ylim(c(-2,2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum temperature coefficient')

coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Taxon, y = Maximum_temperature, fill = Taxon)) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::scale_fill_manual(values = sp_col) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum temperature coefficient') +
  ggplot2::ylim(c(-1, 1))

# Minimum VPD
coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_VPD, color = Taxon)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_manual(values = sp_col) +
  ggplot2::ylim(c(-2,2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum VPD coefficient')

coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Taxon, y = Minimum_VPD, fill = Taxon)) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::scale_fill_manual(values = sp_col) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum VPD coefficient') +
  ggplot2::ylim(c(-1, 1))

# Maximum VPD
coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_VPD, color = Taxon)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_manual(values = sp_col) +
  ggplot2::ylim(c(-2,2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum VPD coefficient')

coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Taxon, y = Maximum_VPD, fill = Taxon)) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::scale_fill_manual(values = sp_col) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum VPD coefficient') +
  ggplot2::ylim(c(-1, 1))

# Basal area of individual trees
coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = BA, color = Taxon)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = BA), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_manual(values = sp_col) +
  ggplot2::ylim(c(-2,2)) +
  ggplot2::xlab('') + ggplot2::ylab('Basal area coefficient')

coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Taxon, y = BA, fill = Taxon)) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::scale_fill_manual(values = sp_col) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('') + ggplot2::ylab('Basal area coefficient') +
  ggplot2::ylim(c(-1, 1))

#### By plot ####

# R2
coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = R2, color = Plot)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = R2), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2))

coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Taxon, y = R2, fill = Plot)) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2))

# Precipitation
coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Precipitation, color = Plot)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::ylim(c(-0.2, 0.3)) +
  ggplot2::xlab('') + ggplot2::ylab('Precipitation coefficient')

coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Taxon, y = Precipitation, fill = Plot)) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('') + ggplot2::ylab('Precipitation coefficient') +
  ggplot2::ylim(c(-0.05, 0.2))

# Temperature
coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Temperature, color = Plot)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::ylim(c(-6,5)) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature coefficient')

coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Taxon, y = Temperature, fill = Plot)) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature coefficient') +
  ggplot2::ylim(c(-3, 1))

# SD Precipitation
coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Precipitation, color = Plot)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::ylim(c(-0.25,0.25)) +
  ggplot2::xlab('') + ggplot2::ylab('Precipitation variation coefficient')

coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Taxon, y = SD_Precipitation, fill = Plot)) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('') + ggplot2::ylab('Precipitation variation coefficient') +
  ggplot2::ylim(c(-0.1, 0.1))

# SD Temperature
coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Temperature, color = Plot)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::ylim(c(-10,6)) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature variation coefficient')

coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Taxon, y = SD_Temperature, fill = Plot)) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature variation coefficient') +
  ggplot2::ylim(c(-5, 5))

# Minimum temperature
coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_temperature, color = Plot)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::ylim(c(-2,2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum temperature coefficient')

coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Taxon, y = Minimum_temperature, fill = Plot)) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum temperature coefficient') +
  ggplot2::ylim(c(-1, 1))

# Maximum temperature
coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_temperature, color = Plot)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::ylim(c(-2,2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum temperature coefficient')

coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Taxon, y = Maximum_temperature, fill = Plot)) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum temperature coefficient') +
  ggplot2::ylim(c(-1, 1))

# Minimum VPD
coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_VPD, color = Plot)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::ylim(c(-2,2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum VPD coefficient')

coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Taxon, y = Minimum_VPD, fill = Plot)) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum VPD coefficient') +
  ggplot2::ylim(c(-1, 1))

# Maximum VPD
coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_VPD, color = Plot)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::ylim(c(-2,2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum VPD coefficient')

coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Taxon, y = Maximum_VPD, fill = Plot)) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum VPD coefficient') +
  ggplot2::ylim(c(-1, 1))

# Basal area of individual trees
coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = BA, color = Plot)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = BA), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::ylim(c(-2,2)) +
  ggplot2::xlab('') + ggplot2::ylab('Basal area coefficient')

coeff_save |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = Taxon, y = BA, fill = Plot)) +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('') + ggplot2::ylab('Basal area coefficient') +
  ggplot2::ylim(c(-1, 1))

#### By tree size ####

GOOSE_size <- readRDS('sites/GOOSE/runs/v2.0_012021/output/DBH_STAN_GOOSE_v2.0_012021.RDS')
NRP_size <- readRDS('sites/NORTHROUND/runs/v2.0_082020/output/DBH_STAN_NORTHROUND_v2.0_082020.RDS')
ROOSTER_size <- readRDS('sites/ROOSTER/runs/v2.0_082020/output/DBH_STAN_ROOSTER_v2.0_082020.RDS')
SYLV_size <- readRDS('sites/SYLVANIA/runs/v2.0_082020/output/DBH_STAN_SYLVANIA_v2.0_082020.RDS')

GOOSE_size <- GOOSE_size |>
  dplyr::select(-model, -type) |>
  dplyr::group_by(tree, year, plot, taxon) |>
  dplyr::summarize(value = mean(value)) |>
  dplyr::mutate(Site = 'GOOSE')
NRP_size <- NRP_size |>
  dplyr::select(-model, -type) |>
  dplyr::group_by(tree, year, plot, taxon) |>
  dplyr::summarize(value = mean(value)) |>
  dplyr::mutate(Site = 'NRP')
ROOSTER_size <- ROOSTER_size |>
  dplyr::select(-model, -type) |>
  dplyr::group_by(tree, year, plot, taxon) |>
  dplyr::summarize(value = mean(value)) |>
  dplyr::mutate(Site = 'ROOSTER')
SYLV_size <- SYLV_size |>
  dplyr::select(-model, -type) |>
  dplyr::group_by(tree, year, plot, taxon) |>
  dplyr::summarize(value = mean(value)) |>
  dplyr::mutate(Site = 'SYLVANIA')

size <- rbind(GOOSE_size, NRP_size, ROOSTER_size, SYLV_size)

coeff_size_1960 <- size |>
  dplyr::filter(year == 1960) |>
  dplyr::rename(Tree = tree,
                Plot = plot,
                Taxon = taxon,
                DBH = value) |>
  dplyr::mutate(Tree = as.character(Tree),
                Plot = as.character(Plot)) |>
  dplyr::right_join(coeff_save, by = c('Tree', 'Plot', 'Taxon', 'Site'))
coeff_size_1985 <- size |>
  dplyr::filter(year == 1985) |>
  dplyr::rename(Tree = tree,
                Plot = plot,
                Taxon = taxon,
                DBH = value) |>
  dplyr::mutate(Tree = as.character(Tree),
                Plot = as.character(Plot)) |>
  dplyr::right_join(coeff_save, by = c('Tree', 'Plot', 'Taxon', 'Site'))
coeff_size_2010 <- size |>
  dplyr::filter(year == 2010) |>
  dplyr::rename(Tree = tree,
                Plot = plot,
                Taxon = taxon,
                DBH = value) |>
  dplyr::mutate(Tree = as.character(Tree),
                Plot = as.character(Plot)) |>
  dplyr::right_join(coeff_save, by = c('Tree', 'Plot', 'Taxon', 'Site'))
coeff_growth <- size |>
  dplyr::group_by(tree, plot, taxon, Site) |>
  dplyr::summarize(slope = coefficients(lm(formula = value ~ year))[2]) |>
  dplyr::rename(Tree = tree,
                Plot = plot,
                Taxon = taxon,
                Rate = slope) |>
  dplyr::mutate(Tree = as.character(Tree),
                Plot = as.character(Plot)) |>
  dplyr::right_join(coeff_save, by = c('Tree', 'Plot', 'Taxon', 'Site'))

## Size at start of series

# R2
coeff_size_1960 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = R2, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = R2), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2))

# Precipitation
coeff_size_1960 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Precipitation, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-0.2, 0.3)) +
  ggplot2::xlab('') + ggplot2::ylab('Precipitation coefficient')

# Temperature
coeff_size_1960 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(-6, 5) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature coefficient')

# SD Precipitation
coeff_size_1960 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Precipitation, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-0.25, 0.25)) +
  ggplot2::xlab('') + ggplot2::ylab('Precpititation variation coefficient')

# SD Temperature
coeff_size_1960 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(-10, 6) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature variation coefficient')

# Minimum temperature
coeff_size_1960 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum temperature coefficient')

# Maximum temperature
coeff_size_1960 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum temperature coefficient')

# Minimum VPD
coeff_size_1960 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_VPD, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum VPD coefficient')

# Maximum VPD
coeff_size_1960 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_VPD, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum VPD coefficient')

# Basal area of individual trees
coeff_size_1960 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = BA, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = BA), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-1, 1)) +
  ggplot2::xlab('') + ggplot2::ylab('Basal area coefficient')

## Separating eastern sites & Sylvania

# R2
p1 <- coeff_size_1960 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = R2, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = R2), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(0.6, -0.6)) +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2))
p2 <- coeff_size_1960 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = R2, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = R2), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(0.6, -0.6)) +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2))
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Precipitation
p1 <- coeff_size_1960 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Precipitation, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-0.2, 0.3)) +
  ggplot2::xlab('') + ggplot2::ylab('Precipitation coefficient')
p2 <- coeff_size_1960 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Precipitation, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-0.2, 0.3)) +
  ggplot2::xlab('') + ggplot2::ylab('Precipitation coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Temperature
p1 <- coeff_size_1960 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(-6, 5) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature coefficient')
p2 <- coeff_size_1960 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(-6, 5) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# SD Precipitation
p1 <- coeff_size_1960 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Precipitation, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-0.25, 0.25)) +
  ggplot2::xlab('') + ggplot2::ylab('Precpititation variation coefficient')
p2 <- coeff_size_1960 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Precipitation, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-0.25, 0.25)) +
  ggplot2::xlab('') + ggplot2::ylab('Precpititation variation coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# SD Temperature
p1 <- coeff_size_1960 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(-10, 6) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature variation coefficient')
p2 <- coeff_size_1960 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(-10, 6) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature variation coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Minimum temperature
p1 <- coeff_size_1960 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum temperature coefficient')
p2 <- coeff_size_1960 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum temperature coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Maximum temperature
p1 <- coeff_size_1960 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum temperature coefficient')
p2 <- coeff_size_1960 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum temperature coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Minimum VPD
p1 <- coeff_size_1960 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_VPD, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum VPD coefficient')
p2 <- coeff_size_1960 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_VPD, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum VPD coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Maximum VPD
p1 <- coeff_size_1960 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_VPD, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum VPD coefficient')
p2 <- coeff_size_1960 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_VPD, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum VPD coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Basal area of individual trees
p1 <- coeff_size_1960 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = BA, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = BA), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-1, 1)) +
  ggplot2::xlab('') + ggplot2::ylab('Basal area coefficient')
p2 <- coeff_size_1960 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = BA, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = BA), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-1, 1)) +
  ggplot2::xlab('') + ggplot2::ylab('Basal area coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

## Size in middle of series

# R2
coeff_size_1985 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = R2, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = R2), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2))

# Precipitation
coeff_size_1985 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Precipitation, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-0.2, 0.3)) +
  ggplot2::xlab('') + ggplot2::ylab('Precipitation coefficient')

# Temperature
coeff_size_1985 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(-6, 5) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature coefficient')

# SD Precipitation
coeff_size_1985 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Precipitation, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-0.25, 0.25)) +
  ggplot2::xlab('') + ggplot2::ylab('Precpititation variation coefficient')

# SD Temperature
coeff_size_1985 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(-10, 6) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature variation coefficient')

# Minimum temperature
coeff_size_1985 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum temperature coefficient')

# Maximum temperature
coeff_size_1985 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum temperature coefficient')

# Minimum VPD
coeff_size_1985 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_VPD, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum VPD coefficient')

# Maximum VPD
coeff_size_1985 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_VPD, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum VPD coefficient')

# Basal area of individual trees
coeff_size_1985 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = BA, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = BA), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-1, 1)) +
  ggplot2::xlab('') + ggplot2::ylab('Basal area coefficient')

## Separating eastern sites & Sylvania

# R2
p1 <- coeff_size_1985 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = R2, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = R2), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(0.6, -0.6)) +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2))
p2 <- coeff_size_1985 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = R2, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = R2), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(0.6, -0.6)) +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2))
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Precipitation
p1 <- coeff_size_1985 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Precipitation, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-0.2, 0.3)) +
  ggplot2::xlab('') + ggplot2::ylab('Precipitation coefficient')
p2 <- coeff_size_1985 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Precipitation, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-0.2, 0.3)) +
  ggplot2::xlab('') + ggplot2::ylab('Precipitation coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Temperature
p1 <- coeff_size_1985 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(-6, 5) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature coefficient')
p2 <- coeff_size_1985 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(-6, 5) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# SD Precipitation
p1 <- coeff_size_1985 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Precipitation, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-0.25, 0.25)) +
  ggplot2::xlab('') + ggplot2::ylab('Precpititation variation coefficient')
p2 <- coeff_size_1985 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Precipitation, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-0.25, 0.25)) +
  ggplot2::xlab('') + ggplot2::ylab('Precpititation variation coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# SD Temperature
p1 <- coeff_size_1985 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(-10, 6) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature variation coefficient')
p2 <- coeff_size_1985 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(-10, 6) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature variation coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Minimum temperature
p1 <- coeff_size_1985 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum temperature coefficient')
p2 <- coeff_size_1985 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum temperature coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Maximum temperature
p1 <- coeff_size_1985 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum temperature coefficient')
p2 <- coeff_size_1985 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum temperature coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Minimum VPD
p1 <- coeff_size_1985 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_VPD, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum VPD coefficient')
p2 <- coeff_size_1985 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_VPD, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum VPD coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Maximum VPD
p1 <- coeff_size_1985 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_VPD, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum VPD coefficient')
p2 <- coeff_size_1985 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_VPD, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum VPD coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Basal area of individual trees
p1 <- coeff_size_1985 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = BA, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = BA), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-1, 1)) +
  ggplot2::xlab('') + ggplot2::ylab('Basal area coefficient')
p2 <- coeff_size_1985 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = BA, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = BA), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-1, 1)) +
  ggplot2::xlab('') + ggplot2::ylab('Basal area coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

## Size at end of series

# R2
coeff_size_2010 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = R2, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = R2), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2))

# Precipitation
coeff_size_2010 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Precipitation, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-0.2, 0.3)) +
  ggplot2::xlab('') + ggplot2::ylab('Precipitation coefficient')

# Temperature
coeff_size_2010 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(-6, 5) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature coefficient')

# SD Precipitation
coeff_size_2010 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Precipitation, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-0.25, 0.25)) +
  ggplot2::xlab('') + ggplot2::ylab('Precpititation variation coefficient')

# SD Temperature
coeff_size_2010 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(-10, 6) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature variation coefficient')

# Minimum temperature
coeff_size_2010 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum temperature coefficient')

# Maximum temperature
coeff_size_2010 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum temperature coefficient')

# Minimum VPD
coeff_size_2010 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_VPD, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum VPD coefficient')

# Maximum VPD
coeff_size_2010 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_VPD, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum VPD coefficient')

# Basal area of individual trees
coeff_size_2010 |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = BA, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = BA), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-1, 1)) +
  ggplot2::xlab('') + ggplot2::ylab('Basal area coefficient')

## Separating eastern sites & Sylvania

# R2
p1 <- coeff_size_2010 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = R2, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = R2), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(0.6, -0.6)) +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2))
p2 <- coeff_size_2010 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = R2, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = R2), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(0.6, -0.6)) +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2))
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Precipitation
p1 <- coeff_size_2010 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Precipitation, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-0.2, 0.3)) +
  ggplot2::xlab('') + ggplot2::ylab('Precipitation coefficient')
p2 <- coeff_size_2010 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Precipitation, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-0.2, 0.3)) +
  ggplot2::xlab('') + ggplot2::ylab('Precipitation coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Temperature
p1 <- coeff_size_2010 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(-6, 5) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature coefficient')
p2 <- coeff_size_2010 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(-6, 5) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# SD Precipitation
p1 <- coeff_size_2010 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Precipitation, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-0.25, 0.25)) +
  ggplot2::xlab('') + ggplot2::ylab('Precpititation variation coefficient')
p2 <- coeff_size_2010 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Precipitation, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-0.25, 0.25)) +
  ggplot2::xlab('') + ggplot2::ylab('Precpititation variation coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# SD Temperature
p1 <- coeff_size_2010 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(-10, 6) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature variation coefficient')
p2 <- coeff_size_2010 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(-10, 6) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature variation coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Minimum temperature
p1 <- coeff_size_2010 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum temperature coefficient')
p2 <- coeff_size_2010 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum temperature coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Maximum temperature
p1 <- coeff_size_2010 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum temperature coefficient')
p2 <- coeff_size_2010 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_temperature, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum temperature coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Minimum VPD
p1 <- coeff_size_2010 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_VPD, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum VPD coefficient')
p2 <- coeff_size_2010 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_VPD, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum VPD coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Maximum VPD
p1 <- coeff_size_2010 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_VPD, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum VPD coefficient')
p2 <- coeff_size_2010 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_VPD, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum VPD coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Basal area of individual trees
p1 <- coeff_size_2010 |>
  dplyr::filter(Site == 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = BA, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = BA), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-1, 1)) +
  ggplot2::xlab('') + ggplot2::ylab('Basal area coefficient')
p2 <- coeff_size_2010 |>
  dplyr::filter(Site != 'SYLVANIA') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = BA, color = DBH)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = BA), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'H') +
  ggplot2::ylim(c(-1, 1)) +
  ggplot2::xlab('') + ggplot2::ylab('Basal area coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

## Growth rate through series

# R2
coeff_growth |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = R2, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = R2), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2))

# Precipitation
coeff_growth |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Precipitation, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(c(-0.2, 0.3)) +
  ggplot2::xlab('') + ggplot2::ylab('Precipitation coefficient')

# Temperature
coeff_growth |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Temperature, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(-6, 5) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature coefficient')

# SD Precipitation
coeff_growth |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Precipitation, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(c(-0.25, 0.25)) +
  ggplot2::xlab('') + ggplot2::ylab('Precpititation variation coefficient')

# SD Temperature
coeff_growth |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Temperature, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(-10, 6) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature variation coefficient')

# Minimum temperature
coeff_growth |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_temperature, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum temperature coefficient')

# Maximum temperature
coeff_growth |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_temperature, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum temperature coefficient')

# Minimum VPD
coeff_growth |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_VPD, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum VPD coefficient')

# Maximum VPD
coeff_growth |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_VPD, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum VPD coefficient')

# Basal area of individual trees
coeff_growth |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = BA, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = BA), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(c(-1, 1)) +
  ggplot2::xlab('') + ggplot2::ylab('Basal area coefficient')

## Separating NRP

# R2
p1 <- coeff_growth |>
  dplyr::filter(Site == 'NRP') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = R2, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = R2), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(c(0.6, -0.6)) +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2))
p2 <- coeff_growth |>
  dplyr::filter(Site != 'NRP') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = R2, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = R2), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(c(0.6, -0.6)) +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2))
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Precipitation
p1 <- coeff_growth |>
  dplyr::filter(Site == 'NRP') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Precipitation, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(c(-0.2, 0.3)) +
  ggplot2::xlab('') + ggplot2::ylab('Precipitation coefficient')
p2 <- coeff_growth |>
  dplyr::filter(Site != 'NRP') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Precipitation, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(c(-0.2, 0.3)) +
  ggplot2::xlab('') + ggplot2::ylab('Precipitation coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Temperature
p1 <- coeff_growth |>
  dplyr::filter(Site == 'NRP') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Temperature, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(-6, 5) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature coefficient')
p2 <- coeff_growth |>
  dplyr::filter(Site != 'NRP') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Temperature, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(-6, 5) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# SD Precipitation
p1 <- coeff_growth |>
  dplyr::filter(Site == 'NRP') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Precipitation, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(c(-0.25, 0.25)) +
  ggplot2::xlab('') + ggplot2::ylab('Precpititation variation coefficient')
p2 <- coeff_growth |>
  dplyr::filter(Site != 'NRP') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Precipitation, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Precipitation), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(c(-0.25, 0.25)) +
  ggplot2::xlab('') + ggplot2::ylab('Precpititation variation coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# SD Temperature
p1 <- coeff_growth |>
  dplyr::filter(Site == 'NRP') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Temperature, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(-10, 6) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature variation coefficient')
p2 <- coeff_growth |>
  dplyr::filter(Site != 'NRP') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = SD_Temperature, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = SD_Temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(-10, 6) +
  ggplot2::xlab('') + ggplot2::ylab('Temperature variation coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Minimum temperature
p1 <- coeff_growth |>
  dplyr::filter(Site == 'NRP') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_temperature, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum temperature coefficient')
p2 <- coeff_growth |>
  dplyr::filter(Site != 'NRP') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_temperature, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum temperature coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Maximum temperature
p1 <- coeff_growth |>
  dplyr::filter(Site == 'NRP') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_temperature, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum temperature coefficient')
p2 <- coeff_growth |>
  dplyr::filter(Site != 'NRP') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_temperature, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_temperature), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum temperature coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Minimum VPD
p1 <- coeff_growth |>
  dplyr::filter(Site == 'NRP') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_VPD, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum VPD coefficient')
p2 <- coeff_growth |>
  dplyr::filter(Site != 'NRP') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Minimum_VPD, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Minimum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Minimum VPD coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Maximum VPD
p1 <- coeff_growth |>
  dplyr::filter(Site == 'NRP') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_VPD, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum VPD coefficient')
p2 <- coeff_growth |>
  dplyr::filter(Site != 'NRP') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = Maximum_VPD, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = Maximum_VPD), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(c(-2, 2)) +
  ggplot2::xlab('') + ggplot2::ylab('Maximum VPD coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))

# Basal area of individual trees
p1 <- coeff_growth |>
  dplyr::filter(Site == 'NRP') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = BA, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = BA), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(c(-1, 1)) +
  ggplot2::xlab('') + ggplot2::ylab('Basal area coefficient')
p2 <- coeff_growth |>
  dplyr::filter(Site != 'NRP') |>
  ggplot2::ggplot() +
  ggplot2::geom_jitter(ggplot2::aes(x = Site, y = BA, color = Rate)) +
  ggplot2::geom_violin(ggplot2::aes(x = Site, y = BA), fill = NA) +
  ggplot2::theme_minimal() +
  ggplot2::scale_color_viridis_c(option = 'A') +
  ggplot2::ylim(c(-1, 1)) +
  ggplot2::xlab('') + ggplot2::ylab('Basal area coefficient')
cowplot::plot_grid(p1, p2, nrow = 1, rel_widths = c(0.3, 0.7))
