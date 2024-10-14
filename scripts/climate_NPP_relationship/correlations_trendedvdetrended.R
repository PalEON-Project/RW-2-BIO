## Checking correlations between climate variables before and after detrending

rm(list = ls())

# Load climate variables
load('climate/prism_clim.RData')

# Pivot wider
prism_monthly <- prism_long |>
  dplyr::select(-Vpdmin2) |>
  dplyr::group_by(loc) |>
  tidyr::pivot_wider(names_from = 'month',
                     values_from = c('PPT2', 'Tmean2', 'Tmin2', 
                                     'Tmax2', 'Vpdmax2')) |>
  dplyr::mutate(year = as.numeric(year))

# Growing season climate summaries
# Pivot wider
prism_growing <- prism_long |>
  dplyr::mutate(year = as.numeric(year)) |>
  dplyr::mutate(growing_year = dplyr::if_else(month %in% c('01', '02', '03', '04',
                                                           '05', '06', '07', '08'),
                                              year, year + 1)) |>
  dplyr::group_by(growing_year, loc) |>
  dplyr::summarize(PPT_growing = mean(PPT2),
                   Tmean_growing = mean(Tmean2),
                   sd_PPT_growing = sd(PPT2),
                   sd_Tmean_growing = sd(Tmean2),
                   Tmin_growing = min(Tmin2),
                   Tmax_growing = max(Tmax2),
                   Vpdmin_growing = min(Vpdmin2),
                   Vpdmax_growing = max(Vpdmax2)) |>
  dplyr::rename(year = growing_year)

#### Individual-level ####

# Load detrended individual AGBI
load('out/tree_detrended_AGBI.RData')

# Combine climate and AGBI
clim_ind_agbi <- save_comb |>
  dplyr::mutate(loc = dplyr::if_else(site == 'ROOSTER', 'ROOSTER', NA),
                loc = dplyr::if_else(site == 'NRP', 'NRP', loc),
                loc = dplyr::if_else(site == 'SYLVANIA', 'SYLVANIA', loc),
                loc = dplyr::if_else(site == 'GOOSE', 'GOOSE', loc),
                loc = dplyr::if_else(site == 'HARVARD Model RW', 'HARVARD', loc),
                loc = dplyr::if_else(site == 'HARVARD Model RW + Census', 'HARVARD', loc)) |>
  dplyr::left_join(y = prism_monthly,
                   by = c('year', 'loc')) |>
  dplyr::left_join(y = prism_growing,
                   by = c('year', 'loc'))

# Correlations for each individual untrended/trended AGBI by site
goose <- clim_ind_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'GOOSE') |>
  dplyr::select(mean, PPT2_01:Vpdmax_growing)

p1 <- goose |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = mean)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = mean), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p1

cor_goose <- cor(x = goose[,1],
                 y = goose[,2:69])

cor_goose <- t(cor_goose)
cor_goose <- as.data.frame(cor_goose)

p2 <- cor_goose |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = mean),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p2

goose2 <- clim_ind_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'GOOSE') |>
  dplyr::select(residual_AGBI, PPT2_01:Vpdmax_growing)

p3 <- goose2 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = residual_AGBI)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = residual_AGBI), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p3

cor_goose2 <- cor(x = goose2[,1],
                  y = goose2[,2:69])

cor_goose2 <- t(cor_goose2)
cor_goose2 <- as.data.frame(cor_goose2)

p4 <- cor_goose2 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = residual_AGBI),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p4

p1 # versus
p3

cowplot::plot_grid(p2, p4)

# North Round Pond
nrp <- clim_ind_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'NRP') |>
  dplyr::select(mean, PPT2_01:Vpdmax_growing)

p1 <- nrp |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = mean)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = mean), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p1

cor_nrp <- cor(x = nrp[,1],
               y = nrp[,2:69])

cor_nrp <- t(cor_nrp)
cor_nrp <- as.data.frame(cor_nrp)

p2 <- cor_nrp |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = mean),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p2

nrp2 <- clim_ind_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'NRP') |>
  dplyr::select(residual_AGBI, PPT2_01:Vpdmax_growing)

p3 <- nrp2 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = residual_AGBI)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = residual_AGBI), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p3

cor_nrp2 <- cor(x = nrp2[,1],
                y = nrp2[,2:69])

cor_nrp2 <- t(cor_nrp2)
cor_nrp2 <- as.data.frame(cor_nrp2)

p4 <- cor_nrp2 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = residual_AGBI),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p4

p1 # versus
p3

cowplot::plot_grid(p2, p4)

# Rooster Hill
