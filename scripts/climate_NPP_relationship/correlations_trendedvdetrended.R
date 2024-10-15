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
rooster <- clim_ind_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'ROOSTER') |>
  dplyr::select(mean, PPT2_01:Vpdmax_growing)

p1 <- rooster |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = mean)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = mean), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p1

cor_rooster <- cor(x = rooster[,1],
                   y = rooster[,2:69])

cor_rooster <- t(cor_rooster)
cor_rooster <- as.data.frame(cor_rooster)

p2 <- cor_rooster |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = mean),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p2

rooster2 <- clim_ind_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'ROOSTER') |>
  dplyr::select(residual_AGBI, PPT2_01:Vpdmax_growing)

p3 <- rooster2 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = residual_AGBI)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = residual_AGBI), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p3

cor_rooster2 <- cor(x = rooster2[,1],
                    y = rooster2[,2:69])

cor_rooster2 <- t(cor_rooster2)
cor_rooster2 <- as.data.frame(cor_rooster2)

p4 <- cor_rooster2 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = residual_AGBI),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p4

p1 # versus
p3

cowplot::plot_grid(p2, p4)

# Sylvania
sylv <- clim_ind_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'SYLVANIA') |>
  dplyr::select(mean, PPT2_01:Vpdmax_growing)

p1 <- sylv |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = mean)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = mean), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p1

cor_sylv <- cor(x = sylv[,1],
                y = sylv[,2:69])

cor_sylv <- t(cor_sylv)
cor_sylv <- as.data.frame(cor_sylv)

p2 <- cor_sylv |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = mean),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p2

sylv2 <- clim_ind_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'SYLVANIA') |>
  dplyr::select(residual_AGBI, PPT2_01:Vpdmax_growing)

p3 <- sylv2 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = residual_AGBI)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = residual_AGBI), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p3

cor_sylv2 <- cor(x = sylv2[,1],
                 y = sylv2[,2:69])

cor_sylv2 <- t(cor_sylv2)
cor_sylv2 <- as.data.frame(cor_sylv2)

p4 <- cor_sylv2 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = residual_AGBI),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p4

p1 # versus
p3

cowplot::plot_grid(p2, p4)

# Harvard RW only
hf1 <- clim_ind_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'HARVARD Model RW') |>
  dplyr::select(mean, PPT2_01:Vpdmax_growing)

p1 <- hf1 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = mean)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = mean), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p1

cor_hf1 <- cor(x = hf1[,1],
               y = hf1[,2:69])

cor_hf1 <- t(cor_hf1)
cor_hf1 <- as.data.frame(cor_hf1)

p2 <- cor_hf1 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = mean),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p2

hf2 <- clim_ind_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'HARVARD Model RW') |>
  dplyr::select(residual_AGBI, PPT2_01:Vpdmax_growing)

p3 <- hf2 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = residual_AGBI)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = residual_AGBI), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p3

cor_hf2 <- cor(x = hf2[,1],
               y = hf2[,2:69])

cor_hf2 <- t(cor_hf2)
cor_hf2 <- as.data.frame(cor_hf2)

p4 <- cor_hf2 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = residual_AGBI),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p4

p1 # versus
p3

cowplot::plot_grid(p2, p4)

# Harvard RW + Census
hf3 <- clim_ind_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'HARVARD Model RW + Census') |>
  dplyr::select(mean, PPT2_01:Vpdmax_growing)

p1 <- hf3 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = mean)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = mean), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p1

cor_hf3 <- cor(x = hf3[,1],
               y = hf3[,2:69])

cor_hf3 <- t(cor_hf3)
cor_hf3 <- as.data.frame(cor_hf3)

p2 <- cor_hf3 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = mean),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p2

hf4 <- clim_ind_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'HARVARD Model RW + Census') |>
  dplyr::select(residual_AGBI, PPT2_01:Vpdmax_growing)

p3 <- hf4 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = residual_AGBI)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = residual_AGBI), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p3

cor_hf4 <- cor(x = hf4[,1],
               y = hf4[,2:69])

cor_hf4 <- t(cor_hf4)
cor_hf4 <- as.data.frame(cor_hf4)

p4 <- cor_hf4 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = residual_AGBI),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p4

p1 # versus
p3

cowplot::plot_grid(p2, p4)

#### Taxon-level ####

# Load detrended taxon-level AGBI
load('out/taxon_detrended_AGBI.RData')

# Combine climate and AGBI
clim_tax_agbi <- taxon_save_comb |>
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
goose <- clim_tax_agbi |>
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

goose2 <- clim_tax_agbi |>
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
nrp <- clim_tax_agbi |>
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

nrp2 <- clim_tax_agbi |>
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
rooster <- clim_tax_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'ROOSTER') |>
  dplyr::select(mean, PPT2_01:Vpdmax_growing)

p1 <- rooster |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = mean)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = mean), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p1

cor_rooster <- cor(x = rooster[,1],
                   y = rooster[,2:69])

cor_rooster <- t(cor_rooster)
cor_rooster <- as.data.frame(cor_rooster)

p2 <- cor_rooster |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = mean),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p2

rooster2 <- clim_tax_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'ROOSTER') |>
  dplyr::select(residual_AGBI, PPT2_01:Vpdmax_growing)

p3 <- rooster2 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = residual_AGBI)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = residual_AGBI), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p3

cor_rooster2 <- cor(x = rooster2[,1],
                    y = rooster2[,2:69])

cor_rooster2 <- t(cor_rooster2)
cor_rooster2 <- as.data.frame(cor_rooster2)

p4 <- cor_rooster2 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = residual_AGBI),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p4

p1 # versus
p3

cowplot::plot_grid(p2, p4)

# Sylvania
sylv <- clim_tax_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'SYLVANIA') |>
  dplyr::select(mean, PPT2_01:Vpdmax_growing)

p1 <- sylv |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = mean)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = mean), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p1

cor_sylv <- cor(x = sylv[,1],
                y = sylv[,2:69])

cor_sylv <- t(cor_sylv)
cor_sylv <- as.data.frame(cor_sylv)

p2 <- cor_sylv |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = mean),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p2

sylv2 <- clim_tax_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'SYLVANIA') |>
  dplyr::select(residual_AGBI, PPT2_01:Vpdmax_growing)

p3 <- sylv2 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = residual_AGBI)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = residual_AGBI), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p3

cor_sylv2 <- cor(x = sylv2[,1],
                 y = sylv2[,2:69])

cor_sylv2 <- t(cor_sylv2)
cor_sylv2 <- as.data.frame(cor_sylv2)

p4 <- cor_sylv2 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = residual_AGBI),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p4

p1 # versus
p3

cowplot::plot_grid(p2, p4)

# Harvard RW only
hf1 <- clim_tax_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'HARVARD Model RW') |>
  dplyr::select(mean, PPT2_01:Vpdmax_growing)

p1 <- hf1 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = mean)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = mean), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p1

cor_hf1 <- cor(x = hf1[,1],
               y = hf1[,2:69])

cor_hf1 <- t(cor_hf1)
cor_hf1 <- as.data.frame(cor_hf1)

p2 <- cor_hf1 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = mean),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p2

hf2 <- clim_tax_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'HARVARD Model RW') |>
  dplyr::select(residual_AGBI, PPT2_01:Vpdmax_growing)

p3 <- hf2 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = residual_AGBI)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = residual_AGBI), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p3

cor_hf2 <- cor(x = hf2[,1],
               y = hf2[,2:69])

cor_hf2 <- t(cor_hf2)
cor_hf2 <- as.data.frame(cor_hf2)

p4 <- cor_hf2 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = residual_AGBI),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p4

p1 # versus
p3

cowplot::plot_grid(p2, p4)

# Harvard RW + Census
hf3 <- clim_tax_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'HARVARD Model RW + Census') |>
  dplyr::select(mean, PPT2_01:Vpdmax_growing)

p1 <- hf3 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = mean)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = mean), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p1

cor_hf3 <- cor(x = hf3[,1],
               y = hf3[,2:69])

cor_hf3 <- t(cor_hf3)
cor_hf3 <- as.data.frame(cor_hf3)

p2 <- cor_hf3 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = mean),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p2

hf4 <- clim_tax_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'HARVARD Model RW + Census') |>
  dplyr::select(residual_AGBI, PPT2_01:Vpdmax_growing)

p3 <- hf4 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = residual_AGBI)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = residual_AGBI), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p3

cor_hf4 <- cor(x = hf4[,1],
               y = hf4[,2:69])

cor_hf4 <- t(cor_hf4)
cor_hf4 <- as.data.frame(cor_hf4)

p4 <- cor_hf4 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = residual_AGBI),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p4

p1 # versus
p3

cowplot::plot_grid(p2, p4)

#### Plot-level ####

# Load detrended plot level AGBI
load('out/plot_detrended_AGBI.RData')

# Combine climate and AGBI
clim_plot_agbi <- plot_save_comb |>
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
goose <- clim_plot_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'GOOSE') |>
  dplyr::select(plot_agbi, PPT2_01:Vpdmax_growing)

p1 <- goose |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = plot_agbi)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = plot_agbi), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p1

cor_goose <- cor(x = goose[,1],
                 y = goose[,2:69])

cor_goose <- t(cor_goose)
cor_goose <- as.data.frame(cor_goose)

p2 <- cor_goose |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = plot_agbi),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p2

goose2 <- clim_plot_agbi |>
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
nrp <- clim_plot_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'NRP') |>
  dplyr::select(plot_agbi, PPT2_01:Vpdmax_growing)

p1 <- nrp |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = plot_agbi)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = plot_agbi), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p1

cor_nrp <- cor(x = nrp[,1],
               y = nrp[,2:69])

cor_nrp <- t(cor_nrp)
cor_nrp <- as.data.frame(cor_nrp)

p2 <- cor_nrp |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = plot_agbi),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p2

nrp2 <- clim_plot_agbi |>
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
rooster <- clim_plot_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'ROOSTER') |>
  dplyr::select(plot_agbi, PPT2_01:Vpdmax_growing)

p1 <- rooster |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = plot_agbi)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = plot_agbi), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p1

cor_rooster <- cor(x = rooster[,1],
                   y = rooster[,2:69])

cor_rooster <- t(cor_rooster)
cor_rooster <- as.data.frame(cor_rooster)

p2 <- cor_rooster |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = plot_agbi),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p2

rooster2 <- clim_plot_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'ROOSTER') |>
  dplyr::select(residual_AGBI, PPT2_01:Vpdmax_growing)

p3 <- rooster2 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = residual_AGBI)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = residual_AGBI), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p3

cor_rooster2 <- cor(x = rooster2[,1],
                    y = rooster2[,2:69])

cor_rooster2 <- t(cor_rooster2)
cor_rooster2 <- as.data.frame(cor_rooster2)

p4 <- cor_rooster2 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = residual_AGBI),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p4

p1 # versus
p3

cowplot::plot_grid(p2, p4)

# Sylvania
sylv <- clim_plot_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'SYLVANIA') |>
  dplyr::select(plot_agbi, PPT2_01:Vpdmax_growing)

p1 <- sylv |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = plot_agbi)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = plot_agbi), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p1

cor_sylv <- cor(x = sylv[,1],
                y = sylv[,2:69])

cor_sylv <- t(cor_sylv)
cor_sylv <- as.data.frame(cor_sylv)

p2 <- cor_sylv |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = plot_agbi),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p2

sylv2 <- clim_plot_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'SYLVANIA') |>
  dplyr::select(residual_AGBI, PPT2_01:Vpdmax_growing)

p3 <- sylv2 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = residual_AGBI)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = residual_AGBI), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p3

cor_sylv2 <- cor(x = sylv2[,1],
                 y = sylv2[,2:69])

cor_sylv2 <- t(cor_sylv2)
cor_sylv2 <- as.data.frame(cor_sylv2)

p4 <- cor_sylv2 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = residual_AGBI),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p4

p1 # versus
p3

cowplot::plot_grid(p2, p4)

# Harvard RW only
hf1 <- clim_plot_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'HARVARD Model RW') |>
  dplyr::select(plot_agbi, PPT2_01:Vpdmax_growing)

p1 <- hf1 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = plot_agbi)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = plot_agbi), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p1

cor_hf1 <- cor(x = hf1[,1],
               y = hf1[,2:69])

cor_hf1 <- t(cor_hf1)
cor_hf1 <- as.data.frame(cor_hf1)

p2 <- cor_hf1 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = plot_agbi),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p2

hf2 <- clim_plot_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'HARVARD Model RW') |>
  dplyr::select(residual_AGBI, PPT2_01:Vpdmax_growing)

p3 <- hf2 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = residual_AGBI)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = residual_AGBI), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p3

cor_hf2 <- cor(x = hf2[,1],
               y = hf2[,2:69])

cor_hf2 <- t(cor_hf2)
cor_hf2 <- as.data.frame(cor_hf2)

p4 <- cor_hf2 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = residual_AGBI),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p4

p1 # versus
p3

cowplot::plot_grid(p2, p4)

# Harvard RW + Census
hf3 <- clim_plot_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'HARVARD Model RW + Census') |>
  dplyr::select(plot_agbi, PPT2_01:Vpdmax_growing)

p1 <- hf3 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = plot_agbi)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = plot_agbi), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p1

cor_hf3 <- cor(x = hf3[,1],
               y = hf3[,2:69])

cor_hf3 <- t(cor_hf3)
cor_hf3 <- as.data.frame(cor_hf3)

p2 <- cor_hf3 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = plot_agbi),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p2

hf4 <- clim_plot_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'HARVARD Model RW + Census') |>
  dplyr::select(residual_AGBI, PPT2_01:Vpdmax_growing)

p3 <- hf4 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = residual_AGBI)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = residual_AGBI), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p3

cor_hf4 <- cor(x = hf4[,1],
               y = hf4[,2:69])

cor_hf4 <- t(cor_hf4)
cor_hf4 <- as.data.frame(cor_hf4)

p4 <- cor_hf4 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = residual_AGBI),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p4

p1 # versus
p3

cowplot::plot_grid(p2, p4)

#### Site-level ####

# Load detrended site level AGBI
load('out/site_detrended_AGBI.RData')

# Combine climate and AGBI
clim_site_agbi <- site_save_comb |>
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
goose <- clim_site_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'GOOSE') |>
  dplyr::select(site_agbi, PPT2_01:Vpdmax_growing)

p1 <- goose |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = site_agbi)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = site_agbi), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p1

cor_goose <- cor(x = goose[,1],
                 y = goose[,2:69])

cor_goose <- t(cor_goose)
cor_goose <- as.data.frame(cor_goose)

p2 <- cor_goose |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = site_agbi),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p2

goose2 <- clim_site_agbi |>
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
nrp <- clim_site_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'NRP') |>
  dplyr::select(site_agbi, PPT2_01:Vpdmax_growing)

p1 <- nrp |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = site_agbi)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = site_agbi), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p1

cor_nrp <- cor(x = nrp[,1],
               y = nrp[,2:69])

cor_nrp <- t(cor_nrp)
cor_nrp <- as.data.frame(cor_nrp)

p2 <- cor_nrp |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = site_agbi),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p2

nrp2 <- clim_site_agbi |>
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
rooster <- clim_site_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'ROOSTER') |>
  dplyr::select(site_agbi, PPT2_01:Vpdmax_growing)

p1 <- rooster |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = site_agbi)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = site_agbi), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p1

cor_rooster <- cor(x = rooster[,1],
                   y = rooster[,2:69])

cor_rooster <- t(cor_rooster)
cor_rooster <- as.data.frame(cor_rooster)

p2 <- cor_rooster |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = site_agbi),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p2

rooster2 <- clim_site_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'ROOSTER') |>
  dplyr::select(residual_AGBI, PPT2_01:Vpdmax_growing)

p3 <- rooster2 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = residual_AGBI)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = residual_AGBI), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p3

cor_rooster2 <- cor(x = rooster2[,1],
                    y = rooster2[,2:69])

cor_rooster2 <- t(cor_rooster2)
cor_rooster2 <- as.data.frame(cor_rooster2)

p4 <- cor_rooster2 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = residual_AGBI),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p4

p1 # versus
p3

cowplot::plot_grid(p2, p4)

# Sylvania
sylv <- clim_site_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'SYLVANIA') |>
  dplyr::select(site_agbi, PPT2_01:Vpdmax_growing)

p1 <- sylv |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = site_agbi)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = site_agbi), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p1

cor_sylv <- cor(x = sylv[,1],
                y = sylv[,2:69])

cor_sylv <- t(cor_sylv)
cor_sylv <- as.data.frame(cor_sylv)

p2 <- cor_sylv |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = site_agbi),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p2

sylv2 <- clim_site_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'SYLVANIA') |>
  dplyr::select(residual_AGBI, PPT2_01:Vpdmax_growing)

p3 <- sylv2 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = residual_AGBI)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = residual_AGBI), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p3

cor_sylv2 <- cor(x = sylv2[,1],
                 y = sylv2[,2:69])

cor_sylv2 <- t(cor_sylv2)
cor_sylv2 <- as.data.frame(cor_sylv2)

p4 <- cor_sylv2 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = residual_AGBI),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p4

p1 # versus
p3

cowplot::plot_grid(p2, p4)

# Harvard RW only
hf1 <- clim_site_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'HARVARD Model RW') |>
  dplyr::select(site_agbi, PPT2_01:Vpdmax_growing)

p1 <- hf1 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = site_agbi)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = site_agbi), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p1

cor_hf1 <- cor(x = hf1[,1],
               y = hf1[,2:69])

cor_hf1 <- t(cor_hf1)
cor_hf1 <- as.data.frame(cor_hf1)

p2 <- cor_hf1 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = site_agbi),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p2

hf2 <- clim_site_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'HARVARD Model RW') |>
  dplyr::select(residual_AGBI, PPT2_01:Vpdmax_growing)

p3 <- hf2 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = residual_AGBI)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = residual_AGBI), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p3

cor_hf2 <- cor(x = hf2[,1],
               y = hf2[,2:69])

cor_hf2 <- t(cor_hf2)
cor_hf2 <- as.data.frame(cor_hf2)

p4 <- cor_hf2 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = residual_AGBI),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p4

p1 # versus
p3

cowplot::plot_grid(p2, p4)

# Harvard RW + Census
hf3 <- clim_site_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'HARVARD Model RW + Census') |>
  dplyr::select(site_agbi, PPT2_01:Vpdmax_growing)

p1 <- hf3 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = site_agbi)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = site_agbi), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p1

cor_hf3 <- cor(x = hf3[,1],
               y = hf3[,2:69])

cor_hf3 <- t(cor_hf3)
cor_hf3 <- as.data.frame(cor_hf3)

p2 <- cor_hf3 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = site_agbi),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p2

hf4 <- clim_site_agbi |>
  dplyr::ungroup() |>
  dplyr::filter(site == 'HARVARD Model RW + Census') |>
  dplyr::select(residual_AGBI, PPT2_01:Vpdmax_growing)

p3 <- hf4 |>
  tidyr::pivot_longer(cols = PPT2_01:Vpdmax_growing,
                      names_to = 'var',
                      values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = val, y = residual_AGBI)) +
  ggplot2::geom_smooth(ggplot2::aes(x = val, y = residual_AGBI), method = 'lm') +
  ggplot2::facet_wrap(~var, scales = 'free')
p3

cor_hf4 <- cor(x = hf4[,1],
               y = hf4[,2:69])

cor_hf4 <- t(cor_hf4)
cor_hf4 <- as.data.frame(cor_hf4)

p4 <- cor_hf4 |>
  tibble::rownames_to_column(var = 'var') |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = var, y = residual_AGBI),
                    stat = 'identity') +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
p4

p1 # versus
p3

cowplot::plot_grid(p2, p4)
