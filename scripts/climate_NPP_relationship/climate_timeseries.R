## Climate variable time series

rm(list = ls())

# Load cliamte data
load('climate/prism_clim.RData')

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
  dplyr::rename(year = growing_year,
                site = loc)

prism_ts <- prism_long |>
  dplyr::mutate(time = paste0(month, '-', year),
                time = zoo::as.yearmon(time, '%m-%Y'))

#### Goose Egg ####

prism_ts |>
  dplyr::filter(loc == 'GOOSE') |>
  tidyr::pivot_longer(c(PPT2, Tmean2:Vpdmax2),
                      names_to = 'variable', values_to = 'val') |> 
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = val)) +
  ggplot2::facet_wrap(~variable, scales = 'free') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::ggtitle('Goose Egg') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

prism_ts |>
  dplyr::filter(loc == 'GOOSE') |>
  dplyr::group_by(month) |>
  dplyr::summarize(PPT2 = mean(PPT2),
                   Tmean2 = mean(Tmean2),
                   Tmin2 = mean(Tmin2),
                   Tmax2 = mean(Tmax2),
                   Vpdmin2 = mean(Vpdmin2),
                   Vpdmax2 = mean(Vpdmax2)) |>
  tidyr::pivot_longer(PPT2:Vpdmax2,
                      names_to = 'variable', values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = as.numeric(month), y = val)) +
  ggplot2::facet_wrap(~variable, scales = 'free') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::ggtitle('Goose Egg') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

prism_growing |>
  dplyr::filter(site == 'GOOSE') |>
  tidyr::pivot_longer(PPT_growing:Vpdmax_growing,
                      names_to = 'variable', values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = as.numeric(year), y = val)) +
  ggplot2::facet_wrap(~variable, scales = 'free') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::ggtitle('Goose Egg') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

#### North Round Pond ####

prism_ts |>
  dplyr::filter(loc == 'NRP') |>
  tidyr::pivot_longer(c(PPT2, Tmean2:Vpdmax2),
                      names_to = 'variable', values_to = 'val') |> 
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = val)) +
  ggplot2::facet_wrap(~variable, scales = 'free') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::ggtitle('North Round Pond') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

prism_ts |>
  dplyr::filter(loc == 'NRP') |>
  dplyr::group_by(month) |>
  dplyr::summarize(PPT2 = mean(PPT2),
                   Tmean2 = mean(Tmean2),
                   Tmin2 = mean(Tmin2),
                   Tmax2 = mean(Tmax2),
                   Vpdmin2 = mean(Vpdmin2),
                   Vpdmax2 = mean(Vpdmax2)) |>
  tidyr::pivot_longer(PPT2:Vpdmax2,
                      names_to = 'variable', values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = as.numeric(month), y = val)) +
  ggplot2::facet_wrap(~variable, scales = 'free') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::ggtitle('North Round Pond') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

prism_growing |>
  dplyr::filter(site == 'NRP') |>
  tidyr::pivot_longer(PPT_growing:Vpdmax_growing,
                      names_to = 'variable', values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = as.numeric(year), y = val)) +
  ggplot2::facet_wrap(~variable, scales = 'free') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::ggtitle('North Round Pond') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

#### Rooster Hill ####

prism_ts |>
  dplyr::filter(loc == 'ROOSTER') |>
  tidyr::pivot_longer(c(PPT2, Tmean2:Vpdmax2),
                      names_to = 'variable', values_to = 'val') |> 
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = val)) +
  ggplot2::facet_wrap(~variable, scales = 'free') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::ggtitle('Rooster Hill') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

prism_ts |>
  dplyr::filter(loc == 'ROOSTER') |>
  dplyr::group_by(month) |>
  dplyr::summarize(PPT2 = mean(PPT2),
                   Tmean2 = mean(Tmean2),
                   Tmin2 = mean(Tmin2),
                   Tmax2 = mean(Tmax2),
                   Vpdmin2 = mean(Vpdmin2),
                   Vpdmax2 = mean(Vpdmax2)) |>
  tidyr::pivot_longer(PPT2:Vpdmax2,
                      names_to = 'variable', values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = as.numeric(month), y = val)) +
  ggplot2::facet_wrap(~variable, scales = 'free') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::ggtitle('Rooster Hill') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

prism_growing |>
  dplyr::filter(site == 'ROOSTER') |>
  tidyr::pivot_longer(PPT_growing:Vpdmax_growing,
                      names_to = 'variable', values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = as.numeric(year), y = val)) +
  ggplot2::facet_wrap(~variable, scales = 'free') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::ggtitle('Rooster Hill') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

#### Sylvania ####

prism_ts |>
  dplyr::filter(loc == 'SYLVANIA') |>
  tidyr::pivot_longer(c(PPT2, Tmean2:Vpdmax2),
                      names_to = 'variable', values_to = 'val') |> 
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = val)) +
  ggplot2::facet_wrap(~variable, scales = 'free') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::ggtitle('Sylvania') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

prism_ts |>
  dplyr::filter(loc == 'SYLVANIA') |>
  dplyr::group_by(month) |>
  dplyr::summarize(PPT2 = mean(PPT2),
                   Tmean2 = mean(Tmean2),
                   Tmin2 = mean(Tmin2),
                   Tmax2 = mean(Tmax2),
                   Vpdmin2 = mean(Vpdmin2),
                   Vpdmax2 = mean(Vpdmax2)) |>
  tidyr::pivot_longer(PPT2:Vpdmax2,
                      names_to = 'variable', values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = as.numeric(month), y = val)) +
  ggplot2::facet_wrap(~variable, scales = 'free') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::ggtitle('Sylvania') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

prism_growing |>
  dplyr::filter(site == 'SYLVANIA') |>
  tidyr::pivot_longer(PPT_growing:Vpdmax_growing,
                      names_to = 'variable', values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = as.numeric(year), y = val)) +
  ggplot2::facet_wrap(~variable, scales = 'free') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::ggtitle('Sylvania') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

#### Harvard Forest ####

prism_ts |>
  dplyr::filter(loc == 'HARVARD') |>
  tidyr::pivot_longer(c(PPT2, Tmean2:Vpdmax2),
                      names_to = 'variable', values_to = 'val') |> 
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = time, y = val)) +
  ggplot2::facet_wrap(~variable, scales = 'free') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::ggtitle('Harvard Forest') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

prism_ts |>
  dplyr::filter(loc == 'HARVARD') |>
  dplyr::group_by(month) |>
  dplyr::summarize(PPT2 = mean(PPT2),
                   Tmean2 = mean(Tmean2),
                   Tmin2 = mean(Tmin2),
                   Tmax2 = mean(Tmax2),
                   Vpdmin2 = mean(Vpdmin2),
                   Vpdmax2 = mean(Vpdmax2)) |>
  tidyr::pivot_longer(PPT2:Vpdmax2,
                      names_to = 'variable', values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = as.numeric(month), y = val)) +
  ggplot2::facet_wrap(~variable, scales = 'free') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::ggtitle('Harvard Forest') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))

prism_growing |>
  dplyr::filter(site == 'HARVARD') |>
  tidyr::pivot_longer(PPT_growing:Vpdmax_growing,
                      names_to = 'variable', values_to = 'val') |>
  ggplot2::ggplot() +
  ggplot2::geom_line(ggplot2::aes(x = as.numeric(year), y = val)) +
  ggplot2::facet_wrap(~variable, scales = 'free') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::ggtitle('Harvard Forest') +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, hjust = 0.5, face = 'bold'))
