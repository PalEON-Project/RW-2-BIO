## Selecting climate variables using random forest analysis

rm(list = ls())

# Load tree data
load('out/tree_detrended_AGBI.RData')

rm(save_comb_oos)

# Load climate data
load('climate/prism_clim.RData')

# Pivot wider
prism_monthly <- prism_long |>
  dplyr::group_by(loc) |>
  tidyr::pivot_wider(names_from = 'month',
                     values_from = c('PPT2', 'Tmean2', 'Tmin2', 'Tmax2',
                                     'Vpdmin2', 'Vpdmax2')) |>
  dplyr::mutate(year = as.numeric(year)) |>
  dplyr::rename(site = loc)

# Join with tree data
agbi_monthly <- save_comb |>
  dplyr::left_join(y = prism_monthly,
                   by = c('year', 'site'))

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

# Join with rest of data
agbi_monthly_growing <- agbi_monthly |>
  dplyr::left_join(y = prism_growing,
                   by = c('year', 'site')) |>
  dplyr::ungroup()

test <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                   data = dplyr::select(agbi_monthly_growing, 
                                                        -tree, -year, -plot, -taxon, -site, -mean),
                                   ntree = 1000,
                                   importance = TRUE)

