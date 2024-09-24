## Selecting climate variables using correlation analysis

rm(list = ls())

# Load tree data
# Choose trended or detrended
#load('out/tree_trended_AGBI.RData')
load('out/tree_detrended_AGBI.RData')

# Rename trended dataframe
if(exists('tree_agbi')) save_comb <- tree_agbi
# Rename response variable
if(exists('tree_agbi')) save_comb <- dplyr::mutate(save_comb, residual_AGBI = mean)

# Load cliamte data
load('climate/prism_clim.RData')

# Duplicate Harvard climate
prism_harv <- dplyr::filter(prism_long, loc == 'HARVARD')
prism_long <- dplyr::mutate(prism_long, loc = dplyr::if_else(loc == 'HARVARD', 'HARVARD Model RW', loc))
prism_long <- rbind(prism_long, prism_harv)
prism_long <- dplyr::mutate(prism_long, loc = dplyr::if_else(loc == 'HARVARD', 'HARVARD Model RW + Census', loc))

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

#### Correlation with all sites and taxa pooled ####

# Correlation between AGBI and all climate variables
all_sites_taxa <- cor(x = agbi_monthly_growing$residual_AGBI,
                      y = dplyr::select(agbi_monthly_growing,
                                        -tree, -year, -plot,
                                        -taxon, -site, -mean,
                                        -residual_AGBI))

# Format
corr_all_sites_taxa <- as.data.frame(t(all_sites_taxa))
corr_all_sites_taxa |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

corr_all_sites_taxa |>
  tibble::rownames_to_column(var = 'variable_time') |>
  dplyr::rename(correlation = V1)|>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_time),
                time = sub(pattern = '.*_', replacement = '', x = variable_time)) |>
  dplyr::filter(variable %in% c('PPT2', 'PPT')) |>
  dplyr::mutate(correlation = dplyr::if_else(abs(correlation) < 0.1, NA, correlation)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable_time, y = 'All sites', fill = correlation)) +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90))

corr_all_sites_taxa |>
  tibble::rownames_to_column(var = 'variable_time') |>
  dplyr::rename(correlation = V1)|>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_time),
                time = sub(pattern = '.*_', replacement = '', x = variable_time)) |>
  dplyr::filter(variable %in% c('Tmean2', 'Tmean')) |>
  dplyr::mutate(correlation = dplyr::if_else(abs(correlation) < 0.1, NA, correlation)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable_time, y = 'All sites', fill = correlation)) +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90))

corr_all_sites_taxa |>
  tibble::rownames_to_column(var = 'variable_time') |>
  dplyr::rename(correlation = V1)|>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_time),
                time = sub(pattern = '.*_', replacement = '', x = variable_time)) |>
  dplyr::filter(variable %in% c('Tmin2', 'Tmin')) |>
  dplyr::mutate(correlation = dplyr::if_else(abs(correlation) < 0.1, NA, correlation)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable_time, y = 'All sites', fill = correlation)) +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90))

corr_all_sites_taxa |>
  tibble::rownames_to_column(var = 'variable_time') |>
  dplyr::rename(correlation = V1)|>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_time),
                time = sub(pattern = '.*_', replacement = '', x = variable_time)) |>
  dplyr::filter(variable %in% c('Tmax2', 'Tmax')) |>
  dplyr::mutate(correlation = dplyr::if_else(abs(correlation) < 0.1, NA, correlation)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable_time, y = 'All sites', fill = correlation)) +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90))

corr_all_sites_taxa |>
  tibble::rownames_to_column(var = 'variable_time') |>
  dplyr::rename(correlation = V1)|>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_time),
                time = sub(pattern = '.*_', replacement = '', x = variable_time)) |>
  dplyr::filter(variable %in% c('Vpdmin2', 'Vpdmin')) |>
  dplyr::mutate(correlation = dplyr::if_else(abs(correlation) < 0.1, NA, correlation)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable_time, y = 'All sites', fill = correlation)) +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90))

corr_all_sites_taxa |>
  tibble::rownames_to_column(var = 'variable_time') |>
  dplyr::rename(correlation = V1)|>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_time),
                time = sub(pattern = '.*_', replacement = '', x = variable_time)) |>
  dplyr::filter(variable %in% c('Vpdmax2', 'Vpdmax')) |>
  dplyr::mutate(correlation = dplyr::if_else(abs(correlation) < 0.1, NA, correlation)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable_time, y = 'All sites', fill = correlation)) +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90))

#### Correlations for each site individually across all individual trees ####

## GOOSE
goose_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'GOOSE') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

goose_cor <- cor(x = goose_agbi_monthly_growing$residual_AGBI,
                 y = dplyr::select(goose_agbi_monthly_growing, -residual_AGBI))

cor_goose <- as.data.frame(t(goose_cor))
cor_goose |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_goose2 <- cor_goose |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                site = 'Goose Egg')
  
## NRP

nrp_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'NRP') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

nrp_cor <- cor(x = nrp_agbi_monthly_growing$residual_AGBI,
               y = dplyr::select(nrp_agbi_monthly_growing, -residual_AGBI))

cor_nrp <- as.data.frame(t(nrp_cor))
cor_nrp |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_nrp2 <- cor_nrp |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                site = 'North Round Pond')

## ROOSTER

rooster_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'ROOSTER') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

rooster_cor <- cor(x = rooster_agbi_monthly_growing$residual_AGBI,
                   y = dplyr::select(rooster_agbi_monthly_growing, -residual_AGBI))

cor_rooster <- as.data.frame(t(rooster_cor))
cor_rooster |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_rooster2 <- cor_rooster |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                site = 'Rooster Hill')

## SYLVANIA

sylvania_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'SYLVANIA') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

sylvania_cor <- cor(x = sylvania_agbi_monthly_growing$residual_AGBI,
                    y = dplyr::select(sylvania_agbi_monthly_growing, -residual_AGBI))

cor_sylvania <- as.data.frame(t(sylvania_cor))
cor_sylvania |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_sylvania2 <- cor_sylvania |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                site = 'Sylvania Wilderness')

## HARVARD Model RW

harvard_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'HARVARD Model RW') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

harvard_cor <- cor(x = harvard_agbi_monthly_growing$residual_AGBI,
                   y = dplyr::select(harvard_agbi_monthly_growing, -residual_AGBI))

cor_harvard <- as.data.frame(t(harvard_cor))
cor_harvard |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_harvard_rw <- cor_harvard |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                site = 'Harvard Forest\nRW only')

## HARVARD Model RW + Census

harvard_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'HARVARD Model RW + Census') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

harvard_cor <- cor(x = harvard_agbi_monthly_growing$residual_AGBI,
                   y = dplyr::select(harvard_agbi_monthly_growing, -residual_AGBI))

cor_harvard <- as.data.frame(t(harvard_cor))
cor_harvard |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_harvard_rw_census <- cor_harvard |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                site = 'Harvard Forest\nRW + Census')

# Combine
corr_by_site <- rbind(cor_goose2, cor_nrp2, cor_rooster2, cor_sylvania2,
                      cor_harvard_rw, cor_harvard_rw_census)

# Plot correlations by site
corr_by_site |>
  dplyr::filter(variable %in% c('PPT2', 'PPT')) |>
  dplyr::mutate(correlation = dplyr::if_else(abs(correlation) < 0.1, NA, correlation)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable_month, y = site, fill = correlation)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

corr_by_site |>
  dplyr::filter(variable %in% c('Tmean2', 'Tmean')) |>
  dplyr::mutate(correlation = dplyr::if_else(abs(correlation) < 0.1, NA, correlation)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable_month, y = site, fill = correlation)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

corr_by_site |>
  dplyr::filter(variable %in% c('Tmin2', 'Tmin')) |>
  dplyr::mutate(correlation = dplyr::if_else(abs(correlation) < 0.1, NA, correlation)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable_month, y = site, fill = correlation)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

corr_by_site |>
  dplyr::filter(variable %in% c('Tmax2', 'Tmax')) |>
  dplyr::mutate(correlation = dplyr::if_else(abs(correlation) < 0.1, NA, correlation)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable_month, y = site, fill = correlation)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

corr_by_site |>
  dplyr::filter(variable %in% c('Vpdmin2', 'Vpdmin')) |>
  dplyr::mutate(correlation = dplyr::if_else(abs(correlation) < 0.1, NA, correlation)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable_month, y = site, fill = correlation)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

corr_by_site |>
  dplyr::filter(variable %in% c('Vpdmax2', 'Vpdmax')) |>
  dplyr::mutate(correlation = dplyr::if_else(abs(correlation) < 0.1, NA, correlation)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable_month, y = site, fill = correlation)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

#### Correlations for each taxon individually across all individual sites and trees ####

unique(agbi_monthly_growing$taxon)

## ACRU (n = 9526 obs, 4 sites, 296 trees)

acru_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'ACRU') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

acru_cor <- cor(x = acru_agbi_monthly_growing$residual_AGBI,
                y = dplyr::select(acru_agbi_monthly_growing, -residual_AGBI))

cor_acru <- as.data.frame(t(acru_cor))
cor_acru |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_acru2 <- cor_acru |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                taxon = 'Red Maple')

## ACSA (n = 1295 obs, 3 sites, 32 trees)

acsa_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'ACSA') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

acsa_cor <- cor(x = acsa_agbi_monthly_growing$residual_AGBI,
                y = dplyr::select(acsa_agbi_monthly_growing, -residual_AGBI))

cor_acsa <- as.data.frame(t(acsa_cor))
cor_acsa |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_acsa2 <- cor_acsa |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                taxon = 'Sugar Maple')

## QUAL (n = 465 obs, 1 site, 17 trees)

qual_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'QUAL') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

qual_cor <- cor(x = qual_agbi_monthly_growing$residual_AGBI,
                y = dplyr::select(qual_agbi_monthly_growing, -residual_AGBI))

cor_qual <- as.data.frame(t(qual_cor))
cor_qual |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_qual2 <- cor_qual |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                taxon = 'White Oak')

## BEAL (n = 1786 obs, 3 sites, 57 trees)

beal_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'BEAL') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

beal_cor <- cor(x = beal_agbi_monthly_growing$residual_AGBI,
                y = dplyr::select(beal_agbi_monthly_growing, -residual_AGBI))

cor_beal <- as.data.frame(t(beal_cor))
cor_beal |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_beal2 <- cor_beal |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                taxon = 'Yellow Birch')

## PCRU (n = 1500 obs, 1 site, 39 trees)

pcru_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'PCRU') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

pcru_cor <- cor(x = pcru_agbi_monthly_growing$residual_AGBI,
                y = dplyr::select(pcru_agbi_monthly_growing, -residual_AGBI))

cor_pcru <- as.data.frame(t(pcru_cor))
cor_pcru |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_pcru2 <- cor_pcru |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                taxon = 'Red Spruce')

## PIST (n = 4262 obs, 4 sites, 120 trees)

pist_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'PIST') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

pist_cor <- cor(x = pist_agbi_monthly_growing$residual_AGBI,
                y = dplyr::select(pist_agbi_monthly_growing, -residual_AGBI))

cor_pist <- as.data.frame(t(pist_cor))
cor_pist |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_pist2 <- cor_pist |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                taxon = 'White Pine')

## FAGR (n = 3881 obs, 4 sites, 107 trees)

fagr_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'FAGR') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

fagr_cor <- cor(x = fagr_agbi_monthly_growing$residual_AGBI,
                y = dplyr::select(fagr_agbi_monthly_growing, -residual_AGBI))

cor_fagr <- as.data.frame(t(fagr_cor))
cor_fagr |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_fagr2 <- cor_fagr |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                taxon = 'American Beech')

## AMAR (n = 41 obs, 1 site, 1 tree)

amar_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'AMAR') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

amar_cor <- cor(x = amar_agbi_monthly_growing$residual_AGBI,
                y = dplyr::select(amar_agbi_monthly_growing, -residual_AGBI))

cor_amar <- as.data.frame(t(amar_cor))
cor_amar |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_amar2 <- cor_amar |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                taxon = 'Common Serviceberry')

## QUMO (n = 1151 obs, 1 site, 30 trees)

qumo_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'QUMO') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

qumo_cor <- cor(x = qumo_agbi_monthly_growing$residual_AGBI,
                y = dplyr::select(qumo_agbi_monthly_growing, -residual_AGBI))

cor_qumo <- as.data.frame(t(qumo_cor))
cor_qumo |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_qumo2 <- cor_qumo |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                taxon = 'Chestnut Oak')

## FRAM (n = 1270 obs, 1 site, 31 trees)

fram_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'FRAM') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

fram_cor <- cor(x = fram_agbi_monthly_growing$residual_AGBI,
                y = dplyr::select(fram_agbi_monthly_growing, -residual_AGBI))

cor_fram <- as.data.frame(t(fram_cor))
cor_fram |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_fram2 <- cor_fram |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                taxon = 'White Ash')

## QURU (n = 10467 obs, 4 sites, 215 trees)

quru_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'QURU') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

quru_cor <- cor(x = quru_agbi_monthly_growing$residual_AGBI,
                y = dplyr::select(quru_agbi_monthly_growing, -residual_AGBI))

cor_quru <- as.data.frame(t(quru_cor))
cor_quru |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_quru2 <- cor_quru |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                taxon = 'Red Oak')

## THOC (n = 440 obs, 1 site, 11 trees)

thoc_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'THOC') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

thoc_cor <- cor(x = thoc_agbi_monthly_growing$residual_AGBI,
                y = dplyr::select(thoc_agbi_monthly_growing, -residual_AGBI))

cor_thoc <- as.data.frame(t(thoc_cor))
cor_thoc |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_thoc2 <- cor_thoc |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                taxon = 'Arborvitae')

## OSVI (n = 82 obs, 1 site, 2 trees)

osvi_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'OSVI') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

osvi_cor <- cor(x = osvi_agbi_monthly_growing$residual_AGBI,
                y = dplyr::select(osvi_agbi_monthly_growing, -residual_AGBI))

cor_osvi <- as.data.frame(t(osvi_cor))
cor_osvi |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_osvi2 <- cor_osvi |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                taxon = 'American\nHophornbeam')

## TSCA (n = 7219 obs, 3 sites, 177 trees)

tsca_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'TSCA') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

tsca_cor <- cor(x = tsca_agbi_monthly_growing$residual_AGBI,
                y = dplyr::select(tsca_agbi_monthly_growing, -residual_AGBI))

cor_tsca <- as.data.frame(t(tsca_cor))
cor_tsca |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_tsca2 <- cor_tsca |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                taxon = 'Eastern Hemlock')

## PRSE (n = 84 obs, 2 sites, 2 trees)

prse_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'PRSE') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

prse_cor <- cor(x = prse_agbi_monthly_growing$residual_AGBI,
                y = dplyr::select(prse_agbi_monthly_growing, -residual_AGBI))

cor_prse <- as.data.frame(t(prse_cor))
cor_prse |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_prse2 <- cor_prse |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                taxon = 'Black Cherry')

## BEPA (n = 161 obs, 3 sites, 4 trees)

bepa_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'BEPA') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

bepa_cor <- cor(x = bepa_agbi_monthly_growing$residual_AGBI,
                y = dplyr::select(bepa_agbi_monthly_growing, -residual_AGBI))

cor_bepa <- as.data.frame(t(bepa_cor))
cor_bepa |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_bepa2 <- cor_bepa |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                taxon = 'Paper Birch')

## BELE (n = 1026 obs, 3 sites, 33 trees)

bele_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'BELE') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

bele_cor <- cor(x = bele_agbi_monthly_growing$residual_AGBI,
                y = dplyr::select(bele_agbi_monthly_growing, -residual_AGBI))

cor_bele <- as.data.frame(t(bele_cor))
cor_bele |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_bele2 <- cor_bele |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                taxon = 'Black Birch')

## QUVE (n = 239 obs, 1 site, 8 trees)

quve_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'QUVE') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

quve_cor <- cor(x = quve_agbi_monthly_growing$residual_AGBI,
                y = dplyr::select(quve_agbi_monthly_growing, -residual_AGBI))

cor_quve <- as.data.frame(t(quve_cor))
cor_quve |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_quve2 <- cor_quve |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                taxon = 'Black Oak')

## HAVI (n = 422 obs, 1 site, 37 trees)

havi_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'HAVI') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

havi_cor <- cor(x = havi_agbi_monthly_growing$residual_AGBI,
                y = dplyr::select(havi_agbi_monthly_growing, -residual_AGBI))

cor_havi <- as.data.frame(t(havi_cor))
cor_havi |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::rename(cor = V1) |>
  dplyr::arrange(desc(abs(cor))) |>
  dplyr::slice_head(n = 10)

cor_havi2 <- cor_havi |>
  tibble::rownames_to_column(var = 'variable_month') |>
  dplyr::rename(correlation = V1) |>
  dplyr::mutate(variable = sub(pattern = '_.*', replacement = '', x = variable_month),
                month = sub(pattern = '.*_', replacement = '', x = variable_month),
                taxon = 'Witch Hazel')

# Combine
corr_by_taxon <- rbind(cor_acru2, cor_acsa2, cor_qual2, cor_tsca2,
                       cor_beal2, cor_pcru2, cor_pist2, cor_quru2,
                       cor_fagr2, cor_amar2, cor_qumo2, cor_fram2,
                       cor_thoc2, cor_osvi2, cor_bele2, cor_prse2,
                       cor_bepa2, cor_quve2, cor_havi2)

# Plot correlations by taxon
corr_by_taxon |>
  dplyr::filter(variable %in% c('PPT2', 'PPT')) |>
  dplyr::mutate(correlation = dplyr::if_else(abs(correlation) < 0.1, NA, correlation)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable_month, y = taxon, fill = correlation)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

corr_by_taxon |>
  dplyr::filter(variable %in% c('Tmean2', 'Tmean')) |>
  dplyr::mutate(correlation = dplyr::if_else(abs(correlation) < 0.1, NA, correlation)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable_month, y = taxon, fill = correlation)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

corr_by_taxon |>
  dplyr::filter(variable %in% c('Tmin2', 'Tmin')) |>
  dplyr::mutate(correlation = dplyr::if_else(abs(correlation) < 0.1, NA, correlation)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable_month, y = taxon, fill = correlation)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

corr_by_taxon |>
  dplyr::filter(variable %in% c('Tmax2', 'Tmax')) |>
  dplyr::mutate(correlation = dplyr::if_else(abs(correlation) < 0.1, NA, correlation)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable_month, y = taxon, fill = correlation)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

corr_by_taxon |>
  dplyr::filter(variable %in% c('Vpdmin2', 'Vpdmin')) |>
  dplyr::mutate(correlation = dplyr::if_else(abs(correlation) < 0.1, NA, correlation)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable_month, y = taxon, fill = correlation)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

corr_by_taxon |>
  dplyr::filter(variable %in% c('Vpdmax2', 'Vpdmax')) |>
  dplyr::mutate(correlation = dplyr::if_else(abs(correlation) < 0.1, NA, correlation)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable_month, y = taxon, fill = correlation)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

#### Correlations for each individual tree ####

# Add index for individual trees
agbi_monthly_growing <- dplyr::mutate(agbi_monthly_growing,
                                      ind = paste0(site,'_',plot,'_',tree))

# Individual trees
inds <- unique(agbi_monthly_growing$ind)

# Loop over individual trees
for(i in inds){
  temp <- dplyr::filter(agbi_monthly_growing, ind == i)
  
  tree <- unique(temp$tree)
  plot <- unique(temp$plot)
  taxon <- unique(temp$taxon)
  site <- unique(temp$site)
  
  temp <- dplyr::select(temp, -tree, -year, -plot,
                        -taxon, -site, -mean, -ind)
  
  temp_cor <- cor(x = temp$residual_AGBI,
                  y = dplyr::select(temp, -residual_AGBI))
  
  temp_cor <- as.data.frame(temp_cor)
  temp_cor$tree <- tree
  temp_cor$plot <- plot
  temp_cor$taxon <- taxon
  temp_cor$site <- site
  
  if(i == inds[1]){
    ind_cors <- temp_cor
  }else{
    ind_cors <- rbind(ind_cors, temp_cor)
  }
  print(i)
}

### Distribution of correlations across all taxa and sites ###

## Precipitation

quants <- ind_cors |>
  tidyr::pivot_longer(cols = c(PPT2_01:PPT2_12, PPT_growing, sd_PPT_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable) |>
  dplyr::summarize(low = quantile(cor, probs = 0.025, na.rm = TRUE),
                   high = quantile(cor, probs = 0.975, na.rm = TRUE))

ind_cors |>
  tidyr::pivot_longer(cols = c(PPT2_01:PPT2_12, PPT_growing, sd_PPT_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::left_join(y = quants, by = 'variable') |>
  dplyr::mutate(cor = dplyr::if_else(cor > low & cor < high, cor, NA)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = variable, y = cor), 
                       fill = NA, linewidth = 1) +
  ggplot2::geom_jitter(ggplot2::aes(x = variable, y = cor),
                       width = 0.2, alpha = 0.1, color = 'black') +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed', linewidth = 1, color = 'maroon') +
  ggplot2::xlab('') + ggplot2::ylab('Correlation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

## Mean temperature

quants <- ind_cors |>
  tidyr::pivot_longer(cols = c(Tmean2_01:Tmean2_12, Tmean_growing, sd_Tmean_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable) |>
  dplyr::summarize(low = quantile(cor, probs = 0.025, na.rm = TRUE),
                   high = quantile(cor, probs = 0.975, na.rm = TRUE))

ind_cors |>
  tidyr::pivot_longer(cols = c(Tmean2_01:Tmean2_12, Tmean_growing, sd_Tmean_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::left_join(y = quants, by = 'variable') |>
  dplyr::mutate(cor = dplyr::if_else(cor > low & cor < high, cor, NA)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = variable, y = cor),
                       fill = NA, linewidth = 1) +
  ggplot2::geom_jitter(ggplot2::aes(x = variable, y = cor),
                       width = 0.2, alpha = 0.1, color = 'black') +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed', linewidth = 1, color = 'maroon') +
  ggplot2::xlab('') + ggplot2::ylab('Correlation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

## Minimum temperature

quants <- ind_cors |>
  tidyr::pivot_longer(cols = c(Tmin2_01:Tmin2_12, Tmin_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable) |>
  dplyr::summarize(low = quantile(cor, probs = 0.025, na.rm = TRUE),
                   high = quantile(cor, probs = 0.975, na.rm = TRUE))

ind_cors |>
  tidyr::pivot_longer(cols = c(Tmin2_01:Tmin2_12, Tmin_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::left_join(y = quants, by = 'variable') |>
  dplyr::mutate(cor = dplyr::if_else(cor > low & cor < high, cor, NA)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = variable, y = cor),
                       fill = NA, linewidth = 1) +
  ggplot2::geom_jitter(ggplot2::aes(x = variable, y = cor),
                       width = 0.2, alpha = 0.1, color = 'black') +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed', linewidth = 1, color = 'maroon') +
  ggplot2::xlab('') + ggplot2::ylab('Correlation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

## Maximum temperature

quants <- ind_cors |>
  tidyr::pivot_longer(cols = c(Tmax2_01:Tmax2_12, Tmax_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable) |>
  dplyr::summarize(low = quantile(cor, probs = 0.025, na.rm = TRUE),
                   high = quantile(cor, probs = 0.975, na.rm = TRUE))

ind_cors |>
  tidyr::pivot_longer(cols = c(Tmax2_01:Tmax2_12, Tmax_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::left_join(y = quants, by = 'variable') |>
  dplyr::mutate(cor = dplyr::if_else(cor > low & cor < high, cor, NA)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = variable, y = cor),
                       fill = NA, linewidth = 1) +
  ggplot2::geom_jitter(ggplot2::aes(x = variable, y = cor),
                       width = 0.2, alpha = 0.1, color = 'black') +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed', linewidth = 1, color = 'maroon') +
  ggplot2::xlab('') + ggplot2::ylab('Correlation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

## Minimum VPD

quants <- ind_cors |>
  tidyr::pivot_longer(cols = c(Vpdmin2_01:Vpdmin2_12, Vpdmin_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable) |>
  dplyr::summarize(low = quantile(cor, probs = 0.025, na.rm = TRUE),
                   high = quantile(cor, probs = 0.975, na.rm = TRUE))

ind_cors |>
  tidyr::pivot_longer(cols = c(Vpdmin2_01:Vpdmin2_12, Vpdmin_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::left_join(y = quants, by = 'variable') |>
  dplyr::mutate(cor = dplyr::if_else(cor > low & cor < high, cor, NA)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = variable, y = cor),
                       fill = NA, linewidth = 1) +
  ggplot2::geom_jitter(ggplot2::aes(x = variable, y = cor),
                       width = 0.2, alpha = 0.1, color = 'black') +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed', linewidth = 1, color = 'maroon') +
  ggplot2::xlab('') + ggplot2::ylab('Correlation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

## Maximum VPD

quants <- ind_cors |>
  tidyr::pivot_longer(cols = c(Vpdmax2_01:Vpdmax2_12, Vpdmax_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable) |>
  dplyr::summarize(low = quantile(cor, probs = 0.025, na.rm = TRUE),
                   high = quantile(cor, probs = 0.975, na.rm = TRUE))

ind_cors |>
  tidyr::pivot_longer(cols = c(Vpdmax2_01:Vpdmax2_12, Vpdmax_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::left_join(y = quants, by = 'variable') |>
  dplyr::mutate(cor = dplyr::if_else(cor > low & cor < high, cor, NA)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = variable, y = cor),
                       fill = NA, linewidth = 1) +
  ggplot2::geom_jitter(ggplot2::aes(x = variable, y = cor),
                       width = 0.2, alpha = 0.1, color = 'black') +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed', linewidth = 1, color = 'maroon') +
  ggplot2::xlab('') + ggplot2::ylab('Correlation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

# Correlation matrices across all individuals
ind_cors |>
  tidyr::pivot_longer(cols = c(PPT2_01:PPT2_12, PPT_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable) |>
  dplyr::summarize(mean = mean(cor, na.rm = TRUE)) |>
  dplyr::mutate(mean = dplyr::if_else(abs(mean) < 0.1, NA, mean)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable, y = 'All sites', fill = mean)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90))

ind_cors |>
  tidyr::pivot_longer(cols = c(Tmean2_01:Tmean2_12, Tmean_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable) |>
  dplyr::summarize(mean = mean(cor, na.rm = TRUE)) |>
  dplyr::mutate(mean = dplyr::if_else(abs(mean) < 0.1, NA, mean)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable, y = 'All sites', fill = mean)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90))

ind_cors |>
  tidyr::pivot_longer(cols = c(Tmin2_01:Tmin2_12, Tmin_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable) |>
  dplyr::summarize(mean = mean(cor, na.rm = TRUE)) |>
  dplyr::mutate(mean = dplyr::if_else(abs(mean) < 0.1, NA, mean)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable, y = 'All sites', fill = mean)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90))

ind_cors |>
  tidyr::pivot_longer(cols = c(Tmax2_01:Tmax2_12, Tmax_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable) |>
  dplyr::summarize(mean = mean(cor, na.rm = TRUE)) |>
  dplyr::mutate(mean = dplyr::if_else(abs(mean) < 0.1, NA, mean)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable, y = 'All sites', fill = mean)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90))

ind_cors |>
  tidyr::pivot_longer(cols = c(Vpdmin2_01:Vpdmin2_12, Vpdmin_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable) |>
  dplyr::summarize(mean = mean(cor, na.rm = TRUE)) |>
  dplyr::mutate(mean = dplyr::if_else(abs(mean) < 0.1, NA, mean)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable, y = 'All sites', fill = mean)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90))

ind_cors |>
  tidyr::pivot_longer(cols = c(Vpdmax2_01:Vpdmax2_12, Vpdmax_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable) |>
  dplyr::summarize(mean = mean(cor, na.rm = TRUE)) |>
  dplyr::mutate(mean = dplyr::if_else(abs(mean) < 0.1, NA, mean)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable, y = 'All sites', fill = mean)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90))

### Distribution by site ###

## Precipitation

quants <- ind_cors |>
  tidyr::pivot_longer(cols = c(PPT2_01:PPT2_12, PPT_growing, sd_PPT_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable, site) |>
  dplyr::summarize(low = quantile(cor, probs = 0.025, na.rm = TRUE),
                   high = quantile(cor, probs = 0.975, na.rm = TRUE))

ind_cors |>
  tidyr::pivot_longer(cols = c(PPT2_01:PPT2_12, PPT_growing, sd_PPT_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::left_join(y = quants, by = c('variable', 'site')) |>
  dplyr::mutate(cor = dplyr::if_else(cor > low & cor < high, cor, NA)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = variable, y = cor),
                       fill = NA, linewidth = 1) +
  ggplot2::geom_jitter(ggplot2::aes(x = variable, y = cor, color = taxon),
                       width = 0.2, alpha = 0.1) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed', linewidth = 1, color = 'maroon') +
  ggplot2::facet_wrap(~site) +
  ggplot2::xlab('') + ggplot2::ylab('Correlation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

## Mean temperature

quants <- ind_cors |>
  tidyr::pivot_longer(cols = c(Tmean2_01:Tmean2_12, Tmean_growing, sd_Tmean_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable, site) |>
  dplyr::summarize(low = quantile(cor, probs = 0.025, na.rm = TRUE),
                   high = quantile(cor, probs = 0.975, na.rm = TRUE))

ind_cors |>
  tidyr::pivot_longer(cols = c(Tmean2_01:Tmean2_12, Tmean_growing, sd_Tmean_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::left_join(y = quants, by = c('variable', 'site')) |>
  dplyr::mutate(cor = dplyr::if_else(cor > low & cor < high, cor, NA)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = variable, y = cor),
                       fill = NA, linewidth = 1) +
  ggplot2::geom_jitter(ggplot2::aes(x = variable, y = cor, color = taxon),
                       width = 0.2, alpha = 0.1) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed', linewidth = 1, color = 'maroon') +
  ggplot2::facet_wrap(~site) +
  ggplot2::xlab('') + ggplot2::ylab('Correlation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

## Minimum temperature

quants <- ind_cors |>
  tidyr::pivot_longer(cols = c(Tmin2_01:Tmin2_12, Tmin_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable, site) |>
  dplyr::summarize(low = quantile(cor, probs = 0.025, na.rm = TRUE),
                   high = quantile(cor, probs = 0.975, na.rm = TRUE))

ind_cors |>
  tidyr::pivot_longer(cols = c(Tmin2_01:Tmin2_12, Tmin_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::left_join(y = quants, by = c('variable', 'site')) |>
  dplyr::mutate(cor = dplyr::if_else(cor > low & cor < high, cor, NA)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = variable, y = cor),
                       fill = NA, linewidth = 1) +
  ggplot2::geom_jitter(ggplot2::aes(x = variable, y = cor, color = taxon),
                       width = 0.2, alpha = 0.1) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed', linewidth = 1, color = 'maroon') +
  ggplot2::facet_wrap(~site) +
  ggplot2::xlab('') + ggplot2::ylab('Correlation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

## Maximum temperature

quants <- ind_cors |>
  tidyr::pivot_longer(cols = c(Tmax2_01:Tmax2_12, Tmax_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable, site) |>
  dplyr::summarize(low = quantile(cor, probs = 0.025, na.rm = TRUE),
                   high = quantile(cor, probs = 0.975, na.rm = TRUE))

ind_cors |>
  tidyr::pivot_longer(cols = c(Tmax2_01:Tmax2_12, Tmax_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::left_join(y = quants, by = c('variable', 'site')) |>
  dplyr::mutate(cor = dplyr::if_else(cor > low & cor < high, cor, NA)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = variable, y = cor),
                       fill = NA, linewidth = 1) +
  ggplot2::geom_jitter(ggplot2::aes(x = variable, y = cor, color = taxon),
                       width = 0.2, alpha = 0.1) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed', linewidth = 1, color = 'maroon') +
  ggplot2::facet_wrap(~site) +
  ggplot2::xlab('') + ggplot2::ylab('Correlation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

## Minimum VPD

quants <- ind_cors |>
  tidyr::pivot_longer(cols = c(Vpdmin2_01:Vpdmin2_12, Vpdmin_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable, site) |>
  dplyr::summarize(low = quantile(cor, probs = 0.025, na.rm = TRUE),
                   high = quantile(cor, probs = 0.975, na.rm = TRUE))

ind_cors |>
  tidyr::pivot_longer(cols = c(Vpdmin2_01:Vpdmin2_12, Vpdmin_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::left_join(y = quants, by = c('variable', 'site')) |>
  dplyr::mutate(cor = dplyr::if_else(cor > low & cor < high, cor, NA)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = variable, y = cor),
                       fill = NA, linewidth = 1) +
  ggplot2::geom_jitter(ggplot2::aes(x = variable, y = cor, color = taxon),
                       width = 0.2, alpha = 0.1) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed', linewidth = 1, color = 'maroon') +
  ggplot2::facet_wrap(~site) +
  ggplot2::xlab('') + ggplot2::ylab('Correlation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

## Maximum VPD

quants <- ind_cors |>
  tidyr::pivot_longer(cols = c(Vpdmax2_01:Vpdmax2_12, Vpdmax_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable, site) |>
  dplyr::summarize(low = quantile(cor, probs = 0.025, na.rm = TRUE),
                   high = quantile(cor, probs = 0.975, na.rm = TRUE))

ind_cors |>
  tidyr::pivot_longer(cols = c(Vpdmax2_01:Vpdmax2_12, Vpdmax_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::left_join(y = quants, by = c('variable', 'site')) |>
  dplyr::mutate(cor = dplyr::if_else(cor > low & cor < high, cor, NA)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = variable, y = cor),
                       fill = NA, linewidth = 1) +
  ggplot2::geom_jitter(ggplot2::aes(x = variable, y = cor, color = taxon),
                       width = 0.2, alpha = 0.1) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed', linewidth = 1, color = 'maroon') +
  ggplot2::facet_wrap(~site) +
  ggplot2::xlab('') + ggplot2::ylab('Correlation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

# Mean correlations
ind_cors |>
  tidyr::pivot_longer(cols = c(PPT2_01:PPT2_12, PPT_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable, site) |>
  dplyr::summarize(mean = mean(cor, na.rm = TRUE)) |>
  dplyr::mutate(mean = dplyr::if_else(abs(mean) < 0.1, NA, mean)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable, y = site, fill = mean)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

ind_cors |>
  tidyr::pivot_longer(cols = c(Tmean2_01:Tmean2_12, Tmean_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable, site) |>
  dplyr::summarize(mean = mean(cor, na.rm = TRUE)) |>
  dplyr::mutate(mean = dplyr::if_else(abs(mean) < 0.1, NA, mean)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable, y = site, fill = mean)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

ind_cors |>
  tidyr::pivot_longer(cols = c(Tmin2_01:Tmin2_12, Tmin_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable, site) |>
  dplyr::summarize(mean = mean(cor, na.rm = TRUE)) |>
  dplyr::mutate(mean = dplyr::if_else(abs(mean) < 0.1, NA, mean)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable, y = site, fill = mean)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

ind_cors |>
  tidyr::pivot_longer(cols = c(Tmax2_01:Tmax2_12, Tmax_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable, site) |>
  dplyr::summarize(mean = mean(cor, na.rm = TRUE)) |>
  dplyr::mutate(mean = dplyr::if_else(abs(mean) < 0.1, NA, mean)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable, y = site, fill = mean)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

ind_cors |>
  tidyr::pivot_longer(cols = c(Vpdmin2_01:Vpdmin2_12, Vpdmin_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable, site) |>
  dplyr::summarize(mean = mean(cor, na.rm = TRUE)) |>
  dplyr::mutate(mean = dplyr::if_else(abs(mean) < 0.1, NA, mean)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable, y = site, fill = mean)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

ind_cors |>
  tidyr::pivot_longer(cols = c(Vpdmax2_01:Vpdmax2_12, Vpdmax_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable, site) |>
  dplyr::summarize(mean = mean(cor, na.rm = TRUE)) |>
  dplyr::mutate(mean = dplyr::if_else(abs(mean) < 0.1, NA, mean)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable, y = site, fill = mean)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

### Distribution by taxon ###

## Precipitation

quants <- ind_cors |>
  tidyr::pivot_longer(cols = c(PPT2_01:PPT2_12, PPT_growing, sd_PPT_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable, taxon) |>
  dplyr::summarize(low = quantile(cor, probs = 0.025, na.rm = TRUE),
                   high = quantile(cor, probs = 0.975, na.rm = TRUE))

ind_cors |>
  tidyr::pivot_longer(cols = c(PPT2_01:PPT2_12, PPT_growing, sd_PPT_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::left_join(y = quants, by = c('variable', 'taxon')) |>
  dplyr::mutate(cor = dplyr::if_else(cor > low & cor < high, cor, NA)) |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = variable, y = cor),
                       fill = NA, linewidth = 1) +
  ggplot2::geom_jitter(ggplot2::aes(x = variable, y = cor, color = site),
                       width = 0.2, alpha = 0.1) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed', linewidth = 1, color = 'maroon') +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::xlab('') + ggplot2::ylab('Correlation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

## Mean temperature

quants <- ind_cors |>
  tidyr::pivot_longer(cols = c(Tmean2_01:Tmean2_12, Tmean_growing, sd_Tmean_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable, taxon) |>
  dplyr::summarize(low = quantile(cor, probs = 0.025, na.rm = TRUE),
                   high = quantile(cor, probs = 0.975, na.rm = TRUE))

ind_cors |>
  tidyr::pivot_longer(cols = c(Tmean2_01:Tmean2_12, Tmean_growing, sd_Tmean_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::left_join(y = quants, by = c('variable', 'taxon')) |>
  dplyr::mutate(cor = dplyr::if_else(cor > low & cor < high, cor, NA)) |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = variable, y = cor),
                       fill = NA, linewidth = 1) +
  ggplot2::geom_jitter(ggplot2::aes(x = variable, y = cor, color = site),
                       width = 0.2, alpha = 0.1) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed', linewidth = 1, color = 'maroon') +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::xlab('') + ggplot2::ylab('Correlation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

## Minimum temperature

quants <- ind_cors |>
  tidyr::pivot_longer(cols = c(Tmin2_01:Tmin2_12, Tmin_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable, taxon) |>
  dplyr::summarize(low = quantile(cor, probs = 0.025, na.rm = TRUE),
                   high = quantile(cor, probs = 0.975, na.rm = TRUE))

ind_cors |>
  tidyr::pivot_longer(cols = c(Tmin2_01:Tmin2_12, Tmin_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::left_join(y = quants, by = c('variable', 'taxon')) |>
  dplyr::mutate(cor = dplyr::if_else(cor > low & cor < high, cor, NA)) |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = variable, y = cor),
                       fill = NA, linewidth = 1) +
  ggplot2::geom_jitter(ggplot2::aes(x = variable, y = cor, color = site),
                       width = 0.2, alpha = 0.1) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed', linewidth = 1, color = 'maroon') +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::xlab('') + ggplot2::ylab('Correlation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

## Maximum temperature

quants <- ind_cors |>
  tidyr::pivot_longer(cols = c(Tmax2_01:Tmax2_12, Tmax_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable, taxon) |>
  dplyr::summarize(low = quantile(cor, probs = 0.025, na.rm = TRUE),
                   high = quantile(cor, probs = 0.975, na.rm = TRUE))

ind_cors |>
  tidyr::pivot_longer(cols = c(Tmax2_01:Tmax2_12, Tmax_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::left_join(y = quants, by = c('variable', 'taxon')) |>
  dplyr::mutate(cor = dplyr::if_else(cor > low & cor < high, cor, NA)) |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = variable, y = cor),
                       fill = NA, linewidth = 1) +
  ggplot2::geom_jitter(ggplot2::aes(x = variable, y = cor, color = site),
                       width = 0.2, alpha = 0.1) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed', linewidth = 1, color = 'maroon') +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::xlab('') + ggplot2::ylab('Correlation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

## Minimum VPD

quants <- ind_cors |>
  tidyr::pivot_longer(cols = c(Vpdmin2_01:Vpdmin2_12, Vpdmin_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable, taxon) |>
  dplyr::summarize(low = quantile(cor, probs = 0.025, na.rm = TRUE),
                   high = quantile(cor, probs = 0.975, na.rm = TRUE))

ind_cors |>
  tidyr::pivot_longer(cols = c(Vpdmin2_01:Vpdmin2_12, Vpdmin_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::left_join(y = quants, by = c('variable', 'taxon')) |>
  dplyr::mutate(cor = dplyr::if_else(cor > low & cor < high, cor, NA)) |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = variable, y = cor),
                       fill = NA, linewidth = 1) +
  ggplot2::geom_jitter(ggplot2::aes(x = variable, y = cor, color = site),
                       width = 0.2, alpha = 0.1) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed', linewidth = 1, color = 'maroon') +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::xlab('') + ggplot2::ylab('Correlation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

## Maximum VPD

quants <- ind_cors |>
  tidyr::pivot_longer(cols = c(Vpdmax2_01:Vpdmax2_12, Vpdmax_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable, taxon) |>
  dplyr::summarize(low = quantile(cor, probs = 0.025, na.rm = TRUE),
                   high = quantile(cor, probs = 0.975, na.rm = TRUE))

ind_cors |>
  tidyr::pivot_longer(cols = c(Vpdmax2_01:Vpdmax2_12, Vpdmax_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::left_join(y = quants, by = c('variable', 'taxon')) |>
  dplyr::mutate(cor = dplyr::if_else(cor > low & cor < high, cor, NA)) |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = variable, y = cor),
                       fill = NA, linewidth = 1) +
  ggplot2::geom_jitter(ggplot2::aes(x = variable, y = cor, color = site),
                       width = 0.2, alpha = 0.1) +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0),
                      linetype = 'dashed', linewidth = 1, color = 'maroon') +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::xlab('') + ggplot2::ylab('Correlation') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

# Mean correlations
ind_cors |>
  tidyr::pivot_longer(cols = c(PPT2_01:PPT2_12, PPT_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable, taxon) |>
  dplyr::summarize(mean = mean(cor, na.rm = TRUE)) |>
  dplyr::mutate(mean = dplyr::if_else(abs(mean) < 0.1, NA, mean)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable, y = taxon, fill = mean)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

ind_cors |>
  tidyr::pivot_longer(cols = c(Tmean2_01:Tmean2_12, Tmean_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable, taxon) |>
  dplyr::summarize(mean = mean(cor, na.rm = TRUE)) |>
  dplyr::mutate(mean = dplyr::if_else(abs(mean) < 0.1, NA, mean)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable, y = taxon, fill = mean)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

ind_cors |>
  tidyr::pivot_longer(cols = c(Tmin2_01:Tmin2_12, Tmin_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable, taxon) |>
  dplyr::summarize(mean = mean(cor, na.rm = TRUE)) |>
  dplyr::mutate(mean = dplyr::if_else(abs(mean) < 0.1, NA, mean)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable, y = taxon, fill = mean)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

ind_cors |>
  tidyr::pivot_longer(cols = c(Tmax2_01:Tmax2_12, Tmax_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable, taxon) |>
  dplyr::summarize(mean = mean(cor, na.rm = TRUE)) |>
  dplyr::mutate(mean = dplyr::if_else(abs(mean) < 0.1, NA, mean)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable, y = taxon, fill = mean)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

ind_cors |>
  tidyr::pivot_longer(cols = c(Vpdmin2_01:Vpdmin2_12, Vpdmin_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable, taxon) |>
  dplyr::summarize(mean = mean(cor, na.rm = TRUE)) |>
  dplyr::mutate(mean = dplyr::if_else(abs(mean) < 0.1, NA, mean)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable, y = taxon, fill = mean)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

ind_cors |>
  tidyr::pivot_longer(cols = c(Vpdmax2_01:Vpdmax2_12, Vpdmax_growing),
                      names_to = 'variable', values_to = 'cor') |>
  dplyr::group_by(variable, taxon) |>
  dplyr::summarize(mean = mean(cor, na.rm = TRUE)) |>
  dplyr::mutate(mean = dplyr::if_else(abs(mean) < 0.1, NA, mean)) |>
  ggplot2::ggplot() +
  ggplot2::geom_tile(ggplot2::aes(x = variable, y = taxon, fill = mean)) +
  ggplot2::scale_fill_gradient2(low = 'darkred', high = 'cadetblue',
                                limits = c(-1, 1),
                                na.value = 'grey85',
                                name = 'Correlation') +
  ggplot2::xlab('') + ggplot2::ylab('') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
