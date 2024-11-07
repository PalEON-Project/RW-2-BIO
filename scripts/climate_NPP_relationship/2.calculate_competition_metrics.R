# Calculating metrics of community processes
# Calculates from outputs of model

rm(list = ls())

## Read in outputs

# Tree level
tree_dbh_goose <- readRDS('sites/GOOSE/runs/v3.1_012021/output/DBH_STAN_GOOSE_v3.1_012021.RDS')
tree_dbh_nrp <- readRDS('sites/NORTHROUND/runs/v3.1_082020/output/DBH_STAN_NORTHROUND_v3.1_082020.RDS')
tree_dbh_rooster <- readRDS('sites/ROOSTER/runs/v3.1_082020/output/DBH_STAN_ROOSTER_v3.1_082020.RDS')
tree_dbh_sylv <- readRDS('sites/SYLVANIA/runs/v3.1_082020/output/DBH_STAN_SYLVANIA_v3.1_082020.RDS')
tree_dbh_harv <- readRDS('sites/HARVARD/runs/v3.1_102020/output/DBH_STAN_HARVARD_v3.1_102020.RDS')

# Split harvard rw and rw + census
tree_dbh_harv_rw <- dplyr::filter(tree_dbh_harv, model == 'Model RW')
tree_dbh_harv_cen <- dplyr::filter(tree_dbh_harv, model == 'Model RW + Census')

# Remove total
rm(tree_dbh_harv)

# Subset for 1960 and after. This leaves us with about 50 years
# and this should limit the fading record problem
tree_dbh_goose <- tree_dbh_goose |>
  dplyr::mutate(site = 'GOOSE') |>
  dplyr::filter(year > 1959)
tree_dbh_nrp <- tree_dbh_nrp |>
  dplyr::mutate(site = 'NRP') |>
  dplyr::filter(year > 1959)
tree_dbh_rooster <- tree_dbh_rooster |>
  dplyr::mutate(site = 'ROOSTER') |>
  dplyr::filter(year > 1959)
tree_dbh_sylv <- tree_dbh_sylv |>
  dplyr::mutate(site = 'SYLVANIA') |>
  dplyr::filter(year > 1959)
tree_dbh_harv_rw <- tree_dbh_harv_rw |>
  dplyr::mutate(site = 'HARVARD Model RW') |>
  dplyr::filter(year > 1959)
tree_dbh_harv_cen <- tree_dbh_harv_cen |>
  dplyr::mutate(site = 'HARVARD Model RW + Census') |>
  dplyr::filter(year > 1959)

# Bind together
tree_dbh <- rbind(tree_dbh_goose, 
                  tree_dbh_nrp, 
                  tree_dbh_rooster, 
                  tree_dbh_sylv,
                  tree_dbh_harv_rw,
                  tree_dbh_harv_cen)

# Calculate tree-level metrics
tree_dbh2 <- tree_dbh |>
  dplyr::rename(dbh_cm = value) |>
  # Calculate basal area of each tree from its DBH
  dplyr::mutate(ba_m2 = pi * dbh_cm^2 / (4 * 10000)) |>
  # Group so that we're working at the tree level
  dplyr::group_by(tree, year, plot, taxon, site) |>
  # Mean DBH and BA per tree over all iterations
  dplyr::summarize(dbh_cm = mean(dbh_cm),
                   ba_m2 = mean(ba_m2))

# Save
save(tree_dbh2, file = 'data/ind_dbh_ba.RData')

## Calculate basal area (m2/ha)

# Goose = triple nested

# factors to adjust basal area to incorporate 
# triple nested sampling design
# new units = m2/m2
inner_factor = (1 / (pi*13^2))
mid_factor = (1 / (pi*20^2))
outer_factor = (1 / (pi*30^2))

# Last year's data because this will give us which
# nested ring each tree fell within
goose_lastyear <- tree_dbh2 |>
  dplyr::filter(site == 'GOOSE') |>
  dplyr::group_by(tree, plot, taxon, site) |>
  dplyr::summarize(year = max(year),
                   last = TRUE)
goose_lastyear2 <- tree_dbh2 |>
  dplyr::filter(site == 'GOOSE') |>
  dplyr::left_join(y = goose_lastyear,
                   by = c('tree', 'plot', 'taxon', 'site', 'year')) |>
  dplyr::filter(!is.na(last)) |>
  dplyr::mutate(size = dplyr::if_else(dbh_cm < 20, 'small', NA),
                size = dplyr::if_else(dbh_cm >= 20 & dbh_cm < 30, 'med', size),
                size = dplyr::if_else(dbh_cm >= 30, 'large', size)) |>
  dplyr::ungroup() |>
  dplyr::select(tree, plot, taxon, size)

# Add size column to full goose dataset
goose_taxon_ba <- tree_dbh2 |>
  dplyr::filter(site == 'GOOSE') |>
  dplyr::left_join(y = goose_lastyear2,
                   by = c('tree', 'plot', 'taxon')) |>
  dplyr::mutate(corr_ba_m2_m2 = dplyr::if_else(size == 'small',
                                               ba_m2 * inner_factor,
                                               NA),
                corr_ba_m2_m2 = dplyr::if_else(size == 'med',
                                               ba_m2 * mid_factor,
                                               corr_ba_m2_m2),
                corr_ba_m2_m2 = dplyr::if_else(size == 'large',
                                               ba_m2 * outer_factor,
                                               corr_ba_m2_m2)) |>
  dplyr::group_by(year, plot, taxon, site) |>
  dplyr::summarize(taxon_ba_m2_m2 = sum(corr_ba_m2_m2))

# North Round Pond = triple nested
nrp_lastyear <- tree_dbh2 |>
  dplyr::filter(site == 'NRP') |>
  dplyr::group_by(tree, plot, taxon, site) |>
  dplyr::summarize(year = max(year),
                   last = TRUE)
nrp_lastyear2 <- tree_dbh2 |>
  dplyr::filter(site == 'NRP') |>
  dplyr::left_join(y = nrp_lastyear,
                   by = c('tree', 'plot', 'taxon', 'site', 'year')) |>
  dplyr::filter(!is.na(last)) |>
  dplyr::mutate(size = dplyr::if_else(dbh_cm < 20, 'small', NA),
                size = dplyr::if_else(dbh_cm >= 20 & dbh_cm < 30, 'med', size),
                size = dplyr::if_else(dbh_cm >= 30, 'large', size)) |>
  dplyr::ungroup() |>
  dplyr::select(tree, plot, taxon, size)

nrp_taxon_ba <- tree_dbh2 |>
  dplyr::filter(site == 'NRP') |>
  dplyr::left_join(y = nrp_lastyear2,
                   by = c('tree', 'plot', 'taxon')) |>
  dplyr::mutate(corr_ba_m2_m2 = dplyr::if_else(size == 'small',
                                               ba_m2 * inner_factor,
                                               NA),
                corr_ba_m2_m2 = dplyr::if_else(size == 'med',
                                               ba_m2 * mid_factor,
                                               corr_ba_m2_m2),
                corr_ba_m2_m2 = dplyr::if_else(size == 'large',
                                               ba_m2 * outer_factor,
                                               corr_ba_m2_m2)) |>
  dplyr::group_by(year, plot, taxon, site) |>
  dplyr::summarize(taxon_ba_m2_m2 = sum(corr_ba_m2_m2))

# Rooster Hill = triple nested
rooster_lastyear <- tree_dbh2 |>
  dplyr::filter(site == 'ROOSTER') |>
  dplyr::group_by(tree, plot, taxon, site) |>
  dplyr::summarize(year = max(year),
                   last = TRUE)
rooster_lastyear2 <- tree_dbh2 |>
  dplyr::filter(site == 'ROOSTER') |>
  dplyr::left_join(y = rooster_lastyear,
                   by = c('tree', 'plot', 'taxon', 'site', 'year')) |>
  dplyr::filter(!is.na(last)) |>
  dplyr::mutate(size = dplyr::if_else(dbh_cm < 20, 'small', NA),
                size = dplyr::if_else(dbh_cm >= 20 & dbh_cm < 30, 'med', size),
                size = dplyr::if_else(dbh_cm >= 30, 'large', size)) |>
  dplyr::ungroup() |>
  dplyr::select(tree, plot, taxon, size)

rooster_taxon_ba <- tree_dbh2 |>
  dplyr::filter(site == 'ROOSTER') |>
  dplyr::left_join(y = rooster_lastyear2,
                   by = c('tree', 'plot', 'taxon')) |>
  dplyr::mutate(corr_ba_m2_m2 = dplyr::if_else(size == 'small',
                                               ba_m2 * inner_factor,
                                               NA),
                corr_ba_m2_m2 = dplyr::if_else(size == 'med',
                                               ba_m2 * mid_factor,
                                               corr_ba_m2_m2),
                corr_ba_m2_m2 = dplyr::if_else(size == 'large',
                                               ba_m2 * outer_factor,
                                               corr_ba_m2_m2)) |>
  dplyr::group_by(year, plot, taxon, site) |>
  dplyr::summarize(taxon_ba_m2_m2 = sum(corr_ba_m2_m2))

# Sylvania = triple nested
sylvania_lastyear <- tree_dbh2 |>
  dplyr::filter(site == 'SYLVANIA') |>
  dplyr::group_by(tree, plot, taxon, site) |>
  dplyr::summarize(year = max(year),
                   last = TRUE)
sylvania_lastyear2 <- tree_dbh2 |>
  dplyr::filter(site == 'SYLVANIA') |>
  dplyr::left_join(y = sylvania_lastyear,
                   by = c('tree', 'plot', 'taxon', 'site', 'year')) |>
  dplyr::filter(!is.na(last)) |>
  dplyr::mutate(size = dplyr::if_else(dbh_cm < 20, 'small', NA),
                size = dplyr::if_else(dbh_cm >= 20 & dbh_cm < 30, 'med', size),
                size = dplyr::if_else(dbh_cm >= 30, 'large', size)) |>
  dplyr::ungroup() |>
  dplyr::select(tree, plot, taxon, size)

sylvania_taxon_ba <- tree_dbh2 |>
  dplyr::filter(site == 'SYLVANIA') |>
  dplyr::left_join(y = sylvania_lastyear2,
                   by = c('tree', 'plot', 'taxon')) |>
  dplyr::mutate(corr_ba_m2_m2 = dplyr::if_else(size == 'small',
                                               ba_m2 * inner_factor,
                                               NA),
                corr_ba_m2_m2 = dplyr::if_else(size == 'med',
                                               ba_m2 * mid_factor,
                                               corr_ba_m2_m2),
                corr_ba_m2_m2 = dplyr::if_else(size == 'large',
                                               ba_m2 * outer_factor,
                                               corr_ba_m2_m2)) |>
  dplyr::group_by(year, plot, taxon, site) |>
  dplyr::summarize(taxon_ba_m2_m2 = sum(corr_ba_m2_m2))

# Harvard Forest = double nested

# New factors for double-nested plots
inner_factor = (1 / (pi*13^2))
outer_factor = (1 / (pi*20^2))

harvard_rw_lastyear <- tree_dbh2 |>
  dplyr::filter(site == 'HARVARD Model RW') |>
  dplyr::group_by(tree, plot, taxon, site) |>
  dplyr::summarize(year = max(year),
                   last = TRUE)
harvard_rw_lastyear2 <- tree_dbh2 |>
  dplyr::filter(site == 'HARVARD Model RW') |>
  dplyr::left_join(y = harvard_rw_lastyear,
                   by = c('tree', 'plot', 'taxon', 'site', 'year')) |>
  dplyr::filter(!is.na(last)) |>
  dplyr::mutate(size = dplyr::if_else(dbh_cm < 20, 'small', NA),
                size = dplyr::if_else(dbh_cm >= 20, 'large', size)) |>
  dplyr::ungroup() |>
  dplyr::select(tree, plot, taxon, size)

harvard_rw_taxon_ba <- tree_dbh2 |>
  dplyr::filter(site == 'HARVARD Model RW') |>
  dplyr::left_join(y = harvard_rw_lastyear2,
                   by = c('tree', 'plot', 'taxon')) |>
  dplyr::mutate(corr_ba_m2_m2 = dplyr::if_else(size == 'small',
                                               ba_m2 * inner_factor,
                                               NA),
                corr_ba_m2_m2 = dplyr::if_else(size == 'large',
                                               ba_m2 * outer_factor,
                                               corr_ba_m2_m2)) |>
  dplyr::group_by(year, plot, taxon, site) |>
  dplyr::summarize(taxon_ba_m2_m2 = sum(corr_ba_m2_m2))

## What to do with harvard rw + census?
save(goose_taxon_ba,
     nrp_taxon_ba,
     rooster_taxon_ba,
     sylvania_taxon_ba,
     harvard_rw_taxon_ba,
     file = 'data/taxon_ba.RData')
