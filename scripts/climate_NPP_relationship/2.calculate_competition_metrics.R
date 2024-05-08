# Calculating metrics of community processes
# Calculates from outputs of model

rm(list = ls())

## Read in outputs

# Tree level
tree_dbh_goose <- readRDS('sites/GOOSE/runs/v3.1_012021/output/DBH_STAN_GOOSE_v3.1_012021.RDS')
tree_dbh_nrp <- readRDS('sites/NORTHROUND/runs/v3.1_082020/output/DBH_STAN_NORTHROUND_v3.1_082020.RDS')
tree_dbh_rooster <- readRDS('sites/ROOSTER/runs/v3.1_082020/output/DBH_STAN_ROOSTER_v3.1_082020.RDS')
tree_dbh_sylv <- readRDS('sites/SYLVANIA/runs/v3.1_082020/output/DBH_STAN_SYLVANIA_v3.1_082020.RDS')

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

# Bind together
tree_dbh <- rbind(tree_dbh_goose, 
                  tree_dbh_nrp, 
                  tree_dbh_rooster, 
                  tree_dbh_sylv)

# Calculate tree-level metrics
tree_dbh <- tree_dbh |>
  dplyr::rename(dbh = value) |>
  # Calculate basal area of each tree from its DBH
  dplyr::mutate(ba = pi * (dbh/2)^2) |>
  # Group so that we're working at the tree level
  dplyr::group_by(tree, year, plot, taxon, site) |>
  # Mean DBH and BA per tree over all iterations
  dplyr::summarize(dbh = mean(dbh),
                   ba = mean(ba))


# Save
save(tree_dbh, file = 'data/competition_metrics.RData')
