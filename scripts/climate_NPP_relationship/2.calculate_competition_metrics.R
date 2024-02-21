# Calculating metrics of community processes
# Calculates from outputs of model

rm(list = ls())

# Read in outputs
dbh_goose <- readRDS('sites/GOOSE/runs/v2.0_012021/output/DBH_STAN_GOOSE_v2.0_012021.RDS')
dbh_nrp <- readRDS('sites/NORTHROUND/runs/v2.0_082020/output/DBH_STAN_NORTHROUND_v2.0_082020.RDS')
dbh_rooster <- readRDS('sites/ROOSTER/runs/v2.0_082020/output/DBH_STAN_ROOSTER_v2.0_082020.RDS')
dbh_sylv <- readRDS('sites/SYLVANIA/runs/v2.0_082020/output/DBH_STAN_SYLVANIA_v2.0_082020.RDS')

# Subset for 1960 and after. This leaves us with about 50 years
# and this should limit the fading record problem
dbh_goose <- dbh_goose |>
  dplyr::mutate(site = 'GOOSE') |>
  dplyr::filter(year > 1959)
dbh_nrp <- dbh_nrp |>
  dplyr::mutate(site = 'NRP') |>
  dplyr::filter(year > 1959)
dbh_rooster <- dbh_rooster |>
  dplyr::mutate(site = 'ROOSTER') |>
  dplyr::filter(year > 1959)
dbh_sylv <- dbh_sylv |>
  dplyr::mutate(site = 'SYLVANIA') |>
  dplyr::filter(year > 1959)

# Bind together
dbh <- rbind(dbh_goose, dbh_nrp, dbh_rooster, dbh_sylv)

# Calculate tree-level metrics
dbh <- dbh |>
  dplyr::rename(dbh = value) |>
  # Calculate basal area of each tree from its DBH
  dplyr::mutate(ba = pi * (dbh/2)^2) |>
  # Group so that we're working at the tree level
  dplyr::group_by(tree, year, plot, taxon, site) |>
  # Mean DBH and BA per tree over all iterations
  dplyr::summarize(dbh = mean(dbh),
                   ba = mean(ba))

# Plot-level metric
# This uses the mean of iterations from above
total_ba <- dbh |>
  # Group by plot and summarize over tree and taxon
  dplyr::group_by(plot, site, year) |>
  # Total basal area per plot = sum of individual basal area
  dplyr::summarize(total_ba = sum(ba))

# Taxon-level metric
ba_by_taxon <- dbh |>
  # Group by plot and taxon and summarize over individual trees
  dplyr::group_by(plot, site, year, taxon) |>
  # Basal area per taxon (sum over individual trees in each taxon)
  dplyr::summarize(total_ba = sum(ba)) |>
  # Total basal area per plot
  dplyr::left_join(y = total_ba, by = c('plot', 'site', 'year')) |>
  # Fraction of basal area belonging to each species
  dplyr::mutate(frac = total_ba.x / total_ba.y)

# Tree-level fraction of total basal area
ba_by_tree <- dbh |>
  # Group by plot, taxon and tree
  dplyr::group_by(plot, site, year, taxon, tree) |>
  # Total basal area per plot
  dplyr::left_join(y = total_ba, by = c('plot', 'site', 'year')) |>
  # Fracton of basal area belonging to each individual tree
  dplyr::mutate(ind_frac = ba / total_ba)

# Basal area of trees larger than each individual tree
sites <- c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA')

# Index
ind <- 0
# Storage
# 5 columns = site, plot, year, tree, basal area greater than
bagt <- matrix(, ncol = 5, nrow = nrow(ba_by_tree))

# Loop over each site
for(s in sites){
  # Loop over each plot within the site
  for(p in 1:length(unique(dbh$plot[which(dbh$site == s)]))){
    # Loop over each year within each site
    for(y in 1960:max(dbh$year[which(dbh$site == s)])){
      # Subset the tree-level summary for a given site, plot, and year
      sub <- dbh |>
        dplyr::filter(year == y &
                        plot == p &
                        site == s)
      # Loop over each tree
      for(t in unique(sub$tree)){
        # Increment index
        ind <- ind + 1
        # Find which trees have greater basal area than the given tree
        gt <- which(sub$ba > sub$ba[which(sub$tree == t)])
        # Sum of basal area of all trees larger than the given tree
        bagt[ind,5] <- sum(sub$ba[gt])
        # Record other columns
        bagt[ind,1] <- s
        bagt[ind,2] <- p
        bagt[ind,3] <- y
        bagt[ind,4] <- t
      }
    }
  }
  # Progress
  print(s)
}

# Rename columns
bagt <- as.data.frame(bagt)
colnames(bagt) <- c('site', 'plot', 'year', 'tree', 'bagt')
# Format
bagt$tree <- as.numeric(bagt$tree)
bagt$year <- as.numeric(bagt$year)
bagt$plot <- as.numeric(bagt$plot)

# Add to individual tree dataframe
ba_by_tree <- ba_by_tree |>
  dplyr::left_join(y = bagt, by = c('tree', 'year', 'plot', 'site'))

# Save
save(total_ba, ba_by_taxon, ba_by_tree, file = 'data/competition_metrics.RData')
