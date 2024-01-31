# Calculating metrics of community processes

rm(list = ls())

dbh_goose <- readRDS('sites/GOOSE/runs/v2.0_012021/output/DBH_STAN_GOOSE_v2.0_012021.RDS')
dbh_nrp <- readRDS('sites/NORTHROUND/runs/v2.0_082020/output/DBH_STAN_NORTHROUND_v2.0_082020.RDS')
dbh_rooster <- readRDS('sites/ROOSTER/runs/v2.0_082020/output/DBH_STAN_ROOSTER_v2.0_082020.RDS')
dbh_sylv <- readRDS('sites/SYLVANIA/runs/v2.0_082020/output/DBH_STAN_SYLVANIA_v2.0_082020.RDS')

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

dbh <- rbind(dbh_goose, dbh_nrp, dbh_rooster, dbh_sylv)

dbh <- dbh |>
  dplyr::rename(dbh = value) |>
  dplyr::mutate(ba = pi * (dbh/2)^2) |>
  dplyr::group_by(tree, year, plot, taxon, site) |>
  dplyr::summarize(dbh = mean(dbh),
                   ba = mean(ba))

# Total basal area per plot each year
total_ba <- dbh |>
  dplyr::group_by(plot, site, year) |>
  dplyr::summarize(total_ba = sum(ba))

# Fraction basal area taken up by the taxon each year
ba_by_taxon <- dbh |>
  dplyr::group_by(plot, site, year, taxon) |>
  dplyr::summarize(total_ba = sum(ba)) |>
  dplyr::left_join(y = total_ba, by = c('plot', 'site', 'year')) |>
  dplyr::mutate(frac = total_ba.x / total_ba.y)

# Fraction of basal area by tree
ba_by_tree <- dbh |>
  dplyr::group_by(plot, site, year, taxon, tree) |>
  dplyr::left_join(y = total_ba, by = c('plot', 'site', 'year')) |>
  dplyr::mutate(frac = ba / total_ba)

# Basal area of trees larger than each tree
sites <- c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA')

ind <- 0
bagt <- c()
for(s in sites){
  for(p in 1:length(unique(dbh$plot[which(dbh$site == s)]))){
    for(y in 1960:max(dbh$year[which(dbh$site == s)])){
      sub <- dbh |>
        dplyr::filter(year == y &
                        plot == p &
                        site == s)
      for(t in unique(dbh$tree[which(dbh$site == s & dbh$year == y & dbh$plot == p)])){
        ind <- ind + 1
        gt <- which(sub$ba > sub$ba[which(sub$tree == t)])
        bagt[ind] <- sum(sub$ba[gt])
      }
    }
  }
  print(s)
}

# Basal area greater than
ba_by_tree <- cbind(ba_by_tree, bagt)
colnames(ba_by_tree)[10] <- 'bagt'

# Save
save(total_ba, ba_by_taxon, ba_by_tree, file = 'data/competition_metrics.RData')
