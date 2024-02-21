rm(list = ls())

# Load total increment
goose_total_agbi <- readRDS('sites/GOOSE/runs/v2.0_012021/output/AGBI_STAN_GOOSE_v2.0_012021.RDS')
nrp_total_agbi <- readRDS('sites/NORTHROUND/runs/v2.0_082020/output/AGBI_STAN_NORTHROUND_v2.0_082020.RDS')
rooster_total_agbi <- readRDS('sites/ROOSTER/runs/v2.0_082020/output/AGBI_STAN_ROOSTER_v2.0_082020.RDS')
sylv_total_agbi <- readRDS('sites/SYLVANIA/runs/v2.0_082020/output/AGBI_STAN_SYLVANIA_v2.0_082020.RDS')

goose_total_agbi <- goose_total_agbi |>
  dplyr::mutate(site = 'GOOSE') |>
  dplyr::filter(year > 1959)
nrp_total_agbi <- nrp_total_agbi |>
  dplyr::mutate(site = 'NRP') |>
  dplyr::filter(year > 1959)
rooster_total_agbi <- rooster_total_agbi |>
  dplyr::mutate(site = 'ROOSTER') |>
  dplyr::filter(year > 1959)
sylv_total_agbi <- sylv_total_agbi |>
  dplyr::mutate(site = 'SYLVANIA') |>
  dplyr::filter(year > 1959)

# Combine sites
total_agbi <- rbind(goose_total_agbi, nrp_total_agbi,
                    rooster_total_agbi, sylv_total_agbi)

# Plot NPP for each tree and site
total_agbi |>
  dplyr::group_by(tree, year, plot, taxon, site) |>
  dplyr::summarize(mean = mean(value)) |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = mean, color = as.factor(tree))) +
  ggplot2::geom_line(show.legend = F) +
  ggplot2::facet_wrap(~site) +
  ggplot2::xlab('') + ggplot2::ylab('AGBI') +
  ggplot2::theme_minimal()

# Save mean over iterations in dataframe
total_agbi <- total_agbi |>
  dplyr::group_by(tree, year, plot, taxon, site) |>
  dplyr::summarize(mean = mean(value))

# Indexing for loops
site <- c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA')

# Load climate data
load('climate/prism_clim.RData')

# Format
prism_long <- dplyr::rename(prism_long, site = loc) |>
  dplyr::mutate(year = as.numeric(year))

# Pivot wider
prism_annual <- prism_long |>
  dplyr::group_by(year, site) |>
  dplyr::summarize(mean_PPT = mean(PPT2),
                   mean_Tmean = mean(Tmean2),
                   mean_Tmin = mean(Tmin2),
                   mean_Tmax = mean(Tmax2),
                   mean_Vpdmin = mean(Vpdmin2),
                   mean_Vpdmax = mean(Vpdmax2)) 
prism_month <- tidyr::pivot_wider(prism_long, names_from = 'month', values_from = c('PPT2', 'Tmean2', 'Tmin2', 'Tmax2', 'Vpdmin2', 'Vpdmax2'))

# Load competition information
load('data/competition_metrics.RData')

ntrees <- c(length(unique(goose_total_agbi$tree)),
            length(unique(nrp_total_agbi$tree)),
            length(unique(rooster_total_agbi$tree)),
            length(unique(sylv_total_agbi$tree)))
# Storage
importance_save <- list()

row_ind <- 0
# For each site, let's iteratively fit a random forest
for(i in 1:4){
  # Tree number index, unique to each site
  tree <- unique(total_agbi$tree[which(total_agbi$site == site[i])])
  # Save site name
  site_name <- site[i]
  # Loop through each tree at a given site
  for(j in tree){
    # Increment counter
    row_ind <- row_ind + 1
    # Subset full data for one tree
    sub <- dplyr::filter(total_agbi, site == site_name &
                           tree == j)
    # Combine tree data with climate
    joined <- sub |>
      dplyr::left_join(y = prism_annual, by = c('site', 'year')) |>
      dplyr::left_join(y = ba_by_tree, by = c('tree', 'year', 'plot', 'taxon', 'site')) |>
      # only keep ba, bagt
      dplyr::select(-frac, -total_ba, -dbh) |>
      dplyr::left_join(y = ba_by_taxon, by = c('plot', 'site', 'year', 'taxon')) |>
      # only keep ba, bagt (from before), frac
      dplyr::select(-total_ba.x, -total_ba.y) |>
      dplyr::left_join(y = total_ba, by = c('plot', 'site', 'year')) |>
      dplyr::ungroup() |>
      dplyr::select(-tree, -year, -plot, -taxon, -site)
    
    # Fit random forest
    mod <- randomForest::randomForest(formula = mean ~ .,
                                      data = joined,
                                      ntree = 2000,
                                      maxnodes = 3,
                                      importance = TRUE,
                                      mtry = sqrt(ncol(joined)-1))
    
    imp <- randomForest::importance(mod)
    imp <- as.data.frame(imp)
    imp$site = site[i]
    imp$tree = j
    imp <- tibble::rownames_to_column(imp, var = 'variable')
    imp$rankIncMSE <- rank(-imp$`%IncMSE`)
    imp$rankIncNodePurity <- rank(-imp$IncNodePurity)
    
    # Save site name, tree number, coefficients, and r2 in matrix
    importance_save[[row_ind]] <- imp
    
    
    print(j)
  }
  print(paste0('---------------------',i,'----------------'))
}

# Unlist
ind_importance <- do.call(rbind, importance_save)

ind_importance |>
  dplyr::mutate(dplyr::if_else(variable == 'mean_PPT', 'precipitation', variable),
                dplyr::if_else(variable == 'mean_Tmean', 'temperature', variable),
                dplyr::if_else(variable == 'mean_Tmin', 'min. temperature', variable),
                dplyr::if_else(variable == 'mean_Tmax', 'max. temperature', variable),
                dplyr::if_else(variable == 'mean_Vpdmin', 'min. VPD', variable),
                dplyr::if_else(variable == 'mean_Vpdmax', 'max. VPD', variable),
                dplyr::if_else(variable == 'ba', 'basal area of ind.', variable),
                dplyr::if_else(variable == 'bagt', 'basal area greater than', variable),
                dplyr::if_else(variable == 'frac', 'frac. basal area/species', variable),
                dplyr::if_else(variable == 'total_ba', 'plot ba', variable)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = variable, y = rankIncNodePurity)) +
  ggplot2::facet_wrap(~site) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::scale_y_reverse()
