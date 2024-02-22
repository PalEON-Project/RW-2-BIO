## Random forest at taxon level

rm(list = ls())

# Load total increment
goose_total_agbi <- readRDS('sites/GOOSE/runs/v2.0_012021/output/AGBI_STAN_GOOSE_v2.0_012021.RDS')
nrp_total_agbi <- readRDS('sites/NORTHROUND/runs/v2.0_082020/output/AGBI_STAN_NORTHROUND_v2.0_082020.RDS')
rooster_total_agbi <- readRDS('sites/ROOSTER/runs/v2.0_082020/output/AGBI_STAN_ROOSTER_v2.0_082020.RDS')
sylv_total_agbi <- readRDS('sites/SYLVANIA/runs/v2.0_082020/output/AGBI_STAN_SYLVANIA_v2.0_082020.RDS')

# Subset for 1960 and beyond to reduce problem of fading record
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

# Save mean over iterations in dataframe
total_agbi <- total_agbi |>
  dplyr::group_by(year, plot, taxon, site) |>
  dplyr::summarize(mean = mean(value))

# Indexing for loops
site <- c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA')
taxa <- c()
taxa[1] <- length(unique(total_agbi$taxon[which(total_agbi$site == 'GOOSE')]))
taxa[2] <- length(unique(total_agbi$taxon[which(total_agbi$site == 'NRP')]))
taxa[3] <- length(unique(total_agbi$taxon[which(total_agbi$site == 'ROOSTER')]))
taxa[4] <- length(unique(total_agbi$taxon[which(total_agbi$site == 'SYLVANIA')]))

# Load climate data
load('climate/prism_clim.RData')

# Format
prism_long <- dplyr::rename(prism_long, site = loc) |>
  dplyr::mutate(year = as.numeric(year))

# Pivot wider
prism_annual <- prism_long |>
  dplyr::group_by(year, site) |>
  # Average over months
  dplyr::summarize(mean_PPT = mean(PPT2),
                   mean_Tmean = mean(Tmean2),
                   sd_PPT = sd(PPT2),
                   sd_Tmean = sd(Tmean2),
                   mean_Tmin = min(Tmin2),
                   mean_Tmax = max(Tmax2),
                   mean_Vpdmin = min(Vpdmin2),
                   mean_Vpdmax = max(Vpdmax2))

# Load competition information
load('data/competition_metrics.RData')

# Storage
importance_save <- list()

row_ind <- 0
# For each site, iteratively fit a random forest
for(i in 1:4){
  # Taxon number index, unique to each site
  taxon <- seq(from = 1, to = taxa[i], by = 1)
  # Save site name
  site_name <- site[i]
  # Loop through each taxon at a given site
  for(j in taxon){
    # Increment counter
    row_ind <- row_ind + 1
    # Save taxon number
    taxon_name <- unique(total_agbi$taxon[which(total_agbi$site == site_name)])[j]
    # Subset full data for one taxon
    sub <- dplyr::filter(total_agbi, site == site_name &
                           taxon == taxon_name)
    # Combine tree data with climate
    joined <- sub |>
      # Join with annual climate drivers
      dplyr::left_join(y = prism_annual, by = c('site', 'year')) |>
      # Join with taxon-level competition metrics 
      dplyr::left_join(y = ba_by_taxon, by = c('plot', 'site', 'year', 'taxon')) |>
      # only keep fraction of total basal area from each taxon (frac)
      dplyr::select(-total_ba.x, -total_ba.y) |>
      # join with plot-level competition metrics
      dplyr::left_join(y = total_ba, by = c('plot', 'site', 'year')) |>
      # ungroup so we  can removing indexing columns
      dplyr::ungroup() |>
      # removing indexing columns that we don't want to be in random forest
      dplyr::select(-year, -plot, -taxon, -site)
    
    # Fit random forest
    mod <- randomForest::randomForest(formula = mean ~ ., # all predictors predicting taxon level BAI
                                      data = joined, # data subset with all predictors
                                      ntree = 2000, # kinda high number of trees?
                                      importance = TRUE) # calculate importance of variables
    
    # Extract importance statistics
    imp <- randomForest::importance(mod)
    # Format
    imp <- as.data.frame(imp)
    # Add information about site, tree, variable
    imp$site <- site[i]
    imp$taxon <- taxon_name
    imp <- tibble::rownames_to_column(imp, var = 'variable')
    # Imporatnce statistics
    imp$rankIncMSE <- rank(-imp$`%IncMSE`)
    imp$rankIncNodePurity <- rank(-imp$IncNodePurity)
    
    # Save site name, taxon name, importance statistics
    importance_save[[row_ind]] <- imp
    
    print(j)
  }
  print(paste0('----------------------',i,'--------------------'))
}

# Unlist
taxon_importance <- do.call(rbind, importance_save)

# Order of x-axis
level_order <- c('precipitation', 'temperature', 'precipitation var.', 'temperature var.',
                 'min. temperature', 'max. temperature', 'min. VPD', 'max. VPD',
                 'frac. ba/species', 'plot ba')

taxon_importance |>
  dplyr::mutate(variable = dplyr::if_else(variable == 'mean_PPT', 'precipitation', variable),
                variable = dplyr::if_else(variable == 'mean_Tmean', 'temperature', variable),
                variable = dplyr::if_else(variable == 'sd_PPT', 'precipitation var.', variable),
                variable = dplyr::if_else(variable == 'sd_Tmean', 'temperature var.', variable),
                variable = dplyr::if_else(variable == 'mean_Tmin', 'min. temperature', variable),
                variable = dplyr::if_else(variable == 'mean_Tmax', 'max. temperature', variable),
                variable = dplyr::if_else(variable == 'mean_Vpdmin', 'min. VPD', variable),
                variable = dplyr::if_else(variable == 'mean_Vpdmax', 'max. VPD', variable),
                variable = dplyr::if_else(variable == 'frac', 'frac. ba/species', variable),
                variable = dplyr::if_else(variable == 'total_ba', 'plot ba', variable)) |>
  dplyr::mutate(class = dplyr::if_else(variable %in% c('precipitation', 'temperature',
                                                       'precipitation var.', 'temperature var.',
                                                       'min. temperature', 'max. temperature',
                                                       'min. VPD', 'max. VPD'),
                                       'climate', NA),
                class = dplyr::if_else(variable == 'frac. ba/species',
                                       'sp.-level competition', class),
                class = dplyr::if_else(variable == 'plot ba',
                                       'plot-level competition', class)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = factor(variable, level = level_order), y = rankIncNodePurity, color = class), show.legend = F) +
  ggplot2::facet_wrap(~site) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::scale_y_reverse(breaks = seq(0, 12, by = 2)) +
  ggplot2::xlab('')

taxon_importance |>
  dplyr::mutate(variable = dplyr::if_else(variable == 'mean_PPT', 'precipitation', variable),
                variable = dplyr::if_else(variable == 'mean_Tmean', 'temperature', variable),
                variable = dplyr::if_else(variable == 'sd_PPT', 'precipitation var.', variable),
                variable = dplyr::if_else(variable == 'sd_Tmean', 'temperature var.', variable),
                variable = dplyr::if_else(variable == 'mean_Tmin', 'min. temperature', variable),
                variable = dplyr::if_else(variable == 'mean_Tmax', 'max. temperature', variable),
                variable = dplyr::if_else(variable == 'mean_Vpdmin', 'min. VPD', variable),
                variable = dplyr::if_else(variable == 'mean_Vpdmax', 'max. VPD', variable),
                variable = dplyr::if_else(variable == 'frac', 'frac. ba/species', variable),
                variable = dplyr::if_else(variable == 'total_ba', 'plot ba', variable)) |>
  dplyr::mutate(class = dplyr::if_else(variable %in% c('precipitation', 'temperature',
                                                       'precipitation var.', 'temperature var.',
                                                       'min. temperature', 'max. temperature',
                                                       'min. VPD', 'max. VPD'),
                                       'climate', NA),
                class = dplyr::if_else(variable == 'frac. ba/species',
                                       'sp.-level competition', class),
                class = dplyr::if_else(variable == 'plot ba',
                                       'plot-level competition', class)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = factor(variable, level = level_order), y = rankIncMSE, color = class), show.legend = F) +
  ggplot2::facet_wrap(~site) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::scale_y_reverse(breaks = seq(0, 12, by = 2)) +
  ggplot2::xlab('')

taxon_importance |>
  dplyr::mutate(variable = dplyr::if_else(variable == 'mean_PPT', 'precipitation', variable),
                variable = dplyr::if_else(variable == 'mean_Tmean', 'temperature', variable),
                variable = dplyr::if_else(variable == 'sd_PPT', 'precipitation var.', variable),
                variable = dplyr::if_else(variable == 'sd_Tmean', 'temperature var.', variable),
                variable = dplyr::if_else(variable == 'mean_Tmin', 'min. temperature', variable),
                variable = dplyr::if_else(variable == 'mean_Tmax', 'max. temperature', variable),
                variable = dplyr::if_else(variable == 'mean_Vpdmin', 'min. VPD', variable),
                variable = dplyr::if_else(variable == 'mean_Vpdmax', 'max. VPD', variable),
                variable = dplyr::if_else(variable == 'frac', 'frac. ba/species', variable),
                variable = dplyr::if_else(variable == 'total_ba', 'plot ba', variable)) |>
  dplyr::mutate(class = dplyr::if_else(variable %in% c('precipitation', 'temperature',
                                                       'precipitation var.', 'temperature var.',
                                                       'min. temperature', 'max. temperature',
                                                       'min. VPD', 'max. VPD'),
                                       'climate', NA),
                class = dplyr::if_else(variable == 'frac. ba/species',
                                       'sp.-level competition', class),
                class = dplyr::if_else(variable == 'plot ba',
                                       'plot-level competition', class)) |>
  dplyr::group_by(site, variable, class) |>
  dplyr::summarize(mean_rank = mean(rankIncNodePurity),
                   mean_rank = 12-mean_rank) |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = factor(variable, level = level_order), y = mean_rank, fill = class), 
                    show.legend = F, stat = 'identity') +
  ggplot2::facet_wrap(~site) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  ggplot2::xlab('')

taxon_importance |>
  dplyr::mutate(variable = dplyr::if_else(variable == 'mean_PPT', 'precipitation', variable),
                variable = dplyr::if_else(variable == 'mean_Tmean', 'temperature', variable),
                variable = dplyr::if_else(variable == 'sd_PPT', 'precipitation var.', variable),
                variable = dplyr::if_else(variable == 'sd_Tmean', 'temperature var.', variable),
                variable = dplyr::if_else(variable == 'mean_Tmin', 'min. temperature', variable),
                variable = dplyr::if_else(variable == 'mean_Tmax', 'max. temperature', variable),
                variable = dplyr::if_else(variable == 'mean_Vpdmin', 'min. VPD', variable),
                variable = dplyr::if_else(variable == 'mean_Vpdmax', 'max. VPD', variable),
                variable = dplyr::if_else(variable == 'frac', 'frac. ba/species', variable),
                variable = dplyr::if_else(variable == 'total_ba', 'plot ba', variable)) |>
  dplyr::mutate(class = dplyr::if_else(variable %in% c('precipitation', 'temperature',
                                                       'precipitation var.', 'temperature var.',
                                                       'min. temperature', 'max. temperature',
                                                       'min. VPD', 'max. VPD'),
                                       'climate', NA),
                class = dplyr::if_else(variable == 'frac. ba/species',
                                       'sp.-level competition', class),
                class = dplyr::if_else(variable == 'plot ba',
                                       'plot-level competition', class)) |>
  dplyr::group_by(site, variable, class) |>
  dplyr::summarize(mean_rank = mean(rankIncMSE),
                   mean_rank = 12-mean_rank) |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = factor(variable, level = level_order), y = mean_rank, fill = class), 
                    show.legend = F, stat = 'identity') +
  ggplot2::facet_wrap(~site) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  ggplot2::xlab('')

save(taxon_importance, file = 'out/taxon_rf_importance_save.RData')
