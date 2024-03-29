rm(list = ls())

# Load detrended AGBI
load('out/detrended_AGBI.RData')

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
                   sd_PPT = sd(PPT2),
                   sd_Tmean = sd(Tmean2),
                   mean_Tmin = min(Tmin2),
                   mean_Tmax = max(Tmax2),
                   mean_Vpdmin = min(Vpdmin2),
                   mean_Vpdmax = max(Vpdmax2)) 
prism_month <- tidyr::pivot_wider(prism_long, names_from = 'month', values_from = c('PPT2', 'Tmean2', 'Tmin2', 'Tmax2', 'Vpdmin2', 'Vpdmax2'))

# Load competition information
load('data/competition_metrics.RData')

ntrees <- c(length(unique(save_comb$tree[which(save_comb$site == 'GOOSE')])),
            length(unique(save_comb$tree[which(save_comb$site == 'NRP')])),
            length(unique(save_comb$tree[which(save_comb$site == 'ROOSTER')])),
            length(unique(save_comb$tree[which(save_comb$site == 'SYLVANIA')])))
# Storage
importance_save <- list()

row_ind <- 0
# For each site, let's iteratively fit a random forest
for(i in 1:4){
  # Tree number index, unique to each site
  tree <- unique(save_comb$tree[which(save_comb$site == site[i])])
  # Save site name
  site_name <- site[i]
  # Loop through each tree at a given site
  for(j in tree){
    # Increment counter
    row_ind <- row_ind + 1
    # Subset full data for one tree
    sub <- dplyr::filter(save_comb, site == site_name &
                           tree == j)
    # Combine tree data with climate
    joined <- sub |>
      # Join with annual climate drivers
      dplyr::left_join(y = prism_annual, by = c('site', 'year')) |>
      # Join with tree-level competition metrics
      dplyr::left_join(y = ba_by_tree, by = c('tree', 'year', 'plot', 'taxon', 'site')) |>
      # only keep individual tree basal area (ba)
      # and basal area greater than (bagt)
      dplyr::select(-ind_frac, -total_ba, -dbh) |>
      # Join with taxon-level competition metrics
      dplyr::left_join(y = ba_by_taxon, by = c('plot', 'site', 'year', 'taxon')) |>
      # only keep ba, bagt (from before)
      # and fraction of total basal area from each taxon (frac)
      dplyr::select(-total_ba.x, -total_ba.y) |>
      # Join with plot-level competition metrics
      dplyr::left_join(y = total_ba, by = c('plot', 'site', 'year')) |>
      # Ungroup so we can remove indexing columns
      dplyr::ungroup() |>
      # Remove indexing columns that we don't want to be in random forest
      dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)
    
    if(nrow(joined) == 1){
      imp$variable <- NA
      imp$`%IncMSE` <- NA
      imp$IncNodePurity <- NA
      imp$site <- site[i]
      imp$tree <- j
      imp$rankIncMSE <- NA
      imp$rankIncNodePurity <- NA
    }else{
      # Fit random forest
      mod <- randomForest::randomForest(formula = residual_AGBI ~ ., # all predictors predicting individual tree BAI
                                        data = joined, # data subset joined with all predictors
                                        ntree = 2000, # kinda high number of trees?
                                        importance = TRUE) # calculate importance of variables
      
      # Extract importance statistics
      imp <- randomForest::importance(mod)
      # Format
      imp <- as.data.frame(imp)
      # Add information about site, tree, variable
      imp$site <- site[i]
      imp$tree <- j
      imp <- tibble::rownames_to_column(imp, var = 'variable')
      # Importance statistics
      imp$rankIncMSE <- rank(-imp$`%IncMSE`)
      imp$rankIncNodePurity <- rank(-imp$IncNodePurity)
      
    }
    
    # Save site name, tree number, importance statistics
    importance_save[[row_ind]] <- imp
    
    
    print(j)
  }
  print(paste0('---------------------',i,'----------------'))
}

# Unlist
ind_importance <- do.call(rbind, importance_save)

#load('out/ind_rf_importance_save.RData')

# Order of x-axis
level_order <- c('precipitation', 'temperature', 'precipitation var.', 'temperature var.',
                 'min. temperature', 'max. temperature', 'min. VPD', 'max. VPD',
                 'basal area', 'basal area greater than', 'frac. ba/species',
                 'plot ba')

ind_importance |>
  dplyr::mutate(variable = dplyr::if_else(variable == 'mean_PPT', 'precipitation', variable),
                variable = dplyr::if_else(variable == 'mean_Tmean', 'temperature', variable),
                variable = dplyr::if_else(variable == 'sd_PPT', 'precipitation var.', variable),
                variable = dplyr::if_else(variable == 'sd_Tmean', 'temperature var.', variable),
                variable = dplyr::if_else(variable == 'mean_Tmin', 'min. temperature', variable),
                variable = dplyr::if_else(variable == 'mean_Tmax', 'max. temperature', variable),
                variable = dplyr::if_else(variable == 'mean_Vpdmin', 'min. VPD', variable),
                variable = dplyr::if_else(variable == 'mean_Vpdmax', 'max. VPD', variable),
                variable = dplyr::if_else(variable == 'ba', 'basal area', variable),
                variable = dplyr::if_else(variable == 'bagt', 'basal area greater than', variable),
                variable = dplyr::if_else(variable == 'frac', 'frac. ba/species', variable),
                variable = dplyr::if_else(variable == 'total_ba', 'plot ba', variable)) |>
  dplyr::mutate(class = dplyr::if_else(variable %in% c('precipitation', 'temperature',
                                                       'precipitation var.', 'temperature var.',
                                                       'min. temperature', 'max. temperature',
                                                       'min. VPD', 'max. VPD'),
                                       'climate', NA),
                class = dplyr::if_else(variable %in% c('basal area', 'basal area greater than'),
                                       'ind. competition', class),
                class = dplyr::if_else(variable == 'frac. ba/species',
                                       'sp.-level competition', class),
                class = dplyr::if_else(variable == 'plot ba',
                                       'plot-level competition', class)) |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = factor(variable, level = level_order), y = rankIncNodePurity, color = class), show.legend = F) +
  ggplot2::facet_wrap(~site) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::scale_y_reverse(breaks = seq(0, 12, by = 2)) +
  ggplot2::xlab('')

ind_importance |>
  dplyr::mutate(variable = dplyr::if_else(variable == 'mean_PPT', 'precipitation', variable),
                variable = dplyr::if_else(variable == 'mean_Tmean', 'temperature', variable),
                variable = dplyr::if_else(variable == 'sd_PPT', 'precipitation var.', variable),
                variable = dplyr::if_else(variable == 'sd_Tmean', 'temperature var.', variable),
                variable = dplyr::if_else(variable == 'mean_Tmin', 'min. temperature', variable),
                variable = dplyr::if_else(variable == 'mean_Tmax', 'max. temperature', variable),
                variable = dplyr::if_else(variable == 'mean_Vpdmin', 'min. VPD', variable),
                variable = dplyr::if_else(variable == 'mean_Vpdmax', 'max. VPD', variable),
                variable = dplyr::if_else(variable == 'ba', 'basal area', variable),
                variable = dplyr::if_else(variable == 'bagt', 'basal area greater than', variable),
                variable = dplyr::if_else(variable == 'frac', 'frac. ba/species', variable),
                variable = dplyr::if_else(variable == 'total_ba', 'plot ba', variable)) |>
  dplyr::mutate(class = dplyr::if_else(variable %in% c('precipitation', 'temperature',
                                                       'precipitation var.', 'temperature var.',
                                                       'min. temperature', 'max. temperature',
                                                       'min. VPD', 'max. VPD'),
                                       'climate', NA),
                class = dplyr::if_else(variable %in% c('basal area', 'basal area greater than'),
                                       'ind. competition', class),
                class = dplyr::if_else(variable == 'frac. ba/species',
                                       'sp.-level competition', class),
                class = dplyr::if_else(variable == 'plot ba',
                                       'plot-level competition', class)) |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = factor(variable, level = level_order), y = rankIncMSE, color = class), show.legend = F) +
  ggplot2::facet_wrap(~site) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::scale_y_reverse(breaks = seq(0, 12, by = 2)) +
  ggplot2::xlab('')

ind_importance |>
  dplyr::mutate(variable = dplyr::if_else(variable == 'mean_PPT', 'precipitation', variable),
                variable = dplyr::if_else(variable == 'mean_Tmean', 'temperature', variable),
                variable = dplyr::if_else(variable == 'sd_PPT', 'precipitation var.', variable),
                variable = dplyr::if_else(variable == 'sd_Tmean', 'temperature var.', variable),
                variable = dplyr::if_else(variable == 'mean_Tmin', 'min. temperature', variable),
                variable = dplyr::if_else(variable == 'mean_Tmax', 'max. temperature', variable),
                variable = dplyr::if_else(variable == 'mean_Vpdmin', 'min. VPD', variable),
                variable = dplyr::if_else(variable == 'mean_Vpdmax', 'max. VPD', variable),
                variable = dplyr::if_else(variable == 'ba', 'basal area', variable),
                variable = dplyr::if_else(variable == 'bagt', 'basal area greater than', variable),
                variable = dplyr::if_else(variable == 'frac', 'frac. ba/species', variable),
                variable = dplyr::if_else(variable == 'total_ba', 'plot ba', variable)) |>
  dplyr::mutate(class = dplyr::if_else(variable %in% c('precipitation', 'temperature',
                                                       'precipitation var.', 'temperature var.',
                                                       'min. temperature', 'max. temperature',
                                                       'min. VPD', 'max. VPD'),
                                       'climate', NA),
                class = dplyr::if_else(variable %in% c('basal area', 'basal area greater than'),
                                       'ind. competition', class),
                class = dplyr::if_else(variable == 'frac. ba/species',
                                       'sp.-level competition', class),
                class = dplyr::if_else(variable == 'plot ba',
                                       'plot-level competition', class)) |>
  dplyr::group_by(site, variable, class) |>
  dplyr::summarize(mean_rank = mean(rankIncNodePurity),
                   mean_rank = 12-mean_rank) |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = factor(variable, level = level_order), y = mean_rank, fill = class), 
                    show.legend = F, stat = 'identity') +
  ggplot2::facet_wrap(~site) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  ggplot2::xlab('')

ind_importance |>
  dplyr::mutate(variable = dplyr::if_else(variable == 'mean_PPT', 'precipitation', variable),
                variable = dplyr::if_else(variable == 'mean_Tmean', 'temperature', variable),
                variable = dplyr::if_else(variable == 'sd_PPT', 'precipitation var.', variable),
                variable = dplyr::if_else(variable == 'sd_Tmean', 'temperature var.', variable),
                variable = dplyr::if_else(variable == 'mean_Tmin', 'min. temperature', variable),
                variable = dplyr::if_else(variable == 'mean_Tmax', 'max. temperature', variable),
                variable = dplyr::if_else(variable == 'mean_Vpdmin', 'min. VPD', variable),
                variable = dplyr::if_else(variable == 'mean_Vpdmax', 'max. VPD', variable),
                variable = dplyr::if_else(variable == 'ba', 'basal area', variable),
                variable = dplyr::if_else(variable == 'bagt', 'basal area greater than', variable),
                variable = dplyr::if_else(variable == 'frac', 'frac. ba/species', variable),
                variable = dplyr::if_else(variable == 'total_ba', 'plot ba', variable)) |>
  dplyr::mutate(class = dplyr::if_else(variable %in% c('precipitation', 'temperature',
                                                       'precipitation var.', 'temperature var.',
                                                       'min. temperature', 'max. temperature',
                                                       'min. VPD', 'max. VPD'),
                                       'climate', NA),
                class = dplyr::if_else(variable %in% c('basal area', 'basal area greater than'),
                                       'ind. competition', class),
                class = dplyr::if_else(variable == 'frac. ba/species',
                                       'sp.-level competition', class),
                class = dplyr::if_else(variable == 'plot ba',
                                       'plot-level competition', class)) |>
  dplyr::group_by(site, variable, class) |>
  dplyr::summarize(mean_rank = mean(rankIncMSE),
                   mean_rank = 12-mean_rank) |>
  tidyr::drop_na() |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = factor(variable, level = level_order), y = mean_rank, fill = class), 
                    show.legend = F, stat = 'identity') +
  ggplot2::facet_wrap(~site) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
  ggplot2::scale_y_continuous(breaks = seq(0, 12, by = 2)) +
  ggplot2::xlab('')

save(ind_importance, file = 'out/ind_rf_importance_save.RData')
