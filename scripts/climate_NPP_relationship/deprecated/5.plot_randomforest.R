## Random forest at plot level

rm(list = ls())

# Load detrended AGBI
load('out/detrended_AGBI.RData')

# Average over individuals
save_comb <- save_comb |>
  dplyr::group_by(year, plot, site) |>
  dplyr::summarize(residual_AGBI = mean(residual_AGBI))

# Indexing for loops
site <- c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA')
plot <- c()
plot[1] <- length(unique(save_comb$plot[which(save_comb$site == 'GOOSE')]))
plot[2] <- length(unique(save_comb$plot[which(save_comb$site == 'NRP')]))
plot[3] <- length(unique(save_comb$plot[which(save_comb$site == 'ROOSTER')]))
plot[4] <- length(unique(save_comb$plot[which(save_comb$site == 'SYLVANIA')]))

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
# For each site, let's iteratively fit a simple linear model with
# average temperature and precipitation as predictors of each tree's annual growth
for(i in 1:4){
  # Plot number index, unique to each site
  plot_j <- seq(from = 1, to = plot[i], by = 1)
  # Save site name
  site_name <- site[i]
  # Loop through each plot at a given site
  for(j in plot_j){
    # Increment counter
    row_ind <- row_ind + 1
    # Save plot number
    plot_name <- plot_j[j]
    # Subset full data for one plot
    sub <- dplyr::filter(save_comb, site == site_name &
                           plot == plot_name)
    # Combine tree data with climate
    joined <- sub |>
      # Join with annual climate drivers
      dplyr::left_join(y = prism_annual, by = c('site', 'year')) |>
      # Join with plot-level competition metrics
      dplyr::left_join(y = total_ba, by = c('plot', 'site', 'year')) |>
      # ungroup so we can remove indexing collumns
      dplyr::ungroup() |>
      # removing indexing columns that we don't want to be in the random forest
      dplyr::select(-year, -plot, -site) |>
      tidyr::drop_na()
    
    # Fit random forest
    mod <- randomForest::randomForest(formula = residual_AGBI ~ ., # all predictors predicting plot level BAI
                                      data = joined, # data subset with all predictors
                                      ntree = 2000, # kinda high number of trees?
                                      importance = TRUE) # calculate importance of variables
    
    # Extract importance statistics
    imp <- randomForest::importance(mod)
    # Format
    imp <- as.data.frame(imp)
    # Add information about site, tree, variable
    imp$site <- site[i]
    imp <- tibble::rownames_to_column(imp, var = 'variable')
    # Imporatnce statistics
    imp$rankIncMSE <- rank(-imp$`%IncMSE`)
    imp$rankIncNodePurity <- rank(-imp$IncNodePurity)
    
    # Save site name, taxon name, importance statistics
    importance_save[[row_ind]] <- imp
  }
  print(paste0('-----------------------',i,'-----------------------'))
}

# Unlist
taxon_importance <- do.call(rbind, importance_save)

# Order of x-axis
level_order <- c('precipitation', 'temperature', 'precipitation var.', 'temperature var.',
                 'min. temperature', 'max. temperature', 'min. VPD', 'max. VPD',
                 'plot ba')

