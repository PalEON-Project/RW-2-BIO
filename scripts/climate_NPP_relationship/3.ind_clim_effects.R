## Linear models between annual growth of each tree over time and
## climate drivers at annual scale
## Previously attempted to use monthly data and this was too much
## for a linear regression model
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

ntrees <- c(length(unique(goose_total_agbi$tree)),
            length(unique(nrp_total_agbi$tree)),
            length(unique(rooster_total_agbi$tree)),
            length(unique(sylv_total_agbi$tree)))
# Storage
coeff_save <- matrix(, nrow = sum(ntrees), ncol = 17)

row_ind <- 0
# For each site, let's iteratively fit a simple linear model with
# average temperature and precipitation as predictors of each tree's annual growth
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
      dplyr::left_join(y = total_ba, by = c('plot', 'site', 'year'))
    # Fit linear model
    # annual increment of individual tree is a function of 
    # mean annual precipitation, mean annual temperature,
    # annual precipitation seasonality, annual temperature seasonality
    # minimum annual temperature, maximum annual temperature,
    # minimum annual VPD, maximum annual VPD,
    # individual tree basal area, basal area greater than,
    # fraction of basal area for given taxon,
    # total plot basal area
    mod <- lm(formula = mean ~ mean_PPT + mean_Tmean + 
                sd_PPT + sd_Tmean +
                mean_Tmin + mean_Tmax + 
                mean_Vpdmin + mean_Vpdmax + 
                ba + bagt + frac + total_ba,
              data = joined)   
    # Save site name, tree number, coefficients, and r2 in matrix
    coeff_save[row_ind,1] <- i
    coeff_save[row_ind,2] <- unique(sub$taxon)
    coeff_save[row_ind,3] <- j
    coeff_save[row_ind,4:16] <- coefficients(mod)
    coeff_save[row_ind,17] <- summary(mod)$adj.r.squared
    print(j)
  }
  print(paste0('---------------------',i,'----------------'))
}

# Column names
colnames(coeff_save) <- c('Site', 'Taxon', 'Tree', 'Intercept',
                          'Precipitation', 'Temperature', 
                          'SD_Precipitation', 'SD_Temperature',
                          'Minimum_temperature', 'Maximum_temperature',
                          'Minimum_VPD', 'Maximum_VPD', 
                          'BA', 'BAGT', 'frac_ba', 'total_ba', 'R2')
# Format
coeff_save <- as.data.frame(coeff_save)

# Replace site numbers with names
coeff_save <- coeff_save |>
  dplyr::mutate(Site = as.character(Site)) |>
  dplyr::mutate(Site = dplyr::if_else(Site == 1, 'GOOSE', Site),
                Site = dplyr::if_else(Site == 2, 'NRP', Site),
                Site = dplyr::if_else(Site == 3, 'ROOSTER', Site),
                Site = dplyr::if_else(Site == 4, 'SYLVANIA', Site)) |>
  # Format
  dplyr::mutate(Intercept = as.numeric(Intercept),
                Precipitation = as.numeric(Precipitation),
                Temperature = as.numeric(Temperature),
                SD_Precipitation = as.numeric(SD_Precipitation),
                SD_Temperature = as.numeric(SD_Temperature),
                Minimum_temperature = as.numeric(Minimum_temperature),
                Maximum_temperature = as.numeric(Maximum_temperature),
                Minimum_VPD = as.numeric(Minimum_VPD),
                Maximum_VPD = as.numeric(Maximum_VPD),
                BA = as.numeric(BA),
                BAGT = as.numeric(BAGT),
                frac_ba = as.numeric(frac_ba),
                total_ba = as.numeric(total_ba),
                R2 = as.numeric(R2))

# Distribution of R2 for each site with individual models
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = R2)) +
  ggplot2::geom_density() +
  ggplot2::facet_wrap(~Site) +
  ggplot2::xlab(expression(R^2)) + ggplot2::ylab('Density') +
  ggplot2::theme_minimal()

# Violin plot of R2
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = R2)) +
  ggplot2::geom_violin() +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2)) +
  ggplot2::theme_minimal()

# Violin of precipitation coefficient
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = Precipitation)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for precipitation') +
  ggplot2::theme_minimal()

# Violin of temperature coefficient
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = Temperature)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for temperature') +
  ggplot2::theme_minimal()

# Violin of precipitation seasonality coefficient
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = SD_Precipitation)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for precipitation seasonality') +
  ggplot2::theme_minimal()

# Violin for temperature seasonality coefficient
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = SD_Temperature)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for temperature seasonality') +
  ggplot2::theme_minimal()

# Violin of minimum temperature coefficient
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = Minimum_temperature)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for minimum temperature') +
  ggplot2::theme_minimal()

# Violin of maximum temperature coefficient
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = Maximum_temperature)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for maximum temperature') +
  ggplot2::theme_minimal()

# Violin of minimum VPD
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = Minimum_VPD)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for minimum VPD') +
  ggplot2::theme_minimal()

# Violin of maximum VPD
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = Maximum_VPD)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for maximum VPD') +
  ggplot2::theme_minimal()

# Violin of BA
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = BA)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for tree basal area') +
  ggplot2::theme_minimal()

# Violin of BAGT
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = BAGT)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for basal area greater than') +
  ggplot2::theme_minimal()

# Violin of fraction of plot-level basal area belonging to the taxon
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = frac_ba)) +
  ggplot2::geom_violin() + 
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for fraction of basal area of each species') +
  ggplot2::theme_minimal()

# Violin of total plot level basal area
coeff_save |>
  ggplot2::ggplot(ggplot2::aes(x = Site, y = total_ba)) +
  ggplot2::geom_violin() +
  ggplot2::geom_hline(ggplot2::aes(yintercept = 0), linetype = 'dashed') +
  ggplot2::xlab('') + ggplot2::ylab('Coefficient for total basal area') +
  ggplot2::theme_minimal()

save(coeff_save, file = 'out/ind_lm_coeff_save.RData')
