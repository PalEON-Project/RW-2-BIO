rm(list = ls())

library(dplyr)
library(ggplot2)
library(forecast)
library(lme4)
library(performance)

# Load total biomass
goose_total_agb <- readRDS('sites/GOOSE/runs/v2.0_012021/output/AGB_STAN_GOOSE_v2.0_012021.RDS')
nrp_total_agb <- readRDS('sites/NORTHROUND/runs/v2.0_082020/output/AGB_STAN_NORTHROUND_v2.0_082020.RDS')
rooster_total_agb <- readRDS('sites/ROOSTER/runs/v2.0_082020/output/AGB_STAN_ROOSTER_v2.0_082020.RDS')
sylv_total_agb <- readRDS('sites/SYLVANIA/runs/v2.0_082020/output/AGB_STAN_SYLVANIA_v2.0_082020.RDS')

# Load total increment
goose_total_agbi <- readRDS('sites/GOOSE/runs/v2.0_012021/output/AGBI_STAN_GOOSE_v2.0_012021.RDS')
nrp_total_agbi <- readRDS('sites/NORTHROUND/runs/v2.0_082020/output/AGBI_STAN_NORTHROUND_v2.0_082020.RDS')
rooster_total_agbi <- readRDS('sites/ROOSTER/runs/v2.0_082020/output/AGBI_STAN_ROOSTER_v2.0_082020.RDS')
sylv_total_agbi <- readRDS('sites/SYLVANIA/runs/v2.0_082020/output/AGBI_STAN_SYLVANIA_v2.0_082020.RDS')

# Add site names column
goose_total_agb <- dplyr::mutate(goose_total_agb, site = 'GOOSE')
nrp_total_agb <- dplyr::mutate(nrp_total_agb, site = 'NRP')
rooster_total_agb <- dplyr::mutate(rooster_total_agb, site = 'ROOSTER')
sylv_total_agb <- dplyr::mutate(sylv_total_agb, site = 'SYLVANIA')

goose_total_agbi <- dplyr::mutate(goose_total_agbi, site = 'GOOSE')
nrp_total_agbi <- dplyr::mutate(nrp_total_agbi, site = 'NRP')
rooster_total_agbi <- dplyr::mutate(rooster_total_agbi, site = 'ROOSTER')
sylv_total_agbi <- dplyr::mutate(sylv_total_agbi, site = 'SYLVANIA')

# Combine sites
total_agb <- rbind(goose_total_agb, nrp_total_agb,
                   rooster_total_agb, sylv_total_agb)
total_agbi <- rbind(goose_total_agbi, nrp_total_agbi,
                    rooster_total_agbi, sylv_total_agbi)

# Plot NPP for each tree and site
total_agbi |>
  dplyr::group_by(tree, year, plot, taxon, site) |>
  dplyr::summarize(mean = mean(value)) |>
  ggplot(aes(x = year, y = mean, color = as.factor(tree))) +
  geom_line(show.legend = F) +
  facet_wrap(~site)

# Save mean over iterations in dataframe
total_agbi <- total_agbi |>
  dplyr::group_by(tree, year, plot, taxon, site) |>
  dplyr::summarize(mean = mean(value))

# Indexing for loops
site <- c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA')
trees <- c()
trees[1] <- max(total_agbi$tree[which(total_agbi$site == 'GOOSE')])
trees[2] <- max(total_agbi$tree[which(total_agbi$site == 'NRP')])
trees[3] <- max(total_agbi$tree[which(total_agbi$site == 'ROOSTER')])
trees[4] <- max(total_agbi$tree[which(total_agbi$site == 'SYLVANIA')])

# Load climate data
load('climate/prism_clim.RData')

# Format
prism_long <- dplyr::rename(prism_long, site = loc) |>
  dplyr::mutate(year = as.numeric(year))

# Pivot wider
prism_annual <- prism_long |>
  dplyr::group_by(year, site) |>
  summarize(mean_PPT = mean(PPT2),
            mean_Tmean = mean(Tmean2)) 
prism_month <- tidyr::pivot_wider(prism_long, names_from = 'month', values_from = c('PPT2', 'Tmean2'))

# Storage
coeff_save <- matrix(, nrow = sum(trees), ncol = 7)

row_ind <- 0
# For each site, let's iteratively fit a simple linear model with
# average temperature and precipitation as predictors of each tree's annual growth
for(i in 1:4){
  tree <- seq(from = 1, to = trees[i], by = 1)
  site_name <- site[i]
  for(j in 1:trees[i]){
    row_ind <- row_ind + 1
    tree_number <- tree[j]
    sub <- dplyr::filter(total_agbi, site == site_name &
                           tree == tree_number)
    joined <- dplyr::left_join(x = sub, y = prism_annual, by = c('site', 'year'))
    mod <- lm(formula = mean ~ mean_PPT + mean_Tmean + mean_PPT * mean_Tmean,
              data = joined)   
    coeff_save[row_ind,1] <- i
    coeff_save[row_ind,2] <- tree_number
    coeff_save[row_ind,3:6] <- coefficients(mod)
    coeff_save[row_ind,7] <- summary(mod)$adj.r.squared
    print(j)
  }
  print(paste0('---------------------',i,'----------------'))
}

colnames(coeff_save) <- c('Site', 'Tree', 'Interception',
                          'Precipitation', 'Temperature', 
                          'Interaction', 'R2')
coeff_save <- as.data.frame(coeff_save)

coeff_save <- coeff_save |>
  dplyr::mutate(Site = as.character(Site)) |>
  dplyr::mutate(Site = if_else(Site == 1, 'GOOSE', Site),
                Site = if_else(Site == 2, 'NRP', Site),
                Site = if_else(Site == 3, 'ROOSTER', Site),
                Site = if_else(Site == 4, 'SYLVANIA', Site))

coeff_save |>
  ggplot2::ggplot(aes(x = R2)) +
  ggplot2::geom_density() +
  ggplot2::facet_wrap(~Site)
