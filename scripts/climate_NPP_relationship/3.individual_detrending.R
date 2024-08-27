## Detrending for individual trees
## Also removing years for out-of-sample prediction

rm(list = ls())

# Load total increment
goose_tree_agbi <- readRDS('sites/GOOSE/runs/v3.1_012021/output/AGBI_STAN_GOOSE_v3.1_012021.RDS')
nrp_tree_agbi <- readRDS('sites/NORTHROUND/runs/v3.1_082020/output/AGBI_STAN_NORTHROUND_v3.1_082020.RDS')
rooster_tree_agbi <- readRDS('sites/ROOSTER/runs/v3.1_082020/output/AGBI_STAN_ROOSTER_v3.1_082020.RDS')
sylv_tree_agbi <- readRDS('sites/SYLVANIA/runs/v3.1_082020/output/AGBI_STAN_SYLVANIA_v3.1_082020.RDS')
harv_tree_agbi <- readRDS('sites/HARVARD/runs/v3.1_102020/output/AGBI_STAN_HARVARD_v3.1_102020.RDS')

# Split harvard rw and rw + census
harv_tree_agbi_rw <- dplyr::filter(harv_tree_agbi, model == 'Model RW')
harv_tree_agbi_cen <- dplyr::filter(harv_tree_agbi, model == 'Model RW + Census')

# Remove total
rm(harv_tree_agbi)

# Subset for 1960 and beyond to reduce problem of fading record
goose_tree_agbi <- goose_tree_agbi |>
  dplyr::mutate(site = 'GOOSE') |>
  dplyr::filter(year > 1959)
nrp_tree_agbi <- nrp_tree_agbi |>
  dplyr::mutate(site = 'NRP') |>
  dplyr::filter(year > 1959)
rooster_tree_agbi <- rooster_tree_agbi |>
  dplyr::mutate(site = 'ROOSTER') |>
  dplyr::filter(year > 1959)
sylv_tree_agbi <- sylv_tree_agbi |>
  dplyr::mutate(site = 'SYLVANIA') |>
  dplyr::filter(year > 1959)
harv_tree_agbi_rw <- harv_tree_agbi_rw |>
  dplyr::mutate(site = 'HARVARD Model RW') |>
  dplyr::filter(year > 1959)
harv_tree_agbi_cen <- harv_tree_agbi_cen |>
  dplyr::mutate(site = 'HARVARD Model RW + Census') |>
  dplyr::filter(year > 1959)

# Combine sites
tree_agbi <- rbind(goose_tree_agbi, nrp_tree_agbi,
                   rooster_tree_agbi, sylv_tree_agbi,
                   harv_tree_agbi_rw, harv_tree_agbi_cen)

# Save mean over iterations in dataframe
tree_agbi <- tree_agbi |>
  dplyr::group_by(tree, year, plot, taxon, site) |>
  dplyr::summarize(mean = mean(value))

# Conduct box tests for each tree over time
box_test <- tree_agbi |>
  dplyr::group_by(tree, plot, taxon, site) |>
  dplyr::summarize(box_test = Box.test(mean) |> broom::tidy())
# Proportion of trees demonstrating significant temporal autocorrelation
length(which(box_test$box_test$p.value < 0.05)) / nrow(box_test)

site <- c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA', 'HARVARD Model RW', 'HARVARD Model RW + Census')

# Loop over each site and tree
for(i in 1:length(site)){
  print(paste0('---------------',i,'------------------'))
  tree <- unique(tree_agbi$tree[which(tree_agbi$site == site[i])])
  site_name <- site[i]
  for(j in tree){
    sub <- dplyr::filter(tree_agbi, site == site_name &
                           tree == j)
    current_step <- ts(sub$mean, start = min(sub$year), end = max(sub$year),
                       frequency = 1)
    last_step <- lag(current_step, k = -1)
    
    combined <- as.data.frame(cbind(current_step, last_step))

    if(nrow(combined) <= 2){
      temp <- cbind(0,
                    site_name,
                    j,
                   (min(sub$year)-1):max(sub$year))
    }else{
      fit <- lm(formula = current_step ~ last_step, data = combined)
      
      temp <- cbind(residuals(fit), 
                    rep(site_name, times = length(residuals(fit))),
                    rep(j, times = length(residuals(fit))),
                    (min(sub$year)+1):max(sub$year))
    }
    if(i == 1 & j == 1){
      save <- temp
    }else{
      save <- rbind(save, temp)
    }
    print(j)
  }
}

save <- as.data.frame(save)
colnames(save) <- c('residual_AGBI', 'site', 'tree', 'year')
save$residual_AGBI <- as.numeric(save$residual_AGBI)
save$tree <- as.numeric(save$tree)
save$year <- as.numeric(save$year)

save_comb <- tree_agbi |>
  dplyr::left_join(y = save, by = c('site', 'tree', 'year')) |>
  tidyr::drop_na()

# Conduct box tests for each tree over time
box_test <- save_comb |>
  dplyr::group_by(tree, plot, taxon, site) |>
  dplyr::summarize(box_test = Box.test(residual_AGBI) |> broom::tidy())
# Proportion of trees demonstrating significant temporal autocorrelation
length(which(box_test$box_test$p.value < 0.05)) / nrow(box_test)

# Set seed to ensure reproducibility
set.seed(1996)

# Remove some years for OOS prediction
unique_years <- unique(save_comb$year)
n_oos <- length(unique_years) * 0.2
oos_years <- sample(unique_years, size = n_oos,
                    replace = FALSE)
insample_years <- unique_years[!(unique_years %in% oos_years)]

# OOS
tree_agbi_oos <- dplyr::filter(tree_agbi, year %in% oos_years)
save_comb_oos <- dplyr::filter(save_comb, year %in% oos_years)
# in sample
tree_agbi <- dplyr::filter(tree_agbi, year %in% insample_years)
save_comb <- dplyr::filter(save_comb, year %in% insample_years)

# Save
save(tree_agbi, tree_agbi_oos,
     file = 'out/tree_trended_AGBI.RData')
save(save_comb, save_comb_oos,
     file = 'out/tree_detrended_AGBI.RData')
