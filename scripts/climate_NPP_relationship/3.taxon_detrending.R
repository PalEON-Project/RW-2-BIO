## Detrending for taxa

rm(list = ls())

# Load taxon-level increments
goose_taxon_agbi <- readRDS('sites/GOOSE/runs/v3.1_012021/output/AGBI_TAXA_STAN_GOOSE_v3.1_012021.RDS')
nrp_taxon_agbi <- readRDS('sites/NORTHROUND/runs/v3.1_082020/output/AGBI_TAXA_STAN_NORTHROUND_v3.1_082020.RDS')
rooster_taxon_agbi <- readRDS('sites/ROOSTER/runs/v3.1_082020/output/AGBI_TAXA_STAN_ROOSTER_v3.1_082020.RDS')
sylv_taxon_agbi <- readRDS('sites/SYLVANIA/runs/v3.1_082020/output/AGBI_TAXA_STAN_SYLVANIA_v3.1_082020.RDS')
harv_taxon_agbi <- readRDS('sites/HARVARD/runs/v3.1_102020/output/AGBI_TAXA_STAN_HARVARD_v3.1_102020.RDS')

# Split harvard rw and rw + census
harv_taxon_agbi_rw <- dplyr::filter(harv_taxon_agbi, model == 'Model RW')
harv_taxon_agbi_cen <- dplyr::filter(harv_taxon_agbi, model == 'Model RW + Census')

# Remove total
rm(harv_taxon_agbi)

# Subset for 1960 and beyond to reduce problem of fading record
goose_taxon_agbi <- goose_taxon_agbi |>
  dplyr::mutate(site = 'GOOSE') |>
  dplyr::filter(year > 1959)
nrp_taxon_agbi <- nrp_taxon_agbi |>
  dplyr::mutate(site = 'NRP') |>
  dplyr::filter(year > 1959)
rooster_taxon_agbi <- rooster_taxon_agbi |>
  dplyr::mutate(site = 'ROOSTER') |>
  dplyr::filter(year > 1959)
sylv_taxon_agbi <- sylv_taxon_agbi |>
  dplyr::mutate(site = 'SYLVANIA') |>
  dplyr::filter(year > 1959)
harv_taxon_agbi_rw <- harv_taxon_agbi_rw |>
  dplyr::mutate(site = 'HARVARD Model RW') |>
  dplyr::filter(year > 1959)
harv_taxon_agbi_cen <- harv_taxon_agbi_cen |>
  dplyr::mutate(site = 'HARVARD Model RW + Census') |>
  dplyr::filter(year > 1959)

# Combine sites
taxon_agbi <- rbind(goose_taxon_agbi, nrp_taxon_agbi,
                    rooster_taxon_agbi, sylv_taxon_agbi,
                    harv_taxon_agbi_rw, harv_taxon_agbi_cen)

# Save mean over iterations in dataframe
taxon_agbi <- taxon_agbi |>
  dplyr::group_by(year, plot, taxon, site) |>
  dplyr::summarize(mean = mean(abi))

# Conduct box tests for each taxon over time
box_test <- taxon_agbi |>
  dplyr::group_by(plot, taxon, site) |>
  dplyr::summarize(box_test = Box.test(mean) |> broom::tidy())
# Proportion of taxa demonstrating significant temporal autocorrelation
length(which(box_test$box_test$p.value < 0.05)) / nrow(box_test)

site <- c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA', 'HARVARD Model RW', 'HARVARD Model RW + Census')

# Loop over each site and taxon
for(i in 1:length(site)){
  print(paste0('---------------',i,'------------------'))
  taxon <- unique(taxon_agbi$taxon[which(taxon_agbi$site == site[i])])
  site_name <- site[i]
  tt <- 0
  for(j in taxon){
    tt <- tt + 1
    sub <- dplyr::filter(taxon_agbi, site == site_name &
                           taxon == j)
    current_step <- ts(sub$mean, start = min(sub$year), end = max(sub$year),
                       frequency = 1)
    last_step <- lag(current_step, k = -1)
    
    combined <- as.data.frame(cbind(current_step, last_step))
    
    if(nrow(combined) <= 2){
      temp <- cbind(0,
                    site_name,
                    j,
                    (min(sub$year)-1):max(sub$year))
      print('Assumed zero')
      
    }else{
      fit <- lm(formula = current_step ~ last_step, data = combined)
      
      temp <- cbind(residuals(fit),
                    rep(site_name, times = length(residuals(fit))),
                    rep(j, times = length(residuals(fit))),
                    (min(sub$year)+1):max(sub$year))
    }
    if(i == 1 & tt == 1){
      save <- temp
    }else{
      save <- rbind(save, temp)
    }
    print(j)
  }
}

save <- as.data.frame(save)
colnames(save) <- c('residual_AGBI', 'site', 'taxon', 'year')
save$residual_AGBI <- as.numeric(save$residual_AGBI)
save$taxon <- as.character(save$taxon)
save$year <- as.numeric(save$year)

taxon_save_comb <- taxon_agbi |>
  dplyr::left_join(y = save, by = c('site', 'taxon', 'year')) |>
  tidyr::drop_na()

# Conduct box tests for each taxon over time
box_test <- taxon_save_comb |>
  dplyr::group_by(taxon, plot, site) |>
  dplyr::summarize(box_test = Box.test(residual_AGBI) |> broom::tidy())
# Proportion of taxa demonstrating significant temporal autocorrelation
length(which(box_test$box_test$p.value < 0.05)) / nrow(box_test)

# Save
save(taxon_agbi,
     file = 'out/taxon_trended_AGBI.RData')
save(taxon_save_comb,
     file = 'out/taxon_detrended_AGBI.RData')
