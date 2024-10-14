## Detrending for sites

rm(list = ls())

# Load trended taxon-level AGBI
load('out/taxon_trended_AGBI.RData')

# Summarize over taxa and plots
site_agbi <- taxon_agbi |>
  dplyr::group_by(year, site) |>
  dplyr::summarize(site_agbi = sum(mean))

# Conduct box test for each plot over time
box_test <- site_agbi |>
  dplyr::group_by(site) |>
  dplyr::summarize(box_test = Box.test(site_agbi) |> broom::tidy())
# Number of sites demonstrating significant temporal autocorrelation
length(which(box_test$box_test$p.value < 0.05))

# Sites
site <- c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA', 'HARVARD Model RW', 'HARVARD Model RW + Census')

# Loop over each site
for(i in 1:length(site)){
  print(paste0('---------------',i,'------------------'))
  site_name <- site[i]
  
  sub <- dplyr::filter(site_agbi, site == site_name)
  current_step <- ts(sub$site_agbi, start = min(sub$year), end = max(sub$year),
                     frequency = 1)
  last_step <- lag(current_step, k = -1)
  
  combined <- as.data.frame(cbind(current_step, last_step))
  
  fit <- lm(formula = current_step ~ last_step, data = combined)
  
  temp <- cbind(residuals(fit),
                rep(site_name, times = length(residuals(fit))),
                (min(sub$year) + 1):max(sub$year))
  
  if(i == 1){
    save <- temp
  }else{
    save <- rbind(save, temp)
  }
}

save <- as.data.frame(save)
colnames(save) <- c('residual_AGBI', 'site', 'year')
save$residual_AGBI <- as.numeric(save$residual_AGBI)
save$year <- as.numeric(save$year)

site_save_comb <- site_agbi |>
  dplyr::left_join(y = save, by = c('site', 'year')) |>
  tidyr::drop_na()

# Conduct box tests for each site over time
box_test <- site_save_comb |>
  dplyr::group_by(site) |>
  dplyr::summarize(box_test = Box.test(residual_AGBI) |> broom::tidy())
# Number of sites demonstrating significant temporal autocorrelation
length(which(box_test$box_test$p.value < 0.05)) # GOOSE still p < 0.05

# Save
save(site_agbi,
     file = 'out/site_trended_AGBI.RData')
save(site_save_comb,
     file = 'out/site_detrended_AGBI.RData')
