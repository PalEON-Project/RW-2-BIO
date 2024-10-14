## Detrending for plots

rm(list = ls())

# Load trended taxon-level AGBI
load('out/taxon_trended_AGBI.RData')

# Summarize over taxa
plot_agbi <- taxon_agbi |>
  dplyr::group_by(year, plot, site) |>
  dplyr::summarize(plot_agbi = sum(mean))

# Conduct box tests for each plot over time
box_test <- plot_agbi |>
  dplyr::group_by(plot, site) |>
  dplyr::summarize(box_test = Box.test(plot_agbi) |> broom::tidy())
# Proportion of plots demonstrating significant temporal autocorrelation
length(which(box_test$box_test$p.value < 0.05)) / nrow(box_test)

# Sites
site <- c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA', 'HARVARD Model RW', 'HARVARD Model RW + Census')

# Loop over each site and plot
for(i in 1:length(site)){
  print(paste0('---------------',i,'------------------'))
  plot <- unique(plot_agbi$plot[which(plot_agbi$site == site[i])])
  site_name <- site[i]
  tt <- 0
  for(j in plot){
    tt <- tt + 1
    sub <- dplyr::filter(plot_agbi, site == site_name &
                           plot == j)
    current_step <- ts(sub$plot_agbi, start = min(sub$year), end = max(sub$year),
                       frequency = 1)
    last_step <- lag(current_step, k = -1)
    
    combined <- as.data.frame(cbind(current_step, last_step))
    
    if(nrow(combined) <= 2){
      temp <- cbind(0,
                    site_name,
                    j,
                    (min(sub$year) - 1):max(sub$year))
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
colnames(save) <- c('residual_AGBI', 'site', 'plot', 'year')
save$residual_AGBI <- as.numeric(save$residual_AGBI)
save$plot <- as.numeric(save$plot)
save$year <- as.numeric(save$year)

plot_save_comb <- plot_agbi |>
  dplyr::left_join(y = save, by = c('site', 'plot', 'year')) |>
  tidyr::drop_na()

# Conduct box tests for each plot over time
box_test <- plot_save_comb |>
  dplyr::group_by(plot, site) |>
  dplyr::summarize(box_test = Box.test(residual_AGBI) |> broom::tidy())
# Proportion of taxa demonstrating significant temporal autocorrelation
length(which(box_test$box_test$p.value < 0.05)) / nrow(box_test)

# Save
save(plot_agbi,
     file = 'out/plot_trended_AGBI.RData')
save(plot_save_comb,
     file = 'out/plot_detrended_AGBI.RData')
