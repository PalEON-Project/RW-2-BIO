## Climate correlations

rm(list = ls())

#### Individual months ####

# Load climate data
load('climate/prism_clim.RData')
prism_long = prism_long[,-8 ]

# Split by site
clim_goose <- prism_long |>
  dplyr::filter(loc == 'GOOSE') |>
  dplyr::select(PPT2, Tmean2:Vpdmax2)
clim_nrp <- prism_long |>
  dplyr::filter(loc == 'NRP') |>
  dplyr::select(PPT2, Tmean2:Vpdmax2)
clim_rooster <- prism_long |>
  dplyr::filter(loc == 'ROOSTER') |>
  dplyr::select(PPT2, Tmean2:Vpdmax2)
clim_sylv <- prism_long |>
  dplyr::filter(loc == 'SYLVANIA') |>
  dplyr::select(PPT2, Tmean2:Vpdmax2)
clim_harv <- prism_long |>
  dplyr::filter(loc == 'HARVARD') |>
  dplyr::select(PPT2, Tmean2:Vpdmax2)



# Correlations by site
cor_goose <- cor(clim_goose)
cor_nrp <- cor(clim_nrp)
cor_rooster <- cor(clim_rooster)
cor_sylv <- cor(clim_sylv)
cor_harv <- cor(clim_harv)

# Correlation matrices
corrplot::corrplot(corr = cor_goose,
                   method = 'circle',
                   type = 'upper', diag = FALSE)
corrplot::corrplot(corr = cor_nrp,
                   method = 'circle',
                   type = 'upper', diag = FALSE)
corrplot::corrplot(corr = cor_rooster,
                   method = 'circle',
                   type = 'upper', diag = FALSE)
corrplot::corrplot(corr = cor_sylv,
                   method = 'circle',
                   type = 'upper', diag = FALSE)
corrplot::corrplot(corr = cor_harv,
                   method = 'circle',
                   type = 'upper', diag = FALSE)

## Can use Tmean or Tmin or Tmax or Vpdmax but not any combinations of them

#### Monthly as different variables ####

# Split by site
clim_goose <- prism_long |>
  dplyr::filter(loc == 'GOOSE') |>
  # Pivot so each month and each variable has a different column
  tidyr::pivot_wider(names_from = 'month', 
                     values_from = c('PPT2', 'Tmean2', 'Tmin2', 
                                     'Tmax2', 'Vpdmax2')) |>
  dplyr::select(-year, -loc) |>
  tidyr::drop_na()
clim_nrp <- prism_long |>
  dplyr::filter(loc == 'NRP') |>
  tidyr::pivot_wider(names_from = 'month',
                     values_from = c('PPT2', 'Tmean2', 'Tmin2',
                                     'Tmax2', 'Vpdmax2')) |>
  dplyr::select(-year, -loc) |>
  tidyr::drop_na()
clim_rooster <- prism_long |>
  dplyr::filter(loc == 'ROOSTER') |>
  tidyr::pivot_wider(names_from = 'month',
                     values_from = c('PPT2', 'Tmean2', 'Tmin2',
                                     'Tmax2', 'Vpdmax2')) |>
  dplyr::select(-year, -loc) |>
  tidyr::drop_na()
clim_sylv <- prism_long |>
  dplyr::filter(loc == 'SYLVANIA') |>
  tidyr::pivot_wider(names_from = 'month',
                     values_from = c('PPT2', 'Tmean2', 'Tmin2',
                                     'Tmax2', 'Vpdmax2')) |>
  dplyr::select(-year, -loc) |>
  tidyr::drop_na()
clim_harv <- prism_long |>
  dplyr::filter(loc == 'HARVARD') |>
  tidyr::pivot_wider(names_from = 'month',
                     values_from = c('PPT2', 'Tmean2', 'Tmin2',
                                     'Tmax2', 'Vpdmax2')) |>
  dplyr::select(-year, -loc) |>
  tidyr::drop_na()

# Correlations by site
cor_goose <- cor(clim_goose)
cor_nrp <- cor(clim_nrp)
cor_rooster <- cor(clim_rooster)
cor_sylv <- cor(clim_sylv)
cor_harv <- cor(clim_harv)

# Correlation matrices
corrplot::corrplot(corr = cor_goose,
                   method = 'circle',
                   type = 'upper', diag = FALSE)
corrplot::corrplot(corr = cor_nrp,
                   method = 'circle',
                   type = 'upper', diag = FALSE)
corrplot::corrplot(corr = cor_rooster,
                   method = 'circle',
                   type = 'upper', diag = FALSE)
corrplot::corrplot(corr = cor_sylv,
                   method = 'circle',
                   type = 'upper', diag = FALSE)
corrplot::corrplot(corr = cor_harv,
                   method = 'circle',
                   type = 'upper', diag = FALSE)

## Correlated with each other:
## tmin and tmean of the same month across all sites
## tmax and tmean of the same month across all sites
## tmin and tmax of the same month across all sites
## vpdmax and ppt of the same month during growing season across all sites except Sylvania
## vpdmax and tmean of the same month across all sites
## vpdmax and tmin of the same month during winter (northeast)/most of the year (Sylvnania)
## vpdmax and tmax of the same month across all sites
## vpdmin with itself across most months (northeast)/during winter (Sylvania)

#### Annual summaries ####

# Pivot wider
prism_annual <- prism_long |>
  dplyr::group_by(year, loc) |>
  # Average over months
  dplyr::summarize(PPT = mean(PPT2),
                   Tmean = mean(Tmean2),
                   sd_PPT = sd(PPT2),
                   sd_Tmean = sd(Tmean2),
                   Tmin = min(Tmin2),
                   Tmax = max(Tmax2),
                   # Vpdmin = min(Vpdmin2),
                   Vpdmax = max(Vpdmax2)) 

# Split by site
clim_goose <- prism_annual |>
  dplyr::ungroup() |>
  dplyr::filter(loc == 'GOOSE') |>
  dplyr::select(PPT:Vpdmax)
clim_nrp <- prism_annual |>
  dplyr::ungroup() |>
  dplyr::filter(loc == 'NRP') |>
  dplyr::select(PPT:Vpdmax)
clim_rooster <- prism_annual |>
  dplyr::ungroup() |>
  dplyr::filter(loc == 'ROOSTER') |>
  dplyr::select(PPT:Vpdmax)
clim_sylv <- prism_annual |>
  dplyr::ungroup() |>
  dplyr::filter(loc == 'SYLVANIA') |>
  dplyr::select(PPT:Vpdmax)
clim_harv <- prism_annual |>
  dplyr::ungroup() |>
  dplyr::filter(loc == 'HARVARD') |>
  dplyr::select(PPT:Vpdmax)

# Correlations by site
cor_goose <- cor(clim_goose)
cor_nrp <- cor(clim_nrp)
cor_rooster <- cor(clim_rooster)
cor_sylv <- cor(clim_sylv)
cor_harv <- cor(clim_harv)

# Correlation matrices
corrplot::corrplot(cor = cor_goose,
                   method = 'circle',
                   type = 'upper', diag = FALSE)
corrplot::corrplot(cor = cor_nrp,
                   method = 'circle',
                   type = 'upper', diag = FALSE)
corrplot::corrplot(cor = cor_rooster,
                   method = 'circle',
                   type = 'upper', diag = FALSE)
corrplot::corrplot(cor = cor_sylv,
                   method = 'circle',
                   type = 'upper', diag = FALSE)
corrplot::corrplot(cor = cor_harv,
                   method = 'circle',
                   type = 'upper', diag = FALSE)

## # CENTROID
library(factoextra)

clim_goose_scaled = scale(clim_goose)

clim_goose_pca = prcomp(clim_goose_scaled)
clim_goose_pca$x[,'PC1']

# foo = princomp(BVC_normalized)

# 
# BVN_pca$loadings[,1:3]
# BVN_pca$scores

eigs <- clim_goose_pca$sdev^2
rbind(SD = sqrt(eigs),
      Proportion = eigs/sum(eigs),
      Cumulative = cumsum(eigs)/sum(eigs))

# png('figures/BVC_scree_plot.png')#, width = 14, height=8)
fviz_eig(clim_goose_pca, addlabels = TRUE)
# dev.off()

fviz_pca_var(clim_goose_pca, col.var = "black")
fviz_cos2(clim_goose_pca, choice = "var", axes = 1:2)
clim_goose_pca_time = data.frame(year=seq(1, nrow(clim_goose)), clim_goose_scaled %*% clim_goose_pca$rotation)

ggplot(data=clim_goose_pca_time) +
  geom_line(aes(x=year, y=PC1))

pca_time_melt = melt(clim_goose_pca_time, id.vars='year')
pca_time_melt$type = rep('goose')



## Can use mean_PPT, mean_Tmean, sd_Tmean, mean_Vpdmax I think

#### Growing year ####

## Instead of calendar year

# Pivot wider
prism_growing <- prism_long |> 
  dplyr::mutate(year = as.numeric(year)) |>
  dplyr::mutate(growing_year = dplyr::if_else(month %in% c('01', '02', '03', '04', 
                                                           '05', '06', '07', '08'),
                                              year, year + 1)) |>
  dplyr::group_by(growing_year, loc) |>
  dplyr::summarize(PPT = mean(PPT2),
                   Tmean = mean(Tmean2),
                   sd_PPT = sd(PPT2),
                   sd_Tmean = sd(Tmean2),
                   Tmin = min(Tmin2),
                   Tmax = max(Tmax2),
                   Vpdmin = min(Vpdmin2),
                   Vpdmax = max(Vpdmax2))

# Split by site
clim_goose <- prism_growing |>
  dplyr::ungroup() |>
  dplyr::filter(loc == 'GOOSE') |>
  dplyr::select(PPT:Vpdmax)
clim_nrp <- prism_growing |>
  dplyr::ungroup() |>
  dplyr::filter(loc == 'NRP') |>
  dplyr::select(PPT:Vpdmax)
clim_rooster <- prism_growing |>
  dplyr::ungroup() |>
  dplyr::filter(loc == 'ROOSTER') |>
  dplyr::select(PPT:Vpdmax)
clim_sylv <- prism_growing |>
  dplyr::ungroup() |>
  dplyr::filter(loc == 'SYLVANIA') |>
  dplyr::select(PPT:Vpdmax)
clim_harv <- prism_growing |>
  dplyr::ungroup() |>
  dplyr::filter(loc == 'HARVARD') |>
  dplyr::select(PPT:Vpdmax)

# Correlations by site
cor_goose <- cor(clim_goose)
cor_nrp <- cor(clim_nrp)
cor_rooster <- cor(clim_rooster)
cor_sylv <- cor(clim_sylv)
cor_harv <- cor(clim_harv)

# Correlation matrices
corrplot::corrplot(corr = cor_goose,
                   method = 'circle',
                   type = 'upper', diag = FALSE)
corrplot::corrplot(corr = cor_nrp,
                   method = 'circle',
                   type = 'upper', diag = FALSE)
corrplot::corrplot(corr = cor_rooster,
                   method = 'circle',
                   type = 'upper', diag = FALSE)
corrplot::corrplot(corr = cor_sylv,
                   method = 'circle',
                   type = 'upper', diag = FALSE)
corrplot::corrplot(corr = cor_harv,
                   method = 'circle',
                   type = 'upper', diag = FALSE)

## Can still use Tmean, PPT, sd_Tmean, Vpdmax