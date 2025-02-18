library(corrplot)
library(dplyr)
library(factoextra)
library(reshape2)
library(tidyr)

## Climate correlations

rm(list = ls())

#### Individual months ####

# Load climate data
load('climate/prism_clim.RData')
prism_long = prism_long[,-8 ] %>% 
  filter(year <2012)

# Split by site
#for each year for each month 
#total unfiltered data
clim_goose <- prism_long |>
  filter(loc == 'GOOSE') |>
  select(PPT2, Tmean2:Vpdmax2)
clim_nrp <- prism_long |>
  filter(loc == 'NRP') |>
  select(PPT2, Tmean2:Vpdmax2)
clim_rooster <- prism_long |>
  filter(loc == 'ROOSTER') |>
  select(PPT2, Tmean2:Vpdmax2)
clim_sylv <- prism_long |>
  filter(loc == 'SYLVANIA') |>
  select(PPT2, Tmean2:Vpdmax2)
clim_harv <- prism_long |>
  filter(loc == 'HARVARD') |>
  select(PPT2, Tmean2:Vpdmax2)
clim_hmc = prism_long |>
  filter(loc == 'HMC') |>
  select(PPT2, Tmean2:Vpdmax2)



# Correlations by site
cor_goose <- cor(clim_goose)
cor_nrp <- cor(clim_nrp)
cor_rooster <- cor(clim_rooster)
cor_sylv <- cor(clim_sylv)
cor_harv <- cor(clim_harv)
cor_hmc <- cor(clim_hmc)

# Correlation matrices
corrplot(corr = cor_goose,
                   method = 'circle',
                   type = 'upper', diag = FALSE, 
                   title = "goose_cor")
corrplot(corr = cor_nrp,
                   method = 'circle',
                   type = 'upper', diag = FALSE,
                   title = "NRP_cor")
corrplot(corr = cor_rooster,
                   method = 'circle',
                   type = 'upper', diag = FALSE,
                   title = "rooster_cor")
corrplot(corr = cor_sylv,
                   method = 'circle',
                   type = 'upper', diag = FALSE,
                   title = "sylvania_cor")
corrplot(corr = cor_harv,
                   method = 'circle',
                   type = 'upper', diag = FALSE,
                   title = "harvard_cor")
corrplot(corr = cor_hmc,
                   method = 'circle',
                   type = 'upper', diag = FALSE,
                   title = "hmc_cor")

## Can use Tmean or Tmin or Tmax or Vpdmax but not any combinations of them

#### Monthly as different variables ####

# Split by site
#########GOOSE MONTHLY###########
clim_goose_month <- prism_long |>
  filter(loc == 'GOOSE') |>
  # Pivot so each month and each variable has a different column
  pivot_wider(names_from = 'month', 
                     values_from = c('PPT2', 'Tmean2', 'Tmin2', 
                                     'Tmax2', 'Vpdmax2')) |>
  select(-year, -loc) |>
  drop_na()

goose_summer = clim_goose_month %>% 
  select(ends_with("06"), ends_with("07"), ends_with("08"))
goose_summer_time = goose_summer
goose_summer_time$year = 1:nrow(goose_summer)
goose_summer_long = melt(goose_summer_time, id.vars = "year")

goose_summer_mean = goose_summer %>% 
  rowwise() %>%
  mutate(
    PPT_mean = mean(c_across(starts_with("PPT")), na.rm = TRUE),
    Tmean_mean = mean(c_across(starts_with("Tmean")), na.rm = TRUE),
    Tmin_mean = mean(c_across(starts_with("Tmin")), na.rm = TRUE),
    Tmax_mean = mean(c_across(starts_with("Tmax")), na.rm = TRUE),
    Vpdmax_mean = mean(c_across(starts_with("Vpd")), na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  subset(select = -c(PPT2_06:Vpdmax2_08))

#monthly climate plot for goose all on one
ggplot(data = goose_summer_long)+
  geom_line(aes(x= year, y = value,colour = variable))
#mean of climate variables for summer
ggplot(data = goose_summer_mean)+
  geom_line(aes(x= year, y = value,colour = variable))

###PCA

goose_scaled_sum = scale(goose_summer_mean)

goose_summer_pca = prcomp(goose_scaled_sum)
#PCA values for PC1
goose_summer_pca$x[,'PC1']

##????
eigs <- goose_summer_pca$sdev^2
rbind(SD = sqrt(eigs),
      Proportion = eigs/sum(eigs),
      Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(goose_summer_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(goose_summer_pca)



####GOOSE WINTER#####
goose_winter = clim_goose_month %>% 
  select(ends_with("01"))
# goose_winter_jan = goose_winter
# goose_winter_jan$year = 1:nrow(goose_winter)
# goose_winter_long = melt(goose_winter_jan, id.vars = "year")

###PCA
goose_scaled_jan = scale(goose_winter)

goose_jan_pca = prcomp(goose_scaled_jan)
#PCA values for PC1
goose_jan_pca$x[,'PC1']

##????
eigs <- goose_jan_pca$sdev^2
rbind(SD = sqrt(eigs),
      Proportion = eigs/sum(eigs),
      Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(goose_jan_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(goose_jan_pca)


clim_nrp_month <- prism_long |>
  filter(loc == 'NRP') |>
  pivot_wider(names_from = 'month',
                     values_from = c('PPT2', 'Tmean2', 'Tmin2',
                                     'Tmax2', 'Vpdmax2')) |>
  select(-year, -loc) |>
  drop_na()
clim_rooster_month <- prism_long |>
  filter(loc == 'ROOSTER') |>
  pivot_wider(names_from = 'month',
                     values_from = c('PPT2', 'Tmean2', 'Tmin2',
                                     'Tmax2', 'Vpdmax2')) |>
  select(-year, -loc) |>
  drop_na()
clim_sylv_month <- prism_long |>
  filter(loc == 'SYLVANIA') |>
  pivot_wider(names_from = 'month',
                     values_from = c('PPT2', 'Tmean2', 'Tmin2',
                                     'Tmax2', 'Vpdmax2')) |>
  select(-year, -loc) |>
  drop_na()
clim_harv_month <- prism_long |>
  filter(loc == 'HARVARD') |>
  pivot_wider(names_from = 'month',
                     values_from = c('PPT2', 'Tmean2', 'Tmin2',
                                     'Tmax2', 'Vpdmax2')) |>
  select(-year, -loc) |>
  drop_na()

# Correlations by site
cor_goose_month <- cor(clim_goose_month)
cor_nrp_month <- cor(clim_nrp_month)
cor_rooster_month <- cor(clim_rooster_month)
cor_sylv_month <- cor(clim_sylv_month)
cor_harv_month <- cor(clim_harv_month)

# Correlation matrices
corrplot(corr = cor_goose_month,
                   method = 'circle',
                   type = 'upper', diag = FALSE)
corrplot(corr = cor_nrp_month,
                   method = 'circle',
                   type = 'upper', diag = FALSE)
corrplot(corr = cor_rooster_month,
                   method = 'circle',
                   type = 'upper', diag = FALSE)
corrplot(corr = cor_sylv_month,
                   method = 'circle',
                   type = 'upper', diag = FALSE)
corrplot(corr = cor_harv_month,
                   method = 'circle',
                   type = 'upper', diag = FALSE)

#####SUMMER PCA############
#scaling the dataset
goose_scaled_sum = scale(goose_summer)

goose_summer_pca = prcomp(goose_scaled_sum)
#PCA values for PC1
goose_summer_pca$x[,'PC1']

##????
eigs <- goose_summer_pca$sdev^2
rbind(SD = sqrt(eigs),
      Proportion = eigs/sum(eigs),
      Cumulative = cumsum(eigs)/sum(eigs))


#shows variables per component
fviz_eig(goose_summer_pca, addlabels = TRUE)


#biplot
fviz_pca_biplot(goose_summer_pca)

#visualize principal component analysis
fviz_pca_var(clim_goose_pca, col.var = "black")
fviz_cos2(clim_goose_pca, choice = "var", axes = 1:2)
clim_goose_pca_time = data.frame(year=seq(1, nrow(clim_goose)), clim_goose_scaled %*% clim_goose_pca$rotation)

ggplot(data=clim_goose_pca_time) +
  geom_line(aes(x=year, y=PC1))

ggplot(data = goose_long, aes(x= year, y = value))+
  geom_line()+
  facet_wrap(~variable, scales = "free_y")

pca_time_melt = melt(clim_goose_pca_time, id.vars='year')
pca_time_melt$type = rep('goose')


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
  group_by(year, loc) |>
  # Average over months
  summarize(PPT = mean(PPT2),
                   Tmean = mean(Tmean2),
                   sd_PPT = sd(PPT2),
                   sd_Tmean = sd(Tmean2),
                   Tmin = min(Tmin2),
                   Tmax = max(Tmax2),
                   # Vpdmin = min(Vpdmin2),
                   Vpdmax = max(Vpdmax2)) 


# Split by site
clim_goose_1 <- prism_annual |>
  ungroup() |>
  filter(loc == 'GOOSE') |>
  select(PPT:Vpdmax)

clim_goose_year = clim_goose_1
clim_goose_year$year = 1:nrow(clim_goose_year)

goose_long = melt(clim_goose_year, id.vars = "year")

#annual climate plot
ggplot()+
  geom_line(data = goose_long, aes(x= year, y = value, colour = variable))
#clim_goose_scaled = scale(goose_long)

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
clim_hmc = prism_annual %>% 
  ungroup() %>% 
  filter(loc == "HMC") %>% 
  select(PPT:Vpdmax)

# Correlations by site
cor_goose <- cor(clim_goose)
cor_nrp <- cor(clim_nrp)
cor_rooster <- cor(clim_rooster)
cor_sylv <- cor(clim_sylv)
cor_harv <- cor(clim_harv)
cor_hmc = cor(clim_hmc)

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
corrplot(cor = cor_hmc, method = 'circle', type = 'upper',
         diag = FALSE, title = 'hmc_cor')

## # CENTROID
library(factoextra)

####GOOSE PCA################
#scaling the dataset
clim_goose_scaled = scale(clim_goose)

clim_goose_pca = prcomp(clim_goose_scaled)
#PCA values for PC1
clim_goose_pca$x[,'PC1']

##????
eigs <- clim_goose_pca$sdev^2
rbind(SD = sqrt(eigs),
      Proportion = eigs/sum(eigs),
      Cumulative = cumsum(eigs)/sum(eigs))


#shows variables per component
fviz_eig(clim_goose_pca, addlabels = TRUE)


#biplot
fviz_pca_biplot(clim_goose_pca)

#visualize principal component analysis
fviz_pca_var(clim_goose_pca, col.var = "black")
fviz_cos2(clim_goose_pca, choice = "var", axes = 1:2)
clim_goose_pca_time = data.frame(year=seq(1, nrow(clim_goose)), clim_goose_scaled %*% clim_goose_pca$rotation)

ggplot(data=clim_goose_pca_time) +
  geom_line(aes(x=year, y=PC1))

ggplot(data = goose_long, aes(x= year, y = value))+
  geom_line()+
  facet_wrap(~variable, scales = "free_y")

pca_time_melt = melt(clim_goose_pca_time, id.vars='year')
pca_time_melt$type = rep('goose')

####ROOSTER PCA#############

clim_rooster_pca = prcomp(clim_rooster,scale. = TRUE)
summary(clim_rooster_pca)
clim_rooster_pca$x[,'PC1']

##????
eigs <- clim_rooster_pca$sdev^2
rbind(SD = sqrt(eigs),
      Proportion = eigs/sum(eigs),
      Cumulative = cumsum(eigs)/sum(eigs))


#shows variables per component
fviz_eig(clim_rooster_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(clim_rooster_pca)

#visualize principal component analysis
fviz_pca_var(clim_rooster_pca, col.var = "black")
fviz_cos2(clim_rooster_pca, choice = "var", axes = 1:2)
#clim_goose_pca_time = data.frame(year=seq(1, nrow(clim_rooster)), clim_goose_scaled %*% clim_goose_pca$rotation)

#ggplot(data=clim_rooster_pca_time) +
 # geom_line(aes(x=year, y=PC1))

# pca_time_melt = melt(clim_goose_pca_time, id.vars='year')
# pca_time_melt$type = rep('goose')

####HARVARD PCA#####
clim_harvard_pca = prcomp(clim_harv,scale. = TRUE)
summary(clim_harvard_pca)
clim_harvard_pca$x[,'PC1']

##????
eigs <- clim_harvard_pca$sdev^2
rbind(SD = sqrt(eigs),
      Proportion = eigs/sum(eigs),
      Cumulative = cumsum(eigs)/sum(eigs))


#shows variables per component
fviz_eig(clim_harvard_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(clim_harvard_pca)

#visualize principal component analysis
fviz_pca_var(clim_harvard_pca, col.var = "black")
fviz_cos2(clim_harvard_pca, choice = "var", axes = 1:2)
#clim_harvard_pca_time = data.frame(year=seq(1, nrow(clim_rooster)), clim_goose_scaled %*% clim_goose_pca$rotation)

####NRP PCA####
clim_nrp_pca = prcomp(clim_nrp,scale. = TRUE)
summary(clim_nrp_pca)
clim_nrp_pca$x[,'PC1']

##????
eigs <- clim_nrp_pca$sdev^2
rbind(SD = sqrt(eigs),
      Proportion = eigs/sum(eigs),
      Cumulative = cumsum(eigs)/sum(eigs))


#shows variables per component
fviz_eig(clim_nrp_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(clim_nrp_pca)

#visualize principal component analysis
fviz_pca_var(clim_nrp_pca, col.var = "black")
fviz_cos2(clim_nrp_pca, choice = "var", axes = 1:2)


####SYLVANIA PCA#########
clim_sylv_pca = prcomp(clim_sylv,scale. = TRUE)
summary(clim_sylv_pca)
clim_sylv_pca$x[,'PC1']

##????
eigs <- clim_sylv_pca$sdev^2
rbind(SD = sqrt(eigs),
      Proportion = eigs/sum(eigs),
      Cumulative = cumsum(eigs)/sum(eigs))


#shows variables per component
fviz_eig(clim_sylv_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(clim_sylv_pca)

#visualize principal component analysis
fviz_pca_var(clim_sylv_pca, col.var = "black")
fviz_cos2(clim_sylv_pca, choice = "var", axes = 1:2)
#


####HMC PCA####
clim_hmc_pca = prcomp(clim_hmc,scale. = TRUE)
summary(clim_hmc_pca)
clim_hmc_pca$x[,'PC1']

##????
eigs <- clim_hmc_pca$sdev^2
rbind(SD = sqrt(eigs),
      Proportion = eigs/sum(eigs),
      Cumulative = cumsum(eigs)/sum(eigs))


#shows variables per component
fviz_eig(clim_hmc_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(clim_hmc_pca)

#visualize principal component analysis
fviz_pca_var(clim_hmc_pca, col.var = "black")
fviz_cos2(clim_hmc_pca, choice = "var", axes = 1:2)
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