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



# # Correlations by site
# cor_goose <- cor(clim_goose)
# cor_nrp <- cor(clim_nrp)
# cor_rooster <- cor(clim_rooster)
# cor_sylv <- cor(clim_sylv)
# cor_harv <- cor(clim_harv)
# cor_hmc <- cor(clim_hmc)
# 
# # Correlation matrices
# corrplot(corr = cor_goose,
#                    method = 'circle',
#                    type = 'upper', diag = FALSE, 
#                    title = "goose_cor")
# corrplot(corr = cor_nrp,
#                    method = 'circle',
#                    type = 'upper', diag = FALSE,
#                    title = "NRP_cor")
# corrplot(corr = cor_rooster,
#                    method = 'circle',
#                    type = 'upper', diag = FALSE,
#                    title = "rooster_cor")
# corrplot(corr = cor_sylv,
#                    method = 'circle',
#                    type = 'upper', diag = FALSE,
#                    title = "sylvania_cor")
# corrplot(corr = cor_harv,
#                    method = 'circle',
#                    type = 'upper', diag = FALSE,
#                    title = "harvard_cor")
# corrplot(corr = cor_hmc,
#                    method = 'circle',
#                    type = 'upper', diag = FALSE,
#                    title = "hmc_cor")

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
  select(-loc) |>
  drop_na()

goose_summer = clim_goose_month %>% 
  select(starts_with("year"),ends_with("06"), ends_with("07"), ends_with("08"))
# goose_summer_time = goose_summer
# goose_summer_time$year = 1:nrow(goose_summer)
goose_summer_long = melt(goose_summer, id.vars = "year")

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
  subset(select = -c(PPT2_06:Vpdmax2_08)) %>% 
  slice(-1)

#monthly climate plot for goose all on one
# ggplot(data = goose_summer_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for summer
# ggplot(data = goose_summer_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA SUMMER GOOSE ####

goose_scaled_sum = scale(goose_summer_mean[,2:6])

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
  select(starts_with("year"),ends_with("01"), ends_with("02"), ends_with("12"))

goose_winter_reorg = goose_winter %>% 
  mutate(
    winter_prev_PPT = PPT2_12,
    winter_current_PPT = rowMeans(select(., starts_with("PPT2_01"), starts_with("PPT2_02")), na.rm = TRUE),
    winter_prev_Tmean = Tmean2_12,
    winter_current_Tmean = rowMeans(select(., starts_with("Tmean2_01"), starts_with("Tmean2_02")), na.rm = TRUE),
    winter_prev_Tmin = Tmin2_12,
    winter_current_Tmin = rowMeans(select(., starts_with("Tmin2_01"), starts_with("Tmin2_02")), na.rm = TRUE),
    winter_prev_Tmax = Tmax2_12,
    winter_current_Tmax = rowMeans(select(., starts_with("Tmax2_01"), starts_with("Tmax2_02")), na.rm = TRUE),
    winter_prev_Vpd = Vpdmax2_12,
    winter_current_Vpd = rowMeans(select(., starts_with("Vpdmax2_01"), starts_with("Vpdmax2_02")), na.rm = TRUE)
  ) %>% 
  subset(select = -c(PPT2_01:Vpdmax2_12))

goose_winter_reorg$mean_winter_PPT = NA
goose_winter_reorg$mean_winter_Tmean = NA
goose_winter_reorg$mean_winter_Tmin = NA
goose_winter_reorg$mean_winter_Tmax = NA
goose_winter_reorg$mean_winter_Vpd = NA

# Calculate the mean for the previous year with the current year for each variable
N_years = nrow(goose_winter_reorg)
goose_winter_reorg$mean_winter_PPT[2:N_years] = rowMeans(cbind(goose_winter_reorg$winter_prev_PPT[1:(N_years-1)], 
                                                               goose_winter_reorg$winter_current_PPT[2:N_years]), na.rm = TRUE)
goose_winter_reorg$mean_winter_Tmean[2:N_years] = rowMeans(cbind(goose_winter_reorg$winter_prev_Tmean[1:(N_years-1)], 
                                                                 goose_winter_reorg$winter_current_Tmean[2:N_years]), na.rm = TRUE)
goose_winter_reorg$mean_winter_Tmin[2:N_years] = rowMeans(cbind(goose_winter_reorg$winter_prev_Tmin[1:(N_years-1)], 
                                                                goose_winter_reorg$winter_current_Tmin[2:N_years]), na.rm = TRUE)
goose_winter_reorg$mean_winter_Tmax[2:N_years] = rowMeans(cbind(goose_winter_reorg$winter_prev_Tmax[1:(N_years-1)], 
                                                                goose_winter_reorg$winter_current_Tmax[2:N_years]), na.rm = TRUE)
goose_winter_reorg$mean_winter_Vpd[2:N_years] = rowMeans(cbind(goose_winter_reorg$winter_prev_Vpd[1:(N_years-1)], 
                                                                goose_winter_reorg$winter_current_Vpd[2:N_years]), na.rm = TRUE) 

goose_winter_mean = goose_winter_reorg %>% 
  select(-c(winter_prev_PPT:winter_current_Vpd)) %>% 
  slice(-1) 
  


###PCA WINTER GOOSE
goose_scaled_winter = scale(goose_winter_mean[,2:6])

goose_winter_pca = prcomp(goose_scaled_winter)
#PCA values for PC1
goose_winter_pca$x[,'PC1']

##????
eigs <- goose_winter_pca$sdev^2
rbind(SD = sqrt(eigs),
      Proportion = eigs/sum(eigs),
      Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(goose_winter_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(goose_winter_pca)

goose_PCA = data.frame(year=goose_summer_mean$year, site = "GOOSE",
                       PCA1_summer = goose_summer_pca$x[,'PC1'],
                       PCA2_summer = goose_summer_pca$x[,'PC2'],
                       PCA1_winter = goose_winter_pca$x[,'PC1'],
                       PCA2_winter = goose_winter_pca$x[,'PC2'])

goose_PCA$year = as.numeric(goose_PCA$year)
saveRDS(goose_PCA, "goose_PCA.RDS")


############NRP ###########################################
clim_nrp_month <- prism_long |>
  filter(loc == 'NRP') |>
  pivot_wider(names_from = 'month',
                     values_from = c('PPT2', 'Tmean2', 'Tmin2',
                                     'Tmax2', 'Vpdmax2')) |>
  select(-loc) |>
  drop_na()

nrp_summer = clim_nrp_month %>% 
  select(starts_with("year"),ends_with("06"), ends_with("07"), ends_with("08"))
# nrp_summer_time = nrp_summer
# nrp_summer_time$year = 1:nrow(nrp_summer)
nrp_summer_long = melt(nrp_summer, id.vars = "year")

nrp_summer_mean = nrp_summer %>% 
  rowwise() %>%
  mutate(
    PPT_mean = mean(c_across(starts_with("PPT")), na.rm = TRUE),
    Tmean_mean = mean(c_across(starts_with("Tmean")), na.rm = TRUE),
    Tmin_mean = mean(c_across(starts_with("Tmin")), na.rm = TRUE),
    Tmax_mean = mean(c_across(starts_with("Tmax")), na.rm = TRUE),
    Vpdmax_mean = mean(c_across(starts_with("Vpd")), na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  subset(select = -c(PPT2_06:Vpdmax2_08)) %>% 
  slice(-1)

#monthly climate plot for nrp all on one
# ggplot(data = nrp_summer_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for summer
# ggplot(data = nrp_summer_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA SUMMER NRP ####

nrp_scaled_sum = scale(nrp_summer_mean[,2:6])

nrp_summer_pca = prcomp(nrp_scaled_sum)
#PCA values for PC1
nrp_summer_pca$x[,'PC1']


##????
eigs <- nrp_summer_pca$sdev^2
rbind(SD = sqrt(eigs),
      Proportion = eigs/sum(eigs),
      Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(nrp_summer_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(nrp_summer_pca)

####NRP WINTER#####
nrp_winter = clim_nrp_month %>% 
  select(starts_with("year"),ends_with("01"), ends_with("02"), ends_with("12"))

nrp_winter_reorg = nrp_winter %>% 
  mutate(
    winter_prev_PPT = PPT2_12,
    winter_current_PPT = rowMeans(select(., starts_with("PPT2_01"), starts_with("PPT2_02")), na.rm = TRUE),
    winter_prev_Tmean = Tmean2_12,
    winter_current_Tmean = rowMeans(select(., starts_with("Tmean2_01"), starts_with("Tmean2_02")), na.rm = TRUE),
    winter_prev_Tmin = Tmin2_12,
    winter_current_Tmin = rowMeans(select(., starts_with("Tmin2_01"), starts_with("Tmin2_02")), na.rm = TRUE),
    winter_prev_Tmax = Tmax2_12,
    winter_current_Tmax = rowMeans(select(., starts_with("Tmax2_01"), starts_with("Tmax2_02")), na.rm = TRUE),
    winter_prev_Vpd = Vpdmax2_12,
    winter_current_Vpd = rowMeans(select(., starts_with("Vpdmax2_01"), starts_with("Vpdmax2_02")), na.rm = TRUE)
  ) %>% 
  subset(select = -c(PPT2_01:Vpdmax2_12))

nrp_winter_reorg$mean_winter_PPT = NA
nrp_winter_reorg$mean_winter_Tmean = NA
nrp_winter_reorg$mean_winter_Tmin = NA
nrp_winter_reorg$mean_winter_Tmax = NA
nrp_winter_reorg$mean_winter_Vpd = NA

# Calculate the mean for the previous year with the current year for each variable
N_years = nrow(nrp_winter_reorg)
nrp_winter_reorg$mean_winter_PPT[2:N_years] = rowMeans(cbind(nrp_winter_reorg$winter_prev_PPT[1:(N_years-1)], 
                                                               nrp_winter_reorg$winter_current_PPT[2:N_years]), na.rm = TRUE)
nrp_winter_reorg$mean_winter_Tmean[2:N_years] = rowMeans(cbind(nrp_winter_reorg$winter_prev_Tmean[1:(N_years-1)], 
                                                                 nrp_winter_reorg$winter_current_Tmean[2:N_years]), na.rm = TRUE)
nrp_winter_reorg$mean_winter_Tmin[2:N_years] = rowMeans(cbind(nrp_winter_reorg$winter_prev_Tmin[1:(N_years-1)], 
                                                                nrp_winter_reorg$winter_current_Tmin[2:N_years]), na.rm = TRUE)
nrp_winter_reorg$mean_winter_Tmax[2:N_years] = rowMeans(cbind(nrp_winter_reorg$winter_prev_Tmax[1:(N_years-1)], 
                                                                nrp_winter_reorg$winter_current_Tmax[2:N_years]), na.rm = TRUE)
nrp_winter_reorg$mean_winter_Vpd[2:N_years] = rowMeans(cbind(nrp_winter_reorg$winter_prev_Vpd[1:(N_years-1)], 
                                                               nrp_winter_reorg$winter_current_Vpd[2:N_years]), na.rm = TRUE) 

nrp_winter_mean = nrp_winter_reorg %>% 
  select(-c(winter_prev_PPT:winter_current_Vpd)) %>% 
  slice(-1) 



###PCA WINTER nrp
nrp_scaled_winter = scale(nrp_winter_mean[,2:6])

nrp_winter_pca = prcomp(nrp_scaled_winter)
#PCA values for PC1
nrp_winter_pca$x[,'PC1']

##????
eigs <- nrp_winter_pca$sdev^2
rbind(SD = sqrt(eigs),
      Proportion = eigs/sum(eigs),
      Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(nrp_winter_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(nrp_winter_pca)

nrp_PCA = data.frame(year=nrp_summer_mean$year, site = "NRP",
                       PCA1_summer = nrp_summer_pca$x[,'PC1'],
                       PCA2_summer = nrp_summer_pca$x[,'PC2'],
                       PCA1_winter = nrp_winter_pca$x[,'PC1'],
                       PCA2_winter = nrp_winter_pca$x[,'PC2'])
nrp_PCA$year = as.numeric(nrp_PCA$year)
saveRDS(nrp_PCA, "nrp_PCA.RDS")

############ROOSTER ###########################################
clim_rooster_month <- prism_long |>
  filter(loc == 'ROOSTER') |>
  pivot_wider(names_from = 'month',
                     values_from = c('PPT2', 'Tmean2', 'Tmin2',
                                     'Tmax2', 'Vpdmax2')) |>
  select(-loc) |>
  drop_na()

rooster_summer = clim_rooster_month %>% 
  select(starts_with("year"),ends_with("06"), ends_with("07"), ends_with("08"))
# rooster_summer_time = rooster_summer
# rooster_summer_time$year = 1:nrow(rooster_summer)
rooster_summer_long = melt(rooster_summer, id.vars = "year")

rooster_summer_mean = rooster_summer %>% 
  rowwise() %>%
  mutate(
    PPT_mean = mean(c_across(starts_with("PPT")), na.rm = TRUE),
    Tmean_mean = mean(c_across(starts_with("Tmean")), na.rm = TRUE),
    Tmin_mean = mean(c_across(starts_with("Tmin")), na.rm = TRUE),
    Tmax_mean = mean(c_across(starts_with("Tmax")), na.rm = TRUE),
    Vpdmax_mean = mean(c_across(starts_with("Vpd")), na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  subset(select = -c(PPT2_06:Vpdmax2_08)) %>% 
  slice(-1)

#monthly climate plot for rooster all on one
# ggplot(data = rooster_summer_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for summer
# ggplot(data = rooster_summer_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA SUMMER rooster ####

rooster_scaled_sum = scale(rooster_summer_mean[,2:6])

rooster_summer_pca = prcomp(rooster_scaled_sum)
#PCA values for PC1
rooster_summer_pca$x[,'PC1']


##????
eigs <- rooster_summer_pca$sdev^2
rbind(SD = sqrt(eigs),
      Proportion = eigs/sum(eigs),
      Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(rooster_summer_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(rooster_summer_pca)

####ROOSTER WINTER#####
rooster_winter = clim_rooster_month %>% 
  select(starts_with("year"),ends_with("01"), ends_with("02"), ends_with("12"))

rooster_winter_reorg = rooster_winter %>% 
  mutate(
    winter_prev_PPT = PPT2_12,
    winter_current_PPT = rowMeans(select(., starts_with("PPT2_01"), starts_with("PPT2_02")), na.rm = TRUE),
    winter_prev_Tmean = Tmean2_12,
    winter_current_Tmean = rowMeans(select(., starts_with("Tmean2_01"), starts_with("Tmean2_02")), na.rm = TRUE),
    winter_prev_Tmin = Tmin2_12,
    winter_current_Tmin = rowMeans(select(., starts_with("Tmin2_01"), starts_with("Tmin2_02")), na.rm = TRUE),
    winter_prev_Tmax = Tmax2_12,
    winter_current_Tmax = rowMeans(select(., starts_with("Tmax2_01"), starts_with("Tmax2_02")), na.rm = TRUE),
    winter_prev_Vpd = Vpdmax2_12,
    winter_current_Vpd = rowMeans(select(., starts_with("Vpdmax2_01"), starts_with("Vpdmax2_02")), na.rm = TRUE)
  ) %>% 
  subset(select = -c(PPT2_01:Vpdmax2_12))

rooster_winter_reorg$mean_winter_PPT = NA
rooster_winter_reorg$mean_winter_Tmean = NA
rooster_winter_reorg$mean_winter_Tmin = NA
rooster_winter_reorg$mean_winter_Tmax = NA
rooster_winter_reorg$mean_winter_Vpd = NA

# Calculate the mean for the previous year with the current year for each variable
N_years = nrow(rooster_winter_reorg)
rooster_winter_reorg$mean_winter_PPT[2:N_years] = rowMeans(cbind(rooster_winter_reorg$winter_prev_PPT[1:(N_years-1)], 
                                                             rooster_winter_reorg$winter_current_PPT[2:N_years]), na.rm = TRUE)
rooster_winter_reorg$mean_winter_Tmean[2:N_years] = rowMeans(cbind(rooster_winter_reorg$winter_prev_Tmean[1:(N_years-1)], 
                                                               rooster_winter_reorg$winter_current_Tmean[2:N_years]), na.rm = TRUE)
rooster_winter_reorg$mean_winter_Tmin[2:N_years] = rowMeans(cbind(rooster_winter_reorg$winter_prev_Tmin[1:(N_years-1)], 
                                                              rooster_winter_reorg$winter_current_Tmin[2:N_years]), na.rm = TRUE)
rooster_winter_reorg$mean_winter_Tmax[2:N_years] = rowMeans(cbind(rooster_winter_reorg$winter_prev_Tmax[1:(N_years-1)], 
                                                              rooster_winter_reorg$winter_current_Tmax[2:N_years]), na.rm = TRUE)
rooster_winter_reorg$mean_winter_Vpd[2:N_years] = rowMeans(cbind(rooster_winter_reorg$winter_prev_Vpd[1:(N_years-1)], 
                                                             rooster_winter_reorg$winter_current_Vpd[2:N_years]), na.rm = TRUE) 

rooster_winter_mean = rooster_winter_reorg %>% 
  select(-c(winter_prev_PPT:winter_current_Vpd)) %>% 
  slice(-1) 



###PCA WINTER rooster
rooster_scaled_winter = scale(rooster_winter_mean[,2:6])

rooster_winter_pca = prcomp(rooster_scaled_winter)
#PCA values for PC1
rooster_winter_pca$x[,'PC1']

##????
eigs <- rooster_winter_pca$sdev^2
rbind(SD = sqrt(eigs),
      Proportion = eigs/sum(eigs),
      Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(rooster_winter_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(rooster_winter_pca)

rooster_PCA = data.frame(year=rooster_summer_mean$year, site = "ROOSTER",
                     PCA1_summer = rooster_summer_pca$x[,'PC1'],
                     PCA2_summer = rooster_summer_pca$x[,'PC2'],
                     PCA1_winter = rooster_winter_pca$x[,'PC1'],
                     PCA2_winter = rooster_winter_pca$x[,'PC2'])
rooster_PCA$year = as.numeric(rooster_PCA$year)
saveRDS(rooster_PCA, "rooster_PCA.RDS")


############SYLVANIA ###########################################
clim_sylvania_month <- prism_long |>
  filter(loc == 'SYLVANIA') |>
  pivot_wider(names_from = 'month',
                     values_from = c('PPT2', 'Tmean2', 'Tmin2',
                                     'Tmax2', 'Vpdmax2')) |>
  select(-loc) |>
  drop_na()

sylvania_summer = clim_sylvania_month %>% 
  select(starts_with("year"),ends_with("06"), ends_with("07"), ends_with("08"))
# sylvania_summer_time = sylvania_summer
# sylvania_summer_time$year = 1:nrow(sylvania_summer)
sylvania_summer_long = melt(sylvania_summer, id.vars = "year")

sylvania_summer_mean = sylvania_summer %>% 
  rowwise() %>%
  mutate(
    PPT_mean = mean(c_across(starts_with("PPT")), na.rm = TRUE),
    Tmean_mean = mean(c_across(starts_with("Tmean")), na.rm = TRUE),
    Tmin_mean = mean(c_across(starts_with("Tmin")), na.rm = TRUE),
    Tmax_mean = mean(c_across(starts_with("Tmax")), na.rm = TRUE),
    Vpdmax_mean = mean(c_across(starts_with("Vpd")), na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  subset(select = -c(PPT2_06:Vpdmax2_08)) %>% 
  slice(-1)

#monthly climate plot for sylvania all on one
# ggplot(data = sylvania_summer_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for summer
# ggplot(data = sylvania_summer_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA SUMMER sylvania ####

sylvania_scaled_sum = scale(sylvania_summer_mean[,2:6])

sylvania_summer_pca = prcomp(sylvania_scaled_sum)
#PCA values for PC1
sylvania_summer_pca$x[,'PC1']


##????
eigs <- sylvania_summer_pca$sdev^2
rbind(SD = sqrt(eigs),
      Proportion = eigs/sum(eigs),
      Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(sylvania_summer_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(sylvania_summer_pca)

####SYLVANIA WINTER#####
sylvania_winter = clim_sylvania_month %>% 
  select(starts_with("year"),ends_with("01"), ends_with("02"), ends_with("12"))

sylvania_winter_reorg = sylvania_winter %>% 
  mutate(
    winter_prev_PPT = PPT2_12,
    winter_current_PPT = rowMeans(select(., starts_with("PPT2_01"), starts_with("PPT2_02")), na.rm = TRUE),
    winter_prev_Tmean = Tmean2_12,
    winter_current_Tmean = rowMeans(select(., starts_with("Tmean2_01"), starts_with("Tmean2_02")), na.rm = TRUE),
    winter_prev_Tmin = Tmin2_12,
    winter_current_Tmin = rowMeans(select(., starts_with("Tmin2_01"), starts_with("Tmin2_02")), na.rm = TRUE),
    winter_prev_Tmax = Tmax2_12,
    winter_current_Tmax = rowMeans(select(., starts_with("Tmax2_01"), starts_with("Tmax2_02")), na.rm = TRUE),
    winter_prev_Vpd = Vpdmax2_12,
    winter_current_Vpd = rowMeans(select(., starts_with("Vpdmax2_01"), starts_with("Vpdmax2_02")), na.rm = TRUE)
  ) %>% 
  subset(select = -c(PPT2_01:Vpdmax2_12))

sylvania_winter_reorg$mean_winter_PPT = NA
sylvania_winter_reorg$mean_winter_Tmean = NA
sylvania_winter_reorg$mean_winter_Tmin = NA
sylvania_winter_reorg$mean_winter_Tmax = NA
sylvania_winter_reorg$mean_winter_Vpd = NA

# Calculate the mean for the previous year with the current year for each variable
N_years = nrow(sylvania_winter_reorg)
sylvania_winter_reorg$mean_winter_PPT[2:N_years] = rowMeans(cbind(sylvania_winter_reorg$winter_prev_PPT[1:(N_years-1)], 
                                                                 sylvania_winter_reorg$winter_current_PPT[2:N_years]), na.rm = TRUE)
sylvania_winter_reorg$mean_winter_Tmean[2:N_years] = rowMeans(cbind(sylvania_winter_reorg$winter_prev_Tmean[1:(N_years-1)], 
                                                                   sylvania_winter_reorg$winter_current_Tmean[2:N_years]), na.rm = TRUE)
sylvania_winter_reorg$mean_winter_Tmin[2:N_years] = rowMeans(cbind(sylvania_winter_reorg$winter_prev_Tmin[1:(N_years-1)], 
                                                                  sylvania_winter_reorg$winter_current_Tmin[2:N_years]), na.rm = TRUE)
sylvania_winter_reorg$mean_winter_Tmax[2:N_years] = rowMeans(cbind(sylvania_winter_reorg$winter_prev_Tmax[1:(N_years-1)], 
                                                                  sylvania_winter_reorg$winter_current_Tmax[2:N_years]), na.rm = TRUE)
sylvania_winter_reorg$mean_winter_Vpd[2:N_years] = rowMeans(cbind(sylvania_winter_reorg$winter_prev_Vpd[1:(N_years-1)], 
                                                                 sylvania_winter_reorg$winter_current_Vpd[2:N_years]), na.rm = TRUE) 

sylvania_winter_mean = sylvania_winter_reorg %>% 
  select(-c(winter_prev_PPT:winter_current_Vpd)) %>% 
  slice(-1) 



###PCA WINTER sylvania
sylvania_scaled_winter = scale(sylvania_winter_mean[,2:6])

sylvania_winter_pca = prcomp(sylvania_scaled_winter)
#PCA values for PC1
sylvania_winter_pca$x[,'PC1']

##????
eigs <- sylvania_winter_pca$sdev^2
rbind(SD = sqrt(eigs),
      Proportion = eigs/sum(eigs),
      Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(sylvania_winter_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(sylvania_winter_pca)

sylvania_PCA = data.frame(year=sylvania_summer_mean$year, site = "SYLVANIA",
                         PCA1_summer = sylvania_summer_pca$x[,'PC1'],
                         PCA2_summer = sylvania_summer_pca$x[,'PC2'],
                         PCA1_winter = sylvania_winter_pca$x[,'PC1'],
                         PCA2_winter = sylvania_winter_pca$x[,'PC2'])
sylvania_PCA$year = as.numeric(sylvania_PCA$year)
saveRDS(sylvania_PCA, "sylvania_PCA.RDS")
############HARVARD ###########################################
clim_harvard_month <- prism_long |>
  filter(loc == 'HARVARD') |>
  pivot_wider(names_from = 'month',
                     values_from = c('PPT2', 'Tmean2', 'Tmin2',
                                     'Tmax2', 'Vpdmax2')) |>
  select(-loc) |>
  drop_na()

harvard_summer = clim_harvard_month %>% 
  select(starts_with("year"),ends_with("06"), ends_with("07"), ends_with("08"))
# harvard_summer_time = harvard_summer
# harvard_summer_time$year = 1:nrow(harvard_summer)
harvard_summer_long = melt(harvard_summer, id.vars = "year")

harvard_summer_mean = harvard_summer %>% 
  rowwise() %>%
  mutate(
    PPT_mean = mean(c_across(starts_with("PPT")), na.rm = TRUE),
    Tmean_mean = mean(c_across(starts_with("Tmean")), na.rm = TRUE),
    Tmin_mean = mean(c_across(starts_with("Tmin")), na.rm = TRUE),
    Tmax_mean = mean(c_across(starts_with("Tmax")), na.rm = TRUE),
    Vpdmax_mean = mean(c_across(starts_with("Vpd")), na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  subset(select = -c(PPT2_06:Vpdmax2_08)) %>% 
  slice(-1)

#monthly climate plot for harvard all on one
# ggplot(data = harvard_summer_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for summer
# ggplot(data = harvard_summer_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA SUMMER harvard ####

harvard_scaled_sum = scale(harvard_summer_mean[,2:6])

harvard_summer_pca = prcomp(harvard_scaled_sum)
#PCA values for PC1
harvard_summer_pca$x[,'PC1']


##????
eigs <- harvard_summer_pca$sdev^2
rbind(SD = sqrt(eigs),
      Proportion = eigs/sum(eigs),
      Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(harvard_summer_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(harvard_summer_pca)

####harvard WINTER#####
harvard_winter = clim_harvard_month %>% 
  select(starts_with("year"),ends_with("01"), ends_with("02"), ends_with("12"))

harvard_winter_reorg = harvard_winter %>% 
  mutate(
    winter_prev_PPT = PPT2_12,
    winter_current_PPT = rowMeans(select(., starts_with("PPT2_01"), starts_with("PPT2_02")), na.rm = TRUE),
    winter_prev_Tmean = Tmean2_12,
    winter_current_Tmean = rowMeans(select(., starts_with("Tmean2_01"), starts_with("Tmean2_02")), na.rm = TRUE),
    winter_prev_Tmin = Tmin2_12,
    winter_current_Tmin = rowMeans(select(., starts_with("Tmin2_01"), starts_with("Tmin2_02")), na.rm = TRUE),
    winter_prev_Tmax = Tmax2_12,
    winter_current_Tmax = rowMeans(select(., starts_with("Tmax2_01"), starts_with("Tmax2_02")), na.rm = TRUE),
    winter_prev_Vpd = Vpdmax2_12,
    winter_current_Vpd = rowMeans(select(., starts_with("Vpdmax2_01"), starts_with("Vpdmax2_02")), na.rm = TRUE)
  ) %>% 
  subset(select = -c(PPT2_01:Vpdmax2_12))

harvard_winter_reorg$mean_winter_PPT = NA
harvard_winter_reorg$mean_winter_Tmean = NA
harvard_winter_reorg$mean_winter_Tmin = NA
harvard_winter_reorg$mean_winter_Tmax = NA
harvard_winter_reorg$mean_winter_Vpd = NA

# Calculate the mean for the previous year with the current year for each variable
N_years = nrow(harvard_winter_reorg)
harvard_winter_reorg$mean_winter_PPT[2:N_years] = rowMeans(cbind(harvard_winter_reorg$winter_prev_PPT[1:(N_years-1)], 
                                                                  harvard_winter_reorg$winter_current_PPT[2:N_years]), na.rm = TRUE)
harvard_winter_reorg$mean_winter_Tmean[2:N_years] = rowMeans(cbind(harvard_winter_reorg$winter_prev_Tmean[1:(N_years-1)], 
                                                                    harvard_winter_reorg$winter_current_Tmean[2:N_years]), na.rm = TRUE)
harvard_winter_reorg$mean_winter_Tmin[2:N_years] = rowMeans(cbind(harvard_winter_reorg$winter_prev_Tmin[1:(N_years-1)], 
                                                                   harvard_winter_reorg$winter_current_Tmin[2:N_years]), na.rm = TRUE)
harvard_winter_reorg$mean_winter_Tmax[2:N_years] = rowMeans(cbind(harvard_winter_reorg$winter_prev_Tmax[1:(N_years-1)], 
                                                                   harvard_winter_reorg$winter_current_Tmax[2:N_years]), na.rm = TRUE)
harvard_winter_reorg$mean_winter_Vpd[2:N_years] = rowMeans(cbind(harvard_winter_reorg$winter_prev_Vpd[1:(N_years-1)], 
                                                                  harvard_winter_reorg$winter_current_Vpd[2:N_years]), na.rm = TRUE) 

harvard_winter_mean = harvard_winter_reorg %>% 
  select(-c(winter_prev_PPT:winter_current_Vpd)) %>% 
  slice(-1) 



###PCA WINTER harvard
harvard_scaled_winter = scale(harvard_winter_mean[,2:6])

harvard_winter_pca = prcomp(harvard_scaled_winter)
#PCA values for PC1
harvard_winter_pca$x[,'PC1']

##????
eigs <- harvard_winter_pca$sdev^2
rbind(SD = sqrt(eigs),
      Proportion = eigs/sum(eigs),
      Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(harvard_winter_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(harvard_winter_pca)

harvard_PCA = data.frame(year=harvard_summer_mean$year, site = "HARVARD",
                          PCA1_summer = harvard_summer_pca$x[,'PC1'],
                          PCA2_summer = harvard_summer_pca$x[,'PC2'],
                          PCA1_winter = harvard_winter_pca$x[,'PC1'],
                          PCA2_winter = harvard_winter_pca$x[,'PC2'])
harvard_PCA$year = as.numeric(harvard_PCA$year)
saveRDS(harvard_PCA, "harvard_PCA.RDS")

############HMC ###########################################
clim_hmc_month <- prism_long |>
  filter(loc == 'HMC') |>
  pivot_wider(names_from = 'month',
              values_from = c('PPT2', 'Tmean2', 'Tmin2',
                              'Tmax2', 'Vpdmax2')) |>
  select(-loc) |>
  drop_na()

hmc_summer = clim_hmc_month %>% 
  select(starts_with("year"),ends_with("06"), ends_with("07"), ends_with("08"))
# hmc_summer_time = hmc_summer
# hmc_summer_time$year = 1:nrow(hmc_summer)
hmc_summer_long = melt(hmc_summer, id.vars = "year")

hmc_summer_mean = hmc_summer %>% 
  rowwise() %>%
  mutate(
    PPT_mean = mean(c_across(starts_with("PPT")), na.rm = TRUE),
    Tmean_mean = mean(c_across(starts_with("Tmean")), na.rm = TRUE),
    Tmin_mean = mean(c_across(starts_with("Tmin")), na.rm = TRUE),
    Tmax_mean = mean(c_across(starts_with("Tmax")), na.rm = TRUE),
    Vpdmax_mean = mean(c_across(starts_with("Vpd")), na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  subset(select = -c(PPT2_06:Vpdmax2_08)) %>% 
  slice(-1)

#monthly climate plot for hmc all on one
# ggplot(data = hmc_summer_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for summer
# ggplot(data = hmc_summer_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA SUMMER hmc ####

hmc_scaled_sum = scale(hmc_summer_mean[,2:6])

hmc_summer_pca = prcomp(hmc_scaled_sum)
#PCA values for PC1
hmc_summer_pca$x[,'PC1']


##????
eigs <- hmc_summer_pca$sdev^2
rbind(SD = sqrt(eigs),
      Proportion = eigs/sum(eigs),
      Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(hmc_summer_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(hmc_summer_pca)

####hmc WINTER#####
hmc_winter = clim_hmc_month %>% 
  select(starts_with("year"),ends_with("01"), ends_with("02"), ends_with("12"))

hmc_winter_reorg = hmc_winter %>% 
  mutate(
    winter_prev_PPT = PPT2_12,
    winter_current_PPT = rowMeans(select(., starts_with("PPT2_01"), starts_with("PPT2_02")), na.rm = TRUE),
    winter_prev_Tmean = Tmean2_12,
    winter_current_Tmean = rowMeans(select(., starts_with("Tmean2_01"), starts_with("Tmean2_02")), na.rm = TRUE),
    winter_prev_Tmin = Tmin2_12,
    winter_current_Tmin = rowMeans(select(., starts_with("Tmin2_01"), starts_with("Tmin2_02")), na.rm = TRUE),
    winter_prev_Tmax = Tmax2_12,
    winter_current_Tmax = rowMeans(select(., starts_with("Tmax2_01"), starts_with("Tmax2_02")), na.rm = TRUE),
    winter_prev_Vpd = Vpdmax2_12,
    winter_current_Vpd = rowMeans(select(., starts_with("Vpdmax2_01"), starts_with("Vpdmax2_02")), na.rm = TRUE)
  ) %>% 
  subset(select = -c(PPT2_01:Vpdmax2_12))

hmc_winter_reorg$mean_winter_PPT = NA
hmc_winter_reorg$mean_winter_Tmean = NA
hmc_winter_reorg$mean_winter_Tmin = NA
hmc_winter_reorg$mean_winter_Tmax = NA
hmc_winter_reorg$mean_winter_Vpd = NA

# Calculate the mean for the previous year with the current year for each variable
N_years = nrow(hmc_winter_reorg)
hmc_winter_reorg$mean_winter_PPT[2:N_years] = rowMeans(cbind(hmc_winter_reorg$winter_prev_PPT[1:(N_years-1)], 
                                                                 hmc_winter_reorg$winter_current_PPT[2:N_years]), na.rm = TRUE)
hmc_winter_reorg$mean_winter_Tmean[2:N_years] = rowMeans(cbind(hmc_winter_reorg$winter_prev_Tmean[1:(N_years-1)], 
                                                                   hmc_winter_reorg$winter_current_Tmean[2:N_years]), na.rm = TRUE)
hmc_winter_reorg$mean_winter_Tmin[2:N_years] = rowMeans(cbind(hmc_winter_reorg$winter_prev_Tmin[1:(N_years-1)], 
                                                                  hmc_winter_reorg$winter_current_Tmin[2:N_years]), na.rm = TRUE)
hmc_winter_reorg$mean_winter_Tmax[2:N_years] = rowMeans(cbind(hmc_winter_reorg$winter_prev_Tmax[1:(N_years-1)], 
                                                                  hmc_winter_reorg$winter_current_Tmax[2:N_years]), na.rm = TRUE)
hmc_winter_reorg$mean_winter_Vpd[2:N_years] = rowMeans(cbind(hmc_winter_reorg$winter_prev_Vpd[1:(N_years-1)], 
                                                                 hmc_winter_reorg$winter_current_Vpd[2:N_years]), na.rm = TRUE) 

hmc_winter_mean = hmc_winter_reorg %>% 
  select(-c(winter_prev_PPT:winter_current_Vpd)) %>% 
  slice(-1) 



###PCA WINTER hmc
hmc_scaled_winter = scale(hmc_winter_mean[,2:6])

hmc_winter_pca = prcomp(hmc_scaled_winter)
#PCA values for PC1
hmc_winter_pca$x[,'PC1']

##????
eigs <- hmc_winter_pca$sdev^2
rbind(SD = sqrt(eigs),
      Proportion = eigs/sum(eigs),
      Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(hmc_winter_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(hmc_winter_pca)

hmc_PCA = data.frame(year=hmc_summer_mean$year, site = "HMC",
                         PCA1_summer = hmc_summer_pca$x[,'PC1'],
                         PCA2_summer = hmc_summer_pca$x[,'PC2'],
                         PCA1_winter = hmc_winter_pca$x[,'PC1'],
                         PCA2_winter = hmc_winter_pca$x[,'PC2'])
hmc_PCA$year = as.numeric(hmc_PCA$year)
saveRDS(hmc_PCA, "hmc_PCA.RDS")


#################ALL SITES######################
#PCA1/2 for all sites for summer and winter 
pca_sites = data.frame(year = character(0), 
                       site = character(0), PCA1_summer = numeric(0), 
                       PCA2_summer = numeric(0), PCA1_winter = numeric(0), 
                       PCA2_winter = numeric(0))

pca_sites = rbind(goose_PCA, rooster_PCA, nrp_PCA, harvard_PCA, sylvania_PCA, hmc_PCA)
saveRDS(pca_sites, "pca_sites.RDS")

# Correlations by site
# cor_goose_month <- cor(clim_goose_month)
# cor_nrp_month <- cor(clim_nrp_month)
# cor_rooster_month <- cor(clim_rooster_month)
# cor_sylv_month <- cor(clim_sylv_month)
# cor_harv_month <- cor(clim_harv_month)
# 
# # Correlation matrices
# corrplot(corr = cor_goose_month,
#                    method = 'circle',
#                    type = 'upper', diag = FALSE)
# corrplot(corr = cor_nrp_month,
#                    method = 'circle',
#                    type = 'upper', diag = FALSE)
# corrplot(corr = cor_rooster_month,
#                    method = 'circle',
#                    type = 'upper', diag = FALSE)
# corrplot(corr = cor_sylv_month,
#                    method = 'circle',
#                    type = 'upper', diag = FALSE)
# corrplot(corr = cor_harv_month,
#                    method = 'circle',
#                    type = 'upper', diag = FALSE)

#####SUMMER PCA############
#for the months of june-aug not averaged 
#scaling the dataset
# goose_scaled_sum = scale(goose_summer)
# 
# goose_summer_pca = prcomp(goose_scaled_sum)
# #PCA values for PC1
# goose_summer_pca$x[,'PC1']
# 
# ##????
# eigs <- goose_summer_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))
# 
# 
# #shows variables per component
# fviz_eig(goose_summer_pca, addlabels = TRUE)
# 
# 
# #biplot
# fviz_pca_biplot(goose_summer_pca)
# 
# #visualize principal component analysis
# fviz_pca_var(clim_goose_pca, col.var = "black")
# fviz_cos2(clim_goose_pca, choice = "var", axes = 1:2)
# clim_goose_pca_time = data.frame(year=seq(1, nrow(clim_goose)), clim_goose_scaled %*% clim_goose_pca$rotation)
# 
# ggplot(data=clim_goose_pca_time) +
#   geom_line(aes(x=year, y=PC1))
# 
# ggplot(data = goose_long, aes(x= year, y = value))+
#   geom_line()+
#   facet_wrap(~variable, scales = "free_y")
# 
# pca_time_melt = melt(clim_goose_pca_time, id.vars='year')
# pca_time_melt$type = rep('goose')


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
# cor_goose <- cor(clim_goose)
# cor_nrp <- cor(clim_nrp)
# cor_rooster <- cor(clim_rooster)
# cor_sylv <- cor(clim_sylv)
# cor_harv <- cor(clim_harv)
# cor_hmc = cor(clim_hmc)
# 
# # Correlation matrices
# corrplot::corrplot(cor = cor_goose,
#                    method = 'circle',
#                    type = 'upper', diag = FALSE)
# corrplot::corrplot(cor = cor_nrp,
#                    method = 'circle',
#                    type = 'upper', diag = FALSE)
# corrplot::corrplot(cor = cor_rooster,
#                    method = 'circle',
#                    type = 'upper', diag = FALSE)
# corrplot::corrplot(cor = cor_sylv,
#                    method = 'circle',
#                    type = 'upper', diag = FALSE)
# corrplot::corrplot(cor = cor_harv,
#                    method = 'circle',
#                    type = 'upper', diag = FALSE)
# corrplot(cor = cor_hmc, method = 'circle', type = 'upper',
#          diag = FALSE, title = 'hmc_cor')

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
# prism_growing <- prism_long |> 
#   dplyr::mutate(year = as.numeric(year)) |>
#   dplyr::mutate(growing_year = dplyr::if_else(month %in% c('01', '02', '03', '04', 
#                                                            '05', '06', '07', '08'),
#                                               year, year + 1)) |>
#   dplyr::group_by(growing_year, loc) |>
#   dplyr::summarize(PPT = mean(PPT2),
#                    Tmean = mean(Tmean2),
#                    sd_PPT = sd(PPT2),
#                    sd_Tmean = sd(Tmean2),
#                    Tmin = min(Tmin2),
#                    Tmax = max(Tmax2),
#                    Vpdmax = max(Vpdmax2))
# 
# # Split by site
# clim_goose <- prism_growing |>
#   dplyr::ungroup() |>
#   dplyr::filter(loc == 'GOOSE') |>
#   dplyr::select(PPT:Vpdmax)
# clim_nrp <- prism_growing |>
#   dplyr::ungroup() |>
#   dplyr::filter(loc == 'NRP') |>
#   dplyr::select(PPT:Vpdmax)
# clim_rooster <- prism_growing |>
#   dplyr::ungroup() |>
#   dplyr::filter(loc == 'ROOSTER') |>
#   dplyr::select(PPT:Vpdmax)
# clim_sylv <- prism_growing |>
#   dplyr::ungroup() |>
#   dplyr::filter(loc == 'SYLVANIA') |>
#   dplyr::select(PPT:Vpdmax)
# clim_harv <- prism_growing |>
#   dplyr::ungroup() |>
#   dplyr::filter(loc == 'HARVARD') |>
#   dplyr::select(PPT:Vpdmax)
# 
# # Correlations by site
# cor_goose <- cor(clim_goose)
# cor_nrp <- cor(clim_nrp)
# cor_rooster <- cor(clim_rooster)
# cor_sylv <- cor(clim_sylv)
# cor_harv <- cor(clim_harv)
# 
# # Correlation matrices
# corrplot::corrplot(corr = cor_goose,
#                    method = 'circle',
#                    type = 'upper', diag = FALSE)
# corrplot::corrplot(corr = cor_nrp,
#                    method = 'circle',
#                    type = 'upper', diag = FALSE)
# corrplot::corrplot(corr = cor_rooster,
#                    method = 'circle',
#                    type = 'upper', diag = FALSE)
# corrplot::corrplot(corr = cor_sylv,
#                    method = 'circle',
#                    type = 'upper', diag = FALSE)
# corrplot::corrplot(corr = cor_harv,
#                    method = 'circle',
#                    type = 'upper', diag = FALSE)

## Can still use Tmean, PPT, sd_Tmean, Vpdmax