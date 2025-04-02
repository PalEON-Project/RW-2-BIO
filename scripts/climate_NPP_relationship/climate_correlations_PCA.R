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
  filter(year>1948,year <2012)

# Split by site
#for each year for each month 
#total unfiltered data
# clim_goose <- prism_long |>
#   filter(loc == 'GOOSE') |>
#   select(PPT2, Tmean2:Vpdmax2)
# clim_nrp <- prism_long |>
#   filter(loc == 'NRP') |>
#   select(PPT2, Tmean2:Vpdmax2)
# clim_rooster <- prism_long |>
#   filter(loc == 'ROOSTER') |>
#   select(PPT2, Tmean2:Vpdmax2)
# clim_sylv <- prism_long |>
#   filter(loc == 'SYLVANIA') |>
#   select(PPT2, Tmean2:Vpdmax2)
# clim_harv <- prism_long |>
#   filter(loc == 'HARVARD') |>
#   select(PPT2, Tmean2:Vpdmax2)
# clim_hmc = prism_long |>
#   filter(loc == 'HMC') |>
#   select(PPT2, Tmean2:Vpdmax2)



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
###averaged over summer###
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
#pulling each month for each season 
goose_summer = subset(clim_goose_month, year>1949)%>% 
  select(starts_with("year"),ends_with("06"), ends_with("07"), ends_with("08"))
goose_spring = subset(clim_goose_month, year>1949) %>% 
  select(starts_with("year"),ends_with("03"), ends_with("04"), ends_with("05"))
goose_fall = subset(clim_goose_month, year>1949) %>% 
  select(starts_with("year"),ends_with("09"), ends_with("10"), ends_with("11"))

#taking the mean of each climate variable over all the months 
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
# ggplot(data = goose_summer_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for summer
# ggplot(data = goose_summer_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA SUMMER GOOSE ####
#scaling data
goose_scaled_sum = scale(goose_summer_mean[,2:6])
#PCA of summer months 
goose_summer_pca = prcomp(goose_scaled_sum)

# #PCA values for PC1
# goose_summer_pca$x[,'PC1']
# ##????
# eigs <- goose_summer_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(goose_summer_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(goose_summer_pca)

###SPRING
goose_spring_mean = goose_spring %>% 
  rowwise() %>%
  mutate(
    PPT_mean = mean(c_across(starts_with("PPT")), na.rm = TRUE),
    Tmean_mean = mean(c_across(starts_with("Tmean")), na.rm = TRUE),
    Tmin_mean = mean(c_across(starts_with("Tmin")), na.rm = TRUE),
    Tmax_mean = mean(c_across(starts_with("Tmax")), na.rm = TRUE),
    Vpdmax_mean = mean(c_across(starts_with("Vpd")), na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  subset(select = -c(PPT2_03:Vpdmax2_05)) 

#monthly climate plot for goose all on one
# ggplot(data = goose_spring_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for spring
# ggplot(data = goose_spring_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA spring GOOSE ####
#scaling dataa
goose_scaled_spring = scale(goose_spring_mean[,2:6])

goose_spring_pca = prcomp(goose_scaled_spring)

# #PCA values for PC1
# goose_spring_pca$x[,'PC1']
# ##????
# eigs <- goose_spring_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(goose_spring_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(goose_spring_pca)


####FALL
goose_fall_mean = goose_fall %>% 
  rowwise() %>%
  mutate(
    PPT_mean = mean(c_across(starts_with("PPT")), na.rm = TRUE),
    Tmean_mean = mean(c_across(starts_with("Tmean")), na.rm = TRUE),
    Tmin_mean = mean(c_across(starts_with("Tmin")), na.rm = TRUE),
    Tmax_mean = mean(c_across(starts_with("Tmax")), na.rm = TRUE),
    Vpdmax_mean = mean(c_across(starts_with("Vpd")), na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  subset(select = -c(PPT2_09:Vpdmax2_11)) 

#monthly climate plot for goose all on one
# ggplot(data = goose_fall_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for fall
# ggplot(data = goose_fall_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA fall GOOSE ####

goose_scaled_fall = scale(goose_fall_mean[,2:6])

goose_fall_pca = prcomp(goose_scaled_fall)

# #PCA values for PC1
# goose_fall_pca$x[,'PC1']
# ##????
# eigs <- goose_fall_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(goose_fall_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(goose_fall_pca)

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

# #PCA values for PC1
# goose_winter_pca$x[,'PC1']
# ##????
# eigs <- goose_winter_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(goose_winter_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(goose_winter_pca)

goose_PCA = data.frame(year=goose_summer_mean$year, site = "GOOSE",
                       PCA1_summer = goose_summer_pca$x[,'PC1'],
                       PCA2_summer = goose_summer_pca$x[,'PC2'],
                       PCA1_spring = goose_spring_pca$x[,'PC1'],
                       PCA2_spring = goose_spring_pca$x[,'PC2'],
                       PCA1_fall = goose_fall_pca$x[,'PC1'],
                       PCA2_fall = goose_fall_pca$x[,'PC2'],
                       PCA1_winter = goose_winter_pca$x[,'PC1'],
                       PCA2_winter = goose_winter_pca$x[,'PC2'])

goose_PCA$year = as.numeric(goose_PCA$year)
saveRDS(goose_PCA, "goose_PCA_mean.RDS")


############NRP ###########################################
clim_nrp_month <- prism_long |>
  filter(loc == 'NRP') |>
  pivot_wider(names_from = 'month',
                     values_from = c('PPT2', 'Tmean2', 'Tmin2',
                                     'Tmax2', 'Vpdmax2')) |>
  select(-loc) |>
  drop_na()

nrp_summer = subset(clim_nrp_month, year>1949)%>% 
  select(starts_with("year"),ends_with("06"), ends_with("07"), ends_with("08"))
nrp_spring = subset(clim_nrp_month, year>1949) %>% 
  select(starts_with("year"),ends_with("03"), ends_with("04"), ends_with("05"))
nrp_fall = subset(clim_nrp_month, year>1949) %>% 
  select(starts_with("year"),ends_with("09"), ends_with("10"), ends_with("11"))

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
  subset(select = -c(PPT2_06:Vpdmax2_08)) 

#monthly climate plot for nrp all on one
# ggplot(data = nrp_summer_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for summer
# ggplot(data = nrp_summer_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA SUMMER nrp ####

nrp_scaled_sum = scale(nrp_summer_mean[,2:6])

nrp_summer_pca = prcomp(nrp_scaled_sum)

# #PCA values for PC1
# nrp_summer_pca$x[,'PC1']
# ##????
# eigs <- nrp_summer_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(nrp_summer_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(nrp_summer_pca)

###SPRING
nrp_spring_mean = nrp_spring %>% 
  rowwise() %>%
  mutate(
    PPT_mean = mean(c_across(starts_with("PPT")), na.rm = TRUE),
    Tmean_mean = mean(c_across(starts_with("Tmean")), na.rm = TRUE),
    Tmin_mean = mean(c_across(starts_with("Tmin")), na.rm = TRUE),
    Tmax_mean = mean(c_across(starts_with("Tmax")), na.rm = TRUE),
    Vpdmax_mean = mean(c_across(starts_with("Vpd")), na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  subset(select = -c(PPT2_03:Vpdmax2_05)) 

#monthly climate plot for nrp all on one
# ggplot(data = nrp_spring_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for spring
# ggplot(data = nrp_spring_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA spring nrp ####

nrp_scaled_spring = scale(nrp_spring_mean[,2:6])

nrp_spring_pca = prcomp(nrp_scaled_spring)

# #PCA values for PC1
# nrp_spring_pca$x[,'PC1']
# ##????
# eigs <- nrp_spring_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(nrp_spring_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(nrp_spring_pca)


####FALL
nrp_fall_mean = nrp_fall %>% 
  rowwise() %>%
  mutate(
    PPT_mean = mean(c_across(starts_with("PPT")), na.rm = TRUE),
    Tmean_mean = mean(c_across(starts_with("Tmean")), na.rm = TRUE),
    Tmin_mean = mean(c_across(starts_with("Tmin")), na.rm = TRUE),
    Tmax_mean = mean(c_across(starts_with("Tmax")), na.rm = TRUE),
    Vpdmax_mean = mean(c_across(starts_with("Vpd")), na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  subset(select = -c(PPT2_09:Vpdmax2_11)) 

#monthly climate plot for nrp all on one
# ggplot(data = nrp_fall_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for fall
# ggplot(data = nrp_fall_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA fall nrp ####

nrp_scaled_fall = scale(nrp_fall_mean[,2:6])

nrp_fall_pca = prcomp(nrp_scaled_fall)

# #PCA values for PC1
# nrp_fall_pca$x[,'PC1']
# ##????
# eigs <- nrp_fall_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(nrp_fall_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(nrp_fall_pca)

####nrp WINTER#####
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

# #PCA values for PC1
# nrp_winter_pca$x[,'PC1']
# ##????
# eigs <- nrp_winter_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(nrp_winter_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(nrp_winter_pca)

nrp_PCA = data.frame(year=nrp_summer_mean$year, site = "NRP",
                     PCA1_summer = nrp_summer_pca$x[,'PC1'],
                     PCA2_summer = nrp_summer_pca$x[,'PC2'],
                     PCA1_spring = nrp_spring_pca$x[,'PC1'],
                     PCA2_spring = nrp_spring_pca$x[,'PC2'],
                     PCA1_fall = nrp_fall_pca$x[,'PC1'],
                     PCA2_fall = nrp_fall_pca$x[,'PC2'],
                     PCA1_winter = nrp_winter_pca$x[,'PC1'],
                     PCA2_winter = nrp_winter_pca$x[,'PC2'])

nrp_PCA$year = as.numeric(nrp_PCA$year)
saveRDS(nrp_PCA, "nrp_PCA_mean.RDS")


############ROOSTER ###########################################
clim_rooster_month <- prism_long |>
  filter(loc == 'ROOSTER') |>
  pivot_wider(names_from = 'month',
                     values_from = c('PPT2', 'Tmean2', 'Tmin2',
                                     'Tmax2', 'Vpdmax2')) |>
  select(-loc) |>
  drop_na()

rooster_summer = subset(clim_rooster_month, year>1949)%>% 
  select(starts_with("year"),ends_with("06"), ends_with("07"), ends_with("08"))
rooster_spring = subset(clim_rooster_month, year>1949) %>% 
  select(starts_with("year"),ends_with("03"), ends_with("04"), ends_with("05"))
rooster_fall = subset(clim_rooster_month, year>1949) %>% 
  select(starts_with("year"),ends_with("09"), ends_with("10"), ends_with("11"))

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
  subset(select = -c(PPT2_06:Vpdmax2_08))

#monthly climate plot for rooster all on one
# ggplot(data = rooster_summer_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for summer
# ggplot(data = rooster_summer_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA SUMMER rooster ####

rooster_scaled_sum = scale(rooster_summer_mean[,2:6])

rooster_summer_pca = prcomp(rooster_scaled_sum)

# #PCA values for PC1
# rooster_summer_pca$x[,'PC1']
# ##????
# eigs <- rooster_summer_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(rooster_summer_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(rooster_summer_pca)

###SPRING
rooster_spring_mean = rooster_spring %>% 
  rowwise() %>%
  mutate(
    PPT_mean = mean(c_across(starts_with("PPT")), na.rm = TRUE),
    Tmean_mean = mean(c_across(starts_with("Tmean")), na.rm = TRUE),
    Tmin_mean = mean(c_across(starts_with("Tmin")), na.rm = TRUE),
    Tmax_mean = mean(c_across(starts_with("Tmax")), na.rm = TRUE),
    Vpdmax_mean = mean(c_across(starts_with("Vpd")), na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  subset(select = -c(PPT2_03:Vpdmax2_05)) 

#monthly climate plot for rooster all on one
# ggplot(data = rooster_spring_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for spring
# ggplot(data = rooster_spring_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA spring rooster ####

rooster_scaled_spring = scale(rooster_spring_mean[,2:6])

rooster_spring_pca = prcomp(rooster_scaled_spring)

# #PCA values for PC1
# rooster_spring_pca$x[,'PC1']
# ##????
# eigs <- rooster_spring_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(rooster_spring_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(rooster_spring_pca)


####FALL
rooster_fall_mean = rooster_fall %>% 
  rowwise() %>%
  mutate(
    PPT_mean = mean(c_across(starts_with("PPT")), na.rm = TRUE),
    Tmean_mean = mean(c_across(starts_with("Tmean")), na.rm = TRUE),
    Tmin_mean = mean(c_across(starts_with("Tmin")), na.rm = TRUE),
    Tmax_mean = mean(c_across(starts_with("Tmax")), na.rm = TRUE),
    Vpdmax_mean = mean(c_across(starts_with("Vpd")), na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  subset(select = -c(PPT2_09:Vpdmax2_11))

#monthly climate plot for rooster all on one
# ggplot(data = rooster_fall_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for fall
# ggplot(data = rooster_fall_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA fall rooster ####

rooster_scaled_fall = scale(rooster_fall_mean[,2:6])

rooster_fall_pca = prcomp(rooster_scaled_fall)

# #PCA values for PC1
# rooster_fall_pca$x[,'PC1']
# ##????
# eigs <- rooster_fall_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(rooster_fall_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(rooster_fall_pca)

####rooster WINTER#####
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

# #PCA values for PC1
# rooster_winter_pca$x[,'PC1']
# ##????
# eigs <- rooster_winter_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(rooster_winter_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(rooster_winter_pca)

rooster_PCA = data.frame(year=rooster_summer_mean$year, site = "ROOSTER",
                         PCA1_summer = rooster_summer_pca$x[,'PC1'],
                         PCA2_summer = rooster_summer_pca$x[,'PC2'],
                         PCA1_spring = rooster_spring_pca$x[,'PC1'],
                         PCA2_spring = rooster_spring_pca$x[,'PC2'],
                         PCA1_fall = rooster_fall_pca$x[,'PC1'],
                         PCA2_fall = rooster_fall_pca$x[,'PC2'],
                         PCA1_winter = rooster_winter_pca$x[,'PC1'],
                         PCA2_winter = rooster_winter_pca$x[,'PC2'])

rooster_PCA$year = as.numeric(rooster_PCA$year)
saveRDS(rooster_PCA, "rooster_PCA_mean.RDS")



############SYLVANIA ###########################################
clim_sylvania_month <- prism_long |>
  filter(loc == 'SYLVANIA') |>
  pivot_wider(names_from = 'month',
                     values_from = c('PPT2', 'Tmean2', 'Tmin2',
                                     'Tmax2', 'Vpdmax2')) |>
  select(-loc) |>
  drop_na()

sylvania_summer = subset(clim_sylvania_month, year>1949)%>% 
  select(starts_with("year"),ends_with("06"), ends_with("07"), ends_with("08"))
sylvania_spring = subset(clim_sylvania_month, year>1949) %>% 
  select(starts_with("year"),ends_with("03"), ends_with("04"), ends_with("05"))
sylvania_fall = subset(clim_sylvania_month, year>1949) %>% 
  select(starts_with("year"),ends_with("09"), ends_with("10"), ends_with("11"))

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
  subset(select = -c(PPT2_06:Vpdmax2_08)) 

#monthly climate plot for sylvania all on one
# ggplot(data = sylvania_summer_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for summer
# ggplot(data = sylvania_summer_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA SUMMER sylvania ####

sylvania_scaled_sum = scale(sylvania_summer_mean[,2:6])

sylvania_summer_pca = prcomp(sylvania_scaled_sum)

# #PCA values for PC1
# sylvania_summer_pca$x[,'PC1']
# ##????
# eigs <- sylvania_summer_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(sylvania_summer_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(sylvania_summer_pca)

###SPRING
sylvania_spring_mean = sylvania_spring %>% 
  rowwise() %>%
  mutate(
    PPT_mean = mean(c_across(starts_with("PPT")), na.rm = TRUE),
    Tmean_mean = mean(c_across(starts_with("Tmean")), na.rm = TRUE),
    Tmin_mean = mean(c_across(starts_with("Tmin")), na.rm = TRUE),
    Tmax_mean = mean(c_across(starts_with("Tmax")), na.rm = TRUE),
    Vpdmax_mean = mean(c_across(starts_with("Vpd")), na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  subset(select = -c(PPT2_03:Vpdmax2_05)) 

#monthly climate plot for sylvania all on one
# ggplot(data = sylvania_spring_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for spring
# ggplot(data = sylvania_spring_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA spring sylvania ####

sylvania_scaled_spring = scale(sylvania_spring_mean[,2:6])

sylvania_spring_pca = prcomp(sylvania_scaled_spring)

# #PCA values for PC1
# sylvania_spring_pca$x[,'PC1']
# ##????
# eigs <- sylvania_spring_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(sylvania_spring_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(sylvania_spring_pca)


####FALL
sylvania_fall_mean = sylvania_fall %>% 
  rowwise() %>%
  mutate(
    PPT_mean = mean(c_across(starts_with("PPT")), na.rm = TRUE),
    Tmean_mean = mean(c_across(starts_with("Tmean")), na.rm = TRUE),
    Tmin_mean = mean(c_across(starts_with("Tmin")), na.rm = TRUE),
    Tmax_mean = mean(c_across(starts_with("Tmax")), na.rm = TRUE),
    Vpdmax_mean = mean(c_across(starts_with("Vpd")), na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  subset(select = -c(PPT2_09:Vpdmax2_11)) 

#monthly climate plot for sylvania all on one
# ggplot(data = sylvania_fall_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for fall
# ggplot(data = sylvania_fall_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA fall sylvania ####

sylvania_scaled_fall = scale(sylvania_fall_mean[,2:6])

sylvania_fall_pca = prcomp(sylvania_scaled_fall)

# #PCA values for PC1
# sylvania_fall_pca$x[,'PC1']
# ##????
# eigs <- sylvania_fall_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(sylvania_fall_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(sylvania_fall_pca)

####sylvania WINTER#####
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

# #PCA values for PC1
# sylvania_winter_pca$x[,'PC1']
# ##????
# eigs <- sylvania_winter_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(sylvania_winter_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(sylvania_winter_pca)

sylvania_PCA = data.frame(year=sylvania_summer_mean$year, site = "SYLVANIA",
                          PCA1_summer = sylvania_summer_pca$x[,'PC1'],
                          PCA2_summer = sylvania_summer_pca$x[,'PC2'],
                          PCA1_spring = sylvania_spring_pca$x[,'PC1'],
                          PCA2_spring = sylvania_spring_pca$x[,'PC2'],
                          PCA1_fall = sylvania_fall_pca$x[,'PC1'],
                          PCA2_fall = sylvania_fall_pca$x[,'PC2'],
                          PCA1_winter = sylvania_winter_pca$x[,'PC1'],
                          PCA2_winter = sylvania_winter_pca$x[,'PC2'])

sylvania_PCA$year = as.numeric(sylvania_PCA$year)
saveRDS(sylvania_PCA, "sylvania_PCA_mean.RDS")

############HARVARD ###########################################
clim_harvard_month <- prism_long |>
  filter(loc == 'HARVARD') |>
  pivot_wider(names_from = 'month',
                     values_from = c('PPT2', 'Tmean2', 'Tmin2',
                                     'Tmax2', 'Vpdmax2')) |>
  select(-loc) |>
  drop_na()

harvard_summer = subset(clim_harvard_month, year>1949)%>% 
  select(starts_with("year"),ends_with("06"), ends_with("07"), ends_with("08"))
harvard_spring = subset(clim_harvard_month, year>1949) %>% 
  select(starts_with("year"),ends_with("03"), ends_with("04"), ends_with("05"))
harvard_fall = subset(clim_harvard_month, year>1949) %>% 
  select(starts_with("year"),ends_with("09"), ends_with("10"), ends_with("11"))

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
  subset(select = -c(PPT2_06:Vpdmax2_08)) 

#monthly climate plot for harvard all on one
# ggplot(data = harvard_summer_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for summer
# ggplot(data = harvard_summer_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA SUMMER harvard ####

harvard_scaled_sum = scale(harvard_summer_mean[,2:6])

harvard_summer_pca = prcomp(harvard_scaled_sum)

# #PCA values for PC1
# harvard_summer_pca$x[,'PC1']
# ##????
# eigs <- harvard_summer_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(harvard_summer_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(harvard_summer_pca)

###SPRING
harvard_spring_mean = harvard_spring %>% 
  rowwise() %>%
  mutate(
    PPT_mean = mean(c_across(starts_with("PPT")), na.rm = TRUE),
    Tmean_mean = mean(c_across(starts_with("Tmean")), na.rm = TRUE),
    Tmin_mean = mean(c_across(starts_with("Tmin")), na.rm = TRUE),
    Tmax_mean = mean(c_across(starts_with("Tmax")), na.rm = TRUE),
    Vpdmax_mean = mean(c_across(starts_with("Vpd")), na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  subset(select = -c(PPT2_03:Vpdmax2_05)) 

#monthly climate plot for harvard all on one
# ggplot(data = harvard_spring_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for spring
# ggplot(data = harvard_spring_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA spring harvard ####

harvard_scaled_spring = scale(harvard_spring_mean[,2:6])

harvard_spring_pca = prcomp(harvard_scaled_spring)

# #PCA values for PC1
# harvard_spring_pca$x[,'PC1']
# ##????
# eigs <- harvard_spring_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(harvard_spring_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(harvard_spring_pca)


####FALL
harvard_fall_mean = harvard_fall %>% 
  rowwise() %>%
  mutate(
    PPT_mean = mean(c_across(starts_with("PPT")), na.rm = TRUE),
    Tmean_mean = mean(c_across(starts_with("Tmean")), na.rm = TRUE),
    Tmin_mean = mean(c_across(starts_with("Tmin")), na.rm = TRUE),
    Tmax_mean = mean(c_across(starts_with("Tmax")), na.rm = TRUE),
    Vpdmax_mean = mean(c_across(starts_with("Vpd")), na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  subset(select = -c(PPT2_09:Vpdmax2_11)) 

#monthly climate plot for harvard all on one
# ggplot(data = harvard_fall_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for fall
# ggplot(data = harvard_fall_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA fall harvard ####

harvard_scaled_fall = scale(harvard_fall_mean[,2:6])

harvard_fall_pca = prcomp(harvard_scaled_fall)

# #PCA values for PC1
# harvard_fall_pca$x[,'PC1']
# ##????
# eigs <- harvard_fall_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(harvard_fall_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(harvard_fall_pca)

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

# #PCA values for PC1
# harvard_winter_pca$x[,'PC1']
# ##????
# eigs <- harvard_winter_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(harvard_winter_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(harvard_winter_pca)

harvard_PCA = data.frame(year=harvard_summer_mean$year, site = "HARVARD",
                         PCA1_summer = harvard_summer_pca$x[,'PC1'],
                         PCA2_summer = harvard_summer_pca$x[,'PC2'],
                         PCA1_spring = harvard_spring_pca$x[,'PC1'],
                         PCA2_spring = harvard_spring_pca$x[,'PC2'],
                         PCA1_fall = harvard_fall_pca$x[,'PC1'],
                         PCA2_fall = harvard_fall_pca$x[,'PC2'],
                         PCA1_winter = harvard_winter_pca$x[,'PC1'],
                         PCA2_winter = harvard_winter_pca$x[,'PC2'])

harvard_PCA$year = as.numeric(harvard_PCA$year)
saveRDS(harvard_PCA, "harvard_PCA_mean.RDS")


############HMC ###########################################
clim_hmc_month <- prism_long |>
  filter(loc == 'HMC') |>
  pivot_wider(names_from = 'month',
              values_from = c('PPT2', 'Tmean2', 'Tmin2',
                              'Tmax2', 'Vpdmax2')) |>
  select(-loc) |>
  drop_na()

hmc_summer = subset(clim_hmc_month, year>1949)%>% 
  select(starts_with("year"),ends_with("06"), ends_with("07"), ends_with("08"))
hmc_spring = subset(clim_hmc_month, year>1949) %>% 
  select(starts_with("year"),ends_with("03"), ends_with("04"), ends_with("05"))
hmc_fall = subset(clim_hmc_month, year>1949) %>% 
  select(starts_with("year"),ends_with("09"), ends_with("10"), ends_with("11"))

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
  subset(select = -c(PPT2_06:Vpdmax2_08)) 

#monthly climate plot for hmc all on one
# ggplot(data = hmc_summer_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for summer
# ggplot(data = hmc_summer_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA SUMMER hmc ####

hmc_scaled_sum = scale(hmc_summer_mean[,2:6])

hmc_summer_pca = prcomp(hmc_scaled_sum)

# #PCA values for PC1
# hmc_summer_pca$x[,'PC1']
# ##????
# eigs <- hmc_summer_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(hmc_summer_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(hmc_summer_pca)

###SPRING
hmc_spring_mean = hmc_spring %>% 
  rowwise() %>%
  mutate(
    PPT_mean = mean(c_across(starts_with("PPT")), na.rm = TRUE),
    Tmean_mean = mean(c_across(starts_with("Tmean")), na.rm = TRUE),
    Tmin_mean = mean(c_across(starts_with("Tmin")), na.rm = TRUE),
    Tmax_mean = mean(c_across(starts_with("Tmax")), na.rm = TRUE),
    Vpdmax_mean = mean(c_across(starts_with("Vpd")), na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  subset(select = -c(PPT2_03:Vpdmax2_05)) 

#monthly climate plot for hmc all on one
# ggplot(data = hmc_spring_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for spring
# ggplot(data = hmc_spring_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA spring hmc ####

hmc_scaled_spring = scale(hmc_spring_mean[,2:6])

hmc_spring_pca = prcomp(hmc_scaled_spring)

# #PCA values for PC1
# hmc_spring_pca$x[,'PC1']
# ##????
# eigs <- hmc_spring_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(hmc_spring_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(hmc_spring_pca)


####FALL
hmc_fall_mean = hmc_fall %>% 
  rowwise() %>%
  mutate(
    PPT_mean = mean(c_across(starts_with("PPT")), na.rm = TRUE),
    Tmean_mean = mean(c_across(starts_with("Tmean")), na.rm = TRUE),
    Tmin_mean = mean(c_across(starts_with("Tmin")), na.rm = TRUE),
    Tmax_mean = mean(c_across(starts_with("Tmax")), na.rm = TRUE),
    Vpdmax_mean = mean(c_across(starts_with("Vpd")), na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  subset(select = -c(PPT2_09:Vpdmax2_11)) 

#monthly climate plot for hmc all on one
# ggplot(data = hmc_fall_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for fall
# ggplot(data = hmc_fall_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA fall hmc ####

hmc_scaled_fall = scale(hmc_fall_mean[,2:6])

hmc_fall_pca = prcomp(hmc_scaled_fall)

# #PCA values for PC1
# hmc_fall_pca$x[,'PC1']
# ##????
# eigs <- hmc_fall_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(hmc_fall_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(hmc_fall_pca)

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

# #PCA values for PC1
# hmc_winter_pca$x[,'PC1']
# ##????
# eigs <- hmc_winter_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(hmc_winter_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(hmc_winter_pca)

hmc_PCA = data.frame(year=hmc_summer_mean$year, site = "HMC",
                     PCA1_summer = hmc_summer_pca$x[,'PC1'],
                     PCA2_summer = hmc_summer_pca$x[,'PC2'],
                     PCA1_spring = hmc_spring_pca$x[,'PC1'],
                     PCA2_spring = hmc_spring_pca$x[,'PC2'],
                     PCA1_fall = hmc_fall_pca$x[,'PC1'],
                     PCA2_fall = hmc_fall_pca$x[,'PC2'],
                     PCA1_winter = hmc_winter_pca$x[,'PC1'],
                     PCA2_winter = hmc_winter_pca$x[,'PC2'])

hmc_PCA$year = as.numeric(hmc_PCA$year)
saveRDS(hmc_PCA, "hmc_PCA_mean.RDS")


#################ALL SITES######################
#################ALL SITES######################
#PCA1/2 for all sites for summer and winter 
pca_sites = data.frame(year = character(0), 
                       site = character(0), PCA1_summer = numeric(0), 
                       PCA2_summer = numeric(0),PCA1_spring = numeric(0), 
                       PCA2_spring = numeric(0), PCA1_fall = numeric(0), 
                       PCA2_fall = numeric(0),
                       PCA1_winter = numeric(0), 
                       PCA2_winter = numeric(0))

pca_sites = rbind(goose_PCA, rooster_PCA, nrp_PCA, harvard_PCA, sylvania_PCA, hmc_PCA)
saveRDS(pca_sites, "pca_sites_mean.RDS")

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