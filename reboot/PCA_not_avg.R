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
prism_long$year = as.numeric(prism_long$year)

#water year function
# assign water year
wtr_yr <- function(df, start_month=12) {
  # Year offset
  offset = ifelse(as.numeric(df$month) >= start_month,  1, 0)
  # Water year
  adj.year = as.numeric(df$year) + offset
  # Return the water year
  adj.year
}

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


goose_summer = subset(clim_goose_month, year>1949)%>% 
  select(starts_with("year"),ends_with("06"), ends_with("07"), ends_with("08"))
# goose_summer_time = goose_summer
# goose_summer_time$year = 1:nrow(goose_summer)
#goose_summer_long = melt(goose_summer, id.vars = "year")
goose_spring = subset(clim_goose_month, year>1949) %>% 
  select(starts_with("year"),ends_with("03"), ends_with("04"), ends_with("05"))
goose_fall = subset(clim_goose_month, year>1949) %>% 
  select(starts_with("year"),ends_with("09"), ends_with("10"), ends_with("11"))


###PCA SUMMER GOOSE ####

goose_scaled_sum = scale(goose_summer[,2:ncol(goose_summer)])
goose_scaled_spring = scale(goose_spring[,2:ncol(goose_spring)])
goose_scaled_fall = scale(goose_fall[,2:ncol(goose_fall)])


#summer
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

#Spring#########################
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


##FALL##################
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

goose_long = pivot_longer(clim_goose_month, cols=2:ncol(clim_goose_month))
goose_long$variable = sapply(strsplit(goose_long$name, split='_'), function(x) x[1])
goose_long$month = sapply(strsplit(goose_long$name, split='_'), function(x) x[2])
goose_long$month = as.numeric(goose_long$month)


goose_long$water_year <- wtr_yr(goose_long)

goose_long = goose_long[which((goose_long$month %in% c(12, 1, 2))),]

goose_winter = pivot_wider(goose_long, id_cols = 'water_year')


#
# ###PCA WINTER
goose_winter = goose_winter[-c(1, 64),]
#
#
###PCA WINTER goose
goose_scaled_winter = scale(goose_winter[,2:16])

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

goose_PCA = data.frame(year=goose_summer$year, site = "GOOSE",
                       PCA1_summer = goose_summer_pca$x[,'PC1'],
                       PCA2_summer = goose_summer_pca$x[,'PC2'],
                       PCA1_spring = goose_spring_pca$x[,'PC1'],
                       PCA2_spring = goose_spring_pca$x[,'PC2'],
                       PCA1_fall = goose_fall_pca$x[,'PC1'],
                       PCA2_fall = goose_fall_pca$x[,'PC2'],
                       PCA1_winter = goose_winter_pca$x[,'PC1'],
                       PCA2_winter = goose_winter_pca$x[,'PC2'])

saveRDS(goose_PCA, "goose_PCA.RDS")


############NRP ###########################################
clim_nrp_month <- prism_long |>
  filter(loc == 'NRP') |>
  pivot_wider(names_from = 'month',
              values_from = c('PPT2', 'Tmean2', 'Tmin2',
                              'Tmax2', 'Vpdmax2')) |>
  select(-loc) |>
  drop_na()

nrp_summer = subset(clim_nrp_month, year>1949) %>% 
  select(starts_with("year"),ends_with("06"), ends_with("07"), ends_with("08"))
# nrp_summer_time = nrp_summer
# nrp_summer_time$year = 1:nrow(nrp_summer)
#nrp_summer_long = melt(nrp_summer, id.vars = "year")
nrp_spring = subset(clim_nrp_month, year>1949) %>% 
  select(starts_with("year"),ends_with("03"), ends_with("04"), ends_with("05"))
nrp_fall = subset(clim_nrp_month, year>1949) %>% 
  select(starts_with("year"),ends_with("09"), ends_with("10"), ends_with("11"))


#monthly climate plot for nrp all on one
# ggplot(data = nrp_summer_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for summer
# ggplot(data = nrp_summer_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA SUMMER NRP ####

nrp_scaled_sum = scale(nrp_summer[,2:ncol(nrp_summer)])
nrp_scaled_spring = scale(nrp_spring[,2:ncol(nrp_spring)])
nrp_scaled_fall = scale(nrp_fall[,2:ncol(nrp_fall)])

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


##########SPring
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


###FALL###
nrp_fall_pca = prcomp(nrp_scaled_fall)

# #PCA values for PC1
# nrp_spring_pca$x[,'PC1']
# ##????
# eigs <- nrp_fall_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(nrp_fall_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(nrp_fall_pca)

####NRP WINTER#####


####Winter
nrp_long = pivot_longer(clim_nrp_month, cols=2:ncol(clim_nrp_month))
nrp_long$variable = sapply(strsplit(nrp_long$name, split='_'), function(x) x[1])
nrp_long$month = sapply(strsplit(nrp_long$name, split='_'), function(x) x[2])
nrp_long$month = as.numeric(nrp_long$month)


nrp_long$water_year <- wtr_yr(nrp_long)

nrp_long = nrp_long[which((nrp_long$month %in% c(12, 1, 2))),]

nrp_winter = pivot_wider(nrp_long, id_cols = 'water_year')


#
# ###PCA WINTER
nrp_winter = nrp_winter[-c(1, 64),]
#
#
###PCA WINTER nrp
nrp_scaled_winter = scale(nrp_winter[,2:16])


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

nrp_PCA = data.frame(year=nrp_summer$year, site = "NRP",
                     PCA1_summer = nrp_summer_pca$x[,'PC1'],
                     PCA2_summer = nrp_summer_pca$x[,'PC2'],
                     PCA1_spring = nrp_spring_pca$x[,'PC1'],
                     PCA2_spring = nrp_spring_pca$x[,'PC2'],
                     PCA1_fall = nrp_fall_pca$x[,'PC1'],
                     PCA2_fall = nrp_fall_pca$x[,'PC2'],
                     PCA1_winter = nrp_winter_pca$x[,'PC1'],
                     PCA2_winter = nrp_winter_pca$x[,'PC2'])

saveRDS(nrp_PCA, "nrp_PCA.RDS")

############ROOSTER ###########################################
clim_rooster_month <- prism_long |>
  filter(loc == 'ROOSTER') |>
  pivot_wider(names_from = 'month',
              values_from = c('PPT2', 'Tmean2', 'Tmin2',
                              'Tmax2', 'Vpdmax2')) |>
  select(-loc) |>
  drop_na()

rooster_summer = subset(clim_rooster_month, year>1949) %>% 
  select(starts_with("year"),ends_with("06"), ends_with("07"), ends_with("08"))
# rooster_summer_time = rooster_summer
# rooster_summer_time$year = 1:nrow(rooster_summer)
#rooster_summer_long = melt(rooster_summer, id.vars = "year")

rooster_spring = subset(clim_rooster_month, year>1949) %>% 
  select(starts_with("year"),ends_with("03"), ends_with("04"), ends_with("05"))
rooster_fall = subset(clim_rooster_month, year>1949) %>% 
  select(starts_with("year"),ends_with("09"), ends_with("10"), ends_with("11"))


###PCA SUMMER rooster ####

rooster_scaled_sum = scale(rooster_summer[,2:ncol(rooster_summer)])
rooster_scaled_spring = scale(rooster_spring[,2:ncol(rooster_spring)])
rooster_scaled_fall = scale(rooster_fall[,2:ncol(rooster_fall)])



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


####SPRING
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

###FALL####
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

####ROOSTER WINTER#####


####Winter
rooster_long = pivot_longer(clim_rooster_month, cols=2:ncol(clim_rooster_month))
rooster_long$variable = sapply(strsplit(rooster_long$name, split='_'), function(x) x[1])
rooster_long$month = sapply(strsplit(rooster_long$name, split='_'), function(x) x[2])
rooster_long$month = as.numeric(rooster_long$month)


rooster_long$water_year <- wtr_yr(rooster_long)

rooster_long = rooster_long[which((rooster_long$month %in% c(12, 1, 2))),]

rooster_winter = pivot_wider(rooster_long, id_cols = 'water_year')


# 
# ###PCA WINTER 
rooster_winter = rooster_winter[-c(1, 64),]
# 
# 
###PCA WINTER rooster
rooster_scaled_winter = scale(rooster_winter[,2:16])

rooster_winter_pca = prcomp(rooster_scaled_winter)

# #PCA values for PC1
# rooster_winter_pca$x[,'PC1']
# 
# ##????
# eigs <- rooster_winter_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(rooster_winter_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(rooster_winter_pca)

rooster_PCA = data.frame(year=rooster_summer$year, site = "ROOSTER",
                         PCA1_summer = rooster_summer_pca$x[,'PC1'],
                         PCA2_summer = rooster_summer_pca$x[,'PC2'],
                         PCA1_spring = rooster_spring_pca$x[,'PC1'],
                         PCA2_spring = rooster_spring_pca$x[,'PC2'],
                         PCA1_fall = rooster_fall_pca$x[,'PC1'],
                         PCA2_fall = rooster_fall_pca$x[,'PC2'],
                         PCA1_winter = rooster_winter_pca$x[,'PC1'],
                         PCA2_winter = rooster_winter_pca$x[,'PC2'])

saveRDS(rooster_PCA, "rooster_PCA.RDS")


############SYLVANIA ###########################################
clim_sylvania_month <- prism_long |>
  filter(loc == 'SYLVANIA') |>
  pivot_wider(names_from = 'month',
              values_from = c('PPT2', 'Tmean2', 'Tmin2',
                              'Tmax2', 'Vpdmax2')) |>
  select(-loc) |>
  drop_na()

sylvania_summer = subset(clim_sylvania_month, year>1949) %>% 
  select(starts_with("year"),ends_with("06"), ends_with("07"), ends_with("08"))

sylvania_spring = subset(clim_sylvania_month, year>1949) %>% 
  select(starts_with("year"),ends_with("03"), ends_with("04"), ends_with("05"))
sylvania_fall = subset(clim_sylvania_month, year>1949) %>% 
  select(starts_with("year"),ends_with("09"), ends_with("10"), ends_with("11"))



###PCA SUMMER sylvania ####

sylvania_scaled_sum = scale(sylvania_summer[,2:ncol(sylvania_summer)])
sylvania_scaled_spring = scale(sylvania_spring[,2:ncol(sylvania_spring)])
sylvania_scaled_fall = scale(sylvania_fall[,2:ncol(sylvania_fall)])



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

###Spring
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


###FAll
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

####Winter
sylvania_long = pivot_longer(clim_sylvania_month, cols=2:ncol(clim_sylvania_month))
sylvania_long$variable = sapply(strsplit(sylvania_long$name, split='_'), function(x) x[1])
sylvania_long$month = sapply(strsplit(sylvania_long$name, split='_'), function(x) x[2])
sylvania_long$month = as.numeric(sylvania_long$month)


sylvania_long$water_year <- wtr_yr(sylvania_long)

sylvania_long = sylvania_long[which((sylvania_long$month %in% c(12, 1, 2))),]

sylvania_winter = pivot_wider(sylvania_long, id_cols = 'water_year')


# 
# ###PCA WINTER 
sylvania_winter = sylvania_winter[-c(1, 64),]
# 
# 
###PCA WINTER sylvania
sylvania_scaled_winter = scale(sylvania_winter[,2:16])

sylvania_winter_pca = prcomp(sylvania_scaled_winter)
# #PCA values for PC1
# sylvania_winter_pca$x[,'PC1']
# 
# ##????
# eigs <- sylvania_winter_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))
# 
#shows variables per component
fviz_eig(sylvania_winter_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(sylvania_winter_pca)

sylvania_PCA = data.frame(year=sylvania_summer$year, site = "SYLVANIA",
                          PCA1_summer = sylvania_summer_pca$x[,'PC1'],
                          PCA2_summer = sylvania_summer_pca$x[,'PC2'],
                          PCA1_spring = sylvania_spring_pca$x[,'PC1'],
                          PCA2_spring = sylvania_spring_pca$x[,'PC2'],
                          PCA1_fall = sylvania_fall_pca$x[,'PC1'],
                          PCA2_fall = sylvania_fall_pca$x[,'PC2'],
                          PCA1_winter = sylvania_winter_pca$x[,'PC1'],
                          PCA2_winter = sylvania_winter_pca$x[,'PC2'])

saveRDS(sylvania_PCA, "sylvania_PCA.RDS")
############HARVARD ###########################################
clim_harvard_month <- prism_long |>
  filter(loc == 'HARVARD') |>
  pivot_wider(names_from = 'month',
              values_from = c('PPT2', 'Tmean2', 'Tmin2',
                              'Tmax2', 'Vpdmax2')) |>
  select(-loc) |>
  drop_na()

harvard_summer = subset(clim_harvard_month, year>1949) %>% 
  select(starts_with("year"),ends_with("06"), ends_with("07"), ends_with("08"))
# harvard_summer_time = harvard_summer
# harvard_summer_time$year = 1:nrow(harvard_summer)
#harvard_summer_long = melt(harvard_summer, id.vars = "year")
harvard_spring = subset(clim_harvard_month, year>1949) %>% 
  select(starts_with("year"),ends_with("03"), ends_with("04"), ends_with("05"))
harvard_fall = subset(clim_harvard_month, year>1949) %>% 
  select(starts_with("year"),ends_with("09"), ends_with("10"), ends_with("11"))


#monthly climate plot for harvard all on one
# ggplot(data = harvard_summer_long)+
#   geom_line(aes(x= year, y = value,colour = variable))
# #mean of climate variables for summer
# ggplot(data = harvard_summer_mean)+
#   geom_line(aes(x= year, y = value,colour = variable))

###PCA SUMMER harvard ####

harvard_scaled_sum = scale(harvard_summer[,2:ncol(harvard_summer)])
harvard_scaled_spring = scale(harvard_spring[,2:ncol(harvard_spring)])
harvard_scaled_fall = scale(harvard_fall[,2:ncol(harvard_fall)])


harvard_summer_pca = prcomp(harvard_scaled_sum)
#PCA values for PC1
# harvard_summer_pca$x[,'PC1']
# 
# ##????
# eigs <- harvard_summer_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(harvard_summer_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(harvard_summer_pca)


###SPring##
harvard_spring_pca = prcomp(harvard_scaled_spring)

# #PCA values for PC1
# harvard_spring_pca$x[,'PC1']
# 
# ##????
# eigs <- harvard_spring_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(harvard_spring_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(harvard_spring_pca)


###FALL
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

####harvard winter
harvard_long = pivot_longer(clim_harvard_month, cols=2:ncol(clim_harvard_month))
harvard_long$variable = sapply(strsplit(harvard_long$name, split='_'), function(x) x[1])
harvard_long$month = sapply(strsplit(harvard_long$name, split='_'), function(x) x[2])
harvard_long$month = as.numeric(harvard_long$month)

#adding column for water year 
harvard_long$water_year <- wtr_yr(harvard_long)

harvard_long = harvard_long[which((harvard_long$month %in% c(12, 1, 2))),]

harvard_winter = pivot_wider(harvard_long, id_cols = 'water_year')



###PCA WINTER harvard
#deleting NA values from row 1/63, year 1949/2012
harvard_winter = harvard_winter[-c(1, 64),]

harvard_scaled_winter = scale(harvard_winter[,2:16])

harvard_winter_pca = prcomp(harvard_scaled_winter)
#PCA values for PC1
# harvard_winter_pca$x[,'PC1']
# 
# ##????
# eigs <- harvard_winter_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))
# 
# #shows variables per component
fviz_eig(harvard_winter_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(harvard_winter_pca)

harvard_PCA = data.frame(year=harvard_summer$year, site = "HARVARD",
                         PCA1_summer = harvard_summer_pca$x[,'PC1'],
                         PCA2_summer = harvard_summer_pca$x[,'PC2'],
                         PCA1_spring = harvard_spring_pca$x[,'PC1'],
                         PCA2_spring = harvard_spring_pca$x[,'PC2'],
                         PCA1_fall = harvard_fall_pca$x[,'PC1'],
                         PCA2_fall = harvard_fall_pca$x[,'PC2'],
                         PCA1_winter = harvard_winter_pca$x[,'PC1'],
                         PCA2_winter = harvard_winter_pca$x[,'PC2'])

saveRDS(harvard_PCA, "harvard_PCA.RDS")

############HMC ###########################################
clim_hmc_month <- prism_long |>
  filter(loc == 'HMC') |>
  pivot_wider(names_from = 'month',
              values_from = c('PPT2', 'Tmean2', 'Tmin2',
                              'Tmax2', 'Vpdmax2')) |>
  select(-loc) |>
  drop_na()

hmc_summer = subset(clim_hmc_month, year>1949) %>% 
  select(starts_with("year"),ends_with("06"), ends_with("07"), ends_with("08"))
hmc_spring = subset(clim_hmc_month, year>1949)%>% 
  select(starts_with("year"),ends_with("03"), ends_with("04"), ends_with("05"))
hmc_fall = subset(clim_hmc_month, year>1949) %>% 
  select(starts_with("year"),ends_with("09"), ends_with("10"), ends_with("11"))



###PCA SUMMER hmc ####

hmc_scaled_sum = scale(hmc_summer[,2:ncol(hmc_summer)])
hmc_scaled_spring = scale(hmc_spring[,2:ncol(hmc_spring)])
hmc_scaled_fall = scale(hmc_fall[,2:ncol(hmc_fall)])


hmc_summer_pca = prcomp(hmc_scaled_sum)
#PCA values for PC1
# hmc_summer_pca$x[,'PC1']
# 
# 
# ##????
# eigs <- hmc_summer_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(hmc_summer_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(hmc_summer_pca)


###SPRING\
hmc_spring_pca = prcomp(hmc_scaled_spring)
#PCA values for PC1
#hmc_spring_pca$x[,'PC1']


##????
#eigs <- hmc_spring_pca$sdev^2
#rbind(SD = sqrt(eigs),
#      Proportion = eigs/sum(eigs),
#      Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(hmc_spring_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(hmc_spring_pca)

#FALL###########
hmc_fall_pca = prcomp(hmc_scaled_fall)
#PCA values for PC1
# hmc_fall_pca$x[,'PC1']
# 
# 
# ##????
# eigs <- hmc_fall_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(hmc_fall_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(hmc_fall_pca)

# ####hmc WINTER#####
hmc_long = pivot_longer(clim_hmc_month, cols=2:ncol(clim_hmc_month))
hmc_long$variable = sapply(strsplit(hmc_long$name, split='_'), function(x) x[1])
hmc_long$month = sapply(strsplit(hmc_long$name, split='_'), function(x) x[2])
hmc_long$month = as.numeric(hmc_long$month)


hmc_long$water_year <- wtr_yr(hmc_long)


hmc_long = hmc_long[which((hmc_long$month %in% c(12, 1, 2))),]

hmc_winter = pivot_wider(hmc_long, id_cols = 'water_year')


# 
# ###PCA WINTER hmc
hmc_winter = hmc_winter[-c(1, 64),]

hmc_scaled_winter = scale(hmc_winter[,2:16])

hmc_winter_pca = prcomp(hmc_scaled_winter)
#PCA values for PC1
# hmc_winter_pca$x[,'PC1']
# 
# ##????
# eigs <- hmc_winter_pca$sdev^2
# rbind(SD = sqrt(eigs),
#       Proportion = eigs/sum(eigs),
#       Cumulative = cumsum(eigs)/sum(eigs))

#shows variables per component
fviz_eig(hmc_winter_pca, addlabels = TRUE)

#biplot
fviz_pca_biplot(hmc_winter_pca)

hmc_PCA = data.frame(year=hmc_summer$year, site = "HMC",
                     PCA1_summer = hmc_summer_pca$x[,'PC1'],
                     PCA2_summer = hmc_summer_pca$x[,'PC2'],
                     PCA1_spring = hmc_spring_pca$x[,'PC1'],
                     PCA2_spring = hmc_spring_pca$x[,'PC2'],
                     PCA1_fall = hmc_fall_pca$x[,'PC1'],
                     PCA2_fall = hmc_fall_pca$x[,'PC2'],
                     PCA1_winter = hmc_winter_pca$x[,'PC1'],
                     PCA2_winter = hmc_winter_pca$x[,'PC2'])

saveRDS(hmc_PCA, "hmc_PCA.RDS")


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
saveRDS(pca_sites, "pca_sites.RDS")


