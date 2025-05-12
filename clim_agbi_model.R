library(broom)
library(dplyr)
library(tidyr)
library(reshape2)
library(purrr)
library(ggplot2)

#arima(xreg = INDEPENDENT.VARS)

tree_yr <- function(df, start_month=12) {
  # Year offset
  offset = ifelse(as.numeric(df$month) >= start_month,  1, 0)
  # Water year
  adj.year = as.numeric(df$year) + offset
  # Return the water year
  adj.year
}

AGBI_data = readRDS("AGBI_taxon_data.RDS")

load('climate/prism_clim.RData')
clim_data = prism_long

#removing Vpdmin from dataset
df = subset(clim_data, select = -c(Vpdmin2) )

#renaming columns
clim_data = df %>% 
  dplyr::rename(site = loc, PPT = PPT2, Tmean = Tmean2, Tmin = Tmin2, Tmax = Tmax2, 
                 Vpdmax = Vpdmax2)
clim_data$year <- as.numeric(clim_data$year)

#using tree_yr function to make prev and current year 
clim_data$year_tree = tree_yr(clim_data, start_month=9)

clim_data_current = clim_data[which(clim_data$year == clim_data$year_tree),]
clim_data_prev = clim_data[which(clim_data$year != clim_data$year_tree),]


# #putting the climat variables in wide format
# clim_wide =  pivot_wider(data = clim_data,
#                          names_from = month, 
#                          values_from = c(PPT, Tmean, Tmin, Tmax, Vpdmax))

#putting the climate variables in wide format
clim_wide_current =  pivot_wider(data = clim_data_current,
                         names_from = month, 
                         values_from = c(PPT, Tmean, Tmin, Tmax, Vpdmax))

#putting the climate variables in wide format
clim_wide_prev =  pivot_wider(data = subset(clim_data_prev, select = -year),
                                 names_from = month, 
                                 values_from = c(PPT, Tmean, Tmin, Tmax, Vpdmax))
clim_wide_prev = rename(clim_wide_prev, year = year_tree)

#climate variables in wide format seperated tree year
clim_wide = merge(clim_wide_current, clim_wide_prev, by = c('site', 'year'))


AGBI_data = AGBI_data %>%
  group_by(site, taxon) %>%
  arrange(site, taxon, year) %>%
  mutate(AGBI.mean.prev1 = lag(AGBI.mean, n=1),
         AGBI.mean.prev2 = lag(AGBI.mean, n=2))



# #long format
# clim_agbi_long <- AGBI_data %>% 
#   left_join(clim_data, by = c('year', 'site'))

#wide format of climate variables with AGBI by tree_year
clim_agbi <- AGBI_data %>% 
  left_join(clim_wide, by = c('year', 'site'))



############################################################################
# months = c(paste0('0', seq(1,9)), seq(10, 12))
# 
# predictor_clim_names = paste(rep(c('PPT', 'Tmean', 'Tmin', 'Tmax', 'Vpdmax'), each=12), rep(months, times = 5), sep='_')

months = c(paste0('0', c(3, 6, 9)), 12)

predictor_clim_names = paste(rep(c('PPT', 'Tmean', 'Tmin', 'Tmax', 'Vpdmax'), each=4), rep(months, times = 5), sep='_')

# 
# predictor_clim_names_keep = c()

predictor_lag_names = c('AGBI.mean.prev1', 'AGBI.mean.prev2')

predictor_names = predictor_clim_names 

models_clim <- clim_agbi %>% 
  group_by(site, taxon) %>%
  do({
    # predictors <- names(.)[13:72]
    # print(predictors)
    # print(length(predictors))
    predictors <- predictor_clim_names
    formula <- reformulate(predictors, response = "AGBI.mean")
    print(formula)
    model <- lm(formula, data = .)
    tibble(mod = list(model))
  })


#rsq = models %>% rowwise() %>% mutate(mod$r.squared)

summary(models_clim[[3]][1][[1]])

#R2 for the models
rsq_values_clim <- models_clim %>%
  mutate(glance = map(mod, glance)) %>%
  unnest(glance) %>%
  dplyr::select(site, taxon, r.squared)

#adjusted R2 for the models
rsq_adj_values_clim <- models_clim %>%
  mutate(glance = map(mod, glance)) %>%
  unnest(glance) %>%
  dplyr::select(site, taxon, adj.r.squared)


ggplot(data=rsq_values_clim) +
  geom_point(aes(x=site, y = r.squared, colour=taxon))

ggplot(data=rsq_values_clim) +
  geom_point(aes(y=taxon, x = r.squared, colour=site))


ggplot(data=rsq_adj_values_clim) +
  geom_point(aes(y=taxon, x = adj.r.squared, colour=site))

#
# now include prev year AGBI
#

predictor_names = c(predictor_clim_names, 'AGBI.mean.prev1') 

# below not working, prob with AGBI.mean.prev1

ggplot(data=clim_agbi) +
  geom_point(aes(x=AGBI.mean, y=AGBI.mean.prev1))
ggplot(data=clim_agbi) +
  geom_point(aes(x=AGBI.mean, y=AGBI.mean.prev2))


models_clim_ar1 <- clim_agbi %>% 
  group_by(site, taxon) %>%
  do({
    # predictors <- names(.)[13:72]
    # print(predictors)
    # print(length(predictors))
    predictors <- predictor_names
    print(predictors)
    formula <- reformulate(predictors, response = "AGBI.mean")
    print(formula)
    model <- lm(formula, data = ., na.action = na.exclude)
    tibble(mod = list(model))
  })


#rsq = models %>% rowwise() %>% mutate(mod$r.squared)

summary(models_clim_ar1[[3]][1][[1]])

summary(models_clim_ar1[[3]][3][[1]])

#R2 for the models
rsq_values_clim <- models_clim_ar1 %>%
  mutate(glance = map(mod, glance)) %>%
  unnest(glance) %>%
  dplyr::select(site, taxon, r.squared)

#adjusted R2 for the models
rsq_adj_values_clim <- models_clim_ar1 %>%
  mutate(glance = map(mod, glance)) %>%
  unnest(glance) %>%
  dplyr::select(site, taxon, adj.r.squared)

ggplot(data=rsq_values_clim) +
  geom_point(aes(x=site, y = r.squared, colour=taxon))

ggplot(data=rsq_values_clim) +
  geom_point(aes(y=taxon, x = r.squared, colour=site))

ggplot(data=rsq_adj_values_clim) +
  geom_point(aes(y=taxon, x = adj.r.squared, colour=site))

## 
AGBI_data_site = readRDS("AGBI_site_data.RDS")

AGBI_data_site = AGBI_data_site %>%
  group_by(site) %>%
  arrange(site, year) %>%
  mutate(AGBI.mean.prev1 = lag(AGBI.mean, n=1),
         AGBI.mean.prev2 = lag(AGBI.mean, n=2))


#wide format
clim_agbi_site <- AGBI_data_site %>% 
  left_join(clim_wide, by = c('year', 'site'))


# months = c(paste0('0', seq(1,9)), seq(10, 12))
# 
# predictor_clim_names = paste(rep(c('PPT', 'Tmean', 'Tmin', 'Tmax', 'Vpdmax'), each=12), rep(months, times = 5), sep='_')

months = c(paste0('0', c(3, 6, 9)), 12)

seasons ()

predictor_clim_names = paste(rep(c('PPT', 'Tmean', 'Tmin', 'Tmax', 'Vpdmax'), each=4), rep(months, times = 5), sep='_')

# 
# predictor_clim_names_keep = c()

predictor_lag_names = c('AGBI.mean.prev1', 'AGBI.mean.prev2')

predictor_names = predictor_clim_names 

models_clim_site <- clim_agbi_site %>% 
  group_by(site) %>%
  do({
    # predictors <- names(.)[13:72]
    # print(predictors)
    # print(length(predictors))
    predictors <- predictor_clim_names
    formula <- reformulate(predictors, response = "AGBI.mean")
    print(formula)
    model <- lm(formula, data = .)
    tibble(mod = list(model))
  })


#rsq = models %>% rowwise() %>% mutate(mod$r.squared)

summary(models_clim_site[[2]][1][[1]])

#R2 for the models
rsq_values_clim_site <- models_clim_site %>%
  mutate(glance = map(mod, glance)) %>%
  unnest(glance) %>%
  dplyr::select(site, r.squared)

#adjusted R2 for the models
rsq_adj_values_clim_site <- models_clim_site %>%
  mutate(glance = map(mod, glance)) %>%
  unnest(glance) %>%
  dplyr::select(site, adj.r.squared)


ggplot(data=rsq_values_clim_site) +
  geom_point(aes(x=site, y = r.squared))

# ggplot(data=rsq_values_clim_site) +
#   geom_point(aes(y=taxon, x = r.squared, colour=site))
# 
# 
# ggplot(data=rsq_adj_values_clim_site) +
#   geom_point(aes(y=taxon, x = adj.r.squared, colour=site))

#
# now include prev year AGBI
#

predictor_names = c(predictor_clim_names, 'AGBI.mean.prev1') 

# below not working, prob with AGBI.mean.prev1

ggplot(data=clim_agbi_site) +
  geom_point(aes(x=AGBI.mean, y=AGBI.mean.prev1))
ggplot(data=clim_agbi_site) +
  geom_point(aes(x=AGBI.mean, y=AGBI.mean.prev2))


models_clim_ar1_site <- clim_agbi_site %>% 
  group_by(site) %>%
  do({
    # predictors <- names(.)[13:72]
    # print(predictors)
    # print(length(predictors))
    predictors <- predictor_names
    print(predictors)
    formula <- reformulate(predictors, response = "AGBI.mean")
    print(formula)
    model <- lm(formula, data = ., na.action = na.exclude)
    tibble(mod = list(model))
  })


#rsq = models %>% rowwise() %>% mutate(mod$r.squared)

summary(models_clim_ar1_site[[2]][1][[1]])

summary(models_clim_ar1_site[[2]][3][[1]])

#R2 for the models
rsq_values_clim_site <- models_clim_ar1_site %>%
  mutate(glance = map(mod, glance)) %>%
  unnest(glance) %>%
  dplyr::select(site, r.squared)

#adjusted R2 for the models
rsq_adj_values_clim_site <- models_clim_ar1_site %>%
  mutate(glance = map(mod, glance)) %>%
  unnest(glance) %>%
  dplyr::select(site, adj.r.squared)

ggplot(data=rsq_values_clim_site) +
  geom_point(aes(x=site, y = r.squared))

# ggplot(data=rsq_values_clim_site) +
#   geom_point(aes(y=taxon, x = r.squared, colour=site))
# 
# ggplot(data=rsq_adj_values_clim) +
#   geom_point(aes(y=taxon, x = adj.r.squared, colour=site))


## merge species and site r-squared

foo = data.frame(rsq_values_clim, level='SPECIES')

bar = data.frame(rsq_values_clim_site, taxon='SITE', level='SITE')

rsq_merged = rbind(foo[,c('site', 'taxon', 'level', 'r.squared')], bar[,c('site', 'taxon', 'level', 'r.squared')])

ggplot(data=rsq_merged) +
  geom_point(aes(x=site, y=r.squared, colour=level))


#taking the sum of winter ppt grouping by year, site, taxon
#winter being prev december, jan, feb
# ppt_winter = clim_agbi %>%
#   group_by(year, site, taxon) %>% 
#   mutate(ppt_winter = rowSums(dplyr::pick('PPT_12', 'PPT_01', 'PPT_02')),
#          Vpdmax_winter = rowMeans(dplyr::pick('Vpdmax_12', 'Vpdmax_01','Vpdmax_02' )))

#######################################################################################


#Across seasons, sum PPT, mean Tmean, max Tmax, min Tmin
clim_seasons = clim_agbi %>% 
  group_by(year, site, taxon) %>% 
  mutate(PPT_winter = sum(dplyr::pick('PPT_12', 'PPT_01', 'PPT_02')),
         PPT_spring = sum(dplyr::pick('PPT_03', 'PPT_04', 'PPT_05')),
         PPT_summer = sum(dplyr::pick('PPT_06', 'PPT_07', 'PPT_08')),
         PPT_fall = sum(dplyr::pick('PPT_09', 'PPT_10', 'PPT_11')),
         Vpdmax_winter = rowMeans(dplyr::pick('Vpdmax_12', 'Vpdmax_01','Vpdmax_02')),
         Vpdmax_spring = rowMeans(dplyr::pick('Vpdmax_03', 'Vpdmax_04','Vpdmax_05')),
         Vpdmax_summer = rowMeans(dplyr::pick('Vpdmax_06', 'Vpdmax_07','Vpdmax_08')),
         Vpdmax_fall = rowMeans(dplyr::pick('Vpdmax_09', 'Vpdmax_10','Vpdmax_11')),
         Tmin_winter = min(dplyr::pick('Tmin_12', 'Tmin_01', 'Tmin_02')),
         Tmin_spring = min(dplyr::pick('Tmin_03', 'Tmin_04', 'Tmin_05')),
         Tmin_summer = min(dplyr::pick('Tmin_06', 'Tmin_07', 'Tmin_08')),
         Tmin_fall = min(dplyr::pick('Tmin_09', 'Tmin_10', 'Tmin_11')),
         Tmax_winter = max(dplyr::pick('Tmax_12', 'Tmax_01', 'Tmax_02')),
         Tmax_spring = max(dplyr::pick('Tmax_03', 'Tmax_04', 'Tmax_05')),
         Tmax_summer = max(dplyr::pick('Tmax_06', 'Tmax_07', 'Tmax_08')),
         Tmax_fall = max(dplyr::pick('Tmax_09', 'Tmax_10', 'Tmax_11')),
         Tmean_winter = rowMeans(dplyr::pick('Tmean_12', 'Tmean_01', 'Tmean_02')),
         Tmean_spring = rowMeans(dplyr::pick('Tmean_03', 'Tmean_04', 'PPT_05')),
         Tmean_summer = rowMeans(dplyr::pick('Tmean_06', 'Tmean_07', 'Tmean_08')),
         Tmean_fall = rowMeans(dplyr::pick('Tmean_09', 'Tmean_10', 'Tmean_11'))
         )


predictor_names = c('PPT_winter', 'PPT_spring', 'PPT_summer', 'PPT_fall',
                    'Vpdmax_winter','Vpdmax_spring', 'Vpdmax_summer', 'Vpdmax_fall',
                    'Tmean_winter', 'Tmean_spring', 'Tmean_summer', 'Tmean_fall',
                    'Tmin_winter', 'Tmin_spring', 'Tmin_summer', 'Tmin_fall',
                    'Tmax_winter', 'Tmax_spring', 'Tmax_summer', 'Tmax_fall','AGBI.mean.prev1') 

models_clim2 <- clim_seasons %>% 
  group_by(site, taxon) %>%
  do({
    # predictors <- names(.)[13:72]
    # print(predictors)
    # print(length(predictors))
    predictors <- predictor_names
    formula <- reformulate(predictors, response = "AGBI.mean")
    print(formula)
    model <- lm(formula, data = .)
    tibble(mod = list(model))
  })

#creating a column for model names 
models_clim2 <- dplyr::mutate(models_clim2, model = paste0(site, "_", taxon))

summary(models_clim2[[3]][1][[1]])

rsq_values_clim2 <- models_clim2 %>%
  mutate(glance = map(mod, glance)) %>%
  unnest(glance) %>%
  dplyr::select(site, taxon, r.squared)

#rsq
ggplot(data=rsq_values_clim2) +
  geom_point(aes(x=site, y = r.squared, colour=taxon))

ggplot(data=rsq_values_clim2) +
  geom_point(aes(y=taxon, x = r.squared, colour=site))

#####RESIDUALS#################
#residuals not mean 
#pulling residuals for models

res = lapply(models_clim2[[3]], function(x) {
  if(length(x$residuals) < 62){ rep(NA, 62)}else{x$residuals}})


#creating df where each column has the residuals for each model
res_df = data.frame(matrix(unlist(res), ncol =length(res), byrow=FALSE))
#changing column names to site_taxon corresponding model
colnames(res_df) <- models_clim2$model
res_df <- res_df %>%
  mutate(year = 1950:2011)%>%
  dplyr::select(year, everything())

# #removing column with NA values 
# res_df_na <- subset(res_df, select = -c(HARVARD_HAVI, NRP_BEPA)) 
# res_df$period = NA
