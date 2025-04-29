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

df = subset(clim_data, select = -c(Vpdmin2) )

clim_data = df %>% 
  dplyr::rename(site = loc, PPT = PPT2, Tmean = Tmean2, Tmin = Tmin2, Tmax = Tmax2, 
                 Vpdmax = Vpdmax2)
clim_data$year <- as.numeric(clim_data$year)

clim_data$year_tree = tree_yr(clim_data, start_month=9)

clim_data_current = clim_data[which(clim_data$year == clim_data$year_tree),]
clim_data_prev = clim_data[which(clim_data$year != clim_data$year_tree),]


# #putting the climat variables in wide format
# clim_wide =  pivot_wider(data = clim_data,
#                          names_from = month, 
#                          values_from = c(PPT, Tmean, Tmin, Tmax, Vpdmax))

#putting the climat variables in wide format
clim_wide_current =  pivot_wider(data = clim_data_current,
                         names_from = month, 
                         values_from = c(PPT, Tmean, Tmin, Tmax, Vpdmax))

#putting the climat variables in wide format
clim_wide_prev =  pivot_wider(data = subset(clim_data_prev, select = -year),
                                 names_from = month, 
                                 values_from = c(PPT, Tmean, Tmin, Tmax, Vpdmax))
clim_wide_prev = rename(clim_wide_prev, year = year_tree)

clim_wide = merge(clim_wide_current, clim_wide_prev, by = c('site', 'year'))


AGBI_data = AGBI_data %>%
  group_by(site, taxon) %>%
  arrange(site, taxon, year) %>%
  mutate(AGBI.mean.prev1 = lag(AGBI.mean, n=1),
         AGBI.mean.prev2 = lag(AGBI.mean, n=2))



# #long format
# clim_agbi_long <- AGBI_data %>% 
#   left_join(clim_data, by = c('year', 'site'))

#wide format
clim_agbi <- AGBI_data %>% 
  left_join(clim_wide, by = c('year', 'site'))


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

