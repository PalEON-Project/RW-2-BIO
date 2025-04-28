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


months = c(paste0('0', seq(1,9)), seq(10, 12))

predictor_clim_names = paste(rep(c('PPT', 'Tmean', 'Tmin', 'Tmax', 'Vpdmax'), each=12), rep(months, times = 5), sep='_')

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


# wtr_yr <- function(df, start_month=12) {
#   # Year offset
#   offset = ifelse(as.numeric(df$month) >= start_month,  1, 0)
#   # Water year
#   adj.year = as.numeric(df$year) + offset
#   # Return the water year
#   adj.year
# }
# 
# 
# 
# # moving mean for that day and previous days (e.g. 5 represents the mean of that day and the for previous days)
# df2 = df %>%
#   group_by(site, year) %>%
#   arrange(site, year, day) %>%
#   mutate(temp.5 = rollmean(x = temp, 5, align = "right", fill = NA))
# head(df2, 75)
# 
# # moving mean for the previous days not including the current day (e.g. 5 represents the mean of the 5 previous days)
# df2 = df2 %>%
#   mutate(temp.lag1 = lag(temp, n = 1)) %>%
#   mutate(temp.5.previous = rollapply(data = temp.lag1, 
#                                      width = 5, 
#                                      FUN = mean, 
#                                      align = "right", 
#                                      fill = NA, 
#                                      na.rm = T))
# head(df2, 75)
# 
# foo = clim_agbi_long %>%
#   group_by(site, taxon) %>%
#   arrange(site, taxon, year) %>%
#   mutate(AGBI_prev = lag(AGBI.mean, n=1))
# 
# 
# foo = clim_agbi_long %>%  
#   group_by(year, site, taxon) %>% 
#   dplyr::mutate(prev_abgi = wtr_yr(.))
#   
# 
# clim_agbi_long$year_water = wtr_yr(clim_agbi_long)
# 
# AGBI_prev = clim_agbi_long[,c('year', 'month', 'site', 'year_water')]
# 
# water <- AGBI_prev %>%
#   left_join(AGBI_data, by = c('year_water'='year', 'site'))
