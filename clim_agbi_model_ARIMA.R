library(broom)
library(dplyr)
library(tidyr)
library(reshape2)
library(purrr)
library(ggplot2)
library(GGally)

#arima(xreg = INDEPENDENT.VARS)


# COMBINING AGBI AND CLIM DF ----------------------------------------------

#function to offset prev years
tree_yr <- function(df, start_month=12) {
  # Year offset
  offset = ifelse(as.numeric(df$month) >= start_month,  1, 0)
  # Water year
  adj.year = as.numeric(df$year) + offset
  # Return the water year
  adj.year
}

#dataframe with year, site, taxon and AGBI.mean
AGBI_data = readRDS("AGBI_taxon_data.RDS")

#climate data in long format 
load('climate/prism_clim.RData')
clim_data = prism_long

#removing Vpdmin from dataset
df = subset(clim_data, select = -c(Vpdmin2) )

#renaming columns in climate dataframe
clim_data = df %>% 
  dplyr::rename(site = loc, PPT = PPT2, Tmean = Tmean2, Tmin = Tmin2, Tmax = Tmax2, 
                 Vpdmax = Vpdmax2)
clim_data$year <- as.numeric(clim_data$year)

#using tree_yr function to make prev and current year 
clim_data$year_tree = tree_yr(clim_data, start_month=9)
#current clim data jan-sept
clim_data_current = clim_data[which(clim_data$year == clim_data$year_tree),]
#prev climate data, prev sept-dec
clim_data_prev = clim_data[which(clim_data$year != clim_data$year_tree),]


#putting the climate variables in wide format
clim_wide_current =  pivot_wider(data = clim_data_current,
                         names_from = month, 
                         values_from = c(PPT, Tmean, Tmin, Tmax, Vpdmax))

#putting the climate variables in wide format
clim_wide_prev =  pivot_wider(data = subset(clim_data_prev, select = -year),
                                 names_from = month, 
                                 values_from = c(PPT, Tmean, Tmin, Tmax, Vpdmax))
clim_wide_prev = rename(clim_wide_prev, year = year_tree)

#merging prev and current clim data
#climate variables in wide format separated tree year
clim_wide = merge(clim_wide_current, clim_wide_prev, by = c('site', 'year'))

#adding lag years 1+2 to AGBI dateframe 
AGBI_data = AGBI_data %>%
  group_by(site, taxon) %>%
  arrange(site, taxon, year) %>%
  mutate(AGBI.mean.prev1 = lag(AGBI.mean, n=1),
         AGBI.mean.prev2 = lag(AGBI.mean, n=2))

#wide format of climate variables with AGBI by tree_year
clim_agbi <- AGBI_data %>% 
  left_join(clim_wide, by = c('year', 'site'))


# ARIMA Model -------------------------------------------------------------

### ADD ARIMA

#climate predictors
predictor_names = c('PPT_winter', 'PPT_spring', 'PPT_summer', 'PPT_fall',
                    'Vpdmax_winter','Vpdmax_spring', 'Vpdmax_summer', 'Vpdmax_fall',
                    'Tmean_winter', 'Tmean_spring', 'Tmean_summer', 'Tmean_fall',
                    'Tmin_winter', 'Tmin_spring', 'Tmin_summer', 'Tmin_fall',
                    'Tmax_winter', 'Tmax_spring', 'Tmax_summer', 'Tmax_fall') 

#testing ARIMA model
# # Subset to test: one taxon at one site
# test_df <- clim_seasons |>
#   dplyr::filter(site == 'GOOSE',
#                 taxon == 'QURU')
# 
# # Make AGBI a time series
# agbi_ts <- ts(data = test_df$AGBI.mean,
#               start = min(test_df$year),
#               end = max(test_df$year),
#               frequency = 1)
# 
# # Make matrix of covariates
# xreg <- as.matrix(test_df |>
#                     dplyr::ungroup() |>
#                     dplyr::select(dplyr::all_of(predictor_names)))
# 
# #### 5. Fit AR(1) model ####
# 
# # Fit ARIMA model
# mod <- forecast::Arima(y = agbi_ts, # AGBI time series response variable
#                        order = c(1, 0, 0), # AR(1) model with no differencing or moving average- 
#                        # note this is consistent with what you had before but c(1, 0, 1) might be more appropriate idk
#                        xreg = xreg)
# 
# # Check for basic statistics
# print(mod)
# 
# # Pseudo-R2
# fit_stat <- cor(x = fitted(mod),
#                 y = agbi_ts)^2
# fit_stat
# 
# # Residuals plot
# forecast::checkresiduals(mod)

# try for all sites and species

#### 3. Remove last five years ####

clim_agbi_in <- dplyr::filter(clim_seasons, year < 2007)
clim_agbi_out <- dplyr::filter(clim_seasons, year > 2006)

#uses seasonal climate data 
#making a time series 
# Fit ARIMA models by site and taxon
models <- clim_agbi_in %>%
  group_by(site, taxon) %>%
  do({
    df <- .
    
    # Drop rows with NA in response or predictors
    if (any(is.na(df$AGBI.mean)) || any(is.na(df[predictor_names]))) {
      return(tibble(mod = list(NULL), note = "Missing data"))
    }
    
    # Response variable as time series
    agbi_ts <- ts(df$AGBI.mean, start = min(df$year), frequency = 1)
    
    # External regressors
    xreg <- as.matrix(df %>% select(all_of(predictor_names)))
    
    # Fit ARIMA model
    model <- tryCatch({
      forecast::Arima(
        y = agbi_ts,
        order = c(1, 0, 0),  # AR(1)
        xreg = xreg,
        method = "ML"
      )
    }, error = function(e) {
      warning("ARIMA failed for site=", df$site[1], ", taxon=", df$taxon[1], ": ", e$message)
      return(NULL)
    })
    
    tibble(mod = list(model), note = if (is.null(model)) "Model failed" else NA)
  })

models <- dplyr::mutate(models, model = paste0(site, "_", taxon))
# foo = clim_agbi_out %>% 
#   dplyr::select(predictor_names) %>% 
#   dplyr::ungroup() |>
#   dplyr::select(dplyr::all_of(predictor_names)) %>% 
#   as.matrix()
# 
# forecast_out = clim_agbi_out %>% 
#   dplyr::ungroup() %>% 
#   dplyr::select(dplyr::all_of(predictor_names)) %>% 
#   as.matrix()
# 
# bar =forecast::forecast(models[[3]][[11]], xreg = foo)


model_forecasts <- models %>%
  filter(!is.null(mod[[1]])) %>%
  mutate(
    forecast = pmap(list(mod, site, taxon), function(model_obj, site_val, taxon_val) {
      if (is.null(model_obj)) return(NA)
      
      # Get future predictors for the same group
      future_data <- clim_agbi_out %>%
        filter(site == site_val, taxon == taxon_val) %>%
        arrange(year)
      
      if (nrow(future_data) == 0) return(NA)
      
      future_xreg <-future_data %>%
        dplyr::ungroup() %>% 
        dplyr::select(dplyr::all_of(predictor_names)) %>% 
        as.matrix()
      
      # Forecast
      tryCatch({
        forecast::forecast(model_obj, xreg = future_xreg)
      }, error = function(e) {
        warning(paste("Forecast failed for", site_val, taxon_val, ":", e$message))
        return(NA)
      })
    })
  )



# Check object
model_forecasts

fitted = lapply(model_forecasts[[3]], function(x) {
    if(length(x$fitted) < 57){ rep(NA, 57)}else{x$fitted}})

fitted_df = data.frame(matrix(unlist(fitted), ncol=length(fitted), byrow=FALSE))
#changing column names to site_taxon corresponding model
colnames(fitted_df) <- models$model
fitted_df <- fitted_df %>%
  mutate(year = 1950:2006)%>%
  dplyr::select(year, everything())
fit_long <- fitted_df %>%
  pivot_longer(cols = GOOSE_ACRU:SYLVANIA_TSCA, names_to = "site_taxon", values_to = "value") %>%
  separate(site_taxon, into = c("site", "taxon"), sep = "_")



mean_forecast = lapply(model_forecasts[[5]], function(x) {
  if(length(x$mean) < 5){ rep(NA, 5)}else{x$mean}})
forecast_mean = data.frame(matrix(unlist(mean_forecast), ncol=length(mean_forecast), byrow=FALSE))
colnames(forecast_mean) <- models$model
fitted_df <- fitted_df %>%
  mutate(year = 2007:2011)%>%
  dplyr::select(year, everything())
fit_long <- fitted_df %>%
  pivot_longer(cols = GOOSE_ACRU:SYLVANIA_TSCA, names_to = "site_taxon", values_to = "value") %>%
  separate(site_taxon, into = c("site", "taxon"), sep = "_")

lapply(model_forecasts[[5]], function(x) 
  {fmean = x$mean; flo=x$lower; fhi=x$upper})

foo = list_rbind(model_forecasts[[5]])
#changing column names to site_taxon corresponding model
# colnames(fitted_df) <- models$model
# fitted_df <- fitted_df %>%
#   mutate(year = 1950:2006)%>%
#   dplyr::select(year, everything())
# fit_long <- fitted_df %>%
#   pivot_longer(cols = GOOSE_ACRU:SYLVANIA_TSCA, names_to = "site_taxon", values_to = "value") %>%
#   separate(site_taxon, into = c("site", "taxon"), sep = "_")



lower = lapply(model_forecasts[[5]], function(x) {
  if(length(x$lower) < 10){ rep(NA, 10)}else{x$lower}})
upper = lapply(model_forecasts[[5]], function(x) {
  if(length(x$upper) < 10){ rep(NA, 10)}else{x$upper}})


# agbi_pred <- c(models$fitted, model_forecasts$mean)
# lower <- c(mod$fitted, forecast_test$lower[,2])
# upper <- c(mod$fitted, forecast_test$upper[,2])
sites <- c("GOOSE", "ROOSTER", "HARVARD", "HMC", "NRP", "SYLVANIA")
taxa = (unique(clim_agbi$taxon))

# Plot
pdf("report/figures/AGBI_fitted_forecast.pdf")
for (site in sites) {
  for (taxon in taxa) {
    print(site)
    print(taxon)
    clim_agbi_sub = clim_agbi %>%
      filter(site == !!site,
             taxon == !!taxon)
    
    fitted_sub = fit_long %>%
      filter(site == !!site,
             taxon == !!taxon)
    
   p = ggplot2::ggplot(data = clim_agbi_sub) +
      ggplot2::geom_point(ggplot2::aes(x = year, y = AGBI.mean)) +
     geom_point(data = fitted_sub, aes(x = year, y = value), color = "blue")+
     
   print(p) 
    
  }
  
}
dev.off()

# +
  # ggplot2::geom_line(ggplot2::aes(x = year, y = agbi_pred)) +
  # ggplot2::geom_ribbon(ggplot2::aes(x = year, ymin = lower,
  #                                   ymax = upper),
  #                      alpha = 0.5)
  # 

# MODEL -------------------------------------------------------------------

#taking the mean AGBI of each taxa at each site 
mean_AGBI = clim_agbi %>% 
  group_by(site, taxon) %>% 
  mutate(mean_abun = mean(AGBI.mean))

sum_AGBI = clim_agbi %>% 
  group_by(site, taxon) %>% 
  mutate(sum_abun = sum(AGBI.mean))


#pulling the mean values of AGBI
#use this line at line 147
mean_AGBI = data.frame(mean_abun = unique(mean_AGBI$mean_abun))
sum_AGBI = data.frame(sum_abun = unique(sum_AGBI$sum_abun))

#new dataframe with seasonal climate data
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


#predictor names to be inputted into the model
predictor_names = c('PPT_winter', 'PPT_spring', 'PPT_summer', 'PPT_fall',
                    'Vpdmax_winter','Vpdmax_spring', 'Vpdmax_summer', 'Vpdmax_fall',
                    'Tmean_winter', 'Tmean_spring', 'Tmean_summer', 'Tmean_fall',
                    'Tmin_winter', 'Tmin_spring', 'Tmin_summer', 'Tmin_fall',
                    'Tmax_winter', 'Tmax_spring', 'Tmax_summer', 'Tmax_fall','AGBI.mean.prev1') 

###model###
#uses seasonal climate data + AGBI lag1
models <- clim_seasons %>% 
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
models <- dplyr::mutate(models, model = paste0(site, "_", taxon))

#verifying Rsq function 
summary(models[[3]][1][[1]])

#Rsq values for each model 
rsq_values <- models %>%
  dplyr::mutate(glance = map(mod, glance)) %>%
  unnest(glance) %>%
  dplyr::select(site, taxon, r.squared)

#combining Rsq values with mean AGBI across site and taxa
rsq_AGBI = cbind(rsq_values, mean_AGBI)
rsq_AGBI_sum = cbind(rsq_values, sum_AGBI)

#plotting Rsq by site and taxon 
ggplot(data = rsq_AGBI) +
  geom_point(aes(x=site, y = r.squared, colour = taxon, size = mean_abun))+
  theme_light(14)+
  theme(axis.text.x = element_text(angle = -45))

ggplot(data = rsq_AGBI_sum) +
  geom_point(aes(x=site, y = r.squared, colour = taxon, size = sum_abun))+
  theme_light(14)+
  theme(axis.text.x = element_text(angle = -45))


ggplot(data=rsq_values) +
  geom_point(aes(y=taxon, x = r.squared, colour=site))



# RESIDUALS ---------------------------------------------------------------


#residuals not mean 
#pulling residuals for models

# res = lapply(models[[3]], function(x) {
#   if(length(x$residuals) < 62){ rep(NA, 62)}else{x$residuals}})
# res = lapply(models[[3]], function(x) {
#   if(length(residuals(x)) < 62){ rep(NA, 62)}else{residuals(x)}})

models <- dplyr::mutate(models, model = paste0(site, "_", taxon))

# res = apply(models, 2, function(x) {residuals(mod)})
res = lapply(models[[3]], function(x) {
  if(length(residuals(x)) < 62){ rep(NA, 62)}else{residuals(x)}})

#creating df where each column has the residuals for each model
res_df = data.frame(matrix(unlist(res), ncol=length(res), byrow=FALSE))
#changing column names to site_taxon corresponding model
colnames(res_df) <- models$model
res_df <- res_df %>%
  mutate(year = 1950:2011)%>%
  dplyr::select(year, everything())

#changing residula values to long format
res_long <- res_df %>%
  pivot_longer(cols = GOOSE_ACRU:SYLVANIA_TSCA, names_to = "site_taxon", values_to = "value") %>%
  separate(site_taxon, into = c("site", "taxon"), sep = "_")


#assigning unique colour to each site 
site_levels <- unique(res_long$site)
site_colors <- RColorBrewer::brewer.pal(length(site_levels), "Set1")
names(site_colors) <- site_levels

# # 
# 
# resid_LM = res_long[which((res_long$site == 'GOOSE')&(res_long$taxon == 'PIST')),]
# resid_ARIMA = data.frame(resid_ARIMA)
# resid_ARIMA$year = seq(1950, 2006)
# 
# resids = merge(resid_LM, resid_ARIMA)
# 
# ggplot() +
#   geom_point(data=resid_LM, aes(x=year, y=value)) +
#   geom_point(data=resid_ARIMA, aes(x=year, y=resid_ARIMA), colour='red')
# 
# ggplot() +
#   geom_point(data=resids, aes(x=value, y=resid_ARIMA)) + 
#   geom_abline(intercept=0, slope=1)



# PLOTTING RESIDUALS ------------------------------------------------------

#residuals for each site with each taxa
ggplot()+
  geom_point(data = res_long , aes(x = year, y = value, color = taxon))+
  facet_wrap(~site)+
  theme_light(base_size = 11)+
  ggtitle("model residuals")

#residuals for each site with each taxa, free_y 
ggplot(data = res_long , aes(x = year, y = value, color = taxon))+
  geom_point()+
  geom_smooth( method = "gam")+
  facet_wrap(~site, scales = "free_y")+
  theme_light(base_size = 11)+
  ggtitle("model resiudals")

#residuals for each site, wrapped by taxa
sites <- c("GOOSE", "ROOSTER", "HARVARD", "HMC", "NRP", "SYLVANIA")
for (site in sites) {
  site_data <- res_long %>%
    dplyr::filter(site == !!site)
  
  p <- ggplot(data = site_data) +
    geom_point(aes(x = year, y = value)) +
    facet_wrap(~taxon, scales='free_y') +
    theme_light(base_size = 11) +
    ggtitle(site)
  
  print(p)
}
  
  
# #residuals for ony harvard 
# ggplot(data = res_long %>% filter(site== "HARVARD"))+
#   geom_point(aes(x = year, y = value, color = taxon))+
#   # facet_wrap(~site, scales = "free_y")+
#   theme_light(base_size =11)+
#   ggtitle("HARVARD")


#residuals where QURU is found 
ggplot(data = res_long %>% filter(taxon== "QURU"),
       aes(x = year, y = value, color = site))+
  geom_point()+
  # facet_wrap(~site, scales = "free_y")+
  #geom_smooth( method = "gam")+
  scale_color_manual(values = site_colors) +
  theme_light(base_size =11)+
  ggtitle("QURU")

#residuals for PIST at all sites found
ggplot(data = res_long %>% filter(taxon== "PIST"),
       aes(x = year, y = value, color = site))+
  geom_point()+
  # facet_wrap(~site, scales = "free_y")+
  #geom_smooth(method = "gam")+
  scale_color_manual(values = site_colors) +
  theme_light(base_size =11)+
  ggtitle("PIST")

#residuals for TSCA at all sites found
ggplot(data = res_long %>% filter(taxon== "TSCA"),
       aes(x = year, y = value, color = site))+
  geom_point()+
  # facet_wrap(~site, scales = "free_y")+
  #geom_smooth(method = "gam")+
  scale_color_manual(values = site_colors) +
  theme_light(base_size =11)+
  ggtitle("TSCA")

res_df_na <- subset(res_df, select = -c(HARVARD_HAVI, NRP_BEPA)) 

for (site in sites) {
  site_data <- res_df_na %>%
    dplyr::select(starts_with(site))
  
  # Skip if no matching columns (to avoid errors)
  if (ncol(site_data) == 0) next
  
  # Clean column names
  colnames(site_data) <- sub(".*_", "", colnames(site_data))
  
  # Plot
  print(ggpairs(data = site_data, title = paste(site, "Correlations")))
}



# PREVIOUS MODELS ---------------------------------------------------------
#models_clim 

# months = c(paste0('0', seq(1,9)), seq(10, 12))
# 
# predictor_clim_names = paste(rep(c('PPT', 'Tmean', 'Tmin', 'Tmax', 'Vpdmax'), each=12), rep(months, times = 5), sep='_')

months = c(paste0('0', c(3, 6, 9)), 12)

predictor_clim_names = paste(rep(c('PPT', 'Tmean', 'Tmin', 'Tmax', 'Vpdmax'), 
                                 each=4), rep(months, times = 5), sep='_')

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
#loading AGBI data by site 
AGBI_data_site = readRDS("AGBI_site_data.RDS")

#lag AGBI by site 
AGBI_data_site = AGBI_data_site %>%
  group_by(site) %>%
  arrange(site, year) %>%
  mutate(AGBI.mean.prev1 = lag(AGBI.mean, n=1),
         AGBI.mean.prev2 = lag(AGBI.mean, n=2))


#wide format of climate data with site AGBI
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

