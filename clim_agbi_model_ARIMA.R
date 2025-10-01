library(broom)
library(dplyr)
library(tidyr)
library(reshape2)
library(purrr)
library(ggplot2)
library(GGally)
library(stringr)

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


# 
# # Pseudo-R2
# fit_stat <- cor(x = fitted(mod),
#                 y = agbi_ts)^2
# fit_stat
# 
# # Residuals plot
# forecast::checkresiduals(mod)

# try for all sites and species

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

library(tidyverse)

clim_long <- clim_seasons %>%
  pivot_longer(
    cols = all_of(predictor_names),
    names_to = "predictor",
    values_to = "value"
  ) %>%
  separate(predictor, into = c("clim_var", "season"), sep = "_")

cor_df <- clim_long %>%
  group_by(site, taxon, clim_var, season) %>%
  summarise(
    cor = cor(value, AGBI.mean, use = "complete.obs"),
    pval = cor.test(value, AGBI.mean)$p.value,
    .groups = "drop"
  ) %>%
  mutate(sig = ifelse(pval < 0.05, TRUE, NA))

cor_df <- cor_df %>%
  mutate(season = factor(season, levels = c("winter", "spring", "summer", "fall")))

pdf("report/figures/AGBI_predictor_correlations.pdf", width = 10, height = 8)

for (s in unique(cor_df$site)) {
  for (ss in levels(cor_df$season)) {
    
    p <- ggplot(filter(cor_df, site == s, season == ss),
                aes(x = clim_var, y = taxon, fill = cor)) +
      geom_tile(color = "white") +
      geom_point(aes(shape = sig), color = "black", size = 2, na.rm = TRUE) +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", limits = c(-1, 1)) +
      scale_shape_manual(values = c(16, NA)) +
      theme_minimal(base_size = 14) +
      labs(title = paste("Correlation between predictors and AGBI.mean\nSite:", s, "- Season:", ss),
           x = "Predictor",
           y = "Taxon",
           fill = "Correlation",
           shape = "Significant") +
      theme(axis.text.x = element_text(angle = -45, hjust = 0))
    
    print(p)
  }
}

dev.off()


#Remove last five years
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

#adding column for naming
models <- dplyr::mutate(models, model = paste0(site, "_", taxon))


#forecasting
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



foo = lapply(model_forecasts$forecast, function(x){data.frame(year = as.vector(time(x$mean)),
                                                              forecast_mean = x$mean, 
                                                              forecast_lo = x$lower[,"95%"], 
                                                              forecast_hi = x$upper[,"95%"])})
#lower, upper, and mean of predictions from 2007?-2011
forecast_long = data.frame(site = rep(model_forecasts[[1]], each=5), 
                 taxon = rep(model_forecasts[[2]], each=5), 
                 bind_rows(foo))

#residuals and fitted values in long format
fit_res_long <- pmap_dfr(
  list(model_forecasts$forecast, model_forecasts$site, model_forecasts$taxon), 
  function(fcast, site_val, taxon_val) { 
    if (is.null(fcast)) return(NULL) 
    
    tibble(
      site      = site_val, 
      taxon     = taxon_val, 
      year      = as.vector(time(fcast$fitted)), 
      fitted    = as.numeric(fcast$fitted), 
      residuals = as.numeric(fcast$residuals)
    )
  }
)


#coefficients from predictors
fit_coefs_long <- pmap_dfr(
  list(model_forecasts$site, model_forecasts$taxon, model_forecasts$mod),
  function(site_val, taxon_val, mod_obj) {
    coefs <- coef(mod_obj)
    tibble(
      site = site_val,
      taxon = taxon_val,
      coef_name = names(coefs),
      coef_value = as.numeric(coefs)
    )
  }
)

fit_coefs_long <- fit_coefs_long %>%
  left_join(
    agbi_cumsum_filter %>%
      select(site, taxon) %>%
      mutate(in_top95 = TRUE),
    by = c("site", "taxon")
  ) %>%
  mutate(in_top95 = if_else(is.na(in_top95), FALSE, in_top95))



#mean AGBI for each species at a site
mean_taxa_agbi = clim_agbi %>% 
  group_by(taxon, site) %>% 
  summarize(mean_abi = mean(AGBI.mean))


#sd of the residuals 
residuals_sd = fit_res_long %>%
  group_by(site, taxon) %>%
  summarise(sd_residuals = sd(residuals, na.rm = TRUE))

#joining sd(residuals) with residuals and fitted df
fit_res_long_ci = fit_res_long %>%
  left_join(residuals_sd, by = c("site", "taxon"))

fit_res_long_ci <- fit_res_long_ci %>%
  mutate(
    fitted_lo = fitted - 1.96 * sd_residuals,
    fitted_hi = fitted + 1.96 * sd_residuals
  )

#df with CIs for fitted values 
fit_res_long_ci <- fit_res_long_ci %>%
  mutate(
    fitted_lo = fitted - 1.96 * sd_residuals,
    fitted_hi = fitted + 1.96 * sd_residuals
  )

#residuals standardized WITH sd of....
residuals_standardized_sd = fit_res_long %>%
  left_join(residuals_sd, by = c("site", "taxon")) %>%
  mutate(residuals_standardized = residuals / sd_residuals)

#residuals standardized by mean AGBI over time for each taxa at each site
residuals_standardized_mean = fit_res_long %>%
  left_join(mean_taxa_agbi, by = c("site", "taxon"))%>%
  mutate(residuals_standard = residuals/mean_abi)

# #scaling residuals with AGBI??? idk 
# residuals_AGBI = fit_res_long %>% 
#   left_join(AGBI_data) %>% 
#   mutate(residuals_AGBI = residuals/AGBI.mean)

#residuals in wide format with all sites for pairwise correlation
residuals = lapply(model_forecasts[[6]], function(x) {
  if(length(x$residuals) < 57){ rep(NA, 57)}else{x$residuals}})

#Deleting models with missing data HAVI at Harvard and BEPA at NRP
#res = res[-c(16, 32)] 
#creating df where each column has the residuals for each model
res_df = data.frame(matrix(unlist(residuals), ncol =length(residuals), byrow=FALSE))
#changing column names to site_taxon corresponding model
colnames(res_df) <- model_forecasts$model
res_df <- res_df %>%
  mutate(year = 1950:2006)%>%
  dplyr::select(year, everything())
#removing column with NA values 
res_df_na <- subset(res_df, select = -c(HARVARD_HAVI, NRP_BEPA)) 

# summed_AGBI = AGBI_data %>% 
#   group_by(site, taxon, year) %>% 
#   summarise(total = sum(AGBI.mean))


# PLOTTING ----------------------------------------------------------------
sites <- c("GOOSE", "ROOSTER", "HARVARD", "HMC", "NRP", "SYLVANIA")
taxa = (unique(clim_agbi$taxon))

#fitted values plotted for each taxa and site 
pdf("report/figures/AGBI_fitted_forecast.pdf", width=10, height=8)
for (site in sites) {
  for (taxon in taxa) {
    print(site)
    print(taxon)
    
    disturbance_years <- list(
      GOOSE = 1981,
      ROOSTER = c(1983, 1992),
      HARVARD = 1981)
  
    disturbance <- disturbance_years[[site]]
    
    clim_agbi_sub = clim_agbi %>%
      dplyr::filter(site == !!site,
             taxon == !!taxon)
    
    # fitted_sub = fitted_long %>%
    #   dplyr::filter(site == !!site,
    #          taxon == !!taxon)
  
    
    forecast_sub = forecast_long %>%
      filter(site == !!site,
             taxon == !!taxon)
    
    res_fit_sub = fit_res_long_ci %>% 
      filter(site == !!site,
             taxon == !!taxon)
    if (nrow(res_fit_sub) == 0){ next}
    
   p = ggplot() +
     geom_point(data = clim_agbi_sub, aes(x = year, y = AGBI.mean, color = "Observed")) +
     geom_point(data = res_fit_sub, aes(x = year, y = fitted, color = "Fitted")) +
     geom_vline(xintercept = disturbance, linetype = "dashed", color = "red") +
     geom_ribbon(data = forecast_sub, aes(x = year, ymin = forecast_lo, ymax = forecast_hi, fill = "Forecast CI"), alpha = 0.5) +
     geom_ribbon(data = res_fit_sub, aes(x = year, ymin = fitted_lo, ymax = fitted_hi, fill = "Fitted CI"), alpha = 0.5) +
     geom_point(data = forecast_sub, aes(x = year, y = forecast_mean, color = "Forecast")) +
     scale_color_manual(name = "Type", values = c("Observed" = "black", "Fitted" = "blue", 
                                                  "Forecast" = "orange")) +
     scale_fill_manual(name = "Ribbon", values = c("Forecast CI" = "gray", "Fitted CI" = "lightblue" )) +
     ggtitle(paste0(site, '; ', taxon)) +
     theme_light(base_size = 14)
   
     
   print(p) 
    
  }
  
}
dev.off()


#residuals plotted with geom_line for each species at each site 
pdf("report/figures/AGBI_residuals_forecast.pdf", width = 10, height = 8)

# Define disturbance years once
disturbance_years <- list(
  GOOSE = 1981,
  ROOSTER = c(1983, 1992),
  HARVARD = 1981
  # NRP = 1980,
  # SYLVANIA = 1990,
  # HMC = 2000
)

for (site in sites) {
  
  # Subset all taxa for current site
  res_fit_sub <- fit_res_long %>%
    filter(site == !!site)
  
  if (nrow(res_fit_sub) == 0) next
  
  # Get disturbance year(s) for current site
  disturbance <- disturbance_years[[site]]
  
  # Plot residuals for each taxon
  p <- ggplot(res_fit_sub, aes(x = year, y = residuals, color = taxon)) +
    geom_line() +
    geom_vline(xintercept = disturbance, linetype = "dashed", color = "red") +
    ggtitle(site) +
    theme_light(base_size = 14) +
    labs(color = "Taxon")
  
  print(p)
}

dev.off()


#residuals plotted with geom_line for each species at each site on one panel for 
#standardized residuals
pdf("report/figures/AGBI_residuals_standardized.pdf", width = 10, height = 8)

# Define disturbance years once
disturbance_years <- list(
  GOOSE = 1981,
  ROOSTER = c(1983, 1992),
  HARVARD = 1981
  # NRP = 1980,
  # SYLVANIA = 1990,
  # HMC = 2000
)

for (site in sites) {
  
  # Subset all taxa for current site
  res_fit_sub = residuals_standardized_mean %>% 
    filter(site == !!site)
  
  if (nrow(res_fit_sub) == 0) next
  
  # Get disturbance year(s) for current site
  disturbance <- disturbance_years[[site]]
  
  # Plot residuals for each taxon
  p <- ggplot(res_fit_sub, aes(x = year, y = residuals_standard, color = taxon)) +
    geom_line() +
    geom_vline(xintercept = disturbance, linetype = "dashed", color = "red") +
    ggtitle(site) +
    theme_light(base_size = 14) +
    labs(color = "Taxon")
  
  print(p)
}

dev.off()

#correlation of residuals using ggpairs
pdf('report/figures/ggpairs_residuals_ARIMA.pdf')
#ggpairs for each site 
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
dev.off()



#fitted values plotted for each taxa and site 
#scaling data by sd of residuals 
pdf("report/figures/AGBI_fitted_res_scaled.pdf", width=10, height=8)
for (site in sites) {
  for (taxon in taxa) {
    print(site)
    print(taxon)
    
    
    
    clim_agbi_sub = clim_agbi %>%
      dplyr::filter(site == !!site,
                    taxon == !!taxon)
    
    # fitted_sub = fitted_long %>%
    #   dplyr::filter(site == !!site,
    #          taxon == !!taxon)
    
    
    forecast_sub = forecast_long %>%
      filter(site == !!site,
             taxon == !!taxon)
    
    res_AGBI_sub = residuals_AGBI %>% 
      filter(site == !!site,
             taxon == !!taxon)
    res_fit_sub = residuals_standardized_mean %>% 
      filter(site == !!site,
             taxon == !!taxon)
    if (nrow(res_fit_sub) == 0){ next}
    
    p = ggplot() +
      geom_point(data = clim_agbi_sub, aes(x = year, y = AGBI.mean, color = "Observed")) +
      geom_point(data = res_fit_sub, aes(x = year, y = fitted, color = "Fitted")) +
      #geom_point(data = res_AGBI_sub, aes(x = year, y = residuals_AGBI, color = "ResidualsAGBI..")) +
      geom_point(data = res_fit_sub, aes(x = year, y = residuals_standard, color = "Residuals standardize")) +
      #geom_ribbon(data = forecast_sub, aes(x = year, ymin = forecast_lo, ymax = forecast_hi, fill = "Forecast CI"), alpha = 0.5) +
      geom_point(data = forecast_sub, aes(x = year, y = forecast_mean, color = "Forecast")) +
      scale_color_manual(name = "Type", values = c("Observed" = "black", "Fitted" = "blue", 
                                                   "Forecast" = "orange", "ResidualsAGBI.." = "red", 
                                                   "Residuals standardize" = "purple")) +
      scale_fill_manual(name = "Ribbon", values = c("Forecast CI" = "gray")) +
      ggtitle(paste0(site, '; ', taxon)) +
      theme_light(base_size = 14)
    
    
    print(p) 
    
  }
  
}
dev.off()


# Collapse coefficient names into groups
fit_coefs_long <- fit_coefs_long %>%
  mutate(var_group = case_when(
    str_detect(coef_name, regex("ppt", ignore_case = TRUE)) ~ "PPT",
    str_detect(coef_name, regex("tmean|tmin|tmax", ignore_case = TRUE)) ~ "Temperature",
    str_detect(coef_name, regex("vpd", ignore_case = TRUE)) ~ "VPD",
    TRUE ~ "Other"
  ))

pdf("report/figures/predictor_coefficients.pdf", width = 12, height = 8)

for (site in sites) {
  
  site_data <- fit_coefs_long %>%
    filter(site == !!site, !coef_name %in% c("ar1", "intercept"))
  
  p <- ggplot(site_data, aes(x = coef_name, y = coef_value,
                             color = taxon, shape = in_top95)) +
    geom_point(size = 3, alpha = 0.7) +
    facet_wrap(~var_group, scales = "free_x", ncol = 3) +  # PPT | Temp | VPD
    theme_light(base_size = 14) +
    theme(axis.text.x = element_text(angle = -90, hjust = 0)) +
    labs(title = paste("Predictor Coefficients -", site),
         x = "Coefficient",
         y = "Value",
         color = "Taxon",
         shape = "Dominant taxa") +
    scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1))
  
  print(p)
}

dev.off()



# filtering cumsum --------------------------------------------------------

#joining AGBI.mean with 
fit_res_joined <- fit_res_long_ci %>%
  left_join(
    select(clim_agbi, year, site, taxon, AGBI.mean),
    by = c("year", "site", "taxon")
  )

# 1. Calculate fraction + cumulative sum
agbi_fraction <- fit_res_joined %>%
  ungroup() %>% 
  group_by(site, taxon) %>%  
  mutate(taxa.AGBI = sum(AGBI.mean)) %>% 
  ungroup() %>% 
  group_by(site) %>% 
  mutate(site.AGBI = sum(AGBI.mean)) %>% 
  select(site, taxon, taxa.AGBI, site.AGBI) %>% 
  distinct() %>% 
  mutate(frac.AGBI = taxa.AGBI / site.AGBI)

agbi_cumsum <- agbi_fraction %>% 
  arrange(site, desc(frac.AGBI)) %>%   
  group_by(site) %>%
  mutate(cum_sum = cumsum(frac.AGBI)) %>%
  ungroup()

# 2. Filter those making up 95% of biomass
agbi_cumsum_filter <- agbi_cumsum %>% 
  filter(cum_sum < 0.95)

# 3. Add TRUE/FALSE flag back to fit_res_joined
fit_res_flagged <- fit_res_joined %>%
  mutate(in_top95 = if_else(
    paste(site, taxon) %in% paste(agbi_cumsum_filter$site, agbi_cumsum_filter$taxon),
    TRUE, FALSE
  ))


#joining fitted, residuals, CI with filtered cumsum %
filtered_AGBI = inner_join(fit_res_joined, agbi_cumsum_filter[,c('site', 'taxon', 'cum_sum')], by = c('site', 'taxon'))
#joining forecast 2007-2011, residuals, CI with filtered cumsum %
filtered_forecast = inner_join(forecast_long, agbi_cumsum_filter[,c('site', 'taxon', 'cum_sum')], by = c('site', 'taxon'))
#joining AGBI.mean with 
filtered_forecast2 <- filtered_forecast %>%
  left_join(
    select(clim_agbi, year, site, taxon, AGBI.mean),
    by = c("year", "site", "taxon")
  )



#percent of observed values that fall within the CI
#fitted values will all fall within the CI
percent_obs = filtered_AGBI %>% 
  mutate(in.CI = ifelse(AGBI.mean >= fitted_lo & AGBI.mean <= fitted_hi, 1, 0))

foo = percent_obs %>% 
  group_by(at1 %>% 
    pivot_wider(names_from = numbers, values_from = value), taxon) %>% 
  summarize( percent = sum(in.CI)/n())
write.csv(foo, "fitted_percent_in.csv")


percent_forecast = filtered_forecast2 %>% 
  mutate(in.CI = ifelse(AGBI.mean >= forecast_lo & AGBI.mean <= forecast_hi, 1, 0))
percent_forecast_site = percent_forecast %>% 
  group_by(site) %>% 
  dplyr::summarize( percent = sum(in.CI)/n())
#write.csv(foo, "forecast_percent_in.csv")

percent_forecast_taxon = percent_forecast %>% 
  group_by(site, taxon) %>% 
  dplyr::summarize( percent = sum(in.CI)/n())

percent_forecast_taxon2 = percent_forecast %>% 
  group_by(taxon) %>% 
  dplyr::summarize( percent = sum(in.CI)/n())


res_wide_filter = filtered_AGBI %>% 
  mutate(site_taxa = paste0(site, "_", taxon)) %>% 
  subset(select = -c( fitted, fitted_lo, fitted_hi, 
                     AGBI.mean, cum_sum, sd_residuals, site, taxon)) %>% 
  pivot_wider(names_from = site_taxa , values_from = residuals)


#plotting fitted vs observed with CI at each point 
pdf("report/figures/AGBI_fitted_vs_observed.pdf", width = 10, height = 8)
for (site in sites) {
  for (taxon in taxa) {
    print(paste(site, taxon))
    
    clim_agbi_sub <- fit_res_joined %>%
      filter(site == !!site, taxon == !!taxon)
    
    if (nrow(clim_agbi_sub) == 0 ||
        all(is.na(clim_agbi_sub$AGBI.mean)) ||
        all(is.na(clim_agbi_sub$fitted))) next
    
    p <- ggplot(clim_agbi_sub, aes(x = AGBI.mean, y = fitted)) +
      geom_point() +
      geom_errorbar(aes(ymin = fitted_lo, ymax = fitted_hi), width = 0.01, color = "gray40") +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      labs(x = "Observed AGBI.mean", y = "Fitted AGBI", title = paste0(site, "; ", taxon)) +
      theme_light(base_size = 14)
    
    print(p)
  }
}
dev.off()





#fitted values plotted for each taxa and site CUMULATIVE SUM
pdf("report/figures/AGBI_fitted_forecast_CUMSUM.pdf", width=10, height=8)
for (site in sites) {
  for (taxon in taxa) {
    print(site)
    print(taxon)
    
    disturbance_years <- list(
      GOOSE = 1981,
      ROOSTER = c(1983, 1992),
      HARVARD = 1981)
    
    disturbance <- disturbance_years[[site]]
    
    clim_agbi_sub = filtered_AGBI %>%
      dplyr::filter(site == !!site,
                    taxon == !!taxon)
    
    # fitted_sub = fitted_long %>%
    #   dplyr::filter(site == !!site,
    #          taxon == !!taxon)
    
    
    forecast_sub = filtered_forecast2 %>%
      filter(site == !!site,
             taxon == !!taxon)
    
    res_fit_sub = filtered_AGBI %>% 
      filter(site == !!site,
             taxon == !!taxon)
    if (nrow(res_fit_sub) == 0){ next}
    
    p = ggplot() +
      geom_point(data = clim_agbi_sub, aes(x = year, y = AGBI.mean, color = "Observed")) +
      geom_point(data = res_fit_sub, aes(x = year, y = fitted, color = "Fitted")) +
      geom_point(data = forecast_sub, aes(x = year, y = AGBI.mean, color = "Observed")) +
      geom_line(data = res_fit_sub, aes(x = year, y = fitted, color = "Fitted")) +
      geom_vline(xintercept = disturbance, linetype = "dashed", color = "red") +
      geom_ribbon(data = forecast_sub, aes(x = year, ymin = forecast_lo, ymax = forecast_hi, fill = "Forecast CI"), alpha = 0.5) +
      geom_ribbon(data = res_fit_sub, aes(x = year, ymin = fitted_lo, ymax = fitted_hi, fill = "Fitted CI"), alpha = 0.5) +
      geom_point(data = forecast_sub, aes(x = year, y = forecast_mean, color = "Forecast")) +
      geom_line(data = forecast_sub, aes(x = year, y = forecast_mean, color = "Forecast")) +
      scale_color_manual(name = "Type", values = c("Observed" = "black", "Fitted" = "blue", 
                                                   "Forecast" = "orange")) +
      scale_fill_manual(name = "Ribbon", values = c("Forecast CI" = "orange", "Fitted CI" = "lightblue" )) +
      labs( x = "Year", y = "biomass increment (Mg/ha)")+
      ggtitle(paste0(site, '; ', taxon)) +
      theme_light(base_size = 14)
    
    
    print(p) 
    
  }
  
}
dev.off()




#correlation of residuals using ggpairs CUMSUM
pdf('report/figures/ggpairs_residuals_ARIMA_cumsum.pdf')
#ggpairs for each site 
for (site in sites) {
  site_data <- res_wide_filter %>%
    dplyr::select(starts_with(site))
  
  # Skip if no matching columns (to avoid errors)
  if (ncol(site_data) == 0) next
  
  # Clean column names
  colnames(site_data) <- sub(".*_", "", colnames(site_data))
  
  # Plot
  print(ggpairs(data = site_data, title = paste(site, "Correlations")))
}
dev.off()




#plotting fitted/forecast vs observed with CI at each point 
pdf("report/figures/AGBI_fitted_vs_observed_filtered.pdf", width = 10, height = 8)
for (site in sites) {
  for (taxon in taxa) {
    print(paste(site, taxon))
    
    clim_agbi_sub <- filtered_AGBI %>%
      filter(site == !!site, taxon == !!taxon)
    clim_forecast_sub <- filtered_forecast2 %>%
      filter(site == !!site, taxon == !!taxon)
    
    if (nrow(clim_agbi_sub) == 0 ||
        all(is.na(clim_agbi_sub$AGBI.mean)) ||
        all(is.na(clim_agbi_sub$fitted))) next
    
    p <- ggplot() +
      geom_point(data = clim_agbi_sub, aes(x = AGBI.mean, y = fitted)) +
      geom_point(data = clim_forecast_sub, aes(x = AGBI.mean, y =forecast_mean), colour = "blue") +
      geom_errorbar(data = clim_agbi_sub, aes(x= AGBI.mean, ymin = fitted_lo, ymax = fitted_hi), width = 0.01, color = "gray40") +
      geom_errorbar(data = clim_forecast_sub, aes(x = AGBI.mean, ymin = forecast_lo, ymax = forecast_hi), width = 0.01, color = "blue") +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
      labs(x = "Observed AGBI.mean", y = "Fitted AGBI", title = paste0(site, "; ", taxon)) +
      theme_light(base_size = 14)
    
    print(p)
}
}
dev.off()


ggplot() +
  geom_point(data = fit_res_joined, aes(x = AGBI.mean, y = fitted, colour = "Fitted")) +
  geom_point(data = filtered_forecast2, aes(x = AGBI.mean, y =forecast_mean, colour = "Forecast")) +
  geom_errorbar(data = fit_res_joined, aes(x= AGBI.mean, ymin = fitted_lo, ymax = fitted_hi), width = 0.01, color = "gray40", alpha = 0.5) +
  geom_errorbar(data = filtered_forecast2, aes(x = AGBI.mean, ymin = forecast_lo, ymax = forecast_hi), width = 0.01, color = "orange") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  facet_wrap(~site, scales = "free")+
  scale_color_manual(name = "Type", values = c("Fitted" = "black", 
                                               "Forecast" = "orange")) +
  #scale_fill_manual(name = "Ribbon", values = c("Forecast CI" = "orange", "Fitted CI" = "lightblue" )) +
  labs(x = "Observed biomass increment", y = "Fitted and forecast increment")+
  theme_light(base_size = 14)
