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
    
    data.frame(
      site = site_val,
      taxon = taxon_val,
      year = as.vector(time(fcast$fitted)),
      fitted = as.numeric(fcast$fitted),
      residuals = as.numeric(fcast$residuals)
    )
  }
)

#sd of the residuals 
residuals_sd = fit_res_long %>%
  group_by(site, taxon) %>%
  summarise(sd_residuals = sd(residuals, na.rm = TRUE))

#residuals standardized
residuals_standardized <- fit_res_long %>%
  left_join(residuals_sd, by = c("site", "taxon")) %>%
  mutate(residuals_standardized = residuals / sd_residuals)


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



# PLOTTING ----------------------------------------------------------------
sites <- c("GOOSE", "ROOSTER", "HARVARD", "HMC", "NRP", "SYLVANIA")
taxa = (unique(clim_agbi$taxon))

#fitted values plotted for each taxa and site 
pdf("report/figures/AGBI_fitted_forecast.pdf", width=10, height=8)
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
    
    res_fit_sub = residuals_standardized %>% 
      filter(site == !!site,
             taxon == !!taxon)
    if (nrow(res_fit_sub) == 0){ next}
    
   p = ggplot() +
     geom_point(data = clim_agbi_sub, aes(x = year, y = AGBI.mean, color = "Observed")) +
     geom_point(data = res_fit_sub, aes(x = year, y = fitted, color = "Fitted")) +
     geom_point(data = res_fit_sub, aes(x = year, y = residuals, color = "Residuals")) +
     geom_point(data = res_fit_sub, aes(x = year, y = residuals_standardized, color = "Residuals standardize")) +
     geom_ribbon(data = forecast_sub, aes(x = year, ymin = forecast_lo, ymax = forecast_hi, fill = "Forecast CI"), alpha = 0.5) +
     geom_point(data = forecast_sub, aes(x = year, y = forecast_mean, color = "Forecast")) +
     scale_color_manual(name = "Type", values = c("Observed" = "black", "Fitted" = "blue", 
                                                  "Forecast" = "orange", "Residuals" = "red", 
                                                  "Residuals standardize" = "purple")) +
     scale_fill_manual(name = "Ribbon", values = c("Forecast CI" = "gray")) +
     ggtitle(paste0(site, '; ', taxon)) +
     theme_light(base_size = 14)
   
     
   print(p) 
    
  }
  
}
dev.off()


#residuals plotted
pdf("report/figures/AGBI_residuals_forecast.pdf", width = 10, height = 8)
for (site in sites) {
  for (taxon in taxa) {
    print(site)
    print(taxon)
    
    # Define disturbance years once, outside the loop
    disturbance_years <- list(
      GOOSE = 1981,
      ROOSTER = c(1983, 1992),
      HARVARD = 1981
     # NRP = 1980,
      # SYLVANIA = 1990,
      # HMC = 2000
    )
    
    # Filter data for current site and taxon
    res_fit_sub <- fit_res_long %>%
      filter(site == !!site, taxon == !!taxon)
    
    if (nrow(res_fit_sub) == 0) next
    
    # Get the disturbance year for the current site
    disturbance <- disturbance_years[[site]]
    
    # Plot
    p <- ggplot() +
      geom_line(data = res_fit_sub, aes(x = year, y = residuals, color = "Residuals")) +
      geom_vline(xintercept = disturbance, linetype = "dashed", color = "red") +
      ggtitle(paste0(site, "; ", taxon)) +
      theme_light(base_size = 14) +
      scale_color_manual(values = c("Residuals" = "blue")) +
      labs(color = "")
    
    print(p)
  }
}

dev.off()


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



