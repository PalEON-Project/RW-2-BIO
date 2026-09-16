library(broom)
library(dplyr)
library(tidyr)
library(reshape2)
library(purrr)
library(ggplot2)
library(GGally)
library(stringr)
library(tidyverse)
library(patchwork)
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
#################loading biomass and climate data##########

#dataframe with year, site, taxon and AGBI.mean
AGBI_taxon = readRDS("reboot/AGBI_taxon_data.RDS")

head(AGBI_taxon)

#aggregating to site level from taxon data
AGBI_taxon_site = AGBI_taxon %>% group_by(year, model, site) %>% 
  dplyr::summarize(AGB.mid = sum(AGB.mid, na.rm=TRUE), AGBI.mid=sum(AGBI.mid, na.rm=TRUE))

#plotitng AGBI.mid over time (data from increment model)
#full data 1950-2011
ggplot(data= AGBI_taxon_site) +
  geom_line(aes(x=year, y= AGBI.mid, colour=site)) +
  theme_light(14) +
  labs( x = "Year", y = "biomass increment (Mg/ha)")

#adding lag years 1+2 to AGBI dateframe at the taxon level
AGBI_taxon = AGBI_taxon %>%
  group_by(site, taxon) %>%
  arrange(site, taxon, year) %>%
  mutate(AGBI.mid.prev1 = lag(AGBI.mid, n=1),
         AGBI.mid.prev2 = lag(AGBI.mid, n=2))



#adding lag years 1+2 to AGBI dateframe at the site level
AGBI_taxon_site = AGBI_taxon_site %>%
  group_by(site) %>%
  arrange(site, year) %>%
  mutate(AGBI.mid.prev1 = lag(AGBI.mid, n=1),
         AGBI.mid.prev2 = lag(AGBI.mid, n=2))


# climate data ------------------------------------------------------------
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


#list of climate predictors
predictor_names = c('PPT_winter', 'PPT_spring', 'PPT_summer', 'PPT_fall',
                    'Vpdmax_winter','Vpdmax_spring', 'Vpdmax_summer', 'Vpdmax_fall',
                    'Tmean_winter', 'Tmean_spring', 'Tmean_summer', 'Tmean_fall',
                    'Tmin_winter', 'Tmin_spring', 'Tmin_summer', 'Tmin_fall',
                    'Tmax_winter', 'Tmax_spring', 'Tmax_summer', 'Tmax_fall') 

######## reorganizing climate data to add tree_year #############

#using tree_yr function to make prev and current year 
#ex. year=1986 is sept-dec of 1985 and jan-aug of 1986
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
#renaming year_tree to year 
#year_tree is clim data from the previous year 
clim_wide_prev = rename(clim_wide_prev, year = year_tree)



#merging by year which is the prev sept- current august
#year_tree in this df is the year_tree from current df which makes it =year 
clim_wide = merge(clim_wide_current, clim_wide_prev, by = c('site', 'year'))


################################################################################
## merge climate data: TAXON
################################################################################

#wide format of climate variables with AGBI by tree_year
#taxon level
clim_agbi_taxon <- AGBI_taxon %>% 
  left_join(clim_wide, by = c('year', 'site'))

#new dataframe with seasonal climate data
#Across seasons, sum PPT, mean Tmean, max Tmax, min Tmin
clim_seasons_taxon = clim_agbi_taxon %>% 
  group_by(year, site, taxon) %>% 
  mutate(PPT_winter = sum(dplyr::pick('PPT_12', 'PPT_01', 'PPT_02')),
         PPT_spring = sum(dplyr::pick('PPT_03', 'PPT_04', 'PPT_05')),
         PPT_summer = sum(dplyr::pick('PPT_06', 'PPT_07', 'PPT_08')),
         PPT_fall = sum(dplyr::pick('PPT_09', 'PPT_10', 'PPT_11')),
         Vpdmax_winter = rowMeans(dplyr::pick('Vpdmax_12', 'Vpdmax_01','Vpdmax_02')),
         Vpdmax_spring = rowMeans(dplyr::pick('Vpdmax_03', 'Vpdmax_04','Vpdmax_05')),
         Vpdmax_summer = rowMeans(dplyr::pick('Vpdmax_06', 'Vpdmax_07','Vpdmax_08')),
         Vpdmax_fall = rowMeans(dplyr::pick('Vpdmax_09', 'Vpdmax_10','Vpdmax_11')),
         Tmin_winter = rowMeans(dplyr::pick('Tmin_12', 'Tmin_01', 'Tmin_02')),
         Tmin_spring = rowMeans(dplyr::pick('Tmin_03', 'Tmin_04', 'Tmin_05')),
         Tmin_summer = rowMeans(dplyr::pick('Tmin_06', 'Tmin_07', 'Tmin_08')),
         Tmin_fall = rowMeans(dplyr::pick('Tmin_09', 'Tmin_10', 'Tmin_11')),
         Tmax_winter = rowMeans(dplyr::pick('Tmax_12', 'Tmax_01', 'Tmax_02')),
         Tmax_spring = rowMeans(dplyr::pick('Tmax_03', 'Tmax_04', 'Tmax_05')),
         Tmax_summer = rowMeans(dplyr::pick('Tmax_06', 'Tmax_07', 'Tmax_08')),
         Tmax_fall = rowMeans(dplyr::pick('Tmax_09', 'Tmax_10', 'Tmax_11')),
         Tmean_winter = rowMeans(dplyr::pick('Tmean_12', 'Tmean_01', 'Tmean_02')),
         Tmean_spring = rowMeans(dplyr::pick('Tmean_03', 'Tmean_04', 'Tmean_05')),
         Tmean_summer = rowMeans(dplyr::pick('Tmean_06', 'Tmean_07', 'Tmean_08')),
         Tmean_fall = rowMeans(dplyr::pick('Tmean_09', 'Tmean_10', 'Tmean_11'))
  )

#seasonal clim variables in long format 
#separate name columns for variable name and season name
#ex. PPT and winter
clim_agbi_long_taxon <- clim_seasons_taxon %>%
  select(-matches("\\d+$"), -c(year_tree)) %>%  #dropping monthly clim variables
  pivot_longer(
    cols = all_of(predictor_names),
    names_to = "predictor",
    values_to = "climvar_value"
  )%>%
  separate(predictor, into = c("clim_var", "season"), sep = "_")

#seasonal clim variables in long format 
#one column for each varible_season combination
#ex. PPT_winter
clim_agbi_long2_taxon <- clim_seasons_taxon %>%
  select(-matches("\\d+$"), -c(year_tree)) %>%  #dropping monthly clim variables
  pivot_longer(
    cols = all_of(predictor_names),
    names_to = "coef_name",
    values_to = "climvar_value")



################################################################################
# merge climate data: SITE ------------------------------------------------
################################################################################

#wide format of climate variables with AGBI by tree_year
clim_agbi_site <- AGBI_taxon_site %>% 
  left_join(clim_wide, by = c('year', 'site'))


#new dataframe with seasonal climate data
#Across seasons, sum PPT, mean Tmean, max Tmax, min Tmin
clim_seasons_site = clim_agbi_site %>% 
  group_by(year, site) %>% 
  mutate(PPT_winter = sum(dplyr::pick('PPT_12', 'PPT_01', 'PPT_02')),
         PPT_spring = sum(dplyr::pick('PPT_03', 'PPT_04', 'PPT_05')),
         PPT_summer = sum(dplyr::pick('PPT_06', 'PPT_07', 'PPT_08')),
         PPT_fall = sum(dplyr::pick('PPT_09', 'PPT_10', 'PPT_11')),
         Vpdmax_winter = rowMeans(dplyr::pick('Vpdmax_12', 'Vpdmax_01','Vpdmax_02')),
         Vpdmax_spring = rowMeans(dplyr::pick('Vpdmax_03', 'Vpdmax_04','Vpdmax_05')),
         Vpdmax_summer = rowMeans(dplyr::pick('Vpdmax_06', 'Vpdmax_07','Vpdmax_08')),
         Vpdmax_fall = rowMeans(dplyr::pick('Vpdmax_09', 'Vpdmax_10','Vpdmax_11')),
         Tmin_winter = rowMeans(dplyr::pick('Tmin_12', 'Tmin_01', 'Tmin_02')),
         Tmin_spring = rowMeans(dplyr::pick('Tmin_03', 'Tmin_04', 'Tmin_05')),
         Tmin_summer = rowMeans(dplyr::pick('Tmin_06', 'Tmin_07', 'Tmin_08')),
         Tmin_fall = rowMeans(dplyr::pick('Tmin_09', 'Tmin_10', 'Tmin_11')),
         Tmax_winter = rowMeans(dplyr::pick('Tmax_12', 'Tmax_01', 'Tmax_02')),
         Tmax_spring = rowMeans(dplyr::pick('Tmax_03', 'Tmax_04', 'Tmax_05')),
         Tmax_summer = rowMeans(dplyr::pick('Tmax_06', 'Tmax_07', 'Tmax_08')),
         Tmax_fall = rowMeans(dplyr::pick('Tmax_09', 'Tmax_10', 'Tmax_11')),
         Tmean_winter = rowMeans(dplyr::pick('Tmean_12', 'Tmean_01', 'Tmean_02')),
         Tmean_spring = rowMeans(dplyr::pick('Tmean_03', 'Tmean_04', 'Tmean_05')),
         Tmean_summer = rowMeans(dplyr::pick('Tmean_06', 'Tmean_07', 'Tmean_08')),
         Tmean_fall = rowMeans(dplyr::pick('Tmean_09', 'Tmean_10', 'Tmean_11'))
  )

#seasonal clim variables in long format 
#separate name columns for variable name and season name
#ex. PPT and winter
clim_agbi_long_site <- clim_seasons_site %>%
  select(-matches("\\d+$"), -c(year_tree)) %>%  #dropping monthly clim variables
  pivot_longer(
    cols = all_of(predictor_names),
    names_to = "predictor",
    values_to = "climvar_value"
  )%>%
  separate(predictor, into = c("clim_var", "season"), sep = "_")

#seasonal clim variables in long format 
#one column for each varible_season combination
#ex. PPT_winter
clim_agbi_long2_site <- clim_seasons_site %>%
  select(-matches("\\d+$"), -c(year_tree)) %>%  #dropping monthly clim variables
  pivot_longer(
    cols = all_of(predictor_names),
    names_to = "coef_name",
    values_to = "climvar_value")




# correlation of biomass increment data with climate data -----------------

#correlation df climate data vs. AGBI.mean
cor_df_taxon <- clim_agbi_long_taxon %>%
  group_by(site, taxon, clim_var, season) %>%
  summarise(
    cor = cor(climvar_value, AGBI.mid, use = "complete.obs"),
    pval = cor.test(climvar_value, AGBI.mid)$p.value,
    .groups = "drop"
  ) %>%
  mutate(sig = ifelse(pval < 0.05, TRUE, NA),
         season = factor(season, levels = c("winter", "spring", "summer", "fall")))

# #correlation between climate and AGBI at each site for each season
# pdf("report/figures/AGBI_predictor_correlations.pdf", width = 10, height = 8)
# for (s in unique(cor_df$site)) {
#   for (ss in levels(cor_df$season)) {
#     
#     p <- ggplot(filter(cor_df, site == s, season == ss),
#                 aes(x = clim_var, y = taxon, fill = cor)) +
#       geom_tile(color = "white") +
#       geom_point(aes(shape = sig), color = "black", size = 2, na.rm = TRUE) +
#       scale_fill_gradient2(low = "blue", mid = "white", high = "red", limits = c(-1, 1)) +
#       scale_shape_manual(values = c(16, NA)) +
#       theme_minimal(base_size = 14) +
#       labs(title = paste("Correlation between predictors and AGBI.mean\nSite:", s, "- Season:", ss),
#            x = "Predictor",
#            y = "Taxon",
#            fill = "Correlation",
#            shape = "Significant") +
#       theme(axis.text.x = element_text(angle = -45, hjust = 0))
#     
#     print(p)
#   }
# }
# dev.off()


# #plotting AGBI vs climate correlation facet wrap by season 
# #each season for each site on one page
# # correlation between climate and AGBI
# pdf("reboot/figures/AGBI_predictor_correlations_facetwrap.pdf", width = 12, height = 8)
# 
# for (s in unique(cor_df_taxon$site)) {
#   
#   p <- ggplot(filter(cor_df, site == s),
#               aes(x = clim_var, y = taxon, fill = cor)) +
#     geom_tile(color = "white") +
#     geom_point(aes(shape = sig), color = "black", size = 2, na.rm = TRUE) +
#     scale_fill_gradient2(low = "blue", mid = "white", high = "red", limits = c(-1, 1)) +
#     scale_shape_manual(values = c(16, NA)) +
#     theme_minimal(base_size = 14) +
#     labs(title = paste("Correlation between predictors and AGBI.mean\nSite:", s),
#          x = "Predictor",
#          y = "Taxon",
#          fill = "Correlation",
#          shape = "Significant") +
#     theme(axis.text.x = element_text(angle = -45, hjust = 0)) +
#     facet_wrap(~season)   #puts all seasons for this site on one page
#   
#   print(p)
# }
# 
# dev.off()

# ARIMA taxon Model -------------------------------------------------------------

### ADD ARIMA

#setting up for forecast values
#Remove last five years
clim_agbi_in_taxon <- clim_seasons_taxon %>%
  filter(year < 2007) %>%
  select(-matches("_[0-9]+$"))   # drop columns ending with _ + one or more digits

clim_agbi_out_taxon <- clim_seasons_taxon %>%
  filter(year > 2006) %>%
  select(-matches("_[0-9]+$"))   # same for forecast period


#uses seasonal climate data 
#making a time series 
# Fit ARIMA models by site and taxon
models_taxon <- clim_agbi_in_taxon %>%
  dplyr::group_by(site, taxon) %>%
  dplyr::do({
    df <- .
    # 
    # Drop rows with NA in response or predictors
    if (any(is.na(df$AGBI.mid)) || any(is.na(df[predictor_names]))) {
      dplyr::tibble(mod = list(NULL), note = "Missing data")
    }
    
    # Response variable as time series
    agbi_ts <- ts(df$AGBI.mid, start = min(df$year), frequency = 1)

    # External regressors
    xreg <- as.matrix(df %>% dplyr::select(dplyr::all_of(predictor_names)))
    
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
#ex. GOOSE_ACRU
models_taxon <- dplyr::mutate(models_taxon, model = paste0(site, "_", taxon))

# ARIMA site Model -------------------------------------------------------------

### ADD ARIMA

#setting up for forecast values
#Remove last five years
clim_agbi_in_site <- clim_seasons_site %>%
  filter(year < 2007) %>%
  select(-matches("_[0-9]+$"))   # drop columns ending with _ + one or more digits

clim_agbi_out_site <- clim_seasons_site %>%
  filter(year > 2006) %>%
  select(-matches("_[0-9]+$"))   # same for forecast period


#uses seasonal climate data 
#making a time series 
# Fit ARIMA models by site
models_site <- clim_agbi_in_site %>%
  group_by(site) %>%
  do({
    df <- .
    
    # Drop rows with NA in response or predictors
    if (any(is.na(df$AGBI.mid)) || any(is.na(df[predictor_names]))) {
      tibble(mod = list(NULL), note = "Missing data")
    }
    
    # Response variable as time series
    agbi_ts <- ts(df$AGBI.mid, start = min(df$year), frequency = 1)
    
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
#ex. GOOSE_ACRU
models_site <- dplyr::mutate(models_site, model = site)

########### forecasting taxon model #################################

#forecasting
fcast_taxon <- models_taxon %>%
  filter(!is.null(mod[[1]])) %>%
  mutate(
    forecast = pmap(list(mod, site, taxon), function(model_obj, site_val, taxon_val) {
      if (is.null(model_obj)) return(NA)
      
      # Get future predictors for the same group
      future_data <- clim_agbi_out_taxon %>%
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
fcast_taxon


#pulling forcast values into a dataframe of lists with forecast mean,lo,hi
fcast_values_taxon = lapply(fcast_taxon$forecast, function(x){data.frame(year = as.vector(time(x$mean)),
                                                              forecast_mean = x$mean, 
                                                              forecast_lo = x$lower[,"95%"], 
                                                              forecast_hi = x$upper[,"95%"])})
saveRDS(fcast_values_taxon, "reboot/forecast_taxon.RDS")

#lower, upper, and mean of predictions from 2007-2011 in a dataframe
#site, taxon, year, forecast_mean, forecast_low, forecast_high
fcast_long_taxon = data.frame(site = rep(fcast_taxon[[1]], each=5), 
                 taxon = rep(fcast_taxon[[2]], each=5), 
                 bind_rows(fcast_values_taxon))

saveRDS(fcast_long_taxon, "reboot/forecast_taxon_long.RDS")

# extracting taxon model data ---------------------------------------------

#residuals and fitted values in long format
#pulling fitted, and residuals from the forecast model in a dataframe for the taxon model
fitted_res_taxon <- fcast_taxon %>%
  mutate(sigma2 = purrr::map_dbl(mod, ~ .x$sigma2)) %>%
  select(forecast, site, taxon, sigma2) %>%
  pmap_dfr(function(forecast, site, taxon, sigma2) {
    
    if (is.null(forecast)) return(NULL)
    
    tibble(
      site      = site,
      taxon     = taxon,
      year      = as.vector(time(forecast$fitted)),
      fitted    = as.numeric(forecast$fitted),
      residuals = as.numeric(forecast$residuals),
      sigma2    = sigma2
    )
  })

fitted_res_ci_taxon <- fitted_res_taxon %>%
  mutate(
    fitted_lo = fitted - 1.96 * sqrt(sigma2),
    fitted_hi = fitted + 1.96 * sqrt(sigma2)
  )


saveRDS(fitted_res_ci_taxon, "reboot/fitted_taxon.RDS")

# forecasting site level --------------------------------------------------

fcast_site = models_site %>% 
  filter(!purrr::map_lgl(mod, is.null)) %>%
  mutate(
    forecast = pmap(
      list(mod, site),
      function(model_obj, site_val) {
        
        if (is.null(model_obj)) return(NA)
        
        future_data <- clim_agbi_out_site %>%
          filter(site == site_val) %>%
          arrange(year)
        
        if (nrow(future_data) == 0) return(NA)
        
        future_xreg <- future_data %>%
          ungroup() %>% 
          select(all_of(predictor_names)) %>% 
          as.matrix()
        
        tryCatch({
          forecast::forecast(
            model_obj, 
            xreg = future_xreg
          )
        }, error = function(e) {
          warning(
            paste(
              "Forecast failed for", 
              site_val, 
              ":", 
              e$message
            )
          )
          return(NA)
        })
      }
    ))

#checking object
fcast_site

# Pulling forecast values into a dataframe of lists
# with forecast mean, lower, and upper 95% CI
fcast_values_site <- lapply(fcast_site$forecast, function(x) {
  
  if (is.null(x)) return(NULL)
  
  data.frame(
    year = as.vector(time(x$mean)),
    forecast_mean = as.numeric(x$mean),
    forecast_lo = x$lower[, "95%"],
    forecast_hi = x$upper[, "95%"]
  )
})
  

saveRDS(fcast_values_site, "reboot/forecast_site.RDS")

fcast_long_site = data.frame(
  site = rep(fcast_site[[1]], each = 5), 
  bind_rows(fcast_values_site)
)

saveRDS(fcast_long_site, "reboot/forecast_site_long.RDS")

# extracting site model data ----------------------------------------------

fitted_res_site <- models_site %>%
  mutate(sigma2 = purrr::map_dbl(mod, ~ .x$sigma2)) %>%
  select(site, mod, sigma2) %>%
  pmap_dfr(function(site, mod, sigma2) {
    
    if (is.null(mod)) return(NULL)
    
    tibble(
      site      = site,
      year      = as.vector(time(mod$fitted)),
      site_fitted    = as.numeric(mod$fitted),
      residuals = as.numeric(mod$residuals),
      sigma2    = sigma2
    )
  })

fitted_res_ci_site <- fitted_res_site %>%
  mutate(
    fitted_lo = site_fitted - 1.96 * sqrt(sigma2),
    fitted_hi = site_fitted + 1.96 * sqrt(sigma2)
  )

saveRDS(fitted_res_ci_site, "reboot/fitted_site.RDS")

# summing taxon AGBI from ARIMA model to get total site AGBI --------

#summing AGBI at a given site to plot total fitted AGBI from the taxon model
#only goes up to year 2006 since we used model data
arima_taxon_2_site = fitted_res_taxon %>%
  group_by(year, site) %>%
  dplyr::summarise(taxon_2site_fitted = sum(fitted, na.rm=TRUE))



# joining data with fitted taxa -------------------------------------------

# #joining observed AGBI with taxon model AGBI 
# #DATA AGBI from AGB.data
# #goes up to 2006
joined_AGBI_taxon = inner_join(fitted_res_taxon, select(AGBI_taxon, c(year, taxon, site, AGBI.mid)), 
                               by = c("site", "taxon", "year"))

saveRDS(joined_AGBI_taxon, "reboot/joined_taxon_AGBI.RDS")

joined_AGBI_taxon_long <- joined_AGBI_taxon %>%
  pivot_longer(
    cols = c(fitted, AGBI.mid), # Columns to transform
    names_to = "AGBI_type",      # New column for the old column names
    values_to = "value"      # New column for the cell values
  )
#saveRDS(joined_AGBI_taxon_long, "reboot/joined_taxon_AGBI.RDS")


# joining data with fitted site data across all types ----------------------------------


# #joining observed AGBI with sum taxon model AGBI and site model AGBI
# #DATA AGBI from AGB.data
# #goes up to 2006
joined_site_AGBIs = left_join(arima_taxon_2_site,
                        select(fitted_res_site, c(year, site, site_fitted)),
                        by = c("site", "year")) %>%
  inner_join(select(AGBI_taxon_site, c(year, site, AGBI.mid)), by = c("site", "year"))

saveRDS(joined_site_AGBIs, "reboot/joined_site_AGBIs.RDS")


#site AGBIs in long format 
AGBI_sites_long <- joined_site_AGBIs %>%
  pivot_longer(
    cols = c(taxon_2site_fitted, site_fitted, AGBI.mid), # Columns to transform
    names_to = "AGBI_type",      # New column for the old column names
    values_to = "value"      # New column for the cell values
)

#plotting site AGBI types across time
#data, fitted site from model, taxon aggregated to site from fitted 
ggplot()+
  geom_line(data = AGBI_sites_long, aes(x=year, y = value, colour = AGBI_type))+
  theme_light(14)+
  facet_wrap(~site)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
