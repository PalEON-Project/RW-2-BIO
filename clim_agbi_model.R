library(broom)
library(dplyr)


#arima(xreg = INDEPENDENT.VARS)

AGBI_data = readRDS("AGBI_taxon_data.RDS")

load('climate/prism_clim.RData')
clim_data = prism_long

df = subset(clim_data, select = -c(Vpdmin2) )

clim_data = df %>% 
  dplyr::rename(site = loc, PPT = PPT2, Tmean = Tmean2, Tmin = Tmin2, Tmax = Tmax2, 
                 Vpdmax = Vpdmax2)
clim_data$year <- as.numeric(clim_data$year)

#putting the climat variables in wide format
clim_wide =  pivot_wider(data = clim_data,
                         names_from = month, 
                         values_from = c(PPT, Tmean, Tmin, Tmax, Vpdmax))
#long format
clim_agbi_long <- AGBI_data %>% 
  left_join(clim_data, by = c('year', 'site'))

#wide format
clim_agbi <- AGBI_data %>% 
  left_join(clim_wide, by = c('year', 'site'))

clim_agbi$ = NA

models <- clim_agbi %>% 
  group_by(site, taxon) %>%
  do({
    predictors <- names(.)[13:72]
    formula <- reformulate(predictors, response = "AGBI.mean")
    model <- lm(formula, data = .)
    tibble(mod = list(model))
  })


#rsq = models %>% rowwise() %>% mutate(mod$r.squared)

summary(models[[3]][1][[1]])

#R2 for the models
rsq_values <- models %>%
  mutate(glance = map(mod, glance)) %>%
  unnest(glance) %>%
  dplyr::select(site, taxon, r.squared)


wtr_yr <- function(df, start_month=12) {
  # Year offset
  offset = ifelse(as.numeric(df$month) >= start_month,  1, 0)
  # Water year
  adj.year = as.numeric(df$year) + offset
  # Return the water year
  adj.year
}

foo = clim_agbi_long %>%  
  group_by(year, site, taxon) %>% 
  dplyr::mutate(prev_abgi = wtr_yr(.))
  

clim_agbi_long$year_water = wtr_yr(clim_agbi_long)

AGBI_prev = clim_agbi_long[,c('year', 'month', 'site', 'year_water')]

water <- AGBI_prev %>%
  left_join(AGBI_data, by = c('year_water'='year', 'site'))
