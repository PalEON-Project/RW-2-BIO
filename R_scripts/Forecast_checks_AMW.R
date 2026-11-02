#### EDITS/TESTS FOR FORECASTING COMPONENT OF agbi_clim_model.R

#### 1. Looking at influence of phi & sigma on forecast uncertainty ####

## After running through line 449 in agbi_clim_model.R...

## Extract all the ar1 coefficients (phi)
## Also included their uncertainty but ended up not paying attention
ar1_taxon <- fcast_taxon %>%
  ungroup() %>%
  mutate(
    phi = map_dbl(mod, function(m) {
      if (is.null(m) || !("ar1" %in% names(coef(m)))) return(NA_real_)
      unname(coef(m)["ar1"])
    }),
    phi_se = map_dbl(mod, function(m) {
      if (is.null(m) || !("ar1" %in% names(coef(m)))) return(NA_real_)
      sqrt(diag(m$var.coef))[["ar1"]]
    }),
    sigma = map_dbl(mod, function(m) {
      if (is.null(m)) return(NA_real_)
      sqrt(m$sigma2)
    })
  ) %>%
  select(site, taxon, phi, phi_se, sigma)

# Look at summary
ar1_taxon
# Summary stats of just ar[1] coefficients
summary(ar1_taxon$phi)

## Look at the difference in the uncertainty in the forecasts for 2007
## (first year of forecast) and 2011 (last year of forecast)
## I'm looking to see if:
## 1) Uncertainty grows at all
## 2) Uncertainty growth is related to phi (which would mean the model
##    is working correctly)
se_diff_taxon <- fcast_taxon %>%
  ungroup() %>%
  mutate(
    se_2007 = map_dbl(forecast, function(f) {
      if (!inherits(f, "forecast")) return(NA_real_)
      yrs <- as.vector(time(f$mean))
      (f$upper[, "95%"] - f$lower[, "95%"])[yrs == 2007] / (2 * 1.96)
    }),
    se_2011 = map_dbl(forecast, function(f) {
      if (!inherits(f, "forecast")) return(NA_real_)
      yrs <- as.vector(time(f$mean))
      (f$upper[, "95%"] - f$lower[, "95%"])[yrs == 2011] / (2 * 1.96)
    }),
    se_diff = se_2011 - se_2007
  ) %>%
  select(site, taxon, se_2007, se_2011, se_diff)

# Add the uncertainty difference between 2011 and 2007 to the previous df
ar1_taxon <- left_join(ar1_taxon, se_diff_taxon, by = c("site", "taxon"))

# Histogram of change in uncertainty--
# how much does uncertainty change over time across models?
hist(ar1_taxon$se_diff) # Very little change in uncertainty

# Plot of relationship between phi and change in se
plot(ar1_taxon$phi, ar1_taxon$se_diff) # shows model is working properly

# Plot of relationship between phi and change in se scaled by sigma
plot(ar1_taxon$phi, ar1_taxon$se_diff / ar1_taxon$sigma) # better proof that model is working properly

#### 2. New version with order ARIMA(1, 1, 0) ####

## This section uses the same code, but adds d = 1
## This accounts for long-term trends in AGBI, and therefore
## frees up the AR(1) term to account for year-to-year variation
## over shorter timescales

# Exact same code, but with order = c(1, 1, 0)
TEST_models_taxon <- clim_agbi_in_taxon %>%
  dplyr::group_by(site, taxon) %>%
  dplyr::do({
    df <- .
    # 
    # Drop rows with NA in response or predictors
    if (any(is.na(df$AGBI.mean)) || any(is.na(df[predictor_names]))) {
      dplyr::tibble(mod = list(NULL), note = "Missing data")
    }
    
    # Response variable as time series
    agbi_ts <- ts(df$AGBI.mean, start = min(df$year), frequency = 1)
    
    # External regressors
    xreg <- as.matrix(df %>% dplyr::select(dplyr::all_of(predictor_names)))
    
    # Fit ARIMA model
    model <- tryCatch({
      forecast::Arima(
        y = agbi_ts,
        order = c(1, 1, 0),
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
TEST_models_taxon <- dplyr::mutate(TEST_models_taxon, model = paste0(site, "_", taxon))


#forecasting but with ARIMA(1, 1, 0)
TEST_fcast_taxon <- TEST_models_taxon %>%
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

#### 3. Look again at the influence of phi & sigma on forecast uncertainty ####

# Exact same code as before, but now with d = 1 model
TEST_ar1_taxon <- TEST_fcast_taxon %>%
  ungroup() %>%
  mutate(
    phi = map_dbl(mod, function(m) {
      if (is.null(m) || !("ar1" %in% names(coef(m)))) return(NA_real_)
      unname(coef(m)["ar1"])
    }),
    phi_se = map_dbl(mod, function(m) {
      if (is.null(m) || !("ar1" %in% names(coef(m)))) return(NA_real_)
      sqrt(diag(m$var.coef))[["ar1"]]
    }),
    sigma = map_dbl(mod, function(m) {
      if (is.null(m)) return(NA_real_)
      sqrt(m$sigma2)
    })
  ) %>%
  select(site, taxon, phi, phi_se, sigma)

TEST_ar1_taxon
summary(TEST_ar1_taxon$phi) # note change in the summary of phi

TEST_se_diff_taxon <- TEST_fcast_taxon %>%
  ungroup() %>%
  mutate(
    se_2007 = map_dbl(forecast, function(f) {
      if (!inherits(f, "forecast")) return(NA_real_)
      yrs <- as.vector(time(f$mean))
      (f$upper[, "95%"] - f$lower[, "95%"])[yrs == 2007] / (2 * 1.96)
    }),
    se_2011 = map_dbl(forecast, function(f) {
      if (!inherits(f, "forecast")) return(NA_real_)
      yrs <- as.vector(time(f$mean))
      (f$upper[, "95%"] - f$lower[, "95%"])[yrs == 2011] / (2 * 1.96)
    }),
    se_diff = se_2011 - se_2007
  ) %>%
  select(site, taxon, se_2007, se_2011, se_diff)

TEST_ar1_taxon <- left_join(TEST_ar1_taxon, TEST_se_diff_taxon, by = c("site", "taxon"))

hist(TEST_ar1_taxon$se_diff) # note larger change in uncertainty between beginning and end of forecast

plot(TEST_ar1_taxon$phi, TEST_ar1_taxon$se_diff) # funkier relationship without accounting for sigma-- less influence of phi in many models
plot(TEST_ar1_taxon$phi, TEST_ar1_taxon$se_diff / TEST_ar1_taxon$sigma) # perfect relationship when accounting for sigma

#### 4. Plotting the full fitting and forecast periods with uncertainty ####

## This is basically the same as what Ruby does in the plotting code,
## but with the ARIMA(1, 1, 0) model

# Build one data frame with fit-period and forecast-period predictions ------
TEST_pred_taxon <- map_dfr(seq_len(nrow(TEST_fcast_taxon)), function(i) {
  f <- TEST_fcast_taxon$forecast[[i]]
  m <- TEST_fcast_taxon$mod[[i]]
  if (!inherits(f, "forecast")) return(NULL)
  
  sigma <- sqrt(m$sigma2)
  
  # Fit period: fitted values with one-step-ahead 95% interval
  fit <- tibble(
    year   = as.vector(time(f$fitted)),
    mean   = as.numeric(f$fitted),
    lo     = mean - 1.96 * sigma,
    hi     = mean + 1.96 * sigma,
    period = "Fitted",
    obs    = as.numeric(f$x)
  )
  
  # Forecast period: forecast mean with 95% interval
  fc <- tibble(
    year   = as.vector(time(f$mean)),
    mean   = as.numeric(f$mean),
    lo     = as.numeric(f$lower[, "95%"]),
    hi     = as.numeric(f$upper[, "95%"]),
    period = "Forecast",
    obs    = NA_real_
  )
  
  bind_rows(fit, fc) %>%
    mutate(site = TEST_fcast_taxon$site[i], taxon = TEST_fcast_taxon$taxon[i])
})

# Plot function for one site x taxon ----------------------------------------
plot_agbi_trend <- function(site_val, taxon_val, data = TEST_pred_taxon) {
  d <- filter(data, site == site_val, taxon == taxon_val)
  
  ggplot(d, aes(year, mean)) +
    geom_ribbon(aes(ymin = lo, ymax = hi, fill = period), alpha = 0.25) +
    geom_line(aes(color = period), linewidth = 0.9) +
    geom_point(aes(y = obs), size = 1, color = "grey30", na.rm = TRUE) +
    geom_vline(xintercept = 2006.5, linetype = "dashed") +
    labs(x = "Year", y = "AGBI",
         title = paste(site_val, taxon_val),
         color = NULL, fill = NULL) +
    theme_minimal()
}

# Unique combinations of site and taxa
# Note that I was lazy here and didn't remove combinations that didn't
# occur in the data. I removed those combinations by manually deleting
# PDF pages for now
unique_site <- unique(TEST_pred_taxon$site)
unique_taxon <- unique(TEST_pred_taxon$taxon)
unique_comb <- expand.grid(unique_site, unique_taxon)

# Plot into random PDF
pdf(file = 'test.pdf')

for(i in 1:nrow(unique_comb)){
  print(plot_agbi_trend(unique_comb[i,1], unique_comb[i,2]))
}

dev.off()

