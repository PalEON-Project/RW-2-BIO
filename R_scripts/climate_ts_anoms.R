################################################################################
# Identify anomalous annual climate values from the monthly PRISM data.
#
# For each site and annual climate variable, the mean across the full available
# time series is the climate norm. Anomalies are departures greater than two
# standard deviations above or below that norm.
################################################################################

rm(list = ls())

# Read the climate data into a separate environment so loading the .RData file
# does not add unrelated objects to the current R session.
data_path <- "climate/prism_clim.RData"
climate_env <- new.env()
load(data_path, envir = climate_env)

# The input file contains monthly records in the prism_long data frame.
if (!exists("prism_long", envir = climate_env, inherits = FALSE)) {
  stop("Expected to find an object named 'prism_long' in ", data_path)
}
prism_long <- climate_env$prism_long

# List the climate measurements to analyze. Precipitation is accumulated over
# each year; temperature and vapor-pressure-deficit variables are averaged.
climate_variables <- c(
  "PPT2", "Tmean2", "Tmin2", "Tmax2", "Vpdmax2"
)
missing_variables <- setdiff(climate_variables, names(prism_long))
if (length(missing_variables) > 0) {
  stop("Missing expected climate columns: ", paste(missing_variables, collapse = ", "))
}

# Summarize each location-year, keeping only years with all 12 distinct months.
# Excluding incomplete years avoids comparing partial annual precipitation
# totals or incomplete averages with complete years (for example, 2024).
monthly_groups <- split(
  prism_long,
  interaction(prism_long$loc, prism_long$year, drop = TRUE)
)
annual_summaries <- lapply(monthly_groups, function(monthly_data) {
  observed_months <- sort(unique(as.integer(monthly_data$month)))
  is_complete_year <- nrow(monthly_data) == 12L &&
    identical(observed_months, 1:12)

  annual_values <- vapply(climate_variables, function(variable) {
    values <- monthly_data[[variable]]
    if (variable == "PPT2") {
      sum(values)
    } else {
      mean(values)
    }
  }, numeric(1))

  data.frame(
    loc = monthly_data$loc[[1]],
    year = as.integer(monthly_data$year[[1]]),
    complete_year = is_complete_year,
    as.list(annual_values),
    check.names = FALSE
  )
})
annual_climate <- do.call(rbind, annual_summaries)
rownames(annual_climate) <- NULL
annual_climate <- annual_climate[annual_climate$complete_year, ]

# Calculate departures from the full-series climate norm and standardized
# anomalies. The norm is the arithmetic mean across all available complete
# annual values for one site-variable series; the scale is its standard
# deviation. Positive/negative z-scores indicate above-/below-normal values.
find_climate_anomalies <- function(x, t0 = 2) {
  n <- length(x)
  climate_norm <- mean(x, na.rm = TRUE)
  climate_sd <- sd(x, na.rm = TRUE)
  if (!is.finite(climate_norm)) climate_norm <- NA_real_
  baseline <- rep(climate_norm, n)
  anomaly_value <- x - climate_norm
  zscore <- anomaly_value / climate_sd
  anomaly <- rep(FALSE, n)
  adjusted_value <- x

  # A constant or single-value series has no estimable variability and cannot
  # be classified as an outlying standardized anomaly.
  if (!is.finite(climate_norm) || !is.finite(climate_sd) || climate_sd == 0) {
    zscore[] <- NA_real_
    return(list(
      baseline = baseline,
      anomaly_value = anomaly_value,
      zscore = zscore,
      anomaly = anomaly,
      adjusted_value = adjusted_value
    ))
  }

  anomaly <- !is.na(zscore) & abs(zscore) > t0
  adjusted_value[anomaly] <- climate_norm

  list(
    baseline = baseline,
    anomaly_value = anomaly_value,
    zscore = zscore,
    anomaly = anomaly,
    adjusted_value = adjusted_value
  )
}

# Run the full-series detector separately for every climate variable at every
# location, sorting years for consistent output.
t0 <- 2
anomaly_results <- list()
result_index <- 1L

for (location in unique(annual_climate$loc)) {
  location_data <- annual_climate[annual_climate$loc == location, ]
  location_data <- location_data[order(location_data$year), ]

  for (variable in climate_variables) {
    detection <- find_climate_anomalies(location_data[[variable]], t0 = t0)

    anomaly_results[[result_index]] <- data.frame(
      loc = location,
      year = location_data$year,
      variable = variable,
      value = location_data[[variable]],
      baseline = detection$baseline,
      anomaly_value = detection$anomaly_value,
      zscore = detection$zscore,
      anomaly = detection$anomaly,
      adjusted_value = detection$adjusted_value
    )
    result_index <- result_index + 1L
  }
}

# Store one row per location, year, and climate variable, including the
# measured value, full-series mean norm, anomaly departure, standardized score,
# anomaly flag, and mean-replacement value for flagged observations.
climate_year_anoms <- do.call(rbind, anomaly_results)
rownames(climate_year_anoms) <- NULL

# Save the results separately from the input climate data. Paths are relative
# to the project root, as in the other project scripts.
output_path <- "climate/prism_clim_anoms.RDS"
saveRDS(climate_year_anoms, output_path)

# Print the number of flagged years for each location-variable combination.
# The full flagged records remain available in climate_year_anoms and the RDS.
print(xtabs(as.integer(anomaly) ~ loc + variable, data = climate_year_anoms))

# Plot each climate variable in its own figure, with a separate panel for each
# location. Vertical lines mark flagged years: red indicates a negative
# anomaly and blue indicates a positive anomaly. These plots are displayed but
# are not saved to files.
library(ggplot2)

climate_anomaly_plots <- setNames(vector("list", length(climate_variables)), climate_variables)

for (variable in climate_variables) {
  # Limit the displayed time series to 1950 onward while retaining the full
  # historical series above for computing the full-series climate norm.
  plot_data <- climate_year_anoms[
    climate_year_anoms$variable == variable & climate_year_anoms$year >= 1950,
  ]
  anomaly_data <- plot_data[plot_data$anomaly, ]
  norm_data <- unique(plot_data[c("loc", "baseline")])
  anomaly_data$direction <- ifelse(
    anomaly_data$zscore < 0,
    "Negative anomaly",
    "Positive anomaly"
  )

  climate_anomaly_plots[[variable]] <- ggplot(
    plot_data,
    aes(x = year, y = value, group = loc)
  ) +
    geom_line(color = "grey35", na.rm = TRUE) +
    geom_hline(
      data = norm_data,
      aes(yintercept = baseline),
      inherit.aes = FALSE,
      color = "grey55",
      linetype = "dashed"
    ) +
    geom_vline(
      data = anomaly_data,
      aes(xintercept = year, color = direction),
      inherit.aes = FALSE,
      linewidth = 0.5,
      alpha = 0.8
    ) +
    facet_wrap(vars(loc), scales = "free_y", ncol = 2) +
    scale_color_manual(
      name = "Anomaly direction",
      values = c("Negative anomaly" = "red", "Positive anomaly" = "blue")
    ) +
    labs(
      title = paste(variable, "annual climate values by location"),
      x = "Year",
      y = variable
    ) +
    theme_minimal()

  print(climate_anomaly_plots[[variable]])
}
