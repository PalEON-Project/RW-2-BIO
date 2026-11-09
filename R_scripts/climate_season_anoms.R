################################################################################
# Identify anomalous seasonal climate values from clim_seasons_taxon.RDS.
#
# For each site-season-variable series, the mean across all available years is
# the climate norm. Anomalies are departures greater than two standard
# deviations above or below that norm.
################################################################################

rm(list = ls())

# Read the seasonal climate data. Paths are relative to the project root.
data_path <- "reboot/clim_seasons.RDS"
seasonal_data <- as.data.frame(readRDS(data_path))

# The seasonal values are repeated across taxa in this taxon-level data set,
# but climate itself is shared within each site-year. Select the climate fields
# and retain one row for each distinct site-year climate record.
seasonal_measures <- c("PPT", "Vpdmax", "Tmin", "Tmax", "Tmean")
seasons <- c("winter", "spring", "summer", "fall")
climate_variables <- unname(unlist(lapply(
  seasonal_measures,
  function(measure) paste(measure, seasons, sep = "_")
), use.names = FALSE))
required_columns <- c("year", "site", climate_variables)
missing_columns <- setdiff(required_columns, names(seasonal_data))
if (length(missing_columns) > 0) {
  stop("Missing expected columns in ", data_path, ": ",
       paste(missing_columns, collapse = ", "))
}

seasonal_climate <- unique(seasonal_data[required_columns])
seasonal_climate$year <- as.integer(as.character(seasonal_climate$year))

# Calculate departures from the full-series climate norm and standardized
# anomalies for one site-season-variable series.
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

# Run the detector independently for every site and seasonal climate variable,
# sorting by the data's year field before computing each full-series norm.
t0 <- 2
anomaly_results <- list()
result_index <- 1L

for (site_name in unique(seasonal_climate$site)) {
  site_data <- seasonal_climate[seasonal_climate$site == site_name, ]
  site_data <- site_data[order(site_data$year), ]

  for (variable in climate_variables) {
    detection <- find_climate_anomalies(site_data[[variable]], t0 = t0)

    anomaly_results[[result_index]] <- data.frame(
      site = site_name,
      year = site_data$year,
      variable = variable,
      value = site_data[[variable]],
      baseline = detection$baseline,
      anomaly_value = detection$anomaly_value,
      zscore = detection$zscore,
      anomaly = detection$anomaly,
      adjusted_value = detection$adjusted_value
    )
    result_index <- result_index + 1L
  }
}

# Save one row per site, year, and seasonal climate variable, including the
# observed value, full-series mean norm, anomaly departure, standardized score,
# anomaly flag, and mean-replacement value for flagged observations. This does
# not modify the source seasonal data.
seasonal_climate_anoms <- do.call(rbind, anomaly_results)
rownames(seasonal_climate_anoms) <- NULL
output_path <- "climate/clim_seasons_taxon_anoms.RDS"
saveRDS(seasonal_climate_anoms, output_path)

# Summarize the number of flagged years for each site-variable combination.
print(xtabs(as.integer(anomaly) ~ site + variable, data = seasonal_climate_anoms))

# Plot each seasonal climate variable in its own figure, with a separate panel
# for each site. Red vertical lines indicate negative anomalies and blue lines
# indicate positive anomalies. Figures are displayed but are not saved.
library(ggplot2)

seasonal_anomaly_plots <- setNames(
  vector("list", length(climate_variables)),
  climate_variables
)

for (variable in climate_variables) {
  # Limit the displayed time series to 1950 onward; anomaly detection above
  # still uses all available years to compute the full-series climate norm.
  plot_data <- seasonal_climate_anoms[
    seasonal_climate_anoms$variable == variable & seasonal_climate_anoms$year >= 1950,
  ]
  anomaly_data <- plot_data[plot_data$anomaly, ]
  norm_data <- unique(plot_data[c("site", "baseline")])
  anomaly_data$direction <- ifelse(
    anomaly_data$zscore < 0,
    "Negative anomaly",
    "Positive anomaly"
  )

  seasonal_anomaly_plots[[variable]] <- ggplot(
    plot_data,
    aes(x = year, y = value, group = site)
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
    facet_wrap(vars(site), scales = "free_y", ncol = 2) +
    scale_color_manual(
      name = "Anomaly direction",
      values = c("Negative anomaly" = "red", "Positive anomaly" = "blue")
    ) +
    labs(
      title = paste(variable, "seasonal climate values by site"),
      x = "Year",
      y = variable
    ) +
    theme_minimal()

  print(seasonal_anomaly_plots[[variable]])
}
