################################################################################
# Compare taxon-level AGBI disturbance years with annual and seasonal climate
# anomaly years. Climate and AGBI events are considered concurrent when they
# occur at the same site and calendar year.
#
# A same-year match is a possible correspondence, not evidence that climate
# caused the AGBI anomaly. AGBI anomalies without a detected same-year climate
# anomaly are explicitly identified; lagged effects and unmeasured drivers are
# not assessed by this comparison.
################################################################################

rm(list = ls())
library(ggplot2)

# Read AGBI data used by stat_model_anoms.R, plus the anomaly outputs from both
# climate workflows. All paths are relative to the project root.
agbi_path <- "reboot/AGBI_taxon_data.RDS"
annual_climate_path <- "climate/prism_clim_anoms.RDS"
seasonal_climate_path <- "climate/clim_seasons_taxon_anoms.RDS"
input_paths <- c(agbi_path, annual_climate_path, seasonal_climate_path)
missing_paths <- input_paths[!file.exists(input_paths)]
if (length(missing_paths) > 0) {
  stop(
    "Required input file(s) not found: ", paste(missing_paths, collapse = ", "),
    ". Run stat_model_anoms.R and both climate anomaly scripts first."
  )
}

AGBI_taxon <- as.data.frame(readRDS(agbi_path))
annual_climate_anoms <- as.data.frame(readRDS(annual_climate_path))
seasonal_climate_anoms <- as.data.frame(readRDS(seasonal_climate_path))

# Check required columns before calculating or joining any anomaly records.
agbi_required <- c("site", "taxon", "year", "AGBI.mid")
climate_required <- c("year", "variable", "anomaly")
missing_agbi_columns <- setdiff(agbi_required, names(AGBI_taxon))
if (length(missing_agbi_columns) > 0) {
  stop("AGBI input is missing: ", paste(missing_agbi_columns, collapse = ", "))
}
missing_annual_columns <- setdiff(c(climate_required, "loc"), names(annual_climate_anoms))
missing_seasonal_columns <- setdiff(c(climate_required, "site"), names(seasonal_climate_anoms))
if (length(missing_annual_columns) > 0 || length(missing_seasonal_columns) > 0) {
  stop("Climate anomaly inputs do not have the expected schema.")
}

# Reproduce the taxon-level `disturb` rule in stat_model_anoms.R: compare each
# AGBI.mid value against the median of itself and the preceding k observations,
# and flag unusually low values using t0 scaled MADs. This matches the script's
# one-sided disturbance flags (k = 7, t0 = 2), not its separate Hampel flags.
k <- 7L
t0 <- 2
AGBI_taxon <- AGBI_taxon[!is.na(AGBI_taxon$AGBI.mid), ]
AGBI_taxon$site <- as.character(AGBI_taxon$site)
AGBI_taxon$taxon <- as.character(AGBI_taxon$taxon)
AGBI_taxon$year <- as.integer(as.character(AGBI_taxon$year))
if (anyDuplicated(AGBI_taxon[c("site", "taxon", "year")])) {
  stop("AGBI input has duplicate site-taxon-year records; resolve duplicates before comparison.")
}

# Keep the time series in year order and calculate the same rolling robust
# score and low-side disturbance flag separately for each site-taxon group.
AGBI_taxon <- AGBI_taxon[order(AGBI_taxon$site, AGBI_taxon$taxon, AGBI_taxon$year), ]
AGBI_taxon$agbi_zscore <- NA_real_
AGBI_taxon$agbi_disturb <- FALSE
AGBI_groups <- split(
  seq_len(nrow(AGBI_taxon)),
  interaction(AGBI_taxon$site, AGBI_taxon$taxon, drop = TRUE)
)

for (group_indices in AGBI_groups) {
  values <- AGBI_taxon$AGBI.mid[group_indices]
  if (length(values) <= k) {
    next
  }

  for (i in seq.int(k + 1L, length(values))) {
    window <- values[(i - k):i]
    center <- median(window)
    scale <- 1.4826 * median(abs(window - center))

    # The original detector's z-score is undefined when the rolling MAD is
    # zero. Leave these observations unflagged rather than divide by zero.
    if (!is.finite(scale) || scale == 0) {
      next
    }

    score <- (values[[i]] - center) / scale
    AGBI_taxon$agbi_zscore[group_indices[[i]]] <- score
    if ((values[[i]] - center) < (-t0 * scale)) {
      AGBI_taxon$agbi_disturb[group_indices[[i]]] <- TRUE
    }
  }
}

# Standardize the two climate outputs to common site-year-variable-source
# fields, retaining only climate records already flagged as anomalous.
annual_flags <- data.frame(
  site = as.character(annual_climate_anoms$loc),
  year = as.integer(as.character(annual_climate_anoms$year)),
  variable = as.character(annual_climate_anoms$variable),
  source = "annual",
  anomaly = annual_climate_anoms$anomaly %in% TRUE,
  stringsAsFactors = FALSE
)
seasonal_flags <- data.frame(
  site = as.character(seasonal_climate_anoms$site),
  year = as.integer(as.character(seasonal_climate_anoms$year)),
  variable = as.character(seasonal_climate_anoms$variable),
  source = "seasonal",
  anomaly = seasonal_climate_anoms$anomaly %in% TRUE,
  stringsAsFactors = FALSE
)
climate_flags <- rbind(annual_flags, seasonal_flags)
climate_flags <- climate_flags[climate_flags$anomaly, ]

# Collapse all flagged climate variables by site-year. The variable lists show
# which annual and/or seasonal detections coincide with each AGBI disturbance.
climate_groups <- split(
  climate_flags,
  interaction(climate_flags$site, climate_flags$year, drop = TRUE)
)
climate_by_year <- do.call(rbind, lapply(climate_groups, function(group_data) {
  annual_variables <- sort(unique(group_data$variable[group_data$source == "annual"]))
  seasonal_variables <- sort(unique(group_data$variable[group_data$source == "seasonal"]))

  data.frame(
    site = group_data$site[[1]],
    year = group_data$year[[1]],
    annual_anomaly_count = length(annual_variables),
    annual_anomaly_variables = paste(annual_variables, collapse = "; "),
    seasonal_anomaly_count = length(seasonal_variables),
    seasonal_anomaly_variables = paste(seasonal_variables, collapse = "; "),
    stringsAsFactors = FALSE
  )
}))
if (is.null(climate_by_year)) {
  climate_by_year <- data.frame(
    site = character(), year = integer(),
    annual_anomaly_count = integer(), annual_anomaly_variables = character(),
    seasonal_anomaly_count = integer(), seasonal_anomaly_variables = character()
  )
}

# Classify each taxon AGBI disturbance according to whether at least one
# annual or seasonal climate anomaly was detected for that same site-year.
agbi_events <- AGBI_taxon[AGBI_taxon$agbi_disturb, c(
  "site", "taxon", "year", "AGBI.mid", "agbi_zscore"
)]
agbi_climate_correspondence <- merge(
  agbi_events,
  climate_by_year,
  by = c("site", "year"),
  all.x = TRUE,
  sort = TRUE
)
count_columns <- c("annual_anomaly_count", "seasonal_anomaly_count")
text_columns <- c("annual_anomaly_variables", "seasonal_anomaly_variables")
for (column in count_columns) {
  agbi_climate_correspondence[[column]][is.na(agbi_climate_correspondence[[column]])] <- 0L
}
for (column in text_columns) {
  agbi_climate_correspondence[[column]][is.na(agbi_climate_correspondence[[column]])] <- ""
}
agbi_climate_correspondence$total_climate_anomaly_count <-
  agbi_climate_correspondence$annual_anomaly_count +
  agbi_climate_correspondence$seasonal_anomaly_count
agbi_climate_correspondence$climate_correspondence <-
  agbi_climate_correspondence$total_climate_anomaly_count > 0L
agbi_climate_correspondence$event_status <- ifelse(
  agbi_climate_correspondence$climate_correspondence,
  "AGBI + climate anomaly same year",
  "AGBI anomaly; no climate anomaly detected"
)

# Explicitly retain the AGBI events with no same-year climate anomaly. These
# are unmatched by the climate variables and methods in this comparison; this
# does not rule out lagged climate effects or other explanations.
unmatched_agbi_anomalies <- agbi_climate_correspondence[
  !agbi_climate_correspondence$climate_correspondence,
]

# Summarize event counts by site-year for the second visualization.
if (nrow(agbi_climate_correspondence) > 0) {
  site_year_summary <- aggregate(
    cbind(matched_events = as.integer(agbi_climate_correspondence$climate_correspondence),
          unmatched_events = as.integer(!agbi_climate_correspondence$climate_correspondence)) ~
      site + year,
    data = agbi_climate_correspondence,
    FUN = sum
  )
} else {
  site_year_summary <- data.frame(
    site = character(), year = integer(),
    matched_events = integer(), unmatched_events = integer()
  )
}

# Save the full event-level correspondence table and the unmatched subset for
# review. These tables include all years; figure display starts at 1950.
dir.create("climate", showWarnings = FALSE)
saveRDS(
  agbi_climate_correspondence,
  "climate/agbi_climate_anomaly_correspondence.RDS"
)
write.csv(
  agbi_climate_correspondence,
  "climate/agbi_climate_anomaly_correspondence.csv",
  row.names = FALSE
)
saveRDS(
  unmatched_agbi_anomalies,
  "climate/agbi_anomalies_without_same_year_climate_flags.RDS"
)

# Report the number of AGBI disturbance events with and without a same-year
# annual or seasonal climate flag. Print a sample of unmatched events; the full
# list is saved above.
cat("Taxon AGBI disturbance events:", nrow(agbi_climate_correspondence), "\n")
cat("With same-year climate anomaly:", sum(agbi_climate_correspondence$climate_correspondence), "\n")
cat("Without same-year climate anomaly:", nrow(unmatched_agbi_anomalies), "\n")
if (nrow(unmatched_agbi_anomalies) > 0) {
  print(utils::head(unmatched_agbi_anomalies, 30))
}

# Figure 1: every taxon-level AGBI disturbance year, colored by whether any
# annual or seasonal climate anomaly coincides at that site's same year.
# Restrict display to 1950 onward while keeping all years in saved tables.
plot_start_year <- 1950
agbi_event_plot_data <- agbi_climate_correspondence[
  agbi_climate_correspondence$year >= plot_start_year,
]
agbi_event_plot <- ggplot(
  agbi_event_plot_data,
  aes(x = year, y = taxon, color = event_status)
) +
  geom_point(size = 2, alpha = 0.85) +
  facet_wrap(vars(site), scales = "free_y", ncol = 2) +
  scale_color_manual(
    name = "Same-year climate flags",
    values = c(
      "AGBI + climate anomaly same year" = "steelblue",
      "AGBI anomaly; no climate anomaly detected" = "firebrick"
    )
  ) +
  labs(
    title = "Taxon AGBI disturbance years and concurrent climate anomalies",
    subtitle = "Blue: one or more annual/seasonal climate flags; red: none detected",
    x = "Year",
    y = "Taxon"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(agbi_event_plot)

# Figure 2: annual counts of taxon AGBI disturbances, separated into events
# with and without any same-year climate anomaly, by site.
site_year_plot_data <- site_year_summary[
  site_year_summary$year >= plot_start_year,
]
site_year_plot_data <- rbind(
  data.frame(
    site = site_year_plot_data$site,
    year = site_year_plot_data$year,
    correspondence = "Climate anomaly also detected",
    events = site_year_plot_data$matched_events
  ),
  data.frame(
    site = site_year_plot_data$site,
    year = site_year_plot_data$year,
    correspondence = "No climate anomaly detected",
    events = site_year_plot_data$unmatched_events
  )
)
site_year_plot_data <- site_year_plot_data[site_year_plot_data$events > 0, ]

agbi_climate_count_plot <- ggplot(
  site_year_plot_data,
  aes(x = year, y = events, fill = correspondence)
) +
  geom_col() +
  facet_wrap(vars(site), scales = "free_y", ncol = 2) +
  scale_fill_manual(
    name = "AGBI disturbance events",
    values = c(
      "Climate anomaly also detected" = "steelblue",
      "No climate anomaly detected" = "firebrick"
    )
  ) +
  labs(
    title = "Counts of taxon AGBI disturbances by correspondence status",
    subtitle = "Correspondence means same site and year; it is not a causal test",
    x = "Year",
    y = "Number of taxon disturbances"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(agbi_climate_count_plot)
