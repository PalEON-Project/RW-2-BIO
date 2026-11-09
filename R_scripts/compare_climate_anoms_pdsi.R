################################################################################
# Compare site-level annual/seasonal climate anomalies with regional NOAA PDSI.
#
# Each site's PDSI series is taken from its assigned NOAA region. Therefore,
# sites in the same region share exactly the same PDSI values/categories, but
# the climate anomaly flags and comparison plots are computed separately by
# site. Seasonal PDSI uses the months and Sep-Aug tree-year assignment from
# agbi_clim_model.R.
################################################################################

rm(list = ls())
library(ggplot2)

# Read the climate anomaly tables and NOAA PDSI files.
annual_anomaly_path <- "climate/prism_clim_anoms.RDS"
seasonal_anomaly_path <- "climate/clim_seasons_taxon_anoms.RDS"
pdsi_files <- c(NE = "climate/pdsi-noaa-ne.csv", MW = "climate/pdsi-noaa-mw.csv")
input_paths <- c(annual_anomaly_path, seasonal_anomaly_path, pdsi_files)
missing_paths <- input_paths[!file.exists(input_paths)]
if (length(missing_paths) > 0) {
  stop("Required input file(s) not found: ", paste(missing_paths, collapse = ", "))
}

annual_climate_anoms <- as.data.frame(readRDS(annual_anomaly_path))
seasonal_climate_anoms <- as.data.frame(readRDS(seasonal_anomaly_path))
required_anomaly_columns <- c("year", "variable", "anomaly", "zscore")
if (!all(c(required_anomaly_columns, "loc") %in% names(annual_climate_anoms))) {
  stop("Annual anomaly input must contain loc, year, variable, anomaly, and zscore.")
}
if (!all(c(required_anomaly_columns, "site") %in% names(seasonal_climate_anoms))) {
  stop("Seasonal anomaly input must contain site, year, variable, anomaly, and zscore.")
}

# Each site maps to one NOAA region. PDSI classes below are fixed thresholds,
# so the values and categories are identical for sites sharing that region.
site_region <- data.frame(
  site = c("GOOSE", "HARVARD", "NRP", "ROOSTER", "SYLVANIA", "HMC"),
  region = c("NE", "NE", "NE", "NE", "MW", "MW"),
  stringsAsFactors = FALSE
)

classify_pdsi <- function(x) {
  category <- ifelse(x >= 4, "Extremely Wet",
    ifelse(x >= 3, "Very Wet",
      ifelse(x >= 2, "Moderately Wet",
        ifelse(x >= -1.99, "Near Normal",
          ifelse(x >= -2.99, "Moderate Drought",
            ifelse(x >= -3.99, "Severe Drought", "Extreme Drought")
          )
        )
      )
    )
  )
  factor(category, levels = c(
    "Extreme Drought", "Severe Drought", "Moderate Drought", "Near Normal",
    "Moderately Wet", "Very Wet", "Extremely Wet"
  ), ordered = TRUE)
}

# Read monthly PDSI (skip NOAA's title row) and decode the YYYYMM dates.
read_pdsi_monthly <- function(path, region_name) {
  monthly <- read.csv(path, skip = 1, stringsAsFactors = FALSE)
  if (!all(c("Date", "Value") %in% names(monthly))) {
    stop("Expected Date and Value columns in ", path)
  }
  monthly$Date <- as.integer(monthly$Date)
  monthly$year <- monthly$Date %/% 100L
  monthly$month <- monthly$Date %% 100L
  if (anyNA(monthly$year) || anyNA(monthly$Value) ||
      any(monthly$month < 1L | monthly$month > 12L)) {
    stop("Invalid or missing monthly PDSI records in ", path)
  }
  monthly$region <- region_name
  monthly$pdsi_date <- as.Date(sprintf("%04d-%02d-01", monthly$year, monthly$month))
  monthly$pdsi <- monthly$Value
  monthly$pdsi_category <- classify_pdsi(monthly$pdsi)
  monthly[c("region", "year", "month", "pdsi_date", "pdsi", "pdsi_category")]
}

pdsi_monthly <- do.call(rbind, lapply(names(pdsi_files), function(region_name) {
  read_pdsi_monthly(pdsi_files[[region_name]], region_name)
}))
rownames(pdsi_monthly) <- NULL
pdsi_monthly <- pdsi_monthly[order(pdsi_monthly$region, pdsi_monthly$pdsi_date), ]

# Seasonal PDSI months match agbi_clim_model.R exactly. For tree year Y:
# winter=Dec(Y-1)+Jan-Feb(Y), spring=Mar-May(Y), summer=Jun-Aug(Y),
# and fall=Sep-Nov(Y-1). Incomplete boundary seasons are skipped.
season_months <- data.frame(
  season = c("winter", "winter", "winter", "spring", "spring", "spring",
             "summer", "summer", "summer", "fall", "fall", "fall"),
  month = c(12L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L),
  year_offset = c(-1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, -1L, -1L, -1L),
  stringsAsFactors = FALSE
)
seasonal_records <- list()
seasonal_index <- 1L
for (region_name in unique(pdsi_monthly$region)) {
  region_monthly <- pdsi_monthly[pdsi_monthly$region == region_name, ]
  for (tree_year in sort(unique(region_monthly$year))) {
    for (season_name in unique(season_months$season)) {
      months_for_season <- season_months[season_months$season == season_name, ]
      lookup <- merge(
        data.frame(
          year = tree_year + months_for_season$year_offset,
          month = months_for_season$month
        ),
        region_monthly,
        by = c("year", "month"),
        all.x = TRUE
      )
      if (nrow(lookup) != 3L || anyNA(lookup$pdsi)) next
      seasonal_records[[seasonal_index]] <- data.frame(
        region = region_name,
        tree_year = tree_year,
        season = season_name,
        pdsi_season_mean = mean(lookup$pdsi),
        pdsi_category = as.character(classify_pdsi(mean(lookup$pdsi))),
        months = paste(sprintf("%04d-%02d", lookup$year, lookup$month), collapse = "; "),
        stringsAsFactors = FALSE
      )
      seasonal_index <- seasonal_index + 1L
    }
  }
}
pdsi_seasonal <- do.call(rbind, seasonal_records)
rownames(pdsi_seasonal) <- NULL
pdsi_seasonal$pdsi_category <- factor(
  pdsi_seasonal$pdsi_category,
  levels = levels(pdsi_monthly$pdsi_category),
  ordered = TRUE
)

# Keep flagged anomaly records by their actual site. Do not combine other sites
# in the same NOAA region: their climate anomalies are independent of this site's.
annual_flags <- data.frame(
  site = as.character(annual_climate_anoms$loc),
  year = as.integer(as.character(annual_climate_anoms$year)),
  variable = as.character(annual_climate_anoms$variable),
  direction = ifelse(annual_climate_anoms$zscore >= 0, "HIGH", "LOW"),
  anomaly = annual_climate_anoms$anomaly %in% TRUE,
  stringsAsFactors = FALSE
)
annual_flags <- annual_flags[annual_flags$anomaly, c("site", "year", "variable", "direction")]
seasonal_flags <- data.frame(
  site = as.character(seasonal_climate_anoms$site),
  tree_year = as.integer(as.character(seasonal_climate_anoms$year)),
  variable = as.character(seasonal_climate_anoms$variable),
  direction = ifelse(seasonal_climate_anoms$zscore >= 0, "HIGH", "LOW"),
  anomaly = seasonal_climate_anoms$anomaly %in% TRUE,
  stringsAsFactors = FALSE
)
seasonal_flags$season <- sub("^.*_", "", seasonal_flags$variable)
seasonal_flags <- seasonal_flags[seasonal_flags$anomaly,
                                 c("site", "tree_year", "season", "variable", "direction")]

# Output files are grouped by site. Each site's comparison uses the common PDSI
# series for its NOAA region but only that site's annual and seasonal climate flags.
output_dir <- "climate/pdsi_site_comparisons"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
site_comparisons <- setNames(vector("list", nrow(site_region)), site_region$site)
site_plots <- setNames(vector("list", nrow(site_region)), site_region$site)

pdsi_category_colors <- c(
  "Extreme Drought" = "#7f0000",
  "Severe Drought" = "#d73027",
  "Moderate Drought" = "#fc8d59",
  "Near Normal" = "#ffffbf",
  "Moderately Wet" = "#91bfdb",
  "Very Wet" = "#4575b4",
  "Extremely Wet" = "#313695"
)
climate_flag_colors <- c(
  "No anomaly" = "grey94",
  "HIGH anomaly" = "darkolivegreen3",
  "LOW anomaly" = "goldenrod2",
  pdsi_category_colors
)
plot_start_year <- 1950L

for (site_index in seq_len(nrow(site_region))) {
  site_name <- site_region$site[[site_index]]
  region_name <- site_region$region[[site_index]]

  # Create monthly PDSI rows for this site's mapped region, then attach only
  # that site's annual anomaly flags for the corresponding calendar year.
  site_monthly_pdsi <- pdsi_monthly[pdsi_monthly$region == region_name, ]
  site_annual_flags <- annual_flags[annual_flags$site == site_name, ]
  annual_signature <- if (nrow(site_annual_flags) > 0) {
    groups <- split(site_annual_flags, site_annual_flags$year)
    do.call(rbind, lapply(groups, function(x) data.frame(
      year = x$year[[1]],
      annual_anomaly_count = nrow(unique(x[c("variable", "direction")])),
      annual_anomaly_variables = paste(
        sort(unique(paste0(x$direction, ":", x$variable))), collapse = "; "
      ),
      stringsAsFactors = FALSE
    )))
  } else {
    data.frame(
      year = integer(), annual_anomaly_count = integer(),
      annual_anomaly_variables = character()
    )
  }
  site_monthly_comparison <- merge(
    site_monthly_pdsi,
    annual_signature,
    by = "year",
    all.x = TRUE,
    sort = FALSE
  )
  site_monthly_comparison$annual_anomaly_count[
    is.na(site_monthly_comparison$annual_anomaly_count)
  ] <- 0L
  site_monthly_comparison$annual_anomaly_variables[
    is.na(site_monthly_comparison$annual_anomaly_variables)
  ] <- ""
  site_monthly_comparison$has_annual_climate_anomaly <-
    site_monthly_comparison$annual_anomaly_count > 0L
  site_monthly_comparison$site <- site_name
  site_monthly_comparison <- site_monthly_comparison[
    order(site_monthly_comparison$pdsi_date),
  ]

  # Match seasonal anomaly flags by this site, tree-year, and season, alongside
  # the PDSI category computed from the exact same three-month season.
  site_seasonal_pdsi <- pdsi_seasonal[pdsi_seasonal$region == region_name, ]
  site_seasonal_flags <- seasonal_flags[seasonal_flags$site == site_name, ]
  seasonal_signature <- if (nrow(site_seasonal_flags) > 0) {
    groups <- split(
      site_seasonal_flags,
      interaction(site_seasonal_flags$tree_year, site_seasonal_flags$season, drop = TRUE)
    )
    do.call(rbind, lapply(groups, function(x) data.frame(
      tree_year = x$tree_year[[1]],
      season = x$season[[1]],
      seasonal_anomaly_count = nrow(unique(x[c("variable", "direction")])),
      seasonal_anomaly_variables = paste(
        sort(unique(paste0(x$direction, ":", x$variable))), collapse = "; "
      ),
      stringsAsFactors = FALSE
    )))
  } else {
    data.frame(
      tree_year = integer(), season = character(),
      seasonal_anomaly_count = integer(), seasonal_anomaly_variables = character()
    )
  }
  site_seasonal_comparison <- merge(
    site_seasonal_pdsi,
    seasonal_signature,
    by = c("tree_year", "season"),
    all.x = TRUE,
    sort = FALSE
  )
  site_seasonal_comparison$seasonal_anomaly_count[
    is.na(site_seasonal_comparison$seasonal_anomaly_count)
  ] <- 0L
  site_seasonal_comparison$seasonal_anomaly_variables[
    is.na(site_seasonal_comparison$seasonal_anomaly_variables)
  ] <- ""
  site_seasonal_comparison$has_seasonal_climate_anomaly <-
    site_seasonal_comparison$seasonal_anomaly_count > 0L
  site_seasonal_comparison$site <- site_name
  site_seasonal_comparison <- site_seasonal_comparison[
    order(site_seasonal_comparison$tree_year, site_seasonal_comparison$season),
  ]

  site_comparisons[[site_name]] <- list(
    site = site_name,
    region = region_name,
    monthly = site_monthly_comparison,
    seasonal = site_seasonal_comparison,
    annual_climate_flags = site_annual_flags,
    seasonal_climate_flags = site_seasonal_flags
  )
  saveRDS(
    site_comparisons[[site_name]],
    file.path(output_dir, paste0(site_name, "_pdsi_climate_comparison.RDS"))
  )

  # Annual plot: tile rows are this site's annual climate anomaly variables;
  # monthly PDSI category tiles share the same timeline on a dedicated row.
  plot_years <- sort(unique(site_monthly_comparison$year[
    site_monthly_comparison$year >= plot_start_year
  ]))
  site_variables <- sort(unique(site_annual_flags$variable))
  if (length(site_variables) == 0L) site_variables <- "No annual climate flags"
  annual_rows <- c("Monthly PDSI category", site_variables)
  annual_row_map <- data.frame(
    variable = annual_rows,
    y = rev(seq_along(annual_rows)),
    stringsAsFactors = FALSE
  )
  annual_grid <- expand.grid(
    year = plot_years,
    variable = site_variables,
    stringsAsFactors = FALSE
  )
  annual_grid <- merge(annual_grid, site_annual_flags, by = c("year", "variable"),
                       all.x = TRUE, sort = FALSE)
  annual_grid$flag_status <- ifelse(
    is.na(annual_grid$direction), "No anomaly", paste(annual_grid$direction, "anomaly")
  )
  annual_grid$xmin <- annual_grid$year
  annual_grid$xmax <- annual_grid$year + 1
  annual_category_tiles <- site_monthly_comparison[
    site_monthly_comparison$year >= plot_start_year,
    c("year", "month", "pdsi_category")
  ]
  annual_category_tiles$xmin <- annual_category_tiles$year +
    (annual_category_tiles$month - 1) / 12
  annual_category_tiles$xmax <- annual_category_tiles$year +
    annual_category_tiles$month / 12
  annual_category_tiles$variable <- "Monthly PDSI category"
  annual_category_tiles$flag_status <- as.character(annual_category_tiles$pdsi_category)
  annual_plot_tiles <- rbind(
    annual_grid[c("variable", "xmin", "xmax", "flag_status")],
    annual_category_tiles[c("variable", "xmin", "xmax", "flag_status")]
  )
  annual_plot_tiles <- merge(annual_plot_tiles, annual_row_map, by = "variable")
  annual_plot <- ggplot(annual_plot_tiles) +
    geom_rect(
      aes(xmin = xmin, xmax = xmax, ymin = y - 0.45, ymax = y + 0.45,
          fill = flag_status),
      color = "white", linewidth = 0.15
    ) +
    scale_y_continuous(breaks = annual_row_map$y, labels = annual_row_map$variable) +
    scale_fill_manual(values = climate_flag_colors, drop = FALSE) +
    scale_x_continuous(breaks = pretty(plot_years), expand = c(0, 0)) +
    labs(
      title = paste(site_name, "annual climate anomalies and monthly PDSI"),
      subtitle = paste("NOAA region:", region_name,
                       "| climate flags are specific to", site_name),
      x = "Year", y = NULL, fill = "Climate anomaly / PDSI category"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  # Seasonal plot: within each season panel, show this site's seasonal climate
  # anomaly variables and the PDSI category for the same Sep-Aug tree year.
  plot_tree_years <- sort(unique(site_seasonal_comparison$tree_year[
    site_seasonal_comparison$tree_year >= plot_start_year
  ]))
  season_order <- c("winter", "spring", "summer", "fall")
  site_season_variables <- sort(unique(site_seasonal_flags$variable))
  if (length(site_season_variables) == 0L) site_season_variables <- "No seasonal climate flags"
  seasonal_plot_rows <- do.call(rbind, lapply(season_order, function(season_name) {
    variables <- site_season_variables[grepl(paste0("_", season_name, "$"), site_season_variables)]
    if (length(variables) == 0L) variables <- "No seasonal climate flags"
    data.frame(season = season_name, variable = c("PDSI category", variables))
  }))
  seasonal_plot_rows$variable <- factor(
    seasonal_plot_rows$variable,
    levels = unique(seasonal_plot_rows$variable)
  )
  seasonal_plot_tiles <- list()
  tile_index <- 1L
  for (season_name in season_order) {
    season_variables <- seasonal_plot_rows$variable[
      seasonal_plot_rows$season == season_name &
        seasonal_plot_rows$variable != "PDSI category"
    ]
    if (identical(season_variables, "No seasonal climate flags")) {
      season_variables <- character()
    }
    if (length(season_variables) > 0L) {
      season_grid <- expand.grid(
        tree_year = plot_tree_years,
        variable = season_variables,
        stringsAsFactors = FALSE
      )
      flags_this_season <- site_seasonal_flags[
        site_seasonal_flags$season == season_name,
        c("tree_year", "variable", "direction")
      ]
      season_grid <- merge(season_grid, flags_this_season,
                           by = c("tree_year", "variable"), all.x = TRUE)
      season_grid$flag_status <- ifelse(
        is.na(season_grid$direction), "No anomaly", paste(season_grid$direction, "anomaly")
      )
      season_grid$season <- season_name
      seasonal_plot_tiles[[tile_index]] <- season_grid[c(
        "season", "tree_year", "variable", "flag_status"
      )]
      tile_index <- tile_index + 1L
    }

    pdsi_this_season <- site_seasonal_comparison[
      site_seasonal_comparison$season == season_name &
        site_seasonal_comparison$tree_year >= plot_start_year,
      c("tree_year", "pdsi_category")
    ]
    pdsi_this_season$season <- season_name
    pdsi_this_season$variable <- "PDSI category"
    pdsi_this_season$flag_status <- as.character(pdsi_this_season$pdsi_category)
    seasonal_plot_tiles[[tile_index]] <- pdsi_this_season[c(
      "season", "tree_year", "variable", "flag_status"
    )]
    tile_index <- tile_index + 1L
  }
  seasonal_plot_tiles <- do.call(rbind, seasonal_plot_tiles)
  seasonal_plot_tiles <- merge(
    seasonal_plot_tiles,
    seasonal_plot_rows,
    by = c("season", "variable"),
    sort = FALSE
  )
  seasonal_plot <- ggplot(seasonal_plot_tiles) +
    geom_tile(aes(x = tree_year, y = variable, fill = flag_status),
              color = "white", linewidth = 0.15) +
    facet_wrap(vars(season), ncol = 2, scales = "free_y") +
    scale_fill_manual(values = climate_flag_colors, drop = FALSE) +
    scale_x_continuous(breaks = pretty(plot_tree_years), expand = c(0.02, 0)) +
    labs(
      title = paste(site_name, "seasonal climate anomalies and PDSI"),
      subtitle = paste("Same season and Sep-Aug tree year | NOAA region:", region_name),
      x = "Tree year", y = NULL, fill = "Climate anomaly / PDSI category"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  # Save a separate two-page PDF per site: page 1 shows annual anomalies with
  # monthly PDSI categories; page 2 shows season-matched anomalies and PDSI.
  pdf(file.path(output_dir, paste0(site_name, "_pdsi_climate_comparison.pdf")),
      width = 12, height = 8)
  print(annual_plot)
  print(seasonal_plot)
  dev.off()
  site_plots[[site_name]] <- list(annual = annual_plot, seasonal = seasonal_plot)
}

# Also retain consolidated in-memory comparison tables for convenient analysis.
monthly_pdsi_comparison <- do.call(rbind, lapply(site_comparisons, `[[`, "monthly"))
seasonal_pdsi_comparison <- do.call(rbind, lapply(site_comparisons, `[[`, "seasonal"))
