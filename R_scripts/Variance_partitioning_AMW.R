library(dplyr)
library(tidyr)
library(purrr)
library(forecast)

#### 1. Variance partitioning ####

# Different model components we are testing the effect of
components <- c("clim", "ar", "diff")

# Function for computing R2 on one-step-ahead fitted predictions
level_r2 <- function(model, y) {
  fit <- as.numeric(fitted(model))
  idx <- 2:length(y) # has to be 2:N because we don't get one-step-ahead prediction of the first time step
  1 - sum((y[idx] - fit[idx])^2) / sum((y[idx] - mean(y[idx]))^2)
}

# subset of components -> model key and ARIMA spec
subset_key <- function(S) if (length(S) == 0) "null" else paste(components[components %in% S], collapse = "+")

# Setting up how subsets of models are defined
subset_spec <- function(S) {
  list(
    order = c(as.integer("ar" %in% S), as.integer("diff" %in% S), 0L),
    use_x = "clim" %in% S
  )
}

# all 8 subsets of climate, AR(1) and differencing models
all_subsets <- unlist(lapply(0:3, function(k) combn(components, k, simplify = FALSE)),
                      recursive = FALSE)

# Function for actually computing the fitted R2 of each model
# using the level_r2 function from above
fit_r2 <- function(y, xreg, S) {
  if (length(S) == 0) return(0) # null model: R2 = 0 by definition
  s <- subset_spec(S)
  m <- tryCatch(
    forecast::Arima(y, order = s$order,
                    xreg = if (s$use_x) xreg else NULL,
                    method = "ML"),
    error = function(e) NULL
  )
  if (is.null(m)) NA_real_ else level_r2(m, as.numeric(y))
}

# Shapley attribution of the full-model R2 across the 3 components
shapley <- function(v) {
  perms <- list(c(1,2,3), c(1,3,2), c(2,1,3), c(2,3,1), c(3,1,2), c(3,2,1))
  contrib <- setNames(numeric(3), components)
  for (p in perms) {
    S <- character(0)
    for (i in p) {
      comp  <- components[i]
      before <- v[[subset_key(S)]]
      S      <- c(S, comp)
      after  <- v[[subset_key(S)]]
      contrib[comp] <- contrib[comp] + (after - before)
    }
  }
  contrib / length(perms)
}

# Fit the models and determine Shapley contributions
variance_partition <- clim_agbi_in_taxon %>%
  group_by(site, taxon) %>%
  group_modify(~ {
    df <- arrange(.x, year)
    
    if (anyNA(df$AGBI.mean) || anyNA(df[predictor_names])) {
      return(tibble(note = "Missing data"))
    }
    
    y    <- ts(df$AGBI.mean, start = min(df$year), frequency = 1)
    xreg <- as.matrix(df[predictor_names])
    
    v <- map_dbl(all_subsets, ~ fit_r2(y, xreg, .x))
    names(v) <- map_chr(all_subsets, subset_key)
    
    if (anyNA(v)) return(tibble(note = "Model failed"))
    
    sh <- shapley(v)
    tibble(
      shap_clim   = sh[["clim"]],
      shap_ar     = sh[["ar"]],
      shap_diff   = sh[["diff"]],
      r2_full     = v[["clim+ar+diff"]],
      unexplained = 1 - v[["clim+ar+diff"]],
      note        = NA_character_
    ) %>% bind_cols(as_tibble(as.list(v)) %>% rename_with(~ paste0("r2_", .x)))
  }) %>%
  ungroup()

# Look at variance partitioning results
View(variance_partition)

## Notes on interpretation:
## 1. shap_* columns: On average (averaging over the models including 
##    that contribution out of 8 total), how much of the model fit can be
##    attributed to that component (climate, AR(1) or differencing)?
## 2. r2_full: R2 of full model with climate predictors and order (1, 1, 0)
## 3. r2_null: 0, a null baseline for comparison
## 4. r2_* columns: one-step-ahead R2 for each model (one of the 8 models)


## The issue with this is that "climate" means something different
## when including differencing (d = 1) than when differencing is not
## included in the model (d = 0). When d = 1, it is the year-to-year
## variability in climate. When d = 0, it includes both long-term
## trends and year-to-year variability.

## We can look at the contribution of climate for different combinations
## of the underlying time series dynamics by looking at the r2 of full models
## minus the r2 of the component time series parts (p = 1, d = 1, or p,d = 1)
climate_contrib <- variance_partition %>%
  mutate(
    # climate effect on levels (d = 0 contexts)
    clim_level_alone   = `r2_clim`,
    clim_level_given_ar = `r2_clim+ar` - `r2_ar`,
    
    # climate effect on year-to-year changes (d = 1 contexts)
    clim_change_given_diff    = `r2_clim+diff` - `r2_diff`,
    clim_change_given_ar_diff = `r2_clim+ar+diff` - `r2_ar+diff`
  )

## Interpretations added here:
## 1. clim_level_alone: = r2_clim. Amount of variance explained by climate
##    alone
## 2. clim_level_given_ar: = r2_clim+ar - r2_ar. Amount of variance
##    explained by adding climate to an autocorrelated process
## 3. clim_change_given_diff: r2_clim+diff - r2_diff. Amount of variance
##    explaned by adding climate to a differenced random walk
## 4. clim_change_given_ar_diff: r2_clim+ar_diff - r2_ar+diff: Amount of
##    variance explained by adding climate to a differenced random
##    walk with autocorrelated process

## Additional interpretation:
## 1. clim_level_alone >> clim_level_given_ar: climate level signal
##    shared with persistence. Can't separate climate signal from
##    signal of internal intertia
## 2. Large clim_level_given_ar: climate explains level variation that
##    persistence/autocorrelation cannot
## 3. clim_change_* << clim_level_*: expected because of scaling,
##    not ecologically meaningful

#### 2. Interpretation ####

## 1. Across all sites & taxa, how much does climate, ar, and diff
##    contribute to the model fit (Shapley values)?
climate_contrib |>
  tidyr::pivot_longer(cols = shap_clim:shap_diff,
                      names_to = 'name', values_to = 'val') |>
  dplyr::mutate(name = dplyr::if_else(name == 'shap_ar',
                                      'AR(1)',
                                      name),
                name = dplyr::if_else(name == 'shap_clim',
                                      'Climate',
                                      name),
                name = dplyr::if_else(name == 'shap_diff',
                                      'Difference',
                                      name)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = name, y = val,
                                    fill = name),
                       color = NA) +
  ggplot2::scale_fill_discrete(name = 'Model\ncomponent') +
  ggplot2::xlab('') + ggplot2::ylab('Shapley value') +
  ggplot2::theme_minimal() 

## 2. Same, based on site across all taxa
climate_contrib |>
  tidyr::pivot_longer(cols = shap_clim:shap_diff,
                      names_to = 'name', values_to = 'val') |>
  dplyr::mutate(name = dplyr::if_else(name == 'shap_ar',
                                      'AR(1)',
                                      name),
                name = dplyr::if_else(name == 'shap_clim',
                                      'Climate',
                                      name),
                name = dplyr::if_else(name == 'shap_diff',
                                      'Difference',
                                      name)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = name, y = val,
                                    fill = name),
                       color = NA) +
  ggplot2::scale_fill_discrete(name = 'Model\ncomponent') +
  ggplot2::xlab('') + ggplot2::ylab('Shapley value') +
  ggplot2::facet_wrap(~site) +
  ggplot2::theme_minimal()

## 3. Same, based on taxa across all sites
climate_contrib |>
  dplyr::filter(!is.na(shap_clim) & !is.na(shap_ar) & !is.na(shap_diff)) |> 
  dplyr::group_by(taxon) |>
  dplyr::filter(dplyr::n() >= 2) |>
  tidyr::pivot_longer(cols = shap_clim:shap_diff,
                      names_to = 'name', values_to = 'val') |>
  dplyr::mutate(name = dplyr::if_else(name == 'shap_ar',
                                      'AR(1)',
                                      name),
                name = dplyr::if_else(name == 'shap_clim',
                                      'Climate',
                                      name),
                name = dplyr::if_else(name == 'shap_diff',
                                      'Difference',
                                      name)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = name, y = val,
                                    fill = name),
                       color = NA) +
  ggplot2::scale_fill_discrete(name = 'Model\ncomponent') +
  ggplot2::xlab('') + ggplot2::ylab('Shapley value') +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::theme_minimal()

## 4. Across all sites & taxa, what variance is explained by each of the
##    8 models with climate, AR(1) and difference combinations?

# Order of x-axis and legend
x_order <- c('Climate only',
             'AR(1) only',
             'Difference only',
             'Climate & AR(1)',
             'Climate & difference',
             'AR(1) & difference',
             'All three')

climate_contrib |>
  tidyr::pivot_longer(cols = r2_clim:`r2_clim+ar+diff`,
                      names_to = 'name', values_to = 'val') |>
  dplyr::mutate(name = dplyr::if_else(name == 'r2_clim',
                                      'Climate only',
                                      name),
                name = dplyr::if_else(name == 'r2_ar',
                                      'AR(1) only',
                                      name),
                name = dplyr::if_else(name == 'r2_diff',
                                      'Difference only',
                                      name),
                name = dplyr::if_else(name == 'r2_clim+ar',
                                      'Climate & AR(1)',
                                      name),
                name = dplyr::if_else(name == 'r2_clim+diff',
                                      'Climate & difference',
                                      name),
                name = dplyr::if_else(name == 'r2_ar+diff',
                                      'AR(1) & difference',
                                      name),
                name = dplyr::if_else(name == 'r2_clim+ar+diff',
                                      'All three',
                                      name)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = factor(name, 
                                               levels = x_order),
                                    y = val,
                                    fill = factor(name,
                                                  levels = x_order)),
                       color = NA) +
  ggplot2::scale_fill_discrete(name = 'Model\ncomponent') +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

## 5. Same, based on site across all taxa
climate_contrib |>
  tidyr::pivot_longer(cols = r2_clim:`r2_clim+ar+diff`,
                      names_to = 'name', values_to = 'val') |>
  dplyr::mutate(name = dplyr::if_else(name == 'r2_clim',
                                      'Climate only',
                                      name),
                name = dplyr::if_else(name == 'r2_ar',
                                      'AR(1) only',
                                      name),
                name = dplyr::if_else(name == 'r2_diff',
                                      'Difference only',
                                      name),
                name = dplyr::if_else(name == 'r2_clim+ar',
                                      'Climate & AR(1)',
                                      name),
                name = dplyr::if_else(name == 'r2_clim+diff',
                                      'Climate & difference',
                                      name),
                name = dplyr::if_else(name == 'r2_ar+diff',
                                      'AR(1) & difference',
                                      name),
                name = dplyr::if_else(name == 'r2_clim+ar+diff',
                                      'All three',
                                      name)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = factor(name, 
                                               levels = x_order),
                                    y = val,
                                    fill = factor(name,
                                                  levels = x_order)),
                       color = NA) +
  ggplot2::scale_fill_discrete(name = 'Model\ncomponent') +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2)) +
  ggplot2::facet_wrap(~site) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

## 6. Same, based on taxon across all sites
climate_contrib |>
  dplyr::filter(!is.na(shap_clim) & !is.na(shap_ar) & !is.na(shap_diff)) |> 
  dplyr::group_by(taxon) |>
  dplyr::filter(dplyr::n() >= 2) |>
  tidyr::pivot_longer(cols = r2_clim:`r2_clim+ar+diff`,
                      names_to = 'name', values_to = 'val') |>
  dplyr::mutate(name = dplyr::if_else(name == 'r2_clim',
                                      'Climate only',
                                      name),
                name = dplyr::if_else(name == 'r2_ar',
                                      'AR(1) only',
                                      name),
                name = dplyr::if_else(name == 'r2_diff',
                                      'Difference only',
                                      name),
                name = dplyr::if_else(name == 'r2_clim+ar',
                                      'Climate & AR(1)',
                                      name),
                name = dplyr::if_else(name == 'r2_clim+diff',
                                      'Climate & difference',
                                      name),
                name = dplyr::if_else(name == 'r2_ar+diff',
                                      'AR(1) & difference',
                                      name),
                name = dplyr::if_else(name == 'r2_clim+ar+diff',
                                      'All three',
                                      name)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = factor(name, 
                                               levels = x_order),
                                    y = val,
                                    fill = factor(name,
                                                  levels = x_order)),
                       color = NA) +
  ggplot2::scale_fill_discrete(name = 'Model\ncomponent') +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2)) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

## 7. How much does climate explain variation compared to simple
##    autocorrelation overall?
climate_contrib |>
  tidyr::pivot_longer(cols = clim_level_alone:clim_level_given_ar,
                      names_to = 'name', values_to = 'val') |>
  dplyr::mutate(name = dplyr::if_else(name == 'clim_level_alone',
                                      'Climate alone',
                                      name),
                name = dplyr::if_else(name == 'clim_level_given_ar',
                                      'Climate given AR(1)',
                                      name)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = name, y = val,
                                    fill = name),
                       color = NA) +
  ggplot2::scale_fill_discrete(name = 'Model\ncomponent') +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_blank())

## 8. Same, based on sites across all taxa
climate_contrib |>
  tidyr::pivot_longer(cols = clim_level_alone:clim_level_given_ar,
                      names_to = 'name', values_to = 'val') |>
  dplyr::mutate(name = dplyr::if_else(name == 'clim_level_alone',
                                      'Climate alone',
                                      name),
                name = dplyr::if_else(name == 'clim_level_given_ar',
                                      'Climate given AR(1)',
                                      name)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = name, y = val,
                                    fill = name),
                       color = NA) +
  ggplot2::scale_fill_discrete(name = 'Model\ncomponent') +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2)) +
  ggplot2::facet_wrap(~site) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_blank())

## 9. Same, based on taxa across all sites
climate_contrib |>
  dplyr::filter(!is.na(shap_clim) & !is.na(shap_ar) & !is.na(shap_diff)) |> 
  dplyr::group_by(taxon) |>
  dplyr::filter(dplyr::n() >= 2) |>
  tidyr::pivot_longer(cols = clim_level_alone:clim_level_given_ar,
                      names_to = 'name', values_to = 'val') |>
  dplyr::mutate(name = dplyr::if_else(name == 'clim_level_alone',
                                      'Climate alone',
                                      name),
                name = dplyr::if_else(name == 'clim_level_given_ar',
                                      'Climate given AR(1)',
                                      name)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = name, y = val,
                                    fill = name),
                       color = NA) +
  ggplot2::scale_fill_discrete(name = 'Model\ncomponent') +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2)) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_blank())

## 10. How much does climate explain variation compared to a differenced
##     random walk or differenced random walk with AR(1) autocorrelation
##     overall?
climate_contrib |>
  tidyr::pivot_longer(cols = clim_change_given_diff:clim_change_given_ar_diff,
                      names_to = 'name', values_to = 'val') |>
  dplyr::mutate(name = dplyr::if_else(name == 'clim_change_given_diff',
                                      'Climate given difference',
                                      name),
                name = dplyr::if_else(name == 'clim_change_given_ar_diff',
                                      'Climate given difference & AR(1)',
                                      name)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = name, y = val,
                                    fill = name),
                       color = NA) +
  ggplot2::scale_fill_discrete(name = 'Model\ncomponent') +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_blank())

## 11. Same, based on sites across all taxa
climate_contrib |>
  tidyr::pivot_longer(cols = clim_change_given_diff:clim_change_given_ar_diff,
                      names_to = 'name', values_to = 'val') |>
  dplyr::mutate(name = dplyr::if_else(name == 'clim_change_given_diff',
                                      'Climate given difference',
                                      name),
                name = dplyr::if_else(name == 'clim_change_given_ar_diff',
                                      'Climate given difference & AR(1)',
                                      name)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = name, y = val,
                                    fill = name),
                       color = NA) +
  ggplot2::scale_fill_discrete(name = 'Model\ncomponent') +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2)) +
  ggplot2::facet_wrap(~site) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_blank())

## 12. Same, based on taxa across all sites
climate_contrib |>
  dplyr::filter(!is.na(shap_clim) & !is.na(shap_ar) & !is.na(shap_diff)) |> 
  dplyr::group_by(taxon) |>
  dplyr::filter(dplyr::n() >= 2) |>
  tidyr::pivot_longer(cols = clim_change_given_diff:clim_change_given_ar_diff,
                      names_to = 'name', values_to = 'val') |>
  dplyr::mutate(name = dplyr::if_else(name == 'clim_change_given_diff',
                                      'Climate given difference',
                                      name),
                name = dplyr::if_else(name == 'clim_change_given_ar_diff',
                                      'Climate given difference & AR(1)',
                                      name)) |>
  ggplot2::ggplot() +
  ggplot2::geom_violin(ggplot2::aes(x = name, y = val,
                                    fill = name),
                       color = NA) +
  ggplot2::scale_fill_discrete(name = 'Model\ncomponent') +
  ggplot2::xlab('') + ggplot2::ylab(expression(R^2)) +
  ggplot2::facet_wrap(~taxon) +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_blank())
