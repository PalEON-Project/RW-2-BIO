## Random forest model for finding important variables for NPP across sites

rm(list = ls())

# Load total increment
goose_total_agbi <- readRDS('sites/GOOSE/runs/v2.0_012021/output/AGBI_STAN_GOOSE_v2.0_012021.RDS')
nrp_total_agbi <- readRDS('sites/NORTHROUND/runs/v2.0_082020/output/AGBI_STAN_NORTHROUND_v2.0_082020.RDS')
rooster_total_agbi <- readRDS('sites/ROOSTER/runs/v2.0_082020/output/AGBI_STAN_ROOSTER_v2.0_082020.RDS')
sylv_total_agbi <- readRDS('sites/SYLVANIA/runs/v2.0_082020/output/AGBI_STAN_SYLVANIA_v2.0_082020.RDS')

# Add site names column
goose_total_agbi <- dplyr::mutate(goose_total_agbi, site = 'GOOSE')
nrp_total_agbi <- dplyr::mutate(nrp_total_agbi, site = 'NRP')
rooster_total_agbi <- dplyr::mutate(rooster_total_agbi, site = 'ROOSTER')
sylv_total_agbi <- dplyr::mutate(sylv_total_agbi, site = 'SYLVANIA')

# Combine sites
total_agbi <- rbind(goose_total_agbi, nrp_total_agbi,
                    rooster_total_agbi, sylv_total_agbi)

# Save mean over iterations in dataframe
total_agbi <- total_agbi |>
  dplyr::group_by(year, plot, taxon, site) |>
  dplyr::summarize(mean = mean(value)) |>
  dplyr::ungroup()

# Load climate data
load('climate/prism_clim.RData')

# Format
prism_long <- dplyr::rename(prism_long, site = loc) |>
  dplyr::mutate(year = as.numeric(year)) |>
  dplyr::mutate(PPT2 = scale(PPT2),
                Tmean2 = scale(Tmean2),
                Tmin2 = scale(Tmin2),
                Tmax2 = scale(Tmax2),
                Vpdmin2 = scale(Vpdmin2),
                Vpdmax2 = scale(Vpdmax2))

# Pivot wider
prism_annual <- prism_long |>
  dplyr::group_by(year, site) |>
  dplyr::summarize(mean_PPT = mean(PPT2),
                   mean_Tmean = mean(Tmean2),
                   mean_Tmin = mean(Tmin2),
                   mean_Tmax = mean(Tmax2),
                   mean_Vpdmin = mean(Vpdmin2),
                   mean_Vpdmax = mean(Vpdmax2)) 

prism_month <- tidyr::pivot_wider(prism_long, names_from = 'month', values_from = c('PPT2', 'Tmean2', 'Tmin2',
                                                                                    'Tmax2', 'Vpdmin2', 'Vpdmax2'))
#### GOOSE ####

goose_joined <- total_agbi |>
  dplyr::filter(site == 'GOOSE') |>
  dplyr::left_join(y = prism_annual, by = c('site', 'year')) |>
  dplyr::left_join(y = prism_month, by = c('site', 'year')) |>
  dplyr::select(-site, -plot) |>
  dplyr::mutate(taxon = as.factor(taxon))

goose_split <- rsample::initial_split(goose_joined, prop = 0.7)
goose_train <- rsample::training(goose_split)
goose_test <- rsample::testing(goose_split)

colnames(goose_train) <- colnames(goose_test) <- colnames(goose_joined)

goose_rf <- randomForest::randomForest(formula = mean ~ ., # all variables
                                       data = goose_train, # for goose egg
                                       ntree = 2000, # number of trees- borrowed from Fick & Evett 2018
                                       maxnodes = 7, # trial and error
                                       importance = TRUE,
                                       mtry = sqrt(ncol(goose_joined)-1)) # number of randomly selected variables per node- borrowed from Hanberry et al. 2012

goose_importance <- randomForest::importance(goose_rf)
goose_importance <- as.data.frame(goose_importance)

goose_importance <- goose_importance |>
  tibble::rownames_to_column(var = 'variable')

goose_importance |>
  ggplot2::ggplot(ggplot2::aes(x = reorder(variable, -IncNodePurity), y = log(IncNodePurity))) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('Variable') + ggplot2::ylab('log(mean decrease in node purity)')

goose_importance |>
  dplyr::mutate(variable_class = dplyr::if_else(grepl(pattern = 'Tmean', variable), 'tmean', NA),
                variable_class = dplyr::if_else(grepl(pattern = 'PPT', variable), 'ppt', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Tmin', variable), 'tmin', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Tmax', variable), 'tmax', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Vpdmin', variable), 'vpdmin', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Vpdmax', variable), 'vpdmax', variable_class),
                variable_class = dplyr::if_else(variable == 'taxon', 'taxon', variable_class),
                variable_class = dplyr::if_else(variable == 'year', 'year', variable_class)) |>
  dplyr::group_by(variable_class) |>
  dplyr::summarize(mean_importance = mean(IncNodePurity)) |>
  ggplot2::ggplot(ggplot2::aes(x = reorder(variable_class, -mean_importance), y = log(mean_importance))) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('Variable') + ggplot2::ylab('log(mean decrease in node purity)')

goose_importance |>
  ggplot2::ggplot(ggplot2::aes(x = reorder(variable, -`%IncMSE`), y = `%IncMSE`)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('Variable') + ggplot2::ylab('Mean decrease in accuracy')

goose_importance |>
  dplyr::mutate(variable_class = dplyr::if_else(grepl(pattern = 'Tmean', variable), 'tmean', NA),
                variable_class = dplyr::if_else(grepl(pattern = 'PPT', variable), 'ppt', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Tmin', variable), 'tmin', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Tmax', variable), 'tmax', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Vpdmin', variable), 'vpdmin', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Vpdmax', variable), 'vpdmax', variable_class),
                variable_class = dplyr::if_else(variable == 'taxon', 'taxon', variable_class),
                variable_class = dplyr::if_else(variable == 'year', 'year', variable_class)) |>
  dplyr::group_by(variable_class) |>
  dplyr::summarize(mean_importance = mean(`%IncMSE`)) |>
  ggplot2::ggplot(ggplot2::aes(x = reorder(variable_class, -mean_importance), y = mean_importance)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('Variable') + ggplot2::ylab('Mean decrease in accuracy')

goose_pred <- predict(object = goose_rf,
                      newdata = goose_test)

goose_match <- lm(formula = goose_pred ~ goose_test$mean)

summary(goose_match)

#### NRP ####

nrp_joined <- total_agbi |>
  dplyr::filter(site == 'NRP') |>
  dplyr::left_join(y = prism_annual, by = c('site', 'year')) |>
  dplyr::left_join(y = prism_month, by = c('site', 'year')) |>
  dplyr::select(-site, -plot) |>
  dplyr::mutate(taxon = as.factor(taxon)) |>
  dplyr::mutate(dplyr::across(where(is.numeric), scale))

nrp_split <- rsample::initial_split(nrp_joined, prop = 0.7)
nrp_train <- rsample::training(nrp_split)
nrp_test <- rsample::testing(nrp_split)

nrp_rf <- randomForest::randomForest(formula = mean ~ .,
                                     data = nrp_train,
                                     ntree = 2000,
                                     maxnodes = 7,
                                     importance = TRUE,
                                     mtry = sqrt(ncol(goose_joined)-1))

nrp_importance <- randomForest::importance(nrp_rf)
nrp_importance <- as.data.frame(nrp_importance)

nrp_importance <- nrp_importance |>
  tibble::rownames_to_column(var = 'variable')

nrp_importance |>
  ggplot2::ggplot(ggplot2::aes(x = reorder(variable, -IncNodePurity), y = log(IncNodePurity))) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('Variable') + ggplot2::ylab('log(mean decrease in node purity)')

nrp_importance |>
  dplyr::mutate(variable_class = dplyr::if_else(grepl(pattern = 'Tmean', variable), 'tmean', NA),
                variable_class = dplyr::if_else(grepl(pattern = 'PPT', variable), 'ppt', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Tmin', variable), 'tmin', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Tmax', variable), 'tmax', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Vpdmin', variable), 'vpdmin', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Vpdmax', variable), 'vpdmax', variable_class),
                variable_class = dplyr::if_else(variable == 'taxon', 'taxon', variable_class),
                variable_class = dplyr::if_else(variable == 'year', 'year', variable_class)) |>
  dplyr::group_by(variable_class) |>
  dplyr::summarize(mean_importance = mean(IncNodePurity)) |>
  ggplot2::ggplot(ggplot2::aes(x = reorder(variable_class, -mean_importance), y = log(mean_importance))) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('Variable') + ggplot2::ylab('log(mean decrease in node purity)')

nrp_importance |>
  ggplot2::ggplot(ggplot2::aes(x = reorder(variable, -`%IncMSE`), y = `%IncMSE`)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('Variable') + ggplot2::ylab('Mean decrease in accuracy')

nrp_importance |>
  dplyr::mutate(variable_class = dplyr::if_else(grepl(pattern = 'Tmean', variable), 'tmean', NA),
                variable_class = dplyr::if_else(grepl(pattern = 'PPT', variable), 'ppt', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Tmin', variable), 'tmin', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Tmax', variable), 'tmax', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Vpdmin', variable), 'vpdmin', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Vpdmax', variable), 'vpdmax', variable_class),
                variable_class = dplyr::if_else(variable == 'taxon', 'taxon', variable_class),
                variable_class = dplyr::if_else(variable == 'year', 'year', variable_class)) |>
  dplyr::group_by(variable_class) |>
  dplyr::summarize(mean_importance = mean(`%IncMSE`)) |>
  ggplot2::ggplot(ggplot2::aes(x = reorder(variable_class, -mean_importance), y = mean_importance)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('Variable') + ggplot2::ylab('Mean decrease in accuracy')

nrp_pred <- predict(object = nrp_rf,
                    newdata = nrp_test)

nrp_match <- lm(formula = nrp_pred ~ nrp_test$mean)

summary(nrp_match)

#### ROOSTER ####

rooster_joined <- total_agbi |>
  dplyr::filter(site == 'ROOSTER') |>
  dplyr::left_join(y = prism_annual, by = c('site', 'year')) |>
  dplyr::left_join(y = prism_month, by = c('site', 'year')) |>
  dplyr::select(-site, -plot) |>
  dplyr::mutate(taxon = as.factor(taxon)) |>
  dplyr::mutate(dplyr::across(where(is.numeric), scale))

rooster_split <- rsample::initial_split(rooster_joined, prop = 0.7)
rooster_train <- rsample::training(rooster_split)
rooster_test <- rsample::testing(rooster_split)

rooster_rf <- randomForest::randomForest(formula = mean ~ .,
                                         data = rooster_train,
                                         ntree = 2000,
                                         maxnodes = 7,
                                         importance = TRUE,
                                         mtry = sqrt(ncol(rooster_joined)-1))

rooster_importance <- randomForest::importance(rooster_rf)
rooster_importance <- as.data.frame(rooster_importance)

rooster_importance <- rooster_importance |>
  tibble::rownames_to_column(var = 'variable')

rooster_importance |>
  ggplot2::ggplot(ggplot2::aes(x = reorder(variable, -IncNodePurity), y = log(IncNodePurity))) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('Variable') + ggplot2::ylab('log(mean decrease in node purity)')

rooster_importance |>
  dplyr::mutate(variable_class = dplyr::if_else(grepl(pattern = 'Tmean', variable), 'tmean', NA),
                variable_class = dplyr::if_else(grepl(pattern = 'PPT', variable), 'ppt', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Tmin', variable), 'tmin', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Tmax', variable), 'tmax', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Vpdmin', variable), 'vpdmin', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Vpdmax', variable), 'vpdmax', variable_class),
                variable_class = dplyr::if_else(variable == 'taxon', 'taxon', variable_class),
                variable_class = dplyr::if_else(variable == 'year', 'year', variable_class)) |>
  dplyr::group_by(variable_class) |>
  dplyr::summarize(mean_importance = mean(IncNodePurity)) |>
  ggplot2::ggplot(ggplot2::aes(x = reorder(variable_class, -mean_importance), y = log(mean_importance))) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('Variable') + ggplot2::ylab('log(mean decrease in node purity)')

rooster_importance |>
  ggplot2::ggplot(ggplot2::aes(x = reorder(variable, -`%IncMSE`), y = `%IncMSE`)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('Variable') + ggplot2::ylab('Mean decrease in accuracy')

rooster_importance |>
  dplyr::mutate(variable_class = dplyr::if_else(grepl(pattern = 'Tmean', variable), 'tmean', NA),
                variable_class = dplyr::if_else(grepl(pattern = 'PPT', variable), 'ppt', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Tmin', variable), 'tmin', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Tmax', variable), 'tmax', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Vpdmin', variable), 'vpdmin', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Vpdmax', variable), 'vpdmax', variable_class),
                variable_class = dplyr::if_else(variable == 'taxon', 'taxon', variable_class),
                variable_class = dplyr::if_else(variable == 'year', 'year', variable_class)) |>
  dplyr::group_by(variable_class) |>
  dplyr::summarize(mean_importance = mean(`%IncMSE`)) |>
  ggplot2::ggplot(ggplot2::aes(x = reorder(variable_class, -mean_importance), y = mean_importance)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('Variable') + ggplot2::ylab('Mean decrease in accuracy')

rooster_pred <- predict(object = rooster_rf,
                        newdata = rooster_test)

rooster_match <- lm(formula = rooster_pred ~ rooster_test$mean)

summary(rooster_match)

#### SYLVANIA ####

sylvania_joined <- total_agbi |>
  dplyr::filter(site == 'SYLVANIA') |>
  dplyr::left_join(y = prism_annual, by = c('site', 'year')) |>
  dplyr::left_join(y = prism_month, by = c('site', 'year')) |>
  dplyr::select(-site, -plot) |>
  dplyr::mutate(taxon = as.factor(taxon)) |>
  dplyr::mutate(dplyr::across(where(is.numeric), scale))

sylvania_split <- rsample::initial_split(sylvania_joined, prop = 0.7)
sylvania_train <- rsample::training(sylvania_split)
sylvania_test <- rsample::testing(sylvania_split)

sylvania_rf <- randomForest::randomForest(formula = mean ~ .,
                                          data = sylvania_train,
                                          ntree = 2000,
                                          maxnodes = 7,
                                          importance = TRUE,
                                          mtry = sqrt(ncol(sylvania_joined)-1))

sylvania_importance <- randomForest::importance(sylvania_rf)
sylvania_importance <- as.data.frame(sylvania_importance)

sylvania_importance <- sylvania_importance |>
  tibble::rownames_to_column(var = 'variable')

sylvania_importance |>
  ggplot2::ggplot(ggplot2::aes(x = reorder(variable, -IncNodePurity), y = log(IncNodePurity))) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('Variable') + ggplot2::ylab('log(mean decrease in node purity)')

sylvania_importance |>
  dplyr::mutate(variable_class = dplyr::if_else(grepl(pattern = 'Tmean', variable), 'tmean', NA),
                variable_class = dplyr::if_else(grepl(pattern = 'PPT', variable), 'ppt', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Tmin', variable), 'tmin', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Tmax', variable), 'tmax', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Vpdmin', variable), 'vpdmin', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Vpdmax', variable), 'vpdmax', variable_class),
                variable_class = dplyr::if_else(variable == 'taxon', 'taxon', variable_class),
                variable_class = dplyr::if_else(variable == 'year', 'year', variable_class)) |>
  dplyr::group_by(variable_class) |>
  dplyr::summarize(mean_importance = mean(IncNodePurity)) |>
  ggplot2::ggplot(ggplot2::aes(x = reorder(variable_class, -mean_importance), y = log(mean_importance))) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('Variable') + ggplot2::ylab('log(mean decrease in node purity)')

sylvania_importance |>
  ggplot2::ggplot(ggplot2::aes(x = reorder(variable, -`%IncMSE`), y = `%IncMSE`)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('Variable') + ggplot2::ylab('Mean decrease in accuracy')

sylvania_importance |>
  dplyr::mutate(variable_class = dplyr::if_else(grepl(pattern = 'Tmean', variable), 'tmean', NA),
                variable_class = dplyr::if_else(grepl(pattern = 'PPT', variable), 'ppt', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Tmin', variable), 'tmin', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Tmax', variable), 'tmax', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Vpdmin', variable), 'vpdmin', variable_class),
                variable_class = dplyr::if_else(grepl(pattern = 'Vpdmax', variable), 'vpdmax', variable_class),
                variable_class = dplyr::if_else(variable == 'taxon', 'taxon', variable_class),
                variable_class = dplyr::if_else(variable == 'year', 'year', variable_class)) |>
  dplyr::group_by(variable_class) |>
  dplyr::summarize(mean_importance = mean(`%IncMSE`)) |>
  ggplot2::ggplot(ggplot2::aes(x = reorder(variable_class, -mean_importance), y = mean_importance)) +
  ggplot2::geom_bar(stat = 'identity') +
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90)) +
  ggplot2::xlab('Variable') + ggplot2::ylab('Mean decrease in accuracy')

sylvania_pred <- predict(object = sylvania_rf,
                         newdata = sylvania_test)

sylvania_match <- lm(formula = sylvania_pred ~ sylvania_test$mean)

summary(sylvania_match)
