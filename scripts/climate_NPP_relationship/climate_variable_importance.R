## Selecting climate variables using random forest analysis

rm(list = ls())

# Load tree data
load('out/tree_detrended_AGBI.RData')

rm(save_comb_oos)

# Load climate data
load('climate/prism_clim.RData')

# Duplicate Harvard climate
prism_harv <- dplyr::filter(prism_long, loc == 'HARVARD')
prism_long <- dplyr::mutate(prism_long, loc = dplyr::if_else(loc == 'HARVARD', 'HARVARD Model RW', loc))
prism_long <- rbind(prism_long, prism_harv)
prism_long <- dplyr::mutate(prism_long, loc = dplyr::if_else(loc == 'HARVARD', 'HARVARD Model RW + Census', loc))

# Pivot wider
prism_monthly <- prism_long |>
  dplyr::group_by(loc) |>
  tidyr::pivot_wider(names_from = 'month',
                     values_from = c('PPT2', 'Tmean2', 'Tmin2', 'Tmax2',
                                     'Vpdmin2', 'Vpdmax2')) |>
  dplyr::mutate(year = as.numeric(year)) |>
  dplyr::rename(site = loc)

# Join with tree data
agbi_monthly <- save_comb |>
  dplyr::left_join(y = prism_monthly,
                   by = c('year', 'site'))

# Growing season climate summaries
# Pivot wider
prism_growing <- prism_long |> 
  dplyr::mutate(year = as.numeric(year)) |>
  dplyr::mutate(growing_year = dplyr::if_else(month %in% c('01', '02', '03', '04', 
                                                           '05', '06', '07', '08'),
                                              year, year + 1)) |>
  dplyr::group_by(growing_year, loc) |>
  dplyr::summarize(PPT_growing = mean(PPT2),
                   Tmean_growing = mean(Tmean2),
                   sd_PPT_growing = sd(PPT2),
                   sd_Tmean_growing = sd(Tmean2),
                   Tmin_growing = min(Tmin2),
                   Tmax_growing = max(Tmax2),
                   Vpdmin_growing = min(Vpdmin2),
                   Vpdmax_growing = max(Vpdmax2)) |>
  dplyr::rename(year = growing_year,
                site = loc)

# Join with rest of data
agbi_monthly_growing <- agbi_monthly |>
  dplyr::left_join(y = prism_growing,
                   by = c('year', 'site')) |>
  dplyr::ungroup()

#### Random forest with all sites and taxa pooled ####
all_sites_taxa <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                             data = dplyr::select(agbi_monthly_growing, 
                                                                  -tree, -year, -plot, -taxon, -site, -mean),
                                             ntree = 1000,
                                             importance = TRUE)

# Save importance of variables
importance_all_sites_taxa <- randomForest::importance(all_sites_taxa)

# Format
importance_all_sites_taxa <- as.data.frame(importance_all_sites_taxa)
importance_all_sites_taxa |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_all_sites_taxa |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

#### Random forests for each site individually across all individual trees ####

## GOOSE
goose_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'GOOSE') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

goose_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                       data = goose_agbi_monthly_growing,
                                       ntree = 1000,
                                       importance = TRUE)

importance_goose <- randomForest::importance(goose_rf)
importance_goose <- as.data.frame(importance_goose)
importance_goose |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_goose |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

## NRP
nrp_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'NRP') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

nrp_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                     data = nrp_agbi_monthly_growing,
                                     ntree = 1000,
                                     importance = TRUE)

importance_nrp <- randomForest::importance(nrp_rf)
importance_nrp <- as.data.frame(importance_nrp)
importance_nrp |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_nrp |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

## ROOSTER
rooster_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'ROOSTER') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

rooster_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                         data = rooster_agbi_monthly_growing,
                                         ntree = 1000,
                                         importance = TRUE)

importance_rooster <- randomForest::importance(rooster_rf)
importance_rooster <- as.data.frame(importance_rooster)
importance_rooster |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_rooster |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

## SYLVANIA
sylvania_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'SYLVANIA') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

sylvania_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                          data = sylvania_agbi_monthly_growing,
                                          ntree = 1000,
                                          importance = TRUE)

importance_sylvania <- randomForest::importance(sylvania_rf)
importance_sylvania <- as.data.frame(importance_sylvania)
importance_sylvania |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_sylvania |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

## HARVARD Model RW
harvard_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'HARVARD Model RW') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

harvard_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                         data = harvard_agbi_monthly_growing,
                                         ntree = 1000,
                                         importance = TRUE)

importance_harvard <- randomForest::importance(harvard_rf)
importance_harvard <- as.data.frame(importance_harvard)
importance_harvard |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_harvard |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

## HARVARD Model RW + Census
harvard_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'HARVARD Model RW + Census') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

harvard_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                         data = harvard_agbi_monthly_growing,
                                         ntree = 1000,
                                         importance = TRUE)

importance_harvard <- randomForest::importance(harvard_rf)
importance_harvard <- as.data.frame(importance_harvard)
importance_harvard |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_harvard |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

#### Random forest for each taxon individually across all individual sites and trees ####

unique(agbi_monthly_growing$taxon)

## ACRU (n = 9526 obs, 4 sites, 296 trees)

acru_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'ACRU') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

acru_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = acru_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_acru <- randomForest::importance(acru_rf)
importance_acru <- as.data.frame(importance_acru)
importance_acru |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_acru |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

## ACSA (n = 1295 obs, 3 sites, 32 trees)

acsa_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'ACSA') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

acsa_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = acsa_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_acsa <- randomForest::importance(acsa_rf)
importance_acsa <- as.data.frame(importance_acsa)
importance_acsa |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_acsa |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

## QUAL (n = 465 obs, 1 site, 17 trees)

qual_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'QUAL') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

qual_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = qual_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_qual <- randomForest::importance(qual_rf)
importance_qual <- as.data.frame(importance_qual)
importance_qual |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_qual |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

## BEAL (n = 1786 obs, 3 sites, 57 trees)

beal_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'BEAL') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

beal_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = beal_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_beal <- randomForest::importance(beal_rf)
importance_beal <- as.data.frame(importance_beal)
importance_beal |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_beal |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

## PCRU (n = 1500 obs, 1 site, 39 trees)

pcru_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'PCRU') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

pcru_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = pcru_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_pcru <- randomForest::importance(pcru_rf)
importance_pcru <- as.data.frame(importance_pcru)
importance_pcru |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_pcru |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

## PIST (n = 4262 obs, 4 sites, 120 trees)

pist_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'PIST') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

pist_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = pist_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_pist <- randomForest::importance(pist_rf)
importance_pist <- as.data.frame(importance_pist)
importance_pist |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_pist |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

## FAGR (n = 3881 obs, 4 sites, 107 trees)

fagr_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'FAGR') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

fagr_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = fagr_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_fagr <- randomForest::importance(fagr_rf)
importance_fagr <- as.data.frame(importance_fagr)
importance_fagr |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_fagr |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

## AMAR (n = 41 obs, 1 site, 1 tree)

amar_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'AMAR') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

amar_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = amar_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_amar <- randomForest::importance(amar_rf)
importance_amar <- as.data.frame(importance_amar)
importance_amar |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_amar |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

## QUMO (n = 1151 obs, 1 site, 30 trees)

qumo_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'QUMO') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

qumo_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = qumo_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_qumo <- randomForest::importance(qumo_rf)
importance_qumo <- as.data.frame(importance_qumo)
importance_qumo |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_qumo |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

## FRAM (n = 1270 obs, 1 site, 31 trees)

fram_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'FRAM') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

fram_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = fram_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_fram <- randomForest::importance(fram_rf)
importance_fram <- as.data.frame(importance_fram)
importance_fram |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_fram |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

## QURU (n = 10467 obs, 4 sites, 215 trees)

quru_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'QURU') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

quru_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = quru_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_quru <- randomForest::importance(quru_rf)
importance_quru <- as.data.frame(importance_quru)
importance_quru |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_quru |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

## THOC (n = 440 obs, 1 site, 11 trees)

thoc_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'THOC') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

thoc_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = thoc_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_thoc <- randomForest::importance(thoc_rf)
importance_thoc <- as.data.frame(importance_thoc)
importance_thoc |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_thoc |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

## OSVI (n = 82 obs, 1 site, 2 trees)

osvi_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'OSVI') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

osvi_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = osvi_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_osvi <- randomForest::importance(osvi_rf)
importance_osvi <- as.data.frame(importance_osvi)
importance_osvi |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_osvi |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

## TSCA (n = 7219 obs, 3 sites, 177 trees)

tsca_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'TSCA') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

tsca_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = tsca_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_tsca <- randomForest::importance(tsca_rf)
importance_tsca <- as.data.frame(importance_tsca)
importance_tsca |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_tsca |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

## PRSE (n = 84 obs, 2 sites, 2 trees)

prse_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'PRSE') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

prse_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = prse_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_prse <- randomForest::importance(prse_rf)
importance_prse <- as.data.frame(importance_prse)
importance_prse |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_prse |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

## BEPA (n = 161 obs, 3 sites, 4 trees)

bepa_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'BEPA') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

bepa_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = bepa_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_bepa <- randomForest::importance(bepa_rf)
importance_bepa <- as.data.frame(importance_bepa)
importance_bepa |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_bepa |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

## BELE (n = 1026 obs, 3 sites, 33 trees)

bele_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'BELE') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

bele_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = bele_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_bele <- randomForest::importance(bele_rf)
importance_bele <- as.data.frame(importance_bele)
importance_bele |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_bele |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

## QUVE (n = 239 obs, 1 site, 8 trees)

quve_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'QUVE') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

quve_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = quve_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_quve <- randomForest::importance(quve_rf)
importance_quve <- as.data.frame(importance_quve)
importance_quve |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_quve |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

## HAVI (n = 422 obs, 1 site, 37 trees)

havi_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'HAVI') |>
  dplyr::select(-tree, -year, -plot, -taxon, -site, -mean)

havi_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = havi_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_havi <- randomForest::importance(havi_rf)
importance_havi <- as.data.frame(importance_havi)
importance_havi |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
importance_havi |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(`%IncMSE`)) |>
  dplyr::slice_head(n = 10)

#### TMEAN: Random forest with all sites and taxa pooled ####

## Not updated with HARVARD

clim_vars <- c('residual_AGBI', 
               'PPT2_01', 'PPT2_02', 'PPT2_03', 'PPT2_04', 'PPT2_05', 'PPT2_06',
               'PPT2_07', 'PPT2_08', 'PPT2_09', 'PPT2_10', 'PPT2_11', 'PPT2_12', 
               'Tmean2_01', 'Tmean2_02', 'Tmean2_03', 'Tmean2_04', 'Tmean2_05', 'Tmean2_06', 
               'Tmean2_07', 'Tmean2_08', 'Tmean2_09', 'Tmean2_10', 'Tmean2_11', 'Tmean2_12', 
               'Vpdmin2_01', 'Vpdmin2_02', 'Vpdmin2_03', 'Vpdmin2_04', 'Vpdmin2_05', 'Vpdmin2_06', 
               'Vpdmin2_07', 'Vpdmin2_08', 'Vpdmin2_09', 'Vpdmin2_10', 'Vpdmin2_11', 'Vpdmin2_12', 
               'Tmean_growing', 
               'PPT_growing',
               'sd_PPT_growing',
               'sd_Tmean_growing', 
               'Vpdmin_growing', 
               'Vpdmax_growing')

all_sites_taxa <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                             data = dplyr::select(agbi_monthly_growing, 
                                                                  dplyr::all_of(clim_vars)),
                                             ntree = 1000,
                                             importance = TRUE)

# Save importance of variables
importance_all_sites_taxa <- randomForest::importance(all_sites_taxa)

# Format
importance_all_sites_taxa <- as.data.frame(importance_all_sites_taxa)
importance_all_sites_taxa |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

#### Random forests for each site individually across all individual trees ####

## GOOSE
goose_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'GOOSE') |>
  dplyr::select(dplyr::all_of(clim_vars))

goose_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                       data = goose_agbi_monthly_growing,
                                       ntree = 1000,
                                       importance = TRUE)

importance_goose <- randomForest::importance(goose_rf)
importance_goose <- as.data.frame(importance_goose)
importance_goose |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## NRP
nrp_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'NRP') |>
  dplyr::select(dplyr::all_of(clim_vars))

nrp_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                     data = nrp_agbi_monthly_growing,
                                     ntree = 1000,
                                     importance = TRUE)

importance_nrp <- randomForest::importance(nrp_rf)
importance_nrp <- as.data.frame(importance_nrp)
importance_nrp |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## ROOSTER
rooster_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'ROOSTER') |>
  dplyr::select(dplyr::all_of(clim_vars))

rooster_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                         data = rooster_agbi_monthly_growing,
                                         ntree = 1000,
                                         importance = TRUE)

importance_rooster <- randomForest::importance(rooster_rf)
importance_rooster <- as.data.frame(importance_rooster)
importance_rooster |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## SYLVANIA
sylvania_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'SYLVANIA') |>
  dplyr::select(dplyr::all_of(clim_vars))

sylvania_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                          data = sylvania_agbi_monthly_growing,
                                          ntree = 1000,
                                          importance = TRUE)

importance_sylvania <- randomForest::importance(sylvania_rf)
importance_sylvania <- as.data.frame(importance_sylvania)
importance_sylvania |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

#### Random forest for each taxon individually across all individual sites and trees ####

## ACRU (n = 3132 obs, 3 sites, 78 trees)

acru_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'ACRU') |>
  dplyr::select(dplyr::all_of(clim_vars))

acru_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = acru_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_acru <- randomForest::importance(acru_rf)
importance_acru <- as.data.frame(importance_acru)
importance_acru |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## ACSA (n = 1307 obs, 3 sites, 33 trees)

acsa_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'ACSA') |>
  dplyr::select(dplyr::all_of(clim_vars))

acsa_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = acsa_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_acsa <- randomForest::importance(acsa_rf)
importance_acsa <- as.data.frame(importance_acsa)
importance_acsa |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## QUAL (n = 465 obs, 1 site, 17 trees)

qual_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'QUAL') |>
  dplyr::select(dplyr::all_of(clim_vars))

qual_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = qual_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_qual <- randomForest::importance(qual_rf)
importance_qual <- as.data.frame(importance_qual)
importance_qual |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## BEAL (n = 890 obs, 2 sites, 22 trees)

beal_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'BEAL') |>
  dplyr::select(dplyr::all_of(clim_vars))

beal_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = beal_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_beal <- randomForest::importance(beal_rf)
importance_beal <- as.data.frame(importance_beal)
importance_beal |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## PCRU (n = 1509 obs, 1 site, 39 trees)

pcru_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'PCRU') |>
  dplyr::select(dplyr::all_of(clim_vars))

pcru_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = pcru_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_pcru <- randomForest::importance(pcru_rf)
importance_pcru <- as.data.frame(importance_pcru)
importance_pcru |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## PIST (n = 3669 obs, 3 sites, 91 trees)

pist_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'PIST') |>
  dplyr::select(dplyr::all_of(clim_vars))

pist_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = pist_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_pist <- randomForest::importance(pist_rf)
importance_pist <- as.data.frame(importance_pist)
importance_pist |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## FAGR (n = 3326 obs, 3 sites, 88 trees)

fagr_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'FAGR') |>
  dplyr::select(dplyr::all_of(clim_vars))

fagr_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = fagr_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_fagr <- randomForest::importance(fagr_rf)
importance_fagr <- as.data.frame(importance_fagr)
importance_fagr |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## AMAR (n = 41 obs, 1 site, 1 tree)

amar_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'AMAR') |>
  dplyr::select(dplyr::all_of(clim_vars))

amar_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = amar_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_amar <- randomForest::importance(amar_rf)
importance_amar <- as.data.frame(importance_amar)
importance_amar |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## QUMO (n = 1151 obs, 1 site, 30 trees)

qumo_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'QUMO') |>
  dplyr::select(dplyr::all_of(clim_vars))

qumo_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = qumo_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_qumo <- randomForest::importance(qumo_rf)
importance_qumo <- as.data.frame(importance_qumo)
importance_qumo |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## FRAM (n = 1270 obs, 1 site, 31 trees)

fram_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'FRAM') |>
  dplyr::select(dplyr::all_of(clim_vars))

fram_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = fram_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_fram <- randomForest::importance(fram_rf)
importance_fram <- as.data.frame(importance_fram)
importance_fram |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## QURU (n = 6303 obs, 3 sites, 158 trees)

quru_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'QURU') |>
  dplyr::select(dplyr::all_of(clim_vars))

quru_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = quru_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_quru <- randomForest::importance(quru_rf)
importance_quru <- as.data.frame(importance_quru)
importance_quru |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## THOC (n = 441 obs, 1 site, 11 trees)

thoc_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'THOC') |>
  dplyr::select(dplyr::all_of(clim_vars))

thoc_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = thoc_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_thoc <- randomForest::importance(thoc_rf)
importance_thoc <- as.data.frame(importance_thoc)
importance_thoc |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## OSVI (n = 82 obs, 1 site, 2 trees)

osvi_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'OSVI') |>
  dplyr::select(dplyr::all_of(clim_vars))

osvi_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = osvi_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_osvi <- randomForest::importance(osvi_rf)
importance_osvi <- as.data.frame(importance_osvi)
importance_osvi |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## TSCA (n = 6526 obs, 2 sites, 164 trees)

tsca_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'TSCA') |>
  dplyr::select(dplyr::all_of(clim_vars))

tsca_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = tsca_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_tsca <- randomForest::importance(tsca_rf)
importance_tsca <- as.data.frame(importance_tsca)
importance_tsca |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## PRSE (n = 84 obs, 2 sites, 2 trees)

prse_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'PRSE') |>
  dplyr::select(dplyr::all_of(clim_vars))

prse_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = prse_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_prse <- randomForest::importance(prse_rf)
importance_prse <- as.data.frame(importance_prse)
importance_prse |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## BEPA (n = 161 obs, 3 sites, 4 trees)

bepa_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'BEPA') |>
  dplyr::select(dplyr::all_of(clim_vars))

bepa_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = bepa_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_bepa <- randomForest::importance(bepa_rf)
importance_bepa <- as.data.frame(importance_bepa)
importance_bepa |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## BELE (n = 666 obs, 2 sites, 17 trees)

bele_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'BELE') |>
  dplyr::select(dplyr::all_of(clim_vars))

bele_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = bele_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_bele <- randomForest::importance(bele_rf)
importance_bele <- as.data.frame(importance_bele)
importance_bele |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

#### TMIN: Random forest with all sites and taxa pooled ####

## Not updated with HARVARD

clim_vars <- c('residual_AGBI', 
               'PPT2_01', 'PPT2_02', 'PPT2_03', 'PPT2_04', 'PPT2_05', 'PPT2_06',
               'PPT2_07', 'PPT2_08', 'PPT2_09', 'PPT2_10', 'PPT2_11', 'PPT2_12',
               'Tmin2_01', 'Tmin2_02', 'Tmin2_03', 'Tmin2_04', 'Tmin2_05', 'Tmin2_06',
               'Tmin2_07', 'Tmin2_08', 'Tmin2_09', 'Tmin2_10', 'Tmin2_11', 'Tmin2_12', 
               'Vpdmin2_01', 'Vpdmin2_02', 'Vpdmin2_03', 'Vpdmin2_04', 'Vpdmin2_05', 'Vpdmin2_06', 
               'Vpdmin2_07', 'Vpdmin2_08', 'Vpdmin2_09', 'Vpdmin2_10', 'Vpdmin2_11', 'Vpdmin2_12', 
               'Tmin_growing',
               'PPT_growing',
               'sd_PPT_growing',
               'sd_Tmean_growing', 
               'Vpdmin_growing', 
               'Vpdmax_growing')

all_sites_taxa <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                             data = dplyr::select(agbi_monthly_growing, 
                                                                  dplyr::all_of(clim_vars)),
                                             ntree = 1000,
                                             importance = TRUE)

# Save importance of variables
importance_all_sites_taxa <- randomForest::importance(all_sites_taxa)

# Format
importance_all_sites_taxa <- as.data.frame(importance_all_sites_taxa)
importance_all_sites_taxa |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

#### Random forests for each site individually across all individual trees ####

## GOOSE
goose_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'GOOSE') |>
  dplyr::select(dplyr::all_of(clim_vars))

goose_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                       data = goose_agbi_monthly_growing,
                                       ntree = 1000,
                                       importance = TRUE)

importance_goose <- randomForest::importance(goose_rf)
importance_goose <- as.data.frame(importance_goose)
importance_goose |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## NRP
nrp_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'NRP') |>
  dplyr::select(dplyr::all_of(clim_vars))

nrp_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                     data = nrp_agbi_monthly_growing,
                                     ntree = 1000,
                                     importance = TRUE)

importance_nrp <- randomForest::importance(nrp_rf)
importance_nrp <- as.data.frame(importance_nrp)
importance_nrp |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## ROOSTER
rooster_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'ROOSTER') |>
  dplyr::select(dplyr::all_of(clim_vars))

rooster_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                         data = rooster_agbi_monthly_growing,
                                         ntree = 1000,
                                         importance = TRUE)

importance_rooster <- randomForest::importance(rooster_rf)
importance_rooster <- as.data.frame(importance_rooster)
importance_rooster |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## SYLVANIA
sylvania_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'SYLVANIA') |>
  dplyr::select(dplyr::all_of(clim_vars))

sylvania_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                          data = sylvania_agbi_monthly_growing,
                                          ntree = 1000,
                                          importance = TRUE)

importance_sylvania <- randomForest::importance(sylvania_rf)
importance_sylvania <- as.data.frame(importance_sylvania)
importance_sylvania |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

#### Random forest for each taxon individually across all individual sites and trees ####

## ACRU (n = 3132 obs, 3 sites, 78 trees)

acru_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'ACRU') |>
  dplyr::select(dplyr::all_of(clim_vars))

acru_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = acru_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_acru <- randomForest::importance(acru_rf)
importance_acru <- as.data.frame(importance_acru)
importance_acru |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## ACSA (n = 1307 obs, 3 sites, 33 trees)

acsa_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'ACSA') |>
  dplyr::select(dplyr::all_of(clim_vars))

acsa_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = acsa_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_acsa <- randomForest::importance(acsa_rf)
importance_acsa <- as.data.frame(importance_acsa)
importance_acsa |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## QUAL (n = 465 obs, 1 site, 17 trees)

qual_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'QUAL') |>
  dplyr::select(dplyr::all_of(clim_vars))

qual_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = qual_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_qual <- randomForest::importance(qual_rf)
importance_qual <- as.data.frame(importance_qual)
importance_qual |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## BEAL (n = 890 obs, 2 sites, 22 trees)

beal_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'BEAL') |>
  dplyr::select(dplyr::all_of(clim_vars))

beal_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = beal_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_beal <- randomForest::importance(beal_rf)
importance_beal <- as.data.frame(importance_beal)
importance_beal |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## PCRU (n = 1509 obs, 1 site, 39 trees)

pcru_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'PCRU') |>
  dplyr::select(dplyr::all_of(clim_vars))

pcru_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = pcru_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_pcru <- randomForest::importance(pcru_rf)
importance_pcru <- as.data.frame(importance_pcru)
importance_pcru |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## PIST (n = 3669 obs, 3 sites, 91 trees)

pist_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'PIST') |>
  dplyr::select(dplyr::all_of(clim_vars))

pist_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = pist_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_pist <- randomForest::importance(pist_rf)
importance_pist <- as.data.frame(importance_pist)
importance_pist |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## FAGR (n = 3326 obs, 3 sites, 88 trees)

fagr_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'FAGR') |>
  dplyr::select(dplyr::all_of(clim_vars))

fagr_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = fagr_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_fagr <- randomForest::importance(fagr_rf)
importance_fagr <- as.data.frame(importance_fagr)
importance_fagr |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## AMAR (n = 41 obs, 1 site, 1 tree)

amar_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'AMAR') |>
  dplyr::select(dplyr::all_of(clim_vars))

amar_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = amar_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_amar <- randomForest::importance(amar_rf)
importance_amar <- as.data.frame(importance_amar)
importance_amar |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## QUMO (n = 1151 obs, 1 site, 30 trees)

qumo_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'QUMO') |>
  dplyr::select(dplyr::all_of(clim_vars))

qumo_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = qumo_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_qumo <- randomForest::importance(qumo_rf)
importance_qumo <- as.data.frame(importance_qumo)
importance_qumo |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## FRAM (n = 1270 obs, 1 site, 31 trees)

fram_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'FRAM') |>
  dplyr::select(dplyr::all_of(clim_vars))

fram_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = fram_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_fram <- randomForest::importance(fram_rf)
importance_fram <- as.data.frame(importance_fram)
importance_fram |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## QURU (n = 6303 obs, 3 sites, 158 trees)

quru_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'QURU') |>
  dplyr::select(dplyr::all_of(clim_vars))

quru_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = quru_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_quru <- randomForest::importance(quru_rf)
importance_quru <- as.data.frame(importance_quru)
importance_quru |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## THOC (n = 441 obs, 1 site, 11 trees)

thoc_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'THOC') |>
  dplyr::select(dplyr::all_of(clim_vars))

thoc_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = thoc_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_thoc <- randomForest::importance(thoc_rf)
importance_thoc <- as.data.frame(importance_thoc)
importance_thoc |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## OSVI (n = 82 obs, 1 site, 2 trees)

osvi_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'OSVI') |>
  dplyr::select(dplyr::all_of(clim_vars))

osvi_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = osvi_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_osvi <- randomForest::importance(osvi_rf)
importance_osvi <- as.data.frame(importance_osvi)
importance_osvi |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## TSCA (n = 6526 obs, 2 sites, 164 trees)

tsca_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'TSCA') |>
  dplyr::select(dplyr::all_of(clim_vars))

tsca_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = tsca_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_tsca <- randomForest::importance(tsca_rf)
importance_tsca <- as.data.frame(importance_tsca)
importance_tsca |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## PRSE (n = 84 obs, 2 sites, 2 trees)

prse_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'PRSE') |>
  dplyr::select(dplyr::all_of(clim_vars))

prse_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = prse_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_prse <- randomForest::importance(prse_rf)
importance_prse <- as.data.frame(importance_prse)
importance_prse |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## BEPA (n = 161 obs, 3 sites, 4 trees)

bepa_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'BEPA') |>
  dplyr::select(dplyr::all_of(clim_vars))

bepa_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = bepa_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_bepa <- randomForest::importance(bepa_rf)
importance_bepa <- as.data.frame(importance_bepa)
importance_bepa |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## BELE (n = 666 obs, 2 sites, 17 trees)

bele_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'BELE') |>
  dplyr::select(dplyr::all_of(clim_vars))

bele_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = bele_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_bele <- randomForest::importance(bele_rf)
importance_bele <- as.data.frame(importance_bele)
importance_bele |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

#### TMAX: Random forest with all sites and taxa pooled ####

## Not updated with HARVARD

clim_vars <- c('residual_AGBI', 
               'PPT2_01', 'PPT2_02', 'PPT2_03', 'PPT2_04', 'PPT2_05', 'PPT2_06',
               'PPT2_07', 'PPT2_08', 'PPT2_09', 'PPT2_10', 'PPT2_11', 'PPT2_12',
               'Tmax2_01', 'Tmax2_02', 'Tmax2_03', 'Tmax2_04', 'Tmax2_05', 'Tmax2_06',
               'Tmax2_07', 'Tmax2_08', 'Tmax2_09', 'Tmax2_10', 'Tmax2_11', 'Tmax2_12',
               'Vpdmin2_01', 'Vpdmin2_02', 'Vpdmin2_03', 'Vpdmin2_04', 'Vpdmin2_05', 'Vpdmin2_06',
               'Vpdmin2_07', 'Vpdmin2_08', 'Vpdmin2_09', 'Vpdmin2_10', 'Vpdmin2_11', 'Vpdmin2_12', 
               'Tmax_growing',
               'PPT_growing',
               'sd_PPT_growing',
               'sd_Tmean_growing',
               'Vpdmin_growing',
               'Vpdmax_growing')

all_sites_taxa <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                             data = dplyr::select(agbi_monthly_growing, 
                                                                  dplyr::all_of(clim_vars)),
                                             ntree = 1000,
                                             importance = TRUE)

# Save importance of variables
importance_all_sites_taxa <- randomForest::importance(all_sites_taxa)

# Format
importance_all_sites_taxa <- as.data.frame(importance_all_sites_taxa)
importance_all_sites_taxa |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

#### Random forests for each site individually across all individual trees ####

## GOOSE
goose_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'GOOSE') |>
  dplyr::select(dplyr::all_of(clim_vars))

goose_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                       data = goose_agbi_monthly_growing,
                                       ntree = 1000,
                                       importance = TRUE)

importance_goose <- randomForest::importance(goose_rf)
importance_goose <- as.data.frame(importance_goose)
importance_goose |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## NRP
nrp_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'NRP') |>
  dplyr::select(dplyr::all_of(clim_vars))

nrp_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                     data = nrp_agbi_monthly_growing,
                                     ntree = 1000,
                                     importance = TRUE)

importance_nrp <- randomForest::importance(nrp_rf)
importance_nrp <- as.data.frame(importance_nrp)
importance_nrp |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## ROOSTER
rooster_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'ROOSTER') |>
  dplyr::select(dplyr::all_of(clim_vars))

rooster_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                         data = rooster_agbi_monthly_growing,
                                         ntree = 1000,
                                         importance = TRUE)

importance_rooster <- randomForest::importance(rooster_rf)
importance_rooster <- as.data.frame(importance_rooster)
importance_rooster |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## SYLVANIA
sylvania_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'SYLVANIA') |>
  dplyr::select(dplyr::all_of(clim_vars))

sylvania_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                          data = sylvania_agbi_monthly_growing,
                                          ntree = 1000,
                                          importance = TRUE)

importance_sylvania <- randomForest::importance(sylvania_rf)
importance_sylvania <- as.data.frame(importance_sylvania)
importance_sylvania |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

#### Random forest for each taxon individually across all individual sites and trees ####

## ACRU (n = 3132 obs, 3 sites, 78 trees)

acru_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'ACRU') |>
  dplyr::select(dplyr::all_of(clim_vars))

acru_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = acru_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_acru <- randomForest::importance(acru_rf)
importance_acru <- as.data.frame(importance_acru)
importance_acru |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## ACSA (n = 1307 obs, 3 sites, 33 trees)

acsa_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'ACSA') |>
  dplyr::select(dplyr::all_of(clim_vars))

acsa_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = acsa_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_acsa <- randomForest::importance(acsa_rf)
importance_acsa <- as.data.frame(importance_acsa)
importance_acsa |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## QUAL (n = 465 obs, 1 site, 17 trees)

qual_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'QUAL') |>
  dplyr::select(dplyr::all_of(clim_vars))

qual_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = qual_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_qual <- randomForest::importance(qual_rf)
importance_qual <- as.data.frame(importance_qual)
importance_qual |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## BEAL (n = 890 obs, 2 sites, 22 trees)

beal_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'BEAL') |>
  dplyr::select(dplyr::all_of(clim_vars))

beal_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = beal_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_beal <- randomForest::importance(beal_rf)
importance_beal <- as.data.frame(importance_beal)
importance_beal |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## PCRU (n = 1509 obs, 1 site, 39 trees)

pcru_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'PCRU') |>
  dplyr::select(dplyr::all_of(clim_vars))

pcru_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = pcru_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_pcru <- randomForest::importance(pcru_rf)
importance_pcru <- as.data.frame(importance_pcru)
importance_pcru |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## PIST (n = 3669 obs, 3 sites, 91 trees)

pist_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'PIST') |>
  dplyr::select(dplyr::all_of(clim_vars))

pist_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = pist_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_pist <- randomForest::importance(pist_rf)
importance_pist <- as.data.frame(importance_pist)
importance_pist |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## FAGR (n = 3326 obs, 3 sites, 88 trees)

fagr_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'FAGR') |>
  dplyr::select(dplyr::all_of(clim_vars))

fagr_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = fagr_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_fagr <- randomForest::importance(fagr_rf)
importance_fagr <- as.data.frame(importance_fagr)
importance_fagr |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## AMAR (n = 41 obs, 1 site, 1 tree)

amar_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'AMAR') |>
  dplyr::select(dplyr::all_of(clim_vars))

amar_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = amar_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_amar <- randomForest::importance(amar_rf)
importance_amar <- as.data.frame(importance_amar)
importance_amar |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## QUMO (n = 1151 obs, 1 site, 30 trees)

qumo_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'QUMO') |>
  dplyr::select(dplyr::all_of(clim_vars))

qumo_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = qumo_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_qumo <- randomForest::importance(qumo_rf)
importance_qumo <- as.data.frame(importance_qumo)
importance_qumo |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## FRAM (n = 1270 obs, 1 site, 31 trees)

fram_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'FRAM') |>
  dplyr::select(dplyr::all_of(clim_vars))

fram_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = fram_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_fram <- randomForest::importance(fram_rf)
importance_fram <- as.data.frame(importance_fram)
importance_fram |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## QURU (n = 6303 obs, 3 sites, 158 trees)

quru_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'QURU') |>
  dplyr::select(dplyr::all_of(clim_vars))

quru_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = quru_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_quru <- randomForest::importance(quru_rf)
importance_quru <- as.data.frame(importance_quru)
importance_quru |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## THOC (n = 441 obs, 1 site, 11 trees)

thoc_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'THOC') |>
  dplyr::select(dplyr::all_of(clim_vars))

thoc_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = thoc_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_thoc <- randomForest::importance(thoc_rf)
importance_thoc <- as.data.frame(importance_thoc)
importance_thoc |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## OSVI (n = 82 obs, 1 site, 2 trees)

osvi_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'OSVI') |>
  dplyr::select(dplyr::all_of(clim_vars))

osvi_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = osvi_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_osvi <- randomForest::importance(osvi_rf)
importance_osvi <- as.data.frame(importance_osvi)
importance_osvi |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## TSCA (n = 6526 obs, 2 sites, 164 trees)

tsca_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'TSCA') |>
  dplyr::select(dplyr::all_of(clim_vars))

tsca_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = tsca_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_tsca <- randomForest::importance(tsca_rf)
importance_tsca <- as.data.frame(importance_tsca)
importance_tsca |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## PRSE (n = 84 obs, 2 sites, 2 trees)

prse_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'PRSE') |>
  dplyr::select(dplyr::all_of(clim_vars))

prse_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = prse_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_prse <- randomForest::importance(prse_rf)
importance_prse <- as.data.frame(importance_prse)
importance_prse |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## BEPA (n = 161 obs, 3 sites, 4 trees)

bepa_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'BEPA') |>
  dplyr::select(dplyr::all_of(clim_vars))

bepa_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = bepa_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_bepa <- randomForest::importance(bepa_rf)
importance_bepa <- as.data.frame(importance_bepa)
importance_bepa |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## BELE (n = 666 obs, 2 sites, 17 trees)

bele_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'BELE') |>
  dplyr::select(dplyr::all_of(clim_vars))

bele_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = bele_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_bele <- randomForest::importance(bele_rf)
importance_bele <- as.data.frame(importance_bele)
importance_bele |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

#### VPDMAX: Random forest with all sites and taxa pooled ####

## Not updated with HARVARD

clim_vars <- c('residual_AGBI',
               'PPT2_01', 'PPT2_02', 'PPT2_03', 'PPT2_04', 'PPT2_05', 'PPT2_06',
               'PPT2_07', 'PPT2_08', 'PPT2_09', 'PPT2_10', 'PPT2_11', 'PPT2_12',
               'Vpdmax2_01', 'Vpdmax2_02', 'Vpdmax2_03', 'Vpdmax2_04', 'Vpdmax2_05', 'Vpdmax2_06',
               'Vpdmax2_07', 'Vpdmax2_08', 'Vpdmax2_09', 'Vpdmax2_10', 'Vpdmax2_11', 'Vpdmax2_12',
               'Vpdmin2_01', 'Vpdmin2_02', 'Vpdmin2_03', 'Vpdmin2_04', 'Vpdmin2_05', 'Vpdmin2_06',
               'Vpdmin2_07', 'Vpdmin2_08', 'Vpdmin2_09', 'Vpdmin2_10', 'Vpdmin2_11', 'Vpdmin2_12', 
               'Tmean_growing',
               'PPT_growing',
               'Vpdmax_growing',
               'sd_PPT_growing',
               'sd_Tmean_growing',
               'Vpdmin_growing')

all_sites_taxa <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                             data = dplyr::select(agbi_monthly_growing, 
                                                                  dplyr::all_of(clim_vars)),
                                             ntree = 1000,
                                             importance = TRUE)

# Save importance of variables
importance_all_sites_taxa <- randomForest::importance(all_sites_taxa)

# Format
importance_all_sites_taxa <- as.data.frame(importance_all_sites_taxa)
importance_all_sites_taxa |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

#### Random forests for each site individually across all individual trees ####

## GOOSE
goose_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'GOOSE') |>
  dplyr::select(dplyr::all_of(clim_vars))

goose_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                       data = goose_agbi_monthly_growing,
                                       ntree = 1000,
                                       importance = TRUE)

importance_goose <- randomForest::importance(goose_rf)
importance_goose <- as.data.frame(importance_goose)
importance_goose |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## NRP
nrp_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'NRP') |>
  dplyr::select(dplyr::all_of(clim_vars))

nrp_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                     data = nrp_agbi_monthly_growing,
                                     ntree = 1000,
                                     importance = TRUE)

importance_nrp <- randomForest::importance(nrp_rf)
importance_nrp <- as.data.frame(importance_nrp)
importance_nrp |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## ROOSTER
rooster_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'ROOSTER') |>
  dplyr::select(dplyr::all_of(clim_vars))

rooster_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                         data = rooster_agbi_monthly_growing,
                                         ntree = 1000,
                                         importance = TRUE)

importance_rooster <- randomForest::importance(rooster_rf)
importance_rooster <- as.data.frame(importance_rooster)
importance_rooster |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## SYLVANIA
sylvania_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(site == 'SYLVANIA') |>
  dplyr::select(dplyr::all_of(clim_vars))

sylvania_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                          data = sylvania_agbi_monthly_growing,
                                          ntree = 1000,
                                          importance = TRUE)

importance_sylvania <- randomForest::importance(sylvania_rf)
importance_sylvania <- as.data.frame(importance_sylvania)
importance_sylvania |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

#### Random forest for each taxon individually across all individual sites and trees ####

## ACRU (n = 3132 obs, 3 sites, 78 trees)

acru_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'ACRU') |>
  dplyr::select(dplyr::all_of(clim_vars))

acru_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = acru_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_acru <- randomForest::importance(acru_rf)
importance_acru <- as.data.frame(importance_acru)
importance_acru |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## ACSA (n = 1307 obs, 3 sites, 33 trees)

acsa_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'ACSA') |>
  dplyr::select(dplyr::all_of(clim_vars))

acsa_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = acsa_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_acsa <- randomForest::importance(acsa_rf)
importance_acsa <- as.data.frame(importance_acsa)
importance_acsa |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## QUAL (n = 465 obs, 1 site, 17 trees)

qual_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'QUAL') |>
  dplyr::select(dplyr::all_of(clim_vars))

qual_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = qual_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_qual <- randomForest::importance(qual_rf)
importance_qual <- as.data.frame(importance_qual)
importance_qual |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## BEAL (n = 890 obs, 2 sites, 22 trees)

beal_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'BEAL') |>
  dplyr::select(dplyr::all_of(clim_vars))

beal_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = beal_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_beal <- randomForest::importance(beal_rf)
importance_beal <- as.data.frame(importance_beal)
importance_beal |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## PCRU (n = 1509 obs, 1 site, 39 trees)

pcru_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'PCRU') |>
  dplyr::select(dplyr::all_of(clim_vars))

pcru_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = pcru_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_pcru <- randomForest::importance(pcru_rf)
importance_pcru <- as.data.frame(importance_pcru)
importance_pcru |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## PIST (n = 3669 obs, 3 sites, 91 trees)

pist_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'PIST') |>
  dplyr::select(dplyr::all_of(clim_vars))

pist_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = pist_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_pist <- randomForest::importance(pist_rf)
importance_pist <- as.data.frame(importance_pist)
importance_pist |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## FAGR (n = 3326 obs, 3 sites, 88 trees)

fagr_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'FAGR') |>
  dplyr::select(dplyr::all_of(clim_vars))

fagr_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = fagr_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_fagr <- randomForest::importance(fagr_rf)
importance_fagr <- as.data.frame(importance_fagr)
importance_fagr |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## AMAR (n = 41 obs, 1 site, 1 tree)

amar_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'AMAR') |>
  dplyr::select(dplyr::all_of(clim_vars))

amar_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = amar_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_amar <- randomForest::importance(amar_rf)
importance_amar <- as.data.frame(importance_amar)
importance_amar |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## QUMO (n = 1151 obs, 1 site, 30 trees)

qumo_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'QUMO') |>
  dplyr::select(dplyr::all_of(clim_vars))

qumo_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = qumo_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_qumo <- randomForest::importance(qumo_rf)
importance_qumo <- as.data.frame(importance_qumo)
importance_qumo |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## FRAM (n = 1270 obs, 1 site, 31 trees)

fram_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'FRAM') |>
  dplyr::select(dplyr::all_of(clim_vars))

fram_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = fram_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_fram <- randomForest::importance(fram_rf)
importance_fram <- as.data.frame(importance_fram)
importance_fram |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## QURU (n = 6303 obs, 3 sites, 158 trees)

quru_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'QURU') |>
  dplyr::select(dplyr::all_of(clim_vars))

quru_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = quru_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_quru <- randomForest::importance(quru_rf)
importance_quru <- as.data.frame(importance_quru)
importance_quru |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## THOC (n = 441 obs, 1 site, 11 trees)

thoc_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'THOC') |>
  dplyr::select(dplyr::all_of(clim_vars))

thoc_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = thoc_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_thoc <- randomForest::importance(thoc_rf)
importance_thoc <- as.data.frame(importance_thoc)
importance_thoc |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## OSVI (n = 82 obs, 1 site, 2 trees)

osvi_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'OSVI') |>
  dplyr::select(dplyr::all_of(clim_vars))

osvi_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = osvi_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_osvi <- randomForest::importance(osvi_rf)
importance_osvi <- as.data.frame(importance_osvi)
importance_osvi |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## TSCA (n = 6526 obs, 2 sites, 164 trees)

tsca_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'TSCA') |>
  dplyr::select(dplyr::all_of(clim_vars))

tsca_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = tsca_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_tsca <- randomForest::importance(tsca_rf)
importance_tsca <- as.data.frame(importance_tsca)
importance_tsca |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## PRSE (n = 84 obs, 2 sites, 2 trees)

prse_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'PRSE') |>
  dplyr::select(dplyr::all_of(clim_vars))

prse_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = prse_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_prse <- randomForest::importance(prse_rf)
importance_prse <- as.data.frame(importance_prse)
importance_prse |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## BEPA (n = 161 obs, 3 sites, 4 trees)

bepa_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'BEPA') |>
  dplyr::select(dplyr::all_of(clim_vars))

bepa_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = bepa_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_bepa <- randomForest::importance(bepa_rf)
importance_bepa <- as.data.frame(importance_bepa)
importance_bepa |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)

## BELE (n = 666 obs, 2 sites, 17 trees)

bele_agbi_monthly_growing <- agbi_monthly_growing |>
  dplyr::filter(taxon == 'BELE') |>
  dplyr::select(dplyr::all_of(clim_vars))

bele_rf <- randomForest::randomForest(formula = residual_AGBI ~ .,
                                      data = bele_agbi_monthly_growing,
                                      ntree = 1000,
                                      importance = TRUE)

importance_bele <- randomForest::importance(bele_rf)
importance_bele <- as.data.frame(importance_bele)
importance_bele |>
  tibble::rownames_to_column(var = 'variable') |>
  dplyr::arrange(desc(IncNodePurity)) |>
  dplyr::slice_head(n = 10)
