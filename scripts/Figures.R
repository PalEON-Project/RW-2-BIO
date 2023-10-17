## Figures

# 1. Total biomass over time / site
# 2. Total increment over time / site
# 3. Species specific biomass over time / site
# 4. Species specific incrmenet over time / site
# 5. Comparison of species / site on one plot where species overlap
# 6. Average temperature vs increment
# 7. Average precipitation vs increment
# 8. Simple model of temperature, precipitation, increment across sites
# 9. Simple model with biomass instead of increment

rm(list = ls())

library(dplyr)
library(ggplot2)

# Load total biomass
goose_total_agb <- readRDS('sites/GOOSE/runs/v2.0_012021/output/AGB_STAN_GOOSE_v2.0_012021.RDS')
nrp_total_agb <- readRDS('sites/NORTHROUND/runs/v2.0_082020/output/AGB_STAN_NORTHROUND_v2.0_082020.RDS')
rooster_total_agb <- readRDS('sites/ROOSTER/runs/v2.0_082020/output/AGB_STAN_ROOSTER_v2.0_082020.RDS')
sylv_total_agb <- readRDS('sites/SYLVANIA/runs/v2.0_082020/output/AGB_STAN_SYLVANIA_v2.0_082020.RDS')

# Load total increment
goose_total_agbi <- readRDS('sites/GOOSE/runs/v2.0_012021/output/AGBI_STAN_GOOSE_v2.0_012021.RDS')
nrp_total_agbi <- readRDS('sites/NORTHROUND/runs/v2.0_082020/output/AGBI_STAN_NORTHROUND_v2.0_082020.RDS')
rooster_total_agbi <- readRDS('sites/ROOSTER/runs/v2.0_082020/output/AGBI_STAN_ROOSTER_v2.0_082020.RDS')
sylv_total_agbi <- readRDS('sites/SYLVANIA/runs/v2.0_082020/output/AGBI_STAN_SYLVANIA_v2.0_082020.RDS')

# Combine
goose_total <- goose_total_agb |>
  left_join(goose_total_agbi, by = c('tree', 'year', 'iter',
                                     'taxon', 'model', 'plot')) |>
  rename(AGB = value.x,
         AGBI = value.y) |>
  select(-c(type.x, type.y)) |>
  # Add site name for combining sites into one df
  mutate(site = 'GOOSE')
nrp_total <- nrp_total_agb |>
  left_join(nrp_total_agbi, by = c('tree', 'year', 'iter',
                                   'taxon', 'model', 'plot')) |>
  rename(AGB = value.x,
         AGBI = value.y) |>
  select(-c(type.x, type.y)) |>
  mutate(site = 'NRP')
rooster_total <- rooster_total_agb |>
  left_join(rooster_total_agbi, by = c('tree', 'year', 'iter',
                                       'taxon', 'model', 'plot')) |>
  rename(AGB = value.x,
         AGBI = value.y) |>
  select(-c(type.x, type.y)) |>
  mutate(site = 'ROOSTER')
sylvania_total <- sylv_total_agb |>
  left_join(sylv_total_agbi, by = c('tree', 'year', 'iter',
                                      'taxon', 'model', 'plot')) |>
  rename(AGB = value.x,
         AGBI = value.y) |>
  select(-c(type.x, type.y)) |>
  mutate(site = 'SYLVANIA')

# Combine sites
all_data <- rbind(goose_total, nrp_total, rooster_total, sylvania_total)

# Create summaries by tree
all_tree <- all_data |>
  group_by(tree, year, plot, taxon, model, site) |>
  # Means across iterations
  summarize(AGB.mean = mean(AGB),
            AGBI.mean = mean(AGBI),
            # Standard deviations across iterations
            AGB.sd = sd(AGB),
            AGBI.sd = sd(AGBI),
            # Credible intervals across iterations
            AGB.low = quantile(AGB, probs = 0.025, na.rm = T),
            AGB.high = quantile(AGB, probs = 0.975, na.rm = T),
            AGBI.low = quantile(AGBI, probs = 0.025, na.rm = T),
            AGBI.high = quantile(AGBI, probs = 0.975, na.rm = T))

# Create summaries by species
all_species <- all_data |>
  group_by(year, plot, taxon, model, site) |>
  summarize(AGB.mean = mean(AGB),
            AGBI.mean = mean(AGBI),
            AGB.sd = sd(AGB),
            AGBI.sd = sd(AGBI),
            AGB.low = quantile(AGB, probs = 0.025, na.rm = T),
            AGB.high = quantile(AGB, probs = 0.975, na.rm = T),
            AGBI.low = quantile(AGBI, probs = 0.025, na.rm = T),
            AGBI.high = quantile(AGBI, probs = 0.975, na.rm = T),
            # Number of trees per species
            ntree = n(),
            # Average tree size per species
            agb_pertree = AGB.mean / ntree)

# Create summaries by site
all_site <- all_data |>
  group_by(year, plot, model, site) |>
  summarize(AGB.mean = mean(AGB),
            AGBI.mean = mean(AGBI),
            AGB.sd = sd(AGB),
            AGBI.sd = sd(AGBI),
            AGB.low = quantile(AGB, probs = 0.025, na.rm = T),
            AGB.high = quantile(AGB, probs = 0.975, na.rm = T),
            AGBI.low = quantile(AGBI, probs = 0.025, na.rm = T),
            AGBI.high = quantile(AGBI, probs = 0.975, na.rm = T),
            ntree = n(),
            agb_persite = AGB.mean / ntree)

#### Plot 1 ####

# Total biomass over time/site

all_site |>
  mutate(site = if_else(site == 'GOOSE', 'Goose Egg', site),
         site = if_else(site == 'NRP', 'North Round Pond', site),
         site = if_else(site == 'ROOSTER', 'Rooster Hill', site),
         site = if_else(site == 'SYLVANIA', 'Sylvania', site)) |>
  mutate(plot = paste('Plot',plot)) |>
  ggplot(aes(x = year, y = AGB.mean, ymin = AGB.low, ymax = AGB.high)) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  facet_grid(plot~site) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

#### Plot 2 ####

# Total increment over time/site

all_site |>
  mutate(site = if_else(site == 'GOOSE', 'Goose Egg', site),
         site = if_else(site == 'NRP', 'North Round Pond', site),
         site = if_else(site == 'ROOSTER', 'Rooster Hill', site),
         site = if_else(site == 'SYLVANIA', 'Sylvania', site)) |>
  mutate(plot = paste0('Plot',plot)) |>
  ggplot(aes(x = year, y = AGBI.mean, ymin = AGBI.low, ymax = AGBI.high)) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  facet_grid(plot~site) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))

#### Plot 3 ####

# species specific biomass over time/site

all_species |>
  mutate(site = if_else(site == 'GOOSE', 'Goose Egg', site),
         site = if_else(site == 'NRP', 'North Round Pond', site),
         site = if_else(site == 'ROOSTER', 'Rooster Hill', site),
         site = if_else(site == 'SYLVANIA', 'Sylvania', site)) |>
  mutate(taxon = if_else(taxon == 'ACRU', 'Acer rubrum', taxon),
         taxon = if_else(taxon == 'ACSA', 'Acer saccharum', taxon),
         taxon = if_else(taxon == 'AMAR', 'Amelanchier arborea', taxon),
         taxon = if_else(taxon == 'BEAL', 'Betula alleghaniensis', taxon),
         taxon = if_else(taxon == 'BELE', 'Betula lenta', taxon),
         taxon = if_else(taxon == 'BEPA', 'Betula papyrifera', taxon),
         taxon = if_else(taxon == 'FAGR', 'Fagus grandifolia', taxon),
         taxon = if_else(taxon == 'FRAM', 'Fraxinus americana', taxon),
         taxon = if_else(taxon == 'OSVI', 'Ostrya virginiana', taxon),
         taxon = if_else(taxon == 'PCRU', 'Picea rubens', taxon),
         taxon = if_else(taxon == 'PIST', 'Pinus strobus', taxon),
         taxon = if_else(taxon == 'PRSE', 'Prunus serotina', taxon),
         taxon = if_else(taxon == 'QUAL', 'Quercus alba', taxon),
         taxon = if_else(taxon == 'QUMO', 'Quercus montana', taxon),
         taxon = if_else(taxon == 'QURU', 'Quercus rubra', taxon),
         taxon = if_else(taxon == 'THOC', 'Thuja occidentalis', taxon),
         taxon = if_else(taxon == 'TSCA', 'Tsuga canadensis', taxon)) |>
  rename(Taxon = taxon) |>
  mutate(plot = paste0('Plot',plot)) |>
  ggplot(aes(x = year, y = AGB.mean, ymin = AGB.low, ymax = AGB.high, color = Taxon, fill = Taxon)) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  facet_grid(plot~site) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab('') + ylab('Aboveground biomass')

# Goose Egg
all_species |>
  filter(site == 'GOOSE') |>
  mutate(site = if_else(site == 'GOOSE', 'Goose Egg', site)) |>
  filter(taxon %in% c('FAGR', 'PIST', 'QUAL', 'QUMO', 'QURU')) |>
  mutate(taxon = if_else(taxon == 'FAGR', 'Fagus grandifolia', taxon),
         taxon = if_else(taxon == 'PIST', 'Pinus strobus', taxon),
         taxon = if_else(taxon == 'QUAL', 'Quercus alba', taxon),
         taxon = if_else(taxon == 'QUMO', 'Quercus montana', taxon),
         taxon = if_else(taxon == 'QURU', 'Quercus rubra', taxon)) |>
  rename(Taxon = taxon) |>
  mutate(plot = paste0('Plot',plot)) |>
  ggplot(aes(x = year, y = AGB.mean, ymin = AGB.low, ymax = AGB.high, color = Taxon, fill = Taxon)) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  facet_grid(plot~Taxon) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none',
        plot.title = element_text(size = 14, hjust = 0.5, face = 'bold')) +
  xlab('') + ylab('Aboveground biomass') +
  ggtitle('Goose Egg')

# Rooster Hill
all_species |>
  filter(site == 'ROOSTER') |>
  filter(taxon %in% c('ACRU', 'FAGR', 'PCRU', 'PIST', 'QURU')) |>
  mutate(taxon = if_else(taxon == 'ACRU', 'Acer rubrum', taxon),
         taxon = if_else(taxon == 'FAGR', 'Fagus grandifolia', taxon),
         taxon = if_else(taxon == 'PCRU', 'Picea rubens', taxon),
         taxon = if_else(taxon == 'PIST', 'Pinus strobus', taxon),
         taxon = if_else(taxon == 'QURU', 'Quercus rubra', taxon)) |>
  rename(Taxon = taxon) |>
  mutate(plot = paste0('Plot',plot)) |>
  ggplot(aes(x = year, y = AGB.mean, ymin = AGB.low, ymax = AGB.high, color = Taxon, fill = Taxon)) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  facet_grid(plot~Taxon) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none',
        plot.title = element_text(size = 14, hjust = 0.5, face = 'bold')) +
  xlab('') + ylab('Aboveground biomass') +
  ggtitle('Rooster Hill')

# North Round Pond
all_species |>
  filter(site == 'NRP') |>
  filter(taxon %in% c('ACRU', 'ACSA', 'BEAL', 'FAGR', 'FRAM',
                      'PIST', 'QURU', 'TSCA')) |>
  mutate(taxon = if_else(taxon == 'PIST', 'Pinus strobus', taxon),
         taxon = if_else(taxon == 'QURU', 'Quercus rubra', taxon),
         taxon = if_else(taxon == 'TSCA', 'Tsuga canadensis', taxon),
         taxon = if_else(taxon == 'ACRU', 'Acer rubrum', taxon),
         taxon = if_else(taxon == 'ACSA', 'Acer saccharum', taxon),
         taxon = if_else(taxon == 'BEAL', 'Betula alleghaniensis', taxon),
         taxon = if_else(taxon == 'FAGR', 'Fagus grandifolia', taxon),
         taxon = if_else(taxon == 'FRAM', 'Fraxinus americana', taxon)) |>
  rename(Taxon = taxon) |>
  mutate(plot = paste0('Plot',plot)) |>
  ggplot(aes(x = year, y = AGB.mean, ymin = AGB.low, ymax = AGB.high, color = Taxon, fill = Taxon)) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  facet_grid(plot~Taxon) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none',
        plot.title = element_text(size = 14, hjust = 0.5, face = 'bold')) +
  xlab('') + ylab('Aboveground bimoass') +
  ggtitle('North Round Pond')

# Sylvania
all_species |>
  filter(site == 'SYLVANIA') |>
  filter(taxon %in% c('ACSA', 'BEAL', 'THOC', 'TSCA')) |>
  mutate(taxon = if_else(taxon == 'ACSA', 'Acer saccharum', taxon),
         taxon = if_else(taxon == 'BEAL', 'Betula alleghaniensis', taxon),
         taxon = if_else(taxon == 'THOC', 'Thuja occidentalis', taxon),
         taxon = if_else(taxon == 'TSCA', 'Tsuga canadensis', taxon)) |>
  rename(Taxon = taxon) |>
  mutate(plot = paste0('Plot',plot)) |>
  ggplot(aes(x = year, y = AGB.mean, ymin = AGB.low, ymax = AGB.high, color = Taxon, fill = Taxon)) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  facet_grid(plot~Taxon) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none',
        plot.title = element_text(size = 14, hjust = 0.5, face = 'bold')) +
  xlab('') + ylab('Aboveground biomass') +
  ggtitle('Sylvania')

#### Plot 4 ####

# Species specific increment over time/site

all_species |>
  mutate(site = if_else(site == 'GOOSE', 'Goose Egg', site),
         site = if_else(site == 'NRP', 'North Round Pond', site),
         site = if_else(site == 'ROOSTER', 'Rooster Hill', site),
         site = if_else(site == 'SYLVANIA', 'Sylvania', site)) |>
  mutate(taxon = if_else(taxon == 'ACRU', 'Acer rubrum', taxon),
         taxon = if_else(taxon == 'ACSA', 'Acer saccharum', taxon),
         taxon = if_else(taxon == 'AMAR', 'Amelanchier arborea', taxon),
         taxon = if_else(taxon == 'BEAL', 'Betula alleghaniensis', taxon),
         taxon = if_else(taxon == 'BELE', 'Betula lenta', taxon),
         taxon = if_else(taxon == 'BEPA', 'Betula papyrifera', taxon),
         taxon = if_else(taxon == 'FAGR', 'Fagus grandifolia', taxon),
         taxon = if_else(taxon == 'FRAM', 'Fraxinus americana', taxon),
         taxon = if_else(taxon == 'OSVI', 'Ostrya virginiana', taxon),
         taxon = if_else(taxon == 'PCRU', 'Picea rubens', taxon),
         taxon = if_else(taxon == 'PIST', 'Pinus strobus', taxon),
         taxon = if_else(taxon == 'PRSE', 'Prunus serotina', taxon),
         taxon = if_else(taxon == 'QUAL', 'Quercus alba', taxon),
         taxon = if_else(taxon == 'QUMO', 'Quercus montana', taxon),
         taxon = if_else(taxon == 'QURU', 'Quercus rubra', taxon),
         taxon = if_else(taxon == 'THOC', 'Thuja occidentalis', taxon),
         taxon = if_else(taxon == 'TSCA', 'Tsuga canadensis', taxon)) |>
  rename(Taxon = taxon) |>
  mutate(plot = paste0('Plot',plot)) |>
  ggplot(aes(x = year, y = AGBI.mean, ymin = AGBI.low, ymax = AGBI.high, color = Taxon, fill = Taxon)) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  facet_grid(plot~site) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab('') + ylab('Aboveground biomass increment') 

# Goose Egg
all_species |>
  filter(site == 'GOOSE') |>
  mutate(site = if_else(site == 'GOOSE', 'Goose Egg', site)) |>
  filter(taxon %in% c('FAGR', 'PIST', 'QUAL', 'QUMO', 'QURU')) |>
  mutate(taxon = if_else(taxon == 'FAGR', 'Fagus grandifolia', taxon),
         taxon = if_else(taxon == 'PIST', 'Pinus strobus', taxon),
         taxon = if_else(taxon == 'QUAL', 'Quercus alba', taxon),
         taxon = if_else(taxon == 'QUMO', 'Quercus montana', taxon),
         taxon = if_else(taxon == 'QURU', 'Quercus rubra', taxon)) |>
  rename(Taxon = taxon) |>
  mutate(plot = paste0('Plot',plot)) |>
  ggplot(aes(x = year, y = AGBI.mean, ymin = AGBI.low, ymax = AGBI.high, color = Taxon, fill = Taxon)) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  facet_grid(plot~Taxon) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none',
        plot.title = element_text(size = 14, hjust = 0.5, face = 'bold')) +
  xlab('') + ylab('Aboveground biomass increment') +
  ggtitle('Goose Egg')

# Rooster Hill
all_species |>
  filter(site == 'ROOSTER') |>
  filter(taxon %in% c('ACRU', 'FAGR', 'PCRU', 'PIST', 'QURU')) |>
  mutate(taxon = if_else(taxon == 'ACRU', 'Acer rubrum', taxon),
         taxon = if_else(taxon == 'FAGR', 'Fagus grandifolia', taxon),
         taxon = if_else(taxon == 'PCRU', 'Picea rubens', taxon),
         taxon = if_else(taxon == 'PIST', 'Pinus strobus', taxon),
         taxon = if_else(taxon == 'QURU', 'Quercus rubra', taxon)) |>
  rename(Taxon = taxon) |>
  mutate(plot = paste0('Plot',plot)) |>
  ggplot(aes(x = year, y = AGB.mean, ymin = AGB.low, ymax = AGB.high, color = Taxon, fill = Taxon)) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  facet_grid(plot~Taxon) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none',
        plot.title = element_text(size = 14, face = 'bold', hjust = 0.5)) +
  xlab('') + ylab('Aboveground biomass') +
  ggtitle('Rooster Hill')

# North Round Pond
all_species |>
  filter(site == 'NRP') |>
  filter(taxon %in% c('ACRU', 'ACSA', 'BEAL', 'FAGR', 'FRAM',
                      'PIST', 'QURU', 'TSCA')) |>
  mutate(taxon = if_else(taxon == 'PIST', 'Pinus strobus', taxon),
         taxon = if_else(taxon == 'QURU', 'Quercus rubra', taxon),
         taxon = if_else(taxon == 'TSCA', 'Tsuga canadensis', taxon),
         taxon = if_else(taxon == 'ACRU', 'Acer rubrum', taxon),
         taxon = if_else(taxon == 'ACSA', 'Acer saccharum', taxon),
         taxon = if_else(taxon == 'BEAL', 'Betula alleghaniensis', taxon),
         taxon = if_else(taxon == 'FAGR', 'Fagus grandifolia', taxon),
         taxon = if_else(taxon == 'FRAM', 'Fraxinus americana', taxon)) |>
  rename(Taxon = taxon) |>
  mutate(plot = paste0('Plot',plot)) |>
  ggplot(aes(x = year, y = AGBI.mean, ymin = AGBI.low, ymax = AGBI.high, color = Taxon, fill = Taxon)) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  facet_grid(plot~Taxon) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none',
        plot.title = element_text(size = 14, hjust = 0.5, face = 'bold')) +
  xlab('') + ylab('Aboveground bimoass increment') +
  ggtitle('North Round Pond')

# Sylvania
all_species |>
  filter(site == 'SYLVANIA') |>
  filter(taxon %in% c('ACSA', 'BEAL', 'THOC', 'TSCA')) |>
  mutate(taxon = if_else(taxon == 'ACSA', 'Acer saccharum', taxon),
         taxon = if_else(taxon == 'BEAL', 'Betula alleghaniensis', taxon),
         taxon = if_else(taxon == 'THOC', 'Thuja occidentalis', taxon),
         taxon = if_else(taxon == 'TSCA', 'Tsuga canadensis', taxon)) |>
  rename(Taxon = taxon) |>
  mutate(plot = paste0('Plot',plot)) |>
  ggplot(aes(x = year, y = AGBI.mean, ymin = AGBI.low, ymax = AGBI.high, color = Taxon, fill = Taxon)) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  facet_grid(plot~Taxon) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none',
        plot.title = element_text(size = 14, hjust = 0.4, face = 'bold')) +
  xlab('') + ylab('Aboveground biomass increment') +
  ggtitle('Sylvania')

#### Plot 5 ####

# species/site with species overlap

## Quercus rubra has the most overlap

all_species |>
  filter(taxon == 'QURU') |>
  mutate(taxon = 'Quercus rubra') |>
  rename(Taxon = taxon) |>
  mutate(plot = paste0('Plot',plot)) |>
  ggplot(aes(x = year, y = AGB.mean, ymin = AGB.low, ymax = AGB.high, color = plot, fill = plot)) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  facet_wrap(site~Taxon) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab('') + ylab('Aboveground biomass')

all_species |>
  filter(taxon == 'QURU') |>
  mutate(taxon = 'Quercus rubra') |>
  rename(Taxon = taxon) |>
  mutate(plot = paste0('Plot',plot)) |>
  ggplot(aes(x = year, y = AGBI.mean, ymin = AGBI.low, ymax = AGBI.high, color = plot, fill = plot)) +
  geom_line() +
  geom_ribbon(alpha = 0.5) +
  facet_wrap(site~Taxon) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab('') + ylab('Aboveground biomass increment')

#### Plot 6 ####

# average temperature vs increment

# Load climate data
load('climate/prism_clim.RData')

# Average climate across months
prism_wide <- prism_long |>
  pivot_wider(names_from = month, values_from = c(PPT, Tmean))

prism_wide2 <- prism_wide |>
  mutate(PPT_mean = rowMeans(dplyr::select(prism_wide, starts_with('PPT'))),
         Tmean_mean = rowMeans(dplyr::select(prism_wide, starts_with('Tmean'))))
colnames(prism_wide2)[2] = 'site'
prism_wide2$year <- as.numeric(prism_wide2$year)

climate_increment <- all_site |>
  left_join(prism_wide2, by = c('year', 'site'))

climate_increment |>
  ggplot(aes(x = Tmean_mean, y = AGB.mean, ymin = AGB.low, ymax = AGB.high, color = site)) +
  geom_point() +
  geom_errorbar() +
  theme_minimal() +
  xlab('Mean annual temperature')
  
