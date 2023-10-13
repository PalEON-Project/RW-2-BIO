## Figures

# 1. Total biomass over time / site
# 2. Total increment over time / site
# 3. Species specific biomass over time / site
# 4. Species specific incrmenet over time / site
# 5. Comparison of biomass / site on one plot
# 6. Comparison of increment / site on one plot
# 7. Comparison of species / site on one plot where species overlap
# 8. Average temperature vs increment
# 9. Average precipitation vs increment
# 10. Simple model of temperature, precipitation, increment across sites
# 11. Simple model with biomass instead of increment

rm(list = ls())

# Load total biomass
goose_total_agb <- readRDS('sites/GOOSE/runs/v2.0_012021/output/AGB_STAN_GOOSE_v2.0_012021.RDS')
nrp_total_agb <- readRDS('sites/NORTHROUND/runs/v2.0_082020/output/AGB_STAN_NORTHROUND_v2.0_082020.RDS')
rooster_total_agb <- readRDS('sites/ROOSTER/runs/v2.0_082020/output/AGB_STAN_ROOSTER_v2.0_082020.RDS')
sylv_total_agb <- readRDS('sites/ROOSTER/runs/v2.0_082020/output/AGB_STAN_ROOSTER_v2.0_082020.RDS')

# Load total increment
goose_total_agbi <- readRDS('sites/GOOSE/runs/v2.0_012021/output/AGBI_STAN_GOOSE_v2.0_012021.RDS')
nrp_total_agbi <- readRDS('sites/NORTHROUND/runs/v2.0_082020/output/AGBI_STAN_NORTHROUND_v2.0_082020.RDS')
rooster_total_agbi <- readRDS('sites/ROOSTER/runs/v2.0_082020/output/AGBI_STAN_ROOSTER_v2.0_082020.RDS')
sylv_total_agbi <- readRDS('sites/SYLVANIA/runs/v2.0_082020/output/AGBI_STAN_SYLVANIA_v2.0_082020.RDS')

# Combine
goose_total <- 