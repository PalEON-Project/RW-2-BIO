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
library(forecast)
library(lme4)
library(performance)
library(tidyr)
library(ggcorrplot)


goose_total_agb <- readRDS('sites/GOOSE/runs/v2.0_012021/output/AGB_STAN_GOOSE_v2.0_012021.RDS')
goose_total_agbi <- readRDS('sites/GOOSE/runs/v2.0_012021/output/AGBI_STAN_GOOSE_v2.0_012021.RDS')

# Combine
goose_total <- goose_total_agb |>
  left_join(goose_total_agbi, by = c('tree', 'year', 'iter',
                                     'taxon', 'model', 'plot')) |>
  rename(AGB = value.x,
         AGBI = value.y) |>
  select(-c(type.x, type.y)) |>
  # Add site name for combining sites into one df
  mutate(site = 'GOOSE')


all_tree <- goose_total |>
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

taxon_group <- goose_total |>
  group_by(year, iter, taxon) |>
  # Means across iterations
  summarize(AGBI.sum = sum(AGBI, na.rm = T))


taxon_summary = taxon_group %>%
  group_by(year, taxon) %>% 
  summarize(AGBI.mean = mean(AGBI.sum, na.rm = T),
            AGBI.sd = sd(AGBI.sum))

taxon_3 = taxon_summary %>% 
  group_by(taxon) %>% 
  summarize(AGBI.mean2 = mean(AGBI.mean),
            AGBI.sd = sd(AGBI.mean, na.rm = T))
  
#changing to wide format with taxons as column names and values = ABGI.mean 
#values_fill=0 does not work 
taxon_wide = pivot_wider(data = taxon_group, names_from = taxon, values_from = AGBI.mean, values_fill = 0 )

#correlation matrix between all species for GOOSE for all trees found in one SITE for each given year....
cor(taxon_wide$ACSA, taxon_wide$BEPA, use = "complete.obs")
correlation_A = data.frame(cor(taxon_wide[, c('ACRU','ACSA','BEPA', 'FAGR', 'PIST', 'QUAL', 'QUMO', 'QURU', 'BELE',
                   'OSVI', 'AMAR')],use = "complete.obs"))
#find significance 
correlation_B = correlation_A
correlation_B[correlation_B<0.4] = NA


#correlation figure between species for ABGI.mean
#shitty colours!!!
ggcorrplot(correlation_A, method = "circle", type = "lower", hc.order = FALSE)
ggcorrplot(correlation_B)
#  scale_fill_gradient2(low = "yellow", mid = 'pink', high = "light green", breaks=c(0, 1), limit=c(0, 1))

# all_species <-goose_total |>
#   group_by(year, plot, iter, taxon, model, site) |>
#   summarize(AGB.mean = mean(AGB),
#             AGBI.mean = mean(AGBI),
#             AGB.sd = sd(AGB),
#             AGBI.sd = sd(AGBI),
#             AGB.low = quantile(AGB, probs = 0.025, na.rm = T),
#             AGB.high = quantile(AGB, probs = 0.975, na.rm = T),
#             AGBI.low = quantile(AGBI, probs = 0.025, na.rm = T),
#             AGBI.high = quantile(AGBI, probs = 0.975, na.rm = T),
#             # Number of trees per species
#             ntree = n(),
#             # Average tree size per species
#             agb_pertree = AGB.mean / ntree)

all_species <-goose_total |>
  group_by(year, plot, iter, taxon, model, site) |>
  summarize(AGB.sum = sum(AGB),
            AGBI.sum = sum(AGBI))

all_site <- goose_total |>
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


######start figures
#mean ABGI for the site over time....
ggplot(data = all_site)+
  geom_line(aes(x = year, y = AGBI.mean, colour = plot, group = plot)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = 'none',
        plot.title = element_text(size = 14, hjust = 0.5, face = 'bold')) +
  xlab('') + ylab('Aboveground biomass') +
  ggtitle('Goose Egg')
  


