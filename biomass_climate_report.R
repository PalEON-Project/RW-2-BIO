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
# library(forecast)
#library(lme4)
# library(performance)
library(tidyr)
library(ggcorrplot)
library(reshape2)
library(broom)

# #above ground biomass
# goose_total_agb <- readRDS('sites/GOOSE/runs/v2.0_012021/output/AGB_STAN_GOOSE_v2.0_012021.RDS')
# harvard_total_agb <- readRDS('sites/HARVARD/runs/v2.0_102020/output/AGB_STAN_HARVARD_v2.0_102020.RDS')
# northround_total_agb <- readRDS('sites/NORTHROUND/runs/v2.0_082020/output/AGB_STAN_NORTHROUND_v2.0_082020.RDS')
# rooster_total_agb <- readRDS('sites/ROOSTER/runs/v2.0_082020/output/AGB_STAN_ROOSTER_v2.0_082020.RDS')
# sylvania_total_agb <- readRDS('sites/SYLVANIA/runs/v2.0_082020/output/AGB_STAN_SYLVANIA_v2.0_082020.RDS')
#
# #above ground biomass increment 
# goose_total_agbi <- readRDS('sites/GOOSE/runs/v2.0_012021/output/AGBI_STAN_GOOSE_v2.0_012021.RDS')
# harvard_total_agbi <- readRDS('sites/HARVARD/runs/v2.0_102020/output/AGBI_STAN_HARVARD_v2.0_102020.RDS')
# northround_total_agbi <- readRDS('sites/NORTHROUND/runs/v2.0_082020/output/AGBI_STAN_NORTHROUND_v2.0_082020.RDS')
# rooster_total_agbi <- readRDS('sites/ROOSTER/runs/v2.0_082020/output/AGBI_STAN_ROOSTER_v2.0_082020.RDS')
# sylvania_total_agbi <- readRDS('sites/SYLVANIA/runs/v2.0_082020/output/AGBI_STAN_SYLVANIA_v2.0_082020.RDS')

#above ground biomass
goose_total_agb <- readRDS('sites/GOOSE/runs/v3.1_012021/output/AGB_TAXA_STAN_GOOSE_v3.1_012021.RDS')
# harvard_total_agb <- readRDS('sites/HARVARD/runs/v2.0_102020/output/AGB_TAXA_STAN_HARVARD_v2.0_102020.RDS')
northround_total_agb <- readRDS('sites/NORTHROUND/runs/v3.1_082020/output/AGB_TAXA_STAN_NORTHROUND_v3.1_082020.RDS')
rooster_total_agb <- readRDS('sites/ROOSTER/runs/v3.1_082020/output/AGB_TAXA_STAN_ROOSTER_v3.1_082020.RDS')
sylvania_total_agb <- readRDS('sites/SYLVANIA/runs/v3.1_082020/output/AGB_TAXA_STAN_SYLVANIA_v3.1_082020.RDS')

#above ground biomass increment 
goose_total_agbi <- readRDS('sites/GOOSE/runs/v3.1_012021/output/AGBI_TAXA_STAN_GOOSE_v3.1_012021.RDS')
# harvard_total_agbi <- readRDS('sites/HARVARD/runs/v2.0_102020/output/AGBI_TAXA_STAN_HARVARD_v2.0_102020.RDS')
northround_total_agbi <- readRDS('sites/NORTHROUND/runs/v3.1_082020/output/AGBI_TAXA_STAN_NORTHROUND_v3.1_082020.RDS')
rooster_total_agbi <- readRDS('sites/ROOSTER/runs/v3.1_082020/output/AGBI_TAXA_STAN_ROOSTER_v3.1_082020.RDS')
sylvania_total_agbi <- readRDS('sites/SYLVANIA/runs/v3.1_082020/output/AGBI_TAXA_STAN_SYLVANIA_v3.1_082020.RDS')


# Combining abgi and abg into one dataframe 
goose_total <- goose_total_agb |>
  left_join(goose_total_agbi, by = c('year', 'iter', 'taxon', 
                                     'model', 'plot')) |>
  rename(AGB = ab,
         AGBI = abi) |>
  # select(-c(type.x, type.y)) |>
  # Add site name for combining sites into one df
  mutate(site = 'GOOSE')

nrp_total <- northround_total_agb |>
  left_join(northround_total_agbi, by = c('year', 'iter', 'taxon', 
                                          'model', 'plot')) |>
  rename(AGB = ab,
         AGBI = abi) |>
  # select(-c(type.x, type.y)) |>
  mutate(site = 'NRP')

rooster_total <- rooster_total_agb |>
  left_join(rooster_total_agbi, by = c('year', 'iter', 'taxon',
                                       'model', 'plot')) |>
  rename(AGB = ab,
         AGBI = abi) |>
  # select(-c(type.x, type.y)) |>
  mutate(site = 'ROOSTER')

sylvania_total <- sylvania_total_agb |>
  left_join(sylvania_total_agbi, by = c('year', 'iter', 'taxon', 
                                        'model', 'plot')) |>
  rename(AGB = ab,
         AGBI = abi) |>
  # select(-c(type.x, type.y)) |>
  mutate(site = 'SYLVANIA')

# harvard_total <- harvard_total_agb |>
#   left_join(harvard_total_agbi, by = c('year', 'iter', 'taxon', 
#                                        'model', 'plot')) |>
#   rename(AGB = ab,
#          AGBI = abi) |>
#   # select(-c(type.x, type.y)) |>
#   mutate(site = 'HARVARD')

#combining data from all sites into one dataframe
# all_data <- rbind(goose_total, nrp_total, rooster_total, sylvania_total, harvard_total)
all_data <- rbind(goose_total, nrp_total, rooster_total, sylvania_total)

goose_total_plot <- goose_total |>
  group_by(year, iter, plot) |>
  summarize(AGB.sum = sum(AGB),
            AGBI.sum = sum(AGBI),
            .groups = 'keep') 
goose_total_plot = goose_total_plot %>%
  group_by(year, plot) %>% 
  summarize(AGB.mean = mean(AGB.sum, na.rm = T),
            AGB.sd = sd(AGB.sum),
            AGB.lo = quantile(AGB.sum, c(0.025), na.rm=TRUE),
            AGB.hi = quantile(AGB.sum, c(0.975), na.rm=TRUE), 
            AGBI.mean = mean(AGBI.sum, na.rm = T),
            AGBI.sd = sd(AGBI.sum),
            AGBI.lo = quantile(AGBI.sum, c(0.025), na.rm=TRUE),
            AGBI.hi = quantile(AGBI.sum, c(0.975), na.rm=TRUE), 
            .groups='keep')

goose_plot_wide = pivot_wider(data = goose_total_plot[,(colnames(goose_total_plot) %in% 
                                                          c('year','AGBI.mean', 'plot'))],
                              id_cols = c(year),
                              names_from = plot, 
                              values_from = AGBI.mean, 
                              values_fill = 0 )

#summarizing the data for both agb and agbi
#here we are taking the mean of all the iterations for one tree in a given year
# all_tree <- all_data |>
#   group_by(tree, year, plot, taxon, model, site) |>
#   # Means across iterations
#   summarize(AGB.mean = mean(AGB),
#             AGBI.mean = mean(AGBI),
#             # Standard deviations across iterations
#             AGB.sd = sd(AGB),
#             AGBI.sd = sd(AGBI),
#             # Credible intervals across iterations
#             AGB.low = quantile(AGB, probs = 0.025, na.rm = T),
#             AGB.high = quantile(AGB, probs = 0.975, na.rm = T),
#             AGBI.low = quantile(AGBI, probs = 0.025, na.rm = T),
#             AGBI.high = quantile(AGBI, probs = 0.975, na.rm = T),
#             .groups = 'keep')

#summarizing for goose
# all_tree <- goose_total |>
#   group_by(tree, year, plot, taxon, model, site) |>
#   # Means across iterations
#   summarize(AGB.mean = mean(AGB),
#             AGBI.mean = mean(AGBI),
#             # Standard deviations across iterations
#             AGB.sd = sd(AGB),
#             AGBI.sd = sd(AGBI),
#             # Credible intervals across iterations
#             AGB.low = quantile(AGB, probs = 0.025, na.rm = T),
#             AGB.high = quantile(AGB, probs = 0.975, na.rm = T),
#             AGBI.low = quantile(AGBI, probs = 0.025, na.rm = T),
#             AGBI.high = quantile(AGBI, probs = 0.975, na.rm = T))



# Create summaries by species
# all_species <- all_data |>
#   group_by(year, iter, taxon, site) |>
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

# ## Create summaries by site
# # I am not sure that we are interested in average agb per tree
# # This is defined as agb_persite: mean total site biomass divided by the number of trees
# all_site <- all_data |>
#   group_by(year, plot, model, site) |>
#   summarize(AGB.mean = mean(AGB),
#             AGBI.mean = mean(AGBI),
#             AGB.sd = sd(AGB),
#             AGBI.sd = sd(AGBI),
#             AGB.low = quantile(AGB, probs = 0.025, na.rm = T),
#             AGB.high = quantile(AGB, probs = 0.975, na.rm = T),
#             AGBI.low = quantile(AGBI, probs = 0.025, na.rm = T),
#             AGBI.high = quantile(AGBI, probs = 0.975, na.rm = T),
#             ntree = n(),
#             agb_persite = AGB.mean / ntree) 

# Create summaries by site
# I am not sure that we are interested in average agb per tree
# This is defined as agb_persite: mean total site biomass divided by the number of trees
all_site_plot_by_iter <- all_data |>
  group_by(year, iter, plot, model, site) |>
  summarize(AGB.sum = sum(AGB),
            AGBI.sum = sum(AGBI),
            .groups = 'keep') 


all_site_plot_summary = all_site_plot_by_iter %>%
  group_by(year, plot, model, site) %>% 
  summarize(AGB.mean = mean(AGB.sum, na.rm = T),
            AGB.sd = sd(AGB.sum),
            AGB.lo = quantile(AGB.sum, c(0.025), na.rm=TRUE),
            AGB.hi = quantile(AGB.sum, c(0.975), na.rm=TRUE), 
            AGBI.mean = mean(AGBI.sum, na.rm = T),
            AGBI.sd = sd(AGBI.sum),
            AGBI.lo = quantile(AGBI.sum, c(0.025), na.rm=TRUE),
            AGBI.hi = quantile(AGBI.sum, c(0.975), na.rm=TRUE), 
            .groups='keep')
head(all_site_plot_summary)

all_site_by_iter <- all_data |>
  group_by(year, iter, model, site) |>
  summarize(AGB.sum = sum(AGB),
            AGBI.sum = sum(AGBI),
            .groups = 'keep') 

all_site_summary = all_site_by_iter %>%
  group_by(year, model, site) %>% 
  summarize(AGB.mean = mean(AGB.sum, na.rm = T),
            AGB.sd = sd(AGB.sum),
            AGB.lo = quantile(AGB.sum, c(0.025), na.rm=TRUE),
            AGB.hi = quantile(AGB.sum, c(0.975), na.rm=TRUE), 
            AGBI.mean = mean(AGBI.sum, na.rm = T),
            AGBI.sd = sd(AGBI.sum),
            AGBI.lo = quantile(AGBI.sum, c(0.025), na.rm=TRUE),
            AGBI.hi = quantile(AGBI.sum, c(0.975), na.rm=TRUE), 
            .groups='keep')
head(all_site_summary)

#taxon_group takes the sum of all the trees in one taxon.
#iterations for each taxon not individual trees 
all_taxon_plot_by_iter <- all_data |>
  group_by(year, iter, taxon, plot, model, site) |>
  summarize(AGB.sum = sum(AGB),
            AGBI.sum = sum(AGBI),
            .groups = 'keep') 


all_taxon_plot_summary = all_taxon_plot_by_iter %>%
  group_by(year, taxon, plot, model, site) %>% 
  summarize(AGB.mean = mean(AGB.sum, na.rm = T),
            AGB.sd = sd(AGB.sum),
            AGB.lo = quantile(AGB.sum, c(0.025), na.rm=TRUE),
            AGB.hi = quantile(AGB.sum, c(0.975), na.rm=TRUE), 
            AGBI.mean = mean(AGBI.sum, na.rm = T),
            AGBI.sd = sd(AGBI.sum),
            AGBI.lo = quantile(AGBI.sum, c(0.025), na.rm=TRUE),
            AGBI.hi = quantile(AGBI.sum, c(0.975), na.rm=TRUE), 
            .groups='keep')
head(all_taxon_plot_summary)

#no plot
all_taxon_by_iter <- all_data |>
  group_by(year, iter, taxon, model, site) |>
  summarize(AGB.sum = sum(AGB),
            AGBI.sum = sum(AGBI),
            .groups = 'keep') 


all_taxon_summary = all_taxon_by_iter %>%
  group_by(year, taxon, model, site) %>% 
  summarize(AGB.mean = mean(AGB.sum, na.rm = T),
            AGB.sd = sd(AGB.sum),
            AGB.lo = quantile(AGB.sum, c(0.025), na.rm=TRUE),
            AGB.hi = quantile(AGB.sum, c(0.975), na.rm=TRUE), 
            AGBI.mean = mean(AGBI.sum, na.rm = T),
            AGBI.sd = sd(AGBI.sum),
            AGBI.lo = quantile(AGBI.sum, c(0.025), na.rm=TRUE),
            AGBI.hi = quantile(AGBI.sum, c(0.975), na.rm=TRUE), 
            .groups='keep')
head(all_taxon_summary)


ggplot(data=all_site_summary) +
  geom_ribbon(aes(x=year, ymin=AGBI.lo, ymax=AGBI.hi, colour=site, fill=site)) +
  geom_line(aes(x=year, y=AGBI.mean, colour=site)) +
  theme_bw(14) +
  xlab('Year') +
  ylab('AGBI (Mg/ha)')

ggplot(data=all_taxon_summary) +
  geom_ribbon(aes(x=year, ymin=AGBI.lo, ymax=AGBI.hi, colour=taxon, fill=taxon)) +
  geom_line(aes(x=year, y=AGBI.mean, colour=taxon)) +
  theme_bw(14) +
  xlab('Year') +
  ylab('AGBI (Mg/ha)') +
  facet_wrap(~site)

#######################3
###this is where you stopped for full data
ggplot(data=all_taxon_summary) +
  geom_ribbon(aes(x=year, ymin=AGBI.mean-2*AGBI.sd,
                  ymax=AGBI.mean+2*AGBI.sd, color = taxon, fill = taxon), alpha=0.3) +
  geom_line(aes(x=year, y=AGBI.mean, color = taxon)) +
  facet_wrap(~site)

#average biomass that increases for the whole time period
# taxon_3 = taxon_summary %>% 
#   group_by(taxon) %>% 
#   summarize(AGBI.mean2 = mean(AGBI.mean),
#             AGBI.sd = sd(AGBI.mean, na.rm = T))
  
#changing to wide format with taxons as column names and values = ABGI.mean 
#values_fill=0 does not work 
all_taxon_summary_wide = pivot_wider(data = all_taxon_summary[,(colnames(all_taxon_summary) %in% 
                                                                  c('year', 'taxon', 'AGBI.mean', 'site'))],
                         id_cols = c(year, site),
                         names_from = taxon, 
                         values_from = AGBI.mean, 
                         values_fill = 0 )

all_site_summary_wide = pivot_wider(data = all_site_summary[,(colnames(all_site_summary) %in% 
                                                                c('year','AGBI.mean', 'site'))],
                                    id_cols = c(year),
                                    names_from = site, 
                                    values_from = AGBI.mean, 
                                    values_fill = 0 )

## NOW WE WANT TO DO BY SITE, SO NEED TO REWORK THIS
#########################################################################################
#CORRELATION
################################################################################################


#correlation matrix between all species for GOOSE for all trees found in one SITE for each given year....
cor(all_taxon_summary_wide$ACSA, all_taxon_summary_wide$BEPA, use = "complete.obs")
correlation_A = data.frame(cor(taxon_wide[, c('ACRU','ACSA','BEPA', 'FAGR', 'PIST', 'QUAL', 'QUMO', 'QURU', 'BELE',
                   'OSVI', 'AMAR')],use = "complete.obs"))
#find significance 
correlation_B = correlation_A
correlation_B[correlation_B<0.4] = NA

#correlation figure between species for ABGI.mean
#shitty colours!!!
ggcorrplot(correlation_A, method = "circle", type = "lower", hc.order = FALSE)
ggcorrplot(correlation_B, type = "lower",)
#  scale_fill_gradient2(low = "yellow", mid = 'pink', high = "light green", breaks=c(0, 1), limit=c(0, 1))


#correlation between sites 
cor_site = data.frame(cor(all_site_summary_wide[, c('GOOSE', 'ROOSTER', 'SYLVANIA', 'NRP')], use = "complete.obs"))
ggcorrplot(cor_site, method = "circle", type = "lower", hc.order = FALSE)


cor_plot_goose = data.frame(cor(goose_plot_wide[,c('1', '2', '3')], use = "complete.obs"))
ggcorrplot(cor_plot_goose, method = "square", type = "lower")

#########################################################################################
#CLIMATE
################################################################################################

load('climate/prism_clim.RData')
clim_data = prism_long
clim_data = clim_data %>% 
  rename(site = loc, PPT = PPT2, Tmean = Tmean2)

clim_wide =  pivot_wider(data = clim_data,
                         names_from = month, 
                         values_from = c(PPT, Tmean, Tmin2, Tmax2, Vpdmin2, Vpdmax2))


#PPT_mean is the mean of each month summed to get the mean of the year
#yearly_meanT is the yearly temperature mean based on monthly means

#summarizing climate data, total precipitation, yearly mean temp, 
clim_summary = clim_wide |>
  mutate(PPT_total = rowSums(dplyr::select(clim_wide, starts_with('PPT'))),
         PPT_total_prev_tree = rowSums(dplyr::pick('PPT_09', 'PPT_10', 'PPT_11', 'PPT_12')),
         PPT_total_current_tree = rowSums(dplyr::pick('PPT_01', 'PPT_02', 'PPT_03', 'PPT_04', 
                                              'PPT_05', 'PPT_06', 'PPT_07', 'PPT_08')),
         yearly_meanT = rowMeans(dplyr::select(clim_wide, starts_with('Tmean'))),
         T_min_mean = rowMeans(dplyr::select(clim_wide, starts_with('Tmin'))),
          T_max_mean = rowMeans(dplyr::select(clim_wide, starts_with('Tmax'))),
         )

Vpd_sets = list(c("Vpdmin2_01", "Vpdmax2_01"), c("Vpdmin2_02", "Vpdmax2_02"),
                c("Vpdmin2_03", "Vpdmax2_03"), c("Vpdmin2_04", "Vpdmax2_04"),
                c("Vpdmin2_05", "Vpdmax2_05"), c("Vpdmin2_06", "Vpdmax2_06"),
                c("Vpdmin2_07", "Vpdmax2_07"), c("Vpdmin2_08", "Vpdmax2_08"),
                c("Vpdmin2_09", "Vpdmax2_09"), c("Vpdmin2_10", "Vpdmax2_10"),
                c("Vpdmin2_11", "Vpdmax2_11"), c("Vpdmin2_12", "Vpdmax2_12")
)
#taking the mean of Vpd for each month 
for (i in seq_along(Vpd_sets)) {
  set <- Vpd_sets[[i]]
  clim_summary <- clim_summary %>%
    mutate(!!paste0("Vpdmean_", i) := rowMeans(select(., all_of(set))))

}

N_years = nrow(clim_summary)

clim_summary$PPT_total_tree = NA
clim_summary$PPT_total_tree[2:N_years] = clim_summary$PPT_total_prev_tree[1:(N_years-1)] +
  clim_summary$PPT_total_current_tree[2:N_years]


clim_summary$year <- as.numeric(clim_summary$year)

clim_agb <- all_site_summary|>
  left_join(clim_summary, by = c('year', 'site'))

clim_agb = subset(clim_agb, year>1950)

vpd = select(clim_agb, year, AGBI.mean, site,
                  starts_with('Vpdmean'))
tmin = select(clim_agb, year, AGBI.mean, site,
                  starts_with('Tmin'))
tmax = select(clim_agb, year, AGBI.mean, site,
                  starts_with('Tmax'))
ppt = select(clim_agb, year, AGBI.mean, site,
             starts_with('PPT'))


vpd_melt = melt(vpd, 
                  id.vars = c('model', 'year', 'AGBI.mean', 'site'))

# vpd_melt$variable = sapply(as.vector(vpd_melt$variable), function(x) {strsplit(x, '\\_')[[1]][2]})

tmin_melt = melt(tmin,
                     id.vars = c('model', 'year', 'AGBI.mean', 'site'))
tmax_melt = melt(tmax, 
                     id.vars = c('model', 'year', 'AGBI.mean', 'site'))

ppt_melt = melt(ppt, id.vars = c('model', 'year', 'AGBI.mean', 'site'))


#without taxon
ggplot(data = clim_agb) +
  geom_point(aes(x = yearly_meanT, y = AGBI.mean)) +
  geom_smooth(aes(x = yearly_meanT, y = AGBI.mean), method='lm', formula= y~x)+
  # facet_wrap(~site, scales = 'free')+
  xlab('Mean annual temperature') + 
  ylab('Aboveground biomass increment')

ggplot(data = clim_agb) +
  geom_point(aes(x = yearly_meanT, y = AGBI.mean)) +
  geom_smooth(aes(x = yearly_meanT, y = AGBI.mean), method='lm', formula= y~x)+
  facet_wrap(~site, scales = 'free')+
  xlab('Mean annual temperature') + 
  ylab('Aboveground biomass increment')
#ggsave("AGBI_temp.png")

ggplot(data = clim_agb) +
  geom_point(aes(x = PPT_total, y = AGBI.mean)) +
  geom_smooth(aes(x = PPT_total, y = AGBI.mean), method='lm', formula= y~x)+
  xlab('Mean annual precipitation') + ylab('Aboveground biomass increment')
# ggsave("AGBI_precip.png")

ggplot(data = clim_agb) +
  geom_point(aes(x = PPT_total, y = AGBI.mean)) +
  geom_smooth(aes(x = PPT_total, y = AGBI.mean), method='lm', formula= y~x)+
  facet_wrap(~site, scales = "free")+
  xlab('Mean annual precipitation') + 
  ylab('Aboveground biomass increment')
ggsave("AGBI_precip.png")


# by month
ggplot(data = clim_agb) +
  geom_point(aes(x = PPT_total_tree, y = AGBI.mean)) +
  geom_smooth(aes(x = PPT_total_tree, y = AGBI.mean), method='lm', formula= y~x)+
  xlab('VPD') + 
  ylab('Aboveground biomass increment')
ggsave("AGBI_precip_tree.png")

ggplot(data = clim_agb) +
  geom_point(aes(x = PPT_total_tree, y = AGBI.mean)) +
  geom_smooth(aes(x = PPT_total_tree, y = AGBI.mean), method='lm', formula= y~x) +  
  facet_wrap(~site, scales = "free") +
  xlab('VPD') + 
  ylab('Aboveground biomass increment')
# ggsave("AGBI_precip_tree.png")


ggplot(data = vpd_melt) +
  geom_point(aes(x = value, y = AGBI.mean, colour=site)) +
  geom_smooth(aes(x = value, y = AGBI.mean), method='lm', formula= y~x) +  
  facet_wrap(~variable, scales = "free") +
  xlab('VPD') + 
  ylab('Aboveground biomass increment')

ggplot(data = vpd_melt) +
  geom_point(aes(x = value, y = AGBI.mean, colour=site)) +
  geom_smooth(aes(x = value, y = AGBI.mean, colour=site), method='lm', formula= y~x) +  
  facet_wrap(~variable, scales = "free") +
  xlab('VPD') + 
  ylab('Aboveground biomass increment')

ggplot(data = vpd_melt) +
  geom_point(aes(x = value, y = AGBI.mean, colour=site)) +
  geom_smooth(aes(x = value, y = AGBI.mean), method='lm', formula= y~x) +  
  facet_grid(variable~site, scales = "free") +
  xlab('VPD') + 
  ylab('Aboveground biomass increment')


# by month
ggplot(data = tmin_melt) +
  geom_point(aes(x = value, y = AGBI.mean, colour=site)) +
  geom_smooth(aes(x = value, y = AGBI.mean), method='lm', formula= y~x) +  
  facet_wrap(~variable, scales = "free") +
  xlab('VPD') + 
  ylab('Aboveground biomass increment')

ggplot(data = tmin_melt) +
  geom_point(aes(x = value, y = AGBI.mean, colour=site)) +
  geom_smooth(aes(x = value, y = AGBI.mean, colour=site), method='lm', formula= y~x) +  
  facet_wrap(~variable, scales = "free") +
  xlab('VPD') + 
  ylab('Aboveground biomass increment')

ggplot(data = vpd_melt) +
  geom_point(aes(x = value, y = AGBI.mean, colour=site)) +
  geom_smooth(aes(x = value, y = AGBI.mean), method='lm', formula= y~x) +  
  facet_grid(variable~site, scales = "free") +
  xlab('VPD') + 
  ylab('Aboveground biomass increment')

# ggsave("AGBI_precip_tree.png")


#with taxon



# clim_agb <- all_site_summary|>
#   left_join(clim_summary, by = c('year', 'site'))
# 
# clim_agb = subset(clim_agb, year>1950)
# 
# vpd = select(clim_agb, year, AGBI.mean, taxon, site,
#              starts_with('Vpdmean'))
# tmin = select(clim_agb, year, AGBI.mean, taxon, site,
#               starts_with('Tmin'))
# tmax = select(clim_agb, year, AGBI.mean, taxon, site,
#               starts_with('Tmax'))
# ppt = select(clim_agb, year, AGBI.mean, taxon, site,
#              starts_with('PPT'))
# vpd_melt = melt(vpd, 
#                 id.vars = c('year', 'AGBI.mean', 'taxon', 'site'))
# temp_min_melt = melt(temp_min,
#                      id.vars = c('year', 'AGBI.mean', 'taxon', 'site'))
# temp_max_melt = melt(temp_max, 
#                      id.vars = c('year', 'AGBI.mean', 'taxon', 'site'))
# 
# PPT_melt = melt(PPT, id.vars = c('year', 'AGBI.mean', 'taxon', 'site'))
ggplot(data = climate_increment) +
  geom_point(aes(x = yearly_meanT, y = AGBI.mean)) +
  geom_linerange(aes(x=yearly_meanT, ymin=AGBI.lo, ymax=AGBI.hi)) +
  facet_wrap(~taxon, scales='free_y')+
  geom_smooth(aes(x = yearly_meanT, y = AGBI.mean), method='lm', formula= y~x )+
  xlab('Average annual temperature') + ylab('Aboveground biomass increment') 
ggsave("ABGI_taxon_temp.png")

# ggplot(data = clim_taxon) +
#   geom_point(aes(x = yearly_meanT, y = AGBI.mean)) +
#   geom_linerange(aes(x=yearly_meanT, ymin=AGBI.lo, ymax=AGBI.hi)) +
#   facet_wrap(~taxon, scales='free_y')+
#   geom_smooth(aes(x = yearly_meanT, y = AGBI.mean), method='lm', formula= y~x )+
#   xlab('Average annual temperature') + ylab('Aboveground biomass increment') 
# ggsave("ABGI_taxon_temp.png")

ggplot(data = clim_taxon) +
  geom_point(aes(x = PPT_total, y = AGBI.mean)) +
  geom_linerange(aes(x=PPT_total, ymin=AGBI.lo, ymax=AGBI.hi)) +
  facet_wrap(~taxon, scales = 'free_y')+
  geom_smooth(aes(x = PPT_total, y = AGBI.mean), method='lm', formula= y~x )+
  xlab('Mean annual precipitation') + ylab('Aboveground biomass increment')
ggsave("AGBI_taxon_precip.png")

ggplot(data = clim_taxon) +
  geom_point(aes(x = PPT_total_tree, y = AGBI.mean)) +
  geom_linerange(aes(x=PPT_total_tree, ymin=AGBI.lo, ymax=AGBI.hi)) +
  facet_wrap(~taxon, scales = 'free_y')+
  geom_smooth(aes(x = PPT_total_tree, y = AGBI.mean), method='lm', formula= y~x )+
  xlab('Mean Total Tree Precip') + ylab('Aboveground biomass increment')
ggsave("AGBI_taxon_precip.png")

ggplot(data = clim_taxon) +
  geom_point(aes(x = year, y = AGBI.mean))+
  facet_wrap(~taxon, scales = 'free_y')
ggsave("AGBI_time_taxon.png") 
 



ggplot(data = climate_increment) +
  geom_point(aes(x = year, y = AGBI.mean))
ggsave("AGBI_time.png")  
  
  
ggplot(data = climate_increment) +
  geom_point(aes(x = T_min_mean, y = AGBI.mean))#+
  # geom_point(aes(x = T_max_mean, y = AGBI.mean))

ggplot(data = climate_increment) +
  geom_point(aes(x = T_max_mean, y = AGBI.mean))#+
# geom_point(aes(x = T_max_mean, y = AGBI.mean))



#unsure what this tells us
ggplot(data = Vapor_melt) +
  geom_point(aes(x = value, y = AGBI.mean, color = variable))

ggplot(data = Vapor_melt) +
  geom_point(aes(x = value, y = AGBI.mean, color=taxon)) +
  facet_wrap(~variable, scales='free_x') +
  geom_smooth(aes(x = value, y = AGBI.mean, color=taxon), method='lm', formula= y~x )
  

ggplot(data = climate_increment)+
  geom_point(aes(x = PPT_total_prev_tree, y = AGBI.mean))

ggplot(data = climate_increment)+
  geom_point(aes(x = PPT_03, y = AGBI.mean))

ggplot(data = PPT_melt)+
  geom_point(aes(x = value, y = AGBI.mean, color = variable))

ggplot(data = PPT_melt)+
  geom_point(aes(x = value, y = AGBI.mean, color = taxon)) +
  facet_wrap(~variable, scales='free_x') +
  geom_smooth(aes(x = value, y = AGBI.mean, color=taxon), method='lm', formula= y~x )
#+
 # facet_wrap(~)

ggplot(data = clim_taxon)+
  geom_point(aes(x = PPT_03, y = AGBI.mean, color = taxon)) +
  geom_smooth(aes(x = PPT_03, y = AGBI.mean, color=taxon), method='lm', formula= y~x )


ggplot(data = clim_taxon)+
  geom_point(aes(x = PPT_total, y = AGBI.mean, color = taxon)) +
  geom_smooth(aes(x = PPT_total, y = AGBI.mean, color=taxon), method='lm', formula= y~x )

#################################################################################
#STATS
#################################################################################


PPT_lm = clim_taxon %>% 
  group_by(taxon) %>%
  do(tidy(lm(AGBI.mean ~ PPT_total_tree, .)))

PPT_lm_slope = subset(PPT_lm, term == 'PPT_total_tree')
PPT_lm_slope$sig = ifelse(PPT_lm_slope$p.value < 0.05, TRUE, FALSE)
ggplot(data=PPT_lm_slope) +
  geom_point(aes(x=estimate, y=taxon, colour=sig))

Vpd_lm = Vapor_melt %>% 
  group_by(taxon, variable) %>%
  do(tidy(lm(AGBI.mean ~ value, .)))

VPD_lm_slope = subset(Vpd_lm, term == 'value')
VPD_lm_slope$sig = ifelse(VPD_lm_slope$p.value < 0.05, TRUE, FALSE)
ggplot(data=VPD_lm_slope) +
  geom_point(aes(x=estimate, y=taxon, colour=sig)) +
  facet_wrap(~variable)


Tmin_lm = temp_min_melt %>% 
  group_by(taxon, variable) %>%
  do(tidy(lm(AGBI.mean ~ value, .)))

Tmin_lm_slope = subset(Tmin_lm, term == 'value')
Tmin_lm_slope$sig = ifelse(Tmin_lm_slope$p.value < 0.05, TRUE, FALSE)
ggplot(data=Tmin_lm_slope) +
  geom_point(aes(x=estimate, y=taxon, colour=sig)) +
  facet_wrap(~variable)

Tmax_lm = temp_max_melt %>% 
  group_by(taxon, variable) %>%
  do(tidy(lm(AGBI.mean ~ value, .)))



#################################################################################
# Individual tree 
#################################################################################

# Combining abgi and abg into one dataframe
goose_total <- goose_total_agb |>
  left_join(goose_total_agbi, by = c('tree', 'year', 'iter',
                                     'taxon', 'model', 'plot')) |>
  rename(AGB = value.x,
         AGBI = value.y) |>
  select(-c(type.x, type.y)) |>
  # Add site name for combining sites into one df
  mutate(site = 'GOOSE')

#summarizing the data for both agb and agbi
#here we are taking the mean of all the iterations for one tree in a given year
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


clim_tree = all_tree %>%
  left_join(clim_summary,by = c('year') )
clim_tree = subset(clim_tree, year>1950)

tree_indiv =  goose_total |>
  group_by(tree, year) |>
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

tree_indiv_clim = tree_indiv %>% 
  left_join(clim_summary, by = c('year'))
tree_indiv_clim = subset(tree_indiv_clim, year>1950)


ggplot(data = tree_indiv_clim)+
  geom_point(aes(x = tree, y = AGB.mean, color = year))

ggplot(data = tree_indiv_clim)+
  geom_point(aes(x = PPT_total_tree, y = AGB.mean))

tree_lm = clim_tree %>% 
  group_by(tree, taxon) %>%
  do(tidy(lm(AGBI.mean ~ PPT_total_tree, .)))

tree_lm_slope = subset(tree_lm, term == 'PPT_total_tree')
tree_lm_slope$sig = ifelse(tree_lm_slope$p.value < 0.05, TRUE, FALSE)
ggplot(data=tree_lm_slope) +
  geom_histogram(aes(x=estimate))
summary(tree_lm_slope$estimate)
sum(tree_lm_slope$sig)

ggplot(data=tree_lm_slope) +
  geom_point(aes(x=estimate, y=taxon, colour=sig)) 

plot(clim_tree$AGB.mean, clim_tree$AGBI.mean)

taxon_lm = clim_tree %>% 
  group_by(taxon) %>%
  do(tidy(lm(AGBI.mean ~ PPT_total_tree + AGB.mean, .)))
