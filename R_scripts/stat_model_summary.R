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
library(ggthemes)
library(reshape2)
library(broom)
library(gam)
library(correlation)
#library(RColorBrewer)
library(stringr)
library(corrplot)
library(purrr)
library(Hmisc)


#above ground biomass
goose_total_agb <- readRDS('sites/GOOSE/runs/v3.1_012021/output/AGB_TAXA_STAN_GOOSE_v3.1_012021.RDS')
goose_total_agb_subset = subset(goose_total_agb, year > 1949 & year < 2012)
# harvard_total_agb <- readRDS('sites/HARVARD/runs/v2.0_102020/output/AGB_TAXA_STAN_HARVARD_v2.0_102020.RDS')
northround_total_agb <- readRDS('sites/NORTHROUND/runs/v3.1_082020/output/AGB_TAXA_STAN_NORTHROUND_v3.1_082020.RDS')
northround_total_agb_subset = subset(northround_total_agb, year > 1949 & year < 2012)
rooster_total_agb <- readRDS('sites/ROOSTER/runs/v3.1_082020/output/AGB_TAXA_STAN_ROOSTER_v3.1_082020.RDS')
rooster_total_agb_subset = subset(rooster_total_agb,year > 1949 & year < 2012)
sylvania_total_agb <- readRDS('sites/SYLVANIA/runs/v3.1_082020/output/AGB_TAXA_STAN_SYLVANIA_v3.1_082020.RDS')
sylvania_total_agb_subset = subset(sylvania_total_agb, year > 1949 & year < 2012)
harvard_total_agb <- readRDS('sites/HARVARD/runs/v3.1_102020/output/AGB_TAXA_STAN_HARVARD_v3.1_102020.RDS')
harvard_total_agb_subset = subset(harvard_total_agb, year > 1949 & year < 2012)
hmc_total_agb <- readRDS('sites/HMC/runs/v3.1_082020/output/AGB_TAXA_STAN_HMC_v3.1_082020.RDS')
hmc_total_agb$year = hmc_total_agb$year + 1900 - 1
hmc_total_agb_subset = subset(hmc_total_agb, year > 1949 & year < 2012)

#above ground biomass increment 
goose_total_agbi <- readRDS('sites/GOOSE/runs/v3.1_012021/output/AGBI_TAXA_STAN_GOOSE_v3.1_012021.RDS')
goose_total_agbi$year = goose_total_agbi$year + 1
goose_total_agbi_subset = subset(goose_total_agbi, year > 1949 & year < 2012)


# harvard_total_agbi <- readRDS('sites/HARVARD/runs/v2.0_102020/output/AGBI_TAXA_STAN_HARVARD_v2.0_102020.RDS')
northround_total_agbi <- readRDS('sites/NORTHROUND/runs/v3.1_082020/output/AGBI_TAXA_STAN_NORTHROUND_v3.1_082020.RDS')
northround_total_agbi$year = northround_total_agbi$year + 1
northround_total_agbi_subset = subset(northround_total_agbi,year > 1949 & year < 2012)

rooster_total_agbi <- readRDS('sites/ROOSTER/runs/v3.1_082020/output/AGBI_TAXA_STAN_ROOSTER_v3.1_082020.RDS')
rooster_total_agbi$year = rooster_total_agbi$year + 1
rooster_total_agbi_subset = subset(rooster_total_agbi, year > 1949 & year < 2012)

sylvania_total_agbi <- readRDS('sites/SYLVANIA/runs/v3.1_082020/output/AGBI_TAXA_STAN_SYLVANIA_v3.1_082020.RDS')
sylvania_total_agbi$year = sylvania_total_agbi$year + 1
sylvania_total_agbi_subset = subset(sylvania_total_agbi, year > 1949 & year < 2012)

harvard_total_agbi <- readRDS('sites/HARVARD/runs/v3.1_102020/output/AGBI_TAXA_STAN_HARVARD_v3.1_102020.RDS')
harvard_total_agbi$year = harvard_total_agbi$year + 1
harvard_total_agbi_subset = subset(harvard_total_agbi, year > 1949 & year < 2012)

hmc_total_agbi <- readRDS('sites/HMC/runs/v3.1_082020/output/AGBI_TAXA_STAN_HMC_v3.1_082020.RDS')
hmc_total_agbi$year = hmc_total_agbi$year + 1900 - 1
hmc_total_agbi$year = hmc_total_agbi$year + 1
hmc_total_agbi_subset = subset(hmc_total_agbi, year > 1949 & year < 2012)


# Combining abgi and abg into one dataframe 
goose_total <- goose_total_agb_subset |>
  left_join(goose_total_agbi_subset, by = c('year', 'iter', 'taxon', 
                                     'model', 'plot')) |>
  dplyr::rename(AGB = ab,
         AGBI = abi) |>
  # select(-c(type.x, type.y)) |>
  # Add site name for combining sites into one df
  mutate(site = 'GOOSE')

nrp_total <- northround_total_agb_subset |>
  left_join(northround_total_agbi_subset, by = c('year', 'iter', 'taxon', 
                                          'model', 'plot')) |>
  dplyr::rename(AGB = ab,
         AGBI = abi) |>
  # select(-c(type.x, type.y)) |>
  mutate(site = 'NRP')

rooster_total <- rooster_total_agb_subset |>
  left_join(rooster_total_agbi_subset, by = c('year', 'iter', 'taxon',
                                       'model', 'plot')) |>
  dplyr::rename(AGB = ab,
         AGBI = abi) |>
  # select(-c(type.x, type.y)) |>
  mutate(site = 'ROOSTER')


sylvania_total <- sylvania_total_agb_subset |>
  left_join(sylvania_total_agbi_subset, by = c('year', 'iter', 'taxon', 
                                        'model', 'plot')) |>
  dplyr::rename(AGB = ab,
         AGBI = abi) |>
  # select(-c(type.x, type.y)) |>
  mutate(site = 'SYLVANIA')

harvard_total <- harvard_total_agb_subset |>
  left_join(harvard_total_agbi_subset, by = c('year', 'iter', 'taxon',
                                       'model', 'plot')) |>
  dplyr::rename(AGB = ab,
         AGBI = abi) |>
  # select(-c(type.x, type.y)) |>
  mutate(site = 'HARVARD')

harvard_total <- subset(harvard_total, model == 'Model RW')

hmc_total <- hmc_total_agb_subset |>
  left_join(hmc_total_agbi_subset, by = c('year', 'iter', 'taxon',
                                       'model', 'plot')) |>
  dplyr::rename(AGB = ab,
                AGBI = abi) |>
  # select(-c(type.x, type.y)) |>
  mutate(site = 'HMC')
hmc_total$plot = as.numeric(hmc_total$plot)

harvard_total <- subset(harvard_total, model == 'Model RW')
hmc_total <- subset(hmc_total, model == 'Model RW')


#combining data from all sites into one dataframe
all_data <- rbind(goose_total, nrp_total, rooster_total, sylvania_total, harvard_total, hmc_total)
# all_data <- rbind(goose_total, nrp_total, rooster_total, sylvania_total)


all_data$iter_new = all_data$iter + (all_data$plot - 1) * 1000

ggplot(data=all_data) + geom_histogram(aes(x=AGB)) + facet_wrap(~site)

ggplot(data=all_data) + geom_histogram(aes(x=AGBI)) + facet_wrap(~site)


# sum AGB and AGBI across species for a given site, plot, year, and iter
all_site_by_iter <- all_data |>
  group_by(year, iter_new, model, site) |>
  dplyr::summarize(AGB.sum = sum(AGB, na.rm=TRUE),
                   AGBI.sum = sum(AGBI, na.rm = TRUE),
                   .groups = 'keep')

ggplot(data=all_site_by_iter) + geom_histogram(aes(x=AGB.sum)) + facet_wrap(~site)

# #plot summary data
# all_site_plot_summary = all_site_plot_by_iter %>%
#   group_by(year, plot, model, site) %>%
#   dplyr::summarize(AGB.mean = mean(AGB.sum, na.rm = T),
#             AGB.sd = sd(AGB.sum),
#             AGB.lo = quantile(AGB.sum, c(0.025), na.rm=TRUE),
#             AGB.hi = quantile(AGB.sum, c(0.975), na.rm=TRUE),
#             AGBI.mean = mean(AGBI.sum, na.rm = T),
#             AGBI.sd = sd(AGBI.sum),
#             AGBI.lo = quantile(AGBI.sum, c(0.025), na.rm=TRUE),
#             AGBI.hi = quantile(AGBI.sum, c(0.975), na.rm=TRUE),
#             .groups='keep')
# head(all_site_plot_summary)
# 
# # 
# # ggplot(data=all_site_plot_summary) + geom_histogram(aes(x=AGB.mean)) + facet_wrap(~site)
# 
# # take mean AGB and AGBI across plots for a given site, year, and iter
# # all_site_by_iter <- all_site_plot_by_iter |>
# #   group_by(year, iter, model, site) |>
# #   dplyr::summarize(AGB.iter = mean(AGB.sum, na.rm=TRUE),
# #                    AGBI.iter = mean(AGBI.sum, na.rm = TRUE),
# #                    .groups = 'keep') 
# 
# # ggplot(data=all_site_by_iter) + geom_histogram(aes(x=AGB.iter)) + facet_wrap(~site)
# 
# all_site_by_iter <- all_data |>
#   group_by(year, iter, model, site) |>
#   dplyr::summarize(AGB.sum = sum(AGB),
#             AGBI.sum = sum(AGBI),
#             .groups = 'keep')
# ggplot(data=all_site_by_iter) + geom_histogram(aes(x=AGB.sum)) + facet_wrap(~site)


# summarize mean AGB and AGBI across plots for a given site and year
all_site_summary = all_site_by_iter %>%
  group_by(year, model, site) %>%
  dplyr::summarize(AGB.mean = mean(AGB.sum, na.rm = TRUE),
                   AGB.mid = median(AGB.sum, na.rm = TRUE),
                   AGB.sd = sd(AGB.sum, na.rm = TRUE),
                   AGB.lo = quantile(AGB.sum, c(0.025), na.rm=TRUE),
                   AGB.hi = quantile(AGB.sum, c(0.975), na.rm=TRUE),
                   AGBI.mean = mean(AGBI.sum, na.rm = TRUE),
                   AGBI.mid = median(AGBI.sum, na.rm = TRUE),
                   AGBI.sd = sd(AGBI.sum, na.rm = TRUE),
                   AGBI.lo = quantile(AGBI.sum, c(0.025), na.rm=TRUE),
                   AGBI.hi = quantile(AGBI.sum, c(0.975), na.rm=TRUE),
                   .groups='keep')
head(all_site_summary)
ggplot(data=all_site_summary) + geom_histogram(aes(x=AGB.mean)) + facet_wrap(~site)
# saveRDS(all_site_summary, "AGBI_site_data.RDS")
saveRDS(all_site_summary, "reboot/AGBI_site_data.RDS")
# 
# 
# all_site_summary$period = NA
# all_site_summary$period[which(all_site_summary$year<1960)] = "past"
# all_site_summary$period[which(all_site_summary$year>2000)] = "present"
# 
# 
# #taxon_group takes the sum of all the trees in one taxon.
#iterations for each taxon not individual trees 
# all_taxon_plot_by_iter <- all_data |>
#   group_by(year, iter, taxon, plot, model, site) |>
#   dplyr::summarize(AGB.sum = sum(AGB, na.rm=TRUE),
#             AGBI.sum = sum(AGBI, na.rm = TRUE),
#             .groups = 'keep') 
# #taxon plot summary with plot data
# all_taxon_plot_summary = all_taxon_plot_by_iter %>%
#   group_by(year, taxon, plot, model, site) %>% 
#   dplyr::summarize(AGB.mean = mean(AGB.sum, na.rm = T),
#             AGB.sd = sd(AGB.sum),
#             AGB.lo = quantile(AGB.sum, c(0.025), na.rm=TRUE),
#             AGB.hi = quantile(AGB.sum, c(0.975), na.rm=TRUE), 
#             AGBI.mean = mean(AGBI.sum, na.rm = T),
#             AGBI.sd = sd(AGBI.sum),
#             AGBI.lo = quantile(AGBI.sum, c(0.025), na.rm=TRUE),
#             AGBI.hi = quantile(AGBI.sum, c(0.975), na.rm=TRUE), 
#             .groups='keep')
# head(all_taxon_plot_summary)

#taxon summary data across plots
# all_taxon_site_by_iter = all_data %>%
#   group_by(year, iter_new, taxon, model, site) %>% 
#   dplyr::summarize(AGB.iter.mean = mean(AGB, na.rm = TRUE),
#                    AGBI.iter.mean = mean(AGBI, na.rm = TRUE),
#                    .groups='keep')
# head(all_taxon_site_by_iter)



# all_site_by_iter <- all_site_plot_by_iter |>
#   group_by(year, iter, model, site) |>
#   dplyr::summarize(AGB.iter = mean(AGB.sum, na.rm=TRUE),
#                    AGBI.iter = mean(AGBI.sum, na.rm = TRUE),
#                    .groups = 'keep') 

#taxon summary data without plot 
all_taxon_site_summary = all_data %>%
  group_by(year, taxon, model, site) %>% 
  dplyr::summarize(AGB.mean = mean(AGB, na.rm = TRUE),
                   AGB.mid = median(AGB, na.rm = TRUE),
                   AGB.sd = sd(AGB, na.rm=TRUE),
                   AGB.lo = quantile(AGB, c(0.025), na.rm=TRUE),
                   AGB.hi = quantile(AGB, c(0.975), na.rm=TRUE), 
                   AGBI.mean = mean(AGBI, na.rm = TRUE),
                   AGBI.mid = median(AGBI, na.rm = TRUE),
                   AGBI.sd = sd(AGBI, na.rm = TRUE),
                   AGBI.lo = quantile(AGBI, c(0.025), na.rm=TRUE),
                   AGBI.hi = quantile(AGBI, c(0.975), na.rm=TRUE), 
                   .groups='keep')
head(all_taxon_site_summary)

saveRDS(all_taxon_site_summary, "reboot/AGBI_taxon_data.RDS")

###
AGBI_taxon = readRDS("reboot/AGBI_taxon_data.RDS")

head(AGBI_taxon)

AGBI_taxon_site = AGBI_taxon %>% group_by(year, model, site) %>% 
  dplyr::summarize(AGB.mid = sum(AGB.mid, na.rm=TRUE), AGBI.mid=sum(AGBI.mid, na.rm=TRUE))

saveRDS(AGBI_taxon_site, "reboot/AGBI_taxon_site_data.RDS")


AGBI_taxon$period = NA
AGBI_taxon$period[which(AGBI_taxon$year<1960)] = "past"
AGBI_taxon$period[which(AGBI_taxon$year>2000)] = "present"


foo = AGBI_taxon %>% group_by(site) %>% filter(year==max(year)) %>% group_by(site) %>% top_n(3, AGBI.mid)
taxa_top_3 = unique(foo$taxon)



AGBI_taxon_top_5_present = AGBI_taxon %>% group_by(site) %>% filter(year==max(year)) %>% group_by(site) %>% top_n(5, AGBI.mid)
# taxa_top_3 = unique(foo$taxon)


AGBI_taxon_site$period = NA
AGBI_taxon_site$period[which(AGBI_taxon_site$year<1960)] = "past"
AGBI_taxon_site$period[which(AGBI_taxon_site$year>2000)] = "present"

AGBI_site =  readRDS("reboot/AGBI_site_data.RDS")
AGBI_site$period = NA
AGBI_site$period[which(AGBI_site$year<1960)] = "past"
AGBI_site$period[which(AGBI_site$year>2000)] = "present"

#AGBI over time by taxa
ggplot()+
  geom_line(data = AGBI_taxon, aes(x = year, y = AGBI.mean, color = taxon))+
  facet_wrap(~site)+
  theme_light(14) 


# all_taxon_by_iter <- all_taxon_plot_by_iter |>
#   group_by(year, iter, taxon, model, site) |>
#   dplyr::summarize(AGB.iter = mean(AGB.sum, na.rm=TRUE),
#                    AGBI.iter = mean(AGBI.sum, na.rm=TRUE),
#                    .groups = 'keep') 
# 
# #taxon plot summary with plot data
# #AGBI.mean for each taxa at each site 
# all_taxon_summary = all_taxon_by_iter %>%
#   group_by(year, taxon, model, site) %>% 
#   dplyr::summarize(AGB.mean = mean(AGB.iter, na.rm = TRUE),
#             AGB.sd = sd(AGB.iter, na.rm=TRUE),
#             AGB.lo = quantile(AGB.iter, c(0.025), na.rm=TRUE),
#             AGB.hi = quantile(AGB.iter, c(0.975), na.rm=TRUE), 
#             AGBI.mean = mean(AGBI.iter, na.rm = T),
#             AGBI.sd = sd(AGBI.iter, na.rm=TRUE),
#             AGBI.lo = quantile(AGBI.iter, c(0.025), na.rm=TRUE),
#             AGBI.hi = quantile(AGBI.iter, c(0.975), na.rm=TRUE), 
#             .groups='keep')
# head(all_taxon_summary)

# #no plot
# all_taxon_by_iter <- all_data |>
#   group_by(year, iter, taxon, model, site) |>
#   dplyr::summarize(AGB.sum = sum(AGB),
#             AGBI.sum = sum(AGBI),
#             .groups = 'keep') 
# 
# #taxon summary data without plot 
# all_taxon_summary = all_taxon_by_iter %>%
#   group_by(year, taxon, model, site) %>% 
#   dplyr::summarize(AGB.mean = mean(AGB.sum, na.rm = T),
#             AGB.sd = sd(AGB.sum),
#             AGB.lo = quantile(AGB.sum, c(0.025), na.rm=TRUE),
#             AGB.hi = quantile(AGB.sum, c(0.975), na.rm=TRUE), 
#             AGBI.mean = mean(AGBI.sum, na.rm = TRUE),
#             AGBI.sd = sd(AGBI.sum, na.rm = TRUE),
#             AGBI.lo = quantile(AGBI.sum, c(0.025), na.rm=TRUE),
#             AGBI.hi = quantile(AGBI.sum, c(0.975), na.rm=TRUE), 
#             .groups='keep')
# head(all_taxon_summary)

#changing to wide format with taxons as column names and values = ABGI.mean 
#values_fill=0 does not work 
#wide format of taxon data with AGBI as value data
# all_taxon_summary_wide = pivot_wider(data = all_taxon_summary[,(colnames(all_taxon_summary) %in% 
#                                                                   c('year', 'taxon', 'AGBI.mean', 'site'))],
#                                      id_cols = c(year, site),
#                                      names_from = taxon, 
#                                      values_from = AGBI.mean, 
#                                      values_fill = NA )

# all_taxon_summary = all_taxon_site_summary

AGBI_taxon_top_5 = AGBI_taxon %>% semi_join(AGBI_taxon_top_5_present, by=c('site', 'taxon'))

goose_taxa <- AGBI_taxon_top_5 %>%
  filter(site == "GOOSE") %>%
  dplyr::select(year, taxon, AGBI.mean, site) %>%
  pivot_wider(
    names_from = taxon,
    values_from = AGBI.mean,
    values_fill = list(AGBI.mean = NA))


harvard_taxa <- AGBI_taxon_top_5 %>%
  filter(site == "HARVARD") %>%
  dplyr::select(year, taxon, AGBI.mean, site) %>%
  pivot_wider(
    names_from = taxon,
    values_from = AGBI.mean,
    values_fill = list(AGBI.mean = NA))

NRP_taxa <- AGBI_taxon_top_5 %>%
  filter(site == "NRP") %>%
  dplyr::select(year, taxon, AGBI.mean, site) %>%
  pivot_wider(
    names_from = taxon,
    values_from = AGBI.mean,
    values_fill = list(AGBI.mean = NA))

rooster_taxa <-AGBI_taxon_top_5 %>%
  filter(site == "ROOSTER") %>%
  dplyr::select(year, taxon, AGBI.mean, site) %>%
  pivot_wider(
    names_from = taxon,
    values_from = AGBI.mean,
    values_fill = list(AGBI.mean = NA))

sylvania_taxa <- AGBI_taxon_top_5 %>%
  filter(site == "SYLVANIA") %>%
  dplyr::select(year, taxon, AGBI.mean, site) %>%
  pivot_wider(
    names_from = taxon,
    values_from = AGBI.mean,
    values_fill = list(AGBI.mean = NA))

hmc_taxa <- AGBI_taxon_top_5 %>%
  filter(site == "HMC") %>%
  dplyr::select(year, taxon, AGBI.mean, site) %>%
  pivot_wider(
    names_from = taxon,
    values_from = AGBI.mean,
    values_fill = list(AGBI.mean = NA))

#wide format of site data with AGBI as value
AGBI_taxon_site_wide = pivot_wider(data = AGBI_taxon_site[,(colnames(AGBI_taxon_site) %in% 
                                                                c('year','AGBI.mid', 'site'))],
                                    id_cols = c(year),
                                    names_from = site, 
                                    values_from = AGBI.mid, 
                                    values_fill = NA )

AGB_taxon_site_wide = pivot_wider(data = AGBI_taxon_site[,(colnames(AGBI_taxon_site) %in% 
                                                             c('year','AGB.mid', 'site'))],
                                    id_cols = c(year),
                                    names_from = site, 
                                    values_from = AGB.mid, 
                                    values_fill = NA)

# #plotting histogram of AGBI for different time periods (past and present) 
# ggplot(data = AGBI_taxon_site %>% filter(!is.na(period))) +
#   geom_histogram(aes(x = AGBI.mean, fill = period)) +
#   facet_wrap(site ~ .)+
#   theme(legend.position = "bottom")+
#   theme_light(14)
# ggsave('report/figures/AGBI_hist_past_present.jpg')
# 
# #Plotting histogram of AGBI for the time period past, year<1960
# ggplot(data = all_site_summary %>% filter(period == "past")) +
#   geom_histogram(aes(x = AGBI.mean, fill = period)) +
#   facet_wrap(site ~ .)+
#   theme_light(14)
# #ggsave('report/figures/AGBI_hist_past.jpg')
# 
# #plotting histogram of AGBI for the time period present, year>2000
# ggplot(data = all_site_summary %>% filter(period == "present")) +
#   geom_histogram(aes(x = AGBI.mean, fill = period)) +
#   facet_wrap(site ~ .)+
#   theme_light(14)
# #ggsave('report/figures/AGBI_hist_present.jpg')
# 
# #plotting the overall AGBI over time on a histogram 
# ggplot()+
#   geom_histogram(data =all_site_summary, aes(x = AGBI.mean, fill = site))+
#   facet_grid(site~.)+
#   theme_light(14)+
#   labs(x = 'Aboveground biomass increment')
# #ggsave("report/figures/AGBI_site_over_time_histogram.jpg")
# 
# #all taxon_summary 
# ggplot()+
#   geom_histogram(data = all_taxon_summary, aes(x=AGBI.mean, fill =taxon))+
#   facet_wrap(site~., scales = "free_x")+
#   theme(legend.position = "bottom")
# 
# ggplot()+
#   geom_histogram(data = subset(all_taxon_summary, taxon %in% c('ACRU','QURU', 'PIST')), 
#                                aes(x=AGBI.mean))+
#   facet_grid(site~taxon, scales = "free_x")
# 
# 
# #above ground biomass nrp vs harvard
# ggplot()+
#   geom_point(data= AGB_mean_wide, aes(x= NRP, y=HARVARD))+
#   theme_light(14)
# ggsave("report/figures/AGB_NRP_HARVARD.jpg")

######################################
#plotting pairwise combinations of the different sites AGBI.mean
########################3333
# List of site columns
sites <- c("GOOSE", "HARVARD", "NRP", "ROOSTER", "SYLVANIA", "HMC")

# Generate all unique pairs of sites
site_pairs <- combn(sites, 2, simplify = FALSE)

# Function to create a scatter plot for each pair
plot_list <- map(site_pairs, ~{
  ggplot(data = all_site_summary_wide) +
    geom_point(aes_string(x = .x[1], y = .x[2])) +
    # geom_smooth(aes_string(x = .x[1], y = .x[2]))+
    # geom_smooth(method=lm,fill="blue", color="blue", ...) +
    labs(x = .x[1], y = .x[2], title = paste(.x[1], "vs", .x[2])) +
    theme_light(base_size = 14)
})

# Display all plots in a single layout (optional)
library(gridExtra)
do.call(grid.arrange, plot_list)


pairs(all_site_summary_wide[,2:7])

library(GGally)
p = ggpairs(AGBI_taxon_site_wide[,2:7], lower=list(continuous="smooth"))+
  theme_light(14) 
print(p)
ggsave("figures/AGBI_vs_AGBI_site_pairs.png", width=10, height=8)


#####################################

# for publication?!
p = ggplot(data=AGBI_site) +
  geom_ribbon(aes(x=year, ymin=AGBI.lo, ymax=AGBI.hi), alpha = 0.2) +
  geom_line(aes(x=year, y=AGBI.mid)) +
  theme_light(14) +
  labs( x = "Year", y = "AGBI (Mg/ha)") + 
  scale_x_continuous(breaks=seq(1950, 2015, by=10)) +
  facet_wrap(~site, ncol=2, scales='free_y')
ggsave("figures/AGBI_site_vs_year.png", width=10, height=8)

#AGBI over time from 1950-2011
ggplot(data=AGBI_site) +
  geom_ribbon(aes(x=year, ymin=AGBI.lo, ymax=AGBI.hi, fill=site), alpha = 0.2) +
  geom_line(aes(x=year, y=AGBI.mid, colour=site)) +
  theme_light(14) +
  labs( x = "Year", y = "AGBI (Mg/ha)")
#ggsave("report/figures/AGBI_over_time.jpg")

ggplot(data=AGBI_site) +
  geom_ribbon(aes(x=year, ymin=AGBI.lo, ymax=AGBI.hi), alpha = 0.2) +
  geom_line(aes(x=year, y=AGBI.mid)) +
  theme_light(14) +
  labs( x = "Year", y = "biomass increment (Mg/ha)") + 
  facet_grid(site~.)

ggplot(data=AGBI_site) +
  geom_ribbon(aes(x=year, ymin=AGBI.lo, ymax=AGBI.hi), alpha = 0.2) +
  geom_line(aes(x=year, y=AGBI.mid)) +
  theme_light(14) +
  labs( x = "Year", y = "AGBI (Mg/ha)") + 
  facet_wrap(~site, ncol=2)



ggplot(data=AGBI_site) +
  geom_ribbon(aes(x=year, ymin=AGBI.lo, ymax=AGBI.hi), alpha = 0.2) +
  geom_line(aes(x=year, y=AGBI.mid)) +
  theme_light(14) +
  labs( x = "Year", y = "AGBI (Mg/ha)") + 
  facet_grid(site~., scales='free_y')

#AGB overtime
ggplot(data=AGBI_site) +
  geom_ribbon(aes(x=year, ymin=AGB.lo, ymax=AGB.hi, colour=site, fill=site), alpha = 0.5) +
  geom_line(aes(x=year, y=AGB.mean, colour=site)) +
  theme_light(14) +
  labs( x = "Year", y = "biomass (Mg/ha)")
# ggsave("report/figures/AGB_over_time.png")


## TAXA

AGBI_taxon_top_3 = AGBI_taxon %>% semi_join(foo, by = c('site', 'taxon'))

ggplot(data=AGBI_taxon) +
  geom_ribbon(aes(x=year, ymin=AGBI.lo, ymax=AGBI.hi, fill=taxon), alpha = 0.2) +
  geom_line(aes(x=year, y=AGBI.mean, colour=taxon)) +
  theme_bw(14) +
  xlab('Year') +
  ylab('biomass increment (Mg/ha)') +
  facet_wrap(~site)
# ggsave("report/figures/AGBI_over_time_taxons.jpg")

color_palette = 'Dark2'

ggplot(data=AGBI_taxon_top_3) +
  geom_ribbon(aes(x=year, ymin=AGBI.lo, ymax=AGBI.hi,fill=taxon), alpha = 0.2) +
  geom_line(aes(x=year, y=AGBI.mid, colour=taxon)) +
  theme_light(14) +
  xlab('Year') +
  ylab('AGBI (Mg/ha)') +
  facet_wrap(~site, ncol=2, scales='free_y') +
  scale_color_brewer(palette=color_palette) +
  scale_fill_brewer(palette=color_palette) +
  scale_x_continuous(breaks=seq(1950, 2015, by=10)) +
  labs(color='Species', fill = 'Species')
ggsave("figures/AGBI_species_vs_year.png", width=10, height=8)

ggplot(data=subset(AGBI_taxon, taxon %in% taxa_top_3)) +
  geom_ribbon(aes(x=year, ymin=AGBI.lo, ymax=AGBI.hi,fill=taxon), alpha = 0.2) +
  geom_line(aes(x=year, y=AGBI.mid, colour=taxon)) +
  theme_light(14) +
  xlab('Year') +
  ylab('AGBI (Mg/ha)') +
  facet_wrap(~site, ncol=2, scales='free_y') +
  labs(color='Species', fill = 'Species')



ggplot(data=subset(AGBI_taxon, taxon %in% taxa_top_3)) +
  geom_ribbon(aes(x=year, ymin=AGBI.lo, ymax=AGBI.hi, colour=taxon, fill=taxon), alpha = 0.5) +
  geom_line(aes(x=year, y=AGBI.mean, colour=taxon)) +
  theme_bw(14) +
  xlab('Year') +
  ylab('biomass increment (Mg/ha)') +
  facet_grid(site~., scales = 'free_y')

#AGBI over time by taxon, scales = free_y
ggplot(data=all_taxon_summary) +
  geom_ribbon(aes(x=year, ymin=AGBI.lo, ymax=AGBI.hi, colour=taxon, fill=taxon), alpha = 0.5) +
  geom_line(aes(x=year, y=AGBI.mean, colour=taxon)) +
  theme_light(14) +
  xlab('Year') +
  ylab('biomass increment (Mg/ha)') +
  facet_wrap(~site, scales = 'free_y')+
  theme(axis.text.x = element_text(angle = -45))
# ggsave("report/figures/AGBI_over_time_taxons_freey.jpg")


ggplot(data=subset(AGBI_taxon, taxon %in% taxa_top_3)) +
  geom_ribbon(aes(x=year, ymin=AGBI.lo, ymax=AGBI.hi, colour=taxon, fill=taxon), alpha = 0.5) +
  geom_line(aes(x=year, y=AGBI.mean, colour=taxon)) +
  theme_light(14) +
  xlab('Year') +
  ylab('biomass increment (Mg/ha)') +
  facet_wrap(~site, scales = 'free_y')+
  theme(axis.text.x = element_text(angle = -45))
# 
# #AGBI over time by taxon with sd
# ggplot(data=all_taxon_summary) +
#   geom_ribbon(aes(x=year, ymin=AGBI.mean-2*AGBI.sd,
#                   ymax=AGBI.mean+2*AGBI.sd, color = taxon, fill = taxon), alpha=0.3) +
#   geom_line(aes(x=year, y=AGBI.mean, color = taxon)) +
#   facet_wrap(~site)
# ggsave("report/figures/AGBI_site_taxon_with_sd.jpg")


#########################################################################################
#CORRELATION AT THE SITE LEVEL 
################################################################################################

AGBI_taxon_top_5 = AGBI_taxon %>% semi_join(AGBI_taxon_top_5_present, by=c('site', 'taxon'))
AGBI_taxon_top_5_wide = AGBI_taxon_top_5[,c('year', 'site', 'taxon', 'AGBI.mid')] %>% pivot_wider(names_from=taxon, values_from = AGBI.mid)
ggpairs(AGBI_taxon_top_5_wide, columns = 2:5, aes(colour=site))



#correlation between sites of AGBI
cor_site_AGBI = data.frame(cor(AGBI_taxon_site_wide[, c('GOOSE', 'ROOSTER', 'SYLVANIA', 'NRP', 'HARVARD', 'HMC')], 
                          use = "complete.obs"))
write.csv(cor_site_AGBI, "correlation_AGBI_site.csv")
#plotting correlaiton data
ggcorrplot(cor_site_AGBI, method = "square", type = "lower",show.diag = TRUE, hc.order = FALSE)+
  scale_fill_distiller(
    palette = "PuOr", na.value = "white",
    direction = 1, limits = c(-1, 1),
    name = "Pearson\nCorrelation:") +
  ggtitle("AGBI correlation")

#calculating correlation and pvalues for AGBI between sites
cor_results <- rcorr(as.matrix(AGBI_taxon_site_wide
                    [, c('GOOSE', 'ROOSTER', 'SYLVANIA', 'NRP', 'HARVARD', 'HMC')]))
# Extract correlation coefficients and p-values
cor_coefficients <- cor_results$r
p_values <- cor_results$P

# Convert to data frame
cor_site_AGBI_p <- data.frame(
  Variable1 = rep(colnames(cor_coefficients), each = ncol(cor_coefficients)),
  Variable2 = rep(colnames(cor_coefficients), times = ncol(cor_coefficients)),
  Correlation = as.vector(cor_coefficients),
  P_Value = as.vector(p_values))

# Filter for unique pairs (optional, to avoid duplicates)
cor_site_AGBI_p <- cor_site_AGBI_p[upper.tri(cor_coefficients, diag = FALSE), ]

#correlation between sites AGB
cor_site_AGB = data.frame(cor(AGB_taxon_site_wide[, c('GOOSE', 'ROOSTER', 'SYLVANIA', 'NRP', 'HARVARD', 'HMC')], 
                          use = "complete.obs"))
# ggcorrplot(cor_site_AGB, method = "square", type = "lower", show.diag = TRUE, hc.order = FALSE)+
#   ggtitle("AGB correlation")

#goose correlation
#complete.obs excludes any rows with NA values 
goose_correlations = data.frame( cor(goose_taxa [,c(4:ncol(goose_taxa))], use = "complete.obs"))
harvard_correlations = data.frame( cor(harvard_taxa [,c(4:length(harvard_taxa))], use = "complete.obs"))
NRP_correlations = data.frame( cor(NRP_taxa [,c(4:length(NRP_taxa))], use = "complete.obs"))
rooster_correlations = data.frame( cor(rooster_taxa [,c(4:length(rooster_taxa))], use = "complete.obs"))
sylvania_correlations = data.frame( cor(sylvania_taxa [,c(4:length(sylvania_taxa))], use = "complete.obs"))
hmc_correlations = data.frame( cor(hmc_taxa [,c(4:length(hmc_taxa))], use = "complete.obs"))

pdf("figures/AGBI_species_pairs_by_site.pdf", width=10, height=8)
ggpairs(goose_taxa[,4:ncol(goose_taxa)], lower = list(continuous = wrap("smooth", alpha = 0.5, size=1)), title = 'Site: Goose') 
ggpairs(harvard_taxa[,4:ncol(harvard_taxa)], lower = list(continuous = wrap("smooth", alpha = 0.5, size=1)), title = 'Site: Harvard Forest') 
ggpairs(NRP_taxa[,4:ncol(NRP_taxa)], lower = list(continuous = wrap("smooth", alpha = 0.5, size=1)), title = 'Site: North Round Pond') 
ggpairs(rooster_taxa[,4:ncol(rooster_taxa)], lower = list(continuous = wrap("smooth", alpha = 0.5, size=1)), title = 'Site: Rooster') 
ggpairs(sylvania_taxa[,4:ncol(sylvania_taxa)], lower = list(continuous = wrap("smooth", alpha = 0.5, size=1)), title = 'Site: Sylvania') 
ggpairs(hmc_taxa[,4:ncol(hmc_taxa)], lower = list(continuous = wrap("smooth", alpha = 0.5, size=1)), title = 'Site: Huron Mountain Club') 
dev.off()

# png("figures/AGBI_species_pairs_GOOSE.png", width=10, height=8)
p = ggpairs(goose_taxa[,4:ncol(goose_taxa)], lower = list(continuous = wrap("smooth", alpha = 0.5, size=1)), title = 'Site: Goose') 
print(p)
# dev.off()
ggsave("figures/AGBI_species_pairs_GOOSE.png", width=10, height=8)

# png("figures/AGBI_species_pairs_HF.png", width=10, height=8)
ggpairs(harvard_taxa[,4:ncol(harvard_taxa)], lower = list(continuous = wrap("smooth", alpha = 0.5, size=1)), title = 'Site: Harvard Forest') 
# dev.off()
ggsave("figures/AGBI_species_pairs_HF.png", width=10, height=8)

# png("figures/AGBI_species_pairs_NRP.png", width=10, height=8)
ggpairs(NRP_taxa[,4:ncol(NRP_taxa)], lower = list(continuous = wrap("smooth", alpha = 0.5, size=1)), title = 'Site: North Round Pond') 
# dev.off()
ggsave("figures/AGBI_species_pairs_NRP.png", width=10, height=8)


# png("figures/AGBI_species_pairs_ROOSTER.png", width=10, height=8)
ggpairs(rooster_taxa[,4:ncol(rooster_taxa)], lower = list(continuous = wrap("smooth", alpha = 0.5, size=1)), title = 'Site: Rooster') 
# dev.off()
ggsave("figures/AGBI_species_pairs_ROOSTER.png", width=10, height=8)

# png("figures/AGBI_species_pairs_SYLVANIA.png", width=10, height=8)
ggpairs(sylvania_taxa[,4:ncol(sylvania_taxa)], lower = list(continuous = wrap("smooth", alpha = 0.5, size=1)), title = 'Site: Sylvania') 
# dev.off()
ggsave("figures/AGBI_species_pairs_SYLVANIA.png", width=10, height=8)

# png("figures/AGBI_species_pairs_HMC.png", width=10, height=8)
ggpairs(hmc_taxa[,4:ncol(hmc_taxa)], lower = list(continuous = wrap("smooth", alpha = 0.5, size=1)), title = 'Site: Huron Mountain Club') 
# dev.off()
ggsave("figures/AGBI_species_pairs_HMC.png", width=10, height=8)


# #plotting correlation plots for each site between each taxa
# pdf('report/figures/site_taxa_cor.pdf')
# 
# ggcorrplot(goose_correlations, method = "square", type = "lower", hc.order = FALSE, show.diag = TRUE) +
#   scale_fill_distiller(
#     palette = "PuOr", na.value = "white",
#     direction = 1, limits = c(-1, 1),
#     name = "Pearson\nCorrelation:") +
#   ggtitle("Goose Correlation")
# ggcorrplot(harvard_correlations, method = "square", type = "lower", hc.order = FALSE, show.diag = TRUE) +
#   scale_fill_distiller(
#     palette = "PuOr", na.value = "white",
#     direction = 1, limits = c(-1, 1),
#     name = "Pearson\nCorrelation:") +
#   ggtitle("Harvard Correlation")
# ggcorrplot(NRP_correlations, method = "square", type = "lower", hc.order = FALSE, show.diag = TRUE) +
#   scale_fill_distiller(
#     palette = "PuOr", na.value = "white",
#     direction = 1, limits = c(-1, 1),
#     name = "Pearson\nCorrelation:") +
#   ggtitle("NRP Correlation")
# ggcorrplot(rooster_correlations, method = "square", type = "lower", hc.order = FALSE, show.diag = TRUE) +
#   scale_fill_distiller(
#     palette = "PuOr", na.value = "white",
#     direction = 1, limits = c(-1, 1),
#     name = "Pearson\nCorrelation:") +
#   ggtitle("Rooster Correlation")
# ggcorrplot(sylvania_correlations, method = "square", type = "lower", hc.order = FALSE, show.diag = TRUE) +
#   scale_fill_distiller(
#     palette = "PuOr", na.value = "white",
#     direction = 1, limits = c(-1, 1),
#     name = "Pearson\nCorrelation:") +
#   ggtitle("Sylvania Correlation")
# ggcorrplot(hmc_correlations, method = "square", type = "lower", hc.order = FALSE, show.diag = TRUE) +
#   scale_fill_distiller(
#     palette = "PuOr", na.value = "white",
#     direction = 1, limits = c(-1, 1),
#     name = "Pearson\nCorrelation:") +
#   ggtitle("HMC Correlation")
# dev.off()


clim_vars = c("PPT", "Tmean", "Tmin", "Tmax", "Vpdmin", "Vpdmax", "Vpdmean")

sites = c('GOOSE', 'ROOSTER', 'NRP', 'HARVARD', 'SYLVANIA', 'HMC')

agbi_cumsum = all_taxon_summary %>% 
  #group_by(site, model) %>%
  dplyr::arrange(site, desc(AGBI.mean)) %>%
  group_by(site) %>%
  dplyr::mutate(cum_sum = cumsum(AGBI.mean) / sum(AGBI.mean)) %>% 
  ungroup()

#filtering data for those that make up 95% of the total biomass
agbi_cumsum_filter = agbi_cumsum %>% 
  filter(cum_sum < 0.95)

#wide format of filtered data
agbi_cumsum_filter %>% 
  pivot_wider(names_from = 'site', values_from = 'AGBI.mean')

#joining filtered data with taxon data
df2 = inner_join(clim_taxon, agbi_cumsum_filter[,c('site', 'taxon', 'cum_sum')], by = c('site', 'taxon'))

#################################################################################
# taxon data 
#################################################################################


# taxon_site_total = AGBI_taxon_site_wide %>% 
#   group_by(year, site) %>% 
#   dplyr::summarize(total_AGBI_mean = sum(AGBI.mean, na.rm = TRUE))

# %>% 
#   ungroup() %>% 
#   arrange(site, desc(total_AGBI_mean)) %>% 
#   group_by(site) %>% 
#   slice(1:3)


#taking the sum of each taxon for a given site
# taxon_summed = all_taxon_summary %>% 
#   group_by(year, taxon, site) %>% 
#   dplyr::summarize(sum_taxa_AGBI_mean = sum(AGBI.mean, na.rm = TRUE))

#going with all_site_summary which has the TOTAL AGBI.mean for a given site
all_taxon_summed = all_taxon_summary %>% 
  inner_join(taxon_site_total, by = c("year", "site"))

#dividing the total taxon AGBI.mean by the total AGBI at a site to determine percentage 
#that the taxon is present at a site
fractional_biomass = all_taxon_summed %>% 
  mutate(taxon_fractions = AGBI.mean/total_AGBI_mean)
# 
# #WHYYYYY
# foo = fractional_biomass %>% 
#   group_by(year, site) %>% 
#   mutate(foo = sum(taxon_fractions))

#calculating the overall mean increment over the entire time for each taxon
# fractional_mean_taxon = fractional_biomass %>% 
#   group_by(taxon, site) %>% 
#   dplyr::summarize(taxon_mean = mean(taxon_fractions, na.rm = TRUE))



ggplot(data = fractional_biomass) +
  geom_line(aes(x =year, y = taxon_fractions, color = taxon))+ 
  facet_wrap(~site)
ggsave("report/2025/taxon_fractions_time.png")


ggplot(data = fractional_biomass) +
  geom_area(aes(x =year, y = taxon_fractions, fill=taxon))+ 
  facet_wrap(~site)
ggsave("figures1950/taxon_fractions_fill_time.png")  

# correlation
head(cor_clim_vars_taxon)

foo = taxon_site_total %>% group_by(site) %>% slice_max(order_by=total_AGBI_mean, n=3)

cor_clim_vars_taxon

bar = left_join(cor_clim_vars_taxon, foo)

















ggplot(data = clim_taxon) +
  geom_point(aes(x = year, y = AGBI.mean))+
  facet_wrap(~taxon, scales = 'free_y')
ggsave("AGBI_time_taxon.png")
#  
# 
#
