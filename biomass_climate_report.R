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


#above ground biomass
goose_total_agb <- readRDS('sites/GOOSE/runs/v3.1_012021/output/AGB_TAXA_STAN_GOOSE_v3.1_012021.RDS')
goose_total_agb_subset = subset(goose_total_agb, year>1950)
# harvard_total_agb <- readRDS('sites/HARVARD/runs/v2.0_102020/output/AGB_TAXA_STAN_HARVARD_v2.0_102020.RDS')
northround_total_agb <- readRDS('sites/NORTHROUND/runs/v3.1_082020/output/AGB_TAXA_STAN_NORTHROUND_v3.1_082020.RDS')
northround_total_agb_subset = subset(northround_total_agb, year>1950)
rooster_total_agb <- readRDS('sites/ROOSTER/runs/v3.1_082020/output/AGB_TAXA_STAN_ROOSTER_v3.1_082020.RDS')
rooster_total_agb_subset = subset(rooster_total_agb, year>1950)
sylvania_total_agb <- readRDS('sites/SYLVANIA/runs/v3.1_082020/output/AGB_TAXA_STAN_SYLVANIA_v3.1_082020.RDS')
sylvania_total_agb_subset = subset(sylvania_total_agb, year>1950)
harvard_total_agb <- readRDS('sites/HARVARD/runs/v3.1_102020/output/AGB_TAXA_STAN_HARVARD_v3.1_102020.RDS')
harvard_total_agb_subset = subset(harvard_total_agb, year>1950)
#hmc_total_agb <- readRDS('sites/HMC/runs/v3.1_082020/output/AGB_TAXA_STAN_HMC_v3.1_082020.RDS')


#above ground biomass increment 
goose_total_agbi <- readRDS('sites/GOOSE/runs/v3.1_012021/output/AGBI_TAXA_STAN_GOOSE_v3.1_012021.RDS')
goose_total_agbi_subset = subset(goose_total_agbi, year>1950)
# harvard_total_agbi <- readRDS('sites/HARVARD/runs/v2.0_102020/output/AGBI_TAXA_STAN_HARVARD_v2.0_102020.RDS')
northround_total_agbi <- readRDS('sites/NORTHROUND/runs/v3.1_082020/output/AGBI_TAXA_STAN_NORTHROUND_v3.1_082020.RDS')
northround_total_agbi_subset = subset(northround_total_agbi, year>1950)
rooster_total_agbi <- readRDS('sites/ROOSTER/runs/v3.1_082020/output/AGBI_TAXA_STAN_ROOSTER_v3.1_082020.RDS')
rooster_total_agbi_subset = subset(rooster_total_agbi, year>1950)
sylvania_total_agbi <- readRDS('sites/SYLVANIA/runs/v3.1_082020/output/AGBI_TAXA_STAN_SYLVANIA_v3.1_082020.RDS')
sylvania_total_agbi_subset = subset(sylvania_total_agbi, year>1950)
harvard_total_agbi <- readRDS('sites/HARVARD/runs/v3.1_102020/output/AGBI_TAXA_STAN_HARVARD_v3.1_102020.RDS')
harvard_total_agbi_subset = subset(harvard_total_agbi, year>1950)
#hmc_total_agbi <- readRDS('sites/HMC/runs/v3.1_082020/output/AGBI_TAXA_STAN_HMC_v3.1_082020.RDS')



# Combining abgi and abg into one dataframe 
goose_total <- goose_total_agb_subset |>
  left_join(goose_total_agbi_subset, by = c('year', 'iter', 'taxon', 
                                     'model', 'plot')) |>
  rename(AGB = ab,
         AGBI = abi) |>
  # select(-c(type.x, type.y)) |>
  # Add site name for combining sites into one df
  mutate(site = 'GOOSE')

nrp_total <- northround_total_agb_subset |>
  left_join(northround_total_agbi_subset, by = c('year', 'iter', 'taxon', 
                                          'model', 'plot')) |>
  rename(AGB = ab,
         AGBI = abi) |>
  # select(-c(type.x, type.y)) |>
  mutate(site = 'NRP')

rooster_total <- rooster_total_agb_subset |>
  left_join(rooster_total_agbi_subset, by = c('year', 'iter', 'taxon',
                                       'model', 'plot')) |>
  rename(AGB = ab,
         AGBI = abi) |>
  # select(-c(type.x, type.y)) |>
  mutate(site = 'ROOSTER')


sylvania_total <- sylvania_total_agb_subset |>
  left_join(sylvania_total_agbi_subset, by = c('year', 'iter', 'taxon', 
                                        'model', 'plot')) |>
  rename(AGB = ab,
         AGBI = abi) |>
  # select(-c(type.x, type.y)) |>
  mutate(site = 'SYLVANIA')

harvard_total <- harvard_total_agb_subset |>
  left_join(harvard_total_agbi, by = c('year', 'iter', 'taxon',
                                       'model', 'plot')) |>
  rename(AGB = ab,
         AGBI = abi) |>
  # select(-c(type.x, type.y)) |>
  mutate(site = 'HARVARD')

harvard_total <- subset(harvard_total, model == 'Model RW')

#combining data from all sites into one dataframe
all_data <- rbind(goose_total, nrp_total, rooster_total, sylvania_total, harvard_total)
# all_data <- rbind(goose_total, nrp_total, rooster_total, sylvania_total)


# Create summaries by site
# I am not sure that we are interested in average agb per tree
# This is defined as agb_persite: mean total site biomass divided by the number of trees
all_site_plot_by_iter <- all_data |>
  group_by(year, iter, plot, model, site) |>
  summarize(AGB.sum = sum(AGB),
            AGBI.sum = sum(AGBI),
            .groups = 'keep') 

#plot summary data 
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

#site summary data 
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

all_site_summary$period = NA
all_site_summary$period[which(all_site_summary$year<1960)] = "past"
all_site_summary$period[which(all_site_summary$year>2000)] = "present"


#taxon_group takes the sum of all the trees in one taxon.
#iterations for each taxon not individual trees 
all_taxon_plot_by_iter <- all_data |>
  group_by(year, iter, taxon, plot, model, site) |>
  summarize(AGB.sum = sum(AGB),
            AGBI.sum = sum(AGBI),
            .groups = 'keep') 

#taxon plot summary with plot data
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

#taxon summary data without plot 
all_taxon_summary = all_taxon_by_iter %>%
  group_by(year, taxon, model, site) %>% 
  summarize(AGB.mean = mean(AGB.sum, na.rm = T),
            AGB.sd = sd(AGB.sum),
            AGB.lo = quantile(AGB.sum, c(0.025), na.rm=TRUE),
            AGB.hi = quantile(AGB.sum, c(0.975), na.rm=TRUE), 
            AGBI.mean = mean(AGBI.sum, na.rm = TRUE),
            AGBI.sd = sd(AGBI.sum, na.rm = TRUE),
            AGBI.lo = quantile(AGBI.sum, c(0.025), na.rm=TRUE),
            AGBI.hi = quantile(AGBI.sum, c(0.975), na.rm=TRUE), 
            .groups='keep')
head(all_taxon_summary)

#changing to wide format with taxons as column names and values = ABGI.mean 
#values_fill=0 does not work 
#wide format of taxon data with AGBI as value data
all_taxon_summary_wide = pivot_wider(data = all_taxon_summary[,(colnames(all_taxon_summary) %in% 
                                                                  c('year', 'taxon', 'AGBI.mean', 'site'))],
                                     id_cols = c(year, site),
                                     names_from = taxon, 
                                     values_from = AGBI.mean, 
                                     values_fill = NA )

goose_correlations <- all_taxon_summary %>%
  filter(site == "GOOSE") %>%
  summarize(correlation = cor(AGBI.mean))

  select(year, taxon, AGBI.mean, site) %>%
  pivot_wider(
    names_from = taxon,
    values_from = AGBI.mean,
    values_fill = list(AGBI.mean = NA))


harvard_taxa <- all_taxon_summary %>%
  filter(site == "HARVARD") %>%
  select(year, taxon, AGBI.mean, site) %>%
  pivot_wider(
    names_from = taxon,
    values_from = AGBI.mean,
    values_fill = list(AGBI.mean = NA))

NRP_taxa <- all_taxon_summary %>%
  filter(site == "NRP") %>%
  select(year, taxon, AGBI.mean, site) %>%
  pivot_wider(
    names_from = taxon,
    values_from = AGBI.mean,
    values_fill = list(AGBI.mean = NA))

rooster_taxa <- all_taxon_summary %>%
  filter(site == "ROOSTER") %>%
  select(year, taxon, AGBI.mean, site) %>%
  pivot_wider(
    names_from = taxon,
    values_from = AGBI.mean,
    values_fill = list(AGBI.mean = NA))

sylvania_taxa <- all_taxon_summary %>%
  filter(site == "SYLVANIA") %>%
  select(year, taxon, AGBI.mean, site) %>%
  pivot_wider(
    names_from = taxon,
    values_from = AGBI.mean,
    values_fill = list(AGBI.mean = NA))

#wide format of site data with AGBI as value
all_site_summary_wide = pivot_wider(data = all_site_summary[,(colnames(all_site_summary) %in% 
                                                                c('year','AGBI.mean', 'site'))],
                                    id_cols = c(year),
                                    names_from = site, 
                                    values_from = AGBI.mean, 
                                    values_fill = NA )

AGB_mean_wide = pivot_wider(data = all_site_summary[,(colnames(all_site_summary) %in% 
                                                                c('year','AGB.mean', 'site'))],
                                    id_cols = c(year),
                                    names_from = site, 
                                    values_from = AGB.mean, 
                                    values_fill = NA)

#plotting histogram of AGBI for different time periods (past and present) 
ggplot(data = all_site_summary %>% filter(!is.na(period))) +
  geom_histogram(aes(x = AGBI.mean, fill = period)) +
  facet_wrap(site ~ .)+
  theme(legend.position = "bottom")
ggsave('report/figures/AGBI_hist_past_present.jpg')

#Plotting histogram of AGBI for the time period past, year<1960
ggplot(data = all_site_summary %>% filter(period == "past")) +
  geom_histogram(aes(x = AGBI.mean, fill = period)) +
  facet_wrap(site ~ .)
ggsave('report/figures/AGBI_hist_past.jpg')

#plotting histogram of AGBI for the time period present, year>2000
ggplot(data = all_site_summary %>% filter(period == "present")) +
  geom_histogram(aes(x = AGBI.mean, fill = period)) +
  facet_wrap(site ~ .)
ggsave('report/figures/AGBI_hist_present.jpg')

#plotting the overall AGBI over time on a histogram 
ggplot()+
  geom_histogram(data =all_site_summary, aes(x = AGBI.mean, fill = site))+
  facet_grid(site~.)+
  theme_light(14)
ggsave("report/figures/AGBI_site_over_time_histogram.jpg")

ggplot()+
  geom_histogram(data = all_taxon_summary, aes(x=AGBI.mean, fill =taxon))+
  facet_grid(site~.)+
  theme(legend.position = "bottom")


ggplot()+
  geom_histogram(data = subset(all_taxon_summary, taxon %in% c('ACRU','QURU', 'PIST')), 
                               aes(x=AGBI.mean))+
  facet_grid(site~taxon, scales = "free_x")


#above ground biomass nrp vs harvard
ggplot()+
  geom_point(data= AGB_mean_wide, aes(x= NRP, y=HARVARD))+
  theme_light(14)
ggsave("report/figures/AGB_NRP_HARVARD.jpg")

######################################
#plotting pairwise combinations of the different sites AGBI.mean
########################3333
# List of site columns
sites <- c("GOOSE", "HARVARD", "NRP", "ROOSTER", "SYLVANIA")

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


pairs(all_site_summary_wide[,2:6])

library(GGally)
ggpairs(all_site_summary_wide[,2:6], lower=list(continuous="smooth"))


#####################################

#AGBI over time starting at the year 1900
ggplot(data=all_site_summary) +
  geom_ribbon(aes(x=year, ymin=AGBI.lo, ymax=AGBI.hi, colour=site, fill=site)) +
  geom_line(aes(x=year, y=AGBI.mean, colour=site)) +
  theme_classic(14) +
  labs( title = "AGBI over time", x = "Year", y = "AGBI (Mg/ha)")+
  facet_grid(model~.)
ggsave("report/figures/AGBI_over_time.jpg")

#AGB ovetime
ggplot(data=all_site_summary) +
  geom_ribbon(aes(x=year, ymin=AGB.lo, ymax=AGB.hi, colour=site, fill=site)) +
  geom_line(aes(x=year, y=AGB.mean, colour=site)) +
  theme_light(14) +
  labs( title = "Aboveground biomass over time", x = "Year", y = "AGBI (Mg/ha)")+
  facet_grid(model~.)
ggsave("report/figures/AGB_over_time.png")


#AGBI over time by taxon 
ggplot(data=all_taxon_summary) +
  geom_ribbon(aes(x=year, ymin=AGBI.lo, ymax=AGBI.hi, colour=taxon, fill=taxon)) +
  geom_line(aes(x=year, y=AGBI.mean, colour=taxon)) +
  theme_bw(14) +
  xlab('Year') +
  ylab('AGBI (Mg/ha)') +
  facet_wrap(~site)
ggsave("report/figures/AGBI_over_time_taxons.jpg")

#AGBI over time by taxon, scales = free_y
ggplot(data=all_taxon_summary) +
  geom_ribbon(aes(x=year, ymin=AGBI.lo, ymax=AGBI.hi, colour=taxon, fill=taxon)) +
  geom_line(aes(x=year, y=AGBI.mean, colour=taxon)) +
  theme_bw(14) +
  xlab('Year') +
  ylab('AGBI (Mg/ha)') +
  facet_wrap(~site, scales = 'free_y')
ggsave("report/figures/AGBI_over_time_taxons_freey.jpg")

#AGBI over time by taxon with sd
ggplot(data=all_taxon_summary) +
  geom_ribbon(aes(x=year, ymin=AGBI.mean-2*AGBI.sd,
                  ymax=AGBI.mean+2*AGBI.sd, color = taxon, fill = taxon), alpha=0.3) +
  geom_line(aes(x=year, y=AGBI.mean, color = taxon)) +
  facet_wrap(~site)
ggsave("report/figures/AGBI_site_taxon_with_sd.jpg")

  



#########################################################################################
#CORRELATION AT THE SITE LEVEL 
################################################################################################

#correlation between sites of AGBI
cor_site_AGBI = data.frame(cor(all_site_summary_wide[, c('GOOSE', 'ROOSTER', 'SYLVANIA', 'NRP', 'HARVARD')], 
                          use = "complete.obs"))
ggcorrplot(cor_site_AGBI, method = "circle", type = "lower", hc.order = FALSE)
write.csv(cor_site_AGBI, "correlation_AGBI_site.csv")  

#correlation between sites AGB
cor_site_AGB = data.frame(cor(AGB_mean_wide[, c('GOOSE', 'ROOSTER', 'SYLVANIA', 'NRP', 'HARVARD')], 
                          use = "complete.obs"))
ggcorrplot(cor_site_AGB, method = "circle", type = "lower", hc.order = FALSE)

#goose correlation
#complete.obs excludes any rows with NA values 
goose_correlations = data.frame( cor(goose_taxa [,c(4:length(goose_taxa))], use = "complete.obs"))
harvard_correlations = data.frame( cor(harvard_taxa [,c(4:length(harvard_taxa))], use = "complete.obs"))
NRP_correlations = data.frame( cor(NRP_taxa [,c(4:length(NRP_taxa))], use = "complete.obs"))
rooster_correlations = data.frame( cor(rooster_taxa [,c(4:length(rooster_taxa))], use = "complete.obs"))
sylvania_correlations = data.frame( cor(sylvania_taxa [,c(4:length(sylvania_taxa))], use = "complete.obs"))

#plotting correlation plots for each site between each taxa
pdf('report/figures/site_taxa_cor.pdf')

ggcorrplot(goose_correlations, method = "square", type = "lower", hc.order = FALSE, show.diag = TRUE) +
  ggtitle("Goose Correlation")
ggcorrplot(harvard_correlations, method = "square", type = "lower", hc.order = FALSE, show.diag = TRUE) +
  ggtitle("Harvard Correlation")
ggcorrplot(NRP_correlations, method = "square", type = "lower", hc.order = FALSE, show.diag = TRUE) +
  ggtitle("NRP Correlation")
ggcorrplot(rooster_correlations, method = "square", type = "lower", hc.order = FALSE, show.diag = TRUE) +
  ggtitle("Rooster Correlation")
ggcorrplot(sylvania_correlations, method = "square", type = "lower", hc.order = FALSE, show.diag = TRUE) +
  ggtitle("Sylvania Correlation")

dev.off()

 #########################################################################################
#CLIMATE
################################################################################################

load('climate/prism_clim.RData')
clim_data = prism_long
clim_data = clim_data %>% 
  rename(site = loc, PPT = PPT2, Tmean = Tmean2, Tmin = Tmin2, Tmax = Tmax2, 
         Vpdmin = Vpdmin2, Vpdmax = Vpdmax2)

#putting the climat variables in wide format
clim_wide =  pivot_wider(data = clim_data,
                         names_from = month, 
                         values_from = c(PPT, Tmean, Tmin, Tmax, Vpdmin, Vpdmax))
# clim_melt = melt(clim_data, 
#                 id.vars = c('year', 'month', 'site'))
# 
# clim_melt$year = as.integer(clim_melt$year)
# summary(clim_melt)

#PPT_mean is the mean of each month summed to get the mean of the year
#yearly_meanT is the yearly temperature mean based on monthly means

# clim_summary = clim_wide |>
#   mutate(PPT_total = rowSums(dplyr::select(clim_wide, starts_with('PPT'))),
#          PPT_total_prev_tree = rowSums(dplyr::pick('PPT_09', 'PPT_10', 'PPT_11', 'PPT_12')),
#          PPT_total_current_tree = rowSums(dplyr::pick('PPT_01', 'PPT_02', 'PPT_03', 'PPT_04', 
#                                                       'PPT_05', 'PPT_06', 'PPT_07', 'PPT_08')),
#          yearly_meanT = rowMeans(dplyr::select(clim_wide, starts_with('Tmean'))),
#          T_min_mean = rowMeans(dplyr::select(clim_wide, starts_with('Tmin'))),
#          T_max_mean = rowMeans(dplyr::select(clim_wide, starts_with('Tmax'))),
#   )

clim_summary = clim_wide |>
  mutate(PPT_total = rowSums(dplyr::select(clim_wide, starts_with('PPT'))),
         PPT_total_prev_tree = rowSums(dplyr::pick('PPT_09', 'PPT_10', 'PPT_11', 'PPT_12')),
         PPT_total_current_tree = rowSums(dplyr::pick('PPT_01', 'PPT_02', 'PPT_03', 'PPT_04', 
                                                      'PPT_05', 'PPT_06', 'PPT_07', 'PPT_08')))

Vpd_sets = list(c("Vpdmin_01", "Vpdmax_01"), c("Vpdmin_02", "Vpdmax_02"),
                c("Vpdmin_03", "Vpdmax_03"), c("Vpdmin_04", "Vpdmax_04"),
                c("Vpdmin_05", "Vpdmax_05"), c("Vpdmin_06", "Vpdmax_06"),
                c("Vpdmin_07", "Vpdmax_07"), c("Vpdmin_08", "Vpdmax_08"),
                c("Vpdmin_09", "Vpdmax_09"), c("Vpdmin_10", "Vpdmax_10"),
                c("Vpdmin_11", "Vpdmax_11"), c("Vpdmin_12", "Vpdmax_12")
)

#writing a loop to calculate vpd mean insert it into clim_summary dataframe 
for (i in seq_along(Vpd_sets)) {
  set <- Vpd_sets[[i]]
  # Add a leading 0 for 1-9, otherwise just use i as is
  clim_summary <- clim_summary %>%
    mutate(!!paste0("Vpdmean_", ifelse(i < 10, paste0("0", i), i)) := rowMeans(select(., all_of(set))))
}


N_years = nrow(clim_summary)

clim_summary$PPT_total_tree = NA
clim_summary$PPT_total_tree[2:N_years] = clim_summary$PPT_total_prev_tree[1:(N_years-1)] +
  clim_summary$PPT_total_current_tree[2:N_years]

clim_summary = clim_summary[, !(colnames(clim_summary) %in% c('PPT_total_prev_tree', 'PPT_total_current_tree', 'PPT_total'))]

clim_summary$year <- as.numeric(clim_summary$year)

#joiing climate data with biomass data, climate data still in wide format
#clim_summary is year, site, all climate data in wide format 
clim_agb <- all_site_summary|>
  left_join(clim_summary, by = c('year', 'site'))

#melting climate data
clim_melt = melt(clim_summary,
                id.vars = c('year', 'site'))
clim_melt$year = as.integer(clim_melt$year)
summary(clim_melt)


#dataframe with AGB data (no taxon) + climate data in melted format 
#AGBI.mean represents each site 
clim_total <- all_site_summary|>
  left_join(clim_melt, by = c('year', 'site'))
head(clim_total)
summary(clim_melt)

#biomass data with taxon data and climate in melted format
#AGBI.mean is for each taxon at a site 
clim_taxon <- all_taxon_summary|>
  left_join(clim_melt, by = c('year', 'site')) %>% 
  select(-site)
head(clim_taxon)
summary(clim_melt)

#annual_vars = select(clim_agb, year, AGBI.mean, site,
#                     yearly_meanT, PPT_total_tree, T_min_mean, T_max_mean)
annual_vars = select(clim_agb, year, AGBI.mean, site, PPT_total_tree)
annual_vars_melt = melt(annual_vars, id.vars = c('model', 'year', 'AGBI.mean', 'site'))

##################################################################################
#separating climate variables into separate dataframes 
############PPT##################################################
#ppt data with AGBI.mean at the site level
ppt = select(clim_agb, year, AGBI.mean, site,
             starts_with('PPT'))
colnames(ppt)[which(names(ppt) == "AGBI.mean")] <- "AGBI.mean.site"

#melt at the site level
ppt_melt = melt(ppt, id.vars = c('model', 'year', 'AGBI.mean.site', 'site'))
ppt_melt$site = factor(ppt_melt$site, levels = c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA', 'HARVARD'))

#ppt at the taxon level 
ppt_taxon = all_taxon_summary %>% 
  left_join(ppt, by = c('year', 'site', 'model'))

#melt at the taxon level 
ppt_melt_taxon = melt(ppt_taxon, id.vars = c('model', 'year', 'AGBI.mean','AGBI.mean.site', 'site','taxon', 'AGB.mean',
                                             'AGB.sd','AGB.lo','AGB.hi','AGBI.sd',
                                             'AGBI.lo','AGBI.hi'))


##########VPD#########################################################
vpd = select(clim_agb, year, AGBI.mean, site,
             starts_with('Vpd'))
colnames(vpd)[which(names(vpd) == "AGBI.mean")] <- "AGBI.mean.site"

#vpd at the site level      
vpd_melt = melt(vpd, 
                id.vars = c('model', 'year', 'AGBI.mean.site', 'site'))
vpd_melt$site = factor(vpd_melt$site, levels = c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA', 'HARVARD'))

vpd_taxon = all_taxon_summary %>% 
  left_join(vpd, by = c('year', 'site', 'model'))

#vpd at the taxon level 
vpd_melt_taxon = melt(vpd_taxon, id.vars = c('model', 'year', 'AGBI.mean', 'AGBI.mean.site', 'site','taxon', 'AGB.mean',
                                             'AGB.sd','AGB.lo','AGB.hi','AGBI.sd',
                                             'AGBI.lo','AGBI.hi'))

# vpd_melt$variable = sapply(as.vector(vpd_melt$variable), function(x) {strsplit(x, '\\_')[[1]][2]})



######TMIN and TMAX#############################################################
tmin = select(clim_agb, year, AGBI.mean, site,
              starts_with('Tmin'))
colnames(tmin)[which(names(tmin) == "AGBI.mean")] <- "AGBI.mean.site"

tmin_melt = melt(tmin,
                 id.vars = c('model', 'year', 'AGBI.mean.site', 'site'))
tmin_melt$site = factor(tmin_melt$site, levels = c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA', 'HARVARD'))

tmin_taxon = all_taxon_summary %>% 
  left_join(tmin, by = c('year', 'site', 'model'))

tmin_melt_taxon = melt(tmin_taxon, id.vars = c('model', 'year', 'AGBI.mean', 'AGBI.mean.site', 'site','taxon', 'AGB.mean',
                                             'AGB.sd','AGB.lo','AGB.hi','AGBI.sd',
                                             'AGBI.lo','AGBI.hi'))



tmax = select(clim_agb, year, AGBI.mean, site,
              starts_with('Tmax'))
colnames(tmax)[which(names(tmax) == "AGBI.mean")] <- "AGBI.mean.site"

tmax_melt = melt(tmax, 
                 id.vars = c('model', 'year', 'AGBI.mean.site', 'site'))
tmax_melt$site = factor(tmax_melt$site, levels = c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA', 'HARVARD'))

tmax_taxon = all_taxon_summary %>% 
  left_join(tmax, by = c('year', 'site', 'model'))

tmax_melt_taxon = melt(tmax_taxon, id.vars = c('model', 'year', 'AGBI.mean', 'AGBI.mean.site', 'site','taxon', 'AGB.mean',
                                               'AGB.sd','AGB.lo','AGB.hi','AGBI.sd',
                                               'AGBI.lo','AGBI.hi'))


tmean = select(clim_agb, year, AGBI.mean, site,
               starts_with('Tmean'))
colnames(tmean)[which(names(tmean) == "AGBI.mean")] <- "AGBI.mean.site"


tmean_melt = melt(tmean, 
                  id.vars = c('model', 'year', 'AGBI.mean.site', 'site'))

tmean_melt$site = factor(tmean_melt$site, levels = c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA', 'HARVARD'))

tmean_taxon = all_taxon_summary %>% 
  left_join(tmean, by = c('year', 'site', 'model'))

tmean_melt_taxon = melt(tmean_taxon, id.vars = c('model', 'year', 'AGBI.mean', 'AGBI.mean.site', 'site','taxon', 'AGB.mean',
                                               'AGB.sd','AGB.lo','AGB.hi','AGBI.sd',
                                               'AGBI.lo','AGBI.hi'))


######################################################################################################################
#correlation between tree ring data and climate variables
######################################################################################################################
# clim_total = clim_total

clim_total = na.omit(clim_total)

# clim_total = clim_total[which(clim_total$site != 'HARVARD'),]
# clim_taxon = clim_taxon[which(clim_taxon$site != 'HARVARD'),]
clim_total$variable = as.character(clim_total$variable)
clim_total$type = sapply(strsplit(clim_total$variable, split='_'), function(x) x[1])
clim_total$period = sapply(strsplit(clim_total$variable, split='_'), function(x) x[2])


periods = unique(clim_total$period)
period_names = c('jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 
                 'aug', 'sep', 'oct', 'nov', 'dec', 'total tree') 

clim_total$period_names = period_names[match(clim_total$period, periods)]
clim_total$period_names = factor(clim_total$period_names,
                                            levels = period_names)


clim_taxon$variable = as.character(clim_taxon$variable)
clim_taxon$type = sapply(strsplit(clim_taxon$variable, split='_'), function(x) x[1])
clim_taxon$period = sapply(strsplit(clim_taxon$variable, split='_'), function(x) x[2])

periods = unique(clim_taxon$period)


clim_taxon$period_names = period_names[match(clim_taxon$period, periods)]
clim_taxon$period_names = factor(clim_taxon$period_names,
                                            levels = period_names)


#correlation between AGBI and all climate variables, site level
cor_clim_AGBI <- clim_total %>%
  # Filter to keep only the relevant rows for correlation
  filter(str_detect(variable, "^(PPT|Tmean|Tmax|Tmin|Vpdmin|Vpdmax|Vpdmean)")) %>%
  # Group by site and variable
  group_by(site, variable, type, period, period_names) %>%
  # Summarize by calculating correlation between AGBI.mean and value
  summarize(correlation = cor(AGBI.mean, value, use = "complete.obs"), .groups = 'drop')
head(cor_clim_AGBI)
write.csv(cor_clim_AGBI, file = "report/AGBI_clim_correlation_site.csv")

#correlation between AGBI and clim with pvalues, only at the site level
cor_clim_AGBI_site_pvalue <- clim_total %>%
  # Filter to keep only the relevant rows for correlation
  filter(str_detect(variable, "^(PPT|Tmean|Tmax|Tmin|Vpdmin|Vpdmax|Vpdmean)")) %>%
  # Group by site and variable
  group_by(site, variable, type, period, period_names) %>%
  # Summarize by calculating correlation between AGBI.mean and value
  summarize(correlation = cor.test(AGBI.mean, value, use = "complete.obs")$estimate,
            p_value = cor.test(AGBI.mean, value, use = "complete.obs")$p.value, .groups = 'drop')

cor_clim_p_site_subset = subset(cor_clim_AGBI_site_pvalue, p_value < 0.05)


#for each climate variable at which site is it the highest
max_variable_cor = cor_clim_AGBI %>% 
  group_by(variable) %>%
  summarize(max_cor = max(correlation, na.rm = TRUE), 
            site_with_max_cor = site[which.max(correlation)])


#calculating correlation between AGBI.mean and climate variables for each 
#climate variable at each site, for each taxon
cor_clim_vars_taxon <- clim_taxon %>%
  # Filter to keep only the relevant rows for correlation
  filter(str_detect(variable, "^(PPT|Tmean|Tmax|Tmin|Vpdmin|Vpdmax|Vpdmean)")) %>%
  # Group by site and variable
  group_by(site, taxon, variable, type, period, period_names) %>%
  # Summarize by calculating correlation between AGBI.mean and value
  summarize(correlation = cor(AGBI.mean, value, use = "complete.obs"), .groups = 'drop')
write.csv(cor_clim_vars_taxon, file = "report/AGBI_clim_taxon_correlation.csv")


#generating the pvalues of the correlation between AGBI.mean and climate variables 
#at the taxon level
cor_clim_taxon_pvalue <- clim_taxon %>%
  # Filter to keep only the relevant rows for correlation
  filter(str_detect(variable, "^(PPT|Tmean|Tmax|Tmin|Vpdmin|Vpdmax|Vpdmean)")) %>%
  # Group by site and variable
  group_by(site, taxon, variable, type, period, period_names) %>%
  # Summarize by calculating correlation between AGBI.mean and value
  summarize(correlation = cor.test(AGBI.mean, value, use = "complete.obs")$estimate,
            p_value = cor.test(AGBI.mean, value, use = "complete.obs")$p.value, .groups = 'drop')

#subsetting data set to only have pvalues <0.05  
cor_clim_p_taxon_subset = subset(cor_clim_taxon_pvalue, p_value < 0.05)
head(cor_clim_p_taxon_subset)



clim_vars = c("PPT", "Tmean", "Tmin", "Tmax", "Vpdmin", "Vpdmax", "Vpdmean")

sites = c('GOOSE', 'ROOSTER', 'NRP', 'HARVARD', 'SYLVANIA')

cor_max = max(cor_clim_taxon_pvalue$correlation)
cor_min = min(cor_clim_taxon_pvalue$correlation)

#generatting correlation plots for all taxa for all sites for all clim variables
#with significance shown 
# Open a PDF device
pdf("report/figures/AGBI_clim_cor_taxon.pdf", width = 10, height = 8)

for (site in sites) {
# Loop through each climate variable
for (var in clim_vars) {
  
  # Generate the plot for the current climate variable
  cor_taxa_p = cor_clim_taxon_pvalue[which((cor_clim_taxon_pvalue$site == site )&(cor_clim_taxon_pvalue$type == var)),]
  cor_taxa_p$sig = ifelse(cor_taxa_p$p_value<0.05, TRUE, NA)
  
 p = ggplot()+
    geom_tile(data= cor_taxa_p, aes(x=period_names, y= taxon, fill = correlation))+
    scale_fill_gradient2(limits = c(-0.6, 0.6), 
                         low = "red", mid = "white", high = "blue", 
                         midpoint = 0)+
    geom_point(data = cor_taxa_p, aes(x=period_names, y= taxon, shape = sig), size=3)+
    scale_shape_manual(values=c(1, NA)) + 
   xlab('Period') +
   ylab('Species') + 
   ggtitle(paste0(site, '; ', var)) + 
   theme(plot.title = element_text(size=18))

  
  
  # Print the plot to the PDF
  print(p)
}}
# Close the PDF device
dev.off()



#plotting y=site x= period (jan, feb, etc..) where each page shows each climate variable
#correlation of each site between the different climate variables 
pdf("report/figures/AGBI_clim_cor_sites.pdf", width = 10, height = 8)
  # Loop through each climate variable
for (var in clim_vars) {
  
  # Filter data for the current climate variable
  cor_var_p <- cor_clim_AGBI_site_pvalue[which(cor_clim_AGBI_site_pvalue$type == var), ]
  cor_var_p$sig = ifelse(cor_var_p$p_value<0.05, TRUE, NA)
  
  # Generate the plot for the current climate variable
  p <- ggplot() +
    geom_tile(data = cor_var_p, aes(x = period_names, y = site, fill = correlation)) +
    scale_fill_gradient2(limits = c(-0.6, 0.6),
                         low = "red", mid = "white", high = "purple",
                         midpoint = 0) +
    geom_point(data = cor_var_p, aes(x= period_names, y= site, shape = sig), size=3)+
    scale_shape_manual(values=c(1, NA)) + 
    xlab("Period") +
    ylab('Site') +
    ggtitle(paste0("Climate Variable: ", var)) +
    theme(plot.title = element_text(size = 18))
  
  # Print the plot (one plot per climate variable per page)
  print(p)
    
  }
dev.off()



# all_taxon_summary %>% 
#   group_by(site) %>%
#   summarize(year_max = max(year)) 
# 
# agbi_recent = all_taxon_summary %>% 
#   group_by(site, taxon) %>%
#   filter(year == max(year)) 

#pulling data from the year 2010
#making sure there is data for all sites at this time 
agbi_recent = all_taxon_summary %>% 
  group_by(site, taxon) %>%
  filter(year == 2010) 

#calculating the cumulative sum of the taxon at each site
agbi_cumsum = agbi_recent %>% 
  #group_by(site, model) %>%
  dplyr::arrange(site, desc(AGBI.mean)) %>%
  group_by(site) %>%
  mutate(cum_sum = cumsum(AGBI.mean) / sum(AGBI.mean)) %>% 
  ungroup()


agbi_cumsum_filter = agbi_cumsum %>% 
  filter(cum_sum < 0.95)

agbi_cumsum_filter %>% 
  pivot_wider(names_from = 'site', values_from = 'AGBI.mean')


df2 = inner_join(clim_taxon, agbi_cumsum_filter[,c('site', 'taxon', 'cum_sum')], by = c('site', 'taxon'))

#generating the pvalues of the correlation between AGBI.mean and climate variables 
cor_clim_vars_taxon_filter <- df2 %>%
  # Filter to keep only the relevant rows for correlation
  filter(str_detect(variable, "^(PPT|Tmean|Tmax|Tmin|Vpdmin|Vpdmax|Vpdmean)")) %>%
  # Group by site and variable
  group_by(site, taxon, variable,type, period, period_names) %>%
  # Summarize by calculating correlation between AGBI.mean and value
  summarize(correlation = cor.test(AGBI.mean, value, use = "complete.obs")$estimate,
            p_value = cor.test(AGBI.mean, value, use = "complete.obs")$p.value, .groups = 'drop')



cor_clim_filter_subset = subset(cor_clim_vars_taxon_filter, p_value < 0.05)
head(cor_clim_filter_subset)

#plotting the correlation between taxa and AGBI only for cumsum<0.95  
# Open a PDF device
pdf("report/figures/AGBI_clim_cor_sites_cumsum.pdf", width = 10, height = 8)

for (site in sites) {
  
  
  # Loop through each climate variable
  for (var in clim_vars) {
    
    # Generate the plot for the current climate variable
    cor_taxa_p = cor_clim_vars_taxon_filter[which((cor_clim_vars_taxon_filter$site == site )&(cor_clim_vars_taxon_filter$type == var)),]
    cor_taxa_p$sig = ifelse(cor_taxa_p$p_value<0.05, TRUE, NA)
    
    p = ggplot()+
      geom_tile(data= cor_taxa_p, aes(x=period_names, y= taxon, fill = correlation))+
      scale_fill_gradient2(limits = c(-0.6, 0.6), 
                           low = "red", mid = "white", high = "blue", 
                           midpoint = 0)+
      geom_point(data = cor_taxa_p, aes(x=period_names, y= taxon, shape = sig), size=3)+
      scale_shape_manual(values=c(1, NA)) + 
      xlab('Period') +
      ylab('Species') + 
      ggtitle(paste0(site, '; ', var)) + 
      theme(plot.title = element_text(size=18))
    
    
    
    # Print the plot to the PDF
    print(p)
  }}
# Close the PDF device
dev.off()


#plotting the taxa that occur at the same sites 
taxa_filtered = unique(cor_clim_vars_taxon_filter$taxon)

pdf("report/figures/AGBI_clim_cor_taxa_filter.pdf", width = 10, height = 8)

for (taxon in taxa_filtered) {
  
  
  # Loop through each climate variable
  for (var in clim_vars) {
    
    # Generate the plot for the current climate variable
    cor_taxa_p = cor_clim_vars_taxon_filter[which((cor_clim_vars_taxon_filter$taxon == taxon)&(cor_clim_vars_taxon_filter$type == var)),]
    cor_taxa_p$sig = ifelse(cor_taxa_p$p_value<0.05, TRUE, NA)
    
    p = ggplot()+
      geom_tile(data= cor_taxa_p, aes(x=period_names, y= site, fill = correlation))+
      scale_fill_gradient2(limits = c(-0.6, 0.6), 
                           low = "red", mid = "white", high = "blue", 
                           midpoint = 0)+
      geom_point(data = cor_taxa_p, aes(x=period_names, y= site, shape = sig), size=3)+
      scale_shape_manual(values=c(1, NA)) + 
      xlab('Period') +
      ylab('site') + 
      ggtitle(paste0(taxon, '; ', var)) + 
      theme(plot.title = element_text(size=18))
    
    
    
    # Print the plot to the PDF
    print(p)
  }}
# Close the PDF device
dev.off()




##################################################################################################
#plotting climate variables over time 
###################################################################################################

###################plotting AGBI.mean vs ppt
var_names = unique(ppt_melt_taxon$variable)
N_vars = length(var_names)
#open a pdf device 
pdf("ppt_output_plots.pdf")
# Loop through each variable
for (i in 1:N_vars) {
  
  # Filter the data for the current variable
  ppt_melt_taxon_variable <- ppt_melt_taxon[which(ppt_melt_taxon$variable == var_names[i]),]
  
  # Generate the plot
  p <- ggplot(data = ppt_melt_taxon_variable) +
    geom_point(aes(x = value, y = AGBI.mean, color = site)) +
    geom_smooth(aes(x = value, y = AGBI.mean, color = site), method = 'lm', formula = y ~ x) +
    facet_wrap(~taxon, scales = 'free')+
    ggtitle(paste("Variable:", var_names[i]))
  
  # Print the plot to the PDF
  print(p)
}
# Close the PDF device
dev.off()

################### AGBI.mean vs. vpd 
var_names = unique(vpd_melt_taxon$variable)
N_vars = length(var_names)
#open a pdf device 
pdf("vpd_output_plots.pdf")
# Loop through each variable
for (i in 1:N_vars) {
  
  # Filter the data for the current variable
  vpd_melt_taxon_variable <- vpd_melt_taxon[which(vpd_melt_taxon$variable == var_names[i]),]
  
  # Generate the plot
  p <- ggplot(data = vpd_melt_taxon_variable) +
    geom_point(aes(x = value, y = AGBI.mean, color = site)) +
    geom_smooth(aes(x = value, y = AGBI.mean, color = site), method = 'lm', formula = y ~ x) +
    facet_wrap(~taxon, scales = 'free')+
    ggtitle(paste("Variable:", var_names[i]))
  
  # Print the plot to the PDF
  print(p)
}
# Close the PDF device
dev.off()



ggplot(data = ppt_melt)+
  geom_line(aes(x = year, y = value, color = site))+
  facet_wrap(~variable, scales = "free_y")
ggsave("figures1950/PPT_over_time.jpg")

#correlation between PPT and AGBI
cor_PPT = ppt_melt %>%
  # Filter to keep only the relevant rows for correlation
  filter(variable == "PPT_total_tree") %>%
  # Group by site
  group_by(site) %>%
  # Summarize by calculating correlation between AGBI.mean and value (assuming 'value' holds the PPT_total_tree data)
  summarize(correlation = cor(AGBI.mean, value, use = "complete.obs"))
head(cor_PPT)

#ggcorrplot(cor_PPT, method = "square", type = "lower", hc.order = FALSE)

ppt_melt  %>% 
  group_by(variable) %>%
  correlation(method = "spearman")



#without taxon
ggplot(data = clim_agb) +
  geom_point(aes(x = yearly_meanT, y = AGBI.mean)) +
  geom_smooth(aes(x = yearly_meanT, y = AGBI.mean), method='lm', formula= y~x)+
  # facet_wrap(~site, scales = 'free')+
  xlab('Mean annual temperature') + 
  ylab('Aboveground biomass increment')
ggsave("figures1950/AGBI_vs_meantemp.jpg")


ggplot(data = clim_agb) +
  geom_point(aes(x = yearly_meanT, y = AGBI.mean)) +
  geom_smooth(aes(x = yearly_meanT, y = AGBI.mean), method='lm', formula= y~x)+
  facet_wrap(~site, scales = 'free')+
  xlab('Mean annual temperature') + 
  ylab('Aboveground biomass increment')
ggsave("figures1950/AGBI_temp_site.jpg")

ggplot(data = clim_agb) +
  geom_point(aes(x = PPT_total, y = AGBI.mean)) +
  geom_smooth(aes(x = PPT_total, y = AGBI.mean), method='lm', formula= y~x)+
  xlab('Mean annual precipitation') + ylab('Aboveground biomass increment')
ggsave("figures1950/AGBI_meanprecip.png")

ggplot(data = clim_agb) +
  geom_point(aes(x = PPT_total, y = AGBI.mean)) +
  geom_smooth(aes(x = PPT_total, y = AGBI.mean), method='lm', formula= y~x)+
  facet_wrap(~site, scales = "free")+
  xlab('Mean annual precipitation') + 
  ylab('Aboveground biomass increment')
ggsave("figures1950/AGBI_meanprecip_site.png")


# by month
ggplot(data = clim_agb) +
  geom_point(aes(x = PPT_total_tree, y = AGBI.mean)) +
  geom_smooth(aes(x = PPT_total_tree, y = AGBI.mean), method='lm', formula= y~x)+
  xlab('PPT_total_tree') +
  ylab('Aboveground biomass increment')
ggsave("figures1950/AGBI_ppt_total_tree.png")
# 
ggplot(data = clim_agb) +
  geom_point(aes(x = PPT_total_tree, y = AGBI.mean)) +
  geom_smooth(aes(x = PPT_total_tree, y = AGBI.mean), method='lm', formula= y~x) +
  facet_wrap(~site, scales = "free") +
  xlab('PPT_total_tree') +
  ylab('Aboveground biomass increment')
ggsave("figures1950/AGBI_ppt_total_tree_site.png")


#PPT
ggplot(data = ppt_melt) +
  geom_point(aes(x = value, y = AGBI.mean)) +
  geom_smooth(aes(x = value, y = AGBI.mean), method='lm', formula= y~x) +  
  facet_wrap(~variable, scales = "free") +
  xlab('PPT') + 
  ylab('Aboveground biomass increment')
ggsave("figures1950/AGBI_PPT_monthly.jpg")

ggplot(data = ppt_melt) +
  geom_point(aes(x = value, y = AGBI.mean, color = site)) +
  geom_smooth(aes(x = value, y = AGBI.mean, color = site), method='lm', formula= y~x) +  
  facet_wrap(~variable, scales = "free") +
  xlab('PPT') + 
  ylab('Aboveground biomass increment')
ggsave("figures1950/AGBI_PPT_monthly_site.jpg")

## VPD by month
ggplot(data = vpd_melt) +
  geom_point(aes(x = value, y = AGBI.mean)) +
  geom_smooth(aes(x = value, y = AGBI.mean), method='lm', formula= y~x) +  
  facet_wrap(~variable, scales = "free") +
  xlab('VPD') + 
  ylab('Aboveground biomass increment')
ggsave("figures1950/AGBI_VPD_monthly.jpg")

ggplot(data = vpd_melt) +
  geom_point(aes(x = value, y = AGBI.mean, colour=site)) +
  geom_smooth(aes(x = value, y = AGBI.mean), method='lm', formula= y~x) +  
  facet_wrap(~variable, scales = "free") +
  xlab('VPD') + 
  ylab('Aboveground biomass increment')
ggsave("figures1950/AGBI_PPT_monthly_site.jpg")

ggplot(data = vpd_melt) +
  geom_point(aes(x = value, y = AGBI.mean, colour=site)) +
  geom_smooth(aes(x = value, y = AGBI.mean, colour = site), method='lm', formula= y~x) +  
  facet_wrap(~variable, scales = "free") +
  xlab('VPD') + 
  ylab('Aboveground biomass increment')
ggsave("figures1950/AGBI_PPT_monthly_site.jpg")

#not a great figure
# ggplot(data = vpd_melt) +
#   geom_point(aes(x = value, y = AGBI.mean, colour=site)) +
#   geom_smooth(aes(x = value, y = AGBI.mean), method='lm', formula= y~x) +  
#   facet_grid(variable~site, scales = "free") +
#   xlab('VPD') + 
#   ylab('Aboveground biomass increment')


## TMIN by month
ggplot(data = tmin_melt) +
  geom_point(aes(x = value, y = AGBI.mean)) +
  geom_smooth(aes(x = value, y = AGBI.mean), method='lm', formula= y~x) +  
  facet_wrap(~variable, scales = "free") +
  xlab('TMIN') + 
  ylab('Aboveground biomass increment')
ggsave("figures1950/AGBI_Tmin_monthly.jpg")


ggplot(data = tmin_melt) +
  geom_point(aes(x = value, y = AGBI.mean, colour=site)) +
  geom_smooth(aes(x = value, y = AGBI.mean, colour=site), method='lm', formula= y~x) +  
  facet_wrap(~variable, scales = "free") +
  xlab('TMIN') + 
  ylab('Aboveground biomass increment')
ggsave("figures1950/AGBI_Tmin_monthly_site.jpg")


#not a great figure
#site vs ABGI separted by month
# ggplot(data = tmin_melt) +
#   geom_point(aes(x = value, y = AGBI.mean, colour=site)) +
#   geom_smooth(aes(x = value, y = AGBI.mean), method='lm', formula= y~x) +  
#   facet_grid(variable~site, scales = "free") +
#   xlab('TMIN') + 
#   ylab('Aboveground biomass increment')


## TMAX by month
ggplot(data = tmax_melt) +
  geom_point(aes(x = value, y = AGBI.mean)) +
  geom_smooth(aes(x = value, y = AGBI.mean), method='lm', formula= y~x) +  
  facet_wrap(~variable, scales = "free") +
  xlab('TMAX') + 
  ylab('Aboveground biomass increment')
ggsave("figures1950/AGBI_Tmax_monthly.jpg")


# ggplot(data = tmax_melt) +
#   geom_point(aes(x = value, y = AGBI.mean, colour=site)) +
#   geom_smooth(aes(x = value, y = AGBI.mean), method='lm', formula= y~x) +  
#   facet_wrap(~variable, scales = "free") +
#   xlab('TMAX') + 
#   ylab('Aboveground biomass increment')

ggplot(data = tmax_melt) +
  geom_point(aes(x = value, y = AGBI.mean, colour=site)) +
  geom_smooth(aes(x = value, y = AGBI.mean, colour=site), method='lm', formula= y~x) +  
  facet_wrap(~variable, scales = "free") +
  xlab('TMAX') + 
  ylab('Aboveground biomass increment')
ggsave("figures1950/AGBI_Tmax_monthly_site.jpg")


#not great figure
# ggplot(data = tmax_melt) +
#   geom_point(aes(x = value, y = AGBI.mean, colour=site)) +
#   geom_smooth(aes(x = value, y = AGBI.mean), method='lm', formula= y~x) +  
#   facet_grid(variable~site, scales = "free") +
#   xlab('TMAX') + 
#   ylab('Aboveground biomass increment')

## TMEAN by month
ggplot(data = tmean_melt) +
  geom_point(aes(x = value, y = AGBI.mean)) +
  geom_smooth(aes(x = value, y = AGBI.mean), method='lm', formula= y~x) +  
  facet_wrap(~variable, scales = "free") +
  xlab('TMEAN') + 
  ylab('Aboveground biomass increment')
ggsave("figures1950/AGBI_Tmean_monthly.jpg")


ggplot(data = tmean_melt) +
  geom_point(aes(x = value, y = AGBI.mean, colour=site)) +
  geom_smooth(aes(x = value, y = AGBI.mean), method='lm', formula= y~x) +  
  facet_wrap(~variable, scales = "free") +
  xlab('TMEAN') + 
  ylab('Aboveground biomass increment')
ggsave("figures1950/AGBI_Tmean_monthly_site.jpg")

ggplot(data = tmean_melt) +
  geom_point(aes(x = value, y = AGBI.mean, colour=site)) +
  geom_smooth(aes(x = value, y = AGBI.mean, colour=site), method='lm', formula= y~x) +  
  facet_wrap(~variable, scales = "free") +
  xlab('TMEAN') + 
  ylab('Aboveground biomass increment')
ggsave("figures1950/AGBI_Tmean_monthly_site.jpg")

#not a great figure
# ggplot(data = tmean_melt) +
#   geom_point(aes(x = value, y = AGBI.mean, colour=site)) +
#   geom_smooth(aes(x = value, y = AGBI.mean), method='lm', formula= y~x) +  
#   facet_grid(variable~site, scales = "free") +
#   xlab('TMEAN') + 
#   ylab('Aboveground biomass increment')


## ANNUAL vars by month
ggplot(data = annual_vars_melt) +
  geom_point(aes(x = value, y = AGBI.mean)) +
  geom_smooth(aes(x = value, y = AGBI.mean), method='lm', formula= y~x) +  
  facet_wrap(~variable, scales = "free") +
  xlab('temp or precip') + 
  ylab('Aboveground biomass increment')
ggsave("figures1950/AGBI_annual_vars.jpg")

#same as figure below only one y=x line vs one for each site
# ggplot(data = annual_vars_melt) +
#   geom_point(aes(x = value, y = AGBI.mean, colour=site)) +
#   geom_smooth(aes(x = value, y = AGBI.mean), method='lm', formula= y~x) +  
#   facet_wrap(~variable, scales = "free") +
#   xlab('temp or precip') + 
#   ylab('Aboveground biomass increment')

ggplot(data = annual_vars_melt) +
  geom_point(aes(x = value, y = AGBI.mean, colour=site)) +
  geom_smooth(aes(x = value, y = AGBI.mean, colour=site), method='lm', formula= y~x) +  
  facet_wrap(~variable, scales = "free") +
  xlab('temp or precip') + 
  ylab('Aboveground biomass increment')
ggsave("figures1950/AGBI_annual_vars_site.jpg")



##############PLOTTING PPT WITH TAXON ##############################################################
#organizes sites on one page but generates a page for every taxon for EVERY variable ie. PPT
unique_combinations <- ppt_melt_taxon %>%
  distinct(variable, taxon)

# Open a PDF device
pdf("report/figures/AGBI_vs_Value_by_Variable_Taxon_Site.pdf", width = 10, height = 8)

# Loop through each combination of variable and taxon
for (i in 1:nrow(unique_combinations)) {
  
  # Get the current combination
  current_combination <- unique_combinations[i, ]
  
  # Filter the data for the current combination (no site filtering here)
  df_filtered <- ppt_melt_taxon %>%
    filter(variable == current_combination$variable, 
           taxon == current_combination$taxon)
  
  # Generate the plot with all sites on the same page
  p <- ggplot(df_filtered, aes(x = value, y = AGBI.mean, color = site)) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ x) +
    facet_wrap(~site) +  # Separate plots by site on the same page
    ggtitle(paste("AGBI.mean vs", current_combination$variable, "for", current_combination$taxon)) +
    xlab(paste(current_combination$variable, "Value")) +
    ylab("AGBI.mean") +
    theme_minimal()
  
  # Print the plot to the PDF
  print(p)
}

# Close the PDF device
dev.off()


#plotting AGBI.mean vs PPT for each taxa at a given site 
unique_combinations <- ppt_melt_taxon %>%
  distinct(variable, site)

# Open a PDF device
pdf("report/figures/AGBI_vs_PPT_Site_Taxon.pdf", width = 10, height = 8)

# Loop through each combination of variable and site
for (i in 1:nrow(unique_combinations)) {
  
  # Get the current combination
  current_combination <- unique_combinations[i, ]
  
  # Filter the data for the current combination (variable and site)
  df_filtered <- ppt_melt_taxon %>%
    filter(variable == current_combination$variable, 
           site == current_combination$site)
  
  # Generate the plot with all taxa on the same page for the current site
  p <- ggplot(df_filtered, aes(x = value, y = AGBI.mean, color = taxon)) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ x) +
    facet_wrap(~taxon, scales = 'free_y') +  # Separate plots by taxon on the same page
    ggtitle(paste("AGBI.mean vs", current_combination$variable, "at", current_combination$site)) +
    xlab(paste(current_combination$variable, "Value")) +
    ylab("AGBI.mean") +
    theme_minimal()
  
  # Print the plot to the PDF
  print(p)
}
# Close the PDF device
dev.off()



##plotting AGBI.mean vs PPT for every taxon at a given site, where all variables 
#of PPT are on page 
unique_combinations <- ppt_melt_taxon %>%
  distinct(site, taxon)

# Open a PDF device
pdf("report/figures/AGBI_vs_PPT_by_page_Site_Taxon_Variable.pdf", width = 14, height = 10)

# Loop through each combination of site and taxon
for (i in 1:nrow(unique_combinations)) {
  
  # Get the current combination
  current_combination <- unique_combinations[i, ]
  
  # Filter the data for the current combination (site and taxon)
  df_filtered <- ppt_melt_taxon %>%
    filter(site == current_combination$site, 
           taxon == current_combination$taxon)
  
  # Generate the plot with all variables on the same page for the current site and taxon
  p <- ggplot(df_filtered, aes(x = value, y = AGBI.mean, color = variable)) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ x) +
    facet_wrap(~variable, scales = 'free_x') +  # Separate plots by variable on the same page
    ggtitle(paste("AGBI.mean vs Climate Precipitation for", current_combination$taxon, "at", current_combination$site)) +
    xlab("Climate Variable Value") +
    ylab("AGBI.mean") +
    theme_minimal()
  
  # Print the plot to the PDF
  print(p)
}
# Close the PDF device
dev.off()


###########PLOTTING VPD WITH TAXA###################################3

#organizes sites on one page but generates a page for every taxon for EVERY variable
unique_combinations <- vpd_melt_taxon %>%
  distinct(variable, taxon)

# Open a PDF device
pdf("report/figures/AGBI_vs_Value_by_vpd_Taxon_Site.pdf", width = 10, height = 8)

# Loop through each combination of variable and taxon
for (i in 1:nrow(unique_combinations)) {
  
  # Get the current combination
  current_combination <- unique_combinations[i, ]
  
  # Filter the data for the current combination (no site filtering here)
  df_filtered <- vpd_melt_taxon %>%
    filter(variable == current_combination$variable, 
           taxon == current_combination$taxon)
  
  # Generate the plot with all sites on the same page
  p <- ggplot(df_filtered, aes(x = value, y = AGBI.mean, color = site)) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ x) +
    facet_wrap(~site) +  # Separate plots by site on the same page
    ggtitle(paste("AGBI.mean vs", current_combination$variable, "for", current_combination$taxon)) +
    xlab(paste(current_combination$variable, "Value")) +
    ylab("AGBI.mean") +
    theme_minimal()
  
  # Print the plot to the PDF
  print(p)
}
# Close the PDF device
dev.off()



#plotting AGBI.mean vs VPD for each taxa at a given site 
unique_combinations <- vpd_melt_taxon %>%
  distinct(variable, site)

# Open a PDF device
pdf("report/figures/AGBI_vs_VPD_Site_Taxon.pdf", width = 10, height = 8)

# Loop through each combination of variable and site
for (i in 1:nrow(unique_combinations)) {
  
  # Get the current combination
  current_combination <- unique_combinations[i, ]
  
  # Filter the data for the current combination (variable and site)
  df_filtered <- vpd_melt_taxon %>%
    filter(variable == current_combination$variable, 
           site == current_combination$site)
  
  # Generate the plot with all taxa on the same page for the current site
  p <- ggplot(df_filtered, aes(x = value, y = AGBI.mean, color = taxon)) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ x) +
    facet_wrap(~taxon, scales = 'free_y') +  # Separate plots by taxon on the same page
    ggtitle(paste("AGBI.mean vs", current_combination$variable, "at", current_combination$site)) +
    xlab(paste(current_combination$variable, "Value")) +
    ylab("AGBI.mean") +
    theme_minimal()
  
  # Print the plot to the PDF
  print(p)
}
# Close the PDF device
dev.off()


#######all variables on one page.....
unique_combinations <- vpd_melt_taxon %>%
  distinct(site, taxon)

# Open a PDF device
pdf("report/figures/AGBI_vs_VPD_by_page_Site_Taxon_Variable.pdf", width = 14, height = 10)

# Loop through each combination of site and taxon
for (i in 1:nrow(unique_combinations)) {
  
  # Get the current combination
  current_combination <- unique_combinations[i, ]
  
  # Filter the data for the current combination (site and taxon)
  df_filtered <- vpd_melt_taxon %>%
    filter(site == current_combination$site, 
           taxon == current_combination$taxon)
  
  # Generate the plot with all variables on the same page for the current site and taxon
  p <- ggplot(df_filtered, aes(x = value, y = AGBI.mean, color = variable)) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ x) +
    facet_wrap(~variable, scales = 'free_x') +  # Separate plots by variable on the same page
    ggtitle(paste("AGBI.mean vs Vapor pressure deficit for", current_combination$taxon, "at", current_combination$site)) +
    xlab("Climate Variable Value") +
    ylab("AGBI.mean") +
    theme_minimal()
  
  # Print the plot to the PDF
  print(p)
}

# Close the PDF device
dev.off()

###########PLOTTING VPD WITH TAXA###################################3

#organizes sites on one page but generates a page for every taxon for EVERY variable
unique_combinations <- vpd_melt_taxon %>%
  distinct(variable, taxon)

# Open a PDF device
pdf("report/figures/AGBI_vs_Value_by_vpd_Taxon_Site.pdf", width = 10, height = 8)

# Loop through each combination of variable and taxon
for (i in 1:nrow(unique_combinations)) {
  
  # Get the current combination
  current_combination <- unique_combinations[i, ]
  
  # Filter the data for the current combination (no site filtering here)
  df_filtered <- vpd_melt_taxon %>%
    filter(variable == current_combination$variable, 
           taxon == current_combination$taxon)
  
  # Generate the plot with all sites on the same page
  p <- ggplot(df_filtered, aes(x = value, y = AGBI.mean, color = site)) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ x) +
    facet_wrap(~site) +  # Separate plots by site on the same page
    ggtitle(paste("AGBI.mean vs", current_combination$variable, "for", current_combination$taxon)) +
    xlab(paste(current_combination$variable, "Value")) +
    ylab("AGBI.mean") +
    theme_minimal()
  
  # Print the plot to the PDF
  print(p)
}
# Close the PDF device
dev.off()



#keeping taxa on one page....?
unique_combinations <- vpd_melt_taxon %>%
  distinct(variable, site)

# Open a PDF device
pdf("report/figures/AGBI_vs_VPD_Site_Taxon.pdf", width = 10, height = 8)

# Loop through each combination of variable and site
for (i in 1:nrow(unique_combinations)) {
  
  # Get the current combination
  current_combination <- unique_combinations[i, ]
  
  # Filter the data for the current combination (variable and site)
  df_filtered <- vpd_melt_taxon %>%
    filter(variable == current_combination$variable, 
           site == current_combination$site)
  
  # Generate the plot with all taxa on the same page for the current site
  p <- ggplot(df_filtered, aes(x = value, y = AGBI.mean, color = taxon)) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ x) +
    facet_wrap(~taxon, scales = 'free_y') +  # Separate plots by taxon on the same page
    ggtitle(paste("AGBI.mean vs", current_combination$variable, "at", current_combination$site)) +
    xlab(paste(current_combination$variable, "Value")) +
    ylab("AGBI.mean") +
    theme_minimal()
  
  # Print the plot to the PDF
  print(p)
}
# Close the PDF device
dev.off()


#######all variables on one page.....
unique_combinations <- vpd_melt_taxon %>%
  distinct(site, taxon)

# Open a PDF device
pdf("report/figures/AGBI_vs_VPD_by_page_Site_Taxon_Variable.pdf", width = 14, height = 10)

# Loop through each combination of site and taxon
for (i in 1:nrow(unique_combinations)) {
  
  # Get the current combination
  current_combination <- unique_combinations[i, ]
  
  # Filter the data for the current combination (site and taxon)
  df_filtered <- vpd_melt_taxon %>%
    filter(site == current_combination$site, 
           taxon == current_combination$taxon)
  
  # Generate the plot with all variables on the same page for the current site and taxon
  p <- ggplot(df_filtered, aes(x = value, y = AGBI.mean, color = variable)) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ x) +
    facet_wrap(~variable, scales = 'free_x') +  # Separate plots by variable on the same page
    ggtitle(paste("AGBI.mean vs Vapor pressure deficit for", current_combination$taxon, "at", current_combination$site)) +
    xlab("Climate Variable Value") +
    ylab("AGBI.mean") +
    theme_minimal()
  
  # Print the plot to the PDF
  print(p)
}

# Close the PDF device
dev.off()



###########PLOTTING TMIN WITH TAXA###################################3

#organizes sites on one page but generates a page for every taxon for EVERY variable
unique_combinations <- tmin_melt_taxon %>%
  distinct(variable, taxon)

# Open a PDF device
pdf("report/figures/AGBI_vs_Value_by_tmin_Taxon_Site.pdf", width = 10, height = 8)

# Loop through each combination of variable and taxon
for (i in 1:nrow(unique_combinations)) {
  
  # Get the current combination
  current_combination <- unique_combinations[i, ]
  
  # Filter the data for the current combination (no site filtering here)
  df_filtered <- tmin_melt_taxon %>%
    filter(variable == current_combination$variable, 
           taxon == current_combination$taxon)
  
  # Generate the plot with all sites on the same page
  p <- ggplot(df_filtered, aes(x = value, y = AGBI.mean, color = site)) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ x) +
    facet_wrap(~site) +  # Separate plots by site on the same page
    ggtitle(paste("AGBI.mean vs", current_combination$variable, "for", current_combination$taxon)) +
    xlab(paste(current_combination$variable, "Value")) +
    ylab("AGBI.mean") +
    theme_minimal()
  
  # Print the plot to the PDF
  print(p)
}
# Close the PDF device
dev.off()



#keeping taxa on one page....?
unique_combinations <- tmin_melt_taxon %>%
  distinct(variable, site)

# Open a PDF device
pdf("report/figures/AGBI_vs_TMIN_Site_Taxon.pdf", width = 10, height = 8)

# Loop through each combination of variable and site
for (i in 1:nrow(unique_combinations)) {
  
  # Get the current combination
  current_combination <- unique_combinations[i, ]
  
  # Filter the data for the current combination (variable and site)
  df_filtered <- tmin_melt_taxon %>%
    filter(variable == current_combination$variable, 
           site == current_combination$site)
  
  # Generate the plot with all taxa on the same page for the current site
  p <- ggplot(df_filtered, aes(x = value, y = AGBI.mean, color = taxon)) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ x) +
    facet_wrap(~taxon, scales = 'free_y') +  # Separate plots by taxon on the same page
    ggtitle(paste("AGBI.mean vs", current_combination$variable, "at", current_combination$site)) +
    xlab(paste(current_combination$variable, "Value")) +
    ylab("AGBI.mean") +
    theme_minimal()
  
  # Print the plot to the PDF
  print(p)
}
# Close the PDF device
dev.off()


#######all variables on one page.....
unique_combinations <- tmin_melt_taxon %>%
  distinct(site, taxon)

# Open a PDF device
pdf("report/figures/AGBI_vs_TMIN_by_page_Site_Taxon_Variable.pdf", width = 14, height = 10)

# Loop through each combination of site and taxon
for (i in 1:nrow(unique_combinations)) {
  
  # Get the current combination
  current_combination <- unique_combinations[i, ]
  
  # Filter the data for the current combination (site and taxon)
  df_filtered <- tmin_melt_taxon %>%
    filter(site == current_combination$site, 
           taxon == current_combination$taxon)
  
  # Generate the plot with all variables on the same page for the current site and taxon
  p <- ggplot(df_filtered, aes(x = value, y = AGBI.mean, color = variable)) +
    geom_point() +
    geom_smooth(method = 'lm', formula = y ~ x) +
    facet_wrap(~variable, scales = 'free_x') +  # Separate plots by variable on the same page
    ggtitle(paste("AGBI.mean vs TMIN for", current_combination$taxon, "at", current_combination$site)) +
    xlab("Climate Variable Value") +
    ylab("AGBI.mean") +
    theme_minimal()
  
  # Print the plot to the PDF
  print(p)
}

# Close the PDF device
dev.off()




#################################################################################
#STATS
#################################################################################
PPT_gam_month = ppt_melt %>% 
  group_by(variable) %>%
  do(tidy(gam(AGBI.mean ~ s(value), ., family  = Gamma(link = "identity"))))

PPT_gam_slope_month = subset(PPT_gam_month, term == 'value')
PPT_gam_slope_month$sig = ifelse(PPT_gam_slope_month$p.value < 0.05, TRUE, FALSE)
#plotting the p-value of PPT_total_tree intercept for each site 
#if p<0.05 then TRUE, else FALSE
ggplot(data=PPT_gam_slope_month) +
  geom_point(aes(x=estimate, y=variable, colour=sig))


ggplot(data = ppt_melt) +
  geom_point(aes(x = value, y = AGBI.mean)) +
  geom_smooth(aes(x = value, y = AGBI.mean), method='gam', formula= y~s(x, k=10)) +  
  facet_wrap(~variable, scales = "free") +
  xlab('PPT') + 
  ylab('Aboveground biomass increment')




#by site
PPT_lm = clim_agb %>% 
  group_by(site) %>%
  do(tidy(lm(AGBI.mean ~ PPT_total_tree, .)))

PPT_lm_slope = subset(PPT_lm, term == 'PPT_total_tree')
PPT_lm_slope$sig = ifelse(PPT_lm_slope$p.value < 0.05, TRUE, FALSE)
#plotting the p-value of PPT_total_tree intercept for each site 
#if p<0.05 then TRUE, else FALSE
ggplot(data=PPT_lm_slope) +
  geom_point(aes(x=estimate, y=site, colour=sig))+
  geom_vline(aes(xintercept = 0, linetype = "dashed"))

#site and month
PPT_lm = ppt_melt %>% 
  group_by(site, variable) %>%
  do(tidy(lm(AGBI.mean ~ value, .)))

PPT_lm_slope = subset(PPT_lm, term == 'value')
PPT_lm_slope$sig = ifelse(PPT_lm_slope$p.value < 0.05, TRUE, FALSE)
#plotting the p-value of PPT_total_tree intercept for each site 
#if p<0.05 then TRUE, else FALSE
ggplot(data=PPT_lm_slope) +
  geom_point(aes(x=estimate, y=variable, colour=site, shape=sig), size =4)+
  geom_vline(aes(xintercept = 0, linetype = "dashed"))



#plotting only months and time where PPT is significant 
PPT_sig = subset(PPT_lm_slope, sig == "TRUE")
PPT_sig$site = factor(PPT_sig$site, levels = c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA'))

ggplot(data=PPT_sig) +
  geom_point(aes(x=estimate, y=variable, color = site), size = 3)+
  geom_vline(aes(xintercept = 0, linetype = "dashed"), color = "darkgreen")+
  ggtitle("months where PPT is significant")


#by month
PPT_lm_month = ppt_melt %>% 
  group_by(variable) %>%
  do(tidy(lm(AGBI.mean ~ value, .)))

PPT_lm_slope_month = subset(PPT_lm_month, term == 'value')
PPT_lm_slope_month$sig = ifelse(PPT_lm_slope_month$p.value < 0.05, TRUE, FALSE)
#plotting the p-value of PPT_total_tree intercept for each site 
#if p<0.05 then TRUE, else FALSE
ggplot(data=PPT_lm_slope_month) +
  geom_point(aes(x=estimate, y=variable, colour=sig))+
  geom_vline(aes(xintercept = 0, linetype = "dashed"))


#not sure why this one came out like that
#bysite 
vpd_lm = vpd_melt %>% 
  group_by(site) %>%
  do(tidy(lm(AGBI.mean ~ value, .)))
vpd_lm_slope = subset(vpd_lm, term == 'value')
vpd_lm_slope$sig = ifelse(vpd_lm_slope$p.value < 0.05, TRUE, FALSE)
ggplot(data=vpd_lm_slope) +
  geom_point(aes(x=estimate, y=site, colour=sig))+
  geom_vline(aes(xintercept = 0, linetype = "dashed", color = "blue"))

#bymonth
#do we want month and site on one?
vpd_lm_month = vpd_melt %>% 
  group_by(variable) %>%
  do(tidy(lm(AGBI.mean ~ value, .)))
vpd_lm_slope_month = subset(vpd_lm_month, term == 'value')
vpd_lm_slope_month$sig = ifelse(vpd_lm_slope_month$p.value < 0.05, TRUE, FALSE)
ggplot(data=vpd_lm_slope_month) +
  geom_point(aes(x=estimate, y=variable, colour=sig)) +
  geom_vline(aes(xintercept = 0, linetype = "dashed"))

#plotting only months and time where VPD is significant 
#none significant 
VPD_sig = subset(vpd_lm_slope, sig == "TRUE")
ggplot(data=VPD_sig) +
  geom_point(aes(x=estimate, y=variable, colour=site), size = 3)+
  geom_vline(aes(xintercept = 0, linetype = "dashed"))+
  ggtitle("months where VPD is significant")

#by site
tmin_lm = tmin_melt %>% 
  group_by(site, variable) %>%
  do(tidy(lm(AGBI.mean ~ value, .)))

tmin_lm_slope = subset(tmin_lm, term == 'value')
tmin_lm_slope$sig = ifelse(tmin_lm_slope$p.value < 0.05, TRUE, FALSE)
ggplot(data=tmin_lm_slope) +
  geom_point(aes(x=estimate, y=site, colour=sig))+
  geom_vline(aes(xintercept = 0, linetype = "dashed"))

#by month
tmin_lm_month = tmin_melt %>% 
  group_by(variable) %>%
  do(tidy(lm(AGBI.mean ~ value, .)))
tmin_lm_slope_month = subset(tmin_lm_month, term == 'value')
tmin_lm_slope_month$sig = ifelse(tmin_lm_slope_month$p.value < 0.05, TRUE, FALSE)
ggplot(data=tmin_lm_slope_month) +
  geom_point(aes(x=estimate, y=variable, colour=sig)) +
  geom_vline(aes(xintercept = 0, linetype = "dashed"))

#plotting only months and time where VPD is significant 
tmin_sig = subset(tmin_lm_slope, sig == "TRUE")
ggplot(data=tmin_sig) +
  geom_point(aes(x=estimate, y=variable, colour=site), size = 3)+
  geom_vline(aes(xintercept = 0, linetype = "dashed"))+
  ggtitle("months where tmin is significant")

#by site
tmax_lm = tmax_melt %>% 
  group_by(site, variable) %>%
  do(tidy(lm(AGBI.mean ~ value, .)))
tmax_lm_slope = subset(tmax_lm, term == 'value')
tmax_lm_slope$sig = ifelse(tmax_lm_slope$p.value < 0.05, TRUE, FALSE)
ggplot(data=tmax_lm_slope) +
  geom_point(aes(x=estimate, y=site, colour=sig)) +
  geom_vline(aes(xintercept = 0, linetype = "dashed"))


#by month
tmax_lm_month = tmax_melt %>% 
  group_by(variable) %>%
  do(tidy(lm(AGBI.mean ~ value, .)))
tmax_lm_slope_month = subset(tmax_lm_month, term == 'value')
tmax_lm_slope_month$sig = ifelse(tmax_lm_slope_month$p.value < 0.05, TRUE, FALSE)
ggplot(data=tmax_lm_slope_month) +
  geom_point(aes(x=estimate, y=variable, colour=sig)) +
  geom_vline(aes(xintercept = 0, linetype = "dashed"))

#plotting only months and time where VPD is significant 
tmax_sig = subset(tmax_lm_slope, sig == "TRUE")
ggplot(data=tmax_sig) +
  geom_point(aes(x=estimate, y=variable, colour=site), size = 3)+
  geom_vline(aes(xintercept = 0, linetype = "dashed"))+
  ggtitle("months where tmax is significant")


#################################################################################
# taxon data 
#################################################################################


taxon_site_total = all_taxon_summary %>% 
  group_by(taxon, site) %>% 
  summarize(total_AGBI_mean = sum(AGBI.mean, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(site, desc(total_AGBI_mean)) %>% 
  group_by(site) %>% 
  slice(1:3)


#taking the sum of each taxon for a given site
taxon_summed = all_taxon_summary %>% 
  group_by(year, taxon, site) %>% 
  summarize(total_AGBI_mean = sum(AGBI.mean, na.rm = TRUE))

#going with all_site_summary which has the TOTAL AGBI.mean for a given site
all_taxon_summed = taxon_summed %>% 
  inner_join(all_site_summary, by = c("year", "site"))

#dividing the total taxon AGBI.mean by the total AGBI at a site to determine percentage 
#that the taxon is present at a site
fractional_biomass = all_taxon_summed %>% 
  mutate(taxon_fractions = total_AGBI_mean/AGBI.mean)

#calculating the overall mean increment over the entire time for each taxon
fractional_mean_taxon = fractional_biomass %>% 
  group_by(taxon, site) %>% 
  summarize(taxon_mean = mean(taxon_fractions, na.rm = TRUE))



ggplot(data = fractional_biomass) +
  geom_line(aes(x =year, y = taxon_fractions, color = taxon))+ 
  facet_wrap(~site)
ggsave("figures1950/taxon_fractions_time.png")


ggplot(data = fractional_biomass) +
  geom_area(aes(x =year, y = taxon_fractions, fill = taxon))+ 
  facet_wrap(~site)
ggsave("figures1950/taxon_fractions_fill_time.png")  

# correlation
head(cor_clim_vars_taxon)

foo = taxon_site_total %>% group_by(site) %>% slice_max(order_by=total_AGBI_mean, n=3)

cor_clim_vars_taxon

bar = left_join(cor_clim_vars_taxon, foo)

#################################################################################
# Individual tree 
#################################################################################

# # Combining abgi and abg into one dataframe
# goose_total <- goose_total_agb |>
#   left_join(goose_total_agbi, by = c('tree', 'year', 'iter',
#                                      'taxon', 'model', 'plot')) |>
#   rename(AGB = value.x,
#          AGBI = value.y) |>
#   select(-c(type.x, type.y)) |>
#   # Add site name for combining sites into one df
#   mutate(site = 'GOOSE')
# 
# #summarizing the data for both agb and agbi
# #here we are taking the mean of all the iterations for one tree in a given year
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
# 
# 
# clim_tree = all_tree %>%
#   left_join(clim_summary,by = c('year') )
# clim_tree = subset(clim_tree, year>1950)
# 
# tree_indiv =  goose_total |>
#   group_by(tree, year) |>
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
# 
# tree_indiv_clim = tree_indiv %>% 
#   left_join(clim_summary, by = c('year'))
# tree_indiv_clim = subset(tree_indiv_clim, year>1950)
# 
# 
# ggplot(data = tree_indiv_clim)+
#   geom_point(aes(x = tree, y = AGB.mean, color = year))
# 
# ggplot(data = tree_indiv_clim)+
#   geom_point(aes(x = PPT_total_tree, y = AGB.mean))
# 
# tree_lm = clim_tree %>% 
#   group_by(tree, taxon) %>%
#   do(tidy(lm(AGBI.mean ~ PPT_total_tree, .)))
# 
# tree_lm_slope = subset(tree_lm, term == 'PPT_total_tree')
# tree_lm_slope$sig = ifelse(tree_lm_slope$p.value < 0.05, TRUE, FALSE)
# ggplot(data=tree_lm_slope) +
#   geom_histogram(aes(x=estimate))
# summary(tree_lm_slope$estimate)
# sum(tree_lm_slope$sig)
# 
# ggplot(data=tree_lm_slope) +
#   geom_point(aes(x=estimate, y=taxon, colour=sig)) 
# 
# plot(clim_tree$AGB.mean, clim_tree$AGBI.mean)
# 
# taxon_lm = clim_tree %>% 
#   group_by(taxon) %>%
#   do(tidy(lm(AGBI.mean ~ PPT_total_tree + AGB.mean, .)))





















# #with taxon
# 
# 
# 
# # clim_agb <- all_site_summary|>
# #   left_join(clim_summary, by = c('year', 'site'))
# # 
# # clim_agb = subset(clim_agb, year>1950)
# # 
# # vpd = select(clim_agb, year, AGBI.mean, taxon, site,
# #              starts_with('Vpdmean'))
# # tmin = select(clim_agb, year, AGBI.mean, taxon, site,
# #               starts_with('Tmin'))
# # tmax = select(clim_agb, year, AGBI.mean, taxon, site,
# #               starts_with('Tmax'))
# # ppt = select(clim_agb, year, AGBI.mean, taxon, site,
# #              starts_with('PPT'))
# # vpd_melt = melt(vpd, 
# #                 id.vars = c('year', 'AGBI.mean', 'taxon', 'site'))
# # temp_min_melt = melt(temp_min,
# #                      id.vars = c('year', 'AGBI.mean', 'taxon', 'site'))
# # temp_max_melt = melt(temp_max, 
# #                      id.vars = c('year', 'AGBI.mean', 'taxon', 'site'))
# # 
# # PPT_melt = melt(PPT, id.vars = c('year', 'AGBI.mean', 'taxon', 'site'))
# ggplot(data = climate_increment) +
#   geom_point(aes(x = yearly_meanT, y = AGBI.mean)) +
#   geom_linerange(aes(x=yearly_meanT, ymin=AGBI.lo, ymax=AGBI.hi)) +
#   facet_wrap(~taxon, scales='free_y')+
#   geom_smooth(aes(x = yearly_meanT, y = AGBI.mean), method='lm', formula= y~x )+
#   xlab('Average annual temperature') + ylab('Aboveground biomass increment') 
# ggsave("ABGI_taxon_temp.png")
# 
# # ggplot(data = clim_taxon) +
# #   geom_point(aes(x = yearly_meanT, y = AGBI.mean)) +
# #   geom_linerange(aes(x=yearly_meanT, ymin=AGBI.lo, ymax=AGBI.hi)) +
# #   facet_wrap(~taxon, scales='free_y')+
# #   geom_smooth(aes(x = yearly_meanT, y = AGBI.mean), method='lm', formula= y~x )+
# #   xlab('Average annual temperature') + ylab('Aboveground biomass increment') 
# # ggsave("ABGI_taxon_temp.png")
# 
# ggplot(data = clim_taxon) +
#   geom_point(aes(x = PPT_total, y = AGBI.mean)) +
#   geom_linerange(aes(x=PPT_total, ymin=AGBI.lo, ymax=AGBI.hi)) +
#   facet_wrap(~taxon, scales = 'free_y')+
#   geom_smooth(aes(x = PPT_total, y = AGBI.mean), method='lm', formula= y~x )+
#   xlab('Mean annual precipitation') + ylab('Aboveground biomass increment')
# ggsave("AGBI_taxon_precip.png")
# 
# ggplot(data = clim_taxon) +
#   geom_point(aes(x = PPT_total_tree, y = AGBI.mean)) +
#   geom_linerange(aes(x=PPT_total_tree, ymin=AGBI.lo, ymax=AGBI.hi)) +
#   facet_wrap(~taxon, scales = 'free_y')+
#   geom_smooth(aes(x = PPT_total_tree, y = AGBI.mean), method='lm', formula= y~x )+
#   xlab('Mean Total Tree Precip') + ylab('Aboveground biomass increment')
# ggsave("AGBI_taxon_precip.png")
# 
# ggplot(data = clim_taxon) +
#   geom_point(aes(x = year, y = AGBI.mean))+
#   facet_wrap(~taxon, scales = 'free_y')
# ggsave("AGBI_time_taxon.png") 
#  
# 
# 
# 
# ggplot(data = climate_increment) +
#   geom_point(aes(x = year, y = AGBI.mean))
# ggsave("AGBI_time.png")  
#   
#   
# ggplot(data = climate_increment) +
#   geom_point(aes(x = T_min_mean, y = AGBI.mean))#+
#   # geom_point(aes(x = T_max_mean, y = AGBI.mean))
# 
# ggplot(data = climate_increment) +
#   geom_point(aes(x = T_max_mean, y = AGBI.mean))#+
# # geom_point(aes(x = T_max_mean, y = AGBI.mean))
# 
# 
# 
# #unsure what this tells us
# ggplot(data = Vapor_melt) +
#   geom_point(aes(x = value, y = AGBI.mean, color = variable))
# 
# ggplot(data = Vapor_melt) +
#   geom_point(aes(x = value, y = AGBI.mean, color=taxon)) +
#   facet_wrap(~variable, scales='free_x') +
#   geom_smooth(aes(x = value, y = AGBI.mean, color=taxon), method='lm', formula= y~x )
#   
# 
# ggplot(data = climate_increment)+
#   geom_point(aes(x = PPT_total_prev_tree, y = AGBI.mean))
# 
# ggplot(data = climate_increment)+
#   geom_point(aes(x = PPT_03, y = AGBI.mean))
# 
# ggplot(data = PPT_melt)+
#   geom_point(aes(x = value, y = AGBI.mean, color = variable))
# 
# ggplot(data = PPT_melt)+
#   geom_point(aes(x = value, y = AGBI.mean, color = taxon)) +
#   facet_wrap(~variable, scales='free_x') +
#   geom_smooth(aes(x = value, y = AGBI.mean, color=taxon), method='lm', formula= y~x )
# #+
#  # facet_wrap(~)
# 
# ggplot(data = clim_taxon)+
#   geom_point(aes(x = PPT_03, y = AGBI.mean, color = taxon)) +
#   geom_smooth(aes(x = PPT_03, y = AGBI.mean, color=taxon), method='lm', formula= y~x )
# 
# 
# ggplot(data = clim_taxon)+
#   geom_point(aes(x = PPT_total, y = AGBI.mean, color = taxon)) +
#   geom_smooth(aes(x = PPT_total, y = AGBI.mean, color=taxon), method='lm', formula= y~x )

