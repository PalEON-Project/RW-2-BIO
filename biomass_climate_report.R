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
library(gam)
library(correlation)
#library(RColorBrewer)
library(stringr)

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

goose_ind_agb <- readRDS('sites/GOOSE/runs/v3.1_012021/output/AGB_STAN_GOOSE_v3.1_012021.RDS')

foo = subset(goose_ind_agb, iter==1) %>%
  group_by(year, iter) %>%
  count(type)
ggplot(data=foo) +
  geom_line(aes(x=year, y=n))

foo = subset(goose_ind_agb, iter==1) %>%
  group_by(year, iter) %>%
  count(taxon)
ggplot(data=foo) +
  geom_line(aes(x=year, y=n, colour=taxon))

ggplot(data=goose_ind_agb) +
  geom_line(aes(x=year, y=value,))

rooster_ind_agb <- readRDS('sites/ROOSTER/runs/v3.1_082020/output/AGB_STAN_ROOSTER_v3.1_082020.RDS')

foo = subset(rooster_ind_agb, iter==1) %>%
  group_by(year, iter) %>%
  count(type)
ggplot(data=foo) +
  geom_line(aes(x=year, y=n))

foo = subset(rooster_ind_agb, iter==1) %>%
  group_by(year, iter) %>%
  count(taxon)
ggplot(data=foo) +
  geom_line(aes(x=year, y=n, colour=taxon))

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
            AGBI.mean = mean(AGBI.sum, na.rm = T),
            AGBI.sd = sd(AGBI.sum),
            AGBI.lo = quantile(AGBI.sum, c(0.025), na.rm=TRUE),
            AGBI.hi = quantile(AGBI.sum, c(0.975), na.rm=TRUE), 
            .groups='keep')
head(all_taxon_summary)

#AGBI over time starting at the year 1900
ggplot(data=all_site_summary) +
  geom_ribbon(aes(x=year, ymin=AGBI.lo, ymax=AGBI.hi, colour=site, fill=site)) +
  geom_line(aes(x=year, y=AGBI.mean, colour=site)) +
  theme_bw(14) +
  xlab('Year') +
  ylab('AGBI (Mg/ha)')
ggsave("figures1950/AGBI_over_time.jpg")

#AGB ovetime
ggplot(data=all_site_summary) +
  geom_ribbon(aes(x=year, ymin=AGB.lo, ymax=AGB.hi, colour=site, fill=site)) +
  geom_line(aes(x=year, y=AGB.mean, colour=site)) +
  theme_bw(14) +
  xlab('Year') +
  ylab('AGB (Mg/ha)')
ggsave("figures1950/AGB_over_time.jpg")

ggplot(data=all_taxon_summary) +
  geom_ribbon(aes(x=year, ymin=AGBI.lo, ymax=AGBI.hi, colour=taxon, fill=taxon)) +
  geom_line(aes(x=year, y=AGBI.mean, colour=taxon)) +
  theme_bw(14) +
  xlab('Year') +
  ylab('AGBI (Mg/ha)') +
  facet_wrap(~site)
ggsave("figures1950/AGBI_over_time_taxons.jpg")


#######################3

ggplot(data=all_taxon_summary) +
  geom_ribbon(aes(x=year, ymin=AGBI.mean-2*AGBI.sd,
                  ymax=AGBI.mean+2*AGBI.sd, color = taxon, fill = taxon), alpha=0.3) +
  geom_line(aes(x=year, y=AGBI.mean, color = taxon)) +
  facet_wrap(~site)
ggsave("figures1950/AGBI_site_taxon_with_sd.jpg")

  
#changing to wide format with taxons as column names and values = ABGI.mean 
#values_fill=0 does not work 
#wide format of taxon data with AGBI as value data
all_taxon_summary_wide = pivot_wider(data = all_taxon_summary[,(colnames(all_taxon_summary) %in% 
                                                                  c('year', 'taxon', 'AGBI.mean', 'site'))],
                         id_cols = c(year, site),
                         names_from = taxon, 
                         values_from = AGBI.mean, 
                         values_fill = 0 )

#wide format of site data with AGBI as value
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
# cor(all_taxon_summary_wide$ACSA, all_taxon_summary_wide$BEPA, use = "complete.obs")
# correlation_A = data.frame(cor(all_taxon_summary_wide[, c('ACRU','ACSA','BEPA', 'FAGR', 'PIST', 'QUAL', 'QUMO', 'QURU', 'BELE',
#                    'OSVI', 'AMAR')],use = "complete.obs"))
#find significance 
#correlation_B = correlation_A
#correlation_B[correlation_B<0.4] = NA

#correlation figure between species for ABGI.mean
#ggcorrplot(correlation_A, method = "circle", type = "lower", hc.order = FALSE)
#ggcorrplot(correlation_B, type = "lower",)
#  scale_fill_gradient2(low = "yellow", mid = 'pink', high = "light green", breaks=c(0, 1), limit=c(0, 1))


#correlation between sites of AGBI
cor_site = data.frame(cor(all_site_summary_wide[, c('GOOSE', 'ROOSTER', 'SYLVANIA', 'NRP')], 
                          use = "complete.obs"))
ggcorrplot(cor_site, method = "circle", type = "lower", hc.order = FALSE)
write.csv(cor_site, "correlation_AGBI_site.csv")  


cor_plot_goose = data.frame(cor(goose_plot_wide[,c('1', '2', '3')], use = "complete.obs"))
ggcorrplot(cor_plot_goose, method = "square", type = "lower")


cor(clim_agb$AGBI.mean, clim_agb$PPT_01, use = "complete.obs")


# all_site_plot_summary
goose_plot_summary = subset(all_site_plot_summary, site == 'GOOSE')
goose_plot_summary_wide = pivot_wider(data = goose_plot_summary[,(colnames(goose_plot_summary) %in% 
                                                                c('year', 'plot', 'AGBI.mean', 'site'))],
                                    id_cols = c(year),
                                    names_from = plot, 
                                    values_from = AGBI.mean, 
                                    values_fill = 0 )
colnames(goose_plot_summary_wide) = c('year', 'plot1', 'plot2', 'plot3')

cor_plot_goose = data.frame(cor(goose_plot_summary_wide[,c('plot1', 'plot2', 'plot3')], use = "complete.obs"))
ggcorrplot(cor_plot_goose, method = "square", type = "lower")

# #correlation between AGBI and all climate variables
# cor_clim_vars <- all_taxon_summary_wide %>%
#   # Filter to keep only the relevant rows for correlation
#   filter(str_detect(variable, "^(ACRU|ACSA|AMAR)")) %>%
#   # Group by site and variable
#   group_by(site) %>%
#   # Summarize by calculating correlation between AGBI.mean and value
#   summarize(correlation = cor(c(ACRU|ACSA|AMAR), value, use = "complete.obs"), .groups = 'drop')
# head(cor_clim_vars)
# 
# goose_taxon_summary = subset(all_taxon_summary_wide, site=='GOOSE')
# cor(goose_taxon_summary[,3:ncol(goose_taxon_summary)], use = "na.or.complete")
# 
# 
# correlation_by_site <- all_taxon_summary_wide %>%
#   group_by(site) %>%
#   summarise(across(all_of(3:ncol(all_taxon_summary_wide)), 
#                    list(correlation = ~ cor(., use = "complete.obs")), 
#                    .names = "cor_{col}"))


correlation_matrices_by_site <- all_taxon_summary_wide %>%
  group_by(site) %>%
  summarise(cor_matrix = list(cor(select_if(cur_data(), is.numeric), use = "pairwise.complete.obs")))

correlation_matrices_by_site[[2]][1]

# returns a list
# first list element is a vector of site names
# > correlation_matrices_by_site[[1]]
# [1] "GOOSE"    "NRP"      "ROOSTER"  "SYLVANIA"

# second list element is a list of correlation matrices
# > correlation_matrices_by_site[[2]]
#to get correlation matrix for GOOSE, we do:
# > correlation_matrices_by_site[[2]][[1]]

ggcorrplot(correlation_matrices_by_site[[2]][[1]], method = "square", type = "lower")


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
# clim_melt = melt(clim_data, 
#                 id.vars = c('year', 'month', 'site'))
# 
# clim_melt$year = as.integer(clim_melt$year)
# summary(clim_melt)

#PPT_mean is the mean of each month summed to get the mean of the year
#yearly_meanT is the yearly temperature mean based on monthly means

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

#shouldnt be needed since we subset in the beginning of the code
# clim_agb = subset(clim_agb, year>1950)

clim_agb_filtered = clim_agb %>% 
  select(-AGB.mean,-AGB.sd,-AGB.lo,-AGB.hi,-AGBI.sd,
         -AGBI.lo,-AGBI.hi,-AGBI.sd, -AGBI.mean)

clim_melt = melt(clim_agb_filtered,
                id.vars = c('year', 'site','model'))

clim_melt$year = as.integer(clim_melt$year)
summary(clim_melt)



clim_total <- all_site_summary|>
  left_join(clim_melt, by = c('year', 'site','model'))
head(clim_total)
summary(clim_melt)

vpd = select(clim_agb, year, AGBI.mean, site,
             starts_with('Vpdmean'))
tmin = select(clim_agb, year, AGBI.mean, site,
              starts_with('Tmin'))
tmax = select(clim_agb, year, AGBI.mean, site,
              starts_with('Tmax'))
tmean = select(clim_agb, year, AGBI.mean, site,
               starts_with('Tmean'))
ppt = select(clim_agb, year, AGBI.mean, site,
             starts_with('PPT'))
annual_vars = select(clim_agb, year, AGBI.mean, site,
                     yearly_meanT, PPT_total_tree, T_min_mean, T_max_mean)

vpd_melt = melt(vpd, 
                id.vars = c('model', 'year', 'AGBI.mean', 'site'))
vpd_melt$site = factor(vpd_melt$site, levels = c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA'))

# vpd_melt$variable = sapply(as.vector(vpd_melt$variable), function(x) {strsplit(x, '\\_')[[1]][2]})

tmin_melt = melt(tmin,
                 id.vars = c('model', 'year', 'AGBI.mean', 'site'))
tmin_melt$site = factor(tmin_melt$site, levels = c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA'))

tmax_melt = melt(tmax, 
                 id.vars = c('model', 'year', 'AGBI.mean', 'site'))
tmax_melt$site = factor(tmax_melt$site, levels = c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA'))

tmean_melt = melt(tmean, 
                  id.vars = c('model', 'year', 'AGBI.mean', 'site'))

tmean_melt$site = factor(tmean_melt$site, levels = c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA'))

ppt_melt = melt(ppt, id.vars = c('model', 'year', 'AGBI.mean', 'site'))
ppt_melt$site = factor(ppt_melt$site, levels = c('GOOSE', 'NRP', 'ROOSTER', 'SYLVANIA'))

annual_vars_melt = melt(annual_vars, id.vars = c('model', 'year', 'AGBI.mean', 'site'))



######################################################################################################################
#correlation between tree ring data and climate variables
######################################################################################################################
clim_total = na.omit(clim_total)

#correlation between AGBI and all climate variables
cor_clim_vars <- clim_total %>%
  # Filter to keep only the relevant rows for correlation
  filter(str_detect(variable, "^(PPT|Tmean|Tmax2|Tmin2|Vpdmin2|Vpdmax2)")) %>%
  # Group by site and variable
  group_by(site, variable) %>%
  # Summarize by calculating correlation between AGBI.mean and value
  summarize(correlation = cor(AGBI.mean, value, use = "complete.obs"), .groups = 'drop')
head(cor_clim_vars)

write.csv(cor_clim_vars, file = "AGBI_clim_correlation.csv")


# cor_clim_vars = clim_total %>% 
#   # Filter to keep only the relevant rows for correlation
#   filter(variable %in%  starts_with(c("PPT", "Tmean", "Tmax2", 'Tmin2', "Vpdmin2", "Vpdmax2" ))) %>%
#   # Group by site
#   group_by(site, variable) %>%
#   # Summarize by calculating correlation between AGBI.mean and value (assuming 'value' holds the PPT_total_tree data)
#   summarize(correlation = cor(AGBI.mean, value, use = "complete.obs"))


##################################################################################################
#plotting climate variables over time 
###################################################################################################
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

ggplot(data = fractional_biomass) +
  geom_line(aes(x =year, y = taxon_fractions, color = taxon))+ 
  facet_wrap(~site)
ggsave("figures1950/taxon_fractions_time.png")


ggplot(data = fractional_biomass) +
  geom_area(aes(x =year, y = taxon_fractions, fill = taxon))+ 
  facet_wrap(~site)
ggsave("figures1950/taxon_fractions_fill_time.png")  

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

