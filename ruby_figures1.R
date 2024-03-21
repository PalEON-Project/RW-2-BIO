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

goose_total_agb <- readRDS('sites/GOOSE/runs/v2.0_012021/output/AGB_STAN_GOOSE_v2.0_012021.RDS')
goose_total_agbi <- readRDS('sites/GOOSE/runs/v2.0_012021/output/AGBI_STAN_GOOSE_v2.0_012021.RDS')

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


#taxon_group takes the sum of all the trees in one taxon.
#iterations for each taxon not individual trees 
taxon_group <- goose_total |>
  group_by(year, iter, taxon) |>
  # Means across iterations
  summarize(AGBI.sum = sum(AGBI, na.rm = T))
ggplot(data=taxon_group) +
  geom_line(aes(x=year, y=AGBI.sum, group=iter)) +
  facet_wrap(~taxon)


taxon_summary = taxon_group %>%
  group_by(year, taxon) %>% 
  summarize(AGBI.mean = mean(AGBI.sum, na.rm = T),
            AGBI.sd = sd(AGBI.sum),
            AGBI.lo = quantile(AGBI.sum, c(0.025), na.rm=TRUE),
            AGBI.hi = quantile(AGBI.sum, c(0.975), na.rm=TRUE), 
            .groups='keep')

ggplot(data=taxon_summary) +
  geom_line(aes(x=year, y=AGBI.mean, colour=taxon)) 

site_summary = taxon_group %>% 
  group_by(year, iter) %>% 
  summarize(AGBI.sum = sum(AGBI.sum, na.rm = T))

ggplot(data = site_summary)+
  geom_line(aes(x = year, y = AGBI.sum, group = iter))


site_total = site_summary %>% 
  group_by(year) %>% 
  summarize(AGBI.mean = mean(AGBI.sum, na.rm = T),
            AGBI.sd = sd(AGBI.sum, na.rm = T),
            AGBI.lo = quantile(AGBI.sum, c(0.025), na.rm=TRUE),
            AGBI.hi = quantile(AGBI.sum, c(0.975), na.rm=TRUE))


ggplot(data=taxon_summary) +
  geom_ribbon(aes(x=year, ymin=AGBI.mean-2*AGBI.sd, ymax=AGBI.mean+2*AGBI.sd), fill="blue", alpha=0.3) +
  geom_line(aes(x=year, y=AGBI.mean)) +
  facet_wrap(~taxon)

#average biomass that increases for the whole time period
taxon_3 = taxon_summary %>% 
  group_by(taxon) %>% 
  summarize(AGBI.mean2 = mean(AGBI.mean),
            AGBI.sd = sd(AGBI.mean, na.rm = T))
  
#changing to wide format with taxons as column names and values = ABGI.mean 
#values_fill=0 does not work 
taxon_wide = pivot_wider(data = taxon_summary[,(colnames(taxon_summary) %in% c('year', 'taxon', 'AGBI.mean'))], 
                         names_from = taxon, 
                         values_from = AGBI.mean, 
                         values_fill = 0 )

# taxon_wide = pivot_wider(data = taxon_group,
#                          names_from = taxon, 
#                          values_from = AGBI.mean, 
#                          values_fill = 0 )

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
ggcorrplot(correlation_B, type = "lower",)
#  scale_fill_gradient2(low = "yellow", mid = 'pink', high = "light green", breaks=c(0, 1), limit=c(0, 1))

#########################################################################################
#CLIMATE
################################################################################################

load('climate/prism_clim.RData')
goose_clim = subset(prism_long, loc =='GOOSE')
goose_clim = data.frame(goose_clim)
goose_clim = rename(goose_clim, site = loc, PPT = PPT2, 
                    mean_temp = Tmean2)


clim_wide =  pivot_wider(data = goose_clim,
                         names_from = month, 
                         values_from = c(PPT, mean_temp, Tmin2, Tmax2, Vpdmin2, Vpdmax2))


#PPT_mean is the mean of each month summed to get the mean of the year
#yearly_meanT is the yearly temperature mean based on monthly means

clim_summary = clim_wide |>
  mutate(PPT_total = rowSums(dplyr::select(clim_wide, starts_with('PPT'))),
         PPT_total_prev_tree = rowSums(dplyr::pick('PPT_09', 'PPT_10', 'PPT_11', 'PPT_12')),
         PPT_total_current_tree = rowSums(dplyr::pick('PPT_01', 'PPT_02', 'PPT_03', 'PPT_04', 
                                              'PPT_05', 'PPT_06', 'PPT_07', 'PPT_08')),
         yearly_meanT = rowMeans(dplyr::select(clim_wide, starts_with('mean_temp'))),
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

climate_increment <- site_total|>
  left_join(clim_summary, by = c('year'))

climate_increment = subset(climate_increment, year>1950)

clim_taxon = taxon_summary %>%
  left_join(clim_summary,by = c('year') )
clim_taxon = subset(clim_taxon, year>1950)



Vapor_pd = select(clim_taxon, year, AGBI.mean, taxon,
                  starts_with('Vpdmean'))

temp_min = select(clim_taxon, year, AGBI.mean, taxon,
                  starts_with('Tmin'))
temp_max = select(clim_taxon, year, AGBI.mean, taxon,
                  starts_with('Tmax'))
PPT = select(clim_taxon, year, AGBI.mean, taxon,
             starts_with('PPT'))


Vapor_melt = melt(Vapor_pd, 
                  id.vars = c('year', 'AGBI.mean', 'taxon'))
temp_min_melt = melt(temp_min,
                     id.vars = c('year', 'AGBI.mean', 'taxon'))
temp_max_melt = melt(temp_max, 
                     id.vars = c('year', 'AGBI.mean', 'taxon'))

PPT_melt = melt(PPT, id.vars = c('year', 'AGBI.mean', 'taxon'))


# Vpd_sets = list(c("Vpdmin2_01", "Vpdmax2_01"), c("Vpdmin2_02", "Vpdmax2_02"), 
#                 c("Vpdmin2_03", "Vpdmax2_03"), c("Vpdmin2_04", "Vpdmax2_04"),
#                 c("Vpdmin2_05", "Vpdmax2_05"), c("Vpdmin2_06", "Vpdmax2_06"),
#                 c("Vpdmin2_07", "Vpdmax2_07"), c("Vpdmin2_08", "Vpdmax2_08"),
#                 c("Vpdmin2_09", "Vpdmax2_09"), c("Vpdmin2_10", "Vpdmax2_10"),
#                 c("Vpdmin2_11", "Vpdmax2_11"), c("Vpdmin2_12", "Vpdmax2_12")
# )
# for (i in seq_along(Vpd_sets)) {
#   set <- Vpd_sets[[i]]
#   clim_summary <- clim_summary %>% 
#     mutate(!!paste0("Vpdmean_", i) := rowMeans(select(., all_of(set))))
#   
# }
# 
# N_years = nrow(clim_summary)
# 
# clim_summary$PPT_total_tree = NA
# clim_summary$PPT_total_tree[2:N_years] = clim_summary$PPT_total_prev_tree[1:(N_years-1)] + 
#   clim_summary$PPT_total_current_tree[2:N_years]
# 
# 
# clim_summary$year <- as.numeric(clim_summary$year)
# 
# climate_increment <- site_total|>
#   left_join(clim_summary, by = c('year'))
# 
# climate_increment = subset(climate_increment, year>1950)
# 
# clim_taxon = taxon_summary %>% 
#   left_join(clim_summary,by = c('year') )
# clim_taxon = subset(clim_taxon, year>1950)


#without taxon
ggplot(data = climate_increment) +
  geom_point(aes(x = yearly_meanT, y = AGBI.mean)) +
  geom_smooth(aes(x = yearly_meanT, y = AGBI.mean), method='lm', formula= y~x)+
  xlab('Mean annual temperature') + ylab('Aboveground biomass increment')
#ggsave("AGBI_temp.png")

ggplot(data = climate_increment) +
  geom_point(aes(x = yearly_meanT, y = AGBI.mean)) + 
  geom_linerange(aes(x=yearly_meanT, ymin=AGBI.lo, ymax=AGBI.hi)) +
  geom_smooth(aes(x = yearly_meanT, y = AGBI.mean), method='lm', formula= y~x)+
  xlab('Mean annual temperature') + ylab('Aboveground biomass increment')
# ggsave("AGBI_temp.png")


ggplot(data = climate_increment) +
  geom_point(aes(x = PPT_total, y = AGBI.mean)) +
  geom_smooth(aes(x = PPT_total, y = AGBI.mean), method='lm', formula= y~x)+
  xlab('Mean annual precipitation') + ylab('Aboveground biomass increment')
ggsave("AGBI_precip.png")


ggplot(data = climate_increment) +
  geom_point(aes(x = PPT_total_tree, y = AGBI.mean)) +
  geom_smooth(aes(x = PPT_total_tree, y = AGBI.mean), method='lm', formula= y~x)+
  xlab('Mean Total Tree Precip') + ylab('Aboveground biomass increment')
ggsave("AGBI_precip_tree.png")





#with taxon
ggplot(data = clim_taxon) +
  geom_point(aes(x = yearly_meanT, y = AGBI.mean)) +
  geom_linerange(aes(x=yearly_meanT, ymin=AGBI.lo, ymax=AGBI.hi)) +
  facet_wrap(~taxon, scales='free_y')+
  geom_smooth(aes(x = yearly_meanT, y = AGBI.mean), method='lm', formula= y~x )+
  xlab('Average annual temperature') + ylab('Aboveground biomass increment') 
ggsave("ABGI_taxon_temp.png")

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
  geom_point(aes(x = tree, y = AGB.mean))

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
