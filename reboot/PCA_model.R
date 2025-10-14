library(dplyr)
library(tidyr)
library(ggplot2)
library(segmented)
library(purrr)
library(GGally)

#PCA model for averaged seasons and not averaged climate data
#from 1951-2011
AGBI_data = readRDS("AGBI_taxon_data.RDS")

###NOT AVERAGED
pca_sites = readRDS("pca_sites.RDS")




AGBI_PCA = AGBI_data %>% 
  inner_join(pca_sites, by= (c('year', 'site')))


models = AGBI_PCA %>% 
  group_by(site, taxon) %>% 
  do(mod1 = lm(AGBI.mean ~ PCA1_summer + PCA2_summer + PCA1_spring + PCA2_spring + 
                PCA1_fall + PCA2_fall + PCA1_winter + PCA2_winter, data=.))



#print(models$mod)

coefficients_summary = models %>%
  summarise(
    intercept = coef(mod1)[1],
    slope_PCA1s = coef(mod1)[2],
    slope_PCA2s = coef(mod1)[3],
    slope_PCA1sp = coef(mod1)[4],
    slope_PCA2sp = coef(mod1)[5],
    slope_PCA1f = coef(mod1)[6],
    slope_PCA2f = coef(mod1)[7],
    slope_PCA1w = coef(mod1)[4],
    slope_PCA2w = coef(mod1)[5],
    Rsq = summary(mod1)$r.squared
  ) %>% 
  mutate(taxon = models$taxon, site = models$site)

coefficients_summary <- dplyr::mutate(coefficients_summary, 
                                      model = paste0(site, "_", taxon))

# ggplot()+
#   geom_point(data = coefficients_summary, aes(x = Rsq, y = taxon, color =site))

# Print the summary
print(coefficients_summary)


###########AVERAGED

pca_sites_mean = readRDS("pca_sites_mean.RDS")

#pca_sites_mean$year = as.numeric(pca_sites_mean$year)


AGBI_PCA_mean = AGBI_data %>% 
  inner_join(pca_sites_mean, by= (c('year', 'site')))


models_mean = AGBI_PCA_mean %>% 
  group_by(site, taxon) %>% 
  do(mod2 = lm(AGBI.mean ~ PCA1_summer + PCA2_summer + PCA1_spring + PCA2_spring + 
                PCA1_fall + PCA2_fall + PCA1_winter + PCA2_winter, data=.))




coefficients_summary_mean = models_mean %>%
  summarise(
    intercept = coef(mod2)[1],
    slope_PCA1s = coef(mod2)[2],
    slope_PCA2s = coef(mod2)[3],
    slope_PCA1sp = coef(mod2)[4],
    slope_PCA2sp = coef(mod2)[5],
    slope_PCA1f = coef(mod2)[6],
    slope_PCA2f = coef(mod2)[7],
    slope_PCA1w = coef(mod2)[4],
    slope_PCA2w = coef(mod2)[5],
    Rsq = summary(mod2)$r.squared
  ) %>% 
  mutate(taxon = models_mean$taxon, site = models_mean$site)

coefficients_summary_mean <- dplyr::mutate(coefficients_summary_mean, 
                                      model = paste0(site, "_", taxon))

#####plotting R2#####


#####RESIDUALS#################
#residuals not mean 
#pulling residuals for models
#res = lapply(models[[3]], function(x) x$residuals)

res = lapply(models[[3]], function(x) {
  if(length(x$residuals) < 62){ rep(NA, 62)}else{x$residuals}})



#Deleting models with missing data HAVI at Harvard and BEPA at NRP
#res = res[-c(16, 32)] 
#creating df where each column has the residuals for each model
res_df = data.frame(matrix(unlist(res), ncol =length(res), byrow=FALSE))
#changing column names to site_taxon corresponding model
colnames(res_df) <- coefficients_summary$model
res_df <- res_df %>%
  mutate(year = 1950:2011)%>%
  dplyr::select(year, everything())
#removing column with NA values 
res_df_na <- subset(res_df, select = -c(HARVARD_HAVI, NRP_BEPA)) 

res_df$period = NA

#marking periods pre and post disturbance=1981 at GOOSE 
goose_res = res_df %>% 
  dplyr::select(year, period, starts_with("GOOSE"))
goose_res$period[which(goose_res$year<1981)] = "pre"
goose_res$period[which(goose_res$year>1981)] = "post"

#correlation pre and post disturbance 
ggpairs(data = goose_res %>% filter(period == "pre"),
  columns = which(startsWith(names(goose_res), "GOOSE")))
ggpairs(data = goose_res %>% filter(period == "post"),
        columns = which(startsWith(names(goose_res), "GOOSE")))

harvard_res = res_df_na %>% 
  dplyr::select(year, period, starts_with("HARVARD"))
harvard_res$period[which(harvard_res$year<1981)] = "pre"
harvard_res$period[which(harvard_res$year>1981)] = "post"

#correlation pre and post disturbance 
ggpairs(data = harvard_res %>% filter(period == "pre"),
        columns = which(startsWith(names(harvard_res), "HARVARD")))
ggpairs(data = harvard_res %>% filter(period == "post"),
        columns = which(startsWith(names(harvard_res), "HARVARD")))

rooster_res = res_df %>% 
  dplyr::select(year, period, starts_with("ROOSTER"))
rooster_res$period[which(rooster_res$year<1983)] = "pre"
rooster_res$period[which(rooster_res$year>1983)] = "post"

#correlation pre and post disturbance 
ggpairs(data = rooster_res %>% filter(period == "pre"),
        columns = which(startsWith(names(rooster_res), "ROOSTER")))
ggpairs(data = rooster_res %>% filter(period == "post"),
        columns = which(startsWith(names(rooster_res), "ROOSTER")))

# nrp_res = res_df_na %>% 
#   dplyr::select(year, period, starts_with("NRP"))
# nrp_res$period[which(nrp_res$year<1981)] = "pre"
# nrp_res$period[which(nrp_res$year>1981)] = "post"
# 
# #correlation pre and post disturbance 
# ggpairs(data = nrp_res %>% filter(period == "pre"),
#         columns = which(startsWith(names(nrp_res), "NRP")))
# ggpairs(data = nrp_res %>% filter(period == "post"),
#         columns = which(startsWith(names(nrp_res), "NRP")))

# hmc_res = res_df %>% 
#   dplyr::select(year, period, starts_with("HMC"))
# hmc_res$period[which(hmc_res$year<1981)] = "pre"
# hmc_res$period[which(hmc_res$year>1981)] = "post"
# 
# #correlation pre and post disturbance 
# ggpairs(data = hmc_res %>% filter(period == "pre"),
#         columns = which(startsWith(names(hmc_res), "HMC")))
# ggpairs(data = hmc_res %>% filter(period == "post"),
#         columns = which(startsWith(names(hmc_res), "HMC")))




res_long <- res_df %>%
  pivot_longer(cols = GOOSE_ACRU:SYLVANIA_TSCA, names_to = "site_taxon", values_to = "value") %>%
  separate(site_taxon, into = c("site", "taxon"), sep = "_")

#making cor df for each site 
sites <- c("GOOSE", "ROOSTER", "HARVARD", "HMC", "NRP", "SYLVANIA")
for (site in sites) {
  # Select columns that start with the site name
  cor_mat <- res_df %>%
    dplyr::select(starts_with(site)) %>%
    cor(use = "pairwise.complete.obs") %>%
    as.data.frame()
  
  # Clean up the row and column names
  clean_names <- sub(".*_", "", colnames(cor_mat))
  colnames(cor_mat) <- clean_names
  rownames(cor_mat) <- clean_names
  
  # Save as an individual data frame like goose_cor, rooster_cor, etc.
  assign(paste0(tolower(site), "_cor"), cor_mat)
}



pdf('report/figures/res_cor_PCA.pdf')

ggcorrplot(goose_cor, method = "square", type = "lower", hc.order = FALSE, show.diag = TRUE) +
  scale_fill_distiller(
    palette = "PuOr", na.value = "white",
    direction = 1, limits = c(-1, 1),
    name = "Pearson\nCorrelation:") +
  ggtitle("Goose residual Correlations (PCA)")
ggcorrplot(harvard_cor, method = "square", type = "lower", hc.order = FALSE, show.diag = TRUE) +
  scale_fill_distiller(
    palette = "PuOr", na.value = "white",
    direction = 1, limits = c(-1, 1),
    name = "Pearson\nCorrelation:") +
  ggtitle("Harvard residual Correlations (PCA)")
ggcorrplot(nrp_cor, method = "square", type = "lower", hc.order = FALSE, show.diag = TRUE) +
  scale_fill_distiller(
    palette = "PuOr", na.value = "white",
    direction = 1, limits = c(-1, 1),
    name = "Pearson\nCorrelation:") +
  ggtitle("NRP residual Correlations (PCA)")
ggcorrplot(rooster_cor, method = "square", type = "lower", hc.order = FALSE, show.diag = TRUE) +
  scale_fill_distiller(
    palette = "PuOr", na.value = "white",
    direction = 1, limits = c(-1, 1),
    name = "Pearson\nCorrelation:") +
  ggtitle("Rooster residual Correlations (PCA)")
ggcorrplot(sylvania_cor, method = "square", type = "lower", hc.order = FALSE, show.diag = TRUE) +
  scale_fill_distiller(
    palette = "PuOr", na.value = "white",
    direction = 1, limits = c(-1, 1),
    name = "Pearson\nCorrelation:") +
  ggtitle("Sylvania residual Correlations (PCA)")
ggcorrplot(hmc_cor, method = "square", type = "lower", hc.order = FALSE, show.diag = TRUE) +
  scale_fill_distiller(
    palette = "PuOr", na.value = "white",
    direction = 1, limits = c(-1, 1),
    name = "Pearson\nCorrelation:") +
  ggtitle("HMC residual Correlations (PCA)")
dev.off()




####RESIDUALS PLOTS#####
#plotting residuals for each site for each taxa 
#giving each site a unique colour
site_levels <- unique(res_long$site)
site_colors <- RColorBrewer::brewer.pal(length(site_levels), "Set1")
names(site_colors) <- site_levels




pdf('report/figures/ggpairs_residuals_sites.pdf')
#ggpairs for each site 
for (site in sites) {
  site_data <- res_df_na %>%
    dplyr::select(starts_with(site))
  
  # Skip if no matching columns (to avoid errors)
  if (ncol(site_data) == 0) next
  
  # Clean column names
  colnames(site_data) <- sub(".*_", "", colnames(site_data))
  
  # Plot
  print(ggpairs(data = site_data, title = paste(site, "Correlations")))
}
dev.off()

#residuals for each site with each taxa
ggplot()+
  geom_point(data = res_long , aes(x = year, y = value, color = taxon))+
  facet_wrap(~site)+
  theme_light(base_size = 11)+
  ggtitle("PCA res")

#residuals for each site with each taxa, free_y 
ggplot(data = res_long , aes(x = year, y = value, color = taxon))+
  geom_point()+
  geom_smooth( method = "gam")+
  facet_wrap(~site, scales = "free_y")+
  theme_light(base_size = 11)+
  ggtitle("PCA res")

#residuals for ony harvard 
ggplot(data = res_long %>% filter(site== "HARVARD"))+
  geom_point(aes(x = year, y = value, color = taxon))+
  # facet_wrap(~site, scales = "free_y")+
  theme_light(base_size =11)+
  ggtitle("HARVARD")

#residuals where QURU is found 
ggplot(data = res_long %>% filter(taxon== "QURU"),
       aes(x = year, y = value, color = site))+
  geom_point()+
 # facet_wrap(~site, scales = "free_y")+
  geom_smooth( method = "gam")+
  scale_color_manual(values = site_colors) +
  theme_light(base_size =11)+
  ggtitle("QURU")

ggplot(data = res_long %>% filter(taxon== "PIST"),
       aes(x = year, y = value, color = site))+
  geom_point()+
  # facet_wrap(~site, scales = "free_y")+
  geom_smooth(method = "gam")+
  scale_color_manual(values = site_colors) +
  theme_light(base_size =11)+
  ggtitle("PIST")

ggplot(data = res_long %>% filter(taxon== "TSCA"),
       aes(x = year, y = value, color = site))+
  geom_point()+
  # facet_wrap(~site, scales = "free_y")+
  geom_smooth(method = "gam")+
  scale_color_manual(values = site_colors) +
  theme_light(base_size =11)+
  ggtitle("TSCA")

####Residuals MEAN
#pulling residuals for models
#res_mean = lapply(models_mean[[3]], function(x) x$residuals)

res_mean = lapply(models_mean[[3]], function(x) {
  if(length(x$residuals) < 62){ rep(NA, 62)}else{x$residuals}})



#Deleting models with missing data HAVI at Harvard and BEPA at NRP
#res = res[-c(16, 32)] 
#creating df where each column has the residuals for each model
res_mean_df = data.frame(matrix(unlist(res_mean), ncol =length(res_mean), byrow=FALSE))
#changing column names to site_taxon corresponding model
colnames(res_mean_df) <- coefficients_summary_mean$model
res_mean_df <- res_mean_df %>%
  mutate(year = 1950:2011) %>%
  select(year, everything())

res_mean_long <- res_mean_df %>%
  pivot_longer(cols = GOOSE_ACRU:SYLVANIA_TSCA, names_to = "site_taxon", values_to = "value") %>%
  separate(site_taxon, into = c("site", "taxon"), sep = "_")


goose_correlations_mean <- res_mean_df %>%
  select(starts_with("GOOSE")) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame()
rooster_correlations_mean <- res_mean_df %>%
  select(starts_with("ROOSTER")) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame()
harvard_correlations_mean <- res_mean_df %>%
  select(starts_with("HARVARD")) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame()
nrp_correlations_mean <- res_mean_df %>%
  select(starts_with("NRP")) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame()
hmc_correlations_mean <- res_mean_df %>%
  select(starts_with("HMC")) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame()
sylvania_correlations_mean <- res_mean_df %>%
  select(starts_with("SYLVANIA")) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame()

res_df$period = NA
res_df$period[which(all_site_summary$year<1960)] = "past"
res_df$period[which(all_site_summary$year>2000)] = "present"
res_df$period[which(all_site_summary$year<1960)] = "past"
res_df$period[which(all_site_summary$year>2000)] = "present"





pdf('report/figures/res_cor_PCAmean.pdf')

ggcorrplot(goose_correlations_mean, method = "square", type = "lower", hc.order = FALSE, show.diag = TRUE) +
  scale_fill_distiller(
    palette = "PuOr", na.value = "white",
    direction = 1, limits = c(-1, 1),
    name = "Pearson\nCorrelation:") +
  ggtitle("Goose residual Correlations (mean PCA)")
ggcorrplot(harvard_correlations_mean, method = "square", type = "lower", hc.order = FALSE, show.diag = TRUE) +
  scale_fill_distiller(
    palette = "PuOr", na.value = "white",
    direction = 1, limits = c(-1, 1),
    name = "Pearson\nCorrelation:") +
  ggtitle("Harvard residual Correlations (mean PCA)")
ggcorrplot(nrp_correlations_mean, method = "square", type = "lower", hc.order = FALSE, show.diag = TRUE) +
  scale_fill_distiller(
    palette = "PuOr", na.value = "white",
    direction = 1, limits = c(-1, 1),
    name = "Pearson\nCorrelation:") +
  ggtitle("NRP residual Correlations (mean PCA)")
ggcorrplot(rooster_correlations_mean, method = "square", type = "lower", hc.order = FALSE, show.diag = TRUE) +
  scale_fill_distiller(
    palette = "PuOr", na.value = "white",
    direction = 1, limits = c(-1, 1),
    name = "Pearson\nCorrelation:") +
  ggtitle("Rooster residual Correlations (mean PCA)")
ggcorrplot(sylvania_correlations_mean, method = "square", type = "lower", hc.order = FALSE, show.diag = TRUE) +
  scale_fill_distiller(
    palette = "PuOr", na.value = "white",
    direction = 1, limits = c(-1, 1),
    name = "Pearson\nCorrelation:") +
  ggtitle("Sylvania residual Correlations (mean PCA)")
ggcorrplot(hmc_correlations_mean, method = "square", type = "lower", hc.order = FALSE, show.diag = TRUE) +
  scale_fill_distiller(
    palette = "PuOr", na.value = "white",
    direction = 1, limits = c(-1, 1),
    name = "Pearson\nCorrelation:") +
  ggtitle("HMC residual Correlations (mean PCA)")
dev.off()



####RESIDUALS PLOTS#####
#plotting residuals for each site for each taxa 
ggplot()+
  geom_point(data = res_mean_long , aes(x = year, y = value, color = taxon))+
  theme_light(base_size =11)+
  facet_wrap(~site)+
  ggtitle("mean PCA")
#ggsave("report/2025/meanPCA_res.jpg")

ggplot()+
  geom_point(data = res_mean_long , aes(x = year, y = value, color = taxon))+
  facet_wrap(~site, scales="free_y")+
  theme_light(base_size = 11)+
  ggtitle("mean PCA")
#ggsave("report/2025/meanPCA_res_freey.jpg")


ggplot(data = res_mean_long %>% filter(taxon== "QURU"))+
  geom_point(aes(x = year, y = value, color = site))+
  # facet_wrap(~site, scales = "free_y")+
  scale_color_manual(values = site_colors) +
  theme_light(base_size =11)+
  ggtitle("QURU mean PCA")

ggplot(data = res_mean_long %>% filter(taxon== "PIST"))+
  geom_point(aes(x = year, y = value, color = site))+
  # facet_wrap(~site, scales = "free_y")+
  scale_color_manual(values = site_colors) +
  theme_light(base_size =11)+
  ggtitle("PIST mean PCA")

ggplot(data = res_long %>% filter(taxon== "TSCA"))+
  geom_point(aes(x = year, y = value, color = site))+
  # facet_wrap(~site, scales = "free_y")+
  scale_color_manual(values = site_colors) +
  theme_light(base_size =11)+
  ggtitle("TSCA mean PCA")


#apply(models_mean, 1, function(x) models_mean[x,3][['residuals']])
#apply(models_mean, 1, function(x) models_mean[x,3])







###########################
######################################################################################################################

# foo = pollen_BVs_summary[, c('taxon', 'timeMid', 'bvc_mean', 'bvc_median')]
# colnames(foo) = c('taxon', 'year_ybp', 'bvc_mean', 'bvc_median')

foo = res_long %>% filter(site=='GOOSE')

foo = foo %>% group_by(taxon) %>% arrange(taxon, year)

year = sort(unique(foo$year))
# year_map = data.frame(year = year, year_idx = seq(1950, 1950 + length(year) - 1))
# year_map$year_k = seq(20.5, 0.5, by=-1)#round(year_map$year_ybp)
# 
# # 
# # library(tibbletime)
# # library(lubridate)
# 
# foo$year_k = year_map$year_k[match(foo$year_ybp, year_map$year_ybp)]
# 
# foo$year_idx = year_map$year_idx[match(foo$year_ybp, year_map$year_ybp)]
foo$year_month = paste0(foo$year, '01') 

foo$year_idx = ym(foo$year_month)

foo_tbl = as_tbl_time(foo, index=year_idx)

# does this work with groups?
taxa_anom = foo_tbl %>% 
  group_by(taxon) %>%
  time_decompose(value, method="stl", merge = TRUE)  %>%
  anomalize(remainder, method='iqr', alpha = 0.08) %>%
  time_recompose()

# pdf('figures/BVC_anom_grouped.pdf', width=10, height=14)
p = taxa_anom %>%
  plot_anomalies(ncol = 3, alpha_dots = 0.25)
print(p)
# dev.off()




taxa = unique(foo$taxon)

# taxa_anom_list = lapply(taxa, function(this_taxon){ subset(foo_tbl, taxon==this_taxon) %>% 
#   # group_by(taxon) %>%
#   time_decompose(bvc_mean, method="stl", merge = TRUE)  %>%
#   anomalize(remainder, method='iqr', alpha = 0.08) %>%
#   time_recompose()})
# 
# taxa_anom = taxa_anom_list %>% bind_rows()#.id = 'taxon')

taxa_anom %>%
  plot_anomalies(ncol = 3, alpha_dots = 0.25)

taxa_anom %>%
  plot_anomaly_decomposition(ncol = 3, alpha_dots = 0.25)



# set frequency
taxa_anom_test = foo_tbl %>% 
  group_by(taxon) %>%
  time_decompose(value, method="stl", merge = TRUE, frequency=10)  %>%
  anomalize(remainder, method='iqr', alpha = 0.08) %>%
  time_recompose()

taxa = unique(foo$taxon)


taxa_anom_test %>%
  plot_anomalies(ncol = 3, alpha_dots = 0.25)

ggplot(data=taxa_anom_test) +
  geom_point(aes(x=year, y=observed)) +
  geom_line(aes(x=year, y=(trend + season))) +
  facet_wrap(~taxon, scales='free_y') #+
# scale_x_reverse()

ggplot() +
  geom_point(data=taxa_anom, aes(x=year, y=observed*100, 
                                 colour=anomaly, shape=anomaly, size=anomaly)) +
  # scale_x_reverse() + 
  facet_wrap(~taxon, scales='free_y') +
  # scale_colour_brewer(palette = "Set1", direction=-1) +
  # scale_colour_manual(values = c('#b2df8a', '#33a02c')) +
  scale_colour_manual(values = c('grey34', 'indianred')) +
  scale_shape_manual(values=c(19, 19)) +
  scale_fill_brewer(palette = "BrBG", direction=-1) +
  scale_size_manual(values = c(1, 2)) +
  theme_bw() +
  xlab('year (YBP)') +
  ylab('ABI (KG/m2)')


ggplot() +
  geom_rect(data=climate_periods, inherit.aes=FALSE, aes(xmin=time_from, 
                                                         xmax=time_to, 
                                                         ymin=0,
                                                         ymax=13*100,
                                                         group=period_name,
                                                         fill=period_name),
            # colour='transparent',
            alpha=0.3) +
  geom_point(data=taxa_anom, aes(x=year_k, y=observed*100, 
                                 colour=anomaly, shape=anomaly,
                                 size = anomaly)) +
  scale_x_reverse() + 
  # facet_wrap(~taxon, scales='free_y') +
  # scale_colour_brewer(palette = "Set1", direction=-1) +
  # scale_shape_manual(values=c(19, 19)) 
  scale_colour_manual(values = c('grey34', 'indianred')) +
  scale_shape_manual(values=c(19, 19)) +
  scale_size_manual(values = c(1, 3)) +
  scale_fill_brewer(palette = "BrBG", direction=-1) +
  theme_bw() +
  xlab('year (YBP)') +
  ylab('biotic velocity (m/year)') +
  annotate("text", x=climate_periods$time_from - 0.8 + c(0, 0, 0, 0.2, -0.2, 0, 0), y=12*100, label=climate_periods$type)
ggsave('figures/BV_time_series_anom_all.pdf')

ggplot() +
  geom_rect(data=climate_periods, inherit.aes=FALSE, aes(xmin=time_from, 
                                                         xmax=time_to, 
                                                         ymin=0,
                                                         ymax=13*100,
                                                         group=period_name,
                                                         fill=period_name),
            # colour='transparent',
            alpha=0.3) +
  geom_point(data=taxa_anom, aes(x=year_k, y=observed*100, 
                                 colour=anomaly, shape=anomaly,
                                 size = anomaly)) +
  scale_x_reverse() + 
  # facet_wrap(~taxon, scales='free_y') +
  # scale_colour_brewer(palette = "Set1", direction=-1) +
  # scale_shape_manual(values=c(19, 19)) 
  scale_colour_manual(values = c('grey34', 'indianred')) +
  scale_shape_manual(values=c(NA, 19)) +
  scale_size_manual(values = c(1, 3)) +
  scale_fill_brewer(palette = "BrBG", direction=-1) +
  theme_bw() +
  xlab('year (YBP)') +
  ylab('biotic velocity (m/year)') +
  annotate("text", x=climate_periods$time_from - 0.8 + c(0, 0, 0, 0.2, -0.2, 0, 0), y=12*100, label=climate_periods$type)
ggsave('figures/BV_only_anom_all.pdf')

ggplot() +
  geom_rect(data=climate_periods, inherit.aes=FALSE, aes(xmin=time_from, 
                                                         xmax=time_to, 
                                                         ymin=-2*100,
                                                         ymax=9*100,
                                                         group=period_name,
                                                         fill=period_name),
            # colour='transparent',
            alpha=0.3) +
  geom_point(data=taxa_anom, aes(x=year_k, y=remainder*100, colour=anomaly, shape=anomaly, size=anomaly)) +
  scale_x_reverse() + 
  # facet_wrap(~taxon, scales='free_y') +
  # scale_colour_brewer(palette = "Set1", direction=-1) +
  # scale_shape_manual(values=c(19, 19)) 
  scale_colour_manual(values = c('grey34', 'indianred')) +
  scale_shape_manual(values=c(19, 19)) +
  scale_size_manual(values = c(1, 3)) +
  scale_fill_brewer(palette = "BrBG", direction=-1) +
  theme_bw() +
  xlab('year (kYBP)') +
  ylab('remainder (m/year)') +
  geom_hline(yintercept = 0, alpha=0.2) +
  annotate("text", x=climate_periods$time_from - 0.8 + c(0, 0, 0, 0.2, -0.2, 0, 0), y=8*100, label=climate_periods$type)
ggsave('figures/BV_time_series_anom_remainder_all.pdf')

ggplot() +
  geom_point(data=taxa_anom, aes(x=year, y=remainder*100, colour=anomaly, shape=anomaly, size=anomaly)) +
  scale_colour_manual(values = c('grey34', 'indianred')) +
  scale_shape_manual(values=c(19, 19)) +
  scale_size_manual(values = c(1, 3)) +
  scale_fill_brewer(palette = "BrBG", direction=-1) +
  theme_bw() +
  xlab('year (kYBP)') +
  ylab('remainder (m/year)')  +
  facet_wrap(~taxon, scales='free_y') +
  geom_hline(yintercept = 0, alpha=0.2) #+
# annotate("text", x=climate_periods$time_from, y=rep(Inf, 7), label=climate_periods$type)
# ggsave('figures/BV_time_series_anom_remainder_taxon.pdf')

ggplot() +
  geom_rect(data=climate_periods_taxa, inherit.aes=FALSE, aes(xmin=time_from, 
                                                              xmax=time_to, 
                                                              ymin=0, #ymin_trend,
                                                              ymax=ymax_trend,
                                                              group=period_name,
                                                              fill=period_name),
            colour='transparent',
            alpha=0.3) +
  geom_point(data=taxa_anom, aes(x=year_k, y=trend*100, colour=anomaly, shape=anomaly, size=anomaly)) +
  scale_x_reverse() + 
  # scale_colour_brewer(palette = "Set1", direction=-1) +
  # scale_shape_manual(values=c(19, 19)) 
  scale_colour_manual(values = c('grey34', 'indianred')) +
  scale_shape_manual(values=c(19, 19)) +
  scale_size_manual(values = c(1, 3)) +
  scale_fill_brewer(palette = "BrBG", direction=-1) +
  theme_bw() +
  xlab('year (kYBP)') +
  ylab('trend (m/year)')  +
  facet_wrap(~taxon, scales='free_y')# +
# geom_hline(yintercept = 0, alpha=0.2) #+
# annotate("text", x=climate_periods$time_from, y=rep(Inf, 7), label=climate_periods$type)
ggsave('figures/BV_time_series_anom_trend_taxon.pdf')

taxa_anom_other_rid = taxa_anom[which(taxa_anom$taxon != 'Other'),] 
climate_periods_taxa = climate_periods_taxa[which(climate_periods_taxa$taxon != 'Other'),]

# taxa_anom_other_rid$taxon

ggplot() +
  geom_rect(data=climate_periods_taxa, inherit.aes=FALSE, aes(xmin=time_from, 
                                                              xmax=time_to, 
                                                              ymin=0, #ymin_trend,
                                                              ymax=ymax_obs,
                                                              group=period_name,
                                                              fill=period_name),
            colour='transparent',
            alpha=0.3) +
  geom_line(data=taxa_anom_other_rid, aes(x=year_k, y=trend*100)) +
  geom_point(data=taxa_anom_other_rid, aes(x=year_k, y=observed*100, colour=anomaly, shape=anomaly, size=anomaly), alpha=0.5) +
  scale_x_reverse() + 
  # scale_colour_brewer(palette = "Set1", direction=-1) +
  # scale_shape_manual(values=c(19, 19)) 
  scale_colour_manual(values = c('grey34', 'indianred')) +
  scale_shape_manual(values=c(19, 19)) +
  scale_size_manual(values = c(1, 2)) +
  scale_fill_brewer(name = "period", palette = "BrBG", direction=-1) +
  theme_bw(12) +
  xlab('year (kYBP)') +
  ylab('Biotic velocity (m/year)')  +
  facet_wrap(~taxon, scales='free_y', ncol=4)# +
# geom_hline(yintercept = 0, alpha=0.2) #+
# annotate("text", x=climate_periods$time_from, y=rep(Inf, 7), label=climate_periods$type)
ggsave('figures/BV_time_series_anom_observed_trend_taxon.pdf')


# seasonal
ggplot() +
  geom_rect(data=climate_periods_taxa, inherit.aes=FALSE, aes(xmin=time_from, 
                                                              xmax=time_to, 
                                                              ymin=ymin_season,
                                                              ymax=ymax_season,
                                                              group=period_name,
                                                              fill=period_name),
            colour='transparent',
            alpha=0.3) +
  geom_line(data=taxa_anom_other_rid, aes(x=year_k, y=season*100)) +
  geom_point(data=taxa_anom_other_rid, aes(x=year_k, y=season*100, colour=anomaly, shape=anomaly, size=anomaly), alpha=0.5) +
  scale_x_reverse() + 
  # scale_colour_brewer(palette = "Set1", direction=-1) +
  # scale_shape_manual(values=c(19, 19)) 
  scale_colour_manual(values = c('grey34', 'indianred')) +
  scale_shape_manual(values=c(19, 19)) +
  scale_size_manual(values = c(1, 2)) +
  scale_fill_brewer(name = "period", palette = "BrBG", direction=-1) +
  theme_bw(12) +
  xlab('year (kYBP)') +
  ylab('Biotic velocity anomaly from trend (m/year)')  +
  facet_wrap(~taxon, scales='free_y', ncol=4)# +
# geom_hline(yintercept = 0, alpha=0.2) #+
# annotate("text", x=climate_periods$time_from, y=rep(Inf, 7), label=climate_periods$type)
ggsave('figures/BV_time_series_anom_seasonal_taxon.pdf')


# seasonal
ggplot() +
  geom_rect(data=climate_periods_taxa, inherit.aes=FALSE, aes(xmin=time_from, 
                                                              xmax=time_to, 
                                                              ymin=ymin_season,
                                                              ymax=ymax_season,
                                                              group=period_name,
                                                              fill=period_name),
            colour='transparent',
            alpha=0.3) +
  geom_line(data=taxa_anom_other_rid, aes(x=year_k, y=season*100, group=taxon)) +
  geom_point(data=taxa_anom_other_rid, aes(x=year_k, y=season*100, group=taxon, colour=anomaly, shape=anomaly, size=anomaly), alpha=0.5) +
  scale_x_reverse() + 
  # scale_colour_brewer(palette = "Set1", direction=-1) +
  # scale_shape_manual(values=c(19, 19)) 
  scale_colour_manual(values = c('grey34', 'indianred')) +
  scale_shape_manual(values=c(19, 19)) +
  scale_size_manual(values = c(1, 2)) +
  scale_fill_brewer(name = "period", palette = "BrBG", direction=-1) +
  theme_bw(12) +
  xlab('year (kYBP)') +
  ylab('Biotic velocity anomaly from trend (m/year)')  #+
# facet_wrap(~taxon, scales='free_y', ncol=4)# +
# geom_hline(yintercept = 0, alpha=0.2) #+
# annotate("text", x=climate_periods$time_from, y=rep(Inf, 7), label=climate_periods$type)
ggsave('figures/BV_time_series_anom_seasonal_all.pdf')

# seasonal
ggplot() +
  geom_rect(data=climate_periods_taxa, inherit.aes=FALSE, aes(xmin=time_from, 
                                                              xmax=time_to, 
                                                              ymin=ymin_obs,
                                                              ymax=ymax_obs,
                                                              group=period_name,
                                                              fill=period_name),
            colour='transparent',
            alpha=0.3) +
  geom_line(data=taxa_anom_other_rid, aes(x=year_k, y=(trend+season)*100)) +
  geom_point(data=taxa_anom_other_rid, aes(x=year_k, y=(observed)*100, colour=anomaly, shape=anomaly, size=anomaly), alpha=0.5) +
  scale_x_reverse() + 
  # scale_colour_brewer(palette = "Set1", direction=-1) +
  # scale_shape_manual(values=c(19, 19)) 
  scale_colour_manual(values = c('grey34', 'indianred')) +
  scale_shape_manual(values=c(19, 19)) +
  scale_size_manual(values = c(1, 2)) +
  scale_fill_brewer(name = "period", palette = "BrBG", direction=-1) +
  theme_bw(12) +
  xlab('year (kYBP)') +
  ylab('Biotic velocity (m/year)')  +
  facet_wrap(~taxon, scales='free_y', ncol=4)# +
# geom_hline(yintercept = 0, alpha=0.2) #+
# annotate("text", x=climate_periods$time_from, y=rep(Inf, 7), label=climate_periods$type)
ggsave('figures/BV_time_series_anom_trend_seasonal_taxon.pdf')

######################################################################
# res_long2 = res_long %>% 
#   group_by(site, taxon) %>% 
#   dplyr::reframe(zscore = (value - mean(value)) / sd(value))


res_long2 = res_long %>% 
  group_by(site, taxon) %>% 
  dplyr::mutate(zscore = (value - mean(value)) / sd(value))
res_long2$outlier = abs(res_long2$zscore) > 3  

ggplot(data=res_long2) + 
  geom_point(aes(x=year, y=value, colour=site, size=zscore)) + 
  facet_wrap(~taxon)

ggplot(data=res_long) + 
  geom_point(aes(x=year, y=value, colour=site)) + 
  facet_wrap(~taxon, scales='free_y') +
  geom_smooth(method='lm', aes(x=year, y=value, colour=site))



goose_correlations <- res_df %>%
  dplyr::select(starts_with("GOOSE")) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame()
rooster_correlations <- res_df %>%
  dplyr::select(starts_with("ROOSTER")) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame()
harvard_correlations <- res_df %>%
  dplyr::select(starts_with("HARVARD")) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame()
nrp_correlations <- res_df %>%
  dplyr::select(starts_with("NRP")) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame()
hmc_correlations <- res_df %>%
  dplyr::select(starts_with("HMC")) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame()
colnames(hmc_correlations) <- sub(".*_", "", colnames(hmc_correlations))
sylvania_correlations <- res_df %>%
  dplyr::select(starts_with("SYLVANIA")) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame()


res_df$period = NA
res_df$period[which(all_site_summary$year<1960)] = "past"
res_df$period[which(all_site_summary$year>2000)] = "present"



ggpairs(data = res_df,
        columns = which(startsWith(names(res_df), "HARVARD")), 
        ggplot2:: theme_light())

ggpairs(data = res_df,
        columns = which(startsWith(names(res_df), "NRP")))
ggpairs(data = res_df,
        columns = which(startsWith(names(res_df), "HMC")))
ggpairs(data = res_df,
        columns = which(startsWith(names(res_df), "ROOSTER")))
