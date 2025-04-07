library(dplyr)

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


ggplot()
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
  mutate(year = 1950:2011) %>%
  select(year, everything())

res_long <- res_df %>%
  pivot_longer(cols = GOOSE_ACRU:SYLVANIA_TSCA, names_to = "site_taxon", values_to = "value") %>%
  separate(site_taxon, into = c("site", "taxon"), sep = "_")


####RESIDUALS PLOTS#####
#plotting residuals for each site for each taxa 
site_levels <- unique(res_long$site)
site_colors <- RColorBrewer::brewer.pal(length(site_levels), "Set1")
names(site_colors) <- site_levels


ggplot()+
  geom_point(data = res_long , aes(x = year, y = value, color = taxon))+
  facet_wrap(~site)+
  theme_light(base_size = 11)+
  ggtitle("PCA res")

ggplot()+
  geom_point(data = res_long , aes(x = year, y = value, color = taxon))+
  facet_wrap(~site, scales = "free_y")+
  theme_light(base_size = 11)+
  ggtitle("PCA res")


ggplot(data = res_long %>% filter(site== "HARVARD"))+
  geom_point(aes(x = year, y = value, color = taxon))+
  # facet_wrap(~site, scales = "free_y")+
  theme_light(base_size =11)+
  ggtitle("HARVARD")

ggplot(data = res_long %>% filter(taxon== "QURU"))+
  geom_point(aes(x = year, y = value, color = site))+
 # facet_wrap(~site, scales = "free_y")+
  scale_color_manual(values = site_colors) +
  theme_light(base_size =11)+
  ggtitle("QURU")

ggplot(data = res_long %>% filter(taxon== "PIST"))+
  geom_point(aes(x = year, y = value, color = site))+
  # facet_wrap(~site, scales = "free_y")+
  scale_color_manual(values = site_colors) +
  theme_light(base_size =11)+
  ggtitle("PIST")

ggplot(data = res_long %>% filter(taxon== "TSCA"))+
  geom_point(aes(x = year, y = value, color = site))+
  # facet_wrap(~site, scales = "free_y")+
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


