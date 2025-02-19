AGBI_data = readRDS("AGBI_taxon_data.RDS")
pca_sites = readRDS("pca_sites.RDS")




AGBI_PCA = AGBI_data %>% 
  inner_join(goose_PCA, by= (c('year', 'site')))

#mod = lm(AGBI.mean ~ PCA1_summer + PCA2_summer, data=AGBI_PCA)


models = AGBI_PCA %>% 
  group_by(taxon) %>% 
  do(mod = lm(AGBI.mean ~ PCA1_summer + PCA2_summer, data=.)) 
 
  
#print(models$mod)

coefficients_summary = models %>%
  summarise(
    intercept = coef(mod)[1],
    slope_PCA1s = coef(mod)[2],
    slope_PCA2s = coef(mod)[3],
    Rsq = summary(mod)$r.squared
  ) %>% 
  mutate(taxon = models$taxon)

# Print the summary
print(coefficients_summary)
 
