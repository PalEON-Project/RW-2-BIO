library(ggplot2)
library(grid)
library(reshape2)
library(dplyr)
library(tidyr)
# library(ggdogs)

source('sites/GOOSE/inst/config.R')
# create save folders for data 
site_dir <- file.path('sites',site)

# fname_model = paste0(c('ring_model_t_pdbh_sigd_species_STAN'), '_', site, '_',mvers,'_',dvers,'.RDS')
fname_model = paste0(c('ring_model_t_pdbh_sigd_species_sigxk_STAN'), '_', site, '_',mvers,'_',dvers,'.RDS')
models = c('Model RW')
output_dir = file.path(site_dir,'runs',paste0(mvers,'_',dvers),'output')

dat = readRDS(file.path(site_dir,'runs',paste0(mvers,'_',dvers),'input',paste0('tree_data_', site ,'_STAN_',mvers,'_', dvers, '.RDS')))
post = readRDS(paste0(output_dir,'/', fname_model))

list2env(dat, envir = globalenv())

# beta_t = post$beta_t

variables = colnames(post[,1,])
temp.ind = which(substr(variables, 1, 7) == 'beta_t[')#which(variables == paste0('beta_t[', tree, ',' ,byr, ']'))
temp.data = reshape2::melt(post[,,temp.ind])
#   # temp.data$iterations = seq(1, nrow(temp.data))
var.split = strsplit(as.vector(temp.data$parameters), "\\[|,|\\]")
temp.data$year = as.numeric(lapply(var.split, function(x) x[[2]]))
temp.data$taxon_id = as.numeric(lapply(var.split, function(x) x[[3]]))

beta_t_quant = temp.data %>% 
  group_by(year, taxon_id) %>%  
  dplyr::summarize(lo = quantile(value, 0.025, na.rm=TRUE),
            median = quantile(value, 0.5, na.rm=TRUE),
            hi = quantile(value, 0.975, na.rm=TRUE),
            mean = mean(value, na.rm=TRUE),
            .groups = 'keep')

# foo = apply(beta_t, c(2,3), function(x) quantile(x, c(0.025, 0.5, 0.975)))
# bar = melt(foo)
# beta_t_quant = bar %>% pivot_wider(names_from = Var1, values_from = value)
beta_t_quant$taxon_id = taxa[beta_t_quant$taxon_id]
colnames(beta_t_quant) = c('year', 'taxon_id', 'lo', 'mid', 'hi')

beta_t_quant$year = years[beta_t_quant$year]
# beta_t_quant$species_id = species_ids[beta_t_quant$species_id]

# beta_t_quant = data.frame(t(apply(beta_t, 2, function(x) quantile(x, c(0.025, 0.5, 0.975)))))
# colnames(beta_t_quant) = c('lo', 'mid', 'hi')

# beta_t_quant$year = years

year_lo = 1930#min(years)
year_hi = max(years)

taxa_keep = taxa[as.numeric(table(Tr2taxon))>2]

ggplot(data=beta_t_quant) +
  geom_hline(aes(yintercept=0), lty=2, lwd=1.2) +
  geom_point(aes(x=year, y=mid)) + 
  geom_linerange(aes(x=year, ymin=lo, ymax=hi)) +
  xlab('year') +
  ylab('beta_t') +
  xlim(c(year_lo, year_hi)) +
  theme_bw(16) +
  facet_grid(taxon_id~.)
# ggsave(file.path(site_dir,'runs',paste0(mvers,'_',dvers),'figures','time_species_effect.pdf'))

ggplot(data=subset(beta_t_quant, taxon_id %in% taxa_keep)) +
  geom_hline(aes(yintercept=0), lty=2, lwd=1.2) +
  geom_point(aes(x=year, y=mid)) + 
  geom_linerange(aes(x=year, ymin=lo, ymax=hi)) +
  xlab('year') +
  ylab('beta_t') +
  xlim(c(year_lo, year_hi)) +
  theme_bw(16) +
  facet_grid(taxon_id~., scales ='free_y')
ggsave(file.path(site_dir,'runs',paste0(mvers,'_',dvers),'figures','time_species_effect.pdf'))


# pdf(file.path(site_dir,'runs',paste0(mvers,'_',dvers),'figures','tree_growth_model_data.pdf'), width=10, height=6)

