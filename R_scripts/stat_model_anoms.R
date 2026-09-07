library(tibbletime)
library(lubridate)
library(anomalize)

head(AGBI_taxon)


foo = AGBI_taxon

foo$year_idx = foo$year
foo$year_idx = ymd(paste0(foo$year_idx, "-01-01"))
# foo$year_idx = as.Date(foo$year_idx, format="%Y")


foo_tbl = as_tbl_time(foo, index=year_idx)
foo_tbl = foo_tbl[which(!is.na(foo_tbl$AGBI.mid)),]

site_taxa_anom = foo_tbl %>% 
  group_by(site, taxon) %>%
  time_decompose(AGBI.mid, method="stl", merge = TRUE)  %>%
  anomalize(remainder, method='iqr', alpha = 0.08) %>%
  time_recompose()

site_taxa_anom = foo_tbl %>% 
  group_by(site, taxon) %>%
  time_decompose(AGBI.mid, method="stl", merge=TRUE, frequency=16)  %>%
  anomalize(remainder, method='iqr', alpha = 0.08) %>%
  time_recompose()

this_site_taxa_anom = subset(site_taxa_anom, site=='HARVARD')

ggplot(data=this_site_taxa_anom) +
  geom_point(aes(x=year, y=observed)) +
  geom_line(aes(x=year, y=(trend + season))) +
  facet_wrap(~taxon, scales='free_y')

site_taxa_anom = foo_tbl %>%
  group_by(site, taxon) %>%
  # time_decompose(AGBI.mid, method="stl", merge = TRUE, trend=50)  %>%
  anomalize(target=AGBI.mid, method='iqr', alpha = 0.08)

this_site_taxa_anom = subset(site_taxa_anom, site=='HARVARD')

ggplot(data=this_site_taxa_anom) +
  geom_point(aes(x=year, y=AGBI.mid, colour=anomaly)) +
  # geom_line(aes(x=year, y=(trend + season))) +
  facet_wrap(~taxon, scales='free_y')


ggplot(data=this_site_taxa_anom) +
  geom_point(aes(x=year, y=observed)) +
  geom_line(aes(x=year, y=(trend + season))) +
  facet_wrap(~taxon, scales='free_y')

# 
# p = site_taxa_anom %>%
#   plot_anomalies(ncol = 3, alpha_dots = 0.25)
# print(p)



find_local_minima <- function(x) {
  # Find indices where the slope changes from negative to positive
  which(diff(sign(diff(x))) == 2) + 1
}

# Example Usage:
set.seed(42)
time_series <- sin(seq(0, 10, length.out = 100)) + rnorm(100, sd = 0.1) # Noisy sine wave
minima_indices <- find_local_minima(time_series)

# View the values at those indices
time_series[minima_indices]

plot(time_series)


# library(spatialEco)
# 
# foo_tbl = subset(foo_tbl, site=='HARVARD' & taxon == 'QURU')
# 
# lmm = local.min.max(foo_tbl$AGBI.mid)

library(pracma)

hampel_results <- hampel(foo_tbl$AGBI.mid, k = 3, t0 = 3)
local_anomalies <- hampel_results$ind


site_taxa_anom = foo_tbl %>%
  group_by(site, taxon) %>%
  # time_decompose(AGBI.mid, method="stl", merge = TRUE, trend=50)  %>%
  hampel(x=AGBI.mid, k=3, t0=3)


##################################################################################
# find anomalous years
##################################################################################

find_disturb =function (x, k, t0 = 3) 
{
  n <- length(x)
  y <- x
  zscore <- rep(NA, n)
  ind <- c()
  L <- 1.4826
  for (i in (k + 1):n) {
    x0 <- median(x[(i - k):i])
    S0 <- L * median(abs(x[(i - k):i] - x0))
    # if (abs(x[i] - x0) > t0 * S0) {
    #   y[i] <- x0
    #   ind <- c(ind, i)
    # }
    zscore[i] = (x[i] - x0) / S0
    if ((x[i] - x0) < (-t0 * S0)) {
      y[i] <- x0
      ind <- c(ind, i)
    }
  }
  list(y = y, zscore = zscore, ind = ind)
}


find_disturb_new =function (x, k, t0 = 3)
{
  n <- length(x)
  y <- x
  zscore <- rep(NA, n)
  ind <- c()
  L <- 1.4826
  for (i in (k + 1):n) {
    x0 <- mean(x[(i - k):i])
    S0 <- L * median(abs(x[(i - k):i] - x0))
    # if (abs(x[i] - x0) > t0 * S0) {
    #   y[i] <- x0
    #   ind <- c(ind, i)
    # }
    zscore[i] = (x[i] - x0) / S0
    if ((x[i] - x0) < (-t0 * S0)) {
      y[i] <- x0
      ind <- c(ind, i)
    }
  }
  list(y = y, zscore = zscore, ind = ind)
}



AGBI_taxon_site_anom = AGBI_taxon_site

AGBI_taxon_site_anom = AGBI_taxon_site_anom[which(!is.na(AGBI_taxon_site_anom$AGBI.mid)),]

AGBI_taxon_site_anom$anom = NA
AGBI_taxon_site_anom$disturb = NA

AGBI_taxon_anom = AGBI_taxon 

AGBI_taxon_anom = AGBI_taxon_anom[which(!is.na(AGBI_taxon_anom$AGBI.mid)),]

AGBI_taxon_anom$anom = NA
AGBI_taxon_anom$disturb = NA

k=7
t0=2

pdf(paste0('figures/AGBI_stat_model_anomalies_k', k, '_z', t0*10, '.pdf'), width=10,height=8)
for (site in sites){
  
  print(site)
  
  AGBI_taxon_anom_site = AGBI_taxon_anom[which(AGBI_taxon_anom$site == site),]
  
  these_taxa = unique(AGBI_taxon_anom_site$taxon)
  for (taxon in these_taxa) {
    
    print(paste0('>> ', taxon))
    
    idx_site_taxon = which((AGBI_taxon_anom$site == site)&(AGBI_taxon_anom$taxon == taxon))
    
    this_AGBI = AGBI_taxon_anom[idx_site_taxon, 'AGBI.mid']$AGBI.mid
    
    this_hampel = hampel(x=this_AGBI, k=3, t0=3)
    this_disturb = find_disturb(x=this_AGBI, k=k, t0=t0)
    this_disturb
    
    this_disturb_new = find_disturb_new(x=this_AGBI, k=k, t0=t0)
    
    
    AGBI_taxon_anom[idx_site_taxon, 'anom'] = FALSE
    AGBI_taxon_anom[idx_site_taxon[this_hampel$ind], 'anom'] = TRUE
    
    AGBI_taxon_anom[idx_site_taxon, 'disturb'] = FALSE
    AGBI_taxon_anom[idx_site_taxon[this_disturb$ind], 'disturb'] = TRUE
    
    # ggplot(data=AGBI_taxon_anom[idx_site_taxon, ]) +
    #   geom_line(aes(x=year, y=AGBI.mid)) +
    #   geom_point(aes(x=year, y=AGBI.mid, colour=anomaly)) +
    #   theme_light() 
    
  }
  
  p = ggplot(data=AGBI_taxon_anom[which(AGBI_taxon_anom$site == site),]) +
    geom_line(aes(x=year, y=AGBI.mid)) +
    geom_point(aes(x=year, y=AGBI.mid, colour=disturb)) +
    theme_light() +
    facet_wrap(~taxon, scales='free_y') +
    scale_colour_manual(values = c('grey34', 'indianred')) +
    ggtitle(site)
  print(p)
  
  this_site_AGBI = AGBI_taxon_site_anom[which(AGBI_taxon_site_anom$site == site),]$AGBI.mid
  this_site_years = AGBI_taxon_site_anom[which(AGBI_taxon_site_anom$site == site),]$year
  
  
  this_site_hampel = hampel(x=this_site_AGBI, k=3, t0=3)
  
  this_site_disturb = find_disturb(x=this_site_AGBI, k=k, t0=t0)
  this_site_disturb
  this_site_years[this_site_disturb$ind]    
  
  
  this_site_disturb = find_disturb(x=this_site_AGBI, k=k, t0=t0)
  this_site_disturb
  
  AGBI_taxon_site_anom[which(AGBI_taxon_site_anom$site == site), 'anomaly'] = FALSE
  AGBI_taxon_site_anom[which(AGBI_taxon_site_anom$site == site)[this_site_hampel$ind], 'anomaly'] = TRUE
  
  AGBI_taxon_site_anom[which(AGBI_taxon_site_anom$site == site), 'disturb'] = FALSE
  AGBI_taxon_site_anom[which(AGBI_taxon_site_anom$site == site)[this_site_disturb$ind], 'disturb'] = TRUE
  
  p = ggplot(data=AGBI_taxon_site_anom[which(AGBI_taxon_site_anom$site == site),]) +
    geom_line(aes(x=year, y=AGBI.mid)) +
    geom_point(aes(x=year, y=AGBI.mid, colour=disturb)) +
    theme_light() +
    # facet_wrap(~taxon, scales='free_y') +
    scale_colour_manual(values = c('grey34', 'indianred'))  +
    ggtitle(site)
  print(p)

  # p = ggplot(data=AGBI_taxon_anom[which(AGBI_taxon_anom$site == site),]) +
  #   geom_line(aes(x=year, y=AGBI.mid)) +
  #   geom_point(aes(x=year, y=AGBI.mid, colour=anomaly)) +
  #   theme_light() +
  #   facet_wrap(~taxon, scales='free_y') +
  #   scale_colour_manual(values = c('grey34', 'indianred')) 
  # print(p)
  
}
dev.off()

ggplot(data=AGBI_taxon_anom[which(AGBI_taxon_anom$site == site),]) +
  geom_point(aes(x=year, y=taxon, colour=disturb))


saveRDS(AGBI_taxon_anom, 'reboot/AGBI_taxon_anom.RDS')
saveRDS(AGBI_taxon_site_anom, 'reboot/AGBI_taxon_site_anom.RDS')



##################################################################################
# plot assigned disturbance years
##################################################################################

ggplot(data=AGBI_taxon_anom) +
  geom_tile(aes(x=year, y=taxon, fill=disturb)) +
  facet_wrap(~site, scales='free_y')

taxon_disturb = AGBI_taxon_anom %>% group_by(site, year) %>% dplyr::summarize(disturb_count = sum(disturb))

ggplot(data=taxon_disturb) +
  geom_col(aes(x=year, y=disturb_count)) +
  facet_wrap(~site, scales='free_y')




