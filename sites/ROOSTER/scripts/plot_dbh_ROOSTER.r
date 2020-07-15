library(reshape2)
library(ggplot2)

source('scripts/plot_funs.r')
# source('config')

site = "ROOSTER"

data_dir <- file.path('sites',site, 'data')
output_dir <- file.path('sites',site, 'output')
figure_dir <- file.path('sites',site, 'figures')

dvers = "v0.1"
mvers = "v0.1"

fname_data = paste0('tree_data_ROOSTER_STAN_', dvers)
dat = readRDS(paste0(data_dir, '/', fname_data, '.RDS'))

N_trees = dat$N_trees
N_years = dat$N_years
N_vals = dat$N_vals
logXobs = dat$logXobs
logPDobs = dat$logPDobs
year_idx = dat$year_idx
m2taxon = dat$m2taxon
N_taxa = dat$N_taxa
pdbh_year = dat$pdbh_year
idx_tree = dat$idx_tree
pdbh2val = dat$pdbh2val
x2tree = dat$x2tree
x2year = dat$x2year
taxaMatch = dat$taxaMatch
years = dat$years
plot_id = dat$plot_id
m2tree = dat$m2tree
m2t = dat$m2t

trees = seq(1, N_trees)

# fnames = c('ring_model_t_pdbh_IT')
fnames = "ring_model_t_pdbh_ROOSTER"
models = c('Model RW')

# get ring width data 
nmodels = length(fnames)
post = list()
for (i in 1:length(fnames)) {
  fname_model = fnames[i]
  post[[i]]   =   readRDS(file   = paste0(output_dir,'/', fname_model, '_', mvers, '.RDS'))
}  
niter =length(post$lp__)

burn  = 0#800#400
# niter = dim(out)[1]

# if only one model used
# post=post[[1]]

########################################################################################################################################
library(RColorBrewer)
darkcols <- brewer.pal(4, "Set1")

nc=FALSE

trees = sort(unique(x2tree))

pdf(paste0(figure_dir, '/growth_results_', site, '_', mvers, '.pdf'), width=12, height=10)
for (i in 1:N_trees){
  
  print(paste0('Tree ', i))
  
  tree = trees[i]
  tree_idx = which(x2tree == tree)

  X_qs = apply(post$X[,tree_idx], 2, function(x) quantile(x, probs=c(0.025,0.5, 0.975)))
  
  tree_years = x2year[tree_idx]
  
  # raw data
  dat_idx   = which(m2tree == tree)
  rws = exp(logXobs[dat_idx])
  
  if (length(rws)>0){
    ymin = min(c(rws,X_qs[1,]))
    ymax = max(c(rws,X_qs[3,]))
  } else {
    ymin = min(X_qs[1,])
    ymax = max(X_qs[3,])
  }
  
  
  tree_dat = data.frame(value=numeric(0), year=numeric(0), var=character(0), type=character(0), subtype=character(0))
  tree_dat = rbind(tree_dat, data.frame(value=X_qs[2,], year=years[tree_years], var=rep('RW'), type=rep('model'), subtype=rep('Both')))
  
  ribbon_dat = data.frame(L=numeric(0), U=numeric(0), year=numeric(0), var=character(0), subtype=character(0))
  ribbon_dat = rbind(ribbon_dat, data.frame(L=X_qs[1,], U=X_qs[3,], year=years[tree_years], var=rep('RW'), subtype=rep('Both')))
  
  idx = which((m2tree == tree))
  yrs = years[m2t[idx]]
  tree_dat = rbind(tree_dat, data.frame(value=exp(logXobs[idx]), year=yrs, var=rep('RW'), type=rep('data'), subtype=rep("Core")))

  # ggplot(tree_dat) + geom_line(aes(x=year, y=value, colour=subtype))
  
  # now plot D!
  D_qs = apply(post$D[,tree_idx], 2, function(x) quantile(x, probs=c(0.025,0.5, 0.975)))

  tree_dat = rbind(tree_dat, data.frame(value=D_qs[2,], year=years[tree_years], var=rep('DBH'), type=rep('model'), subtype=rep('Both')))
  
  ribbon_dat = rbind(ribbon_dat, data.frame(L=D_qs[1,], U=D_qs[3,], year=years[tree_years], var=rep('DBH'), subtype=rep('Both')))
  
  ymax = max(c(D_qs[2,]), na.rm=TRUE)
  
  # tree_dat = rbind(tree_dat, data.frame(value=D_dat, year=years[yrs], var=rep('DBH'), type=rep('data'), subtype=rep('census')))
  
  
  if (!is.na(logPDobs)){

    yrs = pdbh_year[tree]
    PD_dat = exp(logPDobs[tree])
    
    ymax = max(c(PD_dat,ymax))
    
    tree_dat = rbind(tree_dat, data.frame(value=PD_dat, year=years[yrs], var=rep('DBH'), type=rep('data'), subtype=rep('paleon')))
    
  } else {
    tree_dat = rbind(tree_dat, data.frame(value=NA, year=NA, var=rep('DBH'), type=rep('data'), subtype=rep('paleon')))
    
  }
  
  cols = c('#084594', '#8c2d04')
  # cols_fill = c('#4292c6', '#feedde')
  cols_fill = c('#4292c6', 'coral2')

  p <- ggplot() + #geom_ribbon(data=ribbon_dat, aes(x=year, ymin=L, ymax=U, fill=subtype), alpha=0.4) + 
    geom_line(data=subset(tree_dat, type %in% c('model')), aes(x=year, y=value, group=1), size=1) + 
    geom_point(data=subset(tree_dat, (type %in% c('data')) & (var %in% c('RW')) & (subtype %in% c('Core'))),
               aes(x=year, y=value, group=subtype), colour='brown', size=4, shape=20, alpha=0.5) +
    # geom_line(data=subset(tree_dat, (type %in% c('data')) & (var %in% c('RW')) & (!(subtype %in% c('raw avg')))), 
    #           aes(x=year, y=value, group=subtype), alpha=0.7,  colour='black', linetype=2, size=0.8, show.legend=FALSE) +
    geom_point(data=subset(tree_dat, (type %in% c('data')) & (var %in% c('DBH'))) ,
               aes(x=year, y=value, shape=subtype), size=3) +
    # scale_color_manual(values=cols, name='Data', labels=c('RW + Census', 'RW')) + 
    # scale_fill_manual(values=cols_fill, name='Data', labels=c('RW + Census', 'RW')) + 
    # scale_shape_manual(values=c(19, 8, 10), guide='none') +
    theme_bw()+
    theme(axis.title.y=element_blank()) +
    theme(axis.title=element_text(size=18),
          axis.text=element_text(size=18),
          legend.text=element_text(size=18),
          legend.title=element_text(size=18),
          strip.text = element_text(size=18)) +
    # scale_x_continuous(breaks=seq(min(years), max(years), by=5)) +
    facet_grid(var~., scales="free_y") + 
    ggtitle(paste0('Stat id: ', i)) #+ 
  # annotate("text",  x=min(tree_dat$year), y = Inf, label = "Some text", vjust=1, hjust=-3)
  
  print(p)
}
dev.off()


# ggplot() +  geom_ribbon(data=ab_p_quants, aes(x=year, ymin=ab25, ymax=ab975, fill=model), alpha=0.4) +
#   geom_line(data=ab_p_quants, aes(x=year, y=ab50, colour=model), size=1) + 
#   geom_line(data=ab_m_sum, aes(x=year, y=ab, colour='Empirical RW', fill='Empirical RW'),size=1) + 
#   geom_point(data=ab_c_sum, aes(x=year, y=ab, colour='Empirical Census', fill='Empirical Census'), size=2) + 
#   # geom_line(data=ab_p_quants, aes(x=year, y=ab25, colour=model), linetype=2, size=0.5) + 
#   # geom_line(data=ab_p_quants, aes(x=year, y=ab975, colour=model), linetype=2, size=0.5) + 
#   facet_grid(site_id~.) + scale_color_manual(values=cols, name='Method')+
#   scale_fill_manual(values=cols_fill, name='Method')+
#   theme_bw() + theme(axis.title=element_text(size=14), axis.text=element_text(size=14)) +
#   ylab("Biomass (Mg/ha)") + xlab('Year') +
#   scale_x_continuous(breaks=seq(min(years), max(years), by=5))
# ggsave(file=paste0('figures/AGB_by_site_', mvers, '.pdf'))
# ggsave(file=paste0('figures/AGB_by_site_', mvers, '.png'))

####################################################################################################################################
plot_sig(post, site, mvers, sig_d=FALSE)

#########################################################################################################################################

