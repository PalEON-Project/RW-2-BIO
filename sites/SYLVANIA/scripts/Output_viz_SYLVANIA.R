## This script provides a rough visualization of the STAN model output

## Prepare workspace
rm(list = ls())
library(ggplot2)
library(reshape2)
library(dplyr)
setwd('~/Google Drive 2/McLachlan/SU20/npp-stat-model/')

## Load model output and site data
out = readRDS('sites/SYLVANIA/output/ring_model_t_pdbh_SYLVANIA_072020.RDS')
data = readRDS('sites/SYLVANIA/data/tree_data_SYLVANIA_STAN_072020.RDS')

# Unlist
for(i in 1:length(out)){
  temp = out[[i]]
  eval(parse(text=paste(names(out)[[i]],"= temp")))
}
rm(out)
rm(temp)

# Reformat
beta_sd = as.vector(beta_sd)
beta_t_sd = as.vector(beta_t_sd)
beta0 = as.vector(beta0)
sig_x = as.vector(sig_x)
sig_x_obs = as.vector(sig_x_obs)

#colnames(beta) = 1:63
#rownames(beta) = 1:7500

#beta_melt = melt(beta)
#colnames(beta_melt) = c('Iteration', 'Tree', 'Val')

# Extract species from data
#spec = matrix(, nrow = data$N_trees, ncol = data$N_years)
#for(i in 1:data$N_trees){
#  for(j in 1:data$N_years){
#    spec[i,j] = data$taxon[i]
#  }
#}

#colnames(spec) = data$years
#rownames(spec) = 1:data$N_trees

#spec_melt = melt(spec)
#colnames(spec_melt) = c('Tree', 'Year', 'Species')

# Add species to beta data frame
#beta_melt = beta_melt %>%
#  full_join(spec_melt, by = 'Tree')

# Repeat for beta t
#colnames(beta_t) = data$years
#rownames(beta_t) = 1:7500

#beta_t_melt = melt(beta_t)
#colnames(beta_t_melt) = c('Iteration', 'Year', 'Val')

# Repeat for D0
#colnames(D0) = 1:data$N_trees
#rownames(D0) = 1:7500

#D0_melt = melt(D0)
#colnames(D0_melt) = c('Iteration', 'Tree', 'Val')

#D0_melt = D0_melt %>%
#  full_join(spec_melt, by = 'Tree')

######################
## Biomass analysis ##
######################

## This section provides a visual check on the derived biomass output from
## the model by species

## Are the trees increasing in biomass over time?
## Are there any weird dips in biomass?

# Index
keep = c(7250:7500)
nkeep = length(keep)
ntree = data$N_trees
nyear = data$N_years

# Match tree to taxon
t2tax = cbind(c(1:data$N_trees), data$taxon)
t2tax = as.data.frame(t2tax)
colnames(t2tax) = c('Tree', 'Species')

# Add beta coefficients for each species
acro_level3a = read.csv('data/acronym_to_level3a_v0.2.csv', stringsAsFactors = FALSE)
acro_level3a = left_join(data$taxaMatch, acro_level3a, by = c('species'='acronym'))
choj = read.csv('data/level3a_to_chojnacky_v0.4.csv', stringsAsFactors = FALSE)
choj = left_join(acro_level3a, choj, id = 'level3a') 

# Match beta coefficients to tree
for(i in 1:data$N_trees){
  if(t2tax$Species[i] == 1){
    t2tax$b0[i] = choj$beta0[1]
    t2tax$b1[i] = choj$beta1[1]
  }
  if(t2tax$Species[i] == 2){
    t2tax$b0[i] = choj$beta0[2]
    t2tax$b1[i] = choj$beta1[2]
  }
  if(t2tax$Species[i] == 3){
    t2tax$b0[i] = choj$beta0[3]
    t2tax$b1[i] = choj$beta1[3]
  }
  if(t2tax$Species[i] == 4){
    t2tax$b0[i] = choj$beta0[4]
    t2tax$b1[i] = choj$beta1[4]
  }
}

# Storage
biom = array(, dim = c(ntree, nyear + 1, nkeep))

# Match value to taxon
v2tax = cbind(c(1:data$N_vals), data$x2tree, data$x2year)
v2tax = as.data.frame(v2tax)
colnames(v2tax) = c('Index', 'Tree', 'Year')

# Match species and beta coefficients to each value
for(i in 1:data$N_vals){
  if(v2tax$Tree[i] %in% which(t2tax$Species == 1)){
    v2tax$Species[i] = 1
  }
  if(v2tax$Tree[i] %in% which(t2tax$Species == 2)){
    v2tax$Species[i] = 2
  }
  if(v2tax$Tree[i] %in% which(t2tax$Species == 3)){
    v2tax$Species[i] = 3
  }
  if(v2tax$Tree[i] %in% which(t2tax$Species == 4)){
    v2tax$Species[i] = 4
  }
  if(v2tax$Species[i] == 1){
    v2tax$b0 = choj$beta0[1]
    v2tax$b1 = choj$beta1[2]
  }
  if(v2tax$Species[i] == 2){
    v2tax$b0 = choj$beta0[2]
    v2tax$b1 = choj$beta1[2]
  }
  if(v2tax$Species[i] == 3){
    v2tax$b0 = choj$beta0[3]
    v2tax$b1 = choj$beta1[3]
  }
  if(v2tax$Species[i] == 4){
    v2tax$b0 = choj$beta0[4]
    v2tax$b1 = choj$beta1[4]
  }
}

# Calculate biomass from DBH
for(j in 1:data$N_vals){
  b0 = v2tax$b0[j]
  b1 = v2tax$b1[j]
  tree = v2tax$Tree[j]
  year = v2tax$Year[j]
  ind = 0
  for(i in keep){
    bio = b0 + b1 * log(D[i,j])
    ind = ind + 1
    biom[tree, year+1, ind] = exp(bio)
  }
  print(j)
}

# Organize in data frame
biom_melt = melt(biom)
colnames(biom_melt) = c('Tree', 'Year', 'Iteration', 'Biomass')

biom_quants = apply(biom, c(1,2), quantile, probs = c(0.025, 0.5, 0.975), na.rm = T)
biom_quants_melt = melt(biom_quants)
colnames(biom_quants_melt) = c('Quantile', 'Tree', 'Year', 'Biomass')

# Acer biomass
spec_1_biom_quants = biom_quants_melt %>%
  filter(Tree %in% c(1:3))

# Plot Acer biomass
ggplot() +
  geom_line(aes(x = spec_1_biom_quants$Year[which(spec_1_biom_quants$Quantile == '50%')], y = spec_1_biom_quants$Biomass[which(spec_1_biom_quants$Quantile == '50%')], group = as.factor(spec_1_biom_quants$Tree[which(spec_1_biom_quants$Quantile == '50%')]))) +
  geom_ribbon(aes(x = spec_1_biom_quants$Year[which(spec_1_biom_quants$Quantile == '50%')], ymin = spec_1_biom_quants$Biomass[which(spec_1_biom_quants$Quantile == '2.5%')], ymax = spec_1_biom_quants$Biomass[which(spec_1_biom_quants$Quantile == '97.5%')], group = as.factor(spec_1_biom_quants$Tree[which(spec_1_biom_quants$Quantile == '50%')]), fill = as.factor(spec_1_biom_quants$Tree[which(spec_1_biom_quants$Quantile == '50%')])), alpha = 0.5, show.legend = F) +
  xlab('Year') + ylab('Biomass') +
  ggtitle('ACSA')

# Betula biomass
spec_2_biom_quants = biom_quants_melt %>%
  filter(Tree %in% c(4:15))

# Plot Betula biomass
ggplot() +
  geom_line(aes(x = spec_2_biom_quants$Year[which(spec_2_biom_quants$Quantile == '50%')], y = spec_2_biom_quants$Biomass[which(spec_2_biom_quants$Quantile == '50%')], group = as.factor(spec_2_biom_quants$Tree[which(spec_2_biom_quants$Quantile == '50%')]))) +
  geom_ribbon(aes(x = spec_2_biom_quants$Year[which(spec_2_biom_quants$Quantile == '50%')], ymin = spec_2_biom_quants$Biomass[which(spec_2_biom_quants$Quantile == '2.5%')], ymax = spec_2_biom_quants$Biomass[which(spec_2_biom_quants$Quantile == '97.5%')], group = as.factor(spec_2_biom_quants$Tree[which(spec_2_biom_quants$Quantile == '50%')]), fill = as.factor(spec_2_biom_quants$Tree[which(spec_2_biom_quants == '50%')])), alpha = 0.5, show.legend = F) +
  xlab('Year') + ylab('Biomass') +
  ggtitle('BEAL')

# Thuja biomass
spec_3_biom_quants = biom_quants_melt %>%
  filter(Tree %in% c(16:26))

# Plot Thuja biomass
ggplot() +
  geom_line(aes(x = spec_3_biom_quants$Year[which(spec_3_biom_quants$Quantile == '50%')], y = spec_3_biom_quants$Biomass[which(spec_3_biom_quants$Quantile == '50%')], group = as.factor(spec_3_biom_quants$Tree[which(spec_3_biom_quants$Quantile == '50%')]))) +
  geom_ribbon(aes(x = spec_3_biom_quants$Year[which(spec_3_biom_quants$Quantile == '50%')], ymin = spec_3_biom_quants$Biomass[which(spec_3_biom_quants$Quantile == '2.5%')], ymax = spec_3_biom_quants$Biomass[which(spec_3_biom_quants$Quantile == '97.5%')], group = as.factor(spec_3_biom_quants$Tree[which(spec_3_biom_quants$Quantile == '50%')]), fill = as.factor(spec_3_biom_quants$Tree[which(spec_3_biom_quants$Quantile == '50%')])), alpha = 0.5, show.legend = F) +
  xlab('Year') + ylab('Biomass') +
  ggtitle('THOC')

# Tsuga biomass
spec_4_biom_quants = biom_quants_melt %>%
  filter(Tree %in% c(27:63))

# Plot Tsuga biomass
ggplot() +
  geom_line(aes(x = spec_4_biom_quants$Year[which(spec_4_biom_quants$Quantile == '50%')], y = spec_4_biom_quants$Biomass[which(spec_4_biom_quants$Quantile == '50%')], group = as.factor(spec_4_biom_quants$Tree[which(spec_4_biom_quants$Quantile == '50%')]))) +
  geom_ribbon(aes(x = spec_4_biom_quants$Year[which(spec_4_biom_quants$Quantile == '50%')], ymin = spec_4_biom_quants$Biomass[which(spec_4_biom_quants$Quantile == '2.5%')], ymax = spec_4_biom_quants$Biomass[which(spec_4_biom_quants$Quantile == '97.5%')], group = as.factor(spec_4_biom_quants$Tree[which(spec_4_biom_quants$Quantile == '50%')]), fill = as.factor(spec_4_biom_quants$Tree[which(spec_4_biom_quants$Quantile == '50%')])), alpha = 0.5, show.legend = F) +
  xlab('Year') + ylab('Biomass') +
  ggtitle('TSCA')

##################################
## Biomass increments over time ##
##################################

## This section visualizes biomass increment (not total biomass) over time

## This could help explain any weird results in the previous section

# Storage
biom_inc = array(, dim = c(ntree, nyear, nkeep))

# Formatting
biom_melt2 = biom_melt %>%
  arrange(Tree) %>%
  arrange(Iteration)

# Find biomass increment by difference in biomass between years
for(i in 1:ntree){
  for(j in 1:nkeep){
    bioty = biom_melt2$Biomass[which(biom_melt2$Tree == i & biom_melt2$Iteration == j)]
    for(k in 1:nyear-1){
      biom_inc[i,k,j] = bioty[k + 1] - bioty[k]
    }
  }
  print(i)
}

# Melt to dataframe
inc_melt = melt(biom_inc)
colnames(inc_melt) = c('Tree', 'Year', 'Iteration', 'Biomass_Increment')

# Find quantiles
inc_quants = apply(biom_inc, c(1,2), quantile, probs = c(0.025, 0.5, 0.975), na.rm  = T)

# Melt again
inc_quants_melt = melt(inc_quants)
colnames(inc_quants_melt) = c('Quantile', 'Tree', 'Year', 'Increment')

# Species 1 plot
spec_1_inc_quants = inc_quants_melt %>%
  filter(Tree %in% c(1:3))

ggplot() +
  geom_line(aes(x = spec_1_inc_quants$Year[which(spec_1_inc_quants$Quantile == '50%')], y = spec_1_inc_quants$Increment[which(spec_1_inc_quants$Quantile == '50%')], group = as.factor(spec_1_inc_quants$Tree[which(spec_1_inc_quants$Quantile == '50%')]))) +
  geom_ribbon(aes(x = spec_1_inc_quants$Year[which(spec_1_inc_quants$Quantile == '50%')], ymin = spec_1_inc_quants$Increment[which(spec_1_inc_quants$Quantile == '2.5%')], ymax = spec_1_inc_quants$Increment[which(spec_1_inc_quants$Quantile == '97.5%')], group = as.factor(spec_1_inc_quants$Tree[which(spec_1_inc_quants$Quantile == '50%')]), fill = as.factor(spec_1_inc_quants$Tree[which(spec_1_inc_quants$Quantile == '97.5%')])), alpha = 0.5, show.legend = F) +
  xlab('Year') + ylab('Biomass Increment') +
  ggtitle('ACSA')

#ggsave(filename = 'sites/SYLVANIA/figures/ACSA_inc.png', plot = last_plot())

# Species 2 plot
spec_2_inc_quants = inc_quants_melt %>%
  filter(Tree %in% c(4:15))

ggplot() +
  geom_line(aes(x = spec_2_inc_quants$Year[which(spec_2_inc_quants$Quantile == '50%')], y = spec_2_inc_quants$Increment[which(spec_2_inc_quants$Quantile == '50%')], group = as.factor(spec_2_inc_quants$Tree[which(spec_2_inc_quants$Quantile == '50%')]))) +
  geom_ribbon(aes(x = spec_2_inc_quants$Year[which(spec_2_inc_quants$Quantile == '50%')], ymin = spec_2_inc_quants$Increment[which(spec_2_inc_quants$Quantile == '2.5%')], ymax = spec_2_inc_quants$Increment[which(spec_2_inc_quants$Quantile == '97.5%')], group = as.factor(spec_2_inc_quants$Tree[which(spec_2_inc_quants$Quantile == '50%')]), fill = as.factor(spec_2_inc_quants$Tree[which(spec_2_inc_quants$Quantile == '50%')])), alpha = 0.5, show.legend = F) +
  xlab('Year') + ylab('Biomass Increment') +
  ggtitle('BEAL')

#ggsave(filename = 'sites/SYLVANIA/figures/BEAL_inc.png', plot = last_plot())

# Species 3 plot
spec_3_inc_quants = inc_quants_melt %>%
  filter(Tree %in% c(16:26))

ggplot() +
  geom_line(aes(x = spec_3_inc_quants$Year[which(spec_3_inc_quants$Quantile == '50%')], y = spec_3_inc_quants$Increment[which(spec_3_inc_quants$Quantile == '50%')], group = as.factor(spec_3_inc_quants$Tree[which(spec_3_inc_quants$Quantile == '50%')]))) +
  geom_ribbon(aes(x = spec_3_inc_quants$Year[which(spec_3_inc_quants$Quantile == '50%')], ymin = spec_3_inc_quants$Increment[which(spec_3_inc_quants$Quantile == '2.5%')], ymax = spec_3_inc_quants$Increment[which(spec_3_inc_quants$Quantile == '97.5%')], group = as.factor(spec_3_inc_quants$Tree[which(spec_3_inc_quants$Quantile == '50%')]), fill = as.factor(spec_3_inc_quants$Tree[which(spec_3_inc_quants$Quantile == '50%')])), alpha = 0.5, show.legend = F) +
  xlab('Year') + ylab('Biomass Increment') +
  ggtitle('THOC')

#ggsave(filename = 'sites/SYLVANIA/figures/THOC_inc.png', plot = last_plot())

# Species 4 plot
spec_4_inc_quants = inc_quants_melt %>%
  filter(Tree %in% c(27:63))

ggplot() +
  geom_line(aes(x = spec_4_inc_quants$Year[which(spec_4_inc_quants$Quantile == '50%')], y = spec_4_inc_quants$Increment[which(spec_4_inc_quants$Quantile == '50%')], group = as.factor(spec_4_inc_quants$Tree[which(spec_4_inc_quants$Quantile == '50%')]))) +
  geom_ribbon(aes(x = spec_4_inc_quants$Year[which(spec_4_inc_quants$Quantile == '2.5%')], ymin = spec_4_inc_quants$Increment[which(spec_4_inc_quants$Quantile == '2.5%')], ymax = spec_4_inc_quants$Increment[which(spec_4_inc_quants$Quantile == '97.5%')], group = as.factor(spec_4_inc_quants$Tree[which(spec_4_inc_quants$Quantile == '50%')]), fill = as.factor(spec_4_inc_quants$Tree[which(spec_4_inc_quants$Quantile == '50%')])), alpha = 0.5, show.legend = F) +
  xlab('Year') + ylab('Biomass Increment') +
  ggtitle('TSCA')

#ggsave(filename = 'sites/SYLVANIA/figures/TSCA_inc.png', plot = last_plot())
