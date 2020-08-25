data = readRDS('sites/SYLVANIA/data/built/tree_data_SYLVANIA_STAN_1.0_072020_mine.RDS')
data2 = readRDS('sites/SYLVANIA/data/built/tree_data_SYLVANIA_STAN_1.0_072020.RDS')

data = readRDS('sites/ROOSTER/data/built/tree_data_ROOSTER_STAN_1.0_072020.RDS')

# check 
data$N_trees==data2$N_trees 
data$N_years==data2$N_years
data$N_vals==data2$N_vals
data$N_inc==data2$N_inc
sum(data$logXobs==data2$logXobs)/length(data$logXobs)
sum(data$year_idx==data2$year_idx)/length(data$year_idx)
data$N_taxa==data2$N_taxa
data$idx_tree==data2$idx_tree
sum(data$x2tree==data2$x2tree)/length(data$x2tree)
sum(data$x2year==data2$x2year)/length(data$x2year)
sum(data$meas2x==data2$meas2x)/length(data$meas2x)
sum(data$m2taxon==data2$m2taxon)/length(data$meas2x)
sum(data$taxon==data2$taxon)/length(data$taxon)
data$taxaMatch==data2$taxaMatch
sum(data$plot_id==data2$plot_id)/length(data$plot_id)
sum(data$years==data2$years)/length(data$years)
sum(data$m2tree==data2$m2tree)/length(data$m2tree)
sum(data$m2t==data2$m2t)/length(data$m2t)


sum(data$distance==data2$distance)/length(data$distance)
sum(data$logPDobs==data2$logPDobs)/length(data$logPDobs)
sum(data$pdbh_year==data2$pdbh_year)/length(data$pdbh_year)
sum(data$pdbh2val==data2$pdbh2val)/length(data$pdbh2val)

