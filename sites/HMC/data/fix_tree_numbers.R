source('sites/HMC/inst/config.R')

plots = c(7094, 7092, 7095, 7093)

file1 = 'sites/HMC/data/raw/past/HMC.field.data_UPDATED.csv'
rw_meta = read.csv(file1, stringsAsFactors = FALSE) 
# treeMeta = treeMeta[,which(!(colnames(treeMeta) %in% c('TreeID', 'Tree')))]
# treeMeta$site = plyr::mapvalues(treeMeta$Plot, from = c(7094, 7092, 7095, 7093),
#                                 to = paste0('HMC',seq(1,4)))
rw_meta = rw_meta[,c('TreeID', 'Species', 'Status', 'DBH','Distance', 'Azimuth', 'Plot', 'No.')]
colnames(rw_meta) = c('ID', 'species', 'status', 'dbh','distance', 'azimuth', 'plot', 'number')

census = read.csv('sites/HMC/data/raw/archive/Hurons_old_plots_archival.csv', stringsAsFactors = FALSE)
census = census[which(census$Plot %in% plots), ]
census = census[, c('Stem', 'No', 'Plot', 'Sp', 'Dist', 'Azi', 'year', 'DBH', 'dead')]
colnames(census) = c('stem_index', 'number', 'plot', 'species', 'distance', 'azimuth',  'year', 'dbh', 'dead')

census$species[which(census$species == 'BELU')] = 'BEAL'

index_na = unique(census[which(is.na(census$number)),'stem_index'])
number_max = max(census$number, na.rm=TRUE)

for (i in 1:length(index_na)){
  stem_rows = which(census$stem_index == index_na[i])
  census[stem_rows, 'number'] = rep(number_max + i)
}

rw_missing = census[which(census$stem_index == 481),]
rw_meta$number[which(is.na(rw_meta$number) & (rw_meta$plot == rw_missing$plot[1]))] = 940

any(is.na(census$number))
any(is.na(rw_meta$number))

tree_distinct = distinct(census, plot, number, species, stem_index)
tree_distinct = arrange(tree_distinct, plot, number, stem_index)

N_trees = nrow(tree_distinct)

tree_distinct$tree_number = seq(1, N_trees)  

census$tree_number = NA
rw_meta$tree_number = NA

for (tree in 1:N_trees){
  tree_info = tree_distinct[which(tree_distinct$tree_number == tree), ]
  
  census[which((census$number == tree_info$number) & 
                 (census$plot == tree_info$plot) &
                 (census$stem_index == tree_info$stem_index)), 'tree_number'] = tree_info$tree_number
  
  if (any((rw_meta$number == tree_info$number) & 
          (rw_meta$plot == tree_info$plot))){
    
    rw_meta[which((rw_meta$number == tree_info$number) & 
                    (rw_meta$plot == tree_info$plot)), 'tree_number'] = tree_info$tree_number
  }
}


all(rw_meta$tree_number %in% census$tree_number)


census$species = toupper(census$species)

census_loc = file.path('sites',site,'data','raw',paste0(site,'_census_',dvers,'.csv'))
write.csv(census, census_loc )

meta_loc = file.path('sites',site,'data','raw',paste0(site,'_treeMeta_',dvers,'.csv'))
write.csv(rw_meta, meta_loc )

