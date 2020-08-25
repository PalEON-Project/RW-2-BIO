
#####################################################
################ HURUN MOUNTAIN CLUB ################
#####################################################

# reformat tree meta csv
library(dplyr)
file1 = 'sites/HMC/data/raw/previous/HMC.field.data_UPDATED.csv'
treeMeta = read.csv(file1, stringsAsFactors = FALSE) %>% 
  mutate(ID = TreeID, distance = Distance, site = PlotID, dbh = DBH, species = Species) %>%
  dplyr::select(ID, distance, site, dbh, species)

any(is.na(treeMeta$site))
any(is.na(treeMeta$ID))
any(is.na(treeMeta$species))
any(is.na(treeMeta$dbh))
any(is.na(treeMeta$distance))

write.csv(treeMeta, 'sites/HMC/data/raw/HMC_treeMeta_082020.csv', row.names = FALSE)

# reformat census csv 
file2 = 'sites/HMC/data/raw/previous/HMC_census_v2.csv'
treeMetaOrig = read.csv(file1, stringsAsFactors = FALSE)
censusFull = read.csv(file2, stringsAsFactors = FALSE)

# we need to go through and find trees that are dead and remove those diameter values if there to keep all data consistent (check each year)
# first 1989
censusFull %>% filter(!is.na(X89comm) & X89comm != "") %>% dplyr::select(stemindex, X89comm) 
remove = c(220, 236, 251, 255, 281, 285, 306, 309, 366, 407, 433, 463, 487, 488, 492, 499, 539, 546)
censusFull %>% filter(stemindex %in% remove) # all good here, no values recorded for year 1989 

# then 1994
censusFull %>% filter(!is.na(X94comm) & X94comm != "") %>% dplyr::select(stemindex, X94comm) 

# then 1999
censusFull %>% filter(!is.na(X99comm) & X99comm != "") %>% dplyr::select(stemindex, X99comm) 
remove = c(315, 500, 554)
censusFull %>% filter(stemindex  %in% remove) # all good here, no 1999 dbh values here

# then 2004
censusFull %>% filter(!is.na(X04comm) & X04comm != "") %>% dplyr::select(stemindex, X04comm) 
remove = c(245, 274, 343, 361, 364, 369, 449, 516, 518, 519)
censusFull %>% filter(stemindex  %in% remove) # all good here, no 2004 dbh values here

# then 2009
censusFull %>% filter(!is.na(X09comm) & X09comm != "") %>% dplyr::select(stemindex, X09comm) 
remove = c(387,394,455)
censusFull %>% filter(stemindex  %in% remove) # all good here, no 2009 dbh values here

# then 2014
censusFull %>% filter(!is.na(X14comm) & X14comm != "") %>% dplyr::select(stemindex, X14comm) 

censusFull = censusFull %>%
  dplyr::select(-dd67, -dd89, -dd94, -dd99, -dd04, -dd09, -dd14, 
                -X89comm, -X94comm, -X99comm, -X04comm, -X09comm, 
                -X14comm)
maxNo = max(censusFull$No., na.rm = TRUE) + 1

# This cored tree was not matched up with a census tree and I think I figured out which one it should be
treeMetaOrig$No.[which(is.na(treeMetaOrig$No.))] = maxNo
censusFull$No.[which(censusFull$stemindex == 481)] = maxNo

# Need to give all census trees a num for identification later
censusFull$No.[which(is.na(censusFull$No.))] = seq(maxNo+1, maxNo+length(which(is.na(censusFull$No.))))

merged = left_join(censusFull, treeMetaOrig,  by = c('Plot', 'No.')) %>%
  filter(( !is.na(DBH) | Dist. <= 16))

# remove trees in census that don't fit in RW sampling scheme
# i.e. we remove trees that have distance close to 16 as they were likely outside plot
# i.e. we remove trees that were too small to be cored
remove = merged %>% 
  filter(!is.na(D14), is.na(DBH), (Dist. >= 15.5 | D14 < 10)) %>%
  dplyr::select(stemindex)

# remove dead trees with no census measurements nor RW data
remove2 = merged %>% 
  filter(is.na(D62), is.na(D67), is.na(D89), 
         is.na(D94), is.na(D99), is.na(D04), 
         is.na(D09), is.na(D14), is.na(DBH))

remove = c(remove$stemindex, remove2$stemindex)

merged = merged %>% 
  filter(!(stemindex %in% remove)) %>%
  dplyr::select(PlotID, Plot, TreeID, No., Sp, Dist., D62, D67, D89, D94, D99, D04, D09, D14)

plot_ids = data.frame(plot = paste0('HMC',seq(1,4)), census_plot = c(7094, 7092, 7095, 7093))
merged$site = plyr::mapvalues(merged$Plot, from = c(7094, 7092, 7095, 7093),
                              to = paste0('HMC',seq(1,4)))
final = merged %>% mutate(distance = Dist., ID = TreeID, num = No., species = toupper(Sp)) %>%
  dplyr::select(site, ID, num, species, distance, D62, D67, D89, D94, D99, D04, D09, D14) %>%
  arrange(site, ID) %>% 
  filter(distance <= 16)
for (i in seq_along(final$site)){
  if (is.na(final$ID[i])){
    if (final$num[i] < 10){
      final$ID[i] = paste0(final$site[i],'00',final$num[i],'C')
      next
    }
    if (final$num[i] < 100) {
      final$ID[i] = paste0(final$site[i],'0',final$num[i],'C')
      next
    }
    final$ID[i] = paste0(final$site[i],final$num[i],'C')
  }
}

# we need to change the species for yellow birch to match up with RW data
final = final %>% dplyr::select(-num) %>% mutate(species = ifelse(species == 'BELU', 'BEAL', species))

any(is.na(final$site))
any(is.na(final$ID))
any(is.na(final$species))
any(is.na(final$distance))

write.csv(final, 'sites/HMC/data/raw/HMC_census_082020.csv', row.names = FALSE)

