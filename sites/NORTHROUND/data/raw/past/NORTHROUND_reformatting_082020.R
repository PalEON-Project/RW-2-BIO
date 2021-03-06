
##################################################
################ NORTH ROUND POND ################
##################################################

# reformatting tree meta data 

treeMeta = read.csv('sites/NORTHROUND/data/raw/past/NorthRoundPondAllPlots.csv', skip = 3, stringsAsFactors = FALSE) %>%
  mutate(distance = Distance..base., ID = Tree.Number, site = Site, dbh = DBH, species = Species) %>% 
  dplyr::select(site, ID, species, dbh, distance)

# fixing IDs to have site prefix in it as well 
for (i in seq_along(treeMeta$ID)){
  
  # need to add two leading zeros 
  if (as.numeric(treeMeta$ID[i]) < 10){
    treeMeta$ID[i] = paste0(treeMeta$site[i],'00',treeMeta$ID[i])
    next
  }
  
  # need to add one leading zero 
  if (as.numeric(treeMeta$ID[i]) < 100){
    treeMeta$ID[i] = paste0(treeMeta$site[i],'0',treeMeta$ID[i])
    next
  }
  
  # don't need to add any leading zeros
  if (as.numeric(treeMeta$ID[i]) >= 100){
    treeMeta$ID[i] = paste0(treeMeta$site[i],treeMeta$ID[i])
    next
  }
}

any(is.na(treeMeta$site))
any(is.na(treeMeta$ID))
any(is.na(treeMeta$species))
any(is.na(treeMeta$dbh))
any(is.na(treeMeta$distance))

write.csv(treeMeta, file = 'sites/NORTHROUND/data/raw/NORTHROUND_treeMeta_082020.csv', row.names = FALSE)
