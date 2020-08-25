
##########################################
################ SYLVANIA ################
##########################################

# reformatting tree meta data 
treeMeta = read.csv('sites/SYLVANIA/data/raw/past/alexander_sylvania_June2018.csv',stringsAsFactors = FALSE) %>%
  dplyr::select(ID, species, dbh, distance) %>%
  mutate(site = 'SEH1')

any(is.na(treeMeta$site))
any(is.na(treeMeta$ID))
any(is.na(treeMeta$species))
any(is.na(treeMeta$dbh))
any(is.na(treeMeta$distance))

write.csv(treeMeta, file = 'sites/SYLVANIA/data/raw/SYLVANIA_treeMeta_082020.csv', row.names = FALSE)



