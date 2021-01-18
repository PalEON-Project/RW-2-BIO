
###########################################
################ GOOSE EGG ################
###########################################
library(tidyverse)
library(dplR)

# reformatting tree meta data 
treeMeta = read.csv('sites/GOOSE/data/raw/past/GooseEggAllPlots.csv', skip = 3, stringsAsFactors = FALSE) %>%
  mutate(distance = Distance, ID = Tree.Number, site = Site, dbh = DBH, species = Species) %>% 
  dplyr::select(site, ID, species, dbh, distance, Status)

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

# Some trees are missing species, but these ones were not alive at the time of coring
missingSpec = which(is.na(treeMeta$species))
treeMeta[missingSpec,]

# Two trees are missing DBH measurement => however, only one of them is alive at the time of coring and is included 
# in the ring width measurements. Therefore, we need to use the ring widths to estimate diameter at the time of coring. 
treeMeta[which(is.na(treeMeta$dbh)),]
RW = t((read.tucson('sites/GOOSE/data/raw/rwl/GE_BELE.rwl'))) 
rwS = RW['GE2061s',]
rwS = rwS[which(!is.na(rwS))]
rwN = RW['GE2061n',]
rwN = rwN[which(!is.na(rwN))]
yrs = sort(unique(c(names(rwS), names(rwN))))
est = 0 

# get estimate in mm first
for (y in yrs){
  if (y %in% names(rwS) & y %in% names(rwN)){
    est = est + rwS[which(names(rwS) == y)] + rwN[which(names(rwN) == y)]
  }
  else{
    if (y %in% names(rwS)){
      est = est + (2 * rwS[which(names(rwS) == y)])
    }else{
      est = est + (2 * rwN[which(names(rwN) == y)])
  }}
}
treeMeta$dbh[which(treeMeta$ID == 'GE2061')] = round(est/10, 2)



any(is.na(treeMeta$site))
any(is.na(treeMeta$ID))
any(is.na(treeMeta$species))
any(is.na(treeMeta$dbh))
any(is.na(treeMeta$distance))

write.csv(treeMeta, file = 'sites/GOOSE/data/raw/GOOSE_treeMeta_012021.csv', row.names = FALSE)
