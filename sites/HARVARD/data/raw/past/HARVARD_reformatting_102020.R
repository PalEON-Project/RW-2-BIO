################################################
################ HARVARD FOREST ################
################################################
library(dplR)
library(plyr)
library(dplyr)
library(reshape2)

# reformatting ring width metadata csv
treeMeta = read.csv('sites/HARVARD/data/raw/past/LyfordAllPlots.csv', skip = 3, 
                    stringsAsFactors = FALSE)

# combine site and tree number for appropriate tree ID 
for (i in seq_along(treeMeta$Site)){
  st = treeMeta$Site[i]
  idnum = treeMeta$Tree.Number[i]
  if (idnum < 10) treeMeta$ID[i] = paste0(st, '00',idnum)
  if (idnum >= 10 & idnum < 100) treeMeta$ID[i] = paste0(st,'0',idnum)
  if (idnum >= 100) treeMeta$ID[i] = paste0(st,idnum)
}

treeMeta = treeMeta %>% 
  mutate(site = Site, distance = Distance, dbh = DBH, species = Species) %>% 
  dplyr::select(ID, site, species, dbh, distance)

any(is.na(treeMeta$site))
any(is.na(treeMeta$ID))
any(is.na(treeMeta$species))
any(is.na(treeMeta$dbh))
any(is.na(treeMeta$distance))

write.csv(treeMeta, 'sites/HARVARD/data/raw/HARVARD_treeMeta_102020.csv', row.names = FALSE)

# reformatting census data 
censusFull = read.csv('sites/HARVARD/data/raw/past/hf032-01-tree.csv', 
                      stringsAsFactors = FALSE) %>% mutate(Tag = treeid)
treeids = vector()
temp.matrix = matrix(NA, nrow(censusFull), 8) 
years = c(1962, 1969, 1975, 1987, 1991, 1992, 2001, 2011)
final_cnames = c('id')
colnames(temp.matrix) = years

cnames = colnames(censusFull)

# loop through all census trees
for (i in 1:nrow(censusFull)){
  treeids[i] = censusFull$treeid[i]
  
  for (y in c(62, 69, 75, 91, 01, 11)){
    
    thisyr = ifelse(y > 60, y + 1900, y + 2000)
    
    # if not part of three-year census combo pack
    if (thisyr != 1991){
      
      ind = which(years == thisyr)
      thisdcol = which(cnames == paste0('dbh',toString(y)))
      thisccol = which(cnames == paste0('cond',toString(y)))
      if (y == 1) thisdcol = which(cnames == paste0('dbh0',toString(y)))
      if (y == 1) thisccol = which(cnames == paste0('cond0',toString(y)))
      if (is.na(censusFull[i, thisdcol])) next
      
      # extract DBH value if the tree was still alive at the time of measurement 
      if (censusFull[i,thisccol] == 'L') temp.matrix[i, ind] = censusFull[i, thisdcol]
      
      # if it's the weird year, we need to use meas3yr
    }else{
      
      # get actual year
      thisyr = as.numeric(censusFull$meas3yr[i])
      ind = which(years == thisyr)
      thisdcol = which(cnames == 'dbh91')
      thisccol = which(cnames == 'cond91')
      if (is.na(censusFull[i, thisdcol])) next
      
      # extract DBH value if the tree was still alive at the time of measurement 
      if (censusFull[i,thisccol] == 'L') temp.matrix[i, ind] = censusFull[i, thisdcol]
    }
  }
}

rownames(temp.matrix) = treeids
census_melt = reshape2::melt(temp.matrix) %>% filter(!is.na(value))
colnames(census_melt) = c('id','year','dbh')

censusFix = dcast(census_melt, id ~ year, value.var = 'dbh')
colnames(censusFix) = c('id','D62','D69','D75','D87','D91','D92','D01','D11')

# add species
censusAdd = left_join(censusFix, 
                      censusFull %>% mutate(id = Tag) %>% dplyr::select(block, species, id, xsite, ysite),
                      by = 'id')
treeMetaOrig = read.csv('sites/HARVARD/data/raw/past/LyfordAllPlots.csv', skip = 3, stringsAsFactors = FALSE) %>% 
  mutate(id = Tag) %>% dplyr::select(Site, Species, DBH, id, Tree.Number, Status)
censusSpp = left_join(censusAdd, treeMetaOrig, by = 'id')

# need to check to make sure we lined up trees correctly - where do we have the species wrong?
speciescheck = censusSpp %>% filter(!is.na(Species))
wrongs = which(speciescheck$species != speciescheck$Species)
speciescheck[wrongs,] # we see 7 trees where the species differ

# here genus is correct for both and DBHs follow logical increasing trend over time, but species differs
# here we assume the most recent species assessment is correct
for (id in c(1568, 2160, 2168, 4063, 4107)){
  censusSpp[which(censusSpp$id == id),'species'] = censusSpp[which(censusSpp$id == id),'Species']
}

# ID 3926 in the census is in the wrong block (outside of plot range) so removing census entry
censusSpp = censusSpp %>% filter(id != 3926)

# ID 1006 has two completely different tree species (Beech vs. Red Maple); removing for now because RW will account
censusSpp = censusSpp %>% filter(id != 1006)

# determine which blocks from census are relevant to the RW data plots
sitecheck = censusSpp %>% filter(!is.na(Site))
blocks = unique(sitecheck$block)
sites = sapply(blocks, 
               function(b){
                 sitecheck$Site[which(sitecheck$block == b)[1]]
               })

# need to determine distance from plot center in feet based on xsite and ysite for each plot
censusSite = censusSpp %>% filter(block %in% blocks)

# here we determine the distance from the plot center for each tree in the relevant blocks
# distance is converted to feet and then we remove any tree that is farther than 20 meters 
# then we add tree.numbers to the individuals that do not have id numbers now

# first for plot 1 (-300, -400)
plot1 = censusSite %>% filter(block %in% c('SW32','SW33','SW42','SW43')) %>%
  mutate(distance = sqrt((ysite+400)^2 + (xsite+300)^2), 
         site = 'LF1') %>% 
  mutate(distance = distance * 0.3048) %>%
  filter(distance <= 20) %>%
  arrange(Tree.Number)
nas = which(is.na(plot1$Tree.Number))
plot1$Tree.Number[nas] = seq((max(plot1$Tree.Number,na.rm = TRUE)+1), (max(plot1$Tree.Number,na.rm = TRUE)+length(nas)))

# then, plot 2 (-100, -200)
plot2 = censusSite %>% filter(block %in% c('SW10','SW11','SW20','SW21')) %>%
  mutate(distance = sqrt((ysite+200)^2 + (xsite+100)^2),
         site = 'LF2') %>%
  mutate(distance = distance * 0.3048) %>%
  filter(distance <= 20) %>%
  arrange(Tree.Number)
nas = which(is.na(plot2$Tree.Number))
plot2$Tree.Number[nas] = seq((max(plot2$Tree.Number,na.rm = TRUE)+1), (max(plot2$Tree.Number,na.rm = TRUE)+length(nas)))

# then, plot 3 (0, -600)
plot3 = censusSite %>% filter(block %in% c('SE51','SE61','SW50','SW60')) %>%
  mutate(distance = sqrt((ysite+600)^2 + (xsite)^2), 
         site = 'LF3') %>%
  mutate(distance = distance * 0.3048) %>%
  filter(distance <= 20) %>%
  arrange(Tree.Number)
nas = which(is.na(plot3$Tree.Number))
plot3$Tree.Number[nas] = seq((max(plot3$Tree.Number,na.rm = TRUE)+1), (max(plot3$Tree.Number,na.rm = TRUE)+length(nas)))

# this data frame will therefore only contain trees relevant to the RW plots
censusFinal = rbind(plot1, plot2, plot3)

# now we need to reformat IDs for trees
for (i in seq_along(censusFinal$site)){
  st = censusFinal$site[i]
  idnum = censusFinal$Tree.Number[i]
  if (idnum < 10) censusFinal$id[i] = paste0(st, '00',idnum)
  if (idnum >= 10 & idnum < 100) censusFinal$id[i] = paste0(st,'0',idnum)
  if (idnum >= 100) censusFinal$id[i] = paste0(st,idnum)
}

# remove tree number column
censusFinal = censusFinal %>% mutate(ID = id) %>% dplyr::select(-Tree.Number,-id) 

# there are some gaps between the census data and the ring width data which we need to tell the model about 
# some trees are missing from the ring width data which should have been included according to the sampling design
# let's find them so we can tell the model if these trees should be considered dead by the coring year or if they 
# were likely alive 

# first let's read in the RWL data so we know where we have data (ATTENTION: these are the reformatted RWL files)
# we need the IDS and the last year with data 
rwFiles <- list.files('sites/HARVARD/data/raw/rwl')
rwFiles <- rwFiles[grep(".rwl$", rwFiles)]
rwData <- list()
for(fn in rwFiles) {
  id <- gsub(".rw", "", fn)
  # Insert the contents of each file into the rwData list
  rwData[[id]] <- t((read.tucson(file.path('sites/HARVARD/data/raw/rwl', fn))))  # rows are tree, cols are times
}
incr = ldply(rwData, rbind)
incr = incr[,c(".id", sort(colnames(incr)[2:ncol(incr)]))]
rownames(incr) = as.vector(unlist(lapply(rwData, rownames)))
incr[,1] = rownames(incr)
incr_data = melt(incr)
colnames(incr_data) = c('id', 'year', 'incr')
incr_data$year = as.vector(incr_data$year)
incr_data$id = substr(incr_data$id, 1, 6)
incr_data = incr_data %>% filter(!is.na(incr))
RWinfo = incr_data %>% group_by(id) %>% 
  summarize(lastyr = max(year))
rm(incr_data, incr, rwData)

# now, let's just take a look at the census trees that are present in the final census 
Cinfo = censusFinal %>% mutate(D11 = as.numeric(D11)) %>% filter(!is.na(D11))

# we need to find all the trees where either 
# (1) the census data says the tree lived after the RW data  
# (2) the tree is not in the RW data but should have been according to sampling design
KeepIDs = c() 
for (i in 1:nrow(Cinfo)){
  IDnow = Cinfo$ID[i]
  
  # in RW data?
  if (IDnow %in% RWinfo$id){
    
    # is the final year of RW data on or after 2011?
    finalyr = RWinfo$lastyr[which(RWinfo$id == IDnow)]
    if (finalyr < 2011) KeepIDs = c(KeepIDs, IDnow)
    
  }else{
    # too small?
    if (Cinfo$D11[i] < 10) next
    # too small for outer ring?
    if ((Cinfo$D11[i] < 20) & (Cinfo$distance[i] > 13)) next

    KeepIDs = c(KeepIDs, IDnow)
  }
}

# look closer at these marked IDS 
Cinfo %>% filter(ID %in% KeepIDs)

# these trees were likely measured outside of the plot in the coring year so should be considered alive 
# 1070 (too small for outer ring, distance ~ 13m)
# 2139 (measured outside of the plot area, distance ~ 20m) 
# 2170 (measured outside of the plot area, distance ~ 20m)
# 3106 (measured outside of the plot area, distance ~ 20m)

# this tree was marked in the RW metadata as missing RW data, but still alive at time of coring 
# 3036

# these trees were noted as dead in the ring width data, so died between 2011 and coring year
# 1014, 2027, 2045, 3016

# these trees have no clear reason for why they wouldn't be in the RW data, so we assume they died 
# 1130, 2126

# therefore, let's create another data column that tells the model what to do with these trees in terms of their 
# condition in the coring year 
# NOTE: by making all other trees
censusFinal$finalCond = rep('alive',nrow(censusFinal))
censusFinal$finalCond[which(censusFinal$ID %in% c('LF1014','LF2027','LF2045','LF3016','LF1130','LF2126'))] = 'dead'

censusFinal = censusFinal %>% dplyr::select(-block, -Species, -ysite, -xsite, -Status, -DBH, -Site)

any(is.na(censusFinal$site))
any(is.na(censusFinal$ID))
any(is.na(censusFinal$species))
any(is.na(censusFinal$distance))

write.csv(censusFinal, 'sites/HARVARD/data/raw/HARVARD_census_102020.csv', row.names = FALSE)
