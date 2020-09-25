
library(dplR)
library(plyr)
library(dplyr)

# Adjusting RWL file to allow for three digit IDs
rwl_dir = 'sites/HARVARD/data/raw/past/rwl_original/'
rwFiles <- list.files(rwl_dir)
rwFiles <- rwFiles[grep(".rwl$", rwFiles)]

for (file in rwFiles){
  
  # read in Tucson
  X = read.tucson(paste0(rwl_dir,file))
  
  # adjust ID names 
  ids = names(X)
  ids_new = sapply(ids, 
                   function(x){
                     paste0(substr(x, 1, 3), '0',substr(x, 4, 6))
                   })
  names(X) = ids_new
  
  # rewrite file to a new RWL file
  write.tucson(X, 
               fname = paste0('sites/HARVARD/data/raw/rwl/',file),
               long.names = TRUE)
}

# Now, let's just look for Zombie trees because the 20 meter plots were not adjusted like the 13 m version
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

treeMetaOrig = read.csv('sites/HARVARD/data/raw/past/LyfordAllPlots.csv', skip = 3, stringsAsFactors = FALSE) %>% 
  mutate(id = Tag) %>% dplyr::select(Site, Species, DBH, id, Tree.Number, Status) %>% filter(Status == 'Li')
treeMetaOrig$id = rep(NA, nrow(treeMetaOrig))
for (i in 1:nrow(treeMetaOrig)){
  num = treeMetaOrig$Tree.Number[i]
  if (num < 10) treeMetaOrig$id[i] = paste0(treeMetaOrig$Site[i],'00',num)
  if (num < 100 & num > 9) treeMetaOrig$id[i] = paste0(treeMetaOrig$Site[i],'0',num)
  if (num > 99) treeMetaOrig$id[i] = paste0(treeMetaOrig$Site[i],num)
}

RWinfo.red = RWinfo %>% filter(lastyr < 2012)

# any row in the following data frame that has treeMeta information is a zombie because it was recorded as alive at coring, 
# but seems dead according to the RW data 
left_join(RWinfo.red, treeMetaOrig, by = c('id'))
# we need to fill in zeros for growth LF2040, which is a red maple 

ACRU <- read.tucson('sites/HARVARD/data/raw/rwl/LF_ACRU.rwl')
first.ind = which(rownames(ACRU) == '2001')
second.ind = which(rownames(ACRU) == '2012')
ACRU[first.ind:second.ind,which(substr(colnames(ACRU),1,6) == 'LF2040')[1]] = 0
ACRU[,which(substr(colnames(ACRU),1,6) == 'LF2040')]

# save again 
write.tucson(ACRU, 
             fname = 'sites/HARVARD/data/raw/rwl/LF_ACRU.rwl',
             long.names = TRUE)
