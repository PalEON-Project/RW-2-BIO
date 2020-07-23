## This script takes the ring width files (rwl format) and the stand-
## level inputs from the year the trees were cored and formats the data
## to run in the STAN ring width model. In getting to the correct format,
## this script also matches cores with tree species, DBH, and location
## within the plot

## Prepare workspace
rm(list = ls())
library(plotrix)
library(dplR)
library(fields)
library(reshape2)
library(plyr)
setwd('/Users/Aly/Google Drive 2/McLachlan/SU20/npp-stat-model/')

## Before running this script, you will need to obtain data for your site
## These data should be available on the Wiki in a zip file located at
## Data and Products --> Tree Ring Data. To most easily work through
## this script, transfer all rwl files into one folder (no subfolders)
## and title the folder "rwl". The tree CSV should be outside of this
## folder.

## You may also want to open the tree CSV and make sure you understand
## what the columns all are, because this tends to be the most variable
## input between sites and data collectors

# Specify data directory, site, and versions for saving products
dataDir = 'data/'
site = 'SYLVANIA'
dvers = "072020"
mvers = "072020"

## The following couple of commands grab and organize the data from the datafiles

# Load CSV with tree data (DBH, location within plot, species)
# The Sylvania CSV is an example of one in the "correct" PalEON format
treeMeta = read.csv("data/SYLVANIA/alexander_sylvania_June2018.csv")

# List of files with tree ring increments
rwFiles <- list.files(paste0("data/", site, '/', "rwl"))
# In case there are other files in this directory, take only rwl files
rwFiles <- rwFiles[grep(".rwl$", rwFiles)]
rwData <- list()
for(fn in rwFiles) {
  id <- gsub(".rw", "", fn)
  # Insert the contents of each file into the rwData list
  rwData[[id]] <- t(read.tucson(file.path("data", site, "rwl", fn)))  # rows are tree, cols are times
}

# Dimensions: core x year
incr = ldply(rwData, rbind)
# Insert core ID's into the matrix
incr = incr[,c(".id", sort(colnames(incr)[2:ncol(incr)]))]
rownames(incr) = as.vector(unlist(lapply(rwData, rownames)))
incr[,1] = rownames(incr)

###################################
#### STOP ####
###################################

# This makes a cut-off point for the data
# At most sites, we can use the entire time series available
# At Sylvania, this is not possible because only a few trees are very old,
# which runs into problems with the rest of the trees (those not "born") having no DBH for many years
incr = incr[, which((as.numeric(colnames(incr))>1900)|(colnames(incr)==".id"))]

######################################################################################################################################
## make STAN data
######################################################################################################################################
## This section prepares the data organized above for use in the STAN model

## First, make a data frame from incr, with the addition of some organizing variables

# Melt tree ring data frame  
incr_data = melt(incr)
colnames(incr_data) = c('id', 'year', 'incr')
# Assign plot to each core in data frame (if only one plot, all 1)
incr_data$plot   = rep(1, nrow(incr_data))#as.numeric(substr(incr_data$id, 3, 3))
# Assign tree ID to each core (= core ID, there are 2 cores per tree)
# I'm unsure why "tree ID" does not actually ID the tree
incr_data$TreeID = incr_data$id 
# Assign ID to each core (= tree, not core, more than 1 core per tree)
# Again, I don't know why it's like this (consider revisiting?)
incr_data$id = as.numeric(substr(incr_data$id, 4, 6))
# Assign year to each observation (= dating from tree rings)
incr_data$year   = as.vector(incr_data$year)

# Vector of individual tree IDs (removes directionality from core ID)
tree_ids = unique(substr(incr_data$TreeID, 1, 6))
# Number of trees with cores
N_trees  = length(tree_ids)
# Vector sequence to ID each core numerically
stat_ids = seq(1, N_trees)

# Add stat_ids to data frame by matching tree ID's (each tree gets a separate stat ID)
incr_data$stat_id = stat_ids[match(substr(incr_data$TreeID, 1, 6), tree_ids)]
for (n in 1:nrow(incr_data)){
  print(n)
  # Add species to tree ring data by matching species in whole-tree dataset (treeMeta) with the ID
  incr_data$taxon[n] = as.vector(treeMeta$species[which(as.numeric(substr(treeMeta$ID, 4, 6)) == incr_data$id[n])])
}

# Number of species
N_taxa = length(unique(incr_data$taxon))
# Data frame of 4-letter species codes and corresponding numbers
taxaMatch = data.frame(species=sort(unique(incr_data$taxon)), number=seq(1, N_taxa))

# Vector 4-letter species code corresponding to each tree with tree ring observations
taxon = aggregate(taxon~stat_id, incr_data, unique)[,2]
# Convert to numbers corresponding to each species
taxon = taxaMatch$number[match(taxon, taxaMatch$species)]

# Vector of plot number corresponding to each tree (in this case, all 1's)
plot_id = aggregate(plot~stat_id, incr_data, unique)[,2]

# Last year of growth observed in tree rings
year_end = max(as.numeric(incr_data$year), na.rm=TRUE)
# First year of growth observed in tree rings 
# (this should correspond with what was chosen above if the data were subset)
year_start = min(as.numeric(incr_data$year), na.rm=TRUE)
# Number of years in the series
N_years = year_end - year_start + 1
# Get sequence of years
years = seq(year_start, year_end)

# order by tree and year
incr_data = incr_data[order(incr_data$stat_id, incr_data$year),]
# Remove all NA values (only keep the series for which trees were alive/existed)
incr_data = incr_data[which(!is.na(incr_data$incr)),]
# Make units of time in "years since first recorded tree ring"
incr_data$year = as.numeric(incr_data$year) - year_start + 1
# Number of measurements
N_inc   = nrow(incr_data)
# Vector of years for each increment
m2t     = incr_data$year
# Vector of tree ID's for each increment
m2tree  = incr_data$stat_id
# Vector of plot for each increment
m2plot  = incr_data$plot
# Vector of species numbers for each increment
m2taxon = taxaMatch$number[match(incr_data$taxon, taxaMatch$species)]
# Vector of increments (actual data)
Xobs    = incr_data$incr
Xobs[Xobs==0] = 0.0001
logXobs = log(Xobs)

# Data frame of the first and last year of observation (years corresponding to first and last increment) for each tree
year_idx = data.frame(year_start=as.numeric(aggregate(year~stat_id, data=incr_data, FUN=min, na.rm=TRUE)[,2]), 
                      year_end=as.numeric(aggregate(year~stat_id, incr_data, max)[,2]))

## Now, you should have the incr_data data frame with columns to index the
## tree, year, plot, core, tree (again, but in order now), and species
## in order, not including the incr column, which is ring width increment
## for the given year

## You should also have some vectors of that help to index the ring
## widths in the stat model and match the ring widths to the tree and 
## characteristics of the tree in the stand

## Next, we will focus on the DBH measurements, rather than the cores

## pdbh houses final DBH measurements for each tree, along with associated variables
## I think this would be substantially different if you have census
## data but I'm not sure how that works yet

# Make data frame of tree ID number from whole-tree dataset, sequential tree ID, and last year of observation
pdbh = aggregate(year~stat_id+plot+id, incr_data, max, na.rm=TRUE)
pdbh = pdbh[order(pdbh$stat_id),]

for (n in 1:nrow(pdbh)){
  # Add DBH from whole-tree dataset using ID to match observations to those in increment dataset
  pdbh$dbh[n] = treeMeta$dbh[which(as.numeric(substr(treeMeta$ID, 4, 6)) == pdbh$id[n])]
  # Same with distance (location within plot)
  pdbh$distance[n] = treeMeta$distance[which(as.numeric(substr(treeMeta$ID, 4, 6)) == pdbh$id[n])]
}
# Number of trees with DBH observations
N_pdbh = nrow(pdbh)
logPDobs = log(pdbh$dbh)
# Vector of Tree ID's
pdbh_tree_id = pdbh$stat_id
# Vector of year of observation for each tree
pdbh_year_id = pdbh$year
# Vector of locations within plot
distance = pdbh$distance

## This part basically gives an index of how many theoretical DBH
## measurements we would have for a given tree if we measure DBH annually
## for the years the tree has ring width observations
idx_stack = data.frame(meas=numeric(0), tree_id=numeric(0), year=numeric(0))
n = 1
for (tree in 1:N_trees){
  # Vector of years of increment observations for a given tree
  year = seq(year_idx[tree,1], year_idx[tree,2])
  # Vector of observation index for each increment of a given tree
  meas = seq(n, n+length(year)-1)
  n = n + length(year)
  idx_stack = rbind(idx_stack, data.frame(meas=meas, tree_id=rep(tree, length(year)), year=year))
}

# Find location of first observation for each tree
idx_tree = which(!duplicated(idx_stack$tree_id))
# Data frame of first and last "observation" for each tree
idx_tree = data.frame(idx_tree, c(idx_tree[-1]-1, nrow(idx_stack)))

# Vector of tree IDs
x2tree  = idx_stack$tree_id
# Vector of years
x2year  = idx_stack$year 

# Number of observations (= DBH measurements) in new data frame
N_vals   = nrow(idx_stack)

meas2x = vector(length=N_inc)
for (i in 1:N_inc) {
  print(i)
  # ID number for given tree from increment data frame
  id = incr_data$stat_id[i]
  # Year for given tree from increment data frame
  year = incr_data$year[i]
  
  # Find observations with given ID and year in new data frame
  meas2x[i] = which((idx_stack$tree_id == id) & (idx_stack$year == year))
}

## This ends up indexing the DBH observation, so that the model knows
## when DBH was measured and how many DBH increments there are in the 
## ring width files for each tree

pdbh2val = vector(length=N_pdbh)
for (i in 1:N_pdbh){
  # ID number for given tree from DBH data frame
  id = pdbh$stat_id[i]
  # Year for given tree from DBH data frame (always the alst year in this case)
  year = pdbh$year[i]
  
  print(i)
  which((idx_stack$tree_id == id) & (idx_stack$year == year))
  
  # Find observations with given ID and year in new data frame
  pdbh2val[i] = which((idx_stack$tree_id == id) & (idx_stack$year == year))
}

site_dir <- file.path('sites',site)
if (!file.exists(site_dir)){
  dir.create(site_dir)
  dir.create(file.path(site_dir,'data'))
  dir.create(file.path(site_dir,'output'))
  dir.create(file.path(site_dir, 'figures'))
}

saveRDS(list(N_trees=N_trees, 
             N_years=N_years,
             N_vals=N_vals,
             N_inc = N_inc,
             logXobs=logXobs, 
             logPDobs=logPDobs,
             year_idx=year_idx, 
             N_taxa=N_taxa,
             pdbh_year=pdbh_year_id,
             idx_tree =idx_tree, 
             pdbh2val=pdbh2val,
             x2tree=x2tree,
             x2year=x2year,
             meas2x = meas2x, 
             m2taxon = m2taxon,
             taxon = taxon,
             taxaMatch=taxaMatch,
             plot_id = plot_id,
             years = years,
             m2tree = m2tree,
             m2t = m2t,
             distance=distance),
        file=paste0('sites/', site, '/data/tree_data_', site ,'_STAN_', dvers, '.RDS'))
