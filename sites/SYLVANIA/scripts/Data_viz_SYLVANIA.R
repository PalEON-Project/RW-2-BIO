## This script provides visualization for the data products processed
## in the build_data script

## These should be compared with assumptions made in the STAN model
## to ensure that the assumptions made are appropriate given the data

## Prepare workspace

rm(list = ls())
library(ggplot2)
library(reshape2)
library(dplyr)
setwd('~/Google Drive 2/McLachlan/SU20/npp-stat-model/')

## Load data
# Output from build_data
data = readRDS('sites/SYLVANIA/data/tree_data_SYLVANIA_STAN_072020.RDS')

##################
## Process data ##
##################

## This section processes the outputs of build_data into a usable format

# Increment data
xobs = data$logXobs
xobs = exp(xobs)

# Storage
Xobs = matrix(, nrow = data$N_trees, ncol = data$N_years)
spec = matrix(, nrow = data$N_trees, ncol = data$N_years)

# Organize increment data by tree and by year
for(i in 1:data$N_trees){
  for(j in 1:data$N_years){
    species = data$taxon[i]
    spec[i,j] = species
    x = xobs[which(data$m2tree == i & data$m2t == data$years[j] - 1900)]
    if(length(x) >= 1){
      x = mean(x)
    }
    if(length(x) == 0){
      x = 0
    }
    Xobs[i,j] = x
  }
}

# Format matrix 
colnames(Xobs) = data$years
rownames(Xobs) = 1:data$N_trees

# Melt to dataframe
Xobs_melt = melt(Xobs)
colnames(Xobs_melt) = c('Tree', 'Year', 'X')

## Use average increments and current DBH to get initial DBH (D0)
## = DBH at the start of the series

# Set initial DBH (= current DBH because we are working backwards)
D = matrix(, nrow = data$N_trees, ncol = data$N_years)
D[,data$N_years] = exp(data$logPDobs)

for(i in 1:data$N_trees){
  for(j in (data$N_years - 1):1){
    inc = Xobs_melt$X[which(Xobs_melt$Tree == i & Xobs_melt$Year == j + 1900)]
    D[i,j] = D[i,j+1] - (2 * inc / 10)
  }
}

D0 = D[,1]

# Organize DBH estimates
colnames(D) = c(data$years)
rownames(D) = c(1:data$N_trees)

D_melt = melt(D)
colnames(D_melt) = c('Tree', 'Year', 'DBH')

###########################
## Visualize initial DBH ##
###########################

## This section provides visuals to ensure that the initial DBHs in the 
## data match the assumptions in the prior D0 in the model

## Right now, the prior for D0 is ~U(0, 80)
## Make sure that the calculated D0 from above matches this!

# Plot histogram of estimated initial DBH
ggplot() +
  geom_histogram(aes(x = D0), bins = 40)

# Plot density with prior distribution
ggplot() +
  stat_density(aes(x = D0, fill = 'Calc'), alpha = 0.5) +
  stat_density(aes(x = runif(1000, 0, 80), fill = 'Prior'), alpha = 0.5) +
  scale_fill_manual(values = c('Calc' = 'lightblue', 'Prior' = 'seagreen'), name = '')

max(D0)

########################
## Process by species ##
########################

## This section processes species-specific data
## Increment by species, DBH by species

# Organize species from increment processing above
colnames(spec) = data$years
rownames(spec) = 1:data$N_trees

spec_melt = melt(spec)
colnames(spec_melt) = c('Tree', 'Year', 'Species')

# Add to Xobs_melt
X_spec = Xobs_melt %>%
  full_join(spec_melt, by = c('Tree', 'Year'))

# Add DBH to data frame with species
X_D_spec = X_spec %>%
  full_join(D_melt, by = c('Tree', 'Year'))

############################
## Species visualizations ##
############################

## This section provides visuals to make sure that there nothing weird
## is happening at the site for a specific species

## Are all the trees growing over time?
## Is DBH increasing over time?
## How do increments change over time?

labs = c('1' = 'ACSA', '2' = 'BEAL', '3' = 'THOC', '4' = 'TSCA')

# Plot increments with species as facets
ggplot(X_spec, aes(x = Year, y = X, group = Tree, color = as.factor(Tree))) +
  geom_line(show.legend = F) +
  facet_wrap(~Species, labeller = labeller(Species = labs))

# Plot DBH with species as facets
ggplot(X_D_spec, aes(x = Year, y = DBH, group = Tree, color = as.factor(Tree))) +
  geom_line(show.legend = F) +
  facet_wrap(~Species, labeller = labeller(Species = labs))
