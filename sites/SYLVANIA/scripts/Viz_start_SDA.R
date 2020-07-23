## This script explores stand structure through time to help pick a cutoff point
## to start SDA, for which the data reliably estimate the biomass of the stand
## (i.e. before the fading record becomes a big issue)

## This script requires that you ran the build_data script with the enitre
## time series and not the subset at 1900
## This is because this is intended to helpdetermine a cut-off point for 
## the build_data script

rm(list = ls())
setwd('~/Google Drive 2/McLachlan/SU20/npp-stat-model/')
library(ggplot2)
library(dplyr)
library(gridExtra)

###############
## Load data ##
###############

# Load
data = readRDS(file = 'sites/SYLVANIA/data/tree_data_SYLVANIA_STAN_FOR.AGE.ONLYv0.1.RDS')

# Unlist
for(i in 1:length(data)){
  temp = data[[i]]
  eval(parse(text=paste(names(data)[[i]],"= temp")))
}
rm(data)
rm(temp)

#########################
## Process to find age ##
#########################

# Make data frame of years the tree occurs in the time series
years = year_idx

# Add age based on start and end years
## Note: this only works if all end years are the same
for(i in 1:nrow(years)){
  years$age[i] = years$year_end[i] - years$year_start[i]
}

# Add tree index and species
years$tree = 1:N_trees
years$taxon = taxon

# Index for species numbers
specs = 1:N_taxa

# Add species code
for(i in 1:nrow(years)){
  for(j in 1:length(specs)){
    if(years$taxon[i] == j){
      years$taxon2[i] = as.character(taxaMatch[j,1])
    }
  }
}

# Plot ages of trees

ggplot(years, aes(x = age, fill = taxon2)) +
  geom_histogram(binwidth = 10)

##########################
## Live trees over time ##
##########################

# Storage
live_trees = cbind(c(1:N_years), rep(NA, length = N_years))
live_trees = as.data.frame(live_trees)
colnames(live_trees) = c('Year', 'Count')

# Initially one tree is in the series according to the start year column of years
live_trees[1,2] = length(which(years$year_start == 1))

# Add all other years
for(i in 2:nrow(live_trees)){
  live_trees[i,2] = live_trees[i-1,2] + length(which(years$year_start == i))
}

# Add calendar years
live_trees$cal_year = c(1639:2018)

# Plot number of trees over time
ggplot(live_trees, aes(x = cal_year, y = Count)) +
  geom_line() +
  xlab('Calendar year') + ylab('Number of living trees')

# Find year at which different fractions of extant trees were living
frac_50 = live_trees$cal_year[min(which(live_trees$Count >= 0.5 * max(live_trees$Count)))]
frac_75 = live_trees$cal_year[min(which(live_trees$Count >= 0.75 * max(live_trees$Count)))]
frac_90 = live_trees$cal_year[min(which(live_trees$Count >= 0.9 * max(live_trees$Count)))]
frac_95 = live_trees$cal_year[min(which(live_trees$Count >= 0.95 * max(live_trees$Count)))]

# Plot with these lines
ggplot(live_trees, aes(x = cal_year, y = Count)) +
  geom_line() +
  geom_vline(aes(xintercept = frac_50, color = '50%'), lty = 2) +
  geom_vline(aes(xintercept = frac_75, color = '75%'), lty = 2) +
  geom_vline(aes(xintercept = frac_90, color = '90%'), lty = 2) +
  geom_vline(aes(xintercept = frac_95, color = '95%'), lty = 2) +
  xlab('Calendar year') + ylab('Number of living trees') +
  scale_color_discrete(name = 'Fraction trees alive')

####################
## Age by species ##
####################

## Note: this needs to be adjusted for the number of species in a given dataset

# Repeat steps to count number of trees existing over time for each species
live_trees_1 = cbind(c(1:N_years), rep(NA, length = N_years))
live_trees_2 = cbind(c(1:N_years), rep(NA, length = N_years))
live_trees_3 = cbind(c(1:N_years), rep(NA, length = N_years))
live_trees_4 = cbind(c(1:N_years), rep(NA, length = N_years))

live_trees_1 = as.data.frame(live_trees_1)
live_trees_2 = as.data.frame(live_trees_2)
live_trees_3 = as.data.frame(live_trees_3)
live_trees_4 = as.data.frame(live_trees_4)

colnames(live_trees_1) = c('Year', 'Count')
colnames(live_trees_2) = c('Year', 'Count')
colnames(live_trees_3) = c('Year', 'Count')
colnames(live_trees_4) = c('Year', 'Count')

live_trees_1[1,2] = length(which(years$taxon == 1 & years$year_start == 1))
live_trees_2[1,2] = length(which(years$taxon == 2 & years$year_start == 1))
live_trees_3[1,2] = length(which(years$taxon == 3 & years$year_start == 1))
live_trees_4[1,2] = length(which(years$taxon == 4 & years$year_start == 1))

for(i in 2:N_years){
  live_trees_1[i,2] = live_trees_1[i-1,2] + length(which(years$taxon == 1 & years$year_start == i))
  live_trees_2[i,2] = live_trees_2[i-1,2] + length(which(years$taxon == 2 & years$year_start == i))
  live_trees_3[i,2] = live_trees_3[i-1,2] + length(which(years$taxon == 3 & years$year_start == i))
  live_trees_4[i,2] = live_trees_4[i-1,2] + length(which(years$taxon == 4 & years$year_start == i))
}

# Add species column
live_trees_1$Species = rep(as.character(taxaMatch[1,1]), length = N_years)
live_trees_2$Species = rep(as.character(taxaMatch[2,1]), length = N_years)
live_trees_3$Species = rep(as.character(taxaMatch[3,1]), length = N_years)
live_trees_4$Species = rep(as.character(taxaMatch[4,1]), length = N_years)

# Add calendar years
live_trees_1$cal_year = c(1639:2018)
live_trees_2$cal_year = c(1639:2018)
live_trees_3$cal_year = c(1639:2018)
live_trees_4$cal_year = c(1639:2018)

# Add fraction column
for(i in 1:N_years){
  live_trees_1$frac[i] = live_trees_1$Count[i] / max(live_trees_1$Count)
  live_trees_2$frac[i] = live_trees_2$Count[i] / max(live_trees_2$Count)
  live_trees_3$frac[i] = live_trees_3$Count[i] / max(live_trees_3$Count)
  live_trees_4$frac[i] = live_trees_4$Count[i] / max(live_trees_4$Count)
}

# Put together
live_trees_all = rbind(live_trees_1, live_trees_2, live_trees_3, live_trees_4)

# Plot number of trees of each species over time
ggplot(live_trees_all, aes(x = cal_year, y = Count, group = Species)) +
  geom_line() +
  facet_wrap(~Species) +
  xlab('Calendar year') + ylab('Number of living trees')

ggplot(live_trees_all, aes(x = cal_year, y = frac, group = Species)) +
  geom_line() +
  facet_wrap(~Species) +
  xlab('Calendar year') + ylab('Number of living trees')

# Make fractions as before
frac_1_50 = live_trees_1$cal_year[min(which(live_trees_1$Count >= 0.5 * max(live_trees_1$Count)))]
frac_1_75 = live_trees_1$cal_year[min(which(live_trees_1$Count >= 0.75 * max(live_trees_1$Count)))]
frac_1_90 = live_trees_1$cal_year[min(which(live_trees_1$Count >= 0.9 * max(live_trees_1$Count)))]
frac_1_95 = live_trees_1$cal_year[min(which(live_trees_1$Count >= 0.95 * max(live_trees_1$Count)))]

frac_2_50 = live_trees_2$cal_year[min(which(live_trees_2$Count >= 0.5 * max(live_trees_2$Count)))]
frac_2_75 = live_trees_2$cal_year[min(which(live_trees_2$Count >= 0.75 * max(live_trees_2$Count)))]
frac_2_90 = live_trees_2$cal_year[min(which(live_trees_2$Count >= 0.9 * max(live_trees_2$Count)))]
frac_2_95 = live_trees_2$cal_year[min(which(live_trees_2$Count >= 0.95 * max(live_trees_2$Count)))]

frac_3_50 = live_trees_3$cal_year[min(which(live_trees_3$Count >= 0.5 * max(live_trees_3$Count)))]
frac_3_75 = live_trees_3$cal_year[min(which(live_trees_3$Count >= 0.75 * max(live_trees_3$Count)))]
frac_3_90 = live_trees_3$cal_year[min(which(live_trees_3$Count >= 0.9 * max(live_trees_3$Count)))]
frac_3_95 = live_trees_3$cal_year[min(which(live_trees_3$Count >= 0.95 * max(live_trees_3$Count)))]

frac_4_50 = live_trees_4$cal_year[min(which(live_trees_4$Count >= 0.5 * max(live_trees_4$Count)))]
frac_4_75 = live_trees_4$cal_year[min(which(live_trees_4$Count >= 0.75 * max(live_trees_4$Count)))]
frac_4_90 = live_trees_4$cal_year[min(which(live_trees_4$Count >= 0.9 * max(live_trees_4$Count)))]
frac_4_95 = live_trees_4$cal_year[min(which(live_trees_4$Count >= 0.95 * max(live_trees_4$Count)))]

# Plot all trees with lines for each species
p1 = ggplot(live_trees, aes(x = cal_year, y = Count)) +
  geom_line() +
  geom_vline(aes(xintercept = frac_1_50, color = 'ACSA')) +
  geom_vline(aes(xintercept = frac_2_50, color = 'BEAL')) +
  geom_vline(aes(xintercept = frac_3_50, color = 'THOC')) +
  geom_vline(aes(xintercept = frac_4_50, color = 'TSCA')) +
  xlab('Calendar year') + ylab('Number of living trees') +
  scale_color_discrete(name = '50% of trees alive')

p2 = ggplot(live_trees, aes(x = cal_year, y = Count)) +
  geom_line() +
  geom_vline(aes(xintercept = frac_1_75, color = 'ACSA')) +
  geom_vline(aes(xintercept = frac_2_75, color = 'BEAL')) +
  geom_vline(aes(xintercept = frac_3_75, color = 'THOC')) +
  geom_vline(aes(xintercept = frac_4_75, color = 'TSCA')) +
  xlab('Calendar year') + ylab('Number of living trees') +
  scale_color_discrete(name = '75% of trees alive')

p3 = ggplot(live_trees, aes(x = cal_year, y = Count)) +
  geom_line() +
  geom_vline(aes(xintercept = frac_1_90, color = 'ACSA')) +
  geom_vline(aes(xintercept = frac_2_90, color = 'BEAL')) +
  geom_vline(aes(xintercept = frac_3_90, color = 'THOC')) +
  geom_vline(aes(xintercept = frac_4_90, color = 'TSCA')) +
  xlab('Calendar year') + ylab('Number of living trees') +
  scale_color_discrete(name = '90% of trees alive')

p4 = ggplot(live_trees, aes(x = cal_year, y = Count)) +
  geom_line() +
  geom_vline(aes(xintercept = frac_1_95, color = 'ACSA')) +
  geom_vline(aes(xintercept = frac_2_95, color = 'BEAL')) +
  geom_vline(aes(xintercept = frac_3_95, color = 'THOC')) +
  geom_vline(aes(xintercept = frac_4_95, color = 'TSCA')) +
  xlab('Calendar year') + ylab('Number of living trees') +
  scale_color_discrete(name = '95% of trees alive')

grid.arrange(p1, p2, p3, p4, nrow = 2)
