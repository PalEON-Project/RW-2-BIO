# The following R script can be submitted on the ND CRC cluster using the job script (after customization)
# in the "jobs" folder
# rm(list = ls())
# setwd('~/RW-2-BIO')

# .libPaths('~/Rlibs')

# load model run details
source('sites/HMC/inst/config.R')

# load needed function
# source('R/build_data.R')
source('R/build_data_HMC_edit.R')

# prepare workspace
require(plotrix)
require(dplR)
require(fields)

require(reshape2)
require(plyr)
require(dplyr)
require(ggplot2)

# run step
build_data(site = site,
           dvers = dvers,
           mvers = mvers,
           prefix = prefix,
           census_site = census_site)
