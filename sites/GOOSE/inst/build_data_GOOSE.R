
# The following R script can be submitted on the ND CRC cluster using the job script (after customization) 
# in the "jobs" folder
# rm(list=ls())
# setwd('~/RW-2-BIO')

# load model run details 
source('sites/GOOSE/inst/config.R')

# load needed function
source('R/build_data.R')

require(reshape2)

# .libPaths('~/Rlibs')

# prepare workspace 
require(plotrix)
require(dplR)
require(fields)

require(plyr)
require(dplyr)
require(ggplot2)

# run step 
build_data(site = site, 
           dvers = dvers, 
           mvers = mvers, 
           prefix = prefix,
           census_site = census_site)
