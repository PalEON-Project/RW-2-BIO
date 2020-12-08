
# The following R script can be submitted on the ND CRC cluster using the job script (after customization) 
# in the "jobs" folder

setwd('~/RW-2-BIO')

# load model run details 
source('sites/ROOSTER/inst/config.R')

# load needed function
source('R/run_rw_model.R')

.libPaths('~/Rlibs')

# prepare workspace 
library(rstan)
library(gridExtra)
library(ggplotify)

# run step 
run_rw_model(census_site = census_site, 
             site = site, 
             mvers = mvers, 
             dvers = dvers)
