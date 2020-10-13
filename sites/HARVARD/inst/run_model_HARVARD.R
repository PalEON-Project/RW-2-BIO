
# The following R script can be submitted on the ND CRC cluster using the job script (after customization) 
# in the "jobs" folder

setwd('~/RW-2-BIO')

# load model run details 
load('/sites/HARVARD/inst/config.R')

# load needed function
source('R/run_rw_model.R')

# prepare workspace 
require(rstan)
require(gridExtra)
require(ggplotify)

# run step 
run_rw_model(census_site = census_site, 
             site = site, 
             mvers = mvers, 
             dvers = dvers)
