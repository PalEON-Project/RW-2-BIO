
# The following R script can be submitted on the ND CRC cluster using the job script (after customization) 
# in the "jobs" folder

setwd('~/RW-2-BIO')

# load model run details 
source('sites/GOOSE/inst/config.R')

# load needed function
source('R/run_rw_model.R')

.libPaths('~/Rlibs')

# prepare workspace 
library(rstan)
library(gridExtra)
library(ggplotify)

# run_rw_model <- function(census_site, site, mvers, 
#                          dvers, keep = 300, nchains = 3, pool = 2500){

# run step 
run_rw_model(census_site = census_site, 
             site = site, 
             mvers = mvers, 
             dvers = dvers,
             nchains = 1, 
             iter = 500)
