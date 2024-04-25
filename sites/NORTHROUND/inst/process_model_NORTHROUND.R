
# The following R script can be submitted on the ND CRC cluster using the job script (after customization) 
# in the "jobs" folder

# setwd('~/RW-2-BIO')

# load model run details 
source('sites/NORTHROUND/inst/config.R')

# load needed function
source('R/process_rw_model.R')
source('scripts/validate_model.R')

require(reshape2)

# .libPaths('~/Rlibs')

# prepare workspace 
require(ggplot2)
require(abind)
require(dplyr)
require(grid)
require(gridExtra)

iter = 500

# run step 
process_rw_model(census_site = census_site,
                 mvers = mvers, 
                 dvers = dvers, 
                 site = site, 
                 nest = nest, 
                 finalyr = finalyr,
                 nchains = 1,
                 keep = iter/2,
                 pool = iter/2)

validate_rw_model(census_site = census_site,
                  mvers = mvers, 
                  dvers = dvers, 
                  site = site, 
                  nest = nest, 
                  nchains = 1,
                  keep = iter/2,
                  pool = iter/2)
