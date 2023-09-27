
# The following R script can be submitted on the ND CRC cluster using the job script (after customization) 
# in the "jobs" folder

# setwd('~/RW-2-BIO')

# load model run details 
source('sites/ROOSTER/inst/config.R')

# load needed function
source('R/process_rw_model.R')

require(reshape2)

.libPaths('~/Rlibs')

# prepare workspace 
require(ggplot2)
require(abind)
require(dplyr)
require(grid)

# run step 
process_rw_model(census_site = census_site,
                 mvers = mvers, 
                 dvers = dvers, 
                 site = site, 
                 nest = nest, 
                 finalyr = finalyr,
                 nchains = 1,
                 pool = 500)
