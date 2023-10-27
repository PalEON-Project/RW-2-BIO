# The following R script can be submitted on the ND CRC cluster using the job script (after customization)
# in the "jobs" folder

rm(list = ls())

# load model run details
source('sites/HMC/inst/config.R')

# load needed function
source('R/process_rw_model.R')

require(reshape2)

# preapre workspace
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
