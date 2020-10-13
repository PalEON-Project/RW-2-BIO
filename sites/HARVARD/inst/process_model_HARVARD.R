
# The following R script can be submitted on the ND CRC cluster using the job script (after customization) 
# in the "jobs" folder

setwd('~/RW-2-BIO')

# load model run details 
load('/sites/HARVARD/inst/config.R')

# load needed function
source('R/processs_rw_model.R')

# prepare workspace 
require(ggplot2)
require(reshape2)
require(abind)
require(dplyr)

# run step 
process_rw_model(census_site = census_site,
                 mvers = mvers, 
                 dvers = dvers, 
                 site = site, 
                 nest = nest, 
                 finalyr = finalyr)