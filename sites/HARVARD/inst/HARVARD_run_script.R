
# The following script can be used to run the model for Harvard Forest on your local machine. 

#####################################################
################ 0. Set up workspace ################
#####################################################
library(RW2BIO)

# rm(list=ls())
# setwd('/Users/marissakivi/Desktop/PalEON/RW-2-BIO')

# Load config file 
source('sites/HARVARD/inst/config.R')

# run reformatting script 
source(paste0('sites/',site,'/data/raw/past/',site,'_reformatting_',dvers,'.R'))

# rm(list=ls())
source('sites/HARVARD/inst/config.R')

#########################################################
################ 1. Check data files ################
#########################################################

# we need to see five columns here: ID, site, species, dbh, distance
# if site isn't present, that's OK 
treeMeta = read.csv(file.path('sites',site,'data','raw',paste0(site,'_treeMeta_',dvers,'.csv')), 
                    stringsAsFactors = FALSE)
head(treeMeta)

# we need to see at least six columns here: ID, site, species, distance, finalCond, and then columns with census diameter information
census = read.csv(file.path('sites',site,'data','raw',paste0(site,'_census_',dvers,'.csv')),
                  stringsAsFactors = FALSE)
head(census)

rm(treeMeta, census)

###############################################
################ 2. Build data ################
###############################################

library(plotrix)
library(dplR)
library(fields)
library(reshape2)
library(dplyr)
library(plyr)
library(ggplot2)

build_data(site = site, 
           dvers = dvers, 
           mvers = mvers, 
           prefix = prefix,
           census_site = census_site)

##############################################
################ 3. Run model ################
##############################################

library(rstan)
library(gridExtra)
library(ggplotify)

# Run STAN model(s)
run_model(census_site = census_site,
          site = site, 
          mvers = mvers, 
          dvers = dvers)

###################################################
################ 4. Process output ################
###################################################

library(ggplot2)
library(reshape2)
library(abind)
library(dplyr)

# Process STAN output(s) 
process_rw_model(census_site = census_site,
                 mvers = mvers, 
                 dvers = dvers, 
                 site = site, 
                 nest = nest, 
                 finalyr = finalyr)
