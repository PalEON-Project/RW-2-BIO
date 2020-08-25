
# The xlsx file with the tree meta data from the zip file on the wiki was first converted 
# into a CSV file. 

#####################################################
################ 0. Set up workspace ################
#####################################################

rm(list=ls())
setwd('/Users/marissakivi/Desktop/RW-2-BIO')

# Load config file 
source('sites/SYLVANIA/config.R')

# run reformatting script 
source(paste0('sites/',site,'/data/raw/past/',site,'_reformatting_',dvers,'.R'))

rm(list=ls())
source('sites/SYLVANIA/config.R')

#########################################################
################ 1. Check data files ################
#########################################################

# we need to see five columns here: ID, site, species, dbh, distance
# if site isn't present, that's OK 
treeMeta = read.csv(file.path('sites',site,'data','raw',paste0(site,'_treeMeta_',dvers,'.csv')), 
                    stringsAsFactors = FALSE)
head(treeMeta)

rm(treeMeta)

###############################################
################ 2. Build data ################
###############################################

source('R/build_data.R')
build_data(site = site, 
           dvers = dvers, 
           mvers = mvers, 
           prefix = prefix,
           census_site = census_site)

##############################################
################ 3. Run model ################
##############################################

# Run STAN model(s)
source('R/run_model.R') 
run_model(census_site = census_site,
          site = site, 
          mvers = mvers, 
          dvers = dvers)

###################################################
################ 4. Process output ################
###################################################

# Process STAN output(s) 
source('R/process_rw_model.R')
process_rw_model(census_site = census_site,
                 mvers = mvers, 
                 dvers = dvers, 
                 site = site, 
                 nest = nest, 
                 finalyr = finalyr)
