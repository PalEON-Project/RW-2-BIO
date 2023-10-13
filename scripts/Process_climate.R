## Climate

rm(list = ls())

library(devtools)
install_github('ropensci/prism')
library(prism)
library(sp)
library(raster)

options(prism.path = 'climate')

get_prism_monthlys(type = 'tmean', years = 1900:1980, mon = 1:12, keepZip = F)
get_prism_monthlys(type = 'ppt', years = 1900:1980, mon = 1:12, keepZip = F)
get_prism_monthlys(type = 'tmean', years = 1981:2010, mon = 1:12, keepZip = F)
get_prism_monthlys(type = 'ppt', years = 1981:2010, mon = 1:12, keepZip = F)

prism_archive_ls()
RS <- pd_stack(prism_archive_ls())
proj4string(RS) <- CRS('+init=EPSG:4326')

df <- data.frame(rasterToPoints(RS))

