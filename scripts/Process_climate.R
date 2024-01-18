## Climate

rm(list = ls())

library(raster)
library(dplyr)
library(tidyr)
library(ggplot2)
library(terra)
library(sf)
library(rgdal)
library(fields)

# List all files that we want to read in ('bil' files)
ppt_historical_files <- list.files(path='/Volumes/FileBackup/PRISM_for_RW2BIO/PRISM_ppt_stable_4kmM2_189501_198012_bil/',pattern=paste(".*_",".*\\.bil$", sep = ""),full.names=TRUE)
tmean_historical_files <- list.files(path = '/Volumes/FileBackup/PRISM_for_RW2BIO/PRISM_tmean_stable_4kmM3_189501_198012_bil/', pattern = paste('.*_','.*\\.bil$', sep = ''), full.names = TRUE)
ppt_recent_files <- list.files(path = '/Volumes/FileBackup/PRISM_for_RW2BIO/PRISM_ppt_stable_4kmM3_198101_202303_bil/', pattern = paste0('.*_', '.*\\.bil$', sep = ''), full.names = T)
tmean_recent_files <- list.files(path = '/Volumes/FileBackup/PRISM_for_RW2BIO/PRISM_tmean_stable_4kmM3_198101_202303_bil/', pattern = paste0('.*_', '.*\\.bil$', sep = ''), full.names = T) 

# Stack the files
ppt_historical <- raster::stack(ppt_historical_files)
tmean_historical <- raster::stack(tmean_historical_files)
ppt_recent <- raster::stack(ppt_recent_files)
tmean_recent <- raster::stack(tmean_recent_files)

# NE and UMW extents
NE_ROU <- as(extent(-75, -72, 42, 43), 'SpatialPolygons')
UMW_ROU <- as(extent(-90, -89, 46, 47), 'SpatialPolygons')

# Clip
ppt_hisorical_NE <- raster::crop(ppt_historical, NE_ROU)
ppt_historical_UMW <- raster::crop(ppt_historical, UMW_ROU)
tmean_historical_NE <- raster::crop(tmean_historical, NE_ROU)
tmean_historical_UMW <- raster::crop(tmean_historical, UMW_ROU)
ppt_recent_NE <- raster::crop(ppt_recent, NE_ROU)
ppt_recent_UMW <- raster::crop(ppt_recent, UMW_ROU)
tmean_recent_NE <- raster::crop(tmean_recent, NE_ROU)
tmean_recent_UMW <- raster::crop(tmean_recent, UMW_ROU)

# Make points from rasters
ppt_historical_NE <- raster::rasterToPoints(ppt_hisorical_NE)
ppt_historical_UMW <- raster::rasterToPoints(ppt_historical_UMW)
tmean_historical_NE <- raster::rasterToPoints(tmean_historical_NE)
tmean_historical_UMW <- raster::rasterToPoints(tmean_historical_UMW)
ppt_recent_NE <- raster::rasterToPoints(ppt_recent_NE)
ppt_recent_UMW <- raster::rasterToPoints(ppt_recent_UMW)
tmean_recent_NE <- raster::rasterToPoints(tmean_recent_NE)
tmean_recent_UMW <- raster::rasterToPoints(tmean_recent_UMW)

ppt_historical <- rbind(ppt_historical_NE, ppt_historical_UMW)
tmean_historical <- rbind(tmean_historical_NE, tmean_historical_UMW)
ppt_recent <- rbind(ppt_recent_NE, ppt_recent_UMW)
tmean_recent <- rbind(tmean_recent_NE, tmean_recent_UMW)

# Save all points
save(ppt_historical, tmean_historical,
     ppt_recent, tmean_recent, file = '/Volumes/FileBackup/PRISM_for_RW2BIO/climate_matrix.RData')

# Re-load saved data
load('/Volumes/FileBackup/PRISM_for_RW2BIO/climate_matrix.RData')

# Reformat
ppt_historical <- as.data.frame(ppt_historical)
tmean_historical <- as.data.frame(tmean_historical)
ppt_recent <- as.data.frame(ppt_recent)
tmean_recent <- as.data.frame(tmean_recent)

# Add coordinates
coordinates(ppt_historical) <- ~x + y
coordinates(tmean_historical) <- ~x + y
coordinates(ppt_recent) <- ~x + y
coordinates(tmean_recent) <- ~x + y

# Add current projection
# Currently in GCS_North_American_1983 EPSG 4269
proj4string(ppt_historical) <- CRS('+init=epsg:4269')
proj4string(tmean_historical) <- CRS('+init=epsg:4269')
proj4string(ppt_recent) <- CRS('+init=epsg:4269')
proj4string(tmean_recent) <- CRS('+init=epsg:4269')

# Reproject to EPSG 4326
ppt_historical <- spTransform(ppt_historical, CRS('+init=epsg:4326'))
tmean_historical <- spTransform(tmean_historical, CRS('+init=epsg:4326'))
ppt_recent <- spTransform(ppt_recent, CRS('+init=epsg:4326'))
tmean_recent <- spTransform(tmean_recent, CRS('+init=epsg:4326'))

# Convert back to regular data frame
ppt_historical <- as.data.frame(ppt_historical)
tmean_historical <- as.data.frame(tmean_historical)
ppt_recent <- as.data.frame(ppt_recent)
tmean_recent <- as.data.frame(tmean_recent)

# Make matrix of only coordinates of PRISM data
prism_coords <- dplyr::select(ppt_historical, y, x)
colnames(prism_coords) <- c('lat', 'long')

# Make matrix of site coordinates
site_coords <- matrix(c(43.068496,	-73.297425,
                        43.2309,	-74.5267,
                        42.84514,	-72.4473,
                        46.241944,	-89.347778), byrow = T, ncol = 2)
site_coords <- cbind(site_coords,
                     c('GOOSE', 'ROOSTER', 'NRP', 'SYLVANIA'))

# Find distance between each prism-site pair
dists <- rdist(prism_coords, site_coords[,1:2])
# Find the closest PRISM estimate to each site
closest_points <- apply(dists, 2, which.min)

# Add uniqueID to ppt
ppt_historical2 <- ppt_historical |> 
  mutate(uniqueID = paste0(y,'_',x))

# Extract uniqueIDs of the closest points
uniqueIDs <- ppt_historical2$uniqueID[closest_points]

# Extract ppt columns that we want to pivot longer for pipeline below
ppt_cols <- colnames(ppt_historical)
ppt_cols <- ppt_cols[-((length(ppt_cols)-1):length(ppt_cols))]

# pivot longer and filter for specific sites
ppt_long <- ppt_historical |>
  mutate(uniqueID = paste0(y,'_',x)) |>
  rename(Longitude = x,
         Latitude = y) |>
  filter(uniqueID %in% uniqueIDs) |>
  dplyr::select(-uniqueID) |>
  pivot_longer(all_of(ppt_cols),
               names_to = 'var', values_to = 'PPT')

# Verify that historical and recent have the same coordinates in the same order
all(ppt_historical$x == ppt_recent$x & ppt_historical$y == ppt_recent$y)

# Repeat for recent
ppt_recent_cols <- colnames(ppt_recent)
ppt_recent_cols <- ppt_recent_cols[-((length(ppt_recent_cols)-1):length(ppt_recent_cols))]

ppt_recent_long <- ppt_recent |>
  mutate(uniqueID = paste0(y,'_',x)) |>
  rename(Longitude = x,
         Latitude = y) |>
  filter(uniqueID %in% uniqueIDs) |>
  dplyr::select(-uniqueID) |>
  pivot_longer(all_of(ppt_recent_cols),
               names_to = 'var', values_to = 'PPT')

# Combine
ppt_long_total <- ppt_long |>
  full_join(ppt_recent_long, by = c('Longitude', 'Latitude', 'var')) |>
  mutate(PPT2 = coalesce(PPT.x, PPT.y)) |>
  dplyr::select(-c(PPT.x, PPT.y))

# Repeat for temperature
# Make matrix of only coordinates of PRISM data
prism_coords <- dplyr::select(tmean_historical, y, x)
colnames(prism_coords) <- c('lat', 'long')

# Find distance between each prism-site pair
dists <- rdist(prism_coords, site_coords[,1:2])
# Find the closest PRISM estimate to each site
closest_points <- apply(dists, 2, which.min)

# Add uniqueID to tmean
tmean_historical2 <- tmean_historical |> 
  mutate(uniqueID = paste0(y,'_',x))

# Extract uniqueIDs of the closest points
uniqueIDs <- tmean_historical2$uniqueID[closest_points]

# Extract tmean columns that we want to pivot longer for pipeline below
tmean_cols <- colnames(tmean_historical)
tmean_cols <- tmean_cols[-((length(tmean_cols)-1):length(tmean_cols))]

# pivot longer and filter for specific sites
tmean_long <- tmean_historical |>
  mutate(uniqueID = paste0(y,'_',x)) |>
  rename(Longitude = x,
         Latitude = y) |>
  filter(uniqueID %in% uniqueIDs) |>
  dplyr::select(-uniqueID) |>
  pivot_longer(all_of(tmean_cols),
               names_to = 'var', values_to = 'Tmean')

# Verify that historical and recent have the same coordinates in the same order
all(tmean_historical$x == tmean_recent$x & tmean_historical$y == tmean_recent$y)

# Repeat for recent
tmean_recent_cols <- colnames(tmean_recent)
tmean_recent_cols <- tmean_recent_cols[-((length(tmean_recent_cols)-1):length(tmean_recent_cols))]

tmean_recent_long <- tmean_recent |>
  mutate(uniqueID = paste0(y,'_',x)) |>
  rename(Longitude = x,
         Latitude = y) |>
  filter(uniqueID %in% uniqueIDs) |>
  dplyr::select(-uniqueID) |>
  pivot_longer(all_of(tmean_recent_cols),
               names_to = 'var', values_to = 'Tmean')

# Combine
tmean_long_total <- tmean_long |>
  full_join(tmean_recent_long, by = c('Longitude', 'Latitude', 'var')) |>
  mutate(Tmean2 = coalesce(Tmean.x, Tmean.y)) |>
  dplyr::select(-c(Tmean.x, Tmean.y))

# Separate year and month from the "var" variable
# Done in a separate step to avoid maxing out memory
ppt_long <- ppt_long_total |>  
  mutate(year = substr(var, 24, 27),
         month = substr(var, 28, 29))

tmean_long <- tmean_long_total |>
  mutate(year = substr(var, 26, 29),
         month = substr(var, 30, 31))

# Add site names
ppt_long <- ppt_long |>
  mutate(loc = if_else(Longitude == unique(ppt_long$Longitude)[2], 'GOOSE', NA),
         loc = if_else(Longitude == unique(ppt_long$Longitude)[1], 'ROOSTER', loc),
         loc = if_else(Longitude == unique(ppt_long$Longitude)[3], 'NRP', loc),
         loc = if_else(Longitude == unique(ppt_long$Longitude)[4], 'SYLVANIA', loc)) |>
  dplyr::select(-c(var, Longitude, Latitude))

tmean_long <- tmean_long |>
  mutate(loc = if_else(Longitude == unique(tmean_long$Longitude)[2], 'GOOSE', NA),
         loc = if_else(Longitude == unique(tmean_long$Longitude)[1], 'ROOSTER', loc),
         loc = if_else(Longitude == unique(tmean_long$Longitude)[3], 'NRP', loc),
         loc = if_else(Longitude == unique(tmean_long$Longitude)[4], 'SYLVANIA', loc)) |>
  dplyr::select(-c(var, Longitude, Latitude))

# Combine
prism_long <- ppt_long |>
  left_join(tmean_long, by = c('year', 'month', 'loc'))

# Save
save(prism_long, file = '/Volumes/FileBackup/PRISM_for_RW2BIO/prism_clim.RData')
save(prism_long, file = 'climate/prism_clim.RData')
