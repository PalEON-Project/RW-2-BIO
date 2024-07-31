## Climate

rm(list = ls())

# List all files that we want to read in ('bil' files)
ppt_historical_files <- list.files(path='/Volumes/FileBackup/PRISM_for_RW2BIO/PRISM_ppt_stable_4kmM2_189501_198012_bil/',pattern=paste(".*_",".*\\.bil$", sep = ""),full.names=TRUE)
tmean_historical_files <- list.files(path = '/Volumes/FileBackup/PRISM_for_RW2BIO/PRISM_tmean_stable_4kmM3_189501_198012_bil/', pattern = paste('.*_','.*\\.bil$', sep = ''), full.names = TRUE)
tmax_historical_files <- list.files(path = '/Volumes/FileBackup/PRISM_for_RW2BIO/PRISM_tmax_stable_4kmM3_189501_198012_bil/', pattern = paste('.*_','.*\\.bil$', sep = ''), full.names = TRUE)
tmin_historical_files <- list.files(path = '/Volumes/FileBackup/PRISM_for_RW2BIO/PRISM_tmin_stable_4kmM3_189501_198012_bil/', pattern = paste('.*_','.*\\.bil$', sep = ''), full.names = TRUE)
vpdmax_historical_files <- list.files(path = '/Volumes/FileBackup/PRISM_for_RW2BIO/PRISM_vpdmax_stable_4kmM3_189501_198012_bil/', pattern = paste('.*_', '.*\\.bil$', sep = ''), full.names = TRUE)
vpdmin_historical_files <- list.files(path = '/Volumes/FileBackup/PRISM_for_RW2BIO/PRISM_vpdmin_stable_4kmM3_189501_198012_bil/', pattern = paste('.*_', '.*\\.bil$', sep = ''), full.names = TRUE)
ppt_recent_files <- list.files(path = '/Volumes/FileBackup/PRISM_for_RW2BIO/PRISM_ppt_stable_4kmM3_198101_202303_bil/', pattern = paste0('.*_', '.*\\.bil$', sep = ''), full.names = T)
tmean_recent_files <- list.files(path = '/Volumes/FileBackup/PRISM_for_RW2BIO/PRISM_tmean_stable_4kmM3_198101_202303_bil/', pattern = paste0('.*_', '.*\\.bil$', sep = ''), full.names = T) 
tmax_recent_files <- list.files(path = '/Volumes/FileBackup/PRISM_for_RW2BIO/PRISM_tmax_stable_4kmM3_198101_202306_bil/', pattern = paste0('.*_','.*\\.bil$', sep = ''), full.names = TRUE)
tmin_recent_files <- list.files(path = '/Volumes/FileBackup/PRISM_for_RW2BIO/PRISM_tmin_stable_4kmM3_198101_202306_bil/', pattern = paste0('.*_', '.*\\.bil$', sep = ''), full.names = TRUE)
vpdmax_recent_files <- list.files(path = '/Volumes/FileBackup/PRISM_for_RW2BIO/PRISM_vpdmax_stable_4kmM3_198101_202306_bil/', pattern = paste0('.*_', '.*\\.bil$', sep = ''), full.names = TRUE)
vpdmin_recent_files <- list.files(path = '/Volumes/FileBackup/PRISM_for_RW2BIO/PRISM_vpdmin_stable_4kmM3_198101_202306_bil/', pattern = paste0('.*_', '.*\\.bil$', sep = ''), full.names = TRUE)

# Stack the files
ppt_historical <- raster::stack(ppt_historical_files)
tmean_historical <- raster::stack(tmean_historical_files)
tmax_historical <- raster::stack(tmax_historical_files)
tmin_historical <- raster::stack(tmin_historical_files)
vpdmax_historical <- raster::stack(vpdmax_historical_files)
vpdmin_historical <- raster::stack(vpdmin_historical_files)
ppt_recent <- raster::stack(ppt_recent_files)
tmean_recent <- raster::stack(tmean_recent_files)
tmax_recent <- raster::stack(tmax_recent_files)
tmin_recent <- raster::stack(tmin_recent_files)
vpdmax_recent <- raster::stack(vpdmax_recent_files)
vpdmin_recent <- raster::stack(vpdmin_recent_files)

# NE and UMW extents
NE_ROU <- as(raster::extent(-75, -72, 42, 43), 'SpatialPolygons')
UMW_ROU <- as(raster::extent(-90, -89, 46, 47), 'SpatialPolygons')

# Clip
ppt_hisorical_NE <- raster::crop(ppt_historical, NE_ROU)
ppt_historical_UMW <- raster::crop(ppt_historical, UMW_ROU)
tmean_historical_NE <- raster::crop(tmean_historical, NE_ROU)
tmean_historical_UMW <- raster::crop(tmean_historical, UMW_ROU)
tmin_historical_NE <- raster::crop(tmin_historical, NE_ROU)
tmin_historical_UMW <- raster::crop(tmin_historical, UMW_ROU)
tmax_historical_NE <- raster::crop(tmax_historical, NE_ROU)
tmax_historical_UMW <- raster::crop(tmax_historical, UMW_ROU)
vpdmax_historical_NE <- raster::crop(vpdmax_historical, NE_ROU)
vpdmax_historical_UMW <- raster::crop(vpdmax_historical, UMW_ROU)
vpdmin_historical_NE <- raster::crop(vpdmin_historical, NE_ROU)
vpdmin_historical_UMW <- raster::crop(vpdmin_historical, UMW_ROU)
ppt_recent_NE <- raster::crop(ppt_recent, NE_ROU)
ppt_recent_UMW <- raster::crop(ppt_recent, UMW_ROU)
tmean_recent_NE <- raster::crop(tmean_recent, NE_ROU)
tmean_recent_UMW <- raster::crop(tmean_recent, UMW_ROU)
tmax_recent_NE <- raster::crop(tmax_recent, NE_ROU)
tmax_recent_UMW <- raster::crop(tmax_recent, UMW_ROU)
tmin_recent_NE <- raster::crop(tmin_recent, NE_ROU)
tmin_recent_UMW <- raster::crop(tmin_recent, UMW_ROU)
vpdmin_recent_NE <- raster::crop(vpdmin_recent, NE_ROU)
vpdmin_recent_UMW <- raster::crop(vpdmin_recent, UMW_ROU)
vpdmax_recent_NE <- raster::crop(vpdmax_recent, NE_ROU)
vpdmax_recent_UMW <- raster::crop(vpdmax_recent, UMW_ROU)

# Make points from rasters
ppt_historical_NE <- raster::rasterToPoints(ppt_hisorical_NE)
ppt_historical_UMW <- raster::rasterToPoints(ppt_historical_UMW)
tmean_historical_NE <- raster::rasterToPoints(tmean_historical_NE)
tmean_historical_UMW <- raster::rasterToPoints(tmean_historical_UMW)
tmax_historical_NE <- raster::rasterToPoints(tmax_historical_NE)
tmax_historical_UMW <- raster::rasterToPoints(tmax_historical_UMW)
tmin_historical_NE <- raster::rasterToPoints(tmin_historical_NE)
tmin_historical_UMW <- raster::rasterToPoints(tmin_historical_UMW)
vpdmax_historical_NE <- raster::rasterToPoints(vpdmax_historical_NE)
vpdmax_historical_UMW <- raster::rasterToPoints(vpdmax_historical_UMW)
vpdmin_historical_NE <- raster::rasterToPoints(vpdmin_historical_NE)
vpdmin_historical_UMW <- raster::rasterToPoints(vpdmin_historical_UMW)
ppt_recent_NE <- raster::rasterToPoints(ppt_recent_NE)
ppt_recent_UMW <- raster::rasterToPoints(ppt_recent_UMW)
tmean_recent_NE <- raster::rasterToPoints(tmean_recent_NE)
tmean_recent_UMW <- raster::rasterToPoints(tmean_recent_UMW)
tmax_recent_NE <- raster::rasterToPoints(tmax_recent_NE)
tmax_recent_UMW <- raster::rasterToPoints(tmax_recent_UMW)
tmin_recent_NE <- raster::rasterToPoints(tmin_recent_NE)
tmin_recent_UMW <- raster::rasterToPoints(tmin_recent_UMW)
vpdmax_recent_NE <- raster::rasterToPoints(vpdmax_recent_NE)
vpdmax_recent_UMW <- raster::rasterToPoints(vpdmax_recent_UMW)
vpdmin_recent_NE <- raster::rasterToPoints(vpdmin_recent_NE)
vpdmin_recent_UMW <- raster::rasterToPoints(vpdmin_recent_UMW)

# Combine dataframes
ppt_historical <- rbind(ppt_historical_NE, ppt_historical_UMW)
tmean_historical <- rbind(tmean_historical_NE, tmean_historical_UMW)
tmax_historical <- rbind(tmax_historical_NE, tmax_historical_UMW)
tmin_historical <- rbind(tmin_historical_NE, tmin_historical_UMW)
vpdmin_historical <- rbind(vpdmin_historical_NE, vpdmin_historical_UMW)
vpdmax_historical <- rbind(vpdmax_historical_NE, vpdmax_historical_UMW)
ppt_recent <- rbind(ppt_recent_NE, ppt_recent_UMW)
tmean_recent <- rbind(tmean_recent_NE, tmean_recent_UMW)
tmax_recent <- rbind(tmax_recent_NE, tmax_recent_UMW)
tmin_recent <- rbind(tmin_recent_NE, tmin_recent_UMW)
vpdmin_recent <- rbind(vpdmin_recent_NE, vpdmin_recent_UMW)
vpdmax_recent <- rbind(vpdmax_recent_NE, vpdmax_recent_UMW)

# Save all points
save(ppt_historical, tmean_historical, tmin_historical, tmax_historical, vpdmin_historical, vpdmax_historical,
     ppt_recent, tmean_recent, tmin_recent, tmax_recent, vpdmin_recent, vpdmax_recent, 
     file = '/Volumes/FileBackup/PRISM_for_RW2BIO/climate_matrix.RData')

# Re-load saved data
load('/Volumes/FileBackup/PRISM_for_RW2BIO/climate_matrix.RData')

# Reformat
ppt_historical <- as.data.frame(ppt_historical)
tmean_historical <- as.data.frame(tmean_historical)
tmin_historical <- as.data.frame(tmin_historical)
tmax_historical <- as.data.frame(tmax_historical)
vpdmin_historical <- as.data.frame(vpdmin_historical)
vpdmax_historical <- as.data.frame(vpdmax_historical)
ppt_recent <- as.data.frame(ppt_recent)
tmean_recent <- as.data.frame(tmean_recent)
tmin_recent <- as.data.frame(tmin_recent)
tmax_recent <- as.data.frame(tmax_recent)
vpdmin_recent <- as.data.frame(vpdmin_recent)
vpdmax_recent <- as.data.frame(vpdmax_recent)

# Add coordinates
ppt_historical <- sf::st_as_sf(ppt_historical, coords = c('x', 'y'))
tmean_historical <- sf::st_as_sf(tmean_historical, coords = c('x', 'y'))
tmin_historical <- sf::st_as_sf(tmin_historical, coords = c('x', 'y'))
tmax_historical <- sf::st_as_sf(tmax_historical, coords = c('x', 'y'))
vpdmin_historical <- sf::st_as_sf(vpdmin_historical, coords = c('x', 'y'))
vpdmax_historical <- sf::st_as_sf(vpdmax_historical, coords = c('x', 'y'))
ppt_recent <- sf::st_as_sf(ppt_recent, coords = c('x', 'y'))
tmean_recent <- sf::st_as_sf(tmean_recent, coords = c('x', 'y'))
tmin_recent <- sf::st_as_sf(tmin_recent, coords = c('x', 'y'))
tmax_recent <- sf::st_as_sf(tmax_recent, coords = c('x', 'y'))
vpdmin_recent <- sf::st_as_sf(vpdmin_recent, coords = c('x', 'y'))
vpdmax_recent <- sf::st_as_sf(vpdmax_recent, coords = c('x', 'y'))

# Add current projection
# Currently in GCS_North_American_1983 EPSG 4269
sf::st_crs(ppt_historical) <- sf::st_crs(tmean_historical) <- sf::st_crs(tmin_historical) <-
  sf::st_crs(tmax_historical) <- sf::st_crs(vpdmin_historical) <- sf::st_crs(vpdmax_historical) <- 'EPSG:4269'
sf::st_crs(ppt_recent) <- sf::st_crs(tmean_recent) <- sf::st_crs(tmin_recent) <- sf::st_crs(tmax_recent) <-
  sf::st_crs(vpdmin_recent) <- sf::st_crs(vpdmax_recent) <- 'EPSG:4269'

# Reproject to EPSG 4326
ppt_historical <- sf::st_transform(ppt_historical, crs = 'EPSG:4326')
tmean_historical <- sf::st_transform(tmean_historical, crs = 'EPSG:4326')
tmin_historical <- sf::st_transform(tmin_historical, crs = 'EPSG:4326')
tmax_historical <- sf::st_transform(tmax_historical, crs = 'EPSG:4326')
vpdmin_historical <- sf::st_transform(vpdmin_historical, crs = 'EPSG:4326')
vpdmax_historical <- sf::st_transform(vpdmax_historical, crs = 'EPSG:4326')
ppt_recent <- sf::st_transform(ppt_recent, crs = 'EPSG:4326')
tmean_recent <- sf::st_transform(tmean_recent, crs = 'EPSG:4326')
tmin_recent <- sf::st_transform(tmin_recent, crs = 'EPSG:4326')
tmax_recent <- sf::st_transform(tmax_recent, crs = 'EPSG:4326')
vpdmin_recent <- sf::st_transform(vpdmin_recent, crs = 'EPSG:4326')
vpdmax_recent <- sf::st_transform(vpdmax_recent, crs = 'EPSG:4326')

# Convert back to regular data frame
ppt_historical <- sfheaders::sf_to_df(ppt_historical, fill = TRUE)
tmean_historical <- sfheaders::sf_to_df(tmean_historical, fill = TRUE)
tmin_historical <- sfheaders::sf_to_df(tmin_historical, fill = TRUE)
tmax_historical <- sfheaders::sf_to_df(tmax_historical, fill = TRUE)
vpdmin_historical <- sfheaders::sf_to_df(vpdmin_historical, fill = TRUE)
vpdmax_historical <- sfheaders::sf_to_df(vpdmax_historical, fill = TRUE)
ppt_recent <- sfheaders::sf_to_df(ppt_recent, fill = TRUE)
tmean_recent <- sfheaders::sf_to_df(tmean_recent, fill = TRUE)
tmin_recent <- sfheaders::sf_to_df(tmin_recent, fill = TRUE)
tmax_recent <- sfheaders::sf_to_df(tmax_recent, fill = TRUE)
vpdmin_recent <- sfheaders::sf_to_df(vpdmin_recent, fill = TRUE)
vpdmax_recent <- sfheaders::sf_to_df(vpdmax_recent, fill = TRUE)

ppt_historical <- dplyr::select(ppt_historical, -c(sfg_id, point_id))
tmean_historical <- dplyr::select(tmean_historical, -c(sfg_id, point_id))
tmin_historical <- dplyr::select(tmin_historical, -c(sfg_id, point_id))
tmax_historical <- dplyr::select(tmax_historical, -c(sfg_id, point_id))
vpdmin_historical <- dplyr::select(vpdmin_historical, -c(sfg_id, point_id))
vpdmax_historical <- dplyr::select(vpdmax_historical, -c(sfg_id, point_id))
ppt_recent <- dplyr::select(ppt_recent, -c(sfg_id, point_id))
tmean_recent <- dplyr::select(tmean_recent, -c(sfg_id, point_id))
tmin_recent <- dplyr::select(tmin_recent, -c(sfg_id, point_id))
tmax_recent <- dplyr::select(tmax_recent, -c(sfg_id, point_id))
vpdmin_recent <- dplyr::select(vpdmin_recent, -c(sfg_id, point_id))
vpdmax_recent <- dplyr::select(vpdmax_recent, -c(sfg_id, point_id))

#### PPT ####

# Make matrix of only coordinates of PRISM data
prism_coords <- dplyr::select(ppt_historical, y, x)
colnames(prism_coords) <- c('lat', 'long')

# Make matrix of site coordinates
site_coords <- matrix(c(43.068496,	-73.297425,
                        43.2309,	-74.5267,
                        42.84514,	-72.4473,
                        46.241944,	-89.347778,
                        42.53, -72.18), 
                      byrow = T, ncol = 2)
site_coords <- cbind(site_coords,
                     c('GOOSE', 'ROOSTER', 'NRP', 'SYLVANIA', 'HARVARD'))

# Find distance between each prism-site pair
dists <- fields::rdist(prism_coords, site_coords[,1:2])
# Find the closest PRISM estimate to each site
closest_points <- apply(dists, 2, which.min)

# Add uniqueID to ppt
ppt_historical2 <- ppt_historical |> 
  dplyr::mutate(uniqueID = paste0(y,'_',x))

# Extract uniqueIDs of the closest points
uniqueIDs <- ppt_historical2$uniqueID[closest_points]

# Extract ppt columns that we want to pivot longer for pipeline below
ppt_cols <- colnames(ppt_historical)
ppt_cols <- ppt_cols[-((length(ppt_cols)-1):length(ppt_cols))]

# pivot longer and filter for specific sites
ppt_long <- ppt_historical |>
  dplyr::mutate(uniqueID = paste0(y,'_',x)) |>
  dplyr::rename(Longitude = x,
                Latitude = y) |>
  dplyr::filter(uniqueID %in% uniqueIDs) |>
  dplyr::select(-uniqueID) |>
  tidyr::pivot_longer(all_of(ppt_cols),
                      names_to = 'var', values_to = 'PPT')

# Verify that historical and recent have the same coordinates in the same order
all(ppt_historical$x == ppt_recent$x & ppt_historical$y == ppt_recent$y)

# Repeat for recent
ppt_recent_cols <- colnames(ppt_recent)
ppt_recent_cols <- ppt_recent_cols[-((length(ppt_recent_cols)-1):length(ppt_recent_cols))]

ppt_recent_long <- ppt_recent |>
  dplyr::mutate(uniqueID = paste0(y,'_',x)) |>
  dplyr::rename(Longitude = x,
                Latitude = y) |>
  dplyr::filter(uniqueID %in% uniqueIDs) |>
  dplyr::select(-uniqueID) |>
  tidyr::pivot_longer(all_of(ppt_recent_cols),
                      names_to = 'var', values_to = 'PPT')

# Combine
ppt_long_total <- ppt_long |>
  dplyr::full_join(ppt_recent_long, by = c('Longitude', 'Latitude', 'var')) |>
  dplyr::mutate(PPT2 = dplyr::coalesce(PPT.x, PPT.y)) |>
  dplyr::select(-c(PPT.x, PPT.y))

#### TMEAN ####

# Make matrix of only coordinates of PRISM data
prism_coords <- dplyr::select(tmean_historical, y, x)
colnames(prism_coords) <- c('lat', 'long')

# Find distance between each prism-site pair
dists <- fields::rdist(prism_coords, site_coords[,1:2])
# Find the closest PRISM estimate to each site
closest_points <- apply(dists, 2, which.min)

# Add uniqueID to tmean
tmean_historical2 <- tmean_historical |> 
  dplyr::mutate(uniqueID = paste0(y,'_',x))

# Extract uniqueIDs of the closest points
uniqueIDs <- tmean_historical2$uniqueID[closest_points]

# Extract tmean columns that we want to pivot longer for pipeline below
tmean_cols <- colnames(tmean_historical)
tmean_cols <- tmean_cols[-((length(tmean_cols)-1):length(tmean_cols))]

# pivot longer and filter for specific sites
tmean_long <- tmean_historical |>
  dplyr::mutate(uniqueID = paste0(y,'_',x)) |>
  dplyr::rename(Longitude = x,
                Latitude = y) |>
  dplyr::filter(uniqueID %in% uniqueIDs) |>
  dplyr::select(-uniqueID) |>
  tidyr::pivot_longer(all_of(tmean_cols),
                      names_to = 'var', values_to = 'Tmean')

# Verify that historical and recent have the same coordinates in the same order
all(tmean_historical$x == tmean_recent$x & tmean_historical$y == tmean_recent$y)

# Repeat for recent
tmean_recent_cols <- colnames(tmean_recent)
tmean_recent_cols <- tmean_recent_cols[-((length(tmean_recent_cols)-1):length(tmean_recent_cols))]

tmean_recent_long <- tmean_recent |>
  dplyr::mutate(uniqueID = paste0(y,'_',x)) |>
  dplyr::rename(Longitude = x,
                Latitude = y) |>
  dplyr::filter(uniqueID %in% uniqueIDs) |>
  dplyr::select(-uniqueID) |>
  tidyr::pivot_longer(all_of(tmean_recent_cols),
                      names_to = 'var', values_to = 'Tmean')

# Combine
tmean_long_total <- tmean_long |>
  dplyr::full_join(tmean_recent_long, by = c('Longitude', 'Latitude', 'var')) |>
  dplyr::mutate(Tmean2 = dplyr::coalesce(Tmean.x, Tmean.y)) |>
  dplyr::select(-c(Tmean.x, Tmean.y))

#### TMIN ####

# Make matrix of only coordinates of PRISM data
prism_coords <- dplyr::select(tmin_historical, y, x)
colnames(prism_coords) <- c('lat', 'long')

# Find distance between each prism-site pair
dists <- fields::rdist(prism_coords, site_coords[,1:2])
# Find the closest PRISM estimate to each site
closest_points <- apply(dists, 2, which.min)

# Add uniqueID to tmin
tmin_historical2 <- tmin_historical |>
  dplyr::mutate(uniqueID = paste0(y,'_',x))

# Extract uniqueIDs of the closest points
uniqueIDs <- tmin_historical2$uniqueID[closest_points]

# Extract tmin columns that we want to pivot longer for pipeline below
tmin_cols <- colnames(tmin_historical)
tmin_cols <- tmin_cols[-((length(tmin_cols)-1):length(tmin_cols))]

# pivot longer and filter for specific sites
tmin_long <- tmin_historical |>
  dplyr::mutate(uniqueID = paste0(y,'_',x)) |>
  dplyr::rename(Longitude = x,
                Latitude = y) |>
  dplyr::filter(uniqueID %in% uniqueIDs) |>
  dplyr::select(-uniqueID) |>
  tidyr::pivot_longer(all_of(tmin_cols),
                      names_to = 'var', values_to = 'Tmin')

# repeat for recent
prism_coords <- dplyr::select(tmin_recent, y, x)
colnames(prism_coords) <- c('lat', 'long')

# Find distance between each prism-site pair
dists <- fields::rdist(prism_coords, site_coords[,1:2])
# Find the closest PRISM estimate to each site
closest_points <- apply(dists, 2, which.min)

# Add uniqueID to tmin
tmin_recent2 <- tmin_recent |>
  dplyr::mutate(uniqueID = paste0(y,'_',x))

# Extract uniqueIDs of the closest points
uniqueIDs <- tmin_recent2$uniqueID[closest_points]

# Extract tmin columns that we want to pivot longer for pipeline below
tmin_cols <- colnames(tmin_recent)
tmin_cols <- tmin_cols[-((length(tmin_cols)-1):length(tmin_cols))]

# pivot longer and filter for specific sites
tmin_recent_long <- tmin_recent |>
  dplyr::mutate(uniqueID = paste0(y,'_',x)) |>
  dplyr::rename(Longitude = x,
                Latitude = y) |>
  dplyr::filter(uniqueID %in% uniqueIDs) |>
  dplyr::select(-uniqueID) |>
  tidyr::pivot_longer(all_of(tmin_cols),
                      names_to = 'var', values_to = 'Tmin')

# Combine
tmin_long_total <- tmin_long |>
  dplyr::full_join(tmin_recent_long, by = c('Longitude', 'Latitude', 'var')) |>
  dplyr::mutate(Tmin2 = dplyr::coalesce(Tmin.x, Tmin.y)) |>
  dplyr::select(-c(Tmin.x, Tmin.y))

#### TMAX ####

# Make matrix of only coordinates of PRISM data
prism_coords <- dplyr::select(tmax_historical, y, x)
colnames(prism_coords) <- c('lat', 'long')

# Find distance between each prism-site pair
dists <- fields::rdist(prism_coords, site_coords[,1:2])
# Find the closest PRISM estimate to each site
closest_points <- apply(dists, 2, which.min)

# Add uniqueID to tmax
tmax_historical_2 <- tmax_historical |>
  dplyr::mutate(uniqueID = paste0(y,'_',x))

# Extract uniqueIDs of the closest points
uniqueIDs <- tmax_historical_2$uniqueID[closest_points]

# Extract tmax columns that we want to pivot longer for pipeline below
tmax_cols <- colnames(tmax_historical)
tmax_cols <- tmax_cols[-((length(tmax_cols)-1):length(tmax_cols))]

# pivot longer and filter for specific sites
tmax_long <- tmax_historical |>
  dplyr::mutate(uniqueID = paste0(y,'_',x)) |>
  dplyr::rename(Longitude = x,
                Latitude = y) |>
  dplyr::filter(uniqueID %in% uniqueIDs) |>
  dplyr::select(-uniqueID) |>
  tidyr::pivot_longer(all_of(tmax_cols),
                      names_to = 'var', values_to = 'Tmax')

# Verify that historical and recent have the same coordinates in the same order
all(tmax_historical$x == tmax_recent$x & tmax_historical$y == tmax_recent$y)

# Repeat for recent
tmax_recent_cols <- colnames(tmax_recent)
tmax_recent_cols <- tmax_recent_cols[-((length(tmax_recent_cols)-1):length(tmax_recent_cols))]

tmax_recent_long <- tmax_recent |>
  dplyr::mutate(uniqueID = paste0(y,'_',x)) |>
  dplyr::rename(Longitude = x,
                Latitude = y) |>
  dplyr::filter(uniqueID %in% uniqueIDs) |>
  dplyr::select(-uniqueID) |>
  tidyr::pivot_longer(all_of(tmax_recent_cols),
                      names_to = 'var', values_to = 'Tmax')

# Combine
tmax_long_total <- tmax_long |>
  dplyr::full_join(tmax_recent_long, by = c('Longitude', 'Latitude', 'var')) |>
  dplyr::mutate(Tmax2 = dplyr::coalesce(Tmax.x, Tmax.y)) |>
  dplyr::select(-c(Tmax.x, Tmax.y))

#### VPDMIN ####

# Make matrix of only coordinates of PRISM data
prism_coords <- dplyr::select(vpdmin_historical, y, x)
colnames(prism_coords) <- c('lat', 'long')

# Find distance between each prism-site pair
dists <- fields::rdist(prism_coords, site_coords[,1:2])
# Find the closest PRISM estimate to each site
closest_points <- apply(dists, 2, which.min)

# Add uniqueID to vpdmin
vpdmin_historical2 <- vpdmin_historical |>
  dplyr::mutate(uniqueID = paste0(y,'_',x))

# Extract uniqueIDs of the closest points
uniqueIDs <- vpdmin_historical2$uniqueID[closest_points]

# Extract vpdmin columns that we want to pivot longer for pipeline below
vpdmin_cols <- colnames(vpdmin_historical)
vpdmin_cols <- vpdmin_cols[-((length(vpdmin_cols)-1):length(vpdmin_cols))]

# pivot longer and filter for specific sites
vpdmin_long <- vpdmin_historical |>
  dplyr::mutate(uniqueID = paste0(y,'_',x)) |>
  dplyr::rename(Longitude = x,
                Latitude = y) |>
  dplyr::filter(uniqueID %in% uniqueIDs) |>
  dplyr::select(-uniqueID) |>
  tidyr::pivot_longer(all_of(vpdmin_cols),
                      names_to = 'var', values_to = 'Vpdmin')

# Repeat for recent
prism_coords <- dplyr::select(vpdmin_recent, y, x)
colnames(prism_coords) <- c('lat', 'long')

dists <- fields::rdist(prism_coords, site_coords[,1:2])
closest_points <- apply(dists, 2, which.min)

vpdmin_historical2 <- vpdmin_historical |>
  dplyr::mutate(uniqueID = paste0(y,'_',x))

uniqueIDs <- vpdmin_historical2$uniqueID[closest_points]

vpdmin_cols <- colnames(vpdmin_recent)
vpdmin_cols <- vpdmin_cols[-((length(vpdmin_cols)-1):length(vpdmin_cols))]

vpdmin_recent_long <- vpdmin_recent |>
  dplyr::mutate(uniqueID = paste0(y,'_',x)) |>
  dplyr::rename(Longitude = x,
                Latitude = y) |>
  dplyr::filter(uniqueID %in% uniqueIDs) |>
  dplyr::select(-uniqueID) |>
  tidyr::pivot_longer(all_of(vpdmin_cols),
                      names_to = 'var', values_to = 'Vpdmin')

# Combine
vpdmin_long_total <- vpdmin_long |>
  dplyr::full_join(vpdmin_recent_long, by = c('Longitude', 'Latitude', 'var')) |>
  dplyr::mutate(Vpdmin2 = dplyr::coalesce(Vpdmin.x, Vpdmin.y)) |>
  dplyr::select(-c(Vpdmin.x, Vpdmin.y))

#### VPDMAX ####

# Make matrix of only coordinates of PRISM data
prism_coords <- dplyr::select(vpdmax_historical, y, x)
colnames(prism_coords) <- c('lat', 'long')

# Find distance between each prism-site pair
dists <- fields::rdist(prism_coords, site_coords[,1:2])
# Find the closest PRISM estimate to each site
closest_points <- apply(dists, 2, which.min)

# Add uniqueID to vpdmax
vpdmax_historical2 <- vpdmax_historical |>
  dplyr::mutate(uniqueID = paste0(y,'_',x))

# Extract uniqueIDs of the  closest points
uniqueIDs <- vpdmax_historical2$uniqueID[closest_points]

# Extract vpdmax columns that we want to pivot longer for pipeline below
vpdmax_cols <- colnames(vpdmax_historical)
vpdmax_cols <- vpdmax_cols[-((length(vpdmax_cols)-1):length(vpdmax_cols))]

# pivot longer and filter for specific sites
vpdmax_long <- vpdmax_historical |>
  dplyr::mutate(uniqueID = paste0(y,'_',x)) |>
  dplyr::rename(Longitude = x,
                Latitude = y) |>
  dplyr::filter(uniqueID %in% uniqueIDs) |>
  dplyr::select(-uniqueID) |>
  tidyr::pivot_longer(all_of(vpdmax_cols),
                      names_to = 'var', values_to = 'Vpdmax')

# Verify that historical and recent ahve the same coordinates in the same order
all(vpdmax_historical$x == vpdmax_recent$x & vpdmax_historical$y & vpdmax_recent$y)

# Repeat for recent
vpdmax_recent_cols <- colnames(vpdmax_recent)
vpdmax_recent_cols <- vpdmax_recent_cols[-((length(vpdmax_recent_cols)-1):length(vpdmax_recent_cols))]

vpdmax_recent_long <- vpdmax_recent |>
  dplyr::mutate(uniqueID = paste0(y,'_',x)) |>
  dplyr::rename(Longitude = x,
                Latitude = y) |>
  dplyr::filter(uniqueID %in% uniqueIDs) |>
  dplyr::select(-uniqueID) |>
  tidyr::pivot_longer(all_of(vpdmax_recent_cols),
                      names_to = 'var', values_to = 'Vpdmax')

# Combine
vpdmax_long_total <- vpdmax_long |>
  dplyr::full_join(vpdmax_recent_long, by = c('Longitude', 'Latitude', 'var')) |>
  dplyr::mutate(Vpdmax2 = dplyr::coalesce(Vpdmax.x, Vpdmax.y)) |>
  dplyr::select(-c(Vpdmax.x, Vpdmax.y))

# Separate year and month from the "var" variable
# Done in a separate step to avoid maxing out memory
ppt_long <- ppt_long_total |>  
  dplyr::mutate(year = substr(var, 24, 27),
                month = substr(var, 28, 29))

tmean_long <- tmean_long_total |>
  dplyr::mutate(year = substr(var, 26, 29),
                month = substr(var, 30, 31))

tmin_long <- tmin_long_total |>
  dplyr::mutate(year = substr(var, 25, 28),
                month = substr(var, 29, 30))

tmax_long <- tmax_long_total |>
  dplyr::mutate(year = substr(var, 25, 28),
                month = substr(var, 29, 30))

vpdmin_long <- vpdmin_long_total |>
  dplyr::mutate(year = substr(var, 27, 30),
                month = substr(var, 31, 32))

vpdmax_long <- vpdmax_long_total |>
  dplyr::mutate(year = substr(var, 27, 30),
                month = substr(var, 31, 32))

# Add site names
ppt_long <- ppt_long |>
  dplyr::mutate(loc = dplyr::if_else(Longitude == unique(ppt_long$Longitude)[2], 'GOOSE', NA),
                loc = dplyr::if_else(Longitude == unique(ppt_long$Longitude)[1], 'ROOSTER', loc),
                loc = dplyr::if_else(Longitude == unique(ppt_long$Longitude)[3], 'NRP', loc),
                loc = dplyr::if_else(Longitude == unique(ppt_long$Longitude)[5], 'SYLVANIA', loc),
                loc = dplyr::if_else(Longitude == unique(ppt_long$Longitude)[4], 'HARVARD', loc)) |>
  dplyr::select(-c(var, Longitude, Latitude))

tmean_long <- tmean_long |>
  dplyr::mutate(loc = dplyr::if_else(Longitude == unique(tmean_long$Longitude)[2], 'GOOSE', NA),
                loc = dplyr::if_else(Longitude == unique(tmean_long$Longitude)[1], 'ROOSTER', loc),
                loc = dplyr::if_else(Longitude == unique(tmean_long$Longitude)[3], 'NRP', loc),
                loc = dplyr::if_else(Longitude == unique(tmean_long$Longitude)[5], 'SYLVANIA', loc),
                loc = dplyr::if_else(Longitude == unique(tmean_long$Longitude)[4], 'HARVARD', loc)) |>
  dplyr::select(-c(var, Longitude, Latitude))

tmin_long <- tmin_long |>
  dplyr::mutate(loc = dplyr::if_else(Longitude == unique(tmin_long$Longitude)[1], 'ROOSTER', NA),
                loc = dplyr::if_else(Longitude == unique(tmin_long$Longitude)[2], 'GOOSE', loc),
                loc = dplyr::if_else(Longitude == unique(tmin_long$Longitude)[3], 'NRP', loc),
                loc = dplyr::if_else(Longitude == unique(tmin_long$Longitude)[4], 'HARVARD', loc),
                loc = dplyr::if_else(Longitude == unique(tmin_long$Longitude)[5], 'SYLVANIA', loc),
                loc = dplyr::if_else(Longitude == unique(tmin_long$Longitude)[6], 'ROOSTER', loc),
                loc = dplyr::if_else(Longitude == unique(tmin_long$Longitude)[7], 'GOOSE', loc),
                loc = dplyr::if_else(Longitude == unique(tmin_long$Longitude)[8], 'NRP', loc),
                loc = dplyr::if_else(Longitude == unique(tmin_long$Longitude)[9], 'HARVARD', loc),
                loc = dplyr::if_else(Longitude == unique(tmin_long$Longitude)[10], 'SYLVANIA', loc)) |>
  dplyr::select(-c(var, Longitude, Latitude))

tmax_long <- tmax_long |>
  dplyr::mutate(loc = dplyr::if_else(Longitude == unique(tmax_long$Longitude)[1], 'ROOSTER', NA),
                loc = dplyr::if_else(Longitude == unique(tmax_long$Longitude)[2], 'GOOSE', loc),
                loc = dplyr::if_else(Longitude == unique(tmax_long$Longitude)[3], 'NRP', loc),
                loc = dplyr::if_else(Longitude == unique(tmax_long$Longitude)[4], 'HARVARD', loc),
                loc = dplyr::if_else(Longitude == unique(tmax_long$Longitude)[5], 'SYLVANIA', loc)) |>
  dplyr::select(-c(var, Longitude, Latitude))

vpdmin_long <- vpdmin_long |>
  dplyr::mutate(loc = dplyr::if_else(Longitude == unique(vpdmin_long$Longitude)[1], 'ROOSTER', NA),
                loc = dplyr::if_else(Longitude == unique(vpdmin_long$Longitude)[2], 'GOOSE', loc),
                loc = dplyr::if_else(Longitude == unique(vpdmin_long$Longitude)[3], 'NRP', loc),
                loc = dplyr::if_else(Longitude == unique(vpdmin_long$Longitude)[4], 'HARVARD', loc),
                loc = dplyr::if_else(Longitude == unique(vpdmin_long$Longitude)[5], 'SYLVANIA', loc)) |>
  dplyr::select(-c(var, Longitude, Latitude))

vpdmax_long <- vpdmax_long |>
  dplyr::mutate(loc = dplyr::if_else(Longitude == unique(vpdmax_long$Longitude)[1], 'ROOSTER', NA),
                loc = dplyr::if_else(Longitude == unique(vpdmax_long$Longitude)[2], 'GOOSE', loc),
                loc = dplyr::if_else(Longitude == unique(vpdmax_long$Longitude)[3], 'NRP', loc),
                loc = dplyr::if_else(Longitude == unique(vpdmax_long$Longitude)[4], 'HARVARD', loc),
                loc = dplyr::if_else(Longitude == unique(vpdmax_long$Longitude)[5], 'SYLVANIA', loc)) |>
  dplyr::select(-c(var, Longitude, Latitude))

# Combine
prism_long <- ppt_long |>
  dplyr::left_join(tmean_long, by = c('year', 'month', 'loc')) |>
  dplyr::left_join(tmin_long, by = c('year', 'month', 'loc')) |>
  dplyr::left_join(tmax_long, by = c('year', 'month', 'loc')) |>
  dplyr::left_join(vpdmin_long, by = c('year', 'month', 'loc')) |>
  dplyr::left_join(vpdmax_long, by = c('year', 'month', 'loc'))

# Save
save(prism_long, file = '/Volumes/FileBackup/PRISM_for_RW2BIO/prism_clim.RData')
save(prism_long, file = 'climate/prism_clim.RData')
