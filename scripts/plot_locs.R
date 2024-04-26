sites <- c('harvard', 'tension_zone',
           'bigwoods', 'sylvania',
           'goose', 'rooster',
           'glacial_lakes', 'gill_brook',
           'palmghatt', 'nrp',
           'pisgah_state', 'coral_woods',
           'pioneer_mothers', 'howland',
           'poconos', 'huron',
           'vermilion', 'ozark', 'umbs')
status <- c('model_done', 'model_ready',
            'find_data', 'model_done',
            'model_done', 'model_done',
            'model_ready', 'model_ready',
            'model_ready', 'model_done',
            'model_ready', 'cross_dating',
            'find_data', 'model_ready',
            'find_data', 'model_ready',
            'cross_dating_find_data', 'cross_dating_find_data',
            'cross_dating_find_data')

howland_coords <- c(45.20482, -68.74225)
howland_coords <- as.data.frame(t(howland_coords))
colnames(howland_coords) <- c('x', 'y')
howland_coords <- sf::st_as_sf(howland_coords, coords = c('x', 'y'),
                               crs = 'EPSG:4326')
howland_coords <- sf::st_transform(howland_coords, crs = 'EPSG:3175')
howland_coords <- sfheaders::sf_to_df(howland_coords, fill = TRUE)
howland_coords <- dplyr::select(howland_coords, -sfg_id, -point_id)

poconos_coords <- c(-74.253876, 41.703568)
poconos_coords <- as.data.frame(t(poconos_coords))
colnames(poconos_coords) <- c('x', 'y')
poconos_coords <- sf::st_as_sf(poconos_coords, coords = c('x', 'y'),
                               crs = 'EPSG:4326')
poconos_coords <- sf::st_transform(poconos_coords, crs = 'EPSG:3175')
poconos_coords <- sfheaders::sf_to_df(poconos_coords, fill = TRUE)
poconos_coords <- dplyr::select(poconos_coords, -sfg_id, -point_id)

albers_x <- c(1906179.06, 101608.8592,
              184444.1256, 530878.7584,
              1808606.875, 1706475.738,
              46160.77309, 1753145.499,
              1747435.08, 1879736.393,
              1879736.393, 560402.8825,
              719315.8531, 0,
              poconos_coords[,1], 643554.8669,
              0, 0, 0)
albers_y <- c(724378.1188, 1247775.292,
              1035553.883, 1092739.806,
              771912.3702, 778169.478,
              1069933.397, 883387.1025,
              611955.7402, 756110.9333,
              756110.9333, 642009.3414,
              225136.0492, 0,
              poconos_coords[,2], 1156359.891,
              0, 0, 0)

locs <- as.data.frame(cbind(sites, status, albers_x, albers_y))
locs$albers_x <- as.numeric(locs$albers_x)
locs$albers_y <- as.numeric(locs$albers_y)

status_order <- c('model_done', 'model_ready', 'cross_dating', 'find_data', 'cross_dating_find_data')

locs |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = factor(status, status_order))) +
  ggplot2::theme_minimal() +
  ggplot2::xlab('') +
  ggplot2::theme(axis.text = ggplot2::element_text(angle = 90))

states <- sf::st_as_sf(maps::map('state', region = c('minnesota', 'wisconsin',
                                                     'michigan', 'illinois', 'indiana',
                                                     'ohio', 'pennsylvania', 'new york',
                                                     'massachusetts', 'vermont', 'new hampshire'),
                                 fill = TRUE, plot = FALSE))
states <- sf::st_transform(states, crs = 'EPSG:3175')

locs |>
  dplyr::filter(albers_x != 0) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = albers_x, y = albers_y, color = status)) +
  ggplot2::geom_sf(data = states, color = 'black', fill = NA) +
  ggplot2::theme_void() +
  ggplot2::scale_color_manual(name = '', 
                              breaks = c('model_done', 'model_ready',
                                         'cross_dating', 'find_data'),
                              values = c('darkgreen', 'lightgreen',
                                         'orange', 'red'))

locs |>
  dplyr::filter(albers_x == 0) |>
  tibble::tibble()
