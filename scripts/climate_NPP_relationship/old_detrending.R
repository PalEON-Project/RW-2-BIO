# Leftovers from first attempts at detrending
# Loop over sites
for(i in 1:4){
  tree <- unique(total_agbi$tree[which(total_agbi$site == site[i])])
  site_name <- site[i]
  for(j in tree){
    sub <- dplyr::filter(total_agbi, site == site_name &
                           tree == j)
    m <- lm(mean ~ year + I(year^2), data = sub)
    detr <- as.vector(zoo::zoo(resid(m), sub$year))
    sub <- as.matrix(sub)
    sub <- cbind(sub, detr)
    if(i == 1 & j == 1){
      new_total_agbi = sub
    }else{
      new_total_agbi = base::rbind(new_total_agbi, sub)
    }
    print(j)
  }
  print(paste0('-----------------',site_name,'----------------------'))
}

new_total_agbi <- as.data.frame(new_total_agbi)
new_total_agbi <- new_total_agbi |>
  dplyr::mutate(tree = as.factor(tree),
                year = as.numeric(year),
                plot = as.factor(plot),
                taxon = as.factor(taxon),
                site = as.factor(site),
                mean = as.numeric(mean),
                detr = as.numeric(detr))

box_test <- new_total_agbi |>
  dplyr::group_by(tree, plot, taxon, site) |>
  dplyr::summarize(box_test = Box.test(detr) |> broom::tidy())

samp <- dplyr::filter(total_agbi, tree == 2, site == 'GOOSE')

samp |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = mean)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = 'lm')

test <- pracma::detrend(samp$mean, tt = 'linear', bp = 1)

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = samp$year, y = test)) +
  ggplot2::geom_smooth(ggplot2::aes(x = samp$year, y = test), method = 'lm')

Box.test(test)
# Add in AGB?# Loop over sites
for(i in 1:4){
  tree <- unique(total_agbi$tree[which(total_agbi$site == site[i])])
  site_name <- site[i]
  for(j in tree){
    sub <- dplyr::filter(total_agbi, site == site_name &
                           tree == j)
    m <- lm(mean ~ year + I(year^2), data = sub)
    detr <- as.vector(zoo::zoo(resid(m), sub$year))
    sub <- as.matrix(sub)
    sub <- cbind(sub, detr)
    if(i == 1 & j == 1){
      new_total_agbi = sub
    }else{
      new_total_agbi = base::rbind(new_total_agbi, sub)
    }
    print(j)
  }
  print(paste0('-----------------',site_name,'----------------------'))
}

new_total_agbi <- as.data.frame(new_total_agbi)
new_total_agbi <- new_total_agbi |>
  dplyr::mutate(tree = as.factor(tree),
                year = as.numeric(year),
                plot = as.factor(plot),
                taxon = as.factor(taxon),
                site = as.factor(site),
                mean = as.numeric(mean),
                detr = as.numeric(detr))

box_test <- new_total_agbi |>
  dplyr::group_by(tree, plot, taxon, site) |>
  dplyr::summarize(box_test = Box.test(detr) |> broom::tidy())

samp <- dplyr::filter(total_agbi, tree == 2, site == 'GOOSE')

samp |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = mean)) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = 'lm')

test <- pracma::detrend(samp$mean, tt = 'linear', bp = 1)

ggplot2::ggplot() +
  ggplot2::geom_point(ggplot2::aes(x = samp$year, y = test)) +
  ggplot2::geom_smooth(ggplot2::aes(x = samp$year, y = test), method = 'lm')

Box.test(test)
# Add in AGB?