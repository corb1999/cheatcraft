
library(tidyverse)
library(sf)

# function to make a grid off a sf object 
fun_geotess_make <- function(dfsf, hex_size_miles = 50) {
  cell_size_miles <- hex_size_miles / 54.6
  interim <- st_make_grid(dfsf, square = FALSE, flat_topped = FALSE,
                          cellsize = cell_size_miles)
  cellid <- seq_along(interim)
  interim <- st_sf(interim)
  interim <- cbind(cellid, interim) %>% rename(geometry = interim)
  return_me <- interim
  return(return_me)}

# take a point sf object and aggregate into the tesselation
fun_geotess_agg <- function(dfsf, geotess) {
  aa <- st_join(x = dfsf, y = geotess, 
                join = st_intersects, left = TRUE)
  summer <- aa %>% as_tibble() %>% select(-geometry) %>% 
    group_by(cellid) %>% 
    # compute any special aggregation calcs ::::::::::::::::::
    summarise(recs = n())
  geotess_agg <- left_join(geotess, summer, by = 'cellid') %>% 
    # compute any post aggregation calcs/ratios ::::::::;:::::
    mutate(cellcount = 1) %>% 
    relocate(geometry, .after = last_col())
  return_me <- geotess_agg
  return(return_me)}

# tests ???????????????????????????????????????????????????????

# nc <- read_sf(system.file("shape/nc.shp", package="sf"))
# ncp <- st_centroid(nc)

(aa <- fun_geotess_make(nc, hex_size_miles = 50))

(bb <- fun_geotess_agg(ncp, aa))

ggplot() + 
  geom_sf(data = nc, fill = NA) + 
  geom_sf(data = bb, aes(fill = recs), alpha = 0.5) +
  geom_sf(data = ncp, color = 'black', alpha = 0.85) + 
  # geom_sf(data = fun_geotess_make(nc, hex_size_miles = 20),
  #         fill = NA) +
  scale_fill_distiller(na.value = NA, palette = 'Spectral') +
  theme_minimal()


