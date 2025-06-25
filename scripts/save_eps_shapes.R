library(tidyverse)
library(tidycensus)
library(sf)
library(leaflet)


data_dir <- file.path('.', 'data')
eps_data_dir <- file.path(data_dir, 'eps_market')
tracts_dir <- file.path(eps_data_dir, 'tracts')

scripts_dir <- file.path('.', 'scripts')

shape_dir <- file.path('..', 'cb_geomarket_shape')


source(file.path(scripts_dir, 'metro_eps_codes.R'))


# ------------
# Save shapes
# ------------

# tracts_2000.RData and zcta_2010.RData are too big to push to GitHub
# See: https://www.dropbox.com/scl/fo/6a48fl8isgd3wuhxjiiaa/AOtyIPHmvLuixYU5_VyFmHs?rlkey=ucr4rkbtmoi2pg4hnbkcgsa1r&st=5q4x20sp&dl=0

# Tract shapes (1980)
# d1980_tract_sf <- st_read(file.path(shape_dir, 'nhgis0001_shape', 'nhgis0001_shapefile_tl2000_us_tract_1980', 'US_tract_1980.shp')) %>%
#   rename_with(str_to_lower)
# 
# d1980_bna_sf <- st_read(file.path(shape_dir, 'nhgis0001_shape', 'nhgis0001_shapefile_tl2000_us_tract_1980', 'US_bna_1980.shp')) %>%
#   rename_with(str_to_lower)
# 
# tracts_1980 <- bind_rows(d1980_tract_sf, d1980_bna_sf) %>%
#   st_as_sf(sf_column_name = 'geometry') %>%
#   st_transform(crs = 4326)
# 
# save(tracts_1980, file = file.path(eps_data_dir, 'tracts_1980.RData'))

# Tract shapes (2000)
# tracts_2000 <- st_read(file.path(shape_dir, '2000_decennial', 'US_tract_2000.shp')) %>%
#   rename_with(str_to_lower) %>%
#   st_as_sf(sf_column_name = 'geometry') %>%
#   st_transform(crs = 4326)
# 
# save(tracts_2000, file = file.path(eps_data_dir, 'tracts_2000.RData'))

# Tract shapes (2020)
# tracts_2020 <- do.call(
#   bind_rows,
#   lapply(c('GA', 'CA', 'MA', 'IL', 'OH', 'TX', 'DC', 'MD', 'VA', 'MI', 'NY', 'FL', 'NJ', 'PA'), function(st) {
#     tracts(
#       state = st,
#       cb = TRUE, 
#       year = 2020
#     )
#   })
# )
# 
# save(tracts_2020, file = file.path(eps_data_dir, 'tracts_2020.RData'))

# Zip code shapes (2010)
# zcta_2010 <- zctas(
#   cb = TRUE, 
#   year = 2010
# )
#
# save(zcta_2010, file = file.path(eps_data_dir, 'zcta_2010.RData'))

# County shapes (2020)
# counties_2020 <- counties(
#   cb = TRUE, 
#   year = 2020
# )
# 
# save(counties_2020, file = file.path(eps_data_dir, 'counties_2020.RData'))

# County subdivision shapes (2020)
# county_subdivisions_2020 <- do.call(
#   bind_rows,
#   lapply(c('GA', 'CA', 'MA', 'IL', 'OH', 'TX', 'DC', 'MD', 'VA', 'MI', 'NY', 'FL', 'NJ', 'PA'), function(st) {
#     county_subdivisions(
#       state = st,
#       cb = TRUE, 
#       year = 2020
#     )
#   })
# )
# 
# save(county_subdivisions_2020, file = file.path(eps_data_dir, 'county_subdivisions_2020.RData'))

# Place shapes (2020)
# places_2020 <- do.call(
#   bind_rows,
#   lapply(c('GA', 'CA', 'MA', 'IL', 'OH', 'TX', 'DC', 'MD', 'VA', 'MI', 'NY', 'FL', 'NJ', 'PA'), function(st) {
#     places(
#       state = st,
#       cb = TRUE, 
#       year = 2020
#     )
#   })
# )
# 
# save(places_2020, file = file.path(eps_data_dir, 'places_2020.RData'))


# ------------
# Load shapes
# ------------

# Tract shapes (1980)
load(file.path(eps_data_dir, 'tracts_1980.RData'))

# Tract shapes (2000)
load(file.path(eps_data_dir, 'tracts_2000.RData'))

# Tract shapes (2020)
load(file.path(eps_data_dir, 'tracts_2020.RData'))


# ------------
# Save shapes
# ------------

get_shapes <- function(tracts_df, geo_col, yr, eps) {
  eps_files <- str_c(eps, '.txt')
  tracts_df %>% 
    mutate(eps = case_when(!!!rlang::parse_exprs(str_c(geo_col, ' %in% str_split(read.table("', file.path(tracts_dir, yr, eps_files), '"), ",")[[1]] ~ "', eps, '"')))) %>% 
    filter(!is.na(eps)) %>% 
    group_by(eps) %>% 
    summarise(geometry = st_union(st_make_valid(geometry))) %>%
    nngeo::st_remove_holes()
}

get_shape_df <- function(tracts_df, geo_col, yr) {
  eps_df <- do.call(
    bind_rows,
    lapply(names(all_codes), function(metro) {
      get_shapes(tracts_df, geo_col, yr, all_codes[[metro]]$eps)
    })
  )
  
  eps_df %>% 
    mutate(geometry = if_else(
      eps == 'CA11',
      st_difference((eps_df %>% filter(eps == 'CA11'))$geometry, (eps_df %>% filter(eps == 'CA10'))$geometry),
      geometry
    ))
}


# Save shapes (1980)
eps_1980 <- get_shape_df(tracts_1980, 'gisjoin2', '1980')

st_is_valid(eps_1980) %>% table()

save(eps_1980, file = file.path(eps_data_dir, 'eps_shapes_1980.RData'))

leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = eps_1980, label = ~eps, weight = 1, opacity = 1)


# Save shapes (2000)
eps_2000 <- get_shape_df(tracts_2000, 'gisjoin2', '2000')

st_is_valid(eps_2000) %>% table()

save(eps_2000, file = file.path(eps_data_dir, 'eps_shapes_2000.RData'))

leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = eps_2000, label = ~eps, weight = 1, opacity = 1)


# Save shapes (2020)
eps_2020 <- get_shape_df(tracts_2020, 'GEOID', '2020')

st_is_valid(eps_2020) %>% table()

save(eps_2020, file = file.path(eps_data_dir, 'eps_shapes_2020.RData'))

leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = eps_2020, label = ~eps, weight = 1, opacity = 1)
