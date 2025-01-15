################################################################################
## [ PROJ ] < College Board Geomarket >
## [ FILE ] < leaflet_maps.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 1/13/2025
## [ DESC ] < append tract-level and EPS-level census data from 1980, 2000, and 2016-2020 ACS (for RQ1) >
################################################################################


### SETTINGS
#rm(list = ls()) # remove all objects
options(max.print=1000)
#options(width = 160)
# Set the scipen option to a high value to avoid scientific notation
options(scipen = 999)

################################# LIBRARIES
  library(tidyverse)
  library(readxl)
  library(lubridate)
  library(haven)
  library(labelled)
  library(tidycensus)
  # get census api key
  #http://api.census.gov/data/key_signup.html
  # set census api key, and install for future use
  #census_api_key('aff360d1fe8a919619776f48e975f03b8bb1379e', install = TRUE)
  Sys.getenv("CENSUS_API_KEY") # retreive API key


  library(sf)
  # Enable caching for tigris
  options(tigris_use_cache = TRUE)
  library(tigris)
  #library(stars)
  #library(spatstat)
  #library(rgeos)
  library(lwgeom) # library has checks/vixes for valid geometries
  library(leaflet)
  library(shiny)
  library(kableExtra)

################################# DIRECTORY PATHS

  getwd()
  
  data_dir <- file.path('.','data') # main data directory
  list.files(path = data_dir)
  
  d1980_data_dir <-   file.path('.',data_dir,'1980_decennial') # main data directory
  list.files(path = d1980_data_dir)
  
  d2000_data_dir <-   file.path('.',data_dir,'2000_decennial') # 
  list.files(path = d2000_data_dir)
  
  acs2020_data_dir <-   file.path('.',data_dir,'2020_acs') # 
  list.files(path = acs2020_data_dir)
  
  # can't save shape files in the repo cuz they too big
  shape_dir <-   file.path('.','..','cb_geomarket_shape') # main data directory
  list.files(path = shape_dir)
  
  # had to move analysis data files into shape folder because too big for git
  analysis_data_dir <- file.path('.',shape_dir,'analysis_data') # analysis data directory
  list.files(path = analysis_data_dir)
  
  eps_data_dir <- file.path(data_dir,'eps_market') # has eps geomarket data and spaical data files
  list.files(path = eps_data_dir)
  
  scripts_dir <- file.path('.','scripts') # 
  list.files(path = scripts_dir)
  
  tables_dir <- file.path('.','results','tables') # 
  list.files(path = tables_dir)

##### LOAD EPS-LEVEL DATA

  # load EPS data
  #1980
  load(file =  file.path(analysis_data_dir, 'd1980_stf1f3_anal_eps_sf.RData'))
  #2000
  load(file = file.path(analysis_data_dir, 'd2000_sf1a_anal_eps_sf.RData'))
  #2020
  load(file = file.path(analysis_data_dir, 'acs2020_ab_anal_eps_sf.RData'))
  
  # add a year variable to each
  d1980_stf1f3_anal_eps_sf$year <- 1980
  d2000_sf1a_anal_eps_sf$year <- 2000
  acs2020_ab_anal_eps_sf$year <- 2020
  
  # append datasets
  allyr_anal_eps_sf <- bind_rows(d1980_stf1f3_anal_eps_sf,d2000_sf1a_anal_eps_sf,acs2020_ab_anal_eps_sf)
  allyr_anal_eps_sf %>% as.data.frame() %>% count(year)
  allyr_anal_eps_sf %>% class()
  
  # dataset with eps names
  
  eps_names <- read_csv(file.path(eps_data_dir, 'eps_names.csv'), col_names = TRUE, skip=0)
  
  # create version of eps code variable that is consistent with how variable is stored in other data frames
  eps_names <- eps_names %>%
    mutate(
      eps = if_else(
        str_detect(eps_code, "0[1-9]$"),
        str_replace(eps_code, "0", " "),
        eps_code
      )
    )
  # eps_names %>% select(eps_code,eps,eps_name) %>% print(n=400)
  
  # merge EPS name to eps code and transform CRS
  allyr_anal_eps_sf <- allyr_anal_eps_sf %>% inner_join(
    y = eps_names %>% select(-eps_code),
    by = c('eps')
  ) %>% 
    rename(med_inc_house = med_inc_house_med) %>% 
    # Reproject the data to WGS84 (longitude/latitude)
    st_transform(crs = 4326)
  
  allyr_anal_eps_sf %>% glimpse()
  
  rm(d1980_stf1f3_anal_eps_sf,d2000_sf1a_anal_eps_sf,acs2020_ab_anal_eps_sf)

########### LOAD TRACT-LEVEL DATA

  # load tract-level sf object
  load(file = file.path(shape_dir,'analysis_data', 'd1980_stf1f3_anal_tract_sf.RData'))
  d1980_stf1f3_anal_tract_sf <- d1980_stf1f3_anal_tract_sf %>% mutate(year = 1980)
  
  load(file = file.path(shape_dir,'analysis_data', 'd2000_sf1a_anal_tract_sf.RData'))
  d2000_sf1a_anal_tract_sf <- d2000_sf1a_anal_tract_sf %>% mutate(year = 2000)
  
  load(file = file.path(shape_dir,'analysis_data', 'acs2020_ab_anal_tract_sf.RData'))
  acs2020_ab_anal_tract_sf <- acs2020_ab_anal_tract_sf %>% mutate(year = 2020)  
  
  st_geometry(acs2020_ab_anal_tract_sf) <- "geometry"  # Manually setting the active geometry column
  
  # problem: some observations for 2020 have geometry == GEOMETRYCOLLECTION, which gives sf/leaflet problems
  # solution: identify rows with GEOMETRYCOLLECTION geometries; extract polygons from those geometries; if no polygon exists, delete the row; 
  # then replace original GEOMETRYCOLLECTION rows with revised polygon/multipolygon rows
  
  # Step 1: Identify rows with GEOMETRYCOLLECTION geometries
  geometrycollection_rows <- acs2020_ab_anal_tract_sf %>% filter(st_geometry_type(geometry) == "GEOMETRYCOLLECTION")
  geometrycollection_rows$year %>% table() # 2,627 INSTANCES OF GEOMETRYCOLLECTION; all instances of GEOMETRYCOLLECTION ARE IN 2020
  
  # Step 2: Extract POLYGON or MULTIPOLYGON geometries, filtering out rows without POLYGONs
  geometrycollection_fixed <- geometrycollection_rows %>% 
    rowwise() %>% 
    filter(length(st_collection_extract(geometry, "POLYGON")) > 0) %>%  # Keep only those with POLYGONs
    mutate(geometry = st_collection_extract(geometry, "POLYGON")) %>%
    ungroup()
  
  # Step 3: Replace the original GEOMETRYCOLLECTION rows with the extracted geometries
  # Filter out the original GEOMETRYCOLLECTION rows and bind the fixed geometries
  acs2020_ab_anal_tract_sf_fixed <- acs2020_ab_anal_tract_sf %>%
    filter(st_geometry_type(geometry) != "GEOMETRYCOLLECTION") %>%
    bind_rows(geometrycollection_fixed)
  
  rm(acs2020_ab_anal_tract_sf,geometrycollection_fixed,geometrycollection_rows)
  
  acs2020_ab_anal_tract_sf_fixed <- st_make_valid(acs2020_ab_anal_tract_sf_fixed) # is the issue that geometries are not valid or that they are GEOMETRYCOLLECTION
  
  # Create a new column with the geometry type for each row [starts with 97,472 obs then goes to 97,403 obs]
  # filter out obs except for POLYGON AND MULTIPOLYGON
  acs2020_ab_anal_tract_sf_fixed <- acs2020_ab_anal_tract_sf_fixed %>%
    mutate(geometry_type = st_geometry_type(geometry)) %>% 
    filter(geometry_type %in% c('POLYGON','MULTIPOLYGON')) %>% select(-geometry_type)
  
  #acs2020_ab_anal_tract_sf_fixed %>% as.data.frame() %>%  count(geometry_type)
  
  ## append datasets
  allyr_anal_tract_sf <- bind_rows(d1980_stf1f3_anal_tract_sf, d2000_sf1a_anal_tract_sf, acs2020_ab_anal_tract_sf_fixed) %>%
    # Merge EPS name to eps code and transform CRS
    left_join(
      y = eps_names %>% select(-eps_code),
      by = c('eps')
    )  
  
  st_geometry(allyr_anal_tract_sf) <- "geometry"  # Manually setting the active geometry column
  
  # transform into sf object, using WGS84 CRS which leaflet mapping says is required
  allyr_anal_tract_sf <- st_transform(allyr_anal_tract_sf, crs = 4326)
  
  #allyr_anal_tract_sf <- st_make_valid(allyr_anal_tract_sf) # is the issue that geometries are not valid or that they are GEOMETRYCOLLECTION
  
  rm(d1980_stf1f3_anal_tract_sf,d2000_sf1a_anal_tract_sf,acs2020_ab_anal_tract_sf_fixed)
  
  