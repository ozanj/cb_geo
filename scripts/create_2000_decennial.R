################################################################################
## [ PROJ ] < College Board Geomarket >
## [ FILE ] < create_2000_decennial.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 8/2/2024
## [ DESC ] < Get 2000 decennial census data, shape files, and (hopefully) merge w/ eps geomarkets >
################################################################################

### SETTINGS
rm(list = ls())
options(max.print=1000)
#options(width = 160)
#d1980_stf1f3_anal_eps %>% glimpse()

### LIBRARIES
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

### DIRECTORY PATHS

data_dir <- file.path('.','data') # main data directory
list.files(path = data_dir)

d2000_data_dir <-   file.path('.',data_dir,'2000_decennial') # main data directory
list.files(path = d2000_data_dir)

# can't save shape files in the repo cuz they too big
shape_dir <-   file.path('.','..','cb_geomarket_shape') # main data directory
list.files(path = shape_dir)

eps_data_dir <- file.path(data_dir,'eps_market') # has eps geomarket data and spaical data files
list.files(path = eps_data_dir)

scripts_dir <- file.path('.','scripts') # 
list.files(path = scripts_dir)

###### LOAD EPS SHAPE FILE DATA

  load(file.path(eps_data_dir, 'eps_shapes.RData'))

######## READ IN SHAPE FILE FOR 2000 CENSUS TRACTS

list.files(path = file.path(shape_dir,'2000_decennial'))

# Read the tract shapefile using sf
d2000_tract_sf <- st_read(file.path(shape_dir,'2000_decennial','US_tract_2000.shp'))  


names(d2000_tract_sf) <- d2000_tract_sf %>% names %>% tolower()

d2000_tract_sf %>% glimpse()
d2000_tract_sf %>% class() # sf, data frame
d2000_tract_sf %>% st_crs

####### READ IN DATA

# SF1A POPULATION BY RACE AND ETHNICITY

# Read the first row to get the variable names
variable_names <- read_csv(file.path(d2000_data_dir, 'nhgis0009_ds146_2000_tract.csv'), col_names = FALSE, n_max = 1) %>% as.character()

# Read the second row to get the variable labels
variable_labels <- read_csv(file.path(d2000_data_dir, 'nhgis0009_ds146_2000_tract.csv'), col_names = FALSE, skip = 1, n_max = 1) %>% as.character()
variable_labels
# Read the data starting from the third row using the first row as column names

d2000_sf1a_tract <- read_csv(file.path(d2000_data_dir, 'nhgis0009_ds146_2000_tract.csv'), col_names = variable_names, skip = 2)

# Add variable labels using the var_label function from labelled package
var_label(d2000_sf1a_tract) <- variable_labels
rm(variable_labels,variable_names)

# replace upper case w/ lower case variable names
names(d2000_sf1a_tract) <- d2000_sf1a_tract %>% names %>% tolower()

# keep only desired variables
# variables you need are in following tables:
  # fp5 = total population
  # fms = population by hispanic or Latino and not hispanic or latino by race
  # fmt = races tallied
  # fmw = races tallied by hispanic or latino and not hispanic or latino by races

d2000_sf1a_tract <- d2000_sf1a_tract %>% select(gisjoin,state,statea,county,countya,tracta,starts_with('fl5'),starts_with('fms'),starts_with('fmt'),starts_with('fmt'),starts_with('fmw'))

d2000_sf1a_tract %>% glimpse()
d2000_sf1a_tract %>% var_label()

# SF3A DATA ON EDUCATIONAL ATTAINMENT

# Read the first row to get the variable names
variable_names <- read_csv(file.path(d2000_data_dir, 'nhgis0010_ds151_2000_tract.csv'), col_names = FALSE, n_max = 1) %>% as.character()

# Read the second row to get the variable labels
variable_labels <- read_csv(file.path(d2000_data_dir, 'nhgis0010_ds151_2000_tract.csv'), col_names = FALSE, skip = 1, n_max = 1) %>% as.character()
variable_labels
# Read the data starting from the third row using the first row as column names

d2000_sf3a_edu_tract <- read_csv(file.path(d2000_data_dir, 'nhgis0010_ds151_2000_tract.csv'), col_names = variable_names, skip = 2)

# Add variable labels using the var_label function from labelled package
var_label(d2000_sf3a_edu_tract) <- variable_labels
rm(variable_labels,variable_names)

d2000_sf3a_edu_tract %>% glimpse()

d2000_sf3a_edu_tract %>% var_label()

# replace upper case w/ lower case variable names
names(d2000_sf3a_edu_tract) <- d2000_sf3a_edu_tract %>% names %>% tolower()

# SF3A DATA ON HOUSEHOLD INCOME AND HOUSEHOLD POVERTY STATUS

# Read the first row to get the variable names
variable_names <- read_csv(file.path(d2000_data_dir, 'nhgis0012_ds151_2000_tract.csv'), col_names = FALSE, n_max = 1) %>% as.character()

# Read the second row to get the variable labels
variable_labels <- read_csv(file.path(d2000_data_dir, 'nhgis0012_ds151_2000_tract.csv'), col_names = FALSE, skip = 1, n_max = 1) %>% as.character()
variable_labels
# Read the data starting from the third row using the first row as column names

d2000_sf3a_inc_tract <- read_csv(file.path(d2000_data_dir, 'nhgis0012_ds151_2000_tract.csv'), col_names = variable_names, skip = 2)

# Add variable labels using the var_label function from labelled package
var_label(d2000_sf3a_inc_tract) <- variable_labels
rm(variable_labels,variable_names)

# replace upper case w/ lower case variable names
names(d2000_sf3a_inc_tract) <- d2000_sf3a_inc_tract %>% names %>% tolower()

d2000_sf3a_inc_tract %>% var_label()

d2000_sf3a_inc_tract %>% glimpse()

d2000_sf3a_inc_tract %>% select(gisjoin,starts_with('ghu'),starts_with('gmx'),starts_with('gmy'),starts_with('gmz'),starts_with('gok')) %>% glimpse()


# [temporarily] merge dataset of population variables to dataset of education variables and dataset of income/poverty variables

d2000_sf1a_tract <- d2000_sf1a_tract %>% 
  # merge educational attainent variables from sf3a
  inner_join(
    y= d2000_sf3a_edu_tract %>% select(gisjoin,starts_with('gkt'),starts_with('grw'),starts_with('grz'),starts_with('gr2')), 
    by = c('gisjoin')
  ) %>% 
  # merge household income and poverty variables from sf3a  
  inner_join(
    y = d2000_sf3a_inc_tract %>% select(gisjoin,starts_with('ghu'),starts_with('gmx'),starts_with('gmy'),starts_with('gmz'),starts_with('gok')),
    by = c('gisjoin')
  )

  d2000_sf1a_tract %>% glimpse()

###### MERGE 2000 CENSUS DATA TO SF DATASET W/ 2000 CENSUS TRACT POLYGONS

# join between sf1a_2000_tract and d2000_tract_sf
d2000_sf1a_tract %>% glimpse()

d2000_sf1a_tract_sf <- d2000_sf1a_tract %>% 
  left_join(y = d2000_tract_sf%>% select(gisjoin, geometry), by = c('gisjoin')
  ) 

d2000_sf1a_tract_sf %>% count
d2000_sf1a_tract_sf %>% glimpse()
d2000_sf1a_tract_sf %>% st_crs

# assert data structure
d2000_sf1a_tract_sf %>% select(gisjoin) %>% as.data.frame() %>% group_by(gisjoin) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # uniquely identifies

# Convert to sf object
  d2000_sf1a_tract_sf %>% class()
  d2000_sf1a_tract_sf <- st_as_sf(d2000_sf1a_tract_sf, sf_column_name = "geometry")
  d2000_sf1a_tract_sf %>% class()
  d2000_sf1a_tract_sf %>% st_crs()

  # assert/Assign the CRS
  st_crs(d2000_sf1a_tract_sf) == st_crs(d2000_tract_sf) # crs is the same
  
####################
####################
  # ASSIGN EPS CODE TO EACH CENSUS TRACT
  
  # inputs you have 
    # have sf object with tract-level data containing the geometry for each tract and variables with counts of population in each tract
    # have sf object of eps-level data with one obs per EPS code and geometry for each EPS
  
  # desired output
    # or sf object that has one obs per tract-eps and for each tract we know the eps code of that tract
  
    # st_crs function retreives coordinate reference system for an object
    st_crs(eps_geometry_zcta) == st_crs(d2000_sf1a_tract_sf) # false 
    d2000_sf1a_tract_sf %>% st_crs()
    eps_geometry_zcta %>% st_crs()
    
    # transform so that they have the same CRS system
    # what chat gpt says: The st_transform() function is used to reproject the coordinates of your
    # spatial object from one CRS to another, ensuring that the geometries are correctly 
    # transformed to the new coordinate system. In contrast, st_crs() is used to either set
    #or retrieve the CRS of an object without transforming the coordinates.
    
    # Transform eps_geometry_zcta to match the CRS of d2000_sf1a_tract_sf
    eps_geometry_zcta <- st_transform(eps_geometry_zcta, st_crs(d2000_sf1a_tract_sf))
    st_crs(eps_geometry_zcta) == st_crs(d2000_sf1a_tract_sf)

# PERFORM PARTIAL SPATIAL JOIN. here are conceptual steps:
  #1 = make sure CRS is the same
  #2 = perform a spatial intersection
    #Use st_intersection() to find the intersection of the census tracts and geomarkets. This function will create a new sf object where each feature represents the area of overlap between a census tract and a geomarket.
  #3 = calculate the area of each intersection
    #After obtaining the intersection, calculate the area of each intersected polygon (which represents the part of the census tract within a geomarket). Then calculate the area of each original census tract.
  #4 calculate the area proportion
    #Calculate the proportion of the census tract that is within each geomarket by dividing the area of the intersection by the total area of the original census tract.
  #5 = adjust tract data by proportion
    #Adjust any tract-level variables (e.g., population counts) by multiplying them by the proportion of the tract within each geomarket.
  #6 = Aggregate Data by Geomarket
    #Finally, aggregate the proportionally adjusted data by geomarket to get the total contributions from all overlapping tracts.

# calculate the original area of the census tracts; on the object that has one obs per geoid
    d2000_sf1a_tract_sf <- d2000_sf1a_tract_sf %>% mutate(area_tract = st_area(.))

####
# 2 = Perform spacial intersection

  # first simplify the geometry
    #d2000_sf1a_tract_sf <- st_simplify(d2000_sf1a_tract_sf, dTolerance = 0.001) # chatGPT said this was a "pretty cautious" value for dTolerance for census tract geometries
      # geometryies are vertices (points) arranged in order, connected by lines. st_simplify() removes points that are "close" together, with "close" defined by the value you assign to the dTolerance argument
      # Understanding dTolerance in EPSG:4269; Since your CRS is in degrees, the dTolerance value you choose will represent a distance in degrees. Here's what a dTolerance of 0.001 would correspond to:
        # 0.001 degrees of latitude would be about 0.001 × 69 =0.069
        # 0.001×69=0.069 miles, or approximately 364 feet.
  # calculate geometries of intersections
    d2000_tract_eps_intersect <- st_intersection(d2000_sf1a_tract_sf, eps_geometry_zcta)
    
  # check if geometry is valid
    valid_geom <- st_is_valid(d2000_tract_eps_intersect) # check to see whether geometries are "valid"; valid geometries needed for st_intersect
    summary(valid_geom) # ALL TRUE
    rm(valid_geom)
    
  # make geometries valid
    #d2000_tract_eps_intersect <- st_make_valid(d2000_tract_eps_intersect)    
   
#3 = calculate the area of each intersection
  #After obtaining the intersection, calculate the area of each intersected polygon (which represents the part of the census tract within a geomarket). Then calculate the area of each original census tract.

  # calculate area of each intersected polygon  
    d2000_tract_eps_intersect$area_intersection <- st_area(d2000_tract_eps_intersect) # does same as this:  st_area(acs2020_tract_eps_intersect$geometry)
    
#4 calculate the area proportion
  #Calculate the proportion of the census tract that is within each geomarket by dividing the area of the intersection by the total area of the original census tract.
  # steps
  
  # calculate proportion:
    d2000_tract_eps_intersect <- d2000_tract_eps_intersect %>%
      mutate(proportion = as.numeric(area_intersection / area_tract))

    # Perform the spatial join
    #d2000_sf1a_tract_sf_eps <- st_join(d2000_sf1a_tract_sf, eps_geometry_zcta, join = st_intersects)  # previous spatial join
    
    d2000_tract_eps_intersect %>% glimpse()
    d2000_tract_eps_intersect %>% class()    
    
    #investigate data structure
    # gisjoin uniquely identifies obs in the input dataset
    d2000_sf1a_tract_sf %>% as.data.frame() %>% 
      select(gisjoin) %>% group_by(gisjoin) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # one observation per gisjoin value
    
    # gisjoin does not uniquely identify obs in the dataset created by st_join with join = st_intersect    
    d2000_tract_eps_intersect %>% as.data.frame() %>% 
      select(gisjoin) %>% group_by(gisjoin) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) #
    
    # gisjoin,eps uniquely identifies obs in new dataset
    d2000_tract_eps_intersect %>% as.data.frame() %>% 
      select(gisjoin,eps) %>% group_by(gisjoin,eps) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # one observation per zip code  
    
    # transform crs to one that is good for mapping contiguous US
    # what chat gpt says about NAD83 / Conus Albers
    #NAD83 / Conus Albers CRS (Coordinate Reference System):
    #CRS Definition: A CRS is a coordinate-based system used to locate geographical entities on a map. It defines how the two-dimensional, projected map in your GIS relates to real places on the earth.
    #NAD83: North American Datum 1983, a geodetic datum providing a frame of reference for measuring locations on the Earth's surface in North America.
    #Conus Albers: A conic projection that is particularly well-suited for maps of the contiguous United States.
    # Why NAD83 / Conus Albers is a Good Choice for Mapping Metropolitan Areas:
    # Minimal Distortion: The Albers Equal Area Conic projection minimizes distortion in area, making it suitable for statistical and analytical purposes, especially for larger regions.
    # Uniform Coverage: It provides a consistent projection for the entire contiguous US, ensuring that all metropolitan areas are displayed with the same spatial reference.
    # Balance of Accuracy: The projection is designed to maintain a balance between shape and area accuracy, which is important for visualizing and analyzing spatial data across different regions.      
    
    # Reproject sf object to NAD83 / Conus Albers
    d2000_tract_eps_intersect <- st_transform(x = d2000_tract_eps_intersect, crs = 5070)
    
    # Check the new CRS
    st_crs(d2000_tract_eps_intersect)
 
############## 
# USING GROUP-BY AND SUMMARIZE, CREATE OBJECTS AT THE EPS LEVEL THAT HAVE POPULATION VARIABLES SUMMARIZED 
  # MERGE EPS-LEVEL CHARACTERISTIC DATA TO DATAFRAME THAT HAS EPS GEOMETRY; THEN DO SOME ANALYSES:           
    
    
    d2000_tract_eps_intersect %>% glimpse()
    
  d2000_sf3a_inc_tract %>% var_label()
    
  # create tract-level analysis vars
  d2000_sf1a_anal_tract <- d2000_tract_eps_intersect %>% as.data.frame() %>% 
    # RENAME AND CREATE VARS OF POPULATION BY RACE, ETHNICITY, AND AGE
    rename(
      tot_all = fl5001,
      nhisp_white = fms001,
      nhisp_black = fms002,
      nhisp_native = fms003,
      nhisp_asian = fms004,
      nhisp_nhpi = fms005,
      nhisp_other = fms006,
      nhisp_multi = fms007,
      
      hisp_white = fms008,
      hisp_black = fms009,
      hisp_native = fms010,
      hisp_asian = fms011,
      hisp_nhpi = fms012,
      hisp_other = fms013,
      hisp_multi = fms014,
      tot_all_tally = fmt001,
      
      nhisp_white_tally = fmw001,
      nhisp_black_tally = fmw002,
      nhisp_native_tally = fmw003,
      nhisp_asian_tally = fmw004,
      nhisp_nhpi_tally = fmw005,
      nhisp_other_tally = fmw006,
      
      hisp_white_tally = fmw007,
      hisp_black_tally = fmw008,
      hisp_native_tally = fmw009,
      hisp_asian_tally = fmw010,
      hisp_nhpi_tally = fmw011,
      hisp_other_tally = fmw012,      
    ) %>% 
    # create race/ethnicity variables
    mutate(
      
      # api vars
      nhisp_api = rowSums(select(., nhisp_asian, nhisp_nhpi), na.rm = TRUE),
      hisp_api =  rowSums(select(., hisp_asian, hisp_nhpi), na.rm = TRUE),
      nhisp_api_tally = rowSums(select(., nhisp_asian_tally, nhisp_nhpi_tally), na.rm = TRUE),
      hisp_api_tally =  rowSums(select(., hisp_asian_tally, hisp_nhpi_tally), na.rm = TRUE),

      # all hispanic
      hisp_all = rowSums(select(., hisp_white,hisp_black, hisp_native, hisp_asian, hisp_nhpi,hisp_other,hisp_multi), na.rm = TRUE),
      hisp_all_tally = rowSums(select(., hisp_white_tally,hisp_black_tally, hisp_native_tally, hisp_asian_tally, hisp_nhpi_tally,hisp_other_tally), na.rm = TRUE),
    ) %>% 
    # education variables
    mutate(
      
      # all racial groups
      edu_lths_all = rowSums(select(., gkt001, gkt017, gkt002, gkt018, gkt003, gkt019, gkt004, gkt020, gkt005, gkt021, gkt006, gkt022, gkt007, gkt023, gkt008, gkt024), na.rm = TRUE),
      edu_hs_all = rowSums(select(., gkt009, gkt025), na.rm = TRUE),
      edu_ltassoc_all = rowSums(select(., gkt010, gkt026, gkt011, gkt027), na.rm = TRUE),
      edu_assoc_all = rowSums(select(., gkt012, gkt028), na.rm = TRUE),
      edu_ltba_all = edu_ltassoc_all + edu_assoc_all,
      edu_ba_all = rowSums(select(., gkt013, gkt029), na.rm = TRUE),
      edu_ma_all = rowSums(select(., gkt014, gkt030), na.rm = TRUE),
      edu_fprof_all = rowSums(select(., gkt015, gkt031), na.rm = TRUE),
      edu_doct_all = rowSums(select(., gkt016, gkt032), na.rm = TRUE),
      edu_baplus_all = rowSums(select(., gkt013, gkt029, gkt014, gkt030, gkt015, gkt031, gkt016, gkt032), na.rm = TRUE),
      edu_tot_all = rowSums(select(., starts_with("gkt")), na.rm = TRUE),
      
      # White alone
      edu_lths_white = rowSums(select(., grw001, grw008, grw002, grw009), na.rm = TRUE),
      edu_hs_white = rowSums(select(., grw003, grw010), na.rm = TRUE),
      edu_ltassoc_white = rowSums(select(., grw004, grw011), na.rm = TRUE),
      edu_assoc_white = rowSums(select(., grw005, grw012), na.rm = TRUE),
      edu_ltba_white = rowSums(select(., grw004, grw011, grw005, grw012), na.rm = TRUE),
      edu_ba_white = rowSums(select(., grw006, grw013), na.rm = TRUE),
      edu_grad_white = rowSums(select(., grw007, grw014), na.rm = TRUE),
      edu_baplus_white = rowSums(select(., grw006, grw013, grw007, grw014), na.rm = TRUE),
      edu_tot_white = rowSums(select(., grw001:grw014), na.rm = TRUE),
      
      # Black or African American alone
      edu_lths_black = rowSums(select(., grw015, grw022, grw016, grw023), na.rm = TRUE),
      edu_hs_black = rowSums(select(., grw017, grw024), na.rm = TRUE),
      edu_ltassoc_black = rowSums(select(., grw018, grw025), na.rm = TRUE),
      edu_assoc_black = rowSums(select(., grw019, grw026), na.rm = TRUE),
      edu_ltba_black = rowSums(select(., grw018, grw025, grw019, grw026), na.rm = TRUE),
      edu_ba_black = rowSums(select(., grw020, grw027), na.rm = TRUE),
      edu_grad_black = rowSums(select(., grw021, grw028), na.rm = TRUE),
      edu_baplus_black = rowSums(select(., grw020, grw027, grw021, grw028), na.rm = TRUE),
      edu_tot_black = rowSums(select(., grw015:grw028), na.rm = TRUE),
      
      # American Indian and Alaska Native alone
      edu_lths_native = rowSums(select(., grw029, grw036, grw030, grw037), na.rm = TRUE),
      edu_hs_native = rowSums(select(., grw031, grw038), na.rm = TRUE),
      edu_ltassoc_native = rowSums(select(., grw032, grw039), na.rm = TRUE),
      edu_assoc_native = rowSums(select(., grw033, grw040), na.rm = TRUE),
      edu_ltba_native = rowSums(select(., grw032, grw039, grw033, grw040), na.rm = TRUE),
      edu_ba_native = rowSums(select(., grw034, grw041), na.rm = TRUE),
      edu_grad_native = rowSums(select(., grw035, grw042), na.rm = TRUE),
      edu_baplus_native = rowSums(select(., grw034, grw041, grw035, grw042), na.rm = TRUE),
      edu_tot_native = rowSums(select(., grw029:grw042), na.rm = TRUE),
      
      # Asian alone
      edu_lths_asian = rowSums(select(., grw043, grw050, grw044, grw051), na.rm = TRUE),
      edu_hs_asian = rowSums(select(., grw045, grw052), na.rm = TRUE),
      edu_ltassoc_asian = rowSums(select(., grw046, grw053), na.rm = TRUE),
      edu_assoc_asian = rowSums(select(., grw047, grw054), na.rm = TRUE),
      edu_ltba_asian = rowSums(select(., grw046, grw053, grw047, grw054), na.rm = TRUE),
      edu_ba_asian = rowSums(select(., grw048, grw055), na.rm = TRUE),
      edu_grad_asian = rowSums(select(., grw049, grw056), na.rm = TRUE),
      edu_baplus_asian = rowSums(select(., grw048, grw055, grw049, grw056), na.rm = TRUE),
      edu_tot_asian = rowSums(select(., grw043:grw056), na.rm = TRUE),
      
      # Native Hawaiian and Other Pacific Islander alone
      edu_lths_nhpi = rowSums(select(., grw057, grw064, grw058, grw065), na.rm = TRUE),
      edu_hs_nhpi = rowSums(select(., grw059, grw066), na.rm = TRUE),
      edu_ltassoc_nhpi = rowSums(select(., grw060, grw067), na.rm = TRUE),
      edu_assoc_nhpi = rowSums(select(., grw061, grw068), na.rm = TRUE),
      edu_ltba_nhpi = rowSums(select(., grw060, grw067, grw061, grw068), na.rm = TRUE),
      edu_ba_nhpi = rowSums(select(., grw062, grw069), na.rm = TRUE),
      edu_grad_nhpi = rowSums(select(., grw063, grw070), na.rm = TRUE),
      edu_baplus_nhpi = rowSums(select(., grw062, grw069, grw063, grw070), na.rm = TRUE),
      edu_tot_nhpi = rowSums(select(., grw057:grw070), na.rm = TRUE),
      
      # API (Asian and Native Hawaiian and Other Pacific Islander combined)
      edu_lths_api = rowSums(select(., grw043, grw050, grw044, grw051, grw057, grw064, grw058, grw065), na.rm = TRUE),
      edu_hs_api = rowSums(select(., grw045, grw052, grw059, grw066), na.rm = TRUE),
      edu_ltassoc_api = rowSums(select(., grw046, grw053, grw060, grw067), na.rm = TRUE),
      edu_assoc_api = rowSums(select(., grw047, grw054, grw061, grw068), na.rm = TRUE),
      edu_ltba_api = rowSums(select(., grw046, grw053, grw047, grw054, grw060, grw067, grw061, grw068), na.rm = TRUE),
      edu_ba_api = rowSums(select(., grw048, grw055, grw062, grw069), na.rm = TRUE),
      edu_grad_api = rowSums(select(., grw049, grw056, grw063, grw070), na.rm = TRUE),
      edu_baplus_api = rowSums(select(., grw048, grw055, grw049, grw056, grw062, grw069, grw063, grw070), na.rm = TRUE),
      edu_tot_api = rowSums(select(., grw043:grw056, grw057:grw070), na.rm = TRUE),
      
      # hispanic
      edu_lths_hisp = rowSums(select(., grz001, grz008, grz002, grz009), na.rm = TRUE),
      edu_hs_hisp = rowSums(select(., grz003, grz010), na.rm = TRUE),
      edu_ltassoc_hisp = rowSums(select(., grz004, grz011), na.rm = TRUE),
      edu_assoc_hisp = rowSums(select(., grz005, grz012), na.rm = TRUE),
      edu_ltba_hisp = rowSums(select(., grz004, grz011, grz005, grz012), na.rm = TRUE),
      edu_ba_hisp = rowSums(select(., grz006, grz013), na.rm = TRUE),
      edu_grad_hisp = rowSums(select(., grz007, grz014), na.rm = TRUE),
      edu_baplus_hisp = rowSums(select(., grz006, grz013, grz007, grz014), na.rm = TRUE),
      edu_tot_hisp = rowSums(select(., grz001:grz014), na.rm = TRUE),
      
      #non-hispanic white
      edu_lths_nhisp_white = rowSums(select(., gr2001, gr2008, gr2002, gr2009), na.rm = TRUE),
      edu_hs_nhisp_white = rowSums(select(., gr2003, gr2010), na.rm = TRUE),
      edu_ltassoc_nhisp_white = rowSums(select(., gr2004, gr2011), na.rm = TRUE),
      edu_assoc_nhisp_white = rowSums(select(., gr2005, gr2012), na.rm = TRUE),
      edu_ltba_nhisp_white = rowSums(select(., gr2004, gr2011, gr2005, gr2012), na.rm = TRUE),
      edu_ba_nhisp_white = rowSums(select(., gr2006, gr2013), na.rm = TRUE),
      edu_grad_nhisp_white = rowSums(select(., gr2007, gr2014), na.rm = TRUE),
      edu_baplus_nhisp_white = rowSums(select(., gr2006, gr2013, gr2007, gr2014), na.rm = TRUE),
      edu_tot_nhisp_white = rowSums(select(., gr2001:gr2014), na.rm = TRUE),   
      
    ) %>% select(-c(starts_with("gkt"),starts_with('grw'),starts_with('grz'),starts_with('gr2'))) %>% 
    # create household income and poverty variables
    rename(
      # income vars
        # did checks on income input vars and no missing or weird values
      households_tot = ghu001, # total number of households, regardless of whether income data collected for the household
      inc_house_med = gmy001, # median household income; universe = households
      inc_house_agg = gmz001, # aggregate household income; universe = households
      # poverty vars
        # did checks on income input vars and no missing or weird values
      pov_yes = gok001,
      pov_no = gok002,
    ) %>% 
    mutate(
      households_tot_calc = rowSums(select(., gmx001:gmx016), na.rm = TRUE), # calculated number of households that have values for household income bands
        #check_calc = if_else(households_tot==households_tot_calc,1,0), # check whether total number of households from Census equals calculated total number of households; always equals 1!
        # ran frequency: check_calc always equals 1
      pov_denom = rowSums(select(., pov_yes,pov_no), na.rm = TRUE),
        #check_calc = if_else(households_tot==pov_denom,1,0), # check whether total number of households from Census equals number of households that are either above or below poverty line
        # ran frequency: check_calc always equals 1
    ) %>% select(-households_tot_calc,-pov_denom,-c(starts_with('gmx')),-c(area_tract,area_intersection,state,statea,county,countya,tracta))
    

  d2000_sf1a_anal_tract %>% glimpse()
  
  
  
  #d2000_sf1a_anal_tract %>% select(gisjoin,grw015:grw028,edu_tot_black) %>% View()

  # create character vector of names of variables to be summed
    # remove income variables that should not be summed
  sum_vars <- d2000_sf1a_anal_tract %>% select(-c(eps,geometry,gisjoin,inc_house_med,proportion)) %>% names()
  sum_vars  
  
  #5 = adjust tract data by proportion
  #Adjust any tract-level variables (e.g., population counts) by multiplying them by the proportion of the tract within each geomarket.
  
  d2000_sf1a_anal_tract <- d2000_sf1a_anal_tract %>%
    mutate(across(
      .cols = all_of(sum_vars),
      .fns = ~ .x * proportion
    ))  
  
  d2000_sf1a_anal_tract %>% glimpse()
  
  # create tract-level analysis dataset that is sf  
  d2000_sf1a_anal_tract_sf <- d2000_sf1a_anal_tract %>% 
    mutate(
      
      # create percent race and ethnicity variables
      pct_nhisp_white = nhisp_white / tot_all * 100,
      pct_nhisp_black = nhisp_black / tot_all * 100,
      pct_nhisp_native = nhisp_native / tot_all * 100,
      pct_nhisp_asian = nhisp_asian / tot_all * 100,
      pct_nhisp_nhpi = nhisp_nhpi / tot_all * 100,
      pct_nhisp_other = nhisp_other / tot_all * 100,
      pct_nhisp_multi = nhisp_multi / tot_all * 100,
      pct_hisp_white = hisp_white / tot_all * 100,
      pct_hisp_black = hisp_black / tot_all * 100,
      pct_hisp_native = hisp_native / tot_all * 100,
      pct_hisp_asian = hisp_asian / tot_all * 100,
      pct_hisp_nhpi = hisp_nhpi / tot_all * 100,
      pct_hisp_other = hisp_other / tot_all * 100,
      pct_hisp_multi = hisp_multi / tot_all * 100,
      pct_nhisp_api = nhisp_api / tot_all * 100,
      pct_hisp_api = hisp_api / tot_all * 100,
      pct_hisp_all = hisp_all / tot_all * 100,
      
      # education vars; focus on percent of households w/ a BA or higher
      pct_edu_baplus_all = edu_baplus_all / edu_tot_all * 100,
      pct_edu_baplus_white = edu_baplus_white / edu_tot_white * 100,
      pct_edu_baplus_black = edu_baplus_black / edu_tot_black * 100,
      pct_edu_baplus_native = edu_baplus_native / edu_tot_native * 100,
      pct_edu_baplus_asian = edu_baplus_asian / edu_tot_asian * 100,
      pct_edu_baplus_nhpi = edu_baplus_nhpi / edu_tot_nhpi * 100,
      pct_edu_baplus_api = edu_baplus_api / edu_tot_api * 100,
      pct_edu_baplus_hisp = edu_baplus_hisp / edu_tot_hisp * 100,
      pct_edu_baplus_nhisp_white = edu_baplus_nhisp_white / edu_tot_nhisp_white * 100,

      # create mean income variable
      # sum of agg inc across all tracts/ (sum of total households across all tracts)
      mean_inc_house = inc_house_agg/households_tot,
      
      # Convert 1999 dollars to 2024 dollars using CPI data
      # CPI for 1999: 166.6
      # CPI for 2024: 314.175
      # Formula: Adjusted Amount = Original Amount * (CPI in 2024 / CPI in 1999) = (og amoun)t * (314.175/166.6) = (og amount) * 1.885804
      
      mean_inc_house = mean_inc_house*1.885804,
      med_inc_house = inc_house_med*1.885804,
      
      # create percent poverty variables
      pct_pov_yes = pov_yes/households_tot*100,  
      
    ) %>% rename(geometry_eps = geometry) %>% 
    # retrieve tract-level geometry (including partial geometries for those that cross eps borders)
    inner_join(
      y = d2000_tract_eps_intersect %>% select(gisjoin,eps,geometry),
      by = c('gisjoin','eps')
    ) %>% 
    # transform into sf object, using WGS84 CRS which leaflet mapping says is required
    st_as_sf() %>% st_transform(crs = 4326)
  
  
  d2000_sf1a_anal_tract_sf %>% class()
  d2000_sf1a_anal_tract_sf %>% glimpse()

  # save to analysis folder, outside the repo cuz too big for repo
  save(d2000_sf1a_anal_tract_sf, file = file.path(shape_dir,'analysis_data', 'd2000_sf1a_anal_tract_sf.RData'))
  
  load(file = file.path(shape_dir,'analysis_data', 'd2000_sf1a_anal_tract_sf.RData'))
  
  
  d2000_sf1a_anal_tract %>% select(contains('_baplus')) %>% names()
  d2000_sf1a_anal_tract %>% select(starts_with('edu_tot')) %>% names()
  
  # create eps-level analysis dataset
  d2000_sf1a_anal_eps <- d2000_sf1a_anal_tract %>% 
    group_by(eps) %>% summarize(
      n_tracts = n(),
      # sum vars
      across(.cols = all_of(sum_vars), ~ sum(.x, na.rm = TRUE), .names = "sum_{.col}"),
      
      # median of median household income at tract-level
      med_inc_house_med = median(inc_house_med, na.rm = TRUE),
      
    ) %>% 
    mutate(
      
      # create percent race and ethnicity variables
      pct_nhisp_white = sum_nhisp_white / sum_tot_all * 100,
      pct_nhisp_black = sum_nhisp_black / sum_tot_all * 100,
      pct_nhisp_native = sum_nhisp_native / sum_tot_all * 100,
      pct_nhisp_asian = sum_nhisp_asian / sum_tot_all * 100,
      pct_nhisp_nhpi = sum_nhisp_nhpi / sum_tot_all * 100,
      pct_nhisp_other = sum_nhisp_other / sum_tot_all * 100,
      pct_nhisp_multi = sum_nhisp_multi / sum_tot_all * 100,
      pct_hisp_white = sum_hisp_white / sum_tot_all * 100,
      pct_hisp_black = sum_hisp_black / sum_tot_all * 100,
      pct_hisp_native = sum_hisp_native / sum_tot_all * 100,
      pct_hisp_asian = sum_hisp_asian / sum_tot_all * 100,
      pct_hisp_nhpi = sum_hisp_nhpi / sum_tot_all * 100,
      pct_hisp_other = sum_hisp_other / sum_tot_all * 100,
      pct_hisp_multi = sum_hisp_multi / sum_tot_all * 100,
      pct_nhisp_api = sum_nhisp_api / sum_tot_all * 100,
      pct_hisp_api = sum_hisp_api / sum_tot_all * 100,
      pct_hisp_all = sum_hisp_all / sum_tot_all * 100,
      
      pct_nhisp_white_tally = sum_nhisp_white_tally / sum_tot_all * 100,
      pct_nhisp_black_tally = sum_nhisp_black_tally / sum_tot_all * 100,
      pct_nhisp_native_tally = sum_nhisp_native_tally / sum_tot_all * 100,
      pct_nhisp_asian_tally = sum_nhisp_asian_tally / sum_tot_all * 100,
      pct_nhisp_nhpi_tally = sum_nhisp_nhpi_tally / sum_tot_all * 100,
      pct_nhisp_other_tally = sum_nhisp_other_tally / sum_tot_all * 100,
      pct_hisp_white_tally = sum_hisp_white_tally / sum_tot_all * 100,
      pct_hisp_black_tally = sum_hisp_black_tally / sum_tot_all * 100,
      pct_hisp_native_tally = sum_hisp_native_tally / sum_tot_all * 100,
      pct_hisp_asian_tally = sum_hisp_asian_tally / sum_tot_all * 100,
      pct_hisp_nhpi_tally = sum_hisp_nhpi_tally / sum_tot_all * 100,
      pct_hisp_other_tally = sum_hisp_other_tally / sum_tot_all * 100,
      pct_nhisp_api_tally = sum_nhisp_api_tally / sum_tot_all * 100,
      pct_hisp_api_tally = sum_hisp_api_tally / sum_tot_all * 100,
      pct_hisp_all_tally = sum_hisp_all_tally / sum_tot_all * 100,
      
      # education vars; focus on percent of households w/ a BA or higher
      pct_edu_baplus_all = sum_edu_baplus_all / sum_edu_tot_all * 100,
      pct_edu_baplus_white = sum_edu_baplus_white / sum_edu_tot_white * 100,
      pct_edu_baplus_black = sum_edu_baplus_black / sum_edu_tot_black * 100,
      pct_edu_baplus_native = sum_edu_baplus_native / sum_edu_tot_native * 100,
      pct_edu_baplus_asian = sum_edu_baplus_asian / sum_edu_tot_asian * 100,
      pct_edu_baplus_nhpi = sum_edu_baplus_nhpi / sum_edu_tot_nhpi * 100,
      pct_edu_baplus_api = sum_edu_baplus_api / sum_edu_tot_api * 100,
      pct_edu_baplus_hisp = sum_edu_baplus_hisp / sum_edu_tot_hisp * 100,
      pct_edu_baplus_nhisp_white = sum_edu_baplus_nhisp_white / sum_edu_tot_nhisp_white * 100,

      # create mean income variable
      # sum of agg inc across all tracts/ (sum of total households across all tracts)
      mean_inc_house = sum_inc_house_agg/sum_households_tot,
      
      # Convert 1999 dollars to 2024 dollars using CPI data
      # CPI for 1999: 166.6
      # CPI for 2024: 314.175
      # Formula: Adjusted Amount = Original Amount * (CPI in 2024 / CPI in 1999) = (og amoun)t * (314.175/166.6) = (og amount) * 1.885804
      
      mean_inc_house = mean_inc_house*1.885804,
      med_inc_house_med = med_inc_house_med*1.885804,
      
      # create percent poverty variables
      pct_pov_yes = sum_pov_yes/sum_households_tot*100,  
      pct_pov_no = sum_pov_no/sum_households_tot*100,
      
    )
  
  # merge to object w/ eps shape files and convert to sf object
  d2000_sf1a_anal_eps_sf <- d2000_sf1a_anal_eps %>% 
    inner_join(y = eps_geometry_zcta, by = c('eps')) %>% 
    st_as_sf() %>% 
    st_transform(5070)
  
  # save in analysis data file
  save(d2000_sf1a_anal_eps_sf, file = file.path(data_dir,'analysis_data', 'd2000_sf1a_anal_eps_sf.RData'))
  
  load(file = file.path(data_dir,'analysis_data', 'd2000_sf1a_anal_eps_sf.RData'))
  
  # results for "important" vars
  d2000_sf1a_anal_eps %>% 
    select(eps,pct_nhisp_white,pct_hisp_all,pct_nhisp_black,pct_nhisp_api,pct_nhisp_other,pct_nhisp_multi,med_inc_house_med,mean_inc_house,pct_pov_yes,pct_edu_baplus_all,pct_edu_baplus_nhisp_white,pct_edu_baplus_black,pct_edu_baplus_hisp) %>% View()
  
  # look at result of income and poverty variables
  d2000_sf1a_anal_eps %>% select(eps,pct_nhisp_white,pct_nhisp_black,pct_nhisp_native,pct_nhisp_api,pct_nhisp_multi,pct_hisp_all,med_inc_house_med,mean_inc_house,pct_pov_yes) %>% View()
  
  # look at result of education vars
  # all education vars
  d2000_sf1a_anal_eps %>% 
    select(eps,pct_edu_baplus_all,,pct_edu_baplus_white,pct_edu_baplus_nhisp_white,pct_edu_baplus_hisp,pct_edu_baplus_black,pct_edu_baplus_api,pct_edu_baplus_asian,pct_edu_baplus_nhpi,pct_edu_baplus_native) %>% View() # ,pct_edu_baplus_multi
  
  
  d2000_sf1a_anal_eps %>% select(eps,pct_nhisp_white,pct_nhisp_black,pct_nhisp_native,pct_nhisp_api,pct_nhisp_multi,pct_hisp_all,pct_edu_baplus_all,pct_edu_baplus_nhisp_white,pct_edu_baplus_black,pct_edu_baplus_hisp,pct_edu_baplus_api) %>% View()
  
  # look at result of education vars
  d2000_sf1a_anal_eps %>% select(eps,pct_nhisp_white,pct_nhisp_black,pct_nhisp_native,pct_nhisp_api,pct_nhisp_multi,pct_hisp_all,pct_edu_baplus_all,pct_edu_baplus_nhisp_white,pct_edu_baplus_black,pct_edu_baplus_hisp,pct_edu_baplus_api) %>% View()
  
  

  # look at results of of race/ethnicity vars
  
  d2000_sf1a_anal_eps %>% select(eps,sum_tot_all,pct_nhisp_white,pct_nhisp_black,pct_nhisp_native,pct_nhisp_api,pct_nhisp_other,pct_nhisp_multi,pct_hisp_all) %>% View()
  
  # people allowed to identify as more than one race (sums to greater than 100)
  
  d2000_sf1a_anal_eps %>% select(eps,pct_nhisp_white_tally,pct_nhisp_black_tally,pct_nhisp_native_tally,pct_nhisp_api_tally,pct_nhisp_other_tally,pct_hisp_all_tally) %>% View()
  