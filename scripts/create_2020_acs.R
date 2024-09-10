################################################################################
## [ PROJ ] < College Board Geomarket >
## [ FILE ] < create_2020_acs.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 8/8/2024
## [ DESC ] < Get 2020 ACS (2016-2020) census tract characteristics, shape files, and merge w/ eps geomarkets and create eps-level measures>
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

acs2020_data_dir <-   file.path('.',data_dir,'2020_acs') # main data directory
list.files(path = acs2020_data_dir)

# can't save shape files in the repo cuz they too big
shape_dir <-   file.path('.','..','cb_geomarket_shape') # main data directory
list.files(path = shape_dir)

eps_data_dir <- file.path(data_dir,'eps_market') # has eps geomarket data and spaical data files
list.files(path = eps_data_dir)

scripts_dir <- file.path('.','scripts') # 
list.files(path = scripts_dir)

###### LOAD EPS SHAPE FILE DATA

  load(file.path(eps_data_dir, 'eps_shapes.RData'))

######## READ IN SHAPE FILE FOR 2020 CENSUS TRACTS

list.files(path = file.path(shape_dir,'2020'))

#acs2020_tract_sf <- tracts(cb = TRUE, year = 2020, class = "sf")
#names(acs2020_tract_sf) <- acs2020_tract_sf %>% names %>% tolower()
#save(acs2020_tract_sf, file = file.path(shape_dir,'2020', 'acs2020_tract_sf.RData'))

# command to load data frame
load(file = file.path(shape_dir,'2020', 'acs2020_tract_sf.RData'))

acs2020_tract_sf %>% glimpse()
acs2020_tract_sf %>% class() # sf, data frame
acs2020_tract_sf %>% st_crs

# check data structure
acs2020_tract_sf %>% select(geoid) %>% as.data.frame() %>% group_by(geoid) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # uniquely identifies

############ GET TRACT-LEVEL 2020 ACS DATA ON INCOME AND POVERTY FROM TIDYCENSUS

# Get the list of variables for the 2016-2020 5-year ACS
acs2020_vars <- load_variables(2020, "acs5", cache = TRUE) # %>% filter(geography == "tract")
acs2020_vars %>% glimpse()

acs2020_vars %>% filter(name == "B19001_001") %>% print()

acs2020_vars %>% filter(name == "B01001A_001") %>% print()

#### get educational attainment variables

# Filter for variable names
edu_variables <- acs2020_vars %>%
  filter(str_detect(name, "^B15003") | str_detect(name, "^C15002"))

edu_var_names <- edu_variables$name

##### 

variables <- c(
  "B19025_001E",  # Aggregate household income (from table B19025)
  "B19001_001E",  # "Total households" — This variable gives the total number of households that reported income data at the census tract level.
  "B19013_001E",  # "Median Household Income in the Past 12 Months (in 2020 Inflation-Adjusted Dollars)"
  "B17017_002E",  # "Number of households living below poverty level" 
  "B17017_001E",  # "Total number of households that were considered for poverty status determination" 
  "B03002_001E",  # Total population
  "B03002_002E",  # Not Hispanic or Latino
  "B03002_003E",  # Not Hispanic or Latino: White alone
  "B03002_004E",  # Not Hispanic or Latino: Black or African American alone
  "B03002_005E",  # Not Hispanic or Latino: American Indian and Alaska Native alone
  "B03002_006E",  # Not Hispanic or Latino: Asian alone
  "B03002_007E",  # Not Hispanic or Latino: Native Hawaiian and Other Pacific Islander alone
  "B03002_008E",  # Not Hispanic or Latino: Some other race alone
  "B03002_009E",  # Not Hispanic or Latino: Two or more races
  "B03002_010E",  # Not Hispanic or Latino: Two races including Some other race
  "B03002_011E",  # Not Hispanic or Latino: Two races excluding Some other race, and three or more races
  "B03002_012E",  # Hispanic or Latino
  "B03002_013E",  # Hispanic or Latino: White alone
  "B03002_014E",  # Hispanic or Latino: Black or African American alone
  "B03002_015E",  # Hispanic or Latino: American Indian and Alaska Native alone
  "B03002_016E",  # Hispanic or Latino: Asian alone
  "B03002_017E",  # Hispanic or Latino: Native Hawaiian and Other Pacific Islander alone
  "B03002_018E",  # Hispanic or Latino: Some other race alone
  "B03002_019E",  # Hispanic or Latino: Two or more races
  "B03002_020E",  # Hispanic or Latino: Two races including Some other race
  "B03002_021E"   # Hispanic or Latino: Two races excluding Some other race, and three or more races
)
variables <- c(variables,edu_var_names)
variables

acs2020_vars %>% filter(name %in% variables) %>% print()

# READ IN TRACT-LEVEL 2016-2020 ACS DATA FROM TIDYCENSUS

  # List of all state abbreviations plus Washington, D.C.
  states <- c(state.abb, "DC")
  states
  
  # Initialize an empty list to store data
  all_data <- list()
  
  # Loop through each state to get tract-level data
  for (state in states) {
    state_data <- get_acs(
      geography = "tract",
      state = state,
      variables = variables,
      year = 2020,
      survey = "acs5",
      output = "wide"
    )
    all_data[[state]] <- state_data
  }

  # Combine all the state data into one data frame
  acs2020_ab_tract <- bind_rows(all_data)
  rm(states,state,all_data,state_data)
  
  acs2020_ab_tract %>% glimpse()

# ADD VARIABLE LABELS FROM load_variables() FUNCTION TO ACS DATA
  
  # Use str_subset to get the variables matching the pattern, vars that end with E
  matching_variables <- str_subset(string = acs2020_ab_tract %>% names(), pattern = "_\\d+E$")
  
  # Remove the 'E' suffix to match with the 'name' column in acs2020_vars
  matching_variables <- str_remove(matching_variables, "E$")
  
  matching_variables  

  # Create a mapping dataframe
  label_mapping <- acs2020_vars %>% 
    filter(name %in% matching_variables) %>% 
    select(name, label)
  label_mapping
  
  # Create a named vector for labels, including both 'E' and 'M' suffixes
  labels <- setNames(rep(label_mapping$label, each = 2), 
                     paste0(rep(label_mapping$name, each = 2), c("E", "M")))

  labels
  
  # Replace "Estimate!!" with "Margin of error!!" for margin of error variables
  labels <- setNames(
    c(label_mapping$label, str_replace(label_mapping$label, "^Estimate!!", "Margin of error!!")),
    c(paste0(label_mapping$name, "E"), paste0(label_mapping$name, "M"))
  )
  labels
  
  # Create placeholders for GEOID and NAME
  placeholders <- setNames(rep("", 2), c("GEOID", "NAME"))
  
  # Combine the placeholders with the existing labels
  full_labels <- c(placeholders, labels)
  full_labels
  
  # Reorder full_labels to match the order of var_names
  reordered_labels <- full_labels[names(acs2020_ab_tract)]
  reordered_labels
  
  # Assign the labels using labelled::var_label
  var_label(acs2020_ab_tract) <- reordered_labels

  acs2020_ab_tract %>% glimpse()
  acs2020_ab_tract %>% var_label()
  
  # remove objects
  rm(variables,placeholders,full_labels,labels,label_mapping,matching_variables,reordered_labels)
  
  # rename vars to lowercase
  names(acs2020_ab_tract) <- acs2020_ab_tract %>% names %>% tolower()
  
  # drop the margin of error variables
  acs2020_ab_tract <- acs2020_ab_tract %>% select(-c(matches('_\\d+m$')))

  acs2020_ab_tract %>% var_label()
  
###### MERGE 2000 CENSUS DATA TO SF DATASET W/ 2000 CENSUS TRACT POLYGONS

# join between sf1a_2000_tract and d2000_tract_sf
acs2020_ab_tract %>% glimpse()

acs2020_ab_tract_sf <- acs2020_ab_tract %>% 
    left_join(y = acs2020_tract_sf %>% select(geoid, geometry), by = c('geoid')
  ) # note: 84414 - 84122 = 292 tracts that are in the shapefile dataframe and not in the data dataframe

acs2020_ab_tract_sf %>% count
acs2020_ab_tract_sf %>% glimpse()
acs2020_ab_tract_sf %>% st_crs

# assert data structure
acs2020_ab_tract_sf %>% select(geoid) %>% as.data.frame() %>% group_by(geoid) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # uniquely identifies

# Convert to sf object
acs2020_ab_tract_sf %>% class()
acs2020_ab_tract_sf <- st_as_sf(acs2020_ab_tract_sf, sf_column_name = "geometry")
acs2020_ab_tract_sf %>% class()
acs2020_ab_tract_sf %>% st_crs()

# assert/Assign the CRS
st_crs(acs2020_ab_tract_sf) == st_crs(acs2020_tract_sf) # crs is the same

####################
####################
# ASSIGN EPS CODE TO EACH CENSUS TRACT

# inputs you have 
# have sf object with tract-level data containing the geometry for each tract and variables with counts of population in each tract
# have sf object of eps-level data with one obs per EPS code and geometry for each EPS

# desired output
# or sf object that has one obs per tract-eps and for each tract we know the eps code of that tract

# st_crs function retreives coordinate reference system for an object
st_crs(eps_geometry_zcta) == st_crs(acs2020_ab_tract_sf) # false 
acs2020_ab_tract_sf %>% st_crs()
eps_geometry_zcta %>% st_crs()


# transform so that they have the same CRS system
# what chat gpt says: The st_transform() function is used to reproject the coordinates of your
# spatial object from one CRS to another, ensuring that the geometries are correctly 
# transformed to the new coordinate system. In contrast, st_crs() is used to either set
#or retrieve the CRS of an object without transforming the coordinates.

# Transform eps_geometry_zcta to match the CRS of d2000_sf1a_tract_sf
eps_geometry_zcta <- st_transform(eps_geometry_zcta, st_crs(acs2020_ab_tract_sf))
st_crs(eps_geometry_zcta) == st_crs(acs2020_ab_tract_sf)

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
  acs2020_ab_tract_sf <- acs2020_ab_tract_sf %>% mutate(area_tract = st_area(.))

####
# 2 = Perform spacial intersection

  # first simplify the geometry
    #acs2020_ab_tract_sf <- st_simplify(acs2020_ab_tract_sf, dTolerance = 0.001) # chatGPT said this was a "pretty cautious" value for dTolerance for census tract geometries
      # geometryies are vertices (points) arranged in order, connected by lines. st_simplify() removes points that are "close" together, with "close" defined by the value you assign to the dTolerance argument
      # Understanding dTolerance in EPSG:4269; Since your CRS is in degrees, the dTolerance value you choose will represent a distance in degrees. Here's what a dTolerance of 0.001 would correspond to:
        # 0.001 degrees of latitude would be about 0.001 × 69 =0.069
        # 0.001×69=0.069 miles, or approximately 364 feet.
  # this is very computationally/time intensive! [like 45 minutes +]
    acs2020_tract_eps_intersect <- st_intersection(acs2020_ab_tract_sf, eps_geometry_zcta)
    
  # check if geometry is valid
    valid_geom <- st_is_valid(acs2020_tract_eps_intersect) # check to see whether geometries are "valid"; valid geometries needed for st_intersect
    summary(valid_geom) 
      # with using st_simplify with dTolerance = 0.001: 13567 obs FALSE; 84147 obs TRUE; this was worse performance than prior to st_simplify
      # without using st_simplify: 9,523 FALSE, 88129 TRUE
    rm(valid_geom)
    
  # make geometries valid
    acs2020_tract_eps_intersect <- st_make_valid(acs2020_tract_eps_intersect)    
    
  # save/load for use    
   save(acs2020_tract_eps_intersect, file = file.path(shape_dir,'2020', 'acs2020_tract_eps_intersect.RData'))

  # load object created by st_intersection
    load(file = file.path(shape_dir,'2020', 'acs2020_tract_eps_intersect.RData'))

  acs2020_tract_eps_intersect %>% glimpse()

#3 = calculate the area of each intersection
  #After obtaining the intersection, calculate the area of each intersected polygon (which represents the part of the census tract within a geomarket). Then calculate the area of each original census tract.

  # calculate area of each intersected polygon  
  acs2020_tract_eps_intersect$area_intersection <- st_area(acs2020_tract_eps_intersect) # does same as this:  st_area(acs2020_tract_eps_intersect$geometry)
  
#4 calculate the area proportion
  #Calculate the proportion of the census tract that is within each geomarket by dividing the area of the intersection by the total area of the original census tract.
  # steps
  
  # calculate proportion:
  acs2020_tract_eps_intersect <- acs2020_tract_eps_intersect %>%
    mutate(proportion = as.numeric(area_intersection / area_tract))
  
  # Perform the spatial join 
    #acs2020_ab_tract_sf_eps <- st_join(acs2020_ab_tract_sf, eps_geometry_zcta, join = st_intersects) # this is the previous spatial join

  acs2020_tract_eps_intersect %>% glimpse()
  acs2020_tract_eps_intersect %>% class()  

#investigate data structure
# gisjoin uniquely identifies obs in the input dataset
acs2020_ab_tract_sf %>% as.data.frame() %>% 
  select(geoid) %>% group_by(geoid) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # one observation per gisjoin value

# gisjoin does not uniquely identify obs in the dataset created by st_join with join = st_intersect    
acs2020_tract_eps_intersect %>% as.data.frame() %>% 
  select(geoid) %>% group_by(geoid) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) #

# gisjoin,eps uniquely identifies obs in new dataset
acs2020_tract_eps_intersect %>% as.data.frame() %>% 
  select(geoid,eps) %>% group_by(geoid,eps) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # one observation per zip code  

# result: if the geometry of a census tract overlaps with more than one eps geometry, that census tract appears multiple times
  # what chat gpt says    
    # Yes, in the solution you've implemented, a given census tract will appear once if it is wholly contained within a single geomarket and will appear multiple times if it intersects with multiple geomarkets. Each appearance represents an intersection with a different geomarket.
    #Wholly Contained Tracts: If a census tract is entirely within a single geomarket, it will appear once in the joined dataframe with that geomarket's eps code.
    #Intersecting Tracts: If a census tract intersects with multiple geomarkets, it will appear once for each geomarket it intersects with, each time with the respective geomarket's eps code.
    #This results in some census tracts being duplicated in the joined dataframe, reflecting the fact that they span more than one geomarket.    

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
acs2020_tract_eps_intersect <- st_transform(x = acs2020_tract_eps_intersect, crs = 5070) # important to do this after the st_simplify step (above) because in previous crs the dTolerance level of 0.001 is like 364 feet, whereas in this crs it is like milimeters or something

# Check the new CRS
st_crs(acs2020_tract_eps_intersect)

############## 
# USING GROUP-BY AND SUMMARIZE, CREATE OBJECTS AT THE EPS LEVEL THAT HAVE POPULATION VARIABLES SUMMARIZED 
# MERGE EPS-LEVEL CHARACTERISTIC DATA TO DATAFRAME THAT HAS EPS GEOMETRY; THEN DO SOME ANALYSES:           

acs2020_tract_eps_intersect %>% glimpse()

acs2020_ab_tract %>% var_label()

acs2020_ab_tract %>% select(matches('B15003')) %>% var_label()


acs2020_ab_anal_tract <- acs2020_tract_eps_intersect %>% as.data.frame() %>% 
  # RACE/ETHNICITY VARIABLES
  # drop variables you don't need (after running checks)
  # non-hispanic, two-or more races: amp3e009 always == amp3e010+amp3e011
    # amp3e009: "Estimates: Not Hispanic or Latino: Two or more races"
    # amp3e010: "Estimates: Not Hispanic or Latino: Two or more races: Two races including Some other race"
    # amp3e011: [1] "Estimates: Not Hispanic or Latino: Two or more races: Two races excluding Some other race, and three or more races"
  # hispanic, two or more races: amp3e019 always == amp3e020+amp3e021
    # amp3e019. "Estimates: Hispanic or Latino: Two or more races"
    # amp3e020. "Estimates: Hispanic or Latino: Two or more races: Two races including Some other race"
    # amp3e021. "Estimates: Hispanic or Latino: Two or more races: Two races excluding Some other race, and three or more races"  
  select(-c(name,b03002_010e,b03002_011e,b03002_020e,b03002_021e)) %>% 
  # RENAME AND CREATE VARS OF POPULATION BY RACE, ETHNICITY, AND AGE
  rename(
    tot_all = b03002_001e,
    nhisp_all = b03002_002e,
    hisp_all = b03002_012e,
    
    nhisp_white = b03002_003e,
    nhisp_black = b03002_004e,
    nhisp_native = b03002_005e,
    nhisp_asian = b03002_006e,
    nhisp_nhpi = b03002_007e,
    nhisp_other = b03002_008e,
    nhisp_multi = b03002_009e,
    
    hisp_white = b03002_013e,
    hisp_black = b03002_014e,
    hisp_native = b03002_015e,
    hisp_asian = b03002_016e,
    hisp_nhpi = b03002_017e,
    hisp_other = b03002_018e,
    hisp_multi = b03002_019e,
  ) %>% 
  # api = asian + nhpi
  mutate(
    # api vars
    nhisp_api = rowSums(select(., nhisp_asian, nhisp_nhpi), na.rm = TRUE),
    hisp_api =  rowSums(select(., hisp_asian, hisp_nhpi), na.rm = TRUE),
  ) %>% 
  # INCOME AND POVERTY VARIABLES
  rename(
    #income
    households_tot = b19001_001e, # "Total households" — This variable gives the total number of households that reported income data at the census tract level.
    inc_house_agg = b19025_001e, # "Estimate!!Aggregate household income in the past 12 months (in 2020 inflation-adjusted dollars)"
    inc_house_med = b19013_001e, # "Estimate!!Median household income in the past 12 months (in 2020 inflation-adjusted dollars)"
      
    # poverty
    pov_yes = b17017_002e,
    pov_denom = b17017_001e
  ) %>%
  # EDUCATIONAL ATTAINMENT
  mutate(
    # all races
    edu_lths_all = rowSums(select(., b15003_002e:b15003_016e), na.rm = TRUE),  # No schooling to 12th grade, no diploma
    edu_hs_all = rowSums(select(., b15003_017e, b15003_018e), na.rm = TRUE),  # Regular high school diploma, GED or alternative credential
    edu_ltassoc_all = rowSums(select(., b15003_019e, b15003_020e), na.rm = TRUE),  # Some college, less than 1 year, 1+ years, no degree
    edu_assoc_all = rowSums(select(., b15003_021e), na.rm = TRUE),  # Associate's degree
    edu_ltba_all = edu_ltassoc_all + edu_assoc_all,  # Less than bachelor's (some college + associate's)
    edu_ba_all = rowSums(select(., b15003_022e), na.rm = TRUE),  # Bachelor's degree
    edu_ma_all = rowSums(select(., b15003_023e), na.rm = TRUE),  # Master's degree
    edu_fprof_all = rowSums(select(., b15003_024e), na.rm = TRUE),  # Professional school degree
    edu_doct_all = rowSums(select(., b15003_025e), na.rm = TRUE),  # Doctorate degree
    edu_baplus_all = rowSums(select(., b15003_022e:b15003_025e), na.rm = TRUE),  # Bachelor's degree and higher
    edu_tot_all = rowSums(select(., b15003_001e), na.rm = TRUE),  # Total population (summing all educational attainment levels)
    
    # white alone (includes hispanic and non-hispanic)
    edu_lths_white = rowSums(select(., c15002a_003e, c15002a_008e), na.rm = TRUE),   # Less than high school diploma
    edu_hs_white = rowSums(select(., c15002a_004e, c15002a_009e), na.rm = TRUE),     # High school graduate (includes equivalency)
    edu_somecol_white = rowSums(select(., c15002a_005e, c15002a_010e), na.rm = TRUE), # Some college or associate's degree
    edu_baplus_white = rowSums(select(., c15002a_006e, c15002a_011e), na.rm = TRUE), # Bachelor's degree or higher
    edu_tot_white = rowSums(select(., c15002a_002e, c15002a_007e), na.rm = TRUE),     # Total population
    
    # black alone (includes hispanic and non-hispanic)
    edu_lths_black = rowSums(select(., c15002b_003e, c15002b_008e), na.rm = TRUE),   # Less than high school diploma
    edu_hs_black = rowSums(select(., c15002b_004e, c15002b_009e), na.rm = TRUE),     # High school graduate (includes equivalency)
    edu_somecol_black = rowSums(select(., c15002b_005e, c15002b_010e), na.rm = TRUE), # Some college or associate's degree
    edu_baplus_black = rowSums(select(., c15002b_006e, c15002b_011e), na.rm = TRUE), # Bachelor's degree or higher
    edu_tot_black = rowSums(select(., c15002b_002e, c15002b_007e), na.rm = TRUE),     # Total population
    
    # native (includes hispanic and non-hispanic)
    edu_lths_native = rowSums(select(., c15002c_003e, c15002c_008e), na.rm = TRUE),   # Less than high school diploma
    edu_hs_native = rowSums(select(., c15002c_004e, c15002c_009e), na.rm = TRUE),     # High school graduate (includes equivalency)
    edu_somecol_native = rowSums(select(., c15002c_005e, c15002c_010e), na.rm = TRUE), # Some college or associate's degree
    edu_baplus_native = rowSums(select(., c15002c_006e, c15002c_011e), na.rm = TRUE), # Bachelor's degree or higher
    edu_tot_native = rowSums(select(., c15002c_002e, c15002c_007e), na.rm = TRUE),     # Total population    
    
    # asian (includes hispanic and non-hispanic)
    edu_lths_asian = rowSums(select(., c15002d_003e, c15002d_008e), na.rm = TRUE),   # Less than high school diploma
    edu_hs_asian = rowSums(select(., c15002d_004e, c15002d_009e), na.rm = TRUE),     # High school graduate (includes equivalency)
    edu_somecol_asian = rowSums(select(., c15002d_005e, c15002d_010e), na.rm = TRUE), # Some college or associate's degree
    edu_baplus_asian = rowSums(select(., c15002d_006e, c15002d_011e), na.rm = TRUE), # Bachelor's degree or higher
    edu_tot_asian = rowSums(select(., c15002d_002e, c15002d_007e), na.rm = TRUE),      # Total population    
    
    # nhpi (includes hispanic and non-hispanic)
    edu_lths_nhpi = rowSums(select(., c15002e_003e, c15002e_008e), na.rm = TRUE),   # Less than high school diploma
    edu_hs_nhpi = rowSums(select(., c15002e_004e, c15002e_009e), na.rm = TRUE),     # High school graduate (includes equivalency)
    edu_somecol_nhpi = rowSums(select(., c15002e_005e, c15002e_010e), na.rm = TRUE), # Some college or associate's degree
    edu_baplus_nhpi = rowSums(select(., c15002e_006e, c15002e_011e), na.rm = TRUE), # Bachelor's degree or higher
    edu_tot_nhpi = rowSums(select(., c15002e_002e, c15002e_007e), na.rm = TRUE),     # Total population    
    
    # API (Asian + NHPI) (includes hispanic and non-hispanic)
    edu_lths_api = rowSums(select(., c15002d_003e, c15002d_008e, c15002e_003e, c15002e_008e), na.rm = TRUE),   # Less than high school diploma
    edu_hs_api = rowSums(select(., c15002d_004e, c15002d_009e, c15002e_004e, c15002e_009e), na.rm = TRUE),     # High school graduate (includes equivalency)
    edu_somecol_api = rowSums(select(., c15002d_005e, c15002d_010e, c15002e_005e, c15002e_010e), na.rm = TRUE), # Some college or associate's degree
    edu_baplus_api = rowSums(select(., c15002d_006e, c15002d_011e, c15002e_006e, c15002e_011e), na.rm = TRUE), # Bachelor's degree or higher
    edu_tot_api = rowSums(select(., c15002d_002e, c15002d_007e, c15002e_002e, c15002e_007e), na.rm = TRUE),      # Total population    
    
    # multi (includes hispanic and non-hispanic)
    edu_lths_multi = rowSums(select(., c15002g_003e, c15002g_008e), na.rm = TRUE),   # Less than high school diploma
    edu_hs_multi = rowSums(select(., c15002g_004e, c15002g_009e), na.rm = TRUE),     # High school graduate (includes equivalency)
    edu_somecol_multi = rowSums(select(., c15002g_005e, c15002g_010e), na.rm = TRUE), # Some college or associate's degree
    edu_baplus_multi = rowSums(select(., c15002g_006e, c15002g_011e), na.rm = TRUE), # Bachelor's degree or higher
    edu_tot_multi = rowSums(select(., c15002g_002e, c15002g_007e), na.rm = TRUE),     # Total population
    
    # nhisp_white (Non-Hispanic White alone)
    edu_lths_nhisp_white = rowSums(select(., c15002h_003e, c15002h_008e), na.rm = TRUE),   # Less than high school diploma
    edu_hs_nhisp_white = rowSums(select(., c15002h_004e, c15002h_009e), na.rm = TRUE),     # High school graduate (includes equivalency)
    edu_somecol_nhisp_white = rowSums(select(., c15002h_005e, c15002h_010e), na.rm = TRUE), # Some college or associate's degree
    edu_baplus_nhisp_white = rowSums(select(., c15002h_006e, c15002h_011e), na.rm = TRUE), # Bachelor's degree or higher
    edu_tot_nhisp_white = rowSums(select(., c15002h_002e, c15002h_007e), na.rm = TRUE),     # Total population
    
    # hisp (Hispanic or Latino)
    edu_lths_hisp = rowSums(select(., c15002i_003e, c15002i_008e), na.rm = TRUE),   # Less than high school diploma
    edu_hs_hisp = rowSums(select(., c15002i_004e, c15002i_009e), na.rm = TRUE),     # High school graduate (includes equivalency)
    edu_somecol_hisp = rowSums(select(., c15002i_005e, c15002i_010e), na.rm = TRUE), # Some college or associate's degree
    edu_baplus_hisp = rowSums(select(., c15002i_006e, c15002i_011e), na.rm = TRUE), # Bachelor's degree or higher
    edu_tot_hisp = rowSums(select(., c15002i_002e, c15002i_007e), na.rm = TRUE)     # Total population    
  ) %>% 
  # drop input vars
  select(-c(starts_with('c15002')),-c(starts_with('b15003')),-area_intersection,-area_tract)

#acs2020_ab_anal_tract %>% select(c15002i_006e, c15002i_011e,edu_baplus_hisp) %>% View()
#acs2020_ab_anal_tract %>% select(c15002i_002e, c15002i_007e,edu_tot_hisp) %>% View()

acs2020_ab_anal_tract %>% glimpse()

# create character vector of names of variables to be summed
# remove income variables that should not be summed
sum_vars <- acs2020_ab_anal_tract %>% select(-c(eps,geometry,geoid,inc_house_med,proportion)) %>% names()
sum_vars

#5 = adjust tract data by proportion
  #Adjust any tract-level variables (e.g., population counts) by multiplying them by the proportion of the tract within each geomarket.

acs2020_ab_anal_tract <- acs2020_ab_anal_tract %>%
  mutate(across(
    .cols = all_of(sum_vars),
    .fns = ~ .x * proportion
  ))  

acs2020_ab_anal_tract %>% glimpse()

# create tract-level analysis sf dataset that has percent variables
acs2020_ab_anal_tract_sf <- acs2020_ab_anal_tract %>% 
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
    pct_nhisp_all = nhisp_all / tot_all * 100,
    
    # create mean income variable
    # sum of agg inc across all tracts/ (sum of total households across all tracts)
    mean_inc_house = inc_house_agg/households_tot,
    
    # Convert 2020 dollars to 2024 dollars using CPI data
    # CPI for 2020: 258.811
    # CPI for 2024: 314.175
    # Formula: Adjusted Amount = Original Amount * (CPI in 2024 / CPI in 2020) = (og amoun)t * (314.175/258.811) = (og amount) * 1.213917
    mean_inc_house = mean_inc_house*1.213917,
    med_inc_house = inc_house_med*1.213917,
    
    # create percent poverty variables
    pct_pov_yes = pov_yes/pov_denom*100,  
    
    # education vars; focus on percent of households w/ a BA or higher
    pct_edu_baplus_all = edu_baplus_all / edu_tot_all * 100,
    pct_edu_baplus_white = edu_baplus_white / edu_tot_white * 100,
    pct_edu_baplus_black = edu_baplus_black / edu_tot_black * 100,
    pct_edu_baplus_native = edu_baplus_native / edu_tot_native * 100,
    pct_edu_baplus_asian = edu_baplus_asian / edu_tot_asian * 100,
    pct_edu_baplus_nhpi = edu_baplus_nhpi / edu_tot_nhpi * 100,
    pct_edu_baplus_api = edu_baplus_api / edu_tot_api * 100,
    pct_edu_baplus_multi = edu_baplus_multi / edu_tot_multi * 100,
    pct_edu_baplus_hisp = edu_baplus_hisp / edu_tot_hisp * 100,
    pct_edu_baplus_nhisp_white = edu_baplus_nhisp_white / edu_tot_nhisp_white * 100,  
    
  ) %>% rename(geometry_eps = geometry) %>% 
  # retrieve tract-level geometry (including partial geometries for those that cross eps borders)
  inner_join(
    y = acs2020_tract_eps_intersect %>% select(geoid,eps,geometry),
    by = c('geoid','eps')
  ) %>% 
  # transform into sf object, using WGS84 CRS which leaflet mapping says is required
  st_as_sf() %>% st_transform(crs = 4326)

  acs2020_ab_anal_tract_sf %>% glimpse()
  acs2020_ab_anal_tract_sf %>% class()
  
  # save to analysis folder, outside the repo cuz too big for repo
  save(acs2020_ab_anal_tract_sf, file = file.path(shape_dir,'analysis_data', 'acs2020_ab_anal_tract_sf.RData'))
  
  load(file = file.path(shape_dir,'analysis_data', 'acs2020_ab_anal_tract_sf.RData'))


# create eps-level analysis dataset
acs2020_ab_anal_eps <- acs2020_ab_anal_tract %>% 
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
    pct_nhisp_all = sum_nhisp_all / sum_tot_all * 100,
    
    # create mean income variable
    # sum of agg inc across all tracts/ (sum of total households across all tracts)
    mean_inc_house = sum_inc_house_agg/sum_households_tot,
    
    # Convert 2020 dollars to 2024 dollars using CPI data
    # CPI for 2020: 258.811
    # CPI for 2024: 314.175
    # Formula: Adjusted Amount = Original Amount * (CPI in 2024 / CPI in 2020) = (og amoun)t * (314.175/258.811) = (og amount) * 1.213917
    mean_inc_house = mean_inc_house*1.213917,
    med_inc_house_med = med_inc_house_med*1.213917,
    
    # create percent poverty variables
    pct_pov_yes = sum_pov_yes/sum_pov_denom*100,  
    
    # education vars; focus on percent of households w/ a BA or higher
    pct_edu_baplus_all = sum_edu_baplus_all / sum_edu_tot_all * 100,
    pct_edu_baplus_white = sum_edu_baplus_white / sum_edu_tot_white * 100,
    pct_edu_baplus_black = sum_edu_baplus_black / sum_edu_tot_black * 100,
    pct_edu_baplus_native = sum_edu_baplus_native / sum_edu_tot_native * 100,
    pct_edu_baplus_asian = sum_edu_baplus_asian / sum_edu_tot_asian * 100,
    pct_edu_baplus_nhpi = sum_edu_baplus_nhpi / sum_edu_tot_nhpi * 100,
    pct_edu_baplus_api = sum_edu_baplus_api / sum_edu_tot_api * 100,
    pct_edu_baplus_multi = sum_edu_baplus_multi / sum_edu_tot_multi * 100,
    pct_edu_baplus_hisp = sum_edu_baplus_hisp / sum_edu_tot_hisp * 100,
    pct_edu_baplus_nhisp_white = sum_edu_baplus_nhisp_white / sum_edu_tot_nhisp_white * 100,  
    
  )

  acs2020_ab_anal_eps %>% glimpse()
  
  
  # merge to object w/ eps shape files and convert to sf object
  acs2020_ab_anal_eps_sf <- acs2020_ab_anal_eps %>% 
    inner_join(y = eps_geometry_zcta, by = c('eps')) %>% 
    st_as_sf() %>% 
    st_transform(5070)
  
  
  # save in analysis data file
  save(acs2020_ab_anal_eps_sf, file = file.path(data_dir,'analysis_data', 'acs2020_ab_anal_eps_sf.RData'))
  
  load(file = file.path(data_dir,'analysis_data', 'acs2020_ab_anal_eps_sf.RData'))
  
  # results for "important" vars
  acs2020_ab_anal_eps %>% 
    select(eps,pct_nhisp_white,pct_hisp_all,pct_nhisp_black,pct_nhisp_api,pct_nhisp_other,pct_nhisp_multi,med_inc_house_med,mean_inc_house,pct_pov_yes,pct_edu_baplus_all,pct_edu_baplus_nhisp_white,pct_edu_baplus_black,pct_edu_baplus_hisp) %>% View()
  

  # look at result of education vars
  # all education vars
  acs2020_ab_anal_eps %>% 
    select(eps,pct_edu_baplus_all,pct_edu_baplus_nhisp_white,pct_edu_baplus_white,pct_edu_baplus_hisp,pct_edu_baplus_black,pct_edu_baplus_api,pct_edu_baplus_asian,pct_edu_baplus_nhpi,pct_edu_baplus_native,pct_edu_baplus_multi) %>% View()
  
  
  
  
  
  acs2020_ab_anal_eps %>% 
    select(eps,pct_edu_baplus_all,pct_edu_baplus_nhisp_white,pct_edu_baplus_black,pct_edu_baplus_hisp,pct_edu_baplus_api,pct_edu_baplus_asian,pct_edu_baplus_nhpi,pct_edu_baplus_multi) %>% View()
  

  # look at result of income and poverty variables
  acs2020_ab_anal_eps %>% select(eps,pct_nhisp_white,pct_nhisp_black,pct_nhisp_api,pct_hisp_all,med_inc_house_med,mean_inc_house,pct_pov_yes) %>% View()
  
  # check race-ethnicity variables temp <- 
  acs2020_ab_anal_eps %>% select(eps,sum_tot_all,pct_nhisp_white,pct_nhisp_black,pct_nhisp_native,pct_nhisp_api,pct_nhisp_other,pct_nhisp_multi,pct_hisp_all) %>% View()
    #acs2020_ab_anal_eps %>% select(eps,sum_nhisp_white,sum_nhisp_black,sum_tot_all,pct_nhisp_white,pct_nhisp_black) %>% View()


