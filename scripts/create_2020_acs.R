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

# Read the tract shapefile using sf
acs2020_tract_sf <- st_read(file.path(shape_dir,'2020','US_tract_2020.shp'))  

names(acs2020_tract_sf) <- acs2020_tract_sf %>% names %>% tolower()

acs2020_tract_sf %>% glimpse()
acs2020_tract_sf %>% class() # sf, data frame
acs2020_tract_sf %>% st_crs

# check data structure
acs2020_tract_sf %>% select(gisjoin) %>% as.data.frame() %>% group_by(gisjoin) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # uniquely identifies

####### READ IN DATA

# ACS5A, POPULATION BY RACE AND ETHNICITY

# Read the first row to get the variable names
variable_names <- read_csv(file.path(acs2020_data_dir, 'nhgis0013_ds249_20205_tract.csv'), col_names = FALSE, n_max = 1) %>% as.character()

# Read the second row to get the variable labels
variable_labels <- read_csv(file.path(acs2020_data_dir, 'nhgis0013_ds249_20205_tract.csv'), col_names = FALSE, skip = 1, n_max = 1) %>% as.character()
variable_labels
# Read the data starting from the third row using the first row as column names

acs2020_a_tract <- read_csv(file.path(acs2020_data_dir, 'nhgis0013_ds249_20205_tract.csv'), col_names = variable_names, skip = 2)

# Add variable labels using the var_label function from labelled package
var_label(acs2020_a_tract) <- variable_labels
rm(variable_labels,variable_names)

# replace upper case w/ lower case variable names
names(acs2020_a_tract) <- acs2020_a_tract %>% names %>% tolower()

acs2020_a_tract %>% glimpse()
acs2020_a_tract %>% var_label()

# keep only desired variables
  # variables you need are in following tables:
    # ampv = total population
    # amp3 = population by hispanic or Latino and not hispanic or latino by race
    # amp4 = Hispanic or Latino Origin
  # drop margin of error variables

acs2020_a_tract <- acs2020_a_tract %>% 
  select(gisjoin,state,statea,county,countya,tracta,starts_with('ampv'),starts_with('amp3'),starts_with('amp4')) %>% 
  # drop margin of error variables
  select(-c(matches('^amp\\dm'),ampvm001))

acs2020_a_tract %>% select(gisjoin) %>% glimpse()

############ GET INCOME AND POVERTY DATA

# Get the list of variables for the 2016-2020 5-year ACS
acs2020_vars <- load_variables(2020, "acs5", cache = TRUE) # %>% filter(geography == "tract")

acs2020_vars %>% filter(name == "B19001_001") %>% print()

acs2020_vars %>% filter(name == "B01001A_001") %>% print()

# Table B19025: "Aggregate Household Income in the Past 12 Months (in 2020 Inflation-Adjusted Dollars)"
acs2020_vars %>% glimpse()

#  variable B19025_001E represents "Aggregate Household Income in the Past 12 Months."


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

variables

california_income <- get_acs(
  geography = "tract",        # Specify the geography level
  state = "CA",               # Specify the state (California)
  table = "B19025",           # Specify the table ID for aggregate household income
  year = 2020,                # Specify the end year of the 5-year ACS period
  survey = "acs5",            # Specify the 5-year ACS data
  output = "wide"             # Choose wide format for easier handling
)

# List of all state abbreviations plus Washington, D.C.
states <- c(state.abb, "DC")

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
national_data <- bind_rows(all_data)

national_data %>% glimpse()

acs2020_a_tract %>% count(state) %>% print(n=100)

# NEXT: MONDAY; ADD VARIABLE LABELS BY MERGING VARIABLE NAMES TO DATA FROM THE LOAD_VARIABLES() FUNCTION
  # CHATGPT WAS HAVING DIFFICULTY WITH THE REGULAR EXPRESSIONS LOL

# Print the result to check if it worked
print(cleaned_names)
acs2020_a_tract %>% filter(statea != "72") %>% count() # note: this is same number of obs as in the data frame national_data

# View the first few rows of the combined data
head(national_data)


# create a dataframe called _ab [for later use]

acs2020_ab_tract <- acs2020_a_tract

###### MERGE 2000 CENSUS DATA TO SF DATASET W/ 2000 CENSUS TRACT POLYGONS

# join between sf1a_2000_tract and d2000_tract_sf
acs2020_ab_tract %>% glimpse()

acs2020_ab_tract_sf <- acs2020_ab_tract %>% 
    left_join(y = acs2020_tract_sf %>% select(gisjoin, geometry), by = c('gisjoin')
  ) 

acs2020_ab_tract_sf %>% count
acs2020_ab_tract_sf %>% glimpse()
acs2020_ab_tract_sf %>% st_crs

# assert data structure
acs2020_ab_tract_sf %>% select(gisjoin) %>% as.data.frame() %>% group_by(gisjoin) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # uniquely identifies

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

# desired output
# or sf object that has one obs per tract and for each tract we know the eps code of that tract
# what my homie chatgpt says:
#You can perform a spatial join to assign the eps code from eps_geometry_zcta to each observation in stf1_d1980_tractbna_sf. Here’s how you can do it using the sf package in R

# Perform the spatial join
acs2020_ab_tract_sf_eps <- st_join(acs2020_ab_tract_sf, eps_geometry_zcta, join = st_intersects)  

acs2020_ab_tract_sf_eps %>% glimpse()
acs2020_ab_tract_sf_eps %>% class()  

#investigate data structure
# gisjoin uniquely identifies obs in the input dataset
acs2020_ab_tract_sf %>% as.data.frame() %>% 
  select(gisjoin) %>% group_by(gisjoin) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # one observation per gisjoin value

# gisjoin does not uniquely identify obs in the dataset created by st_join with join = st_intersect    
acs2020_ab_tract_sf_eps %>% as.data.frame() %>% 
  select(gisjoin) %>% group_by(gisjoin) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) #

# gisjoin,eps uniquely identifies obs in new dataset
acs2020_ab_tract_sf_eps %>% as.data.frame() %>% 
  select(gisjoin,eps) %>% group_by(gisjoin,eps) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # one observation per zip code  

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
acs2020_ab_tract_sf_eps <- st_transform(x = acs2020_ab_tract_sf_eps, crs = 5070)

# Check the new CRS
st_crs(acs2020_ab_tract_sf_eps)

############## 
# USING GROUP-BY AND SUMMARIZE, CREATE OBJECTS AT THE EPS LEVEL THAT HAVE POPULATION VARIABLES SUMMARIZED 
# MERGE EPS-LEVEL CHARACTERISTIC DATA TO DATAFRAME THAT HAS EPS GEOMETRY; THEN DO SOME ANALYSES:           


acs2020_ab_tract_sf_eps %>% glimpse()

#acs2020_a_tract %>% select(-c(ampve001,amp4e001,amp4e002,amp4e003,amp3e010,amp3e011,amp3e020,amp3e021)) %>%  var_label()

acs2020_ab_anal_tract <- acs2020_ab_tract_sf_eps %>% as.data.frame() %>% 
  # RACE/ETHNICITY VARIABLES
  # drop variables you don't need (after running checks)
    # variables you need are in following tables:
      # ampv = total population
      # amp3 = population by hispanic or Latino and not hispanic or latino by race
      # amp4 = Hispanic or Latino Origin
  # total population: the measure of total population ampve001 is always equal to the measure amp3e001
    # amp4e001 always == amp3e001
  # non-hipanic population: amp3e002 always == amp4e002
  # hispanic population: amp3e012 always == amp4e003
  # non-hispanic, two-or more races: amp3e009 always == amp3e010+amp3e011
    # amp3e009: "Estimates: Not Hispanic or Latino: Two or more races"
    # amp3e010: "Estimates: Not Hispanic or Latino: Two or more races: Two races including Some other race"
    # amp3e011: [1] "Estimates: Not Hispanic or Latino: Two or more races: Two races excluding Some other race, and three or more races"
  # hispanic, two or more races: amp3e019 always == amp3e020+amp3e021
    # amp3e019. "Estimates: Hispanic or Latino: Two or more races"
    # amp3e020. "Estimates: Hispanic or Latino: Two or more races: Two races including Some other race"
    # amp3e021. "Estimates: Hispanic or Latino: Two or more races: Two races excluding Some other race, and three or more races"  
  select(-c(ampve001,amp4e001,amp4e002,amp4e003,amp3e010,amp3e011,amp3e020,amp3e021)) %>% 
  # RENAME AND CREATE VARS OF POPULATION BY RACE, ETHNICITY, AND AGE
  rename(
    tot_all = amp3e001,
    nhisp_all = amp3e002,
    hisp_all = amp3e012,
    
    nhisp_white = amp3e003,
    nhisp_black = amp3e004,
    nhisp_native = amp3e005,
    nhisp_asian = amp3e006,
    nhisp_nhpi = amp3e007,
    nhisp_other = amp3e008,
    nhisp_multi = amp3e009,
    
    hisp_white = amp3e013,
    hisp_black = amp3e014,
    hisp_native = amp3e015,
    hisp_asian = amp3e016,
    hisp_nhpi = amp3e017,
    hisp_other = amp3e018,
    hisp_multi = amp3e019,
  ) %>% 
  # la la
  mutate(
    # api vars
    nhisp_api = rowSums(select(., nhisp_asian, nhisp_nhpi), na.rm = TRUE),
    hisp_api =  rowSums(select(., hisp_asian, hisp_nhpi), na.rm = TRUE),
  )

acs2020_ab_anal_tract %>% glimpse()

# create character vector of names of variables to be summed
# remove income variables that should not be summed
sum_vars <- acs2020_ab_anal_tract %>% select(-eps,-geometry,-gisjoin,-state,-statea,-county,-countya,-tracta) %>% names()
sum_vars  

# create eps-level analysis dataset
acs2020_ab_anal_eps <- acs2020_ab_anal_tract %>% 
  group_by(eps) %>% summarize(
    n_tracts = n(),
    # sum vars
    across(.cols = all_of(sum_vars), ~ sum(.x, na.rm = TRUE), .names = "sum_{.col}"),
    
    # median of median household income at tract-level
    #med_inc_house_med = median(inc_house_med, na.rm = TRUE),
    
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
    
  )


  # check race-ethnicity variables temp <- 
  acs2020_ab_anal_eps %>% select(eps,sum_tot_all,pct_nhisp_white,pct_nhisp_black,pct_nhisp_native,pct_nhisp_api,pct_nhisp_other,pct_nhisp_multi,pct_hisp_all) %>% View()
    #acs2020_ab_anal_eps %>% select(eps,sum_nhisp_white,sum_nhisp_black,sum_tot_all,pct_nhisp_white,pct_nhisp_black) %>% View()


