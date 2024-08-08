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

acs2020_a_tract %>% select(matches('^amp\\dm')) %>% glimpse() # this capture the margin of error variables

#START HERE ON MONDAY!!

                           acs2020_a_tract <- 
  
  acs2020_a_tract %>% select(gisjoin,state,statea,county,countya,tracta,starts_with('ampv'),starts_with('amp3'),starts_with('amp4')) %>% glimpse()

