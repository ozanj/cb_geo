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

# can't save shape files in the repo cuz they too big
shape_dir <-   file.path('.','..','cb_geomarket_shape') # main data directory
list.files(path = shape_dir)

eps_data_dir <- file.path(data_dir,'eps_market') # has eps geomarket data and spaical data files
list.files(path = eps_data_dir)

scripts_dir <- file.path('.','scripts') # 
list.files(path = scripts_dir)

#################
# CREATE EPS-LEVEL DATA THAT HAS SHAPEFILE FOR EACH GEOMARKET

# Read in .xls file that has name of geomarket associated with each zip
eps_zip <- read_excel(path = file.path(eps_data_dir,'eps.xls'), sheet = 'Sheet 1', col_names = TRUE) %>% 
  # fix error in eps var (e.g., "FL06" should be "FL 6")
  mutate(eps = if_else(str_sub(eps,3,3)=="0",str_c(str_sub(eps,1,2)," ", str_sub(eps,4,4)),eps))

eps_zip %>% glimpse()
# make sure zip codes are unique in each dataset
eps_zip %>% group_by(zip) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # one observation per zip code  

# get ZCTA shape file from around 2010 from tidycensus function
# note: we want 2010 ZCTAs because the year that guy created eps data was around 2010
zcta_2010 <-  zctas(
  cb = TRUE, 
  year = 2010
)
zcta_2010 %>% glimpse()

# read zip code shape file; file has no holes; even in areas that don't have zip codes
# https://cran.r-project.org/web/packages/sf/vignettes/sf2.html
#zip_lower48 <- st_read(file.path(eps_data_dir,"US_Zip_-_Lower_48.shp"))
#zip_lower48

# merge eps file to zcta file by zip code, to obtain dataset that has geomarket and shape-file for each zcta
eps_zcta <- zcta_2010 %>% left_join(y = eps_zip, by = c('ZCTA5' = 'zip'))
eps_zcta %>% glimpse()

# 6 zcta's from zcta_2010 that don't have a match from dataframe eps_zip
eps_zcta %>% filter(is.na(eps))
# 8266 observations from eps_zip that don't have a match from dataframe zcta_2010
# To do: need to investigate why these zip codes are not in the zcta file
eps_zcta_anti <- eps_zip %>% anti_join(y = zcta_2010, by = c('zip' = 'ZCTA5')) %>% arrange(eps,zip)
rm(eps_zcta_anti)

# merge eps file to lower48 zip code file by zip code, to obtain dataset that has geomarket and shape-file for each zip-code       
#eps_zip48 <- zip_lower48 %>% left_join(y = eps_zip, by = c('ZIP' = 'zip'))

# zips from zip_lower48 that don't have a match from dataframe eps_zip
#eps_zip48 %>% filter(is.na(eps)) # 41 obs

# observations from eps_zip that don't have a match from dataframe zcta_2010
# To do: need to investigate why these zip codes are not in the zcta file
#eps_zip48_anti <- eps_zip %>% anti_join(y = zip_lower48, by = c('zip' = 'ZIP')) %>% arrange(eps,zip)
#rm(eps_zip48_anti)


# create dataset that has one observation per geomarket by using aggregate to "dissolve" zip codes into geomarkets
# zcta      
eps_geometry_zcta <- eps_zcta %>% select(eps, geometry) %>% aggregate(by = list(eps_zcta$eps), FUN = identity) %>% # takes a long time; should it be FUN = mean? 
  select(-eps) %>% rename(eps = Group.1)

eps_geometry_zcta %>% glimpse()

# zip lower 48
#eps_geometry_zip48 <- eps_zip48 %>% select(eps, geometry) %>% aggregate(by = list(eps_zip48$eps), FUN = identity) %>% # takes a long time; should it be FUN = mean? 
#  select(-eps) %>% rename(eps = Group.1)

#eps_geometry_zip48 %>% glimpse()

#eps_geometry_zcta %>% ggplot() + geom_sf()
#eps_geometry_zip48 %>% ggplot() + geom_sf()

eps_geometry_zcta %>% glimpse()
#eps_geometry_zip48 %>% glimpse()

# save to hard drive
file.path(eps_data_dir,'eps_shapes.rds')

save(eps_geometry_zcta, file = file.path(eps_data_dir, 'eps_shapes.RData'))

# command to load data frame
#load(file.path(eps_data_dir, 'eps_shapes.RData'))



