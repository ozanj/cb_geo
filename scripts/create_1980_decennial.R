################################################################################
## [ PROJ ] < College Board Geomarket >
## [ FILE ] < create_1980_decennial.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 6/27/2024
## [ DESC ] < Get 1980 decennial census data, shape files, and (hopefully) merge w/ eps geomarkets >
################################################################################

# OTHER VARIABLES TO CREATE
  # PERCENT LIVING IN POVERTY
  # PERCENT RECEIVING PUBLIC ASSISTANCE

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

getwd()

data_dir <- file.path('.','data') # main data directory
  list.files(path = data_dir)

d1980_data_dir <-   file.path('.',data_dir,'1980_decennial') # main data directory
  list.files(path = d1980_data_dir)

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
  
####### READ IN DATA

# PRE-2004 MSA CODES
  msa_pre_2004 <- read_csv(file.path(d1980_data_dir, 'pre-2004-msa-codes-csv.csv'), col_names = TRUE, skip=0) %>% 
    mutate(area_fips = str_sub(area_fips,start=2L, end=-1L))
     
# 1980 DECENNIAL CENSUS DATA, POPULATION BY RACE, ETHNICITY, AND AGE

# Read the first row to get the variable names
  variable_names <- read_csv(file.path(d1980_data_dir, 'nhgis0001_ds104_1980_tract.csv'), col_names = FALSE, n_max = 1) %>% as.character()
  
# Read the second row to get the variable labels
  variable_labels <- read_csv(file.path(d1980_data_dir, 'nhgis0001_ds104_1980_tract.csv'), col_names = FALSE, skip = 1, n_max = 1) %>% as.character()
  variable_labels
# Read the data starting from the third row using the first row as column names
  stf1_d1980_tract <- read_csv(file.path(d1980_data_dir, 'nhgis0001_ds104_1980_tract.csv'), col_names = variable_names, skip = 2)

# Add variable labels using the var_label function from labelled package
  var_label(stf1_d1980_tract) <- variable_labels
  rm(variable_labels,variable_names)
    
  # replace upper case w/ lower case variable names
  names(stf1_d1980_tract) <- stf1_d1980_tract %>% names %>% tolower()
  
# 1980 DECENNIAL CENSUS DATA, EDUCATION AND INCOME
  
  # Read the first row to get the variable names
  stf3_variable_names <- read_csv(file.path(d1980_data_dir, 'nhgis0005_ds107_1980_tract.csv'), col_names = FALSE, n_max = 1) %>% as.character()
  
  # Read the second row to get the variable labels
  stf3_variable_labels <- read_csv(file.path(d1980_data_dir, 'nhgis0005_ds107_1980_tract.csv'), col_names = FALSE, skip = 1, n_max = 1) %>% as.character()
  stf3_variable_labels
  # Read the data starting from the third row using the first row as column names
  stf3_d1980_tract <- read_csv(file.path(d1980_data_dir, 'nhgis0005_ds107_1980_tract.csv'), col_names = stf3_variable_names, skip = 2)
  
  # Add variable labels using the var_label function from labelled package
  var_label(stf3_d1980_tract) <- stf3_variable_labels
  
  # replace upper case w/ lower case variable names
    names(stf3_d1980_tract) <- stf3_d1980_tract %>% names %>% tolower()
  
  rm(stf3_variable_labels,stf3_variable_names)
  
  stf3_d1980_tract %>% glimpse()
  stf3_d1980_tract %>% var_label()
  
  # drop sft3 population by race, ethnicity, and age variables because you have these in sf1
  stf3_d1980_tract <- stf3_d1980_tract %>% 
    select(
      -c(all_of(starts_with('di6')),all_of(starts_with('dfb')),all_of(starts_with('dfn')),
         all_of(starts_with('dgd')),all_of(starts_with('dge')),all_of(starts_with('dgi')),
         all_of(starts_with('dgk')))
    ) 
    # c(all_of(starts_with('dgo')),all_of(starts_with('dg2')),all_of(starts_with('dec')))
    # vars you might keep to use as denominators
      # Table 1:     Persons
      # Universe:    Persons
      # Source code: NT1A
      # NHGIS code:  DGO
      # DGO001:      Total
      
      # Table 2:     100-percent Count of Persons
      # Universe:    100-Percent Count of Persons
      # Source code: NT3
      # NHGIS code:  DG2
      # DG2001:      Total
      
      # Table 4:     Households
      # Universe:    Households
      # Source code: NT10
      # NHGIS code:  DEC
      # DEC001:      Total
  
    #stf3_d1980_tract %>% select(c(all_of(starts_with('dgo')),all_of(starts_with('dg2')),all_of(starts_with('dec')))) %>% var_label()
  
###############
  
# census-tract level variables about population by race, ethnicity, and age

# Verify the data and labels
  print(head(stf1_d1980_tract))
  stf1_d1980_tract %>% glimpse()
  
# figure out data structure

  #$GISJOIN; this variable uniquely identifies observations
  #[1] "GIS Join Match Code"
  stf1_d1980_tract %>% group_by(gisjoin) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group)
  
  #$YEAR
  #[1] "Data File Year"
  stf1_d1980_tract %>% count(year) # always 1980
  
  #AREANAME; uniquely identifies observations
  stf1_d1980_tract %>% select(areaname) %>% print(n=100) 
  stf1_d1980_tract %>% group_by(areaname) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group)
  
  #$TRACTA; does not uniquely identify obs!
  #[1] "Census Tract Code"
  # tracta does not uniquely identify obs
  stf1_d1980_tract %>% group_by(tracta) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group)
  stf1_d1980_tract %>% select(tracta) %>% print(n=100) 
  
  # statea, tracta does not uniquely identify obs
  stf1_d1980_tract %>% group_by(statea, tracta) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # does not uniquely identify
  
  # statea, county, tracta uniquely identify obs
  stf1_d1980_tract %>% group_by(statea, countya, tracta) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # does not uniquely identify

# MERGE STF1 AND STF3
  
  stf1_d1980_tract %>% glimpse()
  
  stf1_d1980_tract %>% select(-starts_with('supflg')) %>% glimpse()

  # create character vector of vars to drop
  drop_vars <- c("year", "fstatus", "regiona", "divisiona", "state", "statea", "smsaa", "county", 
                      "countya", "cty_suba", "placea", "tracta", "blck_grpa", "enumdista", "scsaa", "urb_areaa", 
                      "cda", "aianhha", "mcdseqno", "zipa", "sea", "uatype", "placdesc", "cbd", "indsubr", "areaname")
    # drop_vars
  
  # merge
  d1980_stf1_stf3_tract <- stf1_d1980_tract %>% select(-starts_with('supflg')) %>% 
    inner_join(
      y=stf3_d1980_tract %>% select(-all_of(drop_vars),-starts_with('supflg')),
      by = c('gisjoin')
    )
  d1980_stf1_stf3_tract %>% glimpse()
  rm(drop_vars)
  
  # remove separate datasets
  rm(stf1_d1980_tract,stf3_d1980_tract)
  
######## READ IN SHAPE FILE FOR 1980 CENSUS TRACTS
  
  # what I did previously
  # read zip code shape file; file has no holes; even in areas that don't have zip codes
  # https://cran.r-project.org/web/packages/sf/vignettes/sf2.html
  #zip_lower48 <- st_read(file.path(eps_data_dir,"US_Zip_-_Lower_48.shp"))
  #zip_lower48
  
  list.files(path = file.path(shape_dir,'nhgis0001_shape','nhgis0001_shapefile_tl2000_us_tract_1980'))
  file.path(shape_dir,'nhgis0001_shape','nhgis0001_shapefile_tl2000_us_tract_1980','US_tract_1980.shp')

  # Read the tract shapefile using sf
  d1980_tract_sf <- st_read(file.path(shape_dir,'nhgis0001_shape','nhgis0001_shapefile_tl2000_us_tract_1980','US_tract_1980.shp'))  
  names(d1980_tract_sf) <- d1980_tract_sf %>% names %>% tolower()
  
  d1980_tract_sf %>% glimpse()
  d1980_tract_sf %>% class() # sf, data frame
  d1980_tract_sf %>% st_crs
  
  # GISJOIN uniquely identifies obs  
  d1980_tract_sf %>% select(gisjoin) %>% as.data.frame() %>%  group_by(gisjoin) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # does not uniquey identify
  
  # read the bna shape file; 3292 obs
  d1980_bna_sf <- st_read(file.path(shape_dir,'nhgis0001_shape','nhgis0001_shapefile_tl2000_us_tract_1980','US_bna_1980.shp'))  
  names(d1980_bna_sf) <- d1980_bna_sf %>% names %>% tolower()
  
  d1980_bna_sf %>% glimpse()
  d1980_bna_sf %>% st_crs
  
  # read the uscountry shape file; 3292 obs
  #d1980_tractcounty_sf <- st_read(file.path(shape_dir,'nhgis0001_shape','nhgis0001_shapefile_tl2000_us_tract_1980','US_tractcounty_1980.shp'))  
  #d1980_tractcounty_sf %>% glimpse()
  
############# MERGE CENSUS TRACT DATA TO SHAPEFIL DATA
  
  d1980_tract_sf %>% names
  d1980_bna_sf %>% names

  # inner join between stf1_d1980_tract and d1980_tract_sf
  d1980_stf1_stf3_tract_sf <- d1980_stf1_stf3_tract %>% 
    inner_join(y = d1980_tract_sf %>% select(gisjoin, geometry), by = c('gisjoin')) 
  d1980_stf1_stf3_tract_sf %>% count
  d1980_stf1_stf3_tract_sf %>% st_crs
  
  # inner join between stf1_d1980_tract and d1980_bna_sf
  d1980_stf1_stf3_bna_sf <- d1980_stf1_stf3_tract %>% 
    inner_join(y = d1980_bna_sf %>% select(gisjoin, bna80, geometry), by = c('gisjoin'))
  
  d1980_stf1_stf3_bna_sf %>% count
  
  # append stf1_d1980_tract_sf + stf1_d1980_bna_sf
  d1980_stf1_stf3_tractbna_sf <- bind_rows(d1980_stf1_stf3_tract_sf, d1980_stf1_stf3_bna_sf) %>% 
    arrange(gisjoin)
  
  # assert data structure
  # one obs per value of gisjoin
  d1980_stf1_stf3_tractbna_sf %>% select(gisjoin) %>% as.data.frame() %>% group_by(gisjoin) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # does not uniquey identify
  
  nrow(d1980_stf1_stf3_tract) - nrow(d1980_stf1_stf3_tractbna_sf) # 530 obs that don't have a merge w/ either _tract_ or _bna_ data frames with geometry data
  
  # isolate those 530 observations
  temp <- d1980_stf1_stf3_tract %>% anti_join(y = d1980_stf1_stf3_tractbna_sf, by = c('gisjoin')) %>% 
    mutate(no_geom = 1)
  
  # append those 530 observations to stf1_d1980_tractbna_sf
  d1980_stf1_stf3_tractbna_sf <- bind_rows(d1980_stf1_stf3_tractbna_sf, temp) %>% 
    mutate(
      bna_geom = if_else(is.na(bna80),0,1),
      no_geom = if_else(is.na(no_geom)==1,0,1)      
    ) %>% arrange(gisjoin) 
  rm(temp)

  d1980_stf1_stf3_tractbna_sf %>% count(no_geom)
  d1980_stf1_stf3_tractbna_sf %>% count(bna_geom)  
  
  # Convert to sf object
  d1980_stf1_stf3_tractbna_sf %>% class()
  d1980_stf1_stf3_tractbna_sf <- st_as_sf(d1980_stf1_stf3_tractbna_sf, sf_column_name = "geometry")
  d1980_stf1_stf3_tractbna_sf %>% class()
  d1980_stf1_stf3_tractbna_sf %>% st_crs()
  
  # assert/Assign the CRS
  st_crs(d1980_stf1_stf3_tractbna_sf) == st_crs(d1980_tract_sf) # crs is the same
  
  # INVESTIGATIONS
  # check data structure
    # one obs per gisjoin value
  d1980_stf1_stf3_tractbna_sf %>% select(gisjoin) %>% as.data.frame() %>%  group_by(gisjoin) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # does not uniquey identify
  
  d1980_stf1_stf3_tractbna_sf %>% filter(no_geom==1) %>% count() # 530 obs; checked and all have missing geometry
  
  # investigate the observations that use bna geometry rather than tract
  d1980_stf1_stf3_tractbna_sf %>% filter(is.na(no_geom)) %>% 
    filter(bna_geom ==1) %>% count(state) %>% print(n=100)
  
  #d1980_stf1_stf3_tractbna_sf %>% filter(no_geom==0) %>% group_by(bna_geom) %>% summarize(
  #    n= n(),
  #    med_pop = median(c7l001, na.rm = TRUE),  
  #    mean_pop = mean(c7l001, na.rm = TRUE),
  #  )
  
  # investigate the obs that have no geometry
  d1980_stf1_stf3_tractbna_sf %>% filter(no_geom ==1) %>% count(state) %>% print(n=100)
    
    # shows that obs w/ no geometry are much smaller in terms of total population; which is reassuring
  #d1980_stf1_stf3_tractbna_sf %>% group_by(no_geom) %>% summarize(
  #  n= n(),
  #  med_pop = median(c7l001, na.rm = TRUE),  
  #  mean_pop = mean(c7l001, na.rm = TRUE))
  
  # remove dataframes you no longer need
    rm(d1980_bna_sf,d1980_tract_sf)
    rm(d1980_stf1_stf3_tract_sf,d1980_stf1_stf3_bna_sf)

####################
####################
# ASSIGN EPS CODE TO EACH CENSUS TRACT
    
  # inputs you have 
    # have sf object with tract-level data containing the geometry for each tract and variables with counts of population in each tract
    # have sf object of eps-level data with one obs per EPS code and geometry for each EPS
  
  # desired output
    # or sf object that has one obs per tract and for each tract we know the eps code of that tract
  
    # st_crs function retreives coordinate reference system for an object
    st_crs(eps_geometry_zcta) == st_crs(d1980_stf1_stf3_tractbna_sf) # false
    
    eps_geometry_zcta %>% st_crs
    d1980_stf1_stf3_tractbna_sf %>% st_crs
    
    # transform so that they have the same CRS system
      # what chat gpt says: The st_transform() function is used to reproject the coordinates of your
        # spatial object from one CRS to another, ensuring that the geometries are correctly 
        # transformed to the new coordinate system. In contrast, st_crs() is used to either set
        #or retrieve the CRS of an object without transforming the coordinates.
    
    # Transform eps_geometry_zcta to match the CRS of stf1_d1980_tractbna_sf eps_geometry_zcta
    #d1980_stf1_stf3_tractbna_sf <- st_transform(d1980_stf1_stf3_tractbna_sf, st_crs(eps_geometry_zcta))
    
    eps_geometry_zcta <- st_transform(eps_geometry_zcta, st_crs(d1980_stf1_stf3_tractbna_sf))
    st_crs(eps_geometry_zcta) == st_crs(d1980_stf1_stf3_tractbna_sf)
    
    eps_geometry_zcta %>% glimpse()

  # desired output
    # or sf object that has one obs per tract and for each tract we know the eps code of that tract
    # what my homie chatgpt says:
    #You can perform a spatial join to assign the eps code from eps_geometry_zcta to each observation in stf1_d1980_tractbna_sf. Here’s how you can do it using the sf package in R
    
    # Perform the spatial join
    d1980_stf1_stf3_tract_sf_eps <- st_join(d1980_stf1_stf3_tractbna_sf, eps_geometry_zcta, join = st_intersects)  
    
    d1980_stf1_stf3_tract_sf_eps %>% glimpse()
    d1980_stf1_stf3_tract_sf_eps %>% class()
    
    #investigate data structure
      # gisjoin uniquely identifies obs in the input dataset
    d1980_stf1_stf3_tractbna_sf %>% as.data.frame() %>% 
          select(gisjoin) %>% group_by(gisjoin) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # one observation per zip code  

      # gisjoin does not uniquely identify obs in the dataset created by st_join with join = st_intersect    
      d1980_stf1_stf3_tract_sf_eps %>% as.data.frame() %>% 
        select(gisjoin) %>% group_by(gisjoin) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # one observation per zip code  
      
      # gisjoin,eps uniquely identifies obs in new dataset
      d1980_stf1_stf3_tract_sf_eps %>% as.data.frame() %>% 
        select(gisjoin,eps) %>% group_by(gisjoin,eps) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # one observation per zip code  
      
    # result: if the geometry of a census tract overlaps with more than one eps geometry, that census tract appears multiple times
    # what chat gpt says    
      # Yes, in the solution you've implemented, a given census tract will appear once if it is wholly contained within a single geomarket and will appear multiple times if it intersects with multiple geomarkets. Each appearance represents an intersection with a different geomarket.
      #Wholly Contained Tracts: If a census tract is entirely within a single geomarket, it will appear once in the joined dataframe with that geomarket's eps code.
      #Intersecting Tracts: If a census tract intersects with multiple geomarkets, it will appear once for each geomarket it intersects with, each time with the respective geomarket's eps code.
      #This results in some census tracts being duplicated in the joined dataframe, reflecting the fact that they span more than one geomarket.
    
    # merge smsaa names
      d1980_stf1_stf3_tract_sf_eps <- d1980_stf1_stf3_tract_sf_eps %>% left_join(y = msa_pre_2004, by = c('smsaa' = 'area_fips'))
      
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
      d1980_stf1_stf3_tract_sf_eps <- st_transform(x = d1980_stf1_stf3_tract_sf_eps, crs = 5070)
      
      # Check the new CRS
      st_crs(d1980_stf1_stf3_tract_sf_eps)
      
# WHAT IF YOU WANT EACH CENSUS TRACT TO BE ASSIGNED TO ONE AND ONLY ONE EPS MARKET? (FROM CHATGPT)

      #Conceptually, if you want each census tract to be assigned to one and only one geomarket/eps code, you have a few options. Here are some approaches to consider:
        
      #1. **Dominant Area Assignment**:
      # - **Description**: Assign each census tract to the geomarket/eps code that covers the largest area within the tract.
      #- **Method**: Calculate the intersection area of each census tract with each geomarket. Assign the tract to the geomarket with the largest intersection area.
      
      #2. **Centroid Assignment**:
      #- **Description**: Assign each census tract to the geomarket/eps code that contains the centroid of the tract.
      #- **Method**: Calculate the centroid of each census tract and then find the geomarket that contains this centroid. Assign the tract to that geomarket.
      
      #3. **Priority or Hierarchical Rules**:
      #  - **Description**: Establish a set of rules or priorities to resolve overlaps. This could be based on predefined criteria such as population, geographic priority, or other domain-specific rules.
      #- **Method**: Define and apply the rules to assign each tract to a single geomarket/eps code.
      
      #4. **Random or First Encounter Assignment**:
      #  - **Description**: Assign each census tract to the first geomarket/eps code it encounters in a list or a random selection if there are multiple overlaps.
      #- **Method**: Iterate through the overlapping geomarkets and assign the tract to the first one encountered or select one randomly.
      
      ### Practical Implementation:
      
      #1. **Dominant Area Assignment**:
        
      #  ```r
      #library(dplyr)
      #library(sf)
      
      # Calculate intersection areas
      #intersections <- st_intersection(stf1_d1980_tractbna_sf, eps_geometry_zcta) %>%
      #  mutate(area = st_area(.)) %>%
      #  group_by(gisjoin) %>%
      #  top_n(1, wt = area) %>%
      #  ungroup() %>%
      #  select(gisjoin, eps)
      
      # Join with original data
      #stf1_d1980_tractbna_sf_dominant <- stf1_d1980_tractbna_sf %>%
      #  left_join(intersections, by = "gisjoin")
      #```
      
      #2. **Centroid Assignment**:
        
      #  ```r
      # Calculate centroids
      #centroids <- st_centroid(stf1_d1980_tractbna_sf)
      
      # Join centroids with geomarkets
      #stf1_d1980_tractbna_sf_centroid <- stf1_d1980_tractbna_sf %>%
      #  mutate(centroid = st_centroid(geometry)) %>%
      #  st_join(eps_geometry_zcta, join = st_within) %>%
      #  select(-centroid)
      #```
      
      #3. **Priority or Hierarchical Rules**:
      #  - This approach would require defining and implementing your specific rules for assigning geomarkets.
      
      #4. **Random or First Encounter Assignment**:
        
      #  ```r
      # Assuming stf1_d1980_tractbna_sf_with_eps already has multiple rows per tract
      #stf1_d1980_tractbna_sf_random <- stf1_d1980_tractbna_sf_with_eps %>%
      #  group_by(gisjoin) %>%
      #  slice_sample(n = 1) %>%
      #  ungroup()
      #```
      
      ### Summary
      #Each approach has its advantages and potential drawbacks. The best method depends on the specifics of your project and data, including any priorities or rules you want to enforce. The centroid assignment is straightforward and often used, while the dominant area assignment ensures that the largest overlapping geomarket is chosen. Hierarchical rules offer flexibility but require more effort to define and implement.      
      
      # skinner approach: partially assign tracts based on spatial overlap
        # That’s where the weights came in. I’m trying to remember the specific direction, but I believe what we did was assign values to a HOLC neighborhood as a weighted average of the spatial overlap. If a neighborhood was comprised 30/70 of two tracts then it was .3 x tract 1 + .7 x tract 2
        # see notes on how to implement this This approach, known as "proportional assignment
        # notes are in this google drive doc: https://docs.google.com/document/d/1tzozxhlrzPz4MtIQhhxB4gC24Pt8FCGwrQgxheW9n_c/edit
          # data_methods_to_do_started_6_26_2024
      
# NEXT STEPS: 
  # WITHOUT DISSOLVING TRACT-LEVEL DATA, CREATE MAPS THAT LOOK AT OVERLAP IN PARTICULAR METRO AREAS, JUST TO SEE WHAT THIS LOOKS LIKE;
  # USING GROUP-BY AND SUMMARIZE, CREATE OBJECTS AT THE EPS LEVEL THAT HAVE POPULATION VARIABLES SUMMARIZED 
    # MERGE EPS-LEVEL CHARACTERISTIC DATA TO DATAFRAME THAT HAS EPS GEOMETRY; THEN DO SOME ANALYSES:
      # VISUAL ANALYSES INVOLVING CREATING MAPS WITH CHARACTERISTICS OVERLAYED
      # TABULAR ANALYSES OF CHARACTERISTICS
  # AT SOME POINT NEXT WEEK, TRY TO IMPLEMENT THE PROPORTIONAL ASSIGNMENT SOLUTION
      # THEN SEE HOW THE RESULTS DIFFER FROM ABOVE RESULTS

######################
  # WITHOUT DISSOLVING TRACT-LEVEL DATA, CREATE MAPS THAT LOOK AT OVERLAP IN PARTICULAR METRO AREAS, JUST TO SEE WHAT THIS LOOKS LIKE;

      # what's the difference between metropolitan statistical areas, core-based statistical areas, and combined statistical areas
        # MSAs are regions that consist of a core urban area with a population of at least 50,000, along with the adjacent areas that have a high degree of social and economic integration with the core, as measured by commuting patterns.
      
        # Core-Based Statistical Areas (CBSAs)
          # CBSAs encompass both MSAs and Micropolitan Statistical Areas (μSAs).
          # Metropolitan Statistical Areas (MSAs): Areas with an urban core of 50,000 or more people.
          # Micropolitan Statistical Areas (μSAs): Areas with an urban core of at least 10,000 but less than 50,000 people, plus adjacent territories with high integration with the core.
      
      #Combined Statistical Areas (CSAs)
        #CSAs are composed of two or more adjacent CBSAs that have substantial employment interchange. They provide a larger regional framework for analysis.
        #Components: Multiple CBSAs (both MSAs and μSAs) that are linked by significant commuting ties.

      cbsa <- core_based_statistical_areas(cb = TRUE, year = 2010) %>% st_transform(st_crs(d1980_stf1_stf3_tract_sf_eps)) # not available prior to 2010
      
      cbsa_la <- cbsa %>% filter(str_detect(NAME, "Los Angeles")) 
      st_crs(d1980_stf1_stf3_tract_sf_eps) == st_crs(cbsa)
      
      # tracts
      tracts_la <- d1980_stf1_stf3_tract_sf_eps %>% filter(smsaa == '4480')
      
      # los angeles eps
      eps_la <- eps_geometry_zcta %>% filter(eps %in% c('CA14','CA15','CA16','CA17','CA18','CA19','CA20','CA21','CA22','CA23','CA24','CA25','CA26')) 
      
      ggplot(data = tracts_la) + geom_sf(fill = "white", color = "grey")
    
      # overlap multiple sf objects
      ggplot() + geom_sf(data = tracts_la, fill = "white", color = "grey") +
        geom_sf(data = cbsa_la, fill = NA, color = "green") + 
        geom_sf(data = eps_la, fill = NA, color = "red") + geom_sf_label(data=eps_la, aes(label = eps))  + theme_void()
      
      ggplot() + 
        geom_sf(data = tracts_la, fill = "white", color = "grey") +
        geom_sf(data = cbsa_la, fill = NA, color = "green") + 
        geom_sf(data = eps_la, fill = NA, color = "red") + 
        geom_sf_label(data = eps_la, aes(label = eps), fill = NA, alpha = 0.5, label.size = NA) +  # Transparent label background
        theme_void()      
      # Transform eps_geometry_zcta to match the CRS of stf1_d1980_tractbna_sf
      
############## 
# USING GROUP-BY AND SUMMARIZE, CREATE OBJECTS AT THE EPS LEVEL THAT HAVE POPULATION VARIABLES SUMMARIZED 
  # MERGE EPS-LEVEL CHARACTERISTIC DATA TO DATAFRAME THAT HAS EPS GEOMETRY; THEN DO SOME ANALYSES:
    # VISUAL ANALYSES INVOLVING CREATING MAPS WITH CHARACTERISTICS OVERLAYED
    # TABULAR ANALYSES OF CHARACTERISTICS      

  # which race vars do I want?
  
  # available age vars
    # under 5; 5-17; 18-64; 65+
  # which age vars do I want;
    # all; 5-17; 18-64

  # race vars
    # white, black, API, native, other
      
  d1980_stf1_stf3_tract_sf_eps %>% glimpse()
      
  d1980_stf1f3_anal_tract <- d1980_stf1_stf3_tract_sf_eps %>% as.data.frame() %>% 
    # RENAME AND CREATE VARS OF POPULATION BY RACE, ETHNICITY, AND AGE
    rename(
      tot_ageall = c7l001,
      white_ageall = c9d001,
      black_ageall = c9d002,
      other_ageall = c9d015,
      nonhisp_ageall = c9e001,
      hisp_ageall = c9f001,
      hispwhite_ageall = c9g001,
      hispblack_ageall = c9g002,
      hispapi_ageall = c9g003,
      hispother_ageall = c9g004,
      tot_agelt5 = c7b001,
      tot_age517 = c7b002,
      tot_age1864 = c7b003,
      tot_agegt65 = c7b004,
      white_agelt5 = c7c001,
      white_age517 = c7c002,
      white_age1864 = c7c003,
      white_agegt65 = c7c004,
      black_agelt5 = c7c005,
      black_age517 = c7c006,
      black_age1864 = c7c007,
      black_agegt65 = c7c008,    
      native_agelt5 = c7c009,
      native_age517 = c7c010,
      native_age1864 = c7c011,
      native_agegt65 = c7c012,    
      api_agelt5 = c7c013,
      api_age517 = c7c014,
      api_age1864 = c7c015,
      api_agegt65 = c7c016,    
      other_agelt5 = c7c017,
      other_age517 = c7c018,
      other_age1864 = c7c019,
      other_agegt65 = c7c020,
      hisp_agelt5 = c7d001,
      hisp_age517 = c7d002,
      hisp_age1864 = c7d003,
      hisp_agegt65 = c7d004,
      hispwhite_agelt5 = c7e001,
      hispwhite_age517 = c7e002,
      hispwhite_age1864 = c7e003,
      hispwhite_agegt65 = c7e004,
      hispblack_agelt5 = c7e005,
      hispblack_age517 = c7e006,
      hispblack_age1864 = c7e007,
      hispblack_agegt65 = c7e008,    
      hispother_agelt5 = c7e009,
      hispother_age517 = c7e010,
      hispother_age1864 = c7e011,
      hispother_agegt65 = c7e012,    
    ) %>% 
    mutate(
      native_ageall = ifelse(is.na(c9d003) & is.na(c9d004) & is.na(c9d005), NA, 
        rowSums(select(., c9d003, c9d004, c9d005), na.rm = TRUE)),
      api_ageall = ifelse(
          is.na(c9d006) & is.na(c9d007) & is.na(c9d008) & is.na(c9d009) & is.na(c9d010) & is.na(c9d011) & is.na(c9d012) & is.na(c9d013) & is.na(c9d014), NA, 
          rowSums(select(., c9d006, c9d007, c9d008, c9d009, c9d010, c9d011, c9d012, c9d013, c9d014), na.rm = TRUE)
        ),
      #hisp_ageall = ifelse(is.na(c9e002) & is.na(c9e003) & is.na(c9e004) & is.na(c9e005),NA, rowSums(select(., c9e002, c9e003, c9e004, c9e005), na.rm = TRUE)),
      nonhisp_age517 = tot_age517 - hisp_age517,
      nonhisp_age1864 = tot_age1864 - hisp_age1864,
    ) %>% 
    select(-starts_with('c9d'),-starts_with('c9e')) %>% 
    # EDUCATIONAL ATTAINMENT VARIABLES (AGE 25+)
    mutate(
      # all race
      ed_lths_all = if_else(is.na(dhm001) & is.na(dhm002), NA, 
                            rowSums(select(., dhm001, dhm002), na.rm = TRUE)), # lths
      ed_tot_all = if_else(is.na(dhm001) & is.na(dhm002) & is.na(dhm003) & is.na(dhm004) & is.na(dhm005), NA, 
                           rowSums(select(., dhm001, dhm002, dhm003, dhm004, dhm005), na.rm = TRUE)), # all in group
      # white
      ed_lths_white = if_else(is.na(dhn001) & is.na(dhn002), NA, 
                              rowSums(select(., dhn001, dhn002), na.rm = TRUE)), # lths
      ed_tot_white = if_else(is.na(dhn001) & is.na(dhn002) & is.na(dhn003) & is.na(dhn004) & is.na(dhn005), NA, 
                             rowSums(select(., dhn001, dhn002, dhn003, dhn004, dhn005), na.rm = TRUE)), # all in group    
      # black
      ed_lths_black = if_else(is.na(dhn006) & is.na(dhn007), NA, 
                              rowSums(select(., dhn006, dhn007), na.rm = TRUE)), # lths
      ed_tot_black = if_else(is.na(dhn006) & is.na(dhn007) & is.na(dhn008) & is.na(dhn009) & is.na(dhn010), NA, 
                             rowSums(select(., dhn006, dhn007, dhn008, dhn009, dhn010), na.rm = TRUE)), # all in group
      # American Indian, Eskimo, and Aleut
      ed_lths_native = if_else(is.na(dhn011) & is.na(dhn012), NA, 
                               rowSums(select(., dhn011, dhn012), na.rm = TRUE)), # lths
      ed_tot_native = if_else(is.na(dhn011) & is.na(dhn012) & is.na(dhn013) & is.na(dhn014) & is.na(dhn015), NA, 
                              rowSums(select(., dhn011, dhn012, dhn013, dhn014, dhn015), na.rm = TRUE)), # all in group
      # Asian and Pacific Islander
      ed_lths_api = if_else(is.na(dhn016) & is.na(dhn017), NA, 
                            rowSums(select(., dhn016, dhn017), na.rm = TRUE)), # lths
      ed_tot_api = if_else(is.na(dhn016) & is.na(dhn017) & is.na(dhn018) & is.na(dhn019) & is.na(dhn020), NA, 
                           rowSums(select(., dhn016, dhn017, dhn018, dhn019, dhn020), na.rm = TRUE)), # all in group
      # Other
      ed_lths_other = if_else(is.na(dhn021) & is.na(dhn022), NA, 
                              rowSums(select(., dhn021, dhn022), na.rm = TRUE)), # lths
      ed_tot_other = if_else(is.na(dhn021) & is.na(dhn022) & is.na(dhn023) & is.na(dhn024) & is.na(dhn025), NA, 
                             rowSums(select(., dhn021, dhn022, dhn023, dhn024, dhn025), na.rm = TRUE)), # all in group
      # hisp
      ed_lths_hisp = if_else(is.na(dho001) & is.na(dho002), NA, 
                             rowSums(select(., dho001, dho002), na.rm = TRUE)), # lths
      ed_tot_hisp = if_else(is.na(dho001) & is.na(dho002) & is.na(dho003) & is.na(dho004) & is.na(dho005), NA, 
                            rowSums(select(., dho001, dho002, dho003, dho004, dho005), na.rm = TRUE)), # all in group
    ) %>% rename(
      # education, all race
      ed_hs_all = dhm003,
      ed_ltba_all = dhm004,
      ed_ba_all = dhm005,
      # education, white
      ed_hs_white = dhn003,
      ed_ltba_white = dhn004,
      ed_ba_white = dhn005,
      # education, black
      ed_hs_black = dhn008,
      ed_ltba_black = dhn009,
      ed_ba_black = dhn010,
      # education, American Indian, Eskimo, and Aleut
      ed_hs_native = dhn013,
      ed_ltba_native = dhn014,
      ed_ba_native = dhn015,
      # education, Asian and Pacific Islander
      ed_hs_api = dhn018,
      ed_ltba_api = dhn019,
      ed_ba_api = dhn020,
      # education, Other
      ed_hs_other = dhn023,
      ed_ltba_other = dhn024,
      ed_ba_other = dhn025,
      # education, hisp
      ed_hs_hisp = dho003,
      ed_ltba_hisp = dho004,
      ed_ba_hisp = dho005,  
    ) %>% select(-c(dhm001, dhm002, dhn001, dhn002, dhn006, dhn007, dhn011, dhn012, dhn016, dhn017, dhn021, dhn022, dho001, dho002)) %>%
      # checking output variables against input variables
      # %>% select(areaname,dho001,dho002,ed_lths_hisp,ed_hs_hisp,ed_ltba_hisp,ed_ba_hisp,ed_tot_hisp) %>% print(n=100) # all races
      # %>% filter(statea == "06") %>% select(areaname, dhn021, dhn022, ed_lths_other, ed_hs_other, ed_ltba_other, ed_ba_other, ed_tot_other) %>% print(n = 100) 
      # filter(statea == "06") %>% select(areaname, dhn016, dhn017, ed_lths_api, ed_hs_api, ed_ltba_api, ed_ba_api, ed_tot_api) %>% print(n = 100)
      # filter(statea == "06") %>% select(areaname, dhn011, dhn012, ed_lths_native, ed_hs_native, ed_ltba_native, ed_ba_native, ed_tot_native) %>% print(n = 100) 
      # %>% filter(statea=="06") %>% select(areaname,dhn006,dhn007,ed_lths_black,ed_hs_black,ed_ltba_black,ed_ba_black,ed_tot_black) %>% print(n=100) # black
      # %>% filter(statea=="06") %>% select(areaname,dhn001,dhn002,ed_lths_white,ed_hs_white,ed_ltba_white,ed_ba_white,ed_tot_white) %>% print(n=100) # white
      # %>% select(areaname,dhm001,dhm002,ed_lths_all,ed_hs_all,ed_ltba_all,ed_ba_all,ed_tot_all) %>% print(n=100) # all races
  mutate(
  # INCOME VARIABLES
    # different income variables to create
      # median (or mean) of median income
        # input var = median household income, varname = inc_house_med_all
      # mean (or median) income
        # input vars
          # aggregate household income in the census tract, varname = inc_house_agg_all
          # total number of households in the census tract
            # households_tot: input var but set to NA if tract says zero aggregate household income
            # households_tot_calc: input vars = number of households answering income band survey questions did001:did017
              # usually slightly fewer households than households_tot
              # think this denominator is preferred because my guess that households that are not included income band survey questions are also excluded from aggregate income
      # QUESTION: adjust for inflation?
    # create new versions of variables
    households_tot = if_else(dig001==0,NA,dec001), # total number of households
    inc_house_med_all  = if_else(die001==0,NA,die001), # median household income
    inc_house_agg_all = if_else(dig001==0,NA,dig001), # aggregate household income
    households_tot_calc = rowSums(select(., did001:did016), na.rm = TRUE), # calculated number of households that have values for household income bands
    inc_house_mean_all = inc_house_agg_all/households_tot_calc # mean income at tract-level, based on calculated number of houses that report income
  ) %>% rename(
    inc_house_lt2k_all = did001,
    inc_house_2kto5k_all = did002,
    inc_house_5kto7k_all = did003,
    inc_house_7kto10k_all = did004,
    inc_house_10kto12k_all = did005,
    inc_house_12kto15k_all = did006,
    inc_house_15kto17k_all = did007,
    inc_house_17kto20k_all = did008,
    inc_house_20kto22k_all = did009,      
    inc_house_22kto25k_all = did010,
    inc_house_25kto27k_all = did011,
    inc_house_27kto30k_all = did012,
    inc_house_30kto35k_all = did013,
    inc_house_35kto40k_all = did014,
    inc_house_40kto50k_all = did015,
    inc_house_50kto75k_all = did016,
    inc_house_gt75k = did017,    
  ) %>% 
  # drop family income variables, income variables by race/ethnicity, and per capita income
  select(-c(all_of(starts_with('dik')),all_of(starts_with('dil')),
     all_of(starts_with('dim')),all_of(starts_with('din')),all_of(starts_with('dio')),all_of(starts_with('dip')),
     all_of(starts_with('diq')),all_of(starts_with('diz')))) %>% 
  # drop measures of total number of people from sf3 because you have them from sf1
  select(-dgo001,-dg2001)
  
  d1980_stf1f3_anal_tract %>% select(all_of(starts_with('inc_'))) %>% glimpse()
  
  # replace df with one that only has eps and desired variables
  d1980_stf1f3_anal_tract <- d1980_stf1f3_anal_tract %>%
    select(
      # population vars
      eps, tot_ageall, white_ageall, black_ageall, native_ageall, api_ageall, other_ageall, nonhisp_ageall, hisp_ageall, 
      hispwhite_ageall, hispblack_ageall, hispapi_ageall, hispother_ageall, tot_agelt5, tot_age517, tot_age1864, tot_agegt65, 
      white_agelt5, white_age517, white_age1864, white_agegt65, black_agelt5, black_age517, black_age1864, black_agegt65, 
      native_agelt5, native_age517, native_age1864, native_agegt65, api_agelt5, api_age517, api_age1864, api_agegt65, 
      other_agelt5, other_age517, other_age1864, other_agegt65, nonhisp_age517,nonhisp_age1864, hisp_agelt5, hisp_age517, 
      hisp_age1864, hisp_agegt65, hispwhite_agelt5, hispwhite_age517, hispwhite_age1864, hispwhite_agegt65, hispblack_agelt5, 
      hispblack_age517, hispblack_age1864, hispblack_agegt65, hispother_agelt5, hispother_age517, hispother_age1864, hispother_agegt65,
      # education vars
      starts_with('ed_'),
      # income vars
      dec001,die001,dig001,all_of(starts_with('households')),all_of(starts_with('inc_'))
    )
 
  # create character vector of names of variables to be summed
    # remove the two income variables that should not be summed
  sum_vars <- d1980_stf1f3_anal_tract %>% select(-eps,-inc_house_med_all,-inc_house_mean_all) %>% names()
  sum_vars

  d1980_stf1f3_anal_tract %>% select(starts_with('ed_')) %>% glimpse()

  # Create suffix for race variables; used below
  race_suffixes <- c("all", "white", "black", "native", "api", "other", "hisp")
  
  # CREATE EPS LEVEL DATASET WITH ANALYSIS VARIABLES
  d1980_stf1f3_anal_eps <- d1980_stf1f3_anal_tract %>% 
    group_by(eps) %>% summarize(
      n_tracts = n(),
      # sum vars
      across(.cols = all_of(sum_vars), ~ sum(.x, na.rm = TRUE), .names = "sum_{.col}"),
      # median of median household income at tract-level
      med_inc_house_med_all = median(inc_house_med_all, na.rm = TRUE),
      # median of mean household income at tract-level
        # how calculated: calculate mean household income at the tract level; in group_by(eps) take the median value
      med_inc_house_mean_all = median(inc_house_mean_all, na.rm = TRUE)
      # note: prefer med_inc_house_med_all over med_inc_house_mean_all; cleaner variable in that it is median of median rather than median of mean
    ) %>% 
    mutate(
      # create percent race and ethnicity variables
      pct_white_ageall = sum_white_ageall/sum_tot_ageall*100,
      pct_black_ageall = sum_black_ageall/sum_tot_ageall*100,
      pct_api_ageall = sum_api_ageall/sum_tot_ageall*100,
      pct_native_ageall = sum_native_ageall/sum_tot_ageall*100,
      pct_other_ageall = sum_other_ageall/sum_tot_ageall*100,
      pct_nonhisp_ageall = sum_nonhisp_ageall/sum_tot_ageall*100,
      pct_hisp_ageall = sum_hisp_ageall/sum_tot_ageall*100,
      
      pct_white_age517 = sum_white_age517/sum_tot_age517*100,
      pct_black_age517 = sum_black_age517/sum_tot_age517*100,
      pct_api_age517 = sum_api_age517/sum_tot_age517*100,
      pct_native_age517 = sum_native_age517/sum_tot_age517*100,
      pct_other_age517 = sum_other_age517/sum_tot_age517*100,
      pct_nonhisp_age517 = sum_nonhisp_age517/sum_tot_age517*100,
      pct_hisp_age517 = sum_hisp_age517/sum_tot_age517*100,
      
      pct_white_age1864 = sum_white_age1864/sum_tot_age1864*100,
      pct_black_age1864 = sum_black_age1864/sum_tot_age1864*100,
      pct_api_age1864 = sum_api_age1864/sum_tot_age1864*100,
      pct_native_age1864 = sum_native_age1864/sum_tot_age1864*100,
      pct_other_age1864 = sum_other_age1864/sum_tot_age1864*100,
      pct_nonhisp_age1864 = sum_nonhisp_age1864/sum_tot_age1864*100,
      pct_hisp_age1864 = sum_hisp_age1864/sum_tot_age1864*100,
      
      # create mean income variable
        # sum of agg inc across all tracts/ (sum of total households across all tracts)
      mean_inc_house = sum_inc_house_agg_all/sum_households_tot_calc,
      
      # convert income measures from 1979 dollars to 1980 dollars:
        # To convert 1979 dollars to 2024 dollars, you can use the Consumer Price Index (CPI) values for these years.
        # The CPI in 1979 was 72.6, and the CPI for 2024 is 314.069. Using these values, the conversion factor is:
        
        # Conversion factor = CPI_2024 / CPI_1979 = 314.069 / 72.6 ≈ 4.33
        
        # This means that $1 in 1979 is equivalent to about $4.33 in 2024.
        # So, to convert any amount from 1979 dollars to 2024 dollars, you multiply the amount by 4.33.
        
        # For example:
        # $100 in 1979 is equivalent to $433 in 2024.
        # $1000 in 1979 is equivalent to $4330 in 2024.
      mean_inc_house = mean_inc_house*4.33,
      med_inc_house_med_all = med_inc_house_med_all*4.33,
      med_inc_house_mean_all = med_inc_house_mean_all*4.33,
      
      # create measures of highest educational attainment
        #pct_ed_lths_all = sum_ed_lths_all/sum_ed_tot_all*100,
        #pct_ed_hs_all = sum_ed_hs_all/sum_ed_tot_all*100,
        #pct_ed_ltba_all = sum_ed_ltba_all/sum_ed_tot_all*100,
        #pct_ed_ba_all = sum_ed_ba_all/sum_ed_tot_all*100,
      
        # thank you chatgpt:
      across(
        matches(paste0("sum_ed_(lths|hs|ltba|ba)_(", paste(race_suffixes, collapse = "|"), ")")), 
        ~ . / get(paste0("sum_ed_tot_", sub(".*_(.*)", "\\1", cur_column()))) * 100, 
        .names = "pct_{col}"
      ) # across 
      # checks that education vars look ok
        # all
        # select(sum_ed_lths_all,sum_ed_hs_all,sum_ed_ltba_all,sum_ed_ba_all,sum_ed_tot_all,pct_sum_ed_lths_all,pct_sum_ed_hs_all,pct_sum_ed_ltba_all,pct_sum_ed_ba_all) %>% print(n=305)
        # api
        # select(sum_ed_lths_api,sum_ed_hs_api,sum_ed_ltba_api,sum_ed_ba_api,sum_ed_tot_api,pct_sum_ed_lths_api,pct_sum_ed_hs_api,pct_sum_ed_ltba_api,pct_sum_ed_ba_api) %>% print(n=305)
      
    ) # mutate

  d1980_stf1f3_anal_eps %>% glimpse()
 
  # check results
    # race vars to keep: pct_white_ageall,pct_black_ageall,pct_api_ageall,pct_hisp_ageall
    # income vars to keep: med_inc_house_med_all,mean_inc_house
    # education vars to keep: pct_sum_ed_ba_all pct_sum_ed_ba_white pct_sum_ed_ba_black

  d1980_stf1f3_anal_eps %>% select(eps,pct_white_ageall,pct_black_ageall,pct_api_ageall,pct_hisp_ageall,med_inc_house_med_all,mean_inc_house,
                                   pct_sum_ed_lths_all,pct_sum_ed_ba_all,pct_sum_ed_ba_white,pct_sum_ed_ba_black,pct_sum_ed_ba_api,,pct_sum_ed_ba_hisp) %>% print(n=305)  %>% View()
  
  #check if income variables look ok
  d1980_stf1f3_anal_eps %>% select(eps,n_tracts,med_inc_house_med_all,mean_inc_house) %>% print(n=305)  
    #d1980_stf1f3_anal_eps %>% select(eps,n_tracts,med_inc_house_med_all,med_inc_house_mean_all,mean_inc_house,sum_inc_house_agg_all,sum_households_tot_calc) %>% print(n=305)  
  
  # look at results of of race/ethnicity vars
  d1980_stf1f3_anal_eps %>% select(eps,n_tracts,pct_white_ageall,pct_black_ageall,pct_api_ageall,pct_other_ageall,pct_nonhisp_ageall,pct_hisp_ageall) %>% print(n=305)
  
