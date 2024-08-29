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
  rm(list = ls()) # remove all objects
  options(max.print=1000)
  #options(width = 160)
  # Set the scipen option to a high value to avoid scientific notation
  options(scipen = 999)
  
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

###### LOAD EPS SHAPE FILE DATA
  
  load(file.path(eps_data_dir, 'eps_shapes.RData'))
  
####### READ IN DATA

# PRE-2004 MSA CODES
  msa_pre_2004 <- read_csv(file.path(d1980_data_dir, 'pre-2004-msa-codes-csv.csv'), col_names = TRUE, skip=0) %>% 
    mutate(area_fips = str_sub(area_fips,start=2L, end=-1L))
     
# 1980 DECENNIAL CENSUS DATA, STF1 POPULATION BY RACE, ETHNICITY, AND AGE

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
  
# 1980 DECENNIAL CENSUS DATA, STF3, EDUCATION AND INCOME
  
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
 
# 1980 DECENNIAL CENSUS STF3 POVERTY
  
  # Read the first row to get the variable names
  stf3_pov_variable_names <- read_csv(file.path(d1980_data_dir, 'nhgis0006_ds107_1980_tract.csv'), col_names = FALSE, n_max = 1) %>% as.character()
  stf3_pov_variable_names
  
  # Read the second row to get the variable labels
  stf3_pov_variable_labels <- read_csv(file.path(d1980_data_dir, 'nhgis0006_ds107_1980_tract.csv'), col_names = FALSE, skip = 1, n_max = 1) %>% as.character()
  stf3_pov_variable_labels
  # Read the data starting from the third row using the first row as column names
  stf3_pov_d1980_tract <- read_csv(file.path(d1980_data_dir, 'nhgis0006_ds107_1980_tract.csv'), col_names = stf3_pov_variable_names, skip = 2)
  
  # Add variable labels using the var_label function from labelled package
  var_label(stf3_pov_d1980_tract) <- stf3_pov_variable_labels
  
  # replace upper case w/ lower case variable names
  names(stf3_pov_d1980_tract) <- stf3_pov_d1980_tract %>% names %>% tolower()
  
  rm(stf3_pov_variable_labels,stf3_pov_variable_names)
  
  stf3_pov_d1980_tract %>% glimpse()
  stf3_d1980_tract %>% glimpse()
  stf3_d1980_tract %>% var_label()   
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
    ) %>% 
    # add poverty variables
    inner_join(
      y=stf3_pov_d1980_tract %>% select(gisjoin,di8001,di8002),
      by = c('gisjoin')
    )
  
  d1980_stf1_stf3_tract %>% glimpse()
  rm(drop_vars)
  
  # remove separate datasets
  rm(stf1_d1980_tract,stf3_d1980_tract,stf3_pov_d1980_tract)
  
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
    # or sf object that has one obs per tract-eps and for each tract we know the eps code of that tract
  
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
    
    # calculate amount of area for each census tract; 
      # necessary to calculate proportion (below)
      d1980_stf1_stf3_tractbna_sf %>% glimpse()
    
      d1980_stf1_stf3_tractbna_sf <- d1980_stf1_stf3_tractbna_sf %>% mutate(area_tract = st_area(.))

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


# 2 = Perform spacial intersection

  # first simplify the geometry
      #d1980_stf1_stf3_tractbna_sf <- st_simplify(d1980_stf1_stf3_tractbna_sf, dTolerance = 100) # for the CRS I am using, this tolerance means removing points closer to 100 meters apart; The CRS you provided above is USA_Contiguous_Albers_Equal_Area_Conic, specifically identified by the EPSG code ESRI:102003.
      # geometryies are vertices (points) arranged in order, connected by lines. st_simplify() removes points that are "close" together, with "close" defined by the value you assign to the dTolerance argument
  
  # break census tracts that span multiple geomarkets into smaller geometries; resulting dataset has one observation per eps-census tract
    d1980_tract_eps_intersect <- st_intersection(d1980_stf1_stf3_tractbna_sf, eps_geometry_zcta)

  # check if geometry is valid
    valid_geom <- st_is_valid(d1980_tract_eps_intersect) # check to see whether geometries are "valid"; valid geometries needed for st_intersect
    summary(valid_geom) # all geometries valid!
  
  # make geometries valid
    #d1980_tract_eps_intersect <- st_make_valid(d1980_tract_eps_intersect)    
    

  #3 = calculate the area of each intersection
  #After obtaining the intersection, calculate the area of each intersected polygon (which represents the part of the census tract within a geomarket). Then calculate the area of each original census tract.

  # calculate area of each intersected polygon  
    d1980_tract_eps_intersect$area_intersection <- st_area(d1980_tract_eps_intersect) # 
    

    
#4 calculate the area proportion
  #Calculate the proportion of the census tract that is within each geomarket by dividing the area of the intersection by the total area of the original census tract.

  # calculate proportion:
    d1980_tract_eps_intersect <- d1980_tract_eps_intersect %>%
      mutate(proportion = as.numeric(area_intersection / area_tract))
    
 
 
    
        
  # desired output
    # or sf object that has one obs per tract and for each tract we know the eps code of that tract
    # what my homie chatgpt says:
    #You can perform a spatial join to assign the eps code from eps_geometry_zcta to each observation in stf1_d1980_tractbna_sf. Here’s how you can do it using the sf package in R
    
    # Perform the spatial join
    #d1980_stf1_stf3_tract_sf_eps <- st_join(d1980_stf1_stf3_tractbna_sf, eps_geometry_zcta, join = st_intersects)  
    
    d1980_tract_eps_intersect %>% glimpse()
    d1980_tract_eps_intersect %>% class()
    
    #investigate data structure
      # gisjoin uniquely identifies obs in the input dataset
    d1980_stf1_stf3_tractbna_sf %>% as.data.frame() %>% 
          select(gisjoin) %>% group_by(gisjoin) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # one observation per zip code  

      # gisjoin does not uniquely identify obs in the dataset created by st_join with join = st_intersect    
    d1980_tract_eps_intersect %>% as.data.frame() %>% 
        select(gisjoin) %>% group_by(gisjoin) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # one observation per zip code  
      
      # gisjoin,eps uniquely identifies obs in new dataset
    d1980_tract_eps_intersect %>% as.data.frame() %>% 
        select(gisjoin,eps) %>% group_by(gisjoin,eps) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # one observation per zip code  
      
    # result: if the geometry of a census tract overlaps with more than one eps geometry, that census tract appears multiple times
    # what chat gpt says    
      # Yes, in the solution you've implemented, a given census tract will appear once if it is wholly contained within a single geomarket and will appear multiple times if it intersects with multiple geomarkets. Each appearance represents an intersection with a different geomarket.
      #Wholly Contained Tracts: If a census tract is entirely within a single geomarket, it will appear once in the joined dataframe with that geomarket's eps code.
      #Intersecting Tracts: If a census tract intersects with multiple geomarkets, it will appear once for each geomarket it intersects with, each time with the respective geomarket's eps code.
      #This results in some census tracts being duplicated in the joined dataframe, reflecting the fact that they span more than one geomarket.
    
    # merge smsaa names
    d1980_tract_eps_intersect <- d1980_tract_eps_intersect %>% left_join(y = msa_pre_2004, by = c('smsaa' = 'area_fips'))
      
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
      d1980_tract_eps_intersect <- st_transform(x = d1980_tract_eps_intersect, crs = 5070)
      
      # Check the new CRS
      st_crs(d1980_tract_eps_intersect)

      
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
      
  d1980_stf1f3_anal_tract <- d1980_tract_eps_intersect %>% as.data.frame() %>% 
    # RENAME AND CREATE VARS OF POPULATION BY RACE, ETHNICITY, AND AGE
    rename(
      tot_all = c7l001,
      nhisp_all = c9e001,
      hisp_all = c9f001,
      
      white = c9d001,
      black = c9d002,
      other = c9d015,
      
      hisp_white = c9g001,
      hisp_black = c9g002,
      hisp_native_api = c9g003,
      hisp_other = c9g004,
      
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
      native = ifelse(is.na(c9d003) & is.na(c9d004) & is.na(c9d005), NA, 
        rowSums(select(., c9d003, c9d004, c9d005), na.rm = TRUE)),
      api = ifelse(
          is.na(c9d006) & is.na(c9d007) & is.na(c9d008) & is.na(c9d009) & is.na(c9d010) & is.na(c9d011) & is.na(c9d012) & is.na(c9d013) & is.na(c9d014), NA, 
          rowSums(select(., c9d006, c9d007, c9d008, c9d009, c9d010, c9d011, c9d012, c9d013, c9d014), na.rm = TRUE)
        ),
      asian = ifelse(
        is.na(c9d006) & is.na(c9d007) & is.na(c9d008) & is.na(c9d009) & is.na(c9d010) & is.na(c9d011), NA, 
        rowSums(select(., c9d006, c9d007, c9d008, c9d009, c9d010, c9d011), na.rm = TRUE)
      ),
      nhpi = ifelse(
        is.na(c9d012) & is.na(c9d013) & is.na(c9d014), NA, 
        rowSums(select(., c9d012, c9d013, c9d014), na.rm = TRUE)
      ),
      native_api = ifelse(
        is.na(c9d003) & is.na(c9d004) & is.na(c9d005) & is.na(c9d006) & is.na(c9d007) & is.na(c9d008) & is.na(c9d009) & is.na(c9d010) & is.na(c9d011) & is.na(c9d012) & is.na(c9d013) & is.na(c9d014), NA, 
        rowSums(select(., c9d003, c9d004, c9d005,c9d006, c9d007, c9d008, c9d009, c9d010, c9d011, c9d012, c9d013, c9d014), na.rm = TRUE)
      ),
      nhisp_white = white - hisp_white,
      nhisp_black = black - hisp_black,
      nhisp_other = other - hisp_other,
      nhisp_native_api = native_api - hisp_native_api,
      nonhisp_age517 = tot_age517 - hisp_age517,
      nonhisp_age1864 = tot_age1864 - hisp_age1864,
    ) %>% 
    select(-starts_with('c9d'),-starts_with('c9e')) %>% 
    # EDUCATIONAL ATTAINMENT VARIABLES (AGE 25+)
    mutate(
      # all race
      edu_lths_all = if_else(is.na(dhm001) & is.na(dhm002), NA, 
                            rowSums(select(., dhm001, dhm002), na.rm = TRUE)), # lths
      edu_tot_all = if_else(is.na(dhm001) & is.na(dhm002) & is.na(dhm003) & is.na(dhm004) & is.na(dhm005), NA, 
                           rowSums(select(., dhm001, dhm002, dhm003, dhm004, dhm005), na.rm = TRUE)), # all in group
      # white
      edu_lths_white = if_else(is.na(dhn001) & is.na(dhn002), NA, 
                              rowSums(select(., dhn001, dhn002), na.rm = TRUE)), # lths
      edu_tot_white = if_else(is.na(dhn001) & is.na(dhn002) & is.na(dhn003) & is.na(dhn004) & is.na(dhn005), NA, 
                             rowSums(select(., dhn001, dhn002, dhn003, dhn004, dhn005), na.rm = TRUE)), # all in group    
      # black
      edu_lths_black = if_else(is.na(dhn006) & is.na(dhn007), NA, 
                              rowSums(select(., dhn006, dhn007), na.rm = TRUE)), # lths
      edu_tot_black = if_else(is.na(dhn006) & is.na(dhn007) & is.na(dhn008) & is.na(dhn009) & is.na(dhn010), NA, 
                             rowSums(select(., dhn006, dhn007, dhn008, dhn009, dhn010), na.rm = TRUE)), # all in group
      # American Indian, Eskimo, and Aleut
      edu_lths_native = if_else(is.na(dhn011) & is.na(dhn012), NA, 
                               rowSums(select(., dhn011, dhn012), na.rm = TRUE)), # lths
      edu_tot_native = if_else(is.na(dhn011) & is.na(dhn012) & is.na(dhn013) & is.na(dhn014) & is.na(dhn015), NA, 
                              rowSums(select(., dhn011, dhn012, dhn013, dhn014, dhn015), na.rm = TRUE)), # all in group
      # Asian and Pacific Islander
      edu_lths_api = if_else(is.na(dhn016) & is.na(dhn017), NA, 
                            rowSums(select(., dhn016, dhn017), na.rm = TRUE)), # lths
      edu_tot_api = if_else(is.na(dhn016) & is.na(dhn017) & is.na(dhn018) & is.na(dhn019) & is.na(dhn020), NA, 
                           rowSums(select(., dhn016, dhn017, dhn018, dhn019, dhn020), na.rm = TRUE)), # all in group
      # Other
      edu_lths_other = if_else(is.na(dhn021) & is.na(dhn022), NA, 
                              rowSums(select(., dhn021, dhn022), na.rm = TRUE)), # lths
      edu_tot_other = if_else(is.na(dhn021) & is.na(dhn022) & is.na(dhn023) & is.na(dhn024) & is.na(dhn025), NA, 
                             rowSums(select(., dhn021, dhn022, dhn023, dhn024, dhn025), na.rm = TRUE)), # all in group
      # hisp
      edu_lths_hisp = if_else(is.na(dho001) & is.na(dho002), NA, 
                             rowSums(select(., dho001, dho002), na.rm = TRUE)), # lths
      edu_tot_hisp = if_else(is.na(dho001) & is.na(dho002) & is.na(dho003) & is.na(dho004) & is.na(dho005), NA, 
                            rowSums(select(., dho001, dho002, dho003, dho004, dho005), na.rm = TRUE)), # all in group
    ) %>% rename(
      # education, all race
      edu_hs_all = dhm003,
      edu_ltba_all = dhm004,
      edu_baplus_all = dhm005,
      # education, white
      edu_hs_white = dhn003,
      edu_ltba_white = dhn004,
      edu_baplus_white = dhn005,
      # education, black
      edu_hs_black = dhn008,
      edu_ltba_black = dhn009,
      edu_baplus_black = dhn010,
      # education, American Indian, Eskimo, and Aleut
      edu_hs_native = dhn013,
      edu_ltba_native = dhn014,
      edu_baplus_native = dhn015,
      # education, Asian and Pacific Islander
      edu_hs_api = dhn018,
      edu_ltba_api = dhn019,
      edu_baplus_api = dhn020,
      # education, Other
      edu_hs_other = dhn023,
      edu_ltba_other = dhn024,
      edu_baplus_other = dhn025,
      # education, hisp
      edu_hs_hisp = dho003,
      edu_ltba_hisp = dho004,
      edu_baplus_hisp = dho005,  
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
    #inc_house_mean_all = inc_house_agg_all/households_tot_calc, # mean income at tract-level, based on calculated number of houses that report income
    
    pov_denom = if_else(is.na(di8001) & is.na(di8002), NA, 
                           rowSums(select(., di8001, di8002), na.rm = TRUE)), # num families above poverty + num below; will be used as denominator for % poverty
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
    pov_no = di8001,
    pov_yes = di8002,
  ) %>% 
  # drop family income variables, income variables by race/ethnicity, and per capita income
  select(-c(all_of(starts_with('dik')),all_of(starts_with('dil')),
     all_of(starts_with('dim')),all_of(starts_with('din')),all_of(starts_with('dio')),all_of(starts_with('dip')),
     all_of(starts_with('diq')),all_of(starts_with('diz')))) %>% 
  # drop measures of total number of people from sf3 because you have them from sf1
  select(-dgo001,-dg2001) %>% 
  # drop other measures you don't need
        select(-c(year, fstatus, regiona, divisiona, state, statea, smsaa, county, countya, cty_suba, placea, tracta, blck_grpa, blocka, edinda, 
                  enumdista, scsaa, urb_areaa, cda, aianhha, mcdseqno, sea, uatype, placdesc, cbd, indsubr, longitud, latitude, landarea, areaname,
                  area_intersection,area_title)
               )  
  

  # create character vector of names of variables to be summed [and vars to multiply by proportion of census tract area]
    # remove the two income variables that should not be summed
  sum_vars <- d1980_stf1f3_anal_tract %>% select(-c(eps,inc_house_med_all,proportion,gisjoin,geometry)) %>% names()
  sum_vars
    
  #5 = adjust tract data by proportion
  #Adjust any tract-level variables (e.g., population counts) by multiplying them by the proportion of the tract within each geomarket.
  # note: you could alternatively decide to multiply by proportions after you create the analysis variables (at the tract level) below
  
  # create vector of variables to do the proportions calculation on
  d1980_stf1f3_anal_tract <- d1980_stf1f3_anal_tract %>%
    mutate(across(
      .cols = all_of(sum_vars),
      .fns = ~ .x * proportion
    ))  
  

  d1980_stf1f3_anal_tract %>% select(starts_with('edu_')) %>% glimpse()

  d1980_stf1f3_anal_tract %>% glimpse()
  
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
      #med_inc_house_mean_all = median(inc_house_mean_all, na.rm = TRUE)
      # note: prefer med_inc_house_med_all over med_inc_house_mean_all; cleaner variable in that it is median of median rather than median of mean
    ) %>% 
    mutate(
      # create percent race and ethnicity variables
      pct_white = sum_white/sum_tot_all*100,
      pct_black = sum_black/sum_tot_all*100,
      pct_api = sum_api/sum_tot_all*100,
      pct_native = sum_native/sum_tot_all*100,
      pct_other = sum_other/sum_tot_all*100,
      
      pct_nhisp_all = sum_nhisp_all/sum_tot_all*100,
      pct_hisp_all = sum_hisp_all/sum_tot_all*100,
      
      pct_hisp_white = sum_hisp_white/sum_tot_all*100,
      pct_nhisp_white = sum_nhisp_white/sum_tot_all*100,
      pct_hisp_black = sum_hisp_black/sum_tot_all*100,
      pct_nhisp_black = sum_nhisp_black/sum_tot_all*100,
      pct_hisp_other = sum_hisp_other/sum_tot_all*100,
      pct_nhisp_other = sum_nhisp_other/sum_tot_all*100,
      pct_asian = sum_asian/sum_tot_all*100,
      pct_nhpi = sum_nhpi/sum_tot_all*100,
      pct_native_api = sum_native_api/sum_tot_all*100,
      pct_hisp_native_api = sum_hisp_native_api/sum_tot_all*100,
      pct_nhisp_native_api = sum_nhisp_native_api/sum_tot_all*100,      
      
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
      
      # convert income measures from 1979 dollars to 2024 dollars:
        # To convert 1979 dollars to 2024 dollars, you can use the Consumer Price Index (CPI) values for these years.
        # The CPI in 1979 was 72.6, and the CPI for 2024 is 314.069. Using these values, the conversion factor is:
        
        # Conversion factor = CPI_2024 / CPI_1979 = 314.069 / 72.6 ≈ 4.33
        
        # This means that $1 in 1979 is equivalent to about $4.33 in 2024.
        # So, to convert any amount from 1979 dollars to 2024 dollars, you multiply the amount by 4.33.
        
        # For example:
        # $100 in 1979 is equivalent to $433 in 2024.
        # $1000 in 1979 is equivalent to $4330 in 2024.
      mean_inc_house = mean_inc_house*4.33,
      med_inc_house_med = med_inc_house_med_all*4.33,
      #med_inc_house_mean_all = med_inc_house_mean_all*4.33,
     
      # create measures of percent of people in poverty
      pct_pov_yes = sum_pov_yes/sum_pov_denom*100,  
      pct_pov_no = sum_pov_no/sum_pov_denom*100,
      
      # create measures of highest educational attainment
        #pct_ed_lths_all = sum_ed_lths_all/sum_ed_tot_all*100,
        #pct_ed_hs_all = sum_ed_hs_all/sum_ed_tot_all*100,
        #pct_ed_ltba_all = sum_ed_ltba_all/sum_ed_tot_all*100,
        #pct_ed_ba_all = sum_ed_ba_all/sum_ed_tot_all*100,
      
        # thank you chatgpt:
      across(
        matches(paste0("sum_edu_(lths|hs|ltba|baplus)_(", paste(race_suffixes, collapse = "|"), ")")), 
        ~ round(. / get(paste0("sum_edu_tot_", sub(".*_(.*)", "\\1", cur_column()))) * 100, 2), 
        .names = "pct_edu_{sub('sum_edu_', '', col)}"
      )
      # checks that education vars look ok
        # all
        # select(sum_ed_lths_all,sum_ed_hs_all,sum_ed_ltba_all,sum_ed_ba_all,sum_ed_tot_all,pct_sum_ed_lths_all,pct_sum_ed_hs_all,pct_sum_ed_ltba_all,pct_sum_ed_ba_all) %>% print(n=305)
        # api
        # select(sum_ed_lths_api,sum_ed_hs_api,sum_ed_ltba_api,sum_ed_ba_api,sum_ed_tot_api,pct_sum_ed_lths_api,pct_sum_ed_hs_api,pct_sum_ed_ltba_api,pct_sum_ed_ba_api) %>% print(n=305)
      
    ) # mutate
  
  d1980_stf1f3_anal_eps %>% glimpse()
  
  d1980_stf1f3_anal_eps %>% class()
  
  d1980_stf1f3_anal_eps %>% count()
  
  eps_geometry_zcta %>% glimpse()
  
  d1980_stf1f3_anal_eps %>% right_join(y = eps_geometry_zcta, by = c('eps')) %>% glimpse()

  # merge to object w/ eps shape files and convert to sf object
  d1980_stf1f3_anal_eps_sf <- d1980_stf1f3_anal_eps %>% 
    inner_join(y = eps_geometry_zcta, by = c('eps')) %>% 
    st_as_sf() %>% 
    st_transform(5070)

  # save in analysis data file
  save(d1980_stf1f3_anal_eps_sf, file = file.path(data_dir,'analysis_data', 'd1980_stf1f3_anal_eps_sf.RData'))
  
  load(file = file.path(data_dir,'analysis_data', 'd1980_stf1f3_anal_eps_sf.RData'))
  
  d1980_stf1f3_anal_eps_sf %>% glimpse()
  # check results
    # race vars to keep: pct_white_ageall,pct_black_ageall,pct_api_ageall,pct_hisp_ageall
    # income vars to keep: med_inc_house_med_all,mean_inc_house,pct_pov_yes
    # education vars to keep: pct_sum_ed_ba_all pct_sum_ed_ba_white pct_sum_ed_ba_black

  d1980_stf1f3_anal_eps %>% select(eps,pct_white_ageall,pct_black_ageall,pct_api_ageall,pct_hisp_ageall,med_inc_house_med_all,mean_inc_house,pct_pov_yes,
                                   pct_sum_ed_lths_all,pct_sum_ed_ba_all,pct_sum_ed_ba_white,pct_sum_ed_ba_black,pct_sum_ed_ba_api,pct_sum_ed_ba_hisp) %>% print(n=305)  %>% View()

  # check results, including poverty variables
  d1980_stf1f3_anal_eps %>% select(eps,pct_white_ageall,pct_black_ageall,pct_api_ageall,pct_hisp_ageall,pct_pov_yes,med_inc_house_med_all,mean_inc_house,
                                   pct_sum_ed_lths_all,pct_sum_ed_ba_all) %>% print(n=305)  %>% View()

  # income and poverty variables 
  d1980_stf1f3_anal_eps_sf %>% select(eps,med_inc_house_med,mean_inc_house,pct_pov_yes) %>% View()
  
  #check if poverty variables look ok
  
  d1980_stf1f3_anal_eps %>% select(eps,sum_pov_yes,sum_pov_no,sum_pov_denom,pct_pov_yes,pct_pov_no) %>% print(n=305)  
  
  #check if income variables look ok
  d1980_stf1f3_anal_eps %>% select(eps,n_tracts,med_inc_house_med_all,mean_inc_house) %>% print(n=305)  
    #d1980_stf1f3_anal_eps %>% select(eps,n_tracts,med_inc_house_med_all,med_inc_house_mean_all,mean_inc_house,sum_inc_house_agg_all,sum_households_tot_calc) %>% print(n=305)  
  
  # education vars
  d1980_stf1f3_anal_eps %>% 
    select(eps, pct_edu_baplus_all, pct_edu_baplus_white, pct_edu_baplus_black, pct_edu_baplus_native, pct_edu_baplus_api, pct_edu_baplus_other, pct_edu_baplus_hisp) %>% 
    View()
  
    # check inputs against outputs
      #d1980_stf1f3_anal_eps %>% select(eps, sum_edu_baplus_all, sum_edu_tot_all, pct_edu_baplus_all) %>% View()
      #d1980_stf1f3_anal_eps %>% select(eps, sum_edu_baplus_white, sum_edu_tot_white, pct_edu_baplus_white) %>% View()
      #d1980_stf1f3_anal_eps %>% select(eps, sum_edu_baplus_black, sum_edu_tot_black, pct_edu_baplus_black) %>% View()
      #d1980_stf1f3_anal_eps %>% select(eps, sum_edu_baplus_native, sum_edu_tot_native, pct_edu_baplus_native) %>% View()
      #d1980_stf1f3_anal_eps %>% select(eps, sum_edu_baplus_api, sum_edu_tot_api, pct_edu_baplus_api) %>% View()
      #d1980_stf1f3_anal_eps %>% select(eps, sum_edu_baplus_other, sum_edu_tot_other, pct_edu_baplus_other) %>% View()
      #d1980_stf1f3_anal_eps %>% select(eps, sum_edu_baplus_hisp, sum_edu_tot_hisp, pct_edu_baplus_hisp) %>% View()
  
  
# look at results of of race/ethnicity vars
  d1980_stf1f3_anal_eps %>% select(eps,n_tracts,pct_white_ageall,pct_black_ageall,pct_api_ageall,pct_other_ageall,pct_nonhisp_ageall,pct_hisp_ageall) %>% print(n=305)
  
  # SUMMARY OF POPULATION BY RACE/HISPANIC ORIGIN THAT I CAN AND CANNOT CREATE
    # vars we have for 2000/2020 that I use a lot: 
      # nhisp_white, nhisp_black, nhisp_other, nhisp_native, nhisp_api, nhisp_multi, hisp_all,
    # Which vars can I create that we use in 2000/2020
      # nhisp_white,nhisp_black, nhisp_other, hisp_all
    # Which vars I cannot create that we use in 2000/2020
      # nhisp_native, nhisp_api (also can't create hisp_native, hisp_api)
      # nhisp_multi [doesn’t ask about multi race]
    # Analysis vars I can use in place of vars I cannot create
      # native [includes hispanic and non-hispanic]
      # api [includes hispanic and non-hispanic]
      # asian [includes hispanic and non-hispanic]
      # api [includes hispanic and non-hispanic]
      # nhisp_native_api [combines native and api]
      # hisp_native_api [combines native and api]
    # Recommended race/ethnicity vars to use
      # nhisp_white,nhisp_black, nhisp_otherhisp_all
      # native [includes hispanic and non-hispanic]
      # api [includes hispanic and non-hispanic]
      # asian [includes hispanic and non-hispanic]
      # nhpi [includes hispanic and non-hispanic]

  # check new race vars against inputs
  d1980_stf1f3_anal_eps %>% select(eps, pct_white, pct_hisp_white, pct_nhisp_white) %>% View() # nhisp_white
  d1980_stf1f3_anal_eps %>% select(eps, pct_black, pct_hisp_black, pct_nhisp_black) %>% View() # nhisp_black
  d1980_stf1f3_anal_eps %>% select(eps, pct_other, pct_hisp_other, pct_nhisp_other, pct_hisp_all) %>% View() # nhisp_other
  d1980_stf1f3_anal_eps %>% select(eps, pct_asian, pct_nhpi, pct_api) %>% View() # asian, nhpi, api
  d1980_stf1f3_anal_eps %>% select(eps, pct_native, pct_api, pct_native_api) %>% View() # native_api
  d1980_stf1f3_anal_eps %>% select(eps, pct_native_api, pct_hisp_native_api, pct_nhisp_native_api) %>% View() # hisp_native_api, nhisp_native_api
  
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
  