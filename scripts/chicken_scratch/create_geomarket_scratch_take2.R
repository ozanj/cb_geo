################################################################################
## [ PROJ ] < College Board Geomarket >
## [ FILE ] < create_geomarket.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 5/11/2023
## [ DESC ] < Create geomarket shape files >
################################################################################

### SETTINGS
  rm(list = ls())
  options(max.print=1000)

### LIBRARIES
library(tidyverse)
library(readxl)
library(lubridate)
library(haven)
library(labelled)
library(tidycensus)
library(sf)
library(tigris)
#library(stars)
#library(spatstat)
#library(rgeos)

### DIRECTORY PATHS

getwd()

data_dir <- file.path('.','data') # 
  list.files(path = data_dir)
  
eps_data_dir <- file.path(data_dir,'eps_market') # 
  list.files(path = eps_data_dir)
  
scripts_dir <- file.path('.','scripts') # 
  list.files(path = scripts_dir)

  
########### STEPS
  # Read in .xls file that has name of geomarket associated with each zip
  # get ZCTA shape file from around 2010 from tidycensus function
  # merge eps file to zcta file by zip code, to obtain dataset that has geomarket and shape-file for each zip-code
  # create dataset that has one observation per geomarket by using aggregate to "dissolve" zip codes into geomarkets
  # Using tidycensus, create dataset(s) with one observation per zip-code with data on desired characteristics (e.g., income, racial composition, population, education level, etc.)
    # can use this dataset to do analyses of how characteristics vary within geomarket
  # create dataset(s) with one obsr per geomarket by using group_by() and summarize()
    # calculates descriptive statistics at geomarket level, to show how characteristics vary between geomarkets
  # merge dataset w/ one obs per geomarket to dataset w/ geomarket shape file in order to create maps that graph geomarket characteristics
      
###########
  
# Read in .xls file that has name of geomarket associated with each zip
  eps_zip <- read_excel(path = file.path(eps_data_dir,'eps.xls'), sheet = 'Sheet 1', col_names = TRUE)
  eps_zip %>% glimpse()
  # fix error in eps var (e.g., "FL06" should be "FL 6")
  eps_zip <- eps_zip %>% mutate(eps = if_else(str_sub(eps,3,3)=="0",str_c(str_sub(eps,1,2)," ",str_sub(eps,4,4)),eps))

  # make sure zip codes are unique in each dataset
    eps_zip %>% group_by(zip) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # one observation per zip code  
    
# get ZCTA shape file from around 2010 from tidycensus function
  
  zcta_2010 <-  zctas(
    cb = TRUE, 
    year = 2010
  )
  zcta_2010 %>% glimpse()
    # zcta_2015 <-  zctas(cb = TRUE, year = 2015) # this has 33,144 obs; zcta_2010 has 33,120 obs; 2011-2015 ACS has 33120 obs

# merge eps file to zcta file by zip code, to obtain dataset that has geomarket and shape-file for each zip-code
  zcta_eps <- zcta_2010 %>% left_join(y = eps_zip, by = c('ZCTA5' = 'zip'))
    zcta_eps %>% glimpse()
  
    # 6 zcta's from zcta_2010 that don't have a match from dataframe eps_zip
        zcta_eps %>% filter(is.na(eps))
    # 8366 observations from eps_zip that don't have a match from dataframe zcta_2010
      # To do: need to investigate why these zip codes are not in the zcta file
       eps_zip_anti <- eps_zip %>% anti_join(y = zcta_2010, by = c('zip' = 'ZCTA5')) %>% arrange(eps,zip)

# create dataset that has one observation per geomarket by using aggregate to "dissolve" zip codes into geomarkets
    eps_geometry <- zcta_eps %>% select(eps, geometry) %>% aggregate(by = list(zcta_eps$eps), FUN = identity) # takes a long time; should it be FUN = mean? 
    eps_geometry %>% glimpse()
    eps_geometry <- eps_geometry %>% select(-eps) %>% rename(eps = Group.1)
      
    #eps_geometry %>% ggplot() + geom_sf()


  # Using tidycensus, create dataset(s) with one observation per zip-code with data on desired characteristics (e.g., income, racial composition, population, education level, etc.)
    # can use this dataset to do analyses of how characteristics vary within geomarket
  # create dataset(s) with one obsr per geomarket by using group_by() and summarize()
    # calculates descriptive statistics at geomarket level, to show how characteristics vary between geomarkets
    
    
    income_2015_zcta <- get_acs(
      geography = "zcta",
      variables = "B19013_001",
      year = 2015,
      geometry = FALSE,
      survey = 'acs5'
    ) %>% select(-NAME)
    
    
    income_2015_zcta %>% class()
    income_2015_zcta %>% glimpse()
    zcta_eps  %>% glimpse()
    zcta_eps  %>% class()
    
    zcta_eps_attributes <- income_2015_zcta %>% 
      left_join(
        y = zcta_eps %>% select(ZCTA5,eps,geometry), 
        by = c('GEOID' = 'ZCTA5')
      )
    
    zcta_eps_attributes %>% glimpse()
    
    eps_attributes <- zcta_eps_attributes %>% filter(!is.na(eps)) %>% group_by(eps) %>% 
      summarize(
        mean_inc = mean(estimate, na.rm = TRUE)
      ) 

  # create dataset w/ one obs per geomarket that has attributes and geometry
    
    #eps_geometry %>% group_by(eps) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # one observation per zip code      
    eps_attributes %>% group_by(eps) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # one observation per zip code      
    eps_geometry %>% glimpse()
    eps_attributes %>% glimpse()
    
    eps_geometry_attributes <-  eps_geometry %>% left_join(
      y = eps_attributes,
      by = "eps"
    )
    
#### some graphing    

  eps_ca <- eps_geometry_attributes %>% filter(str_sub(string = eps,start = 1L, end = 2L)=="CA")
  
  eps_ca %>% print(n=40)
  eps_ca %>% ggplot() + geom_sf() ## ugh.... lots of holes!!!
      
  eps_la <- eps_ca %>% filter(eps %in% c('CA14','CA15','CA16','CA17','CA18','CA19','CA20','CA21','CA22','CA23','CA24','CA25','CA26')) 
  
  eps_la %>% ggplot() + geom_sf() + geom_sf_label(aes(label = eps))

  eps_la %>% ggplot() + geom_sf(aes(fill=mean_inc)) + geom_sf_label(aes(label = eps))
    
#############    
############# NEXT STEPS 5/30
#############
############# figure out why the shapefiles for eps_geometry and eps_ca have so many holes; problem is way upstream
############# start adding the different attributes you want from ACS; right now you only have income; 
  ################ think it will be one obs per geographic_feature-variable
  
    
  income_2015_zcta %>% glimpse()
  

  # NEXT STEP, FIGURE OUT HOW TO DO THE UNION USING SF FUNCTIONS RATHER THAN RGEOS FUNCTIONS

  # read eps.xls file which has geomarket for each zip code
  eps_zip <- read_excel(path = file.path(eps_data_dir,'eps.xls'), sheet = 'Sheet 1', col_names = TRUE)
  eps_zip %>% glimpse()
  
  # read zip code shape file
  # https://cran.r-project.org/web/packages/sf/vignettes/sf2.html
  zip_lower48 <- st_read(file.path(data_dir,'eps_market',"US_Zip_-_Lower_48.shp"))

    # create sp version of object to compare w/ other script
    #zip_sp <- as_Spatial(from = zip_df)
    
  # make sure zip codes are unique in each dataset
    #zip_sp@data %>% group_by(ZIP) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # one observation per zip code
    eps_zip %>% group_by(zip) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # one observation per zip code  
  

  # merge geomarket to zip_df sf object by zip code
    zip_df <- income_2015_zcta %>% left_join(y = eps_zip, by = c('GEOID' = 'zip'))
    zip_df %>% glimpse()
    
    zip_df2 <- zip_lower48 %>% left_join(y = eps_zip, by = c('ZIP' = 'zip'))    
    zip_df2 %>% glimpse()
    #zip_sp@data <- data.frame(zip_sp@data, eps_df[match(zip_sp@data$ZIP, eps_df$zip),])
    
  # try to figure out and recreate what this code is doing:
    #myzip@data=data.frame(myzip@data, eps[match(as.character(myzip@data$ZIP), eps$zip),])
    # data.frame function creates a data frame
    # match function (base): returns a vector of the positions of (first) matches of its first argument in its second.
      # for each element of zip_sp@data$ZIP, identify the index value of object eps where there is a match between zip_sp@data$ZIP and eps_df$zip
        # NA value means that there is no zip code from eps_df$zip that matches with zip_sp@data$ZIP
        #match(as.character(zip_sp@data$ZIP), eps_df$zip)
        #match(zip_sp@data$ZIP, eps_df$zip) %>% length()  # 29,842 = length of zip_sp
        #eps_df[match(zip_sp@data$ZIP, eps_df$zip),]
        #eps_df[match(zip_sp@data$ZIP, eps_df$zip),] %>% str() # tibble w/ 29,842 obs
        

      # 41 obs where var eps is missing
      zip_df %>% glimpse()
      zip_df$eps
      zip_df %>% filter(is.na(eps))
      
      zip_df2 %>% filter(is.na(eps))

    # step = "dissolve based on the merged eps region -- fixed eps file"
      
      zip_df %>% select(eps, geometry, estimate) %>% aggregate(by = list(zip_df$eps), FUN = identity)
      
      # https://gis.stackexchange.com/questions/316181/how-do-i-combine-geometries-in-a-shapefile-based-on-a-grouping-variable
      
      # takes forever

      zip_df %>% select(eps, geometry, estimate) %>% aggregate(by = list(zip_df$eps), FUN = sum(., na.rm = TRUE))
      
      temp <- zip_df %>% select(eps, geometry, estimate) %>% aggregate(by = list(zip_df$eps), FUN = identity)
      temp %>% glimpse()
      temp %>% print(n=300)
      
      eps_df <- zip_df %>% select(eps, geometry, estimate) %>% aggregate(by = list(zip_df$eps), mean)
      
      eps_df <- eps_df %>% select(-eps) %>% rename(eps = Group.1) %>% glimpse()
      
      eps_df %>% glimpse()
      eps_df$geometry %>% plot()
      
      eps_df %>% ggplot() + geom_sf()
      
      
  ###########
  ###########
  ########### spatial overlays and subsets
  ###########
      
  #which geographies (e.g., tracts) fall within a given metropolitan area
    # this is similar to which geographies fall into a given geomarket      
      
  #Spatial subsetting: figure out which geographies of one type overlap with which geographies of another type
    #- Spatial subsetting uses the extent of one spatial dataset to extract features from another spatial dataset based on co-location, defined by a spatial predicate.
    #- Spatial subsets can be expressed through base R indexing notation:
      #- syntax: `<object_to_subset>[<object_that_determines_subset>, ]`
      #- where
      #  - `<object_to_subset>`: result will be an object of this type with this geography (e.g., tract) but with fewer features (rows)
      #  - `<object_that_determines_subset>`: take rows from `<object_to_subset>` that overlap with `<object_that_determines_subset>`      
  #- note about `<object_to_subset>[<object_that_determines_subset>, ]`
  #  - returns rows from `<object_to_subset>` that partially overlap with `<object_that_determines_subset>`
  #  - it seems to return rows from `<object_to_subset>` that border with `<object_that_determines_subset>` even if they do not overlap with `<object_that_determines_subset>`
    # The spatial subsetting operation returns all the Census tracts that intersect the extent of the Kansas City metropolitan area, using the default spatial predicate, st_intersects(). This gives us back tracts that fall within the metro area’s boundary and those that cross or touch the boundary.

  # find which geographies (tracts) fall within a given geomarket
      # step 1: get sf object of CA tracts
      # step 2: get sf object of particular geomarkets
      # use st_filter to figure out which tracts fit within geomarket
      
  eps_ca <- eps_df %>% filter(str_sub(string = eps,start = 1L, end = 2L)=="CA")
  
  eps_ca %>% print(n=40)
  ggplot() + geom_sf(data = eps_ca)
      
  eps_la <- eps_ca %>% filter(eps %in% c('CA14','CA15','CA16','CA17','CA18','CA19','CA20','CA21','CA22','CA23','CA24','CA25','CA26')) 
  
  eps_la %>% ggplot() + geom_sf() + geom_sf_label(aes(label = eps))
  
  eps_ca17 <- eps_la %>% filter(eps == 'CA17')
  eps_ca17
  
      
  ca_income_2010 <- get_acs(
    geography = "tract",
    variables = "B19013_001",
    state = "CA", 
    year = 2010,
    geometry = TRUE
  ) %>%  st_transform(crs = 'WGS84')
  ca_income_2010
  
  ca_income_2020 <- get_acs(
    geography = "tract",
    variables = "B19013_001",
    state = "CA", 
    year = 2020,
    geometry = TRUE
  ) %>%  st_transform(crs = 'WGS84')
  ca_income_2020
  
  ggplot() + geom_sf(data = ca_income_2010, aes(fill = estimate))
  ggplot() + geom_sf(data = ca_income_2020, aes(fill = estimate))
  
  # which tracts are within/intersect with geomarkets
    # ca17 geomarket
    ca17_tracts_2010_intersect <- ca_income_2010  %>% st_filter(eps_ca17, .predicate = st_intersects)
    ca17_tracts_2010_within <- ca_income_2010  %>% st_filter(eps_ca17, .predicate = st_within)
    
    ca17_tracts_2020_intersect <- ca_income_2020  %>% st_filter(eps_ca17, .predicate = st_intersects)
    ca17_tracts_2020_within <- ca_income_2020  %>% st_filter(eps_ca17, .predicate = st_within)
  
    # graph
      #2010 acs
      ggplot() + geom_sf(data = eps_ca17, fill = NA, color = "red")
      ggplot() + geom_sf(data = ca17_tracts_2010_intersect, fill = "white", color = "grey")
      ggplot() + geom_sf(data = ca17_tracts_2010_intersect, fill = 'white', color = "grey") +  geom_sf(data = eps_ca17, fill = NA, color = "red") 
  
      ggplot() + geom_sf(data = eps_ca17, fill = NA, color = "red")        
      ggplot() + geom_sf(data = ca17_tracts_2010_within, fill = "white", color = "grey")
      ggplot() + geom_sf(data = ca17_tracts_2010_within, fill = 'white', color = "grey") +  geom_sf(data = eps_ca17, fill = NA, color = "red")

      #2020 acs
      ggplot() + geom_sf(data = eps_ca17, fill = NA, color = "red")
      ggplot() + geom_sf(data = ca17_tracts_2020_intersect, fill = "white", color = "grey")
      ggplot() + geom_sf(data = ca17_tracts_2020_intersect, fill = 'white', color = "grey") +  geom_sf(data = eps_ca17, fill = NA, color = "red")
  
      ggplot() + geom_sf(data = eps_ca17, fill = NA, color = "red")        
      ggplot() + geom_sf(data = ca17_tracts_2020_within, fill = "white", color = "grey")
      ggplot() + geom_sf(data = ca17_tracts_2020_within, fill = 'white', color = "grey") +  geom_sf(data = eps_ca17, fill = NA, color = "red")
        
    # la geomarkets
    la_tracts_2010_intersect <- ca_income_2010  %>% st_filter(eps_la, .predicate = st_intersects)
    # note: st_within only includes tracks that are wholly within a single geomarket; if a tract includes space from two geomarkets, it is excluded
    la_tracts_2010_within <- ca_income_2010  %>% st_filter(eps_la, .predicate = st_within) 

    la_tracts_2020_intersect <- ca_income_2020  %>% st_filter(eps_la, .predicate = st_intersects)
    la_tracts_2020_within <- ca_income_2020  %>% st_filter(eps_la, .predicate = st_within)

    # graph
      #2010 acs
      ggplot() + geom_sf(data = eps_la, fill = NA, color = "red")
      ggplot() + geom_sf(data = la_tracts_2010_intersect, fill = "white", color = "grey")
      ggplot() + geom_sf(data = la_tracts_2010_intersect, fill = 'white', color = "grey") +  geom_sf(data = eps_la, fill = NA, color = "red") 
    
      ggplot() + geom_sf(data = eps_la, fill = NA, color = "red")
      ggplot() + geom_sf(data = la_tracts_2010_within, fill = "white", color = "grey")
      ggplot() + geom_sf(data = la_tracts_2010_within, fill = 'white', color = "grey") +  geom_sf(data = eps_la, fill = NA, color = "red") 

      # 2020 acs
      ggplot() + geom_sf(data = eps_la, fill = NA, color = "red")
      ggplot() + geom_sf(data = la_tracts_2020_intersect, fill = "white", color = "grey")
      ggplot() + geom_sf(data = la_tracts_2020_intersect, fill = 'white', color = "grey") +  geom_sf(data = eps_la, fill = NA, color = "red") 
    
      ggplot() + geom_sf(data = eps_la, fill = NA, color = "red")
      ggplot() + geom_sf(data = la_tracts_2020_within, fill = "white", color = "grey")
      ggplot() + geom_sf(data = la_tracts_2020_within, fill = 'white', color = "grey") +  geom_sf(data = eps_la, fill = NA, color = "red") 
      
      
  ###########
  ###########
  ########### spatial joins
  ###########
  
  # potential question: what are the mean and variance of race of census tracts within a geomarket
    # step1: obtain geometries of geomarkets [done]
      # similar to walker chapter 7 when they obtain geometries of largest 4 CBSAs in TX
    # step2: obtain tract-level data on race for state of CA
    # step 3: from this dataset of race for each CA census tract, identify those that fall within particular geomarkets using `st_join`
      # - arguments:
        # x = x object; all CA census tracts
        # y = y object; object w/ geomarkets of interest
        # `join = st_within`: this is the spatial predicate
        # `left = FALSE`: requests an inner spatial join, returning only those tracts that fall within the four metropolitan areas.
        # `suffix =  c("_tracts", "_metro")`: defines the suffixes to be used for columns that share the same names
      # valu7e/result
        # resulting object has unit of analysis equal to the features of object x (i.e., tracts)
        # retains those features (tracts) that are within object y
        # gains attributes (variables) from object y
    # step 4: now, can do analyses of distribution of characteristics within each geomarket, separately by geomarket
    
    # step 5: Output from a spatial join operation can also be “rolled up” to a larger geography through group-wise data analysis, like this:
      # median_by_metro <- hispanic_by_metro %>% group_by(NAME_metro) %>% 
        # summarize(median_hispanic = median(estimate_tracts, na.rm = TRUE))
      # The grouping column (NAME_metro) and the output of summarize() (median_hispanic) are returned as expected. However, the group_by() %>% summarize() operations also return the dataset as a simple features object with geometry, but in this case with only 4 rows.
      # The analytic process we carried out not only summarized the data by group, it also summarized the geometry by group. 
        # The typical name for this geometric process in geographic information systems is a dissolve operation, where geometries are identified by group and combined to return a single larger geometry. In this case, the Census tracts are dissolved by metropolitan area, returning metropolitan area geometries. 

  
    # step1: obtain geometries of geomarkets [done]
      # similar to walker chapter 7 when they obtain geometries of largest 4 CBSAs in TX
      eps_la
    # step2: obtain tract-level data on race for state of CA
      ca_income_2020
      
    # step 3: from this dataset of race for each CA census tract, identify those that fall within particular geomarkets using `st_join`
      la_geo_2020_within <- st_join(
        x = ca_income_2020,
        y = eps_la,
        join = st_within,
        suffix = c("_tracts", "_geo"),
        left = FALSE
      )
      
      la_geo_2020_within %>% group_by(GEOID) %>% summarize(n_per_group = n()) %>% ungroup() %>% count(n_per_group) # one obs per GEOID
      la_geo_2020_within %>% count(eps)
      
      la_geo_2020_within %>% glimpse()
      la_tracts_2020_within %>% glimpse()
      
      ggplot() + geom_sf(data = la_tracts_2020_within, fill = "white", color = "grey")
      ggplot() + geom_sf(data = la_geo_2020_within, fill = "white", color = "grey")

      ggplot() + geom_sf(data = eps_la, fill = NA, color = "red")
      ggplot() + geom_sf(data = la_geo_2020_within, fill = 'white', color = "grey") +  geom_sf(data = eps_la, fill = NA, color = "red")

      # intersects      
        # this is a many to one merge
        # object la_tracts_2020_intersect has one observation for each CA tract that intersects w/ an LA geomarket
          # 2905 obs
        # object la_geo_2020_intersect assigns each CA tract to the geomarket in intersects with
          # some tracts intersect with multiple geomarkets; so these tracts are assigned to more than one geomarket
          # 3,473 obs; 
            # ~ 520 GEOIDs w/ more than one obs per GEOID
      la_geo_2020_intersect <- st_join(
        x = ca_income_2020,
        y = eps_la,
        join = st_intersects,
        suffix = c("_tracts", "_geo"),
        left = FALSE
      )
      
      la_geo_2020_intersect %>% group_by(GEOID) %>% summarize(n_per_group = n()) %>% ungroup() %>% count(n_per_group) # ~ 520 GEOIDs w/ more than one obs per GEOID
      la_geo_2020_intersect %>% count(eps)
      
      
      la_geo_2020_intersect %>% glimpse()
      la_tracts_2020_intersect %>% glimpse()
                        
      ggplot() + geom_sf(data = la_tracts_2020_intersect, fill = "white", color = "grey")
      ggplot() + geom_sf(data = la_geo_2020_intersect, fill = "white", color = "grey")

      ggplot() + geom_sf(data = eps_la, fill = NA, color = "red")
      ggplot() + geom_sf(data = la_geo_2020_intersect, fill = 'white', color = "grey") +  geom_sf(data = eps_la, fill = NA, color = "red")
      
      ggplot() + geom_sf(data = la_tracts_2020_intersect, fill = 'white', color = "grey") +  geom_sf(data = eps_la, fill = NA, color = "red")

  # step 4: now, can do analyses of distribution of characteristics within each geomarket, separately by geomarket  
    la_geo_2020_intersect %>% glimpse()
    
    la_geo_2020_intersect %>%
      ggplot() + 
      geom_density(aes(x = estimate), color = "navy", fill = "navy", 
                   alpha = 0.4) + 
      theme_minimal() + 
      facet_wrap(~eps) + 
      labs(title = "Distribution of median income by Census tract",
           subtitle = "LA geomarkets",
           y = "Kernel density estimate",
           x = "Median income")      

  # step 5: Output from a spatial join operation can also be “rolled up” to a larger geography through group-wise data analysis, like this:
    la_geo_2020_intersect_group <- la_geo_2020_intersect %>% group_by(eps) %>% 
      summarize(
        n_tracts = n(),
        med_inc = median(estimate, na.rm = TRUE)
      ) %>% arrange(desc(med_inc)) %>% print(n=20)
    
    la_geo_2020_intersect_group
    
    la_geo_2020_intersect_group %>% ggplot() + geom_sf(aes(fill=med_inc)) + geom_sf_label(aes(label = eps))

    eps_la %>% ggplot() + geom_sf() + geom_sf_label(aes(label = eps))
    
   ### examine racial composition
  race_vars <- c(
    white = "B03002_003",
    black = "B03002_004",
    native = "B03002_005",
    asian = "B03002_006",
    hipi = "B03002_007",
    hispanic = "B03002_012"
  )
  
    ca_race_2020 <- get_acs(
      geography = "tract",
      variables = race_vars,
      state = 'CA',
      year = 2020,
      geometry = TRUE,
      summary_var = "B03002_001"
    ) %>%  st_transform(crs = 'WGS84')
    ca_race_2020
    
    ca_race_2020 <- ca_race_2020 %>%  mutate(percent = 100 * (estimate / summary_est))
    ca_race_2020
    
    la_geo_race_2020_intersect <- st_join(
      x = ca_race_2020,
      y = eps_la,
      join = st_intersects,
      suffix = c("_tracts", "_geo"),
      left = FALSE
    )
    
    la_geo_race_2020_intersect %>% glimpse()
    la_geo_race_2020_intersect %>% filter(variable == "white") %>% glimpse()

  #### distribution of percent hispanic, and of percent black    
  la_geo_race_2020_intersect %>% filter(variable == 'hispanic') %>% 
  ggplot() + 
  geom_density(aes(x = percent), color = "navy", fill = "navy", alpha = 0.4) + 
  theme_minimal() + 
  facet_wrap(~eps) + 
  labs(title = "Distribution of Hispanic/Latino population by Census tract",
       subtitle = "by EPS",
       y = "Kernel density estimate",
       x = "Percent Hispanic/Latino in Census tract")    
  
  la_geo_race_2020_intersect %>% filter(variable == 'black') %>% 
  ggplot() + 
  geom_density(aes(x = percent), color = "navy", fill = "navy", alpha = 0.4) + 
  theme_minimal() + 
  facet_wrap(~eps) + 
  labs(title = "Distribution of Hispanic/Latino population by Census tract",
       subtitle = "by EPS",
       y = "Kernel density estimate",
       x = "Percent Black in Census tract")    

  
  
  # step 5: Output from a spatial join operation can also be “rolled up” to a larger geography through group-wise data analysis, like this:
  la_geo_race_2020_intersect
  
  la_geo_race_2020_intersect_group <- la_geo_race_2020_intersect %>% group_by(eps,variable) %>% 
      summarize(
        n_tracts = n(),
        sum_race = sum(estimate, na.rm = TRUE),
        sum_tot = sum(summary_est),
        pct_race = sum_race/sum_tot*100
      ) 
  
  la_geo_race_2020_intersect_group
  
  
  la_geo_race_2020_intersect_group %>% filter(variable == 'hispanic') %>%  ggplot() + geom_sf(aes(fill=pct_race)) + geom_sf_label(aes(label = eps))
  la_geo_race_2020_intersect_group %>% filter(variable == 'black') %>%  ggplot() + geom_sf(aes(fill=pct_race)) + geom_sf_label(aes(label = eps))
  la_geo_race_2020_intersect_group %>% filter(variable == 'white') %>%  ggplot() + geom_sf(aes(fill=pct_race)) + geom_sf_label(aes(label = eps))
  la_geo_race_2020_intersect_group %>% filter(variable == 'asian') %>%  ggplot() + geom_sf(aes(fill=pct_race)) + geom_sf_label(aes(label = eps))
  
  la_geo_race_2020_intersect_group %>% filter(variable == 'hispanic') %>% arrange(desc(pct_race))
  la_geo_race_2020_intersect_group %>% filter(variable == 'black') %>% arrange(desc(pct_race))
  

    
        
###########
########### what to do about geomarkets not cleanly intercepting tracts
###########  I think this process is called "interpolation" and is coverred in walker 7.3 and 7.4 
###########  [start here weds/thursday]
  
  # Areal interpolation refers to the allocation of data from one set of zones to a second overlapping set of zones that may or may not perfectly align spatially.

    # area-weighted areal interpolation
      # This method uses the area of overlap of geometries as the interpolation weights. 
      # From a technical standpoint, an intersection is computed between the origin geometries and the destination geometries. 
      # Weights are then computed as the proportion of the overall origin area comprised by the intersection.
      #st_interpolate_aw(x, to, extensive, ...)
        # x = object of class sf, for which we want to aggregate attributes
          # we want to aggregate the attributes of census tracts
        # to = object of class sf or sfc, with the target geometries
          # target geometries would be geomarkets
        # extensive = logical; if TRUE, the attribute variables are assumed to be spatially extensive (like population) and the sum is preserved, otherwise, spatially intensive (like population density) and the mean is preserved.
    
###########
########### ZCTAs
########### 
########### 
  
  # https://walker-data.com/census-r/mapping-census-data-with-r.html#understanding-and-working-with-zctas
  # The US Census Bureau allows for an approximation of zip code mapping with Zip Code Tabulation Areas, or ZCTAs. ZCTAs are shapes built from Census blocks in which the most common zip code for addresses in each block determines how blocks are allocated to corresponding ZCTAs.
  zcta_df <-  zctas(
    cb = TRUE, 
    year = 2010
  )  
  
  zcta_df %>% names()
  zcta_df %>% glimpse()
  zcta_df
  
  zcta_df2 <-  zctas(
    cb = FALSE, 
    year = 2010
  )  
  
  zcta_df2 %>% names()
  zcta_df2 %>% glimpse()
  zcta_df2  
  
  

  
  ca_income_2010_zcta <- get_acs(
    geography = "zcta",
    variables = "B19013_001",
    state = "CA", 
    year = 2018,
    geometry = TRUE
  )
  
  ca_income_2010_zcta <- get_acs(
    geography = "zcta",
    variables = "B19013_001",
    state = "CA", 
    year = 2012,
    geometry = TRUE,
    survey = 'acs5'
  )  
  
  income_2015_zcta <- get_acs(
    geography = "zcta",
    variables = "B19013_001",
    year = 2015,
    geometry = TRUE,
    survey = 'acs5'
  )    
  
  income_2014_zcta <- get_acs(
    geography = "zcta",
    variables = "B19013_001",
    year = 2015,
    geometry = TRUE,
    survey = 'acs5'
  )      
  
  %>%  st_transform(crs = 'WGS84')
  
  ca_income_2010_zcta  

###########
########### 
########### 
###########     
  
  ## which zips are missing markets
eps_missing <- subset(zip_sp@data, is.na(eps))


  load(file.path(eps_data_dir,'eps-map.Rdata'))
  
  eps_markets %>% attributes()
  
  eps_markets@data %>% str()
  
  eps.markets@data %>% str()
  
  eps_markets@data
  eps.markets@data # these data have two instances of TX11! so btibert's data are messed up, not mine!
  