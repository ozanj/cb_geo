################################################################################
## [ PROJ ] < student list hsls >
## [ FILE ] < make_data.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 5/8/2023
## [ DESC ] < Create HSLS Dataset >
################################################################################


############## QUESTIONS
  #
  # should I be merging the geomarket geographies to zctas or tracts or zip codes?
    # see: https://walker-data.com/census-r/mapping-census-data-with-r.html#understanding-and-working-with-zctas




### SETTINGS
rm(list = ls())
options(max.print=1000)

### LIBRARIES
library(tidyverse)
library(lubridate)
library(haven)
library(labelled)
library(tidycensus)
library(sf)
library(tigris)
#library(stars)
#library(spatstat)


### DIRECTORY PATHS

getwd()

data_dir <- file.path('.','data') # 
  list.files(path = data_dir)
  
scripts_dir <- file.path('.','scripts') # 
  list.files(path = scripts_dir)
  
### FUNCTIONS


#### READ AND WRANGLE

  file.path(data_dir,'eps_market','eps-map.Rdata')
  load(file.path(data_dir,'eps_market','eps-map.Rdata'))

  # read shape file
  # https://cran.r-project.org/web/packages/sf/vignettes/sf2.html
  file.path(data_dir,'eps_market',"US_Zip_-_Lower_48.shp")
  myzip2 <- st_read(file.path(data_dir,'eps_market',"US_Zip_-_Lower_48.shp"))
  
#The Rdata file currently includes 3 objects. This will change as I finalize the map files.
  # 1. eps.missing which is a data frame of zip codes that still need to be associated with an EPS territory
  # 2. myzip which is a SpatialPolysDataFrame object. It is the map of the lower 48 by zip code. To plot, simply use the command plot(myzip) but note it will take a minute or so depending on your machine
  # 3. eps.markets is the working draft of the eps markets map and is the same type as myzip  
  
  # eps.missing # data frame
  
  # spatial polygon data frame
  
  #plot(myzip)
  #subset(eps.markets, eps.markets$eps %in% c("IL 7", "IL 8", "IL 9","IL10", "IL11", "IL12", "IL13"))
  
  #plot(eps.markets)
  
# Convert sp objects to sf objects
  class(myzip)
  class(eps.markets)
  
  sf_myzip <- st_as_sf(x=myzip)
  sf_eps_markets <- st_as_sf(x=eps.markets)
  
  myzip2 %>% glimpse()
  sf_myzip %>% glimpse()
  
  sum(myzip2$ObjectID == sf_myzip$ObjectID)

# investigate objects    
  sf_myzip %>% str() # each "feature" is a zip code; 29,842 zip codes
  sf_myzip %>% attributes()
  
  sf_eps_markets %>% str() # each "feature" is a CB geomarket; 301 geomarkets
  sf_eps_markets %>% attributes()
  
  myzip %>% str(max.level = 3)
  myzip %>% attributes()
  
  eps.markets %>% str(max.level = 3)
  eps.markets %>% attributes()  
  
# simple plots
  
  plot(sf_eps_markets)
  
  #plot(sf_myzip) # plots one map per variable
  plot(sf_myzip$geometry)
  
# things to figure out
  
  # what is the source of the zip code file? which version of ACS or Census is this associated with?
  
  # 
  
  plot(sf_eps_markets$geometry)
  sf_eps_markets %>% ggplot() + geom_sf()
  
  ggplot() +
    geom_sf(data=sf_eps_markets) +
    geom_sf(data=sf_myzip)
  
  
  
  sf_eps_markets %>% print(n=60)
  ca_eps <- sf_eps_markets %>% filter(str_sub(string = eps,start = 1L, end = 2L)=="CA")
  ma_eps <- sf_eps_markets %>% filter(str_sub(string = eps,start = 1L, end = 2L)=="MA")
  ny_eps <- sf_eps_markets %>% filter(str_sub(string = eps,start = 1L, end = 2L)=="NY")
  
  ?st_sub
  subset(eps.markets, eps.markets$eps %in% c("IL 7", "IL 8", "IL 9","IL10", "IL11", "IL12", "IL13"))
  
  ggplot() + geom_sf(data=ca_eps)
  ggplot() + geom_sf(data=ny_eps)
  ggplot() + geom_sf(data=ma_eps)
  
  plot(ca_eps)
  
  
  ca_income <- get_acs(
    geography = "tract",
    variables = "B19013_001",
    state = "CA", 
    year = 2010,
    geometry = TRUE
  )
  
  ca_income
  
  plot(ca_income["estimate"])
  
  ggplot(data = ca_income) +  geom_sf()
  ggplot(data = ca_income, aes(fill = estimate)) +  geom_sf()
  
  ggplot(data = ca_income, aes(fill = estimate)) + 
    geom_sf() + 
    scale_fill_distiller(palette = "RdPu", 
                         direction = 1) + 
    labs(title = "Median income CA by tract, 2019",
         caption = "Data source: 2019 1-year ACS, US Census Bureau",
         fill = "ACS estimate") + 
    theme_void()
  
  
## la county
  
  la_race <- get_decennial(
  geography = "tract",
  state = "CA",
  county = "Los Angeles",
  variables = c(
    Hispanic = "P2_002N",
    White = "P2_005N",
    Black = "P2_006N",
    Native = "P2_007N",
    Asian = "P2_008N"
  ),
  summary_var = "P2_001N",
  year = 2020,
  geometry = TRUE
) %>%
  mutate(percent = 100 * (value / summary_value))
  
la_race

la_hispanic <- filter(la_race, variable == "Hispanic")

ggplot(data = la_hispanic) +  geom_sf()
ggplot(data = la_hispanic, aes(fill = percent)) +  geom_sf() +
  scale_fill_distiller(palette = "RdPu", direction = 1)


hist(la_hispanic$percent)



#######################
####################### walker chapter 7
#######################

# spatial overlay: representation of geographic datasets as layers in a GIS
  # By using spatial analytic tools, a researcher could answer questions like “How many customers live within a given Census tract?” or “Which roads intersect a given Census tract?”

# CRS = coordinate reference system; in any spatialoverlay, all layers must have the same CRS

la_hispanic

sf_eps_markets # says CRS == NA
sf_eps_markets[['geometry']] %>% str()

geo <- sf_eps_markets[['geometry']]
geo %>% str()
geo %>% length()
geo %>% typeof()
geo[1]
geo[[1]] %>% typeof()
geo[[2]]
geo[[3]] %>% length()
geo[[301]] %>% length()
geo[[301]]
la_race # CRS = NAD83

library(crsuggest)
suggest_crs(sf_eps_markets)
guess_crs(sf_eps_markets, target_location = c(-88.30671,30.22767))

st_transform(sf_eps_markets, crs = "NAD83") # cannot transform sfc object with missing crs
NAD83