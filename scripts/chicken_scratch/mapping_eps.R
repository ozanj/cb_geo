################################################################################
## [ PROJ ] < College Board Geomarket >
## [ FILE ] < mapping_eps.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 8/22/2024
## [ DESC ] < work with EPS-level sf objects and other objects to create interactive maps of characteristics associated with geomarkets >
################################################################################

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
library(leaflet)
library(shiny)
### DIRECTORY PATHS

getwd()

data_dir <- file.path('.','data') # main data directory
list.files(path = data_dir)

d1980_data_dir <-   file.path('.',data_dir,'1980_decennial') # main data directory
list.files(path = d1980_data_dir)

d2000_data_dir <-   file.path('.',data_dir,'2000_decennial') # 
list.files(path = d2000_data_dir)

acs2020_data_dir <-   file.path('.',data_dir,'2020_acs') # 
list.files(path = acs2020_data_dir)

analysis_data_dir <- file.path('.',data_dir,'analysis_data') # analysis data directory
list.files(path = analysis_data_dir)

# can't save shape files in the repo cuz they too big
shape_dir <-   file.path('.','..','cb_geomarket_shape') # main data directory
list.files(path = shape_dir)

eps_data_dir <- file.path(data_dir,'eps_market') # has eps geomarket data and spaical data files
list.files(path = eps_data_dir)

scripts_dir <- file.path('.','scripts') # 
list.files(path = scripts_dir)

##### LOAD EPS-LEVEL DATA

# read in file w/ EPS name



# load EPS data
  #1980
  load(file = file.path(analysis_data_dir, 'd1980_stf1f3_anal_eps_sf.RData'))
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
  # Reproject the data to WGS84 (longitude/latitude)
  st_transform(d1980_stf1f3_anal_eps_sf, crs = 4326)

  allyr_anal_eps_sf %>% glimpse()


######## CREATE VECTORS OF EPS CODES FOR PARTICULAR METRO AREAS


socal_eps_codes <- paste0("CA", 14:28) # socal 
bay_eps_codes <- c(paste0("CA ", 4:9), paste0("CA", 10:11)) # bay area
bay_eps_codes 
ca_eps_codes <- c(paste0("CA ", 1:9), paste0("CA", 10:34))
chi_eps_codes <- c(paste0("IL ", 7:9), paste0("IL", 10:13))
chi_eps_codes

cleveland_eps_codes <- paste0("OH ", 2:6) #
cleveland_eps_codes

############## CREATING LEAFLET MAPS
  # code for student list empirics maps: https://github.com/mpatricia01/public_requests_eda/blob/main/scripts/interactive_maps.R

#######################
#######################
#######################

create_eps_maps <- function(eps_codes) {
  
  # Define common palettes and breaks for all years
  breaks_inc <- c(0, 50000, 75000, 100000, 125000, 150000, 175000, 200000)
  pal_inc <- colorBin(
    palette = 'YlGnBu',
    domain = allyr_anal_eps_sf$med_inc_house_med,
    bins = breaks_inc
  )
  
  pal_white <- colorNumeric(
    palette = "YlGnBu",
    domain = allyr_anal_eps_sf$pct_nhisp_white,
    n = 5
  )
  
  pal_black <- colorNumeric(
    palette = 'YlGnBu',
    domain = allyr_anal_eps_sf$pct_nhisp_black,
    n = 5
  )
  
  # Create the leaflet map
  map <- leaflet() %>%
    addProviderTiles(provider = providers$CartoDB.Positron)
  
  # Initialize vectors to store group names
  group_names_inc <- c()
  group_names_white <- c()
  group_names_black <- c()
  
  # Add layers for each year
  for (yr in c(1980, 2000, 2020)) {
    
    # Update the group name for median income
    group_name_inc <- paste("Median income", yr)
    group_name_white <- paste("%White (non-Hispanic)", yr)
    group_name_black <- paste("%Black (non-Hispanic)", yr)
    
    # Store group names in vectors
    group_names_inc <- c(group_names_inc, group_name_inc)
    group_names_white <- c(group_names_white, group_name_white)
    group_names_black <- c(group_names_black, group_name_black)
    
    filtered_data <- allyr_anal_eps_sf %>% filter(year == yr, eps %in% eps_codes)
    
    # Add Median income layer for each year
    map <- map %>%
      addPolygons(
        data = filtered_data,
        fillColor = ~pal_inc(med_inc_house_med),
        weight = 1,
        opacity = 1,
        color = '#808080',
        dashArray = '3',
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 4,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = sprintf(
          "<strong>%s</strong> - %s<br/>$%gk median income",
          filtered_data$eps, filtered_data$eps_name, round(filtered_data$med_inc_house_med / 1000)
        ) %>% lapply(htmltools::HTML),
        group = group_name_inc  # Use updated group name variable
      ) %>%
      # Add pct_nhisp_white layer for each year
      addPolygons(
        data = filtered_data,
        fillColor = ~pal_white(pct_nhisp_white),
        weight = 1,
        opacity = 1,
        color = '#808080',
        dashArray = '3',
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 4,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = sprintf(
          "<strong>%s</strong> - %s<br/>%g%% NH White",
          filtered_data$eps, filtered_data$eps_name, round(filtered_data$pct_nhisp_white)
        ) %>% lapply(htmltools::HTML),
        group = group_name_white
      ) %>%
      # Add pct_nhisp_black layer for each year
      addPolygons(
        data = filtered_data,
        fillColor = ~pal_black(pct_nhisp_black),
        weight = 1,
        opacity = 1,
        color = '#808080',
        dashArray = '3',
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 4,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = sprintf(
          "<strong>%s</strong> - %s<br/>%g%% NH Black",
          filtered_data$eps, filtered_data$eps_name, round(filtered_data$pct_nhisp_black)
        ) %>% lapply(htmltools::HTML),
        group = group_name_black
      )
  }
  
  # Add legends for each group, but hide them initially using custom HTML classes
  map <- map %>%
    addLegend(
      position = "topright",
      pal = pal_inc,
      values = allyr_anal_eps_sf$med_inc_house_med,
      title = "Median Income",
      labFormat = labelFormat(prefix = "$", suffix = "k", transform = function(x) x/1000),
      opacity = 0.7,
      className = "legend-inc"
    ) %>%
    addLegend(
      position = "topright",
      pal = pal_white,
      values = allyr_anal_eps_sf$pct_nhisp_white,
      title = "% White (non-Hispanic)",
      labFormat = labelFormat(suffix = "%"),
      opacity = 0.7,
      className = "legend-white"
    ) %>%
    addLegend(
      position = "topright",
      pal = pal_black,
      values = allyr_anal_eps_sf$pct_nhisp_black,
      title = "% Black (non-Hispanic)",
      labFormat = labelFormat(suffix = "%"),
      opacity = 0.7,
      className = "legend-black"
    ) %>%
    
    # Add layers control to switch between years using the group name variables
    addLayersControl(
      baseGroups = c(group_names_inc, group_names_white, group_names_black),
      position = 'bottomleft',
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    
    # Use JavaScript to show/hide legends based on the active base layer
    htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        
        // Initially hide all legends
        document.querySelector('.legend-inc').style.display = 'none';
        document.querySelector('.legend-white').style.display = 'none';
        document.querySelector('.legend-black').style.display = 'none';

        // Apply custom CSS for white, non-transparent borders
        var legends = document.getElementsByClassName('leaflet-control');
        for (var i = 0; i < legends.length; i++) {
          legends[i].style.border = '2px solid white';
          legends[i].style.background = 'rgba(255, 255, 255, 1)';
          legends[i].style.padding = '8px';
          legends[i].style.boxShadow = '0 0 15px rgba(0, 0, 0, 0.3)';
        }

        map.on('baselayerchange', function(e) {
          // Hide all legends
          document.querySelector('.legend-inc').style.display = 'none';
          document.querySelector('.legend-white').style.display = 'none';
          document.querySelector('.legend-black').style.display = 'none';

          // Show the relevant legend
          if (e.name.includes('Median income')) { 
            document.querySelector('.legend-inc').style.display = 'block';
          } else if (e.name.includes('%White (non-Hispanic)')) {
            document.querySelector('.legend-white').style.display = 'block';
          } else if (e.name.includes('%Black (non-Hispanic)')) {
            document.querySelector('.legend-black').style.display = 'block';
          }
        });
      }
    ")
  
  return(map)
}
# additional capabilities to add:
  # can switch which metro area from the map
  # pct_nhisp_white
  # pct_nhisp_black
  # experiment with quantiles vs. bins vs. continuous
  # advise on better color schemes


# Example usage
create_eps_maps(socal_eps_codes)

create_eps_maps(bay_eps_codes)

create_eps_maps(chi_eps_codes)

create_eps_maps(cleveland_eps_codes)
create_eps_maps(c('OH 3','OH 4','OH 5'))


allyr_anal_eps_sf %>% filter(year == 2020) %>% select(eps,eps_name,pct_nhisp_black) %>% View()

#####################
##################### this is the basic version that does not use htmlwidgets/javascript
#####################

create_eps_maps <- function(yr, eps_codes) {
  
  filtered_data <- allyr_anal_eps_sf %>% filter(year == yr, eps %in% eps_codes)

  
  # color of fill for median income
  breaks_inc <- c(0, 50000, 75000, 100000, 125000, 150000, 175000, 200000)
  pal_inc <- colorBin(
    palette = 'YlGnBu', # Use a predefined color palette (Yellow-Orange-Red)
    domain = allyr_anal_eps_sf$med_inc_house_med,
    bins = breaks_inc
  )
  
  # color of fill for pct_nhisp_white
  pal_white <- colorNumeric(
    palette = "YlGnBu",
    domain = allyr_anal_eps_sf$pct_nhisp_white,
    n = 5
  )
  
  # color of fill for pct_nhisp_black
  pal_black <- colorNumeric(
    palette = 'YlGnBu',
    domain = allyr_anal_eps_sf$pct_nhisp_black,
    n = 5
  )
  
  # labels for median income
  labels_inc <- sprintf(
    "<strong>%s</strong> - %s<br/>$%gk median income",
    filtered_data$eps, filtered_data$eps_name, round(filtered_data$med_inc_house_med / 1000)
  ) %>% lapply(htmltools::HTML)
  
  # labels for pct_nhisp_white
  labels_white <- sprintf(
    "<strong>%s</strong> - %s<br/>%g%% NH White",
    filtered_data$eps, filtered_data$eps_name, round(filtered_data$pct_nhisp_white)
  ) %>% lapply(htmltools::HTML)
  
  # labels for pct_nhisp_black
  labels_black <- sprintf(
    "<strong>%s</strong> - %s<br/>%g%% NH Black",
    filtered_data$eps, filtered_data$eps_name, round(filtered_data$pct_nhisp_black)
  ) %>% lapply(htmltools::HTML)
  
  leaflet(data = filtered_data) %>%
    addProviderTiles(
      provider = providers$CartoDB.Positron
    ) %>%  
    # Median income layer
    addPolygons(
      fillColor = ~pal_inc(med_inc_house_med),
      weight = 1,
      opacity = 1,
      color = 'white',
      dashArray = '3',
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 4,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = labels_inc,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      ),
      group = "Median inc"
    ) %>%
    # pct_nhisp_white layer
    addPolygons(
      fillColor = ~pal_white(pct_nhisp_white),
      weight = 1,
      opacity = 1,
      color = 'white',
      dashArray = '3',
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 4,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = labels_white,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      ),
      group = "%White (non-Hispanic)"
    ) %>%
    # pct_nhisp_black layer
    addPolygons(
      fillColor = ~pal_black(pct_nhisp_black),
      weight = 1,
      opacity = 1,
      color = 'white',
      dashArray = '3',
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 4,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = labels_black,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      ),
      group = "%Black (non-Hispanic)"
    ) %>%
    # Layers control
    addLayersControl(
      position = c('bottomleft'),
      baseGroups = c('Median inc', '%White (non-Hispanic)', '%Black (non-Hispanic)'),
      options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    # Legend for median income
    addLegend(
      position = "topright",
      pal = pal_inc,
      values = ~med_inc_house_med,
      title = "Median Income",
      labFormat = labelFormat(prefix = "$", suffix = "k", transform = function(x) x/1000),
      opacity = 0.7
    ) %>%
    # Legend for pct_nhisp_white
    addLegend(
      position = "topright",
      pal = pal_white,
      values = ~pct_nhisp_white,
      title = "%White<br>(non-Hispanic)",
      labFormat = labelFormat(suffix = "%"),
      opacity = 0.7
    ) %>%
    # Legend for pct_nhisp_black
    addLegend(
      position = "topright",
      pal = pal_black,
      values = ~pct_nhisp_black,
      title = "%Black<br>(non-Hispanic)",
      labFormat = labelFormat(suffix = "%"),
      opacity = 0.7
    )
}

# additional capabilities to add:
# can switch which metro area from the map
# can switch which year of data from the map
# can change which variables are shown in legend
# pct_nhisp_white
# pct_nhisp_black
# experiment with quantiles vs. bins vs. continuous
# add legend; 
# advise on better color schemes


create_eps_maps(yr = 1980, eps_codes = bay_eps_codes)

create_eps_maps(1980,bay_eps_codes)
create_eps_maps(2020,bay_eps_codes)

create_eps_maps(1980,socal_eps_codes)

create_eps_maps(1980,chi_eps_codes)
create_eps_maps(2000,chi_eps_codes)
create_eps_maps(2020,chi_eps_codes)

create_eps_maps(cleveland_eps_codes)
create_eps_maps(c('OH 3','OH 4','OH 5'))
# Example usage with chi_eps_codes
#create_eps_maps(chi_eps_codes)

create_eps_maps(1980,bay_eps_codes)

create_eps_maps(chi_eps_codes)
#create_eps_maps(bay_eps_codes)






##### PLAYING AROUND W/ PLOTTING

# plotting all geomarkets (including AK and HI)
ggplot(data = d1980_stf1f3_anal_eps_sf) +
  geom_sf() +
  theme_minimal() +
  labs(title = "College Board Geomarkets (EPS Codes)",
       subtitle = "Mapping all EPS regions in the US")

# Create an object containing the EPS codes to exclude
excluded_eps_codes <- c('AK 1', 'AK 2', 'HI 1', 'HI 2')

# Filter out the EPS codes and plot the remaining regions
ggplot(data = d1980_stf1f3_anal_eps_sf %>% filter(!eps %in% excluded_eps_codes)) +
  geom_sf() +
  theme_minimal() +
  labs(title = "College Board Geomarkets (EPS Codes) Excluding Alaska and Hawaii",
       subtitle = "Mapping all EPS regions in the contiguous US")

# CA ONLY
  # Create a vector of EPS codes for California
  ca_eps_codes <- c(paste0("CA ", 1:9), paste0("CA", 10:34))
  ca_eps_codes
  
  # Filter for California EPS codes and plot
  ggplot(data = d1980_stf1f3_anal_eps_sf %>% filter(eps %in% ca_eps_codes)) +
    geom_sf() +
    theme_minimal() +
    labs(title = "College Board Geomarkets (EPS Codes) for California",
         subtitle = "Mapping all EPS regions in California")

# SoCal
  
  # Create a vector of EPS codes for urban Southern California
  socal_eps_codes <- paste0("CA", 14:28)
  
  # Filter for Southern California EPS codes and plot
  ggplot(data = d1980_stf1f3_anal_eps_sf %>% filter(eps %in% socal_eps_codes)) +
    geom_sf() +
    theme_minimal() +
    labs(title = "College Board Geomarkets (EPS Codes) for Urban Southern California",
         subtitle = "Mapping EPS regions CA14 through CA28")
  
# socal without cataline island [this didn't work!]
  
  # Define the bounding box for Catalina Island
  catalina_bbox <- st_bbox(c(xmin = -118.65, xmax = -118.3, ymin = 33.3, ymax = 33.5), crs = st_crs(d1980_stf1f3_anal_eps_sf))
  
  # Filter for Southern California EPS codes, excluding Catalina Island
  ggplot(data = d1980_stf1f3_anal_eps_sf %>% 
           filter(eps %in% socal_eps_codes) %>% 
           filter(!apply(st_intersects(geometry, st_as_sfc(catalina_bbox), sparse = FALSE), 1, any))) +
    geom_sf() +
    theme_minimal() +
    labs(title = "College Board Geomarkets for Urban Southern California (Excluding Catalina Island)",
         subtitle = "Mapping EPS regions CA14 through CA28")
  
# add eps labels to socal map
  
  # Create a vector of EPS codes for urban Southern California
  socal_eps_codes <- paste0("CA", 14:28)
  
  # Filter for Southern California EPS codes and plot
  ggplot(data = d1980_stf1f3_anal_eps_sf %>% filter(eps %in% socal_eps_codes)) +
    geom_sf() +
    geom_sf_text(aes(label = eps), size = 3, color = "black") +
    theme_minimal() +
    labs(title = "College Board Geomarkets (EPS Codes) for Urban Southern California",
         subtitle = "Mapping EPS regions CA14 through CA28")
  
  # Filter for Southern California EPS codes and create a choropleth map
  ggplot(data = d1980_stf1f3_anal_eps_sf %>% filter(eps %in% socal_eps_codes)) +
    geom_sf(aes(fill = pct_edu_baplus_all)) + # mean_inc_house med_inc_house_med_all pct_edu_baplus_all
    geom_sf_text(aes(label = eps), size = 3, color = "black") +
    scale_fill_viridis_c(option = "plasma", name = "Median Household Income") +
    theme_minimal() +
    labs(title = "Choropleth Map of Median Household Income in Urban Southern California",
         subtitle = "EPS regions CA14 through CA28")  
  
