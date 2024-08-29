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
####################### MODULAR APPROACH
#######################


###################
################### implementing a modular approach
###################
###################

create_layer <- function(map, data, var_name, yr, palette) {
  group_name <- paste(var_name, yr)  # Group name must match exactly
  
  map <- map %>%
    addPolygons(
      data = data,
      fillColor = ~palette(data[[var_name]]),
      weight = 1,
      opacity = 1,
      color = '#808080',  # Line color
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
        "<strong>%s</strong> - %s<br/>%g",
        data$eps, data$eps_name, data[[var_name]]
      ) %>% lapply(htmltools::HTML),
      group = group_name  # Ensure the group name is correctly assigned
    )
  
  return(map)
}

create_legend <- function(map, data, var_name, palette) {
  breaks <- attr(palette, "breaks")
  
  if (is.null(breaks)) {
    breaks <- pretty(data[[var_name]], n = 5)
  }
  
  if (grepl("inc", var_name, ignore.case = TRUE)) {
    labels <- sprintf("$%dk - $%dk", breaks[-length(breaks)]/1000, breaks[-1]/1000)
  } else {
    labels <- sprintf("%.2f", breaks[-length(breaks)])
  }
  
  map <- map %>%
    addLegend(pal = palette, values = data[[var_name]], 
              title = var_name, 
              position = "bottomright", 
              labFormat = labelFormat(prefix = "", suffix = "", between = " - "),
              opacity = 1,
              layerId = paste0("legend-", var_name))
  
  return(map)
}

create_layers_control <- function(map, group_names) {
  map <- map %>%
    addLayersControl(
      baseGroups = group_names,
      position = 'bottomleft',
      options = layersControlOptions(collapsed = FALSE)
    )
  
  return(map)
}

# Modular function to create bins and palettes
create_bins_and_palette <- function(var_name, all_years_data) {
  if (grepl("_inc_", var_name, ignore.case = TRUE)) {
    # Adjust bins to reflect the data in $1000s
    bins <- c(0, 50, 75, 100, 125, 150, 175, 200, 225, 250)
    palette <- colorBin(palette = 'YlGnBu', domain = all_years_data, bins = bins)
    
    # Custom label format for income variables in $1000s
    label_format <- labelFormat(prefix = "$", suffix = "k", between = " - ")
  } else {
    bins <- pretty(range(all_years_data, na.rm = TRUE), n = 5)
    palette <- colorBin(palette = 'YlGnBu', domain = all_years_data, bins = bins)
    
    # Standard label format for non-income variables
    label_format <- labelFormat(suffix = "", between = " - ")
  }
  
  return(list(palette = palette, bins = bins, label_format = label_format))
}

# Main function with display names
create_eps_maps <- function(eps_codes, vars, display_names = NULL) {
  
  # Check for income variables and adjust by dividing by 1000
  for (var_name in vars) {
    if (grepl("_inc_", var_name)) {
      # Apply the division to the relevant variable across all years
      allyr_anal_eps_sf[[var_name]] <- allyr_anal_eps_sf[[var_name]] / 1000
    }
  }
  
  map <- leaflet() %>%
    addProviderTiles(provider = providers$CartoDB.Positron)
  
  group_names <- c()
  palettes <- list()  # Store palettes for each variable
  data_ranges <- list()  # Store data ranges for each variable
  label_formats <- list()  # Store label formats for each variable
  
  for (var_name in vars) {
    all_years_data <- NULL
    
    # First, accumulate data across all years for each variable
    for (yr in c(1980, 2000, 2020)) {
      filtered_data <- allyr_anal_eps_sf %>% filter(year == yr, eps %in% eps_codes)
      if (nrow(filtered_data) > 0 && !all(is.na(filtered_data[[var_name]]))) {
        all_years_data <- c(all_years_data, filtered_data[[var_name]])
      }
    }
    
    # Ensure that the palette is created using the full range of data
    if (!is.null(all_years_data)) {
      palette_info <- create_bins_and_palette(var_name, all_years_data)
      palettes[[var_name]] <- palette_info$palette
      data_ranges[[var_name]] <- all_years_data
      label_formats[[var_name]] <- palette_info$label_format  # Store the label format
    }
    
    # Add layers for each year
    for (yr in c(1980, 2000, 2020)) {
      filtered_data <- allyr_anal_eps_sf %>% filter(year == yr, eps %in% eps_codes)
      if (nrow(filtered_data) > 0 && !all(is.na(filtered_data[[var_name]]))) {
        
        # Display name for layer control with space after comma
        display_name <- if (!is.null(display_names) && var_name %in% names(display_names)) {
          paste(display_names[[var_name]], ", ", yr, sep = "")
        } else {
          paste(var_name, ", ", yr, sep = "")
        }
        
        # Add the layer with the correct group
        map <- map %>%
          addPolygons(
            data = filtered_data,
            fillColor = ~palettes[[var_name]](filtered_data[[var_name]]),
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
              "<strong>%s</strong> - %s<br/>%g",
              filtered_data$eps, filtered_data$eps_name, filtered_data[[var_name]]
            ) %>% lapply(htmltools::HTML),
            group = display_name  # Use display name as the group
          )
        
        group_names <- c(group_names, display_name)
      }
    }
  }
  
  # Add a single legend for each variable
  for (var_name in vars) {
    if (var_name %in% names(palettes)) {
      display_name <- if (!is.null(display_names) && var_name %in% names(display_names)) {
        display_names[[var_name]]
      } else {
        var_name
      }
      
      # Use the label format based on variable type
      label_format <- label_formats[[var_name]]
      
      map <- map %>%
        addLegend(
          pal = palettes[[var_name]], 
          values = data_ranges[[var_name]],  # Use the combined data range for the variable
          title = display_name,  
          position = "topright",
          opacity = 1,
          labFormat = label_format  # Apply the custom label format
        )
    }
  }
  
  # Add layers control
  map <- map %>%
    addLayersControl(
      baseGroups = unique(group_names),  # Use baseGroups for mutually exclusive layers
      position = 'bottomleft',
      options = layersControlOptions(collapsed = FALSE)
    )
  
  return(map)
}


# Example usage with display names:
display_names <- list(
  "pct_hisp_all" = "% Hispanic",
  "mean_inc_house" = "Mean income",
  "med_inc_house_med" = "Median income",
  "pct_nhisp_asian" = "% Asian, non-Hispanic",
  "pct_nhisp_white" = "% White, non-Hispanic",
  "pct_nhisp_black" = "% Black, non-Hispanic"
)

# Test with full set of variables
create_eps_maps(chi_eps_codes, c("pct_hisp_all", "med_inc_house_med", "pct_nhisp_asian"), display_names)

create_eps_maps(chi_eps_codes, c("pct_hisp_all", "mean_inc_house", "pct_nhisp_asian"), display_names)

# Test with full set of variables
create_eps_maps(chi_eps_codes, c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all","pct_nhisp_asian"), display_names)





# Example usage:
create_eps_maps(chi_eps_codes, c("pct_hisp_all", "mean_inc_house", "pct_nhisp_asian"))
create_eps_maps(socal_eps_codes, c("pct_hisp_all", "mean_inc_house", "pct_nhisp_asian"))
create_eps_maps(bay_eps_codes, c("pct_hisp_all", "mean_inc_house", "pct_nhisp_asian"))



