################################################################################
## [ PROJ ] < College Board Geomarket >
## [ FILE ] < leaflet_maps.R >
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
#Sys.getenv("CENSUS_API_KEY") # retreive API key


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
library(kableExtra)

### DIRECTORY PATHS

# run script that creates directories
source(file = file.path('scripts', 'directories.R'))

######## DATA PREP

# run script that appends data
list.files(path = file.path('.',scripts_dir))
source(file = file.path(scripts_dir, 'append_census.R'))

# script that creates character vectors for EPS codes
source(file = file.path(scripts_dir, 'metro_eps_codes.R'))




############## CREATING LEAFLET MAPS
#######################
####################### MODULAR APPROACH; THIS IS WHERE FUNCTION CALL SAYS POLYGONS BASED ON TRACT-LEVEL DATA OR EPS-LEVEL DATA
#######################
# code for student list empirics maps: https://github.com/mpatricia01/public_requests_eda/blob/main/scripts/interactive_maps.R


# Modular function to create bins and palettes
create_bins_and_palette <- function(var_name, all_years_data, mapping_level = "eps") {
  if (grepl("_inc_", var_name, ignore.case = TRUE)) {
    # Adjust bins dynamically if mapping_level = "tract"
    if (mapping_level == "tract") {
      bins <- pretty(range(all_years_data, na.rm = TRUE), n = 6)  # Dynamic bins for income variables
    } else {
      bins <- c(0, 50, 75, 100, 125, 150, 175, 200, 225, 250)  # Fixed bins for EPS level
    }
    palette <- colorBin(palette = 'YlGnBu', domain = all_years_data, bins = bins)
    
    # Custom label format for income variables
    label_format <- labelFormat(prefix = "$", suffix = "k", between = " - ")
  } else {
    # Standard bins for non-income variables
    bins <- pretty(range(all_years_data, na.rm = TRUE), n = 5)
    palette <- colorBin(palette = 'YlGnBu', domain = all_years_data, bins = bins)
    
    # Standard label format for non-income variables
    label_format <- labelFormat(suffix = "", between = " - ")
  }
  
  return(list(palette = palette, bins = bins, label_format = label_format))
}

# Main function to create EPS or Census Tract maps
create_eps_maps <- function(eps_codes, vars, mapping_level = "eps", display_names = NULL, eps_border_weight = 3) {
  
  # Determine the correct data frame to use
  if (mapping_level == "tract") {
    primary_data <- allyr_anal_tract_sf
  } else {
    primary_data <- allyr_anal_eps_sf
  }
  
  # Check for income variables and adjust by dividing by 1000
  for (var_name in vars) {
    if (grepl("_inc_", var_name)) {
      primary_data[[var_name]] <- primary_data[[var_name]] / 1000
    }
  }
  
  # Create the Leaflet map
  map <- leaflet() %>%
    addProviderTiles(provider = providers$CartoDB.Positron) %>%
    addMapPane("epsPane", zIndex = 450)  # Create a pane for EPS borders with a higher z-index
  
  group_names <- c()
  palettes <- list()
  data_ranges <- list()
  label_formats <- list()
  
  for (var_name in vars) {
    all_years_data <- NULL
    
    # Accumulate data across all years for each variable
    for (yr in c(1980, 2000, 2020)) {
      filtered_data <- primary_data %>% filter(year == yr, eps %in% eps_codes)
      if (nrow(filtered_data) > 0 && !all(is.na(filtered_data[[var_name]]))) {
        all_years_data <- c(all_years_data, filtered_data[[var_name]])
      }
    }
    
    # Create palette and bins using the full range of data, considering mapping level
    if (!is.null(all_years_data)) {
      palette_info <- create_bins_and_palette(var_name, all_years_data, mapping_level)
      palettes[[var_name]] <- palette_info$palette
      data_ranges[[var_name]] <- all_years_data
      label_formats[[var_name]] <- palette_info$label_format
    }
    
    # Add layers for each year
    for (yr in c(1980, 2000, 2020)) {
      filtered_data <- primary_data %>% filter(year == yr, eps %in% eps_codes)
      if (nrow(filtered_data) > 0 && !all(is.na(filtered_data[[var_name]]))) {
        
        # Display name for layer control
        display_name <- if (!is.null(display_names) && var_name %in% names(display_names)) {
          paste(display_names[[var_name]], ", ", yr, sep = "")
        } else {
          paste(var_name, ", ", yr, sep = "")
        }
        
        # Add the primary layer (EPS or Tract)
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
              bringToFront = TRUE  # Ensure polygons are brought to the front when hovered
            ),
            label = sprintf(
              "<strong>%s</strong> - %s<br/>%g",
              filtered_data$eps, filtered_data$eps_name, filtered_data[[var_name]]
            ) %>% lapply(htmltools::HTML),
            group = display_name
          )
        
        group_names <- c(group_names, display_name)
      }
    }
  }
  
  # Add EPS borders directly when mapping at the tract level, using the custom pane
  if (mapping_level == "tract") {
    eps_data <- allyr_anal_eps_sf %>% filter(eps %in% eps_codes)
    map <- map %>%
      addPolygons(
        data = eps_data,
        fill = FALSE,  # No fill, just borders
        color = "purple",  # Black borders for EPS
        weight = eps_border_weight,  # Thicker line for EPS borders
        opacity = 1,
        options = pathOptions(pane = "epsPane")  # Ensure EPS borders are rendered in the custom pane
      )
  }
  
  # Add a single legend for each variable
  for (var_name in vars) {
    if (var_name %in% names(palettes)) {
      display_name <- if (!is.null(display_names) && var_name %in% names(display_names)) {
        display_names[[var_name]]
      } else {
        var_name
      }
      
      label_format <- label_formats[[var_name]]
      
      map <- map %>%
        addLegend(
          pal = palettes[[var_name]], 
          values = data_ranges[[var_name]],
          title = display_name,  
          position = "topright",
          opacity = 1,
          labFormat = label_format
        )
    }
  }
  
  # Add layers control without the EPS borders as they are always shown at the tract level
  map <- map %>%
    addLayersControl(
      baseGroups = unique(group_names), 
      position = 'bottomleft',
      options = layersControlOptions(collapsed = FALSE)
    )
  
  return(map)
}

# Example usage with display names:
display_names <- list(
  "pct_hisp_all" = "% Hispanic",
  "mean_inc_house" = "Mean income",
  "med_inc_house" = "Median income",
  "pct_nhisp_asian" = "% Asian, non-Hispanic",
  "pct_nhisp_white" = "% White, non-Hispanic",
  "pct_nhisp_black" = "% Black, non-Hispanic"
)

# figuring out socal
create_eps_maps(socal_eps_codes, c("med_inc_house","pct_nhisp_white","pct_nhisp_black","pct_hisp_all","pct_nhisp_asian"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)


create_eps_maps(orange_county_eps_codes, c("med_inc_house","pct_nhisp_white","pct_nhisp_black","pct_hisp_all","pct_nhisp_asian"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)
create_eps_maps(orange_county_eps_codes, c("med_inc_house","pct_nhisp_white","pct_nhisp_black","pct_hisp_all","pct_nhisp_asian"), mapping_level = "tract", display_names = display_names, eps_border_weight = 4)


###
create_eps_maps(chi_eps_codes, c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all","pct_nhisp_asian", "med_inc_house"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)
create_eps_maps(chi_eps_codes, c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all","pct_nhisp_asian", "med_inc_house"), mapping_level = "tract", display_names = display_names, eps_border_weight = 4)


create_eps_maps(philly_eps_codes, c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all","pct_nhisp_asian", "med_inc_house"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)
create_eps_maps(philly_eps_codes, c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all","pct_nhisp_asian", "med_inc_house"), mapping_level = "tract", display_names = display_names, eps_border_weight = 4)


# bay area
create_eps_maps(bay_eps_codes, c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all","pct_nhisp_asian", "med_inc_house"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)
create_eps_maps(bay_eps_codes, c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all","pct_nhisp_asian", "med_inc_house"), mapping_level = "tract", display_names = display_names, eps_border_weight = 4)


# Create Tract-level map with EPS borders
create_eps_maps(socal_eps_codes, c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all","pct_nhisp_asian", "med_inc_house"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)

create_eps_maps(socal_eps_codes, c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all","pct_nhisp_asian", "med_inc_house"), mapping_level = "tract", display_names = display_names, eps_border_weight = 4)

# ga
create_eps_maps(c('GA 1','GA 2','GA 3', 'GA 4'), c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all","pct_nhisp_asian", "med_inc_house"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)

create_eps_maps(c('GA 1','GA 2','GA 3', 'GA 4'), c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all","pct_nhisp_asian", "med_inc_house"), mapping_level = "tract", display_names = display_names, eps_border_weight = 4)


# long island

long_island_eps_codes <- c(paste0('NY',16:21))
long_island_eps_codes

create_eps_maps(long_island_eps_codes, c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all", "mean_inc_house"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)

# westchester and rockland counties
create_eps_maps(c('NY13','NY15'), c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all", "mean_inc_house"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)


# new jersey metro area
nj_metro_eps_codes
create_eps_maps(nj_metro_eps_codes, c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all", "med_inc_house"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)

create_eps_maps(nj_eps_codes, c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all", "med_inc_house"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)

# DALLAS
create_eps_maps(c('TX19','TX20','TX21','TX22','TX23','TX24'), c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all", "med_inc_house"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)

create_eps_maps(c('TX19','TX20','TX21','TX22','TX23','TX24'), c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all", "med_inc_house"), mapping_level = "tract", display_names = display_names, eps_border_weight = 4)

# HOUSTON
create_eps_maps(c('TX15','TX16','TX17','TX18'), c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all","pct_nhisp_asian", "med_inc_house"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)

create_eps_maps(c('TX15','TX16','TX17','TX18'), c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all","pct_nhisp_asian", "med_inc_house"), mapping_level = "tract", display_names = display_names, eps_border_weight = 4)

# CHICAGO
create_eps_maps(chi_eps_codes, c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all","pct_nhisp_asian", "med_inc_house"), mapping_level = "eps", display_names = display_names)
create_eps_maps(chi_eps_codes, c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all","pct_nhisp_asian", "med_inc_house"), mapping_level = "tract", display_names = display_names, eps_border_weight = 4)

# DMV
  # MARYLAND
    # Baltimore metro: 'MD 3','MD 7'
    # DC metro: 'MD 2','MD 5'
    create_eps_maps(c('MD 3','MD 7','MD 2','MD 5'), c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all","pct_nhisp_asian", "med_inc_house"), mapping_level = "eps", display_names = display_names)

  # VIRGINA
    # Greater Alexandria: 'VA 1','VA 2'
      # VA 1 = Arlington and Alexandria
      # VA 2 = Fairfax county
      # VA 3 = North central Virginia
    
    create_eps_maps(c('VA 1','VA 2','VA 3','DC 1'), c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all","pct_nhisp_asian", "med_inc_house"), mapping_level = "eps", display_names = display_names)
    
# new jersey state
create_eps_maps(nj_eps_codes, c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all", "mean_inc_house"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)


# greater dallas
create_eps_maps(c('TX19','TX20','TX21','TX22','TX23','TX24'), c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all", "mean_inc_house"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)


# greater houston
create_eps_maps(c('TX15','TX16','TX17','TX18'), c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all", "mean_inc_house"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)


# greater detroit
create_eps_maps(c('MI 1','MI 2','MI 3'), c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all", "mean_inc_house"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)


# Create Tract-level map with EPS borders
create_eps_maps(chi_eps_codes, c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all", "mean_inc_house"), mapping_level = "tract", display_names = display_names, eps_border_weight = 4)


create_eps_maps(philly_eps_codes, c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all", "mean_inc_house"), mapping_level = "tract", display_names = display_names, eps_border_weight = 4)

create_eps_maps(philly_eps_codes, c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all", "mean_inc_house"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)

# Create Tract-level map with EPS borders
create_eps_maps(socal_eps_codes, c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all","pct_nhisp_asian", "med_inc_house"), mapping_level = "tract", display_names = display_names, eps_border_weight = 4)

create_eps_maps(socal_eps_codes, c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all","pct_nhisp_asian", "med_inc_house"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)

create_eps_maps(cleveland_eps_codes, c("pct_hisp_all", "med_inc_house", "pct_nhisp_black"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)

create_eps_maps(c('OK 1','OK 2'), c("pct_hisp_all", "med_inc_house", "pct_nhisp_native"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)



# Create Tract-level map with EPS borders
create_eps_maps(cleveland_eps_codes, c("pct_hisp_all", "med_inc_house", "pct_nhisp_black"), mapping_level = "tract", display_names = display_names, eps_border_weight = 4)

allyr_anal_tract_sf %>% as.data.frame() %>% count(year)




