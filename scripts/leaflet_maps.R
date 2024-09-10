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
library(kableExtra)

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

# can't save shape files in the repo cuz they too big
shape_dir <-   file.path('.','..','cb_geomarket_shape') # main data directory
list.files(path = shape_dir)

# had to move analysis data files into shape folder because too big for git
analysis_data_dir <- file.path('.',shape_dir,'analysis_data') # analysis data directory
list.files(path = analysis_data_dir)

eps_data_dir <- file.path(data_dir,'eps_market') # has eps geomarket data and spaical data files
list.files(path = eps_data_dir)

scripts_dir <- file.path('.','scripts') # 
list.files(path = scripts_dir)

##### LOAD EPS-LEVEL DATA

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
  rename(med_inc_house = med_inc_house_med) %>% 
  # Reproject the data to WGS84 (longitude/latitude)
  st_transform(crs = 4326)

  allyr_anal_eps_sf %>% glimpse()

  
########### LOAD TRACT-LEVEL DATA
  
  # load tract-level sf object
  load(file = file.path(shape_dir,'analysis_data', 'd1980_stf1f3_anal_tract_sf.RData'))
  d1980_stf1f3_anal_tract_sf <- d1980_stf1f3_anal_tract_sf %>% mutate(year = 1980)
  
  load(file = file.path(shape_dir,'analysis_data', 'd2000_sf1a_anal_tract_sf.RData'))
  d2000_sf1a_anal_tract_sf <- d2000_sf1a_anal_tract_sf %>% mutate(year = 2000)
  
  load(file = file.path(shape_dir,'analysis_data', 'acs2020_ab_anal_tract_sf.RData'))
  acs2020_ab_anal_tract_sf <- acs2020_ab_anal_tract_sf %>% mutate(year = 2020)  
  
  st_geometry(acs2020_ab_anal_tract_sf) <- "geometry"  # Manually setting the active geometry column
  
    # problem: some observations for 2020 have geometry == GEOMETRYCOLLECTION, which gives sf/leaflet problems
    # solution: identify rows with GEOMETRYCOLLECTION geometries; extract polygons from those geometries; if no polygon exists, delete the row; 
      # then replace original GEOMETRYCOLLECTION rows with revised polygon/multipolygon rows
  
  # Step 1: Identify rows with GEOMETRYCOLLECTION geometries
  geometrycollection_rows <- acs2020_ab_anal_tract_sf %>% filter(st_geometry_type(geometry) == "GEOMETRYCOLLECTION")
  geometrycollection_rows$year %>% table() # 2,627 INSTANCES OF GEOMETRYCOLLECTION; all instances of GEOMETRYCOLLECTION ARE IN 2020
  
  # Step 2: Extract POLYGON or MULTIPOLYGON geometries, filtering out rows without POLYGONs
  geometrycollection_fixed <- geometrycollection_rows %>% 
    rowwise() %>% 
    filter(length(st_collection_extract(geometry, "POLYGON")) > 0) %>%  # Keep only those with POLYGONs
    mutate(geometry = st_collection_extract(geometry, "POLYGON")) %>%
    ungroup()
  
  # Step 3: Replace the original GEOMETRYCOLLECTION rows with the extracted geometries
  # Filter out the original GEOMETRYCOLLECTION rows and bind the fixed geometries
  acs2020_ab_anal_tract_sf_fixed <- acs2020_ab_anal_tract_sf %>%
    filter(st_geometry_type(geometry) != "GEOMETRYCOLLECTION") %>%
    bind_rows(geometrycollection_fixed)
  
  rm(acs2020_ab_anal_tract_sf,geometrycollection_fixed,geometrycollection_rows)
  
  acs2020_ab_anal_tract_sf_fixed <- st_make_valid(acs2020_ab_anal_tract_sf_fixed) # is the issue that geometries are not valid or that they are GEOMETRYCOLLECTION
  
  # Create a new column with the geometry type for each row [starts with 97,472 obs then goes to 97,403 obs]
  # filter out obs except for POLYGON AND MULTIPOLYGON
  acs2020_ab_anal_tract_sf_fixed <- acs2020_ab_anal_tract_sf_fixed %>%
    mutate(geometry_type = st_geometry_type(geometry)) %>% 
    filter(geometry_type %in% c('POLYGON','MULTIPOLYGON')) %>% select(-geometry_type)

    #acs2020_ab_anal_tract_sf_fixed %>% as.data.frame() %>%  count(geometry_type)

  ## append datasets
  allyr_anal_tract_sf <- bind_rows(d1980_stf1f3_anal_tract_sf, d2000_sf1a_anal_tract_sf, acs2020_ab_anal_tract_sf_fixed) %>%
    # Merge EPS name to eps code and transform CRS
    left_join(
      y = eps_names %>% select(-eps_code),
      by = c('eps')
    )  
  
  st_geometry(allyr_anal_tract_sf) <- "geometry"  # Manually setting the active geometry column

  # transform into sf object, using WGS84 CRS which leaflet mapping says is required
  allyr_anal_tract_sf <- st_transform(allyr_anal_tract_sf, crs = 4326)
  
  #allyr_anal_tract_sf <- st_make_valid(allyr_anal_tract_sf) # is the issue that geometries are not valid or that they are GEOMETRYCOLLECTION
  

######## CREATE VECTORS OF EPS CODES FOR PARTICULAR METRO AREAS


socal_eps_codes <- paste0("CA", 14:28) # socal 
bay_eps_codes <- c(paste0("CA ", 4:9), paste0("CA", 10:11)) # bay area
bay_eps_codes 
ca_eps_codes <- c(paste0("CA ", 1:9), paste0("CA", 10:34))
chi_eps_codes <- c(paste0("IL ", 7:9), paste0("IL", 10:13))
chi_eps_codes

cleveland_eps_codes <- paste0("OH ", 2:6) #
cleveland_eps_codes


############## CREATING GEOMARKET LEVEL TABLES
#######################
####################### 
#######################

  allyr_anal_tract_sf %>% glimpse()

# Define table_vars in the desired order
table_vars <- c("pct_nhisp_white", "pct_nhisp_black", "pct_hisp_all", "pct_nhisp_api", "pct_nhisp_native", "med_inc_house", "mean_inc_house", "pct_pov_yes", "pct_edu_baplus_all")

table_var_names <- list(
  "pct_hisp_all" = "% Hispanic",
  "mean_inc_house" = "Mean income",
  "med_inc_house" = "Median income",
  "pct_nhisp_asian" = "% Asian, non-Hispanic",
  "pct_nhisp_white" = "% White, non-Hispanic",
  "pct_nhisp_black" = "% Black, non-Hispanic",
  "pct_nhisp_api" = "% API, non-Hispanic", 
  "pct_nhisp_native" = "% Native, non-Hispanic",
  "pct_pov_yes" = "% in poverty",
  "pct_edu_baplus_all" = "% with BA+"
)

eps_table <- allyr_anal_tract_sf %>% 
  as.data.frame() %>% 
  filter(eps %in% chi_eps_codes) %>%
  
  # Divide income variables by 1,000 and round to nearest integer [don't round here]
  mutate(across(contains("_inc"), ~ (.x / 1000))) %>%
  
  group_by(eps_name, year) %>%
  summarize(
    # Mean calculation
    across(all_of(table_vars), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
    
    # Standard deviation calculation
    across(all_of(table_vars), ~ sd(.x, na.rm = TRUE), .names = "sd_{.col}"),
    
    # 25th percentile calculation
    across(all_of(table_vars), ~ quantile(.x, probs = 0.25, na.rm = TRUE), .names = "p25_{.col}"),
    
    # 50th percentile (median) calculation
    across(all_of(table_vars), ~ quantile(.x, probs = 0.50, na.rm = TRUE), .names = "p50_{.col}"),
    
    # 75th percentile calculation
    across(all_of(table_vars), ~ quantile(.x, probs = 0.75, na.rm = TRUE), .names = "p75_{.col}")
  ) %>%
  
  # Reshape variables from columns to rows
  pivot_longer(
    cols = starts_with("mean_") | starts_with("sd_") | starts_with("p25_") | starts_with("p50_") | starts_with("p75_"),
    names_to = c("statistic", "variable"),
    names_pattern = "(mean|sd|p25|p50|p75)_(.*)",  # Regex pattern to correctly split the names
    values_to = "value"
  ) %>%
  
  # Replace variable names with human-friendly names
  mutate(variable = recode(variable, !!!table_var_names)) %>%
  
  # Set the order of the 'variable' column to match table_vars
  mutate(variable = factor(variable, levels = recode(table_vars, !!!table_var_names))) %>%
  
  # Reshape statistics and years from rows to columns
  pivot_wider(
    names_from = c(year, statistic),  # Pivot both year and statistic to columns
    values_from = value,
    names_glue = "{statistic}_{year}"  # Customize the new column names to include only statistic and year
  ) %>%
  ungroup() %>%
  arrange(variable, eps_name) %>%
  select(variable, eps_name, everything()) %>% 
  mutate(
    # Apply integer formatting for income variables
    across(starts_with("mean_") | starts_with("sd_") | starts_with("p25_") | starts_with("p50_") | starts_with("p75_"),
           ~ if_else(str_detect(variable, "income"), round(.x), .x)),
    
    # Apply one decimal point for percentage variables
    across(starts_with("mean_") | starts_with("sd_") | starts_with("p25_") | starts_with("p50_") | starts_with("p75_"),
           ~ if_else(str_detect(variable, "^%"), round(.x, 1), .x))
  )

eps_table %>% print(n=100)

# Then print the table to check the results
#eps_table %>% select(-c(contains('_1980')),-c(contains('_2000'))) %>% print(n = 100)


# Adjust the column names for display in the table
col_names <- c("Variable", "EPS Name", "Mean", "SD", "P25", "P50", "P75",
               "Mean", "SD", "P25", "P50", "P75", 
               "Mean", "SD", "P25", "P50", "P75")

# Get the indices of rows where a new variable starts
new_variable_rows <- which(!duplicated(eps_table$variable))

eps_table %>%
  kbl(format = "html", digits = 1, col.names = col_names, align = c("l", "l", rep("r", length(col_names) - 2))) %>% 
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "basic")) %>%
  
  # Add header with super-columns for years and a horizontal line after the header
  add_header_above(c(" " = 2, "1980" = 5, "2000" = 5, "2020" = 5), line = TRUE) %>%
  
  row_spec(0, bold = TRUE) %>%  # Bold header row
  
  # Add a horizontal line before each new variable
  row_spec(new_variable_rows, hline_after = TRUE) %>%
  
  # Add a vertical line to the right of the Variable column
  column_spec(2, border_right = TRUE)


THINGS TO ADD 9/11
Create function for printing these tables
Function takes arguments for:
  Which eps codes to use
Which variables to create statistics for
Which years to include in the table


# Print the table
  #eps_table %>% print(n = 100)
  eps_table %>% select(-c(contains('_1980')),-c(contains('_2000'))) %>% print(n = 100)

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

# Create Tract-level map with EPS borders
create_eps_maps(socal_eps_codes, c("pct_hisp_all", "med_inc_house", "pct_nhisp_black"), mapping_level = "tract", display_names = display_names, eps_border_weight = 4)

create_eps_maps(socal_eps_codes, c("pct_hisp_all", "med_inc_house", "pct_nhisp_black"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)

create_eps_maps(cleveland_eps_codes, c("pct_hisp_all", "med_inc_house", "pct_nhisp_black"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)

create_eps_maps(c('OK 1','OK 2'), c("pct_hisp_all", "med_inc_house", "pct_nhisp_native"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)


# Create Tract-level map with EPS borders
create_eps_maps(chi_eps_codes, c("pct_hisp_all", "med_inc_house", "pct_nhisp_black"), mapping_level = "tract", display_names = display_names, eps_border_weight = 4)

# Create EPS-level map
create_eps_maps(chi_eps_codes, c("pct_hisp_all", "mean_inc_house", "pct_nhisp_black"), mapping_level = "eps", display_names = display_names)

# Create Tract-level map with EPS borders
create_eps_maps(cleveland_eps_codes, c("pct_hisp_all", "med_inc_house", "pct_nhisp_black"), mapping_level = "tract", display_names = display_names, eps_border_weight = 4)

allyr_anal_tract_sf %>% as.data.frame() %>% count(year)




#######################
####################### MODULAR APPROACH; THIS IS FOR BEFORE YOU TRIED ADDING TRACT-LEVEL DATA
#######################


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



# 
create_eps_maps(chi_eps_codes, c("pct_hisp_all", "med_inc_house_med", "pct_nhisp_asian"), display_names)

create_eps_maps(chi_eps_codes, c("pct_hisp_all", "mean_inc_house", "pct_nhisp_asian"), display_names)

# Test with full set of variables
create_eps_maps(chi_eps_codes, c("pct_nhisp_white","pct_nhisp_black","pct_hisp_all","pct_nhisp_asian"), display_names)

# Example usage:
create_eps_maps(chi_eps_codes, c("pct_hisp_all", "mean_inc_house", "pct_nhisp_asian"))
create_eps_maps(socal_eps_codes, c("pct_hisp_all", "mean_inc_house", "pct_nhisp_asian"))
create_eps_maps(bay_eps_codes, c("pct_hisp_all", "mean_inc_house", "pct_nhisp_asian"))


#################
################# CREATE TRACT-LEVEL INTERACTIVE MAPS
#################

  
# Main function with display names
create_tract_maps <- function(eps_codes) {
  
  map <- leaflet() %>%
    addProviderTiles(provider = providers$CartoDB.Positron)
  
  filtered_data_eps <- allyr_anal_eps_sf %>% filter(year == 1980, eps %in% eps_codes)

  filtered_data_tract <- d1980_stf1f3_anal_tract_sf %>% filter(eps %in% eps_codes)
  
  # Add the layer with the correct group
  map <- map %>%
    addPolygons(
      data = filtered_data_tract,
      #fillColor = ~palettes[[var_name]](filtered_data[[var_name]]),
      weight = 1,
      opacity = 1,
      color = '#808080',
      dashArray = '3',
      fillOpacity = 0.7,
    )
  
  map <- map %>%
    addPolygons(
      data = filtered_data_eps,
      #fillColor = ~palettes[[var_name]](filtered_data[[var_name]]),
      weight = 3,
      opacity = 1,
      color = 'purple',
      #dashArray = '3',
      #fillOpacity = 0.7,
    )
  
  return(map)
  
} 

create_tract_maps(bay_eps_codes)


###################
################### these functions are not utilized by the modular approach; keeping them around in case
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
