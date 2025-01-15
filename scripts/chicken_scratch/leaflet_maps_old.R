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

philly_eps_codes <- paste0("PA ", 1:5) #
philly_eps_codes

nj_eps_codes <- c(paste0("NJ ", 1:9), paste0("NJ", 10:12))
nj_eps_codes

nj_metro_eps_codes <- c('NJ 2','NJ 4','NJ 5',paste0('NJ ', 7:9),paste0("NJ", 10:11))
nj_metro_eps_codes

############## CREATING GEOMARKET LEVEL TABLES
#######################
####################### 
#######################

  allyr_anal_tract_sf %>% glimpse()


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

# Define table_vars in the desired order
table_varlist <- c("pct_nhisp_white", "pct_nhisp_black", "pct_hisp_all", "pct_nhisp_api", "pct_nhisp_native", "pct_nhisp_multi", "med_inc_house", "mean_inc_house", "pct_pov_yes", "pct_edu_baplus_all")


create_eps_table <- function(eps_codes, table_vars) {
  
  # Human-readable names for the variables (you can modify this list based on the variables passed in)
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
    "pct_edu_baplus_all" = "% with BA+",
    "pct_nhisp_multi" = "% two+ races, non-Hispanic"
  )
  
  # Main table creation pipeline
  eps_table <- allyr_anal_tract_sf %>% 
    as.data.frame() %>%
    # Filter based on provided eps_codes
    filter(eps %in% eps_codes) %>%
    
    # Create the eps_codename by concatenating eps_code and eps_name with a comma
    mutate(eps_codename = str_c(eps, eps_name, sep = ", ")) %>%
    
    # Divide income variables by 1,000
    mutate(across(contains("_inc"), ~ (.x / 1000))) %>%
    
    # Group by the new eps_codename and year
    group_by(eps_codename, year) %>%
    
    # Summarize the data (mean, standard deviation, percentiles)
    summarize(
      across(all_of(table_vars), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
      across(all_of(table_vars), ~ sd(.x, na.rm = TRUE), .names = "sd_{.col}"),
      across(all_of(table_vars), ~ quantile(.x, probs = 0.25, na.rm = TRUE), .names = "p25_{.col}"),
      across(all_of(table_vars), ~ quantile(.x, probs = 0.50, na.rm = TRUE), .names = "p50_{.col}"),
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
    
    # Set the order of the 'variable' column based on the provided table_vars
    mutate(variable = factor(variable, levels = recode(table_vars, !!!table_var_names))) %>%

    # Reshape statistics and years from rows to columns
    pivot_wider(
      names_from = c(year, statistic),  # Pivot both year and statistic to columns
      values_from = value,
      names_glue = "{statistic}_{year}"  # Customize the new column names to include only statistic and year
    ) %>%
    
    # Ungroup and arrange by variable and eps_codename
    ungroup() %>%
    arrange(variable, eps_codename) %>%
    
    # Apply integer formatting for income variables and 1 decimal point for percentage variables
    mutate(
      across(starts_with("mean_") | starts_with("sd_") | starts_with("p25_") | starts_with("p50_") | starts_with("p75_"),
             ~ if_else(str_detect(variable, "income"), round(.x), .x)),
      across(starts_with("mean_") | starts_with("sd_") | starts_with("p25_") | starts_with("p50_") | starts_with("p75_"),
             ~ if_else(str_detect(variable, "^%"), round(.x, 1), .x))
    )
  
  # Return the resulting table
  return(eps_table)
}

# Example usage:
chi_eps_table <- create_eps_table(chi_eps_codes, table_varlist)
chi_eps_table %>% filter(year == 2020) %>%  print(n=100)

# Example usage:
chi_eps_table <- create_eps_table(chi_eps_codes, table_varlist)
chi_eps_table

# Example usage:
  chi_eps_table <- create_eps_table(eps_codes = chi_eps_codes, table_vars = table_varlist)
    chi_eps_table %>% print(n=100)
    
    saveRDS(chi_eps_table, file.path(tables_dir, "chi_eps_table.rds"))

        
  bay_eps_table <- create_eps_table(eps_codes = bay_eps_codes, table_vars = table_varlist)
    bay_eps_table %>% print(n=100)
    
  socal_eps_table <- create_eps_table(eps_codes = socal_eps_codes, table_vars = table_varlist)    
    socal_eps_table %>% print(n=100)
  
  philly_eps_table <- create_eps_table(eps_codes = philly_eps_codes, table_vars = table_varlist)    
    philly_eps_table %>% print(n=100)  
    
    saveRDS(socal_eps_table, file.path(tables_dir, "socal_eps_table.rds"))
    saveRDS(philly_eps_table, file.path(tables_dir, "philly_eps_table.rds"))
    
########
######## EXPERIMENT W/ CREATING GRAPHS FROM TABLE 
########
    
chi_eps_table <- create_eps_table(chi_eps_codes, table_varlist)
chi_eps_table %>% glimpse()

# uniquely identify rows
chi_eps_table %>% # start with data frame object
  group_by(eps_codename, variable) %>% # group by unitid
  summarise(n_per_group=n()) %>% # create measure of number of obs per group
  ungroup %>% # ungroup (otherwise frequency table [next step] created) separately for each group (i.e., separate frequency table for each value of unitid)
  count(n_per_group) # frequency of number of observations per group

# does this data frame satisfy the rules for tidy data
chi_eps_table %>% select(eps_codename, variable, mean_1980, mean_2000, mean_2020)  

#Tidy data always follow these 3 interrelated rules:
  #Each variable must have its own column
    # is mean_YYYY a variable? if so, statement is TRUE
    # is mean a variable? if not, variable is spread out over three columns
  #Each observation must have its own row
    # is each observation a geomarket, variable? if so, statement is TRUE
    # is each observation a geomarket, variable, year? if so, current columns must be turned into rows
    # is each observation a geomarket, 
  #Each value must have its own cell
  
# TRY CREATING GRAPHING FUNCTION FROM RAW DATA

allyr_anal_tract_sf %>% glimpse()
allyr_anal_tract_sf %>% select(starts_with('pct')) %>% glimpse()

# Human-readable names for the variables (you can modify this list based on the variables passed in)
graph_var_names <- list(
  "nhisp_white" = "White, non-Hispanic",
  "nhisp_black" = "Black, non-Hispanic",
  "hisp_all" = "Hispanic",
  "nhisp_asian" = "Asian, non-Hispanic",
  "nhisp_nhpi" = "NHPI, non-Hispanic",
  "nhisp_api" = "API, non-Hispanic", 
  "nhisp_native" = "AIAN, non-Hispanic",
  "nhisp_multi" = "two+ races, non-Hispanic",
  "pct_nhisp_white" = "% White, non-Hispanic",
  "pct_nhisp_black" = "% Black, non-Hispanic",
  "pct_hisp_all" = "% Hispanic",
  "pct_nhisp_asian" = "% Asian, non-Hispanic",
  "pct_nhisp_nhpi" = "% NHPI, non-Hispanic",
  "pct_nhisp_api" = "% API, non-Hispanic", 
  "pct_nhisp_native" = "% AIAN, non-Hispanic",
  "pct_nhisp_multi" = "% two+ races, non-Hispanic",
  "mean_inc_house" = "Mean income",
  "med_inc_house" = "Median income",
  "pct_pov_yes" = "% in poverty",
  "pct_edu_baplus_all" = "% with BA+"
)
graph_var_names

# Define graph_vars in the desired order
graph_varlist <- c("nhisp_white", "nhisp_black", "hisp_all", "nhisp_asian", "nhisp_nhpi", "nhisp_api", "nhisp_native", "nhisp_multi", "pct_nhisp_white", "pct_nhisp_black", "pct_hisp_all", "pct_nhisp_asian", "pct_nhisp_nhpi", "pct_nhisp_api", "pct_nhisp_native", "pct_nhisp_multi", "med_inc_house", "mean_inc_house", "pct_pov_yes", "pct_edu_baplus_all")

create_eps_graph <- function(eps_codes, graph_vars) {
  
  # Main table creation pipeline
  eps_table <- allyr_anal_tract_sf %>% 
    as.data.frame() %>%
    # Filter based on provided eps_codes
    filter(eps %in% eps_codes) %>%
    
    # Create the eps_codename by concatenating eps_code and eps_name with a comma
    mutate(eps_codename = str_c(eps, eps_name, sep = ", ")) %>%
    
    # Divide income variables by 1,000
    mutate(across(contains("_inc"), ~ (.x / 1000))) %>%
    
    # Group by the new eps_codename and year
    group_by(eps_codename, year) %>%
    
    # Summarize the data (mean, standard deviation, percentiles)
    summarize(
      across(all_of(graph_vars), ~ sum(.x, na.rm = TRUE), .names = "sum_{.col}"),
      across(all_of(graph_vars), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
      across(all_of(graph_vars), ~ sd(.x, na.rm = TRUE), .names = "sd_{.col}"),
      across(all_of(graph_vars), ~ quantile(.x, probs = 0.25, na.rm = TRUE), .names = "p25_{.col}"),
      across(all_of(graph_vars), ~ quantile(.x, probs = 0.50, na.rm = TRUE), .names = "p50_{.col}"),
      across(all_of(graph_vars), ~ quantile(.x, probs = 0.75, na.rm = TRUE), .names = "p75_{.col}")
    ) %>% ungroup() %>% 
  
    # create a measure that adds number of students across racial groups, only for the sum vars
    mutate(
      sum_has_race = rowSums(select(.,sum_nhisp_white, sum_nhisp_black, sum_hisp_all, sum_nhisp_asian, sum_nhisp_nhpi, sum_nhisp_native, sum_nhisp_multi), na.rm = TRUE)
    ) %>% relocate(eps_codename,year,sum_has_race) %>% 
  
    # Reshape variables from columns to rows
    pivot_longer(
      cols = starts_with("sum_") | starts_with("mean_") | starts_with("sd_") | starts_with("p25_") | starts_with("p50_") | starts_with("p75_"),
      names_to = c("statistic", "variable"),
      names_pattern = "(sum|mean|sd|p25|p50|p75)_(.*)",  # Regex pattern to correctly split the names
      values_to = "value"
    ) %>%
    
    # Replace variable names with human-friendly names
    mutate(variable = recode(variable, !!!graph_var_names)) %>%  
 
    mutate(
      variable = factor(
        variable,
        levels = c(
          "White, non-Hispanic",
          "Asian, non-Hispanic",
          "API, non-Hispanic",
          "Black, non-Hispanic",
          "Hispanic",
          "two+ races, non-Hispanic",
          "NHPI, non-Hispanic",
          "AIAN, non-Hispanic",
          "% White, non-Hispanic",
          "% Asian, non-Hispanic",
          "% API, non-Hispanic",
          "% Black, non-Hispanic",
          "% Hispanic",
          "% two+ races, non-Hispanic",
          "% NHPI, non-Hispanic",
          "% AIAN, non-Hispanic",
          "Median income",
          "Mean income",
          "% in poverty",
          "% with BA+"
        )
      )
    )       
  
  # Return the resulting table
  return(eps_table)
}

chi_eps_graph <- create_eps_graph(eps_codes = chi_eps_codes, graph_vars = graph_varlist)

#chi_eps_graph %>% filter(year == 2020, statistic == 'sum') %>% print(n=200)

chi_eps_graph %>% 
  filter(statistic == 'sum',
         variable %in% c('White, non-Hispanic', 'Black, non-Hispanic', 'Hispanic', 'Asian, non-Hispanic', 'two+ races, non-Hispanic', 'NHPI, non-Hispanic', 'AIAN, non-Hispanic')
  ) %>% 
  # Reverse the factor order of eps_codename
  mutate(eps_codename = fct_rev(factor(eps_codename))) %>%  
  mutate(year = factor(year, levels = c(2020, 2000, 1980))) %>% # turn year into a factor variable so that you can more easily control order of year sub-graphs
  ggplot(aes(x = eps_codename, y = value, fill = variable)) +
  # geom_col(position = position_stack(reverse = TRUE)) +
  geom_col(position = position_fill(reverse = TRUE)) +         # <-- 'fill' creates 100% stacked bars
    coord_flip() +
    labs(
      x = NULL,
      y = "Population",
      fill = "Race/Ethnicity"      # Legend title
    ) +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~ year, ncol = 1, scales = "free_y") +  # Stacks panels for each year
  theme_minimal()

# CREATE BASIC GRAPHS FOR SOME CANDIDATE CASE STUDY METRO AREAS

chi_eps_codes
philly_eps_codes
htown_eps_codes <- str_c("TX",15:18)
dallas_eps_codes <- c('TX19','TX20','TX21','TX22','TX23','TX24')



dallas_eps_codes <- 
  
# race graph
  
create_eps_graph(eps_codes = dallas_eps_codes, graph_vars = graph_varlist) %>% # 
  filter(statistic == 'sum',
         variable %in% c('White, non-Hispanic', 'Black, non-Hispanic', 'Hispanic', 'Asian, non-Hispanic', 'two+ races, non-Hispanic', 'NHPI, non-Hispanic', 'AIAN, non-Hispanic')
  ) %>% 
  # Reverse the factor order of eps_codename
  mutate(eps_codename = fct_rev(factor(eps_codename))) %>%  
  mutate(year = factor(year, levels = c(2020, 2000, 1980))) %>% # turn year into a factor variable so that you can more easily control order of year sub-graphs
  ggplot(aes(x = eps_codename, y = value, fill = variable)) +
  # geom_col(position = position_stack(reverse = TRUE)) +
  geom_col(position = position_fill(reverse = TRUE)) +         # <-- 'fill' creates 100% stacked bars
  coord_flip() +
  labs(
    x = NULL,
    y = "Population",
    fill = "Race/Ethnicity"      # Legend title
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~ year, ncol = 1, scales = "free_y") +  # Stacks panels for each year
  theme_minimal()

# THINGS TO DO ON THURSDAY:
  # START TWEAKING WITH COLOR PALETTES; GET GREYSCALE TO WORK
  # AFTER YOU GET ALL OF THE RACE AND NON-RACE VARIABLES WORKING, SEE IF THERE IS A FUNCTION THAT CAN BE CREATED THAT MAKES ALL OF THEM; 
    # OR AT LEAST ONE THAT DOES MOST OF THE PROCESSING EFFICIENTLY

# create percent in poverty

  create_eps_graph(eps_codes = dallas_eps_codes, graph_vars = graph_varlist) %>% 
    filter(variable== '% in poverty', statistic == 'p50') %>% 
    # Reverse the factor order of eps_codename
    mutate(eps_codename = fct_rev(factor(eps_codename))) %>%  
    mutate(year = factor(year, levels = c(2020, 2000, 1980))) %>% # turn year into a factor variable so that you can more easily control order of year sub-graphs
    ggplot(aes(x = eps_codename, y = value)) +
    geom_col() +
    coord_flip() +
    labs(
      x = NULL,
      y = "Median % in poverty",
    ) +
    facet_wrap(~ year, ncol = 1, scales = "free_y") +  # Stacks panels for each year
    theme_minimal()
  
# create median percent with BA+
  
  create_eps_graph(eps_codes = dallas_eps_codes, graph_vars = graph_varlist) %>% 
    filter(variable== '% with BA+', statistic == 'p50') %>% 
    # Reverse the factor order of eps_codename
    mutate(eps_codename = fct_rev(factor(eps_codename))) %>%  
    mutate(year = factor(year, levels = c(2020, 2000, 1980))) %>% # turn year into a factor variable so that you can more easily control order of year sub-graphs
    ggplot(aes(x = eps_codename, y = value)) +
    geom_col() +
    coord_flip() +
    labs(
      x = NULL,
      y = "Median % with BA+",
    ) +
    facet_wrap(~ year, ncol = 1, scales = "free_y") +  # Stacks panels for each year
    theme_minimal()
  
# median income
  
  create_eps_graph(eps_codes = dallas_eps_codes, graph_vars = graph_varlist) %>% 
    filter(variable== 'Median income', statistic == 'p50') %>% 
    # Reverse the factor order of eps_codename
    mutate(eps_codename = fct_rev(factor(eps_codename))) %>%  
    mutate(year = factor(year, levels = c(2020, 2000, 1980))) %>% # turn year into a factor variable so that you can more easily control order of year sub-graphs
    ggplot(aes(x = eps_codename, y = value)) +
    geom_col() +
    coord_flip() +
    labs(
      x = NULL,
      y = "Median household income",
    ) +
    facet_wrap(~ year, ncol = 1, scales = "free_y") +  # Stacks panels for each year
    theme_minimal()  
  

  
chi_eps_graph %>% select(year,sum_has_race,sum_nhisp_white, sum_nhisp_black, sum_hisp_all, sum_nhisp_asian, sum_nhisp_nhpi, sum_nhisp_native, sum_nhisp_multi) %>% print(n=100)
chi_eps_graph %>% glimpse()

# geomarket and year uniquely identify obs
chi_eps_graph %>% # start with data frame object
  group_by(eps_codename, year) %>% # group by unitid
  summarise(n_per_group=n()) %>% # create measure of number of obs per group
  ungroup %>% # ungroup (otherwise frequency table [next step] created) separately for each group (i.e., separate frequency table for each value of unitid)
  count(n_per_group) # frequency of number of observations per group


# looking at race graph for socal; a little busy; could get rid of orange county/inland empire

create_eps_graph(eps_codes = socal_eps_codes, graph_vars = graph_varlist) %>% # 
  filter(statistic == 'sum',
         variable %in% c('White, non-Hispanic', 'Black, non-Hispanic', 'Hispanic', 'Asian, non-Hispanic', 'two+ races, non-Hispanic', 'NHPI, non-Hispanic', 'AIAN, non-Hispanic')
  ) %>% 
  # Reverse the factor order of eps_codename
  mutate(eps_codename = fct_rev(factor(eps_codename))) %>%  
  mutate(year = factor(year, levels = c(2020, 2000, 1980))) %>% # turn year into a factor variable so that you can more easily control order of year sub-graphs
  ggplot(aes(x = eps_codename, y = value, fill = variable)) +
  # geom_col(position = position_stack(reverse = TRUE)) +
  geom_col(position = position_fill(reverse = TRUE)) +         # <-- 'fill' creates 100% stacked bars
  coord_flip() +
  labs(
    x = NULL,
    y = "Population",
    fill = "Race/Ethnicity"      # Legend title
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  facet_wrap(~ year, ncol = 1, scales = "free_y") +  # Stacks panels for each year
  theme_minimal()

########
######## create function to format the descriptive table for insertion into .Rmd file

format_eps_table <- function(df, years = c(1980, 2000, 2020), metro_area, format = "latex") {
  
  # Load necessary libraries
  library(dplyr)
  library(kableExtra)
  
  # Capture the name of the data frame passed to 'df'
  df_name <- deparse(substitute(df))
  
  # Check that provided years are valid
  valid_years <- c(1980, 2000, 2020)
  if (!all(years %in% valid_years)) {
    stop("Invalid year(s) provided. Only 1980, 2000, and 2020 are allowed.")
  }
  
  # Define column names for the table (excluding "Variable" and "EPS Name")
  col_names <- c(
    "Geomarket name", 
    rep(c("Mean", "SD", "P25", "P50", "P75"), times = length(years))
  )
  
  # Build a vector of selected columns based on the years provided
  selected_columns <- c("variable", "eps_codename")
  for (year in years) {
    selected_columns <- c(
      selected_columns, 
      paste0("mean_", year), 
      paste0("sd_", year), 
      paste0("p25_", year), 
      paste0("p50_", year), 
      paste0("p75_", year)
    )
  }
  
  # Check if all selected columns exist in df
  missing_columns <- setdiff(selected_columns, colnames(df))
  if (length(missing_columns) > 0) {
    stop(paste("The following required columns are missing in the data frame:", 
               paste(missing_columns, collapse = ", ")))
  }
  
  # Select only the relevant columns from the dataframe
  df_selected <- df %>%
    select(all_of(selected_columns)) %>%
    mutate(row_num = row_number())  # Add row number for grouping
  
  # Determine the kable format based on the 'format' argument
  kable_format <- ifelse(format == "latex", "latex", "html")
  
  # Create headers for the years, merging cells appropriately
  header <- c(" " = 1)  # First column is "Geomarket name"
  for (year in years) {
    header <- c(header, setNames(5, as.character(year)))  # Each year spans 5 columns
  }
  
  # Create alignment vector, adding a vertical line after the first column
  align <- c("l|", rep("r", length(col_names) - 1))
  
  # Prepare group indices
  group_indices <- df_selected %>%
    group_by(variable) %>%
    summarize(
      start_row = min(row_num),
      end_row = max(row_num)
    ) %>%
    ungroup()
  
  # Remove 'variable' and 'row_num' columns before creating the table
  df_kable <- df_selected %>%
    select(-variable, -row_num)
  
  # Generate the table using kable and kableExtra
  table <- df_kable %>%
    kbl(
      format = kable_format,
      digits = 1,
      col.names = col_names,
      align = align,
      # Embed the label directly within the caption using {#tab:label} syntax
      caption = paste0(
        "Characteristics of Geomarkets around ", 
        metro_area, 
        " {#tab:", 
        tolower(gsub(" ", "_", df_name)), 
        "_summary}"
      ),
      booktabs = TRUE,
      longtable = TRUE,
      escape = FALSE  # Prevent LaTeX from escaping the caption content
    ) %>%
    kable_styling(
      full_width = FALSE,
      position = "left",
      font_size = 10,
      latex_options = c("hold_position", "repeat_header")
    ) %>%
    add_header_above(header, line = FALSE) %>%
    row_spec(0, bold = TRUE, hline_after = FALSE)
  
  # Apply group_rows to add group labels and horizontal lines
  for (i in seq_len(nrow(group_indices))) {
    table <- table %>%
      group_rows(
        group_label = group_indices$variable[i],
        start_row = group_indices$start_row[i],
        end_row = group_indices$end_row[i],
        latex_gap_space = "0.5em"
      )
  }
  
  return(table)
}



# Create the table
socal_table <- format_eps_table(df = socal_eps_table, years = c(2020), metro_area = "Los Angeles", format = "latex")
# Write to a .tex file
writeLines(socal_table, file.path('.', tables_dir, 'socal_table.tex'))

# Create the table
philly_table <- format_eps_table(df = philly_eps_table, years = c(2000,2020), metro_area = "the Philidelphia metropolitan area", format = "latex")
# Write to a .tex file
writeLines(philly_table, file.path('.', tables_dir, 'philly_table.tex'))

   
    format_eps_table(df = philly_eps_table, years = c(1980,2020), format = "html")    


philly_table <- format_eps_table(df = philly_eps_table, years = c(2020), format = "html")
philly_table



format_eps_table(df = socal_eps_table, years = c(2000,2020), format = "html")
format_eps_table(df = socal_eps_table, years = c(2020), format = "html")



# Example call to the function
socal_table <- format_eps_table(df = socal_eps_table, years = c(2000, 2020), format = "latex")
socal_table




# Example for HTML
format_eps_table(df = bay_eps_table, years = c(1980, 2020), format = "html")

# Example for LaTeX
format_eps_table(bay_eps_table, years = c(1980, 2000), format = "latex")


  
    
    
#### should this code go in RMD file that has results?

# Adjust the column names for display in the table
col_names <- c("Variable", "EPS Name", "Mean", "SD", "P25", "P50", "P75",
               "Mean", "SD", "P25", "P50", "P75", 
               "Mean", "SD", "P25", "P50", "P75")

# Get the indices of rows where a new variable starts
new_variable_rows <- which(!duplicated(bay_eps_table$variable))


bay_eps_table %>%
  kbl(format = "html", digits = 1, col.names = col_names, align = c("l", "l", rep("r", length(col_names) - 2))) %>% 
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "basic")) %>%
  
  # Add header with super-columns for years and a horizontal line after the header
  add_header_above(c(" " = 2, "1980" = 5, "2000" = 5, "2020" = 5), line = TRUE) %>%
  
  row_spec(0, bold = TRUE) %>%  # Bold header row
  
  # Add a horizontal line before each new variable
  row_spec(new_variable_rows, hline_after = TRUE) %>%
  
  # Add a vertical line to the right of the Variable column
  column_spec(2, border_right = TRUE)





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

# figuring out socal
create_eps_maps(socal_eps_codes, c("med_inc_house","pct_nhisp_white","pct_nhisp_black","pct_hisp_all","pct_nhisp_asian"), mapping_level = "eps", display_names = display_names, eps_border_weight = 4)


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




