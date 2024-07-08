################################################################################
## [ PROJ ] < College Board Geomarket >
## [ FILE ] < create_2000_decennial.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 6/27/2024
## [ DESC ] < Get 2000 decennial census data, shape files, and (hopefully) merge w/ eps geomarkets >
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

##### GETTING STARTED FROM CHATGPT

  # This code will retrieve census tract level data from the 2000 Decennial Census, including geometries for each tract, which you can use for further analysis or mapping. The comments provide explanations and additional notes for clarity.

# Get 2000 Decennial Census data for a specific variable and geography (census tract level)
census_2000_tract_data <- get_decennial(
  geography = "tract",      # Set the geography parameter to "tract" to get data at the census tract level
  variables = "P001001",    # Specify the variable code(s) you are interested in, such as "P001001" for total population
  year = 2000,              # Set the year parameter to 2000 to get data from the 2000 Decennial Census
  #state = "06",             # FIPS code for California; adjust as needed
  #county = "*",             # Use "*" to get data for all counties within the state
  geometry = TRUE           # Include geometries for spatial analysis
)

# View the data
head(census_2000_tract_data)

# Explanation:
# 1. Census API Key: Ensure you have set your Census API key using census_api_key("your_api_key_here").
# 2. Geography: Set the geography parameter to "tract" to get data at the census tract level.
# 3. Variables: Specify the variable code(s) you are interested in, such as "P001001" for total population.
# 4. Year: Set the year parameter to 2000 to get data from the 2000 Decennial Census.
# 5. State and County: Use the state and county parameters to specify the geographic area.
#    The state parameter requires the state's FIPS code, and the county parameter can be set to "*" to include all counties within the state.
# 6. Geometry: Set geometry = TRUE to include geometries in the returned data, which is useful for spatial analysis and mapping.

# Additional Notes:
# - Variable Codes: You can find the appropriate variable codes using the Census Data API variable lookup tool or by checking the tidycensus documentation.
# - FIPS Codes: FIPS codes are used to uniquely identify geographic areas. You can find a list of state and county FIPS codes on the Census Bureau's website.

########## iterating over all states

  # ?WHY? CUZ CHATGPT SAYS TAHT TIDYCENSUS WON'T ALLOW YOU TO GET ALL STATES AT ONE

Here's the revised R code with comments explaining each step:

```r
library(tidycensus)
library(purrr)
library(dplyr)

# Set your Census API key
census_api_key("your_api_key_here")

# List of FIPS codes for all states (excluding territories for simplicity)
state_fips <- c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13", 
                "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", 
                "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", 
                "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48", 
                "49", "50", "51", "53", "54", "55", "56")

# Function to get census data for a single state
get_state_data <- function(state) {
  get_decennial(
    geography = "tract",      # Set the geography parameter to "tract" to get data at the census tract level
    variables = "P001001",    # Specify the variable code(s) you are interested in, such as "P001001" for total population
    year = 2000,              # Set the year parameter to 2000 to get data from the 2000 Decennial Census
    state = state,            # Iterate over each state's FIPS code to get data for each state
geometry = TRUE           # Include geometries for spatial analysis
)
}

# Use purrr to map over all states and combine the results into a single data frame
census_2000_tract_data <- map_dfr(state_fips, get_state_data)

# View the data
head(census_2000_tract_data)

# Explanation:
# 1. Census API Key: Ensure you have set your Census API key using census_api_key("your_api_key_here").
# 2. State FIPS Codes: List all the FIPS codes for the states.
# 3. Function get_state_data: This function fetches the data for a single state.
#    - geography = "tract": Set to get data at the census tract level.
#    - variables = "P001001": Specify the variable code for total population.
#    - year = 2000: Set to get data from the 2000 Decennial Census.
#    - state: Use the state parameter to specify the state's FIPS code.
#    - geometry = TRUE: Include geometries in the returned data for spatial analysis and mapping.
# 4. Using purrr to Map Over States: The map_dfr function from the purrr package iterates over each state FIPS code,
#    fetching the data and combining it into a single data frame.

# Additional Notes:
# - Variable Codes: You can find the appropriate variable codes using the Census Data API variable lookup tool
#   or by checking the tidycensus documentation.
# - FIPS Codes: FIPS codes are used to uniquely identify geographic areas. You can find a list of state and county
#   FIPS codes on the Census Bureau's website.
```

This code will retrieve tract-level data for all states from the 2000 Decennial Census, including geometries for each tract, which you can use for further analysis or mapping. The comments provide explanations and additional notes for clarity.