################################################################################
## [ PROJ ] < College Board Geomarket >
## [ FILE ] < create_stu_list.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 8/22/2024
## [ DESC ] < Create analysis datasets to simulate who is included/excluded when student list purchases filter on geomarket >
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

### DIRECTORY PATHS

getwd()

public_requests_eda_dir <- file.path('.','..','public_requests_eda')
list.files(path = public_requests_eda_dir)

# set directory to public_requests_eda REPO
setwd(file.path('.',public_requests_eda_dir))

getwd()

# directory paths
data_dir <- file.path('.', 'data')
list.files(path = data_dir)

scripts_dir <- file.path('.', 'scripts')
list.files(path = scripts_dir)

# source files for data    
# Run script that creates data frames from secondary data sources (e.g., ACS, NCES)
source(file = file.path(scripts_dir, 'create_secondary_datasets.R'))

# Run script that creates analysis data frames from order data and list data
# NOTE: this script relies on data frames created by above create_secondary_datasets.R script
source(file = file.path(scripts_dir, 'create_combined_order_list_analysis_datasets.R'))

# Workaround to Crystal errors with Ozan's source script
#save(lists_orders_zip_hs_df, file = file.path(data_dir, 'tbl_fig_listdata.RData'))    

################### CREATE SECONDARY DATA SETS FOR METROS/SCHOOLS

acs_income_zip <- read_csv(file.path(data_dir, 'acs_income_zip.csv'), col_types = c('state_fips_code' = 'c', 'zip_code' = 'c'), na = c('-666666666'))
acs_income_zip$medincome_2564 <- rowMeans(acs_income_zip[, c('medincome_2544', 'medincome_4564')], na.rm = T)
acs_income_zip$medincome_2564[is.nan(acs_income_zip$medincome_2564)] <- NA

acs_income_metro <- read_csv(file.path(data_dir, 'acs_income_metro.csv'), col_types = c('cbsa_code' = 'c'), na = c('-666666666'))
acs_income_metro$medincome_2564 <- rowMeans(acs_income_metro[, c('medincome_2544', 'medincome_4564')], na.rm = T)
acs_income_metro$medincome_2564[is.nan(acs_income_metro$medincome_2564)] <- NA

zip_locale <- read_sas(file.path(data_dir, 'EDGE_ZCTALOCALE_2021_LOCALE.sas7bdat'))

ccd <- readRDS(file.path(data_dir, 'ccd_membership_1718.RDS')) %>% 
  select(-total_students) %>% 
  left_join(readRDS(file.path(data_dir, 'ccd_1718.RDS')) %>% select(ncessch, matches('g\\d{2}_|total_')), by = 'ncessch') %>% 
  left_join(zip_cbsa_name_data, by = c('lzip' = 'zip_code'))

pss <- readRDS(file.path(data_dir, 'pss_1718.RDS')) %>% 
  left_join(zip_cbsa_name_data, by = 'zip_code')

acs_zip <- acs_income_zip %>% 
  left_join(acs_race_zipcodev3 %>% select(zip_code, pop_total, contains('15_19'), state_code, cbsa_1, cbsa_1_ratio, cbsatitle_1, csacode, csatitle), by = 'zip_code')

###### LOAD EPS SHAPE FILE DATA

setwd(file.path('.','..','cb_geo'))
getwd()

data_dir <- file.path('.','data') # main data directory
list.files(path = data_dir)

eps_data_dir <- file.path(data_dir,'eps_market') # has eps geomarket data and spaical data files
list.files(path = eps_data_dir)

load(file.path(eps_data_dir, 'eps_shapes.RData'))

eps_geometry_zcta %>% class()
eps_geometry_zcta %>% glimpse()
################### FINAL SAMPLE FOR EMPIRICAL REPORT

#remove extra dataframes
rm(lists_orders_zip_df)

#remove MN universities from ordersdf
orders_df %>% count(univ_id, univ_name)
orders_df <- orders_df %>% filter(univ_id!="174358" &  univ_id!="174075") 

#remove MN universities from lists dfs
lists_df %>% count(univ_id, univ_name)
lists_orders_zip_hs_df %>% count(univ_id, univ_name)

lists_df <- lists_df %>% filter(univ_id!="174358" &  univ_id!="174075") 
lists_orders_zip_hs_df <- lists_orders_zip_hs_df %>% filter(univ_id!="174358" &  univ_id!="174075") 


# Orders-- 14 universities (includes NAU)
orders_df %>% 
  summarise(n=n_distinct(univ_id)) 

# Orders-- 835 total orders
orders_df %>% 
  summarise(n=n_distinct(order_num)) 

# Lists-- 13 universities (don't have any lists for NAU)
lists_orders_zip_hs_df %>% 
  summarise(n=n_distinct(univ_id)) 

# Lists-- 596 total lists
lists_orders_zip_hs_df %>% 
  summarise(n=n_distinct(ord_num)) 

lists_df %>% 
  summarise(n=n_distinct(univ_id)) # from 13 universities

# number of orders & lists for each university
orders_df %>% count(univ_name, univ_id)
lists_df %>% count(univ_name)

# create regional versus research university according to our sample
orders_df <- orders_df %>% mutate(
  univ_type = ifelse(univ_id=="145637" | univ_id=="145600" | univ_id=="104151" |
                       univ_id=="110653" | univ_id=="110680" | univ_id=="110644" |
                       univ_id=="228723" , "research", "regional"))

orders_df %>% count(univ_name, univ_type)

lists_df <- lists_df %>% mutate(
  univ_type = ifelse(univ_id=="145637" | univ_id=="145600" | univ_id=="104151" |
                       univ_id=="110653" | univ_id=="110680" | univ_id=="110644" |
                       univ_id=="228723" , "research", "regional"))

lists_df %>% count(univ_name, univ_type)

lists_orders_zip_hs_df <- lists_orders_zip_hs_df %>% mutate(
  univ_type = ifelse(univ_id=="145637" | univ_id=="145600" | univ_id=="104151" |
                       univ_id=="110653" | univ_id=="110680" | univ_id=="110644" |
                       univ_id=="228723" , "research", "regional"))

lists_orders_zip_hs_df %>% count(univ_name, univ_type)


################### CREATE FILTER DUMMIES   


# Frequency of Filters Used Across Orders
orders_df <- orders_df %>% 
  mutate(
    hsgrad_class = ifelse(!is.na(hs_grad_class), 1, 0),
    zip = ifelse(!is.na(zip_code) | !is.na(zip_code_file), 1, 0), #KSshould this include zip_code_file not missing too?
    states_fil = ifelse(!is.na(state_name), 1, 0), 
    cbsa = ifelse(!is.na(cbsa_name), 1, 0), 
    intl = ifelse(!is.na(intl_region), 1, 0), 
    segment = ifelse(!is.na(segment), 1, 0), 
    race = ifelse(!is.na(race_ethnicity), 1, 0), 
    gender = ifelse(!is.na(gender), 1, 0), 
    sat = ifelse((!is.na(sat_score_min) | !is.na(sat_score_max) | !is.na(sat_score_old_min) | !is.na(sat_score_old_max)), 1, 0), 
    psat = ifelse((!is.na(psat_score_min) | !is.na(psat_score_max) | !is.na(psat_score_old_min) | !is.na(psat_score_old_max)), 1, 0), 
    gpa = ifelse((!is.na(gpa_low) | !is.na(gpa_high)), 1, 0), 
    rank = ifelse((!is.na(rank_low) | !is.na(rank_high)), 1, 0), 
    geomarket = ifelse(!is.na(geomarket), 1, 0), 
    ap_score = ifelse(!is.na(ap_scores), 1, 0),
    county = ifelse(!is.na(county), 1, 0),
    college_type = ifelse(!is.na(college_type), 1, 0),
    edu_aspirations = ifelse(!is.na(edu_aspirations), 1, 0),
    rotc = ifelse(!is.na(rotc_plans), 1, 0),
    major = ifelse(!is.na(major), 1, 0),
    citizenship = ifelse(!is.na(citizenship), 1, 0),
    low_ses = ifelse(!is.na(low_ses), 1, 0),
    college_size = ifelse(!is.na(college_size), 1, 0),
    national_recognition_programs = ifelse(!is.na(national_recognition_programs), 1, 0),
    college_location = ifelse(!is.na(college_location), 1, 0),
    financial_aid = ifelse(!is.na(financial_aid), 1, 0),
    college_setting = ifelse(!is.na(college_setting), 1, 0),
    college_studentbody = ifelse(!is.na(college_student_body), 1, 0),
    college_living_plans = ifelse(!is.na(college_living_plans), 1, 0),
    proximity_search = ifelse(!is.na(proximity_search), 1, 0),
    hs_math = ifelse(!is.na(hs_math), 1, 0),
    first_gen_parent = ifelse(!is.na(first_gen_parent_edu), 1, 0),
    sat_math = ifelse((!is.na(sat_score_math_min) | !is.na(sat_score_math_max) | !is.na(sat_score_math_old_min) | !is.na(sat_score_math_old_max)), 1, 0), 
    sat_writing = ifelse((!is.na(sat_score_writing_min) | !is.na(sat_score_writing_max) | !is.na(sat_score_writing_old_min) | !is.na(sat_score_writing_old_max)), 1, 0), 
    sat_reading = ifelse((!is.na(sat_score_reading_min) | !is.na(sat_score_reading_max) | !is.na(sat_score_reading_old_min) | !is.na(sat_score_reading_old_max)), 1, 0), 
  )



#race categories variable
# create new categorical race filters var
orders_df <- orders_df %>% mutate(
  race_filter = ifelse(race_ethnicity=="American Indian or Alaska Native", "Native American", NA_character_),
  race_filter = ifelse(race_ethnicity=="Black or African American", "Black", race_filter),
  race_filter = ifelse(race_ethnicity=="Cuban|Hispanic or Latino (including Spanish origin)|Mexican|Puerto Rican|Other Hispanic or Latino", "Latinx", race_filter),
  #race_filter = ifelse(race_ethnicity=="Asian (including Indian subcontinent and Philippines origin)|Other|I do not wish to respond to race|No, not of Hispanic, Latino, or Spanish origin|White (including Middle Eastern origin)", "Asian", race_filter),
  
  race_filter = ifelse(race_ethnicity=="Asian (including Indian subcontinent and Philippines origin)|Other|I do not wish to respond to race|No, not of Hispanic, Latino, or Spanish origin|White (including Middle Eastern origin)", "Asian, White", race_filter),
  race_filter = ifelse(race_ethnicity=="American Indian or Alaska Native|Native Hawaiian or Other Pacific Islander", "Native American, Native Hawaii/PI", race_filter),
  race_filter = ifelse(race_ethnicity=="American Indian or Alaska Native|Cuban|Black or African American|Hispanic or Latino (including Spanish origin)|Mexican|Puerto Rican|Other Hispanic or Latino", "Latinx, Native American", race_filter),
  race_filter = ifelse(race_ethnicity=="Asian (including Indian subcontinent and Philippines origin)|Other|I do not wish to respond to race|No, not of Hispanic, Latino, or Spanish origin|White (including Middle Eastern origin)|Native Hawaiian or Other Pacific Islander", "Asian, White, NativeHawaii/PI", race_filter),
  race_filter = ifelse(race_ethnicity=="Cuban|Black or African American|Hispanic or Latino (including Spanish origin)|Mexican|Puerto Rican|Other Hispanic or Latino", "Latinx, Black", race_filter),
  race_filter = ifelse(race_ethnicity=="Black or African American|American Indian or Alaska Native|Other Hispanic or Latino|Puerto Rican|Mexican|Hispanic or Latino (including Spanish origin)|Cuban", "Latinx, Black, Native American", race_filter), 
  race_filter = ifelse(race_ethnicity=="Asian (including Indian subcontinent and Philippines origin)|White (including Middle Eastern origin)|Other", "Asian, White", race_filter), 
  race_filter = ifelse(race_ethnicity=="Black or African American|American Indian or Alaska Native|Other Hispanic or Latino|Puerto Rican|Mexican|Hispanic or Latino (including Spanish origin)|Native Hawaiian or Other Pacific Islander|Cuban", "Latinx, Black, Native American, NativeHawaii/PI", race_filter),
  race_filter = ifelse(race_ethnicity=="American Indian or Alaska Native|\rAsian (including Indian subcontinent and Philippines origin)|Cuban|\rBlack or African American|\rHispanic or Latino (including Spanish origin)|\rMexican|\rPuerto Rican|\rOther Hispanic or Latino|\rNative Hawaiian or Other Pacific Islander", "Latinx, Black, Asian, Native American", race_filter),
  #adding in new univs
  race_filter = ifelse(race_ethnicity=="Black or African American|American Indian or Alaska Native|Native Hawaiian or Other Pacific Islander", "Black, Native American, NativeHawaii/PI", race_filter),
  race_filter = ifelse(race_ethnicity=="Other Hispanic or Latino|Puerto Rican|Mexican|Hispanic or Latino (including Spanish origin)|Cuban", "Latinx", race_filter),
  race_filter = ifelse(race_ethnicity=="Native Hawaiian or Other Pacific Islander", "NativeHawaii/PI", race_filter),
  race_filter = ifelse(race_ethnicity=="Black or African American|American Indian or Alaska Native|Other Hispanic or Latino|Puerto Rican|Mexican|Hispanic or Latino (including Spanish origin)|Native Hawaiian or Other Pacific Islander|Cuban", "Latinx, Black, Native American, NativeHawaii/PI", race_filter),
  race_filter = ifelse(race_ethnicity=="Asian (including Indian subcontinent and Philippines origin)|Other|I do not wish to respond to race|No, not of Hispanic, Latino, or Spanish origin|White (including Middle Eastern origin)", "Asian, White", race_filter),
  race_filter = ifelse(race_ethnicity=="Black or African American", "Black", race_filter),
  race_filter = ifelse(race_ethnicity=="Other|Asian (including Indian subcontinent and Philippines origin)|I do not wish to respond to race|No, not of Hispanic, Latino, or Spanish origin|White (including Middle Eastern origin)", "Asian, White", race_filter),
  race_filter = ifelse(race_ethnicity=="Include only National Hispanic Recognition Program recipient", "Latinx", race_filter),
  race_filter = ifelse(race_ethnicity=="NA", NA, race_filter),
  race_filter = ifelse(race_ethnicity=="", NA, race_filter),
)

orders_df$race_filter2 <- as.factor(orders_df$race_filter)
orders_df %>% filter(is.na(race_filter)) %>% count(race_filter2)
orders_df %>% count(race_ethnicity)

orders_df$race_filter[orders_df$race_filter==''] <- NA

#df <- orders_df %>%  count(univ_name, race_filter, race_ethnicity) %>% print(n=25)
#race_orders %>%  count(race_filter)

############# BEGIN EDA FOR GEOMARKET PAPER
  
  # WHICH GEOGRAPHIC AREAS TO CONSIDER
    # makes sense to think of Combined Statistical Area (CSA) rather than Metroplitan Statistical Area (MSA)
    # CSA does better job of thinking about which geographic areas are connected by a big city

    # Definition of an MSA:
    # A Metropolitan Statistical Area (MSA) is a geographical region with a relatively high population density
    # at its core and close economic ties throughout the area. MSAs are defined by the Office of Management 
    # and Budget (OMB) and used by federal agencies in collecting, tabulating, and publishing federal statistics.
    # Each MSA consists of one or more counties that have a high degree of social and economic integration 
    # with the urban core, as measured by commuting patterns.
    
    # 1. New York-Newark-Jersey City, NY-NJ-PA - 19,768,458 [YES]
    # 2. Los Angeles-Long Beach-Anaheim, CA - 13,211,027 [YES]
    # 3. Chicago-Naperville-Elgin, IL-IN-WI - 9,478,801 [YES]
    # 4. Dallas-Fort Worth-Arlington, TX - 7,637,387 [YES]
    # 5. Houston-The Woodlands-Sugar Land, TX - 7,122,240 [YES]
    # 6. Washington-Arlington-Alexandria, DC-VA-MD-WV - 6,371,200 [YES]
    # 7. Miami-Fort Lauderdale-Pompano Beach, FL - 6,138,333 [NO]
    # 8. Philadelphia-Camden-Wilmington, PA-NJ-DE-MD - 6,102,434 [YES]
    # 9. Atlanta-Sandy Springs-Alpharetta, GA - 6,089,815 [YES]
    # 10. Phoenix-Mesa-Chandler, AZ - 4,845,832 [NO]
    # 11. Boston-Cambridge-Newton, MA-NH - 4,875,390 [YES]
    # 12. San Francisco-Oakland-Berkeley, CA - 4,749,008 [YES]
    # 13. Riverside-San Bernardino-Ontario, CA - 4,650,631 [AS PART OF LA]
    # 14. Detroit-Warren-Dearborn, MI - 4,392,041 [YES]
    # 15. Seattle-Tacoma-Bellevue, WA - 4,018,598 [NO]
    # 16. Minneapolis-St. Paul-Bloomington, MN-WI - 3,690,512
    # 17. San Diego-Chula Vista-Carlsbad, CA - 3,338,330 [YES]
    # 18. Tampa-St. Petersburg-Clearwater, FL - 3,219,514 [NO]
    # 19. Denver-Aurora-Lakewood, CO - 2,963,821 [NO]
    # 20. St. Louis, MO-IL - 2,820,253 [NO]
    # 21. Baltimore-Columbia-Towson, MD - 2,839,065 [YES]
    # 22. Charlotte-Concord-Gastonia, NC-SC - 2,733,256 
    # 23. Orlando-Kissimmee-Sanford, FL - 2,694,418 [NO]
    # 24. San Antonio-New Braunfels, TX - 2,558,143 [MAYBE]
    # 25. Portland-Vancouver-Hillsboro, OR-WA - 2,511,612 [NO]
    # 26. Sacramento-Roseville-Folsom, CA - 2,363,730 [NO]
    # 27. Pittsburgh, PA - 2,324,743 [MAYBE]
    # 28. Las Vegas-Henderson-Paradise, NV - 2,227,053 [NO]
    # 29. Austin-Round Rock-Georgetown, TX - 2,283,371 [NO]
    # 30. Cincinnati, OH-KY-IN - 2,256,884
    # 31. Kansas City, MO-KS - 2,192,035
    # 32. Columbus, OH - 2,138,926
    # 33. Indianapolis-Carmel-Anderson, IN - 2,111,040
    # 34. Cleveland-Elyria, OH - 2,088,251
    # 35. San Jose-Sunnyvale-Santa Clara, CA - 1,990,660
    # 36. Nashville-Davidson--Murfreesboro--Franklin, TN - 1,989,519
    # 37. Virginia Beach-Norfolk-Newport News, VA-NC - 1,799,674
    # 38. Providence-Warwick, RI-MA - 1,676,579
    # 39. Milwaukee-Waukesha, WI - 1,574,731
    # 40. Jacksonville, FL - 1,605,848
    
    
    # Definition of a CSA:
    # A Combined Statistical Area (CSA) is a U.S. geographic area defined by the Office of Management and Budget (OMB)
    # that consists of two or more adjacent Core-Based Statistical Areas (CBSAs) that have significant social and 
    # economic ties. A CSA typically includes a larger Metropolitan Statistical Area (MSA) and smaller adjacent MSAs or 
    # Micropolitan Statistical Areas (μSAs). These areas are grouped together based on shared economic or commuting 
    # patterns. CSAs provide a more comprehensive representation of an interconnected region, reflecting 
    # regional economic and social networks more accurately than MSAs alone.
    
    # 1. New York-Newark, NY-NJ-CT-PA - 23,582,649 [YES]
    # 2. Los Angeles-Long Beach, CA - 18,711,054 [YES]
    # 3. Chicago-Naperville, IL-IN-WI - 9,882,634 [YES]
    # 4. Washington-Baltimore-Arlington, DC-MD-VA-WV-PA - 9,814,928 [YES]
    # 5. San Jose-San Francisco-Oakland, CA - 9,714,453 [YES]
    # 6. Boston-Worcester-Providence, MA-RI-NH-CT - 8,472,756 [YES]
    # 7. Dallas-Fort Worth, TX-OK - 7,637,387 [YES]
    # 8. Houston-The Woodlands, TX - 7,122,240 [YES]
    # 9. Miami-Fort Lauderdale-Port St. Lucie, FL - 6,912,434 [NO]
    # 10. Atlanta--Athens-Clarke County--Sandy Springs, GA - 6,789,266 [YES]
    # 11. Philadelphia-Reading-Camden, PA-NJ-DE-MD - 6,228,601 [YES]
    # 12. Phoenix-Mesa, AZ - 5,059,909 [NO]
    # 13. Detroit-Warren-Ann Arbor, MI - 5,020,982 [YES]
    # 14. Seattle-Tacoma, WA - 4,879,311 [NO]
    # 15. Minneapolis-St. Paul, MN-WI - 4,084,979 [NO]
    # 16. San Diego-Carlsbad, CA - 3,347,270 [YES]
    # 17. Denver-Aurora, CO - 3,272,293 [NO]
    # 18. St. Louis-St. Charles-Farmington, MO-IL - 2,909,669 [NO]
    # 19. Tampa-St. Petersburg-Clearwater, FL - 3,219,514 [NO]
    # 20. Orlando-Deltona-Daytona Beach, FL - 3,139,583 NO
    # 21. Cleveland-Akron-Canton, OH - 3,586,918 [YES]
    # 22. Charlotte-Concord, NC-SC - 2,846,550 [NO]
    # 23. Sacramento-Roseville, CA - 2,664,758 [NO]
    # 24. Portland-Vancouver-Salem, OR-WA - 3,211,568 [NO]
    # 25. Kansas City-Overland Park-Kansas City, MO-KS - 2,487,053 [NO]
    # 26. Indianapolis-Carmel-Muncie, IN - 2,327,717 [NO]
    # 27. Las Vegas-Henderson, NV-AZ - 2,266,715 [NO]
    # 28. Cincinnati-Wilmington-Maysville, OH-KY-IN - 2,347,102 [NO]
    # 29. Columbus-Marion-Zanesville, OH - 2,204,564 [NO]
    # 30. Milwaukee-Racine-Waukesha, WI - 2,065,890 [NO]

  #top 20 orders by volume [basically all ASU orders]
  lists_orders_zip_hs_df %>% count(ord_num) %>% arrange(desc(n)) %>% print(n=20)
  orders_df %>% arrange(desc(num_students)) %>% select(univ_name,univ_id,order_num,order_title,num_students) %>% print(n=20)
  
  
  # find which 

  lists_orders_zip_hs_df %>% glimpse()
  # need to find big orders that encompass a metro area;
  
  lists_orders_zip_hs_df %>% count(hs_state_code) %>% arrange(desc(n)) %>% print(n=55)
  
  # identify big orders
  lists_orders_zip_hs_df %>% count(ord_num) %>% arrange(desc(n)) %>%  print(n=800) # dont have orders for 163726
  3665455 - 163726 # 
  
  orders_df %>% glimpse()
  
  
  orders_df %>% count(state_name) %>% print(n=100)
  
  # let's use Philly as a test case; identify orders that include people from PA
  lists_orders_zip_hs_df %>% filter(hs_state_code == 'PA') %>% count(univ_name)
  
  # A tibble: 12 × 2
  # univ_name                                       n
  # 1 Arizona State University-Tempe              67970 [id = 104151]
  # 9 University of California-San Diego          15272 [id = 110680]
  # 12 University of Illinois at Urbana-Champaign 11470 [145637]
  # 10 University of Illinois at Chicago          5015  [id = 145600]
  
    
  lists_orders_zip_hs_df %>% filter(hs_state_code == 'PA' & univ_id ==104151) %>% count(ord_num) %>% arrange(desc(n)) %>% print(n=20) # ASU
  lists_orders_zip_hs_df %>% filter(hs_state_code == 'PA' & univ_id ==110680) %>% count(ord_num) %>% arrange(desc(n)) %>% print(n=10) # UC san diego
  lists_orders_zip_hs_df %>% filter(hs_state_code == 'PA' & univ_id ==145600) %>% count(ord_num) %>% arrange(desc(n)) %>% print(n=10) # U Illinois Chicago
  lists_orders_zip_hs_df %>% filter(hs_state_code == 'PA' & univ_id ==145637) %>% count(ord_num) %>% arrange(desc(n)) %>% print(n=10) # U Illinois Urbana-Champaign
  
  orders_df %>% filter(order_num == '395793')
  
  orders_df %>% glimpse()
  
  lists_orders_zip_hs_df %>% filter(ord_num == '322193') %>% View()
  
  orders_df %>% filter(order_num == '367919')
  
  ##########################
  # order that was just native american indian/alaskan native
  lists_orders_zip_hs_df %>% glimpse()
  lists_orders_zip_hs_df %>% filter(ord_num == '487927') %>% count(hs_state_code) %>% arrange(desc(n)) %>% print(n=50)
  # Combined Statistical Areas (CSAs) consist of two or more adjacent CBSAs that have significant employment interchanges. 
  lists_orders_zip_hs_df %>% filter(ord_num == '487927') %>% count(hs_csatitle) %>% arrange(desc(n)) %>% print(n=50)
  lists_orders_zip_hs_df %>% filter(ord_num == '487927') %>% count(hs_cbsatitle_1) %>% arrange(desc(n)) %>% print(n=50)
  ##########################
  
  # ASU order 
  lists_orders_zip_hs_df %>% filter(ord_num == '448922') %>% count(hs_state_code) %>% arrange(desc(n)) %>% print(n=50)
  lists_orders_zip_hs_df %>% filter(ord_num == '448922') %>% count(hs_cbsatitle_1) %>% arrange(desc(n)) %>% print(n=50)
  
  
  # investigating agreeement between number of names on PDF and number of names on df
  lists_orders_zip_hs_df %>% filter(ord_num == '448427') %>% count()
  lists_orders_zip_hs_df %>% filter(ord_num == '448427') %>% count(hs_state_code) %>% arrange(desc(n)) %>% print(n=50)
                                                                  
   lists_orders_zip_hs_df %>% filter(ord_num == '448922') %>% count(hs_cbsatitle_1) %>% arrange(desc(n)) %>% print(n=50)
   
   
############
############ CREATE DATASET lists_orders_zip_hs_df with SF
   
   lists_orders_zip_hs_df %>% glimpse()
   
   # turn lists_orders_zip_hs_df into sf object
   lists_orders_zip_hs_df_sf <- lists_orders_zip_hs_df %>% 
     # missing values in coordinates not allowed
     filter(!is.na(hs_latitude)) %>% 
     st_as_sf(coords = c('hs_longitude','hs_latitude'), crs = 4326)
   
   # convert eps geometry object to the same zcta
   eps_geometry_zcta <- eps_geometry_zcta %>% st_transform(crs = 4326)
   
   # spatial join
   # st_join() returns a new simple features object that inherits geometry and attributes from a first dataset x with attributes from a second dataset y appended.
   lists_orders_zip_hs_df_sf <- lists_orders_zip_hs_df_sf %>% st_join(eps_geometry_zcta)
   
   lists_orders_zip_hs_df_sf %>% glimpse()
   
   
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
    eps_names   
    
    # merge EPS name to eps code and transform CRS
    lists_orders_zip_hs_df_sf <- lists_orders_zip_hs_df_sf %>% inner_join(
      y = eps_names %>% select(-eps_code),
      by = c('eps')
    )

   
   #save objects for use in qmd files that create manuscript text
   #save(lists_orders_zip_hs_df_sf, orders_df, eps_geometry_zcta, file = file.path('.','..','cb_geomarket_shape','list_data','list_sf.RData'))
   #getwd()
   
   
   
############
############   
   
  # Create a df of PSAT orders from ASU for fall 2020 freshmen class
    fa20_oos_psat_sf <- lists_orders_zip_hs_df_sf %>% filter(ord_num %in% c('448922','448427','448440'))
   
  # create subset of observations from NJO4 (middlesex)
   lists_orders_zip_hs_df_sf %>% filter(ord_num == '547038' & eps == 'NJ 4') %>% count() # PSAT 1070-1180
   lists_orders_zip_hs_df_sf %>% filter(ord_num == '547005' & eps == 'NJ 4') %>% count() # PSAT 1190-1260
   lists_orders_zip_hs_df_sf %>% filter(ord_num == '546978' & eps == 'NJ 4') %>% count() # PSAT 1270-1520
   
   lists_orders_zip_hs_df_sf %>% filter((ord_num == '547038' & eps == 'NJ 4')| (ord_num == '547005' & eps == 'NJ 4') | (ord_num == '546978' & eps == 'NJ 4') ) %>% count()
   
  nj04_subset <- lists_orders_zip_hs_df_sf %>% 
    filter(
      ord_num %in% c('547038', '547005', '546978'),
      eps == 'NJ 4'
    ) %>% 
    mutate(
      ord_num = case_match(ord_num, '547038' ~ '448922', '547005' ~ '448427', '546978' ~ '448440'),
      ord_hs_grad_class = '2020',
      stu_grad_year = 2020,
     ) 
  
    nj04_subset %>% count(ord_num)

    # append nj04 orders from fall 2021 class to orders from fall 2020 class
      # will need to make a note of this in the appendix!
    fa20_oos_psat_sf <- fa20_oos_psat_sf %>% bind_rows(nj04_subset)


# START CREATING TABLES OF PERCENT OF PEOPLE IN A RACIAL GROUP THAT ARE IN EACH GEOMARKET
   
   # Define the function
    create_sim_eps_table <- function(data, ord_nums, eps_codes) {
     df <- data %>%
       as.data.frame() %>%
       filter(ord_num %in% ord_nums, eps %in% eps_codes) %>%
       mutate(eps_codename = str_c(eps, eps_name, sep = ", ")) %>% 
       group_by(eps_codename) %>%
       summarize(
         stu_all = n(),
         stu_white = sum(stu_white_01, na.rm = TRUE),
         stu_asian = sum(stu_asian_01, na.rm = TRUE),
         stu_black = sum(stu_black_01, na.rm = TRUE),
         stu_hispanic = sum(stu_hispanic_01, na.rm = TRUE),
         stu_amerindian = sum(stu_amerindian_01, na.rm = TRUE),
         stu_native_hawaiian = sum(stu_nativehawaii_01, na.rm = TRUE),
         stu_tworaces = sum(stu_tworaces_01, na.rm = TRUE),
         stu_unknown = sum(stu_unknown_01, na.rm = TRUE),
         stu_na = sum(is.na(stu_race_cb), na.rm = TRUE)
       ) %>%
       mutate(
         stu_sum_calc = rowSums(select(., stu_white, stu_asian, stu_black, stu_hispanic,
                                       stu_amerindian, stu_native_hawaiian, stu_tworaces,
                                       stu_unknown, stu_na)),
         stu_race_known = rowSums(select(., stu_white, stu_asian, stu_black, stu_hispanic,
                                         stu_amerindian, stu_native_hawaiian, stu_tworaces))
       ) %>%
       mutate(
         # Create row percentage variables
         row_pct_white = stu_white / stu_race_known * 100,
         row_pct_asian = stu_asian / stu_race_known * 100,
         row_pct_black = stu_black / stu_race_known * 100,
         row_pct_hispanic = stu_hispanic / stu_race_known * 100,
         row_pct_amerindian = stu_amerindian / stu_race_known * 100,
         row_pct_native_hawaiian = stu_native_hawaiian / stu_race_known * 100,
         row_pct_tworaces = stu_tworaces / stu_race_known * 100
       ) %>%
       mutate(
         # Create column percentage variables
         col_pct_white = stu_white / sum(stu_white, na.rm = TRUE) * 100,
         col_pct_asian = stu_asian / sum(stu_asian, na.rm = TRUE) * 100,
         col_pct_black = stu_black / sum(stu_black, na.rm = TRUE) * 100,
         col_pct_hispanic = stu_hispanic / sum(stu_hispanic, na.rm = TRUE) * 100,
         col_pct_amerindian = stu_amerindian / sum(stu_amerindian, na.rm = TRUE) * 100,
         col_pct_native_hawaiian = stu_native_hawaiian / sum(stu_native_hawaiian, na.rm = TRUE) * 100,
         col_pct_tworaces = stu_tworaces / sum(stu_tworaces, na.rm = TRUE) * 100
       )  # %>% select(eps,stu_all,stu_race_known,stu_white,stu_asian,stu_black,stu_hispanic, stu_tworaces, col_pct_white,col_pct_asian,col_pct_black,col_pct_hispanic, col_pct_tworaces)

     # First table: current output with counts
     table1 <- df %>% 
       select(
         eps_codename, stu_all, stu_race_known, stu_white, stu_asian, stu_black, stu_hispanic, stu_tworaces
       )
     
     # Second table: current output with column percentages
      table2 <- df %>% 
        select(
          eps_codename, stu_all, stu_race_known,col_pct_white, col_pct_asian, col_pct_black, col_pct_hispanic, col_pct_tworaces
        )
      
      # Third table: specified columns with row percentages
      table3 <- df %>%
        select(
          eps_codename, stu_all, stu_race_known,row_pct_white, row_pct_asian, row_pct_black, row_pct_hispanic, row_pct_tworaces
        )
      
      # Return a list with both tables
      result <- list(
        count_table = table1,
        col_pct_table = table2,
        row_pct_table = table3
      )
      
      return(result)
   }
  
  # order 448922: PSAT 1070 - 1180; order 448427: PSAT 1190 - 1260; order 448440: PSAT 1270 - 1520
    
    # Philly metro area
    create_sim_eps_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5')) 
    create_sim_eps_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448427'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5'))
    create_sim_eps_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448440'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5'))
    
    philly_order448922_psat1070_1180 <- create_sim_eps_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5')) 
    philly_order448922_psat1070_1180
    
    philly_order448427_psat1190_1260 <- create_sim_eps_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448427'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5')) 
    philly_order448427_psat1190_1260
    
    philly_order448440_psat1270_1520 <- create_sim_eps_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448440'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5')) 
    philly_order448440_psat1270_1520  
    
    save(
      philly_order448922_psat1070_1180, philly_order448427_psat1190_1260, philly_order448440_psat1270_1520,
      file = file.path('.','results','tables','philly_asu_eps_lists.RData'))

   # IL: 07 [Y], 08 [Y], 09 [Y], 10 [Y], 11, 12 [Y], 13 [Y]: doesn't have city of Chicago!!!
   c('448922','448427','448440')

  # Greater Detroit
    # MI 1 = wayne county, MI 2 = detroit's northern suburbs; MI 3 = ann arbor
    create_sim_eps_table(data = fa20_oos_psat_sf, ord_nums = c('448922'), eps_codes = c('MI 1','MI 2','MI 3')) # 52% of Black students in wayne county
    create_sim_eps_table(data = fa20_oos_psat_sf, ord_nums = c('448427'), eps_codes = c('MI 1','MI 2','MI 3')) 
    create_sim_eps_table(data = fa20_oos_psat_sf, ord_nums = c('448440'), eps_codes = c('MI 1','MI 2','MI 3'))

  # Greater Houston. TX 15: Northwest Houston and Conroe School District [Y]; TX 16: Southwest Houston Metro Area [Y]; TX17: Cit of Houston (East) [NO]; TX18:Galveston and East Harris Counties [NO]
    # no ASU order does city of Houston; but note we have Texas A&M
    

  # START HERE ON TUESDAY [LET'S DO ENTIRE NEW YORK METRO AREA]; BUT NJ04 IS IN 547038, 484698; ETC.
    # GO THROUGH NEW JERSEY
      # metro jersey is: 2, 4, 5, 7, 8, 9, 10, 11
    
    nj_metro_eps_codes
    create_sim_eps_table(data = fa20_oos_psat_sf, ord_nums = c('448922'), eps_codes = nj_metro_eps_codes)
    # disproportionate share of black students in NJ08 == essex and southern Passaic County; also, hispanic students concentrated in nj7-nj10 (which is all in one area)
    create_sim_eps_table(data = fa20_oos_psat_sf, ord_nums = c('448427'), eps_codes = nj_metro_eps_codes) # black students concentrated in nj08
    create_sim_eps_table(data = fa20_oos_psat_sf, ord_nums = c('448440'), eps_codes = nj_metro_eps_codes) # black students stil concentrated in nj08

    # order = 448922; FA20 - OOS PSAT AD (Jan19); 2020 HS class; PSAT 1070-1180
      # 1. Southern Jersey NJ01 []
      # 2. Camden and Burlington County NJ02 [Y]
      # 3. Jersey Shore and Pinelands NJ03 []
      # 4. Middlesex County NJ04 [NO]
      # 5. Monmouth County NJ05 [Y]
      # 6. Somerset and Mercer Counties NJ06 [Y]
      # 7. Union County NJ07 [Y]
      # 8. Essex and Southern Passaic County NJ08 [Y]
      # 9. Hudson County NJ09 [Y]
      # 10. Bergen County NJ10 [Y]
      # 11.Morris and Northern Passaic County NJ11 [Y]
      # 12. Sussex, Warren, and Hunterdon Counties NJ12 [Y]
    # order = 448427; FA20 - OOS PSAT SE (JAN19); 2020 HS class; PSAT 1190 - 1260
      # 1. Southern Jersey NJ01 []
      # 2. Camden and Burlington County NJ02 [Y]
      # 3. Jersey Shore and Pinelands NJ03 []
      # 4. Middlesex County NJ04 []
      # 5. Monmouth County NJ05 [Y]
      # 6. Somerset and Mercer Counties NJ06 [Y]
      # 7. Union County NJ07 [Y]
      # 8. Essex and Southern Passaic County NJ08 [Y]
      # 9. Hudson County NJ09 [Y]
      # 10. Bergen County NJ10 [Y]
      # 11.Morris and Northern Passaic County NJ11 [Y]
      # 12. Sussex, Warren, and Hunterdon Counties NJ12 [Y]
    # order = 448440; FA20 - OOS PSAT BE (JAN19); 2020 HS class; PSAT 1270 - 1520
      # 1. Southern Jersey NJ01 []
      # 2. Camden and Burlington County NJ02 [Y]
      # 3. Jersey Shore and Pinelands NJ03 []
      # 4. Middlesex County NJ04 []
      # 5. Monmouth County NJ05 [Y]
      # 6. Somerset and Mercer Counties NJ06 [Y]
      # 7. Union County NJ07 [Y]
      # 8. Essex and Southern Passaic County NJ08 [Y]
      # 9. Hudson County NJ09 [Y]
      # 10. Bergen County NJ10 [Y]
      # 11.Morris and Northern Passaic County NJ11 [Y]
      # 12. Sussex, Warren, and Hunterdon Counties NJ12 [Y]
    
    # order is 484698; FA21 - OOS PSAT AD (JUL 19); 2021 HS grad class; PSAT 1070 - 1180; purchased 7/25/2019
      # 1. Southern Jersey NJ01 []
      # 2. Camden and Burlington County NJ02 [Y]
      # 3. Jersey Shore and Pinelands NJ03 []
      # 4. Middlesex County NJ04 []
      # 5. Monmouth County NJ05 [Y]
      # 6. Somerset and Mercer Counties NJ06 [Y]
      # 7. Union County NJ07 [Y]
      # 8. Essex and Southern Passaic County NJ08 [Y]
      # 9. Hudson County NJ09 [Y]
      # 10. Bergen County NJ10 [Y]
      # 11.Morris and Northern Passaic County NJ11 [Y]
      # 12. Sussex, Warren, and Hunterdon Counties NJ12 [Y]
    
    # order is 484697; FA21 - OOS PSAT AD (JUL 19); 2021 HS grad class; PSAT 1270 - 1520; purchased 7/25/2019
      # 1. Southern Jersey NJ01 []
      # 2. Camden and Burlington County NJ02 [Y]
      # 3. Jersey Shore and Pinelands NJ03 []
      # 4. Middlesex County NJ04 []
      # 5. Monmouth County NJ05 [Y]
      # 6. Somerset and Mercer Counties NJ06 [Y]
      # 7. Union County NJ07 [Y]
      # 8. Essex and Southern Passaic County NJ08 [Y]
      # 9. Hudson County NJ09 [Y]
      # 10. Bergen County NJ10 [Y]
      # 11.Morris and Northern Passaic County NJ11 [Y]
      # 12. Sussex, Warren, and Hunterdon Counties NJ12 [Y]
      
    # order is 547038; FA21 – OOS PSAT AD (JAN20); 2021 HS grad class; PSAT 1070 - 1180
      # 1. Southern Jersey NJ01 []
      # 2. Camden and Burlington County NJ02 [Y]
      # 3. Jersey Shore and Pinelands NJ03 [Y]
      # 4. Middlesex County NJ04 [Y]
      # 5. Monmouth County NJ05 [Y]
      # 6. Somerset and Mercer Counties NJ06 [Y]
      # 7. Union County NJ07 []
      # 8. Essex and Southern Passaic County NJ08 [Y]
      # 9. Hudson County NJ09 []
      # 10. Bergen County NJ10 [Y]
      # 11.Morris and Northern Passaic County NJ11 [Y]
      # 12. Sussex, Warren, and Hunterdon Counties NJ12 []
    
    # order is 547038; FA21 – OOS PSAT SE (JAN20); 2021 HS grad class; PSAT 1190 - 1260; purchased 1/6/2020
      # 1. Southern Jersey NJ01 []
      # 2. Camden and Burlington County NJ02 [Y]
      # 3. Jersey Shore and Pinelands NJ03 []
      # 4. Middlesex County NJ04 [Y]
      # 5. Monmouth County NJ05 [Y]
      # 6. Somerset and Mercer Counties NJ06 [Y]
      # 7. Union County NJ07 []
      # 8. Essex and Southern Passaic County NJ08 [Y]
      # 9. Hudson County NJ09 []
      # 10. Bergen County NJ10 [Y]
      # 11.Morris and Northern Passaic County NJ11 [Y]
      # 12. Sussex, Warren, and Hunterdon Counties NJ12 []
    
    # Order 546978; FA21 - OOS PSAT BE (JAN20); 2021 HS grad class; PSAT 1270-1520
        # missing NJ07, NJ09
      # 1. Southern Jersey NJ01 []
      # 2. Camden and Burlington County NJ02 [Y]
      # 3. Jersey Shore and Pinelands NJ03 [Y]
      # 4. Middlesex County NJ04 [Y]
      # 5. Monmouth County NJ05 [Y]
      # 6. Somerset and Mercer Counties NJ06 [Y]
      # 7. Union County NJ07 []
      # 8. Essex and Southern Passaic County NJ08 [Y]
      # 9. Hudson County NJ09 []
      # 10. Bergen County NJ10 [Y]
      # 11.Morris and Northern Passaic County NJ11 [Y]
      # 12. Sussex, Warren, and Hunterdon Counties NJ12 []

    
    
      # 1. Southern Jersey NJ01 []
      # 2. Camden and Burlington County NJ02 []
      # 3. Jersey Shore and Pinelands NJ03 []
      # 4. Middlesex County NJ04 []
      # 5. Monmouth County NJ05 []
      # 6. Somerset and Mercer Counties NJ06 []
      # 7. Union County NJ07 []
      # 8. Essex and Southern Passaic County NJ08 []
      # 9. Hudson County NJ09 []
      # 10. Bergen County NJ10 []
      # 11.Morris and Northern Passaic County NJ11 []
      # 12. Sussex, Warren, and Hunterdon Counties NJ12 []
          
 
        
  # Greater Dalls [black students located in City of Dallas and Dallas count excluding dallas; but black students who get purchased are not (unlike detroit)]
    # TX19: city of dallas [Y]; 
    # TX20: city of forth worth [Y]; 
    # TX21: irvington, arlington, grand prarie [Y]; 
    # TX22: dallas county excluding city of dallas [Y];  
    # TX23: collin and rockwall counties [Y]; 
    # TX24: counties west of dallas/ft worth metropolex [Y]; 
    create_sim_eps_table(data = fa20_oos_psat_sf, ord_nums = c('448922'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24')) # 
    create_sim_eps_table(data = fa20_oos_psat_sf, ord_nums = c('448427'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24')) 
    create_sim_eps_table(data = fa20_oos_psat_sf, ord_nums = c('448440'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24'))
    
 
  # NEW YORK
    # Westchester and Rockland Counties: 13 and 15
    # Long Island: 16 through 21
    # City of New York: 14, 22 through 30
    
    long_island_eps_codes <- c(paste0('NY',16:21))
    
    create_sim_eps_table(data = fa20_oos_psat_sf, ord_nums = c('448922'), eps_codes = long_island_eps_codes) # NY18 - central Nassau is where disproportionate % of Black students are
    create_sim_eps_table(data = fa20_oos_psat_sf, ord_nums = c('448427'), eps_codes = long_island_eps_codes) # same
    create_sim_eps_table(data = fa20_oos_psat_sf, ord_nums = c('448440'), eps_codes = long_island_eps_codes) # 
    
    
    # order = 448922
      # NY13: Rockland Co []
      # NY15: Westchester Co []
    
      # NY16: Southern Nassau Co []
      # NY17: Northern Nassau Co []
      # NY18: Central Nassau Co []
      # NY19: Northwest Suffolk Co []
      # NY20: Southwest Suffolk Co []
      # NY21: East Suffolk Co []
    
      # NY14: Staten Island []
      # NY22: Southeast Brooklyn []
      # NY23: West Brooklyn []
      # NY24: Northeast Brooklyn []
      # NY25: East Bronx []
      # NY26: West Bronx []
      # NY27: Manhattan []
      # NY28: South Queens []
      # NY29: Northwest Queens []
      # NY30: Northeast Queens []

    
    # order = xxx
      # NY13: Rockland Co [Y]
      # NY15: Westchester Co [Y]
    
      # NY16: Southern Nassau Co [Y]
      # NY17: Northern Nassau Co [Y]
      # NY18: Central Nassau Co [Y]
      # NY19: Northwest Suffolk Co [Y]
      # NY20: Southwest Suffolk Co [Y]
      # NY21: East Suffolk Co [Y]
    
      # NY14: Staten Island []
      # NY22: Southeast Brooklyn []
      # NY23: West Brooklyn []
      # NY24: Northeast Brooklyn []
      # NY25: East Bronx []
      # NY26: West Bronx []
      # NY27: Manhattan [Y]
      # NY28: South Queens []
      # NY29: Northwest Queens []
      # NY30: Northeast Queens []
        
    # NY13: Rockland Co
    # NY14: Staten Island
    # NY15: Westchester Co
    # NY16: Southern Nassau Co
    # NY17: Northern Nassau Co
    # NY18: Central Nassau Co
    # NY19: Northwest Suffolk Co
    # NY20: Southwest Suffolk Co
    # NY21: East Suffolk Co
    # NY22: Southeast Brooklyn
    # NY23: West Brooklyn
    # NY24: Northeast Brooklyn
    # NY25: East Bronx
    # NY26: West Bronx
    # NY27: Manhattan
    # NY28: South Queens
    # NY29: Northwest Queens
    # NY30: Northeast Queens
    
  # start with order 448922; PSAT score 1070 - 1180
  
  df <- fa20_oos_psat_sf %>% as.data.frame() %>% filter(ord_num %in% c('448922'), eps %in% c('PA 1','PA 2','PA 3','PA 4','PA 5'))
  
  df_table <- df %>% group_by(eps) %>% summarize(
    stu_all = n(),
    stu_white = sum(stu_white_01, na.rm = TRUE),
    stu_asian = sum(stu_asian_01, na.rm = TRUE),
    stu_black = sum(stu_black_01, na.rm = TRUE),
    stu_hispanic = sum(stu_hispanic_01, na.rm = TRUE),
    stu_amerindian = sum(stu_amerindian_01, na.rm = TRUE),
    stu_native_hawaiian = sum(stu_nativehawaii_01, na.rm = TRUE),
    stu_tworaces = sum(stu_tworaces_01, na.rm = TRUE),
    stu_unknown = sum(stu_unknown_01, na.rm = TRUE),
    stu_na = sum(is.na(stu_race_cb), na.rm = TRUE)
  ) %>% mutate(
    stu_sum_calc = rowSums(select(.,stu_white,stu_asian,stu_black,stu_hispanic,stu_amerindian,stu_native_hawaiian,stu_tworaces,stu_unknown, stu_na)),
    stu_race_known = rowSums(select(.,stu_white,stu_asian,stu_black,stu_hispanic,stu_amerindian,stu_native_hawaiian,stu_tworaces))
  ) %>% mutate(
    # create the row percent variables;
    row_pct_white = stu_white/stu_race_known*100,
    row_pct_asian = stu_asian/stu_race_known*100,
    row_pct_black = stu_black/stu_race_known*100,
    row_pct_hispanic = stu_hispanic/stu_race_known*100,
    row_pct_amerindian = stu_amerindian/stu_race_known*100,
    row_pct_native_hawaaian = stu_native_hawaiian/stu_race_known*100,
    row_pct_tworaces = stu_tworaces/stu_race_known*100,
  ) %>% mutate( # %>% select(eps,pct_stu_white,pct_stu_asian,pct_stu_black,pct_stu_hispanic,pct_stu_amerindian,pct_stu_native_hawaaian,pct_stu_tworaces)
    # create col pct 
    col_pct_white = stu_white/sum(stu_white, na.rm = TRUE)*100,
    col_pct_asian = stu_asian/sum(stu_asian, na.rm = TRUE)*100,
    col_pct_black = stu_black/sum(stu_black, na.rm = TRUE)*100,
    col_pct_hispanic = stu_hispanic/sum(stu_hispanic, na.rm = TRUE)*100,
    col_pct_amerindian = stu_amerindian/sum(stu_amerindian, na.rm = TRUE)*100,
    col_pct_native_hawaaian = stu_native_hawaiian/sum(stu_native_hawaiian, na.rm = TRUE)*100,
    col_pct_tworaces = stu_tworaces/sum(stu_tworaces, na.rm = TRUE)*100,
  ) %>% select(eps,stu_white,stu_asian,stu_black,stu_hispanic,col_pct_white,col_pct_asian,col_pct_black,col_pct_hispanic)
  

  
   fa20_oos_psat %>% mutate(stu_zip_len = str_length(stu_zip)) %>% count(stu_zip_len) # usually has 9-digit zipcode; 246,401/257993 = 95.5% of the time!
  
   fa20_oos_psat %>% mutate(hs_zip_len = str_length(hs_zip_code)) %>% filter(is.na(hs_zip_len)) %>% count(hs_state_code)
   count(hs_zip_len)
   
   # focus on PA from the first order w/ PSAT score of 1070-1180
   fa20_oos_psat %>% filter(hs_state_code == 'PA',ord_num == '') %>% 
     leaflet() %>%
     addTiles() %>%  # Add default map tiles
     addCircleMarkers(
       lng = ~hs_longitude,  # Longitude for the high school
       lat = ~hs_latitude,   # Latitude for the high school
       radius = 4,           # Marker size
       color = "blue",       # Marker color
       stroke = FALSE,       # No borders around markers
       fillOpacity = 0.7,    # Opacity of the markers
       popup = ~paste("ZIP Code:", hs_zip_code)  # Popup showing the high school ZIP code
     )
   
   
   # Create the subset for PA borders (already done)
   #pa_eps_geometry <- eps_geometry_zcta %>% filter(substr(eps, 1, 2) == 'PA')
   pa_eps_geometry <- eps_geometry_zcta %>% filter(eps %in% c('PA 1','PA 2','PA 3','PA 4','PA 5'))
   
   # Ensure CRS is WGS84
   pa_eps_geometry <- st_transform(pa_eps_geometry, crs = 4326)
   
   fa20_oos_psat_sf %>% glimpse()
   # Group by high school and count the number of students per school
   school_counts <- fa20_oos_psat_sf %>%
     filter(hs_state_code == 'PA', ord_num == '448922') %>%
     group_by(hs_ncessch,geometry) %>%
     summarise(
       stu_all = n(),
       stu_white = sum(stu_white_01, na.rm = TRUE),
       stu_asian = sum(stu_asian_01, na.rm = TRUE),
       stu_black = sum(stu_black_01, na.rm = TRUE),
       stu_hispanic = sum(stu_hispanic_01, na.rm = TRUE),
     )
   
   school_counts %>% glimpse()
  # Create the leaflet map with the PA borders and labels
   leaflet(school_counts) %>%
     addProviderTiles(provider = providers$CartoDB.Positron) %>%
     
     # Markers for All Students
     addCircleMarkers(
       lng = ~st_coordinates(geometry)[,1],
       lat = ~st_coordinates(geometry)[,2],
       radius = ~sqrt(stu_all) + 2,
       color = "blue",
       stroke = TRUE,
       weight = 1,
       fillOpacity = 0.0,
       group = "All Students",
       popup = ~paste("Students (All):", stu_all)
     ) %>%
     
     # Markers for White Students
     addCircleMarkers(
       lng = ~st_coordinates(geometry)[,1],
       lat = ~st_coordinates(geometry)[,2],
       radius = ~sqrt(stu_white) + 2,
       color = "red",
       stroke = TRUE,
       weight = 1,
       fillOpacity = 0.0,
       group = "White Students",
       popup = ~paste("Students (White):", stu_white)
     ) %>%
     
     # Markers for Asian Students
     addCircleMarkers(
       lng = ~st_coordinates(geometry)[,1],
       lat = ~st_coordinates(geometry)[,2],
       radius = ~sqrt(stu_asian) + 2,
       color = "green",
       stroke = TRUE,
       weight = 1,
       fillOpacity = 0.0,
       group = "Asian Students",
       popup = ~paste("Students (Asian):", stu_asian)
     ) %>%
     
     # Markers for Black Students
     addCircleMarkers(
       lng = ~st_coordinates(geometry)[,1],
       lat = ~st_coordinates(geometry)[,2],
       radius = ~sqrt(stu_black) + 2,
       color = "purple",
       stroke = TRUE,
       weight = 1,
       fillOpacity = 0.0,
       group = "Black Students",
       popup = ~paste("Students (Black):", stu_black)
     ) %>%
     
     # Markers for Hispanic Students
     addCircleMarkers(
       lng = ~st_coordinates(geometry)[,1],
       lat = ~st_coordinates(geometry)[,2],
       radius = ~sqrt(stu_hispanic) + 2,
       color = "orange",
       stroke = TRUE,
       weight = 1,
       fillOpacity = 0.0,
       group = "Hispanic Students",
       popup = ~paste("Students (Hispanic):", stu_hispanic)
     ) %>%
     
     # Modify the addPolygons() function
     addPolygons(
       data = pa_eps_geometry,
       color = "black",
       weight = 2,
       fill = TRUE,         # Enable fill to capture mouse events over the area
       fillOpacity = 0,     # Make the fill transparent
       group = "EPS Borders",
       popup = ~paste("EPS:", eps),
       label = ~eps,
       labelOptions = labelOptions(
         style = list("font-weight" = "bold", padding = "3px 8px"),
         textsize = "15px",
         direction = "auto"
       )
     ) %>%
     
     # Add layer controls to toggle visibility
     addLayersControl(
       overlayGroups = c(
         "All Students", "White Students", "Asian Students",
         "Black Students", "Hispanic Students", "EPS Borders"
       ),
       options = layersControlOptions(collapsed = FALSE)
     )
   