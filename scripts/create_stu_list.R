################################################################################
## [ PROJ ] < College Board Geomarket >
## [ FILE ] < create_stu_list.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 8/22/2024
## [ DESC ] < Create analysis datasets to examine who is included/excluded when student list purchases filter on geomarket >
################################################################################


### DIRECTORY PATHS FOR PUBLIC REQUESTS REPO

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

################### CREATE SECONDARY DATA SETS FOR METROS/SCHOOLS [CODE FROM KARINA'S PROXIMITY TO WHITENESS PAPER]

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

################### FINAL SAMPLE FOR EMPIRICAL REPORT

#remove extra dataframes
#rm(lists_orders_zip_df)

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


################### FINAL SAMPLE FOR AJS MANUSCRIPT

# keep only orders that have accompanying students lists 

# How many total orders did we get across all universities?
orders_df %>% count(univ_name) # 14 universities (+2 MN univs removed above)
orders_df %>% summarise(n=n_distinct(order_num)) # 835 total lists from 14 universities
orders_df %>% summarise(sum(num_students, na.rm = TRUE))#total prospects from order sum 4669973

# How many total lists we get across all universities?
lists_orders_zip_hs_df %>% count(univ_name) # total lists from 13 universities (-NAU only gave us orders)
lists_orders_zip_hs_df %>% summarise(n=n_distinct(ord_num)) #596 lists
lists_orders_zip_hs_df %>% nrow() #3665455 prospects

#How many orders + accompanying lists do we have across universities?
final_sample <- subset(lists_orders_zip_hs_df, ord_num %in% orders_df$order_num)
final_sample %>% count(univ_name) # from 11 universities 
final_sample %>% summarise(n=n_distinct(ord_num)) #414 lists
final_sample %>% nrow() #2,549,085



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

##### CREATE DIRECTORY OBJECTS FOR THE CB_GEO METRO

# remove directory objects associated with the public_requests_eda repo
rm(data_dir,public_requests_eda_dir,scripts_dir)
rm(vunique)

# change directories to the cb_geo repo
setwd(file.path('.','..','cb_geo'))
getwd()

# run script that creates directories
source(file = file.path('scripts', 'directories.R'))


# script that creates character vectors for EPS codes
source(file = file.path(scripts_dir, 'metro_eps_codes.R'))

###### LOAD EPS SHAPE FILE DATA


load(file.path(eps_data_dir, 'eps_shapes_2020.RData'))
eps_geometry_zcta <- eps_2020

eps_geometry_zcta %>% class()
eps_geometry_zcta %>% glimpse()


#df <- orders_df %>%  count(univ_name, race_filter, race_ethnicity) %>% print(n=25)
#race_orders %>%  count(race_filter)


############
############ CREATE DATASET lists_orders_zip_hs_df with SF
   
   lists_orders_zip_hs_df %>% glimpse() # note: this dataframe has hs_longitude and hs_latitude
   
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

# append nj04 orders from fall 2021 class to orders from fall 2020 class
# will need to make a note of this in the appendix!

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
lists_orders_zip_hs_df_sf <- lists_orders_zip_hs_df_sf %>% bind_rows(nj04_subset)
rm(nj04_subset)
      
# remove objects you won't need
  rm(lists_df)
  rm(lists_orders_df)
  rm(univ_data)
  rm(zip_cbsa_data,zip_cbsa_name_data,zip_locale,zip_to_state,zip_to_state_v2)
  rm(ccd,privhs_data,pss,pubhs_data)
  rm(acs_income_metro,acs_income_zip,acs_race_zipcodev3,acs_zip)
  
  