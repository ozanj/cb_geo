################################################################################
## [ PROJ ] < College Board Geomarket >
## [ FILE ] < create_stu_list.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 1/16/2025
## [ DESC ] < Create tables and graphs for RQ2 of Geomarket paper >
################################################################################

### SETTINGS
#rm(list = ls()) # remove all objects
options(max.print=1000)
#options(width = 160)
# Set the scipen option to a high value to avoid scientific notation
options(scipen = 999)

### LIBRARIES
library(tidyverse)
library(formattable)
library(patchwork)
library(scales)
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


######### CREATE ANALYSIS DATASET(S) FOR RQ2 TABLES N' GRAPHS

getwd()
source(file = file.path('scripts', 'create_stu_list.R'))
getwd()


# RACE TABLE FUNCTION FUNCTION. CREATES TABLE W/ PERCENT OF PEOPLE IN EACH RACIAL GROUP X GEOMARKET FOR AN ORDER(S)

# Define the function

create_sim_eps_race_table <- function(data, ord_nums, eps_codes) {
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
      stu_aian = sum(stu_amerindian_01, na.rm = TRUE),
      stu_nhpi = sum(stu_nativehawaii_01, na.rm = TRUE),
      stu_multi = sum(stu_tworaces_01, na.rm = TRUE),
      stu_unknown = sum(stu_unknown_01, na.rm = TRUE),
      stu_na = sum(is.na(stu_race_cb), na.rm = TRUE)
    ) %>%
    mutate(
      stu_sum_calc = rowSums(select(., stu_white, stu_asian, stu_black, stu_hispanic,
                                    stu_aian, stu_nhpi, stu_multi,
                                    stu_unknown, stu_na)),
      stu_race_known = rowSums(select(., stu_white, stu_asian, stu_black, stu_hispanic,
                                      stu_aian, stu_nhpi, stu_multi))
    ) %>% relocate(eps_codename,stu_all,stu_sum_calc,stu_na,stu_unknown,stu_race_known)
  
  df_sum <- df %>% summarize(
    stu_all = sum(stu_all, na.rm = TRUE),
    stu_sum_calc = sum(stu_sum_calc, na.rm = TRUE),
    stu_unknown = sum(stu_unknown, na.rm = TRUE),
    stu_na = sum(stu_na, na.rm = TRUE),
    stu_race_known = sum(stu_race_known, na.rm = TRUE),
    stu_white = sum(stu_white, na.rm = TRUE),
    stu_asian = sum(stu_asian, na.rm = TRUE),
    stu_black = sum(stu_black, na.rm = TRUE),
    stu_hispanic = sum(stu_hispanic, na.rm = TRUE),
    stu_aian = sum(stu_aian, na.rm = TRUE),
    stu_nhpi = sum(stu_nhpi, na.rm = TRUE),
    stu_multi = sum(stu_multi, na.rm = TRUE),
  ) %>% mutate(eps_codename = "All") 
  
  df_sum <- bind_rows(df,df_sum)
  
  # create row pct table
  df_r <- df_sum %>%   mutate(
    # Create row percentage variables
    r_known = stu_race_known / stu_race_known * 100,
    r_white = stu_white / stu_race_known * 100,
    r_asian = stu_asian / stu_race_known * 100,
    r_black = stu_black / stu_race_known * 100,
    r_hispanic = stu_hispanic / stu_race_known * 100,
    r_aian = stu_aian / stu_race_known * 100,
    r_nhpi = stu_nhpi / stu_race_known * 100,
    r_multi = stu_multi / stu_race_known * 100
  ) 
  
  # create column pct table
  df_c <- df_sum %>% select(-c(stu_sum_calc)) %>% 
    # get rid of "all" row for column table
    filter(eps_codename != "All") %>% 
    mutate(
      # Create column percentage variables
      c_known = stu_race_known / sum(stu_race_known, na.rm = TRUE) * 100,
      c_white = stu_white / sum(stu_white, na.rm = TRUE) * 100,
      c_asian = stu_asian / sum(stu_asian, na.rm = TRUE) * 100,
      c_black = stu_black / sum(stu_black, na.rm = TRUE) * 100,
      c_hispanic = stu_hispanic / sum(stu_hispanic, na.rm = TRUE) * 100,
      c_aian = stu_aian / sum(stu_aian, na.rm = TRUE) * 100,
      c_nhpi = stu_nhpi / sum(stu_nhpi, na.rm = TRUE) * 100,
      c_multi = stu_multi / sum(stu_multi, na.rm = TRUE) * 100
    )  # %>% select(eps,stu_all,stu_race_known,stu_white,stu_asian,stu_black,stu_hispanic, stu_tworaces, col_pct_white,col_pct_asian,col_pct_black,col_pct_hispanic, col_pct_tworaces)
  
  # create new all row for column table
  
  df_c_sum <- df_c %>% summarize(
    stu_all = sum(stu_all, na.rm = TRUE),
    stu_race_known = sum(stu_race_known, na.rm = TRUE),
    c_known = sum(c_known, na.rm = TRUE),
    c_white = sum(c_white, na.rm = TRUE),
    c_asian = sum(c_asian, na.rm = TRUE),
    c_black = sum(c_black, na.rm = TRUE),
    c_hispanic = sum(c_hispanic, na.rm = TRUE),
    c_aian = sum(c_aian, na.rm = TRUE),
    c_nhpi = sum(c_nhpi, na.rm = TRUE),
    c_multi = sum(c_multi, na.rm = TRUE),
    
  ) %>% mutate(eps_codename = "All")
  
  df_c <- bind_rows(df_c,df_c_sum)
  
  
  # First table: current output with counts
  table1 <- df_sum %>% 
    select(
      eps_codename, stu_all, stu_race_known, stu_white, stu_asian, stu_black, stu_hispanic, stu_multi,  stu_aian, stu_nhpi
    ) %>% rename(all = stu_all, race_known = stu_race_known)
  
  # Second table: specified columns with row percentages
  table2 <- df_r %>%
    select(
      eps_codename, stu_all, stu_race_known, r_known, r_white, r_asian, r_black, r_hispanic, r_multi, r_aian, r_nhpi
    ) %>% rename(all = stu_all, race_known = stu_race_known) # %>% select(-c(r_aian, r_nhpi))
  
  # Third table: current output with column percentages
  table3 <- df_c %>% 
    select(
      eps_codename, stu_all, stu_race_known, c_known, c_white, c_asian, c_black, c_hispanic, c_multi, c_aian, c_nhpi
    ) %>% rename(all = stu_all, race_known = stu_race_known) # %>% select(-c(c_aian, c_nhpi))
  
  # Return a list with both tables
  result <- list(
    count_table = table1, # exclude the count table
    row_pct_table = table2,
    col_pct_table = table3
  )
  
  return(result)
}
# ASU order 448922: PSAT 1070 - 1180; order 448427: PSAT 1190 - 1260; order 448440: PSAT 1270 - 1520

# call function for philly metro area
create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5'))

# FIRSTGEN TABLE FUNCTION. CREATE TABLE OF PERCENT OF PEOPLE IN FIRST GEN STATUS GROUP THAT ARE IN EACH GEOMARKET

# Define the function
create_sim_eps_firstgen_table <- function(data, ord_nums, eps_codes) {
  
  df <- data %>%
    as_tibble() %>%
    filter(ord_num %in% ord_nums, eps %in% eps_codes) %>%
    mutate(
      eps_codename = str_c(eps, eps_name, sep = ", "),
      stu_no_col_01 = if_else(stu_first_gen ==1,1,0),
      stu_some_col_01 = if_else(stu_first_gen ==2,1,0),
      stu_not_first_gen_01 = if_else(stu_first_gen ==3,1,0),
      stu_unknown_01 = if_else(stu_first_gen ==4,1,0),
    ) %>% 
    group_by(eps_codename) %>% 
    summarize(
      stu_all = n(),
      stu_no_col = sum(stu_no_col_01, na.rm = TRUE),
      stu_some_col = sum(stu_some_col_01, na.rm = TRUE),
      stu_not_first = sum(stu_not_first_gen_01, na.rm = TRUE),
      stu_unknown = sum(stu_unknown_01, na.rm = TRUE),
    ) %>% mutate(
      stu_known = rowSums(select(., stu_no_col,stu_some_col,stu_not_first)),
      stu_sum_calc = rowSums(select(., stu_no_col,stu_some_col,stu_not_first,stu_unknown))
    )
  
  # create row that is sum of all geomarkets
  df_sum <- df %>% summarize(
    stu_all = sum(stu_all, na.rm = TRUE),
    stu_sum_calc = sum(stu_sum_calc, na.rm = TRUE),
    stu_known = sum(stu_known, na.rm = TRUE),
    stu_no_col = sum(stu_no_col, na.rm = TRUE),
    stu_some_col = sum(stu_some_col, na.rm = TRUE),
    stu_not_first = sum(stu_not_first, na.rum = TRUE),
    stu_unknown = sum(stu_unknown, na.rm = TRUE),
    stu_sum_calc = sum(stu_sum_calc, na.rm = TRUE),
  ) %>% mutate(
    eps_codename = "All"
  ) 
  
  df <- bind_rows(df,df_sum)
  
  # create row percentage variables
  df_r <- df %>% mutate(
    r_no_col = stu_no_col / stu_known * 100,
    r_some_col = stu_some_col / stu_known * 100,
    r_not_first = stu_not_first / stu_known * 100,
    r_known = stu_known / stu_known * 100,
  ) 
  
  
  # Create column percentage variables
  df_c <- df %>% 
    filter(eps_codename != "All") %>%         
    mutate(
      
      c_no_col = stu_no_col / sum(stu_no_col, na.rm = TRUE) * 100,
      c_some_col = stu_some_col / sum(stu_some_col, na.rm = TRUE) * 100,
      c_not_first = stu_not_first / sum(stu_not_first, na.rm = TRUE) * 100,
      c_known = stu_known/ sum(stu_known, na.rm = TRUE) * 100,
    )
  
  # create new "all" row for column table
  df_c_sum <- df_c %>% summarize(
    stu_all = sum(stu_all, na.rm = TRUE),
    stu_known = sum(stu_known, na.rm = TRUE),
    c_known = sum(c_known, na.rm = TRUE),
    c_no_col = sum(c_no_col, na.rm = TRUE),
    c_some_col = sum(c_some_col, na.rm = TRUE),
    c_not_first = sum(c_not_first, na.rm = TRUE),
  ) %>% mutate(eps_codename = "All")
  
  df_c <- bind_rows(df_c,df_c_sum)
  
  # First table: current output with counts
  table1 <- df %>% 
    select(
      eps_codename, stu_all, stu_known, stu_unknown, stu_no_col, stu_some_col, stu_not_first
    )
  table1
  
  # Second table: current output with row percentages    
  table2 <- df_r %>% 
    select(
      eps_codename, stu_all, stu_known, r_known, r_no_col, r_some_col, r_not_first
    )
  table2
  
  # Third table: specified columns with column percentages
  table3 <- df_c %>%
    select(
      eps_codename, stu_all, stu_known, c_known, c_no_col, c_some_col, c_not_first
    )
  table3
  
  # Return a list with both tables
  result <- list(
    count_table = table1,
    row_pct_table = table2,
    col_pct_table = table3
  )
  return(result)
  
}
# run function that creates row and column tables for first-gen

# RACE X FIRSTGEN TABLE FUNCTION. CREATE TABLE OF RACE X FIRSTGEN STATUS GROUP THAT ARE IN EACH GEOMARKET

create_sim_eps_race_firstgen_table <- function(data, ord_nums, eps_codes) {
  
  # First, create the main processed data frame
  # <- lists_orders_zip_hs_df_sf %>%  as_tibble() %>% filter(ord_num %in% c('448922'), eps %in% c('PA 1','PA 2','PA 3','PA 4','PA 5')) %>%
  df_work <- data %>% 
    as_tibble() %>% 
    filter(ord_num %in% ord_nums, eps %in% eps_codes) %>%
    # get rid of obs where race is not known or first-generation status is not known
    filter(!is.na(stu_race_cb), stu_race_cb != 0, stu_first_gen != 4) %>% 
    filter(stu_race_cb %in% c(2,3,4,9,12)) %>% # only some racial groups
    mutate(
      eps_codename = str_c(eps, eps_name, sep = ", "),
      stu_race_cb = as_factor(stu_race_cb)
    )
  
  
  #df_work %>% count(stu_race_cb)
  
  # create counts of first gen status for all race + all EPS
  df_work_all <- df_work %>% count(stu_first_gen) %>% 
    mutate(stu_first_gen = case_match(
      stu_first_gen,
      1 ~ 'no_col',
      2 ~ 'some_col',
      3 ~ 'not_first_gen'
    )) %>% 
    pivot_wider(
      names_from = stu_first_gen,
      values_from = n
    ) %>% 
    mutate(
      across(c(no_col, some_col, not_first_gen), ~ if_else(is.na(.), 0, .)),
    ) %>% 
    mutate(
      all = rowSums(select(., no_col, some_col, not_first_gen)),
      stu_race_cb = factor("All"),
      eps_codename = "All",
    ) %>% relocate(stu_race_cb, eps_codename)
  
  #df_work_all
  
  # create counts of first gen status by EPS
  df_work_by_eps <- df_work %>% group_by(eps_codename) %>% 
    count(stu_first_gen) %>% 
    ungroup() %>%
    mutate(stu_first_gen = case_match(
      stu_first_gen,
      1 ~ 'no_col',
      2 ~ 'some_col',
      3 ~ 'not_first_gen'
    )) %>% 
    pivot_wider(
      names_from = stu_first_gen,
      values_from = n
    ) %>% 
    mutate(
      across(c(no_col, some_col, not_first_gen), ~ if_else(is.na(.), 0, .)),
    ) %>% 
    mutate(
      all = rowSums(select(., no_col, some_col, not_first_gen)),
      stu_race_cb = "All",
      stu_race_cb = as_factor(stu_race_cb)
    ) %>% relocate(stu_race_cb)
  
  #df_work_by_eps
  
  
  # create counts of first gen status by race, all EPS
  df_work_by_race <- df_work %>% 
    group_by(stu_race_cb) %>%
    count(stu_first_gen) %>% 
    ungroup() %>%
    mutate(stu_first_gen = case_match(
      stu_first_gen,
      1 ~ 'no_col',
      2 ~ 'some_col',
      3 ~ 'not_first_gen'
    )) %>% 
    pivot_wider(
      names_from = stu_first_gen,
      values_from = n
    ) %>% 
    mutate(
      across(c(no_col, some_col, not_first_gen), ~ if_else(is.na(.), 0, .)),
    ) %>% 
    mutate(
      all = rowSums(select(., no_col, some_col, not_first_gen)),
      eps_codename = "All",
    ) %>% relocate(stu_race_cb, eps_codename)
  
  #df_work_by_race
  
  # create counts of first gen status by race and eps
  df_work_by_race_eps <- df_work %>% 
    group_by(stu_race_cb, eps_codename) %>%
    count(stu_first_gen) %>% 
    ungroup() %>%
    mutate(stu_first_gen = case_match(
      stu_first_gen,
      1 ~ 'no_col',
      2 ~ 'some_col',
      3 ~ 'not_first_gen'
    )) %>% 
    pivot_wider(
      names_from = stu_first_gen,
      values_from = n
    ) %>% 
    mutate(
      across(c(no_col, some_col, not_first_gen), ~ if_else(is.na(.), 0, .)),
    ) %>% 
    mutate(
      all = rowSums(select(., no_col, some_col, not_first_gen))      
    )
  
  #df_work_by_race_eps %>% print(n=30)
  
  # Finally, bind the two data frames together
  final_df <- bind_rows(df_work_all, df_work_by_eps, df_work_by_race, df_work_by_race_eps) %>% 
    mutate(
      across(c(no_col, some_col, not_first_gen, some_col),
             ~ (.x / all) * 100,
             .names = "row_{.col}")
    ) %>% select(-c(no_col,some_col,not_first_gen)) %>% 
    # Set factor levels for stu_race_cb in the desired order
    mutate(
      stu_race_cb = fct_relevel(
        stu_race_cb,
        "All", 
        "Asian", 
        "Black/African American", 
        "Hispanic/Latino",
        "white",
        "two or more races, non-Hispanic"
      )
    ) %>%
    # Then arrange by stu_race_cb and eps_codename
    arrange(stu_race_cb, eps_codename) 
  
  #final_df %>% print(n=60)
  
  return(final_df)
  
}

# philly metro area
# order 448922: PSAT 1070 - 1180; order 448427: PSAT 1190 - 1260; order 448440: PSAT 1270 - 1520

create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5'))
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5'))
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5')) %>% print(n=100)


# Philly metro area
create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5')) 
create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448427'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5'))
create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448440'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5'))

philly_order448922_psat1070_1180 <- create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5')) 
philly_order448922_psat1070_1180

philly_order448427_psat1190_1260 <- create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448427'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5')) 
philly_order448427_psat1190_1260

philly_order448440_psat1270_1520 <- create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448440'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5')) 
philly_order448440_psat1270_1520  

save(
  philly_order448922_psat1070_1180, philly_order448427_psat1190_1260, philly_order448440_psat1270_1520,
  file = file.path('.','results','tables','philly_asu_eps_lists.RData'))

# IL: 07 [Y], 08 [Y], 09 [Y], 10 [Y], 11, 12 [Y], 13 [Y]: doesn't have city of Chicago!!!
c('448922','448427','448440')

# Greater Detroit
# MI 1 = wayne county, MI 2 = detroit's northern suburbs; MI 3 = ann arbor
create_sim_eps_race_table(data = fa20_oos_psat_sf, ord_nums = c('448922'), eps_codes = c('MI 1','MI 2','MI 3')) # 52% of Black students in wayne county
create_sim_eps_race_table(data = fa20_oos_psat_sf, ord_nums = c('448427'), eps_codes = c('MI 1','MI 2','MI 3')) 
create_sim_eps_race_table(data = fa20_oos_psat_sf, ord_nums = c('448440'), eps_codes = c('MI 1','MI 2','MI 3'))

# Greater Houston. TX 15: Northwest Houston and Conroe School District [Y]; TX 16: Southwest Houston Metro Area [Y]; TX17: Cit of Houston (East) [NO]; TX18:Galveston and East Harris Counties [NO]
# no ASU order does city of Houston; but note we have Texas A&M


# START HERE ON TUESDAY [LET'S DO ENTIRE NEW YORK METRO AREA]; BUT NJ04 IS IN 547038, 484698; ETC.
# GO THROUGH NEW JERSEY
# metro jersey is: 2, 4, 5, 7, 8, 9, 10, 11

nj_metro_eps_codes
create_sim_eps_race_table(data = fa20_oos_psat_sf, ord_nums = c('448922'), eps_codes = nj_metro_eps_codes)
# disproportionate share of black students in NJ08 == essex and southern Passaic County; also, hispanic students concentrated in nj7-nj10 (which is all in one area)
create_sim_eps_race_table(data = fa20_oos_psat_sf, ord_nums = c('448427'), eps_codes = nj_metro_eps_codes) # black students concentrated in nj08
create_sim_eps_race_table(data = fa20_oos_psat_sf, ord_nums = c('448440'), eps_codes = nj_metro_eps_codes) # black students stil concentrated in nj08

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
create_sim_eps_race_table(data = fa20_oos_psat_sf, ord_nums = c('448922'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24')) # 
create_sim_eps_race_table(data = fa20_oos_psat_sf, ord_nums = c('448427'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24')) 
create_sim_eps_race_table(data = fa20_oos_psat_sf, ord_nums = c('448440'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24'))


# NEW YORK
# Westchester and Rockland Counties: 13 and 15
# Long Island: 16 through 21
# City of New York: 14, 22 through 30

long_island_eps_codes <- c(paste0('NY',16:21))

create_sim_eps_race_table(data = fa20_oos_psat_sf, ord_nums = c('448922'), eps_codes = long_island_eps_codes) # NY18 - central Nassau is where disproportionate % of Black students are
create_sim_eps_race_table(data = fa20_oos_psat_sf, ord_nums = c('448427'), eps_codes = long_island_eps_codes) # same
create_sim_eps_race_table(data = fa20_oos_psat_sf, ord_nums = c('448440'), eps_codes = long_island_eps_codes) # 


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


# Philly
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5'))
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448427'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5'))
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448440'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5'))

# Greater Detroit [not much SES inequity in who is included/excluded when you target particular geomarkets]
# MI 1 = wayne county, MI 2 = detroit's northern suburbs; MI 3 = ann arbor
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = c('MI 1','MI 2','MI 3')) #
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448427'), eps_codes = c('MI 1','MI 2','MI 3')) 
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448440'), eps_codes = c('MI 1','MI 2','MI 3'))

# Greater Dalls [one geomarket -- Collin + Rockwall with way higher share of non-first gen students than others]
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24')) # 
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448427'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24')) 
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448440'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24'))

# New Jersey metropolitan Geomarkets  
# interesting results for hudson county, essex [to a lesser extent], and for bergen (nj10) and morris (nj11) the two richy rich ones
nj_metro_eps_codes
create_sim_eps_firstgen_table(data = fa20_oos_psat_sf, ord_nums = c('448922'), eps_codes = nj_metro_eps_codes) #
create_sim_eps_firstgen_table(data = fa20_oos_psat_sf, ord_nums = c('448427'), eps_codes = nj_metro_eps_codes) #
create_sim_eps_firstgen_table(data = fa20_oos_psat_sf, ord_nums = c('448440'), eps_codes = nj_metro_eps_codes) #

# NEW YORK
# Westchester and Rockland Counties: 13 and 15 [haven't checked if these are in ASU order]
# Long Island: 16 through 21
# City of New York: 14, 22 through 30 [haven't checked if these are in ASU order]

long_island_eps_codes <- c(paste0('NY',16:21))

# NY17, Northern Nassau Co and NY19, Northwest Suffolk Co are the richy rich [i think]
#NY20, Southwest Suffolk Co  has disproportionate share of first gen
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = long_island_eps_codes) # 
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448427'), eps_codes = long_island_eps_codes) # 
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448440'), eps_codes = long_island_eps_codes) # 


create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5')) # order 448922: PSAT 1070 - 1180
create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448427'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5')) # order 448427: PSAT 1190 - 1260
create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448440'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5')) # order 448440: PSAT 1270 - 1520

create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5')) # order 448922: PSAT 1070 - 1180
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448427'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5')) # order 448427: PSAT 1190 - 1260
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448440'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5')) # order 448440: PSAT 1270 - 1520


create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5')) %>% print(n=50) # order 448922: PSAT 1070 - 1180
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448427'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5')) %>% print(n=50) # order 448427: PSAT 1190 - 1260
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448440'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5')) %>% print(n=50) # order 448440: PSAT 1270 - 1520

# WOW. EVEN FOR WHITE PEOPLE, THE WHITE PEOPLE IN PA 5 PHILLY ARE MUCH MORE LIKELY TO BE FIRST-GEN!

# Greater Detroit [not much SES inequity in who is included/excluded when you target particular geomarkets]
# MI 1 = wayne county, MI 2 = detroit's northern suburbs; MI 3 = ann arbor
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = c('MI 1','MI 2','MI 3'))  %>% print(n=50)
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448427'), eps_codes = c('MI 1','MI 2','MI 3'))  %>% print(n=50)
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448440'), eps_codes = c('MI 1','MI 2','MI 3')) %>% print(n=50)

# Greater Dalls [one geomarket -- Collin + Rockwall with way higher share of non-first gen students than others]
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24')) %>% print(n=50) # 
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448427'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24')) %>% print(n=50) 
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448440'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24')) %>% print(n=50)

# New Jersey metropolitan Geomarkets  
# interesting results for hudson county, essex [to a lesser extent], and for bergen (nj10) and morris (nj11) the two richy rich ones
nj_metro_eps_codes
create_sim_eps_race_firstgen_table(data = fa20_oos_psat_sf, ord_nums = c('448922'), eps_codes = nj_metro_eps_codes) %>% print(n=50) #
create_sim_eps_race_firstgen_table(data = fa20_oos_psat_sf, ord_nums = c('448427'), eps_codes = nj_metro_eps_codes) %>% print(n=50) #
create_sim_eps_race_firstgen_table(data = fa20_oos_psat_sf, ord_nums = c('448440'), eps_codes = nj_metro_eps_codes) %>% print(n=50) #

# NEW YORK
# Westchester and Rockland Counties: 13 and 15 [haven't checked if these are in ASU order]
# Long Island: 16 through 21
# City of New York: 14, 22 through 30 [haven't checked if these are in ASU order]

long_island_eps_codes <- c(paste0('NY',16:21))

# NY17, Northern Nassau Co and NY19, Northwest Suffolk Co are the richy rich [i think]
#NY20, Southwest Suffolk Co  has disproportionate share of first gen
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = long_island_eps_codes) %>% print(n=50) # 
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448427'), eps_codes = long_island_eps_codes) %>% print(n=50) # 
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448440'), eps_codes = long_island_eps_codes) %>% print(n=50) # 

############################
############################ INVESTIGATE CALIFORNIA
############################

# CA ORDERS FROM ASU TO DRAW FROM

#    ord_num     n
#    <chr>   <int>
# 1 366935  42790 FA19 - CA PSAT AD {JAN18); ordered 1/17/2018; 2019/20/21 HS grad class; CA; PSAT 1110 - 1210
# 8 366934  15806 FA19 - CA PSAT SE (JAN18); ordered 1/17/2018; 2019/20/21 hs grad class; CA; PSAT 1220-1290    
# 7 366932  15931 FA19 - CA PSAT BE (JAN18); ordered 1/17/2018; 2019/20/21 hs grad class; CA; PSAT 1300 - 1520

# 2 448375  33893 FA20 - CA PSAT AD {JAN19); ordered 1/8/2019; 2020 HS grad class; CA; PSAT 1070 - 1180
# 5 448374  19775; FA20 - CA PSAT BE (JAN19); ordered 1/8/2019; 2020/21 hs grad class; CA; PSAT 1270-1520
# 6 448420  19437; FA20 - CA PSAT SE (JAN19); ordered 1/8/2019; 2020/21 hs grad class; CA; PSAT 1190-1260

# 4 546954  21102 FA21 - CA PSAT AD (JAN20); ordered 1/6/2020; 2021 HS grad class; CA; PSAT 1070-1180
#12 546946  12061 FA21 - CA PSAT SE (JAN20); ordered 1/6/2020; 2021/2022 hs grad class; CA; PSAT 1190-1260
#13 546945  11041 FA21 - CA PSAT BE (JAN20); ordered 1/6/2020; 2021/22 hs grad class; CA; PSAT 1270-1520


# BAY AREA
bay_eps_codes 

create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('366935'), eps_codes = bay_eps_codes) # PSAT 1110 - 1210
create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('366934'), eps_codes = bay_eps_codes) # PSAT 1220-1290    
create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('366932'), eps_codes = bay_eps_codes) # PSAT 1300 - 1520

create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('366935'), eps_codes = bay_eps_codes) # PSAT 1110 - 1210
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('366934'), eps_codes = bay_eps_codes) # PSAT 1220-1290    
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('366932'), eps_codes = bay_eps_codes) # PSAT 1300 - 1520

create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('366935'), eps_codes = bay_eps_codes) %>% print(n=100) # PSAT 1110 - 1210 
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('366934'), eps_codes = bay_eps_codes) %>% print(n=100) # PSAT 1220-1290    
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('366932'), eps_codes = bay_eps_codes) %>% print(n=100) # PSAT 1300 - 1520



# SOCAL
socal_eps_codes

create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('366935'), eps_codes = socal_eps_codes) # PSAT 1110 - 1210
create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('366934'), eps_codes = socal_eps_codes) # PSAT 1220-1290    
create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('366932'), eps_codes = socal_eps_codes) # PSAT 1300 - 1520

create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('366935'), eps_codes = socal_eps_codes) # PSAT 1110 - 1210
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('366934'), eps_codes = socal_eps_codes) # PSAT 1220-1290    
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('366932'), eps_codes = socal_eps_codes) # PSAT 1300 - 1520

# interesting results for Hispanic by first gen status; some interesting results for white by first gen status; 
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('366935'), eps_codes = socal_eps_codes) %>% print(n=100) # PSAT 1110 - 1210 
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('366934'), eps_codes = socal_eps_codes) %>% print(n=100) # PSAT 1220-1290    
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('366932'), eps_codes = socal_eps_codes) %>% print(n=100) # PSAT 1300 - 1520

create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('366932','366934'), eps_codes = socal_eps_codes) %>% print(n=100) # PSAT 1220-1290 ,1300 - 1520

############
############ FIND ORDERS FOR ATL
############


# Order Number 547005, FA21 - OOS PSAT SE (JAN20)
# has GA01 - GA03; not GA04
# PSAT 1190 - 1260
# 2021 HS grad class
# purchased 1/6/2020

# Order Number 546978, FA21 - OOS PSAT BE (JAN20)
# has GA01 - GA03; not GA04
# PSAT 1270 - 1520
# 2021 HS grad class
# purchased 1/6/2020

# Order Number 320148 FA18 - OOS AD SS (JULY17); includes college size (2,000 to 5,000 students; fewer than 2000 students)
# has GA01 - GA04; 
# 2018, 2019, 2020 HS grad class;
# SAT score 1180 - 1300
# Order Number 320184, FA18 - OOS BE LS {JULY17
# has GA01 - GA04; 
# 2018, 2019, 2020 HS grad class; SAT 1370 - 1600; college size (basically any college size, except small)
# Order Number 320186, FA18 - OOS SE LS (JULY17
# has GA01 - GA04; 
# 2018, 2019, 2020 HS grad class; SAT 1310 - 1360; college size (basically any college size, except small)

atl_eps_codes <- paste0("GA ", 1:4) #
atl_eps_codes


# race
create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('547005'), eps_codes = atl_eps_codes) # doesn't have GA04, PSAT 1190 - 1260
create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('546978'), eps_codes = atl_eps_codes) # doesn't have GA04, PSAT 1270 - 1520

# firstgetn
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('547005'), eps_codes = atl_eps_codes) # doesn't have GA04, PSAT 1190 - 1260
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('546978'), eps_codes = atl_eps_codes) # doesn't have GA04, PSAT 1270 - 1520


# firstgen + race
#c('GA 1','GA 2','GA 3')

create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('547005'), eps_codes = c('GA 1','GA 2','GA 3')) %>% print(n=100) ## doesn't have GA04, PSAT 1190 - 1260
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('546978'), eps_codes = c('GA 1','GA 2','GA 3')) %>% print(n=100) ## doesn't have GA04, PSAT 1270 - 1520


# these are the small orders, that condition on college size
create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('320148'), eps_codes = atl_eps_codes) # 
create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('320184'), eps_codes = atl_eps_codes) # 
create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('320186'), eps_codes = atl_eps_codes) # 

############
############ DALLAS
############


# Greater Dalls [black students located in City of Dallas and Dallas count excluding dallas; but black students who get purchased are not (unlike detroit)]
# TX19: city of dallas [Y]; 
# TX20: city of forth worth [Y]; 
# TX21: irvington, arlington, grand prarie [Y]; 
# TX22: dallas county excluding city of dallas [Y];  
# TX23: collin and rockwall counties [Y]; 
# TX24: counties west of dallas/ft worth metropolex [Y]; 

create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24')) # 
create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448427'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24')) 
create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448440'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24'))

create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448440','448427'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24'))

create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24')) # 
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448427'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24')) 
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448440','448427'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24'))

create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24')) %>% print(n=100) #
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448427'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24')) %>% print(n=100) 
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448440','448427'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24')) %>% print(n=100)

############
############ HOUSTON
############

# 2 Stephen F Austin State University, univ_id=228431 ord_num=329702          PSAT 1010-1520; grades C+ to A+, 2019 HS grads, all of TX, ordered 8/31/2017 [but seems to have TX 17....]
create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('329702'), eps_codes = htown_eps_codes) # 
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('329702'), eps_codes = htown_eps_codes) # 
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('329702'), eps_codes = htown_eps_codes) %>% print(n=50) # 

# 2 Stephen F Austin State University, univ_id=228431 ord_num=402900          10407 15, 16, 17, 18; 2019 HS grads, SAT 1020-1410, ordered 8/22/2018

create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('402900'), eps_codes = htown_eps_codes) # 
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('402900'), eps_codes = htown_eps_codes) # 
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('402900'), eps_codes = htown_eps_codes) %>% print(n=50) # 

# 3 Stephen F Austin State University, univ_id=228431 ord_num=465204           7710 15, 16, 17, 18; SAT 1020-1410, 2020 HS grad class, ordered 5/22/2019

create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('465204'), eps_codes = htown_eps_codes) # 
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('465204'), eps_codes = htown_eps_codes) # 
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('465204'), eps_codes = htown_eps_codes) %>% print(n=50) # 

# UC SD
# order 560105 (f) NR 2022 PSAT URM (1100-1290); PSAT 1100 - 1290; GPA A+ TO B HISPANIC/BLACK/NATIVE; ordered 3/8/20
# pretty interesting
create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('560105'), eps_codes = htown_eps_codes) # 
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('560105'), eps_codes = htown_eps_codes) # 
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('560105'), eps_codes = htown_eps_codes) %>% print(n=50) # 


lists_orders_zip_hs_df %>% filter(univ_id == '110680') %>% count(ord_num) %>% arrange(desc(n)) %>% print(n=50)

###################################
################################### CHICAGO
###################################

chi_eps_codes <- c(paste0("IL ", 7:9), paste0("IL", 10:13))
chi_eps_codes    
lists_orders_zip_hs_df %>% filter(hs_state_code == 'IL' & univ_id ==145600) %>% count(ord_num) %>% arrange(desc(n)) %>% print(n=10) # U Illinois Chicago

# A tibble: 21 × 2
#   ord_num     n
#   <chr>   <int>

# 2 327384  15843
# 3 327368  14606
# 7 327385   9221 

# 6 392834  11243 # Illinois honors 2019; ordered 7/9/2018; HS class 2019, 2020; IL; SAT 1160 - 1300; 
# 8 392835   7394 # Illinois GPA 2019; ordered 7/10/2018; HS class 2019, 2020; IL; SAT 1310-1600; GPA A+ to B-
# 4 392833  14000 # Illinois standard 2019; ordered 7/9/2018; HS class 2019, 2020; IL; SAT 1020-1150;  GPA A+ to B-

# 1 487984  16926 # Illinois standard 2020; ordered 7/19/2019; HS class 2020, 2021; IL; SAT 1020-1150;  GPA A+ to B-
# 5 488035  12842 # Illinois HS 2020; ordered 7/19/2019; HS class 2020, 2021; SAT 1160 - 1300; GPA A+ to B-
# 9 488053   7259 # Illinois GPPA 2020 (no cf);ordered 7/19/2019; HS class 2020, 2021; SAT 1310-1600; GPA A+ to B-
create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('487984'), eps_codes = chi_eps_codes) # 
create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('488035'), eps_codes = chi_eps_codes) # 
create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('488053'), eps_codes = chi_eps_codes) # 

create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('487984'), eps_codes = chi_eps_codes) # 
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('488035'), eps_codes = chi_eps_codes) # 
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('488053'), eps_codes = chi_eps_codes) # 

create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('487984'), eps_codes = chi_eps_codes) %>% print(n=50) # 
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('488035'), eps_codes = chi_eps_codes) %>% print(n=50) # 
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('488053','488035'), eps_codes = chi_eps_codes) %>% print(n=50) # 



#10 488076   1414  

lists_orders_zip_hs_df %>% filter(hs_state_code == 'IL' & univ_id ==145637) %>% count(ord_num) %>% arrange(desc(n)) %>% print(n=10) # U Illinois Urbana-Champaign
# IL: 07 [Y], 08 [Y], 09 [Y], 10 [Y], 11, 12 [Y], 13 [Y]: doesn't have city of Chicago!!!
c('448922','448427','448440')

###################################
################################### CAN I HOLLER AT THE DMV
###################################    

lists_orders_zip_hs_df %>% count(zip_cbsatitle_1) %>% arrange(desc(n)) %>%  print(n=50)
lists_orders_zip_hs_df %>% count(zip_cbsa_1) %>% arrange(desc(n)) %>%  print(n=50)

# let's start w/ Maryland
lists_orders_zip_hs_df %>% filter(hs_state_code == 'MD') %>% count(zip_cbsatitle_1) %>% arrange(desc(n)) %>%  print(n=50)

lists_orders_zip_hs_df %>% filter(zip_cbsa_1 == '26420') %>% count(zip_cbsatitle_1) %>% arrange(desc(n)) %>%  print(n=50)

lists_orders_zip_hs_df %>% filter(hs_state_code %in% c('VA','MD','DC') & univ_id ==110680 & stu_black_01 ==1) %>% 
  mutate(
    univ_ordnum = str_c(univ_name,', univ_id=',univ_id,' ord_num=',ord_num, sep = "")
  ) %>%
  count(univ_ordnum)  %>% arrange(desc(n)) %>%  print(n=50)  

create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5')) %>% print(n=50)
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448427'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5')) %>% print(n=50)
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448440'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5')) %>% print(n=50)


2 University of California-San Diego, univ_id=110680 ord_num=479243  2248
3 University of California-San Diego, univ_id=110680 ord_num=479299  2033


# looking for:
# 'MD 3','MD 7','MD 2','MD 5'
# ASU order 448922: 'MD 3' [Y],'MD 7' [],'MD 2' [Y],'MD 5' []
# ASU never selects MD05 - PG county
# ASU never selects MD07 - city of baltimore

# VIRGINA
# Greater Alexandria: 'VA 1','VA 2'
# VA 1 = Arlington and Alexandria
# VA 2 = Fairfax county
# VA 3 = North central Virginia  


# ORDERS THAT DON'T CONDITION ON RACE [these aren't very helpful]

# (f) NR 2022 PSAT All 1300, order number 560104
# 2022 HS grad class; specific states; PSAT 1300 - 1520; A+ to B
create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('560104','479243'), eps_codes = c('MD 3','MD 7','MD 2','MD 5','VA 1','VA 2','VA 3','DC 1')) # 

create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('560104','479243'), eps_codes = c('MD 3','MD 7','MD 2','MD 5','VA 1','VA 2','VA 3','DC 1')) # 

create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('560104','479243'), eps_codes = c('MD 3','MD 7','MD 2','MD 5','VA 1','VA 2','VA 3','DC 1')) %>% print(n=50)

# NR 2021 PSAT All 1300, order number 479243
# 2021 grad class; PSAT 1300+; A+ to B

# NR 2019 SAT ALL = order # 395797
# 2019 grad class; specific states; SAT 1300+; A+ to B


create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('395797'), eps_codes = c('MD 3','MD 7','MD 2','MD 5','VA 1','VA 2','VA 3','DC 1')) # 

# NR 2020 select states 1400, order = 479299
# 2020 HS class;  SAT or maybe PSAT 1400+
create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('479299'), eps_codes = c('MD 3','MD 7','MD 2','MD 5','VA 1','VA 2','VA 3','DC 1')) # 

# 2020 NR All PSAT 1300; order # == 395794; [only a few obs]
# YYYY grad class; specific states PSAT 1300+

# 2021 NR PSAT all, order # == 395795

# ORDERS THAT CONDITION ON RACE

# NR 2022 PSAT URM (1100-1290), order number 560105
# 2022 HS grad class; specific states; PSAT 1100 - 1290; A+ to B GPA; ethnicity black, hispanic, AIAN [not NHPI...]

create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('560105'), eps_codes = c('MD 3','MD 7','MD 2','MD 5','VA 1','VA 2','VA 3','DC 1')) # 
# the first-gen table is interesting
create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('560105'), eps_codes = c('MD 3','MD 7','MD 2','MD 5','VA 1','VA 2','VA 3','DC 1')) # 

# results breaking out first-gen by race are not as compelling. not big enough sample sizes...
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('560105'), eps_codes = c('MD 3','MD 7','MD 2','MD 5','VA 1','VA 2','VA 3','DC 1')) %>% print(n=100) # 

# NR 2021 SAT URM 1200 - 1380, order number 560119
# 2021 HS grad class; specific states; SAT 1200 - 1380; ; ethnicity black, hispanic, AIAN [not NHPI...]

# START HERE MONDAY 1/6/2025

# NR 2021 PSAT URM 1200+
# 2021 HS hs grad class; specific states; PSAT 1200+; A+ to B GPA (guessing)

#1 University of California-San Diego, univ_id=110680 ord_num=479200   953
#2 University of California-San Diego, univ_id=110680 ord_num=560105   692 [got it]
#3 University of California-San Diego, univ_id=110680 ord_num=479301   262
#4 University of California-San Diego, univ_id=110680 ord_num=560119   184 [got it] 
#5 University of California-San Diego, univ_id=110680 ord_num=395797   157
#6 University of California-San Diego, univ_id=110680 ord_num=479299    99
#7 University of California-San Diego, univ_id=110680 ord_num=479243    94    

# Found these ord_nums in file NR_2020_AA_1200.xlsx : 479200 
#Found these ord_nums in file NR_2020_Select_States_1400.xlsx : 479299 
#Found these ord_nums in file NR_2021_PSAT_All_1300+_-_Redacted.csv : 479243 
#Found these ord_nums in file NR_2021_PSAT_URM_1200_-_Redacted.csv : 479301     

###################################
################################### CREATE GRAPHS
###################################

######### GRAPHS BASED ON OUTPUT OF RACE FUNCTION create_sim_eps_race_table()

#### NEXT STEPS: THURSDAY? MODIFY FUNCTION TO CREATE GRAPHS FOR FIRSTGEN.
    
create_sim_eps_race_graph <- function(data_graph, ord_nums_graph, eps_codes_graph) {
  
  # Row percent plots
  df_r <- create_sim_eps_race_table(data = data_graph, ord_nums = ord_nums_graph, eps_codes = eps_codes_graph)[[2]] %>%
    pivot_longer(
      cols = c(r_white, r_asian, r_black, r_hispanic, r_multi, r_aian, r_nhpi),
      names_to = "race",
      values_to = "value"
    ) %>% 
    # some data processing
    mutate(
      # Recode pivoted names to your desired labels
      race = dplyr::recode(race,
                           "r_white"    = "White, non-Hispanic",
                           "r_asian"    = "Asian, non-Hispanic",
                           "r_black"    = "Black, non-Hispanic",
                           "r_hispanic" = "Hispanic",
                           "r_multi"    = "two+ races, non-Hispanic",
                           "r_aian"     = "AIAN, non-Hispanic",
                           "r_nhpi"     = "NHPI, non-Hispanic"
      ),
      # Now apply factor() with those levels
      race = factor(race, levels = c(
        "White, non-Hispanic",
        "Asian, non-Hispanic",
        "Black, non-Hispanic",
        "Hispanic",
        "two+ races, non-Hispanic",
        "AIAN, non-Hispanic",
        "NHPI, non-Hispanic"
      )),
      eps_codename = forcats::fct_rev(factor(eps_codename)),
      eps_codename = fct_relevel(factor(eps_codename), "All", after = 0L)
    )
  
  # begin plotting
  plot_r <- df %>%  ggplot(aes(x = eps_codename, y = value, fill = race)) + 
    geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) +
    # Add labels, one per EPS code, placed at the right edge of the bar
    geom_text(
      data = df %>% distinct(eps_codename, race_known),
      aes(x = eps_codename, y = 1, 
          label = paste0("N=", formattable::comma(race_known, digits = 0))),
      #label = paste0("N=", race_known)),
      hjust = -0.1,      # shift text a bit to the right
      size = 3,
      inherit.aes = FALSE
    )  +   
    scale_y_continuous(labels = scales::percent_format(scale = 100)) + 
    labs(title = "Race Distribution Within Each EPS Code (Row %)",
         x = NULL, y = NULL) +
    theme_minimal() + coord_flip(clip = 'off')  
  
  # Column percent plots
  df_c <- create_sim_eps_race_table(data = data_graph, ord_nums = ord_nums_graph, eps_codes = eps_codes_graph)[[3]] %>%
  pivot_longer(
    cols = c(c_known,c_white, c_asian, c_black, c_hispanic, c_multi, c_aian, c_nhpi),
    names_to = "race",
    values_to = "value"
  ) %>% 
  # some data processing
  filter(eps_codename != 'All') %>% 
  mutate(
    # Recode pivoted names to your desired labels
    race = dplyr::recode(race,
                         "c_white"    = "White, non-Hispanic",
                         "c_asian"    = "Asian, non-Hispanic",
                         "c_black"    = "Black, non-Hispanic",
                         "c_hispanic" = "Hispanic",
                         "c_multi"    = "two+ races, non-Hispanic",
                         "c_aian"     = "AIAN, non-Hispanic",
                         "c_nhpi"     = "NHPI, non-Hispanic",
                         'c_known'    = "All (race known)"
    ),
    # Now apply factor() with those levels
    race = factor(race, levels = c(
      "All (race known)",      
      "White, non-Hispanic",
      "Asian, non-Hispanic",
      "Black, non-Hispanic",
      "Hispanic",
      "two+ races, non-Hispanic",
      "AIAN, non-Hispanic",
      "NHPI, non-Hispanic"

    )),
    race = forcats::fct_rev(factor(race)),
    eps_codename = forcats::fct_rev(factor(eps_codename)),
    #eps_codename = fct_relevel(factor(eps_codename), "All", after = 0L)
  ) 

  # Extract totals for each race group
  race_totals <- create_sim_eps_race_table(data = data_graph, ord_nums = ord_nums_graph, eps_codes = eps_codes_graph)[[1]] %>%
    filter(eps_codename == "All") %>%
    pivot_longer(
      cols = c(race_known, starts_with("stu_")),
      names_to = "race",
      values_to = "total_race"
    ) %>% select(-all,-eps_codename) %>% 
    mutate(
      race = dplyr::recode(race,
                           "stu_white"    = "White, non-Hispanic",
                           "stu_asian"    = "Asian, non-Hispanic",
                           "stu_black"    = "Black, non-Hispanic",
                           "stu_hispanic" = "Hispanic",
                           "stu_multi"    = "two+ races, non-Hispanic",
                           "stu_aian"     = "AIAN, non-Hispanic",
                           "stu_nhpi"     = "NHPI, non-Hispanic",
                           "race_known"   = "All (race known)"
      ),
      total_race = scales::comma(total_race, accuracy = 1)
    )
  
  # Combine totals with transformed data
  df_c <- df_c %>%
    left_join(race_totals, by = "race") %>% 
    mutate(
      # Reapply factor levels to race
      race = factor(race, levels = c(
        "All (race known)",      
        "White, non-Hispanic",
        "Asian, non-Hispanic",
        "Black, non-Hispanic",
        "Hispanic",
        "two+ races, non-Hispanic",
        "AIAN, non-Hispanic",
        "NHPI, non-Hispanic"
      )),    
      # Reverse levels for race_label
      race_label = factor(
        paste0(race, "\n(N = ", total_race, ")"),
        levels = rev(paste0(c(
          "All (race known)",      
          "White, non-Hispanic",
          "Asian, non-Hispanic",
          "Black, non-Hispanic",
          "Hispanic",
          "two+ races, non-Hispanic",
          "AIAN, non-Hispanic",
          "NHPI, non-Hispanic"
        ), "\n(N = ", race_totals$total_race, ")"))
      )
    )  
  #remove AIAN or NHPI
  df_c <- df_c %>% filter(race != 'AIAN, non-Hispanic', race != 'NHPI, non-Hispanic')

  # Plot with updated race labels
  plot_c <- df_c %>% ggplot(aes(x = race_label, y = value, fill = eps_codename)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = "Distribution of Each Race Across EPS Codes (Column %)",
      x = NULL,
      y = NULL,
      fill = "Geomarket"
    ) +
    scale_y_continuous(labels = percent_format(scale = 1), expand = expansion(mult = c(0, 0.1))) +
    scale_fill_discrete(guide = guide_legend(reverse = TRUE)) + # Reverse legend order
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
      axis.title.x = element_blank(),
      panel.grid.major.x = element_blank()
    ) # + coord_flip(clip = 'off')

  # create list object that stores both plots
  plots = list(
    plot_r = plot_r,
    plot_c = plot_c
  )
  return(plots)

}


# 1 487984  16926 # Illinois standard 2020; ordered 7/19/2019; HS class 2020, 2021; IL; SAT 1020-1150;  GPA A+ to B-
# 5 488035  12842 # Illinois HS 2020; ordered 7/19/2019; HS class 2020, 2021; SAT 1160 - 1300; GPA A+ to B-
# 9 488053   7259 # Illinois GPPA 2020 (no cf);ordered 7/19/2019; HS class 2020, 2021; SAT 1310-1600; GPA A+ to B-
chi_plot <- create_sim_eps_race_graph(lists_orders_zip_hs_df_sf, c('487984'), chi_eps_codes) # 
chi_plot[[1]]
chi_plot[[2]]

plot1 + plot2 + plot_layout(ncol = 1)

(chi_plot[[1]] / chi_plot[[2]]) + plot_layout(heights = c(1, 2))


create_sim_eps_race_graph(lists_orders_zip_hs_df_sf, c('488035'), chi_eps_codes) # 
create_sim_eps_race_graph(lists_orders_zip_hs_df_sf, c('488053'), chi_eps_codes) # 

plot2 <- create_sim_eps_race_graph(data_graph = lists_orders_zip_hs_df_sf, ord_nums_graph = c('448922'), eps_codes_graph = philly_eps_codes)

plot2 <- create_sim_eps_race_graph(lists_orders_zip_hs_df_sf, c('487984'), chi_eps_codes) # 
plot2

plot1
plot2

library(patchwork)
class(plot1)
class(plot2)
plot1 / plot2




create_sim_eps_race_graph(data_graph = lists_orders_zip_hs_df_sf, ord_nums_graph = c('448427','448440'), eps_codes_graph = philly_eps_codes)

create_sim_eps_race_graph(data_graph = lists_orders_zip_hs_df_sf, ord_nums_graph = c('448922'), eps_codes_graph = dallas_eps_codes)
create_sim_eps_race_graph(data_graph = lists_orders_zip_hs_df_sf, ord_nums_graph = c('448427','448440'), eps_codes_graph = dallas_eps_codes)





create_sim_eps_race_graph(data_graph = lists_orders_zip_hs_df_sf, ord_nums_graph = c('366935'), eps_codes_graph = bay_eps_codes) # PSAT 1110 - 1210
create_sim_eps_race_graph(data_graph = lists_orders_zip_hs_df_sf, ord_nums_graph = c('366934'), eps_codes_graph = bay_eps_codes) # PSAT 1220-1290    
create_sim_eps_race_graph(data_graph = lists_orders_zip_hs_df_sf, ord_nums_graph = c('366932'), eps_codes_graph = bay_eps_codes) # PSAT 1300 - 1520

### this is the graph for the count plot when it is not in a function

# [from count table] total students by EPS code, stacked by Race,

create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5'))[[1]] %>%
  pivot_longer(
    cols = c(stu_white, stu_asian, stu_black, stu_hispanic, stu_multi, stu_aian, stu_nhpi),
    names_to = "race",
    values_to = "count"
  ) %>% filter(eps_codename != "All") %>% ggplot(aes(x = eps_codename, y = count, fill = race)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Students by EPS Code (Stacked by Race)",
       x = "EPS Code", y = "Number of Students") +
  theme_minimal() + coord_flip(clip = 'off')

### this is the graph for row percent plot when it is not in a function


# from row percent table. goal: how known-race students are distributed across race categories
df <- create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = philly_eps_codes)[[2]] %>%
  pivot_longer(
    cols = c(r_white, r_asian, r_black, r_hispanic, r_multi, r_aian, r_nhpi),
    names_to = "race",
    values_to = "value"
  ) %>% 
  # some data processing
  mutate(
    # Recode pivoted names to your desired labels
    race = dplyr::recode(race,
                         "r_white"    = "White, non-Hispanic",
                         "r_asian"    = "Asian, non-Hispanic",
                         "r_black"    = "Black, non-Hispanic",
                         "r_hispanic" = "Hispanic",
                         "r_multi"    = "two+ races, non-Hispanic",
                         "r_aian"     = "AIAN, non-Hispanic",
                         "r_nhpi"     = "NHPI, non-Hispanic"
    ),
    # Now apply factor() with those levels
    race = factor(race, levels = c(
      "White, non-Hispanic",
      "Asian, non-Hispanic",
      "Black, non-Hispanic",
      "Hispanic",
      "two+ races, non-Hispanic",
      "AIAN, non-Hispanic",
      "NHPI, non-Hispanic"
    )),
    eps_codename = forcats::fct_rev(factor(eps_codename)),
    eps_codename = fct_relevel(factor(eps_codename), "All", after = 0L)
  )

# begin plotting
plot1 <- df %>%  ggplot(aes(x = eps_codename, y = value, fill = race)) + 
  geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) +
  # Add labels, one per EPS code, placed at the right edge of the bar
  geom_text(
    data = df %>% distinct(eps_codename, race_known),
    aes(x = eps_codename, y = 1, 
        label = paste0("N=", formattable::comma(race_known, digits = 0))),
    #label = paste0("N=", race_known)),
    hjust = -0.1,      # shift text a bit to the right
    size = 3,
    inherit.aes = FALSE
  )  +   
  scale_y_continuous(labels = scales::percent_format(scale = 100)) + 
  labs(title = "Race Distribution Within Each EPS Code (Row %)",
       x = NULL, y = NULL) +
  theme_minimal() + coord_flip(clip = 'off')