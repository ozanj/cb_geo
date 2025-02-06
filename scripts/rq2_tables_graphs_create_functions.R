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
library(ggbreak) # custom breaks
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

#getwd()
#source(file = file.path('scripts', 'create_stu_list.R'))
#getwd()


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
#create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('448922'), eps_codes = c('PA 1','PA 2','PA 3','PA 4','PA 5'))

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
#stu_race_cb                                 n
#<dbl+lbl>                               <int>
#1  0 [no response]                       135083
#2  1 [American Indian/Alaska Native]      19196
#3  2 [Asian]                             656927
#4  3 [Black/African American]            182284
#5  4 [Hispanic/Latino]                   758051
#6  8 [Native Hawaiian/Pacific Islander]    4497
#7  9 [white]                            1706890
#8 10 [other]                               1561
#9 12 [two or more races, non-Hispanic]   183166
#10 NA                                      17800

create_sim_eps_race_firstgen_table <- function(data, ord_nums, eps_codes, exclude_race = c(1,8)) {
  
  # First, create the main processed data frame
  # <- lists_orders_zip_hs_df_sf %>%  as_tibble() %>% filter(ord_num %in% c('448922'), eps %in% c('PA 1','PA 2','PA 3','PA 4','PA 5')) %>%
  df_work <- data %>% 
    as_tibble() %>% 
    filter(ord_num %in% ord_nums, eps %in% eps_codes) %>%
    # get rid of obs where race is not known or first-generation status is not known
    filter(!is.na(stu_race_cb), stu_race_cb != 0, stu_first_gen != 4) %>% 
    filter(stu_race_cb %in% c(1,2,3,4,8,9,12)) %>% # only some racial groups. excludes: no-response; other; and NA
    filter(!stu_race_cb %in% exclude_race) %>% 
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
        "two or more races, non-Hispanic",
        'American Indian/Alaska Native',
        'Native Hawaiian/Pacific Islander'
      )
    ) %>%
    # Then arrange by stu_race_cb and eps_codename
    arrange(stu_race_cb, eps_codename) 
  
  #final_df %>% print(n=60)
  
  return(final_df)
  
}

# philly metro area
# order 448922: PSAT 1070 - 1180; order 448427: PSAT 1190 - 1260; order 448440: PSAT 1270 - 1520

#create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('487984'), eps_codes = chi_eps_codes) %>% print(n=70) # 
#create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('487984'), eps_codes = chi_eps_codes, exclude_race = '') %>% print(n=70) # 
#create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('487984'), eps_codes = chi_eps_codes, exclude_race = c(1,8,12)) %>% print(n=70) # 
#lists_orders_zip_hs_df_sf %>% as_tibble() %>% count(stu_race_cb)

###################################
################################### CREATE GRAPH FOR RQ2, SEPARATE GRAPH FOR RACE AND FOR FIRST-GEN STATUS
###################################

######### GRAPHS BASED ON OUTPUT OF RACE FUNCTION create_sim_eps_race_table()

#### NEXT STEPS: THURSDAY? MODIFY FUNCTION TO CREATE GRAPHS FOR FIRSTGEN.

##----------------------------------------------------------------
## 1) Reusable: map raw race column names to final race labels
##----------------------------------------------------------------


# FIXES TO MAKE
  # write loop that runs functions for each combination of metro and order number
  # save graph to disk
  # think about how you will combine graphs or not
    # same graph [e.g., only row percent graph], but different order [test scores]?
    # different graphs [row_pct and col_pct graph], but same order


##------------------------------------------
## A) Setup: define factor level orders & recode helpers
##------------------------------------------

# We'll define the factor levels in a "human" top→bottom order.
# Then, in the code where we build the plots, we'll apply fct_rev()
# if we want to invert them for the flipped coordinates.

# Race column factor levels (top→bottom)
# e.g. "All (race known)" at top, then "White, ...", down to "NHPI, non-Hispanic".
race_col_levels <- c(
  "All (race known)",
  "White, non-Hispanic",
  "Asian, non-Hispanic",
  "Black, non-Hispanic",
  "Hispanic",
  "Two+, non-Hispanic",
  "AIAN, non-Hispanic",
  "NHPI, non-Hispanic"
)

# Race row factor levels (top→bottom)
# e.g. "White" at top, then "Asian", etc. down to "NHPI".
race_row_levels <- c(
  "White, non-Hispanic",
  "Asian, non-Hispanic",
  "Black, non-Hispanic",
  "Hispanic",
  "Two+, non-Hispanic",
  "AIAN, non-Hispanic",
  "NHPI, non-Hispanic"
)

# First-gen column factor levels (top→bottom)
# e.g. "All (first-gen known)" at top, then "No college", etc.
firstgen_col_levels <- c(
  "All (first-gen known)",
  "No college",
  "Some college",
  "Not first-gen"
)

# First-gen row factor levels (top→bottom)
# e.g. "No college" at the top, then "Some college", then "Not first-gen".
firstgen_row_levels <- c(
  "No college",
  "Some college",
  "Not first-gen"
)

# Helper function that moves "All" to the top in the row plot,
# keeping original order for the other EPS codes.
move_all_to_top_in_row <- function(vec) {
  # We'll reverse it so that "All" becomes first in the factor,
  # which ends up at top once we do fct_rev() or coordinate flipping.
  # Or just reorder explicitly: "All" => front, others after.
  
  # For your code: you wanted "All" to appear at the top in the flipped chart,
  # which is the last factor level if you do *not* reverse. 
  # But if you do want to define it in a top→bottom sense, you might just do:
  
  # 1) Remove "All"
  remainder <- setdiff(vec, "All")
  # 2) Put "All" as the first item in top→bottom sense
  c("All", remainder)
}

## -- recode helpers as you had them --
recode_race <- function(x) {
  dplyr::recode(
    x,
    "r_white"    = "White, non-Hispanic",
    "r_asian"    = "Asian, non-Hispanic",
    "r_black"    = "Black, non-Hispanic",
    "r_hispanic" = "Hispanic",
    "r_multi"    = "Two+, non-Hispanic",
    "r_aian"     = "AIAN, non-Hispanic",
    "r_nhpi"     = "NHPI, non-Hispanic",
    
    "c_white"    = "White, non-Hispanic",
    "c_asian"    = "Asian, non-Hispanic",
    "c_black"    = "Black, non-Hispanic",
    "c_hispanic" = "Hispanic",
    "c_multi"    = "Two+, non-Hispanic",
    "c_aian"     = "AIAN, non-Hispanic",
    "c_nhpi"     = "NHPI, non-Hispanic",
    "c_known"    = "All (race known)",
    
    "stu_white"    = "White, non-Hispanic",
    "stu_asian"    = "Asian, non-Hispanic",
    "stu_black"    = "Black, non-Hispanic",
    "stu_hispanic" = "Hispanic",
    "stu_multi"    = "Two+, non-Hispanic",
    "stu_aian"     = "AIAN, non-Hispanic",
    "stu_nhpi"     = "NHPI, non-Hispanic",
    "race_known"   = "All (race known)"
  )
}

recode_firstgen <- function(x) {
  dplyr::recode(
    x,
    "r_no_col"     = "No college",
    "r_some_col"   = "Some college",
    "r_not_first"  = "Not first-gen",
    
    "c_no_col"     = "No college",
    "c_some_col"   = "Some college",
    "c_not_first"  = "Not first-gen",
    "c_known"      = "All (first-gen known)",
    
    "stu_no_col"   = "No college",
    "stu_some_col" = "Some college",
    "stu_not_first"= "Not first-gen",
    "stu_known"    = "All (first-gen known)"
  )
}

##------------------------------------------
## B) Main function with Approach A (top→bottom + fct_rev)
##------------------------------------------

create_sim_eps_graph <- function(data_graph,
                                 ord_nums_graph,
                                 eps_codes_graph,
                                 variable = c("race", "firstgen"),
                                 title = "") {
  
  # Match user input to "race" or "firstgen"
  variable <- match.arg(variable)
  
  if (variable == "race") {
    table_list <- create_sim_eps_race_table(
      data      = data_graph,
      ord_nums  = ord_nums_graph,
      eps_codes = eps_codes_graph
    )
    
    # ----------------------------
    # Build the row data (df_r)
    # ----------------------------
    df_r <- table_list[[2]] %>%
      tidyr::pivot_longer(
        cols      = c(r_white, r_asian, r_black, r_hispanic, r_multi, r_aian, r_nhpi),
        names_to  = "group",
        values_to = "value"
      ) %>%
      dplyr::mutate(
        group = recode_race(group),
        group = factor(group, levels = race_row_levels)
      )
    
    # Reorder eps_codename so that "All" appears at the top (for flipped coordinates)
    all_eps <- unique(df_r$eps_codename)
    new_eps <- move_all_to_top_in_row(all_eps)
    df_r <- df_r %>%
      dplyr::mutate(
        eps_codename = factor(eps_codename, levels = new_eps)
      )
    
    # ----------------------------
    # Build the column data (df_c)
    # ----------------------------
    df_c <- table_list[[3]] %>%
      tidyr::pivot_longer(
        cols      = c(c_known, c_white, c_asian, c_black, c_hispanic, c_multi, c_aian, c_nhpi),
        names_to  = "group",
        values_to = "value"
      ) %>%
      dplyr::filter(eps_codename != "All") %>%
      dplyr::mutate(
        group       = recode_race(group),
        group       = factor(group, levels = race_col_levels),
        eps_codename = factor(eps_codename)
      ) %>%
      dplyr::mutate(
        group       = forcats::fct_rev(group),
        eps_codename = forcats::fct_rev(eps_codename)
      )
    
    totals <- table_list[[1]] %>%
      dplyr::filter(eps_codename == "All") %>%
      tidyr::pivot_longer(
        cols      = c(race_known, dplyr::starts_with("stu_")),
        names_to  = "group",
        values_to = "total_group"
      ) %>%
      dplyr::select(-all, -eps_codename) %>%
      dplyr::mutate(
        group       = recode_race(group),
        total_group = scales::comma(total_group, accuracy = 1)
      )
    
    df_c <- df_c %>%
      dplyr::left_join(totals, by = "group") %>%
      dplyr::mutate(
        group       = factor(group, levels = rev(race_col_levels)),
        group_label = factor(
          paste0(group, "\n(N = ", total_group, ")"),
          levels = rev(
            paste0(race_col_levels, "\n(N = ", totals$total_group, ")")
          )
        )
      ) %>%
      dplyr::filter(!group %in% c("AIAN, non-Hispanic", "NHPI, non-Hispanic",'Two+, non-Hispanic'))
    
    # Titles and legend labels
    row_plot_title   <- "Race Distribution Within Each EPS Code (Row %)"
    col_plot_title   <- "Distribution of Each Race Across EPS Codes (Column %)"
    fill_legend      <- "Geomarket"        # for the column plot
    row_legend_title <- "Race/ethnicity"   # for the row plot
    
  } else {  # variable == "firstgen"
    table_list <- create_sim_eps_firstgen_table(
      data      = data_graph,
      ord_nums  = ord_nums_graph,
      eps_codes = eps_codes_graph
    )
    
    df_r <- table_list[[2]] %>%
      tidyr::pivot_longer(
        cols      = c(r_no_col, r_some_col, r_not_first),
        names_to  = "group",
        values_to = "value"
      ) %>%
      dplyr::mutate(
        group = recode_firstgen(group),
        group = factor(group, levels = firstgen_row_levels)
      )
    
    all_eps <- unique(df_r$eps_codename)
    new_eps <- move_all_to_top_in_row(all_eps)
    df_r <- df_r %>%
      dplyr::mutate(
        eps_codename = factor(eps_codename, levels = new_eps)
      )
    
    df_c <- table_list[[3]] %>%
      tidyr::pivot_longer(
        cols      = c(c_known, c_no_col, c_some_col, c_not_first),
        names_to  = "group",
        values_to = "value"
      ) %>%
      dplyr::filter(eps_codename != "All") %>%
      dplyr::mutate(
        group       = recode_firstgen(group),
        group       = factor(group, levels = firstgen_col_levels),
        eps_codename = factor(eps_codename)
      ) %>%
      dplyr::mutate(
        group       = forcats::fct_rev(group),
        eps_codename = forcats::fct_rev(eps_codename)
      )
    
    totals <- table_list[[1]] %>%
      dplyr::filter(eps_codename == "All") %>%
      tidyr::pivot_longer(
        cols      = c(stu_known, stu_no_col, stu_some_col, stu_not_first),
        names_to  = "group",
        values_to = "total_group"
      ) %>%
      dplyr::select(-eps_codename) %>%
      dplyr::mutate(
        group       = recode_firstgen(group),
        total_group = scales::comma(total_group, accuracy = 1)
      )
    
    df_c <- df_c %>%
      dplyr::left_join(totals, by = "group") %>%
      dplyr::mutate(
        group    = factor(group, levels = rev(firstgen_col_levels)),
        label_str = paste0(group, "\n(N = ", total_group, ")")
      )
    
    label_levels <- rev(
      sapply(firstgen_col_levels, function(g) {
        val <- totals %>%
          dplyr::filter(group == g) %>%
          dplyr::pull(total_group)
        paste0(g, "\n(N = ", val, ")")
      })
    )
    
    df_c <- df_c %>%
      dplyr::mutate(
        group_label = factor(label_str, levels = label_levels)
      )
    
    row_plot_title   <- "First-Gen Distribution Within Each EPS Code (Row %)"
    col_plot_title   <- "Distribution of First-Gen Groups Across EPS Codes (Column %)"
    fill_legend      <- "Geomarket"          # for the column plot
    row_legend_title <- "Parental education" # for the row plot
  }
  
  # (A) Append the user-supplied title to each plot title if provided
  if (!is.null(title) && nzchar(title)) {
    row_plot_title <- paste0(title)
    col_plot_title <- paste0(title)
  }
  
  # Determine which column contains the overall sample size.
  known_col <- if (variable == "race") "race_known" else "stu_known"
  
  # ----------------------------------------------------------------
  # (B) Create a new label that combines the EPS code and its sample size
  # ----------------------------------------------------------------
  df_r <- df_r %>%
    dplyr::group_by(eps_codename) %>%
    dplyr::mutate(
      eps_label = paste0(as.character(eps_codename),
                         " (n=", formattable::comma(first(.data[[known_col]]), digits = 0), ")")
    ) %>%
    dplyr::ungroup()
  
  # ----------------------------------------------------------------
  # (C) Build the Row-Percent Plot using the new eps_label
  # ----------------------------------------------------------------
  plot_r <- df_r %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x    = forcats::fct_rev(eps_label),
        y    = value,
        fill = group
      )
    ) +
    ggplot2::geom_bar(stat = "identity", 
                      position = ggplot2::position_fill(reverse = TRUE)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    ggplot2::labs(
      title = row_plot_title,
      x     = NULL,
      y     = NULL,
      fill  = row_legend_title
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 20, r = 0, b = 20, l = 20),  # Lower the right margin (r=2) so the plot takes up more horizontal space. Increase 'r' to add more white space.
      legend.box.spacing = grid::unit(0, "line"),  # Reduce spacing between the plot and the legend. Increase this value for a larger gap.
      legend.text        = ggplot2::element_text(size = 14),
      legend.title       = ggplot2::element_text(size = 14),
      axis.text.y        = ggplot2::element_text(size = 13),
      axis.text.x        = ggplot2::element_text(size = 13),
      strip.text         = ggplot2::element_text(size = 14),
      plot.title         = ggplot2::element_text(size = 16, face = "bold")
    ) +
    ggplot2::coord_flip(clip = "off")
  
  # ----------------------------------------------------------------
  # (D) Build the Column-Percent Plot (unchanged)
  # ----------------------------------------------------------------
  plot_c <- df_c %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x    = group_label,
        y    = value,
        fill = eps_codename
      )
    ) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::labs(
      title = col_plot_title,
      x     = NULL,
      y     = NULL,
      fill  = fill_legend
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(scale = 1),
      expand = ggplot2::expansion(mult = c(0, 0.1))
    ) +
    ggplot2::scale_fill_discrete(
      guide = ggplot2::guide_legend(reverse = TRUE)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.text       = ggplot2::element_text(size = 14),
      legend.title      = ggplot2::element_text(size = 14),
      axis.text.x       = ggplot2::element_text(size = 13),
      axis.text.y       = ggplot2::element_text(size = 13),
      strip.text        = ggplot2::element_text(size = 14),
      axis.text.x.top   = ggplot2::element_text(size = 13),
      plot.title        = ggplot2::element_text(size = 16, face = "bold"),
      panel.grid.major.x = ggplot2::element_blank()
    ) +
    ggplot2::coord_flip(clip = "off")
  
  # ----------------------------------------------------------------
  # (E) Return both plots as a list
  # ----------------------------------------------------------------
  list(plot_r = plot_r, plot_c = plot_c)
}

# 1 487984  16926 # Illinois standard 2020; ordered 7/19/2019; HS class 2020, 2021; IL; SAT 1020-1150;  GPA A+ to B-
# 5 488035  12842 # Illinois HS 2020; ordered 7/19/2019; HS class 2020, 2021; SAT 1160 - 1300; GPA A+ to B-
# 9 488053   7259 # Illinois GPPA 2020 (no cf);ordered 7/19/2019; HS class 2020, 2021; SAT 1310-1600; GPA A+ to B-

# philly metro area
# order 448922: PSAT 1070 - 1180; order 448427: PSAT 1190 - 1260; order 448440: PSAT 1270 - 1520

###################################
################################### CREATE GRAPH FOR RQ2 RACE X SES
###################################


# These graphs based on the table-creating function: 
  #create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('487984'), eps_codes = chi_eps_codes) %>% print(n=70) # 
  #create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('488035'), eps_codes = chi_eps_codes) %>% print(n=70) # 
  #create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('488053','488035'), eps_codes = chi_eps_codes) %>% print(n=70) # 

# 1) Define the custom orders/labels for group
custom_group_order <- c("all","row_no_col", "row_some_col", "row_not_first_gen")
custom_group_labels <- c(
  "all"               = "All",
  "row_no_col"        = "No college",
  "row_some_col"      = "Some college",
  "row_not_first_gen" = "Not first-gen"
)

# 2) Define the custom orders/labels for race
custom_race_labels <- c(
  "All"     = "All",  
  "white"   = "White, non-Hispanic",
  "Asian"   = "Asian, non-Hispanic",
  "Black/African American" = "Black, non-Hispanic",
  "Hispanic/Latino"        = "Hispanic",
  "two or more races, non-Hispanic" = "Multi-race, non-Hispanic",
  "American Indian/Alaska Native" = "AIAN, non-Hispanic",
  "Native Hawaiian/Pacific Islander" = "NHPI, non-Hispanic"
  # optionally add more if needed
)


create_race_by_firstgen_graph <- function(data_graph,
                                          ord_nums_graph,
                                          eps_codes_graph,
                                          metro_name = '',
                                          exclude_race_graph = c(1,8,12),
                                          title_suf = '') {

  # 3) Create & pivot the table
  df <- create_sim_eps_race_firstgen_table(
    data      = data_graph, 
    ord_nums  = ord_nums_graph, 
    eps_codes = eps_codes_graph,
    exclude_race = exclude_race_graph
  ) %>% 
    pivot_longer(
      cols      = c(all, row_no_col, row_some_col, row_not_first_gen),
      names_to  = "group",
      values_to = "value"
    ) %>%
    mutate(
      # Relabel race
      stu_race_cb = factor(
        stu_race_cb, 
        levels = names(custom_race_labels), 
        labels = custom_race_labels
      ),      
      # Relabel group
      group = factor(
        group, 
        levels = custom_group_order, 
        labels = custom_group_labels
      ),
      # Reverse the order of eps_codename globally
      eps_codename = factor(
        eps_codename,
        levels = rev(unique(eps_codename))  # reverse the unique order
      )
    )
  
  df %>% print(n=250)
  
  # 4) Split out the subgroup vs. total N
  df_plot <- df %>%
    filter(group != "All")
  
  df_totals <- df %>%
    filter(group == "All") %>%
    distinct(stu_race_cb, eps_codename, value) %>%  # <--- ensure no duplicates
    rename(total_n = value)
  
  # 5) Join the total N onto the plotting data (and remove any exact duplicates)
  df_plot <- df_plot %>%
    left_join(df_totals, by = c("stu_race_cb", "eps_codename")) %>%
    distinct()
  
  # 6) Create a new label (N=xxxx) & factor it once
  df_plot <- df_plot %>%
    mutate(
      # character label with “(N=...)”
      eps_label = str_c(
        as.character(eps_codename), 
        " (n=", format(total_n, big.mark = ",", trim = TRUE), ")"
      ),
    ) %>%
    mutate(
      # single factor across the entire data
      eps_label_factor = factor(
        eps_label,
        levels = rev(unique(eps_label))
      )
    )
  
  # 7) Plot, with "free_y" so each facet only shows relevant codes
  plot <- ggplot(df_plot, aes(
    x    = eps_label_factor, 
    y    = value,
    fill = group
  )) +
    geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) +
    coord_flip(clip = "off") +
    facet_grid(
      rows = vars(stu_race_cb), 
      switch = "y",
      scales = "free_y",          # <--- each race can drop labels it doesn't use
      space  = "free_y"     # each facet’s height will scale to number of bars
    ) +
    scale_x_discrete(drop = TRUE) +  # <--- drop unused factor levels
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = NULL,
      x     = NULL,
      y     = NULL,
      fill  = "Parental education"  # <-- Set legend title here
    ) +
    theme_minimal() +
    theme(
      strip.text.y.left = element_text(size = 12, face = "plain", angle = 0, hjust = 0), # these are the labels for race/ethnicity that appear to the left of the labels for geomarket
      strip.placement   = "outside",
      panel.spacing     = unit(0.2, "lines"),
      axis.text.y       = element_text(size = 10.5), # these are the labels for geomarket name that appear to the right of the labels for race ethnicity
      plot.title        = element_text(hjust = 0.5),
      legend.position   = "right",
      legend.key.size   = unit(0.8, "lines"),
      legend.title      = element_text(size = 12, face = "bold"),
      legend.text       = element_text(size = 12),
      axis.text.x = element_text(size = 12) # Controls the size of "0% 25% 50% 75% 100%" labels  
    )

  # create strings for file names
  
  # Read the caption (if missing, use a default)
  if (is.na(metro_name)) { # if metro name not specified use the first part of the eps_codes object
    
    metro <- str_replace(string = deparse(substitute(eps_codes_graph)), pattern = '_eps_codes', replacement = '')
    
  } else if (!is.na(metro_name)) { # if metro name specified, create version of metro name suitable for naming files
    metro <- str_replace_all(string = metro_name, pattern = ' ', replacement = '_')
  }

    
  
  plot_name <- str_c('rq2',metro,'order',str_c(ord_nums_graph, collapse = '_'),'race_by_firstgen', sep = '_')
  
  writeLines(plot_name)
  
  # Define the figure title
  figure_title <- str_c(
    "First-generation status by race for ", str_to_title(metro_name), " area Geomarkets, ", title_suf
  )
  
  writeLines(figure_title)
  
  # Save the title to a .txt file
  writeLines(figure_title, file.path(graphs_dir,'rq2', str_c(plot_name, '_title.txt')))  
  
  # Save plot to file
  ggsave(
    filename = file.path(graphs_dir,'rq2', str_c(plot_name, '.png', sep = '')),
    plot = plot,
    width = 14,
    height = 8,
    bg = 'white'
  )
  
  # 2) Create the text you want to store [REVISE NOTE TEXT LATER!]
  note_text <- c(
    "Figure Notes:",
    "- Median income is shown in thousands ($k).",
    "- '% in poverty' is the share of individuals below the federal poverty line.",
    "- Data sources: US Census, etc."
  )
  
  # 3) Write that text to a file
  writeLines(note_text, file.path(graphs_dir,'rq2', str_c(plot_name, '_note.txt')))  
  
  # Return the plot
  return(plot)
  
}

#create_race_by_firstgen_graph(data_graph,ord_nums_graph,eps_codes_graph,metro_name = '',exclude_race_graph = c(1,8,12),title_suf = '')
  
create_race_by_firstgen_graph(lists_orders_zip_hs_df_sf, c('487984'), chi_eps_codes, 'chicago', exclude_race = c(1,8,12), title_suf = ', SAT score 1020 - 1150')

#stu_race_cb                                 n
#1  0 [no response]                       135083
#2  1 [American Indian/Alaska Native]      19196
#3  2 [Asian]                             656927
#4  3 [Black/African American]            182284
#5  4 [Hispanic/Latino]                   758051
#6  8 [Native Hawaiian/Pacific Islander]    4497
#7  9 [white]                            1706890
#8 10 [other]                               1561
#9 12 [two or more races, non-Hispanic]   183166
#10 NA                                      17800

# 1 487984  16926 # Illinois standard 2020; ordered 7/19/2019; HS class 2020, 2021; IL; SAT 1020-1150;  GPA A+ to B-
# 5 488035  12842 # Illinois HS 2020; ordered 7/19/2019; HS class 2020, 2021; SAT 1160 - 1300; GPA A+ to B-
# 9 488053   7259 # Illinois GPPA 2020 (no cf);ordered 7/19/2019; HS class 2020, 2021; SAT 1310-1600; GPA A+ to B-
#create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('487984'), eps_codes = chi_eps_codes) %>% print(n=50) # 

# philly metro area
# order 448922: PSAT 1070 - 1180; order 448427: PSAT 1190 - 1260; order 448440: PSAT 1270 - 1520


#create_race_by_firstgen_graph(lists_orders_zip_hs_df_sf, c('487984'), chi_eps_codes, 'Chicago', exclude_race = c(1,8,12), title_suf = ', SAT score 1020 - 1150')
#create_race_by_firstgen_graph(lists_orders_zip_hs_df_sf, c('488035','488053'), chi_eps_codes, 'Chicago', exclude_race = c(1,8,12), title_suf = ', SAT score 1160 - 1600')

#create_race_by_firstgen_graph(lists_orders_zip_hs_df_sf, c('448922'), philly_eps_codes, 'Philadelphia', exclude_race = c(1,8,12), title_suf = ', SAT score 1070 - 1180')
