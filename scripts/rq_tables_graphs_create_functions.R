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
# FIRSTGEN TABLE FUNCTION
# Default: combines "No college" + "Some college" into "First-gen"
# Optional: set firstgen_detail = "disaggregated" to keep old categories

create_sim_eps_firstgen_table <- function(
    data,
    ord_nums,
    eps_codes,
    firstgen_detail = c("collapsed", "disaggregated")
) {
  
  firstgen_detail <- match.arg(firstgen_detail)
  
  df <- data %>%
    as_tibble() %>%
    filter(ord_num %in% ord_nums, eps %in% eps_codes) %>%
    mutate(
      eps_codename = str_c(eps, eps_name, sep = ", "),
      
      # Original College Board categories
      # 1 = No college
      # 2 = Some college
      # 3 = Not first-gen
      # 4 = Unknown
      stu_no_col_01 = if_else(stu_first_gen == 1, 1, 0),
      stu_some_col_01 = if_else(stu_first_gen == 2, 1, 0),
      stu_first_gen_01 = if_else(stu_first_gen %in% c(1, 2), 1, 0),
      stu_not_first_gen_01 = if_else(stu_first_gen == 3, 1, 0),
      stu_unknown_01 = if_else(stu_first_gen == 4, 1, 0)
    ) %>% 
    group_by(eps_codename) %>% 
    summarize(
      stu_all = n(),
      stu_no_col = sum(stu_no_col_01, na.rm = TRUE),
      stu_some_col = sum(stu_some_col_01, na.rm = TRUE),
      stu_first_gen = sum(stu_first_gen_01, na.rm = TRUE),
      stu_not_first = sum(stu_not_first_gen_01, na.rm = TRUE),
      stu_unknown = sum(stu_unknown_01, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    mutate(
      stu_known = rowSums(select(., stu_first_gen, stu_not_first)),
      stu_sum_calc = rowSums(select(., stu_first_gen, stu_not_first, stu_unknown))
    )
  
  # Create row that is sum of all geomarkets
  df_sum <- df %>% 
    summarize(
      stu_all = sum(stu_all, na.rm = TRUE),
      stu_sum_calc = sum(stu_sum_calc, na.rm = TRUE),
      stu_known = sum(stu_known, na.rm = TRUE),
      stu_no_col = sum(stu_no_col, na.rm = TRUE),
      stu_some_col = sum(stu_some_col, na.rm = TRUE),
      stu_first_gen = sum(stu_first_gen, na.rm = TRUE),
      stu_not_first = sum(stu_not_first, na.rm = TRUE),
      stu_unknown = sum(stu_unknown, na.rm = TRUE)
    ) %>% 
    mutate(eps_codename = "All")
  
  df <- bind_rows(df, df_sum)
  
  if (firstgen_detail == "collapsed") {
    
    # Row percentages: first-gen composition within each Geomarket
    df_r <- df %>% 
      mutate(
        r_first_gen = stu_first_gen / stu_known * 100,
        r_not_first = stu_not_first / stu_known * 100,
        r_known = stu_known / stu_known * 100
      )
    
    # Column percentages: where each first-gen group comes from
    df_c <- df %>% 
      filter(eps_codename != "All") %>%         
      mutate(
        c_first_gen = stu_first_gen / sum(stu_first_gen, na.rm = TRUE) * 100,
        c_not_first = stu_not_first / sum(stu_not_first, na.rm = TRUE) * 100,
        c_known = stu_known / sum(stu_known, na.rm = TRUE) * 100
      )
    
    # Create new "All" row for column table
    df_c_sum <- df_c %>% 
      summarize(
        stu_all = sum(stu_all, na.rm = TRUE),
        stu_known = sum(stu_known, na.rm = TRUE),
        c_known = sum(c_known, na.rm = TRUE),
        c_first_gen = sum(c_first_gen, na.rm = TRUE),
        c_not_first = sum(c_not_first, na.rm = TRUE)
      ) %>% 
      mutate(eps_codename = "All")
    
    df_c <- bind_rows(df_c, df_c_sum)
    
    # Count table
    table1 <- df %>% 
      select(
        eps_codename,
        stu_all,
        stu_known,
        stu_unknown,
        stu_first_gen,
        stu_not_first
      )
    
    # Row percentage table
    table2 <- df_r %>% 
      select(
        eps_codename,
        stu_all,
        stu_known,
        r_known,
        r_first_gen,
        r_not_first
      )
    
    # Column percentage table
    table3 <- df_c %>%
      select(
        eps_codename,
        stu_all,
        stu_known,
        c_known,
        c_first_gen,
        c_not_first
      )
    
  } else if (firstgen_detail == "disaggregated") {
    
    # Row percentages: original no-college / some-college / not-first-gen categories
    df_r <- df %>% 
      mutate(
        r_no_col = stu_no_col / stu_known * 100,
        r_some_col = stu_some_col / stu_known * 100,
        r_not_first = stu_not_first / stu_known * 100,
        r_known = stu_known / stu_known * 100
      )
    
    # Column percentages: original no-college / some-college / not-first-gen categories
    df_c <- df %>% 
      filter(eps_codename != "All") %>%         
      mutate(
        c_no_col = stu_no_col / sum(stu_no_col, na.rm = TRUE) * 100,
        c_some_col = stu_some_col / sum(stu_some_col, na.rm = TRUE) * 100,
        c_not_first = stu_not_first / sum(stu_not_first, na.rm = TRUE) * 100,
        c_known = stu_known / sum(stu_known, na.rm = TRUE) * 100
      )
    
    # Create new "All" row for column table
    df_c_sum <- df_c %>% 
      summarize(
        stu_all = sum(stu_all, na.rm = TRUE),
        stu_known = sum(stu_known, na.rm = TRUE),
        c_known = sum(c_known, na.rm = TRUE),
        c_no_col = sum(c_no_col, na.rm = TRUE),
        c_some_col = sum(c_some_col, na.rm = TRUE),
        c_not_first = sum(c_not_first, na.rm = TRUE)
      ) %>% 
      mutate(eps_codename = "All")
    
    df_c <- bind_rows(df_c, df_c_sum)
    
    # Count table
    table1 <- df %>% 
      select(
        eps_codename,
        stu_all,
        stu_known,
        stu_unknown,
        stu_no_col,
        stu_some_col,
        stu_not_first
      )
    
    # Row percentage table
    table2 <- df_r %>% 
      select(
        eps_codename,
        stu_all,
        stu_known,
        r_known,
        r_no_col,
        r_some_col,
        r_not_first
      )
    
    # Column percentage table
    table3 <- df_c %>%
      select(
        eps_codename,
        stu_all,
        stu_known,
        c_known,
        c_no_col,
        c_some_col,
        c_not_first
      )
  }
  
  result <- list(
    count_table = table1,
    row_pct_table = table2,
    col_pct_table = table3
  )
  
  return(result)
}

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

create_sim_eps_race_firstgen_table <- function(
    data,
    ord_nums,
    eps_codes,
    exclude_race = c(1, 8),
    firstgen_detail = c("collapsed", "disaggregated")
) {
  
  firstgen_detail <- match.arg(firstgen_detail)
  
  # First, create the main processed data frame
  df_work <- data %>% 
    as_tibble() %>% 
    filter(ord_num %in% ord_nums, eps %in% eps_codes) %>%
    # get rid of obs where race is not known or first-generation status is not known
    filter(!is.na(stu_race_cb), stu_race_cb != 0, stu_first_gen != 4) %>% 
    # only selected racial groups; excludes no-response, other, and NA
    filter(stu_race_cb %in% c(1, 2, 3, 4, 8, 9, 12)) %>% 
    filter(!stu_race_cb %in% exclude_race) %>% 
    mutate(
      eps_codename = str_c(eps, eps_name, sep = ", "),
      stu_race_cb = as_factor(stu_race_cb),
      
      # Collapsed first-gen variable:
      # 1 = no college; 2 = some college; both count as first-gen
      stu_first_gen_collapsed = case_when(
        stu_first_gen %in% c(1, 2) ~ "first_gen",
        stu_first_gen == 3 ~ "not_first_gen",
        TRUE ~ NA_character_
      ),
      
      # Original disaggregated first-gen variable
      stu_first_gen_disaggregated = case_match(
        stu_first_gen,
        1 ~ "no_col",
        2 ~ "some_col",
        3 ~ "not_first_gen"
      )
    )
  
  if (firstgen_detail == "collapsed") {
    
    firstgen_var <- "stu_first_gen_collapsed"
    firstgen_cols <- c("first_gen", "not_first_gen")
    
  } else {
    
    firstgen_var <- "stu_first_gen_disaggregated"
    firstgen_cols <- c("no_col", "some_col", "not_first_gen")
  }
  
  # Counts of first-gen status for all races + all EPS
  df_work_all <- df_work %>% 
    count(.data[[firstgen_var]]) %>% 
    rename(firstgen_group = .data[[firstgen_var]]) %>% 
    filter(!is.na(firstgen_group)) %>% 
    pivot_wider(
      names_from = firstgen_group,
      values_from = n
    ) %>% 
    mutate(
      across(all_of(firstgen_cols), ~ if_else(is.na(.), 0L, as.integer(.)))
    ) %>% 
    mutate(
      all = rowSums(select(., all_of(firstgen_cols))),
      stu_race_cb = factor("All"),
      eps_codename = "All"
    ) %>% 
    relocate(stu_race_cb, eps_codename)
  
  # Counts of first-gen status by EPS, all races
  df_work_by_eps <- df_work %>% 
    group_by(eps_codename) %>% 
    count(.data[[firstgen_var]]) %>% 
    ungroup() %>% 
    rename(firstgen_group = .data[[firstgen_var]]) %>% 
    filter(!is.na(firstgen_group)) %>% 
    pivot_wider(
      names_from = firstgen_group,
      values_from = n
    ) %>% 
    mutate(
      across(all_of(firstgen_cols), ~ if_else(is.na(.), 0L, as.integer(.)))
    ) %>% 
    mutate(
      all = rowSums(select(., all_of(firstgen_cols))),
      stu_race_cb = "All",
      stu_race_cb = as_factor(stu_race_cb)
    ) %>% 
    relocate(stu_race_cb)
  
  # Counts of first-gen status by race, all EPS
  df_work_by_race <- df_work %>% 
    group_by(stu_race_cb) %>%
    count(.data[[firstgen_var]]) %>% 
    ungroup() %>% 
    rename(firstgen_group = .data[[firstgen_var]]) %>% 
    filter(!is.na(firstgen_group)) %>% 
    pivot_wider(
      names_from = firstgen_group,
      values_from = n
    ) %>% 
    mutate(
      across(all_of(firstgen_cols), ~ if_else(is.na(.), 0L, as.integer(.)))
    ) %>% 
    mutate(
      all = rowSums(select(., all_of(firstgen_cols))),
      eps_codename = "All"
    ) %>% 
    relocate(stu_race_cb, eps_codename)
  
  # Counts of first-gen status by race and EPS
  df_work_by_race_eps <- df_work %>% 
    group_by(stu_race_cb, eps_codename) %>%
    count(.data[[firstgen_var]]) %>% 
    ungroup() %>% 
    rename(firstgen_group = .data[[firstgen_var]]) %>% 
    filter(!is.na(firstgen_group)) %>% 
    pivot_wider(
      names_from = firstgen_group,
      values_from = n
    ) %>% 
    mutate(
      across(all_of(firstgen_cols), ~ if_else(is.na(.), 0L, as.integer(.)))
    ) %>% 
    mutate(
      all = rowSums(select(., all_of(firstgen_cols)))
    )
  
  final_df <- bind_rows(
    df_work_all,
    df_work_by_eps,
    df_work_by_race,
    df_work_by_race_eps
  ) %>% 
    mutate(
      across(
        all_of(firstgen_cols),
        ~ (.x / all) * 100,
        .names = "row_{.col}"
      )
    ) %>% 
    select(-all_of(firstgen_cols)) %>% 
    mutate(
      stu_race_cb = fct_relevel(
        stu_race_cb,
        "All", 
        "Asian", 
        "Black/African American", 
        "Hispanic/Latino",
        "white",
        "two or more races, non-Hispanic",
        "American Indian/Alaska Native",
        "Native Hawaiian/Pacific Islander"
      )
    ) %>%
    arrange(stu_race_cb, eps_codename)
  
  return(final_df)
}

# philly metro area
# order 448922: PSAT 1070 - 1180; order 448427: PSAT 1190 - 1260; order 448440: PSAT 1270 - 1520

#create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('487984'), eps_codes = chi_eps_codes) %>% print(n=70) # 
#create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('487984'), eps_codes = chi_eps_codes, exclude_race = '') %>% print(n=70) # 
#create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('487984'), eps_codes = chi_eps_codes, exclude_race = c(1,8,12)) %>% print(n=70) # 
#lists_orders_zip_hs_df_sf %>% as_tibble() %>% count(stu_race_cb)


############################################################
# CREATE RQ2 OVERLAY CONTRIBUTION GRAPHS
# Race-only and first-gen-only versions
############################################################


############################################################
# 1) RACE CONTRIBUTION OVERLAY DATA
############################################################

create_rq2_race_contribution_overlay_df <- function(
    data,
    ord_nums,
    eps_codes,
    metro = NULL,
    order_ids = NULL,
    test_range = NULL,
    race_keep = c(
      "White, non-Hispanic",
      "Asian, non-Hispanic",
      "Black, non-Hispanic",
      "Hispanic"
    )
) {
  
  # ----------------------------------------------------------
  # 1) Build race table from existing function
  # ----------------------------------------------------------
  
  table_list <- create_sim_eps_race_table(
    data      = data,
    ord_nums  = ord_nums,
    eps_codes = eps_codes
  )
  
  count_df <- table_list$count_table
  col_df   <- table_list$col_pct_table
  
  # ----------------------------------------------------------
  # 2) Lookup for race contribution columns and totals
  # ----------------------------------------------------------
  
  race_lookup <- tibble::tibble(
    col_name = c(
      "c_white",
      "c_asian",
      "c_black",
      "c_hispanic",
      "c_multi",
      "c_aian",
      "c_nhpi"
    ),
    total_col = c(
      "stu_white",
      "stu_asian",
      "stu_black",
      "stu_hispanic",
      "stu_multi",
      "stu_aian",
      "stu_nhpi"
    ),
    group = c(
      "White, non-Hispanic",
      "Asian, non-Hispanic",
      "Black, non-Hispanic",
      "Hispanic",
      "Two+, non-Hispanic",
      "AIAN, non-Hispanic",
      "NHPI, non-Hispanic"
    )
  ) %>%
    dplyr::filter(group %in% race_keep)
  
  # ----------------------------------------------------------
  # 3) Get group totals from All row
  # ----------------------------------------------------------
  
  totals_df <- count_df %>%
    dplyr::filter(eps_codename == "All") %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(c("race_known", race_lookup$total_col)),
      names_to = "total_col",
      values_to = "total_n"
    )
  
  baseline_total <- totals_df %>%
    dplyr::filter(total_col == "race_known") %>%
    dplyr::pull(total_n)
  
  focal_totals <- race_lookup %>%
    dplyr::left_join(totals_df, by = "total_col") %>%
    dplyr::select(group, total_n)
  
  # ----------------------------------------------------------
  # 4) Build long contribution dataframe
  # ----------------------------------------------------------
  
  focal_df <- col_df %>%
    dplyr::filter(eps_codename != "All") %>%
    dplyr::select(
      eps_codename,
      baseline_pct = c_known,
      dplyr::all_of(race_lookup$col_name)
    ) %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(race_lookup$col_name),
      names_to = "col_name",
      values_to = "focal_pct"
    ) %>%
    dplyr::left_join(
      race_lookup %>% dplyr::select(col_name, group),
      by = "col_name"
    ) %>%
    dplyr::left_join(
      focal_totals,
      by = "group"
    ) %>%
    dplyr::mutate(
      group = factor(group, levels = race_keep),
      baseline_group = "All race-known prospects",
      baseline_total = baseline_total,
      metro = metro,
      order_ids = order_ids,
      test_range = test_range,
      facet_label = stringr::str_c(
        group,
        "\nN = ",
        scales::comma(round(total_n))
      )
    )
  
  return(focal_df)
}


############################################################
# 2) RACE CONTRIBUTION OVERLAY PLOT
############################################################

create_rq2_race_contribution_overlay_plot <- function(
    plot_df,
    title = NULL,
    subtitle = NULL,
    metro_label = NULL,
    sort_order = c("geomarket", "focal", "baseline"),
    point_size = 1.4,
    focal_line_width = 0.55,
    baseline_bar_width = 4.4,
    show_value_labels = FALSE
) {
  
  sort_order <- match.arg(sort_order)
  
  # ----------------------------------------------------------
  # 1) Order Geomarkets within race/facet
  # ----------------------------------------------------------
  
  df_ordered <- plot_df %>%
    dplyr::mutate(
      geomarket_num = readr::parse_number(eps_codename),
      
      # Revised facet label: group name + n on same line
      facet_label = stringr::str_c(
        group,
        " (n = ",
        scales::comma(round(total_n)),
        ")"
      ),
      
      sort_value = dplyr::case_when(
        sort_order == "geomarket" ~ geomarket_num,
        sort_order == "focal"     ~ -focal_pct,
        sort_order == "baseline"  ~ -baseline_pct,
        TRUE ~ geomarket_num
      )
    ) %>%
    dplyr::group_by(group) %>%
    dplyr::arrange(
      is.na(sort_value),
      sort_value,
      is.na(geomarket_num),
      geomarket_num,
      eps_codename,
      .by_group = TRUE
    ) %>%
    dplyr::mutate(
      geomarket_order = dplyr::row_number()
    ) %>%
    dplyr::ungroup()
  
  # ----------------------------------------------------------
  # 2) Order facet labels
  # ----------------------------------------------------------
  
  group_levels <- levels(df_ordered$group)
  
  facet_levels <- df_ordered %>%
    dplyr::distinct(group, facet_label) %>%
    dplyr::arrange(match(group, group_levels)) %>%
    dplyr::pull(facet_label)
  
  df_ordered <- df_ordered %>%
    dplyr::mutate(
      facet_label = factor(facet_label, levels = facet_levels)
    )
  
  # ----------------------------------------------------------
  # 3) Numeric y positions, globally unique across facets
  # ----------------------------------------------------------
  
  y_lookup <- df_ordered %>%
    dplyr::select(group, facet_label, eps_codename, geomarket_order) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      facet_label = factor(facet_label, levels = facet_levels)
    ) %>%
    dplyr::arrange(facet_label, geomarket_order) %>%
    dplyr::group_by(facet_label) %>%
    dplyr::mutate(
      n_in_facet = dplyr::n(),
      y_within_facet = n_in_facet - dplyr::row_number() + 1
    ) %>%
    dplyr::ungroup()
  
  facet_offsets <- y_lookup %>%
    dplyr::distinct(facet_label, n_in_facet) %>%
    dplyr::arrange(facet_label) %>%
    dplyr::mutate(
      facet_offset = dplyr::lag(cumsum(n_in_facet), default = 0)
    )
  
  y_lookup <- y_lookup %>%
    dplyr::left_join(
      facet_offsets,
      by = c("facet_label", "n_in_facet")
    ) %>%
    dplyr::mutate(
      y_base = facet_offset + y_within_facet
    )
  
  df_plot <- df_ordered %>%
    dplyr::left_join(
      y_lookup %>%
        dplyr::select(group, facet_label, eps_codename, geomarket_order, y_base),
      by = c("group", "facet_label", "eps_codename", "geomarket_order")
    ) %>%
    dplyr::mutate(
      focal_label = stringr::str_c(
        scales::number(focal_pct, accuracy = 0.1),
        "%"
      ),
      baseline_label = stringr::str_c(
        scales::number(baseline_pct, accuracy = 0.1),
        "%"
      )
    )
  
  # ----------------------------------------------------------
  # 4) Y-axis breaks and labels
  # ----------------------------------------------------------
  
  y_breaks_df <- y_lookup %>%
    dplyr::arrange(y_base)
  
  y_breaks <- y_breaks_df$y_base
  y_labels <- y_breaks_df$eps_codename
  
  # ----------------------------------------------------------
  # 5) Title/subtitle defaults
  # ----------------------------------------------------------
  
  if (is.null(metro_label)) {
    
    metro_label <- plot_df$metro[1] %>%
      stringr::str_replace_all("_", " ") %>%
      tools::toTitleCase()
    
    if (!is.na(metro_label) && metro_label != "Bay Area") {
      metro_label <- stringr::str_c(metro_label, " area")
    }
  }
  
  if (is.null(title)) {
    title <- stringr::str_c(
      "Where do racial/ethnic groups come from, ",
      metro_label,
      "?"
    )
  }
  
  if (is.null(subtitle)) {
    
    test_range_this <- plot_df$test_range[1]
    
    subtitle <- stringr::str_c(
      test_range_this,
      " — focal racial/ethnic groups overlaid on all race-known prospects"
    )
  }
  
  # ----------------------------------------------------------
  # 6) Build plot
  # ----------------------------------------------------------
  
  plot <- ggplot2::ggplot(df_plot) +
    
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 0,
        xend = baseline_pct,
        y = y_base,
        yend = y_base,
        color = "All race-known prospects"
      ),
      linewidth = baseline_bar_width,
      lineend = "butt"
    ) +
    
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 0,
        xend = focal_pct,
        y = y_base,
        yend = y_base,
        color = "Focal racial/ethnic group"
      ),
      linewidth = focal_line_width,
      lineend = "round"
    ) +
    
    ggplot2::geom_point(
      ggplot2::aes(
        x = focal_pct,
        y = y_base,
        color = "Focal racial/ethnic group"
      ),
      size = point_size
    ) +
    
    ggplot2::facet_wrap(
      ~ facet_label,
      scales = "free_y",
      ncol = 2
    ) +
    ggplot2::scale_y_continuous(
      breaks = y_breaks,
      labels = y_labels,
      expand = ggplot2::expansion(add = c(0.6, 0.6))
    ) +
    ggplot2::scale_x_continuous(
      labels = function(x) {
        stringr::str_c(scales::number(x, accuracy = 1), "%")
      }
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Focal racial/ethnic group" = "#0072B2",
        "All race-known prospects" = "gray82"
      )
    ) +
    ggplot2::labs(
      x = "Share of group contributed by Geomarket",
      y = NULL,
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      axis.text.y = ggplot2::element_text(size = 8),
      axis.text.x = ggplot2::element_text(size = 10),
      axis.title.x = ggplot2::element_text(size = 11),
      legend.position = "bottom"
    )
  
  if (show_value_labels) {
    
    plot <- plot +
      ggplot2::geom_text(
        ggplot2::aes(
          x = focal_pct,
          y = y_base,
          label = focal_label,
          color = "Focal racial/ethnic group"
        ),
        hjust = -0.15,
        size = 3,
        show.legend = FALSE
      ) +
      ggplot2::coord_cartesian(clip = "off")
  }
  
  attr(plot, "figure_title") <- title
  attr(plot, "figure_subtitle") <- subtitle
  
  return(plot)
}


############################################################
# 3) FIRST-GEN CONTRIBUTION OVERLAY DATA
############################################################

create_rq2_firstgen_contribution_overlay_df <- function(
    data,
    ord_nums,
    eps_codes,
    metro = NULL,
    order_ids = NULL,
    test_range = NULL,
    firstgen_detail = c("collapsed", "disaggregated")
) {
  
  firstgen_detail <- match.arg(firstgen_detail)
  
  if (firstgen_detail != "collapsed") {
    stop("This overlay plot currently expects firstgen_detail = 'collapsed'.")
  }
  
  # ----------------------------------------------------------
  # 1) Build first-gen table from existing function
  # ----------------------------------------------------------
  
  table_list <- create_sim_eps_firstgen_table(
    data            = data,
    ord_nums        = ord_nums,
    eps_codes       = eps_codes,
    firstgen_detail = "collapsed"
  )
  
  count_df <- table_list$count_table
  col_df   <- table_list$col_pct_table
  
  # ----------------------------------------------------------
  # 2) Totals from All row
  # ----------------------------------------------------------
  
  totals_df <- count_df %>%
    dplyr::filter(eps_codename == "All")
  
  baseline_total <- totals_df$stu_known
  firstgen_total <- totals_df$stu_first_gen
  nonfg_total    <- totals_df$stu_not_first
  
  # ----------------------------------------------------------
  # 3) Build contribution dataframe
  # ----------------------------------------------------------
  
  df_plot <- col_df %>%
    dplyr::filter(eps_codename != "All") %>%
    dplyr::transmute(
      eps_codename,
      baseline_pct = c_known,
      firstgen_pct = c_first_gen,
      nonfg_pct = c_not_first,
      baseline_total = baseline_total,
      firstgen_total = firstgen_total,
      nonfg_total = nonfg_total,
      metro = metro,
      order_ids = order_ids,
      test_range = test_range
    )
  
  return(df_plot)
}


############################################################
# 4) FIRST-GEN CONTRIBUTION OVERLAY PLOT
############################################################

create_rq2_firstgen_contribution_overlay_plot <- function(
    plot_df,
    title = NULL,
    subtitle = NULL,
    metro_label = NULL,
    sort_order = c("geomarket", "focal", "baseline"),
    point_size = 1.4,
    focal_line_width = 0.55,
    baseline_bar_width = 4.4,
    show_value_labels = FALSE
) {
  
  sort_order <- match.arg(sort_order)
  
  # ----------------------------------------------------------
  # 1) Reshape to one row per Geomarket x first-gen group
  # ----------------------------------------------------------
  
  df_long <- plot_df %>%
    dplyr::mutate(
      geomarket_num = readr::parse_number(eps_codename)
    ) %>%
    dplyr::select(
      eps_codename,
      geomarket_num,
      baseline_pct,
      firstgen_pct,
      nonfg_pct,
      firstgen_total,
      nonfg_total,
      metro,
      order_ids,
      test_range
    ) %>%
    tidyr::pivot_longer(
      cols = c(firstgen_pct, nonfg_pct),
      names_to = "group_raw",
      values_to = "focal_pct"
    ) %>%
    dplyr::mutate(
      group = dplyr::recode(
        group_raw,
        "firstgen_pct" = "First-gen",
        "nonfg_pct" = "Not first-gen"
      ),
      group = factor(
        group,
        levels = c("First-gen", "Not first-gen")
      ),
      total_n = dplyr::if_else(
        group == "First-gen",
        firstgen_total,
        nonfg_total
      ),
      
      # Revised facet label: group name + n on same line
      facet_label = stringr::str_c(
        group,
        " (n = ",
        scales::comma(round(total_n)),
        ")"
      ),
      
      sort_value = dplyr::case_when(
        sort_order == "geomarket" ~ geomarket_num,
        sort_order == "focal"     ~ -focal_pct,
        sort_order == "baseline"  ~ -baseline_pct,
        TRUE ~ geomarket_num
      )
    )
  
  # ----------------------------------------------------------
  # 2) Order Geomarkets within each first-gen group/facet
  # ----------------------------------------------------------
  
  df_ordered <- df_long %>%
    dplyr::group_by(group) %>%
    dplyr::arrange(
      is.na(sort_value),
      sort_value,
      is.na(geomarket_num),
      geomarket_num,
      eps_codename,
      .by_group = TRUE
    ) %>%
    dplyr::mutate(
      geomarket_order = dplyr::row_number()
    ) %>%
    dplyr::ungroup()
  
  # ----------------------------------------------------------
  # 3) Order facet labels
  # ----------------------------------------------------------
  
  facet_levels <- df_ordered %>%
    dplyr::distinct(group, facet_label) %>%
    dplyr::arrange(group) %>%
    dplyr::pull(facet_label)
  
  df_ordered <- df_ordered %>%
    dplyr::mutate(
      facet_label = factor(facet_label, levels = facet_levels)
    )
  
  # ----------------------------------------------------------
  # 4) Numeric y positions, globally unique across facets
  # ----------------------------------------------------------
  
  y_lookup <- df_ordered %>%
    dplyr::select(group, facet_label, eps_codename, geomarket_order) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      facet_label = factor(facet_label, levels = facet_levels)
    ) %>%
    dplyr::arrange(facet_label, geomarket_order) %>%
    dplyr::group_by(facet_label) %>%
    dplyr::mutate(
      n_in_facet = dplyr::n(),
      y_within_facet = n_in_facet - dplyr::row_number() + 1
    ) %>%
    dplyr::ungroup()
  
  facet_offsets <- y_lookup %>%
    dplyr::distinct(facet_label, n_in_facet) %>%
    dplyr::arrange(facet_label) %>%
    dplyr::mutate(
      facet_offset = dplyr::lag(cumsum(n_in_facet), default = 0)
    )
  
  y_lookup <- y_lookup %>%
    dplyr::left_join(
      facet_offsets,
      by = c("facet_label", "n_in_facet")
    ) %>%
    dplyr::mutate(
      y_base = facet_offset + y_within_facet
    )
  
  df_plot <- df_ordered %>%
    dplyr::left_join(
      y_lookup %>%
        dplyr::select(group, facet_label, eps_codename, geomarket_order, y_base),
      by = c("group", "facet_label", "eps_codename", "geomarket_order")
    ) %>%
    dplyr::mutate(
      focal_label = stringr::str_c(
        scales::number(focal_pct, accuracy = 0.1),
        "%"
      ),
      baseline_label = stringr::str_c(
        scales::number(baseline_pct, accuracy = 0.1),
        "%"
      )
    )
  
  # ----------------------------------------------------------
  # 5) Y-axis breaks and labels
  # ----------------------------------------------------------
  
  y_breaks_df <- y_lookup %>%
    dplyr::arrange(y_base)
  
  y_breaks <- y_breaks_df$y_base
  y_labels <- y_breaks_df$eps_codename
  
  # ----------------------------------------------------------
  # 6) Title/subtitle defaults
  # ----------------------------------------------------------
  
  if (is.null(metro_label)) {
    
    metro_label <- plot_df$metro[1] %>%
      stringr::str_replace_all("_", " ") %>%
      tools::toTitleCase()
    
    if (!is.na(metro_label) && metro_label != "Bay Area") {
      metro_label <- stringr::str_c(metro_label, " area")
    }
  }
  
  if (is.null(title)) {
    title <- stringr::str_c(
      "Where do first-generation and non-first-generation prospects come from, ",
      metro_label,
      "?"
    )
  }
  
  if (is.null(subtitle)) {
    
    test_range_this <- plot_df$test_range[1]
    
    subtitle <- stringr::str_c(
      test_range_this,
      " — focal first-generation-status groups overlaid on all first-gen-known prospects"
    )
  }
  
  # ----------------------------------------------------------
  # 7) Build plot
  # ----------------------------------------------------------
  
  plot <- ggplot2::ggplot(df_plot) +
    
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 0,
        xend = baseline_pct,
        y = y_base,
        yend = y_base,
        color = "All first-gen-known prospects"
      ),
      linewidth = baseline_bar_width,
      lineend = "butt"
    ) +
    
    ggplot2::geom_segment(
      ggplot2::aes(
        x = 0,
        xend = focal_pct,
        y = y_base,
        yend = y_base,
        color = "Focal first-generation-status group"
      ),
      linewidth = focal_line_width,
      lineend = "round"
    ) +
    
    ggplot2::geom_point(
      ggplot2::aes(
        x = focal_pct,
        y = y_base,
        color = "Focal first-generation-status group"
      ),
      size = point_size
    ) +
    
    ggplot2::facet_wrap(
      ~ facet_label,
      scales = "free_y",
      ncol = 1
    ) +
    ggplot2::scale_y_continuous(
      breaks = y_breaks,
      labels = y_labels,
      expand = ggplot2::expansion(add = c(0.6, 0.6))
    ) +
    ggplot2::scale_x_continuous(
      labels = function(x) {
        stringr::str_c(scales::number(x, accuracy = 1), "%")
      }
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Focal first-generation-status group" = "#0072B2",
        "All first-gen-known prospects" = "gray82"
      )
    ) +
    ggplot2::labs(
      x = "Share of group contributed by Geomarket",
      y = NULL,
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      axis.text.y = ggplot2::element_text(size = 8),
      axis.text.x = ggplot2::element_text(size = 10),
      axis.title.x = ggplot2::element_text(size = 11),
      legend.position = "bottom"
    )
  
  if (show_value_labels) {
    
    plot <- plot +
      ggplot2::geom_text(
        ggplot2::aes(
          x = focal_pct,
          y = y_base,
          label = focal_label,
          color = "Focal first-generation-status group"
        ),
        hjust = -0.15,
        size = 3,
        show.legend = FALSE
      ) +
      ggplot2::coord_cartesian(clip = "off")
  }
  
  attr(plot, "figure_title") <- title
  attr(plot, "figure_subtitle") <- subtitle
  
  return(plot)
}
############################################################
# CREATE RQ2 RACE X FIRST-GEN CONTRIBUTION DATA + GRAPH
############################################################

############
############ function to create data for two lollipop plot
############

create_rq2_race_firstgen_contribution_df <- function(
    data,
    ord_nums,
    eps_codes,
    metro = NULL,
    order_ids = NULL,
    test_range = NULL,
    exclude_race = c(1, 8, 12),
    race_keep = c(
      "White, non-Hispanic",
      "Asian, non-Hispanic",
      "Black, non-Hispanic",
      "Hispanic"
    )
) {
  
  # ----------------------------------------------------------
  # 1) Build underlying race x first-gen table
  # ----------------------------------------------------------
  
  df_raw <- create_sim_eps_race_firstgen_table(
    data            = data,
    ord_nums        = ord_nums,
    eps_codes       = eps_codes,
    exclude_race    = exclude_race,
    firstgen_detail = "collapsed"
  )
  
  # ----------------------------------------------------------
  # 2) Recode race labels and keep relevant rows
  # ----------------------------------------------------------
  
  df_plot <- df_raw %>%
    dplyr::mutate(
      race = dplyr::recode(
        as.character(stu_race_cb),
        "white" = "White, non-Hispanic",
        "Asian" = "Asian, non-Hispanic",
        "Black/African American" = "Black, non-Hispanic",
        "Hispanic/Latino" = "Hispanic",
        "two or more races, non-Hispanic" = "Two+, non-Hispanic",
        "American Indian/Alaska Native" = "AIAN, non-Hispanic",
        "Native Hawaiian/Pacific Islander" = "NHPI, non-Hispanic",
        "All" = "All"
      )
    ) %>%
    dplyr::filter(
      race %in% race_keep,
      eps_codename != "All"
    )
  
  # ----------------------------------------------------------
  # 3) Convert within-Geomarket composition back into counts
  #    and then calculate contribution shares
  # ----------------------------------------------------------
  
  df_plot <- df_plot %>%
    dplyr::mutate(
      fg_n = all * (row_first_gen / 100),
      nonfg_n = all * (row_not_first_gen / 100)
    ) %>%
    dplyr::group_by(race) %>%
    dplyr::mutate(
      fg_total = sum(fg_n, na.rm = TRUE),
      nonfg_total = sum(nonfg_n, na.rm = TRUE),
      fg_pct = 100 * fg_n / fg_total,
      nonfg_pct = 100 * nonfg_n / nonfg_total
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      race = factor(race, levels = race_keep),
      facet_label = stringr::str_c(
        race,
        "\nFirst-gen n = ", scales::comma(round(fg_total)),
        " | Not first-gen n = ", scales::comma(round(nonfg_total))
      ),
      metro = metro,
      order_ids = order_ids,
      test_range = test_range
    )
  
  return(df_plot)
}

############
############ function to create two lollipop plot
############

create_rq2_race_firstgen_two_lollipop_plot <- function(
    plot_df,
    title = NULL,
    subtitle = NULL,
    metro_label = NULL,
    point_size = 1.2,
    line_width = 0.45,
    pair_nudge = 0.13,
    show_value_labels = FALSE
) {
  
  # ----------------------------------------------------------
  # 1) Order Geomarkets within race by first-gen contribution
  # ----------------------------------------------------------
  
  df_ordered <- plot_df %>%
    dplyr::group_by(race) %>%
    dplyr::arrange(dplyr::desc(fg_pct), .by_group = TRUE) %>%
    dplyr::mutate(
      geomarket_order = dplyr::row_number()
    ) %>%
    dplyr::ungroup()
  
  # ----------------------------------------------------------
  # 2) Order facet labels
  # ----------------------------------------------------------
  
  facet_levels <- df_ordered %>%
    dplyr::distinct(race, facet_label) %>%
    dplyr::arrange(match(
      race,
      c(
        "White, non-Hispanic",
        "Asian, non-Hispanic",
        "Black, non-Hispanic",
        "Hispanic"
      )
    )) %>%
    dplyr::pull(facet_label)
  
  df_ordered <- df_ordered %>%
    dplyr::mutate(
      facet_label = factor(facet_label, levels = facet_levels)
    )
  
  # ----------------------------------------------------------
  # 3) Create one numeric y-base row per Geomarket
  #    IMPORTANT: y_base must be globally unique across facets
  # ----------------------------------------------------------
  
  y_lookup <- df_ordered %>%
    dplyr::select(race, facet_label, eps_codename, geomarket_order) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      facet_label = factor(facet_label, levels = facet_levels)
    ) %>%
    dplyr::arrange(facet_label, geomarket_order) %>%
    dplyr::group_by(facet_label) %>%
    dplyr::mutate(
      n_in_facet = dplyr::n(),
      y_within_facet = n_in_facet - dplyr::row_number() + 1
    ) %>%
    dplyr::ungroup()
  
  facet_offsets <- y_lookup %>%
    dplyr::distinct(facet_label, n_in_facet) %>%
    dplyr::arrange(facet_label) %>%
    dplyr::mutate(
      facet_offset = dplyr::lag(cumsum(n_in_facet), default = 0)
    )
  
  y_lookup <- y_lookup %>%
    dplyr::left_join(
      facet_offsets,
      by = c("facet_label", "n_in_facet")
    ) %>%
    dplyr::mutate(
      y_base = facet_offset + y_within_facet
    )
  
  df_ordered <- df_ordered %>%
    dplyr::left_join(
      y_lookup %>%
        dplyr::select(race, facet_label, eps_codename, geomarket_order, y_base),
      by = c("race", "facet_label", "eps_codename", "geomarket_order")
    )
  
  # ----------------------------------------------------------
  # 4) Long format: first-gen and not-first-gen
  # ----------------------------------------------------------
  
  df_long <- df_ordered %>%
    dplyr::select(
      race,
      facet_label,
      eps_codename,
      y_base,
      fg_pct,
      nonfg_pct
    ) %>%
    tidyr::pivot_longer(
      cols = c(fg_pct, nonfg_pct),
      names_to = "firstgen_status",
      values_to = "contribution_pct"
    ) %>%
    dplyr::mutate(
      firstgen_status = dplyr::recode(
        firstgen_status,
        "fg_pct" = "First-gen",
        "nonfg_pct" = "Not first-gen"
      ),
      firstgen_status = factor(
        firstgen_status,
        levels = c("First-gen", "Not first-gen")
      ),
      y_plot = dplyr::if_else(
        firstgen_status == "First-gen",
        y_base + pair_nudge,
        y_base - pair_nudge
      ),
      value_label = stringr::str_c(
        scales::number(contribution_pct, accuracy = 0.1),
        "%"
      )
    )
  
  df_fg <- df_long %>%
    dplyr::filter(firstgen_status == "First-gen")
  
  df_nonfg <- df_long %>%
    dplyr::filter(firstgen_status == "Not first-gen")
  
  # ----------------------------------------------------------
  # 5) Y-axis breaks and labels
  # ----------------------------------------------------------
  
  y_breaks_df <- y_lookup %>%
    dplyr::arrange(y_base)
  
  y_breaks <- y_breaks_df$y_base
  y_labels <- y_breaks_df$eps_codename
  
  # ----------------------------------------------------------
  # 6) Title/subtitle defaults
  # ----------------------------------------------------------
  
  if (is.null(metro_label)) {
    
    metro_label <- plot_df$metro[1] %>%
      stringr::str_replace_all("_", " ") %>%
      tools::toTitleCase()
    
    if (!is.na(metro_label) && metro_label != "Bay Area") {
      metro_label <- stringr::str_c(metro_label, " area")
    }
  }
  
  if (is.null(title)) {
    title <- stringr::str_c(
      "Where do first-generation and non-first-generation prospects come from, ",
      metro_label,
      "?"
    )
  }
  
  if (is.null(subtitle)) {
    
    test_range_this <- plot_df$test_range[1]
    
    subtitle <- stringr::str_c(
      test_range_this,
      " — share of each race × first-generation subgroup contributed by each Geomarket"
    )
  }
  
  # ----------------------------------------------------------
  # 7) Build plot
  # ----------------------------------------------------------
  
  plot <- ggplot2::ggplot() +
    
    ggplot2::geom_segment(
      data = df_fg,
      ggplot2::aes(
        x = 0,
        xend = contribution_pct,
        y = y_plot,
        yend = y_plot,
        color = firstgen_status
      ),
      linewidth = line_width
    ) +
    ggplot2::geom_point(
      data = df_fg,
      ggplot2::aes(
        x = contribution_pct,
        y = y_plot,
        color = firstgen_status
      ),
      size = point_size
    ) +
    
    ggplot2::geom_segment(
      data = df_nonfg,
      ggplot2::aes(
        x = 0,
        xend = contribution_pct,
        y = y_plot,
        yend = y_plot,
        color = firstgen_status
      ),
      linewidth = line_width
    ) +
    ggplot2::geom_point(
      data = df_nonfg,
      ggplot2::aes(
        x = contribution_pct,
        y = y_plot,
        color = firstgen_status
      ),
      size = point_size
    ) +
    
    ggplot2::facet_wrap(
      ~ facet_label,
      scales = "free_y",
      ncol = 2
    ) +
    ggplot2::scale_y_continuous(
      breaks = y_breaks,
      labels = y_labels,
      expand = ggplot2::expansion(add = c(0.6, 0.6))
    ) +
    ggplot2::scale_x_continuous(
      labels = function(x) {
        stringr::str_c(scales::number(x, accuracy = 1), "%")
      }
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "First-gen" = "#0072B2",
        "Not first-gen" = "#D55E00"
      )
    ) +
    ggplot2::labs(
      x = "Share of subgroup contributed by Geomarket",
      y = NULL,
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      axis.text.y = ggplot2::element_text(size = 8),
      axis.text.x = ggplot2::element_text(size = 10),
      axis.title.x = ggplot2::element_text(size = 11),
      legend.position = "bottom"
    )
  
  if (show_value_labels) {
    plot <- plot +
      ggplot2::geom_text(
        data = df_long,
        ggplot2::aes(
          x = contribution_pct,
          y = y_plot,
          label = value_label,
          color = firstgen_status
        ),
        hjust = -0.15,
        size = 3,
        show.legend = FALSE
      ) +
      ggplot2::coord_cartesian(clip = "off")
  }
  
  attr(plot, "figure_title") <- title
  attr(plot, "figure_subtitle") <- subtitle
  
  return(plot)
}

######################################
############## CREATE TABLE FOR COMPOSITION OF FILTERING OUT PARTICULAR GEOMARKETS
######################################

create_rq3_eps_exclusion_table <- function(
    data,
    ord_nums,
    eps_codes,
    composition_type = c("race", "firstgen", "race_firstgen"),
    exclude_race = c(1, 8),
    firstgen_detail = c("collapsed", "disaggregated")
) {
  
  composition_type <- match.arg(composition_type)
  firstgen_detail  <- match.arg(firstgen_detail)
  
  # ----------------------------------------------------------
  # 1) Subset to the relevant order(s) and Geomarkets
  # ----------------------------------------------------------
  
  df_base <- data %>%
    as_tibble() %>%
    filter(
      ord_num %in% ord_nums,
      eps %in% eps_codes
    ) %>%
    mutate(
      eps_codename = str_c(eps, eps_name, sep = ", ")
    )
  
  # Full raw pool before exclusions. This is for diagnostics only.
  full_pool_n <- nrow(df_base)
  
  # Number of raw profiles removed when each Geomarket is excluded.
  removed_n_df <- df_base %>%
    count(eps_codename, name = "removed_n") %>%
    rename(excluded_eps_codename = eps_codename) %>%
    mutate(
      removed_pct = removed_n / full_pool_n * 100
    )
  
  # ----------------------------------------------------------
  # 2) Helper: create one composition table from a supplied df
  # ----------------------------------------------------------
  
  make_one_composition <- function(df, scenario_type, excluded_eps_codename) {
    
    # ----------------------------
    # Race composition
    # ----------------------------
    
    if (composition_type == "race") {
      
      out <- df %>%
        filter(
          !is.na(stu_race_cb),
          stu_race_cb != 0,
          stu_race_cb %in% c(1, 2, 3, 4, 8, 9, 12),
          !stu_race_cb %in% exclude_race
        ) %>%
        mutate(
          group = case_match(
            stu_race_cb,
            1  ~ "AIAN, non-Hispanic",
            2  ~ "Asian, non-Hispanic",
            3  ~ "Black, non-Hispanic",
            4  ~ "Hispanic",
            8  ~ "NHPI, non-Hispanic",
            9  ~ "White, non-Hispanic",
            12 ~ "Two+, non-Hispanic"
          )
        ) %>%
        count(group, name = "n") %>%
        mutate(
          denom = sum(n, na.rm = TRUE),
          pct = n / denom * 100,
          composition_type = "race",
          scenario_type = scenario_type,
          excluded_eps_codename = excluded_eps_codename
        ) %>%
        select(
          composition_type,
          scenario_type,
          excluded_eps_codename,
          group,
          n,
          denom,
          pct
        )
      
      return(out)
    }
    
    # ----------------------------
    # First-gen composition
    # ----------------------------
    
    if (composition_type == "firstgen") {
      
      if (firstgen_detail == "collapsed") {
        
        out <- df %>%
          filter(stu_first_gen %in% c(1, 2, 3)) %>%
          mutate(
            group = case_when(
              stu_first_gen %in% c(1, 2) ~ "First-gen",
              stu_first_gen == 3 ~ "Not first-gen",
              TRUE ~ NA_character_
            )
          ) %>%
          count(group, name = "n") %>%
          mutate(
            denom = sum(n, na.rm = TRUE),
            pct = n / denom * 100,
            composition_type = "firstgen",
            scenario_type = scenario_type,
            excluded_eps_codename = excluded_eps_codename
          ) %>%
          select(
            composition_type,
            scenario_type,
            excluded_eps_codename,
            group,
            n,
            denom,
            pct
          )
        
      } else {
        
        out <- df %>%
          filter(stu_first_gen %in% c(1, 2, 3)) %>%
          mutate(
            group = case_match(
              stu_first_gen,
              1 ~ "No college",
              2 ~ "Some college",
              3 ~ "Not first-gen"
            )
          ) %>%
          count(group, name = "n") %>%
          mutate(
            denom = sum(n, na.rm = TRUE),
            pct = n / denom * 100,
            composition_type = "firstgen",
            scenario_type = scenario_type,
            excluded_eps_codename = excluded_eps_codename
          ) %>%
          select(
            composition_type,
            scenario_type,
            excluded_eps_codename,
            group,
            n,
            denom,
            pct
          )
      }
      
      return(out)
    }
    
    # ----------------------------
    # Race x first-gen composition
    # ----------------------------
    
    if (composition_type == "race_firstgen") {
      
      df_work <- df %>%
        filter(
          !is.na(stu_race_cb),
          stu_race_cb != 0,
          stu_race_cb %in% c(1, 2, 3, 4, 8, 9, 12),
          !stu_race_cb %in% exclude_race,
          stu_first_gen %in% c(1, 2, 3)
        ) %>%
        mutate(
          race_group = case_match(
            stu_race_cb,
            1  ~ "AIAN, non-Hispanic",
            2  ~ "Asian, non-Hispanic",
            3  ~ "Black, non-Hispanic",
            4  ~ "Hispanic",
            8  ~ "NHPI, non-Hispanic",
            9  ~ "White, non-Hispanic",
            12 ~ "Two+, non-Hispanic"
          )
        )
      
      if (firstgen_detail == "collapsed") {
        
        out <- df_work %>%
          mutate(
            firstgen_group = case_when(
              stu_first_gen %in% c(1, 2) ~ "First-gen",
              stu_first_gen == 3 ~ "Not first-gen",
              TRUE ~ NA_character_
            ),
            group = str_c(race_group, " | ", firstgen_group)
          ) %>%
          count(group, name = "n") %>%
          mutate(
            denom = sum(n, na.rm = TRUE),
            pct = n / denom * 100,
            composition_type = "race_firstgen",
            scenario_type = scenario_type,
            excluded_eps_codename = excluded_eps_codename
          ) %>%
          select(
            composition_type,
            scenario_type,
            excluded_eps_codename,
            group,
            n,
            denom,
            pct
          )
        
      } else {
        
        out <- df_work %>%
          mutate(
            firstgen_group = case_match(
              stu_first_gen,
              1 ~ "No college",
              2 ~ "Some college",
              3 ~ "Not first-gen"
            ),
            group = str_c(race_group, " | ", firstgen_group)
          ) %>%
          count(group, name = "n") %>%
          mutate(
            denom = sum(n, na.rm = TRUE),
            pct = n / denom * 100,
            composition_type = "race_firstgen",
            scenario_type = scenario_type,
            excluded_eps_codename = excluded_eps_codename
          ) %>%
          select(
            composition_type,
            scenario_type,
            excluded_eps_codename,
            group,
            n,
            denom,
            pct
          )
      }
      
      return(out)
    }
  }
  
  # ----------------------------------------------------------
  # 3) Full-pool baseline: no Geomarket excluded
  # ----------------------------------------------------------
  
  all_pool <- make_one_composition(
    df = df_base,
    scenario_type = "all",
    excluded_eps_codename = "All Geomarkets"
  )
  
  # ----------------------------------------------------------
  # 4) Exclusion scenarios: remove one Geomarket at a time
  # ----------------------------------------------------------
  
  eps_to_exclude <- df_base %>%
    distinct(eps_codename) %>%
    arrange(eps_codename) %>%
    pull(eps_codename)
  
  exclusion_pool <- map_dfr(
    eps_to_exclude,
    function(this_eps_codename) {
      
      df_excluded <- df_base %>%
        filter(eps_codename != this_eps_codename)
      
      make_one_composition(
        df = df_excluded,
        scenario_type = "exclusion",
        excluded_eps_codename = this_eps_codename
      )
    }
  )
  
  # ----------------------------------------------------------
  # 5) Return clean long table for plotting
  # ----------------------------------------------------------
  
  bind_rows(all_pool, exclusion_pool) %>%
    left_join(removed_n_df, by = "excluded_eps_codename") %>%
    mutate(
      removed_n = if_else(
        scenario_type == "all",
        0L,
        removed_n
      ),
      removed_pct = if_else(
        scenario_type == "all",
        0,
        removed_pct
      ),
      full_pool_n = full_pool_n,
      remaining_pool_n = full_pool_n - removed_n,
      scenario_label = if_else(
        scenario_type == "all",
        "All Geomarkets",
        str_c("Excluding ", excluded_eps_codename)
      ),
      excluded_eps_codename = factor(
        excluded_eps_codename,
        levels = c("All Geomarkets", eps_to_exclude)
      ),
      scenario_label = factor(
        scenario_label,
        levels = c("All Geomarkets", str_c("Excluding ", eps_to_exclude))
      )
    ) %>%
    select(
      composition_type,
      scenario_type,
      excluded_eps_codename,
      scenario_label,
      group,
      n,
      denom,
      pct,
      full_pool_n,
      removed_n,
      removed_pct,
      remaining_pool_n
    ) %>%
    arrange(
      composition_type,
      excluded_eps_codename,
      group
    )
}

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
firstgen_col_levels <- c(
  "All (first-gen known)",
  "First-gen",
  "Not first-gen"
)

firstgen_row_levels <- c(
  "First-gen",
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
    "r_first_gen" = "First-gen",
    "r_no_col" = "No college",
    "r_some_col" = "Some college",
    "r_not_first" = "Not first-gen",
    
    "c_first_gen" = "First-gen",
    "c_no_col" = "No college",
    "c_some_col" = "Some college",
    "c_not_first" = "Not first-gen",
    "c_known" = "All (first-gen known)",
    
    "stu_first_gen" = "First-gen",
    "stu_no_col" = "No college",
    "stu_some_col" = "Some college",
    "stu_not_first" = "Not first-gen",
    "stu_known" = "All (first-gen known)"
  )
}


############## CREATE GRAPH FOR REVISED RQ3 EXCLUSION COMPOSITION

create_rq3_firstgen_dualaxis_plot <- function(
    rq3_df,
    order_ids_this,
    title = NULL,
    subtitle = NULL,
    include_n_in_label = FALSE,
    include_removed_pct_in_label = FALSE,
    point_label = c("pct", "delta", "none")
) {
  
  point_label <- match.arg(point_label)
  
  # ----------------------------------------------------------
  # 1) Filter to one order group and the first-gen group
  # ----------------------------------------------------------
  
  df_plot <- rq3_df %>%
    filter(
      order_ids == order_ids_this,
      composition_type == "firstgen",
      group == "First-gen"
    )
  
  if (nrow(df_plot) == 0) {
    stop("No rows found. Check order_ids_this and confirm rq3_df is the firstgen table.")
  }
  
  baseline_pct <- df_plot %>%
    filter(scenario_type == "all") %>%
    pull(pct) %>%
    unique()
  
  if (length(baseline_pct) != 1) {
    stop("Could not identify a unique full-pool baseline.")
  }
  
  df_plot <- df_plot %>%
    filter(scenario_type == "exclusion") %>%
    mutate(
      delta_pp = pct - baseline_pct,
      scenario_label_clean = as.character(excluded_eps_codename)
    )
  
  # ----------------------------------------------------------
  # 2) Build y-axis labels
  # ----------------------------------------------------------
  
  df_plot <- df_plot %>%
    mutate(
      scenario_label_n = case_when(
        include_n_in_label & include_removed_pct_in_label ~
          str_c(
            scenario_label_clean,
            " (n=", scales::comma(denom),
            "; removed ", scales::percent(removed_pct / 100, accuracy = 1),
            ")"
          ),
        
        include_n_in_label & !include_removed_pct_in_label ~
          str_c(
            scenario_label_clean,
            " (n=", scales::comma(denom), ")"
          ),
        
        !include_n_in_label & include_removed_pct_in_label ~
          str_c(
            scenario_label_clean,
            " (removed ", scales::percent(removed_pct / 100, accuracy = 1),
            ")"
          ),
        
        TRUE ~ scenario_label_clean
      )
    )
  
  scenario_levels <- df_plot %>%
    arrange(delta_pp) %>%
    pull(scenario_label_n)
  
  # Most negative change appears at top
  df_plot <- df_plot %>%
    mutate(
      scenario_label_n = factor(
        scenario_label_n,
        levels = rev(scenario_levels)
      )
    )
  
  # ----------------------------------------------------------
  # 3) Title/subtitle defaults
  # ----------------------------------------------------------
  
  if (is.null(title)) {
    
    metro_name <- df_plot$metro[1] %>%
      str_replace_all("_", " ") %>%
      tools::toTitleCase()
    
    if (metro_name == "Bay Area") {
      title <- str_c(
        "Change in first-gen share after excluding each Geomarket, ",
        metro_name
      )
    } else {
      title <- str_c(
        "Change in first-gen share after excluding each Geomarket, ",
        metro_name,
        " area"
      )
    }
  }
  
  if (is.null(subtitle)) {
    subtitle <- str_c(
      df_plot$test_range[1],
      " \u2014 full-pool baseline = ",
      scales::number(baseline_pct, accuracy = 0.1),
      "% first-gen"
    )
  }
  
  # ----------------------------------------------------------
  # 4) Axis limits
  # ----------------------------------------------------------
  
  x_vals <- c(df_plot$delta_pp, 0)
  x_min <- min(x_vals, na.rm = TRUE)
  x_max <- max(x_vals, na.rm = TRUE)
  x_pad <- max(0.75, (x_max - x_min) * 0.12)
  
  x_limits <- c(x_min - x_pad, x_max + x_pad)
  
  # ----------------------------------------------------------
  # 5) Optional point labels
  # ----------------------------------------------------------
  
  df_plot <- df_plot %>%
    mutate(
      point_label_text = case_when(
        point_label == "pct" ~ str_c(scales::number(pct, accuracy = 0.1), "%"),
        point_label == "delta" & delta_pp > 0 ~ str_c("+", scales::number(delta_pp, accuracy = 0.1)),
        point_label == "delta" ~ scales::number(delta_pp, accuracy = 0.1),
        TRUE ~ NA_character_
      ),
      hjust_val = if_else(delta_pp >= 0, -0.15, 1.15)
    )
  
  # ----------------------------------------------------------
  # 6) Plot
  # ----------------------------------------------------------
  
  plot <- ggplot(
    df_plot,
    aes(
      x = delta_pp,
      y = scenario_label_n
    )
  ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.6
    ) +
    geom_segment(
      aes(
        x = 0,
        xend = delta_pp,
        y = scenario_label_n,
        yend = scenario_label_n
      ),
      linewidth = 0.8,
      color = "gray35"
    ) +
    geom_point(
      size = 2.8,
      color = "gray20"
    ) +
    scale_x_continuous(
      limits = x_limits,
      labels = function(x) {
        ifelse(
          x > 0,
          str_c("+", scales::number(x, accuracy = 0.1)),
          scales::number(x, accuracy = 0.1)
        )
      },
      sec.axis = sec_axis(
        ~ . + baseline_pct,
        name = "First-gen share of remaining visible pool (%)",
        labels = function(x) scales::number(x, accuracy = 0.1)
      )
    ) +
    labs(
      x = "Percentage-point change from full-pool baseline",
      y = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 11),
      axis.title.x = element_text(size = 12),
      axis.title.x.top = element_text(size = 12, face = "bold"),
      axis.text.x.top = element_text(size = 11),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  if (point_label != "none") {
    plot <- plot +
      geom_text(
        aes(
          label = point_label_text,
          hjust = hjust_val
        ),
        size = 3.6,
        color = "gray15"
      )
  }
  
  # Store title/subtitle as metadata so the run script can save them separately.
  attr(plot, "figure_title") <- title
  attr(plot, "figure_subtitle") <- subtitle
  
  return(plot)
}

###########
###########
###########
  

create_rq3_race_delta_facet_plot <- function(
    rq3_df,
    order_ids_this,
    title = NULL,
    subtitle = NULL,
    race_groups_to_keep = c(
      "White, non-Hispanic",
      "Asian, non-Hispanic",
      "Black, non-Hispanic",
      "Hispanic"
    ),
    point_label = TRUE,
    point_size = 2.0,
    label_nudge = 0.1
) {
  
  # ----------------------------------------------------------
  # 1) Filter to one order group and race composition
  # ----------------------------------------------------------
  
  df_plot <- rq3_df %>%
    filter(
      order_ids == order_ids_this,
      composition_type == "race",
      group %in% race_groups_to_keep
    )
  
  if (nrow(df_plot) == 0) {
    stop("No rows found. Check order_ids_this and confirm rq3_df is the race table.")
  }
  
  # ----------------------------------------------------------
  # 2) Compute baseline and delta
  # ----------------------------------------------------------
  
  baseline_df <- df_plot %>%
    filter(scenario_type == "all") %>%
    select(
      group,
      baseline_pct = pct
    )
  
  df_plot <- df_plot %>%
    filter(scenario_type == "exclusion") %>%
    left_join(baseline_df, by = "group") %>%
    mutate(
      delta_pp = pct - baseline_pct,
      excluded_label = as.character(excluded_eps_codename)
    )
  
  # ----------------------------------------------------------
  # 3) Race/facet order and within-facet sorting
  # ----------------------------------------------------------
  
  df_plot <- df_plot %>%
    mutate(
      group = factor(group, levels = race_groups_to_keep),
      facet_label = str_c(
        group,
        " (full pool = ",
        scales::number(baseline_pct, accuracy = 0.1),
        "%)"
      )
    )
  
  facet_levels <- df_plot %>%
    distinct(group, facet_label) %>%
    arrange(group) %>%
    pull(facet_label)
  
  df_plot <- df_plot %>%
    mutate(
      facet_label = factor(facet_label, levels = facet_levels)
    )
  
  # Create unique y-axis labels so each facet can be sorted independently.
  # The visible label removes the "___racegroup" suffix later.
  y_levels <- df_plot %>%
    arrange(group, delta_pp) %>%
    mutate(y_label = str_c(excluded_label, "___", group)) %>%
    pull(y_label)
  
  df_plot <- df_plot %>%
    mutate(
      y_label = str_c(excluded_label, "___", group),
      y_label = factor(y_label, levels = rev(y_levels)),
      delta_label = if_else(
        delta_pp > 0,
        str_c("+", scales::number(delta_pp, accuracy = 0.1)),
        scales::number(delta_pp, accuracy = 0.1)
      ),
      label_x = if_else(
        delta_pp >= 0,
        delta_pp + label_nudge,
        delta_pp - label_nudge
      ),
      hjust_val = if_else(delta_pp >= 0, 0, 1)
    )
  
  # ----------------------------------------------------------
  # 4) Title/subtitle defaults
  # ----------------------------------------------------------
  
  if (is.null(title)) {
    
    metro_name <- df_plot$metro[1] %>%
      str_replace_all("_", " ") %>%
      tools::toTitleCase()
    
    if (metro_name == "Bay Area") {
      title <- str_c(
        "Change in racial/ethnic composition after excluding each Geomarket, ",
        metro_name
      )
    } else {
      title <- str_c(
        "Change in racial/ethnic composition after excluding each Geomarket, ",
        metro_name,
        " area"
      )
    }
  }
  
  if (is.null(subtitle)) {
    subtitle <- df_plot$test_range[1]
  }
  
  # ----------------------------------------------------------
  # 5) Axis limits
  # ----------------------------------------------------------
  
  x_vals <- c(df_plot$delta_pp, df_plot$label_x, 0)
  x_min <- min(x_vals, na.rm = TRUE)
  x_max <- max(x_vals, na.rm = TRUE)
  x_pad <- max(0.75, (x_max - x_min) * 0.12)
  
  x_limits <- c(x_min - x_pad, x_max + x_pad)
  
  # ----------------------------------------------------------
  # 6) Plot
  # ----------------------------------------------------------
  
  plot <- ggplot(
    df_plot,
    aes(
      x = delta_pp,
      y = y_label
    )
  ) +
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.6
    ) +
    geom_segment(
      aes(
        x = 0,
        xend = delta_pp,
        y = y_label,
        yend = y_label
      ),
      linewidth = 0.75,
      color = "gray35"
    ) +
    geom_point(
      size = point_size,
      color = "gray20"
    ) +
    facet_wrap(
      facets = vars(facet_label),
      ncol = 1,
      scales = "free_y",
      strip.position = "top"
    ) +
    scale_y_discrete(
      labels = function(x) str_remove(x, "___.*$")
    ) +
    scale_x_continuous(
      limits = x_limits,
      labels = function(x) {
        ifelse(
          x > 0,
          str_c("+", scales::number(x, accuracy = 0.1)),
          scales::number(x, accuracy = 0.1)
        )
      }
    ) +
    labs(
      x = "Percentage-point change from full-pool baseline",
      y = NULL
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(
        size = 12,
        face = "bold",
        hjust = 0
      ),
      strip.background = element_blank(),
      axis.text.y = element_text(size = 10.5),
      axis.text.x = element_text(size = 11),
      axis.title.x = element_text(size = 12),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.spacing.y = unit(0.9, "lines")
    )
  
  if (point_label) {
    plot <- plot +
      geom_text(
        aes(
          x = label_x,
          label = delta_label,
          hjust = hjust_val
        ),
        size = 3.3,
        color = "gray15"
      )
  }
  
  # Store title/subtitle as metadata so the run script can save them separately.
  attr(plot, "figure_title") <- title
  attr(plot, "figure_subtitle") <- subtitle
  
  return(plot)
}

##------------------------------------------
## B) Main function with Approach A (top→bottom + fct_rev)
##------------------------------------------

create_sim_eps_graph <- function(data_graph,
                                 ord_nums_graph,
                                 eps_codes_graph,
                                 variable = c("race", "firstgen"),
                                 title = "",
                                 firstgen_detail = c("collapsed", "disaggregated")) {
  
  variable <- match.arg(variable)
  firstgen_detail <- match.arg(firstgen_detail)
  
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
      dplyr::filter(!group %in% c("AIAN, non-Hispanic", "NHPI, non-Hispanic", "Two+, non-Hispanic"))
    
    row_plot_title   <- "Race Distribution Within Each EPS Code (Row %)"
    col_plot_title   <- "Distribution of Each Race Across EPS Codes (Column %)"
    fill_legend      <- "Geomarket"        # for the column plot
    row_legend_title <- "Race/ethnicity"   # for the row plot
    
  } else {  # variable == "firstgen"
    
    table_list <- create_sim_eps_firstgen_table(
      data            = data_graph,
      ord_nums        = ord_nums_graph,
      eps_codes       = eps_codes_graph,
      firstgen_detail = firstgen_detail
    )
    
    if (firstgen_detail == "collapsed") {
      
      row_cols <- c("r_first_gen", "r_not_first")
      col_cols <- c("c_known", "c_first_gen", "c_not_first")
      
      firstgen_row_levels_this <- c(
        "First-gen",
        "Not first-gen"
      )
      
      firstgen_col_levels_this <- c(
        "All (first-gen known)",
        "First-gen",
        "Not first-gen"
      )
      
    } else {
      
      row_cols <- c("r_no_col", "r_some_col", "r_not_first")
      col_cols <- c("c_known", "c_no_col", "c_some_col", "c_not_first")
      
      firstgen_row_levels_this <- c(
        "No college",
        "Some college",
        "Not first-gen"
      )
      
      firstgen_col_levels_this <- c(
        "All (first-gen known)",
        "No college",
        "Some college",
        "Not first-gen"
      )
    }
    
    df_r <- table_list[[2]] %>%
      pivot_longer(
        cols      = all_of(row_cols),
        names_to  = "group",
        values_to = "value"
      ) %>%
      mutate(
        group = recode_firstgen(group),
        group = factor(group, levels = firstgen_row_levels_this)
      )
    
    all_eps <- unique(df_r$eps_codename)
    new_eps <- move_all_to_top_in_row(all_eps)
    
    df_r <- df_r %>%
      mutate(
        eps_codename = factor(eps_codename, levels = new_eps)
      )
    
    df_c <- table_list[[3]] %>%
      pivot_longer(
        cols      = all_of(col_cols),
        names_to  = "group",
        values_to = "value"
      ) %>%
      filter(eps_codename != "All") %>%
      mutate(
        group = recode_firstgen(group),
        group = factor(group, levels = firstgen_col_levels_this),
        eps_codename = factor(eps_codename)
      ) %>%
      mutate(
        group = forcats::fct_rev(group),
        eps_codename = forcats::fct_rev(eps_codename)
      )
    
    if (firstgen_detail == "collapsed") {
      
      totals <- table_list[[1]] %>%
        filter(eps_codename == "All") %>%
        pivot_longer(
          cols      = c(stu_known, stu_first_gen, stu_not_first),
          names_to  = "group",
          values_to = "total_group"
        ) %>%
        select(-stu_all, -stu_unknown, -eps_codename) %>%
        mutate(
          group = recode_firstgen(group),
          total_group = scales::comma(total_group, accuracy = 1)
        )
      
    } else {
      
      totals <- table_list[[1]] %>%
        filter(eps_codename == "All") %>%
        pivot_longer(
          cols      = c(stu_known, stu_no_col, stu_some_col, stu_not_first),
          names_to  = "group",
          values_to = "total_group"
        ) %>%
        select(-stu_all, -stu_unknown, -eps_codename) %>%
        mutate(
          group = recode_firstgen(group),
          total_group = scales::comma(total_group, accuracy = 1)
        )
    }
    
    df_c <- df_c %>%
      left_join(totals, by = "group") %>%
      mutate(
        group = factor(group, levels = rev(firstgen_col_levels_this)),
        group_label = factor(
          paste0(group, "\n(N = ", total_group, ")"),
          levels = rev(
            paste0(firstgen_col_levels_this, "\n(N = ", totals$total_group, ")")
          )
        )
      )
    
    row_plot_title   <- "First-generation Status Within Each EPS Code (Row %)"
    col_plot_title   <- "Distribution of Each First-generation Status Group Across EPS Codes (Column %)"
    fill_legend      <- "Geomarket"
    row_legend_title <- "First-generation status"
  }
  
  # (A) Append the user-supplied title if provided
  if (!is.null(title) && nzchar(title)) {
    row_plot_title <- paste0(title)
    col_plot_title <- paste0(title)
  }
  
  # (B) For the row plot, build the "eps_label" with sample size
  known_col <- if (variable == "race") "race_known" else "stu_known"
  
  df_r <- df_r %>%
    dplyr::group_by(eps_codename) %>%
    dplyr::mutate(
      eps_label = paste0(
        as.character(eps_codename),
        " (n=",
        formattable::comma(dplyr::first(.data[[known_col]]), digits = 0),
        ")"
      )
    ) %>%
    dplyr::ungroup()
  
  # ----------------------------------------------------------------
  # (C) Build the Row-Percent Plot (plot_r)
  # ----------------------------------------------------------------
  plot_r <- df_r %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x    = forcats::fct_rev(eps_label),
        y    = value,
        fill = group
      )
    ) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_fill(reverse = TRUE)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    ggplot2::labs(
      title = row_plot_title,
      x     = NULL,
      y     = NULL,
      fill  = row_legend_title
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.margin       = ggplot2::margin(t = 20, r = 0, b = 20, l = 20),
      legend.box.spacing = grid::unit(0, "line"),
      legend.text       = ggplot2::element_text(size = 14),
      legend.title      = ggplot2::element_text(size = 14),
      axis.text.y       = ggplot2::element_text(size = 13),
      axis.text.x       = ggplot2::element_text(size = 13),
      strip.text        = ggplot2::element_text(size = 14),
      plot.title        = ggplot2::element_text(size = 16, face = "bold")
    ) +
    ggplot2::coord_flip(clip = "off")
  
  # ----------------------------------------------------------------
  # (D) Build the Column-Percent Plot (plot_c) with percentage labels
  # ----------------------------------------------------------------
  plot_c <- df_c %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x    = group_label,
        y    = value,
        fill = eps_codename
      )
    ) +
    # Dodge bars
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.9)) +
    # Add text labels with e.g. "37%"
    ggplot2::geom_text(
      ggplot2::aes(
        label = scales::percent(value/100, accuracy = 1)
      ),
      position = ggplot2::position_dodge(width = 0.9),
      vjust = 0.5,    # vertically center the text in the bar
      hjust = -0.2,   # shift it slightly to the right
      size  = 4
    ) +
    ggplot2::labs(
      title = col_plot_title,
      x     = NULL,
      y     = NULL,
      fill  = fill_legend
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(scale = 1),
      expand = ggplot2::expansion(mult = c(0, 0.2))  # add space on right for text
    ) +
    ggplot2::scale_fill_discrete(
      guide = ggplot2::guide_legend(reverse = TRUE)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.text        = ggplot2::element_text(size = 14),
      legend.title       = ggplot2::element_text(size = 14),
      axis.text.x        = ggplot2::element_text(size = 13),
      axis.text.y        = ggplot2::element_text(size = 13),
      strip.text         = ggplot2::element_text(size = 14),
      axis.text.x.top    = ggplot2::element_text(size = 13),
      plot.title         = ggplot2::element_text(size = 16, face = "bold"),
      panel.grid.major.x = ggplot2::element_blank()
    ) +
    ggplot2::coord_flip(clip = "off")
  
  # (E) Return both plots as a list
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


create_race_by_firstgen_graph <- function(
    data,
    ord_nums,
    eps_codes,
    metro_name,
    exclude_race = c(1, 8),
    title_suf = "",
    firstgen_detail = c("collapsed", "disaggregated")
) {
  
  firstgen_detail <- match.arg(firstgen_detail)
  
  # ----------------------------------------------------------
  # 0) Set first-gen columns and labels
  # ----------------------------------------------------------
  
  if (firstgen_detail == "collapsed") {
    
    firstgen_cols <- c("row_first_gen", "row_not_first_gen")
    
    custom_group_order <- c(
      "all",
      "row_first_gen",
      "row_not_first_gen"
    )
    
    custom_group_labels <- c(
      "All",
      "First-gen",
      "Not first-gen"
    )
    
  } else {
    
    firstgen_cols <- c("row_no_col", "row_some_col", "row_not_first_gen")
    
    custom_group_order <- c(
      "all",
      "row_no_col",
      "row_some_col",
      "row_not_first_gen"
    )
    
    custom_group_labels <- c(
      "All",
      "No college",
      "Some college",
      "Not first-gen"
    )
  }
  
  # ----------------------------------------------------------
  # 1) Build the main data frame
  # ----------------------------------------------------------
  
  df <- create_sim_eps_race_firstgen_table(
    data            = data, 
    ord_nums        = ord_nums, 
    eps_codes       = eps_codes,
    exclude_race    = exclude_race,
    firstgen_detail = firstgen_detail
  ) %>% 
    pivot_longer(
      cols      = c("all", all_of(firstgen_cols)),
      names_to  = "group",
      values_to = "value"
    ) %>%
    mutate(
      stu_race_cb = factor(
        stu_race_cb, 
        levels = names(custom_race_labels), 
        labels = custom_race_labels
      ),
      group = factor(
        group, 
        levels = custom_group_order, 
        labels = custom_group_labels
      ),
      eps_codename = factor(
        eps_codename,
        levels = rev(unique(eps_codename))
      )
    )
  
  # ----------------------------------------------------------
  # 2) Separate the "All" group totals from the subgroups
  # ----------------------------------------------------------
  
  df_plot <- df %>% 
    filter(group != "All")
  
  df_totals <- df %>%
    filter(group == "All") %>%
    distinct(stu_race_cb, eps_codename, value) %>%
    rename(total_n = value)
  
  # ----------------------------------------------------------
  # 3) Attach total_n to each row in df_plot
  # ----------------------------------------------------------
  
  df_plot <- df_plot %>%
    left_join(df_totals, by = c("stu_race_cb", "eps_codename")) %>%
    distinct() %>%
    mutate(
      eps_label = str_c(
        eps_codename, 
        " (n=", format(total_n, big.mark = ",", trim = TRUE), ")"
      ),
      eps_label_factor = factor(eps_label, levels = rev(unique(eps_label)))
    )
  
  # ----------------------------------------------------------
  # 4) Create the ggplot
  # ----------------------------------------------------------
  
  plot <- ggplot(
    df_plot,
    aes(
      x    = eps_label_factor,
      y    = value,
      fill = group
    )
  ) +
    geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) +
    coord_flip(clip = "off") +
    facet_grid(
      rows = vars(stu_race_cb),
      switch = "y",
      scales = "free_y",
      space  = "free_y"
    ) +
    scale_x_discrete(drop = TRUE) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = NULL,
      x     = NULL,
      y     = NULL,
      fill  = "Parental education"
    ) +
    theme_minimal() +
    theme(
      strip.text.y.left = element_text(size = 12, face = "plain", angle = 0, hjust = 0),
      strip.placement   = "outside",
      panel.spacing     = unit(0.2, "lines"),
      axis.text.y       = element_text(size = 10.5),
      plot.title        = element_text(hjust = 0.5),
      legend.position   = "right",
      legend.key.size   = unit(0.8, "lines"),
      legend.title      = element_text(size = 12, face = "bold"),
      legend.text       = element_text(size = 12),
      axis.text.x       = element_text(size = 12)
    )
  
  # ----------------------------------------------------------
  # 5) Figure file name: use underscores
  # ----------------------------------------------------------
  
  if (is.na(metro_name) || metro_name == "") {
    metro <- str_replace(
      deparse(substitute(eps_codes)),
      pattern = "_eps_codes",
      replacement = ""
    )
  } else {
    metro <- str_replace_all(metro_name, " ", "_")
  }
  
  plot_name <- str_c(
    "rq2b", 
    metro,
    "order",
    str_c(ord_nums, collapse = "_"),
    "race_by_firstgen",
    sep = "_"
  )
  
  writeLines(plot_name)
  
  # ----------------------------------------------------------
  # 6) Build the figure title with a friendly metro name
  # ----------------------------------------------------------
  
  friendly_metro_name <- gsub("_", " ", metro_name, fixed = TRUE)
  friendly_metro_name <- tools::toTitleCase(friendly_metro_name)
  
  suffix_part <- if (nzchar(title_suf)) str_c(", ", title_suf) else ""
  
  if (friendly_metro_name == "Bay Area") {
    figure_title <- str_c(
      "First-generation status by race for ",
      friendly_metro_name,
      " Geomarkets",
      suffix_part
    )
  } else {
    figure_title <- str_c(
      "First-generation status by race for ",
      friendly_metro_name,
      " area Geomarkets",
      suffix_part
    )
  }
  
  writeLines(figure_title)
  
  # ----------------------------------------------------------
  # 7) Write title
  # ----------------------------------------------------------
  
  writeLines(
    figure_title,
    file.path(graphs_dir, "rq2b", str_c(plot_name, "_title.txt"))
  )
  
  # ----------------------------------------------------------
  # 8) Save the plot
  # ----------------------------------------------------------
  
  ggsave(
    filename = file.path(graphs_dir, "rq2b", str_c(plot_name, ".png")),
    plot     = plot,
    width    = 14,
    height   = 8,
    bg       = "white"
  )
  
  # ----------------------------------------------------------
  # 9) Notes
  # ----------------------------------------------------------
  
  note_text <- c(
    "Figure Notes:",
    "- Excludes students who have missing values for race or parental education"
  )
  
  if (exists("orders_df", envir = .GlobalEnv)) {
    
    orders_df_global <- get("orders_df", envir = .GlobalEnv)
    orders_str <- str_c(ord_nums, collapse = "_")
    
    row_match <- orders_df_global %>%
      filter(order_ids == orders_str)
    
    if (nrow(row_match) > 0) {
      figure_note_local <- row_match$figure_note[1]
      note_text[2] <- str_c(note_text[2], ". ", figure_note_local)
    }
  }
  
  note_text <- str_c(note_text, ".")
  
  writeLines(
    note_text,
    file.path(graphs_dir, "rq2b", str_c(plot_name, "_note.txt"))
  )
  
  return(plot)
}
#create_race_by_firstgen_graph(data_graph,ord_nums_graph,eps_codes_graph,metro_name = '',exclude_race_graph = c(1,8,12),title_suf = '')
  
#create_race_by_firstgen_graph(lists_orders_zip_hs_df_sf, c('487984'), chi_eps_codes, 'chicago', exclude_race = c(1,8,12), title_suf = ', SAT score 1020 - 1150')

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
