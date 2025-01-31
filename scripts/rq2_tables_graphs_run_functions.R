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

# script that creates functions to create tables and graphs
source(file = file.path(scripts_dir, 'rq2_tables_graphs_create_functions.R'))

###########################
############################ RUN FUNCTION create_sim_eps_graph TO CREATE GRAPH OF RACE AND GRAPH OF FIRSTGEN
###########################

all_orders <- list(
  chicago = c('chicago', 'chi_eps_codes', 'SAT score 1020 - 1150', '487984'),
  chicago = c('chicago', 'chi_eps_codes', 'SAT score 1160 - 1600', '488035', '488053'),
  philadelphia = c('philadelphia', 'philly_eps_codes', 'PSAT score 1070 - 1180', '448922'),
  philadelphia = c('philadelphia', 'philly_eps_codes', 'PSAT score 1190 - 1520', '448427', '448440')
)
all_orders %>% str()

for (orders in all_orders) {
  
  metro_name_graph <- orders[1]
  eps_codes_graph <- get(orders[2])  # First element as eps_codes_graph
  graph_title <- orders[3]  # second element as graph title
  ord_nums_graph <- orders[-c(1, 2, 3)]  # Remaining elements as ord_nums_graph
  
  for (g in c('race', 'firstgen')) {
    
    plot_name <- str_c('rq2',metro_name_graph, g, "plot_order", str_c(ord_nums_graph, collapse = "_"), sep = '_')
    writeLines(str_c(plot_name))
    
    # Remove the object if it exists
    if (exists(plot_name, envir = .GlobalEnv)) {
      rm(list = plot_name, envir = .GlobalEnv)
    }
    
    # Create the object
    assign(
      plot_name,
      create_sim_eps_graph(
        data_graph    = lists_orders_zip_hs_df_sf,
        ord_nums_graph = ord_nums_graph,
        eps_codes_graph = eps_codes_graph,
        variable = g,
        title = graph_title
      )
    )
  }
}

### Philly
rq2_philadelphia_race_plot_order_448922$plot_r
rq2_philadelphia_race_plot_order_448922$plot_c

rq2_philadelphia_firstgen_plot_order_448922$plot_r
rq2_philadelphia_firstgen_plot_order_448922$plot_c

rq2_philadelphia_race_plot_order_448427_448440$plot_r
rq2_philadelphia_race_plot_order_448427_448440$plot_c

rq2_philadelphia_firstgen_plot_order_448427_448440$plot_r
rq2_philadelphia_firstgen_plot_order_448427_448440$plot_c




# combined graphs
# race row pct. for lower SAT score and higher SAT score
rq2_philadelphia_race_row_plot <- rq2_philadelphia_race_plot_order_448922$plot_r + rq2_philadelphia_race_plot_order_448427_448440$plot_r + plot_layout(ncol = 1)
# race column pct. for lower SAT score and higher SAT score
rq2_philadelphia_race_col_plot <- rq2_philadelphia_race_plot_order_448922$plot_c + rq2_philadelphia_race_plot_order_448427_448440$plot_c + plot_layout(ncol = 1)

# firstgen row pct. for lower SAT score and higher SAT score
rq2_philadelphia_firstgen_row_plot <- rq2_philadelphia_firstgen_plot_order_448922$plot_r + rq2_philadelphia_firstgen_plot_order_448427_448440$plot_r + plot_layout(ncol = 1)
# firstgen column pct. for lower SAT score and higher SAT score
rq2_philadelphia_firstgen_col_plot <- rq2_philadelphia_firstgen_plot_order_448922$plot_c + rq2_philadelphia_firstgen_plot_order_448427_448440$plot_c + plot_layout(ncol = 1)

# loop that saves combined graphs as .png and creates title.txt and note.txt
for (m in c('philadelphia')) {
  
  for (g in c('race', 'firstgen')) {
    
    for (rc in c('row', 'col')) {    
      plot_name <- str_c('rq2',m,g,rc,'plot', sep = '_')    
      
      writeLines(plot_name)

      # Retrieve the actual plot object
      plot_obj <- get(plot_name, envir = .GlobalEnv)  # Fetch the ggplot object
      
      # Save the combined plot to disk
      ggsave(
        filename = file.path(graphs_dir, str_c(plot_name,'.png', sep = '')),
        plot = plot_obj,
        width = 16,
        height = 10,  # Adjusted height to accommodate the combined plots
        bg = 'white'
      )
      
      # Define the figure title
      figure_title <- str_c(
        'metro =', m, 'variable=', g, rc, 'percent', sep = ' '
      )
      
      writeLines(figure_title)
      
      # Save the title to a .txt file
      writeLines(figure_title, file.path(graphs_dir, str_c(plot_name, '_title.txt')))  
 
      # 2) Create the text you want to store [REVISE NOTE TEXT LATER!]
      note_text <- c(
        "Figure Notes:",
        "- Median income is shown in thousands ($k).",
        "- '% in poverty' is the share of individuals below the federal poverty line.",
        "- Data sources: US Census, etc."
      )
      
      # 3) Write that text to a file
      writeLines(note_text, file.path(graphs_dir, str_c(plot_name, '_note.txt')))  
      
    }
  }
}
  
# Save the combined plot to disk
ggsave(
  filename = file.path(graphs_dir, "rq2_philadelphia_race_rowpct_low_high_psat.png"),
  plot = combined_plot,
  width = 16,
  height = 10,  # Adjusted height to accommodate the combined plots
  bg = 'white'
)

#### chicago

chicago_race_plot_order_487984$plot_r
chicago_race_plot_order_487984$plot_c

chicago_firstgen_plot_order_487984$plot_r
chicago_firstgen_plot_order_487984$plot_c

chicago_race_plot_order_488035_488053$plot_r
chicago_race_plot_order_488035_488053$plot_c

chicago_firstgen_plot_order_488035_488053$plot_r
chicago_firstgen_plot_order_488035_488053$plot_c

# combined graphs
# race row pct. for lower SAT score and higher SAT score
chicago_race_plot_order_487984$plot_r + chicago_race_plot_order_488035_488053$plot_r + plot_layout(ncol = 1)
# firstgen row pct. for lower SAT score and higher SAT score
chicago_firstgen_plot_order_487984$plot_r + chicago_firstgen_plot_order_488035_488053$plot_r + plot_layout(ncol = 1)
# race column pct. for lower SAT score and higher SAT score
chicago_race_plot_order_487984$plot_c + chicago_race_plot_order_488035_488053$plot_c + plot_layout(ncol = 1)
# firstgen column pct. for lower SAT score and higher SAT score
chicago_firstgen_plot_order_487984$plot_c + chicago_firstgen_plot_order_488035_488053$plot_c + plot_layout(ncol = 1)

###########################
############################ RUN FUNCTION create_race_by_firstgen_graph TO CREATE GRAPH OF RACE X FIRSTGEN
###########################

# 1 487984  16926 # Illinois standard 2020; ordered 7/19/2019; HS class 2020, 2021; IL; SAT 1020-1150;  GPA A+ to B-
# 5 488035  12842 # Illinois HS 2020; ordered 7/19/2019; HS class 2020, 2021; SAT 1160 - 1300; GPA A+ to B-
# 9 488053   7259 # Illinois GPPA 2020 (no cf);ordered 7/19/2019; HS class 2020, 2021; SAT 1310-1600; GPA A+ to B-
#create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('487984'), eps_codes = chi_eps_codes) %>% print(n=50) # 

# philly metro area
# order 448922: PSAT 1070 - 1180; order 448427: PSAT 1190 - 1260; order 448440: PSAT 1270 - 1520

create_race_by_firstgen_graph(lists_orders_zip_hs_df_sf, c('487984'), chi_eps_codes, 'chicago', exclude_race = c(1,8,12), title_suf = ', SAT score 1020 - 1150')
create_race_by_firstgen_graph(lists_orders_zip_hs_df_sf, c('488035','488053'), chi_eps_codes, 'chicago', exclude_race = c(1,8,12), title_suf = ', SAT score 1160 - 1600')

create_race_by_firstgen_graph(lists_orders_zip_hs_df_sf, c('448922'), philly_eps_codes, 'philadelphia', exclude_race = c(1,8,12), title_suf = ', SAT score 1070 - 1180')
create_race_by_firstgen_graph(lists_orders_zip_hs_df_sf, c('448427','448440'), philly_eps_codes, 'philadelphia', exclude_race = c(1,8,12), title_suf = ', SAT score 1190 - 1520')


all_orders <- list(
  chicago = c('chicago', 'chi_eps_codes', 'SAT score 1020 - 1150', '487984'),
  chicago = c('chicago', 'chi_eps_codes', 'SAT score 1160 - 1600', '488035','488053'),
  philadelphia = c('philadelphia', 'philly_eps_codes', 'PSAT score 1070 - 1180', '448922'),
  philadelphia = c('philadelphia', 'philly_eps_codes', 'PSAT score 1190 - 1520', '448427','448440')
)
all_orders %>% str()


for (orders in all_orders) {
  
  metro_name_graph <- orders[1]
    #writeLines(metro_name_graph)
  eps_codes_graph <- get(orders[2])  # First element as eps_codes_graph
    #writeLines(eps_codes_graph)
  graph_title <- orders[3]  # second element as graph title
    #writeLines(graph_title)  
  ord_nums_graph <- orders[-c(1, 2, 3)]  # Remaining elements as ord_nums_graph
    #writeLines(ord_nums_graph)
  
  create_race_by_firstgen_graph(
    data_graph = lists_orders_zip_hs_df_sf, 
    ord_nums_graph = ord_nums_graph, 
    eps_codes_graph = eps_codes_graph, 
    metro_name = metro_name_graph, 
    exclude_race = c(1,8,12), 
    title_suf = graph_title
    )
  
}

#create_race_by_firstgen_graph(data_graph,ord_nums_graph,eps_codes_graph,metro_name = '',exclude_race_graph = c(1,8,12),title_suf = '')






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

create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('487984'), eps_codes = chi_eps_codes) %>% print(n=70) # 
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('487984'), eps_codes = chi_eps_codes, exclude_race = '') %>% print(n=70) # 
create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('487984'), eps_codes = chi_eps_codes, exclude_race = c(1,8,12)) %>% print(n=70) # 
lists_orders_zip_hs_df_sf %>% as_tibble() %>% count(stu_race_cb)

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
  "two+ races, non-Hispanic",
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
  "two+ races, non-Hispanic",
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
    "r_multi"    = "two+ races, non-Hispanic",
    "r_aian"     = "AIAN, non-Hispanic",
    "r_nhpi"     = "NHPI, non-Hispanic",
    
    "c_white"    = "White, non-Hispanic",
    "c_asian"    = "Asian, non-Hispanic",
    "c_black"    = "Black, non-Hispanic",
    "c_hispanic" = "Hispanic",
    "c_multi"    = "two+ races, non-Hispanic",
    "c_aian"     = "AIAN, non-Hispanic",
    "c_nhpi"     = "NHPI, non-Hispanic",
    "c_known"    = "All (race known)",
    
    "stu_white"    = "White, non-Hispanic",
    "stu_asian"    = "Asian, non-Hispanic",
    "stu_black"    = "Black, non-Hispanic",
    "stu_hispanic" = "Hispanic",
    "stu_multi"    = "two+ races, non-Hispanic",
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
    
    # -- Row pivot code (unchanged logic) --
    df_r <- table_list[[2]] %>%
      tidyr::pivot_longer(
        cols = c(r_white, r_asian, r_black, r_hispanic, r_multi, r_aian, r_nhpi),
        names_to  = "group",
        values_to = "value"
      ) %>%
      dplyr::mutate(
        group = recode_race(group),
        group = factor(group, levels = race_row_levels)
      )
    
    # Reorder eps_codename so "All" is top
    all_eps <- unique(df_r$eps_codename)
    new_eps <- move_all_to_top_in_row(all_eps)
    df_r <- df_r %>%
      dplyr::mutate(
        eps_codename = factor(eps_codename, levels = new_eps)
      )
    
    # -- Column pivot code (unchanged logic) --
    df_c <- table_list[[3]] %>%
      tidyr::pivot_longer(
        cols = c(c_known, c_white, c_asian, c_black, c_hispanic, c_multi, c_aian, c_nhpi),
        names_to  = "group",
        values_to = "value"
      ) %>%
      dplyr::filter(eps_codename != "All") %>%
      dplyr::mutate(
        group = recode_race(group),
        group = factor(group, levels = race_col_levels),
        eps_codename = factor(eps_codename)
      ) %>%
      dplyr::mutate(
        group       = forcats::fct_rev(group),
        eps_codename = forcats::fct_rev(eps_codename)
      )
    
    # Totals & labeling
    totals <- table_list[[1]] %>%
      dplyr::filter(eps_codename == "All") %>%
      tidyr::pivot_longer(
        cols = c(race_known, dplyr::starts_with("stu_")),
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
        group = factor(group, levels = rev(race_col_levels)),
        group_label = factor(
          paste0(group, "\n(N = ", total_group, ")"),
          levels = rev(
            paste0(race_col_levels, "\n(N = ", totals$total_group, ")")
          )
        )
      ) %>%
      dplyr::filter(!group %in% c("AIAN, non-Hispanic", "NHPI, non-Hispanic"))
    
    # Original titles
    row_plot_title <- "Race Distribution Within Each EPS Code (Row %)"
    col_plot_title <- "Distribution of Each Race Across EPS Codes (Column %)"
    fill_legend    <- "Geomarket"
    
  } else {
    # -- FIRST-GEN BRANCH --
    table_list <- create_sim_eps_firstgen_table(
      data = data_graph,
      ord_nums = ord_nums_graph,
      eps_codes = eps_codes_graph
    )
    
    # Row pivot
    df_r <- table_list[[2]] %>%
      tidyr::pivot_longer(
        cols = c(r_no_col, r_some_col, r_not_first),
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
    
    # Column pivot
    df_c <- table_list[[3]] %>%
      tidyr::pivot_longer(
        cols = c(c_known, c_no_col, c_some_col, c_not_first),
        names_to  = "group",
        values_to = "value"
      ) %>%
      dplyr::filter(eps_codename != "All") %>%
      dplyr::mutate(
        group = recode_firstgen(group),
        group = factor(group, levels = firstgen_col_levels),
        eps_codename = factor(eps_codename)
      ) %>%
      dplyr::mutate(
        group       = forcats::fct_rev(group),
        eps_codename = forcats::fct_rev(eps_codename)
      )
    
    # Totals & labeling
    totals <- table_list[[1]] %>%
      dplyr::filter(eps_codename == "All") %>%
      tidyr::pivot_longer(
        cols = c(stu_known, stu_no_col, stu_some_col, stu_not_first),
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
        group = factor(group, levels = rev(firstgen_col_levels)),
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
    
    # Original titles
    row_plot_title <- "First-Gen Distribution Within Each EPS Code (Row %)"
    col_plot_title <- "Distribution of First-Gen Groups Across EPS Codes (Column %)"
    fill_legend    <- "Geomarket"
  }
  
  # ----------------------------------------------------------------
  # (A) Append the user-supplied 'title' to each existing plot title
  # ----------------------------------------------------------------
  if (!is.null(title) && nzchar(title)) {
    row_plot_title <- paste0(row_plot_title, " - ", title)
    col_plot_title <- paste0(col_plot_title, " - ", title)
  }
  
  # ----------------------------------------------------------------
  # 3) Build the row-percent Plot
  # ----------------------------------------------------------------
  known_col <- if (variable == "race") "race_known" else "stu_known"
  
  plot_r <- df_r %>%
    ggplot2::ggplot(ggplot2::aes(x = fct_rev(eps_codename), y = value, fill = group)) +
    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_fill(reverse = TRUE)) +
    
    ggplot2::geom_text(
      data = df_r %>% dplyr::distinct(eps_codename, .keep_all = TRUE),
      ggplot2::aes(
        x = eps_codename,
        y = 1,
        label = paste0("N=", formattable::comma(.data[[known_col]], digits = 0))
      ),
      hjust       = -0.1,
      size        = 3,
      inherit.aes = FALSE
    ) +
    
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 100)) +
    ggplot2::labs(
      title = row_plot_title,
      x     = NULL,
      y     = NULL
    ) +
    ggplot2::theme_minimal() +
    ggplot2::coord_flip(clip = "off")
  
  # ----------------------------------------------------------------
  # 4) Build the column-percent Plot
  # ----------------------------------------------------------------
  plot_c <- df_c %>%
    ggplot2::ggplot(ggplot2::aes(x = group_label, y = value, fill = eps_codename)) +
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
    ggplot2::scale_fill_discrete(guide = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x        = ggplot2::element_text(angle = 0, vjust = 1, hjust = 0.5),
      axis.title.x       = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank()
    ) +
    ggplot2::coord_flip(clip = "off")
  
  # ----------------------------------------------------------------
  # 5) Return both plots
  # ----------------------------------------------------------------
  list(plot_r = plot_r, plot_c = plot_c)
}


# 1 487984  16926 # Illinois standard 2020; ordered 7/19/2019; HS class 2020, 2021; IL; SAT 1020-1150;  GPA A+ to B-
# 5 488035  12842 # Illinois HS 2020; ordered 7/19/2019; HS class 2020, 2021; SAT 1160 - 1300; GPA A+ to B-
# 9 488053   7259 # Illinois GPPA 2020 (no cf);ordered 7/19/2019; HS class 2020, 2021; SAT 1310-1600; GPA A+ to B-

# philly metro area
# order 448922: PSAT 1070 - 1180; order 448427: PSAT 1190 - 1260; order 448440: PSAT 1270 - 1520


all_orders <- list(
  chicago = c('chicago', 'chi_eps_codes', 'SAT score 1020 - 1150', '487984'),
  chicago = c('chicago', 'chi_eps_codes', 'SAT score 1160 - 1600', '488035', '488053'),
  philadelphia = c('philadelphia', 'philly_eps_codes', 'PSAT score 1070 - 1180', '448922'),
  philadelphia = c('philadelphia', 'philly_eps_codes', 'PSAT score 1190 - 1520', '448427', '448440')
)
all_orders

for (orders in all_orders) {
  
  metro_name_graph <- orders[1]
  eps_codes_graph <- get(orders[2])  # First element as eps_codes_graph
  graph_title <- orders[3]  # second element as graph title
  ord_nums_graph <- orders[-c(1, 2, 3)]  # Remaining elements as ord_nums_graph
  
  for (g in c('race', 'firstgen')) {
    
    plot_name <- str_c(metro_name_graph, g, "plot_order", str_c(ord_nums_graph, collapse = "_"), sep = '_')
    writeLines(str_c(plot_name))
    
    # Remove the object if it exists
    if (exists(plot_name, envir = .GlobalEnv)) {
      rm(list = plot_name, envir = .GlobalEnv)
    }
    
    # Create the object
    assign(
      plot_name,
      create_sim_eps_graph(
        data_graph    = lists_orders_zip_hs_df_sf,
        ord_nums_graph = ord_nums_graph,
        eps_codes_graph = eps_codes_graph,
        variable = g,
        title = graph_title
      )
    )
  }
}

### Philly
philadelphia_race_plot_order_448922$plot_r
philadelphia_race_plot_order_448922$plot_c

philadelphia_firstgen_plot_order_448922$plot_r
philadelphia_firstgen_plot_order_448922$plot_c

philadelphia_race_plot_order_448427_448440$plot_r
philadelphia_race_plot_order_448427_448440$plot_c

philadelphia_firstgen_plot_order_448427_448440$plot_r
philadelphia_firstgen_plot_order_448427_448440$plot_c




# combined graphs
# race row pct. for lower SAT score and higher SAT score
philadelphia_race_plot_order_448922$plot_r + philadelphia_race_plot_order_448427_448440$plot_r + plot_layout(ncol = 1)
# firstgen row pct. for lower SAT score and higher SAT score
philadelphia_firstgen_plot_order_448922$plot_r + philadelphia_firstgen_plot_order_448427_448440$plot_r + plot_layout(ncol = 1)
# race column pct. for lower SAT score and higher SAT score
philadelphia_race_plot_order_448922$plot_c + philadelphia_race_plot_order_448427_448440$plot_c + plot_layout(ncol = 1)
# firstgen column pct. for lower SAT score and higher SAT score
philadelphia_firstgen_plot_order_448922$plot_c + philadelphia_firstgen_plot_order_448427_448440$plot_c + plot_layout(ncol = 1)

# saving plots to disk
# Combine the two plots using patchwork
combined_plot <- philadelphia_race_plot_order_448922$plot_r + 
  philadelphia_race_plot_order_448427_448440$plot_r + 
  patchwork::plot_layout(ncol = 1)

# Save the combined plot to disk
ggsave(
  filename = file.path(graphs_dir, "rq2_philadelphia_race_rowpct_low_high_psat.png"),
  plot = combined_plot,
  width = 16,
  height = 10,  # Adjusted height to accommodate the combined plots
  bg = 'white'
)

#### chicago

chicago_race_plot_order_487984$plot_r
chicago_race_plot_order_487984$plot_c

chicago_firstgen_plot_order_487984$plot_r
chicago_firstgen_plot_order_487984$plot_c

chicago_race_plot_order_488035_488053$plot_r
chicago_race_plot_order_488035_488053$plot_c

chicago_firstgen_plot_order_488035_488053$plot_r
chicago_firstgen_plot_order_488035_488053$plot_c

# combined graphs
# race row pct. for lower SAT score and higher SAT score
chicago_race_plot_order_487984$plot_r + chicago_race_plot_order_488035_488053$plot_r + plot_layout(ncol = 1)
# firstgen row pct. for lower SAT score and higher SAT score
chicago_firstgen_plot_order_487984$plot_r + chicago_firstgen_plot_order_488035_488053$plot_r + plot_layout(ncol = 1)
# race column pct. for lower SAT score and higher SAT score
chicago_race_plot_order_487984$plot_c + chicago_race_plot_order_488035_488053$plot_c + plot_layout(ncol = 1)
# firstgen column pct. for lower SAT score and higher SAT score
chicago_firstgen_plot_order_487984$plot_c + chicago_firstgen_plot_order_488035_488053$plot_c + plot_layout(ncol = 1)

###################################
################################### CREATE GRAPH FOR RQ2 RACE X SES
###################################


# These graphs based on the table-creating function: 
  create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('487984'), eps_codes = chi_eps_codes) %>% print(n=70) # 
  create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('488035'), eps_codes = chi_eps_codes) %>% print(n=70) # 
  create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('488053','488035'), eps_codes = chi_eps_codes) %>% print(n=70) # 

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
      fill  = NULL
    ) +
    theme_minimal() +
    theme(
      strip.text.y.left = element_text(size = 10, face = "plain", angle = 0, hjust = 0),
      strip.placement   = "outside",
      panel.spacing     = unit(0.2, "lines"),
      axis.text.y       = element_text(size = 8),
      plot.title        = element_text(hjust = 0.5),
      legend.position   = "right",
      legend.key.size   = unit(0.8, "lines"),
      legend.title      = element_text(size = 10, face = "bold"),
      legend.text       = element_text(size = 10)
    )

  # create strings for file names
  metro <- str_replace(string = deparse(substitute(eps_codes_graph)), pattern = '_eps_codes', replacement = '')
  plot_name <- str_c('rq2',metro,'order',str_c(ord_nums_graph, collapse = '_'),'race_by_firstgen', sep = '_')
  writeLines(plot_name)
  
  # Define the figure title
  figure_title <- str_c(
    "First-generation status by race for ", metro_name, " Geomarkets", title_suf
  )
  
  writeLines(figure_title)
  
  # Save the title to a .txt file
  writeLines(figure_title, file.path(graphs_dir, str_c(plot_name, '_title.txt')))  
  
  # Save plot to file
  ggsave(
    filename = file.path(graphs_dir, str_c(plot_name, '.png', sep = '_')),
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
  #writeLines(note_text, file.path(graphs_dir, str_c(file_prefix, '_', graph_type, '.txt')))
  
  # Return the plot
  return(plot)
  
}

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

create_race_by_firstgen_graph(lists_orders_zip_hs_df_sf, c('487984'), chi_eps_codes, 'Chicago', exclude_race = c(1,8,12), title_suf = ', SAT score 1020 - 1150')
create_race_by_firstgen_graph(lists_orders_zip_hs_df_sf, c('488035','488053'), chi_eps_codes, 'Chicago', exclude_race = c(1,8,12), title_suf = ', SAT score 1160 - 1600')

create_race_by_firstgen_graph(lists_orders_zip_hs_df_sf, c('448922'), philly_eps_codes, 'Philadelphia', exclude_race = c(1,8,12), title_suf = ', SAT score 1070 - 1180')



###################################
################################### PREVIOUS VERSION OF GRAPH
###################################


###################################
################################### TABLE FUNCTION CALLS FOR SPECIFIC METROS; KEEP IT AROUND CUZ IT TELLS YOU WHICH ORDERS FOR WHICH METRO ARES
###################################

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
