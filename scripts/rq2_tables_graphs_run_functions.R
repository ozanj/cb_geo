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
#CA ORDER NUMBERS

# 2 448375  33893 FA20 - CA PSAT AD {JAN19); ordered 1/8/2019; 2020 HS grad class; CA; PSAT 1070 - 1180
# 6 448420  19437; FA20 - CA PSAT SE (JAN19); ordered 1/8/2019; 2020/21 hs grad class; CA; PSAT 1190-1260
# 5 448374  19775; FA20 - CA PSAT BE (JAN19); ordered 1/8/2019; 2020/21 hs grad class; CA; PSAT 1270-1520


all_orders <- list(
  chicago = c('chicago', 'chi_eps_codes', 'SAT score 1020 - 1150', '487984'),
  chicago = c('chicago', 'chi_eps_codes', 'SAT score 1160 - 1600', '488035', '488053'),
  philadelphia = c('philadelphia', 'philly_eps_codes', 'PSAT score 1070 - 1180', '448922'),
  philadelphia = c('philadelphia', 'philly_eps_codes', 'PSAT score 1190 - 1520', '448427', '448440'),
  #los_angeles = c('los_angeles', 'los_angeles_eps_codes', 'PSAT score 1110 - 1210', '366935'), # ordered jan 2018; don't like these cutpoints as much
  #los_angeles = c('los_angeles', 'los_angeles_eps_codes', 'PSAT score 1220 - 1520', '366934', '366932'),
  los_angeles = c('los_angeles', 'los_angeles_eps_codes', 'PSAT score 1070 - 1180', '448375'), # ordered jan 2019
  los_angeles = c('los_angeles', 'los_angeles_eps_codes', 'PSAT score 1190 - 1520', '448374', '448420'),
  #los_angeles = c('los_angeles', 'los_angeles_eps_codes', 'PSAT score 1070 - 1180', '546954'), # ordered jan 2020; much smaller sample size than orders from jan 2019
  #los_angeles = c('los_angeles', 'los_angeles_eps_codes', 'PSAT score 1190 - 1520', '546946', '546945'),
  #orange_county = c('orange_county', 'orange_county_eps_codes', 'PSAT score 1110 - 1210', '366935'), # ordered jan 2018; don't like these cutpoints as much
  #orange_county = c('orange_county', 'orange_county_eps_codes', 'PSAT score 1220 - 1520', '366934', '366932'),
  orange_county = c('orange_county', 'orange_county_eps_codes', 'PSAT score 1070 - 1180', '448375'), # ordered jan 2019
  orange_county = c('orange_county', 'orange_county_eps_codes', 'PSAT score 1190 - 1520', '448374', '448420'),
  #orange_county = c('orange_county', 'orange_county_eps_codes', 'PSAT score 1070 - 1180', '546954'), # ordered jan 2020; much smaller sample size than orders from jan 2019
  #orange_county = c('orange_county', 'orange_county_eps_codes', 'PSAT score 1190 - 1520', '546946', '546945'),
  #san_diego = c('san_diego', 'san_diego_eps_codes', 'PSAT score 1110 - 1210', '366935'), # ordered jan 2018; don't like these cutpoints as much
  #san_diego = c('san_diego', 'san_diego_eps_codes', 'PSAT score 1220 - 1520', '366934', '366932'),
  san_diego = c('san_diego', 'san_diego_eps_codes', 'PSAT score 1070 - 1180', '448375'), # ordered jan 2019
  san_diego = c('san_diego', 'san_diego_eps_codes', 'PSAT score 1190 - 1520', '448374', '448420'),
  #san_diego = c('san_diego', 'san_diego_eps_codes', 'PSAT score 1070 - 1180', '546954'), # ordered jan 2020; much smaller sample size than orders from jan 2019
  #san_diego = c('san_diego', 'san_diego_eps_codes', 'PSAT score 1190 - 1520', '546946', '546945'),
  #bay_area = c('bay_area', 'bay_area_eps_codes', 'PSAT score 1110 - 1210', '366935'), # ordered jan 2018; don't like these cutpoints as much
  #bay_area = c('bay_area', 'bay_area_eps_codes', 'PSAT score 1220 - 1520', '366934', '366932'),
  bay_area = c('bay_area', 'bay_area_eps_codes', 'PSAT score 1070 - 1180', '448375'), # ordered jan 2019
  bay_area = c('bay_area', 'bay_area_eps_codes', 'PSAT score 1190 - 1520', '448374', '448420'),
  #bay_area = c('bay_area', 'bay_area_eps_codes', 'PSAT score 1070 - 1180', '546954'), # ordered jan 2020; much smaller sample size than orders from jan 2019
  #bay_area = c('bay_area', 'bay_area_eps_codes', 'PSAT score 1190 - 1520', '546946', '546945'),
  northern_new_jersey = c('northern_new_jersey', 'nj_north_metro_eps_codes', 'PSAT score 1070 - 1180', '448922'),
  northern_new_jersey = c('northern_new_jersey', 'nj_north_metro_eps_codes', 'PSAT score 1190 - 1520', '448427', '448440'),
  long_island = c('long_island', 'long_island_eps_codes', 'PSAT score 1070 - 1180', '448922'),
  long_island = c('long_island', 'long_island_eps_codes', 'PSAT score 1190 - 1520', '448427', '448440'),
  detroit = c('detroit', 'detroit_eps_codes', 'PSAT score 1070 - 1180', '448922'),
  detroit = c('detroit', 'detroit_eps_codes', 'PSAT score 1190 - 1520', '448427', '448440'),
  dallas = c('dallas', 'dallas_eps_codes', 'PSAT score 1070 - 1180', '448922'),
  dallas = c('dallas', 'dallas_eps_codes', 'PSAT score 1190 - 1520', '448427', '448440'),
  houston = c('houston', 'houston_eps_codes', 'PSAT score 1010 - 1520', '329702')
)

# 2 Stephen F Austin State University, univ_id=228431 ord_num=329702          PSAT 1010-1520; grades C+ to A+, 2019 HS grads, all of TX, ordered 8/31/2017 [but seems to have TX 17....]

#create_sim_eps_race_table(data = fa20_oos_psat_sf, ord_nums = c('448922'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24')) # 
#create_sim_eps_race_table(data = fa20_oos_psat_sf, ord_nums = c('448427'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24')) 
#create_sim_eps_race_table(data = fa20_oos_psat_sf, ord_nums = c('448440'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24'))


all_orders %>% str()

orange_county_eps_codes

# CA ORDER NUMBERS
# 1 366935  42790 FA19 - CA PSAT AD {JAN18); ordered 1/17/2018; 2019/20/21 HS grad class; CA; PSAT 1110 - 1210
# 8 366934  15806 FA19 - CA PSAT SE (JAN18); ordered 1/17/2018; 2019/20/21 hs grad class; CA; PSAT 1220-1290    
# 7 366932  15931 FA19 - CA PSAT BE (JAN18); ordered 1/17/2018; 2019/20/21 hs grad class; CA; PSAT 1300 - 1520

# 2 448375  33893 FA20 - CA PSAT AD {JAN19); ordered 1/8/2019; 2020 HS grad class; CA; PSAT 1070 - 1180
# 5 448374  19775; FA20 - CA PSAT BE (JAN19); ordered 1/8/2019; 2020/21 hs grad class; CA; PSAT 1270-1520
# 6 448420  19437; FA20 - CA PSAT SE (JAN19); ordered 1/8/2019; 2020/21 hs grad class; CA; PSAT 1190-1260

# 4 546954  21102 FA21 - CA PSAT AD (JAN20); ordered 1/6/2020; 2021 HS grad class; CA; PSAT 1070-1180
#12 546946  12061 FA21 - CA PSAT SE (JAN20); ordered 1/6/2020; 2021/2022 hs grad class; CA; PSAT 1190-1260
#13 546945  11041 FA21 - CA PSAT BE (JAN20); ordered 1/6/2020; 2021/22 hs grad class; CA; PSAT 1270-1520


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


################
# 1) Utility function to fetch the order IDs
get_order_ids <- function(metro) {
  
  # Initialize a result list
  result <- list()
  
  # Chicago
  if (metro == "chicago") {
    result$first_order  <- 487984
    result$second_order <- "488035_488053"
    
    # Houston
  } else if (metro %in% c("houston")) {
    result$first_order  <- 329702
    
    # Philadelphia, Northern NJ, etc.
  } else if (metro %in% c("philadelphia", "northern new jersey",'long island','detroit','dallas')) {
    result$first_order  <- 448922
    result$second_order <- "448427_448440"
    
    # Los Angeles, Orange County, San Diego, Bay Area
  } else if (metro %in% c("los angeles", "orange county", "san diego", "bay area")) {
    result$first_order  <- 448375
    result$second_order <- "448374_448420"
    
  } else {
    stop("No matching order logic for metro: ", metro)
  }
  
  return(result)
}


# 2) Main loop to create & save the plots
for (m in c("chicago", "philadelphia", "los angeles", 
            "orange county", "san diego", "bay area", 
            "northern new jersey", "long island", "detroit", 
            "dallas", "houston")) {
  
  # Replace spaces with underscores for object/file naming
  m_underscore <- str_replace_all(m, " ", "_")
  
  # Retrieve the order IDs for this metro
  orders       <- get_order_ids(m)
  first_order  <- orders$first_order
  second_order <- orders$second_order
  
  # Check if there's a second order
  has_two_orders <- !is.null(second_order) && nzchar(second_order)
  
  for (g in c("race", "firstgen")) {
    
    # For more readable figure titles:
    g_prefix <- if (g == "race") "Racial/ethnic" else "First-generation status"
    
    for (rc in c("row", "col")) {
      
      # "plot_r" for row-percent, or "plot_c" for column-percent
      subslot <- if (rc == "row") "plot_r" else "plot_c"
      
      # We'll use this base name for saving or identifying the figure
      # (Note: we might attach the order ID below if needed)
      plot_name_base <- str_c("rq2_", m_underscore, "_", g, "_", rc, "_plot")
      
      # Build a figure title
      if (rc == "row") {
        figure_title <- str_c(
          g_prefix,
          " composition of purchased student profiles by Geomarket, ",
          tools::toTitleCase(m),
          " area"
        )
      } else {
        figure_title <- str_c(
          "Geomarket contribution to purchased student profiles by ",
          g_prefix,
          " group, ",
          tools::toTitleCase(m),
          " area"
        )
      }
      
      # --------------------------------------------------------------------
      # NEW LOGIC:
      #   1) If there's only ONE order -> single-plot approach
      #   2) If there are TWO orders:
      #        - row-percent => combine them
      #        - col-percent => separate them
      # --------------------------------------------------------------------
      
      if (has_two_orders) {
        
        # 2-A) If row-percent => combine the two subplots
        if (rc == "row") {
          
          # Build object names for the first + second subplots
          first_plot_obj_name  <- str_c("rq2_", m_underscore, "_", g, "_plot_order_", first_order)
          second_plot_obj_name <- str_c("rq2_", m_underscore, "_", g, "_plot_order_", second_order)
          
          # Retrieve sub-plot objects
          first_sub  <- get(first_plot_obj_name,  envir = .GlobalEnv)[[ subslot ]]
          second_sub <- get(second_plot_obj_name, envir = .GlobalEnv)[[ subslot ]]
          
          # Combine them in a single column
          combined_plot <- first_sub + second_sub + plot_layout(ncol = 1)
          
          # e.g. "rq2_chicago_race_row_plot.png"
          plot_name <- plot_name_base
          
          # For clarity in console/log
          writeLines(plot_name)
          writeLines(figure_title)
          
          # Write the figure title to a .txt file
          writeLines(
            figure_title, 
            file.path(graphs_dir, "rq2", str_c(plot_name, "_title.txt"))
          )
          
          # Save the combined ggplot object
          ggsave(
            filename = file.path(graphs_dir, "rq2", str_c(plot_name, ".png")),
            plot     = combined_plot,
            width    = 16,
            height   = 10,
            bg       = "white"
          )
          
          # Write out a .txt file with the footnotes
          note_text <- c('Figure Notes:',str_c(
            "- Excludes students with missing values for", g, sep = ' '
          ))
          writeLines(
            note_text, 
            file.path(graphs_dir, "rq2", str_c(plot_name, "_note.txt"))
          )
          
          # 2-B) If col-percent => make two separate plots (one per order)
        } else { 
          
          # We make two separate plots, one for each order ID
          for (order_id in c(first_order, second_order)) {
            
            # e.g. "rq2_los_angeles_race_plot_order_448375"
            single_plot_obj_name <- str_c("rq2_", m_underscore, "_", g, "_plot_order_", order_id)
            
            # Grab that sub-plot (row or col version)
            single_subplot <- get(single_plot_obj_name, envir = .GlobalEnv)[[ subslot ]]
            
            # e.g. "rq2_los_angeles_race_col_plot_448375.png"
            plot_name <- str_c(plot_name_base, "_", order_id)
            
            # For clarity in console/log
            writeLines(plot_name)
            writeLines(figure_title)
            
            # Write the figure title to a .txt file
            writeLines(
              figure_title, 
              file.path(graphs_dir, "rq2", str_c(plot_name, "_title.txt"))
            )
            
            # Save the separate plot
            ggsave(
              filename = file.path(graphs_dir, "rq2", str_c(plot_name, ".png")),
              plot     = single_subplot,
              width    = 16,
              height   = 10,
              bg       = "white"
            )
            
            # Write out a .txt file with the footnotes
            note_text <- c('Figure Notes:',str_c(
              "- Excludes students with missing values for", g, sep = ' '
            ))
            writeLines(
              note_text, 
              file.path(graphs_dir, "rq2", str_c(plot_name, "_note.txt"))
            )
          }
        }
        
        # --------------------------------------------------------------------
        # 1) If there's ONLY one order (no second_order) => single-plot
        # --------------------------------------------------------------------
      } else {
        
        # There's only a first_order
        single_plot_obj_name <- str_c("rq2_", m_underscore, "_", g, "_plot_order_", first_order)
        single_subplot       <- get(single_plot_obj_name, envir = .GlobalEnv)[[ subslot ]]
        
        # For single-order metros, we usually do just one figure name.
        # BUT if rc == "col", you want the order number appended:
        if (rc == "col") {
          plot_name <- str_c(plot_name_base, "_", first_order)
        } else {
          plot_name <- plot_name_base
        }
        
        # For clarity in console/log
        writeLines(plot_name)
        writeLines(figure_title)
        
        # Write the figure title to a .txt file
        writeLines(
          figure_title, 
          file.path(graphs_dir, "rq2", str_c(plot_name, "_title.txt"))
        )
        
        # Save the single plot
        ggsave(
          filename = file.path(graphs_dir, "rq2", str_c(plot_name, ".png")),
          plot     = single_subplot,
          width    = 16,
          height   = 10,
          bg       = "white"
        )
        
        # Footnotes
        note_text <- c('Figure Notes:',str_c(
          "- Excludes students with missing values for", g, sep = ' '
        ))
        writeLines(
          note_text, 
          file.path(graphs_dir, "rq2", str_c(plot_name, "_note.txt"))
        )
        
      } # end if (has_two_orders)
      
    } # end rc loop
    
  }   # end g (race/firstgen) loop
  
}     # end m (metro) loop

###########################
############################ RUN FUNCTION create_race_by_firstgen_graph TO CREATE GRAPH OF RACE X FIRSTGEN
###########################

# NEXT 2/5: NEED TO MODIFY THE FONT SIZES FOR THIS

# 1 487984  16926 # Illinois standard 2020; ordered 7/19/2019; HS class 2020, 2021; IL; SAT 1020-1150;  GPA A+ to B-
# 5 488035  12842 # Illinois HS 2020; ordered 7/19/2019; HS class 2020, 2021; SAT 1160 - 1300; GPA A+ to B-
# 9 488053   7259 # Illinois GPPA 2020 (no cf);ordered 7/19/2019; HS class 2020, 2021; SAT 1310-1600; GPA A+ to B-
#create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('487984'), eps_codes = chi_eps_codes) %>% print(n=50) # 

# philly metro area
# order 448922: PSAT 1070 - 1180; order 448427: PSAT 1190 - 1260; order 448440: PSAT 1270 - 1520

#create_race_by_firstgen_graph(lists_orders_zip_hs_df_sf, c('487984'), chi_eps_codes, 'chicago', exclude_race = c(1,8,12), title_suf = ', SAT score 1020 - 1150')
#create_race_by_firstgen_graph(lists_orders_zip_hs_df_sf, c('488035','488053'), chi_eps_codes, 'chicago', exclude_race = c(1,8,12), title_suf = ', SAT score 1160 - 1600')

#create_race_by_firstgen_graph(lists_orders_zip_hs_df_sf, c('448922'), philly_eps_codes, 'philadelphia', exclude_race = c(1,8,12), title_suf = ', SAT score 1070 - 1180')
#create_race_by_firstgen_graph(lists_orders_zip_hs_df_sf, c('448427','448440'), philly_eps_codes, 'philadelphia', exclude_race = c(1,8,12), title_suf = ', SAT score 1190 - 1520')


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

