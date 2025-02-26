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


# 1) Load the CSV into a data frame
orders_df <- read_csv(file.path(scripts_dir,"metro_orders.csv"))


# Part #1) Creating "rq2_{metro}_{g}_plot_order_{ids}" objects
# via create_sim_eps_graph()

for (i in seq_len(nrow(orders_df))) {
  
  # Extract row i
  row_i <- orders_df[i, ]
  
  # Underscored name
  metro_name_graph <- row_i$metro  # e.g. "long_island"
  
  eps_codes_graph  <- get(row_i$eps_codes, envir = .GlobalEnv)
  graph_title      <- row_i$test_range
  
  # For the object name, we keep underscores
  ord_nums_graph <- str_split(row_i$order_ids, "_")[[1]]
  
  for (g in c("race", "firstgen")) {
    
    plot_name <- str_c(
      "rq2",
      metro_name_graph,
      g,
      "plot_order",
      str_c(ord_nums_graph, collapse = "_"),
      sep = "_"
    )
    
    if (exists(plot_name, envir = .GlobalEnv)) {
      rm(list = plot_name, envir = .GlobalEnv)
    }
    
    assign(
      plot_name,
      create_sim_eps_graph(
        data_graph     = lists_orders_zip_hs_df_sf,
        ord_nums_graph = ord_nums_graph,
        eps_codes_graph = eps_codes_graph,
        variable       = g,
        title          = graph_title
      ),
      envir = .GlobalEnv
    )
  }
}

# Part #2) Generating row/col percent figures (patchwork logic)
unique_metros <- unique(orders_df$metro)

for (m in unique_metros) {
  
  # Convert underscores -> spaces, then title case
  friendly_metro_name <- gsub("_", " ", m, fixed = TRUE)
  friendly_metro_name <- tools::toTitleCase(friendly_metro_name)
  
  df_metro <- orders_df %>% filter(metro == m)
  
  order_groups <- df_metro$order_ids  # vector of strings like "448922_484698"
  
  for (g in c("race", "firstgen")) {
    
    g_prefix <- if (g == "race") "Racial/ethnic" else "First-generation status"
    
    for (rc in c("row", "col")) {
      
      subslot <- if (rc == "row") "plot_r" else "plot_c"
      plot_name_base <- str_c("rq2_", m, "_", g, "_", rc, "_plot")
      
      # Now incorporate friendly_metro_name into your figure_title
      if (rc == "row") {
        figure_title <- str_c(
          g_prefix,
          " composition of purchased student profiles by Geomarket, ",
          friendly_metro_name,
          " area"
        )
      } else {
        figure_title <- str_c(
          "Geomarket contribution to purchased student profiles by ",
          g_prefix,
          " group, ",
          friendly_metro_name,
          " area"
        )
      }
      
      if (rc == "row") {
        # Combine all subplots vertically
        sub_plots_list <- vector("list", length(order_groups))
        
        for (i in seq_along(order_groups)) {
          og <- order_groups[i]
          plot_obj_name <- str_c("rq2_", m, "_", g, "_plot_order_", og)
          
          if (!exists(plot_obj_name, envir = .GlobalEnv)) {
            warning("Object ", plot_obj_name, " not found; skipping.")
            next
          }
          
          sub_plots_list[[i]] <- get(plot_obj_name, envir = .GlobalEnv)[[subslot]]
        }
        
        combined_plot <- wrap_plots(sub_plots_list, ncol = 1)
        plot_name <- plot_name_base
        
        message("Saving combined (row-percent) figure: ", plot_name)
        writeLines(figure_title, file.path(graphs_dir, "rq2", str_c(plot_name, "_title.txt")))
        
        ggsave(
          filename = file.path(graphs_dir, "rq2", str_c(plot_name, ".png")),
          plot     = combined_plot,
          width    = 16,
          height   = 10,
          bg       = "white"
        )
        
        note_text <- c(
          "Figure Notes:",
          str_c("- Excludes students with missing values for ", g)
        )
        writeLines(
          note_text, 
          file.path(graphs_dir, "rq2", str_c(plot_name, "_note.txt"))
        )
        
      } else {
        # COLUMN-PERCENT => separate figure per order group
        for (og in order_groups) {
          
          plot_obj_name <- str_c("rq2_", m, "_", g, "_plot_order_", og)
          if (!exists(plot_obj_name, envir = .GlobalEnv)) {
            warning("Object ", plot_obj_name, " not found; skipping.")
            next
          }
          
          single_subplot <- get(plot_obj_name, envir = .GlobalEnv)[[subslot]]
          plot_name <- str_c(plot_name_base, "_", og)
          
          message("Saving separate (col-percent) figure: ", plot_name)
          writeLines(figure_title, file.path(graphs_dir, "rq2", str_c(plot_name, "_title.txt")))
          
          ggsave(
            filename = file.path(graphs_dir, "rq2", str_c(plot_name, ".png")),
            plot     = single_subplot,
            width    = 16,
            height   = 10,
            bg       = "white"
          )
          
          note_text <- c(
            "Figure Notes:",
            str_c("- Excludes students with missing values for ", g)
          )
          writeLines(
            note_text, 
            file.path(graphs_dir, "rq2", str_c(plot_name, "_note.txt"))
          )
        }
      }
    }
  }
}

############################################################
# Part #3) create_race_by_firstgen_graph() loop
############################################################

for (i in seq_len(nrow(orders_df))) {
  
  row_i <- orders_df[i, ]
  
  # 1) Underscored name for internal use
  metro_name_graph <- row_i$metro  # e.g. "long_island"
  
  # 2) Get EPS codes object
  eps_codes_graph <- get(row_i$eps_codes, envir = .GlobalEnv)
  
  # 3) We'll use only the test range as the suffix
  #    (No leading commas or repeated metro name.)
  graph_title <- row_i$test_range
  custom_title_suf <- graph_title  # e.g. "PSAT score 1190 - 1260"
  
  # 4) Split order IDs
  ord_nums_graph <- str_split(row_i$order_ids, "_")[[1]]
  
  # 5) Call the function
  create_race_by_firstgen_graph(
    data_graph     = lists_orders_zip_hs_df_sf,
    ord_nums_graph = ord_nums_graph,
    eps_codes_graph = eps_codes_graph,
    metro_name     = metro_name_graph,  # still underscores internally
    exclude_race   = c(1, 8, 12),
    title_suf      = custom_title_suf   # pass the test range only
  )
}
