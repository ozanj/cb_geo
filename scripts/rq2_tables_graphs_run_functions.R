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

# get_table_data <- function(metro_df) {
#   list(
#     race = do.call(bind_rows, lapply(1:nrow(metro_df), function(i) {
#       res <- create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = str_split(metro_df[[i, 'order_ids']], '_')[[1]], eps_codes = get(metro_df[[i, 'eps_codes']]))
#       
#       for (r in names(res)) {
#         names(res[[r]]) <- sub('^stu_|^r_|^c_', '', names(res[[r]]))
#         if ('known' %in% names(res[[r]])) {
#           res[[r]] <- res[[r]] %>% select(-race_known) %>% rename('race_known' = 'known')
#         }
#         res[[r]]$table <- r
#       }
#       
#       do.call(bind_rows, res) %>% mutate(order_ids = metro_df[[i, 'order_ids']], test_range = metro_df[[i, 'test_range']])
#     })),
#     firstgen = do.call(bind_rows, lapply(1:nrow(metro_df), function(i) {
#       res <- create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = str_split(metro_df[[i, 'order_ids']], '_')[[1]], eps_codes = get(metro_df[[i, 'eps_codes']]))
#       
#       for (r in names(res)) {
#         if (r %in% c('row_pct_table', 'col_pct_table')) {
#           res[[r]] <- res[[r]] %>% select(-stu_known)
#         }
#         names(res[[r]]) <- sub('^stu_|^r_|^c_', '', names(res[[r]]))
#         res[[r]]$table <- r
#       }
#       
#       do.call(bind_rows, res) %>% mutate(order_ids = metro_df[[i, 'order_ids']], test_range = metro_df[[i, 'test_range']])
#     })),
#     race_firstgen = do.call(bind_rows, lapply(1:nrow(metro_df), function(i) {
#       create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = str_split(metro_df[[i, 'order_ids']], '_')[[1]], eps_codes = get(metro_df[[i, 'eps_codes']]), exclude_race = '') %>%
#         mutate(order_ids = metro_df[[i, 'order_ids']], test_range = metro_df[[i, 'test_range']])
#     }))
#   )
# }
# 
# rq2_orders_df <- orders_df %>% filter(order_ids != '487927')
# 
# for (m in unique(rq2_orders_df$metro)) {
#   metro_df <- rq2_orders_df %>% filter(metro == m)
#   
#   l <- get_table_data(metro_df)
#   
#   saveRDS(l, file.path('.', 'results', 'tables', str_c('rq2_table_', m, '.rds')))
# }
# 
# aian_orders_df <- orders_df %>% filter(order_ids == '487927')
# 
# for (m in unique(aian_orders_df$metro)) {
#   metro_df <- aian_orders_df %>% filter(metro == m)
#   
#   l <- get_table_data(metro_df)
#   
#   saveRDS(l, file.path('.', 'results', 'tables', str_c('rq2_aian_table_', m, '.rds')))
# }

orders_df
# Part #1) Creating "rq2_{metro}_{g}_plot_order_{ids}" objects
# via create_sim_eps_graph()

for (i in seq_len(nrow(orders_df))) {
  
  row_i <- orders_df[i, ]
  
  # E.g. "long_island"
  metro_name_graph <- row_i$metro
  
  eps_codes_graph <- get(row_i$eps_codes, envir = .GlobalEnv)
  
  # Split into a character vector
  ord_nums_graph <- str_split(row_i$order_ids, "_")[[1]]
  n_orders <- length(ord_nums_graph)
  
  # Construct the "orders" portion
  if (n_orders == 1) {
    # Single order
    orders_str <- ord_nums_graph[1]
    orders_prefix <- "Order "
  } else if (n_orders == 2) {
    # Two orders
    orders_str <- str_c(ord_nums_graph[1], " and ", ord_nums_graph[2])
    orders_prefix <- "Orders "
  } else {
    # Three or more orders
    all_but_last <- ord_nums_graph[1:(n_orders - 1)]
    last_one     <- ord_nums_graph[n_orders]
    # Join the first n-1 with commas
    joined       <- str_c(all_but_last, collapse = ", ")
    # Add the final one with ", and ..."
    orders_str   <- str_c(joined, ", and ", last_one)
    orders_prefix <- "Orders "
  }
  
  # E.g. "Orders 392833, 487984, and 555444, SAT score 1020 - 1150"
  graph_title <- str_c(orders_prefix, orders_str, ", ", row_i$test_range)
  
  for (g in c("race", "firstgen")) {
    
    plot_name <- str_c(
      "rq2",
      metro_name_graph,
      g,
      "plot_order",
      # Keep underscores in the object name
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

# -------------------------------------------------------------------------
# Part #2) Generating row/col percent figures (patchwork logic)
# -------------------------------------------------------------------------

# List the special AI/AN order(s) that should not be combined with others
special_orders <- c("487927")

unique_metros <- unique(orders_df$metro)

for (m in unique_metros) {
  
  # Convert underscores -> spaces, then title case
  friendly_metro_name <- gsub("_", " ", m, fixed = TRUE)
  friendly_metro_name <- tools::toTitleCase(friendly_metro_name)
  
  # Subset orders_df to just this metro
  df_metro <- orders_df %>% filter(metro == m)
  
  # Gather order_ids from that subset
  order_groups <- df_metro$order_ids  # e.g. c("448922_484698", "...")
  
  for (g in c("race", "firstgen")) {
    
    # For non‐special orders, we build a default prefix
    g_prefix <- if (g == "race") "Racial/ethnic" else "First-generation status"
    
    for (rc in c("row", "col")) {
      
      # "plot_r" or "plot_c"
      subslot <- if (rc == "row") "plot_r" else "plot_c"
      plot_name_base <- str_c("rq2_", m, "_", g, "_", rc, "_plot")
      
      # ---------------------------------------------------------
      # Build the *default* figure title for non‐special orders
      # ---------------------------------------------------------
      if (rc == "row") {
        # e.g. "Racial/ethnic composition of purchased student profiles by Geomarket, Los Angeles area"
        if (friendly_metro_name == "Bay Area") {
          default_figure_title <- str_c(
            g_prefix,
            " composition of purchased student profiles by Geomarket, ",
            friendly_metro_name
          )
        } else {
          default_figure_title <- str_c(
            g_prefix,
            " composition of purchased student profiles by Geomarket, ",
            friendly_metro_name,
            " area"
          )
        }
      } else {
        # e.g. "Geomarket contribution to purchased student profiles by Racial/ethnic group, Los Angeles area"
        if (friendly_metro_name == "Bay Area") {
          default_figure_title <- str_c(
            "Geomarket contribution to purchased student profiles by ",
            g_prefix,
            " group, ",
            friendly_metro_name
          )
        } else {
          default_figure_title <- str_c(
            "Geomarket contribution to purchased student profiles by ",
            g_prefix,
            " group, ",
            friendly_metro_name,
            " area"
          )
        }
      }
      
      # ------------------------------------------------------------------
      # 1) ROW-PERCENT logic
      # ------------------------------------------------------------------
      if (rc == "row") {
        
        # We'll store the combined subplots here
        sub_plots_list <- list()
        # We'll also collect figure_notes for the combined portion
        row_fig_notes <- character(0)
        
        for (og in order_groups) {
          plot_obj_name <- str_c("rq2_", m, "_", g, "_plot_order_", og)
          
          if (!exists(plot_obj_name, envir = .GlobalEnv)) {
            warning("Object ", plot_obj_name, " not found; skipping.")
            next
          }
          
          # If this order is special => use an "AI/AN" figure title
          if (og %in% special_orders) {
            
            # Decide row-percent text for RACE vs FIRSTGEN
            if (g == "race") {
              figure_title <- "Racial/ethnic composition of purchased AI/AN student profiles by Geomarket"
            } else {
              figure_title <- "First-generation status composition of purchased AI/AN student profiles by Geomarket"
            }
            # If you want to append the metro name => uncomment:
            # figure_title <- str_c(figure_title, ", ", friendly_metro_name, " area")
            
            single_subplot <- get(plot_obj_name, envir = .GlobalEnv)[[subslot]]
            plot_name      <- str_c(plot_name_base, "_", og)
            
            message("Saving stand-alone (row-percent) figure for special order: ", plot_name)
            writeLines(
              figure_title, 
              file.path(graphs_dir, "rq2", str_c(plot_name, "_title.txt"))
            )
            
            # Look up figure_note for this order
            row_og <- df_metro %>% filter(order_ids == og)
            figure_note_local <- if (nrow(row_og) > 0) row_og$figure_note[1] else ""
            
            # Build the note text
            note_text <- c(
              "Figure Notes:",
              str_c("- Excludes students with missing values for ", g, ". ", figure_note_local, ".")
            )
            
            ggsave(
              filename = file.path(graphs_dir, "rq2", str_c(plot_name, ".png")),
              plot     = single_subplot,
              width    = 16,
              height   = 10,
              bg       = "white"
            )
            
            writeLines(
              note_text, 
              file.path(graphs_dir, "rq2", str_c(plot_name, "_note.txt"))
            )
            
          } else {
            # Non-special => combine
            sub_plots_list[[length(sub_plots_list) + 1]] <-
              get(plot_obj_name, envir = .GlobalEnv)[[subslot]]
            
            # Also collect the figure_note for combining
            row_og <- df_metro %>% filter(order_ids == og)
            if (nrow(row_og) > 0) {
              row_fig_notes <- c(row_fig_notes, row_og$figure_note[1])
            }
          }
        } # end for (og in order_groups)
        
        # Combine "non-special" plots (if any exist)
        if (length(sub_plots_list) > 0) {
          
          combined_plot <- wrap_plots(sub_plots_list, ncol = 1)
          plot_name <- plot_name_base
          
          message("Saving combined (row-percent) figure: ", plot_name)
          writeLines(
            default_figure_title,
            file.path(graphs_dir, "rq2", str_c(plot_name, "_title.txt"))
          )
          
          ggsave(
            filename = file.path(graphs_dir, "rq2", str_c(plot_name, ".png")),
            plot     = combined_plot,
            width    = 16,
            height   = 10,
            bg       = "white"
          )
          
          # Build note_text with a single line about excludes + all notes
          line2 <- str_c(
            "- Excludes students with missing values for ", g, ". ",
            paste(row_fig_notes, collapse = ". ")
          )
          line2 <- str_c(line2, ".")
          
          note_text <- c("Figure Notes:", line2)
          writeLines(
            note_text, 
            file.path(graphs_dir, "rq2", str_c(plot_name, "_note.txt"))
          )
        }
        
        # ------------------------------------------------------------------
        # 2) COLUMN-PERCENT logic
        # ------------------------------------------------------------------
      } else {
        
        for (og in order_groups) {
          
          plot_obj_name <- str_c("rq2_", m, "_", g, "_plot_order_", og)
          
          if (!exists(plot_obj_name, envir = .GlobalEnv)) {
            warning("Object ", plot_obj_name, " not found; skipping.")
            next
          }
          
          # If this order is special => AI/AN override
          if (og %in% special_orders) {
            
            # Decide col-percent text for RACE vs FIRSTGEN
            if (g == "race") {
              figure_title <- "Geomarket contribution to purchased AI/AN student profiles by Racial/ethnic group"
            } else {
              figure_title <- "Geomarket contribution to purchased AI/AN student profiles by First-generation status group"
            }
            # If you want to append the metro name => uncomment:
            # figure_title <- str_c(figure_title, ", ", friendly_metro_name, " area")
            
          } else {
            # Non-special orders => use default
            figure_title <- default_figure_title
          }
          
          single_subplot <- get(plot_obj_name, envir = .GlobalEnv)[[subslot]]
          plot_name      <- str_c(plot_name_base, "_", og)
          
          message("Saving separate (col-percent) figure: ", plot_name)
          writeLines(
            figure_title,
            file.path(graphs_dir, "rq2", str_c(plot_name, "_title.txt"))
          )
          
          # Look up figure_note for this order
          row_og <- df_metro %>% filter(order_ids == og)
          figure_note_local <- if (nrow(row_og) > 0) row_og$figure_note[1] else ""
          
          note_text <- c(
            "Figure Notes:",
            str_c("- Excludes students with missing values for ", g, ". ", figure_note_local, ".")
          )
          
          ggsave(
            filename = file.path(graphs_dir, "rq2", str_c(plot_name, ".png")),
            plot     = single_subplot,
            width    = 16,
            height   = 10,
            bg       = "white"
          )
          
          writeLines(
            note_text, 
            file.path(graphs_dir, "rq2", str_c(plot_name, "_note.txt"))
          )
        } # end for (og in order_groups)
        
      } # end if row vs col
      
    } # end rc loop
    
  } # end g loop
  
} # end for (m in unique_metros)


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
