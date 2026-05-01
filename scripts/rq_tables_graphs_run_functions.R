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
source(file = file.path(scripts_dir, 'rq_tables_graphs_create_functions.R'))


# 1) Load the CSV into a data frame
orders_df <- read_csv(file.path(scripts_dir,"metro_orders.csv"))

format_metro_for_slide_subtitle <- function(metro) {
  
  metro_clean <- metro %>%
    stringr::str_replace_all("_", " ") %>%
    tools::toTitleCase()
  
  dplyr::case_when(
    metro == "bay_area" ~ "Bay Area",
    metro == "dc_maryland_virginia" ~ "D.C., Maryland, and Virginia",
    TRUE ~ metro_clean
  )
}

format_score_range_for_slide_subtitle <- function(test_range) {
  
  test_range %>%
    stringr::str_replace_all("\\s+-\\s+", "–")
}


get_table_data <- function(metro_df) {
  list(
    race = do.call(bind_rows, lapply(1:nrow(metro_df), function(i) {
      res <- create_sim_eps_race_table(data = lists_orders_zip_hs_df_sf, ord_nums = str_split(metro_df[[i, 'order_ids']], '_')[[1]], eps_codes = get(metro_df[[i, 'eps_codes']]))

      for (r in names(res)) {
        names(res[[r]]) <- sub('^stu_|^r_|^c_', '', names(res[[r]]))
        if ('known' %in% names(res[[r]])) {
          res[[r]] <- res[[r]] %>% select(-race_known) %>% rename('race_known' = 'known')
        }
        res[[r]]$table <- r
      }

      do.call(bind_rows, res) %>% mutate(order_ids = metro_df[[i, 'order_ids']], test_range = metro_df[[i, 'test_range']])
    })),
    firstgen = do.call(bind_rows, lapply(1:nrow(metro_df), function(i) {
      res <- create_sim_eps_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = str_split(metro_df[[i, 'order_ids']], '_')[[1]], eps_codes = get(metro_df[[i, 'eps_codes']]))

      for (r in names(res)) {
        if (r %in% c('row_pct_table', 'col_pct_table')) {
          res[[r]] <- res[[r]] %>% select(-stu_known)
        }
        names(res[[r]]) <- sub('^stu_|^r_|^c_', '', names(res[[r]]))
        res[[r]]$table <- r
      }

      do.call(bind_rows, res) %>% mutate(order_ids = metro_df[[i, 'order_ids']], test_range = metro_df[[i, 'test_range']])
    })),
    race_firstgen = do.call(bind_rows, lapply(1:nrow(metro_df), function(i) {
      create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = str_split(metro_df[[i, 'order_ids']], '_')[[1]], eps_codes = get(metro_df[[i, 'eps_codes']]), exclude_race = '') %>%
        mutate(order_ids = metro_df[[i, 'order_ids']], test_range = metro_df[[i, 'test_range']])
    }))
  )
}

rq2_orders_df <- orders_df %>% filter(order_ids != '487927')

for (m in unique(rq2_orders_df$metro)) {
  metro_df <- rq2_orders_df %>% filter(metro == m)

  l <- get_table_data(metro_df)

  saveRDS(l, file.path('.', 'results', 'tables', str_c('rq2_table_', m, '.rds')))
}

aian_orders_df <- orders_df %>% filter(order_ids == '487927')

for (m in unique(aian_orders_df$metro)) {
  metro_df <- aian_orders_df %>% filter(metro == m)

  l <- get_table_data(metro_df)

  saveRDS(l, file.path('.', 'results', 'tables', str_c('rq2_aian_table_', m, '.rds')))
}

############################################################
# CREATE ANALYSIS DATASET(S) FOR REVISED RQ3 EXCLUSION GRAPHS
############################################################

# For main RQ3 analyses, exclude the special AI/AN order.
# We can handle that separately later if needed.
rq3_orders_df <- orders_df %>% 
  filter(order_ids != "487927")

get_rq3_exclusion_data <- function(metro_df) {
  
  list(
    
    race = do.call(
      bind_rows,
      lapply(seq_len(nrow(metro_df)), function(i) {
        
        create_rq3_eps_exclusion_table(
          data             = lists_orders_zip_hs_df_sf,
          ord_nums         = str_split(metro_df[[i, "order_ids"]], "_")[[1]],
          eps_codes        = get(metro_df[[i, "eps_codes"]]),
          composition_type = "race"
        ) %>%
          mutate(
            metro       = metro_df[[i, "metro"]],
            order_ids   = metro_df[[i, "order_ids"]],
            test_range  = metro_df[[i, "test_range"]],
            figure_note = metro_df[[i, "figure_note"]]
          )
      })
    ),
    
    firstgen = do.call(
      bind_rows,
      lapply(seq_len(nrow(metro_df)), function(i) {
        
        create_rq3_eps_exclusion_table(
          data             = lists_orders_zip_hs_df_sf,
          ord_nums         = str_split(metro_df[[i, "order_ids"]], "_")[[1]],
          eps_codes        = get(metro_df[[i, "eps_codes"]]),
          composition_type = "firstgen",
          firstgen_detail  = "collapsed"
        ) %>%
          mutate(
            metro       = metro_df[[i, "metro"]],
            order_ids   = metro_df[[i, "order_ids"]],
            test_range  = metro_df[[i, "test_range"]],
            figure_note = metro_df[[i, "figure_note"]]
          )
      })
    ),
    
    race_firstgen = do.call(
      bind_rows,
      lapply(seq_len(nrow(metro_df)), function(i) {
        
        create_rq3_eps_exclusion_table(
          data             = lists_orders_zip_hs_df_sf,
          ord_nums         = str_split(metro_df[[i, "order_ids"]], "_")[[1]],
          eps_codes        = get(metro_df[[i, "eps_codes"]]),
          composition_type = "race_firstgen",
          exclude_race     = c(1, 8),
          firstgen_detail  = "collapsed"
        ) %>%
          mutate(
            metro       = metro_df[[i, "metro"]],
            order_ids   = metro_df[[i, "order_ids"]],
            test_range  = metro_df[[i, "test_range"]],
            figure_note = metro_df[[i, "figure_note"]]
          )
      })
    )
  )
}

for (m in unique(rq3_orders_df$metro)) {
  
  metro_df <- rq3_orders_df %>% 
    filter(metro == m)
  
  rq3_l <- get_rq3_exclusion_data(metro_df)
  
  saveRDS(
    rq3_l,
    file.path(".", "results", "tables", str_c("rq3_exclusion_table_", m, ".rds"))
  )
}

############################################################
# CREATE GRAPHS FOR REVISED RQ3 EXCLUSION ANALYSES
############################################################

# Create RQ3 graph folder if needed
dir.create(
  file.path(graphs_dir, "rq3"),
  showWarnings = FALSE,
  recursive = TRUE
)

# ----------------------------------------------------------
# RQ3 plot configuration
# ----------------------------------------------------------

rq3_plot_specs <- tibble::tribble(
  ~g,              ~effect_scale, ~width, ~height_min, ~height_base, ~height_per_exclusion,
  "race",          "pp",          14,     8,           3.5,          0.55,
  "race",          "relative",    14,     8,           3.5,          0.55,
  "firstgen",      "pp",          14,     6,           3.5,          0.45,
  "firstgen",      "relative",    14,     6,           3.5,          0.45,
  "race_firstgen", "pp",          16,     10,          4.5,          0.65,
  "race_firstgen", "relative",    16,     10,          4.5,          0.65
)

# ----------------------------------------------------------
# Helper: create plot object
# ----------------------------------------------------------

create_rq3_plot_obj <- function(g, effect_scale, rq3_l, order_ids_this) {
  
  if (g == "race") {
    
    create_rq3_race_delta_facet_plot(
      rq3_df = rq3_l$race,
      order_ids_this = order_ids_this,
      effect_scale = effect_scale
    )
    
  } else if (g == "firstgen") {
    
    create_rq3_firstgen_delta_facet_plot(
      rq3_df = rq3_l$firstgen,
      order_ids_this = order_ids_this,
      effect_scale = effect_scale
    )
    
  } else if (g == "race_firstgen") {
    
    create_rq3_race_firstgen_delta_facet_plot(
      rq3_df = rq3_l$race_firstgen,
      order_ids_this = order_ids_this,
      sort_by = "firstgen_delta",
      effect_scale = effect_scale
    )
    
  } else {
    
    stop("Unknown RQ3 graph type: ", g)
  }
}

# ----------------------------------------------------------
# Helper: title text
# ----------------------------------------------------------

make_rq3_title_tex <- function(g, effect_scale, metro, test_range) {
  
  scale_phrase <- if_else(
    effect_scale == "pp",
    "Percentage-point change",
    "Relative percent change"
  )
  
  outcome_phrase <- case_when(
    g == "race" ~ "racial/ethnic composition",
    g == "firstgen" ~ "first-generation status composition",
    g == "race_firstgen" ~ "race × first-generation composition",
    TRUE ~ "composition"
  )
  
  geomarket_phrase <- if_else(
    g == "race",
    "after excluding each Geomarket: ",
    "after excluding Geomarket: "
  )
  
  stringr::str_c(
    "#### ",
    scale_phrase,
    " in ",
    outcome_phrase,
    " ",
    geomarket_phrase,
    format_metro_for_slide_subtitle(metro),
    ", ",
    format_score_range_for_slide_subtitle(test_range)
  )
}

# ----------------------------------------------------------
# Helper: figure notes
# ----------------------------------------------------------

make_rq3_note_text <- function(g, effect_scale, figure_note_local) {
  
  scale_sentence <- if_else(
    effect_scale == "pp",
    "Each point shows the percentage-point change",
    "Each point shows the relative percent change"
  )
  
  formula_sentence <- if_else(
    effect_scale == "relative",
    " Relative percent change is calculated as 100 × (remaining-pool share − full-pool share) / full-pool share.",
    ""
  )
  
  interpretation_sentence <- if_else(
    effect_scale == "pp",
    "Positive values mean the remaining pool has a higher share than the full metro pool; negative values mean the remaining pool has a lower share.",
    "Positive values mean the remaining pool has higher relative representation than the full metro pool; negative values mean the remaining pool has lower relative representation."
  )
  
  if (g == "race") {
    
    note_body <- str_c(
      "- ",
      scale_sentence,
      " in the racial/ethnic composition of the remaining visible prospect pool after excluding the named Geomarket.",
      formula_sentence,
      " ",
      interpretation_sentence,
      " Facet titles report the full-pool count and baseline share for each racial/ethnic group. ",
      "Excludes students with missing values for race/ethnicity. ",
      figure_note_local
    )
    
  } else if (g == "firstgen") {
    
    note_body <- str_c(
      "- ",
      scale_sentence,
      " in first-generation status composition of the remaining visible prospect pool after excluding the named Geomarket.",
      formula_sentence,
      " ",
      interpretation_sentence,
      " Facet titles report the full-pool count and baseline share for each first-generation status group. ",
      "Excludes students with missing values for first-generation status. ",
      figure_note_local
    )
    
  } else if (g == "race_firstgen") {
    
    sort_sentence <- if_else(
      effect_scale == "pp",
      "Within each racial/ethnic row, Geomarkets are sorted by the first-generation subgroup's percentage-point change. ",
      "Within each racial/ethnic row, Geomarkets are sorted by the first-generation subgroup's relative percent change. "
    )
    
    note_body <- str_c(
      "- ",
      scale_sentence,
      " in the race-by-first-generation composition of the remaining visible prospect pool after excluding the named Geomarket.",
      formula_sentence,
      " ",
      interpretation_sentence,
      " Rows show racial/ethnic groups; columns distinguish first-generation and non-first-generation prospects. ",
      sort_sentence,
      "Panel labels report the full-pool count and baseline share for each race-by-first-generation subgroup. ",
      "Excludes students with missing values for race/ethnicity or first-generation status. ",
      figure_note_local
    )
    
  } else {
    
    stop("Unknown RQ3 graph type: ", g)
  }
  
  c("Figure Notes:", note_body)
}

# ----------------------------------------------------------
# Main RQ3 graph loop
# ----------------------------------------------------------

for (m in unique(rq3_orders_df$metro)) {
  
  message("Creating RQ3 exclusion plots for: ", m)
  
  rq3_l <- readRDS(
    file.path(".", "results", "tables", str_c("rq3_exclusion_table_", m, ".rds"))
  )
  
  metro_df <- rq3_orders_df %>%
    filter(metro == m)
  
  for (i in seq_len(nrow(metro_df))) {
    
    order_ids_this <- metro_df$order_ids[i]
    test_range_this <- metro_df$test_range[i]
    figure_note_local <- metro_df$figure_note[i]
    
    for (spec_i in seq_len(nrow(rq3_plot_specs))) {
      
      spec <- rq3_plot_specs[spec_i, ]
      
      g <- spec$g
      effect_scale_this <- spec$effect_scale
      
      plot_obj <- create_rq3_plot_obj(
        g = g,
        effect_scale = effect_scale_this,
        rq3_l = rq3_l,
        order_ids_this = order_ids_this
      )
      
      plot_name <- str_c(
        "rq3_",
        m,
        "_",
        g,
        "_",
        effect_scale_this,
        "_exclusion_plot_order_",
        order_ids_this
      )
      
      message("Saving RQ3 plot: ", plot_name)
      
      n_exclusions <- rq3_l[[g]] %>%
        filter(
          order_ids == order_ids_this,
          scenario_type == "exclusion"
        ) %>%
        distinct(excluded_eps_codename) %>%
        nrow()
      
      plot_height <- max(
        spec$height_min,
        spec$height_base + spec$height_per_exclusion * n_exclusions
      )
      
      ggsave(
        filename = file.path(graphs_dir, "rq3", str_c(plot_name, ".png")),
        plot = plot_obj,
        width = spec$width,
        height = plot_height,
        bg = "white"
      )
      
      title_tex <- make_rq3_title_tex(
        g = g,
        effect_scale = effect_scale_this,
        metro = m,
        test_range = test_range_this
      )
      
      writeLines(
        title_tex,
        file.path(graphs_dir, "rq3", str_c(plot_name, "_title.tex"))
      )
      
      note_text <- make_rq3_note_text(
        g = g,
        effect_scale = effect_scale_this,
        figure_note_local = figure_note_local
      )
      
      writeLines(
        note_text,
        file.path(graphs_dir, "rq3", str_c(plot_name, "_note.txt"))
      )
    }
  }
}

############################################################
############################################################

############################################################
############################################################

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
# Part #2A) Create RQ2 overlay contribution plots
#           Race-only and first-gen-only versions
############################################################

# These plots answer:
# 1) Where do racial/ethnic groups come from?
#    - Gray bar = all race-known prospects
#    - Blue lollipop = focal racial/ethnic group
#
# 2) Where do first-gen and not-first-gen prospects come from?
#    - Gray bar = all first-gen-known prospects
#    - Blue lollipop = focal first-generation-status group

dir.create(
  file.path(graphs_dir, "rq2"),
  showWarnings = FALSE,
  recursive = TRUE
)

# Main student-list orders only.
# Exclude the special AI/AN order and Houston from regular RQ2 student-list plots.
rq2_overlay_orders_df <- orders_df %>%
  filter(
    order_ids != "487927",
    metro != "houston"
  )

for (i in seq_len(nrow(rq2_overlay_orders_df))) {
  
  row_i <- rq2_overlay_orders_df[i, ]
  
  metro_this <- row_i$metro
  order_ids_this <- row_i$order_ids
  test_range_this <- row_i$test_range
  figure_note_this <- row_i$figure_note
  
  message(
    "Creating RQ2 overlay contribution plots for: ",
    metro_this,
    ", order group ",
    order_ids_this
  )
  
  # ----------------------------------------------------------
  # 1) Build inputs
  # ----------------------------------------------------------
  
  ord_nums_this <- stringr::str_split(order_ids_this, "_")[[1]]
  eps_codes_this <- get(row_i$eps_codes, envir = .GlobalEnv)
  
  # ----------------------------------------------------------
  # 2) Create race overlay plot
  # ----------------------------------------------------------
  
  race_overlay_df <- create_rq2_race_contribution_overlay_df(
    data       = lists_orders_zip_hs_df_sf,
    ord_nums   = ord_nums_this,
    eps_codes  = eps_codes_this,
    metro      = metro_this,
    order_ids  = order_ids_this,
    test_range = test_range_this
  )
  
  race_plot_obj <- create_rq2_race_contribution_overlay_plot(
    plot_df    = race_overlay_df,
    sort_order = "geomarket"
  )
  
  race_plot_name <- stringr::str_c(
    "rq2_",
    metro_this,
    "_race_contribution_overlay_plot_",
    order_ids_this
  )
  
  n_geomarkets_race <- race_overlay_df %>%
    dplyr::distinct(eps_codename) %>%
    nrow()
  
  race_plot_height <- max(
    8,
    4.5 + 0.55 * n_geomarkets_race
  )
  
  ggsave(
    filename = file.path(graphs_dir, "rq2", stringr::str_c(race_plot_name, ".png")),
    plot     = race_plot_obj,
    width    = 14,
    height   = race_plot_height,
    bg       = "white"
  )
  
  race_title_tex <- stringr::str_c(
    "#### Geomarket contributions to each racial/ethnic group: ",
    format_metro_for_slide_subtitle(metro_this),
    ", ",
    format_score_range_for_slide_subtitle(test_range_this)
  )
  
  writeLines(
    race_title_tex,
    file.path(graphs_dir, "rq2", stringr::str_c(race_plot_name, "_title.tex"))
  )
  
  race_note_text <- c(
    "Figure Notes:",
    stringr::str_c(
      "- Gray bars show the share of all race-known purchased student profiles contributed by each Geomarket. ",
      "Blue lollipops show the share of the focal racial/ethnic group contributed by each Geomarket. ",
      "Geomarkets are sorted by Geomarket number. ",
      "Excludes students with missing values for race/ethnicity. ",
      "Figure shows White, Asian, Black, and Hispanic groups; it excludes Two+, non-Hispanic; AIAN, non-Hispanic; and NHPI, non-Hispanic groups. ",
      figure_note_this
    )
  )
  
  writeLines(
    race_note_text,
    file.path(graphs_dir, "rq2", stringr::str_c(race_plot_name, "_note.txt"))
  )
  
  # ----------------------------------------------------------
  # 3) Create first-gen overlay plot
  # ----------------------------------------------------------
  
  firstgen_overlay_df <- create_rq2_firstgen_contribution_overlay_df(
    data       = lists_orders_zip_hs_df_sf,
    ord_nums   = ord_nums_this,
    eps_codes  = eps_codes_this,
    metro      = metro_this,
    order_ids  = order_ids_this,
    test_range = test_range_this
  )
  
  firstgen_plot_obj <- create_rq2_firstgen_contribution_overlay_plot(
    plot_df    = firstgen_overlay_df,
    sort_order = "geomarket"
  )
  
  firstgen_plot_name <- stringr::str_c(
    "rq2_",
    metro_this,
    "_firstgen_contribution_overlay_plot_",
    order_ids_this
  )
  
  n_geomarkets_firstgen <- firstgen_overlay_df %>%
    dplyr::distinct(eps_codename) %>%
    nrow()
  
  firstgen_plot_height <- max(
    7,
    4.5 + 0.65 * n_geomarkets_firstgen
  )
  
  ggsave(
    filename = file.path(graphs_dir, "rq2", stringr::str_c(firstgen_plot_name, ".png")),
    plot     = firstgen_plot_obj,
    width    = 14,
    height   = firstgen_plot_height,
    bg       = "white"
  )
  
  firstgen_title_tex <- stringr::str_c(
    "#### Geomarket contributions by first-generation status: ",
    format_metro_for_slide_subtitle(metro_this),
    ", ",
    format_score_range_for_slide_subtitle(test_range_this)
  )
  
  writeLines(
    firstgen_title_tex,
    file.path(graphs_dir, "rq2", stringr::str_c(firstgen_plot_name, "_title.tex"))
  )
  
  firstgen_note_text <- c(
    "Figure Notes:",
    stringr::str_c(
      "- Gray bars show the share of all first-gen-known purchased student profiles contributed by each Geomarket. ",
      "Blue lollipops show the share of the focal first-generation-status group contributed by each Geomarket. ",
      "Geomarkets are sorted by Geomarket number. ",
      "Excludes students with missing values for first-generation status. ",
      figure_note_this
    )
  )
  
  writeLines(
    firstgen_note_text,
    file.path(graphs_dir, "rq2", stringr::str_c(firstgen_plot_name, "_note.txt"))
  )
}

############################################################
# Part #2B) Create RQ2 race x first-gen contribution plots
#           Two-lollipop version
############################################################

# These plots answer:
# Where do first-generation and non-first-generation students
# from each racial/ethnic group come from?

dir.create(
  file.path(graphs_dir, "rq2"),
  showWarnings = FALSE,
  recursive = TRUE
)

# Main student-list orders only.
# Exclude the special AI/AN order from the regular RQ2 race x first-gen plots.
rq2_rfgen_orders_df <- orders_df %>%
  filter(
    order_ids != "487927",
    metro != "houston"
  )

for (i in seq_len(nrow(rq2_rfgen_orders_df))) {
  
  row_i <- rq2_rfgen_orders_df[i, ]
  
  metro_this <- row_i$metro
  order_ids_this <- row_i$order_ids
  test_range_this <- row_i$test_range
  figure_note_this <- row_i$figure_note
  
  message(
    "Creating RQ2 race x first-gen contribution plot for: ",
    metro_this,
    ", order group ",
    order_ids_this
  )
  
  # ----------------------------------------------------------
  # 1) Build inputs
  # ----------------------------------------------------------
  
  ord_nums_this <- stringr::str_split(order_ids_this, "_")[[1]]
  eps_codes_this <- get(row_i$eps_codes, envir = .GlobalEnv)
  
  # ----------------------------------------------------------
  # 2) Create plotting data
  # ----------------------------------------------------------
  
  plot_df <- create_rq2_race_firstgen_contribution_df(
    data       = lists_orders_zip_hs_df_sf,
    ord_nums   = ord_nums_this,
    eps_codes  = eps_codes_this,
    metro      = metro_this,
    order_ids  = order_ids_this,
    test_range = test_range_this,
    exclude_race = c(1, 8, 12)
  )
  
  # ----------------------------------------------------------
  # 3) Create plot
  # ----------------------------------------------------------
  
  plot_obj <- create_rq2_race_firstgen_two_lollipop_plot(
    plot_df = plot_df
  )
  
  # ----------------------------------------------------------
  # 4) Stable file name
  # ----------------------------------------------------------
  
  plot_name <- stringr::str_c(
    "rq2_",
    metro_this,
    "_race_firstgen_contribution_plot_",
    order_ids_this
  )
  
  # ----------------------------------------------------------
  # 5) Dynamic plot height
  # ----------------------------------------------------------
  
  n_geomarkets <- plot_df %>%
    distinct(eps_codename) %>%
    nrow()
  
  plot_height <- max(
    8,
    4.5 + 0.50 * n_geomarkets
  )
  
  ggsave(
    filename = file.path(graphs_dir, "rq2", stringr::str_c(plot_name, ".png")),
    plot     = plot_obj,
    width    = 14,
    height   = plot_height,
    bg       = "white"
  )
  
  # ----------------------------------------------------------
  # 6) Save title/subtitle as .tex sidecar
  # ----------------------------------------------------------
  
  title_tex <- stringr::str_c(
    "#### Geomarket contributions by race and first-generation status: ",
    format_metro_for_slide_subtitle(metro_this),
    ", ",
    format_score_range_for_slide_subtitle(test_range_this)
  )
  
  writeLines(
    title_tex,
    file.path(graphs_dir, "rq2", stringr::str_c(plot_name, "_title.tex"))
  )
  
  # ----------------------------------------------------------
  # 7) Save note
  # ----------------------------------------------------------
  
  note_text <- c(
    "Figure Notes:",
    stringr::str_c(
      "- Each lollipop shows the share of a racial/ethnic × first-generation subgroup contributed by a Geomarket. ",
      "For each racial/ethnic group, Geomarkets are sorted by the first-generation contribution share. ",
      "Excludes students with missing values for race/ethnicity or first-generation status. ",
      "Figure excludes Two+, non-Hispanic; AIAN, non-Hispanic; and NHPI, non-Hispanic groups. ",
      figure_note_this
    )
  )
  
  writeLines(
    note_text,
    file.path(graphs_dir, "rq2", stringr::str_c(plot_name, "_note.txt"))
  )
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
    data         = lists_orders_zip_hs_df_sf,
    ord_nums     = ord_nums_graph,
    eps_codes    = eps_codes_graph,
    metro_name   = metro_name_graph,
    exclude_race = c(1, 8, 12),
    title_suf    = custom_title_suf
  )
}
