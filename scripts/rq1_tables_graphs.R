################################################################################
## [ PROJ ] < College Board Geomarket >
## [ FILE ] < rq1_tables_graphs.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 1/13/2025
## [ DESC ] < functions that create tables and graphs for RQ1 (differences between geomarkets, based on census data) >
################################################################################

### SETTINGS
#rm(list = ls()) # remove all objects
options(max.print=1000)
#options(width = 160)
# Set the scipen option to a high value to avoid scientific notation
options(scipen = 999)

### LIBRARIES
library(tidyverse)
library(ggh4x) # additional graphing capabilities
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

### DIRECTORY PATHS

# run script that creates directories
source(file = file.path('scripts', 'directories.R'))

######## DATA PREP

# run script that appends data
list.files(path = file.path('.',scripts_dir))
source(file = file.path(scripts_dir, 'append_census.R'))

# script that creates character vectors for EPS codes
source(file = file.path(scripts_dir, 'metro_eps_codes.R'))

# functions for TableX

########## FUNCTION TO CREATE TABLE OF STATS FOR GEOMARKETS IN A METRO AREA

allyr_anal_tract_sf %>% glimpse()

table_var_names <- list(
  "pct_hisp_all" = "% Hispanic",
  "mean_inc_house" = "Mean income",
  "med_inc_house" = "Median income",
  "pct_nhisp_asian" = "% Asian, non-Hispanic",
  "pct_nhisp_white" = "% White, non-Hispanic",
  "pct_nhisp_black" = "% Black, non-Hispanic",
  "pct_nhisp_api" = "% API, non-Hispanic", 
  "pct_nhisp_native" = "% Native, non-Hispanic",
  "pct_pov_yes" = "% in poverty",
  "pct_edu_baplus_all" = "% with BA+"
)

# Define table_vars in the desired order
table_varlist <- c("pct_nhisp_white", "pct_nhisp_black", "pct_hisp_all", "pct_nhisp_api", "pct_nhisp_native", "pct_nhisp_multi", "med_inc_house", "mean_inc_house", "pct_pov_yes", "pct_edu_baplus_all")

create_eps_table <- function(eps_codes, table_vars) {
  
  # Human-readable names for the variables (you can modify this list based on the variables passed in)
  table_var_names <- list(
    "pct_hisp_all" = "% Hispanic",
    "mean_inc_house" = "Mean income",
    "med_inc_house" = "Median income",
    "pct_nhisp_asian" = "% Asian, non-Hispanic",
    "pct_nhisp_white" = "% White, non-Hispanic",
    "pct_nhisp_black" = "% Black, non-Hispanic",
    "pct_nhisp_api" = "% API, non-Hispanic", 
    "pct_nhisp_native" = "% Native, non-Hispanic",
    "pct_pov_yes" = "% in poverty",
    "pct_edu_baplus_all" = "% with BA+",
    "pct_nhisp_multi" = "% Two+, non-Hispanic"
  )
  
  # Main table creation pipeline
  eps_table <- allyr_anal_tract_sf %>% 
    as.data.frame() %>%
    # Filter based on provided eps_codes
    filter(eps %in% eps_codes) %>%
    
    # Create the eps_codename by concatenating eps_code and eps_name with a comma
    mutate(eps_codename = str_c(eps, eps_name, sep = ", ")) %>%
    
    # Divide income variables by 1,000
    mutate(across(contains("_inc"), ~ (.x / 1000))) %>%
    
    # Group by the new eps_codename and year
    group_by(eps_codename, year) %>%
    
    # Summarize the data (mean, standard deviation, percentiles)
    summarize(
      across(all_of(table_vars), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
      across(all_of(table_vars), ~ sd(.x, na.rm = TRUE), .names = "sd_{.col}"),
      across(all_of(table_vars), ~ quantile(.x, probs = 0.25, na.rm = TRUE), .names = "p25_{.col}"),
      across(all_of(table_vars), ~ quantile(.x, probs = 0.50, na.rm = TRUE), .names = "p50_{.col}"),
      across(all_of(table_vars), ~ quantile(.x, probs = 0.75, na.rm = TRUE), .names = "p75_{.col}")
    ) %>%
    
    # Reshape variables from columns to rows
    pivot_longer(
      cols = starts_with("mean_") | starts_with("sd_") | starts_with("p25_") | starts_with("p50_") | starts_with("p75_"),
      names_to = c("statistic", "variable"),
      names_pattern = "(mean|sd|p25|p50|p75)_(.*)",  # Regex pattern to correctly split the names
      values_to = "value"
    ) %>%
    
    # Replace variable names with human-friendly names
    mutate(variable = recode(variable, !!!table_var_names)) %>%
    
    # Set the order of the 'variable' column based on the provided table_vars
    mutate(variable = factor(variable, levels = recode(table_vars, !!!table_var_names))) %>%
    
    # Reshape statistics and years from rows to columns
    pivot_wider(
      names_from = c(year, statistic),  # Pivot both year and statistic to columns
      values_from = value,
      names_glue = "{statistic}_{year}"  # Customize the new column names to include only statistic and year
    ) %>%
    
    # Ungroup and arrange by variable and eps_codename
    ungroup() %>%
    arrange(variable, eps_codename) %>%
    
    # Apply integer formatting for income variables and 1 decimal point for percentage variables
    mutate(
      across(starts_with("mean_") | starts_with("sd_") | starts_with("p25_") | starts_with("p50_") | starts_with("p75_"),
             ~ if_else(str_detect(variable, "income"), round(.x), .x)),
      across(starts_with("mean_") | starts_with("sd_") | starts_with("p25_") | starts_with("p50_") | starts_with("p75_"),
             ~ if_else(str_detect(variable, "^%"), round(.x, 1), .x))
    )
  
  # Return the resulting table
  return(eps_table)
}

# Example usage:
chi_eps_table <- create_eps_table(chi_eps_codes, table_varlist)

# Example usage:
chi_eps_table <- create_eps_table(chi_eps_codes, table_varlist)
chi_eps_table

# Example usage:
chi_eps_table <- create_eps_table(eps_codes = chi_eps_codes, table_vars = table_varlist)
chi_eps_table %>% print(n=100)

saveRDS(chi_eps_table, file.path(tables_dir, "chi_eps_table.rds"))


bay_area_eps_table <- create_eps_table(eps_codes = bay_area_eps_codes, table_vars = table_varlist)
bay_area_eps_table %>% print(n=100)

socal_eps_table <- create_eps_table(eps_codes = socal_eps_codes, table_vars = table_varlist)    
socal_eps_table %>% print(n=100)

philly_eps_table <- create_eps_table(eps_codes = philly_eps_codes, table_vars = table_varlist)    
philly_eps_table %>% print(n=100)  

saveRDS(socal_eps_table, file.path(tables_dir, "socal_eps_table.rds"))
saveRDS(philly_eps_table, file.path(tables_dir, "philly_eps_table.rds"))

########## FUNCTION TO CREATE (INPUT DATA FRAME FOR) GRAPHS RELEVANT TO RQ1

# Human-readable names for the variables (you can modify this list based on the variables passed in)
graph_var_names <- list(
  "nhisp_white" = "White, non-Hispanic",
  "nhisp_black" = "Black, non-Hispanic",
  "hisp_all" = "Hispanic",
  "nhisp_asian" = "Asian, non-Hispanic",
  "nhisp_nhpi" = "NHPI, non-Hispanic",
  "nhisp_api" = "API, non-Hispanic", 
  "nhisp_native" = "AIAN, non-Hispanic",
  "nhisp_multi" = "Two+, non-Hispanic",
  "pct_nhisp_white" = "% White, non-Hispanic",
  "pct_nhisp_black" = "% Black, non-Hispanic",
  "pct_hisp_all" = "% Hispanic",
  "pct_nhisp_asian" = "% Asian, non-Hispanic",
  "pct_nhisp_nhpi" = "% NHPI, non-Hispanic",
  "pct_nhisp_api" = "% API, non-Hispanic", 
  "pct_nhisp_native" = "% AIAN, non-Hispanic",
  "pct_nhisp_multi" = "% Two+, non-Hispanic",
  "mean_inc_house" = "Mean income",
  "med_inc_house" = "Median income",
  "pct_pov_yes" = "% in poverty",
  "pct_edu_baplus_all" = "% with BA+"
)
graph_var_names

# Define graph_vars in the desired order
graph_varlist <- c("nhisp_white", "nhisp_black", "hisp_all", "nhisp_asian", "nhisp_nhpi", "nhisp_api", "nhisp_native", "nhisp_multi", "pct_nhisp_white", "pct_nhisp_black", "pct_hisp_all", "pct_nhisp_asian", "pct_nhisp_nhpi", "pct_nhisp_api", "pct_nhisp_native", "pct_nhisp_multi", "med_inc_house", "mean_inc_house", "pct_pov_yes", "pct_edu_baplus_all")

create_eps_graph <- function(eps_codes, 
                             prefix = NULL,
                             graph_vars = graph_varlist, 
                             graph_type = "ses", 
                             stat = ifelse(graph_type == "race", "sum", "p50"), 
                             facet_rows = vars(year), 
                             facet_cols = vars(variable), 
                             x_label = NULL, 
                             y_label = NULL, 
                             plot_title = NULL, 
                             plot_subtitle = NULL,
                             theme_custom = theme_minimal(), 
                             fill_palette = NULL, 
                             debug = FALSE,
                             note = NULL) {
  
  # Validate inputs
  if (!graph_type %in% c("race", "ses")) {
    stop("Invalid `graph_type`. Please specify 'race' or 'ses'.")
  }
  if (missing(eps_codes) || length(eps_codes) == 0) {
    stop("Please provide a valid `eps_codes` vector.")
  }
  
  # 1) Determine prefix for filenames
  if (is.null(prefix)) {
    # Fall back to old logic: use the variable name for eps_codes
    file_prefix <- str_replace(deparse(substitute(eps_codes)), '_eps_codes', '')
  } else {
    file_prefix <- prefix
  }
  
  # 2) Build the main table to plot
  eps_table <- allyr_anal_tract_sf %>% 
    as.data.frame() %>%
    filter(eps %in% eps_codes) %>%
    mutate(eps_codename = str_c(eps, eps_name, sep = ", ")) %>%
    mutate(across(contains("_inc"), ~ (.x / 1000))) %>%
    group_by(eps_codename, year) %>%
    summarize(
      across(all_of(graph_vars), ~ sum(.x, na.rm = TRUE), .names = "sum_{.col}"),
      across(all_of(graph_vars), ~ mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
      across(all_of(graph_vars), ~ sd(.x, na.rm = TRUE), .names = "sd_{.col}"),
      across(all_of(graph_vars), ~ quantile(.x, probs = 0.25, na.rm = TRUE), .names = "p25_{.col}"),
      across(all_of(graph_vars), ~ quantile(.x, probs = 0.50, na.rm = TRUE), .names = "p50_{.col}"),
      across(all_of(graph_vars), ~ quantile(.x, probs = 0.75, na.rm = TRUE), .names = "p75_{.col}")
    ) %>% 
    ungroup() %>%
    mutate(
      sum_has_race = rowSums(
        select(., sum_nhisp_white, sum_nhisp_black, sum_hisp_all, sum_nhisp_asian, 
               sum_nhisp_nhpi, sum_nhisp_native, sum_nhisp_multi),
        na.rm = TRUE
      )
    ) %>% 
    relocate(eps_codename, year, sum_has_race) %>% 
    pivot_longer(
      cols = starts_with("sum_") | starts_with("mean_") | starts_with("sd_") | 
        starts_with("p25_") | starts_with("p50_") | starts_with("p75_"),
      names_to = c("statistic", "variable"),
      names_pattern = "(sum|mean|sd|p25|p50|p75)_(.*)",
      values_to = "value"
    ) %>%
    mutate(variable = recode(variable, !!!graph_var_names)) %>%
    mutate(
      variable = factor(
        variable,
        levels = c(
          "White, non-Hispanic", "Asian, non-Hispanic", "API, non-Hispanic",
          "Black, non-Hispanic", "Hispanic", "Two+, non-Hispanic", 
          "NHPI, non-Hispanic", "AIAN, non-Hispanic",
          "% White, non-Hispanic", "% Asian, non-Hispanic", "% API, non-Hispanic", 
          "% Black, non-Hispanic", "% Hispanic", "% Two+, non-Hispanic", 
          "% NHPI, non-Hispanic", "% AIAN, non-Hispanic",
          "Median income", "Mean income", "% in poverty", "% with BA+"
        )
      )
    ) %>%
    mutate(eps_codename = fct_rev(factor(eps_codename))) %>%
    mutate(year = factor(year, levels = c(2020, 2000, 1980)))
  
  # If debug=TRUE, show intermediate table
  if (debug) {
    message("Using statistic: ", stat)
    print(head(eps_table))
  }
  
  # 3) Filter table & build plot
  if (graph_type == "race") {
    # Race/ethnicity approach
    eps_table <- eps_table %>%
      filter(
        statistic == stat,
        variable %in% c(
          "White, non-Hispanic", "Black, non-Hispanic", "Hispanic", 
          "Asian, non-Hispanic", "Two+, non-Hispanic", 
          "NHPI, non-Hispanic", "AIAN, non-Hispanic"
        )
      )
    
    plot <- eps_table %>%
      ggplot(aes(x = eps_codename, y = value, fill = variable)) +
      geom_col(position = position_fill(reverse = TRUE)) +
      coord_flip() +
      labs(
        x = x_label, y = y_label,
        title = plot_title, subtitle = plot_subtitle,
        fill = "Race/Ethnicity"
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      facet_wrap(~ year, ncol = 1, scales = "free_y") +
      theme_custom +
      theme(
        legend.text  = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.text.y  = element_text(size = 12),
        axis.text.x  = element_text(size = 12),
        strip.text   = element_text(size = 14)
      )
    
    if (!is.null(fill_palette)) {
      plot <- plot + scale_fill_manual(values = fill_palette)
    }
    
  } else {
    # SES approach
    eps_table <- eps_table %>%
      filter(
        statistic == stat,
        variable %in% c("Median income", "% in poverty", "% with BA+")
      )
    
    plot <- eps_table %>%
      ggplot(aes(x = eps_codename, y = value)) +
      geom_col(fill = "grey50", position = position_dodge(width = 0.8), width = 0.7) +
      coord_flip() +
      labs(
        x = x_label, y = y_label,
        title = plot_title, subtitle = plot_subtitle
      ) +
      facet_grid2(
        rows = facet_rows,
        cols = facet_cols,
        scales = "free",
        space = "fixed"
      ) +
      theme_custom +
      theme(
        strip.text.x = element_text(size = 12, face = "bold"),
        strip.text.y = element_text(size = 12, face = "bold", angle = 90),
        axis.text.y  = element_text(size = 12),
        axis.text.x  = element_text(size = 12),
        panel.spacing = unit(1, "lines"),
        legend.position = "none"
      ) +
      facetted_pos_scales(
        y = list(
          (variable == "Median income") ~ scale_y_continuous(
            labels = scales::label_number(suffix = "k")
          ),
          (variable %in% c("% in poverty", "% with BA+")) ~ scale_y_continuous(
            labels = scales::label_number(suffix = "%")
          )
        )
      )
  }
  
  # 4) Construct file names & write
  file_prefix_underscore <- str_replace_all(string = file_prefix, pattern = ' ', replacement = '_')
  message("File prefix: ", file_prefix)
  
  plot_name <- str_c("rq1", file_prefix_underscore, graph_type, sep = "_")
  message("Plot name: ", plot_name)
  
  ggsave(
    filename = file.path(graphs_dir, "rq1", str_c(plot_name, ".png")),
    plot = plot,
    width = 14,
    height = 8,
    bg = "white"
  )
  
  # 5) Determine which_characteristics for figure_title
  if (graph_type == "race") {
    which_characteristics <- "Racial/ethnic composition"
  } else {
    which_characteristics <- "Socioeconomic characteristics"
  }
  
  # 6) Friendly prefix, handle special cases for text
  friendly_prefix <- str_to_title(file_prefix)
  
  # e.g. "dc maryland virginia" -> "D.C., Maryland, and Virginia"
  if (file_prefix == "dc maryland virginia") {
    friendly_prefix <- "D.C., Maryland, and Virginia"
  }
  
  # Construct figure_title
  if (friendly_prefix == "Bay Area") {
    # Omit "area" for Bay Area
    figure_title <- str_c(
      which_characteristics,
      "of",
      friendly_prefix,
      "Geomarkets",
      sep = " "
    )
  } else {
    # Normal approach: "<which_characteristics> of <friendly_prefix> area Geomarkets"
    figure_title <- str_c(
      which_characteristics,
      "of",
      friendly_prefix,
      "area Geomarkets",
      sep = " "
    )
  }
  
  message("Figure Title: ", figure_title)
  
  # 7) Build note_text differently for race vs. ses
  if (graph_type == "race") {
    note_text <- c(
      "Figure Notes:",
      "- Race/ethnicity categories not available in 1980 Census: Asian, non-Hispanic; Two+ races, non-Hispanic; NHPI, non-Hispanic; AIAN non-Hispanic",
      note
    )
  } else {
    note_text <- c(
      "Figure Notes:",
      "- Household income measured using 2024 CPI",
      note
    )
  }
  
  # 8) Save figure_title and note_text
  writeLines(figure_title, file.path(graphs_dir, "rq1", str_c(plot_name, "title.txt", sep = "_")))
  writeLines(note_text,   file.path(graphs_dir, "rq1", str_c(plot_name, "note.txt",  sep = "_")))
  
  # Return the plot object
  return(plot)
}



all_codes <- list(
  'philadelphia' = philly_eps_codes,
  'dallas' = dallas_eps_codes,
  'atlanta' = atl_eps_codes,
  'chicago' = chi_eps_codes,
  'cleveland' = cleveland_eps_codes,
  'northern new jersey' = nj_north_metro_eps_codes,
  'houston' = houston_eps_codes,
  'bay area' = bay_area_eps_codes,
  'long island' = long_island_eps_codes,
  'new york city' = nyny_metro_eps_codes,
  'detroit' = detroit_eps_codes,
  'boston' = boston_eps_codes,
  'miami' = miami_eps_codes,
  'dc maryland virginia' = dmv_eps_codes,
  'orange county' = orange_county_eps_codes,
  'san diego' = san_diego_eps_codes,
  'los angeles' = los_angeles_eps_codes
)
all_codes

graph_types = c('race','ses')
graph_types


for (metro_name in names(all_codes)) {
  metro_codes <- all_codes[[metro_name]]
  
  for (g in seq_along(graph_types)) {
    create_eps_graph(
      eps_codes  = metro_codes,
      prefix     = metro_name,        # <--- pass "philly" or "dallas"
      graph_type = graph_types[g],
      note       = NULL
    )
  }
}

