
###################
################### implementing a modular approach
###################
###################

create_layer <- function(map, data, var_name, yr, palette) {
  group_name <- paste(var_name, yr)
  
  map <- map %>%
    addPolygons(
      data = data,
      fillColor = ~palette(data[[var_name]]),
      weight = 1,
      opacity = 1,
      color = '#808080',  # Line color
      dashArray = '3',
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 4,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = sprintf(
        "<strong>%s</strong> - %s<br/>%g",
        data$eps, data$eps_name, data[[var_name]]
      ) %>% lapply(htmltools::HTML),
      group = group_name
    )
  
  return(map)
}

# 2. create_legend()
  #This function will create a legend for a given variable.

create_legend <- function(map, data, var_name, palette) {
  map <- map %>%
    addLegend(
      position = "topright",
      pal = palette,
      values = data[[var_name]],
      title = var_name,
      opacity = 0.7,
      className = paste0("legend-", var_name)
    )
  
  return(map)
}

#3. create_layers_control()
  #This function will create the layers control based on the provided group names.

create_layers_control <- function(map, group_names) {
  map <- map %>%
    addLayersControl(
      baseGroups = group_names,
      position = 'bottomleft',
      options = layersControlOptions(collapsed = FALSE)
    )
  
  return(map)
}

# 4. create_eps_maps()
  #This is the main function that coordinates the map creation by calling the above modular functions.

create_eps_maps <- function(eps_codes, vars) {
  
  # Create the leaflet map
  map <- leaflet() %>%
    addProviderTiles(provider = providers$CartoDB.Positron)
  
  # Store group names for layers control
  group_names <- c()
  
  # Initialize a list to store palettes for each variable
  palettes <- list()
  
  # Add layers for each year and variable
  for (var in vars) {
    var_group_names <- c()  # Initialize the group name for each variable
    
    for (yr in c(1980, 2000, 2020)) {
      filtered_data <- allyr_anal_eps_sf %>% filter(year == yr, eps %in% eps_codes)
      
      # Skip if the filtered data is empty or contains only missing values
      if (nrow(filtered_data) == 0 || all(is.na(filtered_data[[var]]))) {
        warning(sprintf("Skipping variable '%s' for year %d because the data is empty or contains only NA values.", var, yr))
        next
      }
      
      # Create a palette for the variable if it hasn't been created already
      if (!var %in% names(palettes)) {
        palettes[[var]] <- if (grepl("inc", var, ignore.case = TRUE)) {
          colorBin(palette = 'YlGnBu', domain = allyr_anal_eps_sf[[var]], bins = c(0, 50000, 75000, 100000, 125000, 150000, 175000, 200000, 225000, 250000))
        } else {
          colorNumeric(palette = "YlGnBu", domain = allyr_anal_eps_sf[[var]], n = 5)
        }
      }
      
      # Add the layer to the map
      map <- create_layer(map, filtered_data, var, yr, palettes[[var]])
      
      # Store the group name for layers control, ensuring order is by variable then year
      var_group_names <- c(var_group_names, paste(var, yr))
    }
    
    # Add the variable's group names to the overall group names
    group_names <- c(group_names, var_group_names)
  }
  
  # Add legends for each variable (only once per variable)
  for (var in vars) {
    if (var %in% names(palettes)) {
      map <- create_legend(map, allyr_anal_eps_sf, var, palettes[[var]])
    }
  }
  
  # Add layers control to the map
  map <- create_layers_control(map, group_names)
  
  # Generate JavaScript to hide/show legends
  legend_classes <- paste0("['legend-", paste(vars, collapse = "','legend-"), "']")
  
  # Use JavaScript to show/hide legends based on the active base layer
  map <- map %>%
    htmlwidgets::onRender(sprintf("
      function(el, x) {
        var map = this;
        
        // Initially hide all legends
        var legend_classes = %s;
        legend_classes.forEach(function(cls) {
          var elem = document.querySelector('.' + cls);
          if (elem) elem.style.display = 'none';
        });

        map.on('baselayerchange', function(e) {
          // Hide all legends
          legend_classes.forEach(function(cls) {
            var elem = document.querySelector('.' + cls);
            if (elem) elem.style.display = 'none';
          });

          // Show the relevant legend
          var active_legend = 'legend-' + e.name.split(' ')[0];
          var active_elem = document.querySelector('.' + active_legend);
          if (active_elem) active_elem.style.display = 'block';
        });
      }
    ", legend_classes))
  
  return(map)
}

# Example usage:
create_eps_maps(bay_eps_codes, c("pct_hisp_all", "mean_inc_house", "pct_nhisp_asian"))
create_eps_maps(bay_eps_codes, c("pct_hisp_all", "med_inc_house_med", "pct_nhisp_asian"))
