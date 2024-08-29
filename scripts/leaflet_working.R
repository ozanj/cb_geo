
# This script contains code that is not my current version but is good enough that I don't want to delete it forever




##### this solution doesn't use any javascript; the downside is that legends are shown for all variables, regardless of which base-layer is shown

create_eps_maps <- function(eps_codes, vars) {
  
  # Create the leaflet map
  map <- leaflet() %>%
    addProviderTiles(provider = providers$CartoDB.Positron)
  
  # Store group names for layers control
  group_names <- c()
  palettes <- list()  # Store palette functions
  domains <- list()   # Store domains (data ranges)
  
  # Iterate through each variable
  for (var in vars) {
    all_years_data <- NULL
    
    # Aggregate data across all years for this variable
    for (yr in c(1980, 2000, 2020)) {
      filtered_data <- allyr_anal_eps_sf %>% filter(year == yr, eps %in% eps_codes)
      if (nrow(filtered_data) > 0 && !all(is.na(filtered_data[[var]]))) {
        all_years_data <- c(all_years_data, filtered_data[[var]])
        
        # Create and add the layer for this year
        max_value <- max(filtered_data[[var]], na.rm = TRUE)
        min_value <- min(filtered_data[[var]], na.rm = TRUE)
        bins <- if (grepl("inc", var, ignore.case = TRUE)) {
          c(0, 50000, 75000, 100000, 125000, 150000, 175000, 200000, 225000, max_value)
        } else {
          pretty(c(min_value, max_value), n = 5)
        }
        
        palette <- colorBin(palette = 'YlGnBu', domain = filtered_data[[var]], bins = bins)
        map <- create_layer(map, filtered_data, var, yr, palette)
        group_names <- c(group_names, paste(var, yr))
      }
    }
    
    # Create a single palette for this variable across all years
    if (!is.null(all_years_data)) {
      combined_palette <- colorBin(palette = 'YlGnBu', domain = all_years_data, bins = bins)
      palettes[[var]] <- combined_palette
      domains[[var]] <- all_years_data
    }
  }
  
  # Add a single legend for each variable
  for (var in vars) {
    if (var %in% names(palettes)) {
      map <- map %>%
        addLegend(
          pal = palettes[[var]], 
          values = domains[[var]],  # Use the combined domain for the variable
          title = var,  
          position = "topright",
          opacity = 1
        )
    }
  }
  
  # Add layers control to the map
  map <- create_layers_control(map, group_names)
  
  return(map)
}

# Example usage:
create_eps_maps(bay_eps_codes, c("pct_hisp_all", "mean_inc_house", "pct_nhisp_asian"))
create_eps_maps(socal_eps_codes, c("pct_hisp_all", "mean_inc_house", "pct_nhisp_asian"))


## this code doesn't use javascript. it works. and is more efficient (fewer lines) than the other solution cuz it uses apply rather than looping across years.
  # but I like the version that loops across years cuz I understand it better.

create_eps_maps <- function(eps_codes, vars) {
  
  map <- leaflet() %>%
    addProviderTiles(provider = providers$CartoDB.Positron)
  
  group_names <- c()
  palettes <- list()
  
  for (var in vars) {
    all_years_data <- unlist(lapply(c(1980, 2000, 2020), function(yr) {
      filtered_data <- allyr_anal_eps_sf %>% filter(year == yr, eps %in% eps_codes)
      if (nrow(filtered_data) > 0 && !all(is.na(filtered_data[[var]]))) {
        return(filtered_data[[var]])
      }
      NULL
    }))
    
    if (length(all_years_data) > 0) {
      bins <- if (grepl("inc", var, ignore.case = TRUE)) {
        c(0, 50000, 75000, 100000, 125000, 150000, 175000, 200000, 225000, max(all_years_data, na.rm = TRUE))
      } else {
        pretty(range(all_years_data, na.rm = TRUE), n = 5)
      }
      
      combined_palette <- colorBin(palette = 'YlGnBu', domain = all_years_data, bins = bins)
      palettes[[var]] <- combined_palette
      
      for (yr in c(1980, 2000, 2020)) {
        filtered_data <- allyr_anal_eps_sf %>% filter(year == yr, eps %in% eps_codes)
        if (nrow(filtered_data) > 0 && !all(is.na(filtered_data[[var]]))) {
          map <- create_layer(map, filtered_data, var, yr, combined_palette)
          group_names <- c(group_names, paste(var, yr))
        }
      }
      
      map <- map %>%
        addLegend(
          pal = combined_palette, 
          values = all_years_data, 
          title = var,  
          position = "topright",
          opacity = 1
        )
    }
  }
  
  map <- create_layers_control(map, group_names)
  
  return(map)
}

# Example usage:
create_eps_maps(bay_eps_codes, c("pct_hisp_all", "mean_inc_house", "pct_nhisp_asian"))






# this solution works except legend doesn't have legend color or white background
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
      
      # Adjust bins to cover the range of data values
      max_value <- max(filtered_data[[var]], na.rm = TRUE)
      min_value <- min(filtered_data[[var]], na.rm = TRUE)
      bins <- if (grepl("inc", var, ignore.case = TRUE)) {
        c(0, 50000, 75000, 100000, 125000, 150000, 175000, 200000, 225000, max_value)
      } else {
        pretty(c(min_value, max_value), n = 5)
      }
      
      # Create a palette for the variable
      palettes[[var]] <- colorBin(palette = 'YlGnBu', domain = filtered_data[[var]], bins = bins)
      
      # Add the layer to the map
      map <- create_layer(map, filtered_data, var, yr, palettes[[var]])
      
      # Store the group name for layers control, ensuring order is by variable then year
      var_group_names <- c(var_group_names, paste(var, yr))
    }
    
    # Add the variable's group names to the overall group names
    group_names <- c(group_names, var_group_names)
  }
  
  # Add legends for each variable and year
  legend_classes <- c()  # Store legend classes for JavaScript control
  for (var in vars) {
    for (yr in c(1980, 2000, 2020)) {
      if (var %in% names(palettes)) {
        legend_class <- paste0("legend-", var, "-", yr)
        map <- map %>%
          addLegend(pal = palettes[[var]], values = allyr_anal_eps_sf[[var]], 
                    title = paste(var, yr),  
                    position = "topright",  
                    className = legend_class,  # Assign a unique class name for each legend
                    opacity = 1,
                    layerId = legend_class)
        legend_classes <- c(legend_classes, legend_class)  # Add legend class
      }
    }
  }
  
  # Add layers control to the map
  map <- create_layers_control(map, group_names)
  
  # Inject CSS and JavaScript for legend management
  map <- map %>%
    htmlwidgets::onRender(sprintf("
      function(el, x) {
        var style = document.createElement('style');
        style.type = 'text/css';
        style.innerHTML = `
          .leaflet-control .legend { 
            background-color: white !important; 
            padding: 5px; 
            border-radius: 5px; 
            border: 1px solid black;
            color: black !important;
          }
          .leaflet-control .legend label { 
            display: block; 
            font-size: 12px;
          }
        `;
        document.getElementsByTagName('head')[0].appendChild(style);

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
          var active_legend = 'legend-' + e.name.replace(/ /g, '-');
          var active_elem = document.querySelector('.' + active_legend);
          if (active_elem) active_elem.style.display = 'block';
        });
      }
    ", jsonlite::toJSON(legend_classes)))
  
  return(map)
}

# Example usage with multiple variables:
create_eps_maps(bay_eps_codes, c("pct_hisp_all", "mean_inc_house", "pct_nhisp_asian"))



### this is the more simple version of create_legend [from 8/28/2024]

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



#####################
#####################
##################### this is not a modular version; but does show data for all years
#####################


create_eps_maps <- function(eps_codes) {
  
  # Define common palettes and breaks for all years
  breaks_inc <- c(0, 50000, 75000, 100000, 125000, 150000, 175000, 200000)
  pal_inc <- colorBin(
    palette = 'YlGnBu',
    domain = allyr_anal_eps_sf$med_inc_house_med,
    bins = breaks_inc
  )
  
  pal_white <- colorNumeric(
    palette = "YlGnBu",
    domain = allyr_anal_eps_sf$pct_nhisp_white,
    n = 5
  )
  
  pal_black <- colorNumeric(
    palette = 'YlGnBu',
    domain = allyr_anal_eps_sf$pct_nhisp_black,
    n = 5
  )
  
  # Create the leaflet map
  map <- leaflet() %>%
    addProviderTiles(provider = providers$CartoDB.Positron)
  
  # Initialize vectors to store group names
  group_names_inc <- c()
  group_names_white <- c()
  group_names_black <- c()
  
  # Add layers for each year
  for (yr in c(1980, 2000, 2020)) {
    
    # Update the group name for median income
    group_name_inc <- paste("Median income", yr)
    group_name_white <- paste("%White (non-Hispanic)", yr)
    group_name_black <- paste("%Black (non-Hispanic)", yr)
    
    # Store group names in vectors
    group_names_inc <- c(group_names_inc, group_name_inc)
    group_names_white <- c(group_names_white, group_name_white)
    group_names_black <- c(group_names_black, group_name_black)
    
    filtered_data <- allyr_anal_eps_sf %>% filter(year == yr, eps %in% eps_codes)
    
    # Add Median income layer for each year
    map <- map %>%
      addPolygons(
        data = filtered_data,
        fillColor = ~pal_inc(med_inc_house_med),
        weight = 1,
        opacity = 1,
        color = '#808080',
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
          "<strong>%s</strong> - %s<br/>$%gk median income",
          filtered_data$eps, filtered_data$eps_name, round(filtered_data$med_inc_house_med / 1000)
        ) %>% lapply(htmltools::HTML),
        group = group_name_inc  # Use updated group name variable
      ) %>%
      # Add pct_nhisp_white layer for each year
      addPolygons(
        data = filtered_data,
        fillColor = ~pal_white(pct_nhisp_white),
        weight = 1,
        opacity = 1,
        color = '#808080',
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
          "<strong>%s</strong> - %s<br/>%g%% NH White",
          filtered_data$eps, filtered_data$eps_name, round(filtered_data$pct_nhisp_white)
        ) %>% lapply(htmltools::HTML),
        group = group_name_white
      ) %>%
      # Add pct_nhisp_black layer for each year
      addPolygons(
        data = filtered_data,
        fillColor = ~pal_black(pct_nhisp_black),
        weight = 1,
        opacity = 1,
        color = '#808080',
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
          "<strong>%s</strong> - %s<br/>%g%% NH Black",
          filtered_data$eps, filtered_data$eps_name, round(filtered_data$pct_nhisp_black)
        ) %>% lapply(htmltools::HTML),
        group = group_name_black
      )
  }
  
  # Add legends for each group, but hide them initially using custom HTML classes
  map <- map %>%
    addLegend(
      position = "topright",
      pal = pal_inc,
      values = allyr_anal_eps_sf$med_inc_house_med,
      title = "Median Income",
      labFormat = labelFormat(prefix = "$", suffix = "k", transform = function(x) x/1000),
      opacity = 0.7,
      className = "legend-inc"
    ) %>%
    addLegend(
      position = "topright",
      pal = pal_white,
      values = allyr_anal_eps_sf$pct_nhisp_white,
      title = "% White (non-Hispanic)",
      labFormat = labelFormat(suffix = "%"),
      opacity = 0.7,
      className = "legend-white"
    ) %>%
    addLegend(
      position = "topright",
      pal = pal_black,
      values = allyr_anal_eps_sf$pct_nhisp_black,
      title = "% Black (non-Hispanic)",
      labFormat = labelFormat(suffix = "%"),
      opacity = 0.7,
      className = "legend-black"
    ) %>%
    
    # Add layers control to switch between years using the group name variables
    addLayersControl(
      baseGroups = c(group_names_inc, group_names_white, group_names_black),
      position = 'bottomleft',
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    
    # Use JavaScript to show/hide legends based on the active base layer
    htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        
        // Initially hide all legends
        document.querySelector('.legend-inc').style.display = 'none';
        document.querySelector('.legend-white').style.display = 'none';
        document.querySelector('.legend-black').style.display = 'none';

        // Apply custom CSS for white, non-transparent borders
        var legends = document.getElementsByClassName('leaflet-control');
        for (var i = 0; i < legends.length; i++) {
          legends[i].style.border = '2px solid white';
          legends[i].style.background = 'rgba(255, 255, 255, 1)';
          legends[i].style.padding = '8px';
          legends[i].style.boxShadow = '0 0 15px rgba(0, 0, 0, 0.3)';
        }

        map.on('baselayerchange', function(e) {
          // Hide all legends
          document.querySelector('.legend-inc').style.display = 'none';
          document.querySelector('.legend-white').style.display = 'none';
          document.querySelector('.legend-black').style.display = 'none';

          // Show the relevant legend
          if (e.name.includes('Median income')) { 
            document.querySelector('.legend-inc').style.display = 'block';
          } else if (e.name.includes('%White (non-Hispanic)')) {
            document.querySelector('.legend-white').style.display = 'block';
          } else if (e.name.includes('%Black (non-Hispanic)')) {
            document.querySelector('.legend-black').style.display = 'block';
          }
        });
      }
    ")
  
  return(map)
}
# additional capabilities to add:
# can switch which metro area from the map
# can change which variables to create layers for from the function call
# pct_nhisp_white
# pct_nhisp_black
# pct_hisp_all
# experiment with quantiles vs. bins vs. continuous
# advise on better color schemes


# Example usage
create_eps_maps(socal_eps_codes)

create_eps_maps(bay_eps_codes)

create_eps_maps(chi_eps_codes)

create_eps_maps(cleveland_eps_codes)
create_eps_maps(c('OH 3','OH 4','OH 5'))

allyr_anal_eps_sf %>% filter(year == 2020) %>% select(eps,eps_name,pct_nhisp_black,pct_hisp_all) %>% View()

#####################
##################### this is the basic version that does not use htmlwidgets/javascript; and doesn't show data for all years; it is not a modular version
#####################

create_eps_maps <- function(yr, eps_codes) {
  
  filtered_data <- allyr_anal_eps_sf %>% filter(year == yr, eps %in% eps_codes)
  
  
  # color of fill for median income
  breaks_inc <- c(0, 50000, 75000, 100000, 125000, 150000, 175000, 200000)
  pal_inc <- colorBin(
    palette = 'YlGnBu', # Use a predefined color palette (Yellow-Orange-Red)
    domain = allyr_anal_eps_sf$med_inc_house_med,
    bins = breaks_inc
  )
  
  # color of fill for pct_nhisp_white
  pal_white <- colorBin(palette = "YlGnBu", domain = allyr_anal_eps_sf$pct_nhisp_white, bins = 5, pretty = TRUE)
  
  # color of fill for pct_nhisp_black
  pal_black <- colorBin(palette = "YlGnBu", domain = allyr_anal_eps_sf$pct_nhisp_black, bins = 5, pretty = TRUE)
  
  
  # labels for median income
  labels_inc <- sprintf(
    "<strong>%s</strong> - %s<br/>$%gk median income",
    filtered_data$eps, filtered_data$eps_name, round(filtered_data$med_inc_house_med / 1000)
  ) %>% lapply(htmltools::HTML)
  
  # labels for pct_nhisp_white
  labels_white <- sprintf(
    "<strong>%s</strong> - %s<br/>%g%% NH White",
    filtered_data$eps, filtered_data$eps_name, round(filtered_data$pct_nhisp_white)
  ) %>% lapply(htmltools::HTML)
  
  # labels for pct_nhisp_black
  labels_black <- sprintf(
    "<strong>%s</strong> - %s<br/>%g%% NH Black",
    filtered_data$eps, filtered_data$eps_name, round(filtered_data$pct_nhisp_black)
  ) %>% lapply(htmltools::HTML)
  
  leaflet(data = filtered_data) %>%
    addProviderTiles(
      provider = providers$CartoDB.Positron
    ) %>%  
    # Median income layer
    addPolygons(
      fillColor = ~pal_inc(med_inc_house_med),
      weight = 1,
      opacity = 1,
      color = 'white',
      dashArray = '3',
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 4,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = labels_inc,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      ),
      group = "Median inc"
    ) %>%
    # pct_nhisp_white layer
    addPolygons(
      fillColor = ~pal_white(pct_nhisp_white),
      weight = 1,
      opacity = 1,
      color = 'white',
      dashArray = '3',
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 4,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = labels_white,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      ),
      group = "%White (non-Hispanic)"
    ) %>%
    # pct_nhisp_black layer
    addPolygons(
      fillColor = ~pal_black(pct_nhisp_black),
      weight = 1,
      opacity = 1,
      color = 'white',
      dashArray = '3',
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 4,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = labels_black,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      ),
      group = "%Black (non-Hispanic)"
    ) %>%
    # Layers control
    addLayersControl(
      position = c('bottomleft'),
      baseGroups = c('Median inc', '%White (non-Hispanic)', '%Black (non-Hispanic)'),
      options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    # Legend for median income
    addLegend(
      position = "topright",
      pal = pal_inc,
      values = ~med_inc_house_med,
      title = "Median Income",
      labFormat = labelFormat(prefix = "$", suffix = "k", transform = function(x) x/1000),
      opacity = 0.7
    ) %>%
    # Legend for pct_nhisp_white
    addLegend(
      position = "topright",
      pal = pal_white,
      values = ~pct_nhisp_white,
      title = "%White<br>(non-Hispanic)",
      labFormat = labelFormat(suffix = "%"),
      opacity = 0.7
    ) %>%
    # Legend for pct_nhisp_black
    addLegend(
      position = "topright",
      pal = pal_black,
      values = ~pct_nhisp_black,
      title = "%Black<br>(non-Hispanic)",
      labFormat = labelFormat(suffix = "%"),
      opacity = 0.7
    )
}

# additional capabilities to add:
# can switch which metro area from the map
# can switch which year of data from the map
# can change which variables are shown in legend
# pct_nhisp_white
# pct_nhisp_black
# experiment with quantiles vs. bins vs. continuous
# add legend; 
# advise on better color schemes


create_eps_maps(yr = 1980, eps_codes = bay_eps_codes)

create_eps_maps(1980,bay_eps_codes)
create_eps_maps(2020,bay_eps_codes)

create_eps_maps(1980,socal_eps_codes)

create_eps_maps(1980,chi_eps_codes)
create_eps_maps(2000,chi_eps_codes)
create_eps_maps(2020,chi_eps_codes)

create_eps_maps(cleveland_eps_codes)
create_eps_maps(c('OH 3','OH 4','OH 5'))
# Example usage with chi_eps_codes
#create_eps_maps(chi_eps_codes)

create_eps_maps(1980,bay_eps_codes)

create_eps_maps(chi_eps_codes)
#create_eps_maps(bay_eps_codes)


