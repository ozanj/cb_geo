library(leaflet)
library(tidyverse)
library(formattable)
library(htmlwidgets)


# Load data

source(file.path('scripts', 'map_functions.R'))


# Map functions

create_rq2_map <- function(metros) {
  js <- read_file(file.path(scripts_dir, 'rq2_maps.js'))
  
  choices <- list(region_choices = regions_data %>% filter(region %in% metros) %>% select(region, region_name, latitude, longitude))
  
  highlight_shp <- highlightOptions(weight = 2, color = '#606060', dashArray = '', bringToFront = T, sendToBack = T)
  
  m <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    addMiniMap(tiles = providers$CartoDB.Positron,
               toggleDisplay = T) %>%
    
    addEasyButton(easyButton(
      icon = 'fa-crosshairs', title = 'Toggle View',
      onClick = JS("function(btn, map){ let zoom = $(btn.button).attr('data-national'); $('.custom-control').slideUp(); if (zoom === 'true') { let $sel = $('input[name=\"metro-choice\"]:checked'); map.setView([$sel.attr('data-lng'), $sel.attr('data-lat')], 8.2); $(btn.button).attr('data-national', false); $('#view-btn').html('National View'); } else { map.setView([39.828, -98.580], 4); $(btn.button).attr('data-national', true); $('#view-btn').html('MSA View'); }}"))) %>%
    
    addEasyButton(easyButton(
      icon = 'fa-globe', title = 'Select Metro Area',
      onClick = JS("function(btn, map){ $('.custom-control').not('#metro-control').slideUp(); $('#metro-control').slideToggle(); }")))
  
  for (metro in metros) {
    print(metro)
    
    region <- regions_data %>% filter(region == metro)
    eps <- eps_data %>% filter(eps %in% region$eps[[1]], year == 2020)
    tract <- tract_data %>% filter(eps %in% region$eps[[1]], year == 2020)
    
    m <- m %>%
      
      # Metro outline
      addPolylines(data = eps, opacity = 1, color = '#707070', weight = 1.2, group = 'MSA', options = c(className = paste0('metro-shape metro-line metro-', metro))) %>% 
      
      # EPS outline
      addPolylines(data = eps, opacity = 1, color = 'purple', weight = 2, options = c(className = paste0('metro-shape metro-', metro)))
    
    for (v in names(base_vars)) {
      
      color_pal_tract <- get_palette(v, tract[[v]], 'tract')
      
      group_name <- if_else(str_detect(base_vars[[v]]$name, 'Hispanic'), base_vars[[v]]$name, paste0('MSA by ', base_vars[[v]]$name))
      
      m <- m %>% 
        
        # Shapes
        addPolygons(data = tract, opacity = 1, color = '#808080', weight = 1, dashArray = '3', fillOpacity = 0.8, smoothFactor = 0.2, fillColor = ~color_pal_tract$palette(get(v)), label = ~paste0('<b>', eps, ' - ', eps_name, '</b><br>', get(paste0(v, '_text'))) %>% lapply(htmltools::HTML), group = group_name, highlightOptions = highlight_shp, options = pathOptions(className = paste0('metro-shape metro-', metro))) %>%
      
        addLegend(data = tract,
                  position = 'topright', pal = color_pal_tract$palette, values = ~get(v),
                  title = base_vars[[v]]$name,
                  className = paste0('info legend legend-', base_vars[[v]]$abbrev, '-', metro),
                  labFormat = color_pal_tract$label_format,
                  na.label = 'N/A',
                  opacity = 1)
    }
  }
  
  m %>% 
    addLayersControl(
      position = c('bottomleft'),
      baseGroups = c('MSA', flatten_chr(map(names(base_vars), \(x) if_else(str_detect(base_vars[[x]]$name, 'Hispanic'), base_vars[[x]]$name, str_c('MSA by ', base_vars[[x]]$name))))),
      options = layersControlOptions(collapsed = F)
    ) %>% 
    htmlwidgets::onRender(js, choices)
}


# Run/save maps

names(all_codes)

create_rq2_map(c('philly', 'chicago'))
