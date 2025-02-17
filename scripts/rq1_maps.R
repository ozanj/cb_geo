library(leaflet)
library(tidyverse)
library(formattable)
library(htmlwidgets)


# Load data

source(file.path('scripts', 'map_functions.R'))


# Map functions

create_rq1_map <- function(metros, shared_legend = F) {
  js <- read_file(file.path(scripts_dir, 'rq1_maps.js'))
  
  choices <- list(region_choices = regions_data %>% filter(region %in% metros) %>% select(region, region_name, latitude, longitude))
  
  highlight_shp <- highlightOptions(weight = 2, color = '#606060', dashArray = '', bringToFront = T, sendToBack = T)
  
  yrs <- c(1980, 2000, 2020)
  
  m <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) # %>%
    
    # addEasyButton(easyButton(
    #   icon = 'fa-globe', title = 'Select Metro Area',
    #   onClick = JS("function(btn, map){ $('.custom-control').not('#metro-control').slideUp(); $('#metro-control').slideToggle(); }")))
  
  for (metro in metros) {
    print(metro)
    
    region <- regions_data %>% filter(region == metro)
    eps <- eps_data %>% filter(eps %in% region$eps[[1]])
    tract <- tract_data %>% filter(eps %in% region$eps[[1]])
    
    m <- m %>%
      
      # Metro outline
      addPolygons(data = eps %>% filter(year == 2020), opacity = 1, color = 'purple', fillOpacity = 0, weight = 2, label = ~paste0('<b style="font-size:11px">', eps, ' - ', eps_name, '</b>') %>% lapply(htmltools::HTML), group = 'MSA', options = c(className = paste0('metro-shape metro-line metro-', metro)))
    
    for (y in yrs) {
      m <- m %>%
        
        # EPS outline
        addPolylines(data = eps %>% filter(year == y), opacity = 1, color = 'purple', weight = 2, options = c(className = paste0('metro-shape metro-', metro, ' level-tract year-', y)))
    }
    
    for (v in names(base_vars)) {
      
      if (shared_legend && v != 'sum_tot_all') {
        color_pal_shared <- get_palette(v, c(eps[[v]], tract[[v]]), 'tract')
        color_pal_eps <- color_pal_shared
        color_pal_tract <- color_pal_shared
      } else {
        color_pal_eps <- get_palette(v, eps[[v]], 'eps')
        color_pal_tract <- get_palette(v, tract[[v]], 'tract')
      }
      
      group_name <- if_else(str_detect(base_vars[[v]]$name, 'Hispanic'), base_vars[[v]]$name, paste0('MSA by ', base_vars[[v]]$name))
      
      for (y in yrs) {
        m <- m %>% 
          
          # Shapes
          addPolygons(data = eps %>% filter(year == y), opacity = 1, color = '#808080', weight = 1, dashArray = '3', fillOpacity = 0.8, smoothFactor = 0.2, fillColor = ~color_pal_eps$palette(get(v)), label = ~paste0('<b style="font-size:11px">', eps, ' - ', eps_name, '</b><br>', get(paste0(v, '_text'))) %>% lapply(htmltools::HTML), group = group_name, highlightOptions = highlight_shp, options = pathOptions(className = paste0('metro-shape metro-', metro, ' level-eps year-', y))) %>% 
          addPolygons(data = tract %>% filter(year == y), opacity = 1, color = '#808080', weight = 1, dashArray = '3', fillOpacity = 0.8, smoothFactor = 0.2, fillColor = ~color_pal_tract$palette(get(v)), label = ~paste0('<b style="font-size:11px">', eps, ' - ', eps_name, '</b><br><b>Tract ', tract_code, '</b>: ', get(paste0(v, '_text'))) %>% lapply(htmltools::HTML), group = group_name, highlightOptions = highlight_shp, options = pathOptions(className = paste0('metro-shape metro-', metro, ' level-tract year-', y)))
      }
      
      class_name <- paste0('info legend legend-', base_vars[[v]]$abbrev, '-', metro)
      
      m <- m %>%
        addLegend(data = eps,
                  position = 'topright', pal = color_pal_eps$palette, values = ~get(v),
                  title = base_vars[[v]]$name,
                  className = paste0(class_name, '-eps'),
                  labFormat = color_pal_eps$label_format,
                  na.label = 'N/A',
                  opacity = 1) %>%
        
        addLegend(data = tract,
                  position = 'topright', pal = color_pal_tract$palette, values = ~get(v),
                  title = base_vars[[v]]$name,
                  className = paste0(class_name, '-tract'),
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

create_rq1_map(c('bay_area'))
create_rq1_map(c('bay_area'), shared_legend = T)

create_rq1_map(c('long_island'), shared_legend = F)

create_rq1_map(c('philly', 'chicago'))

create_rq1_map(c('bay_area', 'philly', 'chicago'))
saveWidget(create_rq1_map(c('bay_area', 'philly', 'chicago')), file.path('.', 'results', 'maps', 'rq1_map_bayarea_philly_chicago.html'), background = 'transparent', selfcontained = T)


for (region in regions_data$region) {
  saveWidget(create_rq1_map(region), file.path('.', 'results', 'maps', paste0('rq1_map_', region, '.html')), background = 'transparent', selfcontained = T)
}
