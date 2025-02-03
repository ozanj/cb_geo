library(leaflet)
library(tidyverse)
library(formattable)
library(htmlwidgets)


# Load data

source(file.path('scripts', 'directories.R'))
source(file.path(scripts_dir, 'append_census.R'))
source(file.path(scripts_dir, 'metro_eps_codes.R'))

all_codes <- list(
  philly = list(name = 'Philadelphia', eps = philly_eps_codes),
  dallas = list(name = 'Dallas', eps = dallas_eps_codes),
  atl = list(name = 'Atlanta', eps = atl_eps_codes),
  chicago = list(name = 'Chicago', eps = chi_eps_codes),
  cleveland = list(name = 'Cleveland', eps = cleveland_eps_codes),
  northern_nj = list(name = 'Northern New Jersey', eps = nj_north_metro_eps_codes),
  houston = list(name = 'Houston', eps = htown_eps_codes),
  bay_area = list(name = 'Bay Area', eps = bay_eps_codes),
  long_island = list(name = 'Long Island', eps = long_island_eps_codes),
  ny_ny = list(name = 'New York, New York', eps = nyny_metro_eps_codes),
  detroit = list(name = 'Detroit', eps = detroit_eps_codes),
  boston = list(name = 'Boston', eps = boston_eps_codes),
  miami = list(name = 'Miami', eps = miami_eps_codes),
  dmv = list(name = 'DMV', eps = dmv_eps_codes),
  orange_county = list(name = 'Orange County', eps = orange_county_eps_codes),
  san_diego = list(name = 'San Diego', eps = san_diego_eps_codes),
  los_angeles = list(name = 'Los Angeles', eps = los_angeles_eps_codes)
)

regions_data <- data.frame(
  region = flatten_chr(map(names(all_codes), \(x) rep(x, length(all_codes[[x]]$eps)))),
  region_name = flatten_chr(map(names(all_codes), \(x) rep(all_codes[[x]]$name, length(all_codes[[x]]$eps)))),
  eps = flatten_chr(map(names(all_codes), \(x) all_codes[[x]]$eps))
) %>% 
  left_join(allyr_anal_tract_sf %>% filter(year == 2020) %>% select(eps, geometry), by = 'eps') %>% 
  group_by(region, region_name) %>% 
  summarise(
    eps = list(unique(eps)),
    geometry = st_union(geometry),
    latitude = st_coordinates(st_centroid(geometry))[, 1],
    longitude = st_coordinates(st_centroid(geometry))[, 2],
    .groups = 'drop'
  )

base_vars <- list(
  sum_tot_all = list(name = 'Total Population', abbrev = 'pop'),
  pct_nhisp_white = list(name = '% White, non-Hispanic', abbrev = 'nhisp_white'),
  pct_nhisp_black = list(name = '% Black, non-Hispanic', abbrev = 'nhisp_black'),
  pct_hisp_all = list(name = '% Hispanic', abbrev = 'hisp_all'),
  pct_nhisp_asian = list(name = '% Asian, non-Hispanic', abbrev = 'nhisp_asian'),
  pct_nhisp_nhpi = list(name = '% NHPI, non-Hispanic', abbrev = 'nhisp_nhpi'),
  pct_nhisp_native = list(name = '% AIAN, non-Hispanic', abbrev = 'nhisp_native'),
  pct_nhisp_multi = list(name = '% 2+ Races, non-Hispanic', abbrev = 'nhisp_multi'),
  med_inc_house = list(name = 'Median Income', abbrev = 'income'),
  pct_pov_yes = list(name = '% in Poverty', abbrev = 'pov'),
  pct_edu_baplus_all = list(name = '% with BA+', abbrev = 'edu')
)

format_vars <- function(data_df) {
  data_df %>% 
    mutate(
      sum_tot_all_text = format(round(sum_tot_all), big.mark = ',', trim = T),
      med_inc_house_text = currency(med_inc_house, digits = 0L),
      pct_nhisp_white_text = if_else(is.na(pct_nhisp_white), 'NA', paste0(sprintf('%.1f', pct_nhisp_white), '%')),
      pct_nhisp_black_text = if_else(is.na(pct_nhisp_black), 'NA', paste0(sprintf('%.1f', pct_nhisp_black), '%')),
      pct_hisp_all_text = if_else(is.na(pct_hisp_all), 'NA', paste0(sprintf('%.1f', pct_hisp_all), '%')),
      pct_nhisp_asian_text = if_else(is.na(pct_nhisp_asian), 'NA', paste0(sprintf('%.1f', pct_nhisp_asian), '%')),
      pct_nhisp_nhpi_text = if_else(is.na(pct_nhisp_nhpi), 'NA', paste0(sprintf('%.1f', pct_nhisp_nhpi), '%')),
      pct_nhisp_native_text = if_else(is.na(pct_nhisp_native), 'NA', paste0(sprintf('%.1f', pct_nhisp_native), '%')),
      pct_nhisp_multi_text = if_else(is.na(pct_nhisp_multi), 'NA', paste0(sprintf('%.1f', pct_nhisp_multi), '%')),
      pct_pov_yes_text = if_else(is.na(pct_pov_yes), 'NA', paste0(sprintf('%.1f', pct_pov_yes), '%')),
      pct_edu_baplus_all_text = if_else(is.na(pct_edu_baplus_all), 'NA', paste0(sprintf('%.1f', pct_edu_baplus_all), '%'))
    )
}

eps_shapes_from_tract <- allyr_anal_tract_sf %>%
  filter(eps %in% flatten_chr(regions_data$eps)) %>% 
  select(year, eps, geometry) %>%
  group_by(year, eps) %>% 
  summarise(
    geometry = st_union(geometry),
    .groups = 'drop'
  )

eps_data <- allyr_anal_eps_sf %>% 
  filter(eps %in% flatten_chr(regions_data$eps)) %>%
  select(year, eps, eps_name, n_tracts, sum_tot_all, med_inc_house, pct_nhisp_white, pct_nhisp_black, pct_hisp_all, pct_nhisp_asian, pct_nhisp_nhpi, pct_nhisp_native, pct_nhisp_multi, pct_pov_yes, pct_edu_baplus_all, geometry) %>% 
  rename('geometry_eps' = 'geometry') %>% 
  as.data.frame() %>% 
  left_join(as.data.frame(eps_shapes_from_tract), by = c('year', 'eps')) %>%
  st_sf(sf_column_name = 'geometry') %>% 
  format_vars()

eps_data %>% as.data.frame() %>% 
  group_by(year) %>% 
  summarise(
    across(sum_tot_all:pct_edu_baplus_all, ~sum(!is.na(.x)))
  ) %>% View()  # Asian, NHPI, AIAN, 2+ races in 1980 data missing

tract_data <- allyr_anal_tract_sf %>% 
  filter(eps %in% flatten_chr(regions_data$eps)) %>% 
  select(year, eps, eps_name, proportion, tot_all, med_inc_house, pct_nhisp_white, pct_nhisp_black, pct_hisp_all, pct_nhisp_asian, pct_nhisp_nhpi, pct_nhisp_native, pct_nhisp_multi, pct_pov_yes, pct_edu_baplus_all, geometry) %>% 
  rename('sum_tot_all' = 'tot_all') %>% 
  format_vars()

tract_data %>% as.data.frame() %>% 
  group_by(year) %>% 
  summarise(
    across(sum_tot_all:pct_edu_baplus_all, ~sum(!is.na(.x)))
  ) %>% View()  # Asian, NHPI, AIAN, 2+ races in 1980 data missing


# Map functions

get_palette <- function(variable, domain, level, pal = 'YlGnBu') {
  palette <- colorNumeric(pal, domain)
  label_format <- labelFormat()
  
  if (str_detect(variable, '_inc_')) {
    if (level == 'tract') {
      bins <- pretty(range(domain, na.rm = T), n = 6)
    } else {
      bins <- c(0, 25, 50, 75, 100, 125, 150, 175, 200, 1000) * 1000
    }
    palette <- colorBin(pal, domain, bins)
    
    label_format <- function(type, cuts) {
      cuts <- cuts / 1000
      
      f <- cuts[[2]]
      l <- cuts[[length(cuts) - 1]]
      delta <- cuts[[2]] - cuts[[1]]
      
      cuts <- paste0('$', cuts, '-', cuts + delta - 1, 'k')
      cuts[[1]] <- paste0('<$', f, 'k')
      cuts[[length(cuts) - 1]] <- paste0('$', l, 'k+')
      cuts[[length(cuts)]] <- 'NA'
      
      cuts
    }
  } else if (str_detect(variable, 'pct_')) {
    bins <- pretty(range(0, max(domain, na.rm = T)), n = 5)
    palette <- colorBin(pal, domain, bins)
    
    label_format <- function(type, cuts) {
      f <- cuts[[2]]
      l <- cuts[[length(cuts) - 1]]
      delta <- cuts[[2]] - cuts[[1]]
      d <- if_else(delta > 1, 1, 0)
      
      cuts <- paste0(cuts, '-', cuts + delta - d, '%')
      cuts[[length(cuts) - 1]] <- paste0(l, '%+')
      cuts[[length(cuts)]] <- 'NA'
      
      cuts
    }
  }
  
  list(palette = palette, label_format = label_format)
}

create_rq1_map <- function(metros, shared_legend = F) {
  js <- read_file(file.path(scripts_dir, 'rq1_maps.js'))
  
  choices <- list(region_choices = regions_data %>% filter(region %in% metros) %>% select(region, region_name, latitude, longitude))
  
  highlight_shp <- highlightOptions(weight = 2, color = '#606060', dashArray = '', bringToFront = T, sendToBack = T)
  
  yrs <- c(1980, 2000, 2020)
  
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
    eps <- eps_data %>% filter(eps %in% region$eps[[1]])
    tract <- tract_data %>% filter(eps %in% region$eps[[1]])
    
    m <- m %>%
      
      # Metro outline
      addPolylines(data = eps %>% filter(year == 2020), opacity = 1, color = '#707070', weight = 1.2, group = 'MSA', options = c(className = paste0('metro-shape metro-line metro-', metro)))
    
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
          addPolygons(data = eps %>% filter(year == y), opacity = 1, color = '#808080', weight = 1, dashArray = '3', fillOpacity = 0.8, smoothFactor = 0.2, fillColor = ~color_pal_eps$palette(get(v)), label = ~paste0('<b>', eps, ' - ', eps_name, '</b><br>', get(paste0(v, '_text'))) %>% lapply(htmltools::HTML), group = group_name, highlightOptions = highlight_shp, options = pathOptions(className = paste0('metro-shape metro-', metro, ' level-eps year-', y))) %>% 
          addPolygons(data = tract %>% filter(year == y), opacity = 1, color = '#808080', weight = 1, dashArray = '3', fillOpacity = 0.8, smoothFactor = 0.2, fillColor = ~color_pal_tract$palette(get(v)), label = ~paste0('<b>', eps, ' - ', eps_name, '</b><br>', get(paste0(v, '_text'))) %>% lapply(htmltools::HTML), group = group_name, highlightOptions = highlight_shp, options = pathOptions(className = paste0('metro-shape metro-', metro, ' level-tract year-', y)))
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

create_rq1_map(c('bay_area', 'philly', 'chicago'))
saveWidget(create_rq1_map(c('bay_area', 'philly', 'chicago')), file.path('.', 'results', 'maps', 'rq1_map_bayarea_philly_chicago.html'), background = 'transparent', selfcontained = T)
