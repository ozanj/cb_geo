library(leaflet)
library(tidyverse)
library(formattable)
library(htmlwidgets)
library(haven)

# Load data

source(file.path('scripts', 'create_stu_list.R'))
source(file.path('scripts', 'map_functions.R'))

load(file.path('data', 'map_data_final.RData'))


# Used HS filters here: https://github.com/mpatricia01/public_requests_eda/blob/main/scripts/interactive_maps.R

ccd_data <- ccd %>% 
  filter(g12 >= 10, is_virtual == 0, updated_status %in% c('1', '3', '8')) %>%
  select(cbsa_1, ncessch, state_code, sch_name, sch_type, latitude, longitude, total_students, total_white, total_asian, total_black, total_hispanic, total_amerindian, total_nativehawaii, total_tworaces, total_unknown) %>% 
  rename(
    name = sch_name,
    school_type = sch_type
  ) %>% 
  mutate(
    school_type = recode(
      school_type,
      `1` = 'Regular',
      `2` = 'Special education',
      `3` = 'Career and technical',
      `4` = 'Alternative education'
    ),
    control = 'public'
  )

pss_data <- pss %>% 
  filter(total_12 >= 10) %>%
  select(cbsa_1, ncessch, state_code, name, school_type, latitude, longitude, total_students, total_white, total_asian, total_black, total_hispanic, total_amerindian, total_nativehawaii, total_tworaces) %>% 
  mutate(
    total_unknown = 0,
    school_type = recode(
      unclass(school_type),
      `1` = 'Regular',
      `2` = 'Montessori',
      `3` = 'Special program emphasis',
      `4` = 'Special education',
      `5` = 'Career/technical/vocational',
      `6` = 'Alternative/other',
      `7` = 'Early childhood program/child care center'
    ),
    control = 'private'
  )

hs_data <- ccd_data %>% 
  bind_rows(pss_data) %>% 
  mutate(
    pct_white = total_white / total_students,
    pct_asian = total_asian / total_students,
    pct_black = total_black / total_students,
    pct_hispanic = total_hispanic / total_students,
    pct_amerindian = total_amerindian / total_students,
    pct_nativehawaii = total_nativehawaii / total_students,
    pct_tworaces = total_tworaces / total_students,
    pct_unknown = total_unknown / total_students,
    hs_label = paste0(
      '<b>', name, '</b><br>',
      'School Type: ', school_type, '<br><br>',
      '<b>Total Enrollment</b>: ', total_students, '<br>',
      '% White: ', sprintf('%.1f', pct_white * 100), '<br>',
      '% Black: ', sprintf('%.1f', pct_black * 100), '<br>',
      '% Hispanic: ', sprintf('%.1f', pct_hispanic * 100), '<br>',
      '% Asian: ', sprintf('%.1f', pct_asian * 100), '<br>',
      '% NHPI: ', sprintf('%.1f', pct_nativehawaii * 100), '<br>',
      '% AIAN: ', sprintf('%.1f', pct_amerindian * 100), '<br>',
      '% 2+ Races: ', sprintf('%.1f', pct_tworaces * 100), '<br>',
      '% Unknown: ', sprintf('%.1f', pct_unknown * 100)
    )
  ) %>% 
  st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>%
  st_join(eps_geometry_zcta)

table(lists_orders_zip_hs_df$stu_first_gen, useNA = 'always')
# 1: No college
# 2: Some college
# 3: Not first-generation
# 4: No response

table(lists_orders_zip_hs_df$stu_race_cb, useNA = 'always')
# 0: No response (stu_unknown_01=1)
# 1: American Indian/Alaska Native (stu_amerindian_01=1 + stu_native_01=1)
# 2: Asian (stu_asian_01=1)
# 3: Black/African American (stu_black_01=1)
# 4: Hispanic/Latino (stu_hispanic_01=1)
# 8: Native Hawaiian/Pacific Islander (stu_nativehawaii_01=1 + stu_native_01=1)
# 9: White (stu_white_01=1)
# 10: Other (all 0)
# 12: Two or more races, non-Hispanic (stu_tworaces_01=1)

lists_orders_zip_hs_df %>% 
  select(stu_race_cb, stu_white_01, stu_asian_01, stu_black_01, stu_hispanic_01, stu_amerindian_01, stu_nativehawaii_01, stu_native_01, stu_tworaces_01, stu_unknown_01) %>% 
  distinct() %>% 
  View()

purchased_data <- lists_orders_zip_hs_df %>%
  select(univ_id, ord_num, hs_ncessch, stu_first_gen, stu_race_cb) %>% 
  mutate(
    firstgen = recode(
      unclass(stu_first_gen),
      `1` = 1,
      `2` = 1,
      .default = 0
    ),
    race = recode(
      unclass(stu_race_cb),
      `0` = 'unknown',
      `1` = 'aian',
      `2` = 'asian',
      `3` = 'black',
      `4` = 'hispanic',
      `8` = 'nhpi',
      `9` = 'white',
      `10` = 'other',
      `12` = 'multi',
      .missing = 'missing'
    ),
    count = 1
  ) %>% 
  select(-stu_first_gen, -stu_race_cb) 

student_data <- purchased_data %>% 
  pivot_wider(
    id_cols = c(univ_id, ord_num, hs_ncessch),
    names_from = race,
    values_from = c(count, firstgen),
    values_fn = list(count = sum, firstgen = mean),
    values_fill = 0
  ) %>% 
  left_join(purchased_data %>% group_by(univ_id, ord_num, hs_ncessch) %>% summarise(firstgen_total = mean(firstgen), count_total = sum(count), .groups = 'drop'), by = c('univ_id', 'ord_num', 'hs_ncessch')) %>% 
  rename('ncessch' = 'hs_ncessch') %>%
  inner_join(hs_data, by = 'ncessch') %>% 
  mutate(
    order_label = paste0(
      '<br><br><b>Total Purchased</b>: ', count_total, ' (', count_total * firstgen_total, ' first-gen)<br>',
      '# White: ', count_white, ' (', count_white * firstgen_white, ' first-gen)<br>',
      '# Black: ', count_black, ' (', count_black * firstgen_black, ' first-gen)<br>',
      '# Hispanic: ', count_hispanic, ' (', count_hispanic * firstgen_hispanic, ' first-gen)<br>',
      '# Asian: ', count_asian, ' (', count_asian * firstgen_asian, ' first-gen)<br>',
      '# NHPI: ', count_nhpi, ' (', count_nhpi * firstgen_nhpi, ' first-gen)<br>',
      '# AIAN: ', count_aian, ' (', count_aian * firstgen_aian, ' first-gen)<br>',
      '# 2+ Races: ', count_multi, ' (', count_multi * firstgen_multi, ' first-gen)<br>',
      '# Other: ', count_other, ' (', count_other * firstgen_other, ' first-gen)<br>',
      '# Unknown: ', count_unknown, ' (', count_unknown * firstgen_unknown, ' first-gen)<br>',
      '# Missing: ', count_missing, ' (', count_missing * firstgen_missing, ' first-gen)'
    )
  )

orders_data <- data.frame(
  region = c(rep('chicago', 3), rep('philly', 3)),
  order_num = c('487984', '488035', '488053', '448922', '448427', '448440'),
  order_title = c('SAT 1020-1150', 'SAT 1160-1300', 'SAT 1310-1600', 'PSAT 1070-1180', 'PSAT 1190-1260', 'PSAT 1270-1520')
)

pin_vars <- list(
  total = list(group = 'Purchased High Schools', color = 'black'),
  white = list(group = 'White, non-Hispanic', color = 'red'),
  black = list(group = 'Black, non-Hispanic', color = 'orange'),
  hispanic = list(group = 'Hispanic', color = 'gold'),
  asian = list(group = 'Asian, non-Hispanic', color = 'green'),
  nhpi = list(group = 'NHPI, non-Hispanic', color = 'blue'),
  aian = list(group = 'AIAN, non-Hispanic', color = 'purple'),
  multi = list(group = '2+ Races, non-Hispanic', color = 'hotpink'),
  other = list(group = 'Other', color = 'brown'),
  unknown = list(group = 'Unknown', color = 'darkgray'),
  missing = list(group = 'Missing', color = 'lightgray')
)


# Map functions

create_rq2_map <- function(metros) {
  js <- read_file(file.path(scripts_dir, 'rq2_maps.js'))
  
  choices <- list(
    region_choices = regions_data %>% filter(region %in% metros) %>% select(region, region_name, latitude, longitude),
    order_choices = orders_data,
    pin_colors = setNames(map(pin_vars, \(x) x$color), map(pin_vars, \(x) x$group))
  )
  
  highlight_shp <- highlightOptions(weight = 1, color = '#606060', dashArray = '')
  
  m <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    addMiniMap(tiles = providers$CartoDB.Positron,
               toggleDisplay = T) %>%
    
    addEasyButton(easyButton(
      icon = 'fa-crosshairs', title = 'Toggle View',
      onClick = JS("function(btn, map){ let zoom = $(btn.button).attr('data-national'); $('.custom-control').slideUp(); if (zoom === 'true') { let $sel = $('input[name=\"metro-choice\"]:checked'); map.setView([$sel.attr('data-lng'), $sel.attr('data-lat')], 8.2); $(btn.button).attr('data-national', false); $('#view-btn').html('National View'); } else { map.setView([39.828, -98.580], 4); $(btn.button).attr('data-national', true); $('#view-btn').html('MSA View'); }}"))) %>%
    
    addEasyButton(easyButton(
      icon = 'fa-globe', title = 'Select Metro Area',
      onClick = JS("function(btn, map){ $('.custom-control').not('#metro-control').slideUp(); $('#metro-control').slideToggle(); }"))) %>% 
    
    addEasyButton(easyButton(
      icon = 'fa-file', title = 'Select Order Number',
      onClick = JS("function(btn, map){ $('.custom-control').not('#order-control').slideUp(); $('#order-control').slideToggle(); }")))
  
  for (metro in metros) {
    print(metro)
    
    region <- regions_data %>% filter(region == metro)
    eps <- eps_data %>% filter(eps %in% region$eps[[1]], year == 2020)
    tract <- tract_data %>% filter(eps %in% region$eps[[1]], year == 2020)
    
    order_nums <- (orders_data %>% filter(region == metro))$order_num
    
    purchased <- student_data %>% filter(ord_num %in% order_nums, eps %in% region$eps[[1]])

    m <- m %>%
      
      # Metro outline
      addPolylines(data = eps, opacity = 1, color = '#707070', weight = 1.2, group = 'MSA', options = c(className = paste0('metro-shape metro-', metro))) %>% 
      
      # EPS outline
      addPolylines(data = eps, opacity = 1, color = 'purple', weight = 2, options = c(className = paste0('metro-shape metro-line-', metro)))
    
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
    
    # Markers for non-purchased schools (by any of the orders)
    f <- Vectorize(function(sch_id, ord_nums) {
      o <- str_split(ord_nums, ',')[[1]]
      d <- student_data %>% filter(ncessch == sch_id, ord_num %in% o)
      s <- setdiff(o, d$ord_num)
      if_else(length(s) == 0, '', paste0('order-', s, collapse = ' '))
    })
    
    non_purchased <- hs_data %>%
      filter(eps %in% region$eps[[1]]) %>% 
      mutate(np_order = f(ncessch, str_c(order_nums, collapse = ','))) %>%
      filter(np_order != '')

    m <- m %>% 
      
      # Non-purchased markers
      addCircleMarkers(data = non_purchased, lng = ~st_coordinates(geometry)[,1], lat = ~st_coordinates(geometry)[,2], group = 'Non-Purchased High Schools',
                       radius = 1, fillOpacity = 0, opacity = 1, weight = 1.5, color = 'red',
                       popup = ~hs_label, options = pathOptions(className = paste0('metro-', metro, ' order-pin ', non_purchased$np_order)))
    
    # Markers for purchased schools (order-specific)
    for (order_num in order_nums) {
      p <- purchased %>% filter(ord_num == order_num)
      
      for(v in names(pin_vars)) {
        m <- m %>% 
          
          # Purchased markers
          addCircleMarkers(data = p, lng = ~st_coordinates(geometry)[,1], lat = ~st_coordinates(geometry)[,2], group = pin_vars[[v]]$group,
                           radius = ~sqrt(get(paste0('count_', v))) + 1, fillOpacity = ~get(paste0('firstgen_', v)), opacity = 1, weight = 1.5, color = pin_vars[[v]]$color, fillColor = 'black',
                           popup = ~paste0(hs_label, order_label), options = pathOptions(className = paste0('metro-', metro, ' order-pin order-', order_num)))
      }
    }
    
  }
  
  m %>% 
    
    hideGroup(flatten_chr(map(pin_vars, \(x) x$group))[-1]) %>% 
  
    addLayersControl(
      position = c('bottomleft'),
      baseGroups = c('MSA', flatten_chr(map(base_vars, \(x) if_else(str_detect(x$name, 'Hispanic'), x$name, str_c('MSA by ', x$name))))),
      overlayGroups = c('Non-Purchased High Schools', flatten_chr(map(pin_vars, \(x) x$group))),
      options = layersControlOptions(collapsed = F)
    ) %>% 
    htmlwidgets::onRender(js, choices)
}


# Run/save maps

names(all_codes)

create_rq2_map(c('philly', 'chicago'))
saveWidget(create_rq2_map(c('philly', 'chicago')), file.path('.', 'results', 'maps', 'rq2_map_philly_chicago.html'), background = 'transparent', selfcontained = T)
