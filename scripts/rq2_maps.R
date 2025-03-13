library(leaflet)
library(tidyverse)
library(formattable)
library(htmlwidgets)
library(haven)
library(sf)

# Load data

source(file.path('scripts', 'map_functions.R'))
source(file.path('scripts', 'create_stu_list.R'))

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
    control = 'Public'
  )

pss_data <- pss %>% 
  filter(total_12 >= 10) %>%
  select(cbsa_1, ncessch, state_code, name, school_type, latitude, longitude, total_students, total_white, total_asian, total_black, total_hispanic, total_amerindian, total_nativehawaii, total_tworaces, religion) %>% 
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
    religion = recode(
      religion,
      'catholic' = 'Catholic',
      'christian' = 'Christian',
      'nonsectarian' = 'Non-sectarian',
      'other' = 'Other'
    ),
    control = 'Private'
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
      'School Control: ', control, '<br>',
      if_else(control == 'Private', paste0('Religious Affiliation: ', religion, '<br>'), ''),
      'School Type: ', school_type, '<br><br>',
      '<b>Total Enrollment</b>: ', format(total_students, big.mark = ',', trim = T), '<br>',
      '<ul><li>% White: ', sprintf('%.1f', pct_white * 100), '</li>',
      '<li>% Black: ', sprintf('%.1f', pct_black * 100), '</li>',
      '<li>% Hispanic: ', sprintf('%.1f', pct_hispanic * 100), '</li>',
      '<li>% Asian: ', sprintf('%.1f', pct_asian * 100), '</li>',
      '<li>% NHPI: ', sprintf('%.1f', pct_nativehawaii * 100), '</li>',
      '<li>% AIAN: ', sprintf('%.1f', pct_amerindian * 100), '</li>',
      '<li>% 2+ Races: ', sprintf('%.1f', pct_tworaces * 100), '</li>',
      '<li>% Unknown: ', sprintf('%.1f', pct_unknown * 100), '</li></ul>'
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
      `1` = 'nocollege',
      `2` = 'somecollege',
      `3` = 'notfirstgen',
      `4` = 'noedu',
      .missing = 'noedu'
    ),
    race = recode(
      unclass(stu_race_cb),
      `0` = 'norace',
      `1` = 'aian',
      `2` = 'asian',
      `3` = 'black',
      `4` = 'hispanic',
      `8` = 'nhpi',
      `9` = 'white',
      `10` = 'other',
      `12` = 'multi',
      .missing = 'norace'
    ),
    category = paste0(race, '_', firstgen)
  ) %>% 
  select(-stu_first_gen, -stu_race_cb)  # other_nocollege and other_somecollege non-existent

student_data <- purchased_data %>% 
  group_by(univ_id, ord_num, hs_ncessch, category) %>% 
  summarise(count = n(), .groups = 'drop') %>%
  bind_rows(
    purchased_data %>% 
    filter(firstgen %in% c('nocollege', 'somecollege')) %>%
    group_by(univ_id, ord_num, hs_ncessch, race) %>%
    summarise(count = n(), .groups = 'drop') %>%
    mutate(category = paste0(race, '_nosomecollege')) %>%
    select(-race)
  ) %>% 
  bind_rows(
    purchased_data %>% 
    group_by(univ_id, ord_num, hs_ncessch, race) %>% 
    summarise(count = n(), .groups = 'drop') %>%
    mutate(category = paste0(race, '_all')) %>%
    select(-race)
  ) %>% 
  bind_rows(
    purchased_data %>% 
    group_by(univ_id, ord_num, hs_ncessch, firstgen) %>% 
    summarise(count = n(), .groups = 'drop') %>%
    mutate(category = paste0('all_', firstgen)) %>%
    select(-firstgen)
  ) %>%
  bind_rows(
    purchased_data %>% 
    group_by(univ_id, ord_num, hs_ncessch) %>% 
    summarise(count = n(), .groups = 'drop') %>%
    mutate(category = 'all_all')
  ) %>%
  pivot_wider(
    id_cols = c(univ_id, ord_num, hs_ncessch),
    names_from = category,
    values_from = count,
    values_fill = 0,
    names_sort = T
  ) %>% 
  mutate(
    other_nocollege = 0,
    other_somecollege = 0,
    other_nosomecollege = other_nocollege + other_somecollege,
    all_nosomecollege = all_nocollege + all_somecollege
  )

orders_data_full <- read_csv(file.path('scripts', 'metro_orders.csv')) %>% 
  select(-eps_codes, -figure_note) %>% 
  add_row(metro = 'chicago', order_ids = '392834_488035_392835_488053', test_range = 'SAT score 1160 - 1600') %>% 
  add_row(metro = 'philadelphia', order_ids = '448427_448440', test_range = 'PSAT score 1190 - 1520') %>%  
  add_row(metro = 'northern_new_jersey', order_ids = '448427_448440', test_range = 'PSAT score 1190 - 1520') %>%  
  add_row(metro = 'long_island', order_ids = '448427_448440', test_range = 'PSAT score 1190 - 1520') %>%  
  add_row(metro = 'dallas', order_ids = '448427_448440', test_range = 'PSAT score 1190 - 1520') %>%  
  add_row(metro = 'detroit', order_ids = '448427_547005_448440_546978', test_range = 'PSAT score 1190 - 1520') %>% 
  add_row(metro = 'los_angeles', order_ids = '448420_546946_448374_546945', test_range = 'PSAT score 1190 - 1520') %>%
  add_row(metro = 'orange_county', order_ids = '448420_546946_448374_546945', test_range = 'PSAT score 1190 - 1520') %>% 
  add_row(metro = 'san_diego', order_ids = '448420_546946_448374_546945', test_range = 'PSAT score 1190 - 1520') %>% 
  add_row(metro = 'bay_area', order_ids = '448420_546946_448374_546945', test_range = 'PSAT score 1190 - 1520') %>%
  mutate(
    type = c(rep('rq2', 32), rep('aian', 5), rep('rq2', 10))
  )

names(orders_data_full) <- c('region', 'order_num', 'order_title', 'type')

unique_orders <- unique(orders_data_full$order_num)

student_data_multi_orders <- lapply(unique_orders[str_detect(unique_orders, '_')], function(orders) {
  student_data %>% 
    filter(ord_num %in% str_split(orders, '_')[[1]]) %>%
    mutate(ord_num = orders) %>% 
    group_by(univ_id, ord_num, hs_ncessch) %>% 
    summarise(across(aian_all:all_nosomecollege, sum), .groups = 'drop')
})

student_data <- do.call(bind_rows, c(list(student_data), student_data_multi_orders)) %>% 
  rename('ncessch' = 'hs_ncessch') %>%
  inner_join(hs_data, by = 'ncessch') %>%
  mutate(
    order_label = paste0(
      '<br><b>Total Purchased</b>: ', format(all_all, big.mark = ',', trim = T), ' (', all_nocollege, ' no college, ', all_somecollege, ' some college)',
      '<p>Total (parental education)</p>',
      '<ul><li>White: ', white_all, ' (', white_nocollege, ', ', white_somecollege, ')</li>',
      '<li>Black: ', black_all, ' (', black_nocollege, ', ', black_somecollege, ')</li>',
      '<li>Hispanic: ', hispanic_all, ' (', hispanic_nocollege, ', ', hispanic_somecollege, ')</li>',
      '<li>Asian: ', asian_all, ' (', asian_nocollege, ', ', asian_somecollege, ')</li>',
      '<li>NHPI: ', nhpi_all, ' (', nhpi_nocollege, ', ', nhpi_somecollege, ')</li>',
      '<li>AIAN: ', aian_all, ' (', aian_nocollege, ', ', aian_somecollege, ')</li>',
      '<li>2+ Races: ', multi_all, ' (', multi_nocollege, ', ', multi_somecollege, ')</li>',
      '<li>Other: ', other_all, ' (', other_nocollege, ', ', other_somecollege, ')</li>',
      '<li>Unknown/Missing: ', norace_all, ' (', norace_nocollege, ', ', norace_somecollege, ')</li></ul>'
    )
  )

lists_orders_zip_hs_df %>% 
  select(ord_num, ord_sat_score_min, ord_sat_score_max, ord_psat_score_min, ord_psat_score_max) %>% 
  filter(ord_num %in% c('487984', '488035', '488053', '448922', '448427', '448440', '448375', '448374', '448420', '329702')) %>% 
  distinct() %>% 
  View()

race_vars <- data.frame(
  abbrev = c('all', 'white', 'black', 'hispanic', 'asian', 'nhpi', 'aian', 'multi', 'other', 'norace'),
  name = c('All', 'White, non-Hispanic', 'Black, non-Hispanic', 'Hispanic', 'Asian, non-Hispanic', 'NHPI, non-Hispanic', 'AIAN, non-Hispanic', '2+ Races, non-Hispanic', 'Other', 'Unknown/Missing')
)

edu_vars <- data.frame(
  abbrev = c('all', 'nocollege', 'somecollege', 'nosomecollege', 'notfirstgen', 'noedu'),
  name = c('All', 'No college', 'Some college', 'No or some college', 'BA+', 'Unknown/Missing')
)

regions_data[regions_data$region == 'nyny', 'region'] <- 'nyny'  # for the AIAN graphs


# Map functions

create_rq2_map <- function(metros, map_type = 'rq2') {
  js <- read_file(file.path(scripts_dir, 'rq2_maps.js'))
  
  orders_data <- orders_data_full %>% 
    filter(type == map_type)
  
  choices <- list(
    region_choices = regions_data %>% filter(region %in% metros) %>% select(region, region_name, latitude, longitude),
    order_choices = orders_data,
    race_vars = race_vars,
    edu_vars = edu_vars
  )
  
  highlight_shp <- highlightOptions(weight = 1, color = '#606060', dashArray = '')
  
  m <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    # addEasyButton(easyButton(
    #   icon = 'fa-globe', title = 'Select Metro Area',
    #   onClick = JS("function(btn, map){ $('.custom-control').not('#metro-control').slideUp(); $('#metro-control').slideToggle(); }"))) %>% 
    
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
      addPolygons(data = eps, opacity = 1, color = 'purple', fillOpacity = 0, weight = 2, label = ~paste0('<b style="font-size:11px">', eps, ' - ', eps_name, '</b>') %>% lapply(htmltools::HTML), group = 'MSA', options = c(className = paste0('metro-shape metro-', metro))) %>% 
      
      # EPS outline
      addPolylines(data = eps, opacity = 1, color = 'purple', weight = 2, options = c(className = paste0('metro-shape metro-line-', metro)))
    
    for (v in names(base_vars)) {
      
      color_pal_tract <- get_palette(v, tract[[v]], 'tract')
      
      group_name <- if_else(str_detect(base_vars[[v]]$name, 'Hispanic'), base_vars[[v]]$name, paste0('MSA by ', base_vars[[v]]$name))
      
      m <- m %>% 
        
        # Shapes
        addPolygons(data = tract, opacity = 1, color = '#808080', weight = 1, dashArray = '3', fillOpacity = 0.8, smoothFactor = 0.2, fillColor = ~color_pal_tract$palette(get(v)), label = ~paste0('<b style="font-size:11px">', eps, ' - ', eps_name, '</b><br><b>Tract ', tract_code, '</b>: ', get(paste0(v, '_text'))) %>% lapply(htmltools::HTML), group = group_name, highlightOptions = highlight_shp, options = pathOptions(className = paste0('metro-shape metro-', metro))) %>%
      
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
    
    marker_size <- 2

    m <- m %>% 
      
      # Non-purchased markers
      addCircleMarkers(data = non_purchased, lng = ~st_coordinates(geometry)[,1], lat = ~st_coordinates(geometry)[,2], group = 'Non-Purchased High Schools',
                       radius = marker_size, fillOpacity = 1, opacity = 1, weight = 1.5, color = 'red', fillColor = 'red',
                       popup = ~hs_label, options = pathOptions(className = paste0('metro-', metro, ' order-pin ', non_purchased$np_order)))

    # Markers for purchased schools (order-specific)
    for (order_num in order_nums) {
      p <- purchased %>% filter(ord_num == order_num)
      
      for (r in race_vars$abbrev) {
        for (e in edu_vars$abbrev) {
          m <- m %>% 
            
            # Purchased markers
            addCircleMarkers(data = p, lng = ~st_coordinates(geometry)[,1], lat = ~st_coordinates(geometry)[,2], group = 'Purchased High Schools',
                             radius = ~sqrt(get(paste0(r, '_', e))) + marker_size, fillOpacity = ~if_else(get(paste0(r, '_', e)) == 0, 1, 0), opacity = 1, weight = 1.5, color = ~if_else(control == 'Private', 'blue', 'orange'), fillColor = ~if_else(control == 'Private', 'blue', 'orange'),
                             popup = ~paste0(hs_label, order_label), options = pathOptions(className = paste0('metro-', metro, ' order-pin order-', order_num, '-', r, '-', e)))
        }
      }
    }
    
  }
  
  m %>% 
    
    addLayersControl(
      position = c('bottomleft'),
      baseGroups = c('MSA', flatten_chr(map(base_vars, \(x) if_else(str_detect(x$name, 'Hispanic'), x$name, str_c('MSA by ', x$name))))),
      overlayGroups = c('Non-Purchased High Schools', 'Purchased High Schools'),
      options = layersControlOptions(collapsed = F)
    ) %>% 
    htmlwidgets::onRender(js, choices)
}


# Run/save maps

names(all_codes)

create_rq2_map(c('philly', 'chicago'))
create_rq2_map(c('bay_area', 'long_island'))
saveWidget(create_rq2_map(c('philly', 'chicago')), file.path('.', 'results', 'maps', 'rq2_map_philly_chicago.html'), background = 'transparent', selfcontained = T)


for (region in unique((orders_data_full %>% filter(type == 'rq2'))$region)) {
  saveWidget(create_rq2_map(region, 'rq2'), file.path('.', 'results', 'maps', paste0('rq2_map_', region, '.html')), background = 'transparent', selfcontained = T)
}

for (region in unique((orders_data_full %>% filter(type == 'aian'))$region)) {
  saveWidget(create_rq2_map(region, 'aian'), file.path('.', 'results', 'maps', paste0('rq2_aian_map_', region, '.html')), background = 'transparent', selfcontained = T)
}
