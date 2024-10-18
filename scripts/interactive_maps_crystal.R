library(leaflet)
library(rgdal)
library(rgeos)
library(formattable)
library(tidyverse)
library(htmlwidgets)


data_dir <- file.path('.', 'data')
scripts_dir <- file.path('.', 'scripts')
outputs_dir <- file.path('.', 'outputs', 'maps')


# Load data

load(file.path(data_dir, 'map_data_final.RData'))

zip_data <- acs_zip %>% 
  select(
    zip_code, state_code, cbsa_1, cbsa_1_ratio, cbsatitle_1,
    pop_total, medincome_2564, ends_with('_15_19_pct')
  ) %>% 
  mutate(
    inc_brks = cut(
      medincome_2564,
      breaks = c(-1, 50000, 75000, 100000, 150000, 200000, 10000000),
      labels = c('<$50k', '$50k-74k', '$75k-99k',  '$100k-149k', '$150k-199k', '$200k+')
    ),
    # Note: pop_native_15_19 = pop_amerindian_15_19 + pop_nativehawaii_15_19
    pop_poc_15_19_pct = pop_black_15_19_pct + pop_hispanic_15_19_pct + pop_native_15_19_pct,
    race_brks = cut(
      pop_poc_15_19_pct,
      breaks = c(-1, 20, 40, 60, 80, 90, 101),
      labels = c('0-19%', '20-39%', '40-59%', '60-79%', '80-89%', '90-100%')
    )
  )

zip_shp <- readOGR(file.path(data_dir, 'cb_2018_us_zcta510_500k', 'cb_2018_us_zcta510_500k.shp')) %>% 
  merge(zip_data, by.x = 'ZCTA5CE10', by.y = 'zip_code', all.x = T)

ccd_data <- ccd %>% 
  filter(g12 >= 10, is_virtual == 0, updated_status %in% c('1', '3', '8')) %>% 
  select(cbsa_1, ncessch, sch_name, sch_type, latitude, longitude, total_students, total_white, total_asian, total_black, total_hispanic, total_amerindian, total_nativehawaii, total_tworaces, total_unknown) %>% 
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
  select(cbsa_1, ncessch, name, school_type, latitude, longitude, total_students, total_white, total_asian, total_black, total_hispanic, total_amerindian, total_nativehawaii, total_tworaces) %>% 
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
  pivot_longer(
    cols = -c(cbsa_1, control, ncessch, name, school_type, latitude, longitude, total_students),
    names_prefix = 'total_',
    names_to = 'race',
    values_to = 'count'
  ) %>% 
  group_by(cbsa_1, control, ncessch, name, school_type, latitude, longitude, total_students) %>%
  mutate(
    pct = count / sum(count, na.rm = T)
  ) %>%
  ungroup() %>% 
  pivot_wider(
    id_cols = c(cbsa_1, control, ncessch, name, school_type, latitude, longitude, total_students),
    names_from = 'race',
    names_prefix = 'pct_',
    values_from = 'pct'
  )


# Define functions

create_popup <- function(data_df) {
  
  popup <- str_c(
    '<b>',data_df$name,'</b><br>', 
    '<b>School Type</b>: ', data_df$school_type, '<br><br>',
    '<b>Total Enrollment</b>: ', format(data_df$total_students, big.mark = ','), '<br>',
    '% Black: ', sprintf('%.1f', data_df$pct_black * 100), '<br>',
    '% Latinx: ', sprintf('%.1f', data_df$pct_hispanic * 100), '<br>',
    '% AI/AN: ', sprintf('%.1f', data_df$pct_amerindian * 100), '<br>',
    '% NH/PI: ', sprintf('%.1f', data_df$pct_nativehawaii * 100), '<br>',
    '% Asian: ', sprintf('%.1f', data_df$pct_asian * 100), '<br>',
    '% White: ', sprintf('%.1f', data_df$pct_white * 100)
  )
  
  if ('num_total' %in% names(data_df)) {
    popup <- str_c(
      popup,
      '<br><br><b>Total Purchased</b>: ', format(data_df$num_total, big.mark = ','), '<br>',
      '# Black: ', data_df$num_black, '<br>',
      '# Latinx: ', data_df$num_hispanic, '<br>',
      '# AI/AN: ', data_df$num_amerindian, '<br>',
      '# NH/PI: ', data_df$num_nativehawaii, '<br>',
      '# Asian: ', data_df$num_asian, '<br>',
      '# White: ', data_df$num_white
    )
  }
  
  popup
  
}

create_interactive_map <- function(purchases_df) {
  
  purchases_df <- purchases_df %>% 
    arrange(cbsa_name)
  
  metros <- unique(purchases_df$cbsa_code)
  
  purchases_hs_df <- purchases_df %>% 
    group_by(hs_ncessch, race) %>% 
    summarise(
      count = n()
    ) %>% 
    ungroup() %>% 
    pivot_wider(
      id_cols = hs_ncessch,
      names_from = race,
      names_prefix = 'num_',
      values_from = count,
      values_fill = list(count = 0)
    )
  
  if (!'white' %in% purchases_df$race) {
    purchases_hs_df$num_white <- 0
  }
  if (!'asian' %in% purchases_df$race) {
    purchases_hs_df$num_asian <- 0
  }
  if (!'nativehawaii' %in% purchases_df$race) {
    purchases_hs_df$num_nativehawaii <- 0
  }
  
  purchases_hs_df <- purchases_hs_df %>% 
    mutate(
      num_total = num_white + num_black + num_hispanic + num_asian + num_amerindian + num_nativehawaii + num_tworaces + num_noresponse
    ) %>% 
    rename(
      ncessch = hs_ncessch
    )
  
  # create shared color scale functions
  color_income <- colorFactor('YlGnBu', zip_data$inc_brks)
  color_race <- colorFactor('YlGnBu', zip_data$race_brks)
  
  data <- list()
  
  for (metro in metros) {
    print(metro)
    
    data[[metro]] <- list()
    
    # shape, outline, and coordinates for MSA
    cbsa_zip_shps <- subset(zip_shp, ZCTA5CE10 %in% (zip_data %>% filter(cbsa_1 == metro))$zip_code)
    cbsa_union <- gUnaryUnion(cbsa_zip_shps, id = NULL)
    
    data[[metro]]$cbsa_shape <- cbsa_zip_shps
    data[[metro]]$cbsa_outline <- cbsa_union
    
    cbsa_coordinates <- as.numeric(coordinates(cbsa_union))
    data[[metro]]$cbsa_lng <- cbsa_coordinates[1]
    data[[metro]]$cbsa_lat <- cbsa_coordinates[2]
    
    # population color scale function for MSA
    data[[metro]]$color_pop <- colorNumeric('YlGnBu', cbsa_zip_shps$pop_total, n = 5)
    
    # zip-code pop-ups for MSA
    data[[metro]]$popup_pop <- paste0('<b>ZCTA5 ', cbsa_zip_shps$ZCTA5CE10, '</b><br>',
                                      'Total Population: ', format(cbsa_zip_shps$pop_total, big.mark = ',')) %>% lapply(htmltools::HTML)
    data[[metro]]$popup_income <- paste0('<b>ZCTA5 ', cbsa_zip_shps$ZCTA5CE10, '</b><br>',
                                         'Median Household Income: ', currency(cbsa_zip_shps$medincome_2564, digits = 0L)) %>% lapply(htmltools::HTML)
    data[[metro]]$popup_race <- paste0('<b>ZCTA5 ', cbsa_zip_shps$ZCTA5CE10, '</b><br>',
                                       '% 15-19yo Population of Color: ', sprintf('%.1f', cbsa_zip_shps$pop_poc_15_19_pct)) %>% lapply(htmltools::HTML)
    
    # HS pop-ups for MSA
    pubhs_df <- hs_data %>% filter(control == 'public', cbsa_1 == metro)
    privhs_df <- hs_data %>% filter(control == 'private', cbsa_1 == metro)
    
    data[[metro]]$pubhs_purchased <- pubhs_df %>% inner_join(purchases_hs_df, by = 'ncessch')
    data[[metro]]$pubhs_nonpurchased <- pubhs_df %>% anti_join(purchases_hs_df, by = 'ncessch')
    data[[metro]]$privhs_purchased <- privhs_df %>% inner_join(purchases_hs_df, by = 'ncessch')
    data[[metro]]$privhs_nonpurchased <- privhs_df %>% anti_join(purchases_hs_df, by = 'ncessch')
    
    data[[metro]]$popup_pubhspurchased <- create_popup(data[[metro]]$pubhs_purchased)
    data[[metro]]$popup_pubhsnonpurchased <- create_popup(data[[metro]]$pubhs_nonpurchased)
    data[[metro]]$popup_privhspurchased <- create_popup(data[[metro]]$privhs_purchased)
    data[[metro]]$popup_privhsnonpurchased <- create_popup(data[[metro]]$privhs_nonpurchased)
  }
  
  
  js <- read_file(file.path(scripts_dir, 'interactive_maps.js'))
  
  metro_choices <- purchases_df %>% filter(cbsa_code %in% metros) %>% select(cbsa_code, cbsa_name) %>% distinct()
  metro_choices <- metro_choices %>% rowwise() %>% mutate(cbsa_lat = data[[cbsa_code]]$cbsa_lat)
  metro_choices <- metro_choices %>% rowwise() %>% mutate(cbsa_lng = data[[cbsa_code]]$cbsa_lng)
  
  choices <- list(metro_choices = metro_choices)
  
  highlight_shp <- highlightOptions(fillOpacity = 0.5, bringToFront = F)
  
  
  # mapping
  m <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    
    addMiniMap(tiles = providers$CartoDB.Positron,
               toggleDisplay = TRUE) %>%
    
    addEasyButton(easyButton(
      icon = 'fa-crosshairs', title = 'Toggle View',
      onClick = JS("function(btn, map){ let zoom = $(btn.button).attr('data-national'); $('.custom-control').slideUp(); if (zoom === 'true') { let $sel = $('input[name=\"metro-choice\"]:checked'); map.setView([$sel.attr('data-lat'), $sel.attr('data-lng')], 8.2); $(btn.button).attr('data-national', false); $('#view-btn').html('National View'); } else { map.setView([39.828, -98.580], 4); $(btn.button).attr('data-national', true); $('#view-btn').html('MSA View'); }}"))) %>%
    
    addEasyButton(easyButton(
      icon = 'fa-globe', title = 'Select Metro Area',
      onClick = JS("function(btn, map){ $('.custom-control').not('#metro-control').slideUp(); $('#metro-control').slideToggle(); }")))
  
  for (metro in metros) {
    print(metro)
    
    # add zip overlays
    m <- m %>% 
      addPolylines(data = data[[metro]]$cbsa_outline, stroke = T, color = 'black', weight = 3, group = 'MSA', options = c(className = paste0('metro-shape metro-', metro))) %>%
      addPolygons(data = data[[metro]]$cbsa_shape, stroke = FALSE, fillOpacity = 0.8, smoothFactor = 0.2, color = ~data[[metro]]$color_pop(pop_total), label = data[[metro]]$popup_pop, group = 'MSA by Population', highlightOptions = highlight_shp, options = pathOptions(className = paste0('metro-shape metro-', metro))) %>%
      addPolygons(data = data[[metro]]$cbsa_shape, stroke = FALSE, fillOpacity = 0.8, smoothFactor = 0.2, color = ~color_income(inc_brks), label = data[[metro]]$popup_income, group = 'MSA by Median Household Income', highlightOptions = highlight_shp, options = pathOptions(className = paste0('metro-shape metro-', metro))) %>%
      addPolygons(data = data[[metro]]$cbsa_shape, stroke = FALSE, fillOpacity = 0.8, smoothFactor = 0.2, color = ~color_race(race_brks), label = data[[metro]]$popup_race, group = 'MSA by Race/Ethnicity', highlightOptions = highlight_shp, options = pathOptions(className = paste0('metro-shape metro-', metro))) %>%
      
      # add markers
      addCircleMarkers(data = data[[metro]]$pubhs_purchased, lng = ~longitude, lat = ~latitude, group = 'Purchased Public High Schools',
                       radius = ~sqrt(num_total) + 2, fillOpacity = 0, opacity = 1, weight = 1.5, color = 'blue',
                       popup = data[[metro]]$popup_pubhspurchased, options = pathOptions(className = paste0('univ-pin univ-shared-', metro))) %>%
      
      addCircleMarkers(data = data[[metro]]$pubhs_nonpurchased, lng = ~longitude, lat = ~latitude, group = 'Non-Purchased Public High Schools',
                       radius = 2, fillOpacity = 0, opacity = 1, weight = 1.5, color = 'red',
                       popup = data[[metro]]$popup_pubhsnonpurchased, options = pathOptions(className = paste0('univ-pin univ-shared-', metro))) %>%
      
      addCircleMarkers(data = data[[metro]]$privhs_purchased, lng = ~longitude, lat = ~latitude, group = 'Purchased Private High Schools',
                       radius = ~sqrt(num_total) + 2, fillOpacity = 0, opacity = 1, weight = 1.5, color = '#ffa01c',
                       popup = data[[metro]]$popup_privhspurchased, options = pathOptions(className = paste0('univ-pin univ-shared-', metro))) %>%
      
      addCircleMarkers(data = data[[metro]]$privhs_nonpurchased, lng = ~longitude, lat = ~latitude, group = 'Non-Purchased Private High Schools',
                       radius = 2, fillOpacity = 0, opacity = 1, weight = 1.5, color = 'red',
                       popup = data[[metro]]$popup_privhsnonpurchased, options = pathOptions(className = paste0('univ-pin univ-shared-', metro))) %>%
      
      # add metro-specific population total legend
      addLegend(data = data[[metro]]$cbsa_shape,
                position = 'topright', pal = data[[metro]]$color_pop, values = ~pop_total,
                title = 'Population Total',
                className = paste0('info legend legend-pop-', metro),
                na.label = 'NA',
                opacity = 1)
  }
    
  m <- m %>% 
    
    # add legends
    addLegend(data = zip_shp,
              position = 'topright', pal = color_income, values = ~inc_brks,
              title = 'Median Household Income',
              className = 'info legend legend-income',
              na.label = 'NA',
              opacity = 1) %>%
    
    addLegend(data = zip_shp,
              position = 'topright', pal = color_race, values = ~race_brks,
              title = 'Black, Latinx, and <br>Native Population',
              className = 'info legend legend-race',
              na.label = 'NA',
              opacity = 1) %>%
    
    # uncheck options by default
    hideGroup('Purchased Public High Schools') %>%
    hideGroup('Purchased Private High Schools') %>%
    hideGroup('Non-Purchased Public High Schools') %>%
    hideGroup('Non-Purchased Private High Schools') %>%
    hideGroup('MSA by Population') %>%
    hideGroup('MSA by Median Household Income') %>%
    hideGroup('MSA by Race/Ethnicity') %>%
    
    # add options
    addLayersControl(
      position = c('bottomleft'),
      baseGroups = c('MSA', 'MSA by Population', 'MSA by Median Household Income', 'MSA by Race/Ethnicity'),
      overlayGroups = c('Purchased Public High Schools', 'Non-Purchased Public High Schools', 'Purchased Private High Schools', 'Non-Purchased Private High Schools'),
      options = layersControlOptions(collapsed = F)
    ) %>%
    
    htmlwidgets::onRender(js, choices)
    
    m
  
}


# Save maps

saveWidget(create_interactive_map(uiuc), file.path(normalizePath(outputs_dir), 'map_segment.html'), background = 'transparent', selfcontained = T)
saveWidget(create_interactive_map(poc), file.path(normalizePath(outputs_dir), 'map_poc.html'), background = 'transparent', selfcontained = T)
