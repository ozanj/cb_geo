# https://stackoverflow.com/a/65935636

library(tidyverse)
library(shiny)
library(leaflet)
library(sf)


data_dir <- file.path('..', 'data')
eps_data_dir <- file.path(data_dir, 'eps_market')
tracts_dir <- file.path(eps_data_dir, 'tracts')

# Load county shapes for reference
load(file.path(eps_data_dir, 'counties_2020.RData'))

# Load tract shapes to trace
load(file.path(eps_data_dir, 'tracts_1980.RData'))
load(file.path(eps_data_dir, 'tracts_2000.RData'))
load(file.path(eps_data_dir, 'tracts_2020.RData'))

metro <- tracts_2020 %>% filter(str_detect(GEOID, '^17(097|031|043|197)'))
# metro <- tracts_2000 %>% filter(str_detect(gisjoin2, '^170(097|031|043|197)'))
# metro <- tracts_1980 %>% filter(str_detect(gisjoin2, '^170(097|031|043|197)'))

# Load previously traced shapes for reference
eps_2020 <- tracts_2020 %>%
  mutate(eps = case_when(
    GEOID %in% str_split(read.table(file.path(tracts_dir, '2020', 'IL 7.txt')), ',')[[1]] ~ 'IL 7',
    GEOID %in% str_split(read.table(file.path(tracts_dir, '2020', 'IL 8.txt')), ',')[[1]] ~ 'IL 8',
    GEOID %in% str_split(read.table(file.path(tracts_dir, '2020', 'IL 9.txt')), ',')[[1]] ~ 'IL 9',
    GEOID %in% str_split(read.table(file.path(tracts_dir, '2020', 'IL10.txt')), ',')[[1]] ~ 'IL10',
    GEOID %in% str_split(read.table(file.path(tracts_dir, '2020', 'IL11.txt')), ',')[[1]] ~ 'IL11',
    GEOID %in% str_split(read.table(file.path(tracts_dir, '2020', 'IL12.txt')), ',')[[1]] ~ 'IL12',
    GEOID %in% str_split(read.table(file.path(tracts_dir, '2020', 'IL13.txt')), ',')[[1]] ~ 'IL13'
  )) %>%
  filter(!is.na(eps)) %>%
  group_by(eps) %>%
  summarise(geometry = st_union(geometry)) %>%
  nngeo::st_remove_holes()

# eps_2000 <- tracts_2000 %>%
#   mutate(eps = case_when(
#     gisjoin2 %in% str_split(read.table(file.path(tracts_dir, '2000', 'IL 7.txt')), ',')[[1]] ~ 'IL 7',
#     gisjoin2 %in% str_split(read.table(file.path(tracts_dir, '2000', 'IL 8.txt')), ',')[[1]] ~ 'IL 8',
#     gisjoin2 %in% str_split(read.table(file.path(tracts_dir, '2000', 'IL 9.txt')), ',')[[1]] ~ 'IL 9',
#     gisjoin2 %in% str_split(read.table(file.path(tracts_dir, '2000', 'IL10.txt')), ',')[[1]] ~ 'IL10',
#     gisjoin2 %in% str_split(read.table(file.path(tracts_dir, '2000', 'IL11.txt')), ',')[[1]] ~ 'IL11',
#     gisjoin2 %in% str_split(read.table(file.path(tracts_dir, '2000', 'IL12.txt')), ',')[[1]] ~ 'IL12',
#     gisjoin2 %in% str_split(read.table(file.path(tracts_dir, '2000', 'IL13.txt')), ',')[[1]] ~ 'IL13'
#   )) %>%
#   filter(!is.na(eps)) %>%
#   group_by(eps) %>%
#   summarise(geometry = st_union(st_make_valid(geometry))) %>%
#   nngeo::st_remove_holes()
# 
# eps_1980 <- tracts_1980 %>%
#   mutate(eps = case_when(
#     gisjoin2 %in% str_split(read.table(file.path(tracts_dir, '1980', 'IL 7.txt')), ',')[[1]] ~ 'IL 7',
#     gisjoin2 %in% str_split(read.table(file.path(tracts_dir, '1980', 'IL 8.txt')), ',')[[1]] ~ 'IL 8',
#     gisjoin2 %in% str_split(read.table(file.path(tracts_dir, '1980', 'IL 9.txt')), ',')[[1]] ~ 'IL 9',
#     gisjoin2 %in% str_split(read.table(file.path(tracts_dir, '1980', 'IL10.txt')), ',')[[1]] ~ 'IL10',
#     gisjoin2 %in% str_split(read.table(file.path(tracts_dir, '1980', 'IL11.txt')), ',')[[1]] ~ 'IL11',
#     gisjoin2 %in% str_split(read.table(file.path(tracts_dir, '1980', 'IL12.txt')), ',')[[1]] ~ 'IL12',
#     gisjoin2 %in% str_split(read.table(file.path(tracts_dir, '1980', 'IL13.txt')), ',')[[1]] ~ 'IL13'
#   )) %>%
#   filter(!is.na(eps)) %>%
#   group_by(eps) %>%
#   summarise(geometry = st_union(geometry)) %>%
#   nngeo::st_remove_holes()

shinyApp(
  ui = fluidPage(
    'Trace out EPS shape by selecting tracts below:',
    leafletOutput('map'),
    verbatimTextOutput('selected')
  ),
  
  server <- function(input, output, session) {
    
    #create empty vector to hold all click ids
    selected_ids <- reactiveValues(ids = vector())
    
    #initial map output
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(zoomControl = T, zoomSnap = 0, zoomDelta = 0.5)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolylines(data = counties_2020 %>% filter(str_detect(GEOID, '^17(097|031|043|197)')), color = 'black', opacity = 1, weight = 4) %>%
        addPolygons(data = eps_2020, label = ~eps, weight = 4, opacity = 1, color = 'purple') %>%
        # addPolygons(data = eps_2000, label = ~eps, weight = 4, opacity = 1, color = 'green') %>%
        # addPolygons(data = eps_1980, label = ~eps, weight = 4, opacity = 1, color = 'blue') %>%
        addPolygons(data = metro,
                    fillColor = 'white',
                    fillOpacity = 0.5,
                    color = 'black',
                    stroke = TRUE,
                    weight = 1,
                    layerId = ~GEOID,  # GEOID/gisjoin2
                    group = 'regions',
                    label = ~GEOID) %>%  # GEOID/gisjoin2
        addPolygons(data = metro,
                    fillColor = 'red',
                    fillOpacity = 0.5,
                    weight = 1,
                    color = 'black',
                    stroke = TRUE,
                    group = ~GEOID) %>%  # GEOID/gisjoin2
        hideGroup(group = metro$GEOID)  # GEOID/gisjoin2
    }) #END RENDER LEAFLET
    
    #define leaflet proxy for second regional level map
    proxy <- leafletProxy('map')
    
    #create empty vector to hold all click ids
    selected <- reactiveValues(groups = vector())
    
    observeEvent(input$map_shape_click, {
      if(input$map_shape_click$group == 'regions'){
        selected$groups <- c(selected$groups, input$map_shape_click$id)
        proxy %>% showGroup(group = input$map_shape_click$id)
      } else {
        selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
        proxy %>% hideGroup(group = input$map_shape_click$group)
      }
      
      output$selected <- renderPrint({
        cat(selected$groups, sep = ',')
      })
    })
  }
)
