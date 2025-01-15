# EXPERIMENTING WITH LEAFLET

### SETTINGS
rm(list = ls()) # remove all objects
options(max.print=1000)
#options(width = 160)
# Set the scipen option to a high value to avoid scientific notation
options(scipen = 999)

### LIBRARIES
library(tidyverse)
library(readxl)
library(lubridate)
library(haven)
library(labelled)
library(tidycensus)
# get census api key
#http://api.census.gov/data/key_signup.html
# set census api key, and install for future use
#census_api_key('aff360d1fe8a919619776f48e975f03b8bb1379e', install = TRUE)
Sys.getenv("CENSUS_API_KEY") # retreive API key
# mapbox token: pk.eyJ1Ijoib3phbmoiLCJhIjoiY20wYjdwYnZxMDY1NzJrcHZuNHpkZjk4ZCJ9.QTfW1xSq0MgKrXBEe3fSUg
# default public token: pk.eyJ1Ijoib3phbmoiLCJhIjoiY20wYjhzd2c0MDZmOTJqcHRmcXFmaW0zaCJ9.HlonUhSIdWIuWJXUsqh-eA
library(sf)
# Enable caching for tigris
options(tigris_use_cache = TRUE)
library(tigris)
#library(stars)
#library(spatstat)
#library(rgeos)
library(lwgeom) # library has checks/vixes for valid geometries
library(leaflet)

##############
  # from https://walker-data.com/census-r/mapping-census-data-with-r.html#interactive-mapping

dallas_bachelors <- get_acs(
  geography = "tract",
  variables = "DP02_0068P", # percentage of 25+ with a BA
  year = 2020,
  state = "TX",
  county = "Dallas",
  geometry = TRUE
)

# get an error when mapping in leaflet unless we use different CRS
  # Reproject dallas_bachelors to WGS84
  dallas_bachelors <- st_transform(dallas_bachelors, crs = 4326)

dallas_bachelors %>% class()
dallas_bachelors %>% glimpse()

dallas_bachelors %>% select(GEOID) %>% as.data.frame() %>% group_by(GEOID) %>% summarise(n_per_group = n()) %>% ungroup %>% count(n_per_group) # uniquely identifies


# mapview library

library(mapview)
mapview(dallas_bachelors, zcol = "estimate")

# tmap package, using interactive Leaflet maps

library(tmap)
tmap_mode("view") # After entering this command, all subsequent tmap maps in your R session will be rendered as interactive Leaflet maps using the same tmap syntax you’d use to make static maps.
  # tmap_mode('plot') # To switch back to static plotting mode, run the command tmap_mode("plot").

tm_shape(dallas_bachelors) + 
  tm_fill(col = "estimate", palette = "magma",
          alpha = 0.5)

########### leaflet lets you have more control

pal <- colorNumeric(
  palette = "magma",
  domain = dallas_bachelors$estimate,
  # reverse = TRUE
)

pal(c(10, 20, 30, 40, 50)) %>% str()

# build leaflet maps with pipes

  # The leaflet() function initializes the map. A data object can be specified here or in a function that comes later in the pipeline.
  leaflet() %>%
    # addProviderTiles() helps you add a basemap to the map that will be shown beneath your data as a reference
    addProviderTiles() %>% 
    # addPolygons() adds the tract polygons to the map and styles them. In the code below, we are using a series of options to specify the input data; to color the polygons relative to the defined color palette; and to adjust the smoothing between polygon borders, the opacity, and the line weight. The label argument will create a hover tooltip on the map for additional information about the polygons.
    addPolygons() %>% 
    # addLegend() then creates a legend for the map, providing critical information about how the colors on the map relate to the data values.
    addLegend() # 
   
  # EXAMPLE FROM WALKER BOOK
  
  #pal <- colorNumeric(palette = "Blues", domain = dallas_bachelors$estimate, reverse = TRUE)
  pal <- colorNumeric(palette = "Blues", domain = dallas_bachelors$estimate)
  
  leaflet() %>%
    #addProviderTiles(providers$Stamen.TonerLite) %>%
    #addTiles() %>% 
    addProviderTiles(
      provider = 'Stadia.AlidadeSmooth', # provider = 'Stadia.StamenWatercolor' 'Stadia.StamenTonerLite' 'OpenStreetMap.Mapnik'
      layerId = NULL,
      group = NULL,
      #options = providerTileOptions()      
    ) %>%
    addPolygons(data = dallas_bachelors,
                color = ~pal(estimate),
                weight = 0.5,
                smoothFactor = 0.2,
                fillOpacity = 0.5,
                label = ~estimate) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      #values = sort(dallas_bachelors$estimate, decreasing = TRUE),  # Reverse the order here
      values = dallas_bachelors$estimate,
      title = "% with bachelor's<br/>degree",
      labFormat = labelFormat(transform = function(estimate) sort(estimate, decreasing = TRUE)), # put higher values on top of legend
    )
  
  st_crs(dallas_bachelors)
  st_bbox(dallas_bachelors)
  summary(dallas_bachelors)
  
# simplified version works fine
  leaflet(dallas_bachelors) %>% addTiles() %>% addPolygons()
  
# let's add complication one step at a time
  leaflet() %>%
    addTiles() %>%
    addPolygons(
      data = dallas_bachelors,
      color = ~pal(estimate) # pal(dallas_bachelors$estimate)
    )
  
  
######## mucking around with article from Rstudio
  
  # https://rstudio.github.io/leaflet/articles/leaflet.html
  
# basic workflow
  
  # You create a Leaflet map with these basic steps:
    
  # Create a map widget by calling leaflet().
  # Add layers (i.e., features) to the map by using layer functions (e.g., addTiles, addMarkers, addPolygons) to modify the map widget.
  # Repeat step 2 as desired.
  # Print the map widget to display it.  
  
# basic example:
  
  m <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
  m  # Print the map  
  
  
# the map widget
  # https://rstudio.github.io/leaflet/articles/widget.html
  
####### CHLOROPLETS
  
  # https://rstudio.github.io/leaflet/articles/choropleths.html
  
  states <- sf::read_sf("https://rstudio.github.io/leaflet/json/us-states.geojson")
  class(states)
  names(states)

  # m <- leaflet(states) %>% setView(-96, 37.8, 4) %>% addTiles()
  
  m <- leaflet(states) %>%
    setView(-96, 37.8, 4) %>%
    addProviderTiles("MapBox", options = providerTileOptions(
      id = "mapbox/light-v10",
      accessToken = 'pk.eyJ1Ijoib3phbmoiLCJhIjoiY20wYjdwYnZxMDY1NzJrcHZuNHpkZjk4ZCJ9.QTfW1xSq0MgKrXBEe3fSUg'))
  
  m

  m %>% addPolygons()
  
# add some color
  bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
  pal <- colorBin("YlOrRd", domain = states$density, bins = bins)
  
  states$density
  pal(states$density)
  
  m %>% addPolygons(
    fillColor = ~pal(density),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7)
  
  
# adding interaction
  # The next thing we’ll want is to make the polygons highlight as the mouse passes over them. The addPolygon() function has a highlight argument that makes this simple.
  
  m %>% addPolygons(
    fillColor = ~pal(density),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.70,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE))  

# custom info
  # when user clicks on state, should provide state names density values to user
  labels <- sprintf(
    "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
    states$name, states$density
  ) %>% lapply(htmltools::HTML)
  
  labels
  
  m <- m %>% addPolygons(
    fillColor = ~pal(density),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
    )  
  
  m
  
# add legend
  
  m %>% addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
                  position = "bottomright")  
  
  
####### COLORS
  # https://rstudio.github.io/leaflet/articles/colors.html
  
# Color functions
  # essentially, you call the appropriate color function with (1) the colors you want to use; and (2) the range of of inputs (i.e., domain) that are espected
  
  # color function returns a palette function 
    # that can be passed a vector of input values
    # it will return a vector of colors in #RRGGBB(AA) format
  
  d <- colorNumeric(
    palette = c("red", "green", "blue"),
    domain = 1:10,
    na.color = "#808080",
    alpha = FALSE,
    reverse = FALSE
  )
  
  # which color functions exist
    # There are currently three color functions for dealing with continuous input: colorNumeric(), colorBin(), and colorQuantile(); and one for categorical input, colorFactor().
  
    # palette argumen t specifies the colors to map the data to
    # domain argument tells the color function the range of input values
  
  
# coloring continuous data
  # From http://data.okfn.org/data/datasets/geo-boundaries-world-110m
  countries <- sf::read_sf("https://rstudio.github.io/leaflet/json/countries.geojson")
  map <- leaflet(countries)  
  
  countries %>% glimpse()
  
  # graph gdp estimates
  
  par(mar = c(5,5,0,0), cex = 0.8)
  hist(countries$gdp_md_est, breaks = 20, main = "")
  
# continuouis input, continnous colors
  # colorNumeric does this
  
  # Create a continuous palette function
  
  countries$gdp_md_est
  
  pal <- colorNumeric(
    palette = "Blues",
    domain = countries$gdp_md_est)  

  pal(countries$gdp_md_est)  
  
  # Apply the function to provide RGB colors to addPolygons
  map %>%
    addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
                color = ~pal(gdp_md_est))
  
# continuous input, discreet colors
  # colorBin() and colorQuantile() does this
  
  binpal <- colorBin(
    palette = "Blues",
    domain = countries$gdp_md_est,
    bins = 6,
    pretty = FALSE,
    na.color = "#808080",
    alpha = FALSE,
    reverse = FALSE,
    right = FALSE
  )
  
  binpal(countries$gdp_md_est)

  map %>%
    addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
                color = ~binpal(gdp_md_est))

  # colorQuantile() maps numeric input data to a fixed number of output colors using quantiles (slicing the input domain into subsets with equal numbers of observations).
  qpal <- colorQuantile("Blues", countries$gdp_md_est, n = 7)
  
  map %>%
    addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
                color = ~qpal(gdp_md_est))  

  
  
### coloring categorical data
  
  # For categorical data, use colorFactor(). If the palette contains the same number of elements as there are factor levels, then the mapping will be 1:1; otherwise, the palette will be interpolated to produce the desired number of colors.
  
  # Make up some random levels. (TODO: Better example)
  countries$category <- factor(sample.int(5L, nrow(countries), TRUE))
  
  factpal <- colorFactor(topo.colors(5), countries$category)
  
  factpal <-  colorFactor(
    palette = topo.colors(5),
    domain = countries$category,
    levels = NULL,
    ordered = FALSE,
    na.color = "#808080",
    alpha = FALSE,
    reverse = FALSE
  )  
  
  leaflet(countries) %>%
    addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
                color = ~factpal(category))  
  
  
########## POPUPS AND LABELLS
  
  # Popups are small boxes containing arbitrary HTML, that point to a specific point on the map.
  
  # Use the addPopups() function to add standalone popup to the map.
  
  content <- paste(sep = "<br/>",
                   "<b><a href='https://www.samurainoodle.com/'>Samurai Noodle</a></b>",
                   "606 5th Ave. S",
                   "Seattle, WA 98138"
  )
  
  leaflet() %>% addTiles() %>%
    addPopups(-122.327298, 47.597131, content,
              options = popupOptions(closeButton = FALSE)
    )  
  
  content
  
  
  df <- read.csv(textConnection(
    "Name,Lat,Long
    Samurai Noodle,47.597131,-122.327298
    Kukai Ramen,47.6154,-122.327157
    Tsukushinbo,47.59987,-122.326726"
  ))
  
  leaflet(df) %>% addTiles() %>%
    addMarkers(~Long, ~Lat, label = ~htmlEscape(Name))  
  
  
  # Change Text Size and text Only and also a custom CSS
  leaflet() %>% addTiles() %>% setView(-118.456554, 34.09, 13) %>%
    addMarkers(
      lng = -118.456554, lat = 34.105,
      label = "Default Label",
      labelOptions = labelOptions(noHide = TRUE)) %>%
    addMarkers(
      lng = -118.456554, lat = 34.095,
      label = "Label w/o surrounding box",
      labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE)) %>%
    addMarkers(
      lng = -118.456554, lat = 34.085,
      label = "label w/ textsize 15px",
      labelOptions = labelOptions(noHide = TRUE, textsize = "15px")) %>%
    addMarkers(
      lng = -118.456554, lat = 34.075,
      label = "Label w/ custom CSS style",
      labelOptions = labelOptions(noHide = TRUE, direction = "bottom",
                                  style = list(
                                    "color" = "red",
                                    "font-family" = "serif",
                                    "font-style" = "italic",
                                    "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                    "font-size" = "12px",
                                    "border-color" = "rgba(0,0,0,0.5)"
                                  )))  
  
  
######### SHOW/HIDE LAYERS
  
# a group is a label given to a set of layers
  # many layers can belong to the same group. But each layer can only belong to zero or one groups
  rm(list = ls()) # remove all objects
  
  outline <- quakes[chull(quakes$long, quakes$lat),]
  
  map <- leaflet(quakes) %>%
    # Base groups
    addTiles(group = "OSM (default)") %>%
    addProviderTiles(providers$CartoDB.Positron, group = "Positron (minimal)") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery (satellite)") %>%
    # Overlay groups
    addCircles(
      ~ long,
      ~ lat,
      ~ 10 ^ mag / 5,
      stroke = FALSE,
      group = "Quakes",
      fillColor = "tomato"
    ) %>%
    addPolygons(
      data = outline,
      lng = ~ long,
      lat = ~ lat,
      fill = FALSE,
      weight = 2,
      color = "#FFFFCC",
      group = "Outline"
    ) %>%
    # Layers control
    addLayersControl(
      baseGroups = c(
        "OSM (default)",
        "Positron (minimal)",
        "World Imagery (satellite)"
      ),
      overlayGroups = c("Quakes", "Outline"),
      options = layersControlOptions(collapsed = FALSE)
    )
  map
  
  # some explanation:
    # addLayersControl() distinguishes between base groups, which can only be viewed one group at a time, and overlay groups, which can be individually checked or unchecked.
  
  map
  map %>% hideGroup("Outline")
  
# WITH MARKER CLUSTERS
  
  
  quakes <- quakes %>%
    dplyr::mutate(mag.level = cut(mag,c(3,4,5,6),
                                  labels = c('>3 & <=4', '>4 & <=5', '>5 & <=6')))
  
  quakes.df <- split(quakes, quakes$mag.level)
  
  l <- leaflet() %>% addTiles()
  
  names(quakes.df) %>%
    purrr::walk( function(df) {
      l <<- l %>%
        addMarkers(data=quakes.df[[df]],
                   lng=~long, lat=~lat,
                   label=~as.character(mag),
                   popup=~as.character(mag),
                   group = df,
                   clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = FALSE),
                   labelOptions = labelOptions(noHide = FALSE,
                                               direction = 'auto'))
    })
  
  l %>%
    addLayersControl(
      overlayGroups = names(quakes.df),
      options = layersControlOptions(collapsed = FALSE)
    )  
  
  
#### add legends
  rm(list = ls()) # remove all objects
  
   
  countries <- sf::read_sf("https://rstudio.github.io/leaflet/json/countries.geojson")
  map <- leaflet(countries) %>% addTiles()  
  
  map
  
  
  pal <- colorNumeric(
    palette = "YlGnBu",
    domain = countries$gdp_md_est
  )
  map %>%
    addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
                color = ~pal(gdp_md_est)
    ) %>%
    addLegend("bottomright", pal = pal, values = ~gdp_md_est,
              title = "Est. GDP (2010)",
              labFormat = labelFormat(prefix = "$"),
              opacity = 1
    ) 
  

  qpal <- colorQuantile("RdYlBu", countries$gdp_md_est, n = 5)
  map %>%
    addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
                color = ~qpal(gdp_md_est)
    ) %>%
    addLegend(pal = qpal, values = ~gdp_md_est, opacity = 1)
  
############ LINES AND SHAPES
  
  # WHAT WE ARE TALKING ABOUT: take spatial lines and shapes from R and add them to maps
  rm(list = ls()) # remove all objects
  
  
  fullsize <- rnaturalearth::countries110
  object.size(fullsize)
  
  simplified <- rmapshaper::ms_simplify(fullsize)
  object.size(simplified)  
  
  
cities <- read.csv(textConnection("
City,Lat,Long,Pop
Boston,42.3601,-71.0589,645966
Hartford,41.7627,-72.6743,125017
New York City,40.7127,-74.0059,8406000
Philadelphia,39.9500,-75.1667,1553000
Pittsburgh,40.4397,-79.9764,305841
Providence,41.8236,-71.4222,177994
"))

leaflet(cities) %>% addTiles() %>%
  addCircles(lng = ~Long, lat = ~Lat, weight = 1,
    radius = ~sqrt(Pop) * 30, popup = ~City
  )  

leaflet() %>% addTiles() %>%
  addRectangles(
    lng1=-118.456554, lat1=34.078039,
    lng2=-118.436383, lat2=34.062717,
    fillColor = "transparent"
  )

########## BASE MAPS

m <- leaflet() %>% setView(lng = -71.0589, lat = 42.3601, zoom = 12)
m %>% addTiles()

# third-party tiles
  
  # providers$
  names(providers)
  
  m %>% addProviderTiles(provider = providers$CartoDB.Positron)
  
  m %>% addProviderTiles(provider = providers$Esri.NatGeoWorldMap)
  
  
  m %>% addProviderTiles(provider = providers$OpenTopoMap)
  
  m %>% addProviderTiles(providers$Stadia.StamenToner)
  
  m %>%
    addProviderTiles(
      providers$Esri.WorldImagery,
      options = providerTileOptions(opacity = 0.5)
    ) %>%
    addProviderTiles(providers$CartoDB.VoyagerOnlyLabels)  