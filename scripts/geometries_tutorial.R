# investigating different kinds of geometries

# Load the necessary library
library(sf)

# Create a simple POLYGON geometry
# The st_polygon function takes a list of coordinates that form the exterior ring of the polygon.
# Here we define a square with coordinates (0,0), (1,0), (1,1), (0,1), and close it by returning to (0,0)
polygon_geom <- st_sfc(st_polygon(list(rbind(
  c(0, 0),  # First corner (x, y)
  c(1, 0),  # Second corner (x, y)
  c(1, 1),  # Third corner (x, y)
  c(0, 1),  # Fourth corner (x, y)
  c(0, 0)   # Closing the polygon (back to first corner)
))))

polygon_geom %>% class()
polygon_geom %>% str()
polygon_geom %>% glimpse()

# View the numerical representation of the POLYGON geometry
print(polygon_geom)

# Plot the POLYGON
# We use the plot() function to visualize the polygon.
# The col argument sets the fill color, and border sets the color of the polygon's outline.
plot(polygon_geom, col = 'lightblue', border = 'darkblue', main = "Simple Polygon")

###############
############### MULTIPOLYGON
###############

# Load the necessary library
library(sf)

# Create a MULTIPOLYGON geometry
# A MULTIPOLYGON consists of multiple POLYGONs, each defined by a set of coordinates.
# Below we define two squares (two separate POLYGONs) as part of the MULTIPOLYGON:
# - The first square has corners (0,0), (1,0), (1,1), (0,1)
# - The second square has corners (2,2), (3,2), (3,3), (2,3)
multi_polygon_geom <- st_sfc(st_multipolygon(list(
  list(rbind(
    c(0, 0),  # First square's first corner
    c(1, 0),  # First square's second corner
    c(1, 1),  # First square's third corner
    c(0, 1),  # First square's fourth corner
    c(0, 0)   # Closing the first square
  )),
  list(rbind(
    c(2, 2),  # Second square's first corner
    c(3, 2),  # Second square's second corner
    c(3, 3),  # Second square's third corner
    c(2, 3),  # Second square's fourth corner
    c(2, 2)   # Closing the second square
  ))
)))

# View the numerical representation of the MULTIPOLYGON geometry
print(multi_polygon_geom)

# Plot the MULTIPOLYGON
# As before, the plot function will be used to visualize the multipolygon.
# Each individual polygon will have a fill color (lightblue) and an outline (darkblue).
plot(multi_polygon_geom, col = 'lightblue', border = 'darkblue', main = "Simple Multi-Polygon")


###############
############### GEOMETRYCOLLECTION
###############

# Load the necessary library
library(sf)

# Create a GEOMETRYCOLLECTION geometry
# A GEOMETRYCOLLECTION can contain different types of geometries.
# In this case, we'll include:
# - A POINT at (0, 0)
# - A LINESTRING that connects the points (1, 1) to (2, 2)
# - A POLYGON that forms a square with corners at (3,3), (4,3), (4,4), (3,4)

geometry_collection <- st_sfc(st_geometrycollection(list(
  st_point(c(0, 0)),                         # A single POINT at (0,0)
  st_linestring(rbind(c(1, 1), c(2, 2))),    # A LINESTRING from (1,1) to (2,2)
  st_polygon(list(rbind(                     # A POLYGON forming a square
    c(3, 3), c(4, 3), c(4, 4), c(3, 4), c(3, 3)  # Coordinates of the square
  )))
)))

geometry_collection %>% class()
geometry_collection %>% str()
geometry_collection %>% glimpse()

# View the numerical representation of the GEOMETRYCOLLECTION geometry
print(geometry_collection)

# Plot the GEOMETRYCOLLECTION
# The plot() function will visualize the different geometries within the collection.
# Since we have a mix of geometries (point, linestring, polygon), they will be plotted together.
plot(geometry_collection, col = 'lightblue', border = 'darkblue', main = "Geometry Collection")

# To better visualize:
# The POINT will appear as a dot,
# The LINESTRING will appear as a line,
# The POLYGON will appear as a square.

###############
############### EXTRACTING POLYGONS AND MULTIPOLYGONS FROM GEOMETRY COLLECTION
###############


# Create a GEOMETRYCOLLECTION with a POINT, LINESTRING, POLYGON, and MULTIPOLYGON
geometry_collection <- st_sfc(st_geometrycollection(list(
  st_point(c(0, 0)),                         # A single POINT at (0,0)
  st_linestring(rbind(c(1, 1), c(2, 2))),    # A LINESTRING from (1,1) to (2,2)
  st_polygon(list(rbind(                     # A POLYGON forming a square
    c(3, 3), c(4, 3), c(4, 4), c(3, 4), c(3, 3)
  ))),
  st_multipolygon(list(                      # A MULTIPOLYGON with two squares
    list(rbind(c(5, 5), c(6, 5), c(6, 6), c(5, 6), c(5, 5))),
    list(rbind(c(7, 7), c(8, 7), c(8, 8), c(7, 8), c(7, 7)))
  ))
)))

geometry_collection %>% str()
geometry_collection

# Extract both POLYGON and MULTIPOLYGON geometries (they are treated the same)
polygon_geom <- st_collection_extract(geometry_collection, "POLYGON")

# View the extracted POLYGON and MULTIPOLYGON geometries
print(polygon_geom)

polygon_geom %>% str()
polygon_geom %>% typeof()
polygon_geom %>% length()

# Plot the extracted POLYGON and MULTIPOLYGON geometries
plot(polygon_geom, col = 'lightblue', border = 'darkblue', main = "Extracted Polygons and Multi-Polygons")
