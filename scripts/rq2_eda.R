################################################################################
## [ PROJ ] < College Board Geomarket >
## [ FILE ] < rq2_eda.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 1/16/2025
## [ DESC ] < has code for exploratory data analysis associated with eda >
              # has some useful stuff, like identifies the order that grabs all native american SAT test-takers in country
              # also, has order numbers for particular metro areas
################################################################################

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
#Sys.getenv("CENSUS_API_KEY") # retreive API key


library(sf)
# Enable caching for tigris
options(tigris_use_cache = TRUE)
library(tigris)
#library(stars)
#library(spatstat)
#library(rgeos)
library(lwgeom) # library has checks/vixes for valid geometries
library(leaflet)
library(shiny)
library(kableExtra)


######### CREATE ANALYSIS DATASET(S) FOR RQ2 TABLES N' GRAPHS

getwd()
source(file = file.path('scripts', 'create_stu_list.R'))
getwd()





############# BEGIN EDA FOR GEOMARKET PAPER

# WHICH GEOGRAPHIC AREAS TO CONSIDER
# makes sense to think of Combined Statistical Area (CSA) rather than Metroplitan Statistical Area (MSA)
# CSA does better job of thinking about which geographic areas are connected by a big city

# Definition of an MSA:
# A Metropolitan Statistical Area (MSA) is a geographical region with a relatively high population density
# at its core and close economic ties throughout the area. MSAs are defined by the Office of Management 
# and Budget (OMB) and used by federal agencies in collecting, tabulating, and publishing federal statistics.
# Each MSA consists of one or more counties that have a high degree of social and economic integration 
# with the urban core, as measured by commuting patterns.

# 1. New York-Newark-Jersey City, NY-NJ-PA - 19,768,458 [YES]
# 2. Los Angeles-Long Beach-Anaheim, CA - 13,211,027 [YES]
# 3. Chicago-Naperville-Elgin, IL-IN-WI - 9,478,801 [YES]
# 4. Dallas-Fort Worth-Arlington, TX - 7,637,387 [YES]
# 5. Houston-The Woodlands-Sugar Land, TX - 7,122,240 [YES]
# 6. Washington-Arlington-Alexandria, DC-VA-MD-WV - 6,371,200 [YES]
# 7. Miami-Fort Lauderdale-Pompano Beach, FL - 6,138,333 [NO]
# 8. Philadelphia-Camden-Wilmington, PA-NJ-DE-MD - 6,102,434 [YES]
# 9. Atlanta-Sandy Springs-Alpharetta, GA - 6,089,815 [YES]
# 10. Phoenix-Mesa-Chandler, AZ - 4,845,832 [NO]
# 11. Boston-Cambridge-Newton, MA-NH - 4,875,390 [YES]
# 12. San Francisco-Oakland-Berkeley, CA - 4,749,008 [YES]
# 13. Riverside-San Bernardino-Ontario, CA - 4,650,631 [AS PART OF LA]
# 14. Detroit-Warren-Dearborn, MI - 4,392,041 [YES]
# 15. Seattle-Tacoma-Bellevue, WA - 4,018,598 [NO]
# 16. Minneapolis-St. Paul-Bloomington, MN-WI - 3,690,512
# 17. San Diego-Chula Vista-Carlsbad, CA - 3,338,330 [YES]
# 18. Tampa-St. Petersburg-Clearwater, FL - 3,219,514 [NO]
# 19. Denver-Aurora-Lakewood, CO - 2,963,821 [NO]
# 20. St. Louis, MO-IL - 2,820,253 [NO]
# 21. Baltimore-Columbia-Towson, MD - 2,839,065 [YES]
# 22. Charlotte-Concord-Gastonia, NC-SC - 2,733,256 
# 23. Orlando-Kissimmee-Sanford, FL - 2,694,418 [NO]
# 24. San Antonio-New Braunfels, TX - 2,558,143 [MAYBE]
# 25. Portland-Vancouver-Hillsboro, OR-WA - 2,511,612 [NO]
# 26. Sacramento-Roseville-Folsom, CA - 2,363,730 [NO]
# 27. Pittsburgh, PA - 2,324,743 [MAYBE]
# 28. Las Vegas-Henderson-Paradise, NV - 2,227,053 [NO]
# 29. Austin-Round Rock-Georgetown, TX - 2,283,371 [NO]
# 30. Cincinnati, OH-KY-IN - 2,256,884
# 31. Kansas City, MO-KS - 2,192,035
# 32. Columbus, OH - 2,138,926
# 33. Indianapolis-Carmel-Anderson, IN - 2,111,040
# 34. Cleveland-Elyria, OH - 2,088,251
# 35. San Jose-Sunnyvale-Santa Clara, CA - 1,990,660
# 36. Nashville-Davidson--Murfreesboro--Franklin, TN - 1,989,519
# 37. Virginia Beach-Norfolk-Newport News, VA-NC - 1,799,674
# 38. Providence-Warwick, RI-MA - 1,676,579
# 39. Milwaukee-Waukesha, WI - 1,574,731
# 40. Jacksonville, FL - 1,605,848


# Definition of a CSA:
# A Combined Statistical Area (CSA) is a U.S. geographic area defined by the Office of Management and Budget (OMB)
# that consists of two or more adjacent Core-Based Statistical Areas (CBSAs) that have significant social and 
# economic ties. A CSA typically includes a larger Metropolitan Statistical Area (MSA) and smaller adjacent MSAs or 
# Micropolitan Statistical Areas (μSAs). These areas are grouped together based on shared economic or commuting 
# patterns. CSAs provide a more comprehensive representation of an interconnected region, reflecting 
# regional economic and social networks more accurately than MSAs alone.

# 1. New York-Newark, NY-NJ-CT-PA - 23,582,649 [YES]
# 2. Los Angeles-Long Beach, CA - 18,711,054 [YES]
# 3. Chicago-Naperville, IL-IN-WI - 9,882,634 [YES]
# 4. Washington-Baltimore-Arlington, DC-MD-VA-WV-PA - 9,814,928 [YES]
# 5. San Jose-San Francisco-Oakland, CA - 9,714,453 [YES]
# 6. Boston-Worcester-Providence, MA-RI-NH-CT - 8,472,756 [YES]
# 7. Dallas-Fort Worth, TX-OK - 7,637,387 [YES]
# 8. Houston-The Woodlands, TX - 7,122,240 [YES]
# 9. Miami-Fort Lauderdale-Port St. Lucie, FL - 6,912,434 [NO]
# 10. Atlanta--Athens-Clarke County--Sandy Springs, GA - 6,789,266 [YES]
# 11. Philadelphia-Reading-Camden, PA-NJ-DE-MD - 6,228,601 [YES]
# 12. Phoenix-Mesa, AZ - 5,059,909 [NO]
# 13. Detroit-Warren-Ann Arbor, MI - 5,020,982 [YES]
# 14. Seattle-Tacoma, WA - 4,879,311 [NO]
# 15. Minneapolis-St. Paul, MN-WI - 4,084,979 [NO]
# 16. San Diego-Carlsbad, CA - 3,347,270 [YES]
# 17. Denver-Aurora, CO - 3,272,293 [NO]
# 18. St. Louis-St. Charles-Farmington, MO-IL - 2,909,669 [NO]
# 19. Tampa-St. Petersburg-Clearwater, FL - 3,219,514 [NO]
# 20. Orlando-Deltona-Daytona Beach, FL - 3,139,583 NO
# 21. Cleveland-Akron-Canton, OH - 3,586,918 [YES]
# 22. Charlotte-Concord, NC-SC - 2,846,550 [NO]
# 23. Sacramento-Roseville, CA - 2,664,758 [NO]
# 24. Portland-Vancouver-Salem, OR-WA - 3,211,568 [NO]
# 25. Kansas City-Overland Park-Kansas City, MO-KS - 2,487,053 [NO]
# 26. Indianapolis-Carmel-Muncie, IN - 2,327,717 [NO]
# 27. Las Vegas-Henderson, NV-AZ - 2,266,715 [NO]
# 28. Cincinnati-Wilmington-Maysville, OH-KY-IN - 2,347,102 [NO]
# 29. Columbus-Marion-Zanesville, OH - 2,204,564 [NO]
# 30. Milwaukee-Racine-Waukesha, WI - 2,065,890 [NO]


#top 20 orders by volume [basically all ASU orders]
lists_orders_zip_hs_df %>% count(ord_num) %>% arrange(desc(n)) %>% print(n=20)
orders_df %>% arrange(desc(num_students)) %>% select(univ_name,univ_id,order_num,order_title,num_students) %>% print(n=20)


# find which 

lists_orders_zip_hs_df %>% glimpse()
# need to find big orders that encompass a metro area;

lists_orders_zip_hs_df %>% count(hs_state_code) %>% arrange(desc(n)) %>% print(n=55)

# identify big orders
lists_orders_zip_hs_df %>% count(ord_num) %>% arrange(desc(n)) %>%  print(n=800) # dont have orders for 163726
3665455 - 163726 # 

orders_df %>% glimpse()


orders_df %>% count(state_name) %>% print(n=100)

# let's use Philly as a test case; identify orders that include people from PA
lists_orders_zip_hs_df %>% filter(hs_state_code == 'PA') %>% count(univ_name)

# A tibble: 12 × 2
# univ_name                                       n
# 1 Arizona State University-Tempe              67970 [id = 104151]
# 9 University of California-San Diego          15272 [id = 110680]
# 12 University of Illinois at Urbana-Champaign 11470 [145637]
# 10 University of Illinois at Chicago          5015  [id = 145600]

lists_orders_zip_hs_df %>% glimpse()

lists_orders_zip_hs_df %>% count() 

lists_orders_zip_hs_df %>% count(stu_first_gen) 



lists_orders_zip_hs_df %>% filter(hs_state_code == 'PA' & univ_id ==104151) %>% count(ord_num) %>% arrange(desc(n)) %>% print(n=20) # ASU
lists_orders_zip_hs_df %>% filter(hs_state_code == 'PA' & univ_id ==110680) %>% count(ord_num) %>% arrange(desc(n)) %>% print(n=10) # UC san diego
lists_orders_zip_hs_df %>% filter(hs_state_code == 'PA' & univ_id ==145600) %>% count(ord_num) %>% arrange(desc(n)) %>% print(n=10) # U Illinois Chicago
lists_orders_zip_hs_df %>% filter(hs_state_code == 'PA' & univ_id ==145637) %>% count(ord_num) %>% arrange(desc(n)) %>% print(n=10) # U Illinois Urbana-Champaign

orders_df %>% filter(order_num == '395793')

orders_df %>% glimpse()

lists_orders_zip_hs_df %>% filter(ord_num == '322193') %>% View()

orders_df %>% filter(order_num == '367919')

##########################
# order that was just native american indian/alaskan native
lists_orders_zip_hs_df %>% glimpse()
lists_orders_zip_hs_df %>% filter(ord_num == '487927') %>% count(stu_first_gen)
lists_orders_zip_hs_df %>% filter(ord_num == '487927') %>% count(hs_state_code) %>% arrange(desc(n)) %>% print(n=50)
# Combined Statistical Areas (CSAs) consist of two or more adjacent CBSAs that have significant employment interchanges. 
lists_orders_zip_hs_df %>% filter(ord_num == '487927') %>% count(hs_csatitle) %>% arrange(desc(n)) %>% print(n=50)
lists_orders_zip_hs_df %>% filter(ord_num == '487927') %>% count(hs_cbsatitle_1) %>% arrange(desc(n)) %>% print(n=50)
##########################

# ASU order 
lists_orders_zip_hs_df %>% filter(ord_num == '448922') %>% count(hs_state_code) %>% arrange(desc(n)) %>% print(n=50)
lists_orders_zip_hs_df %>% filter(ord_num == '448922') %>% count(hs_cbsatitle_1) %>% arrange(desc(n)) %>% print(n=50)


# investigating agreeement between number of names on PDF and number of names on df
lists_orders_zip_hs_df %>% filter(ord_num == '448427') %>% count()
lists_orders_zip_hs_df %>% filter(ord_num == '448427') %>% count(hs_state_code) %>% arrange(desc(n)) %>% print(n=50)

lists_orders_zip_hs_df %>% filter(ord_num == '448922') %>% count(hs_cbsatitle_1) %>% arrange(desc(n)) %>% print(n=50)

############
############ FIND ORDERS FOR CA
############

lists_orders_zip_hs_df %>% filter(hs_state_code == 'CA' & univ_id ==104151) %>% count(ord_num) %>% arrange(desc(n)) %>% print(n=20) # ASU
#    ord_num     n
#    <chr>   <int>


# PROLLY YES
#    ord_num     n
#    <chr>   <int>
# 1 366935  42790 FA19 - CA PSAT AD {JAN18); ordered 1/17/2018; 2019/20/21 HS grad class; CA; PSAT 1110 - 1210
# 8 366934  15806 FA19 - CA PSAT SE (JAN18); ordered 1/17/2018; 2019/20/21 hs grad class; CA; PSAT 1220-1290    
# 7 366932  15931 FA19 - CA PSAT BE (JAN18); ordered 1/17/2018; 2019/20/21 hs grad class; CA; PSAT 1300 - 1520

# 2 448375  33893 FA20 - CA PSAT AD {JAN19); ordered 1/8/2019; 2020 HS grad class; CA; PSAT 1070 - 1180
# 5 448374  19775; FA20 - CA PSAT BE (JAN19); ordered 1/8/2019; 2020/21 hs grad class; CA; PSAT 1270-1520
# 6 448420  19437; FA20 - CA PSAT SE (JAN19); ordered 1/8/2019; 2020/21 hs grad class; CA; PSAT 1190-1260

# 4 546954  21102 FA21 - CA PSAT AD (JAN20); ordered 1/6/2020; 2021 HS grad class; CA; PSAT 1070-1180
#12 546946  12061 FA21 - CA PSAT SE (JAN20); ordered 1/6/2020; 2021/2022 hs grad class; CA; PSAT 1190-1260
#13 546945  11041 FA21 - CA PSAT BE (JAN20); ordered 1/6/2020; 2021/22 hs grad class; CA; PSAT 1270-1520


# PROLLY NOT
#    ord_num     n
#    <chr>   <int>
#14 320162   7727 FA18 - CAAD LS (JULY17); 2018/19/20 HS class; SAT 1140-1260; college size
#16 366835   6341 FA19 - CA PSAT AD {JAN18) -LOW SES
#11 394956  12426 FA19 - CA SAT AD (JUL 18); ordered 7/23/2018; 2019/20 hs grad class; CA; PSAT 1140-1260


# NOT IN ORDERS DF
#    ord_num     n
#    <chr>   <int>
# 9 300370  15277 not in orders DF
#10 300372  14334 not in orders DF
#15 300203   6437 not in orders df
# 3 484711  26394 not on orders DF


orders_df %>% glimpse()
orders_df %>% filter(univ_id == 104151) %>% count()

############
############ FIND ORDERS FOR ATL
############

# Order Number 320148 FA18 - OOS AD SS (JULY17); includes college size (2,000 to 5,000 students; fewer than 2000 students)
# has GA01 - GA04; 
# 2018, 2019, 2020 HS grad class;
# SAT score 1180 - 1300
# Order Number 320184, FA18 - OOS BE LS {JULY17
# has GA01 - GA04; 
# 2018, 2019, 2020 HS grad class; SAT 1370 - 1600; college size (basically any college size, except small)
# Order Number 320186, FA18 - OOS SE LS (JULY17
# has GA01 - GA04; 
# 2018, 2019, 2020 HS grad class; SAT 1310 - 1360; college size (basically any college size, except small)

# Order Number 546978, FA21 - OOS PSAT BE (JAN20)
# has GA01 - GA03; not GA04
# PSAT 1270 - 1520
# 2021 HS grad class
# purchased 1/6/2020

# Order Number 547005, FA21 - OOS PSAT SE (JAN20)
# has GA01 - GA03; not GA04
# PSAT 1190 - 1260
# 2021 HS grad class
# purchased 1/6/2020


lists_orders_zip_hs_df %>% filter(hs_state_code == 'GA' & univ_id ==104151) %>% count(ord_num) %>% arrange(desc(n)) %>% print(n=20) # ASU
lists_orders_zip_hs_df %>% filter(hs_state_code == 'GA' & univ_id !=104151) %>% count(ord_num) %>% arrange(desc(n)) %>% print(n=20) # ASU

############
############ FIND ORDERS FOR HOUSTON
############

htown_eps_codes <- str_c("TX",15:18)
htown_eps_codes

# ASU ORDERS
# TX 15: Northwest Houston and Conroe School District [Y]; 
# TX 16: Southwest Houston Metro Area [Y]; 
# TX 17: Cit of Houston (East) [NO]; 
# TX 18:Galveston and East Harris Counties [NO]

lists_orders_zip_hs_df %>% count(zip_cbsatitle_1) %>% arrange(desc(n)) %>%  print(n=50)
lists_orders_zip_hs_df %>% count(zip_cbsa_1) %>% arrange(desc(n)) %>%  print(n=50)

# this is houston CBSA
lists_orders_zip_hs_df %>% filter(zip_cbsa_1 == '26420') %>% count(zip_cbsatitle_1) %>% arrange(desc(n)) %>%  print(n=50)

lists_orders_zip_hs_df %>% filter(zip_cbsa_1 == '26420', univ_id != '104151') %>% 
  mutate(
    univ_ordnum = str_c(univ_name,', univ_id=',univ_id,' ord_num=',ord_num, sep = "")
  ) %>%
  count(univ_ordnum)  %>% arrange(desc(n)) %>%  print(n=50)


# 1 Stephen F Austin State University, univ_id=228431 ord_num=329702          17850 # does all TX
# 2 Stephen F Austin State University, univ_id=228431 ord_num=402900          10407 15, 16, 17, 18 2019 HS grads, SAT 1020-1410, ordered 8/22/2018
# 3 Stephen F Austin State University, univ_id=228431 ord_num=465204           7710 15, 16, 17, 18; SAT 1020-1410, 2020 HS grad class, ordered 5/22/2019
# 4 Tarleton State University, univ_id=228529 ord_num=349818                   7602
# 5 Tarleton State University, univ_id=228529 ord_num=639210                   6732
# 6 Texas A&M University-Texarkana, univ_id=224545 ord_num=461422              6187
# 7 Texas A&M University-Texarkana, univ_id=224545 ord_num=460473              5305
# 8 Texas A&M University-Texarkana, univ_id=224545 ord_num=461423              5230
# 9 Stephen F Austin State University, univ_id=228431 ord_num=481281           4984
#10 Texas A&M University-Texarkana, univ_id=224545 ord_num=460489              4627
#11 Texas A&M University-Texarkana, univ_id=224545 ord_num=460468              4341
#12 Stephen F Austin State University, univ_id=228431 ord_num=403497           4289
#13 Stephen F Austin State University, univ_id=228431 ord_num=465241           4075
#14 Texas A&M University-Texarkana, univ_id=224545 ord_num=384707              3267
#15 Texas A&M University-Texarkana, univ_id=224545 ord_num=384743              3256
#16 Texas A&M University-Texarkana, univ_id=224545 ord_num=384742              3224
#17 University of Illinois at Urbana-Champaign, univ_id=145637 ord_num=500590  2926 # uses segment
#18 Texas A&M University-Texarkana, univ_id=224545 ord_num=501171              2410
#19 University of California-San Diego, univ_id=110680 ord_num=560105          2282 # 
#20 Texas A&M University-Texarkana, univ_id=224545 ord_num=398573              2258
#21 Tarleton State University, univ_id=228529 ord_num=269356                   2227
#22 Texas A&M University-Texarkana, univ_id=224545 ord_num=460466              2115



lists_orders_zip_hs_df %>% count(zip_csatitle) %>% arrange(desc(n)) %>%  print(n=50)
lists_orders_zip_hs_df %>% count(zip_csacode) %>% arrange(desc(n)) %>%  print(n=50)



############
############   

###################################
################################### MAPPING STUFF
###################################
fa20_oos_psat %>% mutate(stu_zip_len = str_length(stu_zip)) %>% count(stu_zip_len) # usually has 9-digit zipcode; 246,401/257993 = 95.5% of the time!

fa20_oos_psat %>% mutate(hs_zip_len = str_length(hs_zip_code)) %>% filter(is.na(hs_zip_len)) %>% count(hs_state_code)
count(hs_zip_len)

# focus on PA from the first order w/ PSAT score of 1070-1180
fa20_oos_psat %>% filter(hs_state_code == 'PA',ord_num == '') %>% 
  leaflet() %>%
  addTiles() %>%  # Add default map tiles
  addCircleMarkers(
    lng = ~hs_longitude,  # Longitude for the high school
    lat = ~hs_latitude,   # Latitude for the high school
    radius = 4,           # Marker size
    color = "blue",       # Marker color
    stroke = FALSE,       # No borders around markers
    fillOpacity = 0.7,    # Opacity of the markers
    popup = ~paste("ZIP Code:", hs_zip_code)  # Popup showing the high school ZIP code
  )


# Create the subset for PA borders (already done)
#pa_eps_geometry <- eps_geometry_zcta %>% filter(substr(eps, 1, 2) == 'PA')
pa_eps_geometry <- eps_geometry_zcta %>% filter(eps %in% c('PA 1','PA 2','PA 3','PA 4','PA 5'))

# Ensure CRS is WGS84
pa_eps_geometry <- st_transform(pa_eps_geometry, crs = 4326)

fa20_oos_psat_sf %>% glimpse()
# Group by high school and count the number of students per school
school_counts <- fa20_oos_psat_sf %>%
  filter(hs_state_code == 'PA', ord_num == '448922') %>%
  group_by(hs_ncessch,geometry) %>%
  summarise(
    stu_all = n(),
    stu_white = sum(stu_white_01, na.rm = TRUE),
    stu_asian = sum(stu_asian_01, na.rm = TRUE),
    stu_black = sum(stu_black_01, na.rm = TRUE),
    stu_hispanic = sum(stu_hispanic_01, na.rm = TRUE),
  )

school_counts %>% glimpse()
# Create the leaflet map with the PA borders and labels
leaflet(school_counts) %>%
  addProviderTiles(provider = providers$CartoDB.Positron) %>%
  
  # Markers for All Students
  addCircleMarkers(
    lng = ~st_coordinates(geometry)[,1],
    lat = ~st_coordinates(geometry)[,2],
    radius = ~sqrt(stu_all) + 2,
    color = "blue",
    stroke = TRUE,
    weight = 1,
    fillOpacity = 0.0,
    group = "All Students",
    popup = ~paste("Students (All):", stu_all)
  ) %>%
  
  # Markers for White Students
  addCircleMarkers(
    lng = ~st_coordinates(geometry)[,1],
    lat = ~st_coordinates(geometry)[,2],
    radius = ~sqrt(stu_white) + 2,
    color = "red",
    stroke = TRUE,
    weight = 1,
    fillOpacity = 0.0,
    group = "White Students",
    popup = ~paste("Students (White):", stu_white)
  ) %>%
  
  # Markers for Asian Students
  addCircleMarkers(
    lng = ~st_coordinates(geometry)[,1],
    lat = ~st_coordinates(geometry)[,2],
    radius = ~sqrt(stu_asian) + 2,
    color = "green",
    stroke = TRUE,
    weight = 1,
    fillOpacity = 0.0,
    group = "Asian Students",
    popup = ~paste("Students (Asian):", stu_asian)
  ) %>%
  
  # Markers for Black Students
  addCircleMarkers(
    lng = ~st_coordinates(geometry)[,1],
    lat = ~st_coordinates(geometry)[,2],
    radius = ~sqrt(stu_black) + 2,
    color = "purple",
    stroke = TRUE,
    weight = 1,
    fillOpacity = 0.0,
    group = "Black Students",
    popup = ~paste("Students (Black):", stu_black)
  ) %>%
  
  # Markers for Hispanic Students
  addCircleMarkers(
    lng = ~st_coordinates(geometry)[,1],
    lat = ~st_coordinates(geometry)[,2],
    radius = ~sqrt(stu_hispanic) + 2,
    color = "orange",
    stroke = TRUE,
    weight = 1,
    fillOpacity = 0.0,
    group = "Hispanic Students",
    popup = ~paste("Students (Hispanic):", stu_hispanic)
  ) %>%
  
  # Modify the addPolygons() function
  addPolygons(
    data = pa_eps_geometry,
    color = "black",
    weight = 2,
    fill = TRUE,         # Enable fill to capture mouse events over the area
    fillOpacity = 0,     # Make the fill transparent
    group = "EPS Borders",
    popup = ~paste("EPS:", eps),
    label = ~eps,
    labelOptions = labelOptions(
      style = list("font-weight" = "bold", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) %>%
  
  # Add layer controls to toggle visibility
  addLayersControl(
    overlayGroups = c(
      "All Students", "White Students", "Asian Students",
      "Black Students", "Hispanic Students", "EPS Borders"
    ),
    options = layersControlOptions(collapsed = FALSE)
  )

