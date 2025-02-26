################################################################################
## [ PROJ ] < College Board Geomarket >
## [ FILE ] < rq2_metro_orders.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 2/25/2025
## [ DESC ] < Create a csv file that has info about the orders by metro >
################################################################################

# Load necessary packages
library(dplyr)
library(readr)

# -------------------------------------------------------------------------
# Purpose: Convert revised 'all_orders' list into a CSV containing
#   (1) metro
#   (2) eps_codes
#   (3) order_ids
#   (4) test_range
#   (5) figure_note
#
# Note: We'll write to "metro_orders.csv" in the working directory.
# -------------------------------------------------------------------------

library(dplyr)
library(readr)

# 1) Your revised all_orders list (example below):
all_orders <- list(
  # CHICAGO
  chicago = c(
    "chicago",
    "chi_eps_codes",
    "392833_487984",
    "SAT score 1020 - 1150",
    "Illinois standard 2019 and 2020; some notes here"
  ),
  chicago = c(
    "chicago",
    "chi_eps_codes",
    "392834_488035",
    "SAT score 1160 - 1300",
    "Illinois Honors 2019 and 2020; some notes here"
  ),
  chicago = c(
    "chicago",
    "chi_eps_codes",
    "392835_488053",
    "SAT score 1310 - 1600",
    "Illinois GPPA 2019 and 2020; some notes here"
  ),
  
  # PHILADELPHIA
  philadelphia = c(
    "philadelphia",
    "philly_eps_codes",
    "448922",
    "PSAT score 1070 - 1180",
    "Some notes here"
  ),
  philadelphia = c(
    "philadelphia",
    "philly_eps_codes",
    "448427",
    "PSAT score 1190 - 1260",
    "Some notes here"
  ),
  philadelphia = c(
    "philadelphia",
    "philly_eps_codes",
    "448440",
    "PSAT score 1270 - 1520",
    "Some notes here"
  ),
  
  # LOS ANGELES
  los_angeles = c(
    "los_angeles",
    "los_angeles_eps_codes",
    "448375_546954",
    "PSAT score 1070 - 1180",
    "Ordered Jan 2019; some notes here"
  ),
  los_angeles = c(
    "los_angeles",
    "los_angeles_eps_codes",
    "448420_546946",
    "PSAT score 1190 - 1260",
    "Some notes here"
  ),
  los_angeles = c(
    "los_angeles",
    "los_angeles_eps_codes",
    "448374_546945",
    "PSAT score 1270 - 1520",
    "Some notes here"
  ),
  
  # ORANGE COUNTY
  orange_county = c(
    "orange_county",
    "orange_county_eps_codes",
    "448375_546954",
    "PSAT score 1070 - 1180",
    "Ordered Jan 2019; some notes here"
  ),
  orange_county = c(
    "orange_county",
    "orange_county_eps_codes",
    "448420_546946",
    "PSAT score 1190 - 1260",
    "Some notes here"
  ),
  orange_county = c(
    "orange_county",
    "orange_county_eps_codes",
    "448374_546945",
    "PSAT score 1270 - 1520",
    "Some notes here"
  ),
  
  # SAN DIEGO
  san_diego = c(
    "san_diego",
    "san_diego_eps_codes",
    "448375_546954",
    "PSAT score 1070 - 1180",
    "Ordered Jan 2019; some notes here"
  ),
  san_diego = c(
    "san_diego",
    "san_diego_eps_codes",
    "448420_546946",
    "PSAT score 1190 - 1260",
    "Some notes here"
  ),
  san_diego = c(
    "san_diego",
    "san_diego_eps_codes",
    "448374_546945",
    "PSAT score 1270 - 1520",
    "Some notes here"
  ),
  
  # BAY AREA
  bay_area = c(
    "bay_area",
    "bay_area_eps_codes",
    "448375_546954",
    "PSAT score 1070 - 1180",
    "Ordered Jan 2019; some notes here"
  ),
  bay_area = c(
    "bay_area",
    "bay_area_eps_codes",
    "448420_546946",
    "PSAT score 1190 - 1260",
    "Some notes here"
  ),
  bay_area = c(
    "bay_area",
    "bay_area_eps_codes",
    "448374_546945",
    "PSAT score 1270 - 1520",
    "Some notes here"
  ),
  
  # NORTHERN NEW JERSEY
  northern_new_jersey = c(
    "northern_new_jersey",
    "nj_north_metro_eps_codes",
    "448922",
    "PSAT score 1070 - 1180",
    "Some notes here"
  ),
  northern_new_jersey = c(
    "northern_new_jersey",
    "nj_north_metro_eps_codes",
    "448427",
    "PSAT score 1190 - 1260",
    "Some notes here"
  ),
  northern_new_jersey = c(
    "northern_new_jersey",
    "nj_north_metro_eps_codes",
    "448440",
    "PSAT score 1270 - 1520",
    "Some notes here"
  ),
  
  # LONG ISLAND
  long_island = c(
    "long_island",
    "long_island_eps_codes",
    "448922",
    "PSAT score 1070 - 1180",
    "Some notes here"
  ),
  long_island = c(
    "long_island",
    "long_island_eps_codes",
    "448427",
    "PSAT score 1190 - 1260",
    "Some notes here"
  ),
  long_island = c(
    "long_island",
    "long_island_eps_codes",
    "448440",
    "PSAT score 1270 - 1520",
    "Some notes here"
  ),
  
  # DETROIT
  detroit = c(
    "detroit",
    "detroit_eps_codes",
    "448922_484698",
    "PSAT score 1070 - 1180",
    "Some notes here"
  ),
  detroit = c(
    "detroit",
    "detroit_eps_codes",
    "448427_547005",
    "PSAT score 1190 - 1260",
    "Some notes here"
  ),
  detroit = c(
    "detroit",
    "detroit_eps_codes",
    "448440_546978",
    "PSAT score 1270 - 1520",
    "Some notes here"
  ),
  
  # DALLAS
  dallas = c(
    "dallas",
    "dallas_eps_codes",
    "448922",
    "PSAT score 1070 - 1180",
    "Some notes here"
  ),
  dallas = c(
    "dallas",
    "dallas_eps_codes",
    "448427",
    "PSAT score 1190 - 1260",
    "Some notes here"
  ),
  dallas = c(
    "dallas",
    "dallas_eps_codes",
    "448440",
    "PSAT score 1270 - 1520",
    "Some notes here"
  ),
  
  # HOUSTON
  houston = c(
    "houston",
    "houston_eps_codes",
    "329702",
    "PSAT score 1010 - 1520",
    "Some notes here"
  )
)

# -------------------------------------------------------------------------
# Create a data frame from all_orders
# -------------------------------------------------------------------------
# Each element in the list has 5 entries:
#   1. metro
#   2. eps_codes
#   3. order_ids (already combined if multiple)
#   4. test_range
#   5. figure_note

df_list <- lapply(seq_along(all_orders), function(i) {
  current_vector <- all_orders[[i]]
  tibble(
    metro       = current_vector[1],
    eps_codes   = current_vector[2],
    order_ids   = current_vector[3],
    test_range  = current_vector[4],
    figure_note = current_vector[5]
  )
})

orders_df <- bind_rows(df_list)

# -------------------------------------------------------------------------
# Inspect the result
# -------------------------------------------------------------------------
print(orders_df)
# Or orders_df %>% glimpse()

# -------------------------------------------------------------------------
# Write out to a CSV
# -------------------------------------------------------------------------
write_csv(orders_df, file.path(scripts_dir,"metro_orders.csv"))

# Done! Your "metro_orders.csv" file now has columns:
#   metro, eps_codes, order_ids, test_range, figure_note


###########################
############################ ORDERS YOU USE
###########################

# ASU ORDERS
# 448922 (ordered 1/9/2019)
# 2020 HS grad class; geomarkets; PSAT 1070 - 1180
# 448427 (ordered 1/8/2019)
# 2020 HS grad class; geomarkets; PSAT 1190 - 1260
# 448440 (ordered 1/8/2019)
# 2020 HS grad class; geomarkets; PSAT 1270 - 1520

# 547038 (ordered 1/6/2020); 2021 HS grad class; geomarkets; PSAT 1070 - 1180
# 547005 (ordered 1/6/2020); 2021 HS grad class; geomarkets; PSAT 1190 - 1260
# 546978 (ordered 1/6/2020); 2021 HS grad class; geomarkets; PSAT 1270 - 1520

# 484698 (ordered 7/25/2019); 2021 HS grad class; PSAT 1070 - 1180


###########################
############################ RUN FUNCTION create_sim_eps_graph TO CREATE GRAPH OF RACE AND GRAPH OF FIRSTGEN
###########################
#CA ORDER NUMBERS

# 2 448375  33893 FA20 - CA PSAT AD {JAN19); ordered 1/8/2019; 2020 HS grad class; CA; PSAT 1070 - 1180
# 6 448420  19437; FA20 - CA PSAT SE (JAN19); ordered 1/8/2019; 2020/21 hs grad class; CA; PSAT 1190-1260
# 5 448374  19775; FA20 - CA PSAT BE (JAN19); ordered 1/8/2019; 2020/21 hs grad class; CA; PSAT 1270-1520

# 4 546954  21102 FA21 - CA PSAT AD (JAN20); ordered 1/6/2020; 2021 HS grad class; CA; PSAT 1070-1180
#12 546946  12061 FA21 - CA PSAT SE (JAN20); ordered 1/6/2020; 2021/2022 hs grad class; CA; PSAT 1190-1260
#13 546945  11041 FA21 - CA PSAT BE (JAN20); ordered 1/6/2020; 2021/22 hs grad class; CA; PSAT 1270-1520

# and these we are not including cuz different score ranges
# 1 366935  42790 FA19 - CA PSAT AD {JAN18); ordered 1/17/2018; 2019/20/21 HS grad class; CA; PSAT 1110 - 1210
# 8 366934  15806 FA19 - CA PSAT SE (JAN18); ordered 1/17/2018; 2019/20/21 hs grad class; CA; PSAT 1220-1290    
# 7 366932  15931 FA19 - CA PSAT BE (JAN18); ordered 1/17/2018; 2019/20/21 hs grad class; CA; PSAT 1300 - 1520

# ORDERS FROM UNIVERSITY OF ILLINOIS-CHICAGO
# 392833 (ordered 7/10/2018); Illinois standard 2019; HS class 2019, 2020; SAT 1020 - 1150; GPA A+ to B-
# 487984 Illinois standard 2020; ordered 7/19/2019; HS class 2020, 2021; IL; SAT 1020-1150;  GPA A+ to B-

# 392834 (ordered 7/10/2018); Illinois Honors 2019; HS class 2019, 2020; SAT 1160 - 1300; [no HS gpa criteria]
# 488035 Illinois HC 2020; ordered 7/19/2019; HS class 2020, 2021; SAT 1160 - 1300; GPA A+ to B-

# 392835 (ordered 7/9/2018); Illinois GPPA 2019; HS class 2019, 2020; SAT 1310 - 1600; GPA A+ to B-
# 488053 # Illinois GPPA 2020 (no cf);ordered 7/19/2019; HS class 2020, 2021; SAT 1310-1600; GPA A+ to B-

# 487927 (ordered 7/19/2019); native american 2020; HS class 2020, 2021; SAT score 1040 - 1600; GPA A+ to B-; AIAN

#1            New York-Newark-Jersey City, NY-NJ-PA 871
#2               Chicago-Naperville-Elgin, IL-IN-WI 863
#3             Houston-The Woodlands-Sugar Land, TX 860
#4               Los Angeles-Long Beach-Anaheim, CA 818
#5                  Dallas-Fort Worth-Arlington, TX 722
#6             Riverside-San Bernardino-Ontario, CA 394
#7                                             <NA> 335
#8     Washington-Arlington-Alexandria, DC-VA-MD-WV 323
#9                    San Antonio-New Braunfels, TX 295
#10                     Detroit-Warren-Dearborn, MI 286
#11     Philadelphia-Camden-Wilmington, PA-NJ-DE-MD 268
#12               Atlanta-Sandy Springs-Roswell, GA 262
#13                      Denver-Aurora-Lakewood, CO 262
#14                           Austin-Round Rock, TX 247
#15       Miami-Fort Lauderdale-West Palm Beach, FL 233
#16               San Francisco-Oakland-Hayward, CA 212
#17                          San Diego-Carlsbad, CA 177

#create_sim_eps_race_firstgen_table(data = lists_orders_zip_hs_df_sf, ord_nums = c('487984'), eps_codes = chi_eps_codes) %>% print(n=50) # 

#lists_orders_zip_hs_df_sf %>% as.data.frame() %>% filter(ord_num == '487927') %>% count(zip_cbsatitle_1) %>% arrange(desc(n))

#lists_orders_zip_hs_df_sf %>% as.data.frame() %>% glimpse()
# 2 Stephen F Austin State University, univ_id=228431 ord_num=329702          PSAT 1010-1520; grades C+ to A+, 2019 HS grads, all of TX, ordered 8/31/2017 [but seems to have TX 17....]

#create_sim_eps_race_table(data = fa20_oos_psat_sf, ord_nums = c('448922'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24')) # 
#create_sim_eps_race_table(data = fa20_oos_psat_sf, ord_nums = c('448427'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24')) 
#create_sim_eps_race_table(data = fa20_oos_psat_sf, ord_nums = c('448440'), eps_codes = c('TX19','TX20','TX21','TX22','TX23','TX24'))


# CA ORDER NUMBERS
# 1 366935  42790 FA19 - CA PSAT AD {JAN18); ordered 1/17/2018; 2019/20/21 HS grad class; CA; PSAT 1110 - 1210
# 8 366934  15806 FA19 - CA PSAT SE (JAN18); ordered 1/17/2018; 2019/20/21 hs grad class; CA; PSAT 1220-1290    
# 7 366932  15931 FA19 - CA PSAT BE (JAN18); ordered 1/17/2018; 2019/20/21 hs grad class; CA; PSAT 1300 - 1520

# 2 448375  33893 FA20 - CA PSAT AD {JAN19); ordered 1/8/2019; 2020 HS grad class; CA; PSAT 1070 - 1180
# 6 448420  19437; FA20 - CA PSAT SE (JAN19); ordered 1/8/2019; 2020/21 hs grad class; CA; PSAT 1190-1260
# 5 448374  19775; FA20 - CA PSAT BE (JAN19); ordered 1/8/2019; 2020/21 hs grad class; CA; PSAT 1270-1520


# 4 546954  21102 FA21 - CA PSAT AD (JAN20); ordered 1/6/2020; 2021 HS grad class; CA; PSAT 1070-1180
#13 546945  11041 FA21 - CA PSAT BE (JAN20); ordered 1/6/2020; 2021/22 hs grad class; CA; PSAT 1270-1520
#12 546946  12061 FA21 - CA PSAT SE (JAN20); ordered 1/6/2020; 2021/2022 hs grad class; CA; PSAT 1190-1260
