################################################################################
## [ PROJ ] < College Board Geomarket >
## [ FILE ] < metro_eps_codes.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 1/13/2025
## [ DESC ] < creates character vectors of EPS codes for particular metros (or states), for use across multiple scripts >
################################################################################

library(tidyverse)



bay_area_eps_codes <- c(paste0("CA ", 4:9), paste0("CA", 10:11)) # bay area
bay_area_eps_codes

ca_eps_codes <- c(paste0("CA ", 1:9), paste0("CA", 10:34))
chi_eps_codes <- c(paste0("IL ", 7:9), paste0("IL", 10:13))
chi_eps_codes

cleveland_eps_codes <- paste0("OH ", 2:6) #
cleveland_eps_codes

philly_eps_codes <- paste0("PA ", 1:5) #
philly_eps_codes

long_island_eps_codes <- c(paste0('NY',16:21))

atl_eps_codes <- paste0("GA ", 1:4) #
atl_eps_codes

htown_eps_codes <- str_c("TX",15:18)
htown_eps_codes

# stuff to add
  # some metro for los angeles
  # some metro for NY NY
  # some metro for dmv [exclude cuz don't have lists?]
  # 

dallas_eps_codes <- c('TX19','TX20','TX21','TX22','TX23','TX24')

nj_eps_codes <- c(paste0("NJ ", 1:9), paste0("NJ", 10:12))
nj_eps_codes

#nj_north_metro_eps_codes <- c(paste0('NJ ', 4:9),paste0("NJ", 10:11))
nj_north_metro_eps_codes <- c(paste0('NJ ',c(4,6,7,8,9)),paste0("NJ", 10:11)) # taking monmouth out!
nj_north_metro_eps_codes

######    # GO THROUGH NEW JERSEY
# redefine norther nj metro as: 4, 5, 6, 7, 8, 9, 10, 11

# 4. Middlesex County NJ04
# 5. Monmouth County NJ05
  # including monmouth is a judgment call.
# 6. Somerset and Mercer Counties NJ06
# 7. Union County NJ07
# 8. Essex and Southern Passaic County NJ08
# 9. Hudson County NJ09
# 10. Bergen County NJ10
# 11.Morris and Northern Passaic County NJ11


### NY, NY metro

nyny_metro_eps_codes <- c('NY14','NY15',paste0('NY',22:30))
nyny_metro_eps_codes

# NY14: Staten Island []
# NY15: Westchester Co []
# NY22: Southeast Brooklyn []
# NY23: West Brooklyn []
# NY24: Northeast Brooklyn []
# NY25: East Bronx []
# NY26: West Bronx []
# NY27: Manhattan []
# NY28: South Queens []
# NY29: Northwest Queens []
# NY30: Northeast Queens []

detroit_eps_codes <- c('MI 1','MI 2','MI 3')
detroit_eps_codes

boston_eps_codes <- c('MA 6','MA 8','MA10')
boston_eps_codes

# greater miami
miami_eps_codes <- c('FL 5','FL 6','FL 7')
miami_eps_codes

# DMV

dmv_eps_codes <- c('DC 1','MD 2','MD 5','MD 3','MD 7','VA 1','VA 2')
dmv_eps_codes

# Western Maryland MD01 [exclude]
# Montgomery Metropolitan MD02 [include]
# Central Maryland excluding Baltimore MD03 [include]
# Eastern Shore MD04 [exclude]
# Prince Georges Metropolitan MD05 [include]
# Southern Maryland MD06 [exclude]
# Baltimore (Urban) MD07 [include]

### SOUTHERN CALIFORNIA

socal_eps_codes <- paste0("CA", 14:31) # socal 
socal_eps_codes



los_angeles_eps_codes <- c(paste0("CA", 14:22),'CA23','CA27')
los_angeles_eps_codes

# San Fernando Valley (West) CA14
# San Fernando Valley (East) CA15
# Glendale and Pasadena CA16
# West Los Angeles and West Beach CA17
# Hollywood and Wilshire CA18
# East Los Angeles CA19
# South Bay CA20
# South and South Central Los Angeles CA21
# Long Beach CA22
# INLAND EMPIRE
# Covina and West Covina CA23
# Riverside, San Bernardino, and Ontario CA27



orange_county_eps_codes <- c('CA24','CA25','CA26','CA28')
orange_county_eps_codes
# ANAHEIM/OC
# Whittier and North Orange County CA24
# Anaheim CA25
# Santa Ana CA26
# South Orange County CA28

san_diego_eps_codes <- paste0("CA", 29:31)
san_diego_eps_codes
# SAN DIEGO
# North San Diego County CA29
# South San Diego County excluding San Diego CA30
# City of San Diego CA31
