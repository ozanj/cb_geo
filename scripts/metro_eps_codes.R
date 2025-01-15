################################################################################
## [ PROJ ] < College Board Geomarket >
## [ FILE ] < metro_eps_codes.R >
## [ AUTH ] < Ozan Jaquette >
## [ INIT ] < 1/13/2025
## [ DESC ] < creates character vectors of EPS codes for particular metros (or states), for use across multiple scripts >
################################################################################

library(tidyverse)

socal_eps_codes <- paste0("CA", 14:28) # socal 
bay_eps_codes <- c(paste0("CA ", 4:9), paste0("CA", 10:11)) # bay area
bay_eps_codes 

ca_eps_codes <- c(paste0("CA ", 1:9), paste0("CA", 10:34))
chi_eps_codes <- c(paste0("IL ", 7:9), paste0("IL", 10:13))
chi_eps_codes

cleveland_eps_codes <- paste0("OH ", 2:6) #
cleveland_eps_codes

philly_eps_codes <- paste0("PA ", 1:5) #
philly_eps_codes

nj_eps_codes <- c(paste0("NJ ", 1:9), paste0("NJ", 10:12))
nj_eps_codes

nj_metro_eps_codes <- c('NJ 2','NJ 4','NJ 5',paste0('NJ ', 7:9),paste0("NJ", 10:11))
nj_metro_eps_codes

long_island_eps_codes <- c(paste0('NY',16:21))

atl_eps_codes <- paste0("GA ", 1:4) #
atl_eps_codes

htown_eps_codes <- str_c("TX",15:18)
htown_eps_codes

dallas_eps_codes <- c('TX19','TX20','TX21','TX22','TX23','TX24')