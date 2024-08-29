# ggplot maps




##### PLAYING AROUND W/ PLOTTING

# plotting all geomarkets (including AK and HI)
ggplot(data = d1980_stf1f3_anal_eps_sf) +
  geom_sf() +
  theme_minimal() +
  labs(title = "College Board Geomarkets (EPS Codes)",
       subtitle = "Mapping all EPS regions in the US")

# Create an object containing the EPS codes to exclude
excluded_eps_codes <- c('AK 1', 'AK 2', 'HI 1', 'HI 2')

# Filter out the EPS codes and plot the remaining regions
ggplot(data = d1980_stf1f3_anal_eps_sf %>% filter(!eps %in% excluded_eps_codes)) +
  geom_sf() +
  theme_minimal() +
  labs(title = "College Board Geomarkets (EPS Codes) Excluding Alaska and Hawaii",
       subtitle = "Mapping all EPS regions in the contiguous US")

# CA ONLY
# Create a vector of EPS codes for California
ca_eps_codes <- c(paste0("CA ", 1:9), paste0("CA", 10:34))
ca_eps_codes

# Filter for California EPS codes and plot
ggplot(data = d1980_stf1f3_anal_eps_sf %>% filter(eps %in% ca_eps_codes)) +
  geom_sf() +
  theme_minimal() +
  labs(title = "College Board Geomarkets (EPS Codes) for California",
       subtitle = "Mapping all EPS regions in California")

# SoCal

# Create a vector of EPS codes for urban Southern California
socal_eps_codes <- paste0("CA", 14:28)

# Filter for Southern California EPS codes and plot
ggplot(data = d1980_stf1f3_anal_eps_sf %>% filter(eps %in% socal_eps_codes)) +
  geom_sf() +
  theme_minimal() +
  labs(title = "College Board Geomarkets (EPS Codes) for Urban Southern California",
       subtitle = "Mapping EPS regions CA14 through CA28")

# socal without cataline island [this didn't work!]

# Define the bounding box for Catalina Island
catalina_bbox <- st_bbox(c(xmin = -118.65, xmax = -118.3, ymin = 33.3, ymax = 33.5), crs = st_crs(d1980_stf1f3_anal_eps_sf))

# Filter for Southern California EPS codes, excluding Catalina Island
ggplot(data = d1980_stf1f3_anal_eps_sf %>% 
         filter(eps %in% socal_eps_codes) %>% 
         filter(!apply(st_intersects(geometry, st_as_sfc(catalina_bbox), sparse = FALSE), 1, any))) +
  geom_sf() +
  theme_minimal() +
  labs(title = "College Board Geomarkets for Urban Southern California (Excluding Catalina Island)",
       subtitle = "Mapping EPS regions CA14 through CA28")

# add eps labels to socal map

# Create a vector of EPS codes for urban Southern California
socal_eps_codes <- paste0("CA", 14:28)

# Filter for Southern California EPS codes and plot
ggplot(data = d1980_stf1f3_anal_eps_sf %>% filter(eps %in% socal_eps_codes)) +
  geom_sf() +
  geom_sf_text(aes(label = eps), size = 3, color = "black") +
  theme_minimal() +
  labs(title = "College Board Geomarkets (EPS Codes) for Urban Southern California",
       subtitle = "Mapping EPS regions CA14 through CA28")

# Filter for Southern California EPS codes and create a choropleth map
ggplot(data = d1980_stf1f3_anal_eps_sf %>% filter(eps %in% socal_eps_codes)) +
  geom_sf(aes(fill = pct_edu_baplus_all)) + # mean_inc_house med_inc_house_med_all pct_edu_baplus_all
  geom_sf_text(aes(label = eps), size = 3, color = "black") +
  scale_fill_viridis_c(option = "plasma", name = "Median Household Income") +
  theme_minimal() +
  labs(title = "Choropleth Map of Median Household Income in Urban Southern California",
       subtitle = "EPS regions CA14 through CA28")  

