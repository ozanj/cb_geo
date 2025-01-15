getwd()

data_dir <- file.path('.','data') # main data directory
list.files(path = data_dir)

d1980_data_dir <-   file.path('.',data_dir,'1980_decennial') # main data directory
list.files(path = d1980_data_dir)

d2000_data_dir <-   file.path('.',data_dir,'2000_decennial') # 
list.files(path = d2000_data_dir)

acs2020_data_dir <-   file.path('.',data_dir,'2020_acs') # 
list.files(path = acs2020_data_dir)

# can't save shape files in the repo cuz they too big
shape_dir <-   file.path('.','..','cb_geomarket_shape') # main data directory
list.files(path = shape_dir)

# had to move analysis data files into shape folder because too big for git
analysis_data_dir <- file.path('.',shape_dir,'analysis_data') # analysis data directory
list.files(path = analysis_data_dir)

eps_data_dir <- file.path(data_dir,'eps_market') # has eps geomarket data and spaical data files
list.files(path = eps_data_dir)

scripts_dir <- file.path('.','scripts') # 
list.files(path = scripts_dir)

tables_dir <- file.path('.','results','tables') # 
list.files(path = tables_dir)

graphs_dir <- file.path('.','results','graphs') # 
list.files(path = graphs_dir)