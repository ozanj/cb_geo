### Settings
rm(list = ls())
options(max.print=1000)

### Libraries

library(tidyverse)
library(lubridate)
library(labelled)
library(tidyr)
library(stringr)
#library(eatATA)
library(readxl)
library(usmap)
library(haven)
library(ggplot2)

################### OPEN DATA BY OJ

# set directory to public_requests_eda REPO
    setwd("public_requests_eda/")
    
# directory paths
    data_dir <- file.path('.', 'data')
    list.files(path = data_dir)

    scripts_dir <- file.path('.', 'scripts')
    list.files(path = scripts_dir)
 
# source files for data    
    # Run script that creates data frames from secondary data sources (e.g., ACS, NCES)
    source(file = file.path(scripts_dir, 'create_secondary_datasets.R'))
    
    # Run script that creates analysis data frames from order data and list data
    # NOTE: this script relies on data frames created by above create_secondary_datasets.R script
    source(file = file.path(scripts_dir, 'create_combined_order_list_analysis_datasets.R'))

    # Workaround to Crystal errors with Ozan's source script
    #save(lists_orders_zip_hs_df, file = file.path(data_dir, 'tbl_fig_listdata.RData'))    
  
################### FINAL SAMPLE FOR EMPIRICAL REPORT
    
    #remove extra dataframes
    rm(lists_orders_zip_df)
    
    #remove MN universities from ordersdf
    orders_df %>% count(univ_id, univ_name)
    orders_df <- orders_df %>% filter(univ_id!="174358" &  univ_id!="174075") 
    
    #remove MN universities from lists dfs
    lists_df %>% count(univ_id, univ_name)
    lists_orders_zip_hs_df %>% count(univ_id, univ_name)
    
    lists_df <- lists_df %>% filter(univ_id!="174358" &  univ_id!="174075") 
    lists_orders_zip_hs_df <- lists_orders_zip_hs_df %>% filter(univ_id!="174358" &  univ_id!="174075") 
    
   
    # Orders-- 14 universities (includes NAU)
    orders_df %>% 
      summarise(n=n_distinct(univ_id)) 
    
    # Orders-- 835 total orders
    orders_df %>% 
      summarise(n=n_distinct(order_num)) 
    
    # Lists-- 13 universities (don't have any lists for NAU)
    lists_orders_zip_hs_df %>% 
      summarise(n=n_distinct(univ_id)) 
    
    lists_orders_zip_hs_df %>% 
      summarise(n=n_distinct(ord_num)) 
    
    # Lists-- 596 total lists
    lists_df %>% 
      summarise(n=n_distinct(univ_id)) 
    
    # number of orders & lists for each university
    orders_df %>% count(univ_name, univ_id)
    lists_df %>% count(univ_name)
    
    
    # create regional versus research university according to our sample
    orders_df <- orders_df %>% mutate(
      univ_type = ifelse(univ_id=="145637" | univ_id=="145600" | univ_id=="104151" |
                         univ_id=="110653" | univ_id=="110680" | univ_id=="110644" |
                         univ_id=="228723" , "research", "regional"))

    orders_df %>% count(univ_name, univ_type)
    
    lists_df <- lists_df %>% mutate(
      univ_type = ifelse(univ_id=="145637" | univ_id=="145600" | univ_id=="104151" |
                           univ_id=="110653" | univ_id=="110680" | univ_id=="110644" |
                           univ_id=="228723" , "research", "regional"))
    
    lists_df %>% count(univ_name, univ_type)
    
    lists_orders_zip_hs_df <- lists_orders_zip_hs_df %>% mutate(
      univ_type = ifelse(univ_id=="145637" | univ_id=="145600" | univ_id=="104151" |
                           univ_id=="110653" | univ_id=="110680" | univ_id=="110644" |
                           univ_id=="228723" , "research", "regional"))
    
    lists_orders_zip_hs_df %>% count(univ_name, univ_type)

    
################### CREATE SECONDARY DATA SETS FOR METROS/SCHOOLS
    
    acs_income_zip <- read_csv(file.path(data_dir, 'acs_income_zip.csv'), col_types = c('state_fips_code' = 'c', 'zip_code' = 'c'), na = c('-666666666'))
    acs_income_zip$medincome_2564 <- rowMeans(acs_income_zip[, c('medincome_2544', 'medincome_4564')], na.rm = T)
    acs_income_zip$medincome_2564[is.nan(acs_income_zip$medincome_2564)] <- NA
    
    acs_income_metro <- read_csv(file.path(data_dir, 'acs_income_metro.csv'), col_types = c('cbsa_code' = 'c'), na = c('-666666666'))
    acs_income_metro$medincome_2564 <- rowMeans(acs_income_metro[, c('medincome_2544', 'medincome_4564')], na.rm = T)
    acs_income_metro$medincome_2564[is.nan(acs_income_metro$medincome_2564)] <- NA
    
    zip_locale <- read_sas(file.path(data_dir, 'EDGE_ZCTALOCALE_2021_LOCALE.sas7bdat'))
    
    ccd <- readRDS(file.path(data_dir, 'ccd_membership_1718.RDS')) %>% 
      select(-total_students) %>% 
      left_join(readRDS(file.path(data_dir, 'ccd_1718.RDS')) %>% select(ncessch, matches('g\\d{2}_|total_')), by = 'ncessch') %>% 
      left_join(zip_cbsa_name_data, by = c('lzip' = 'zip_code'))
    
    pss <- readRDS(file.path(data_dir, 'pss_1718.RDS')) %>% 
      left_join(zip_cbsa_name_data, by = 'zip_code')
    
    acs_zip <- acs_income_zip %>% 
      left_join(acs_race_zipcodev3 %>% select(zip_code, pop_total, contains('15_19'), state_code, cbsa_1, cbsa_1_ratio, cbsatitle_1, csacode, csatitle), by = 'zip_code')
  

################### CREATE FILTER DUMMIES   
    
    
    # Frequency of Filters Used Across Orders
    orders_df <- orders_df %>% 
      mutate(
        hsgrad_class = ifelse(!is.na(hs_grad_class), 1, 0),
        zip = ifelse(!is.na(zip_code) | !is.na(zip_code_file), 1, 0), #KSshould this include zip_code_file not missing too?
        states_fil = ifelse(!is.na(state_name), 1, 0), 
        cbsa = ifelse(!is.na(cbsa_name), 1, 0), 
        intl = ifelse(!is.na(intl_region), 1, 0), 
        segment = ifelse(!is.na(segment), 1, 0), 
        race = ifelse(!is.na(race_ethnicity), 1, 0), 
        gender = ifelse(!is.na(gender), 1, 0), 
        sat = ifelse((!is.na(sat_score_min) | !is.na(sat_score_max) | !is.na(sat_score_old_min) | !is.na(sat_score_old_max)), 1, 0), 
        psat = ifelse((!is.na(psat_score_min) | !is.na(psat_score_max) | !is.na(psat_score_old_min) | !is.na(psat_score_old_max)), 1, 0), 
        gpa = ifelse((!is.na(gpa_low) | !is.na(gpa_high)), 1, 0), 
        rank = ifelse((!is.na(rank_low) | !is.na(rank_high)), 1, 0), 
        geomarket = ifelse(!is.na(geomarket), 1, 0), 
        ap_score = ifelse(!is.na(ap_scores), 1, 0),
        county = ifelse(!is.na(county), 1, 0),
        college_type = ifelse(!is.na(college_type), 1, 0),
        edu_aspirations = ifelse(!is.na(edu_aspirations), 1, 0),
        rotc = ifelse(!is.na(rotc_plans), 1, 0),
        major = ifelse(!is.na(major), 1, 0),
        citizenship = ifelse(!is.na(citizenship), 1, 0),
        low_ses = ifelse(!is.na(low_ses), 1, 0),
        college_size = ifelse(!is.na(college_size), 1, 0),
        national_recognition_programs = ifelse(!is.na(national_recognition_programs), 1, 0),
        college_location = ifelse(!is.na(college_location), 1, 0),
        financial_aid = ifelse(!is.na(financial_aid), 1, 0),
        college_setting = ifelse(!is.na(college_setting), 1, 0),
        college_studentbody = ifelse(!is.na(college_student_body), 1, 0),
        college_living_plans = ifelse(!is.na(college_living_plans), 1, 0),
        proximity_search = ifelse(!is.na(proximity_search), 1, 0),
        hs_math = ifelse(!is.na(hs_math), 1, 0),
        first_gen_parent = ifelse(!is.na(first_gen_parent_edu), 1, 0),
        sat_math = ifelse((!is.na(sat_score_math_min) | !is.na(sat_score_math_max) | !is.na(sat_score_math_old_min) | !is.na(sat_score_math_old_max)), 1, 0), 
        sat_writing = ifelse((!is.na(sat_score_writing_min) | !is.na(sat_score_writing_max) | !is.na(sat_score_writing_old_min) | !is.na(sat_score_writing_old_max)), 1, 0), 
        sat_reading = ifelse((!is.na(sat_score_reading_min) | !is.na(sat_score_reading_max) | !is.na(sat_score_reading_old_min) | !is.na(sat_score_reading_old_max)), 1, 0), 
      )
    
    
    
    #race categories variable
    # create new categorical race filters var
    orders_df <- orders_df %>% mutate(
      race_filter = ifelse(race_ethnicity=="American Indian or Alaska Native", "Native American", NA_character_),
      race_filter = ifelse(race_ethnicity=="Black or African American", "Black", race_filter),
      race_filter = ifelse(race_ethnicity=="Cuban|Hispanic or Latino (including Spanish origin)|Mexican|Puerto Rican|Other Hispanic or Latino", "Latinx", race_filter),
      #race_filter = ifelse(race_ethnicity=="Asian (including Indian subcontinent and Philippines origin)|Other|I do not wish to respond to race|No, not of Hispanic, Latino, or Spanish origin|White (including Middle Eastern origin)", "Asian", race_filter),
      
      race_filter = ifelse(race_ethnicity=="Asian (including Indian subcontinent and Philippines origin)|Other|I do not wish to respond to race|No, not of Hispanic, Latino, or Spanish origin|White (including Middle Eastern origin)", "Asian, White", race_filter),
      race_filter = ifelse(race_ethnicity=="American Indian or Alaska Native|Native Hawaiian or Other Pacific Islander", "Native American, Native Hawaii/PI", race_filter),
      race_filter = ifelse(race_ethnicity=="American Indian or Alaska Native|Cuban|Black or African American|Hispanic or Latino (including Spanish origin)|Mexican|Puerto Rican|Other Hispanic or Latino", "Latinx, Native American", race_filter),
      race_filter = ifelse(race_ethnicity=="Asian (including Indian subcontinent and Philippines origin)|Other|I do not wish to respond to race|No, not of Hispanic, Latino, or Spanish origin|White (including Middle Eastern origin)|Native Hawaiian or Other Pacific Islander", "Asian, White, NativeHawaii/PI", race_filter),
      race_filter = ifelse(race_ethnicity=="Cuban|Black or African American|Hispanic or Latino (including Spanish origin)|Mexican|Puerto Rican|Other Hispanic or Latino", "Latinx, Black", race_filter),
      race_filter = ifelse(race_ethnicity=="Black or African American|American Indian or Alaska Native|Other Hispanic or Latino|Puerto Rican|Mexican|Hispanic or Latino (including Spanish origin)|Cuban", "Latinx, Black, Native American", race_filter), 
      race_filter = ifelse(race_ethnicity=="Asian (including Indian subcontinent and Philippines origin)|White (including Middle Eastern origin)|Other", "Asian, White", race_filter), 
      race_filter = ifelse(race_ethnicity=="Black or African American|American Indian or Alaska Native|Other Hispanic or Latino|Puerto Rican|Mexican|Hispanic or Latino (including Spanish origin)|Native Hawaiian or Other Pacific Islander|Cuban", "Latinx, Black, Native American, NativeHawaii/PI", race_filter),
      race_filter = ifelse(race_ethnicity=="American Indian or Alaska Native|\rAsian (including Indian subcontinent and Philippines origin)|Cuban|\rBlack or African American|\rHispanic or Latino (including Spanish origin)|\rMexican|\rPuerto Rican|\rOther Hispanic or Latino|\rNative Hawaiian or Other Pacific Islander", "Latinx, Black, Asian, Native American", race_filter),
      #adding in new univs
      race_filter = ifelse(race_ethnicity=="Black or African American|American Indian or Alaska Native|Native Hawaiian or Other Pacific Islander", "Black, Native American, NativeHawaii/PI", race_filter),
      race_filter = ifelse(race_ethnicity=="Other Hispanic or Latino|Puerto Rican|Mexican|Hispanic or Latino (including Spanish origin)|Cuban", "Latinx", race_filter),
      race_filter = ifelse(race_ethnicity=="Native Hawaiian or Other Pacific Islander", "NativeHawaii/PI", race_filter),
      race_filter = ifelse(race_ethnicity=="Black or African American|American Indian or Alaska Native|Other Hispanic or Latino|Puerto Rican|Mexican|Hispanic or Latino (including Spanish origin)|Native Hawaiian or Other Pacific Islander|Cuban", "Latinx, Black, Native American, NativeHawaii/PI", race_filter),
      race_filter = ifelse(race_ethnicity=="Asian (including Indian subcontinent and Philippines origin)|Other|I do not wish to respond to race|No, not of Hispanic, Latino, or Spanish origin|White (including Middle Eastern origin)", "Asian, White", race_filter),
      race_filter = ifelse(race_ethnicity=="Black or African American", "Black", race_filter),
      race_filter = ifelse(race_ethnicity=="Other|Asian (including Indian subcontinent and Philippines origin)|I do not wish to respond to race|No, not of Hispanic, Latino, or Spanish origin|White (including Middle Eastern origin)", "Asian, White", race_filter),
      race_filter = ifelse(race_ethnicity=="Include only National Hispanic Recognition Program recipient", "Latinx", race_filter),
      race_filter = ifelse(race_ethnicity=="NA", NA, race_filter),
      race_filter = ifelse(race_ethnicity=="", NA, race_filter),
    )
    
    orders_df$race_filter2 <- as.factor(orders_df$race_filter)
    orders_df %>% filter(is.na(race_filter)) %>% count(race_filter2)
    orders_df %>% count(race_ethnicity)
    
    orders_df$race_filter[orders_df$race_filter==''] <- NA
    
    #df <- orders_df %>%  count(univ_name, race_filter, race_ethnicity) %>% print(n=25)
    #race_orders %>%  count(race_filter)
    

    
################### GET TOTALS FOR NUMBER OF INSTITUTIONS THAT USED A RACE/FILTER
    
    #replace empty strings to NA in race_ethnicity
    orders_df %>% count(race_filter2)
    
    # count orders by institution if race/ethnicty filter=1
    orders_df %>% filter(!is.na(race_filter2)) %>% count(univ_name)
    orders_df %>% filter(!is.na(race_filter2)) %>% 
      summarise(n=n_distinct(order_num))
    
    # create new orders_df for only those that used a race+ethnicity filter
    orders_df <- orders_df %>% filter(!is.na(race_filter2)) 
    
    
    #count number of orders for which I have lists too
    lists_df %>% filter(order_no %in% orders_df$order_num,) %>% 
      summarise(n=n_distinct(order_no))
    
    orders_df <- orders_df %>% filter(order_num %in% lists_df$order_no,) 
    
    table1_a <- orders_df %>% filter(order_num %in% lists_df$order_no,) %>% group_by(univ_name) %>%  
      summarise(n=n_distinct(order_num))
    table1_b <-orders_df %>% filter(order_num %in% lists_df$order_no,) %>% group_by(univ_name) %>%summarise(total_prosp = sum(num_students, na.rm=T))
    
    table1 <- merge(table1_a, table1_b, by="univ_name")    
    
    
################################ DESCRIPTIVES ON RACE ORDERS    
    
    # how many orders total; prospects total
    orders_df %>% count()
    
    orders_df %>%
      distinct(univ_id, order_num) %>%
      group_by(univ_id) %>%
      summarize("orders by each univ" = n())
    
    
    orders_fig_totals <- orders_df %>% 
      group_by(univ_id) %>%
      summarise(total_orders = n(),
                total_students = sum(num_students, na.rm = T))
    
    orders_fig_totals <-  orders_fig_totals %>% arrange(-total_students) 
    
    orders_fig_totals<-tibble::rowid_to_column(orders_fig_totals, "university")
    
    
    orders_fig_totals$total_orders_st <- str_c(orders_fig_totals$total_orders, ' orders')
    orders_fig_totals <-  orders_fig_totals %>% arrange(-total_students) 
    
    orders_fig_totals %>% 
      ggplot(aes(x = total_students, y = total_orders)) +
      geom_bar(stat = 'identity') + coord_flip()
    
    ggplot(data=orders_fig_totals, aes(x=total_orders, y=total_students)) +
      geom_bar(stat="identity") + coord_flip()
    
    
    
    # FREQUENCY OF RACE/ETHNICITY CATEGORIES
    race_filters_used <- orders_df %>% count(race_filter2)
    
    race_filters_used  <- race_filters_used %>%
      mutate(
        percent= round((n/75)*100)
      )
    
    # Frequency of Filters Used Across Orders
    
    orders_df[orders_df == 0] <- NA
    
    orders_filters <- orders_df %>% 
      mutate(
        hsgrad_class = ifelse(!is.na(hs_grad_class), 1, 0),
        zip = ifelse(!is.na(zip_code) | !is.na(zip_code_file), 1, 0), #KSshould this include zip_code_file not missing too?
        states_fil = ifelse(!is.na(state_name), 1, 0), 
        cbsa = ifelse(!is.na(cbsa_name), 1, 0), 
        intl = ifelse(!is.na(intl_region), 1, 0), 
        segment = ifelse(!is.na(segment), 1, 0), 
        race = ifelse(!is.na(race_ethnicity), 1, 0), 
        gender = ifelse(!is.na(gender), 1, 0), 
        sat = ifelse((!is.na(sat_score_min) | !is.na(sat_score_max) | !is.na(sat_score_old_min) | !is.na(sat_score_old_max)), 1, 0), 
        psat = ifelse((!is.na(psat_score_min) | !is.na(psat_score_max) | !is.na(psat_score_old_min) | !is.na(psat_score_old_max)), 1, 0), 
        gpa = ifelse((!is.na(gpa_low) | !is.na(gpa_high)), 1, 0), 
        rank = ifelse((!is.na(rank_low) | !is.na(rank_high)), 1, 0), 
        geomarket = ifelse(!is.na(geomarket), 1, 0), 
        ap_score = ifelse(!is.na(ap_scores), 1, 0),
        county = ifelse(!is.na(county), 1, 0),
        college_type = ifelse(!is.na(college_type), 1, 0),
        edu_aspirations = ifelse(!is.na(edu_aspirations), 1, 0),
        rotc = ifelse(!is.na(rotc_plans), 1, 0),
        major = ifelse(!is.na(major), 1, 0),
        citizenship = ifelse(!is.na(citizenship), 1, 0),
        low_ses = ifelse(!is.na(low_ses), 1, 0),
        college_size = ifelse(!is.na(college_size), 1, 0),
        national_recognition_programs = ifelse(!is.na(national_recognition_programs), 1, 0),
        college_location = ifelse(!is.na(college_location), 1, 0),
        financial_aid = ifelse(!is.na(financial_aid), 1, 0),
        college_setting = ifelse(!is.na(college_setting), 1, 0),
        college_studentbody = ifelse(!is.na(college_student_body), 1, 0),
        college_living_plans = ifelse(!is.na(college_living_plans), 1, 0),
        proximity_search = ifelse(!is.na(proximity_search), 1, 0),
        hs_math = ifelse(!is.na(hs_math), 1, 0),
        first_gen_parent = ifelse(!is.na(first_gen_parent_edu), 1, 0),
        sat_math = ifelse((!is.na(sat_score_math_min) | !is.na(sat_score_math_max) | !is.na(sat_score_math_old_min) | !is.na(sat_score_math_old_max)), 1, 0), 
        sat_writing = ifelse((!is.na(sat_score_writing_min) | !is.na(sat_score_writing_max) | !is.na(sat_score_writing_old_min) | !is.na(sat_score_writing_old_max)), 1, 0), 
        sat_reading = ifelse((!is.na(sat_score_reading_min) | !is.na(sat_score_reading_max) | !is.na(sat_score_reading_old_min) | !is.na(sat_score_reading_old_max)), 1, 0), 
      )
    
    
    orders_filters1 <- orders_filters %>% 
      select(hsgrad_class, zip, states_fil, cbsa, 
             intl, segment, race, gender,sat, psat,
             gpa, rank, geomarket, ap_score, county, college_type,
             edu_aspirations, rotc, major, citizenship, low_ses, college_size,
             national_recognition_programs, college_location, financial_aid,
             college_setting, college_student_body, college_living_plans, proximity_search, hs_math, first_gen_parent,
             sat_math, sat_writing, sat_reading) %>%
      summarize_if(is.numeric, sum, na.rm=TRUE)
    
    
  
    orders_filters1  <- as.data.frame(t(orders_filters1))
    
    orders_filters1$filters <- rownames(orders_filters1)
    orders_filters1<- orders_filters1 %>% filter(V1>0)
    
    
    orders_filters1  <- orders_filters1 %>%
      mutate(
        percent= round((V1/75)*100)
      )
    
    
    
    #CURRENTLY FIGURE 8: Filters used in order purchases
    ggplot(orders_filters1, aes(x=filters, y=V1)) +
      geom_bar(stat = "identity") +
      ylab("Number of Orders") +
      geom_text(aes(label = str_c(percent, "%")), hjust = -0.1, colour = "black", size=2) +
      coord_flip() 
    
    
    # GPA Thresholds
    # descriptive stats on GPA Filter
    orders_df %>% filter(!is.na(gpa_high) | !is.na(gpa_high)) %>% group_by(univ_type) %>% count(gpa_low)
    orders_df %>% filter(!is.na(gpa_high) | !is.na(gpa_high)) %>% group_by(univ_type) %>% count(gpa_high)
    
    #replace empty strings with NA
    orders_df <- orders_df %>%
      mutate(across(c("gpa_low","gpa_high"), ~ifelse(.=="", NA, as.character(.))))
    
    orders_df %>% count(gpa_low)
    orders_df %>% count(gpa_high)
    
    research_gpalow <- orders_df %>% filter(univ_type=="research")
    research_gpalow <- research_gpalow %>% filter(!is.na(gpa_high) | !is.na(gpa_low))
    
    regional_gpalow <- orders_df %>% filter(univ_type=="regional")
    regional_gpalow <- regional_gpalow %>% filter(!is.na(gpa_high) | !is.na(gpa_low))
    
    research_gpalow <- research_gpalow  %>% group_by(gpa_low) %>%
      summarise(n_low = n()) %>%
      mutate(pct_low = round(n_low / sum(n_low)*100, digits=1))
    
    regional_gpalow <- regional_gpalow %>% group_by(gpa_low) %>%
      summarise(n_low = n()) %>%
      mutate(pct_low = round(n_low / sum(n_low)*100, digits=1))
    
    
    research_gpalow$type <- "research"
    regional_gpalow$type <- "regional"
    
    table_gpa <- rbind(research_gpalow,regional_gpalow)
    
    
    
    # SAT/PSAT Thresholds
    # descriptive stats on PSAT/SAT Filter
    orders_df %>% count(psat_score_max)
    orders_df %>% count(psat_score_min) 
    
    orders_df %>% count(psat_score_old_max)
    orders_df %>% count(psat_score_old_min)
    
    # PSAT cutoffs tabulations
    
    orders_df$brks <- cut(orders_df$psat_score_min, 
                          breaks=c(-1, 1000, 1101, 1201, 1301, 1401, 1501, 1620), 
                          labels=c("<1000", "1000-1100", "1110-1200", 
                                   "1210-1300", "1310-1400", "1410-1500", "1500,"))
    
    orders_df %>% group_by(psat_score_min) %>%
      summarise(n_high = n()) %>%
      mutate(pct_high = round(n_high / sum(n_high)*100, digits=1)) %>% print(n=50)
    
    
    psat_min<- orders_df %>% filter(!is.na(brks)) %>% group_by(brks) %>%
      summarise(n_high = n()) %>%
      mutate(pct_high = round(n_high / sum(n_high)*100, digits=1)) %>% complete(brks)
    
    psat_min$test<- "PSAT"
    psat_min$range<- "min"
    
    orders_df$brks <- cut(orders_df$psat_score_max, 
                          breaks=c(-1, 1000, 1101, 1201, 1301, 1401, 1501, 1620), 
                          labels=c("<1000", "1000-1100", "1110-1200", 
                                   "1210-1300", "1310-1400", "1410-1500", "1500,"))
    
    psat_max<- orders_df %>% filter(!is.na(brks)) %>% group_by(brks) %>%
      summarise(n_high = n()) %>%
      mutate(pct_high = round(n_high / sum(n_high)*100, digits=1)) %>% complete(brks)
    
    psat_max$test<- "PSAT"
    psat_max$range<- "max"
    
    
    
    # SAT cutoffs tabulations
    
    orders_df$brks <- cut(orders_df$sat_score_min, 
                          breaks=c(-1, 1000, 1101, 1201, 1301, 1401, 1501, 1620), 
                          labels=c("<1000", "1000-1100", "1110-1200", 
                                   "1210-1300", "1310-1400", "1410-1500", "1500,"))
    
    orders_df %>% group_by(sat_score_min) %>%
      summarise(n_high = n()) %>%
      mutate(pct_high = round(n_high / sum(n_high)*100, digits=1)) %>% print(n=50)
    
    
    sat_min <- orders_df %>% filter(!is.na(brks)) %>% group_by(brks) %>%
      summarise(n_high = n()) %>%
      mutate(pct_high = round(n_high / sum(n_high)*100, digits=1)) %>% complete(brks)
    
    sat_min$test<- "SAT"
    sat_min$range<- "min"
    
    
    
    orders_df$brks <- cut(orders_df$sat_score_max, 
                          breaks=c(-1, 1000, 1101, 1201, 1301, 1401, 1501, 1620), 
                          labels=c("<1000", "1000-1100", "1110-1200", 
                                   "1210-1300", "1310-1400", "1410-1500", "1500,"))
    
    sat_max <- orders_df %>% filter(!is.na(brks)) %>% group_by(brks) %>%
      summarise(n_high = n()) %>%
      mutate(pct_high = round(n_high / sum(n_high)*100, digits=1)) %>% complete(brks)
    
    
    sat_max$test<- "SAT"
    sat_max$range<- "max"
    
    
    table_scores <- rbind(psat_min,psat_max, sat_min, sat_max)
    
    table_scores %>% group_by(test) %>%
      summarise(num = sum(n_high, na.rm = T))
    
    #NEWFIGURE: PSAT/SAT Filters used in order purchases --min thresholds
    table_scores %>% filter(range=="min") %>%
      ggplot(aes(x=brks, y=pct_high)) +
      geom_bar(position="dodge", stat="identity") +
      facet_wrap(~test) +
      ylab("Percent of Orders") +
      ggtitle("Minimum Score Filters") +
      geom_text(aes(label = n_high), hjust = -0.1, colour = "black", size=2) +
      coord_flip()
    
    
    #NEWFIGURE: PSAT/SAT Filters used in order purchases--max thresholds
    table_scores %>% filter(range=="max") %>%
      ggplot(aes(x=brks, y=pct_high)) +
    geom_bar(position="dodge", stat="identity") +
    facet_wrap(~test) +
    ylab("Percent of Orders") +
    ggtitle("Maximum Score Filters") +
    geom_text(aes(label = n_high), hjust = -0.1, colour = "black", size=2) +
    coord_flip()
    
    save(table_scores,orders_fig_totals, orders_filters1, race_filters_used, file='/Users/karinasalazar/Library/CloudStorage/Dropbox/recruiting-m3-ProximityToWhiteness/data/race_filter_totals.RData')
    
    
################### QUALITATIVE CATEGORIZATION OF ORDERS (see also Dropbox/recruiting-m3-ProximityToWhiteness/scripts/race_orders.xls)
    
    
    # any patterns in order titles?
    race_orders_df %>% count(univ_name, order_title, race_ethnicity) %>% print(n=100)
    
    
    
    
    
# -----------------------------------------------------------------------------------------------
# Figure  - URM by SAT/PSAT (HS type, average income, racial composition)
# -----------------------------------------------------------------------------------------------
    
    #create aggregate race/ethnicity var figure
    poc_order <- '560119'
    poc_metros <- c('35620', '33100', '26420')
    
    poc <- lists_orders_zip_hs_df %>%
      filter(univ_id == '110680', zip_cbsa_1 %in% poc_metros, ord_num == poc_order) %>% 
      mutate(
        ord_type = 'prospect',
        stu_race_cb = if_else(is.na(stu_race_cb), 999, unclass(stu_race_cb)),
        race = recode(
          stu_race_cb,
          `0` = 'noresponse',
          `1` = 'amerindian',
          `2` = 'asian',
          `3` = 'black',
          `4` = 'hispanic',
          `8` = 'nativehawaii',
          `9` = 'white',
          `12` = 'tworaces',
          `999` = 'unknown'
        )
      ) %>% 
      select(zip_cbsa_1, zip_cbsatitle_1, ord_num, ord_type, stu_race_cb, race, ends_with('_common'), hs_school_control, hs_ncessch, stu_zip_code) %>% 
      left_join(acs_income_zip, by = c('stu_zip_code' = 'zip_code')) %>% 
      rename(
        cbsa_code = zip_cbsa_1,
        cbsa_name = zip_cbsatitle_1,
        control = hs_school_control
      )
    
    poc %>% 
      count(cbsa_code)
    
    poc %>% 
      count(cbsa_code, race)
    
    poc_cb <- poc %>% 
      group_by(cbsa_code, cbsa_name, race) %>% 
      summarise(
        count = n()
      ) %>% 
      ungroup() %>% 
      group_by(cbsa_code, cbsa_name) %>% 
      mutate(
        pct = count / sum(count, na.rm = T)
      ) %>% 
      ungroup()
    
    poc_common <- poc %>% 
      select(cbsa_code, cbsa_name, stu_white_common, stu_asian_common, stu_black_common, stu_is_hisp_common, stu_american_indian_common, stu_native_hawaiian_common) %>% 
      pivot_longer(
        cols = -c(cbsa_code, cbsa_name),
        names_pattern = 'stu_(\\w+)_common',
        names_to = 'race',
        values_to = 'count'
      ) %>% 
      group_by(cbsa_code, cbsa_name, race) %>% 
      summarise(
        count = sum(count, na.rm = T)
      ) %>% 
      ungroup() %>% 
      left_join(
        poc %>% count(cbsa_code),
        by = 'cbsa_code'
      ) %>% 
      mutate(
        pct = count / n
      )
  
    #purchases prospects by metros
    poc_metro_income <- acs_income_metro %>% 
      filter(cbsa_code %in% poc_metros) %>% 
      mutate(
        ord_type = 'metro'
      ) %>% 
      rename(
        income_2564 = medincome_2564
      ) %>% 
      select(cbsa_code, cbsa_name, ord_type, income_2564)
    
    poc_metro_pubhs <- ccd %>%
      filter(g12 >= 10, is_virtual == 0, updated_status %in% c('1', '3', '8'), cbsa_1 %in% poc_metros) %>% 
      select(cbsa_1, cbsatitle_1, ncessch, total_students, total_white, total_asian, total_black, total_hispanic, total_amerindian, total_nativehawaii, total_tworaces, total_unknown, lzip) %>% 
      left_join(acs_income_zip, by = c('lzip' = 'zip_code')) %>% 
      left_join(poc %>% count(hs_ncessch), by = c('ncessch' = 'hs_ncessch')) %>% 
      mutate(
        n_cat = case_when(
          is.na(n) ~ 'zero',
          n <= 5 ~ '1-5',
          T ~ '6+'
        ),
        control = 'public'
      ) %>% 
      select(-lzip)
    
    poc_metro_privhs <- pss %>% 
      filter(total_12 >= 10, cbsa_1 %in% poc_metros) %>% 
      select(cbsa_1, cbsatitle_1, ncessch, total_students, total_white, total_asian, total_black, total_hispanic, total_amerindian, total_nativehawaii, total_tworaces, zip_code) %>% 
      left_join(acs_income_zip, by = 'zip_code') %>% 
      left_join(poc %>% count(hs_ncessch), by = c('ncessch' = 'hs_ncessch')) %>% 
      mutate(
        n_cat = case_when(
          is.na(n) ~ 'zero',
          T ~ '1+'
        ),
        control = 'private'
      ) %>% 
      select(-zip_code)
    
    poc_metro_pubprivhs <- poc_metro_pubhs %>% 
      bind_rows(poc_metro_privhs) %>% 
      rename(
        cbsa_code = cbsa_1,
        cbsa_name = cbsatitle_1
      )
    
    poc_metro_hs <- poc_metro_pubprivhs %>% 
      select(cbsa_code, cbsa_name, control, ncessch, total_students) %>% 
      mutate(
        ord_type = 'metro'
      ) %>% 
      group_by(cbsa_code, cbsa_name, ord_type, control) %>% 
      summarise(
        count = sum(total_students)
      ) %>% 
      mutate(
        pct = count / sum(count, na.rm = T)
      ) %>% 
      ungroup() 
    
    poc_hs <- poc %>% 
      filter(hs_ncessch %in% poc_metro_pubprivhs$ncessch) %>% 
      group_by(cbsa_code, cbsa_name, ord_type, control) %>% 
      summarise(
        count = n()
      ) %>% 
      filter(!is.na(control)) %>% 
      mutate(
        pct = count / sum(count, na.rm = T)
      ) %>% 
      ungroup() %>% 
      bind_rows(poc_metro_hs)
    
    poc_race <- poc_metro_pubprivhs %>% 
      select(cbsa_code, cbsa_name, control, ncessch, n_cat, n, total_white, total_asian, total_black, total_hispanic, total_amerindian, total_nativehawaii, total_tworaces, total_unknown) %>% 
      mutate(
        ord_type = str_c(control, n_cat, sep = '_')
      ) %>% 
      select(-control, -n_cat) %>% 
      pivot_longer(
        cols = starts_with('total_'),
        names_prefix = 'total_',
        names_to = 'race',
        values_to = 'count'
      ) %>% 
      mutate(
        n = if_else(is.na(n), 0L, n),
        count = if_else(is.na(count), 0L, count)
      ) %>% 
      group_by(cbsa_code, cbsa_name, ord_type, ncessch) %>%
      mutate(
        pct = count / sum(count, na.rm = T),
        pct = if_else(is.nan(pct), NA_real_, pct)
      ) %>%
      ungroup() %>%
      group_by(cbsa_code, cbsa_name, ord_type, race) %>%
      summarise(
        num_hs = n(),
        num_prospects = sum(n, na.rm = T),
        count = sum(count, na.rm = T), 
        pct = mean(pct, na.rm = T)
      ) %>% 
      ungroup()
    
    poc_race %>%  # 0 for unknown race so no effect on pct if removed
      filter(race == 'unknown')
    
    poc_income_hs <- poc_metro_pubprivhs %>% 
      select(cbsa_code, cbsa_name, control, ncessch, n_cat, n, medincome_2564) %>% 
      group_by(cbsa_code, cbsa_name, control, n_cat) %>% 
      summarise(
        num_hs = n(),
        num_prospects = sum(n, na.rm = T),
        income_2564 = mean(medincome_2564, na.rm = T)
      ) %>% 
      ungroup() %>% 
      mutate(
        ord_type = str_c(control, n_cat, sep = '_')
      ) %>% 
      select(cbsa_code, cbsa_name, ord_type, num_hs, num_prospects, income_2564)
    
    poc_income <- poc %>% 
      filter(hs_ncessch %in% poc_metro_pubprivhs$ncessch) %>% 
      group_by(cbsa_code, cbsa_name, ord_type) %>% 
      summarise(
        income_2564 = mean(medincome_2564, na.rm = T)
      ) %>% 
      ungroup() %>% 
      bind_rows(poc_metro_income) %>% 
      bind_rows(poc_income_hs)
    
    # Check totals for income and race are consistent
    poc_race %>% group_by(cbsa_code, ord_type) %>% summarise(sum(num_hs) / 8, sum(num_prospects / 8))
    poc_income %>% filter(!is.na(num_hs)) %>% group_by(cbsa_code, ord_type) %>% summarise(sum(num_hs), sum(num_prospects))
    
    #Export DF for figures
    
    
    
    
# -----------------------------------------------------------------------------------------------
# Figure  - URM by CBSA
# -----------------------------------------------------------------------------------------------
    
    #geomarket order
    orders_df %>% filter(race==1) %>% count(univ_name, order_num, order_title, race_filter, cbsa) %>% print(n=100)
    
    # look at 567426 list from UI-UC
    lists_orders_zip_hs_df %>%
      filter(order_num == '567426')
    
    
    #create aggregate race/ethnicity var figure
    geo_order <- '567426'
    
    poc_geo <- lists_orders_zip_hs_df %>%
      filter(univ_id == '145637' & ord_num == geo_order) %>% 
      mutate(
        ord_type = 'prospect',
        stu_race_cb = if_else(is.na(stu_race_cb), 999, unclass(stu_race_cb)),
        race = recode(
          stu_race_cb,
          `0` = 'noresponse',
          `1` = 'amerindian',
          `2` = 'asian',
          `3` = 'black',
          `4` = 'hispanic',
          `8` = 'nativehawaii',
          `9` = 'white',
          `12` = 'tworaces',
          `999` = 'unknown'
        )
      ) %>% 
      select(zip_cbsa_1, zip_cbsatitle_1, ord_num, ord_type, stu_race_cb, race, ends_with('_common'), hs_school_control, hs_ncessch, stu_zip_code) %>% 
      left_join(acs_income_zip, by = c('stu_zip_code' = 'zip_code')) %>% 
      rename(
        cbsa_code = zip_cbsa_1,
        cbsa_name = zip_cbsatitle_1,
        control = hs_school_control
      )
    
    poc_geo %>% 
      count(cbsa_code)
    
    poc_geo %>% 
      count(cbsa_code, race)
    
    poc_cb_geo <- poc_geo %>% 
      group_by(cbsa_code, cbsa_name, race) %>% 
      summarise(
        count = n()
      ) %>% 
      ungroup() %>% 
      group_by(cbsa_code, cbsa_name) %>% 
      mutate(
        pct = count / sum(count, na.rm = T)
      ) %>% 
      ungroup()
    
    poc_common_geo <- poc_geo %>% 
      select(cbsa_code, cbsa_name, stu_white_common, stu_asian_common, stu_black_common, stu_is_hisp_common, stu_american_indian_common, stu_native_hawaiian_common) %>% 
      pivot_longer(
        cols = -c(cbsa_code, cbsa_name),
        names_pattern = 'stu_(\\w+)_common',
        names_to = 'race',
        values_to = 'count'
      ) %>% 
      group_by(cbsa_code, cbsa_name, race) %>% 
      summarise(
        count = sum(count, na.rm = T)
      ) %>% 
      ungroup() %>% 
      left_join(
        poc_geo %>% count(cbsa_code),
        by = 'cbsa_code'
      ) %>% 
      mutate(
        pct = count / n
      )
    
    #purchases prospects by metros
    poc_metro_income_geo <- acs_income_metro %>% 
      mutate(
        ord_type = 'metro'
      ) %>% 
      rename(
        income_2564 = medincome_2564
      ) %>% 
      select(cbsa_code, cbsa_name, ord_type, income_2564)
    
    poc_metro_pubhs_geo <- ccd %>%
      filter(g12 >= 10, is_virtual == 0, updated_status %in% c('1', '3', '8')) %>% 
      select(cbsa_1, cbsatitle_1, ncessch, total_students, total_white, total_asian, total_black, total_hispanic, total_amerindian, total_nativehawaii, total_tworaces, total_unknown, lzip) %>% 
      left_join(acs_income_zip, by = c('lzip' = 'zip_code')) %>% 
      left_join(poc_geo %>% count(hs_ncessch), by = c('ncessch' = 'hs_ncessch')) %>% 
      mutate(
        n_cat = case_when(
          is.na(n) ~ 'zero',
          n <= 5 ~ '1-5',
          T ~ '6+'
        ),
        control = 'public'
      ) %>% 
      select(-lzip)
    
    poc_metro_privhs_geo <- pss %>% 
      filter(total_12 >= 10) %>% 
      select(cbsa_1, cbsatitle_1, ncessch, total_students, total_white, total_asian, total_black, total_hispanic, total_amerindian, total_nativehawaii, total_tworaces, zip_code) %>% 
      left_join(acs_income_zip, by = 'zip_code') %>% 
      left_join(poc_geo %>% count(hs_ncessch), by = c('ncessch' = 'hs_ncessch')) %>% 
      mutate(
        n_cat = case_when(
          is.na(n) ~ 'zero',
          T ~ '1+'
        ),
        control = 'private'
      ) %>% 
      select(-zip_code)
    
    poc_metro_pubprivhs_geo <- poc_metro_pubhs_geo %>% 
      bind_rows(poc_metro_privhs_geo) %>% 
      rename(
        cbsa_code = cbsa_1,
        cbsa_name = cbsatitle_1
      )
    
    poc_metro_hs_geo <- poc_metro_pubprivhs_geo %>% 
      select(cbsa_code, cbsa_name, control, ncessch, total_students) %>% 
      mutate(
        ord_type = 'metro'
      ) %>% 
      group_by(cbsa_code, cbsa_name, ord_type, control) %>% 
      summarise(
        count = sum(total_students)
      ) %>% 
      mutate(
        pct = count / sum(count, na.rm = T)
      ) %>% 
      ungroup() 
    
    poc_hs_geo <- poc_geo %>% 
      filter(hs_ncessch %in% poc_metro_pubprivhs_geo$ncessch) %>% 
      group_by(cbsa_code, cbsa_name, ord_type, control) %>% 
      summarise(
        count = n()
      ) %>% 
      filter(!is.na(control)) %>% 
      mutate(
        pct = count / sum(count, na.rm = T)
      ) %>% 
      ungroup() %>% 
      bind_rows(poc_metro_hs_geo)
    
    poc_race_geo <- poc_metro_pubprivhs_geo %>% 
      select(cbsa_code, cbsa_name, control, ncessch, n_cat, n, total_white, total_asian, total_black, total_hispanic, total_amerindian, total_nativehawaii, total_tworaces, total_unknown) %>% 
      mutate(
        ord_type = str_c(control, n_cat, sep = '_')
      ) %>% 
      select(-control, -n_cat) %>% 
      pivot_longer(
        cols = starts_with('total_'),
        names_prefix = 'total_',
        names_to = 'race',
        values_to = 'count'
      ) %>% 
      mutate(
        n = if_else(is.na(n), 0L, n),
        count = if_else(is.na(count), 0L, count)
      ) %>% 
      group_by(cbsa_code, cbsa_name, ord_type, ncessch) %>%
      mutate(
        pct = count / sum(count, na.rm = T),
        pct = if_else(is.nan(pct), NA_real_, pct)
      ) %>%
      ungroup() %>%
      group_by(cbsa_code, cbsa_name, ord_type, race) %>%
      summarise(
        num_hs = n(),
        num_prospects = sum(n, na.rm = T),
        count = sum(count, na.rm = T), 
        pct = mean(pct, na.rm = T)
      ) %>% 
      ungroup()
    
    poc_race_geo %>%  # 0 for unknown race so no effect on pct if removed
      filter(race == 'unknown')
    
    poc_income_hs_geo <- poc_metro_pubprivhs_geo %>% 
      select(cbsa_code, cbsa_name, control, ncessch, n_cat, n, medincome_2564) %>% 
      group_by(cbsa_code, cbsa_name, control, n_cat) %>% 
      summarise(
        num_hs = n(),
        num_prospects = sum(n, na.rm = T),
        income_2564 = mean(medincome_2564, na.rm = T)
      ) %>% 
      ungroup() %>% 
      mutate(
        ord_type = str_c(control, n_cat, sep = '_')
      ) %>% 
      select(cbsa_code, cbsa_name, ord_type, num_hs, num_prospects, income_2564)
    
    poc_income_geo <- poc_geo %>% 
      filter(hs_ncessch %in% poc_metro_pubprivhs_geo$ncessch) %>% 
      group_by(cbsa_code, cbsa_name, ord_type) %>% 
      summarise(
        income_2564 = mean(medincome_2564, na.rm = T)
      ) %>% 
      ungroup() %>% 
      bind_rows(poc_metro_income_geo) %>% 
      bind_rows(poc_income_hs_geo)
    
    # Check totals for income and race are consistent
    poc_race_geo %>% group_by(cbsa_code, ord_type) %>% summarise(sum(num_hs) / 8, sum(num_prospects / 8)) %>% print(n=2000)
    poc_income %>% filter(!is.na(num_hs)) %>% group_by(cbsa_code, ord_type) %>% summarise(sum(num_hs), sum(num_prospects))

    
# -----------------------------------------------------------------------------------------------
  # Figure  - URM by SAT/PSA for IN-STATE: Purchased profiles for students of color by metro (HS type, average income, racial composition)
# -----------------------------------------------------------------------------------------------
    
    # in state-by PSAT/SAT
    orders_df %>% filter(order_num=="560002") %>% count(univ_name, order_num, order_title, race_filter) %>% print(n=100)
    
    # look at 449322 list from Texas A&M
    URM_instate <-lists_orders_zip_hs_df %>%
      filter(ord_num == '560002') 
    
    URM_instate <- URM_instate %>%
      select(zip_cbsa_1, zip_cbsatitle_1, ord_num, stu_race_cb, ends_with('_common'), hs_school_control, hs_ncessch, stu_zip_code) %>% 
      left_join(acs_income_zip, by = c('stu_zip_code' = 'zip_code')) %>% 
      rename(
        cbsa_code = zip_cbsa_1,
        cbsa_name = zip_cbsatitle_1,
        control = hs_school_control
      ) 
    
    #top metros: Los Angeles-Long Beach-Anaheim, CA (31080); San Diego-Carlsbad, CA (41740); Riverside-San Bernardino-Ontario, CA (40140)
    URM_instate %>%
      group_by(cbsa_name, cbsa_code) %>% 
      summarise(
        count = n()) %>% arrange(-count)
    
    
    
    #create aggregate race/ethnicity var figure
    poc_order <- '560002'
    poc_metros <- c('31080', '41740', '40140')
    
    poc <- lists_orders_zip_hs_df %>%
      filter(univ_id == '110680', zip_cbsa_1 %in% poc_metros, ord_num == poc_order) %>% 
      mutate(
        ord_type = 'prospect',
        stu_race_cb = if_else(is.na(stu_race_cb), 999, unclass(stu_race_cb)),
        race = recode(
          stu_race_cb,
          `0` = 'noresponse',
          `1` = 'amerindian',
          `2` = 'asian',
          `3` = 'black',
          `4` = 'hispanic',
          `8` = 'nativehawaii',
          `9` = 'white',
          `12` = 'tworaces',
          `999` = 'unknown'
        )
      ) %>% 
      select(zip_cbsa_1, zip_cbsatitle_1, ord_num, ord_type, stu_race_cb, race, ends_with('_common'), hs_school_control, hs_ncessch, stu_zip_code) %>% 
      left_join(acs_income_zip, by = c('stu_zip_code' = 'zip_code')) %>% 
      rename(
        cbsa_code = zip_cbsa_1,
        cbsa_name = zip_cbsatitle_1,
        control = hs_school_control
      )
    
    poc %>%   count(cbsa_code,cbsa_name)
    
    poc %>% 
      count(cbsa_code, race)
    
    poc_cb <- poc %>% 
      group_by(cbsa_code, cbsa_name, race) %>% 
      summarise(
        count = n()
      ) %>% 
      ungroup() %>% 
      group_by(cbsa_code, cbsa_name) %>% 
      mutate(
        pct = count / sum(count, na.rm = T)
      ) %>% 
      ungroup()
    
    poc_common <- poc %>% 
      select(cbsa_code, cbsa_name, stu_white_common, stu_asian_common, stu_black_common, stu_is_hisp_common, stu_american_indian_common, stu_native_hawaiian_common) %>% 
      pivot_longer(
        cols = -c(cbsa_code, cbsa_name),
        names_pattern = 'stu_(\\w+)_common',
        names_to = 'race',
        values_to = 'count'
      ) %>% 
      group_by(cbsa_code, cbsa_name, race) %>% 
      summarise(
        count = sum(count, na.rm = T)
      ) %>% 
      ungroup() %>% 
      left_join(
        poc %>% count(cbsa_code),
        by = 'cbsa_code'
      ) %>% 
      mutate(
        pct = count / n
      )
    
    #purchases prospects by metros
    poc_metro_income <- acs_income_metro %>% 
      filter(cbsa_code %in% poc_metros) %>% 
      mutate(
        ord_type = 'metro'
      ) %>% 
      rename(
        income_2564 = medincome_2564
      ) %>% 
      select(cbsa_code, cbsa_name, ord_type, income_2564)
    
    poc_metro_pubhs <- ccd %>%
      filter(g12 >= 10, is_virtual == 0, updated_status %in% c('1', '3', '8'), cbsa_1 %in% poc_metros) %>% 
      select(cbsa_1, cbsatitle_1, ncessch, total_students, total_white, total_asian, total_black, total_hispanic, total_amerindian, total_nativehawaii, total_tworaces, total_unknown, lzip) %>% 
      left_join(acs_income_zip, by = c('lzip' = 'zip_code')) %>% 
      left_join(poc %>% count(hs_ncessch), by = c('ncessch' = 'hs_ncessch')) %>% 
      mutate(
        n_cat = case_when(
          is.na(n) ~ 'zero',
          n <= 5 ~ '1-5',
          T ~ '6+'
        ),
        control = 'public'
      ) %>% 
      select(-lzip)
    
    poc_metro_privhs <- pss %>% 
      filter(total_12 >= 10, cbsa_1 %in% poc_metros) %>% 
      select(cbsa_1, cbsatitle_1, ncessch, total_students, total_white, total_asian, total_black, total_hispanic, total_amerindian, total_nativehawaii, total_tworaces, zip_code) %>% 
      left_join(acs_income_zip, by = 'zip_code') %>% 
      left_join(poc %>% count(hs_ncessch), by = c('ncessch' = 'hs_ncessch')) %>% 
      mutate(
        n_cat = case_when(
          is.na(n) ~ 'zero',
          T ~ '1+'
        ),
        control = 'private'
      ) %>% 
      select(-zip_code)
    
    poc_metro_pubprivhs <- poc_metro_pubhs %>% 
      bind_rows(poc_metro_privhs) %>% 
      rename(
        cbsa_code = cbsa_1,
        cbsa_name = cbsatitle_1
      )
    
    poc_metro_hs <- poc_metro_pubprivhs %>% 
      select(cbsa_code, cbsa_name, control, ncessch, total_students) %>% 
      mutate(
        ord_type = 'metro'
      ) %>% 
      group_by(cbsa_code, cbsa_name, ord_type, control) %>% 
      summarise(
        count = sum(total_students)
      ) %>% 
      mutate(
        pct = count / sum(count, na.rm = T)
      ) %>% 
      ungroup() 
    
    poc_hs <- poc %>% 
      filter(hs_ncessch %in% poc_metro_pubprivhs$ncessch) %>% 
      group_by(cbsa_code, cbsa_name, ord_type, control) %>% 
      summarise(
        count = n()
      ) %>% 
      filter(!is.na(control)) %>% 
      mutate(
        pct = count / sum(count, na.rm = T)
      ) %>% 
      ungroup() %>% 
      bind_rows(poc_metro_hs)
    
    poc_race <- poc_metro_pubprivhs %>% 
      select(cbsa_code, cbsa_name, control, ncessch, n_cat, n, total_white, total_asian, total_black, total_hispanic, total_amerindian, total_nativehawaii, total_tworaces, total_unknown) %>% 
      mutate(
        ord_type = str_c(control, n_cat, sep = '_')
      ) %>% 
      select(-control, -n_cat) %>% 
      pivot_longer(
        cols = starts_with('total_'),
        names_prefix = 'total_',
        names_to = 'race',
        values_to = 'count'
      ) %>% 
      mutate(
        n = if_else(is.na(n), 0L, n),
        count = if_else(is.na(count), 0L, count)
      ) %>% 
      group_by(cbsa_code, cbsa_name, ord_type, ncessch) %>%
      mutate(
        pct = count / sum(count, na.rm = T),
        pct = if_else(is.nan(pct), NA_real_, pct)
      ) %>%
      ungroup() %>%
      group_by(cbsa_code, cbsa_name, ord_type, race) %>%
      summarise(
        num_hs = n(),
        num_prospects = sum(n, na.rm = T),
        count = sum(count, na.rm = T), 
        pct = mean(pct, na.rm = T)
      ) %>% 
      ungroup()
    
    poc_race %>%  # 0 for unknown race so no effect on pct if removed
      filter(race == 'unknown')
    
    poc_income_hs <- poc_metro_pubprivhs %>% 
      select(cbsa_code, cbsa_name, control, ncessch, n_cat, n, medincome_2564) %>% 
      group_by(cbsa_code, cbsa_name, control, n_cat) %>% 
      summarise(
        num_hs = n(),
        num_prospects = sum(n, na.rm = T),
        income_2564 = mean(medincome_2564, na.rm = T)
      ) %>% 
      ungroup() %>% 
      mutate(
        ord_type = str_c(control, n_cat, sep = '_')
      ) %>% 
      select(cbsa_code, cbsa_name, ord_type, num_hs, num_prospects, income_2564)
    
    poc_income <- poc %>% 
      filter(hs_ncessch %in% poc_metro_pubprivhs$ncessch) %>% 
      group_by(cbsa_code, cbsa_name, ord_type) %>% 
      summarise(
        income_2564 = mean(medincome_2564, na.rm = T)
      ) %>% 
      ungroup() %>% 
      bind_rows(poc_metro_income) %>% 
      bind_rows(poc_income_hs)
    
    # Check totals for income and race are consistent
    poc_race %>% group_by(cbsa_code, ord_type) %>% summarise(sum(num_hs) / 8, sum(num_prospects / 8))
    poc_income %>% filter(!is.na(num_hs)) %>% group_by(cbsa_code, ord_type) %>% summarise(sum(num_hs), sum(num_prospects))
    
    URM_instate %>%   count(cbsa_code,cbsa_name)
    
    #Export DF for figures
    poc_URM_instate <- poc
    poc_cb_URM_instate <- poc_cb
    poc_common_URM_instate <- poc_common
    poc_hs_URM_instate <- poc_hs
    poc_income_URM_instate <- poc_income
    poc_race_URM_instate <- poc_race
    save(poc_URM_instate, poc_cb_URM_instate, poc_common_URM_instate, poc_hs_URM_instate, poc_income_URM_instate, poc_race_URM_instate, file = file.path('/Users/karinasalazar/Library/CloudStorage/Dropbox/recruiting-m3-ProximityToWhiteness/data/map_data_URM_instate.RData'))
    
    
        
    
# -----------------------------------------------------------------------------------------------
# Figure  - National Merit Scholars
# -----------------------------------------------------------------------------------------------
    
    #national hispanic scholars order
    orders_df %>% filter(race==1 & national_recognition_programs==1) %>% count(univ_name, order_num, order_title, race_filter) %>% print(n=100)
    
    # look at 449322 list from Texas A&M
    nhmerit <-lists_orders_zip_hs_df %>%
      filter(ord_num == '449322') 
    
    nhmerit <- nhmerit %>%
      select(zip_cbsa_1, zip_cbsatitle_1, ord_num, stu_race_cb, ends_with('_common'), hs_school_control, hs_ncessch, stu_zip_code) %>% 
      left_join(acs_income_zip, by = c('stu_zip_code' = 'zip_code')) %>% 
      rename(
        cbsa_code = zip_cbsa_1,
        cbsa_name = zip_cbsatitle_1,
        control = hs_school_control
      ) 
    
    #top metros: Los Angeles-Long Beach-Anaheim, CA; San Francisco-Oakland-Hayward, CA; Denver-Aurora-Lakewood, CO 
    nhmerit %>%
      group_by(cbsa_name, cbsa_code) %>% 
      summarise(
        count = n()) %>% arrange(-count)
    
 
    
    
    
    #create aggregate race/ethnicity var figure
    poc_order <- '449322'
    poc_metros <- c('31080', '41860', '19740')
    
    poc <- lists_orders_zip_hs_df %>%
      filter(univ_id == '228723', zip_cbsa_1 %in% poc_metros, ord_num == poc_order) %>% 
      mutate(
        ord_type = 'prospect',
        stu_race_cb = if_else(is.na(stu_race_cb), 999, unclass(stu_race_cb)),
        race = recode(
          stu_race_cb,
          `0` = 'noresponse',
          `1` = 'amerindian',
          `2` = 'asian',
          `3` = 'black',
          `4` = 'hispanic',
          `8` = 'nativehawaii',
          `9` = 'white',
          `12` = 'tworaces',
          `999` = 'unknown'
        )
      ) %>% 
      select(zip_cbsa_1, zip_cbsatitle_1, ord_num, ord_type, stu_race_cb, race, ends_with('_common'), hs_school_control, hs_ncessch, stu_zip_code) %>% 
      left_join(acs_income_zip, by = c('stu_zip_code' = 'zip_code')) %>% 
      rename(
        cbsa_code = zip_cbsa_1,
        cbsa_name = zip_cbsatitle_1,
        control = hs_school_control
      )
    
    poc %>%   count(cbsa_code,cbsa_name)
    
    poc %>% 
      count(cbsa_code, race)
    
    poc_cb <- poc %>% 
      group_by(cbsa_code, cbsa_name, race) %>% 
      summarise(
        count = n()
      ) %>% 
      ungroup() %>% 
      group_by(cbsa_code, cbsa_name) %>% 
      mutate(
        pct = count / sum(count, na.rm = T)
      ) %>% 
      ungroup()
    
    poc_common <- poc %>% 
      select(cbsa_code, cbsa_name, stu_white_common, stu_asian_common, stu_black_common, stu_is_hisp_common, stu_american_indian_common, stu_native_hawaiian_common) %>% 
      pivot_longer(
        cols = -c(cbsa_code, cbsa_name),
        names_pattern = 'stu_(\\w+)_common',
        names_to = 'race',
        values_to = 'count'
      ) %>% 
      group_by(cbsa_code, cbsa_name, race) %>% 
      summarise(
        count = sum(count, na.rm = T)
      ) %>% 
      ungroup() %>% 
      left_join(
        poc %>% count(cbsa_code),
        by = 'cbsa_code'
      ) %>% 
      mutate(
        pct = count / n
      )
    
    #purchases prospects by metros
    poc_metro_income <- acs_income_metro %>% 
      filter(cbsa_code %in% poc_metros) %>% 
      mutate(
        ord_type = 'metro'
      ) %>% 
      rename(
        income_2564 = medincome_2564
      ) %>% 
      select(cbsa_code, cbsa_name, ord_type, income_2564)
    
    poc_metro_pubhs <- ccd %>%
      filter(g12 >= 10, is_virtual == 0, updated_status %in% c('1', '3', '8'), cbsa_1 %in% poc_metros) %>% 
      select(cbsa_1, cbsatitle_1, ncessch, total_students, total_white, total_asian, total_black, total_hispanic, total_amerindian, total_nativehawaii, total_tworaces, total_unknown, lzip) %>% 
      left_join(acs_income_zip, by = c('lzip' = 'zip_code')) %>% 
      left_join(poc %>% count(hs_ncessch), by = c('ncessch' = 'hs_ncessch')) %>% 
      mutate(
        n_cat = case_when(
          is.na(n) ~ 'zero',
          n <= 5 ~ '1-5',
          T ~ '6+'
        ),
        control = 'public'
      ) %>% 
      select(-lzip)
    
    poc_metro_privhs <- pss %>% 
      filter(total_12 >= 10, cbsa_1 %in% poc_metros) %>% 
      select(cbsa_1, cbsatitle_1, ncessch, total_students, total_white, total_asian, total_black, total_hispanic, total_amerindian, total_nativehawaii, total_tworaces, zip_code) %>% 
      left_join(acs_income_zip, by = 'zip_code') %>% 
      left_join(poc %>% count(hs_ncessch), by = c('ncessch' = 'hs_ncessch')) %>% 
      mutate(
        n_cat = case_when(
          is.na(n) ~ 'zero',
          T ~ '1+'
        ),
        control = 'private'
      ) %>% 
      select(-zip_code)
    
    poc_metro_pubprivhs <- poc_metro_pubhs %>% 
      bind_rows(poc_metro_privhs) %>% 
      rename(
        cbsa_code = cbsa_1,
        cbsa_name = cbsatitle_1
      )
    
    poc_metro_hs <- poc_metro_pubprivhs %>% 
      select(cbsa_code, cbsa_name, control, ncessch, total_students) %>% 
      mutate(
        ord_type = 'metro'
      ) %>% 
      group_by(cbsa_code, cbsa_name, ord_type, control) %>% 
      summarise(
        count = sum(total_students)
      ) %>% 
      mutate(
        pct = count / sum(count, na.rm = T)
      ) %>% 
      ungroup() 
    
    poc_hs <- poc %>% 
      filter(hs_ncessch %in% poc_metro_pubprivhs$ncessch) %>% 
      group_by(cbsa_code, cbsa_name, ord_type, control) %>% 
      summarise(
        count = n()
      ) %>% 
      filter(!is.na(control)) %>% 
      mutate(
        pct = count / sum(count, na.rm = T)
      ) %>% 
      ungroup() %>% 
      bind_rows(poc_metro_hs)
    
    poc_race <- poc_metro_pubprivhs %>% 
      select(cbsa_code, cbsa_name, control, ncessch, n_cat, n, total_white, total_asian, total_black, total_hispanic, total_amerindian, total_nativehawaii, total_tworaces, total_unknown) %>% 
      mutate(
        ord_type = str_c(control, n_cat, sep = '_')
      ) %>% 
      select(-control, -n_cat) %>% 
      pivot_longer(
        cols = starts_with('total_'),
        names_prefix = 'total_',
        names_to = 'race',
        values_to = 'count'
      ) %>% 
      mutate(
        n = if_else(is.na(n), 0L, n),
        count = if_else(is.na(count), 0L, count)
      ) %>% 
      group_by(cbsa_code, cbsa_name, ord_type, ncessch) %>%
      mutate(
        pct = count / sum(count, na.rm = T),
        pct = if_else(is.nan(pct), NA_real_, pct)
      ) %>%
      ungroup() %>%
      group_by(cbsa_code, cbsa_name, ord_type, race) %>%
      summarise(
        num_hs = n(),
        num_prospects = sum(n, na.rm = T),
        count = sum(count, na.rm = T), 
        pct = mean(pct, na.rm = T)
      ) %>% 
      ungroup()
    
    poc_race %>%  # 0 for unknown race so no effect on pct if removed
      filter(race == 'unknown')
    
    poc_income_hs <- poc_metro_pubprivhs %>% 
      select(cbsa_code, cbsa_name, control, ncessch, n_cat, n, medincome_2564) %>% 
      group_by(cbsa_code, cbsa_name, control, n_cat) %>% 
      summarise(
        num_hs = n(),
        num_prospects = sum(n, na.rm = T),
        income_2564 = mean(medincome_2564, na.rm = T)
      ) %>% 
      ungroup() %>% 
      mutate(
        ord_type = str_c(control, n_cat, sep = '_')
      ) %>% 
      select(cbsa_code, cbsa_name, ord_type, num_hs, num_prospects, income_2564)
    
    poc_income <- poc %>% 
      filter(hs_ncessch %in% poc_metro_pubprivhs$ncessch) %>% 
      group_by(cbsa_code, cbsa_name, ord_type) %>% 
      summarise(
        income_2564 = mean(medincome_2564, na.rm = T)
      ) %>% 
      ungroup() %>% 
      bind_rows(poc_metro_income) %>% 
      bind_rows(poc_income_hs)
    
    # Check totals for income and race are consistent
    poc_race %>% group_by(cbsa_code, ord_type) %>% summarise(sum(num_hs) / 8, sum(num_prospects / 8))
    poc_income %>% filter(!is.na(num_hs)) %>% group_by(cbsa_code, ord_type) %>% summarise(sum(num_hs), sum(num_prospects))
    
    #Export DF for figures
    poc_nhms<- poc
    poc_cb_nhms <- poc_cb
    poc_common_nhms <- poc_common
    poc_hs_nhms <- poc_hs
    poc_income_nhms <- poc_income
    poc_race_nhms <- poc_race
    save(poc_cb_nhms, poc_common_nhms, poc_hs_nhms, poc_income_nhms, poc_race_nhms, file = file.path('/Users/karinasalazar/Library/CloudStorage/Dropbox/recruiting-m3-ProximityToWhiteness/tbl_fig_data_m3.RData'))
    
    
    
    
    
# -----------------------------------------------------------------------------------------------
# Figure  - URM by AP Exams
# -----------------------------------------------------------------------------------------------
    
    # out of state-Black students by AP exams
    orders_df %>% filter(order_num=="622315") %>% count(univ_name, order_num, order_title, race_filter) %>% print(n=100)
    
    # look at 449322 list from Texas A&M
    blackAP <-lists_orders_zip_hs_df %>%
      filter(ord_num == '622315') 
    
    blackAP <- blackAP %>%
      select(zip_cbsa_1, zip_cbsatitle_1, ord_num, stu_race_cb, ends_with('_common'), hs_school_control, hs_ncessch, stu_zip_code) %>% 
      left_join(acs_income_zip, by = c('stu_zip_code' = 'zip_code')) %>% 
      rename(
        cbsa_code = zip_cbsa_1,
        cbsa_name = zip_cbsatitle_1,
        control = hs_school_control
      ) 
    
    #top metros: New York-Newark-Jersey City, NY-NJ-PA (35620); Atlanta-Sandy Springs-Roswell, GA (12060); Dallas-Fort Worth-Arlington, TX (19100)
    blackAP %>%
      group_by(cbsa_name, cbsa_code) %>% 
      summarise(
        count = n()) %>% arrange(-count)
    
    
    
    #create aggregate race/ethnicity var figure
    poc_order <- '622315'
    poc_metros <- c('35620', '12060', '33100', '19100')
    
    poc <- lists_orders_zip_hs_df %>%
      filter(univ_id == '110644', zip_cbsa_1 %in% poc_metros, ord_num == poc_order) %>% 
      mutate(
        ord_type = 'prospect',
        stu_race_cb = if_else(is.na(stu_race_cb), 999, unclass(stu_race_cb)),
        race = recode(
          stu_race_cb,
          `0` = 'noresponse',
          `1` = 'amerindian',
          `2` = 'asian',
          `3` = 'black',
          `4` = 'hispanic',
          `8` = 'nativehawaii',
          `9` = 'white',
          `12` = 'tworaces',
          `999` = 'unknown'
        )
      ) %>% 
      select(zip_cbsa_1, zip_cbsatitle_1, ord_num, ord_type, stu_race_cb, race, ends_with('_common'), hs_school_control, hs_ncessch, stu_zip_code) %>% 
      left_join(acs_income_zip, by = c('stu_zip_code' = 'zip_code')) %>% 
      rename(
        cbsa_code = zip_cbsa_1,
        cbsa_name = zip_cbsatitle_1,
        control = hs_school_control
      )
    
    poc %>%   count(cbsa_code,cbsa_name)
    
    poc %>% 
      count(cbsa_code, race)
    
    poc_cb <- poc %>% 
      group_by(cbsa_code, cbsa_name, race) %>% 
      summarise(
        count = n()
      ) %>% 
      ungroup() %>% 
      group_by(cbsa_code, cbsa_name) %>% 
      mutate(
        pct = count / sum(count, na.rm = T)
      ) %>% 
      ungroup()
    
    poc_common <- poc %>% 
      select(cbsa_code, cbsa_name, stu_white_common, stu_asian_common, stu_black_common, stu_is_hisp_common, stu_american_indian_common, stu_native_hawaiian_common) %>% 
      pivot_longer(
        cols = -c(cbsa_code, cbsa_name),
        names_pattern = 'stu_(\\w+)_common',
        names_to = 'race',
        values_to = 'count'
      ) %>% 
      group_by(cbsa_code, cbsa_name, race) %>% 
      summarise(
        count = sum(count, na.rm = T)
      ) %>% 
      ungroup() %>% 
      left_join(
        poc %>% count(cbsa_code),
        by = 'cbsa_code'
      ) %>% 
      mutate(
        pct = count / n
      )
    
    #purchases prospects by metros
    poc_metro_income <- acs_income_metro %>% 
      filter(cbsa_code %in% poc_metros) %>% 
      mutate(
        ord_type = 'metro'
      ) %>% 
      rename(
        income_2564 = medincome_2564
      ) %>% 
      select(cbsa_code, cbsa_name, ord_type, income_2564)
    
    poc_metro_pubhs <- ccd %>%
      filter(g12 >= 10, is_virtual == 0, updated_status %in% c('1', '3', '8'), cbsa_1 %in% poc_metros) %>% 
      select(cbsa_1, cbsatitle_1, ncessch, total_students, total_white, total_asian, total_black, total_hispanic, total_amerindian, total_nativehawaii, total_tworaces, total_unknown, lzip) %>% 
      left_join(acs_income_zip, by = c('lzip' = 'zip_code')) %>% 
      left_join(poc %>% count(hs_ncessch), by = c('ncessch' = 'hs_ncessch')) %>% 
      mutate(
        n_cat = case_when(
          is.na(n) ~ 'zero',
          n <= 5 ~ '1-5',
          T ~ '6+'
        ),
        control = 'public'
      ) %>% 
      select(-lzip)
    
    poc_metro_privhs <- pss %>% 
      filter(total_12 >= 10, cbsa_1 %in% poc_metros) %>% 
      select(cbsa_1, cbsatitle_1, ncessch, total_students, total_white, total_asian, total_black, total_hispanic, total_amerindian, total_nativehawaii, total_tworaces, zip_code) %>% 
      left_join(acs_income_zip, by = 'zip_code') %>% 
      left_join(poc %>% count(hs_ncessch), by = c('ncessch' = 'hs_ncessch')) %>% 
      mutate(
        n_cat = case_when(
          is.na(n) ~ 'zero',
          T ~ '1+'
        ),
        control = 'private'
      ) %>% 
      select(-zip_code)
    
    poc_metro_pubprivhs <- poc_metro_pubhs %>% 
      bind_rows(poc_metro_privhs) %>% 
      rename(
        cbsa_code = cbsa_1,
        cbsa_name = cbsatitle_1
      )
    
    poc_metro_hs <- poc_metro_pubprivhs %>% 
      select(cbsa_code, cbsa_name, control, ncessch, total_students) %>% 
      mutate(
        ord_type = 'metro'
      ) %>% 
      group_by(cbsa_code, cbsa_name, ord_type, control) %>% 
      summarise(
        count = sum(total_students)
      ) %>% 
      mutate(
        pct = count / sum(count, na.rm = T)
      ) %>% 
      ungroup() 
    
    poc_hs <- poc %>% 
      filter(hs_ncessch %in% poc_metro_pubprivhs$ncessch) %>% 
      group_by(cbsa_code, cbsa_name, ord_type, control) %>% 
      summarise(
        count = n()
      ) %>% 
      filter(!is.na(control)) %>% 
      mutate(
        pct = count / sum(count, na.rm = T)
      ) %>% 
      ungroup() %>% 
      bind_rows(poc_metro_hs)
    
    poc_race <- poc_metro_pubprivhs %>% 
      select(cbsa_code, cbsa_name, control, ncessch, n_cat, n, total_white, total_asian, total_black, total_hispanic, total_amerindian, total_nativehawaii, total_tworaces, total_unknown) %>% 
      mutate(
        ord_type = str_c(control, n_cat, sep = '_')
      ) %>% 
      select(-control, -n_cat) %>% 
      pivot_longer(
        cols = starts_with('total_'),
        names_prefix = 'total_',
        names_to = 'race',
        values_to = 'count'
      ) %>% 
      mutate(
        n = if_else(is.na(n), 0L, n),
        count = if_else(is.na(count), 0L, count)
      ) %>% 
      group_by(cbsa_code, cbsa_name, ord_type, ncessch) %>%
      mutate(
        pct = count / sum(count, na.rm = T),
        pct = if_else(is.nan(pct), NA_real_, pct)
      ) %>%
      ungroup() %>%
      group_by(cbsa_code, cbsa_name, ord_type, race) %>%
      summarise(
        num_hs = n(),
        num_prospects = sum(n, na.rm = T),
        count = sum(count, na.rm = T), 
        pct = mean(pct, na.rm = T)
      ) %>% 
      ungroup()
    
    poc_race %>%  # 0 for unknown race so no effect on pct if removed
      filter(race == 'unknown')
    
    poc_income_hs <- poc_metro_pubprivhs %>% 
      select(cbsa_code, cbsa_name, control, ncessch, n_cat, n, medincome_2564) %>% 
      group_by(cbsa_code, cbsa_name, control, n_cat) %>% 
      summarise(
        num_hs = n(),
        num_prospects = sum(n, na.rm = T),
        income_2564 = mean(medincome_2564, na.rm = T)
      ) %>% 
      ungroup() %>% 
      mutate(
        ord_type = str_c(control, n_cat, sep = '_')
      ) %>% 
      select(cbsa_code, cbsa_name, ord_type, num_hs, num_prospects, income_2564)
    
    poc_income <- poc %>% 
      filter(hs_ncessch %in% poc_metro_pubprivhs$ncessch) %>% 
      group_by(cbsa_code, cbsa_name, ord_type) %>% 
      summarise(
        income_2564 = mean(medincome_2564, na.rm = T)
      ) %>% 
      ungroup() %>% 
      bind_rows(poc_metro_income) %>% 
      bind_rows(poc_income_hs)
    
    # Check totals for income and race are consistent
    poc_race %>% group_by(cbsa_code, ord_type) %>% summarise(sum(num_hs) / 8, sum(num_prospects / 8))
    poc_income %>% filter(!is.na(num_hs)) %>% group_by(cbsa_code, ord_type) %>% summarise(sum(num_hs), sum(num_prospects))
    
    poc_BlackAP %>%   count(cbsa_code,cbsa_name)
    
    #Export DF for figures
    poc_BlackAP <- poc
    poc_cb_BlackAP <- poc_cb
    poc_common_BlackAP <- poc_common
    poc_hs_BlackAP <- poc_hs
    poc_income_BlackAP <- poc_income
    poc_race_BlackAP <- poc_race
    save(poc_BlackAP, poc_cb_BlackAP, poc_common_BlackAP, poc_hs_BlackAP, poc_income_BlackAP, poc_race_BlackAP, file = file.path('/Users/karinasalazar/Library/CloudStorage/Dropbox/recruiting-m3-ProximityToWhiteness/data/map_data_BlackAP_4metros.RData'))
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
################### GET SOME TOTALS FOR MARKET REPORT
    
    #UC SAN DIEGO TOTALS IN 2020
    ucsd <- orders_df %>% filter(univ_id=="110680")
    ucsd %>% count(order_num)
    ucsd %>% count(date_start)
    ucsd %>%count(order_num, gender) %>% print(n=100)
    
    ucsd %>% group_by(date_start) %>% summarise(total_prosp = sum(num_students, na.rm=T))
    
    
    #Illinois Springfield TOTALS IN 2020
    UIs <- orders_df %>% filter(univ_id=="148654")
    UIs %>% group_by(date_start) %>% summarise(total_prosp = sum(num_students, na.rm=T))
        
    
    #Texas A&M University-College Station TOTALS IN 2020
    tam <- orders_df %>% filter(univ_id=="228723")
    tam %>% group_by(date_start) %>% summarise(total_prosp = sum(num_students, na.rm=T))
    
    
    
################### CHECKING URBANA CHAMPAIGN
  
  #BELOW USES RAW DATA TO UNDERSTAND HOW OZAN CREATED RACE/ETHNICITY COMMON VAR
    # checking raw data; does not include Ozan's maniputlations 
    #load(url('https://github.com/mpatricia01/public_requests_eda/raw/main/data/combined_data.RData'))
    
    # create list_df of just urbana
    urbana <- lists_df %>% filter(univ_id==145637)
    
    # Urbana says that 74% of respondents did not fill out the race/ethnicity question; true NAs
    # Ozan's race var creation (line 587 in create_combined. R) assumes if is_hispanic_orgin is NA then is_hispanic_orgin==0
    urbana %>%
      group_by(is_hispanic_origin) %>%
      summarise(n = n()) %>%
      mutate(freq = n / sum(n))
    
    # how many missing race & ethnicity?
    urbana %>%
      group_by(is_hispanic_origin, race) %>%
      summarise(n = n()) %>%
      mutate(freq = n / sum(n)) %>% print(n=200)
    
    
  # compare to other research univs; UC Davis (47% Hispanic)
    lists_df %>% filter(univ_id==110644 | univ_id==145637) %>%
      group_by(univ_name, race_cb) %>%
      summarise(n = n()) %>%
      mutate(pct = (n / sum(n))*100) %>% print(n=200)
    
  # compare to other research univs; UC San Diego (26% Hispanic)
    lists_df %>% filter(univ_id==110680 | univ_id==145637) %>%
      group_by(univ_name, race_cb) %>%
      summarise(n = n()) %>%
      mutate(pct = (n / sum(n))*100) %>% print(n=200)
    
    # compare to other research univs; UI Chicago (17% Hispanic)
    lists_df %>% filter(univ_id==145600 | univ_id==145637) %>%
      group_by(univ_name, race_cb) %>%
      summarise(n = n()) %>%
      mutate(pct = (n / sum(n))*100) %>% print(n=200)
    
    # compare to other research univs; Texas A&M CS (29% Hispanic)
    lists_df %>% filter(univ_id==228723 | univ_id==145637) %>%
      group_by(univ_name, race_cb) %>%
      summarise(n = n()) %>%
      mutate(pct = (n / sum(n))*100) %>% print(n=200)
    
    
    #looks across two similar orders in one metro
    
    
    
    
################### CREATING AND CLEANING OUT_OF_STATE & GENDER VARS NEEDED FOR FUNCTION
    
    # KS CHECKS
    # # non-res categories based on country , state (check for missingness)
    # lists_orders_zip_hs_df %>% count(stu_country) %>% print(n=200) #1245 missing countries
    # 
    # 
    # lists_orders_zip_hs_df %>% filter(stu_in_us==1 | is.na(stu_state)) %>% count(stu_country, stu_state) %>% print(n=200) #1245 missing countries
    # 
    #     #lots of foreign "states/cities" listed as US & US states with NA for country
    #     lists_orders_zip_hs_df <- lists_orders_zip_hs_df %>%
    #          mutate(stu_country = ifelse((stu_state=="Ankara" | stu_state=="Icerenkoy" | stu_state=="Istanbul" | stu_state=="Mudanya"), "turkey", stu_country),
    #                 stu_country = ifelse(stu_state=="Beijing", "china", stu_country), 
    #                 stu_country = ifelse((stu_state=="Central Singapore" | stu_state=="Singapore"), "singapore", stu_country),
    #                 stu_country = ifelse(stu_state=="CHINA", "china", stu_country),
    #                 stu_country = ifelse(stu_state=="Doha", "qatar", stu_country),
    #                 stu_country = ifelse(stu_state=="Karnataka", "india", stu_country),
    #                 stu_country = ifelse(stu_state=="Minas Gerais", "brazil", stu_country),
    #                 stu_country = ifelse((stu_state=="Taipei"|stu_state=="Taiwan"), "taiwan", stu_country),
    #                 stu_country = ifelse(stu_state=="VIC", "australia", stu_country),
    #                 stu_country = ifelse((stu_state=="CA"|stu_state=="IL" | stu_state=="TX"), "united states", stu_country)
    #                 )
    # 
    #     
    #     # 58,225 students with missing country & state [checking school state, city, zip]
    #     lists_orders_zip_hs_df %>% filter(is.na(stu_country) | is.na(stu_state)) %>% select(stu_country, stu_state, hs_state_code, stu_city, stu_zip) %>% print(n=200) 
    #     
    
    #non res categories based on OJ's stu_in_us var
    lists_orders_zip_hs_df <- lists_orders_zip_hs_df %>%
        mutate(stu_internat = ifelse(stu_in_us==1, 0, 1),
               stu_nonres = ifelse((stu_in_us==1 & univ_state==stu_state), 0, NA),
               stu_nonres = ifelse((stu_in_us==1 & univ_state!=stu_state), 1, stu_nonres))
    
    lists_orders_zip_hs_df %>% count(stu_in_us, stu_nonres)
    
    
    # consolidate gender/sex categories
    
    lists_orders_zip_hs_df %>% count(stu_gender)
    
    lists_orders_zip_hs_df <-  lists_orders_zip_hs_df %>% mutate(
      stu_women_dummy = ifelse(stu_gender=="F"| stu_gender=="Female", 1, NA_integer_),
      stu_women_dummy = ifelse(stu_gender=="M"| stu_gender=="Male", 0, stu_women_dummy)
      
    )
    
    lists_orders_zip_hs_df %>% count(stu_gender, stu_women_dummy)
    
################### ANALYSIS VISUALS FOR RQ1: CHARACTERISTICS OF ORDERS
    
    # unique IDs for order nums
    orders_df %>% group_by(univ_type) %>%
      summarise(n=n_distinct(order_num)) 
    
    #for policy report, how many names purchased annually by institution
    orders_df <- orders_df %>% mutate(
      order_year = year(date_start)
    )
    
    policy_report <- orders_df %>% group_by(univ_id, order_year) %>% summarise(
      sum_students_peryear = sum(num_students, na.rm = T)
    )
    
    policy_report <- policy_report %>% filter(!is.na(order_year))  
    
    policy_report %>% group_by(univ_type) %>% summarise(
      mean_students_peryear = mean(sum_students_peryear, na.rm = T)
    )
    
    
    # how many orders total , students total; then by research vs regional
        orders_df %>% count()
        
        orders_df %>%
          distinct(univ_type, univ_id, order_num) %>%
          group_by(univ_id) %>%
          summarize("orders by each univ" = n())
        
        
        orders_df %>%
          distinct(univ_type, univ_id, order_num) %>%
          group_by(univ_type) %>%
          summarize("orders by univ type" = n())
        
        orders_fig_totals <- orders_df %>% 
            group_by(univ_id, univ_type) %>%
            summarise(total_orders = n(),
                      total_students = sum(num_students, na.rm = T))
        
        orders_fig_totals <-  orders_fig_totals %>% arrange(-total_students) 
        
        orders_fig_totals<-tibble::rowid_to_column(orders_fig_totals, "university")

        
        orders_fig_totals$total_orders_st <- str_c(orders_fig_totals$total_orders, ' orders')
      
        #CURERENTLY FIGURE 7: Orders purchased by carnegie classification
        ggplot(orders_fig_totals, aes(x=reorder(university, -total_students), y=total_students, fill=univ_type)) +
            geom_bar(stat = "identity") +
            geom_text(aes(label=total_orders_st), vjust=0, size=2.5) 
        
        
        orders_df %>% select(num_students) %>%
          summarise(across(
            .cols = where(is.numeric), 
            .fns = list(Mean = mean, SD=sd, median =median), na.rm = TRUE, 
            .names = "{col}_{fn}"
          ))
        
        
        orders_df %>% group_by(univ_type) %>% select(num_students) %>%
          summarise(across(
            .cols = where(is.numeric), 
            .fns = list(Mean = mean, SD=sd, median =median), na.rm = TRUE, 
            .names = "{col}_{fn}"
          ))
        
        
        orders_df<-orders_df %>% mutate_if(is.character, list(~na_if(.,""))) 
        
        
    # Frequency of Filters Used Across Orders
        orders_filters <- orders_df %>% 
            mutate(
                hsgrad_class = ifelse(!is.na(hs_grad_class), 1, 0),
                zip = ifelse(!is.na(zip_code) | !is.na(zip_code_file), 1, 0), #KSshould this include zip_code_file not missing too?
                states_fil = ifelse(!is.na(state_name), 1, 0), 
                cbsa = ifelse(!is.na(cbsa_name), 1, 0), 
                intl = ifelse(!is.na(intl_region), 1, 0), 
                segment = ifelse(!is.na(segment), 1, 0), 
                race = ifelse(!is.na(race_ethnicity), 1, 0), 
                gender = ifelse(!is.na(gender), 1, 0), 
                sat = ifelse((!is.na(sat_score_min) | !is.na(sat_score_max) | !is.na(sat_score_old_min) | !is.na(sat_score_old_max)), 1, 0), 
                psat = ifelse((!is.na(psat_score_min) | !is.na(psat_score_max) | !is.na(psat_score_old_min) | !is.na(psat_score_old_max)), 1, 0), 
                gpa = ifelse((!is.na(gpa_low) | !is.na(gpa_high)), 1, 0), 
                rank = ifelse((!is.na(rank_low) | !is.na(rank_high)), 1, 0), 
                geomarket = ifelse(!is.na(geomarket), 1, 0), 
                ap_score = ifelse(!is.na(ap_scores), 1, 0),
                county = ifelse(!is.na(county), 1, 0),
                college_type = ifelse(!is.na(college_type), 1, 0),
                edu_aspirations = ifelse(!is.na(edu_aspirations), 1, 0),
                rotc = ifelse(!is.na(rotc_plans), 1, 0),
                major = ifelse(!is.na(major), 1, 0),
                citizenship = ifelse(!is.na(citizenship), 1, 0),
                low_ses = ifelse(!is.na(low_ses), 1, 0),
                college_size = ifelse(!is.na(college_size), 1, 0),
                national_recognition_programs = ifelse(!is.na(national_recognition_programs), 1, 0),
                college_location = ifelse(!is.na(college_location), 1, 0),
                financial_aid = ifelse(!is.na(financial_aid), 1, 0),
                college_setting = ifelse(!is.na(college_setting), 1, 0),
                college_studentbody = ifelse(!is.na(college_student_body), 1, 0),
                college_living_plans = ifelse(!is.na(college_living_plans), 1, 0),
                proximity_search = ifelse(!is.na(proximity_search), 1, 0),
                hs_math = ifelse(!is.na(hs_math), 1, 0),
                first_gen_parent = ifelse(!is.na(first_gen_parent_edu), 1, 0),
                sat_math = ifelse((!is.na(sat_score_math_min) | !is.na(sat_score_math_max) | !is.na(sat_score_math_old_min) | !is.na(sat_score_math_old_max)), 1, 0), 
                sat_writing = ifelse((!is.na(sat_score_writing_min) | !is.na(sat_score_writing_max) | !is.na(sat_score_writing_old_min) | !is.na(sat_score_writing_old_max)), 1, 0), 
                sat_reading = ifelse((!is.na(sat_score_reading_min) | !is.na(sat_score_reading_max) | !is.na(sat_score_reading_old_min) | !is.na(sat_score_reading_old_max)), 1, 0), 
            )
        
        
        orders_filters1 <- orders_filters %>% group_by(univ_type) %>%
            select(hsgrad_class, zip, states_fil, cbsa, 
                   intl, segment, race, gender,sat, psat,
                   gpa, rank, geomarket, ap_score, county, college_type,
                   edu_aspirations, rotc, major, citizenship, low_ses, college_size,
                   national_recognition_programs, college_location, financial_aid,
                   college_setting, college_student_body, college_living_plans, proximity_search, hs_math, first_gen_parent,
                   sat_math, sat_writing, sat_reading) %>%
            summarize_if(is.numeric, sum, na.rm=TRUE)
        
        
        
        orders_filters_research <- orders_filters1 %>% filter(univ_type=="research") %>% select(-univ_type)

        orders_filters_research  <- as.data.frame(t(orders_filters_research))

        orders_filters_research$filters <- rownames(orders_filters_research)

        total_orders_research <- orders_df %>% filter(univ_type=="research") %>% count()

        orders_filters_research  <- orders_filters_research %>%
            mutate(
                percent= round((V1/sum(total_orders_research$n))*100)
            )
        
        
        orders_filters_research$percent <- str_c(orders_filters_research$percent, '%')

        orders_filters_research$type <- "research"


        #CURRENTLY FIGURE 8: Filters used in order purchases
        ggplot(orders_filters_research, aes(x=reorder(filters, V1), y=V1)) +
            geom_bar(stat = "identity") +
            ylab("Number of Orders") +
            geom_text(aes(label = percent), hjust = -0.1, colour = "black", size=2) +
            coord_flip()
        
        
     # descriptive stats on GPA Filter
        orders_df %>% filter(!is.na(gpa_high) | !is.na(gpa_high)) %>% group_by(univ_type) %>% count(gpa_low)
        orders_df %>% filter(!is.na(gpa_high) | !is.na(gpa_high)) %>% group_by(univ_type) %>% count(gpa_high)
        
        #replace empty strings with NA
        orders_df <- orders_df %>%
            mutate(across(c("gpa_low","gpa_high"), ~ifelse(.=="", NA, as.character(.))))
        
        orders_df %>% count(gpa_low)
        orders_df %>% count(gpa_high)
        
        research_gpalow <- orders_df %>% filter(univ_type=="research")
        research_gpalow <- research_gpalow %>% filter(!is.na(gpa_high) | !is.na(gpa_low))
        
        regional_gpalow <- orders_df %>% filter(univ_type=="regional")
        regional_gpalow <- regional_gpalow %>% filter(!is.na(gpa_high) | !is.na(gpa_low))
        
        research_gpalow <- research_gpalow  %>% group_by(gpa_low) %>%
            summarise(n_low = n()) %>%
            mutate(pct_low = round(n_low / sum(n_low)*100, digits=1))
        
        regional_gpalow <- regional_gpalow %>% group_by(gpa_low) %>%
          summarise(n_low = n()) %>%
          mutate(pct_low = round(n_low / sum(n_low)*100, digits=1))
        
        
        research_gpalow$type <- "research"
        regional_gpalow$type <- "regional"
        
        table_gpa <- rbind(research_gpalow,regional_gpalow)
        
        
        #CURRENTLY TABLE 3: FILTER BY GPA RANGES
        ggplot(table_gpa, aes(x=gpa_low, y=n_low, fill=type)) ,
          geom_bar(position="dodge", stat="identity") ,
          ylab("Number of Orders") ,
          geom_text(aes(label = pct_low), hjust = -0.1, colour = "black", size=2) 
        
        
        # descriptive stats on PSAT/SAT Filter
        orders_df %>% count(psat_score_max)
        orders_df %>% count(psat_score_min) 
        
        orders_df %>% count(psat_score_old_max)
        orders_df %>% count(psat_score_old_min)
        
        # PSAT cutoffs tabulations
        
        orders_df$brks <- cut(orders_df$psat_score_min, 
                              breaks=c(-1, 1000, 1101, 1201, 1301, 1401, 1501, 1620), 
                              labels=c("<1000", "1000-1100", "1110-1200", 
                                       "1210-1300", "1310-1400", "1410-1500", "1500,"))
        
        orders_df %>% group_by(psat_score_min) %>%
          summarise(n_high = n()) %>%
          mutate(pct_high = round(n_high / sum(n_high)*100, digits=1)) %>% print(n=50)
        

        psat_min<- orders_df %>% filter(!is.na(brks)) %>% group_by(univ_type, brks) %>%
          summarise(n_high = n()) %>%
          mutate(pct_high = round(n_high / sum(n_high)*100, digits=1)) %>% complete(univ_type, brks)

        psat_min$test<- "PSAT"
        psat_min$range<- "min"
        
        orders_df$brks <- cut(orders_df$psat_score_max, 
                              breaks=c(-1, 1000, 1101, 1201, 1301, 1401, 1501, 1620), 
                              labels=c("<1000", "1000-1100", "1110-1200", 
                                       "1210-1300", "1310-1400", "1410-1500", "1500,"))
        
        psat_max<- orders_df %>% filter(!is.na(brks)) %>% group_by(univ_type,brks) %>%
          summarise(n_high = n()) %>%
          mutate(pct_high = round(n_high / sum(n_high)*100, digits=1)) %>% complete(univ_type,brks)
        
        psat_max$test<- "PSAT"
        psat_max$range<- "max"
        
    
        
        # SAT cutoffs tabulations

        orders_df$brks <- cut(orders_df$sat_score_min, 
                              breaks=c(-1, 1000, 1101, 1201, 1301, 1401, 1501, 1620), 
                              labels=c("<1000", "1000-1100", "1110-1200", 
                                       "1210-1300", "1310-1400", "1410-1500", "1500,"))
        
        orders_df %>% group_by(sat_score_min) %>%
          summarise(n_high = n()) %>%
          mutate(pct_high = round(n_high / sum(n_high)*100, digits=1)) %>% print(n=50)
        
        
        sat_min <- orders_df %>% filter(!is.na(brks)) %>% group_by(univ_type, brks) %>%
          summarise(n_high = n()) %>%
          mutate(pct_high = round(n_high / sum(n_high)*100, digits=1)) %>% complete(univ_type,brks)
        
        sat_min$test<- "SAT"
        sat_min$range<- "min"
        
         
        
        orders_df$brks <- cut(orders_df$sat_score_max, 
                                      breaks=c(-1, 1000, 1101, 1201, 1301, 1401, 1501, 1620), 
                                      labels=c("<1000", "1000-1100", "1110-1200", 
                                               "1210-1300", "1310-1400", "1410-1500", "1500,"))
        
        sat_max <- orders_df %>% filter(!is.na(brks)) %>% group_by(univ_type, brks) %>%
          summarise(n_high = n()) %>%
          mutate(pct_high = round(n_high / sum(n_high)*100, digits=1)) %>% complete(univ_type,brks)
        
        
        sat_max$test<- "SAT"
        sat_max$range<- "max"
        
        
        table_scores <- rbind(psat_min,psat_max, sat_min, sat_max)
        
        table_scores %>% group_by(univ_type,test) %>%
          summarise(num = sum(n_high, na.rm = T))
        
        #NEWFIGURE: PSAT/SAT Filters used in order purchases --min thresholds
        table_scores %>% filter(range=="min") %>%
        ggplot(aes(x=brks, y=pct_high, fill=c(univ_type))) +
          geom_bar(position="dodge", stat="identity") +
          facet_wrap(~test) +
          ylab("Percent of Orders") +
          ggtitle("Minimum Score Filters") +
          geom_text(aes(label = n_high), hjust = -0.1, colour = "black", size=2) +
          coord_flip()
        
        
        #NEWFIGURE: PSAT/SAT Filters used in order purchases--max thresholds
        table_scores %>% filter(range=="max") %>%
          ggplot(aes(x=brks, y=pct_high, fill=c(univ_type))) ,
          geom_bar(position="dodge", stat="identity") ,
          facet_wrap(~test) ,
          ylab("Percent of Orders") ,
          ggtitle("Maximum Score Filters") ,
          geom_text(aes(label = n_high), hjust = -0.1, colour = "black", size=2) ,
          coord_flip()
        
        
        # test_scores <- orders_df %>% group_by(univ_c15basic) %>%
        #     select(psat_score_min, psat_score_max, 
        #            sat_score_min, sat_score_max, 
        #            sat_score_old_min, sat_score_old_max) %>%
        #     summarise(across(
        #         .cols = where(is.numeric), 
        #         .fns = list(Mean = mean, SD=sd), na.rm = TRUE, 
        #         .names = "{col}_{fn}"
        #     ))
        # 
        # 
        # #average max/min scores by institution type
        # test_scores <- orders_df %>% group_by(carnegie) %>%
        #   select(psat_score_min, psat_score_max, 
        #          sat_score_min, sat_score_max, 
        #          sat_score_old_min, sat_score_old_max) %>%
        #   summarise(across(
        #     .cols = where(is.numeric), 
        #     .fns = list(Mean = mean, SD=sd), na.rm = TRUE, 
        #     .names = "{col}_{fn}"
        #   ))
           
    # descriptive stats on HS RANK Filter
        orders_df %>% count(rank_high)
        orders_df %>% count(rank_low)    
        
        
    # descriptive stats on AP SCORES Filter
        orders_df %>% count(ap_scores)
        orders_df %>% filter(!is.na(ap_scores)) %>% count(univ_id) # only UC Davis (5) and UCSD (17)
        
        ap_filters <- orders_df %>% filter(!is.na(ap_scores)) %>% select(ap_scores, order_num)
        #ap_filters_list <- as.list(ap_filters$ap_scores)
        
        
        ap_filters_list <- ap_filters %>%
            unnest(ap_scores)
        
        ap_filters_list<-  ap_filters_list %>% group_by(order_num) %>% data.frame(do.call("rbind", strsplit(as.character(ap_filters_list$ap_scores), "|", fixed = TRUE)))
    
        
        ap_filters_list_long <- ap_filters_list %>% gather(ap_score_filters, value, -c(ap_scores, order_num))
        ap_filters_list_long$ap_score_filters<-gsub("X","",as.character(ap_filters_list_long$ap_score_filters))
        
        
        table_ap <- ap_filters_list_long %>% group_by(value) %>%
            summarise(n = n()) %>%
            mutate(pct = round(n / sum(n)*100, digits=1))
        
        ggplot(table_ap, aes(x=reorder(value, n), y=n)) ,
            geom_bar(stat = "identity") ,
            ylab("Number of Orders") ,
            geom_text(aes(label = pct), hjust = -0.1, colour = "black", size=2) ,
            coord_flip()
       
        
  # GEOGRAPHIC FILTERS
        
        #zip code filters
        orders_df %>% count(zip_code) #all three digit
        orders_df %>% count(!is.na(zip_code)) #all three digit
        orders_df %>% count(zip_code_file)
        orders_df %>% count(!is.na(zip_code_file)) #all three digit
        
          #KS NOTES: all 3-digits: https://en.wikipedia.org/wiki/List_of_ZIP_Code_prefixes
        
            #check NAU
              nau <- orders_df %>% filter(univ_id=="105330")
              nau %>% count(order_num)
              nau %>% count(date_start)
              nau %>% filter(!is.na(zip_code_file)) %>% count(order_num, zip_code_file) %>% print(n=90)
        
        #ZIP CODES
        orders_df %>% filter(!is.na(zip_code) | !is.na(zip_code_file) ) %>% count(univ_type)
        
        orders_with_zip <- orders_df %>%
          filter(!is.na(zip_code) | !is.na(zip_code_file) ) 
        
        orders_with_zip %>%
          distinct(univ_type, univ_id, order_num) %>%
          group_by(univ_type) %>%
          summarize("orders by univ type" = n())
        
        orders_with_zip %>%
          filter(!is.na(zip_code)) %>%
          distinct(univ_type, univ_id, order_num) %>%
          summarize("orders by univ type" = n())
        
        orders_with_zip %>%
          distinct(univ_id) %>%
          summarize("orders by univ type" = n())
        
        orders_df %>% filter(!is.na(zip_code)) %>% count(zip_code, univ_state)
        orders_df %>% filter(!is.na(zip_code)) %>% count(zip_code, univ_name)
        
        
        #how many did not have attached zip code files
        orders_df %>% filter(!is.na(zip_code) | !is.na(zip_code_file) ) %>%
          count(order_num) 

                
        # descriptive stats on SEGMENT Filter
        orders_df %>% count(segment)
        orders_df %>% filter(!is.na(segment)) %>% count(univ_id) # only Urbana-Champagne (21) and Northeastern (1)
        
        segment_filters <- orders_df %>% filter(!is.na(segment))%>% select(segment, order_num)
        
        
        # descriptive stats of STATE filter  
        
        orders_df %>% count(state_name) %>% print(n=50)
        orders_df %>% group_by(univ_type) %>% count(state_name) %>% print(n=50)
        
            #parse state filters for regional univs
            regional_states <- orders_df %>% filter(univ_type=="regional") %>% count(state_name)
            strsplit(regional_states$state_name, split = "|", fixed=T)
            
            
            regional_states <- regional_states %>% 
             mutate(name=strsplit(state_name, split = "|", fixed=T)) %>% 
             unnest(name) 
           
            regional_states <- regional_states %>% 
              mutate(name=if_else(name=="Arizona", "AZ", name),
                     name= if_else(name=="Texas", "TX", name),
                     name= as.factor(name))
            
            regional_states$fips <- fips(regional_states$name)
            states <- us_map(regions = "states")
            states <- states %>% count(fips)
            states <- states %>% mutate(
              filtered_state = ifelse(fips %in% regional_states$fips, 1, 0),
              filtered_state= as.factor(filtered_state)
            )
            
            # CURRENT NOT IN EMPIRICAL REPORT: STATE FILTER MAPS--Dummy coded
            plot_usmap(regions = "states", data=states, values = "filtered_state",color="grey"), 
              theme(panel.background = element_rect(colour = "black")) ,
              scale_fill_manual(values = c(`0` = "white", `1` = "blue"), name = "filtered_state") , 
              theme(legend.position = "right") ,
              labs(title = "State Filters for Regional Universities")
        
            # CURRENT NOT IN EMPIRICAL REPORT: STATE FILTER MAPS--Scale by # of Orders using filter
            states <- us_map(regions = "states")
            states <- states %>% group_by(fips, abbr, full) %>% count(fips)
            
            regional_states_num <- regional_states %>% filter(!is.na(name)) %>% group_by(name) %>% 
              summarise(frequency = sum(n))
            
            states <- merge(states, regional_states_num, by.x = "abbr", by.y = "name", all.x = TRUE)
            states <- states %>% mutate(
              frequency = if_else(is.na(frequency), as.double(0), as.double(frequency))
            )
            
            plot_usmap(regions = "states", data=states, values = "frequency",color="grey"), 
              theme(panel.background = element_rect(colour = "black")) ,
              scale_fill_continuous(low = "white", high ="darkgreen", 
                                    name = "filtered_state",label = scales::comma,
                                    limits = c(0,35)) , 
              theme(legend.position = "right") ,
              labs(title = "State Filters for Regional Universities")
            
            
            
            
            #parse state filters for research univs
            research_states <- orders_df %>% filter(univ_type=="research") %>% count(state_name)
            strsplit(research_states$state_name, split = "|", fixed=T)
            
            
            research_states <- research_states %>% 
              mutate(name=strsplit(state_name, split = "|", fixed=T)) %>% 
              unnest(name) 
          
            
            research_states <- research_states %>%
              mutate(name=if_else(name=="Arizona", "AZ", name),
                     name= if_else(name=="Texas", "TX", name),
                     name= if_else(name=="Armed Forces Americas (Except Canada)", NA_character_, name),
                     name= if_else(name=="Connecticut", "CT", name),
                     name= if_else(name=="Armed Forces Canada, Europe, Middle East, Africa", NA_character_, name),
                     name= if_else(name=="Missouri", "MO", name),
                     name= if_else(name=="Vermont", "VT", name),
                     name= if_else(name=="California", "CA", name),
                     name= if_else(name=="Armed Forces Pacific", NA_character_, name),
                     name= if_else(name=="Delaware", "DE", name),
                     name= if_else(name=="Hawaii", "HI", name),
                     name= if_else(name=="Massachusetts", "MA", name),
                     name= if_else(name=="Maryland", "MD", name),
                     name= if_else(name=="Iowa", "IA", name),
                     name= if_else(name=="Rhode Island", "RI", name),
                     name= if_else(name=="Maine", "ME", name),
                     name= if_else(name=="Virginia", "VA", name),
                     name= if_else(name=="Michigan", "MI", name),
                     name= if_else(name=="Idaho", "ID", name),
                     name= if_else(name=="Arkansas", "AR", name),
                     name= if_else(name=="Utah", "UT", name),
                     name= if_else(name=="Illinois", "IL", name),
                     name= if_else(name=="Indiana", "IN", name),
                     name= if_else(name=="Minnesota", "MN", name),
                     name= if_else(name=="Montana", "MT", name),
                     name= if_else(name=="Mississippi", "MS", name),
                     name= if_else(name=="New Hampshire", "NH", name),
                     name= if_else(name=="New Jersey", "NJ", name),
                     name= if_else(name=="New Mexico", "NM", name),
                     name= if_else(name=="Alaska", "AK", name),
                     name= if_else(name=="Alabama", "AL", name),
                     name= if_else(name=="North Dakota", "ND", name),
                     name= if_else(name=="Nebraska", "NE", name),
                     name= if_else(name=="New York", "NY", name),
                     name= if_else(name=="Georgia", "GA", name),
                     name= if_else(name=="Nevada", "NV", name),
                     name= if_else(name=="Tennessee", "TN", name),
                     name= if_else(name=="Oklahoma", "OK", name),
                     name= if_else(name=="Ohio", "OH", name),
                     name= if_else(name=="Wyoming", "WY", name),
                     name= if_else(name=="Florida", "FL", name),
                     name= if_else(name=="South Dakota", "SD", name),
                     name= if_else(name=="South Carolina", "SC", name),
                     name= if_else(name=="North Carolina", "NC", name),
                     name= if_else(name=="Connecticut", "CT", name),
                     name= if_else(name=="West Virginia", "WV", name),
                     name= if_else(name=="District of Columbia", "DC", name),
                     name= if_else(name=="Wisconsin", "WI", name),
                     name= if_else(name=="Kentucky", "KY", name),
                     name= if_else(name=="Kansas", "KS", name),
                     name= if_else(name=="Oregon", "OR", name),
                     name= if_else(name=="Louisiana", "LA", name),
                     name= if_else(name=="Washington", "WA", name),
                     name= if_else(name=="Colorado", "CO", name),
                     name= if_else(name=="Pennsylvania", "PA", name),
                     name= as.factor(name))

      
            
            research_states$fips <- fips(research_states$name)
            states <- us_map(regions = "states")
            states <- states %>% count(fips)
            states <- states %>% mutate(
              filtered_state = ifelse(fips %in% research_states$fips, 1, 0),
              filtered_state= as.factor(filtered_state)
            )
            
            
          
            #NOT CURRENT IN FIGURES: BUT ALL STATES FILTERED BY RESEARCH UNIVS
            plot_usmap(regions = "states", data=states, values = "filtered_state",color="grey"), 
              theme(panel.background = element_rect(colour = "black")) ,
              scale_fill_manual(values = c(`0` = "white", `1` = "blue"), name = "filtered_state") , 
              theme(legend.position = "right") ,
              labs(title = "State Filters for research Universities")
            
            
            # CURRENT NOT IN EMPIRICAL REPORT: STATE FILTER MAPS--Scale by # of Orders using filter
            states <- us_map(regions = "states")
            states <- states %>% group_by(fips, abbr, full) %>% count(fips)
            
            research_states_num <- research_states %>% filter(!is.na(name)) %>% group_by(name) %>% 
              summarise(frequency = sum(n))
            
            states <- merge(states, research_states_num, by.x = "abbr", by.y = "name", all.x = TRUE)
            states <- states %>% mutate(
              frequency = if_else(is.na(frequency), as.double(0), as.double(frequency))
            )
            
            plot_usmap(regions = "states", data=states, values = "frequency",color="grey"), 
              theme(panel.background = element_rect(colour = "black")) ,
              scale_fill_continuous(low = "blue", high ="green", 
                                    name = "filtered_state",label = scales::comma,
                                    limits = c(0,120)) , 
              theme(legend.position = "right") ,
              labs(title = "State Filters for Research Universities")
            
            
            
        # descriptive stats for segment filter
        orders_df %>% filter(!is.na(segment)) %>% count(univ_id)
        orders_df %>% filter(univ_id == '110653') %>% count(segment)
        orders_df %>% filter(univ_id == '145637') %>% count(segment)
        orders_df %>% filter(univ_id == '147776') %>% count(segment) #just says include all students, did this Northeastern order use segment?
        

    # Demographic filters
        
        orders_df %>% count(race_ethnicity) %>% print(n=40)

        orders_df %>% count(gender) %>% print(n=40)
        orders_df %>% count(univ_type, gender) %>% print(n=40)
        
        
        #check other demographic filters
        orders_df %>% count(univ_name,first_gen_parent_edu) %>% print(n=40)
        orders_df %>% count(univ_name,low_ses) %>% print(n=40)
        orders_df %>% filter(!is.na(financial_aid)) %>% count(univ_name,financial_aid) %>% pull(financial_aid) 
        
        
        #SEE END OF R-SCRIPT FOR NEW RACE FILTER: FIGURE UNDER TARGETING SOC DEEP DIVE
        
        
        
    # Descriptives on Filter Combos
        
        
        orders_filters <- orders_df %>% 
          mutate(
            hsgrad_class = ifelse(!is.na(hs_grad_class), 1, 0),
            zip = ifelse(!is.na(zip_code) | !is.na(zip_code_file), 1, 0), #KSshould this include zip_code_file not missing too?
            states_fil = ifelse(!is.na(state_name), 1, 0), 
            cbsa = ifelse(!is.na(cbsa_name), 1, 0), 
            intl = ifelse(!is.na(intl_region), 1, 0), 
            segment = ifelse(!is.na(segment), 1, 0), 
            race = ifelse(!is.na(race_ethnicity), 1, 0), 
            gender = ifelse(!is.na(gender), 1, 0), 
            sat = ifelse((!is.na(sat_score_min) | !is.na(sat_score_max) | !is.na(sat_score_old_min) | !is.na(sat_score_old_max)), 1, 0), 
            psat = ifelse((!is.na(psat_score_min) | !is.na(psat_score_max) | !is.na(psat_score_old_min) | !is.na(psat_score_old_max)), 1, 0), 
            gpa = ifelse((!is.na(gpa_low) | !is.na(gpa_high)), 1, 0), 
            rank = ifelse((!is.na(rank_low) | !is.na(rank_high)), 1, 0), 
            geomarket = ifelse(!is.na(geomarket), 1, 0), 
            ap_score = ifelse(!is.na(ap_scores), 1, 0),
            county = ifelse(!is.na(county), 1, 0),
            college_type = ifelse(!is.na(college_type), 1, 0),
            edu_aspirations = ifelse(!is.na(edu_aspirations), 1, 0),
            rotc = ifelse(!is.na(rotc_plans), 1, 0),
            major = ifelse(!is.na(major), 1, 0),
            citizenship = ifelse(!is.na(citizenship), 1, 0),
            low_ses = ifelse(!is.na(low_ses), 1, 0),
            college_size = ifelse(!is.na(college_size), 1, 0),
            national_recognition_programs = ifelse(!is.na(national_recognition_programs), 1, 0),
            college_location = ifelse(!is.na(college_location), 1, 0),
            financial_aid = ifelse(!is.na(financial_aid), 1, 0),
            college_setting = ifelse(!is.na(college_setting), 1, 0),
            college_studentbody = ifelse(!is.na(college_student_body), 1, 0),
            college_living_plans = ifelse(!is.na(college_living_plans), 1, 0),
            proximity_search = ifelse(!is.na(proximity_search), 1, 0),
            hs_math = ifelse(!is.na(hs_math), 1, 0),
            first_gen_parent = ifelse(!is.na(first_gen_parent_edu), 1, 0),
            sat_math = ifelse((!is.na(sat_score_math_min) | !is.na(sat_score_math_max) | !is.na(sat_score_math_old_min) | !is.na(sat_score_math_old_max)), 1, 0), 
            sat_writing = ifelse((!is.na(sat_score_writing_min) | !is.na(sat_score_writing_max) | !is.na(sat_score_writing_old_min) | !is.na(sat_score_writing_old_max)), 1, 0), 
            sat_reading = ifelse((!is.na(sat_score_reading_min) | !is.na(sat_score_reading_max) | !is.na(sat_score_reading_old_min) | !is.na(sat_score_reading_old_max)), 1, 0), 
          )
        
        
        
       filter_combos <- orders_filters %>% group_by(univ_type) %>%
            select(hsgrad_class, zip, states_fil, cbsa, 
                   intl, segment, race, gender,sat, psat,
                   gpa, rank, geomarket, ap_score,  county, college_type,
                   edu_aspirations, rotc, major, citizenship, low_ses, college_size,
                   national_recognition_programs, college_location, financial_aid,
                   college_setting, college_studentbody, college_living_plans, proximity_search, hs_math, first_gen_parent,
                   sat_math, sat_writing, sat_reading, univ_type) %>%
            mutate(filter_sum = hsgrad_class + zip + states_fil + cbsa + 
                             intl + segment + race + gender + sat + psat +
                             gpa + rank + geomarket + ap_score + county +  college_type + 
                   edu_aspirations +  rotc +  major +  citizenship +  low_ses +  college_size + 
                   national_recognition_programs +  college_location +  financial_aid + 
                   college_setting +  college_studentbody +  college_living_plans +  proximity_search +  hs_math +  first_gen_parent + 
                   sat_math +  sat_writing +  sat_reading)
      
        
       #get averages
       filter_combos %>% summarise(mean_criteria = mean(filter_sum, na.rm = TRUE))
       
       
       
        filter_combos <- filter_combos %>% 
            mutate(
                hsgrad_class = ifelse(hsgrad_class==1, "grad_class", NA),
                zip = ifelse(zip==1, "zip", NA), #KSshould this include zip_code_file not missing too?
                states_fil = ifelse(states_fil==1, "state", NA), 
                cbsa = ifelse(cbsa==1, "cbsa", NA), 
                intl = ifelse(intl==1, "intl", NA), 
                segment = ifelse(segment==1, "segment", NA), 
                race = ifelse(race==1, "race", NA), 
                gender = ifelse(gender==1, "gender", NA), 
                sat = ifelse(sat==1, "sat", NA), 
                psat = ifelse(psat==1, "psat", NA), 
                gpa = ifelse(gpa==1, "gpa", NA), 
                rank = ifelse(rank==1, "rank", NA), 
                geomarket = ifelse(geomarket==1, "geomarket", NA), 
                ap_score = ifelse(ap_score==1, "APscores", NA),
                county = ifelse(county==1, "County", NA),
                college_type = ifelse(college_type==1, "College Type", NA), 
                edu_aspirations = ifelse(edu_aspirations==1, "Edu Aspirations", NA),
                rotc = ifelse(rotc==1, "ROTC", NA),
                major = ifelse(major==1, "Major", NA),  
                citizenship = ifelse(citizenship==1, "Citizenship", NA),
                low_ses = ifelse(low_ses==1, "Low SES", NA),
                college_size = ifelse(college_size==1, "College Size", NA), 
                national_recognition_programs = ifelse(national_recognition_programs==1, "National Recognition Programs", NA),
                college_location = ifelse(college_location==1, "College Location", NA),
                financial_aid = ifelse(financial_aid==1, "Financial Aid", NA),
                college_setting = ifelse(college_setting==1, "College Setting", NA),
                college_studentbody = ifelse(college_studentbody==1, "College Student Body", NA),
                college_living_plans = ifelse(college_living_plans==1, "College Living Plans", NA),
                proximity_search = ifelse(proximity_search==1, "Proximity Search", NA),
                hs_math = ifelse(hs_math==1, "HS Math", NA),
                first_gen_parent = ifelse(first_gen_parent==1, "First Gen", NA), 
                sat_math = ifelse(sat_math==1, "SAT Math", NA),
                sat_writing = ifelse(sat_writing==1, "SAT Writing", NA),
                sat_reading = ifelse(sat_reading==1, "SAT Reading", NA))
        
        
        filter_combos[filter_combos == "NA"] <- NA_character_
        
                  
        
        filter_combos_research <- filter_combos %>% filter(univ_type=="research")
        filter_combos_regional <- filter_combos %>% filter(univ_type=="regional")
        
        
              #get averages
              filter_combos %>% summarise(mean_criteria = mean(filter_sum, na.rm = TRUE))
        
        combos_research <- unique(filter_combos_research[c("hsgrad_class", "zip", "states_fil", "cbsa", "intl", "segment", "race",
                                         "gender","sat", "psat","gpa", "rank" , "geomarket", "ap_score",
                                         "county" ,  "college_type" , "edu_aspirations" ,  "rotc" ,  "major" ,  "citizenship" ,  "low_ses" ,  "college_size" , 
                                           "national_recognition_programs" ,  "college_location" ,  "financial_aid" , 
                                           "college_setting" ,  "college_studentbody" ,  "college_living_plans" ,  "proximity_search" ,  "hs_math" ,  "first_gen_parent" , 
                                           "sat_math" ,  "sat_writing" ,  "sat_reading")], na.rm = TRUE)
        
        
        combos_regional <- unique(filter_combos_regional[c("hsgrad_class", "zip", "states_fil", "cbsa", "intl", "segment", "race",
                                                           "gender","sat", "psat","gpa", "rank" , "geomarket", "ap_score",
                                                           "county" ,  "college_type" , "edu_aspirations" ,  "rotc" ,  "major" ,  "citizenship" ,  "low_ses" ,  "college_size" , 
                                                           "national_recognition_programs" ,  "college_location" ,  "financial_aid" , 
                                                           "college_setting" ,  "college_studentbody" ,  "college_living_plans" ,  "proximity_search" ,  "hs_math" ,  "first_gen_parent" , 
                                                           "sat_math" ,  "sat_writing" ,  "sat_reading")], na.rm = TRUE)
        
        
        combos <- unique(filter_combos[c("hsgrad_class", "zip", "states_fil", "cbsa", "intl", "segment", "race",
                                         "gender","sat", "psat","gpa", "rank" , "geomarket", "ap_score",
                                         "county" ,  "college_type" , "edu_aspirations" ,  "rotc" ,  "major" ,  "citizenship" ,  "low_ses" ,  "college_size" , 
                                         "national_recognition_programs" ,  "college_location" ,  "financial_aid" , 
                                         "college_setting" ,  "college_studentbody" ,  "college_living_plans" ,  "proximity_search" ,  "hs_math" ,  "first_gen_parent" , 
                                         "sat_math" ,  "sat_writing" ,  "sat_reading")], na.rm = TRUE)
        
     
       
        #CURRENTLY TABLE 7: UPDATE SHOW RESEARCH VERSUS REGIONAL
            df_0_research <- group_by(filter_combos_research, hsgrad_class, zip, states_fil, 
                          cbsa, intl, segment, race, gender, 
                          sat, psat, gpa, rank, geomarket, ap_score,
                          county ,  college_type , 
                            edu_aspirations ,  rotc ,  major ,  citizenship ,  low_ses ,  college_size , 
                            national_recognition_programs ,  college_location ,  financial_aid , 
                            college_setting ,  college_studentbody ,  college_living_plans ,  proximity_search ,  hs_math ,  first_gen_parent , 
                            sat_math ,  sat_writing ,  sat_reading) %>% count()
            
            df_0_research %>% arrange(-n)

            df_0_research <- df_0_research  %>% unite("string", c(hsgrad_class, zip, states_fil, 
                                                cbsa, intl, segment, race, gender, 
                                                sat, psat, gpa, rank, geomarket, ap_score, county ,  college_type , 
                                                edu_aspirations ,  rotc ,  major ,  citizenship ,  low_ses ,  college_size , 
                                                national_recognition_programs ,  college_location ,  financial_aid , 
                                                college_setting ,  college_studentbody ,  college_living_plans ,  proximity_search ,  hs_math ,  first_gen_parent , 
                                                sat_math ,  sat_writing ,  sat_reading), sep=",", remove = TRUE, na.rm = TRUE)
            
            
            df_0_research <- df_0_research %>% arrange(-n)  #%>% head(10)
            
            sum(df_0_research$n)
            
                  #asu skews combos
                  orders_df %>% filter(univ_id=="104151") %>% count(order_num) #131
                  x<-orders_df %>% group_by(univ_id,order_num) %>% count() 
                  x<- x %>% group_by(univ_id) %>% summarise(sum= sum(n, na.rm = T))
                  
            df_0_regional <- group_by(filter_combos_regional, hsgrad_class, zip, states_fil, 
                                      cbsa, intl, segment, race, gender, 
                                      sat, psat, gpa, rank, geomarket, ap_score, county ,  college_type , 
                                      edu_aspirations ,  rotc ,  major ,  citizenship ,  low_ses ,  college_size , 
                                      national_recognition_programs ,  college_location ,  financial_aid , 
                                      college_setting ,  college_studentbody ,  college_living_plans ,  proximity_search ,  hs_math ,  first_gen_parent , 
                                      sat_math ,  sat_writing ,  sat_reading) %>% count()
            
            df_0_regional %>% arrange(-n)
            
            df_0_regional <- df_0_regional  %>% unite("string", c(hsgrad_class, zip, states_fil, 
                                                                  cbsa, intl, segment, race, gender, 
                                                                  sat, psat, gpa, rank, geomarket, ap_score, county ,  college_type , 
                                                                  edu_aspirations ,  rotc ,  major ,  citizenship ,  low_ses ,  college_size , 
                                                                  national_recognition_programs ,  college_location ,  financial_aid , 
                                                                  college_setting ,  college_studentbody ,  college_living_plans ,  proximity_search ,  hs_math ,  first_gen_parent , 
                                                                  sat_math ,  sat_writing ,  sat_reading), sep=",", remove = TRUE, na.rm = TRUE)
            
            
            df_0_regional <- df_0_regional %>% arrange(-n)   %>% head(10)
            
            sum(df_0_regional$n)
          
            
          # Descriptives for geomarket
             orders_df %>% count(geomarket)
            
            
################### ANALYSIS & VISUALS FOR RQ2 
    
             
    # how many students lists do we have?
        lists_orders_zip_hs_df %>% 
            summarise(n=n_distinct(ord_num)) 
        
        lists_orders_zip_hs_df %>% count()
                
    #FUNCTION FOR TABLE ON N, RACE, INCOME, PUB/PRIV SCHOOL CHARACTERISTICS OF STUDENT LIST PROSPECTS
        table_rq2a <- function(variables, columns) {
            
            #create counter
            counter = 0

            #loop through columns via filters (ex: all students, in-state, out-state, etc. )
            for (i in columns) {
                
                counter = counter+1
                
                if(i=="all_domestic") {
                 filter_string=c("stu_in_us==1")
                } else if(i=="in_state")
                {filter_string=c("stu_nonres==0")
                } else if(i=="out_of_state")
                {filter_string=c("stu_nonres==1")
                } else if(i=="research_univ")
                {filter_string=c("stu_in_us==1 & univ_c15basic=='15'")
                } else if(i=="regional_univ")
                {filter_string=c("stu_in_us==1 & univ_c15basic!='15'")
                } else if(i=="research_univ_instate")
                {filter_string=c("stu_in_us==1 & stu_nonres==0 & univ_c15basic=='15'")
                } else if(i=="research_univ_outofstate")
                {filter_string=c("stu_in_us==1 & stu_nonres==1 & univ_c15basic=='15'")
                } else if(i=="regional_univ_instate")
                {filter_string=c("stu_in_us==1 & stu_nonres==0 & univ_c15basic!='15'")
                } else if(i=="regional_univ_outofstate")
                {filter_string=c("stu_in_us==1 & stu_nonres==1 & univ_c15basic!='15'")}
                
                
                #create N row
                n <- as_data_frame(t(lists_orders_zip_hs_df %>% filter(!! rlang::parse_expr(filter_string)) %>% count()))
                row.names(n) <- "Total N"
                
                #create race rows
                race <- lists_orders_zip_hs_df %>% filter(!! rlang::parse_expr(filter_string)) %>%
                    count(stu_race_cb) %>% mutate(V1 = n / sum(n) * 100)
                
                race <- race %>% select(stu_race_cb, V1)
                race <- race %>% mutate(stu_race_cb = ifelse(is.na(stu_race_cb), "Pct Race-Missing", stu_race_cb),
                                        stu_race_cb = ifelse(stu_race_cb==0, "Pct Race-No Response", stu_race_cb),
                                        stu_race_cb = ifelse(stu_race_cb==1, "Pct AI/AN", stu_race_cb),
                                        stu_race_cb = ifelse(stu_race_cb==2, "Pct Asian", stu_race_cb),
                                        stu_race_cb = ifelse(stu_race_cb==3, "Pct Black", stu_race_cb),
                                        stu_race_cb = ifelse(stu_race_cb==4, "Pct Latinx", stu_race_cb),
                                        stu_race_cb = ifelse(stu_race_cb==8, "Pct NH/PI", stu_race_cb),
                                        stu_race_cb = ifelse(stu_race_cb==9, "Pct White", stu_race_cb),
                                        stu_race_cb = ifelse(stu_race_cb==10, "Pct Other Race", stu_race_cb),
                                        stu_race_cb = ifelse(stu_race_cb==12, "Pct Multiracial", stu_race_cb)                                        )
                
                
                race<- race %>% remove_rownames %>% column_to_rownames(var="stu_race_cb")

                #create income row
                income <- as_data_frame (t(lists_orders_zip_hs_df %>% filter(!! rlang::parse_expr(filter_string)) %>% 
                                                summarise (mean_inc = mean(zip_median_household_income, na.rm=T))))
                row.names(income) <- "Median Household Income (mean)"
                
                
                #create school type rows
                schtype <- lists_orders_zip_hs_df %>% filter(!! rlang::parse_expr(filter_string)) %>%
                    count(hs_school_control) %>% mutate(school_type = n / sum(n) * 100)
                
                schtype <- schtype %>% select(hs_school_control, school_type)
                schtype <- schtype %>% mutate(hs_school_control = ifelse(is.na(hs_school_control), "school unknown", hs_school_control))
                schtype<- schtype %>% remove_rownames %>% column_to_rownames(var="hs_school_control")
                schtype<- schtype %>% rename(V1 = school_type)
                row.names(schtype) <- c("Pct Private", "Pct Public", "Pct School Unknown") #NEED TO RE_DO THIS LIKE RACE ABOVE
                
                
                #concatenate all row_dfs for i-column
                temp_df <- bind_rows(mget(variables))
                #temp_df <- bind_rows(n, race, income, schtype)
                temp_df <- temp_df %>% rename(!!paste0("", i) := V1)
                temp_df <- rownames_to_column(temp_df, "row_subj")

                
                #first loop creates the master_df
                #second , loops appends the master df
                if(counter==1){master_df <- as.data.frame(temp_df)}
                if(counter>1){master_df <- merge(master_df,temp_df, by="row_subj", sort=FALSE)}

                
            }
        
            return(master_df)
            
        }
        
        
    
    # CALL FUNCTION TO CREATE TABLE 1
        
        #all possible vars: n, race, income, schtype
        vars <- c("n", "race", "income", "schtype") #all possible vars: n, race, income, schtype
        
        #all possible columns: all_domestic, in_state, out_of_state, research_univ, regional_univ, research_univ_instate, research_univ_outofstate, regional_univ_instate, regional_univ_outofstate,
        cols <- c("all_domestic","in_state", "out_of_state", "research_univ", "regional_univ", "research_univ_instate", "research_univ_outofstate", "regional_univ_instate", "regional_univ_outofstate") 
        df_rq2a<- table_rq2a(vars, cols) 
        
        #format table
        # df_rq2a <- df_rq2a %>% mutate_if(is.numeric, round, 0)
        # df_rq2a <- df_rq2a %>%  mutate_each(funs(prettyNum(., big.mark=",")))

        
        df_rq2a_out_of_state_research <- df_rq2a %>% select(row_subj, research_univ_outofstate, research_univ_instate)
        df_rq2a_out_of_state_regional <- df_rq2a %>% select(row_subj, regional_univ_instate, regional_univ_outofstate)

      # international students
        
        df_int <- lists_orders_zip_hs_df %>% 
          filter(stu_country!="united states") %>% 
          mutate(stu_country = recode(
            stu_country,
            'korea, south (rok)' = 'south korea',
            'korea south (rok)' = 'south korea'
          )) %>% 
          group_by(stu_country) %>%
          summarise(n= n()) %>%
          mutate(pct = round(n / sum(n)*100, digits=1))
        
        df_int <- df_int %>% arrange(-n)
        
        df_int2 <- lists_orders_zip_hs_df %>% count(stu_internat)
        
        
    # checking for missingness
        
        lists_orders_zip_hs_df %>% count(hs_school_control)
  
        
    #check missing propspect race for regional-in-state orders
        
        lists_orders_zip_hs_df <- lists_orders_zip_hs_df %>% mutate(
          univ_type = ifelse(univ_id=="145637" | univ_id=="145600" | univ_id=="104151" |
                               univ_id=="110653" | univ_id=="110680" | univ_id=="110644" |
                               univ_id=="228723" , "research", "regional"))
        
              
        
        regional_lists <- lists_orders_zip_hs_df %>% filter(univ_type=="regional") 
        
        #issue is UIllinois Springfield==148654
        regional_lists %>% group_by(univ_id) %>%
          count(stu_race_cb) %>% mutate(V1 = n / sum(n) * 100) %>% print(n=100)
        
        
        raceproblem_lists <- lists_orders_zip_hs_df %>% filter(univ_id=="148654")
        
        raceproblem_lists %>% count(stu_race_cb)
        raceproblem_lists %>% count(stu_white)
        raceproblem_lists %>% count(stu_black)
        raceproblem_lists %>% count(stu_asian)
        raceproblem_lists %>% count(stu_hispanic_01)
        
        
################### ANALYSIS & VISUALS FOR RQ3: CHARACTERISTICS ACROSS INDIVIDUAL FILTERS
        
        #create filter dummies for student list data
        lists_orders_zip_hs_df <- lists_orders_zip_hs_df  %>%
            mutate(
                filter_hsgrad_class = ifelse(!is.na(ord_hs_grad_class), 1, 0),
                filter_zip = ifelse(!is.na(ord_zip_code) | !is.na(ord_zip_code_file), 1, 0), #KSshould this include zip_code_file not missing too?
                filter_states_fil = ifelse(!is.na(ord_state_name), 1, 0), 
                filter_cbsa = ifelse(!is.na(ord_cbsa_name), 1, 0), 
                filter_intl = ifelse(!is.na(ord_intl_region), 1, 0), 
                filter_segment = ifelse(!is.na(ord_segment), 1, 0), 
                filter_race = ifelse(!is.na(ord_race_ethnicity), 1, 0), 
                filter_gender = ifelse(!is.na(ord_gender), 1, 0), 
                filter_sat = ifelse((!is.na(ord_sat_score_min) | !is.na(ord_sat_score_max)), 1, 0), 
                filter_psat = ifelse((!is.na(ord_psat_score_min) | !is.na(ord_psat_score_max)), 1, 0), 
                filter_gpa = ifelse((!is.na(ord_gpa_low) | !is.na(ord_gpa_high)), 1, 0), 
                filter_rank = ifelse((!is.na(ord_rank_low) | !is.na(ord_rank_high)), 1, 0), 
                filter_geomarket = ifelse(!is.na(ord_geomarket), 1, 0))
        
        
    # PROSPECT CHARS ACROSS INDIVIDUAL FILTERS
        
        # Create table function for lists across single filters
        #FUNCTION FOR TABLE ON N, RACE, INCOME, PUB/PRIV SCHOOL CHARACTERISTICS OF STUDENT LIST PROSPECTS
        table_rq3 <- function(variables, columns) {
          
          #create counter
          counter = 0
          
          #loop through columns via filters (ex: all students, in-state, out-state, etc. )
          for (i in columns) {
            
            counter = counter+1
            
            if(i=="all_domestic") {
              filter_string=c("stu_in_us==1")
            } else if(i=="GPA")
            {filter_string=c("stu_in_us==1 & filter_gpa==1")
            } else if(i=="PSAT")
            {filter_string=c("stu_in_us==1 & filter_psat==1")
            } else if(i=="SAT")
            {filter_string=c("stu_in_us==1 & filter_sat==1")
            } else if(i=="ZIP")
            {filter_string=c("stu_in_us==1 & filter_zip==1")
            } else if(i=="STATE")
            {filter_string=c("stu_in_us==1 & filter_states_fil==1")
            } else if(i=="RACE")
            {filter_string=c("stu_in_us==1 & filter_race==1")
            } else if(i=="HS Rank")
            {filter_string=c("stu_in_us==1 & filter_rank==1")
            } else if(i=="GENDER")
            {filter_string=c("stu_in_us==1 & filter_gender==1")
            } else if(i=="SEGMENT")
            {filter_string=c("stu_in_us==1 & filter_segment==1")
            } else if(i=="CBSA")
            {filter_string=c("stu_in_us==1 & filter_cbsa==1")}
            
            
            #create N row
            n <- as_data_frame(t(lists_orders_zip_hs_df %>% filter(!! rlang::parse_expr(filter_string)) %>% count()))
            row.names(n) <- "Total N"
            
            #create race rows
            race <- lists_orders_zip_hs_df %>% filter(!! rlang::parse_expr(filter_string)) %>%
              count(stu_race_cb) %>% mutate(V1 = n / sum(n) * 100)
            
            race <- race %>% select(stu_race_cb, V1)
            race <- race %>% mutate(stu_race_cb = ifelse(is.na(stu_race_cb), "Pct Race-Missing", stu_race_cb),
                                    stu_race_cb = ifelse(stu_race_cb==0, "Pct Race-No Response", stu_race_cb),
                                    stu_race_cb = ifelse(stu_race_cb==1, "Pct AI/AN", stu_race_cb),
                                    stu_race_cb = ifelse(stu_race_cb==2, "Pct Asian", stu_race_cb),
                                    stu_race_cb = ifelse(stu_race_cb==3, "Pct Black", stu_race_cb),
                                    stu_race_cb = ifelse(stu_race_cb==4, "Pct Latinx", stu_race_cb),
                                    stu_race_cb = ifelse(stu_race_cb==8, "Pct NH/PI", stu_race_cb),
                                    stu_race_cb = ifelse(stu_race_cb==9, "Pct White", stu_race_cb),
                                    stu_race_cb = ifelse(stu_race_cb==10, "Pct Other Race", stu_race_cb),
                                    stu_race_cb = ifelse(stu_race_cb==12, "Pct Multiracial", stu_race_cb)                                        )
            
            
            race<- race %>% remove_rownames %>% column_to_rownames(var="stu_race_cb")
            
            
            #create gender row
            #gender <- lists_orders_zip_hs_df %>% filter(!! rlang::parse_expr(filter_string)) %>%
              #count(stu_race_cb) %>% mutate(V1 = n / sum(n) * 100)
            

            #create income row
            income <- as_data_frame (t(lists_orders_zip_hs_df %>% filter(!! rlang::parse_expr(filter_string)) %>% 
                                         summarise (mean_inc = mean(zip_median_household_income, na.rm=T))))
            row.names(income) <- "Median Household Income (mean)"
            
            # create in-state versus out-of-state rows
            oos <- lists_orders_zip_hs_df %>% filter(!! rlang::parse_expr(filter_string)) %>%
              count(stu_nonres) %>% mutate(V1 = n / sum(n) * 100)
            
            oos <- oos %>% select(stu_nonres, V1)
            oos <- oos %>% mutate(stu_nonres = ifelse(is.na(stu_nonres), "Pct- Residency Missing", stu_nonres),
                                  stu_nonres = ifelse(stu_nonres==0, "Pct In-State", stu_nonres),
                                  stu_nonres = ifelse(stu_nonres==1, "Pct Out-of-State", stu_nonres))
                                  
            oos<- oos %>% remove_rownames %>% column_to_rownames(var="stu_nonres")
            
            
            #create school type rows
            schtype <- lists_orders_zip_hs_df %>% filter(!! rlang::parse_expr(filter_string)) %>%
              count(hs_school_control) %>% mutate(school_type = n / sum(n) * 100)
            
            schtype <- schtype %>% select(hs_school_control, school_type)
            schtype <- schtype %>% mutate(hs_school_control = ifelse(is.na(hs_school_control), "school unknown", hs_school_control))
            schtype<- schtype %>% remove_rownames %>% column_to_rownames(var="hs_school_control")
            schtype<- schtype %>% rename(V1 = school_type)
            row.names(schtype) <- c("Pct Private", "Pct Public", "Pct School Unknown") #NEED TO RE_DO THIS LIKE RACE ABOVE
            
            
            #concatenate all row_dfs for i-column
            temp_df <- bind_rows(mget(variables))
            #temp_df <- bind_rows(n, race, income, schtype)
            temp_df <- temp_df %>% rename(!!paste0("", i) := V1)
            temp_df <- rownames_to_column(temp_df, "row_subj")
            
            
            #first loop creates the master_df
            #second , loops appends the master df
            if(counter==1){master_df <- as.data.frame(temp_df)}
            if(counter>1){master_df <- merge(master_df,temp_df, by="row_subj", sort=FALSE)}
            
            
          }
          
          return(master_df)
          
        }
        
        
        
        # CALL FUNCTION TO CREATE TABLE 2
        
        #all possible vars: n, race, income, oos, schtype
        vars <- c("n", "race", "income", "oos","schtype") #all possible vars: n, race, income, schtype
        
        #all possible columns: all_domestic, in_state, out_of_state, research_univ, regional_univ, research_univ_instate, research_univ_outofstate, regional_univ_instate, regional_univ_outofstate,
        cols <- c("all_domestic", "GPA", "PSAT", "SAT","HS RANK", "RACE", "GENDER", "ZIP", "STATE","SEGMENT", "CBSA") 
        df_rq3<- table_rq3(vars, cols) 
        
        
        #format table
        # df_rq3 <- df_rq3 %>% mutate_if(is.numeric, round, 0)
        # df_rq3 <- df_rq3 %>%  mutate_each(funs(prettyNum(., big.mark=",")))

        
        
################### ANALYSIS & VISUALS FOR RQ3: ZIP CODE & TEST SCORES-- OUT-OF-STATE LA METRO for ASU Example

        
        # create categorical variable that use different combos of filters
        lists_orders_zip_hs_df <- lists_orders_zip_hs_df %>% 
                          mutate(filter_combo = ifelse(filter_hsgrad_class==1 & filter_zip==1 & filter_psat==1 & filter_gpa==1, "HS Grad, Zip, PSAT, GPA", NA),
                                 filter_combo = ifelse(filter_hsgrad_class==1 & filter_zip==1 & filter_sat==1 & filter_gpa==1, "HS Grad, Zip, SAT, GPA", filter_combo),
                                 filter_combo = ifelse(filter_hsgrad_class==1 & filter_states_fil==1 & filter_race==1 & filter_sat==1 & filter_psat==1 & filter_gpa==1 & filter_rank==1, "HS Grad, State, Race, SAT, PSAT, GPA, Rank", filter_combo),
                                 filter_combo = ifelse(filter_hsgrad_class==1 & filter_zip==1 & filter_sat==1 & filter_psat==1 & filter_gpa==1, "HS Grad, Zip, SAT, PSAT, GPA", filter_combo),
                                 filter_combo = ifelse(filter_hsgrad_class==1 & filter_states_fil==1 & filter_sat==1 & filter_gpa==1, "HS Grad, State, SAT, GPA", filter_combo),
                                 filter_combo = ifelse(filter_hsgrad_class==1 & filter_states_fil==1 & filter_psat==1 & filter_gpa==1, "HS Grad, State, PSAT, GPA", filter_combo),
                                 filter_combo = ifelse(filter_hsgrad_class==1 & filter_states_fil==1 & filter_race==1 & filter_psat==1 & filter_gpa==1, "HS Grad, State, Race, PSAT, GPA", filter_combo),
                                 filter_combo = ifelse(filter_hsgrad_class==1 & filter_states_fil==1 & filter_segment==1 & filter_gender==1 & filter_sat==1 & filter_gpa==1, "HS Grad, State, Segment, Gender, SAT, GPA", filter_combo))
    
        
        # filter for ASU orders; ASU lists
        orders_asu <-  orders_df %>% filter(univ_id==104151)
        lists_asu <-  lists_orders_zip_hs_df %>% filter(univ_id==104151)
        
        #looks at filters across ASU orders
        orders_asu %>% count(order_title) %>% print(n=131)
        
        #orders targeting California
        orders_asu %>% count(state_name, hs_grad_class)  
        orders_asu %>% count(state_name, hs_grad_class, geomarket)  #no geomarkets
        orders_asu %>% filter(state_name=="California") %>% count(order_title,low_ses)  %>% print(n=50)
        orders_asu %>% filter(state_name=="California") %>% count(order_num, order_title, hs_grad_class, sat_score_min, sat_score_max, low_ses)  %>% print(n=50)
                
              # range of PSAT scores used for CA orders without SES filter
              orders_asu %>% count(state_name)
              orders_asu %>% filter(state_name=="California" & is.na(low_ses)) %>% count(hs_grad_class)
              orders_asu %>% filter(state_name=="California" & is.na(low_ses)) %>% count(psat_score_min, sat_score_min)
              orders_asu %>% filter(state_name=="California" & is.na(low_ses)) %>% count(psat_score_min, psat_score_max)
      
              # range of SAT scores used for CA orders without SES filter
              orders_asu %>% filter(state_name=="California" & is.na(low_ses)) %>% count(sat_score_min, sat_score_max)
              
        #top metros by prospects purchased
        orders_asu %>% filter(state_name=="California" & is.na(low_ses)) %>% summarize(total= sum(num_students))
              
        lists_asu %>% count(zip_cbsatitle_1, zip_cbsa_1) %>% arrange (-n) #Los Angeles is second metro; NY is first but its across 3 states

        # filter for ASU orders and ASU lists IN LOS ANGELES
        lists_asu <-  lists_asu %>% filter(zip_cbsa_1==31080)
        lists_orders <-  lists_asu %>% count(ord_num)
        lists_asu %>% count(ord_num) %>% arrange(-n) #largest number of LA prospects are coming from order number 366935 (n=14875)
        orders_asu_la  <-   subset(orders_asu, order_num %in% lists_orders$ord_num)
        
          # top orders by number of prospects; top 10 orders by num of LA prospects using PSAT scores 
          lists_asu %>% count(ord_num, ord_sat_score_min, ord_sat_score_max) %>% arrange(-n) %>% print(n=25)
          lists_asu %>% count(ord_num, ord_hs_grad_class, ord_psat_score_min, ord_psat_score_max) %>% arrange(-n)
          
        
        #ORDER TO PICK FROM LA BASED ON THE MOST NUM OF PROSPECT
        orders_asu_la %>% filter(order_num=="366935") %>% count(state_name, hs_grad_class, geomarket, low_ses)
        orders_asu_la %>% filter(order_num=="366935") %>% count(state_name, hs_grad_class, sat_score_min, sat_score_max)
        orders_asu_la %>% filter(order_num=="366935") %>% count(state_name, hs_grad_class, psat_score_min, psat_score_max)
        orders_asu_la %>% filter(order_num=="366935") %>% count(state_name, hs_grad_class, psat_score_old_min, psat_score_old_max)
        
      
        
  # ##### COMPARE LA PROSPECTS TO THEIR HOME ZIP CODES
  #     
  #        
  #       # prospect home zips == come from 353 unique zip codes in LA
  #       lists_asu_la %>% count(stu_zip_code) %>% 
  #         mutate(V1 = n / sum(n) * 100) 
  #       
  #       # average racial chars across LA propects
  #       lists_asu_la %>% count(stu_race_cb) %>% 
  #         mutate(V1 = n / sum(n) * 100) 
  #       
  #       # average racial chars of prospects by zip
  #       lists_asu_la_race <- lists_asu_la %>% group_by(stu_zip_code) %>% count(stu_race_cb) %>% 
  #         mutate(V1 = n / sum(n) * 100) 
  #       
  #       lists_asu_la_race <- lists_asu_la_race %>% mutate(stu_race_cb = as.character(stu_race_cb),
  #                                                 stu_race_cb = ifelse(stu_race_cb=="0", "NoResponse", stu_race_cb),
  #                                                 stu_race_cb = ifelse(stu_race_cb=="1", "AIAN", stu_race_cb),
  #                                                 stu_race_cb = ifelse(stu_race_cb=="2", "Asian", stu_race_cb),
  #                                                 stu_race_cb = ifelse(stu_race_cb=="3", "Black", stu_race_cb),
  #                                                 stu_race_cb = ifelse(stu_race_cb=="4", "Latinx", stu_race_cb),
  #                                                 stu_race_cb = ifelse(stu_race_cb=="8", "NHPI", stu_race_cb),
  #                                                 stu_race_cb = ifelse(stu_race_cb=="9", "White", stu_race_cb),
  #                                                 stu_race_cb = ifelse(stu_race_cb=="10", "OtherRace", stu_race_cb),
  #                                                 stu_race_cb = ifelse(stu_race_cb=="12", "Multiracial", stu_race_cb))
  #       
  #       
  #       lists_asu_la_race <- lists_asu_la_race %>% select(stu_zip_code, stu_race_cb, V1) %>% pivot_wider(names_from = stu_race_cb, values_from = V1)
  #       lists_asu_la_race$population <- "prospects"
  #       lists_asu_la_race[is.na(lists_asu_la_race)] <- 0
  #       
  #       
  #       # average racial chars of zips
  #       asu_zips <- lists_asu_la %>% count(stu_zip_code)
  #       
  #       la_zips <- subset(acs_race_zipcodev3, zip_code %in% asu_zips$stu_zip_code)
  #       la_zips <- la_zips %>% select(zip_code, pop_white_15_19_pct, pop_black_15_19_pct, pop_asian_15_19_pct, pop_hispanic_15_19_pct, pop_amerindian_15_19_pct, pop_nativehawaii_15_19_pct, pop_tworaces_15_19_pct, pop_otherrace_15_19_pct)
  #       names(la_zips) <- c('stu_zip_code', 'White', 'Black', 'Asian', 'Latinx', 'AIAN', 
  #                              'NHPI','Multiracial' , 'OtherRace')
  #       
  #       la_zips$population <- "zip_population"
  #       
  #       
  #       #merge zip and prospects race chars
  #       zip_figure_race <- rbind(lists_asu_la_race, la_zips)
  #       
  #       zip_figure_race <- zip_figure_race %>% 
  #         gather(race, pct, -stu_zip_code, -population)
  #       
  #       zip_figure_race <- zip_figure_race %>% group_by(population,race) %>% 
  #         summarize(mean(pct, na.rm = TRUE)) 
  #       
  #       ggplot(zip_figure_race, aes(fill=population, y=`mean(pct, na.rm = TRUE)`, x=race)) , 
  #         geom_bar(position="dodge", stat="identity")
  # 
  #    
    ##### COMPARE LA PROSPECTS TO HYPOTHETICAL ZIP CODE LISTS
        
        lists_asu <- lists_asu %>% mutate(stu_race_cb = as.character(stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="0", "NoResponse", stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="1", "AIAN", stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="2", "Asian", stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="3", "Black", stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="4", "Latinx", stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="8", "NHPI", stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="9", "White", stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="10", "OtherRace", stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="12", "Multiracial", stu_race_cb))
        
        
        
        # ALL LOS ANGELES prospect home zips == come from 355 unique zip codes in LA
        lists_asu_la %>% count(stu_zip_code) %>% 
          mutate(V1 = n / sum(n) * 100) 
        
        #get all LA CBSA zips
        la_zips <- acs_race_zipcodev3 %>% filter(cbsa_1=="31080") #378 zips in LA
        
        #get all four-orders we'll be using
        lists_asu_la_psat_low <-  lists_asu %>% filter(ord_num=="366935") #psat score from 1110-1220
        lists_asu_la_psat_med <-  lists_asu %>% filter(ord_num=="448420") #psat score from 1190-1260
        lists_asu_la_psat_high <-  lists_asu %>% filter(ord_num=="448374") #psat score from 1270-1520
        lists_asu_la_sat_med <-  lists_asu %>% filter(ord_num=="394956") #sat score 1140-1260
        
        
        #sort by top 10% (38 out of 378) zips by income
        la_zips_top10pct <- la_zips %>% arrange(-median_household_income) %>% select(zip_code,median_household_income) %>% head(n=38) 
        la_zips_bottom90pct <- la_zips %>% arrange(-median_household_income) %>% select(zip_code,median_household_income) %>% tail(n=340) 
        
      #top 10%zip dummy in student lists
        lists_asu_la_psat_low <- lists_asu_la_psat_low %>% 
          mutate(top10pctzip = ifelse(stu_zip_code %in% la_zips_top10pct$zip_code, "top 10% zip", "not top 10% zip"))
        
        lists_asu_la_psat_low %>% 
          count(top10pctzip)
        
        lists_asu_la_psat_med <- lists_asu_la_psat_med %>% 
          mutate(top10pctzip = ifelse(stu_zip_code %in% la_zips_top10pct$zip_code, "top 10% zip", "not top 10% zip"))
        
        lists_asu_la_psat_med %>% 
          count(top10pctzip)
        
        lists_asu_la_psat_high <- lists_asu_la_psat_high %>% 
          mutate(top10pctzip = ifelse(stu_zip_code %in% la_zips_top10pct$zip_code, "top 10% zip", "not top 10% zip"))
        
        lists_asu_la_psat_high %>% 
          count(top10pctzip)
        
        lists_asu_la_sat_med <- lists_asu_la_sat_med %>% 
          mutate(top10pctzip = ifelse(stu_zip_code %in% la_zips_top10pct$zip_code, "top 10% zip", "not top 10% zip"))
        
        lists_asu_la_sat_med %>% 
          count(top10pctzip)
        
        
        #race/ethnicity by top 10% zips across 4 orders
        lists_asu_la_psat_low_race <- lists_asu_la_psat_low %>% group_by(top10pctzip) %>% count(stu_race_cb) %>% 
          mutate(V1 = n / sum(n) * 100) 
        
        lists_asu_la_psat_med_race <- lists_asu_la_psat_med %>% group_by(top10pctzip) %>% count(stu_race_cb) %>% 
          mutate(V1 = n / sum(n) * 100) 
        
        lists_asu_la_psat_high_race <- lists_asu_la_psat_high %>% group_by(top10pctzip) %>% count(stu_race_cb) %>% 
          mutate(V1 = n / sum(n) * 100) 
        
        lists_asu_la_sat_med_race <- lists_asu_la_sat_med %>% group_by(top10pctzip) %>% count(stu_race_cb) %>% 
          mutate(V1 = n / sum(n) * 100) 
        
        #just a plot to check patterns
        ggplot(lists_asu_la_sat_med_race, aes(fill=top10pctzip, y=V1, x=stu_race_cb)) + 
          geom_bar(position="dodge", stat="identity")
        
        
        
        #income by top 10% zips across 4 orders
        lists_asu_la_psat_low_inc <- lists_asu_la_psat_low %>% group_by(top10pctzip) %>% 
          summarize(
            median_income = mean(zip_median_household_income, na.rm = TRUE)) 
        
        lists_asu_la_psat_med_inc <- lists_asu_la_psat_med %>% group_by(top10pctzip) %>% 
          summarize(
            median_income = mean(zip_median_household_income, na.rm = TRUE)) 
        
        lists_asu_la_psat_high_inc <- lists_asu_la_psat_high %>% group_by(top10pctzip) %>% 
          summarize(
            median_income = mean(zip_median_household_income, na.rm = TRUE)) 
        
        lists_asu_la_sat_med_inc <- lists_asu_la_sat_med %>% group_by(top10pctzip) %>% 
          summarize(
            median_income = mean(zip_median_household_income, na.rm = TRUE)) 
        
        ggplot(lists_asu_la_sat_med_inc, aes(y=median_income, x=top10pctzip)) + 
          geom_bar(position="dodge", stat="identity")
        
        
        
        
        
        #sort by top 20% zips by income
        la_zips_top20pct <- la_zips %>% arrange(-median_household_income) %>% select(zip_code,median_household_income) %>% head(n=76) 
        
   
        #sort by top 20% (76 out of 378) zips by income
        la_zips_top20pct <- la_zips %>% arrange(-median_household_income) %>% select(zip_code,median_household_income) %>% head(n=76) 
        
        #top 20 zip dummy in student lists
        lists_asu_la <- lists_asu_la %>% 
          mutate(top20pctzip = ifelse(stu_zip_code %in% la_zips_top20pct$zip_code, "top zip", "not top zip"))
        
        lists_asu_la %>% 
          count(top20pctzip)
        
        #race/ethnicity & income by top 20 zips
        top20pctzips_figure <- lists_asu_la %>% group_by(top20pctzip) %>% count(stu_race_cb) %>% 
          mutate(V1 = n / sum(n) * 100) 
        
        ggplot(top20pctzips_figure, aes(fill=top20pctzip, y=V1, x=stu_race_cb)) +
          geom_bar(position="dodge", stat="identity")
        
        top20pctzips_figure <- lists_asu_la %>% group_by(top20pctzip) %>% 
          summarize(
            median_income = mean(zip_median_household_income, na.rm = TRUE)) 
        
        ggplot(top20pctzips_figure, aes(y=median_income, x=top20pctzip)) + 
          geom_bar(position="dodge", stat="identity")
        
        
        
        
        
        
        
        #sort by top 20% zips by income
        la_zips_top10pct <- la_zips %>% arrange(-median_household_income) %>% select(zip_code,median_household_income) %>% head(n=38) 
        
        
        #sort by top 20% (76 out of 378) zips by income
        la_zips_top10pct <- la_zips %>% arrange(-median_household_income) %>% select(zip_code,median_household_income) %>% head(n=38) 
        
        #top 20 zip dummy in student lists
        lists_asu_la <- lists_asu_la %>% 
          mutate(top10pctzip = ifelse(stu_zip_code %in% la_zips_top10pct$zip_code, "top zip", "not top zip"))
        
        lists_asu_la %>% 
          count(top10pctzip)
        
        #race/ethnicity & income by top 20 zips
        top10pctzips_figure <- lists_asu_la %>% group_by(top10pctzip) %>% count(stu_race_cb) %>% 
          mutate(V1 = n / sum(n) * 100) 
        
        ggplot(top10pctzips_figure, aes(fill=top10pctzip, y=V1, x=stu_race_cb)) + 
          geom_bar(position="dodge", stat="identity")
        
        top10pctzips_figure <- lists_asu_la %>% group_by(top10pctzip) %>% 
          summarize(
            median_income = mean(zip_median_household_income, na.rm = TRUE)) 
        
        ggplot(top10pctzips_figure, aes(y=median_income, x=top10pctzip)) + 
          geom_bar(position="dodge", stat="identity")
        
        
        
################### ANALYSIS & VISUALS FOR RQ3: ZIP CODE & TEST SCORES-- IN-STATE/ZIP TEXAS A&M Texerkana Example
        
         #### ZOOM INTO TEXAS A&M ZIP CODE ORDERS
       
         # average out racial chars across orders using zip filters by Texas A&M Texerkana
        texasam <- lists_orders_zip_hs_df %>% filter(univ_id=="224545" & filter_combo=="HS Grad, Zip, PSAT, GPA")
        
        texasam %>% count(ord_zip_code)
        texasam %>% count(ord_zip_code, ord_num)
        
        texasam <- texasam %>% mutate(order_type_zips = recode(ord_zip_code, 
                                         "754|717|747|719|712|762|711|710|758|759|757" = "1",
                                         "754|773|770|774|775|762|758|759|757|717|747|719|712|711|710" = "2",
                                         "755|752|718|750|760|751|761|756" = "3",
                                         "773|770|774|775" = "4",
                                         .default = NA_character_))
        
        texasam %>% count(ord_zip_code, order_type_zips)
        
            # how many orders using this combo?
            texasam %>% filter(filter_combo=="HS Grad, Zip, PSAT, GPA") %>% summarise(n=n_distinct(ord_num)) 
            
            # descriptives on filters
            texasam %>% filter(filter_combo=="HS Grad, Zip, PSAT, GPA") %>% count(ord_hs_grad_class)
            texasam %>% filter(filter_combo=="HS Grad, Zip, PSAT, GPA") %>% count(ord_psat_score_max)
            texasam %>% filter(filter_combo=="HS Grad, Zip, PSAT, GPA") %>% count(ord_psat_score_min)
            texasam %>% filter(filter_combo=="HS Grad, Zip, PSAT, GPA") %>% count(ord_gpa_low)
            texasam %>% filter(filter_combo=="HS Grad, Zip, PSAT, GPA") %>% count(ord_gpa_high)
            
            #number of orders within each zip code grouping
            texasam %>% group_by(order_type_zips) %>% summarise(n=n_distinct(ord_num)) 
            
            
            texasam %>% filter(filter_combo=="HS Grad, Zip, PSAT, GPA") %>% count(order_type_zips, ord_psat_score_max)
            
        # racial characteristics
         texasam %>% group_by(order_type_zips) %>%
                count(stu_race_cb) %>% mutate(V1 = n / sum(n) * 100) %>% print(n=50)
        
        

      # Get ZIPCODE Characteristics 
         
         #switch zip to character
        acs_race_zipcodev3 <- acs_race_zipcodev3 %>% mutate(
                zip_char = as.character(zip_code)
              )
        
       texasam_zips_order1 <-  dplyr::filter(acs_race_zipcodev3, grepl('^710|^711|^712|^717|^719|^747|^754|^758|^759|^757|^762', zip_char))
       texasam_zips_order2 <-  dplyr::filter(acs_race_zipcodev3, grepl('^710|^711|^712|^717|^719|^747|^754|^757|^758|^759|^762|^770|^773|^774|^775', zip_char))
       texasam_zips_order3 <-  dplyr::filter(acs_race_zipcodev3, grepl('^718|^750|^751|^752|^755|^756|^760|^761', zip_char))
       texasam_zips_order4 <-  dplyr::filter(acs_race_zipcodev3, grepl('^770|^773|^774|^775', zip_char))
       texasam_zips_orderall <-  dplyr::filter(acs_race_zipcodev3, grepl('^710|^711|^712|^717|^718|^719|^747|^750|^751|^752|^754|^755|^756|^757|^758|^759|^760|^761|^762|^770|^773|^774|^775', zip_char))
       
        
       
       
       
       # create vars for zip codes at 3-digit
       texasam <- texasam %>% mutate(
         zip_3digit = str_sub(stu_zip_code, 1, 3)  
       )
        
       texasam_zips_orderall <- texasam_zips_orderall %>% mutate(
         zip_3digit = str_sub(zip_char, 1, 3)  
       )
       
       
      stu_zips_race <- texasam %>% filter(zip_3digit!="060" & zip_3digit!="201" & zip_3digit!="274" & zip_3digit!="301" & zip_3digit!="303" & zip_3digit!="778" & zip_3digit!="780" & zip_3digit!="781" & zip_3digit!="786" & zip_3digit!="800" & zip_3digit!="804" & zip_3digit!="917" & zip_3digit!="953") %>%
        group_by(zip_3digit) %>%
         count(stu_race_cb) %>% mutate(V1 = n / sum(n) * 100) #%>% print(n=50)
      
      
      stu_zips_race <- as.data.frame(stu_zips_race)
      
      
                          # NEED TO EXPLORE THESE IN JANUARY-- but % is MINIMAL
                          stu_zips_race <- stu_zips_race %>% mutate(stu_race_cb= as.character(unclass(stu_race_cb)))
                          #stu_zips <- stu_zips %>% filter(stu_race_cb>=0) # IDK where the NA came from
                          
                          
                          #Can't get this to work to apply labels
                          # stu_zips_race <- stu_zips_race %>% mutate(
                          #   zip_char = as.character(zip_code)
                          # )
                          
                          stu_zips_race <- stu_zips_race %>% select(-n)
                          stu_zips_race <-  rename(stu_zips_race, stu_pct=V1)
                          
                          stu_zips_race <- stu_zips_race %>% mutate(stu_race_cb = ifelse(stu_race_cb=="0", "NoResponse", stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="1", "AIAN", stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="2", "Asian", stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="3", "Black", stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="4", "Latinx", stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="8", "NHPI", stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="9", "White", stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="10", "OtherRace", stu_race_cb),
                                                          stu_race_cb = ifelse(stu_race_cb=="12", "Multiracial", stu_race_cb))
           
                 
                          
                          
        # NOTE  ZIPS less than 750 are out of state             
       
      pop_zips_race <- texasam_zips_orderall %>% 
        group_by(zip_3digit) %>%
        summarize(
          #n_obs = sum(n()),
          pop_pct.White =  mean(pop_white_15_19_pct, na.rm = TRUE),
          pop_pct.Asian =  mean(pop_asian_15_19_pct, na.rm = TRUE),
          pop_pct.Black =  mean(pop_black_15_19_pct, na.rm = TRUE),
          pop_pct.Latinx =  mean(pop_hispanic_15_19_pct, na.rm = TRUE),
          #pct_pop_amerindian =  mean(pop_amerindian, na.rm = TRUE)*100,
          #pct_pop_nativehawaii =  mean(pop_nativehawaii, na.rm = TRUE)*100,
          pop_pct.AIAN =  mean(pop_amerindian_15_19_pct, na.rm = TRUE),
          pop_pct.Multiracial =  mean(pop_tworaces_15_19_pct, na.rm = TRUE),
          #pop_med.inc = mean(median_household_income, na.rm = TRUE)
        )   
      
      # reshape pop wide to long, drops NA stu_race_cb
      pop_zips_race <- pop_zips_race %>% gather(stu_race_cb, pop_pct, -c(zip_3digit))
      pop_zips_race <- pop_zips_race %>% mutate_all(~gsub("pop_pct.", "", .))
      
      
      # merge by three digit zip CREATE FIGURE OBJECT
      table_texasam_zip <- merge(stu_zips_race, pop_zips_race, by=c("zip_3digit", "stu_race_cb"))
      table_texasam_zip$pop_pct <- as.numeric( table_texasam_zip$pop_pct)
      table_texasam_zip <- table_texasam_zip %>% mutate_if(is.numeric, round, 0)
      table_texasam_zip <- table_texasam_zip %>%  mutate_each(funs(prettyNum(., big.mark=",")))
      
      table_texasam_zip <- table_texasam_zip %>%  mutate(
                              ppt_diff_stu_pop = as.numeric(stu_pct) - as.numeric(pop_pct))
      
      table_texasam_zip <- table_texasam_zip %>% arrange(stu_race_cb, ppt_diff_stu_pop)
      
      # NOTE  ZIPS less than 750 are out of state             
      ggplot(table_texasam_zip, aes(fill=stu_race_cb, y=ppt_diff_stu_pop, x=zip_3digit)) + 
        geom_bar(position="dodge", stat="identity") + coord_flip()
      
      
      
      
      # median income of zip codes fro student prospects versus population
      
          # economic characteristics of population at zip
          pop_zip_inc <- texasam_zips_orderall %>% 
              group_by(zip_3digit) %>%
              summarize(
                pop_med_inc = mean(median_household_income, na.rm = TRUE)
              )   
      
          # economic characteristics of prospects
          stu_zip_inc <-texasam %>% group_by(zip_3digit) %>% 
            summarise (stu_mean_inc = mean(zip_median_household_income, na.rm=T))
          
          table_texasam_zip_inc <- merge(stu_zip_inc, pop_zip_inc, by="zip_3digit")
          
      # EXPLORATORY ANALYSIS BY AVERAGING ACROSS ORDER GROUPINGS
       #  # racial & economic characteristics by filter order for texas a&m 
       # texasam_zips_orderall %>% 
       #   summarize(
       #     n_obs = sum(n()),
       #     pct_pop_white =  mean(pop_white_15_19_pct, na.rm = TRUE),
       #     pct_pop_asian =  mean(pop_asian_15_19_pct, na.rm = TRUE),
       #     pct_pop_black =  mean(pop_black_15_19_pct, na.rm = TRUE),
       #     pct_pop_hispanic =  mean(pop_hispanic_15_19_pct, na.rm = TRUE),
       #     #pct_pop_amerindian =  mean(pop_amerindian, na.rm = TRUE)*100,
       #     #pct_pop_nativehawaii =  mean(pop_nativehawaii, na.rm = TRUE)*100,
       #     pct_pop_native =  mean(pop_amerindian_15_19_pct, na.rm = TRUE),
       #     pct_pop_tworaces =  mean(pop_tworaces_15_19_pct, na.rm = TRUE),
       #     avg_med_inc = mean(median_household_income, na.rm = TRUE)
       #   )   
       # 
       # 
       # texasam_zips_order1 %>% 
       #    summarize(
       #      n_obs = sum(n()),
       #      pct_pop_white =  mean(pop_white_15_19_pct, na.rm = TRUE),
       #      pct_pop_asian =  mean(pop_asian_15_19_pct, na.rm = TRUE),
       #      pct_pop_black =  mean(pop_black_15_19_pct, na.rm = TRUE),
       #      pct_pop_hispanic =  mean(pop_hispanic_15_19_pct, na.rm = TRUE),
       #      #pct_pop_amerindian =  mean(pop_amerindian, na.rm = TRUE)*100,
       #      #pct_pop_nativehawaii =  mean(pop_nativehawaii, na.rm = TRUE)*100,
       #      pct_pop_native =  mean(pop_amerindian_15_19_pct, na.rm = TRUE),
       #      pct_pop_tworaces =  mean(pop_tworaces_15_19_pct, na.rm = TRUE),
       #      avg_med_inc = mean(median_household_income, na.rm = TRUE)
       #    )   
       # 
       # texasam_zips_order2 %>% 
       #   summarize(
       #     n_obs = sum(n()),
       #     pct_pop_white =  mean(pop_white_15_19_pct, na.rm = TRUE),
       #     pct_pop_asian =  mean(pop_asian_15_19_pct, na.rm = TRUE),
       #     pct_pop_black =  mean(pop_black_15_19_pct, na.rm = TRUE),
       #     pct_pop_hispanic =  mean(pop_hispanic_15_19_pct, na.rm = TRUE),
       #     #pct_pop_amerindian =  mean(pop_amerindian, na.rm = TRUE)*100,
       #     #pct_pop_nativehawaii =  mean(pop_nativehawaii, na.rm = TRUE)*100,
       #     pct_pop_native =  mean(pop_amerindian_15_19_pct, na.rm = TRUE),
       #     pct_pop_tworaces =  mean(pop_tworaces_15_19_pct, na.rm = TRUE),
       #     avg_med_inc = mean(median_household_income, na.rm = TRUE)
       #   )   
       # 
       # texasam_zips_order3 %>% 
       #   summarize(
       #     n_obs = sum(n()),
       #     pct_pop_white =  mean(pop_white_15_19_pct, na.rm = TRUE),
       #     pct_pop_asian =  mean(pop_asian_15_19_pct, na.rm = TRUE),
       #     pct_pop_black =  mean(pop_black_15_19_pct, na.rm = TRUE),
       #     pct_pop_hispanic =  mean(pop_hispanic_15_19_pct, na.rm = TRUE),
       #     #pct_pop_amerindian =  mean(pop_amerindian, na.rm = TRUE)*100,
       #     #pct_pop_nativehawaii =  mean(pop_nativehawaii, na.rm = TRUE)*100,
       #     pct_pop_native =  mean(pop_amerindian_15_19_pct, na.rm = TRUE),
       #     pct_pop_tworaces =  mean(pop_tworaces_15_19_pct, na.rm = TRUE),
       #     avg_med_inc = mean(median_household_income, na.rm = TRUE)
       #   ) 
       #  
       # 
       # texasam_zips_order4 %>% 
       #   summarize(
       #     n_obs = sum(n()),
       #     pct_pop_white =  mean(pop_white_15_19_pct, na.rm = TRUE),
       #     pct_pop_asian =  mean(pop_asian_15_19_pct, na.rm = TRUE),
       #     pct_pop_black =  mean(pop_black_15_19_pct, na.rm = TRUE),
       #     pct_pop_hispanic =  mean(pop_hispanic_15_19_pct, na.rm = TRUE),
       #     #pct_pop_amerindian =  mean(pop_amerindian, na.rm = TRUE)*100,
       #     #pct_pop_nativehawaii =  mean(pop_nativehawaii, na.rm = TRUE)*100,
       #     pct_pop_native =  mean(pop_amerindian_15_19_pct, na.rm = TRUE),
       #     pct_pop_tworaces =  mean(pop_tworaces_15_19_pct, na.rm = TRUE),
       #     avg_med_inc = mean(median_household_income, na.rm = TRUE)
       #   ) 
       # 
       
       
    ## PROSPECT CHARS ACROSS COMBOS of FILTERS-- IN-STATE/STATE FILTER EXAMPLE 
          
          lists_orders_zip_hs_df %>% 
            group_by(filter_combo) %>%
            summarise(n=n_distinct(ord_num)) 
          
          lists_orders_zip_hs_df %>% 
            filter(filter_combo=="HS Grad, State, Race, PSAT, GPA") %>%
            count(univ_name) %>% print(n=400)
          
          lists_orders_zip_hs_df %>% 
            filter(filter_combo=="HS Grad, State, Race, PSAT, GPA") %>%
            count(ord_state_name, univ_name) %>% print(n=400)
          
          # Texas A & M University-College Station OR UC San Diego?
          lists_orders_zip_hs_df %>% 
            filter(filter_combo=="HS Grad, State, Race, PSAT, GPA") %>%
            count(ord_state_name, univ_name) %>% print(n=400)
       
     
          
################### ANALYSIS & VISUALS FOR RQ3: ZIP CODE & TEST SCORES--  SERGMENT , TEST SCORES UNIV OF ILLINOIS URBANA-CHAMPAIGN Example
          
          # check filters across orders that use segment; these filter by segment at the state level but not cbsa
          lists_orders_zip_hs_df %>% filter(univ_id == '145637' & filter_segment==1 & filter_states_fil==1 &  filter_cbsa!=1 &  filter_psat==1) %>% count(ord_num)
          lists_orders_zip_hs_df %>% filter(univ_id == '145637' & filter_segment==1 & filter_states_fil==1 &  filter_cbsa!=1 & filter_psat==1) %>% count(ord_state_name)
          
          # check filters across orders that use segment; these filter by segment at the state & CBSA level
          ui_uc <-lists_orders_zip_hs_df %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1)
          lists_orders_zip_hs_df %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% count(ord_num)
          x1 <- lists_orders_zip_hs_df %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% select(ord_num|ord_cbsa_name|starts_with("filter_"))
          x1 <- distinct(x1, ord_num, .keep_all = TRUE)
            
            segment <- orders_df %>% filter(order_num %in% x1$ord_num)
              # see full list of cbsa across three groupings
              c(x1[1,2])
              c(x1[5,2])
              c(x1[6,2])
              
              # descriptives on filters
              ui_uc %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% count(ord_hs_grad_class)
              ui_uc %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% count(ord_psat_score_max)
              ui_uc %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% count(ord_psat_score_min)
              
              ui_uc %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% count(ord_sat_score_max)
              ui_uc %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% count(ord_sat_score_min)
              
              ui_uc %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% count(ord_gpa_low)
              ui_uc %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% count(ord_gpa_high)  
          
              ui_uc %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% count(ord_segment)
              x <- ui_uc %>% filter(univ_id == '145637' & filter_segment==1 & filter_cbsa==1 &  filter_psat==1) %>% count(ord_segment)  
              
                    # see full list of cbsa across three groupings
                    c(x[1,1])
        
        # Philadelphia-Camden-Wilmington, PA-NJ-DE-MD; 37980
        # students purchased across all three types of segment orders 
          lists_orders_zip_hs_df %>% filter(univ_id == '145637' & zip_cbsa_1 == '37980') %>% count()
              
          philly_studentlist <- lists_orders_zip_hs_df %>% filter(univ_id == '145637' & zip_cbsa_1 == '37980') %>%    
              count(stu_race_cb) %>% mutate(metro= "Philadelphia", pct = n / sum(n) * 100) #%>% print(n=50)
                    
          philly_studentlist <- philly_studentlist %>% mutate(
                                                    stu_race_cb = as.character(stu_race_cb),
                                                    stu_race_cb = ifelse(stu_race_cb=="0", "NoResponse", stu_race_cb),
                                                    stu_race_cb = ifelse(stu_race_cb=="1", "AIAN", stu_race_cb),
                                                    stu_race_cb = ifelse(stu_race_cb=="2", "Asian", stu_race_cb),
                                                    stu_race_cb = ifelse(stu_race_cb=="3", "Black", stu_race_cb),
                                                    stu_race_cb = ifelse(stu_race_cb=="4", "Latinx", stu_race_cb),
                                                    stu_race_cb = ifelse(stu_race_cb=="8", "NHPI", stu_race_cb),
                                                    stu_race_cb = ifelse(stu_race_cb=="9", "White", stu_race_cb),
                                                    stu_race_cb = ifelse(stu_race_cb=="10", "OtherRace", stu_race_cb),
                                                    stu_race_cb = ifelse(stu_race_cb=="12", "Multiracial", stu_race_cb))
          
          philly_studentlist <- philly_studentlist[, c("metro", "stu_race_cb", "pct")]
          philly_studentlist <- spread(philly_studentlist, key = c("stu_race_cb"), value = "pct")
          philly_studentlist$population <- "Prospects Purchased"
          philly_studentlist$tot_students <- (lists_orders_zip_hs_df %>% filter(univ_id == '145637' & zip_cbsa_1 == '37980') %>% count())$n
          philly_studentlist <- philly_studentlist[, c("metro", "population", "tot_students", "White", "Asian", "Black", "Latinx", "AIAN", "Multiracial")]
          
        
        # racial composition of all students in public high schools in the CBSA
          pubhs_privhs_data %>% count(cbsa_1, cbsatitle_1) %>% print(n=1000)
          
          philly_metro <- pubhs_privhs_data %>% filter(school_control == 'public',total_12>0, cbsa_1 == '37980') %>%
            summarize(
                metro = "Philadelphia",
                population = "Public HS Students",
                #n_obs = sum(total_students, na.rm = TRUE),
                tot_students = sum(total_students, na.rm = TRUE),
                #tot_white = sum(total_white, na.rm = TRUE),
                #tot_asian = sum(total_asian, na.rm = TRUE),
                White = sum(total_white, na.rm = TRUE)/tot_students*100,
                Asian = sum(total_asian, na.rm = TRUE)/tot_students*100,
                Black = sum(total_black, na.rm = TRUE)/tot_students*100,
                Latinx = sum(total_hispanic, na.rm = TRUE)/tot_students*100,
                AIAN = sum(total_native, na.rm = TRUE)/tot_students*100, # native american , alaska native , native hawaiaan , other pacific islander
                Multiracial = sum(total_tworaces, na.rm = TRUE)/tot_students*100,
                #pct_stu_unknown = sum(total_unknown, na.rm = TRUE)/tot_students*100,
                #pct_all = pct_white , pct_asian , pct_black , pct_hispanic , pct_native , pct_tworaces , pct_unknown
            )
        
        
        # New York; 35620
        # students purchased across all three types of segment orders 
        lists_orders_zip_hs_df %>% filter(univ_id == '145637' & zip_cbsa_1 == '35620') %>% count()
        
        ny_studentlist <- lists_orders_zip_hs_df %>% filter(univ_id == '145637' & zip_cbsa_1 == '35620') %>%    
          count(stu_race_cb) %>% mutate(metro= "New York", pct = n / sum(n) * 100) #%>% print(n=50)
        
        ny_studentlist <- ny_studentlist %>% mutate(
          stu_race_cb = as.character(stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="0", "NoResponse", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="1", "AIAN", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="2", "Asian", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="3", "Black", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="4", "Latinx", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="8", "NHPI", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="9", "White", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="10", "OtherRace", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="12", "Multiracial", stu_race_cb))
        
        ny_studentlist <- ny_studentlist[, c("metro", "stu_race_cb", "pct")]
        ny_studentlist <- spread(ny_studentlist, key = c("stu_race_cb"), value = "pct")
        ny_studentlist$population <- "Prospects Purchased"
        ny_studentlist$tot_students <- (lists_orders_zip_hs_df %>% filter(univ_id == '145637' & zip_cbsa_1 == '35620') %>% count())$n
        ny_studentlist <- ny_studentlist[, c("metro", "population", "tot_students", "White", "Asian", "Black", "Latinx", "AIAN", "Multiracial")]
        
        
        # racial composition of all students in public high schools in the CBSA
        ny_metro <- pubhs_privhs_data %>% filter(school_control == 'public',total_12>0, cbsa_1 == '35620') %>%
          summarize(
            metro = "New York",
            population = "Public HS Students",
            #n_obs = sum(total_students, na.rm = TRUE),
            tot_students = sum(total_students, na.rm = TRUE),
            #tot_white = sum(total_white, na.rm = TRUE),
            #tot_asian = sum(total_asian, na.rm = TRUE),
            White = sum(total_white, na.rm = TRUE)/tot_students*100,
            Asian = sum(total_asian, na.rm = TRUE)/tot_students*100,
            Black = sum(total_black, na.rm = TRUE)/tot_students*100,
            Latinx = sum(total_hispanic, na.rm = TRUE)/tot_students*100,
            AIAN = sum(total_native, na.rm = TRUE)/tot_students*100, # native american , alaska native , native hawaiaan , other pacific islander
            Multiracial = sum(total_tworaces, na.rm = TRUE)/tot_students*100,
            #pct_stu_unknown = sum(total_unknown, na.rm = TRUE)/tot_students*100,
            #pct_all = pct_white , pct_asian , pct_black , pct_hispanic , pct_native , pct_tworaces , pct_unknown
          )
        
        
        # Los Angeles; 31080
        # students purchased across all three types of segment orders 
        lists_orders_zip_hs_df %>% filter(univ_id == '145637' & zip_cbsa_1 == '31080') %>% count()
        
        la_studentlist <- lists_orders_zip_hs_df %>% filter(univ_id == '145637' & zip_cbsa_1 == '31080') %>%    
          count(stu_race_cb) %>% mutate(metro= "Los Angeles", pct = n / sum(n) * 100) #%>% print(n=50)
        
        la_studentlist <- la_studentlist %>% mutate(
          stu_race_cb = as.character(stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="0", "NoResponse", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="1", "AIAN", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="2", "Asian", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="3", "Black", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="4", "Latinx", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="8", "NHPI", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="9", "White", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="10", "OtherRace", stu_race_cb),
          stu_race_cb = ifelse(stu_race_cb=="12", "Multiracial", stu_race_cb))
        
        la_studentlist <- la_studentlist[, c("metro", "stu_race_cb", "pct")]
        la_studentlist <- spread(la_studentlist, key = c("stu_race_cb"), value = "pct")
        la_studentlist$population <- "Prospects Purchased"
        la_studentlist$tot_students <- (lists_orders_zip_hs_df %>% filter(univ_id == '145637' & zip_cbsa_1 == '31080') %>% count())$n
        la_studentlist <- la_studentlist[, c("metro", "population", "tot_students", "White", "Asian", "Black", "Latinx", "AIAN", "Multiracial")]
        
        
        # racial composition of all students in public high schools in the CBSA
        la_metro <- pubhs_privhs_data %>% filter(school_control == 'public',total_12>0, cbsa_1 == '31080') %>%
          summarize(
            metro = "Los Angeles",
            population = "Public HS Students",
            #n_obs = sum(total_students, na.rm = TRUE),
            tot_students = sum(total_students, na.rm = TRUE),
            #tot_white = sum(total_white, na.rm = TRUE),
            #tot_asian = sum(total_asian, na.rm = TRUE),
            White = sum(total_white, na.rm = TRUE)/tot_students*100,
            Asian = sum(total_asian, na.rm = TRUE)/tot_students*100,
            Black = sum(total_black, na.rm = TRUE)/tot_students*100,
            Latinx = sum(total_hispanic, na.rm = TRUE)/tot_students*100,
            AIAN = sum(total_native, na.rm = TRUE)/tot_students*100, # native american , alaska native , native hawaiaan , other pacific islander
            Multiracial = sum(total_tworaces, na.rm = TRUE)/tot_students*100,
            #pct_stu_unknown = sum(total_unknown, na.rm = TRUE)/tot_students*100,
            #pct_all = pct_white , pct_asian , pct_black , pct_hispanic , pct_native , pct_tworaces , pct_unknown
          )  
        
    fig_rq3_segment_race <- rbind(philly_studentlist, philly_metro, ny_studentlist, ny_metro, la_studentlist, la_metro)
        
        
        
    # income of prospects across all three metros [don't know how to incorporate income for the metro area]
    philly_studentlist_inc <- lists_orders_zip_hs_df %>% filter(univ_id == '145637' & zip_cbsa_1 == '37980') %>%
      summarise(stu_mean_inc = mean(zip_median_household_income, na.rm=T)) 
      
    ny_studentlist_inc <- lists_orders_zip_hs_df %>% filter(univ_id == '145637' & zip_cbsa_1 == '35620') %>%
      summarise(stu_mean_inc = mean(zip_median_household_income, na.rm=T))
    
    la_studentlist_inc <- lists_orders_zip_hs_df %>% filter(univ_id == '145637' & zip_cbsa_1 == '31080') %>%
      summarise(stu_mean_inc = mean(zip_median_household_income, na.rm=T))
    
    
    # 2019 ACS
        # Philadelphia = 74,533
        # New York = 83,160
        # LA metro =77,774

    fig_rq3_segment_race_inc <- fig_rq3_segment_race %>% 
      mutate(
        income = c(
          philly_studentlist_inc$stu_mean_inc, 74533,
          ny_studentlist_inc$stu_mean_inc, 83160,
          la_studentlist_inc$stu_mean_inc, 77774
        )
      )
        
        
################### ANALYSIS & VISUALS FOR RQ3: ZIP CODE & TEST SCORES-- Women in STEM Example for UC San Diego
    
    #check orders that used female
    orders_df %>% filter(gender=="Female") %>% count(univ_name,univ_id)
    

    
    
    
      # Focus on UC San Diego Orders
      orders_gender <- orders_df %>% filter(gender=="Female" & univ_id==110680)
      
        # see orders by Urbana Champaign
        #orders_gender_urbana <- orders_df %>% filter(gender=="Female" & univ_id==145637)
      
        # order titles
          orders_gender %>% count(order_title) #5 order for in-state, 6 for out-of-state
          orders_gender <- orders_gender %>% mutate(
            instate = ifelse(str_detect(order_title, "CA"), 1, 0)
          )
          
              # order titles for Urbana Champaign
              #orders_gender_urbana %>% count(order_title) #7 orders for OOS, all looking for "Female ENG"
        
      # instate versus outofstate
          orders_gender %>% count(instate) #5 order for in-state, 6 for out-of-state (Urbana had all OOS)
          
        # filters used
          
              # All filters
              orders_gender %>% group_by(instate) %>% count(gender, state_name,sat_score_min, sat_score_max, ap_scores, gpa_high, gpa_low, major) # Urbana had low of 1300/1310/1350; high 1600
          
              # GPA-- all orders used high A, and low of B (urbana did low of B-)
              orders_gender %>% group_by(instate) %>% count(gpa_high, gpa_low)
              #orders_gender_urbana %>% count(gpa_high, gpa_low)
              
              #for scores, they used wither SAT or AP
              orders_gender %>% group_by(instate) %>% count(sat_score_min, sat_score_max, ap_scores) # Urbana had low of 1300/1310/1350; high 1600
              
              
              #SAT
              orders_gender %>% group_by(instate) %>% count(sat_score_min, sat_score_max, ap_scores) # Urbana had low of 1300/1310/1350; high 1600
              #orders_gender_urbana %>% count(sat_score_min, sat_score_max) # Urbana had low of 1300/1310/1350; high 1600
              
              #For Field they used EITHER major OR AP scores
              orders_gender %>% group_by(instate) %>% count(major, ap_scores)
              orders_gender %>% count(ap_scores)
              
              #orders_gender_urbana %>%  count(major, ap_scores)
              #orders_gender_urbana %>%  count(segment)
              #orders_gender_urbana %>%  count(major)
              
              
              # two different types of order filters by AP Scores
              #exact same fields, except one set scores are filtered 4-5 another are 3-5
               # 1 type --- 2 type 
              "Biology~4~5 --- Biology~3~5
              Chemistry~4~5 --- Chemistry~3~5
              Computer Science A~4~5 --- Computer Science A~3~5
              CompSciP~4~5 --- CompSciP~3~5
              Environmental Science~4~5 --- Environmental Science~3~5
              Calculus AB~4~5 --- Calculus AB~3~5
              Calculus BC~4~5 --- Calculus BC~3~5
              Physics 1 ~4~5 --- Physics 1~3~5
              Physics 2 ~4~5 --- Physics 2~3~5
              Physics B ~4~5 --- Physics B~3~5
              Physics C: Electricity and Magnetism~4~5 --- Physics C: Electricity and Magnetism~3~5
              Physics C: Mechanics~4~5 --- Physics C: Mechanics~3~5
              Statistics~4~5 ---  Statistics~3~5" 
              
              #create score range var for AP filters
              
              orders_gender <- orders_gender %>% mutate(
                ap_score_range = ifelse(str_detect(ap_scores, "3~5"), "3~5", "4-5")
              )
              
              orders_gender %>% count(ap_scores, ap_score_range)
              orders_gender %>% group_by(instate) %>% count(ap_score_range) #in-state orders used only 3-5, both orders for 4-5 are out of state
              
              # how many students purchased
              orders_gender %>%  summarise(total_orders = n(),
                                           total_students = sum(num_students, na.rm = T))
              
              #only look at orders using AP-Scores for AP comparison group
              orders_gender_AP <- orders_gender %>% filter(!is.na(ap_scores)) 
              
              #only look at orders using SAT for AP comparison group
              orders_gender_SAT <- orders_gender %>% filter(!is.na(sat_score_max)) 
              
              
        # resulting student lists 
         list_gender  <-   subset(lists_orders_zip_hs_df, ord_num %in% orders_gender$order_num)
    
           # how many students purchased
            list_gender %>%  summarise(total_students = n())
            #list_gender %>% group_by(instate) %>% summarise(total_students = n())
            
            # unique IDs for order nums
            lists_orders <- list_gender %>%
              count(ord_num)  #ONLY have data for 8 orders??? OTHER THREE ORDERS HAD ZERO STUDENTS; so WE HAVE ALL ACCOMPANYING LISTS
            
            #which orders do I have lists for?
            orderswlists_gender  <-   subset(orders_gender, order_num %in% lists_orders$ord_num)
            orderswlists_gender %>% group_by(instate) %>% count(major, ap_scores)
                  # 5-outofstate: 3 use Major and 2 use AP scores 4-5
                  # 3 in-state: 1 uses Major and 2 use AP scores 3-5
            
            #orders by state
            list_gender %>% count(stu_state) %>% arrange(-n)
            
            list_gender %>% count(zip_cbsa_1, zip_cbsatitle_1) %>% arrange(-n) %>% print(n=30)
            list_gender %>% count(zip_csatitle,zip_cbsa_1) %>% arrange(-n) %>% print(n=40)
            
            # INTRO FIGURE-- PROSPECT CHARS ACROSS WOMEN IN STEM ORDERS FOR BOTH URBANA AND UC SANDIEGO
                
                    #unique orders
                    list_gender %>% group_by(ord_num, univ_id) %>% count()
                    list_gender %>% count()
                    #income
                    intro_womeninstem_inc <- list_gender %>% select(zip_median_household_income) %>%
                      summarise(across(
                        .cols = where(is.numeric), 
                        .fns = list(Mean = mean), na.rm = TRUE, 
                        .names = "{col}_{fn}"
                      )) %>% arrange(-zip_median_household_income_Mean) %>% print(n=100)
                   
                    # race
                    intro_womeninstem_race <- list_gender  %>%
                      count(stu_race_cb) %>% mutate(V1 = n / sum(n) * 100) %>% print(n=150)
                    
                    
                  #mean income by CBSA
                  list_gender %>% group_by(zip_cbsatitle_1) %>% select(zip_median_household_income) %>%
                    summarise(across(
                      .cols = where(is.numeric), 
                      .fns = list(Mean = mean), na.rm = TRUE, 
                      .names = "{col}_{fn}"
                    )) %>% arrange(-zip_median_household_income_Mean) %>% print(n=100)
            
                  
                #race/ethnicity by cbsa
                  list_gender %>%  filter(zip_cbsa_1=="35620"|zip_cbsa_1=="31080"|zip_cbsa_1=="12060"|zip_cbsa_1=="16980"|zip_cbsa_1=="42660"|zip_cbsa_1=="37980"|zip_cbsa_1=="14460"| zip_cbsa_1=="19820"| zip_cbsa_1=="35620") %>% group_by(zip_csatitle) %>%
                    count(stu_race_cb) %>% mutate(V1 = n / sum(n) * 100) %>% print(n=150)
                  
                  #race/ethnicity by cbsa for SAT orders
                  list_gender_SAT <- list_gender %>% filter(ord_num %in% orders_gender_SAT$order_num)
                  list_gender_SAT %>%  filter(zip_cbsa_1=="35620") %>% group_by(zip_csatitle) %>%
                    count(stu_race_cb) %>% mutate(V1 = n / sum(n) * 100) %>% print(n=150)
                  
                  list_gender_SAT %>%  filter(zip_cbsa_1=="12060") %>% group_by(zip_csatitle) %>%
                    count(stu_race_cb) %>% mutate(V1 = n / sum(n) * 100) %>% print(n=150)
                  
                  list_gender_SAT %>%  filter(zip_cbsa_1=="16980") %>% group_by(zip_csatitle) %>%
                    count(stu_race_cb) %>% mutate(V1 = n / sum(n) * 100) %>% print(n=150)
                  
                  #race/ethnicity by cbsa for AP orders
                  list_gender_AP <- list_gender %>% filter(ord_num %in% orders_gender_AP$order_num)
                  list_gender_AP %>%  filter(zip_cbsa_1=="35620") %>% group_by(zip_csatitle) %>%
                    count(stu_race_cb) %>% mutate(V1 = n / sum(n) * 100) %>% print(n=150)
                  
                  list_gender_AP %>%  filter(zip_cbsa_1=="16980") %>% group_by(zip_csatitle) %>%
                    count(stu_race_cb) %>% mutate(V1 = n / sum(n) * 100) %>% print(n=150)
                  
                  philly_lists_ap <- list_gender %>% filter(zip_cbsa_1=="37980")
                  philly_lists_ap %>% count(stu_race_cb)
                  
            
                  # CBSA public HS student chars
                  xyz_metro <- pubhs_privhs_data %>% filter(school_control == 'public',total_12>0, cbsa_1 == '37980') %>%
                    summarize(
                      metro = "Philadelphia",
                      population = "Public HS Students",
                      #n_obs = sum(total_students, na.rm = TRUE),
                      tot_students = sum(total_students, na.rm = TRUE),
                      #tot_white = sum(total_white, na.rm = TRUE),
                      #tot_asian = sum(total_asian, na.rm = TRUE),
                      White = sum(total_white, na.rm = TRUE)/tot_students*100,
                      Asian = sum(total_asian, na.rm = TRUE)/tot_students*100,
                      Black = sum(total_black, na.rm = TRUE)/tot_students*100,
                      Latinx = sum(total_hispanic, na.rm = TRUE)/tot_students*100,
                      AIAN = sum(total_native, na.rm = TRUE)/tot_students*100, # native american , alaska native , native hawaiaan , other pacific islander
                      Multiracial = sum(total_tworaces, na.rm = TRUE)/tot_students*100,
                      #pct_stu_unknown = sum(total_unknown, na.rm = TRUE)/tot_students*100,
                      #pct_all = pct_white , pct_asian , pct_black , pct_hispanic , pct_native , pct_tworaces , pct_unknown
                    )
                  
                  
      # ######## OLD ANALYSIS CODE      
      #  #income characteristics
      #       list_gender %>% group_by(instate) %>% count() #10.6k out of state versus 2.3k in-state
      #       
      #       list_gender %>% group_by(instate) %>% select(zip_median_household_income) %>%
      #         summarise(across(
      #           .cols = where(is.numeric), 
      #           .fns = list(Mean = mean, SD=sd), na.rm = TRUE, 
      #           .names = "{col}_{fn}"
      #         ))
      #       
      #     #race characteristics
      #       list_gender %>% group_by(instate) %>%
      #         count(stu_race_cb) %>% mutate(V1 = n / sum(n) * 100)
      #       
      #       list_gender %>% 
      #         count(stu_race_cb) %>% mutate(V1 = n / sum(n) * 100)
      #       
      #       
      #         #instate= 56% White; 30% Asian; 2% Black, 5% Latinx, 4%multiracial, 0% Native
      #         #outofstate= 32% White; 41% Asian; 1% Black, 14% Latinx, 6%multiracial, 0% Native
      #  
      #     # Zip Codes for one metro/one state
      #       
      #     #metro_zips <- acs_race_zipcodev3 %>% filter(cbsa_1=="19100") #BOSTON MSA=14460
      #     metro_zips <- acs_race_zipcodev3 %>% filter(state_code=="TX") 
      #   
      #     
      #     # aggregate num of prospects purchased at zip level for Boston
      #      #filter student lists for just those with gender filter from UCSD
      #     #list_gender_metro <-   subset(list_gender, stu_zip_code %in% metro_zips$zip_code) #only 314 students purchased in Boston
      #      
      #      list_gender_metro <-   list_gender %>% filter(stu_state=="TX") 
      #      
      #      list_gender_metro %>% count(stu_race_cb)
      # 
      #      #create dummies of race/ethnicity & order filters to aggregate
      #      list_gender_metro <- list_gender_metro %>% mutate(stu_race_missing = ifelse(is.na(stu_race_cb), 1, 0),
      #                                                              stu_race_noresponse = ifelse(stu_race_cb==0, 1, 0),
      #                                                              stu_race_aian = ifelse(stu_race_cb==1, 1, 0),
      #                                                              stu_race_asian = ifelse(stu_race_cb==2, 1, 0),
      #                                                              stu_race_black = ifelse(stu_race_cb==3, 1, 0),
      #                                                              stu_race_latinx = ifelse(stu_race_cb==4, 1, 0),
      #                                                              stu_race_nhpi = ifelse(stu_race_cb==8, 1, 0),
      #                                                              stu_race_white = ifelse(stu_race_cb==9, 1, 0),
      #                                                              stu_race_other = ifelse(stu_race_cb==10, 1, 0),
      #                                                              stu_race_multi = ifelse(stu_race_cb==12, 1, 0),
      #                                                              stu_ordertypeAP = ifelse(ord_title=="(f) NR 2021 Female AP Stem" | ord_title=="(f) NR 2022 Female AP STEM", 1, 0),
      #                                                              stu_ordertypeSAT = ifelse(ord_title=="(f) NR 2021 Female SAT STEM"| ord_title=="(f) NR 2022 Female SAT STEM"| ord_title=="(f) NR 2023 Female SAT STEM", 1, 0))
      #      
      #                                     #checks
      #                                       list_gender_metro %>% count(stu_ordertypeAP, ord_title)
      #                                       list_gender_metro %>% count(stu_ordertypeSAT, ord_title)
      #                                       list_gender_metro %>% count(stu_zip_code) %>% arrange(-n)
      #                                       
      #      
      #      #aggregate student list data to zipcode-level with total num prospects , prospect race/ethnicity       
      #       list_gender_metro <- list_gender_metro %>% select(stu_zip_code, stu_race_noresponse, stu_race_missing,
      #                                                         stu_race_aian, stu_race_asian, stu_race_black,
      #                                                         stu_race_latinx, stu_race_nhpi, stu_race_white,
      #                                                         stu_race_other, stu_race_multi, stu_ordertypeAP, stu_ordertypeSAT) %>% group_by(stu_zip_code) %>% summarize_all(sum)
      #      
      #       #list_gender_metro <- list_gender_metro %>% filter(stu_ordertypeAP>0)
      #       # merge in purchased prospects
      #       metro_zips<- merge(x = metro_zips, y = list_gender_metro, by.x  = "zip_code",  by.y  = "stu_zip_code", all.x=TRUE)
      #       
      #       # replace NAs to zeros
      #       metro_zips <- mutate(metro_zips, across(starts_with("stu_race"), ~ifelse(is.na(.x),0,.x)))
      #       metro_zips <- mutate(metro_zips, across(starts_with("stu_ordertype"), ~ifelse(is.na(.x),0,.x)))
      #       
      #       #add a total prospect purchased by zip; then pct by race
      #       metro_zips <- metro_zips %>% rowwise() %>%
      #         mutate(tot_prospects = sum(across(starts_with("stu_race")), na.rm = T))
      #     
      #       
      #       metro_zips <- metro_zips %>% group_by(zip_code) %>%
      #         mutate(stu_pct_white = (stu_race_white/tot_prospects)*100, 
      #                stu_pct_asian = (stu_race_asian/tot_prospects)*100,
      #                stu_pct_black = (stu_race_black/tot_prospects)*100,
      #                stu_pct_latinx= (stu_race_latinx/tot_prospects)*100)
      #       
      #       #print top purchased zips
      #       metro_topzips <- metro_zips %>% select(zip_code, median_household_income,tot_prospects, pop_white_15_19_pct,  stu_pct_white,pop_asian_15_19_pct, stu_pct_asian, pop_black_15_19_pct, stu_pct_black, pop_hispanic_15_19_pct, stu_pct_latinx) %>% arrange(-tot_prospects) 
      #       
      #       metro_topzips <- metro_zips %>% mutate(
      #         purchased_zip_dummy = if_else(tot_prospects>=1,1,0),
      #         purchased_zip = if_else(tot_prospects>=1,"1-5","0"),
      #         purchased_zip = if_else(tot_prospects>5,"6-10",purchased_zip),
      #         purchased_zip = if_else(tot_prospects>10,"10,",purchased_zip),
      #         purchased_zip = as.factor(purchased_zip))
      #       
      #             #look at number of AI students rather than proportion across purchased zips
      #             # purchased_zips <- metro_topzips %>% filter(purchased_zip_dummy==1)
      #             # tx_zips <- acs_race_zipcodev3 %>% filter(state_code=="TX") 
      #             # tx_zips <-   subset(tx_zips, zipcode %in% purchased_zips$zip_code)
      #             # sum(tx_zips$pop_amerindian_15_19)
      #             
      #       #purchased versus non purchased zips in metro
      #         metro_topzips %>% group_by(purchased_zip_dummy) %>% count()
      #       
      #       metro_topzips$purchased_zip <-  factor(metro_topzips$purchased_zip, levels = c("0", "1-5", "6-10", "10,"))
      # 
      #       metro_topzips %>% group_by(purchased_zip) %>% count()
      # 
      #       #average number of prospects purchased across zips>0
      #       metro_topzips %>% group_by(purchased_zip_dummy) %>% summarise(mean_pros = mean(tot_prospects, na.rm=T),
      #                                                                     med_pros = median(tot_prospects, na.rm=T))
      #       
      #       
      #      figure_gender <- metro_topzips %>% group_by(purchased_zip_dummy) %>%
      #         summarise(total_zips= n(),
      #                   mean_avginc = mean(median_household_income, na.rm=T),
      #                   mean_pct_white = mean(pop_white_15_19_pct, na.rm=T),
      #                   mean_pct_asian = mean(pop_asian_15_19_pct, na.rm=T),
      #                   mean_pct_black= mean(pop_black_15_19_pct, na.rm=T),
      #                   mean_pct_latinx = mean(pop_hispanic_15_19_pct, na.rm=T),
      #                   mean_pct_native = mean(pop_amerindian_15_19_pct, na.rm=T),
      #                   stu_pct_white = mean(stu_pct_white, na.rm=T),
      #                   stu_pct_asian = mean(stu_pct_asian, na.rm=T),
      #                   stu_pct_black= mean(stu_pct_black, na.rm=T),
      #                   stu_pct_latinx = mean(stu_pct_latinx, na.rm=T))
      #       
      #       #wide to long
      #      figure_gender_long <- figure_gender %>% pivot_longer(cols = mean_pct_white:stu_pct_latinx,
      #                                                      names_to= "population",
      #                                                      names_prefix=c("mean", "stu"),
      #                                                      values_to= "pct")
      #      
      #      figure_gender_long <- figure_gender_long %>% mutate(
      #        race= ifelse(str_detect(population, "white"), "white", ""),
      #        race= ifelse(str_detect(population, "asian"), "asian", race),
      #        race= ifelse(str_detect(population, "black"), "black", race),
      #        race= ifelse(str_detect(population, "latinx"), "latinx", race),
      #        race= ifelse(str_detect(population, "native"), "native", race),
      #        population= ifelse(str_detect(population, "stu"), "prospects", "zip population"),
      #      )
      #      
      #      #compare to population of 15-19 year olds
      #      ggplot(figure_gender_long, aes(fill=population, y=pct, x=race)) , 
      #        geom_bar(position="dodge", stat="identity") ,
      #        ggtitle("Texas: Non-Purchased versus Purchased Prospects' Zip Codes") ,
      #        facet_wrap(~purchased_zip_dummy) ,
      #        xlab("")
      # 
      #      #race/ethnicity of Texas Prospects
      #      list_gender_metro <-   list_gender %>% filter(stu_state=="TX") #only 314 students purchased in Boston
      #      
      #      tx_prospects<- list_gender_metro %>% 
      #        count(stu_race_cb) %>% mutate(V1 = n / sum(n) * 100)
      #      
      #      tx_prospects <- tx_prospects %>% mutate(race = ifelse(is.na(stu_race_cb), "missing", ""),
      #                                                              race = ifelse(stu_race_cb==0, "no response", race),
      #                                                              race = ifelse(stu_race_cb==1, "native", race),
      #                                                              race = ifelse(stu_race_cb==2, "asian", race),
      #                                                              race = ifelse(stu_race_cb==3, "black", race),
      #                                                              race = ifelse(stu_race_cb==4, 'latinx', race),
      #                                                              race = ifelse(stu_race_cb==8, "nhpi", race),
      #                                                              race = ifelse(stu_race_cb==9, "white", race),
      #                                                              race = ifelse(stu_race_cb==10, "other", race),
      #                                                              race = ifelse(stu_race_cb==12, "multiracial", race))
      #                                                              
      #      
      #      tx_prospects <- tx_prospects %>% select(race, V1)
      #      
      #      tx_prospects$population <- "purchased prospects"
      #      
      #      # https://tea.texas.gov/sites/default/files/ap-ib-texas-2019-20.pdf, table 3
      #      tx_testtakers  <- data.frame(race=c("no response", "native", "asian", "black", "latinx", "nhpi", "white", "multiracial", NA_character_),
      #                                   V1=c(NA, 0.2291236, 16.19576, 6.906439, 39.98795,0.1348556, 33.99277, 2.545236, NA),
      #                                   population=c("science test takers", "science test takers", "science test takers", "science test takers", "science test takers", "science test takers", "science test takers", "science test takers", NA_character_))
      #      tx <- rbind(tx_prospects, tx_testtakers)
      #      
      #      tx <- na.omit(tx)
      #      
      #      ggplot(tx, aes(fill=population, y=V1, x=race)) , 
      #        geom_bar(position="dodge", stat="identity") ,
      #        ggtitle("Texas Test Takers Versus Purchased Prospects") ,
      #        ylab("Percent")
           
           # ### filter only schools in Boston Metro for AP test takers
         #   boston <- pubhs_privhs_data %>% filter(private==0 & cbsa_1=="14460")
         #   boston <- boston %>% mutate(
         #     state_id = str_sub(st_schid,-8,-1)
         #   )
         #   
         #   boston_ap <-   subset(MA_APscores, ma_doe_id %in% boston$state_id) 
         #   
         #   
         #   boston_ap %>%  summarise(sum_total = sum(tot_taken_all, na.rm=T),
         #                            sum_women = sum(tot_taken_female, na.rm = T),
         #                                                   sum_white = sum(tot_taken_white, na.rm=T),
         #                                                   sum_asian = sum(tot_taken_asian, na.rm=T),
         #                                                   sum_black = sum(tot_taken_black, na.rm=T),
         #                                                   sum_latinx = sum(tot_taken_latinx, na.rm=T))
         #   
         #   list_gender %>% filter(zip_cbsa_1=="14460") %>% count(stu_race_cb)
         #   
         #   
      
                  
                  
                       
################### ANALYSIS & VISUALS FOR RQ3: ZIP CODE & TEST SCORES-- Targeting Students of Color
            
            #check orders that used race/ethnicity explicitly
            orders_df %>%  count(race_ethnicity)
            orders_df$race_ethnicity[orders_df$race_ethnicity==''] <- NA
            
            race_orders <- orders_df %>% filter(!is.na(race_ethnicity))
            race_orders %>%  count(race_ethnicity)
            race_orders %>%  pull(race_ethnicity) #printing really long string
            
            
        # create new categorical race filters var
            race_orders <- race_orders %>% mutate(
              race_filter = ifelse(race_ethnicity=="American Indian or Alaska Native", "Native American", NA_character_),
              race_filter = ifelse(race_ethnicity=="Black or African American", "Black", race_filter),
              race_filter = ifelse(race_ethnicity=="Cuban|Hispanic or Latino (including Spanish origin)|Mexican|Puerto Rican|Other Hispanic or Latino", "Latinx", race_filter),
              #race_filter = ifelse(race_ethnicity=="Asian (including Indian subcontinent and Philippines origin)|Other|I do not wish to respond to race|No, not of Hispanic, Latino, or Spanish origin|White (including Middle Eastern origin)", "Asian", race_filter),
              
              race_filter = ifelse(race_ethnicity=="Asian (including Indian subcontinent and Philippines origin)|Other|I do not wish to respond to race|No, not of Hispanic, Latino, or Spanish origin|White (including Middle Eastern origin)", "Asian, White", race_filter),
              race_filter = ifelse(race_ethnicity=="American Indian or Alaska Native|Native Hawaiian or Other Pacific Islander", "Native American, Native Hawaii/PI", race_filter),
              race_filter = ifelse(race_ethnicity=="American Indian or Alaska Native|Cuban|Black or African American|Hispanic or Latino (including Spanish origin)|Mexican|Puerto Rican|Other Hispanic or Latino", "Latinx, Native American", race_filter),
              race_filter = ifelse(race_ethnicity=="Asian (including Indian subcontinent and Philippines origin)|Other|I do not wish to respond to race|No, not of Hispanic, Latino, or Spanish origin|White (including Middle Eastern origin)|Native Hawaiian or Other Pacific Islander", "Asian, White, NativeHawaii/PI", race_filter),
              race_filter = ifelse(race_ethnicity=="Cuban|Black or African American|Hispanic or Latino (including Spanish origin)|Mexican|Puerto Rican|Other Hispanic or Latino", "Latinx, Black", race_filter),
              race_filter = ifelse(race_ethnicity=="Black or African American|American Indian or Alaska Native|Other Hispanic or Latino|Puerto Rican|Mexican|Hispanic or Latino (including Spanish origin)|Cuban", "Latinx, Black, Native American", race_filter), 
              race_filter = ifelse(race_ethnicity=="Asian (including Indian subcontinent and Philippines origin)|White (including Middle Eastern origin)|Other", "Asian, White", race_filter), 
              race_filter = ifelse(race_ethnicity=="Black or African American|American Indian or Alaska Native|Other Hispanic or Latino|Puerto Rican|Mexican|Hispanic or Latino (including Spanish origin)|Native Hawaiian or Other Pacific Islander|Cuban", "Latinx, Black, Native American, NativeHawaii/PI", race_filter),
              race_filter = ifelse(race_ethnicity=="American Indian or Alaska Native|\rAsian (including Indian subcontinent and Philippines origin)|Cuban|\rBlack or African American|\rHispanic or Latino (including Spanish origin)|\rMexican|\rPuerto Rican|\rOther Hispanic or Latino|\rNative Hawaiian or Other Pacific Islander", "Latinx, Black, Asian, Native American", race_filter),
              #adding in new univs
              race_filter = ifelse(race_ethnicity=="Black or African American|American Indian or Alaska Native|Native Hawaiian or Other Pacific Islander", "Black, Native American, NativeHawaii/PI", race_filter),
              race_filter = ifelse(race_ethnicity=="Other Hispanic or Latino|Puerto Rican|Mexican|Hispanic or Latino (including Spanish origin)|Cuban", "Latinx", race_filter),
              race_filter = ifelse(race_ethnicity=="Native Hawaiian or Other Pacific Islander", "NativeHawaii/PI", race_filter),
              race_filter = ifelse(race_ethnicity=="Black or African American|American Indian or Alaska Native|Other Hispanic or Latino|Puerto Rican|Mexican|Hispanic or Latino (including Spanish origin)|Native Hawaiian or Other Pacific Islander|Cuban", "Latinx, Black, Native American, NativeHawaii/PI", race_filter),
              race_filter = ifelse(race_ethnicity=="Asian (including Indian subcontinent and Philippines origin)|Other|I do not wish to respond to race|No, not of Hispanic, Latino, or Spanish origin|White (including Middle Eastern origin)", "Asian, White", race_filter),
              race_filter = ifelse(race_ethnicity=="Black or African American", "Black", race_filter),
              race_filter = ifelse(race_ethnicity=="Other|Asian (including Indian subcontinent and Philippines origin)|I do not wish to respond to race|No, not of Hispanic, Latino, or Spanish origin|White (including Middle Eastern origin)", "Asian, White", race_filter),
              race_filter = ifelse(race_ethnicity=="NA", NA_character_, race_filter),
              
              )
            

            race_orders %>%  count(univ_id, race_filter, race_ethnicity) %>% print(n=25)
            race_orders %>%  count(race_filter)
            
        
          #universities using race/ethnicity filter
            race_orders %>% distinct(order_num) %>% count()
            race_orders %>% distinct(race_filter) %>% count()
            
            race_orders %>% count(univ_name)
            
            
          #NEW FIGURE FOR RQ1 under DEMOGRAPHIC FILTERS
            race_orders_aggregate <- race_orders %>% filter(!is.na(race_filter)) %>%  count(race_filter)
            
            ggplot(race_orders_aggregate, aes( y=n, x=race_filter)) + 
              geom_bar(position="stack", stat="identity") +
              coord_flip()
            
            
            
            
              # Texas A&M CS- 10 orders for Latinx, Black students (4 instate, 6 out of state)
                race_orders %>% filter(univ_name=="Texas A & M University-College Station") %>%  count(race_filter)
                race_orders %>% filter(univ_name=="Texas A & M University-College Station") %>%  count(race_filter, state_name)
                race_orders %>%  filter(univ_name=="Texas A & M University-College Station") %>% summarise(total_orders = n(),
                                            total_students = sum(num_students, na.rm = T))

                orders_num <- race_orders %>% filter(univ_name=="Texas A & M University-College Station") %>%  count(order_num)
                orderswlists_race  <-   subset(lists_orders_zip_hs_df, ord_num %in% orders_num$order_num)
                orderswlists_race %>% count(ord_num) # we have 8/10 orders' resulting student lists; we don't have orders for Latinx,Black

                    #other filters used with race/ethnicity
                    race_orders_univ <- race_orders %>% filter(univ_name=="Texas A & M University-College Station")
                    race_orders_univ %>% count(order_num, hs_grad_class, order_title, race_filter) #only have latinx orders

                    
             # # University of Illinois at Urbana-Champaign - Across all race/ethnicity but All are for in-state students
             #  race_orders %>% filter(univ_name=="University of Illinois at Urbana-Champaign") %>%  count(race_filter)
             #  race_orders %>% filter(univ_name=="University of Illinois at Urbana-Champaign") %>%  count(race_filter, state_name)
             #  race_orders %>%  filter(univ_name=="University of Illinois at Urbana-Champaign") %>% summarise(total_orders = n(),
             #                                                                                             total_students = sum(num_students, na.rm = T))
             # 
             #  orders_num <- race_orders %>% filter(univ_name=="University of Illinois at Urbana-Champaign") %>%  count(order_num)
             #  orderswlists_race  <-   subset(lists_orders_zip_hs_df, ord_num %in% orders_num$order_num)
             #  orderswlists_race %>% count(ord_num) # we have 51/53 orders' resulting student lists
             # 
             #          #other filters used with race/ethnicity
             #          race_orders_univ <- race_orders %>% filter(univ_name=="University of Illinois at Urbana-Champaign")
             #          race_orders_univ %>% count(hs_grad_class,order_num, order_title, race_filter) %>% print(n=60)
             #          
             #          
             #        #focus on pre-pandemic orders made in August 2017-then make inferences post pandemic "catch ups"  
             #          orders_num <- race_orders %>% filter(univ_name=="University of Illinois at Urbana-Champaign" & hs_grad_class=="2018|2019|2020") %>%  count(order_num)
             #          orderswlists_race  <-   subset(lists_orders_zip_hs_df, ord_num %in% orders_num$order_num)
             #          orderswlists_race %>% count(ord_num) # we have 5/5 orders for 2018/2019/2020 HS classes
             #          race_orders %>% filter(univ_name=="University of Illinois at Urbana-Champaign" & hs_grad_class=="2018|2019|2020") %>% count(hs_grad_class,order_num, order_title, race_filter) %>% print(n=60)
             #          race_orders %>% filter(univ_name=="University of Illinois at Urbana-Champaign" & hs_grad_class=="2018|2019|2020") %>% summarise(total_prosp_purchased = sum(num_students, na.rm = T)) #22,311
             #          orderswlists_race %>% count() #we have 22,310 total students
             #          
              # University of California-San Diego- 7 orders for Latinx, Native American (2 instate, 5 out of state)
                # race_orders %>% filter(univ_name=="University of California-San Diego") %>%  count(race_filter)
                # race_orders %>% filter(univ_name=="University of California-San Diego") %>%  count(race_filter, state_name)
                # race_orders %>%  filter(univ_name=="University of California-San Diego") %>% summarise(total_orders = n(),
                #                                                                                            total_students = sum(num_students, na.rm = T))                
                # 
                # orders_num <- race_orders %>% filter(univ_name=="University of California-San Diego") %>%  count(order_num)
                # orderswlists_race  <-   subset(lists_orders_zip_hs_df, ord_num %in% orders_num$order_num)
                # orderswlists_race %>% count(ord_num) # we have 7/7 orders' resulting student lists
                # 
                #       #other filters used with race/ethnicity
                #       race_orders_univ <- race_orders %>% filter(univ_name=="University of California-San Diego")
                #       race_orders_univ %>% count(order_title, race_filter) %>% print(n=60)
                #       
           
           
          # # Use UI Urbana-Champaign as in-state example-- IL
          #             
          #             #check for missing school id
          #              orderswlists_race %>% count(is.na(stu_hs_code)) #only 52 missing school IDs
          #              orderswlists_race %>% count(ord_title) 
          #              orderswlists_race %>% count() 
          #              
          #              #how many from each metro 
          #              orderswlists_race %>% count(hs_cbsatitle_1) %>% arrange(-n) #77% from Chicago; look at Zoomed Map of Chicago-- then explore rural schools?
          #              orderswlists_race %>% filter(hs_cbsatitle_1=="Chicago-Naperville-Elgin, IL-IN-WI") %>% count() 
          #              orderswlists_race %>% filter(hs_cbsatitle_1=="Chicago-Naperville-Elgin, IL-IN-WI") %>% count(stu_race_cb) #77% from Chicago; look at Zoomed Map of Chicago-- then explore rural schools?
          #              
          #             
          #              #Chicago DF
          #              orderswlists_race_chi <- orderswlists_race %>% filter(hs_cbsatitle_1=="Chicago-Naperville-Elgin, IL-IN-WI") 
          #              
          #              
          #              #create dummies of race/ethnicity & order filters to aggregate
          #              orderswlists_race_chi <- orderswlists_race_chi %>% mutate(stu_race_missing = ifelse(is.na(stu_race_cb), 1, 0),
          #                                                   stu_race_noresponse = ifelse(stu_race_cb==0, 1, 0),
          #                                                   stu_race_aian = ifelse(stu_race_cb==1, 1, 0),
          #                                                   stu_race_asian = ifelse(stu_race_cb==2, 1, 0),
          #                                                   stu_race_black = ifelse(stu_race_cb==3, 1, 0),
          #                                                   stu_race_latinx = ifelse(stu_race_cb==4, 1, 0),
          #                                                   stu_race_nhpi = ifelse(stu_race_cb==8, 1, 0),
          #                                                   stu_race_white = ifelse(stu_race_cb==9, 1, 0),
          #                                                   stu_race_other = ifelse(stu_race_cb==10, 1, 0),
          #                                                   stu_race_multi = ifelse(stu_race_cb==12, 1, 0),
          #                                                   stu_ordertype1 = ifelse(ord_title=="IL 1450, August 2017", 1, 0),
          #                                                   stu_ordertype2 = ifelse(ord_title=="IL Lower Range August 2017", 1, 0),
          #                                                   stu_ordertype3 = ifelse(ord_title=="IL Middle 50% August 2017 ", 1, 0),
          #                                                   stu_ordertype4 = ifelse(ord_title=="PAP Honors August 2017", 1, 0),
          #                                                   stu_ordertype5 = ifelse(ord_title=="PAP Traditional August 2017", 1, 0),
          #                                                   stu_ordertype6 = ifelse(ord_title=="URM Lower Range August 2017", 1, 0))
          # 
          # 
          #                           #aggregate student list data to school-level with total num students , race/ethnicity
          #                         school_lists_chi <- orderswlists_race_chi %>% select(hs_ncessch, stu_race_noresponse, stu_race_missing,
          #                                                              stu_race_aian, stu_race_asian, stu_race_black,
          #                                                              stu_race_latinx, stu_race_nhpi, stu_race_white,
          #                                                              stu_race_other, stu_race_multi, stu_ordertype1, stu_ordertype2, stu_ordertype3,
          #                                                              stu_ordertype4, stu_ordertype5, stu_ordertype6) %>% group_by(hs_ncessch) %>% summarize_all(sum)
          # 
          # 
          #                          # now create school df with total students versus student prosp purchased for Houston
          #                            chi_pubprivhs <- pubhs_privhs_data %>% filter(cbsatitle_1=="Chicago-Naperville-Elgin, IL-IN-WI")
          # 
          #                         # merge in purchased prospects
          #                            chi_pubprivhs<- merge(x = chi_pubprivhs, y = school_lists_chi, by.x  = "ncessch",  by.y  = "hs_ncessch", all.x=TRUE)
          # 
          #                         # replace NAs to zeros on student propects purchased from schools
          #                            chi_pubprivhs <- mutate(chi_pubprivhs, across(starts_with("stu_race"), ~ifelse(is.na(.x),0,.x)))
          #                            chi_pubprivhs <- mutate(chi_pubprivhs, across(starts_with("stu_ordertype"), ~ifelse(is.na(.x),0,.x)))
          # 
          # 
          #                               #KS analyses
          #                               
          #                               # #academic filters used
          #                                   race_orders %>% filter(univ_name=="University of Illinois at Urbana-Champaign" & hs_grad_class=="2018|2019|2020") %>% count(gpa_high, gpa_low)
          #                                   race_orders %>% filter(univ_name=="University of Illinois at Urbana-Champaign" & hs_grad_class=="2018|2019|2020") %>% count(order_title, sat_score_min, sat_score_max)
          #                                   race_orders %>% filter(univ_name=="University of Illinois at Urbana-Champaign" & hs_grad_class=="2018|2019|2020") %>% count(order_title, psat_score_min, psat_score_max)
          #                                   race_orders %>% filter(univ_name=="University of Illinois at Urbana-Champaign" & hs_grad_class=="2018|2019|2020") %>% count(order_title, rank_high, rank_low)
          #                                   race_orders %>% filter(univ_name=="University of Illinois at Urbana-Champaign" & hs_grad_class=="2018|2019|2020") %>% count(order_title, ap_scores)
          #                                   
          #                                   #all six orders use GPA=A, to B-; RANK= highest tenth to second fifth; No AP score filters
          #                                   
          #                                   #total students purchased across different orders
          #                                   orderswlists_race_chi %>%  group_by(ord_title, ord_race_ethnicity) %>% count(stu_race_cb) %>% print(n=40)
          #                                               
          #                                   #prospects from order "IL Middle 50% August 2017  FOR Asian, NativeHawaii/PI" (P)SAT=1280-1440
          #                                   chi_pubprivhs %>% filter(stu_ordertype1==1) %>% summarise(total_prosp_asian = sum(stu_race_asian, na.rm = T))
          #                                   
          #                                   chi_pubprivhs %>% filter(stu_ordertype1==1) %>%
          #                                       group_by(ncessch, private) %>%
          #                                       summarise(total_stu_pctwhite = sum(pct_white, na.rm = T),
          #                                                 total_prosp_asian = sum(stu_race_asian, na.rm = T),
          #                                                 total_prosp_black = sum(stu_race_black, na.rm = T),
          #                                                 total_prosp_latinx = sum(stu_race_latinx, na.rm = T)) %>% arrange(-total_stu_pctwhite) %>% print(n=150)
          # 
          #                                     houston_pubprivhs %>% group_by(private) %>% summarise(total_stu_latinx =  sum(total_hispanic, na.rm = T),
          #                                                                                           total_prosp_black =  sum(stu_race_black, na.rm = T),
          #                                                                                           total_prosp_latinx =  sum(stu_race_latinx, na.rm = T),)
          # 
          #                                     orderswlists_race %>% filter(hs_cbsatitle_1=="Houston-The Woodlands-Sugar Land, TX") %>% count(ord_num)
          #                                     orderswlists_race %>% filter(hs_cbsatitle_1=="Houston-The Woodlands-Sugar Land, TX" & stu_race_cb==3) %>% count(ord_num)
          #                                     race_orders %>% filter(order_num==449030) %>% count(race_filter)
          #                                     race_orders %>% filter(order_num==549428) %>% count(race_filter)
          #                                     race_orders %>% filter(order_num==449339) %>% count(race_filter)

                                
              # Use Texas A&M -- Look Houston (in-state) and Los Angeles (out of state); School-Level Aggregates           

                    #check for missing school id
                    orderswlists_race %>% count(is.na(stu_hs_code)) #only 79 missing school IDs
                    orderswlists_race %>% count(ord_title) #

                    #how many from each metro in-state versus out-of-state
                    orderswlists_race %>% count(hs_cbsatitle_1) %>% arrange(-n) #houston, dallas, austin/san antonio

                    #houston df
                    orderswlists_race_tx <- orderswlists_race %>% filter(hs_cbsatitle_1=="Houston-The Woodlands-Sugar Land, TX")

                        #create dummies of race/ethnicity & order filters to aggregate
                        orderswlists_race_tx <- orderswlists_race_tx %>% mutate(stu_race_missing = ifelse(is.na(stu_race_cb), 1, 0),
                                                stu_race_noresponse = ifelse(stu_race_cb==0, 1, 0),
                                                stu_race_aian = ifelse(stu_race_cb==1, 1, 0),
                                                stu_race_asian = ifelse(stu_race_cb==2, 1, 0),
                                                stu_race_black = ifelse(stu_race_cb==3, 1, 0),
                                                stu_race_latinx = ifelse(stu_race_cb==4, 1, 0),
                                                stu_race_nhpi = ifelse(stu_race_cb==8, 1, 0),
                                                stu_race_white = ifelse(stu_race_cb==9, 1, 0),
                                                stu_race_other = ifelse(stu_race_cb==10, 1, 0),
                                                stu_race_multi = ifelse(stu_race_cb==12, 1, 0),
                                                stu_ordertype1 = ifelse(ord_title=="2020 PSAT NH 1270-1420 NM,OK,AR (H)", 1, 0),
                                                stu_ordertype2 = ifelse(ord_title=="2020 PSAT NH 1270-1470 TX (H_include all)", 1, 0),
                                                stu_ordertype3 = ifelse(ord_title=="2020 PSAT NH 1280-1470 CO,CA (H)", 1, 0),
                                                stu_ordertype4 = ifelse(ord_title=="2020 PSAT NH 1370-1420 MS,AL,SC (H)", 1, 0),
                                                stu_ordertype5 = ifelse(ord_title=="2020 PSAT NH 1370-1450 LA,KY,TN,FL (H)", 1, 0),
                                                stu_ordertype6 = ifelse(ord_title=="2020 PSAT NH 1370-1470 GA,VA (H)", 1, 0),
                                                stu_ordertype7 = ifelse(ord_title=="2021 PSAT NH 1290-1520 TX (H)", 1, 0),
                                                stu_ordertype8 = ifelse(ord_title=="PSAT NH 1310-1470 MO,IL (H)", 1, 0))

                      
                        #aggregate student list data to school-level with total num prospects , prospect race/ethnicity       
                         school_lists_tx <- orderswlists_race_tx %>% select(hs_ncessch, stu_race_noresponse, stu_race_missing,
                                                           stu_race_aian, stu_race_asian, stu_race_black,
                                                           stu_race_latinx, stu_race_nhpi, stu_race_white,
                                                           stu_race_other, stu_race_multi, stu_ordertype1, stu_ordertype2, stu_ordertype3,
                                                           stu_ordertype4, stu_ordertype5, stu_ordertype6, stu_ordertype8) %>% group_by(hs_ncessch) %>% summarize_all(sum)


                       # now create school df with total students versus student prosp purchased for Houston
                         houston_pubprivhs <- pubhs_privhs_data %>% filter(cbsatitle_1=="Houston-The Woodlands-Sugar Land, TX")

                      # merge in purchased prospects
                         houston_pubprivhs<- merge(x = houston_pubprivhs, y = school_lists_tx, by.x  = "ncessch",  by.y  = "hs_ncessch", all.x=TRUE)

                      # replace NAs to zeros
                         houston_pubprivhs <- mutate(houston_pubprivhs, across(starts_with("stu_race"), ~ifelse(is.na(.x),0,.x)))
                         houston_pubprivhs <- mutate(houston_pubprivhs, across(starts_with("stu_ordertype"), ~ifelse(is.na(.x),0,.x)))

                      # merge in pub HS data on SAT scores
                         houston_pubhs <- houston_pubprivhs %>% filter(private==0 & pub_sch_type==1) #only regular schools
                         txhs<-read_xlsx("data/achievement data/satact-campus-data-class-2020.xlsx", sheet = "satact-campus-data-class-2020")
                         
                         txhs$CampName <- toupper(txhs$CampName)
                              #fix 5 obs not merging correctly
                                txhs <- txhs %>% mutate(
                                  CampName= ifelse(CampName=="DEER PARK H S", "DEER PARK HS", CampName),
                                  CampName= ifelse(CampName=="CARVER H S FOR APPLIED TECH/ENGINE", "CARVER H S FOR APPLIED TECH/ENGINEERING/ARTS", CampName),
                                  CampName= ifelse(CampName=="ENERGIZED FOR STEM ACADEMY SOUTHEA", "ENERGIZED FOR STEM ACADEMY SOUTHEAST H S", CampName),
                                  CampName= ifelse(CampName=="ENERGIZED FOR STEM ACADEMY SOUTHWE", "ENERGIZED FOR STEM ACADEMY SOUTHWEST H S", CampName),
                                  CampName= ifelse(CampName=="HARMONY SCHOOL OF ADVANCEMENT-HOUS", "HARMONY SCHOOL OF ADVANCEMENT-HOUSTON", CampName),
                                  CampName= ifelse(CampName=="HARMONY SCHOOL OF DISCOVERY - HOUS", "HARMONY SCHOOL OF DISCOVERY - HOUSTON", CampName),
                                  CampName= ifelse(CampName=="HARMONY SCHOOL OF INGENUITY - HOUS", "HARMONY SCHOOL OF INGENUITY - HOUSTON", CampName),
                                  CampName= ifelse(CampName=="HARMONY SCHOOL OF INNOVATION - HOUS", "HARMONY SCHOOL OF INNOVATION - HOUSTON", CampName),
                                  CampName= ifelse(CampName=="HOUSTON ACADEMY FOR INTERNATIONAL", "HOUSTON ACADEMY FOR INTERNATIONAL STUDIES", CampName),
                                  CampName= ifelse(CampName=="HOUSTON GATEWAY ACADEMY - CORAL CA", "HOUSTON GATEWAY ACADEMY - CORAL CAMPUS", CampName),
                                  CampName= ifelse(CampName=="HOUSTON MATH SCIENCE AND TECHNOLOG", "HOUSTON MATH SCIENCE AND TECHNOLOGY CENTER", CampName),
                                  CampName= ifelse(CampName=="ILTEXAS KATY WESTPARK H S", "ILTEXAS - KATY/WESTPARK H S", CampName),
                                  CampName= ifelse(CampName=="MICKEY LELAND COLLEGE PREP ACAD FO", "MICKEY LELAND COLLEGE PREP ACAD FOR YOUNG MEN", CampName),
                                  CampName= ifelse(CampName=="KINDER H S FOR PERFORMING AND VISU", "PERFOR & VIS ARTS H S", CampName),
                                  CampName= ifelse(CampName=="HOUSTON T-STEM AND EARLY COLLEGE H", "RAUL YZAGUIRRE SCHOOL FOR SUCCESS", CampName),
                                  CampName= ifelse(CampName=="ROBERT TURNER COLLEGE AND CAREER H", "ROBERT TURNER COLLEGE AND CAREER H S", CampName),
                                  CampName= ifelse(CampName=="TEXAS CONNECTIONS ACADEMY AT HOUST", "TEXAS CONNECTIONS ACADEMY AT HOUSTON", CampName),
                                  CampName= ifelse(CampName=="WESTCHESTER ACADEMY FOR INTERNATIO", "WESTCHESTER ACADEMY FOR INTERNATIONAL STUDIES", CampName),
                                  CampName= ifelse(CampName== "HARMONY SCHOOL OF INGENUITY-HOUSTO","HARMONY SCHOOL OF INGENUITY-HOUSTON", CampName),
                                  CampName= ifelse(CampName== "HARMONY SCHOOL OF INNOVATION - KAT","HARMONY SCHOOL OF INNOVATION - KATY", CampName),
                                  CampName= ifelse(CampName=="CARVER H S FOR APPLIED TECH/ENGINE", "CARVER H S FOR APPLIED TECH/ENGINEERING/ARTS", CampName))
                         txhs$name2<-as.factor(txhs$CampName)
                         txhs %>% count(RegnName) 
                         txhs %>% count(CntyName) %>% print(n=300)
                         
                         txhs<- txhs %>% filter(
                           CntyName=="Austin County"|CntyName=="Brazoria County"|CntyName=="Fort Bend County" |
                             CntyName=="Galveston County" | CntyName=="Harris County" | CntyName=="Liberty County" | CntyName=="Chambers County" |
                             CntyName=="Montgomery County" | CntyName=="Waller County" | CntyName=="Fayette County" | CntyName=="Taylor County" | CntyName=="Grimes County" | RegnName=="Houston")

                         
                         txhs %>% 
                           summarise(n=n_distinct(Campus)) #248 schools
                         
                         
                        #reshape wide to long
                         txhs <- txhs %>% filter(Group=="All Students"| Group=="African American" | Group=="American Indian"| Group=="Asian" | 
                                                   Group=="Hispanic" |  Group=="White"| Group=="Pacific Islander" |  Group=="Multiracial"|  Group=="Missing Ethnicity")
                         
                         txhs <- txhs %>% mutate(
                           race=ifelse(Group=="All Students", "all", NA_character_),
                           race= ifelse(Group=="African American", "black", race),
                           race= ifelse(Group=="American Indian", "native", race),
                           race= ifelse(Group=="Asian", "asian", race),
                           race= ifelse(Group=="Hispanic", "latinx", race),
                           race= ifelse(Group=="White", "white", race),
                           race= ifelse(Group=="Pacific Islander", "pacisland", race),
                           race= ifelse(Group=="Multiracial", "multirace", race),
                           race= ifelse(Group=="Missing Ethnicity", "missingrace", race))
                         
                         txhs %>% count(race,Group)
                         
                         txhs <- txhs %>% select(race, Campus, CampName, name2, Grads_Mskd,Exnees_Mskd, Part_Rate, Crit_Mskd, Above_Crit_Rate, TSI_Both_Mskd, Above_TSI_Both_Rate)
                         
                         
                         txhs_wide <- txhs %>%
                           gather(key, value, -Campus, -race, -name2, -CampName) %>%
                           unite(col, key, race) %>%
                           spread(col, value)
                         
                         #merge to CCD to get ncessch
                         txhs<-merge(x=houston_pubhs[, c("name", "ncessch")], y=txhs_wide, by.x="name",by.y="name2", all.x=TRUE)
                         
                         txhs %>% 
                           summarise(n=n_distinct(ncessch)) #only 230 schools
                         
                         txhs %>% 
                           count(is.na(Campus)) #11 missing TEA ID
                         
                         txhs %>% select(ncessch, name, Campus) %>%
                           filter(is.na(Campus))
                         
                        #check_merge <- txhs %>% select(name, CampName) 
                         
                            #KS analyses
                              houston_pubprivhs %>% filter(stu_race_black>1 |stu_race_latinx>1) %>%
                                group_by(ncessch, private) %>%
                                summarise(total_stu_pctwhite = sum(pct_white, na.rm = T),
                                          total_prosp_black = sum(stu_race_black, na.rm = T),
                                          total_prosp_latinx = sum(stu_race_latinx, na.rm = T)) %>% arrange(-total_stu_pctwhite) %>% print(n=150)

                              houston_pubprivhs %>% group_by(private) %>% summarise(total_stu_latinx =  sum(total_hispanic, na.rm = T),
                                                                                    total_prosp_black =  sum(stu_race_black, na.rm = T),
                                                                                    total_prosp_latinx =  sum(stu_race_latinx, na.rm = T),)

                              orderswlists_race %>% filter(hs_cbsatitle_1=="Houston-The Woodlands-Sugar Land, TX") %>% count(ord_num)
                              orderswlists_race %>% filter(hs_cbsatitle_1=="Houston-The Woodlands-Sugar Land, TX" & stu_race_cb==3) %>% count(ord_num)
                              race_orders %>% filter(order_num==449030) %>% count(race_filter)
                              race_orders %>% filter(order_num==549428) %>% count(race_filter)
                              race_orders %>% filter(order_num==449339) %>% count(race_filter)
                              
                              
                  #los angeles df
                    orderswlists_race_la <- orderswlists_race %>% filter(hs_cbsatitle_1=="Los Angeles-Long Beach-Anaheim, CA")

                         #create dummies of race/ethnicity & order filters to aggregate
                         orderswlists_race_la <- orderswlists_race_la %>% mutate(stu_race_missing = ifelse(is.na(stu_race_cb), 1, 0),
                                                                                 stu_race_noresponse = ifelse(stu_race_cb==0, 1, 0),
                                                                                 stu_race_aian = ifelse(stu_race_cb==1, 1, 0),
                                                                                 stu_race_asian = ifelse(stu_race_cb==2, 1, 0),
                                                                                 stu_race_black = ifelse(stu_race_cb==3, 1, 0),
                                                                                 stu_race_latinx = ifelse(stu_race_cb==4, 1, 0),
                                                                                 stu_race_nhpi = ifelse(stu_race_cb==8, 1, 0),
                                                                                 stu_race_white = ifelse(stu_race_cb==9, 1, 0),
                                                                                 stu_race_other = ifelse(stu_race_cb==10, 1, 0),
                                                                                 stu_race_multi = ifelse(stu_race_cb==12, 1, 0),
                                                                                 stu_ordertype1 = ifelse(ord_title=="2020 PSAT NH 1270-1420 NM,OK,AR (H)", 1, 0),
                                                                                 stu_ordertype2 = ifelse(ord_title=="2020 PSAT NH 1270-1470 TX (H_include all)", 1, 0),
                                                                                 stu_ordertype3 = ifelse(ord_title=="2020 PSAT NH 1280-1470 CO,CA (H)", 1, 0),
                                                                                 stu_ordertype4 = ifelse(ord_title=="2020 PSAT NH 1370-1420 MS,AL,SC (H)", 1, 0),
                                                                                 stu_ordertype5 = ifelse(ord_title=="2020 PSAT NH 1370-1450 LA,KY,TN,FL (H)", 1, 0),
                                                                                 stu_ordertype6 = ifelse(ord_title=="2020 PSAT NH 1370-1470 GA,VA (H)", 1, 0),
                                                                                 stu_ordertype7 = ifelse(ord_title=="2021 PSAT NH 1290-1520 TX (H)", 1, 0),
                                                                                 stu_ordertype8 = ifelse(ord_title=="PSAT NH 1310-1470 MO,IL (H)", 1, 0))


                         #aggregate student list data to school-level with total num students , race/ethnicity
                         school_lists_la <- orderswlists_race_la %>% select(hs_ncessch, stu_race_noresponse, stu_race_missing,
                                                                            stu_race_aian, stu_race_asian, stu_race_black,
                                                                            stu_race_latinx, stu_race_nhpi, stu_race_white,
                                                                            stu_race_other, stu_race_multi, stu_ordertype1, stu_ordertype2, stu_ordertype3,
                                                                            stu_ordertype4, stu_ordertype5, stu_ordertype6, stu_ordertype8) %>% group_by(hs_ncessch) %>% summarize_all(sum)


                         # now create school df with total students versus student prosp purchased for Houston
                         la_pubprivhs <- pubhs_privhs_data %>% filter(cbsatitle_1=="Los Angeles-Long Beach-Anaheim, CA")

                         # merge in purchased prospects
                         la_pubprivhs<- merge(x = la_pubprivhs, y = school_lists_la, by.x  = "ncessch",  by.y  = "hs_ncessch", all.x=TRUE)

                         # replace NAs to zeros
                         la_pubprivhs <- mutate(la_pubprivhs, across(starts_with("stu_race"), ~ifelse(is.na(.x),0,.x)))
                         la_pubprivhs <- mutate(la_pubprivhs, across(starts_with("stu_ordertype"), ~ifelse(is.na(.x),0,.x)))

                         
                         # merge in pub HS data on SAT scores [can't find newer data; CA DATA DOESNT HAVE BREAKDOWNS BY RACE]
                         la_pubhs <- la_pubprivhs %>% filter(private==0 & pub_sch_type==1) #only regular schools
                         cahs<-read.csv("data/achievement data/sat16-17.csv", na.strings=c("","NA"), colClasses=c("cds"="factor", "Ccode"="factor", "Scode"="factor"), encoding="UTF-8")
                         
                         cahs2<-read.csv("data/achievement data/CDS_NCES_crosswalk.csv", na.strings=c("","NA"), colClasses=c("CDSCode"="factor", "NCESSchool"="factor"), encoding="UTF-8")
                         cahs2$nces<-paste0(as.character(cahs2$NCESDist), as.character(cahs2$NCESSchool))
                         cahs2$nces<-as.factor(cahs2$nces)
                         
                         lahs<-merge(x = cahs, y = cahs2[ , c("CDSCode", "nces")], by.x="cds", by.y="CDSCode", all.x=TRUE)
                         
                         la_pubhs<-merge(x = la_pubhs, y = lahs, by.x="ncessch", by.y="nces", all.x=TRUE)
                         
                         
                         

                  #        #KS analyses
                  #        la_pubprivhs %>% filter(stu_race_black>1 |stu_race_latinx>1) %>% 
                  #          group_by(ncessch, private) %>% 
                  #          summarise(total_stu_pctwhite = sum(pct_white, na.rm = T),
                  #                    total_prosp_black = sum(stu_race_black, na.rm = T),
                  #                    total_prosp_latinx = sum(stu_race_latinx, na.rm = T)) %>% arrange(-total_stu_pctwhite) %>% print(n=150)
                  #        
                  #        
                         
                  
                    
                    
                    
                    
                    
                    
                    
                         
lists_df_summary <- lists_orders_zip_hs_df %>% count(univ_id, univ_state, univ_c15basic, ord_num)

zip_locale <- read_sas(file.path(data_dir, 'EDGE_ZCTALOCALE_2021_LOCALE.sas7bdat'))
lists_df_urbanization <- lists_orders_zip_hs_df %>% 
  mutate(
    region = case_when(
      stu_in_us == 1 & stu_nonres == 0 ~ 'instate',
      stu_in_us == 1 & stu_nonres == 1 ~ 'outofstate',
      T ~ NA_character_
    )
  ) %>% 
  filter(!is.na(region)) %>% 
  inner_join(zip_locale, by = c('stu_zip_code' = 'ZCTA5CE20')) %>% 
  group_by(univ_type, region, LOCALE) %>% 
  summarise(n = n())


# FOR CRYSTAL
save(lists_orders_zip_hs_df, file = file.path("/Users/karinasalazar/Dropbox", 'lists_orders_zip_hs_df.RData'))
save(orders_df, file = file.path("/Users/karinasalazar/Dropbox", 'orders_df.RData'))
save(acs_race_zipcodev3, file = file.path("/Users/karinasalazar/Dropbox", 'acs_race_zipcodev3.RData'))

# save(orders_df, orders_fig_totals, orders_filters1, table_gpa, df_0, df_rq2a, df_int, df_int2, df_rq3, lists_df_summary, table_texasam_zip, table_texasam_zip_inc, fig_rq3_segment_race_inc, file = file.path(data_dir, 'tbl_fig_data.RData'))
save(orders_df, orders_fig_totals, orders_filters2, table_gpa, table_scores, df_0_research, df_0_regional, race_orders_aggregate, df_rq2a, df_int, df_int2, df_rq3, lists_df_summary, lists_df_urbanization, table_texasam_zip, table_texasam_zip_inc, fig_rq3_segment_race_inc, file = file.path(data_dir, 'tbl_fig_data_revised.RData'))
save(houston_pubprivhs, houston_pubhs, txhs, la_pubprivhs, la_pubhs, lahs, acs_race_zipcodev3, file = file.path(data_dir, 'map_data.RData'))
            