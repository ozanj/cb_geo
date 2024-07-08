# CLEAR MEMORY
rm(list = ls())


#LOAD LIBRARIES 
    options(max.print=999999)
    library(tidyverse) 
    library(DataExplorer)
    library(reshape2)
    library(stringr)
    #library(plyr) # consider getting rid of plyr; dplyr is replacement and is part of tidyverse
    library(scales)
    library(ggmap) #uses google maps API
    library(maps)
    library(mapdata)
    library(ggplot2)
    library(mapproj)
    library(assertthat)
    #library(zipcode)
    library(rgeos)
    library(rgdal)
    library(maptools)
    library(ggalt)
    library(ggthemes)
    library(ggrepel)
    library(RColorBrewer)
    library(leaflet)
    library(leaflet.extras)
    library(spdep)
    library(formattable)
    library(htmlwidgets)
    library(sp)
    library(tigris)
               

#############################################################################    
#DATA

#LOAD RECRUITING/ZIP/HS DATA, MERGE IN ZIP CODE DATA, CREATE EVENT TYPE CATEGORIZATION and ZIP VARS
    
  ##load in recruiting data
    #setwd("~/Dropbox/recruiting-m1/analysis/scripts")
    #all<-read.csv("../data/events_data_2.csv", colClasses=c(determined_zip="character"))
    #all<-read.csv("data/events_data.csv", colClasses=c(determined_zip="character", event_state="character",  ipeds_id="factor"), na.strings=c("","NA"), encoding="UTF-8")
    #all <- as.tibble(all)
  ##fix state whitespace formatting
    #all$event_state <- gsub('\\s+', '', all$event_state)
    
  #IMPORT REQ DATA   
    all<-read.csv("data/events_data_2019-03-23_.csv", colClasses=c(determined_zip="character", event_state="character",  ipeds_id="factor"), na.strings=c("","NA"), encoding="UTF-8")
    
    table(all$univ_id)
    all$univ_id <- as.character(all$univ_id)
    all$univ_id[all$univ_id=="166629_req"]<-c("166629")
    all$univ_id[all$univ_id=="110653_req"]<-c("110653")
    all$univ_id[all$univ_id=="196097_req"]<-c("196097")
    all$univ_id[all$univ_id=="186380_req"]<-c("186380")
    all$univ_id[all$univ_id=="139959_req"]<-c("139959")
    all$univ_id[all$univ_id=="201885_req"]<-c("201885")
    all$univ_id[all$univ_id=="126614_req"]<-c("126614")
    all$univ_id[all$univ_id=="155317_req"]<-c("155317")
    all$univ_id <- as.factor(all$univ_id)
    table(all$univ_id)
    
    print(all$pid[is.na(all$latitude)])
    print(all$pid[is.na(all$longitude)])
    
  #three events missing lat/long
    all$latitude[all$pid==66435]<-34.207670
    all$longitude[all$pid==66435]<--119.076350
    
    all$latitude[all$pid==66675]<-32.971940
    all$longitude[all$pid==66675]<--117.260930
    
    all$latitude[all$pid==74253]<-38.980312
    all$longitude[all$pid==74253]<--77.069787
    
    #assert unitid and pid uniquely id obs
    assert_that(any(duplicated(all, by=c("univ_id", "pid")))==FALSE)
    
  ##check if missing location
    class(all)
    options(tibble.print_min=100) # set default printing 
    all %>% count(event_state) #none are missing 
    
    
  ##drop if ipeds_id of recruting event=ipeds_id of institution (on campus event)
    #all %>% filter(ipeds_id == univ_id) %>% count()
    #all <- all %>% filter(ipeds_id != univ_id | is.na(ipeds_id))
    
  ##load in zip level data
    #zip<-read.csv(("../data/zip_to_state.csv"), colClasses=c("zip_code"="factor"))
    zip<-read.csv(("data/zip_to_state.csv"), colClasses=c("zip_code"="factor"))
    class(zip)
    zip<-as.tibble(zip)
  ##rename zip vars
    names(all)
    names(all)[names(all) == 'determined_zip']
    names(all)[names(all) == 'determined_zip'] <- 'zip'
    names(zip)[names(zip) == 'zip_code'] <- 'zip'
    
  ###create racial composition of zipcode 
    racevars <- function(df,new_col,pop_subgroup,pop_total){
      print(summary(df[[pop_subgroup]]))
      print(summary(df[[pop_total]]))
      df[[new_col]] <- (df[[pop_subgroup]] / df[[pop_total]])*100
      df[[new_col]][is.na(df[[new_col]])] <- 0
      print(summary(df[[new_col]]))
      return(df)
    }
    
  
    zip<-racevars(zip,"pct_popam","pop_amerindian","pop_total")
    zip<-racevars(zip,"pct_popas","pop_asian","pop_total")
    zip<-racevars(zip,"pct_pophi","pop_hispanic","pop_total")
    zip<-racevars(zip,"pct_popbl","pop_black","pop_total")
    zip<-racevars(zip,"pct_popwh","pop_white","pop_total")
    zip<-racevars(zip,"pct_pophp","pop_nativehawaii","pop_total")
    zip<-racevars(zip,"pct_poptr","pop_tworaces","pop_total")
    zip<-racevars(zip,"pct_popothr","pop_otherrace","pop_total")
    
    #round all pct vars to clean up map
    zip %>% mutate_at(vars(contains("pct")), funs(round(.,1))) %>% select(contains("pct"))
    zip<- zip %>% mutate_at(vars(contains("pct")), funs(round(.,1))) 
    #zip %>%  select(contains("pct"))
    #zip<- zip %>% mutate_each(funs(round(.,1)), contains("pct"))
    
    all %>% mutate_at(vars(contains("pct")), funs(round(.,1))) %>% select(contains("pct"))
    all <- all %>% mutate_at(vars(contains("pct")), funs(round(.,1))) 
    #all<- all %>% mutate_each(funs(round(.,1)), contains("pct"))
    
    #Now matches tables and figures [just Black, hispanic, Native American]
    zip$pocrace<-rowSums(zip[,c("pct_popbl", "pct_pophi", "pct_popam")], na.rm = TRUE) #sumrows equals zero if all columns are NA
    #zip[is.na(zip$mbl_nummthprof) & is.na(zip$mhi_nummthprof) & is.na(zip$mas_nummthprof) & is.na(zip$mam_nummthprof), "soc_nummthprof"] <- NA #code to missing
    summary(zip$pocrace) 
    
    zip$race_brks_nonwhiteasian <- cut(zip$pocrace, 
                                            breaks=c(-1, 20, 40, 60, 80, 90, 101), 
                                            labels=c("0-19%", "20-39%", "40-59%", 
                                                     "60-79%", "80-89%", "90-100%"))
    
    #create var for income breaks
    summary(zip$avgmedian_inc_2564)
    zip$inc_brks <- cut(zip$avgmedian_inc_2564, 
                        breaks=c(-1, 50000, 75000, 100000, 150000, 200000, 10000000), 
                        labels=c("<$50k", "$50-74k", "$75-99k", 
                                 "$100-149k", "$150-199k", "$200k+")
    )
    
    summary(zip$inc_brks)
    
    
    

  ##merge visit df with zip  
    df <- merge(x = all, y = zip, by="zip", all.x=TRUE)
    
  ##create visit type var 
    table(df$sector)
    df$eventtype[is.na(df$school_id) & is.na(df$ipeds_id)]<-"other"
    df$eventtype[nchar(as.character(df$school_id))>8]<-"public hs"
    df$eventtype[nchar(as.character(df$school_id))<= 8]<-"private hs"
    df$eventtype[df$sector=="Public, 2-year"]<-"pub 2yr cc"
    df$eventtype[df$sector=="Public, 4-year or above"]<-"pub 4yr univ"
    df$eventtype[df$sector=="Private not-for-profit, 4-year or above"]<-"PNP 4yr"
    df$eventtype[is.na(df$eventtype) & !is.na(df$sector)]<-"other college/univ"
    count(df,eventtype)
    #df %>% count(eventtype)
    

  ##MSA DATA--Using 2017 Q4 HUD Files for Zip to MSA 
    ##cbsa with highest res ratio is cbsa_1
    msa<-read.csv(("data/zip_code_cbsa.csv"), colClasses=c("zip_code"="factor", "cbsa_1"="factor"))

    ##add MSA num to recruiting events
    df <- merge(x = df, y = msa, by.x="zip", by.y="zip_code", all.x=TRUE)
    colnames(df)
    
    ##add MSA to zip
    zip <- merge(x = zip, y = msa, by.x="zip", by.y="zip_code", all.x=TRUE)
    colnames(zip)
    

  #load in meta university table, fix formatting on pct and currency vars, add in cbsa by zip
    ipeds<-read.csv("data/meta_university.csv", na.strings=c("","NA"), colClasses=c("zip_code"="factor", "univ_id"="factor"), encoding="UTF-8")
    ipeds<- ipeds %>% mutate_at(vars(contains("pct")), funs(round(.,1))) 
    ipeds$tuit_fees_instate <- currency(ipeds$tuit_fees_instate, digits = 0L)
    ipeds$tuit_fees_outstate <- currency(ipeds$tuit_fees_outstate, digits = 0L)
    #names(ipeds)[names(ipeds) == 'zip_code']<-"zip"
    ipeds <- merge(x = ipeds, y = msa, by="zip_code", all.x=TRUE)
    colnames(df)
    
      #create univs in our sample df
      univlist<-c( "196097", "186380", "215293", "201885", "181464", "139959", "218663", "100751", "199193", "110635", "110653", "126614", "155317", "106397", "149240", "166629")
      univ<- ipeds[ipeds$univ_id %in% univlist,]
      
      #create cc's in our sample df
      cc<-subset(ipeds, sector=="Public, 2-year" | sector=="Public, less-than 2-year")

    #load in hs universe data
    hs<-read.csv("data/data_hs.csv", na.strings=c("","NA"), colClasses=c("zip_code"="factor"), encoding="UTF-8")
    
  #round HS race ethnicity
    hs %>% mutate_at(vars(contains("pct")), funs(round(.,1))) %>% select(contains("pct"))
    hs<- hs %>% mutate_at(vars(contains("pct")), funs(round(.,1))) 
    
#############################################################################    
#FUNCTIONS

#FREQ FUNCTION
    ##arguments: (1) university unitid (2) university name (3) in-state abbreviation
    
    freq<-function(unitid, univname, instate) {
      
      #create a tempdf based on unitid
      tempdf<-subset(df, df$univ_id==unitid)
      
      #total events
      print(paste(univname, " total events: ", table(tempdf$univ_id), sep=""))
      
      #count of visits by type
      x<- table(tempdf$eventtype)
      obs<-melt(x)
      print(ggplot(obs,aes(x=as.factor(Var1),y=value))+
              geom_bar(stat='identity') +
              xlab("Visits by Type") + ylab("Num of Visits") +
              ggtitle(univname))
      
      
      #count of visits by type and in-state/out-state
      tempdf$event_inst[tempdf$event_state==instate]<-"In-State"
      tempdf$event_inst[tempdf$event_state!=instate]<-"Out-State"
      
      props<- prop.table(table(tempdf$eventtype, tempdf$event_inst),1) 
      obs<-melt(props)
      print(props)
      
      ggplot(obs, aes(Var1, value)) +   
        geom_bar(aes(fill = Var2), position = "dodge", stat="identity") +
        scale_x_discrete(labels = wrap_format(8)) +
        labs(title = univname, x = "Visits by Type", y = "Proportion within Visit Type", col = "In State or Out of State") 
    
    }
    

#NATIONAL MAPPING FUNCTION 
    ##arguments: (1) university id
    
    
    ##populate map
    map<-function(unitid) {
      
        ##load USA map
        usa <- map_data("state")
        usa$stateabb <- state.abb[match(usa$region, tolower(state.name))]
      
        states <- aggregate(cbind(long, lat) ~ stateabb, data=usa, 
                          FUN=function(x)mean(range(x)))
        
        ##load in-state map
        usa <- map_data("state")
        usa$stateabb <- state.abb[match(usa$region, tolower(state.name))]
        
        states <- aggregate(cbind(long, lat) ~ stateabb, data=usa, 
                            FUN=function(x)mean(range(x)))
      
        #create a tempdf based on unitid
        tempdf<-subset(df, df$univ_id==unitid)  
        tempuniv<-subset(univ, univ$univ_id==unitid)
        
        #print usa map
        print(ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill="white", color="black") + 
                geom_point(aes(x = longitude, y = latitude, color = eventtype), data=tempdf, size = 3, shape=20) +
                geom_point(aes(x = longitude, y = latitude, color= univ_name), data=tempuniv, size = 3, shape=17) +
                geom_text(data=tempuniv, aes(longitude, latitude, label=univ_name)) +
                geom_text(data=states, aes(long, lat, label=stateabb), size=3) +
                coord_fixed(1.3) )
    }
    
######################## STATE AND MSA FUNCTIONS
    
    
#LOAD SHAPE FILES OUTSIDE OF STATE AND MSA FUNCTIONS (reduced time to load)
    #read in all US map zip shape file
    #uspoly <- readOGR('data/us_geodata/tl_2015_us_zcta510.shp')
    uspoly <- readOGR('data/us_geodata/cb_2016_us_zcta510_500k.shp')
    
    #see data columns in shapefile
    str(slot(uspoly,"data"))
    
    
    #load states outlines
    states <- states(cb = TRUE)
    
    #use diff icons
    redXIcon <- makeIcon(
      iconUrl = "https://cdn3.iconfinder.com/data/icons/flat-actions-icons-9/792/Close_Icon-512.png",
      iconWidth = 5, iconHeight = 8,
      iconAnchorX = 5, iconAnchorY = 5
    )
    
    
    #load geomarkets
    load("data/eps-market-maps/eps-map.Rdata")
    table(eps.markets$eps)
    
    #Los Angeles
    #eps.markets <- subset(eps.markets, eps.markets$eps %in% c("CA14", "CA15", "CA16","CA17", "CA18", "CA19", "CA20", "CA21", "CA22", "CA23", "CA24", "CA25", "CA26"))
    
    #Bay Area
    #eps.markets <- subset(eps.markets, eps.markets$eps %in% c("CA 4", "CA 5", "CA 6","CA 7", "CA 8", "CA 9", "CA10", "CA11"))
    
    #Illinois
    eps.markets <- subset(eps.markets, eps.markets$eps %in% c("IL 7", "IL 8", "IL 9","IL10", "IL11", "IL12", "IL13"))
    
    
    
#STATE FUNCTION  
    
    statemap<-function(unitid, state_abbreviation) {
      
      # create state specific zip code data
      statezip <- as.data.frame(zip)
      statezip <- subset(statezip, statezip$state_code == state_abbreviation)
      
      #merge by zipcode for race data
      statepoly <- merge(uspoly, zip, by.x="ZCTA5CE10", by.y="zip", all.x = TRUE)
      
      #only keep the state in the spatial polygon
      statepoly<- subset(statepoly, state_code == state_abbreviation)
      
      #clean up race/ethincity
      statepoly$pocrace <-round(statepoly$pocrace, digits=0)
      
      #subset to only visits by the university of interest
      tempdf<-subset(df, df$univ_id==unitid)
      count(tempdf,eventtype)
      
      #subset for university dataset  
      tempuniv<-subset(univ, univ$univ_id==unitid)
      
      #create visit vars
      pubschool<-tempdf %>%
        filter(event_type=="pub_hs", event_state==(state_abbreviation)) %>%
        group_by(school_id) %>%
        summarise(visit_count = n(), visit_dummy = n_distinct(school_id))
      
      pubhs_visits<-subset(hs, (ncessch %in% pubschool$school_id))

      
      privschool<-tempdf %>%
        filter(event_type=="priv_hs", event_state==(state_abbreviation)) %>%
        group_by(school_id) %>%
        summarise(visit_count = n(), visit_dummy = n_distinct(school_id))
      
      privhs_visits<-subset(hs, (ncessch %in% privschool$school_id))
      
      
      ccschool<-tempdf %>%
        filter(event_type=="cc", event_state==(state_abbreviation)) %>%
        group_by(ipeds_id) %>%
        summarise(visit_count = n(), visit_dummy = n_distinct(ipeds_id))
      
      cc_visits<-subset(cc, (univ_id %in% ccschool$ipeds_id))
      cc_visits$tuit_fees_instate <- currency(cc_visits$tuit_fees_instate, digits = 0L)
      
      
      other_visits<-subset(tempdf, (eventtype=="other" | eventtype=="pub 4yr univ" | eventtype=="PNP 4yr") & state_code == state_abbreviation)
        
      #subset non-visit HS datasets and merge with msa to get only non-visits in selected msa
      temphs <- subset(hs, !(ncessch %in% tempdf$school_id))
      temphs <- merge(temphs, zip, by.x="zip_code", by.y="zip", all.x = TRUE)
      temphs_nv<-subset(temphs, state_code.x == state_abbreviation)
      
      #subset non_visit further into pub and priv HS
      temppubhs_nv<-subset(temphs_nv, (school_type=="public"))
      tempprivhs_nv<-subset(temphs_nv, (school_type=="private"))
      
      #subset non_visit CC 
      tempcc <- subset(cc, !(univ_id %in% tempdf$ipeds_id))
      tempcc<-subset(tempcc, state_code == state_abbreviation)
        

      #add pop-ups and income range
      statepoly@data$avgmedian_inc_2564 <- currency(statepoly@data$avgmedian_inc_2564, digits = 0L)
      incpop<- paste0("Zip Code: ", statepoly$ZCTA5CE10, "<br>", "Median Household Income: ", statepoly$avgmedian_inc_2564)
      pop<- paste0("Zip Code: ", statepoly$ZCTA5CE10, "<br>", "Total Population: ", statepoly$pop_total)
      pop2<- paste0("Zip Code: ", statepoly$ZCTA5CE10, "<br>", "% Black, Latinx, and Native American: ", statepoly$pocrace)
      
      
      pubhsvisitpop<- paste0(pubhs_visits$name,"<br>", 
                        "School Type: ", pubhs_visits$school_type, "<br>",
                        "Total Enrollment: ", pubhs_visits$total_students, "<br>",
                        "% Black: ", pubhs_visits$pct_black, "<br>",
                        "% Latinx: ", pubhs_visits$pct_hispanic, "<br>",
                        "% Asian: ", pubhs_visits$pct_asian, "<br>",
                        "% Native American: ", pubhs_visits$pct_amerindian, "<br>",
                        "% White: ", pubhs_visits$pct_white)
               
      
      privhsvisitpop<- paste0(privhs_visits$name,"<br>", 
                      "School Type: ", privhs_visits$school_type, "<br>",
                      "Total Enrollment: ", privhs_visits$total_students, "<br>",
                      "% Black: ", privhs_visits$pct_black, "<br>",
                      "% Latinx: ", privhs_visits$pct_hispanic, "<br>",
                      "% Asian: ", privhs_visits$pct_asian, "<br>",
                      "% Native American: ", privhs_visits$pct_amerindian, "<br>",
                      "% White: ", privhs_visits$pct_white)
               
      
      ccvisitpop<- 
        paste0("Community College: ", cc_visits$univ_name, "<br>",
               "Total Enrollment: ", cc_visits$fteug, "<br>",
               "In-State Tuition: ", cc_visits$tuit_fees_instate, "<br>",
               "% Pell: ", cc_visits$pct_pell_recipients_freshman, "<br>",
               "% Black (freshmen): ", cc_visits$pct_freshman_black, "<br>",
               "% Latinx (freshmen): ", cc_visits$pct_freshman_hispanic, "<br>",
               "% Asian (freshmen): ", cc_visits$pct_freshman_asian, "<br>",
               "% Native American (freshmen): ", cc_visits$pct_freshman_amerindian, "<br>",
               "% Multiracial (freshmen): ", cc_visits$pct_freshman_tworaces, "<br>",
               "% White (freshmen): ", cc_visits$pct_freshman_white)
      
      
      othervisitpop<- 
        paste0("Event Location: ", other_visits$event_name, "<br>",
               "Event Type: ", other_visits$categorized_event_type)
      
      
      #university pop-up
      univpop<- paste0(tempuniv$univ_name, "<br>", 
                       "State: ", tempuniv$state_code, "<br>",
                       "In-State Tuition: ", tempuniv$tuit_fees_instate, "<br>",
                       "Out-of-State Tuition: ", tempuniv$tuit_fees_outstate, "<br>",
                       "% Pell: ", tempuniv$pct_pell_recipients_freshman, "<br>",
                       "% Black (freshmen): ", tempuniv$pct_freshman_black, "<br>",
                       "% Latinx (freshmen): ", tempuniv$pct_freshman_hispanic, "<br>",
                       "% Asian (freshmen): ", tempuniv$pct_freshman_asian, "<br>",
                       "% Native American (freshmen): ", tempuniv$pct_freshman_amerindian, "<br>",
                       "% Multiracial (freshmen): ", tempuniv$pct_freshman_tworaces, "<br>",
                       "% White (freshmen): ", tempuniv$pct_freshman_white)
      
      
      nonvisitpop_pub<- 
        paste0(temppubhs_nv$name,"<br>", 
               "School Type: ", temppubhs_nv$school_type, "<br>",
               "Total Enrollment: ", temppubhs_nv$total_students, "<br>",
               "% Black: ", temppubhs_nv$pct_black, "<br>",
               "% Latinx: ", temppubhs_nv$pct_hispanic, "<br>",
               "% Asian: ", temppubhs_nv$pct_asian, "<br>",
               "% Native American: ", temppubhs_nv$pct_amerindian, "<br>",
               "% White: ", temppubhs_nv$pct_white)
      
      
      nonvisitpop_priv<- 
        paste0(tempprivhs_nv$name,"<br>", 
               "School Type: ", tempprivhs_nv$school_type, "<br>",
               "Total Enrollment: ", tempprivhs_nv$total_students, "<br>",
               "% Black: ", tempprivhs_nv$pct_black, "<br>",
               "% Latinx: ", tempprivhs_nv$pct_hispanic, "<br>",
               "% Asian: ", tempprivhs_nv$pct_asian, "<br>",
               "% Native American: ", tempprivhs_nv$pct_amerindian, "<br>",
               "% White: ", tempprivhs_nv$pct_white)
      
      
      ccpop_nv<- 
        paste0("Community College: ", tempcc$univ_name,"<br>",
               "Total Enrollment: ", tempcc$fteug, "<br>",
               "In-State Tuition: ", tempcc$tuit_fees_instate, "<br>",
               "% Pell: ", tempcc$pct_pell_recipients_freshman, "<br>",
               "% Black (freshmen): ", tempcc$pct_freshman_black, "<br>",
               "% Latinx (freshmen): ", tempcc$pct_freshman_hispanic, "<br>",
               "% Asian (freshmen): ", tempcc$pct_freshman_asian, "<br>",
               "% Native American (freshmen): ", tempcc$pct_freshman_amerindian, "<br>",
               "% Multiracial (freshmen): ", tempcc$pct_freshman_tworaces, "<br>",
               "% White (freshmen): ", tempcc$pct_freshman_white)
      
      
      #add visit markers
      icon.univ <- makeAwesomeIcon(library= 'fa', icon = 'university', markerColor = 'red')
      
      #Build Map
      qpal<-colorFactor("YlGnBu", statepoly@data$inc_brks) 
      qpal2<-colorNumeric("YlGnBu", statepoly@data$pop_total, n=5) 
      qpal3<-colorFactor("YlGnBu", statepoly@data$race_brks_nonwhiteasian) 
      
      # Subset state get the entire state outline shape
      state_frame<- subset(states, STUSPS == state_abbreviation)
      
      #mapping
      leaflet(statepoly) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        
        addPolylines(stroke=TRUE, color = "red", weight = 2, data=eps.markets, group="School Boundaries", smoothFactor=0.2) %>%
        addPolylines(stroke=TRUE, color = "black", weight = 3, data=state_frame, group="State") %>%
        addPolygons(stroke = FALSE, fillOpacity = .8, smoothFactor = 0.2, color = ~qpal(inc_brks), popup=incpop, group="State by Median Household Income") %>%
        addPolygons(stroke = FALSE, fillOpacity = .8, smoothFactor = 0.2, color = ~qpal2(pop_total), popup=pop, group="State by Population Total") %>%
        addPolygons(stroke = FALSE, fillOpacity = .8, smoothFactor = 0.2, color = ~qpal3(race_brks_nonwhiteasian), popup=pop2, group="State by Race/Ethnicity") %>%
        
        addMeasure(position = "bottomright",
                   primaryLengthUnit = "miles") %>%
        
        
        #Set view on first obs of MSA and ZOOM to cover entire MSA
        setView(tempuniv[1,8], tempuniv[1,7], zoom = 6) %>%
        
        #Non-Visits
        addMarkers(temppubhs_nv, lng = ~temppubhs_nv$longitude, lat = ~temppubhs_nv$latitude, icon = redXIcon, popup = nonvisitpop_pub, group="Non-Visited Public High Schools") %>%
        addMarkers(tempprivhs_nv, lng = ~tempprivhs_nv$longitude, lat = ~tempprivhs_nv$latitude, icon = redXIcon, popup = nonvisitpop_priv, group="Non-Visited Private High Schools") %>%
        addMarkers(tempcc, lng = ~tempcc$longitude, lat = ~tempcc$latitude, icon = redXIcon, popup = ccpop_nv, group="Non-Visited Community Colleges") %>%
        
        
        #University Marker
        addAwesomeMarkers(tempuniv, lng = ~tempuniv$longitude, lat = ~tempuniv$latitude, icon = icon.univ, popup=univpop, "University Location") %>%
        
        #visited public high schools
        addCircleMarkers(
          layerId = tempdf$ncessch,
          pubhs_visits$longitude,
          pubhs_visits$latitude,
          group = 'Public High School Visits',
          radius = 3,
          fill = TRUE,
          popup=pubhsvisitpop,
          color = 'blue',
          fillOpacity = 1
        ) %>%
        
        
        #visited private high schools
        addCircleMarkers(
          layerId = privhs_visits$ncessch,
          privhs_visits$longitude,
          privhs_visits$latitude,
          group = 'Private High School Visits',
          radius = 3,
          fill = TRUE,
          popup=privhsvisitpop,
          color = '#FF8333',
          fillOpacity = 1
        ) %>%
        
        
        #visited CCs
        addCircleMarkers(
          layerId = cc_visits$ipeds_id,
          cc_visits$longitude,
          cc_visits$latitude,
          group = 'Community College Visits',
          radius = 3,
          fill = TRUE,
          popup=ccvisitpop,
          color = '#4FFF33',
          fillOpacity = 1
        ) %>%
        
        
        #other visits
        addCircleMarkers(
          layerId = other_visits$zip,
          other_visits$longitude,
          other_visits$latitude,
          group = 'Other Visits',
          radius = 3,
          fill = TRUE,
          popup=othervisitpop,
          color = '#EC33FF',
          fillOpacity = 1
        ) %>%
        
        
        addLegend("topright", pal = qpal, values = ~inc_brks,
                  title = "Median Household Income",
                  className = "info legend legend-income",
                  na.label="NA",
                  opacity = 1) %>%
        
        addLegend("topright", pal = qpal2, values = ~pop_total,
                  title = "Population Total",
                  className = "info legend legend-pop",
                  na.label="NA",
                  opacity = 1) %>%
        
        addLegend("topright", pal = qpal3, values = ~race_brks_nonwhiteasian,
                  title = "Black, Latinx, and <br>Native American Population",
                  className = "info legend legend-race",
                  na.label="NA",
                  opacity = 1) %>%
        
        hideGroup("Public High School Visits") %>%
        hideGroup("Private High School Visits") %>%
        hideGroup("Community College Visits") %>%
        hideGroup("Non-Visited Community Colleges") %>%
        hideGroup("Other Visits") %>%
        hideGroup("University Visits") %>%
        hideGroup("Non-Visited Public High Schools") %>%
        hideGroup("Non-Visited Private High Schools") %>%
        hideGroup("Median Household Income") %>%
        hideGroup("Population Total") %>%
        hideGroup("Race/Ethnicity") %>%
        hideGroup("School Boundaries") %>%
        
        
        
        #legends will not toggle within basegroups, only overlay groups: Leaflet bug: https://github.com/rstudio/leaflet/issues/477
        addLayersControl(
          position = c("bottomleft"),
          baseGroups = c("State", "State by Median Household Income","State by Population Total", "State by Race/Ethnicity"),
          overlayGroups = c("School Boundaries", "Public High School Visits", "Non-Visited Public High Schools", "Private High School Visits", "Non-Visited Private High Schools", "Community College Visits", "Non-Visited Community Colleges", "Other Visits"),
          options = layersControlOptions(collapsed = TRUE)
        )%>%
        
        htmlwidgets::onRender("
                              function(el, x) {
                              var myMap = this;
                              $('.legend').css('display', 'none');
                              myMap.on('baselayerchange', function (e) {
                              $('.legend').css('display', 'none');
                              switch (e.name) {
                              case 'State by Median Household Income':
                              $('.legend-income').css('display', 'inherit');
                              break;
                              case 'State by Population Total':
                              $('.legend-pop').css('display', 'inherit');
                              break;
                              case 'State by Race/Ethnicity':
                              $('.legend-race').css('display', 'inherit');
                              break;
                              }
                              e.layer.bringToBack();
                              })
                              }
                              ")
      
    }
    
    

    
    
        
#MSA FUNCTION  
    
    msamap<-function(unitid, msa_num) {
      
      # create msa specific zip code data
      msazip <- as.data.frame(msa)
      msazip <- subset(msa, msa$cbsa_1 == msa_num | msa$cbsa_2 == msa_num | msa$cbsa_3 == msa_num | msa$cbsa_4 == msa_num)
      
      #merge by zipcode for race data
      msapoly <- merge(uspoly, zip, by.x="ZCTA5CE10", by.y="zip", all.x = TRUE)
      
      #only keep the msa in the spatial polygon
      msapoly<- subset(msapoly, (cbsa_1 == msa_num | cbsa_2== msa_num | cbsa_3==msa_num | cbsa_4==msa_num))
      
      #subset to only visits by the university of interest
      tempdf<-subset(df, df$univ_id==unitid)
      count(tempdf,event_type)
      
      #subset visit datasets
      #create visit vars
      pubschool<-tempdf %>%
        filter(event_type=="pub_hs", (cbsa_1 == msa_num | cbsa_2== msa_num | cbsa_3==msa_num | cbsa_4==msa_num)) %>%
        group_by(school_id) %>%
        summarise(visit_count = n(), visit_dummy = n_distinct(school_id))
      
      pubhs_visits<-subset(hs, (ncessch %in% pubschool$school_id))
      
      privschool<-tempdf %>%
        filter(event_type=="priv_hs",  (cbsa_1 == msa_num | cbsa_2== msa_num | cbsa_3==msa_num | cbsa_4==msa_num)) %>%
        group_by(school_id) %>%
        summarise(visit_count = n(), visit_dummy = n_distinct(school_id))
      
      privhs_visits<-subset(hs, (ncessch %in% privschool$school_id))

      ccschool<-tempdf %>%
        filter(event_type=="cc", (cbsa_1 == msa_num | cbsa_2== msa_num | cbsa_3==msa_num | cbsa_4==msa_num)) %>%
        group_by(ipeds_id) %>%
        summarise(visit_count = n(), visit_dummy = n_distinct(ipeds_id))
      
      cc_visits<-subset(cc, (univ_id %in% ccschool$ipeds_id))
      cc_visits$tuit_fees_instate <- currency(cc_visits$tuit_fees_instate, digits = 0L)
      
      other_visits<-subset(tempdf, (eventtype=="other" | eventtype=="pub 4yr univ" | eventtype=="PNP 4yr") & (cbsa_1 == msa_num | cbsa_2== msa_num | cbsa_3==msa_num | cbsa_4==msa_num))

      
      #subset non-visit HS datasets and merge with msa to get only non-visits in selected msa
      temphs <- subset(hs, !(ncessch %in% tempdf$school_id))
      temphs <- merge(temphs, zip, by.x="zip_code", by.y="zip", all.x = TRUE)
      temphs_nv<-subset(temphs, (cbsa_1 == msa_num | cbsa_2== msa_num | cbsa_3==msa_num | cbsa_4==msa_num))
      
      #subset non_visit further into pub and priv HS
      temppubhs_nv<-subset(temphs_nv, (school_type=="public"))
      tempprivhs_nv<-subset(temphs_nv, (school_type=="private"))
      
      #subset non_visit CC 
      tempcc <- subset(cc, !(univ_id %in% tempdf$ipeds_id))
      tempcc<-subset(tempcc, (cbsa_1 == msa_num | cbsa_2== msa_num | cbsa_3==msa_num | cbsa_4==msa_num))
      
      
      #subset for university dataset  
      tempuniv<-subset(univ, univ$univ_id==unitid)

      #add pop-ups and income range
      msapoly@data$avgmedian_inc_2564 <- currency(msapoly@data$avgmedian_inc_2564, digits = 0L)
      incpop<- paste0("Zip Code: ", msapoly$ZCTA5CE10, "<br>", "Median Household Income: ", msapoly$avgmedian_inc_2564)
      pop<- paste0("Zip Code: ", msapoly$ZCTA5CE10, "<br>", "Total Population: ", msapoly$pop_total)
      pop2<- paste0("Zip Code: ", msapoly$ZCTA5CE10, "<br>", "% Black, Latinx, and Native American: ", msapoly$pocrace)
      
      
      pubhsvisitpop<- paste0(pubhs_visits$name,"<br>", 
                             "School Type: ", pubhs_visits$school_type, "<br>",
                             "Total Enrollment: ", pubhs_visits$total_students, "<br>",
                             "% Black: ", pubhs_visits$pct_black, "<br>",
                             "% Latinx: ", pubhs_visits$pct_hispanic, "<br>",
                             "% Asian: ", pubhs_visits$pct_asian, "<br>",
                             "% Native American: ", pubhs_visits$pct_amerindian, "<br>",
                             "% White: ", pubhs_visits$pct_white)
      
      
      privhsvisitpop<- paste0(privhs_visits$name,"<br>", 
                              "School Type: ", privhs_visits$school_type, "<br>",
                              "Total Enrollment: ", privhs_visits$total_students, "<br>",
                              "% Black: ", privhs_visits$pct_black, "<br>",
                              "% Latinx: ", privhs_visits$pct_hispanic, "<br>",
                              "% Asian: ", privhs_visits$pct_asian, "<br>",
                              "% Native American: ", privhs_visits$pct_amerindian, "<br>",
                              "% White: ", privhs_visits$pct_white)
      
      
      
      ccvisitpop<- 
        paste0("Community College: ", cc_visits$univ_name, "<br>",
               "Total Enrollment: ", cc_visits$fteug, "<br>",
               "In-State Tuition: ", cc_visits$tuit_fees_instate, "<br>",
               "% Pell: ", cc_visits$pct_pell_recipients_freshman, "<br>",
               "% Black (freshmen): ", cc_visits$pct_freshman_black, "<br>",
               "% Latinx (freshmen): ", cc_visits$pct_freshman_hispanic, "<br>",
               "% Asian (freshmen): ", cc_visits$pct_freshman_asian, "<br>",
               "% Native American (freshmen): ", cc_visits$pct_freshman_amerindian, "<br>",
               "% Multiracial (freshmen): ", cc_visits$pct_freshman_tworaces, "<br>",
               "% White (freshmen): ", cc_visits$pct_freshman_white)
      
      
      othervisitpop<- 
        paste0("Event Location: ", other_visits$event_name, "<br>",
               "Event Type: ", other_visits$categorized_event_type)
      
      
      #university pop-up
      univpop<- paste0(tempuniv$univ_name, "<br>", 
                       "State: ", tempuniv$state_code, "<br>",
                       "In-State Tuition: ", tempuniv$tuit_fees_instate, "<br>",
                       "Out-of-State Tuition: ", tempuniv$tuit_fees_outstate, "<br>",
                       "% Pell: ", tempuniv$pct_pell_recipients_freshman, "<br>",
                       "% Black (freshmen): ", tempuniv$pct_freshman_black, "<br>",
                       "% Latinx (freshmen): ", tempuniv$pct_freshman_hispanic, "<br>",
                       "% Asian (freshmen): ", tempuniv$pct_freshman_asian, "<br>",
                       "% Native American (freshmen): ", tempuniv$pct_freshman_amerindian, "<br>",
                       "% Multiracial (freshmen): ", tempuniv$pct_freshman_tworaces, "<br>",
                       "% White (freshmen): ", tempuniv$pct_freshman_white)
      
      
      nonvisitpop_pub<- 
        paste0("Public High School: ", temppubhs_nv$name,"<br>", 
               "School Type: ", temppubhs_nv$school_type, "<br>",
               "Total Enrollment: ", temppubhs_nv$total_students, "<br>",
               "% Black: ", temppubhs_nv$pct_black, "<br>",
               "% Latinx: ", temppubhs_nv$pct_hispanic, "<br>",
               "% Asian: ", temppubhs_nv$pct_asian, "<br>",
               "% Native American: ", temppubhs_nv$pct_amerindian, "<br>",
               "% White: ", temppubhs_nv$pct_white)
      
      
      nonvisitpop_priv<- 
        paste0("Private High School: ", tempprivhs_nv$name,"<br>", 
               "School Type: ", tempprivhs_nv$school_type, "<br>",
               "Total Enrollment: ", tempprivhs_nv$total_students, "<br>",
               "% Black: ", tempprivhs_nv$pct_black, "<br>",
               "% Latinx: ", tempprivhs_nv$pct_hispanic, "<br>",
               "% Asian: ", tempprivhs_nv$pct_asian, "<br>",
               "% Native American: ", tempprivhs_nv$pct_amerindian, "<br>",
               "% White: ", tempprivhs_nv$pct_white)
      
      
      ccpop_nv<- 
        paste0("Community College: ", tempcc$univ_name, "<br>",
               "Total Enrollment: ", tempcc$fteug, "<br>",
               "In-State Tuition: ", tempcc$tuit_fees_instate, "<br>",
               "% Pell: ", tempcc$pct_pell_recipients_freshman, "<br>",
               "% Black (freshmen): ", tempcc$pct_freshman_black, "<br>",
               "% Latinx (freshmen): ", tempcc$pct_freshman_hispanic, "<br>",
               "% Asian (freshmen): ", tempcc$pct_freshman_asian, "<br>",
               "% Native American (freshmen): ", tempcc$pct_freshman_amerindian, "<br>",
               "% Multiracial (freshmen): ", tempcc$pct_freshman_tworaces, "<br>",
               "% White (freshmen): ", tempcc$pct_freshman_white)
      
      
      #add visit markers
      icon.univ <- makeAwesomeIcon(library= 'fa', icon = 'university', markerColor = 'red')
      
      #Build Map
      qpal<-colorFactor("YlGnBu", msapoly@data$inc_brks) 
      qpal2<-colorNumeric("YlGnBu", msapoly@data$pop_total, n=5) 
      qpal3<-colorFactor("YlGnBu", msapoly@data$race_brks_nonwhiteasian) 
      
      # Merge polygons (zip-level)in MSA to get the entire MSA outline shape
      msa_union<-gUnaryUnion(msapoly, id=NULL)
      view<-as.data.frame(coordinates(msa_union)) 
  
      #mapping
      leaflet(msapoly) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        
        addPolylines(stroke=TRUE, smoothFactor = 0.2, color = "black", weight = 3, data=msa_union, group="MSA") %>%
        addPolygons(stroke = FALSE, fillOpacity = .8, smoothFactor = 0.2, color = ~qpal(inc_brks), popup=incpop, group="MSA by Median Household Income") %>%
        addPolygons(stroke = FALSE, fillOpacity = .8, smoothFactor = 0.2, color = ~qpal2(pop_total), popup=pop, group="MSA by Population Total") %>%
        addPolygons(stroke = FALSE, fillOpacity = .8, smoothFactor = 0.2, color = ~qpal3(race_brks_nonwhiteasian), popup=pop2, group="MSA by Race/Ethnicity") %>%
        
        addMeasure(position = "bottomright",
                   primaryLengthUnit = "miles") %>%
      
        
        #Set view on first obs of MSA and ZOOM to cover entire MSA
        setView(view[1,1], view[1,2], zoom = 7) %>%
        
        #Non-Visits
        addMarkers(temppubhs_nv, lng = ~temppubhs_nv$longitude, lat = ~temppubhs_nv$latitude, icon = redXIcon, popup = nonvisitpop_pub, group="Non-Visited Public High Schools") %>%
        addMarkers(tempprivhs_nv, lng = ~tempprivhs_nv$longitude, lat = ~tempprivhs_nv$latitude, icon = redXIcon, popup = nonvisitpop_priv, group="Non-Visited Private High Schools") %>%
        addMarkers(tempcc, lng = ~tempcc$longitude, lat = ~tempcc$latitude, icon = redXIcon, popup = ccpop_nv, group="Non-Visited Community Colleges") %>%
        
        
        #University Marker
        addAwesomeMarkers(tempuniv, lng = ~tempuniv$longitude, lat = ~tempuniv$latitude, icon = icon.univ, popup=univpop, "University Location") %>%

        #visited public high schools
        addCircleMarkers(
          layerId = tempdf$ncessch,
          pubhs_visits$longitude,
          pubhs_visits$latitude,
          group = 'Public High School Visits',
          radius = 3,
          fill = TRUE,
          popup=pubhsvisitpop,
          color = 'blue',
          fillOpacity = 1
        ) %>%
        
    
        #visited private high schools
        addCircleMarkers(
          layerId = privhs_visits$ncessch,
          privhs_visits$longitude,
          privhs_visits$latitude,
          group = 'Private High School Visits',
          radius = 3,
          fill = TRUE,
          popup=privhsvisitpop,
          color = '#FF8333',
          fillOpacity = 1
          ) %>%
        
        
        #visited CCs
        addCircleMarkers(
          layerId = cc_visits$ipeds_id,
          cc_visits$longitude,
          cc_visits$latitude,
          group = 'Community College Visits',
          radius = 3,
          fill = TRUE,
          popup=ccvisitpop,
          color = '#4FFF33',
          fillOpacity = 1
        ) %>%
        
        
        #other visits
        addCircleMarkers(
          layerId = other_visits$zip,
          other_visits$longitude,
          other_visits$latitude,
          group = 'Other Visits',
          radius = 3,
          fill = TRUE,
          popup=othervisitpop,
          color = '#EC33FF',
          fillOpacity = 1
        ) %>%
        
        
        addLegend("topright", pal = qpal, values = ~inc_brks,
                  title = "Median Household Income",
                  className = "info legend legend-income",
                  na.label="NA",
                  opacity = 1) %>%
        
        addLegend("topright", pal = qpal2, values = ~pop_total,
                  title = "Population Total",
                  className = "info legend legend-pop",
                  na.label="NA",
                  opacity = 1) %>%
        
        addLegend("topright", pal = qpal3, values = ~race_brks_nonwhiteasian,
                  title = "Black, Latinx, and <br> Native American Population",
                  className = "info legend legend-race",
                  na.label="NA",
                  opacity = 1) %>%
        
        hideGroup("Public High School Visits") %>%
        hideGroup("Private High School Visits") %>%
        hideGroup("Community College Visits") %>%
        hideGroup("Non-Visited Community Colleges") %>%
        hideGroup("Other Visits") %>%
        hideGroup("University Visits") %>%
        hideGroup("Non-Visited Public High Schools") %>%
        hideGroup("Non-Visited Private High Schools") %>%
        hideGroup("Median Household Income") %>%
        hideGroup("Population Total") %>%
        hideGroup("Race/Ethnicity") %>%
        #hideGroup("University Location") %>%
        
        
        #legends will not toggle within basegroups, only overlay groups: Leaflet bug: https://github.com/rstudio/leaflet/issues/477
        addLayersControl(
          position = c("bottomleft"),
          baseGroups = c("MSA", "MSA by Median Household Income","MSA by Population Total", "MSA by Race/Ethnicity"),
          overlayGroups = c("Public High School Visits", "Non-Visited Public High Schools", "Private High School Visits", "Non-Visited Private High Schools", "Community College Visits", "Non-Visited Community Colleges", "Other Visits"),
          options = layersControlOptions(collapsed = TRUE)
        )%>%
        
        htmlwidgets::onRender("
          function(el, x) {
            var myMap = this;
            $('.legend').css('display', 'none');
            myMap.on('baselayerchange', function (e) {
              $('.legend').css('display', 'none');
              switch (e.name) {
                case 'MSA by Median Household Income':
                  $('.legend-income').css('display', 'inherit');
                  break;
                case 'MSA by Population Total':
                  $('.legend-pop').css('display', 'inherit');
                  break;
                case 'MSA by Race/Ethnicity':
                  $('.legend-race').css('display', 'inherit');
                  break;
                }
                e.layer.bringToBack();
                  })
                  }
                   ")
              
      
    }
    

    
#############################################################################    
#CALLING FUNCTIONS & POPULATING MAPS
    
    
    ##FREQUENCY FUNCTION
    #arguments: (1) university unitid (2) university name (3) in-state abbreviation
    freq(196097, "SUNY Stony Brook", "NY")
    freq(186380, "Rutgers University-New Brunswick", "NJ")
    freq(215293, "University of Pittsburgh", "PA")
    freq(201885, "University of Cincinnati", "OH")
    freq(181464, "University of Nebraska-Lincoln", "NE")
    freq(139959, "University of Georgia", "GA")
    freq(218663, "University of South Carolina-Columbia", "SC")
    freq(100751, "The University of Alabama", "AL")
    freq(199193, "North Carolina State University at Raleigh", "NC")
    freq(110635, "University of California-Berkeley", "CA")
    freq(110653, "University of California-Irvine", "CA")
    freq(126614, "University of Colorado Boulder", "CO")
    freq(155317, "University of Kansas", "KS")
    freq(106397, "University of Arkansas", "AR")
    freq(149222, "Southern Illinois University", "IL")
    
    
    ##US MAP FUNCTION
    #arguments: (1) university unitid
    map(196097)
    map(186380)
    map(215293)
    map(201885)
    map(181464)
    map(139959)
    map(218663)
    map(100751)
    map(199193)
    map(110635)
    map(110653)
    map(126614)
    map(155317)
    map(106397)
    map(149222)
    
  
    ##STATE MAP FUNCTION
    #arguments: (1) university unitid (2) state abbreviation
    
    univ_sample <- read.csv('./data/univ_sample.csv', na.strings=c('','NA'))
    for (i in 1:nrow(univ_sample)) {
      univ_id = as.numeric(univ_sample[i, 'univ_id'])
      state_code = as.character(univ_sample[i, 'state_code'])
      saveWidget(statemap(univ_id, state_code), paste0(univ_id, '_map-state-in.html'), background = 'transparent')
    }
    
    statemap(218663, "IL") #USC in SC
    statemap(199193, "NC") #NC State in NC
    statemap(196097, "NY") #SUNY SB in NY
    statemap(186380, "NJ") #rutgers in NY
    statemap(215293, "PA") #pitt
    statemap(201885, "OH") #cinci
    statemap(181464, "NE") #UNL
    statemap(139959, "GA") #uga
    statemap(100751, "AL") #bama
    statemap(126614, "CO") #CU Boulder
    statemap(155317, "KS") #UKansas
    statemap(110653, "CA") #UC Irvine
    statemap(110635, "CA") #UC Berkeley
    statemap(106397, "AR") #U Arkansas
    statemap(166629, "MA") # Umass Amhers

    ##MSA MAP FUNCTION
    #arguments: (1) university unitid (2) MSA number as a string)
    
    #major MSAs by rank according to 2017 Census:
      "  35620  New York-Newark-Jersey City, NY-NJ-PA
         31080  Los Angeles-Long Beach-Anaheim, CA
         16264  Chicago-Naperville-Elgin, IL-IN-WI
         16980  Dallas-Fort Worth-Arlington, TX
         47900  Washington-Arlington-Alexandria, DC-VA-MD-WV
         33100  Miami-Fort Lauderdale-West Palm Beach, FL
         26420  Houston-The Woodlands-Sugar Land, TX
         37980  Philadelphia-Camden-Wilmington, PA-NJ-DE-MD
         12060  Atlanta-Sandy Springs-Roswell, GA
         14460  Boston-Cambridge-Newton, MA-NH
         38060  Phoenix-Mesa-Scottsdale, AZ 
         41860  San Francisco-Oakland-Hayward, CA
         40140  Riverside-San Bernardino-Ontario, CA
         19820  Detroit-Warren-Dearborn, MI 
         42660  Seattle-Tacoma-Bellevue, WA
         33460  Minneapolis-St. Paul-Bloomington, MN-WI 
         41740  San Diego-Carlsbad, CA Metro Area
         45300  Tampa-St. Petersburg-Clearwater, FL
         19740  Denver-Aurora-Lakewood, CO Metro Area
         12580  Baltimore-Columbia-Towson, MD Metro Area
         41180  St. Louis, MO-IL 
         38300  Pittsburgh, PA 
         17140  Cincinnati, OH-KY-IN
         28140  Kansas City, MO-KS
         39580  Raleigh, NC
      "
    
    for (i in 1:nrow(univ_sample)) {
      univ_id = as.numeric(univ_sample[i, 'univ_id'])
      cbsa_code = as.character(univ_sample[i, 'metro1'])
      saveWidget(msamap(univ_id, cbsa_code), paste0(univ_id, '_map-metro-in.html'), background = 'transparent')
    }
    for (i in 1:nrow(univ_sample)) {
      univ_id = as.numeric(univ_sample[i, 'univ_id'])
      cbsa_code = as.character(univ_sample[i, 'metro2'])
      saveWidget(msamap(univ_id, cbsa_code), paste0(univ_id, '_map-metro-out-1.html'), background = 'transparent')
    }
    for (i in 1:nrow(univ_sample)) {
      univ_id = as.numeric(univ_sample[i, 'univ_id'])
      cbsa_code = as.character(univ_sample[i, 'metro3'])
      saveWidget(msamap(univ_id, cbsa_code), paste0(univ_id, '_map-metro-out-2.html'), background = 'transparent')
    }
    
    # for metro that univ is located in
    for (i in 1:nrow(univ)) {
      univ_id = as.numeric(univ[i, 'univ_id'])
      cbsa_code = as.character(univ[i, 'cbsa_code'])
      saveWidget(msamap(univ_id, cbsa_code), paste0(univ_id, '_map-metro-in.html'), background = 'transparent')
    }
    
    msamap(196097, "35620") #SUNY in NY
    msamap(186380, "16980") # Rutgers in Chicago
    msamap(186380, "37980") # Rutgers in Philly
    msamap(186380, "47900") # Rutgers in DC
    
    msamap(126614, "19740") # "University of Colorado Boulder" in Denver
    msamap(110635, "41860") # "UC Berkeley in Bay
    
    msamap(215293, "14460") # U. Pitt in Boston
    msamap(126614, "46060")
    msamap(110653, "31080") #Irvine in LA
    msamap(166629, "14460") #UMass in Boston
    
    msamap(166629, "35620") #UMass in NY
    msamap(186380, "41860") # Rutgers in BAY
    
    
  #paper
    msamap(199193, "39580") #NC State in RALEIGH
    msamap(199193, "35620") #NC State in NY
    msamap(199193, "47900") #NC State in DC
    msamap(139959, "31080") #Georgia in LA
    
    msamap(186380, "35620") #Rutgers State in NY
    msamap(186380, "47900") #Rutgers in DC ~65 visits 
    msamap(186380, "41860") #Rutgers in SanFran
    
    msamap(196097, "35620") #Stony Brook in NY
    msamap(196097, "25540") #Stony Brook in hartford
    msamap(196097, "37980") #Stony Brook in Philly
    
    
    msamap(126614, "39580") #CU Boulder in RALEIGH
    msamap(126614, "35620") #CU Boulder in NY
    msamap(126614, "47900") #CU Boulder in DC
    msamap(126614, "31080") #CU Boulder in LA

    
    msamap(218663, "35620") #USC  in NY
    msamap(215293, "16980") #Pitt in Chicago
    
    msamap(218663, "19100") #nebraska in dallas
    msamap(218663, "16980") #USC Boulder in Chicago
    msamap(218663, "19100") #Bama in dallas
    
    msamap(215293, "35620") #Pitt in NY
    msamap(215293, "16980") #Pitt in Chicago
    
    
    
    msamap(166629, "35620") #Umass in NY
    msamap(218663, "35620") #Georgia in LA #Baldwin Hills
    