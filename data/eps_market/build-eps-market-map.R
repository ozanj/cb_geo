### http://gis.stackexchange.com/questions/37370/merge-zip-codes-to-create-new-sales-markets


## setup
library(maptools)
library(rgeos)
library(gpclib) 
gpclibPermit() ## needed to do union spatial polygons
library(RODBC)
library(stringr)

## read in the zip file
setwd("C:/Users/btibert/Documents/Dropbox/projects/eps-market-maps")
myzip <- readShapePoly("US_Zip_-_Lower_48.shp")

## read in the eps markets
ch <- odbcConnectExcel("eps.xls")
eps <- sqlFetch(ch, "Sheet 1", stringsAsFactors=F)
close(ch)
eps <- eps[,1:2]
eps$zip <- as.character(eps$zip)
eps <- subset(eps, !is.na(zip))
eps$zip <- str_pad(eps$zip, 5, "left", "0")


###############################################################################
## make the EPS map
## http://gis.stackexchange.com/questions/37503/rename-a-spatialpolygon-class-object-in-r
## http://r-sig-geo.2731867.n2.nabble.com/merging-data-with-SpatialPolygonsDataFrame-td2764348.html
############################################################################### 

## merge on the data to the map, need to use match
myzip@data=data.frame(myzip@data, eps[match(as.character(myzip@data$ZIP), eps$zip),])
head(myzip@data); tail(myzip@data);

## dissolve based on the merged eps region -- fixed eps file
## some zips still are missing data, even after running epsGuess below
eps.markets <- gUnionCascaded(myzip, id=myzip@data$eps)
sdf <- data.frame(eps=row.names(eps.markets))
row.names(sdf) <- row.names(eps.markets)
eps.markets <-  SpatialPolygonsDataFrame(eps.markets, sdf)


## TODO:  look into the issues below
## using the merge above, these markets are still not included
## US 7, GA03, FL06, HI 1, HI 2, OR04, AK 1, AK 2, TX11
## AK and HI make sense, but a few bad codes and not sure about TX

## plot the map, there are a few gaps to fill but not bad overall
plot(eps.markets, col="red")

## which zips are missing markets
eps.missing <- subset(myzip@data, is.na(eps))


## save the map for future use / merging /mapping
## can get rid of myzip and eps.missing once full map
save(eps.markets, myzip, eps.missing, file="eps-map.Rdata")


###############################################################################
## PARSE THE DATA FROM ZIP DATA MAPS TO FIND BEST ESTIAMTE OF EPS MARKET FOR 
## MISSING CODES
## USES GOOGLE CACHED SEARCH
###############################################################################


# eps.known <- subset(myzip@data, 
#                     subset = !is.na(eps),
#                     select = c(zip, eps))
# 
# epsGuess <- function(zip='01453') {
#  require(XML); require(plyr);
#  # takes a string zip code and gets the adjacent zips for the given zip code
# #  BASEURL <- "http://webcache.googleusercontent.com/search?q=cache:http://www.zipdatamaps.com/"
#  BASEURL <- "http://www.zipdatamaps.com/"
#  URL <- paste(BASEURL, zip, sep="")
#  P <- try(readHTMLTable(URL, stringsAsFactors=F))
#  tmp <- P[[18]]
#  tmp <- unlist(tmp)
#  tmp <- tmp[2:length(tmp)]
#  names(tmp) <- NULL
#  zips <- str_sub(tmp, 1, str_locate(tmp, "-")[,1]-2)
#  return(data.frame(zip=zip, nei = zips))
# }
# 
# eps.missing.guess <- data.frame(stringsAsFactors=F)
# for (Z in unique(eps.missing$ZIP) ) {
#  cat("searching for ", Z, "\n")
#  tmp <- epsGuess(Z)
#  eps.missing.guess <- rbind.fill(eps.missing.guess, tmp)
# }
# 
# ## merge on the data we know
# eps.missing.guess.m <- merge(eps.missing.guess,
#                              eps.known,
#                              by.x="nei",
#                              by.y="zip")
# eps.missing.guess.m <- arrange(eps.missing.guess.m, zip)
# 
# ## find the MODE
# Mode <- function() {
#  tmp <- table(x$eps)
#  mode <- tmp[which.max(tmp)]
#  mode <- names(mode)
#  return(mode)
# }
# 
# eps.mode <- ddply(eps.missing.guess.m, .(zip), Mode)
# 
# setdiff(eps.mode$zip, eps.missing$zip)





###############################################################################
## previous
###############################################################################

# ## plot
# plot(myzip)
# 
# ## filter ma
# zip.ma <- myzip[myzip$STATE == 'MA',]
# plot(zip.ma)
# 
# ## union the eps markets -- use RI as a test bc it is small
# zip.ri <- myzip[myzip$STATE == 'RI',]
# plot(zip.ri)
# 
# ## copy the zipss from excel and read in
# zips <- read.table("clipboard", stringsAsFactors=F, colClasses="character")
# zips <- unlist(zips)
# 
# ## code splits into two polygons, either true or false - wont work for multi markets
# # m <- zip.ri$ZIP %in% zips
# # tmp <- unionSpatialPolygons(zip.ri, m)
# # plot(zip.ri)
# # plot(tmp)
# 
# m <- as.character(zip.ri$ZIP)
# for (i in 1:length(m)) {
#  ifelse(m[i] %in% zips, m[i] <- TRUE, m[i] <- NA)
# }
# tmp2 <- unionSpatialPolygons(zip.ri, m)
# plot(zip.ri)
# plot(tmp2)
# ## figure out how to reference and rename
# 
# tmp2@polygons@Polygons@ID
# 
# 
# tmp2@polygons[[1]]@ID <- 'RI 1'



