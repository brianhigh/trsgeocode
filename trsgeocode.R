# Geocode Washington State alfalfa seed crop data given township section TRS 
# codes and plot a map showing acres of alfalfa per township section. 
#
# Copyright 2016 Brian High (https://github.com/brianhigh) and Anika Larsen
# License: GNU GPL v3 http://www.gnu.org/licenses/gpl.txt
#
# This script serves as an example of using the following techniques:
#
# * Querying an ArcGIS REST API using SQL-like syntax
# * Using a URL with a "GET" query string to fetch data
# * Importing data in JSON format 
# * Geocoding TRS codes using the BLM web service API
# * Importing data in XML format
# * Decoding HTML-encoded text
# * Using regular expressions for pattern matching and string replacement
# * Mapping with ggplot2
# * Caching data in files to avoid redoing slower steps if re-running script
#
# -------------------------------------------------------------------------

# Clear R's workspace memory to start with a fresh workspace.
rm(list=ls())

# Initialize variables
cropType <- "Alfalfa Seed"

## Install packages and load into memory
for (pkg in c("RJSONIO", "XML", "httr", "plyr", "dplyr", 
              "maps", "mapdata", "ggplot2")) {
    if(pkg %in% rownames(installed.packages()) == FALSE) {
        install.packages(pkg, quiet = TRUE, 
                         repos="http://cran.fhcrc.org",
                         dependencies=TRUE)
    }
    suppressWarnings(suppressPackageStartupMessages(
        require(pkg, character.only = TRUE, quietly = TRUE)))
}

# ----------------------------------------------------------------------
# Function Definitions
# ----------------------------------------------------------------------

## Function getData fetches the JSON data from the REST service
getData <- function(data_file) {
    # Get crop data from WSDA 2014 ArcGIS REST service.
    # Note: This data is shown on this map: http://arcg.is/1QhDoo2
    
    # Get 2014 Crop Distribution CropType codes (for reference), since
    # you can't search by CropType name, but only by (integer) code
    url <- paste("https://fortress.wa.gov/agr/gis/wsdagis/rest/services/NRAS/",
                 "2014CropDistribution/MapServer/1?f=pjson", sep="")
    fields <- fromJSON(content(GET(url)))
    fieldCodes <- adply(lapply(fields[['fields']][[3]]$domain$codedValues, 
                               FUN=function(x) data.frame(name=x$name, 
                                                          code=x$code)), 1)[,-1]
    write.csv(fieldCodes, "field_codes.csv", quote=FALSE, row.names=FALSE)
    
    # Get crop data
    cropTypeCode <- fieldCodes[fieldCodes$name==cropType, 'code']
    url <- paste("https://fortress.wa.gov/agr/gis/wsdagis/rest/services/NRAS/",
                 "2014CropDistribution/MapServer/1/query?",
                 "where=CropType+%3D+", cropTypeCode, 
                 "&outFields=TRS%2C+ExactAcres&f=pjson",
                 sep="")
    
    # Fetch JSON data and import it into a data frame
    json <- GET(url)
    jsonContent <- content(json, "text")
    write(jsonContent, "crop.json")
    
    cropData <- fromJSON(jsonContent)
    cropFeatures <- cropData[['features']]
    crop <- data.frame(adply(lapply(cropFeatures, function(x) {
        data.frame(acres=x$attributes$ExactAcres, trscode=x$attributes$TRS,
                   stringsAsFactors=FALSE)}), 1)[-1])
    
    # Summarize by trscode to get total acres of chosen crop type per TRS.
    # Use 12 digits and 10 significant figures to reproduce original data set
    # as provided by Anika Larsen (orig. from Perry Beale via Eddie Kasner).
    # This is to confirm that this procedure produces the same results.
    options(digits=12)
    cropGrouped <- group_by(crop, trscode)
    Data <- summarise(cropGrouped, 
                             "acres"=signif(sum(acres), 10))
    Data <- arrange(Data, trscode)
    write.csv(Data, data_file, quote=FALSE, row.names=FALSE)
    return(Data)
}

## Function html2txt decodes HTML-encoded strings
# See: http://stackoverflow.com/questions/5060076
html2txt <- function(str) {
    xpathApply(htmlParse(str, asText=TRUE), "//body//text()", xmlValue) 
}

## Function capwords capitalizes words
# See: https://stat.ethz.ch/R-manual/R-devel/library/base/html/chartr.html
capwords <- function(s, strict=FALSE) {
    cap <- function(s) {
        paste(toupper(substring(s, 1, 1)),
              {s <- substring(s, 2); if(strict) tolower(s) else s},
              sep="", collapse=" " )
    }
    sapply(strsplit(s, split=" "), cap, USE.NAMES=!is.null(names(s)))
}

## Function trs2blm converts TRS code for WA township sections to BLM API format
trs2blm <- function(trscode) {
    # Assume:         state=WA, primary meridian=33, township direction=N
    # Input format:   T#R#[EW]# 
    #                 Where T# is Township (Tier) number, R# is Range number, 
    #                 [EW] is range direction and the final # is Section number.
    # Output format:  WA,33,#,0,N,#,0,[EW]#,,0 (using TRS #s from above)
    # Example input:  T06R32E1
    # Example output: WA,33,06,0,N,32,0,E,1,,0
    # See: http://www.geocommunicator.gov/GeoComm/services.htm#Data
    # And: https://en.wikipedia.org/wiki/Public_Land_Survey_System
    
    regexp <- "^T(\\d+)R(\\d+)([EW]+)(\\d+)$"
    match <- grep(regexp, trscode)
    if (length(match) > 0 && match == 1) {
        return(gsub(regexp, "WA,33,\\1,0,N,\\2,0,\\3,\\4,,0", trscode))
    } else {
        return(NA)
    }
}

## Function getLatLong gets latitude and longitude using BLM API given TRS code
getLatLong <- function(trscode) {
    # Convert the TRS code into the format expected by the BLM API
    blmcode <- trs2blm(trscode)
    if (is.na(blmcode) == TRUE) return(NA)
    
    # Fetch the latitude and longitude data in XML format using the BLM API
    baseurl <-
        'http://www.geocommunicator.gov/TownshipGeoCoder/TownshipGeoCoder.asmx/'
    url <- URLencode(paste(baseurl, "GetLatLon?TRS=", blmcode, sep=""))
    response <- GET(url)
    
    # Parse the XML output into a list structure
    doc <- xmlTreeParse(response, asText=TRUE)
    result <- doc[[1]]$children$TownshipGeocoderResult
    
    # Decode HTML-encoded strings
    decoded <- unlist(html2txt(result))
    
    # Extract the point coordinates into a vector
    point <- as.numeric(unlist(strsplit(decoded[15], " ")))
    names(point) <- c("lon", "lat")
    point <- point[c("lat", "lon")]
    
    # Extract the polygon coordinates into a data frame
    polygon <- as.numeric(as.vector(unlist(strsplit(decoded[25], ","))))
    rows <- length(polygon)/2
    polygon <- as.data.frame(matrix(polygon, nrow=rows, ncol=2, byrow=TRUE))
    names(polygon) <- c("lat", "lon")
    
    # Return the point vector and polygon data frame as a list
    return(list(point=point, polygon=polygon))
}

## Function getCropLatLong gets lat and long for a row in crop dataframe
getCropLatLong <- function(dataRow) {
    latlong <- getLatLong(dataRow$trscode)
    
    # Return only the original values if unable to geocode
    if (length(latlong) > 0 && is.na(latlong) == TRUE) {
        print(paste("Can't geocode", dataRow$trscode, "!"))
        return(data.frame(acres=dataRow$acres, trscode=dataRow$trscode,
                          row.names=NULL))
    }
    
    point <- latlong[[1]]
    polygon <- latlong[[2]]
    result <- data.frame(acres=dataRow$acres, trscode=dataRow$trscode,
                         point.lat=point['lat'], point.lon=point['lon'],
                         polygon=polygon, row.names=NULL)
    return(result)
}

# ----------------------------------------------------------------------
# Main Routine
# ----------------------------------------------------------------------

# Read crop data from a CSV, if present
processed_file <- "GeocodedDataFromREST.csv"

if (! file.exists(processed_file)) {
    # Read in summarized crop data
    data_file <- "DataFromREST.csv"
    if (! file.exists(data_file)) {
        cropData <- getData(data_file)
    } else {
        cropData <- read.csv(data_file, stringsAsFactors=FALSE, header=TRUE)
    }

    # Geocode crop data; .margins=1 means "split up by rows".
    GeocodedData <- adply(.data=cropData[1,], .margins=1, .fun=getCropLatLong)
    
    # Remove incomplete cases (those cases containing NAs)
    GeocodedData <- GeocodedData[complete.cases(GeocodedData),]
    
    # Save as CSV for later use
    write.csv(GeocodedData, processed_file, row.names=FALSE)
} else {
    # Read in CSV containing the geocoded crop data
    GeocodedData <- read.csv(processed_file)
}

# Group by trscode for mapping
GeocodedData <- group_by(GeocodedData, trscode)

# Create a theme with no border, grid, axis, etc.
# From: http://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
ditch_the_axes <- theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()
)

# Get the base map of Washington State
states <- map_data("state")
wa_df <- subset(states, region == "washington")
wa_base <- ggplot(data=wa_df,
                  mapping = aes(x=long, y=lat, group=group)) + 
                  geom_polygon(color="darkgrey", fill="white")

# Get county coordinates for Washington State
counties <- map_data("county")
wa_county <- subset(counties, region == "washington")

# Capitalize the county names
wa_county$subregion <- sapply(wa_county$subregion, function(x) capwords(x))
cnames <- aggregate(cbind(long, lat) ~ subregion, data=wa_county, 
                    FUN=function(x)mean(range(x)))

# Find the coordinates of map boundaries, zoomed in for points of interest
max.lat <- round(max(GeocodedData$point.lat, na.rm = TRUE), 2)
min.lat <- round(min(GeocodedData$point.lat, na.rm = TRUE), 2)
max.lon <- round(max(GeocodedData$point.lon, na.rm = TRUE), 2)
min.lon <- round(min(GeocodedData$point.lon, na.rm = TRUE), 2)
max.lat <- max.lat + ((max.lat - min.lat) / 4)
min.lat <- min.lat - ((max.lat - min.lat) / 4)
max.lon <- max.lon + ((max.lon - min.lon) / 4)
min.lon <- min.lon - ((max.lon - min.lon) / 4)

# Plot the map, zooming in on township sections, with county names
g <- wa_base + theme_bw() + ditch_the_axes + 
     geom_polygon(data = wa_county, fill=NA, color="grey") +
     geom_polygon(data = GeocodedData,
                 aes(x=polygon.lon, y=polygon.lat, 
                     group=trscode, fill=acres)) +
     geom_text(data=cnames, size=4, color="darkgrey",
              aes(long, lat, label=subregion, group=subregion)) +
     scale_fill_gradientn(colours=rev(rainbow(7)),
                        breaks=c(2, 4, 10, 100, 1000, 10000),
                        trans="log10", name=paste("Acres of", 
                                                  cropType, sep="\n")) +
     geom_polygon(color="darkgrey", fill=NA) +
     coord_fixed(xlim=c(min.lon, max.lon), ylim=c(min.lat, max.lat),
                ratio=1.5) + 
     ggtitle(paste(cropType, 
                   "Crop\nAcres per Township Section\n(WSDA, 2014)\n"))

plot(g)

# Compare map to: http://arcg.is/1QhDoo2