##### Tartan Data Science Cup
##### Episode 1: Ride to Glory
#####
##### Steven Silverman (Max Horowitz, Devin Cortese)
#####

# populations
# https://data.cityofnewyork.us/City-Government/New-York-City-Population-By-Census-Tracts/37cg-gxjd

# polygons
# https://data.cityofnewyork.us/City-Government/2010-Census-Blocks-water-areas-included-/v9xd-tt3e

bike.orig <- read.csv("20150708-citibike-tripdata.csv") 
bike <- read.csv("20150708-citibike-tripdata.csv") 


matched.8 <- grep("7/8/2015",as.character(bike$stoptime)) # returned the same day
length(bike$stoptime) - length(matched.8) # number returned later on


library(sp)
library(rgeos)
library(dplyr)

census <- read.csv("nycb2010wi.csv")

disjoint <- grep(", \\(", census$the_geom)
census.clean <- census[-disjoint,]

geom_string <- function(x) {
    clipped <- substring(x, 15, nchar(x))
    return(paste("GEOMETRYCOLLECTION(POLYGON", clipped))
}
census.clean$geom_adj <- sapply(as.character(census.clean$the_geom), geom_string)
census.clean$geom_sp <- sapply(census.clean$geom_adj, readWKT)

station_to_sp <- function(latitude, longitude) {
    return(readWKT(paste("POINT(", longitude, " ", latitude, ")", sep="")))
}

data.rows.counted <- data.frame(dplyr::group_by(bike, start.station.latitude,
                                                start.station.longitude, 
                                                start.station.name) %>%
                                tally(sort = TRUE))

stations_sp <- sapply(1:nrow(data.rows.counted),
                      function(x) { station_to_sp(data.rows.counted[x,1],
                                                  data.rows.counted[x,2]) })

closest_station <- function(tract) {
    distances <- sapply(1:length(stations_sp),
                        function(i) { gDistance(stations_sp[[i]], tract) })
    return(distances)
    
}

census.clean.dist <- census.clean

distances.all <- data.frame(t(sapply(census.clean.dist$geom_sp, closest_station)))
