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
bad.boro <- as.character(census.clean$BoroName) %in% c("Bronx", "Staten Island")
census.clean <- subset(census.clean, bad.boro == FALSE) # remove far boroughs

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

closest_station <- function(point) {
    distances <- sapply(1:length(stations_sp),
                        function(i) { gDistance(stations_sp[[i]], point) })
    return(head(order(distances, decreasing=FALSE),5))
    
}

closest_tract <- function(point) {
    distances <- sapply(1:length(census.clean$geom_sp),
                        function(i) { gDistance(census.clean$geom_sp[[i]], point) })
    return(head(order(distances, decreasing=FALSE),5))
    
}

grid_sp <- sapply(1:nrow(nyc.grid.points.df),
                  function(x) { station_to_sp(nyc.grid.points.df[x,"Latitude"],
                                              nyc.grid.points.df[x,"Longitude"]) })

distances.stations <- data.frame(t(sapply(grid_sp, closest_station)))
distances.tracts <- data.frame(t(sapply(grid_sp, closest_tract)))



mean.station.users <- sapply(1:nrow(distances.stations), function(i) {
                                rows <- as.numeric(distances.stations[i,])
                                return(mean(data.rows.counted$n[rows]))})


pop.cens.2010$CT2010 <- pop.cens.2010$Census.Tract
merged.frame <- inner_join(census.clean.back, pop.cens.2010)

mean.tract.people <- sapply(1:nrow(distances.tracts), function(i) {
    rows <- as.numeric(distances.tracts[i,])
    return(mean(merged.frame$Population[rows]))})