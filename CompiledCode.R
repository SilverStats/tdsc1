###############################
######## Compiled Code ######## 
## Steven, Max, Devin #########
##     2/21/2015     #######

## Packages ##

library(ggmap)
library(ggplot2)
library(lubridate)
library(dplyr)
library(jpeg)
library(grid)
library(grdiExtra)
library(sp)
library(rgeos)
library(dplyr))

## Loading the Datasets ##

bike.orig <- read.csv("20150708-citibike-tripdata.csv") 
bike <- read.csv("20150708-citibike-tripdata.csv") 

## Plots ##

# Plot for Slides

# Number of Rental by loc

maplocation <- c(min(bike$start.station.longitude),
                 min(bike$start.station.latitude),
                 max(bike$start.station.longitude),
                 max(bike$start.station.latitude))

get_map(location= maplocation,
        source= "google", crop=FALSE, zoom = 12)

ggmap(mymap) + geom_point(aes(x = start.station.longitude,
                              y = start.station.latitude,
                              color = sqrt(n)),
                          data = data.rows.counted) +
  scale_color_gradient(low = "royal blue", high = "dark red",
                       name = "Square Root of Rentals") +
  ggtitle("Number of Rentals for Each Citi Bike Station") +
  xlab("Longitude") + ylab("Latitude")

## Maps for 2 page write up

long.newstat <- nyc.grid.points.df$Longitude[c(175,96)]
lat.newstat <- nyc.grid.points.df$Latitude[c(175,96)]

new.stat.data <- data.frame(long = long.newstat,
                            lat = lat.newstat)

final.recs <- ggmap(mymap) + geom_point(aes(x = long ,
                                            y = lat), data = new.stat.data,
                                        size= 5.5, colour = "darkorchid") +
  ggtitle("Citi Bike New Station Recommendations:
          Williamsburg and West Village") + xlab("Longitude") +
  ylab("Latitude")

first.plot <- ggmap(mymap) + geom_point(aes(x = start.station.longitude,
                                            y = start.station.latitude,
                                            color = sqrt(n)),
                                        data = data.rows.counted) +
  scale_color_gradient(low = "royal blue", high = "dark red",
                       name = "Square Root of Rentals") +
  ggtitle("Number of Rentals for Each Citi Bike Station") +
  xlab("Longitude") + ylab("Latitude") + 
  theme(legend.position=c(.85, 0.15))

grid.arrange(final.recs, first.plot, ncol = 2)

######################################################
######## Creating Grid for Dist Location   ############
########       TLong and Lat         ##################

grid.bikes.manhat <- do.call(c, lapply(1:11, FUN = function(x) {paste(x, "avenue and", 
                                                                      seq(from = 5, to = 80, by = 5),
                                                                      "Street", ", Manhattan", sep = c(" ", " ", "", " "))
}))

grid.bikes.misc <- c("Cadman Plaza Park", 
                     "Court Street and Smith Stree, Brooklyn",
                     "Barclays Center",
                     "Lafayette Aveneue and Washington Avenue, Brooklyn",
                     "Court Street and Carrol Street, Brooklyn",
                     "Broadway and South 8th Street, Brooklyn",
                     "Bowery Street and Canal Street",
                     "Spring Street and West Houston Street",
                     "Fulton Street and Nevins Street, Brooklyn",
                     "Myrtle Avenue and Nostrand Avenue, Brooklyn",
                     "St. Johns Place and 5th Avenue, Brooklyn",
                     "Avenue B and East 9th Street",
                     "Greenpoint Avenue and Manhattan Avenue, Brooklyn",
                     "Grand Street and Bushwick Avenue, Brooklyn")

grid.long.lat <- geocode(c(grid.bikes.manhat, grid.bikes.misc))

nyc.grid.points.df <- data.frame(Location = c(grid.bikes, grid.bikes.misc),
                                 Longitude = grid.long.lat[,1],
                                 Latitude = grid.long.lat[,2])

nyc.grid.points.df <- nyc.grid.points.df[-which(nyc.grid.points.df$Longitude < -80),]

save(nyc.grid.points.df, file = "nyc.grid.points.RData")

### Code to compute distance and average population in terms of 
### consensus tracts and distance in terms of bike stands

# Pull out polugon coordinates
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

# Compute station dist
stations_sp <- sapply(1:nrow(data.rows.counted),
                      function(x) { station_to_sp(data.rows.counted[x,1],
                                                  data.rows.counted[x,2]) })

closest_station <- function(point) {
  distances <- sapply(1:length(stations_sp),
                      function(i) { gDistance(stations_sp[[i]], point) })
  return(head(order(distances, decreasing=FALSE),5))
  
}

# Comput tract pop

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
  return(mean(merged.frame$Population[rows]/sapply(rows, function(i) {
    gArea(census.clean$geom_sp[i][[1]])})))}) # population density

nyc.grid.points.df[which.max(mean.tract.people),]
nyc.grid.points.df[which.max(mean.station.users),]
