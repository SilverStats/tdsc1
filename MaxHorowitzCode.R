################# Max Horowitz Code ################# 
########## Code Style: Google Style Format ##########
##########         2/21/2016               ##########
#####################################################

# Libraries

library(ggmap)
library(ggplot2)
library(np)
library(mgcv)
library(lubridate)
library(dplyr)
library(jpeg)
library(grid)

#################
# Loading the Data
bike.orig <- read.csv("20150708-citibike-tripdata.csv") 
bike <- read.csv("20150708-citibike-tripdata.csv") 

## Merged Census Dataframes

pop.cens.tract <- read.csv("pop_by_tract.csv")
pop.cens.2010 <- subset(pop.cens.tract, Year == "2010")

nycensus.2010 <- read.csv("nycb2010wi.csv")

census.merged <- merge(pop.cens.2010, nycensus.2010,
                       by.x = "Census.Tract",
                       by.y = "CT2010")
######################################################
### EDA 

sapply(bike, class)
dim(bike)

# sapply(bike[,which(sapply(bike,class) == "factor")], unique)

### Change Start Time and Stop Time to Minutes

timeonlystart <- sapply(strsplit(as.character(bike$starttime), split = " "), 
       FUN = function(x) x[2])

bike$starttimeseconds <- period_to_seconds(hms(timeonlystart))

ggplot(bike, aes(x = starttimeseconds)) + geom_histogram(bins = 20,
                                          aes(alpha = ..density..))

ggplot(bike, aes(x = start.station.latitude, y = start.station.longitude)) +
  geom_point()

data.rows.counted <- dplyr::group_by(bike, start.station.latitude, start.station.longitude, 
                start.station.name) %>% tally(sort = TRUE)

ggplot(data.rows.counted, aes(x = start.station.latitude, y = start.station.longitude)) +
  geom_point(aes(size = n))

## Finding the location of box for map

maplocation <- c(min(bike$start.station.longitude),
                 min(bike$start.station.latitude),
                  max(bike$start.station.longitude),
                  max(bike$start.station.latitude))

# Creating Maps and overlaying points

mymap <- get_map(location= maplocation,
        source= "google", crop=FALSE, zoom = 12)

ggmap(mymap) + geom_point(aes(x = start.station.longitude,
                          y = start.station.latitude,
                          color = n),
                          data = data.rows.counted) +
  scale_color_gradient(low = "red", high = "green") +ggtitle("TEST")


######################################################
######## Creating Grid for Location ##################
########       Long and Lat         ##################

grid.bikes.manhat <- do.call(c, lapply(1:11, FUN = function(x) {paste(x, "avenue and", 
                                                     seq(from = 5, to = 80, by = 5),
                                                     "Street", sep = c(" ", " ", "", " "))
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

grid.long.lat <- geocode(c(grid.bikes, grid.bikes.misc))

nyc.grid.points.df <- data.frame(Location = c(grid.bikes, grid.bikes.misc),
                                 Longitude = grid.long.lat[,1],
                                 Latitude = grid.long.lat[,2])

