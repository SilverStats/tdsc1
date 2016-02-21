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
#<<<<<<< HEAD
                                          

min(bike$starttimeseconds)

#=======

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
  scale_color_gradient(low = "red", high = "green")


<<<<<<< HEAD
bike[which(bike$end.station.longitude == max(bike$end.station.longitude)),]
#>>>>>>> b65f10ba442eb679d78c5d7988d4afd53321663b
=======
######################################################
>>>>>>> 0ba56dba1d3484db78b3917052a5abc161bc7681
