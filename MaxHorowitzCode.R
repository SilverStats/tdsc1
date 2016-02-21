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

min(bike$start.station.latitude)
max(bike$start.station.latitude)
min(bike$start.station.longitude)
max(bike$start.station.longitude)

maplocation <- c(min(bike$start.station.longitude),
                 min(bike$start.station.latitude),
                  max(bike$start.station.longitude),
                  max(bike$start.station.latitude))

mymap <- get_map(location= maplocation,
        source= "stamen", crop=FALSE, resolution = "high")

ggmap(mymap) + geom_point(aes(x = start.station.longitude, 
                              y = start.station.longitude),
                         data = data.rows.counted)


### GGPLOT with Overalyed Map

citibike <- readJPEG("Citibike.jpeg")

ggplot(data.rows.counted, aes(y = start.station.latitude, x = start.station.longitude)) +
  annotation_custom(rasterGrob(citibike, width=unit(1,"npc"), height=unit(1,"npc"))) + 
  geom_point(aes(size = n))

bike[which(bike$end.station.longitude == max(bike$end.station.longitude)),]
