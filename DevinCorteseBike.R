################### Devin Cortese ################### 
########## Code Style: Google Style Format ##########
##########         2/21/2016               ##########
#####################################################

## Libraries
library(ggmap)
library(ggplot2)
library(np)
library(mgcv)
library(lubridate)
library(dplyr)
library(jpeg)
library(grid)

## Data
bike <- read.csv("C:/Users/rbcor_000/Desktop/tdsc1/newbike.csv")

## Start and End Station Frequency Plots
location_start_plot <- ggplot(data = bike, aes(x = start.station.id))
location_end_plot <- ggplot(data = bike, aes(x = end.station.id))

location_start_plot + geom_bar(stat = "count", fill = "green") + coord_cartesian() + 
  ggtitle("Start Station Frequency") + xlab("Start Station ID Number") + ylab("# of Times each Station is Used")

location_end_plot + geom_bar(stat = "count", fill = "blue") + coord_cartesian() + 
  ggtitle("End Station Frequency") + xlab("End Station ID Number") + ylab("# of Times each Station is Used")

## Gender Distribution over time
gender_distribution <- ggplot(bike, aes(factor(gender, levels = 0:2, labels = c("Unknown/Other", "Male", "Female")), bike$starttimeseconds))

gender_distribution + geom_boxplot(fill = "yellow") + ggtitle("Gender Distribution Over Time For Bike Stations") +
  xlab("Gender") + ylab("Time Trip Started (in Seconds)")

################# NEW YORK MAP #################

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

data.rows.counted$sqrtn <- sqrt(data.rows.counted$n)

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
                              color = sqrt(n)),
                          data = data.rows.counted) +
  scale_color_gradient(low = "royal blue", high = "dark red",
                       name = "Square Root of Rentals") +
  ggtitle("Number of Rentals for Each CitiBike Station") +
  xlab("Longitude") + ylab("Latitude")

bike[which(bike$end.station.longitude == max(bike$end.station.longitude)),]
