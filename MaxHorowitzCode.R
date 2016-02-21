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
                                          

min(bike$starttimeseconds)

