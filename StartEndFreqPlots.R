################### Devin Cortese ################### 
########## Code Style: Google Style Format ##########
##########         2/21/2016               ##########
#####################################################

## Libraries
library(ggplot2)

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
