library(ggplot2)

bike <- read.csv("C:/Users/rbcor_000/Desktop/tdsc1/20150708-citibike-tripdata.csv")

location_start_plot <- ggplot(data = bike, aes(x = start.station.id))
location_end_plot <- ggplot(data = bike, aes(x = end.station.id))

location_start_plot + geom_bar(stat = "count", fill = "green") + coord_cartesian() + 
  ggtitle("Start Station Frequency") + xlab("Start Station ID Number") + ylab("# of Times each Station is Used")

location_end_plot + geom_bar(stat = "count", fill = "blue") + coord_cartesian() + 
  ggtitle("End Station Frequency") + xlab("End Station ID Number") + ylab("# of Times each Station is Used")

