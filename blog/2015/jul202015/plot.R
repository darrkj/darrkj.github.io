library(dplyr)
library(leaflet)

options(stringsAsFactors = F)

# Read files into workspace
routes     <- read.csv("routes.txt")
stop_times <- read.csv("stop_times.txt")
stops      <- read.csv("stops.txt")
trips      <- read.csv("trips.txt")

# Create one clean dataset 
merge(stops, stop_times) %>%
  merge(trips) %>%
  merge(., routes, by = "route_id") %>% 
  select(trip_id, route_id, stop_lon, stop_lat) -> gtfs

ro1 <- gtfs[gtfs$trip_id == unique(gtfs$trip_id)[1], ]

ro1[order(ro1$trip_id), ] %>% arrange(route_id) -> r1


(leaflet() %>% addTiles() %>%
   setView(lng = ro1$stop_lon[1], lat = ro1$stop_lat[1], zoom = 10) %>%
   addCircles(color = 'black', lat = ro1$stop_lat, lng = ro1$stop_lon))


