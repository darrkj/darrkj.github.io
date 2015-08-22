library(dplyr)
library(leaflet)

options(stringsAsFactors = FALSE)

setwd('gtfs_data/a-reich-gmbh-busbetrieb/')

routes     <- read.csv("routes.txt")
stop_times <- read.csv("stop_times.txt")
stops      <- read.csv("stops.txt")
trips      <- read.csv("trips.txt")



stops %>% 
  inner_join(stop_times, by = "stop_id") %>%
  inner_join(trips, by = "trip_id") %>%
  inner_join(routes, by = "route_id") %>%
  select(stop_id, trip_id, route_id, stop_lon, stop_lat) -> data


ro1 <- data[data$trip_id == data$trip_id[1], ]


m = leaflet() %>% addTiles()

ro1 %>% arrange(route_id) -> ro1


(m %>%
   setView(lng = ro1$stop_lon[1], lat = ro1$stop_lat[1], zoom = 10) %>%
   addCircles(color = 'black', lat = ro1$stop_lat, lng = ro1$stop_lon))

##############



trip <- unique(data$trip_id)
route <- unique(data$route_id)

dd <- data[data$route_id == route[5], ]

plot(dd$stop_lon, dd$stop_lat)

ro1 <- data[data$trip_id == data$trip_id[1], ]
ro2 <- data[data$trip_id == data$trip_id[220], ]

ro1 <- ro1[order(ro1$trip_id), ]
ro2 <- ro2[order(ro2$trip_id), ]

x3 <- dd

plot(ro1$stop_lon, ro1$stop_lat, 
     xlim = c(min(x3$stop_lon), max(x3$stop_lon)),
     ylim = c(min(x3$stop_lat), max(x3$stop_lat)), col = 'red')
points(ro2$stop_lon, ro2$stop_lat, col = 'blue')


m = leaflet() %>% addTiles()

ro1 %>% arrange(route_id) -> ro1


(m %>%
   setView(lng = ro1$stop_lon[1], lat = ro1$stop_lat[1], zoom = 10) %>%
   addCircles(color = 'black', lat = ro1$stop_lat, lng = ro1$stop_lon))

