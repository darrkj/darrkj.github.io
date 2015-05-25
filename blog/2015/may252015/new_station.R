options(stringsAsFactors = FALSE)
library(dplyr)
library(lubridate)
library(ggplot2)
library(MCL) # http://www.micans.org/mcl/
library(xml2)
library(deldir)
library(tripack)


get_locs <- function() {
  # Get data from web with locations
  'http://www.capitalbikeshare.com/data/stations/bikeStations.xml' %>%
    read_xml %>%
    xml_find_all('.//station') -> stations

  . %>% paste0('.//', .) %>% xml_find_all(stations, .) %>% xml_text -> get_vals
  
  c('terminalName', 'lat', 'long', 'nbBikes', 
    'nbEmptyDocks', 'lastCommWithServer') %>%
#  c('terminalName', 'lat', 'long') %>% 
    sapply(get_vals) %>% 
    data.frame %>% 
    filter(terminalName != "8D OPS test") %>%
    mutate(lat = as.numeric(lat), long = as.numeric(long))
}

# Read the file, and keep relevant data.
'2014-Q2-Trips-History-Data.csv' %>% 
  read.csv %>%
  select(sdate = Start.date, 
         edate = End.date, 
         sterm = Start.terminal, 
         eterm = End.terminal, 
         bike = Bike., 
         type = Subscriber.Type) %>% 
  filter(type == 'Registered') -> bike 

# A list of all stations in the data set.
c(bike$sterm, bike$eterm) %>% unique %>% na.omit -> term

# This initializes the transition matrix
t_mat <- matrix(0, ncol = length(term), nrow = length(term))

# Loop to get count of all station to station trips.
for (i in 1:length(term)) {
  # Subset local dataset for speed.
  tmp <- bike[bike$sterm == term[i], ]
  t_mat[i, ] <- sapply(term, function(x) sum(tmp$eterm == x, na.rm = T))
}


#
station <- data.frame(terminalName = as.character(term), 
                      clus_id = mcl(t_mat, F)$Cluster)

# Clean the data.
get_locs() %>% 
  select(terminalName, lat, long) %>%
  inner_join(station, by = 'terminalName') %>%
  na.omit() -> stations 

ggplot(stations, aes(long, lat)) + geom_point(aes(colour = factor(clus_id)))





# Which ones have been added over the last year.
geo <- get_locs()
geo[!geo$terminalName %in% stations$terminalName, ]

new <- merge(geo, station, all = T)
new$new <- ifelse(is.na(new$clus_id), 1, 0)


ggplot(new, aes(long, lat)) + geom_point(aes(colour = factor(new)))


# Can we compute the area of the voronoi
#


################

g <- stations[, c('long', 'lat')]
g <- geo[, c('long', 'lat')]
names(g) <- c('x', 'y')
g1 <- tile.list(deldir(g$x, g$y))
g2 <- tile.centroids(g1)
plot(g1,close=TRUE)
points(g2,pch=20,col="red")
############

voronoi.mosaic(g$x, g$y, duplicate="error")

plot(voronoi.mosaic(g$x, g$y, duplicate="error"))
############


# Clean the data.
get_locs() %>% 
  select(terminalName, lat, long) %>%
  inner_join(station, by = 'terminalName') %>%
  na.omit() %>% 
  filter(!clus_id %in% c('13', '14', '15')) -> stations 

ggplot(stations, aes(long, lat)) + geom_point(aes(colour = factor(clus_id)))


g <- stations[, c('long', 'lat')]
g <- geo[, c('long', 'lat')]
names(g) <- c('x', 'y')
g1 <- tile.list(deldir(g$x, g$y))
g2 <- tile.centroids(g1)
plot(g1,close=TRUE)
points(g2,pch=20,col="red")



xx <- data.frame(x = 0, y = 0)

for (i in g1) {
  xx <- rbind(xx, data.frame(x = i$x, y = i$y))
}
xx <- xx[-1, ]
xx <- unique(xx)



xx$x <- (xx$x - min(xx$x)) / (max(xx$x) - min(xx$x))
xx$y <- 1 - (xx$y - min(xx$y)) / (max(xx$y) - min(xx$y))
write.csv(xx, file = 'js/vor.csv', row.names = F)

#
