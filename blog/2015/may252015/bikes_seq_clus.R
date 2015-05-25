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



# Merge station onto bike.

station %>% select(sterm = terminalName, sclus = clus_id) -> start
start$sterm = as.integer(start$sterm)
station %>% select(eterm = terminalName, eclus = clus_id) -> end
end$eterm = as.integer(end$eterm)

bike %>% inner_join(start) %>% inner_join(end) -> bike









# Which ones have been added over the last year.
geo <- get_locs()
geo[!geo$terminalName %in% stations$terminalName, ]

new <- merge(geo, station, all = T)
new$new <- ifelse(is.na(new$clus_id), 1, 0)


ggplot(new, aes(long, lat)) + geom_point(aes(colour = factor(new)))
