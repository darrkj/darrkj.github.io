# Initialize 

# Load packages
library(rvest)
library(dplyr)
library(leaflet)

# This makes reading data in from text files much more logical.
options(stringsAsFactors = FALSE)


# This function gets all of the URLs on the main page.
# The link to each location where data is contained.
get_all_urls <- function() {
  'http://www.gtfs-data-exchange.com/agencies' %>% 
    html() %>% html_nodes('a') %>% 
    html_attr('href') %>% 
    grep('agency', ., value = T) %>% 
    paste0('http://www.gtfs-data-exchange.com', .) -> locations
  
  # Last one is check
  locations[-length(locations)]
}

# This function will get the url for the zip file.
. %>% html() %>% 
  html_nodes('a') %>% 
  html_attr('href') %>%
  grep('.zip', ., value = T) %>% 
  grep('agency', ., value = T) %>%
  paste0('http://www.gtfs-data-exchange.com', .) -> get_zip_url

# Get a list of all of the cities and loop over each 
# site to get the location of the zip file.
# This will take a minute.
all_zips <- lapply(get_all_urls()[1:3], get_zip_url)


# Function to download a zip file, unzip it and move text files around.
download_gtfs <- function(url, loc) {
  download.file(url = url, destfile = paste(loc, basename(url), sep = '/'))
  
  name <- gsub('http://www.gtfs-data-exchange.com/agency', loc, url)
  name <- gsub('/latest.zip', '', name)
  # Unzip downloaded file
  url %>% basename %>% paste(loc, ., sep = '/') %>% unzip(., exdir = name)
  unlink(paste(loc, basename(url), sep = '/'))
}


# Create a folder to store the data.
dir.create('gtfs_data')
download_gtfs(all_zips[[1]], 'gtfs_data/')


# Move to the directory where the data is located.
setwd('gtfs_data/a-reich-gmbh-busbetrieb/')

# Read the files into R.
routes     <- read.csv("routes.txt")
stop_times <- read.csv("stop_times.txt")
stops      <- read.csv("stops.txt")
trips      <- read.csv("trips.txt")


# Join the data together.
stops %>% 
  inner_join(stop_times, by = "stop_id") %>%
  inner_join(trips, by = "trip_id") %>%
  inner_join(routes, by = "route_id") %>%
  select(stop_id, trip_id, route_id, stop_lon, stop_lat) -> data


ro1 <- data[data$trip_id == data$trip_id[1], ]


m <- leaflet() %>% addTiles()

ro1 %>% arrange(route_id) -> ro1


(m %>%
   setView(lng = ro1$stop_lon[1], lat = ro1$stop_lat[1], zoom = 8) %>%
   addCircles(color = 'black', lat = ro1$stop_lat, lng = ro1$stop_lon))




